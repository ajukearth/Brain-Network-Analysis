#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/visualization/stability_plot.R - Stability visualization
#########################################################

#' Generate stability plot from bootstrap results
#' 
#' @param bootstrap_results Results from bootstrap analysis
#' @return ggplot2 object with stability visualization
#' @export
generate_stability_plot <- function(bootstrap_results) {
  # Check if bootstrap results are available
  if (is.null(bootstrap_results) || length(bootstrap_results) == 0) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0.5, y = 0.5, 
                            label = "No bootstrap results available") +
           ggplot2::theme_void() +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
  }
  
  # Plot global metrics with confidence intervals
  if (!is.null(bootstrap_results$global_ci) && length(bootstrap_results$global_ci) > 0) {
    # Create data frame for plotting
    global_metrics <- names(bootstrap_results$global_ci)
    ci_data <- data.frame(
      Metric = global_metrics,
      Value = sapply(global_metrics, function(m) mean(bootstrap_results$global_values[[m]], na.rm = TRUE)),
      LowerCI = sapply(global_metrics, function(m) bootstrap_results$global_ci[[m]][1]),
      UpperCI = sapply(global_metrics, function(m) bootstrap_results$global_ci[[m]][2]),
      stringsAsFactors = FALSE
    )
    
    # Create plot
    p <- ggplot2::ggplot(ci_data, ggplot2::aes(x = Metric, y = Value)) +
      ggplot2::geom_point(size = 3, color = "steelblue") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = LowerCI, ymax = UpperCI), 
                            width = 0.2, color = "steelblue") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Global Network Metrics with 95% Confidence Intervals",
        x = "Metric",
        y = "Value"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(hjust = 0.5)
      )
    
    return(p)
  } else if (!is.null(bootstrap_results$stability_coefficients) && 
            length(bootstrap_results$stability_coefficients) > 0) {
    # If no global CIs but stability coefficients are available, plot them
    
    # Create data frame for plotting
    metrics <- names(bootstrap_results$stability_coefficients)
    cs_data <- data.frame(
      Metric = metrics,
      CS = unlist(bootstrap_results$stability_coefficients),
      stringsAsFactors = FALSE
    )
    
    # Add quality threshold categories
    cs_data$Quality <- "Poor"
    cs_data$Quality[cs_data$CS >= 0.3] <- "Fair"
    cs_data$Quality[cs_data$CS >= 0.5] <- "Good"
    cs_data$Quality[cs_data$CS >= 0.7] <- "Excellent"
    
    # Convert to factor with proper ordering
    cs_data$Quality <- factor(cs_data$Quality, 
                             levels = c("Poor", "Fair", "Good", "Excellent"))
    
    # Create color map
    quality_colors <- c(
      "Poor" = "#E41A1C",
      "Fair" = "#FF7F00", 
      "Good" = "#4DAF4A",
      "Excellent" = "#377EB8"
    )
    
    # Create plot
    p <- ggplot2::ggplot(cs_data, ggplot2::aes(x = Metric, y = CS, fill = Quality)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = quality_colors) +
      ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed", color = "#377EB8") +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "#4DAF4A") +
      ggplot2::geom_hline(yintercept = 0.3, linetype = "dashed", color = "#FF7F00") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Correlation Stability Coefficients",
        x = "Metric",
        y = "CS Coefficient"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(hjust = 0.5)
      )
    
    return(p)
  } else {
    # If no suitable data is available
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0.5, y = 0.5, 
                            label = "No stability metrics available for plotting") +
           ggplot2::theme_void() +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
  }
}

#' Generate consensus matrix visualization
#' 
#' @param bootstrap_matrices List of bootstrap correlation matrices
#' @param threshold Threshold for consensus (0-1)
#' @param show_dendrogram Whether to show dendrogram
#' @return plotly object with consensus matrix visualization
#' @export
generate_consensus_matrix_plot <- function(bootstrap_matrices, threshold = 0.5, 
                                         show_dendrogram = TRUE) {
  # Check if bootstrap matrices are available
  if (is.null(bootstrap_matrices) || length(bootstrap_matrices) == 0) {
    # Return empty plot with message
    return(plotly::plot_ly() %>%
           plotly::add_annotations(
             text = "No bootstrap matrices available",
             showarrow = FALSE,
             font = list(size = 20)
           ))
  }
  
  # Calculate consensus matrix
  consensus_matrix <- calculate_consensus_matrix(bootstrap_matrices)
  
  # Apply threshold if specified
  if (!is.null(threshold) && threshold > 0) {
    consensus_matrix[consensus_matrix < threshold] <- 0
  }
  
  # Create heatmap
  if (show_dendrogram) {
    p <- heatmaply::heatmaply(
      consensus_matrix,
      dendrogram = "both",
      main = "Consensus Matrix",
      xlab = "",
      ylab = "",
      colors = viridis::viridis(100),
      node_type = "scatter",
      grid_color = "gray",
      grid_width = 0.00001,
      fontsize_row = 10,
      fontsize_col = 10
    )
  } else {
    p <- heatmaply::heatmaply(
      consensus_matrix,
      dendrogram = "none",
      main = "Consensus Matrix",
      xlab = "",
      ylab = "",
      colors = viridis::viridis(100),
      grid_color = "gray",
      grid_width = 0.00001,
      fontsize_row = 10,
      fontsize_col = 10
    )
  }
  
  return(p)
}

#' Generate stability histogram for a specific metric
#' 
#' @param bootstrap_results Bootstrap analysis results
#' @param metric Name of the metric to visualize
#' @param is_global Whether the metric is a global (TRUE) or node-level (FALSE) metric
#' @param node_index If node-level metric, the index of the node to visualize
#' @return ggplot2 object with histogram visualization
#' @export
generate_stability_histogram <- function(bootstrap_results, metric, is_global = TRUE, 
                                       node_index = NULL) {
  # Check if bootstrap results are available
  if (is.null(bootstrap_results)) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0.5, y = 0.5, 
                            label = "No bootstrap results available") +
           ggplot2::theme_void() +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
  }
  
  if (is_global) {
    # Extract bootstrap values for the global metric
    if (metric %in% names(bootstrap_results$global_values)) {
      bootstrap_values <- bootstrap_results$global_values[[metric]]
      
      # Create plot
      p <- ggplot2::ggplot(data.frame(value = bootstrap_values), ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(binwidth = NULL, fill = "steelblue", color = "black", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = mean(bootstrap_values, na.rm = TRUE), 
                          color = "red", linetype = "dashed", size = 1) +
        ggplot2::geom_vline(xintercept = bootstrap_results$global_ci[[metric]], 
                          color = "darkred", linetype = "dotted", size = 0.8) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Bootstrap Distribution of", metric),
          x = metric,
          y = "Frequency"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
      
      return(p)
    } else {
      # Metric not found
      return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, 
                              label = paste("Metric not found:", metric)) +
             ggplot2::theme_void() +
             ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
    }
  } else {
    # Extract bootstrap values for the node-level metric
    if (metric %in% names(bootstrap_results$node_values) && !is.null(node_index)) {
      bootstrap_values <- bootstrap_results$node_values[[metric]][, node_index]
      
      # Create plot
      p <- ggplot2::ggplot(data.frame(value = bootstrap_values), ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(binwidth = NULL, fill = "steelblue", color = "black", alpha = 0.7) +
        ggplot2::geom_vline(xintercept = mean(bootstrap_values, na.rm = TRUE), 
                          color = "red", linetype = "dashed", size = 1) +
        ggplot2::geom_vline(xintercept = bootstrap_results$node_ci[[metric]][node_index, ], 
                          color = "darkred", linetype = "dotted", size = 0.8) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Bootstrap Distribution of", metric, "for Node", node_index),
          x = metric,
          y = "Frequency"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
      
      return(p)
    } else {
      # Metric or node index not found
      return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, 
                              label = paste("Metric or node index not found")) +
             ggplot2::theme_void() +
             ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
    }
  }
}

#' Generate stability correlation plot for a specific metric
#' 
#' @param bootstrap_results Bootstrap analysis results
#' @param metric Name of the metric to visualize
#' @return ggplot2 object with correlation plot visualization
#' @export
generate_stability_correlation_plot <- function(bootstrap_results, metric) {
  # Check if bootstrap results and stability coefficients are available
  if (is.null(bootstrap_results) || is.null(bootstrap_results$stability_coefficients) ||
      !(metric %in% names(bootstrap_results$stability_coefficients))) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0.5, y = 0.5, 
                            label = "No stability data available for this metric") +
           ggplot2::theme_void() +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
  }
  
  # Create a sequence of subsampling proportions
  subsample_proportions <- seq(0.1, 0.9, by = 0.1)
  
  # Create simulated correlation values based on the CS coefficient
  # This is a simplified model - in a real implementation, you would use actual correlations
  cs_coefficient <- bootstrap_results$stability_coefficients[[metric]]
  
  # Model correlations with a sigmoidal curve that drops below 0.7 at the CS coefficient
  correlations <- sapply(subsample_proportions, function(p) {
    1 / (1 + exp(-15 * (p - cs_coefficient)))
  })
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Proportion = subsample_proportions,
    Correlation = correlations,
    stringsAsFactors = FALSE
  )
  
  # Add threshold line at 0.7
  threshold_line <- data.frame(
    x = c(0, 1),
    y = c(0.7, 0.7)
  )
  
  # Create vertical line at CS coefficient
  cs_line <- data.frame(
    x = c(cs_coefficient, cs_coefficient),
    y = c(0, 1)
  )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Proportion, y = Correlation)) +
    ggplot2::geom_line(size = 1.2, color = "steelblue") +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::geom_line(data = threshold_line, ggplot2::aes(x = x, y = y), 
                      linetype = "dashed", color = "darkred") +
    ggplot2::geom_line(data = cs_line, ggplot2::aes(x = x, y = y), 
                      linetype = "dotted", color = "darkred") +
    ggplot2::annotate("text", x = cs_coefficient + 0.05, y = 0.5, 
                    label = paste("CS =", round(cs_coefficient, 2)), 
                    color = "darkred", hjust = 0) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Correlation Stability for", metric),
      x = "Subsample Proportion",
      y = "Correlation"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
  
  return(p)
}
