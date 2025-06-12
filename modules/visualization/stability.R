#########################################################
# Enhanced Brain Network Analysis Shiny App
# Visualization/stability.R - Network stability visualization
#########################################################

#' Visualize stability results from bootstrap analysis
#' 
#' @param bootstrap_results Results from bootstrap analysis
#' @param metrics Which metrics to visualize (NULL for all)
#' @param plot_type Type of plot ("boxplot", "density", "scatter", "heatmap")
#' @param options Additional plot options
#' @return ggplot2 object with stability visualization
#' @export
visualize_stability <- function(bootstrap_results, metrics = NULL, 
                               plot_type = "boxplot", options = NULL) {
  # Check input
  if (!is.list(bootstrap_results)) {
    stop("bootstrap_results must be a list")
  }
  
  # Extract options with defaults
  if (is.null(options)) {
    options <- list(
      title = "Network Stability Analysis",
      color_palette = "viridis",
      show_points = TRUE,
      facet_by_metric = TRUE,
      interactive = FALSE
    )
  }
  
  title <- ifelse(is.null(options$title), "Network Stability Analysis", options$title)
  color_palette <- ifelse(is.null(options$color_palette), "viridis", options$color_palette)
  show_points <- ifelse(is.null(options$show_points), TRUE, options$show_points)
  facet_by_metric <- ifelse(is.null(options$facet_by_metric), TRUE, options$facet_by_metric)
  interactive <- ifelse(is.null(options$interactive), FALSE, options$interactive)
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for stability visualization")
  }
  
  # Prepare data for plotting
  if (plot_type == "boxplot" || plot_type == "density") {
    # For boxplot and density plots, we need bootstrap distributions
    
    # Check which metric value arrays are available
    available_metrics <- c()
    for (name in names(bootstrap_results)) {
      if (is.numeric(bootstrap_results[[name]]) && length(bootstrap_results[[name]]) > 1) {
        available_metrics <- c(available_metrics, name)
      }
    }
    
    # Filter metrics if specified
    if (!is.null(metrics)) {
      available_metrics <- intersect(available_metrics, metrics)
    }
    
    if (length(available_metrics) == 0) {
      stop("No suitable metric arrays found in bootstrap_results")
    }
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Metric = character(0),
      Value = numeric(0),
      stringsAsFactors = FALSE
    )
    
    for (metric in available_metrics) {
      values <- bootstrap_results[[metric]]
      
      if (length(values) > 0) {
        metric_data <- data.frame(
          Metric = rep(metric, length(values)),
          Value = values,
          stringsAsFactors = FALSE
        )
        
        plot_data <- rbind(plot_data, metric_data)
      }
    }
    
    # Create plot
    if (plot_type == "boxplot") {
      p <- create_stability_boxplot(plot_data, title, color_palette, 
                                  show_points, facet_by_metric)
    } else {  # density
      p <- create_stability_density(plot_data, title, color_palette, 
                                  show_points, facet_by_metric)
    }
  } else if (plot_type == "scatter") {
    # For scatter plots, we need paired metric values
    if (length(metrics) != 2) {
      stop("Exactly two metrics must be specified for scatter plot")
    }
    
    if (!all(metrics %in% names(bootstrap_results))) {
      stop("Specified metrics not found in bootstrap_results")
    }
    
    metric1 <- metrics[1]
    metric2 <- metrics[2]
    
    if (length(bootstrap_results[[metric1]]) != length(bootstrap_results[[metric2]])) {
      stop("Metrics must have the same number of bootstrap samples")
    }
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Metric1 = bootstrap_results[[metric1]],
      Metric2 = bootstrap_results[[metric2]],
      stringsAsFactors = FALSE
    )
    
    names(plot_data) <- c(metric1, metric2)
    
    # Create scatter plot
    p <- create_stability_scatter(plot_data, metric1, metric2, title, color_palette)
  } else if (plot_type == "heatmap") {
    # For heatmap, we need the stability coefficients
    if (!"stability_coefficients" %in% names(bootstrap_results)) {
      stop("No stability_coefficients found in bootstrap_results")
    }
    
    stability_coefs <- bootstrap_results$stability_coefficients
    
    if (!is.list(stability_coefs) || length(stability_coefs) == 0) {
      stop("Invalid stability_coefficients in bootstrap_results")
    }
    
    # Create data frame for heatmap
    plot_data <- data.frame(
      Metric = names(stability_coefs),
      Coefficient = unlist(stability_coefs),
      stringsAsFactors = FALSE
    )
    
    # Filter metrics if specified
    if (!is.null(metrics)) {
      plot_data <- plot_data[plot_data$Metric %in% metrics, ]
    }
    
    # Create heatmap
    p <- create_stability_heatmap(plot_data, title, color_palette)
  } else {
    stop("Unknown plot_type: ", plot_type)
  }
  
  # Make interactive if requested
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create boxplot of stability results
#' 
#' @param data Data frame with Metric and Value columns
#' @param title Plot title
#' @param color_palette Color palette
#' @param show_points Whether to show individual points
#' @param facet_by_metric Whether to facet by metric
#' @return ggplot2 object with boxplot
#' @export
create_stability_boxplot <- function(data, title, color_palette, 
                                   show_points, facet_by_metric) {
  # Create boxplot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = Metric, y = Value, fill = Metric)) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::labs(title = title, y = "Value", x = "Metric") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  # Add color palette
  if (color_palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
    p <- p + viridis::scale_fill_viridis(discrete = TRUE)
  } else if (color_palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
    p <- p + ggplot2::scale_fill_brewer(palette = "Set1")
  }
  
  # Add individual points if requested
  if (show_points) {
    p <- p + ggplot2::geom_jitter(alpha = 0.3, width = 0.2, height = 0)
  }
  
  # Facet by metric if requested
  if (facet_by_metric) {
    p <- p + ggplot2::facet_wrap(~Metric, scales = "free_y")
  }
  
  return(p)
}

#' Create density plot of stability results
#' 
#' @param data Data frame with Metric and Value columns
#' @param title Plot title
#' @param color_palette Color palette
#' @param show_points Whether to show individual points
#' @param facet_by_metric Whether to facet by metric
#' @return ggplot2 object with density plot
#' @export
create_stability_density <- function(data, title, color_palette, 
                                   show_points, facet_by_metric) {
  # Create density plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = Value, fill = Metric, color = Metric)) +
    ggplot2::geom_density(alpha = 0.3) +
    ggplot2::labs(title = title, x = "Value", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  
  # Add color palette
  if (color_palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
    p <- p + viridis::scale_fill_viridis(discrete = TRUE) +
            viridis::scale_color_viridis(discrete = TRUE)
  } else if (color_palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
    p <- p + ggplot2::scale_fill_brewer(palette = "Set1") +
            ggplot2::scale_color_brewer(palette = "Set1")
  }
  
  # Add rug plot if show_points is TRUE
  if (show_points) {
    p <- p + ggplot2::geom_rug(alpha = 0.3, sides = "b")
  }
  
  # Facet by metric if requested
  if (facet_by_metric) {
    p <- p + ggplot2::facet_wrap(~Metric, scales = "free")
  }
  
  return(p)
}

#' Create scatter plot of stability results
#' 
#' @param data Data frame with values for two metrics
#' @param metric1 Name of first metric
#' @param metric2 Name of second metric
#' @param title Plot title
#' @param color_palette Color palette
#' @return ggplot2 object with scatter plot
#' @export
create_stability_scatter <- function(data, metric1, metric2, title, color_palette) {
  # Create scatter plot
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = metric1, y = metric2)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red", alpha = 0.2) +
    ggplot2::labs(
      title = title,
      x = metric1,
      y = metric2
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  
  # Calculate correlation
  cor_val <- cor(data[[metric1]], data[[metric2]], use = "pairwise.complete.obs")
  p_val <- cor.test(data[[metric1]], data[[metric2]])$p.value
  
  # Add correlation as annotation
  p <- p + ggplot2::annotate(
    "text",
    x = min(data[[metric1]], na.rm = TRUE),
    y = max(data[[metric2]], na.rm = TRUE),
    label = sprintf("r = %.3f, p = %.3e", cor_val, p_val),
    hjust = 0,
    vjust = 1,
    size = 4
  )
  
  # Add density contours
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- p + ggplot2::geom_density_2d(alpha = 0.5)
  }
  
  return(p)
}

#' Create heatmap of stability coefficients
#' 
#' @param data Data frame with Metric and Coefficient columns
#' @param title Plot title
#' @param color_palette Color palette
#' @return ggplot2 object with heatmap
#' @export
create_stability_heatmap <- function(data, title, color_palette) {
  # Sort metrics by stability coefficient
  data <- data[order(data$Coefficient, decreasing = TRUE), ]
  
  # Convert to factors with specific order
  data$Metric <- factor(data$Metric, levels = data$Metric)
  
  # Create heatmap
  p <- ggplot2::ggplot(data, ggplot2::aes(x = "Stability", y = Metric, fill = Coefficient)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Coefficient)), color = "white") +
    ggplot2::labs(
      title = title,
      x = "",
      y = "Metric"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  
  # Add color palette
  if (color_palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
    p <- p + viridis::scale_fill_viridis()
  } else if (color_palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
    p <- p + ggplot2::scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(9, "YlOrRd"),
      limits = c(0, 1)
    )
  } else {
    p <- p + ggplot2::scale_fill_gradient(
      low = "blue",
      high = "red",
      limits = c(0, 1)
    )
  }
  
  return(p)
}

#' Create plot comparing stability across different methods
#' 
#' @param stability_results List of stability results for different methods
#' @param metric_name Name of the metric to compare
#' @param method_names Names of the methods
#' @param options Plot options
#' @return ggplot2 object with comparison plot
#' @export
compare_stability_methods <- function(stability_results, metric_name, 
                                    method_names = NULL, options = NULL) {
  # Check inputs
  if (!is.list(stability_results)) {
    stop("stability_results must be a list")
  }
  
  # Set default method names if not provided
  if (is.null(method_names)) {
    method_names <- names(stability_results)
    
    if (is.null(method_names)) {
      method_names <- paste("Method", 1:length(stability_results))
    }
  }
  
  # Extract options with defaults
  if (is.null(options)) {
    options <- list(
      title = paste("Stability Comparison:", metric_name),
      plot_type = "boxplot",
      color_palette = "viridis",
      show_points = TRUE,
      interactive = FALSE
    )
  }
  
  title <- ifelse(is.null(options$title), 
                 paste("Stability Comparison:", metric_name), options$title)
  plot_type <- ifelse(is.null(options$plot_type), "boxplot", options$plot_type)
  color_palette <- ifelse(is.null(options$color_palette), "viridis", options$color_palette)
  show_points <- ifelse(is.null(options$show_points), TRUE, options$show_points)
  interactive <- ifelse(is.null(options$interactive), FALSE, options$interactive)
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Method = character(0),
    Value = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(stability_results)) {
    result <- stability_results[[i]]
    method <- method_names[i]
    
    if (metric_name %in% names(result)) {
      values <- result[[metric_name]]
      
      if (length(values) > 0) {
        method_data <- data.frame(
          Method = rep(method, length(values)),
          Value = values,
          stringsAsFactors = FALSE
        )
        
        plot_data <- rbind(plot_data, method_data)
      }
    } else if ("stability_coefficients" %in% names(result) &&
              metric_name %in% names(result$stability_coefficients)) {
      # Extract stability coefficient
      value <- result$stability_coefficients[[metric_name]]
      
      method_data <- data.frame(
        Method = method,
        Value = value,
        stringsAsFactors = FALSE
      )
      
      plot_data <- rbind(plot_data, method_data)
    }
  }
  
  if (nrow(plot_data) == 0) {
    stop("No data found for metric: ", metric_name)
  }
  
  # Create plot based on type
  if (plot_type == "boxplot" && length(unique(plot_data$Value)) > 1) {
    # Boxplot is only meaningful if we have distributions
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Method, y = Value, fill = Method)) +
      ggplot2::geom_boxplot(alpha = 0.7) +
      ggplot2::labs(title = title, y = metric_name, x = "Method") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
    
    # Add individual points if requested
    if (show_points) {
      p <- p + ggplot2::geom_jitter(alpha = 0.3, width = 0.2, height = 0)
    }
  } else {
    # Bar plot for single values or if boxplot not applicable
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Method, y = Value, fill = Method)) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.7) +
      ggplot2::labs(title = title, y = metric_name, x = "Method") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
    
    # Add text labels
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", Value)),
      vjust = -0.5,
      size = 3.5
    )
  }
  
  # Add color palette
  if (color_palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
    p <- p + viridis::scale_fill_viridis(discrete = TRUE)
  } else if (color_palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
    p <- p + ggplot2::scale_fill_brewer(palette = "Set1")
  }
  
  # Make interactive if requested
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create a network-level stability visualization
#' 
#' @param bootstrap_results Bootstrap results
#' @param node_names Names of nodes/regions
#' @param metric Node-level metric to visualize
#' @param options Plot options
#' @return ggplot2 object with node stability visualization
#' @export
visualize_node_stability <- function(bootstrap_results, node_names, 
                                   metric = "centrality", options = NULL) {
  # Check inputs
  if (!is.list(bootstrap_results) || 
      !paste0("node_", metric) %in% names(bootstrap_results) ||
      is.null(bootstrap_results[[paste0("node_", metric)]])) {
    stop("Invalid bootstrap_results or metric not found")
  }
  
  node_values <- bootstrap_results[[paste0("node_", metric)]]
  
  if (!is.matrix(node_values)) {
    stop("Node metric values must be a matrix with nodes as columns")
  }
  
  # Set node names if not provided
  if (is.null(node_names)) {
    node_names <- colnames(node_values)
    
    if (is.null(node_names)) {
      node_names <- paste("Node", 1:ncol(node_values))
    }
  } else if (length(node_names) != ncol(node_values)) {
    stop("Length of node_names must match number of columns in node values matrix")
  }
  
  # Extract options with defaults
  if (is.null(options)) {
    options <- list(
      title = paste("Node", metric, "Stability"),
      plot_type = "boxplot",
      color_palette = "viridis",
      show_points = TRUE,
      sort_by_median = TRUE,
      interactive = FALSE
    )
  }
  
  title <- ifelse(is.null(options$title), 
                 paste("Node", metric, "Stability"), options$title)
  plot_type <- ifelse(is.null(options$plot_type), "boxplot", options$plot_type)
  color_palette <- ifelse(is.null(options$color_palette), "viridis", options$color_palette)
  show_points <- ifelse(is.null(options$show_points), TRUE, options$show_points)
  sort_by_median <- ifelse(is.null(options$sort_by_median), TRUE, options$sort_by_median)
  interactive <- ifelse(is.null(options$interactive), FALSE, options$interactive)
  
  # Prepare data for plotting
  plot_data <- data.frame(
    Node = character(0),
    Value = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:ncol(node_values)) {
    node_data <- data.frame(
      Node = rep(node_names[i], nrow(node_values)),
      Value = node_values[, i],
      stringsAsFactors = FALSE
    )
    
    plot_data <- rbind(plot_data, node_data)
  }
  
  # Sort nodes by median value if requested
  if (sort_by_median) {
    node_medians <- aggregate(Value ~ Node, data = plot_data, FUN = median)
    node_order <- node_medians$Node[order(node_medians$Value, decreasing = TRUE)]
    plot_data$Node <- factor(plot_data$Node, levels = node_order)
  }
  
  # Create plot based on type
  if (plot_type == "boxplot") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Node, y = Value, fill = Node)) +
      ggplot2::geom_boxplot(alpha = 0.7) +
      ggplot2::labs(title = title, y = paste(metric, "Value"), x = "Node") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
    
    # Add individual points if requested
    if (show_points) {
      p <- p + ggplot2::geom_jitter(alpha = 0.3, width = 0.2, height = 0)
    }
  } else if (plot_type == "heatmap") {
    # Calculate statistics for heatmap
    node_stats <- aggregate(Value ~ Node, data = plot_data, 
                          FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            ci_lower = quantile(x, 0.025),
                                            ci_upper = quantile(x, 0.975)))
    
    # Convert to data frame
    node_stats_df <- do.call(data.frame, node_stats)
    names(node_stats_df) <- c("Node", "Median", "SD", "CI_Lower", "CI_Upper")
    
    # Sort by median if requested
    if (sort_by_median) {
      node_stats_df <- node_stats_df[order(node_stats_df$Median, decreasing = TRUE), ]
      node_stats_df$Node <- factor(node_stats_df$Node, levels = node_stats_df$Node)
    }
    
    # Create heatmap
    p <- ggplot2::ggplot(node_stats_df, 
                        ggplot2::aes(x = "Stability", y = Node, fill = Median)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Median)), color = "white") +
      ggplot2::labs(
        title = title,
        x = "",
        y = "Node"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
    
    # Add color palette
    if (color_palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
      p <- p + viridis::scale_fill_viridis()
    } else if (color_palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
      p <- p + ggplot2::scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "YlOrRd"))
    } else {
      p <- p + ggplot2::scale_fill_gradient(low = "blue", high = "red")
    }
  } else if (plot_type == "interval") {
    # Calculate statistics for interval plot
    node_stats <- aggregate(Value ~ Node, data = plot_data, 
                          FUN = function(x) c(median = median(x), 
                                            sd = sd(x),
                                            ci_lower = quantile(x, 0.025),
                                            ci_upper = quantile(x, 0.975)))
    
    # Convert to data frame
    node_stats_df <- do.call(data.frame, node_stats)
    names(node_stats_df) <- c("Node", "Median", "SD", "CI_Lower", "CI_Upper")
    
    # Sort by median if requested
    if (sort_by_median) {
      node_stats_df <- node_stats_df[order(node_stats_df$Median, decreasing = TRUE), ]
      node_stats_df$Node <- factor(node_stats_df$Node, levels = node_stats_df$Node)
    }
    
    # Create interval plot
    p <- ggplot2::ggplot(node_stats_df, ggplot2::aes(x = Node, y = Median, color = Node)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = CI_Lower, ymax = CI_Upper),
        width = 0.2
      ) +
      ggplot2::labs(title = title, y = paste(metric, "Value"), x = "Node") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
    
    # Add color palette
    if (color_palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
      p <- p + viridis::scale_color_viridis(discrete = TRUE)
    } else if (color_palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
      p <- p + ggplot2::scale_color_brewer(palette = "Set1")
    }
  }
  
  # Make interactive if requested
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}
