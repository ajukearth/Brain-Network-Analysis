#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/advanced/fingerprinting.R - Individual fingerprinting
#########################################################

#' Calculate individual fingerprinting metrics
#' 
#' @param correlation_matrices List of correlation matrices from multiple sessions/scans
#' @param ids Vector of subject IDs corresponding to the matrices
#' @return List containing fingerprinting results
#' @export
calculate_fingerprinting <- function(correlation_matrices, ids) {
  # Check inputs
  if (length(correlation_matrices) == 0) {
    stop("No correlation matrices provided")
  }
  
  if (length(correlation_matrices) != length(ids)) {
    stop("Number of matrices must match number of IDs")
  }
  
  # Initialize results
  results <- list(
    identifiability = NULL,
    within_subject = list(),
    between_subject = list(),
    discriminability = NULL,
    edge_importance = NULL
  )
  
  # Extract unique subject IDs
  unique_ids <- unique(ids)
  n_subjects <- length(unique_ids)
  
  # Check if there are multiple sessions per subject
  has_multiple_sessions <- any(table(ids) > 1)
  
  if (!has_multiple_sessions) {
    warning("Fingerprinting requires multiple sessions per subject. Limited analysis will be performed.")
    
    # Calculate similarity between all pairs of matrices
    n_matrices <- length(correlation_matrices)
    similarity_matrix <- matrix(NA, nrow = n_matrices, ncol = n_matrices)
    
    for (i in 1:n_matrices) {
      for (j in 1:n_matrices) {
        # Extract upper triangular elements of each matrix
        vec_i <- correlation_matrices[[i]][upper.tri(correlation_matrices[[i]])]
        vec_j <- correlation_matrices[[j]][upper.tri(correlation_matrices[[j]])]
        
        # Calculate correlation
        similarity_matrix[i, j] <- cor(vec_i, vec_j, method = "pearson")
      }
    }
    
    # Set row and column names
    rownames(similarity_matrix) <- ids
    colnames(similarity_matrix) <- ids
    
    # Store results
    results$similarity_matrix <- similarity_matrix
    
    return(results)
  }
  
  # Calculate within-subject and between-subject similarities
  within_subject_sim <- list()
  between_subject_sim <- list()
  
  for (i in 1:n_subjects) {
    subject_id <- unique_ids[i]
    
    # Get indices of matrices for this subject
    subject_indices <- which(ids == subject_id)
    
    if (length(subject_indices) > 1) {
      # Calculate within-subject similarities
      n_sessions <- length(subject_indices)
      subject_sim <- numeric()
      
      for (j in 1:(n_sessions-1)) {
        for (k in (j+1):n_sessions) {
          # Extract upper triangular elements of each matrix
          vec_j <- correlation_matrices[[subject_indices[j]]][upper.tri(correlation_matrices[[subject_indices[j]]])]
          vec_k <- correlation_matrices[[subject_indices[k]]][upper.tri(correlation_matrices[[subject_indices[k]]])]
          
          # Calculate correlation
          similarity <- cor(vec_j, vec_k, method = "pearson")
          subject_sim <- c(subject_sim, similarity)
        }
      }
      
      within_subject_sim[[subject_id]] <- subject_sim
    }
    
    # Calculate between-subject similarities
    for (j in 1:n_subjects) {
      if (i != j) {
        other_id <- unique_ids[j]
        other_indices <- which(ids == other_id)
        
        between_sim <- numeric()
        
        for (k in subject_indices) {
          for (l in other_indices) {
            # Extract upper triangular elements of each matrix
            vec_k <- correlation_matrices[[k]][upper.tri(correlation_matrices[[k]])]
            vec_l <- correlation_matrices[[l]][upper.tri(correlation_matrices[[l]])]
            
            # Calculate correlation
            similarity <- cor(vec_k, vec_l, method = "pearson")
            between_sim <- c(between_sim, similarity)
          }
        }
        
        between_subject_sim[[paste(subject_id, other_id, sep = "-")]] <- between_sim
      }
    }
  }
  
  # Calculate overall within-subject and between-subject similarities
  all_within <- unlist(within_subject_sim)
  all_between <- unlist(between_subject_sim)
  
  # Calculate identifiability
  within_mean <- mean(all_within)
  between_mean <- mean(all_between)
  pooled_sd <- sqrt(((length(all_within) - 1) * var(all_within) + 
                     (length(all_between) - 1) * var(all_between)) / 
                     (length(all_within) + length(all_between) - 2))
  
  identifiability <- (within_mean - between_mean) / pooled_sd
  
  # Store results
  results$within_subject <- within_subject_sim
  results$between_subject <- between_subject_sim
  results$identifiability <- identifiability
  results$within_mean <- within_mean
  results$between_mean <- between_mean
  results$pooled_sd <- pooled_sd
  
  # Calculate discriminability (differential identifiability)
  # This is the percentage of subjects with higher within-subject than between-subject similarity
  discriminability <- numeric(n_subjects)
  
  for (i in 1:n_subjects) {
    subject_id <- unique_ids[i]
    
    # Get within-subject similarity
    if (!is.null(within_subject_sim[[subject_id]])) {
      within_sim <- mean(within_subject_sim[[subject_id]])
      
      # Get all between-subject similarities involving this subject
      between_sim_keys <- grep(paste0("^", subject_id, "-|", "-", subject_id, "$"), 
                              names(between_subject_sim), value = TRUE)
      between_sim <- mean(unlist(between_subject_sim[between_sim_keys]))
      
      # Check if within-subject similarity is higher
      discriminability[i] <- within_sim > between_sim
    } else {
      discriminability[i] <- NA
    }
  }
  
  # Store discriminability
  results$discriminability <- mean(discriminability, na.rm = TRUE)
  results$subject_discriminability <- setNames(discriminability, unique_ids)
  
  # Calculate edge importance for fingerprinting
  # This identifies which connections are most important for individual identification
  
  # Combine all matrices into a 3D array
  n_regions <- nrow(correlation_matrices[[1]])
  all_matrices <- array(NA, dim = c(n_regions, n_regions, length(correlation_matrices)))
  
  for (i in 1:length(correlation_matrices)) {
    all_matrices[, , i] <- correlation_matrices[[i]]
  }
  
  # Calculate edge importance
  edge_importance <- matrix(0, nrow = n_regions, ncol = n_regions)
  
  # For each edge (i, j), calculate the ratio of between-subject to within-subject variance
  for (i in 1:(n_regions-1)) {
    for (j in (i+1):n_regions) {
      # Extract edge values for all matrices
      edge_values <- all_matrices[i, j, ]
      
      # Calculate within-subject and between-subject variance
      within_var <- 0
      between_var <- 0
      
      for (k in 1:n_subjects) {
        subject_id <- unique_ids[k]
        subject_indices <- which(ids == subject_id)
        
        if (length(subject_indices) > 1) {
          # Calculate within-subject variance
          subject_values <- edge_values[subject_indices]
          within_var <- within_var + sum((subject_values - mean(subject_values))^2)
          
          # Calculate between-subject variance
          between_var <- between_var + length(subject_indices) * 
            (mean(subject_values) - mean(edge_values))^2
        }
      }
      
      # Calculate F-statistic (ratio of between to within variance)
      if (within_var > 0) {
        edge_importance[i, j] <- edge_importance[j, i] <- between_var / within_var
      }
    }
  }
  
  # Store edge importance
  results$edge_importance <- edge_importance
  
  return(results)
}

#' Generate differential identifiability plot
#' 
#' @param fingerprinting_results Results from calculate_fingerprinting function
#' @return ggplot2 object with differential identifiability plot
#' @export
generate_identifiability_plot <- function(fingerprinting_results) {
  # Check if fingerprinting results are available
  if (is.null(fingerprinting_results)) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0.5, y = 0.5, 
                            label = "No fingerprinting results available") +
           ggplot2::theme_void() +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
  }
  
  # Create data frame for plotting
  within_mean <- fingerprinting_results$within_mean
  between_mean <- fingerprinting_results$between_mean
  
  if (is.null(within_mean) || is.null(between_mean)) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0.5, y = 0.5, 
                            label = "Insufficient data for identifiability plot") +
           ggplot2::theme_void() +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
  }
  
  # Create data frame with all similarity values
  all_within <- unlist(fingerprinting_results$within_subject)
  all_between <- unlist(fingerprinting_results$between_subject)
  
  plot_data <- data.frame(
    Similarity = c(all_within, all_between),
    Type = c(rep("Within-Subject", length(all_within)),
             rep("Between-Subject", length(all_between))),
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Similarity, fill = Type)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::geom_vline(xintercept = within_mean, 
                      color = "#4DAF4A", linetype = "dashed", size = 1) +
    ggplot2::geom_vline(xintercept = between_mean, 
                      color = "#E41A1C", linetype = "dashed", size = 1) +
    ggplot2::scale_fill_manual(values = c("Within-Subject" = "#4DAF4A", 
                                        "Between-Subject" = "#E41A1C")) +
    ggplot2::annotate("text", x = within_mean + 0.05, y = 0, 
                    label = paste("Within Mean =", round(within_mean, 3)),
                    hjust = 0, color = "#4DAF4A") +
    ggplot2::annotate("text", x = between_mean + 0.05, y = 0.5, 
                    label = paste("Between Mean =", round(between_mean, 3)),
                    hjust = 0, color = "#E41A1C") +
    ggplot2::annotate("text", x = 0.5, y = 5, 
                    label = paste("Identifiability =", 
                                  round(fingerprinting_results$identifiability, 3)),
                    hjust = 0.5, size = 5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Differential Identifiability",
      x = "Similarity (Correlation)",
      y = "Density",
      fill = "Similarity Type"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Generate edge importance plot
#' 
#' @param fingerprinting_results Results from calculate_fingerprinting function
#' @param region_names Vector of region names
#' @param top_n Number of top edges to highlight
#' @return plotly object with edge importance plot
#' @export
generate_edge_importance_plot <- function(fingerprinting_results, region_names = NULL, top_n = 20) {
  # Check if fingerprinting results are available
  if (is.null(fingerprinting_results) || is.null(fingerprinting_results$edge_importance)) {
    # Return empty plot with message
    return(plotly::plot_ly() %>%
           plotly::add_annotations(
             text = "No edge importance data available",
             showarrow = FALSE,
             font = list(size = 20)
           ))
  }
  
  # Get edge importance matrix
  edge_importance <- fingerprinting_results$edge_importance
  
  # If region names not provided, create default names
  if (is.null(region_names)) {
    region_names <- paste0("Region", 1:nrow(edge_importance))
  }
  
  # Set row and column names
  rownames(edge_importance) <- region_names
  colnames(edge_importance) <- region_names
  
  # Create heatmap
  p <- heatmaply::heatmaply(
    edge_importance,
    dendrogram = "both",
    main = "Edge Importance for Fingerprinting",
    xlab = "",
    ylab = "",
    colors = viridis::viridis(100),
    node_type = "scatter",
    grid_color = "gray",
    grid_width = 0.00001,
    fontsize_row = 10,
    fontsize_col = 10
  )
  
  # Highlight top edges in a separate plot
  if (top_n > 0) {
    # Extract upper triangular part of the matrix
    upper_tri <- edge_importance
    upper_tri[lower.tri(upper_tri)] <- NA
    
    # Flatten and sort to find top edges
    edge_values <- upper_tri
    edge_values <- as.vector(edge_values)
    edge_values <- edge_values[!is.na(edge_values)]
    
    if (length(edge_values) > 0) {
      # Sort and get top n values
      top_values <- sort(edge_values, decreasing = TRUE)[1:min(top_n, length(edge_values))]
      
      # Create data frame for top edges
      top_edges <- data.frame(
        Region1 = character(),
        Region2 = character(),
        Importance = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Find the regions corresponding to top values
      for (val in top_values) {
        indices <- which(edge_importance == val, arr.ind = TRUE)
        
        for (i in 1:nrow(indices)) {
          row_idx <- indices[i, 1]
          col_idx <- indices[i, 2]
          
          if (row_idx < col_idx) {  # Only include upper triangular part
            top_edges <- rbind(top_edges, data.frame(
              Region1 = region_names[row_idx],
              Region2 = region_names[col_idx],
              Importance = val,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Remove duplicates if any
      top_edges <- unique(top_edges)
      
      # Create bar plot for top edges
      if (nrow(top_edges) > 0) {
        # Create labels for edges
        top_edges$Label <- paste(top_edges$Region1, "-", top_edges$Region2)
        
        # Sort by importance
        top_edges <- top_edges[order(top_edges$Importance, decreasing = TRUE), ]
        
        # Limit to top_n if necessary
        if (nrow(top_edges) > top_n) {
          top_edges <- top_edges[1:top_n, ]
        }
        
        # Reverse order for plotting
        top_edges$Label <- factor(top_edges$Label, levels = rev(top_edges$Label))
        
        # Create bar plot
        p_bar <- ggplot2::ggplot(top_edges, ggplot2::aes(x = Label, y = Importance)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = paste("Top", nrow(top_edges), "Edges for Fingerprinting"),
            x = "Edge",
            y = "Importance (F-statistic)"
          ) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            axis.text.y = ggplot2::element_text(size = 10)
          )
        
        # Convert to plotly
        p_bar <- plotly::ggplotly(p_bar)
        
        # Return both plots in a list
        return(list(heatmap = p, barplot = p_bar))
      }
    }
  }
  
  # If no top edges or not requested, just return the heatmap
  return(p)
}
