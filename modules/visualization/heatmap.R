#########################################################
# Enhanced Brain Network Analysis Shiny App
# Visualization/heatmap.R - Correlation heatmap functions
#########################################################

#' Create a correlation heatmap for brain regions
#' 
#' @param correlation_matrix Correlation matrix to visualize
#' @param options List of visualization options:
#'   - title: Plot title
#'   - color_scheme: Color scheme ("divergent", "sequential", "custom")
#'   - colors: Custom color palette
#'   - cluster: Cluster method for rows/columns ("none", "hierarchical", "kmeans")
#'   - cluster_k: Number of clusters for k-means
#'   - display_values: Whether to display correlation values in cells
#'   - upper_triangle: Whether to display only upper triangle
#'   - lower_triangle: Whether to display only lower triangle
#'   - interactive: Whether to create an interactive plot
#'   - annotation: Additional annotation data frame
#'   - annotation_colors: Colors for annotation categories
#' @return ggplot2 or plotly object with heatmap
#' @export
create_correlation_heatmap <- function(correlation_matrix, options = NULL) {
  # Check input
  if (!is.matrix(correlation_matrix) && !is.data.frame(correlation_matrix)) {
    stop("Input must be a matrix or data frame")
  }
  
  # Convert to matrix if necessary
  if (is.data.frame(correlation_matrix)) {
    correlation_matrix <- as.matrix(correlation_matrix)
  }
  
  # Default options
  if (is.null(options)) {
    options <- list(
      title = "Correlation Heatmap",
      color_scheme = "divergent",
      colors = NULL,
      cluster = "hierarchical",
      cluster_k = 3,
      display_values = FALSE,
      upper_triangle = FALSE,
      lower_triangle = FALSE,
      interactive = FALSE,
      annotation = NULL,
      annotation_colors = NULL
    )
  }
  
  # Extract options with defaults
  title <- ifelse(is.null(options$title), "Correlation Heatmap", options$title)
  color_scheme <- ifelse(is.null(options$color_scheme), "divergent", options$color_scheme)
  cluster <- ifelse(is.null(options$cluster), "hierarchical", options$cluster)
  cluster_k <- ifelse(is.null(options$cluster_k), 3, options$cluster_k)
  display_values <- ifelse(is.null(options$display_values), FALSE, options$display_values)
  upper_triangle <- ifelse(is.null(options$upper_triangle), FALSE, options$upper_triangle)
  lower_triangle <- ifelse(is.null(options$lower_triangle), FALSE, options$lower_triangle)
  interactive <- ifelse(is.null(options$interactive), FALSE, options$interactive)
  
  # Define color palette
  if (is.null(options$colors)) {
    if (color_scheme == "divergent") {
      colors <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", 
                 "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
    } else if (color_scheme == "sequential") {
      colors <- c("#FFF7FB", "#ECE2F0", "#D0D1E6", "#A6BDDB", "#67A9CF", 
                 "#3690C0", "#02818A", "#016C59", "#014636")
    } else {
      # Default to divergent
      colors <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", 
                 "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
    }
  } else {
    colors <- options$colors
  }
  
  # Apply clustering if requested
  if (cluster != "none") {
    if (cluster == "hierarchical") {
      # Calculate distance matrix
      dist_matrix <- as.dist(1 - abs(correlation_matrix))
      
      # Hierarchical clustering
      hc <- hclust(dist_matrix, method = "ward.D2")
      
      # Reorder matrix
      order <- hc$order
      correlation_matrix <- correlation_matrix[order, order]
      
      # Store clustering for later use
      attr(correlation_matrix, "clustering") <- hc
    } else if (cluster == "kmeans") {
      if (requireNamespace("stats", quietly = TRUE)) {
        # Convert correlation matrix to distance matrix
        dist_matrix <- as.dist(1 - abs(correlation_matrix))
        
        # Apply k-means
        km <- stats::kmeans(cmdscale(dist_matrix, k = 2), centers = cluster_k)
        
        # Reorder matrix by cluster
        order <- order(km$cluster)
        correlation_matrix <- correlation_matrix[order, order]
        
        # Store clustering for later use
        attr(correlation_matrix, "clustering") <- km
      }
    }
  }
  
  # Apply triangle masking if requested
  if (upper_triangle) {
    correlation_matrix[lower.tri(correlation_matrix)] <- NA
  } else if (lower_triangle) {
    correlation_matrix[upper.tri(correlation_matrix)] <- NA
  }
  
  # Create heatmap using different methods based on interactivity and package availability
  if (interactive) {
    # Create interactive heatmap using plotly or heatmaply
    if (requireNamespace("plotly", quietly = TRUE)) {
      # Create data frame for plotly
      melted_matrix <- reshape2::melt(correlation_matrix, na.rm = TRUE)
      names(melted_matrix) <- c("Var1", "Var2", "value")
      
      # Create plotly heatmap
      p <- plotly::plot_ly(
        x = colnames(correlation_matrix),
        y = rownames(correlation_matrix),
        z = correlation_matrix,
        type = "heatmap",
        colorscale = plotly::colorscale_gradient(colors),
        zmin = -1,
        zmax = 1
      ) %>%
        plotly::layout(
          title = title,
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
      
      # Display values if requested
      if (display_values) {
        # Add text for correlation values
        p <- p %>% plotly::add_annotations(
          data = melted_matrix,
          x = ~Var2,
          y = ~Var1,
          text = ~round(value, 2),
          showarrow = FALSE,
          font = list(size = 8)
        )
      }
      
      return(p)
    } else if (requireNamespace("heatmaply", quietly = TRUE)) {
      # Create heatmaply heatmap
      p <- heatmaply::heatmaply(
        correlation_matrix,
        colors = colors,
        limits = c(-1, 1),
        dendrogram = ifelse(cluster == "hierarchical", "both", "none"),
        show_grid = TRUE,
        fontsize_row = 8,
        fontsize_col = 8,
        main = title,
        showticklabels = c(TRUE, TRUE)
      )
      
      return(p)
    } else {
      warning("Neither 'plotly' nor 'heatmaply' available. Creating static heatmap.")
      interactive <- FALSE
    }
  }
  
  # Create static heatmap with ggplot2
  if (!interactive) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      # Create data frame for ggplot
      melted_matrix <- reshape2::melt(correlation_matrix, na.rm = TRUE)
      names(melted_matrix) <- c("Var1", "Var2", "value")
      
      # Create annotation data frame if provided
      if (!is.null(options$annotation)) {
        annotation_df <- data.frame(
          Var1 = rownames(correlation_matrix),
          options$annotation,
          stringsAsFactors = FALSE
        )
        melted_matrix <- merge(melted_matrix, annotation_df, by = "Var1")
      }
      
      # Create ggplot heatmap
      p <- ggplot2::ggplot(melted_matrix, ggplot2::aes(x = Var2, y = Var1, fill = value)) +
        ggplot2::geom_tile(color = "white", size = 0.1) +
        ggplot2::scale_fill_gradientn(
          colors = colors,
          limits = c(-1, 1),
          na.value = "white"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = title,
          x = "",
          y = "",
          fill = "Correlation"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = ggplot2::element_text(size = 8),
          plot.title = ggplot2::element_text(hjust = 0.5),
          panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::coord_fixed()
      
      # Display values if requested
      if (display_values) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = round(value, 2)),
          size = 2
        )
      }
      
      # Add annotation if provided
      if (!is.null(options$annotation) && ncol(options$annotation) > 0) {
        first_anno_col <- names(options$annotation)[1]
        
        # Add annotation bar
        p <- p + ggplot2::facet_grid(
          reformulate(".", first_anno_col),
          scales = "free",
          space = "free"
        )
      }
      
      return(p)
    } else {
      warning("Package 'ggplot2' not available. Using base R heatmap.")
      
      # Create base R heatmap
      heatmap(
        correlation_matrix,
        col = grDevices::colorRampPalette(colors)(100),
        symm = TRUE,
        scale = "none",
        main = title,
        margins = c(10, 10)
      )
      
      return(invisible(correlation_matrix))
    }
  }
}

#' Create a difference heatmap between two correlation matrices
#' 
#' @param matrix1 First correlation matrix
#' @param matrix2 Second correlation matrix
#' @param labels Labels for the matrices
#' @param options Visualization options (see create_correlation_heatmap)
#' @return ggplot2 or plotly object with difference heatmap
#' @export
create_difference_heatmap <- function(matrix1, matrix2, labels = c("Matrix 1", "Matrix 2"),
                                     options = NULL) {
  # Check inputs
  if (!is.matrix(matrix1) || !is.matrix(matrix2)) {
    stop("Inputs must be matrices")
  }
  
  if (nrow(matrix1) != nrow(matrix2) || ncol(matrix1) != ncol(matrix2)) {
    stop("Matrices must have the same dimensions")
  }
  
  # Calculate difference matrix
  diff_matrix <- matrix1 - matrix2
  
  # Set default options if not provided
  if (is.null(options)) {
    options <- list(
      title = paste0("Difference Heatmap (", labels[1], " - ", labels[2], ")"),
      color_scheme = "divergent",
      colors = c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", 
                "#F7F7F7", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"),
      cluster = "hierarchical",
      display_values = FALSE,
      interactive = FALSE
    )
  } else {
    # Ensure title is set
    if (is.null(options$title)) {
      options$title <- paste0("Difference Heatmap (", labels[1], " - ", labels[2], ")")
    }
  }
  
  # Create heatmap
  return(create_correlation_heatmap(diff_matrix, options))
}

#' Create a network-structured heatmap with brain regions grouped by network
#' 
#' @param correlation_matrix Correlation matrix
#' @param network_assignments List or data frame with network assignments
#' @param options Visualization options (see create_correlation_heatmap)
#' @return ggplot2 or plotly object with network-structured heatmap
#' @export
create_network_heatmap <- function(correlation_matrix, network_assignments, options = NULL) {
  # Check inputs
  if (!is.matrix(correlation_matrix) && !is.data.frame(correlation_matrix)) {
    stop("correlation_matrix must be a matrix or data frame")
  }
  
  # Convert to matrix if necessary
  if (is.data.frame(correlation_matrix)) {
    correlation_matrix <- as.matrix(correlation_matrix)
  }
  
  # Process network assignments
  if (is.data.frame(network_assignments)) {
    # Extract network column
    if (!"Network" %in% names(network_assignments)) {
      stop("network_assignments data frame must contain a 'Network' column")
    }
    
    # Match region names
    region_col <- NULL
    for (col in names(network_assignments)) {
      if (all(rownames(correlation_matrix) %in% network_assignments[[col]])) {
        region_col <- col
        break
      }
    }
    
    if (is.null(region_col)) {
      stop("Could not find region identifier column in network_assignments")
    }
    
    # Create named vector
    network_vector <- network_assignments$Network
    names(network_vector) <- network_assignments[[region_col]]
    
    # Filter to regions in correlation matrix
    network_vector <- network_vector[names(network_vector) %in% rownames(correlation_matrix)]
  } else if (is.vector(network_assignments) && !is.null(names(network_assignments))) {
    network_vector <- network_assignments
  } else {
    stop("network_assignments must be a data frame with 'Network' column or a named vector")
  }
  
  # Check if all regions are covered
  missing_regions <- setdiff(rownames(correlation_matrix), names(network_vector))
  if (length(missing_regions) > 0) {
    warning("Some regions don't have network assignments: ", 
           paste(missing_regions[1:min(5, length(missing_regions))], collapse = ", "), 
           ifelse(length(missing_regions) > 5, "...", ""))
    
    # Assign missing regions to "Unknown" network
    for (region in missing_regions) {
      network_vector[region] <- "Unknown"
    }
  }
  
  # Order regions by network
  networks <- unique(network_vector)
  ordered_regions <- c()
  
  for (network in networks) {
    network_regions <- names(network_vector)[network_vector == network]
    ordered_regions <- c(ordered_regions, network_regions)
  }
  
  # Reorder correlation matrix
  correlation_matrix <- correlation_matrix[ordered_regions, ordered_regions]
  
  # Create annotation data frame
  annotation_df <- data.frame(
    Region = ordered_regions,
    Network = network_vector[ordered_regions],
    stringsAsFactors = FALSE
  )
  
  # Set default options if not provided
  if (is.null(options)) {
    options <- list(
      title = "Network-Structured Correlation Heatmap",
      color_scheme = "divergent",
      cluster = "none",  # No clustering since we're using network structure
      display_values = FALSE,
      annotation = data.frame(Network = annotation_df$Network)
    )
  } else {
    # Override clustering and add annotation
    options$cluster <- "none"
    options$annotation <- data.frame(Network = annotation_df$Network)
  }
  
  # Create heatmap
  return(create_correlation_heatmap(correlation_matrix, options))
}

#' Create a multi-panel comparison of correlation heatmaps
#' 
#' @param matrix_list List of correlation matrices
#' @param labels Labels for each matrix
#' @param layout Layout of panels (rows, cols)
#' @param common_scale Whether to use common color scale
#' @param options Visualization options (see create_correlation_heatmap)
#' @return ggplot2 object with multiple heatmaps
#' @export
create_multi_heatmap <- function(matrix_list, labels = NULL, layout = NULL, 
                               common_scale = TRUE, options = NULL) {
  # Check inputs
  if (!is.list(matrix_list)) {
    stop("matrix_list must be a list of matrices")
  }
  
  n_matrices <- length(matrix_list)
  
  if (n_matrices == 0) {
    stop("matrix_list is empty")
  }
  
  # Set default labels if not provided
  if (is.null(labels)) {
    labels <- paste("Matrix", 1:n_matrices)
  } else if (length(labels) != n_matrices) {
    warning("Number of labels doesn't match number of matrices. Using default labels.")
    labels <- paste("Matrix", 1:n_matrices)
  }
  
  # Determine layout if not provided
  if (is.null(layout)) {
    # Calculate default layout (try to make it square-ish)
    n_cols <- ceiling(sqrt(n_matrices))
    n_rows <- ceiling(n_matrices / n_cols)
    layout <- c(n_rows, n_cols)
  }
  
  # Check if patchwork is available for combining plots
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    warning("Package 'patchwork' not available. Returning list of individual heatmaps.")
    
    # Create individual heatmaps
    heatmap_list <- list()
    
    for (i in 1:n_matrices) {
      # Set title from labels
      if (is.null(options)) {
        plot_options <- list(title = labels[i])
      } else {
        plot_options <- options
        plot_options$title <- labels[i]
      }
      
      # Create heatmap
      heatmap_list[[i]] <- create_correlation_heatmap(matrix_list[[i]], plot_options)
    }
    
    return(heatmap_list)
  }
  
  # Create individual heatmaps with patchwork
  heatmap_plots <- list()
  
  # Determine global min and max for common scale
  if (common_scale) {
    global_min <- min(sapply(matrix_list, min, na.rm = TRUE))
    global_max <- max(sapply(matrix_list, max, na.rm = TRUE))
  }
  
  for (i in 1:n_matrices) {
    # Set title from labels
    if (is.null(options)) {
      plot_options <- list(title = labels[i])
    } else {
      plot_options <- options
      plot_options$title <- labels[i]
    }
    
    # Set common scale if requested
    if (common_scale) {
      if (is.null(plot_options$scale_limits)) {
        plot_options$scale_limits <- c(global_min, global_max)
      }
    }
    
    # Create heatmap
    heatmap_plots[[i]] <- create_correlation_heatmap(matrix_list[[i]], plot_options)
  }
  
  # Combine plots with patchwork
  combined_plot <- patchwork::wrap_plots(heatmap_plots, ncol = layout[2])
  
  return(combined_plot)
}
