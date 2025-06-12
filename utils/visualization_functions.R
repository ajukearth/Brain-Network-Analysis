#########################################################
# Enhanced Brain Network Analysis Shiny App
# utils/visualization_functions.R - Visualization utility functions
#########################################################

#' Create a network visualization
#' 
#' @param g An igraph network object
#' @param layout Layout algorithm to use
#' @param node_size Node attribute to determine size
#' @param node_color Node attribute to determine color
#' @param area_mapping List mapping regions to brain areas
#' @param area_colors Named vector of colors for brain areas
#' @return A ggplot2 object with the network visualization
#' @export
create_network_plot <- function(g, layout = "fr", 
                               node_size = "degree",
                               node_color = "area",
                               area_mapping = NULL,
                               area_colors = NULL) {
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' is required for this function")
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  # Add node size attribute if specified
  if (!is.null(node_size) && node_size != "none") {
    if (node_size == "degree") {
      igraph::V(g)$size <- igraph::degree(g)
    } else if (node_size == "betweenness") {
      igraph::V(g)$size <- igraph::betweenness(g, normalized = TRUE)
    } else if (node_size == "closeness") {
      igraph::V(g)$size <- igraph::closeness(g, normalized = TRUE)
    } else if (node_size == "eigenvector") {
      igraph::V(g)$size <- igraph::eigen_centrality(g)$vector
    } else if (node_size %in% igraph::vertex_attr_names(g)) {
      igraph::V(g)$size <- igraph::vertex_attr(g, node_size)
    } else {
      igraph::V(g)$size <- 3  # Default size
    }
    
    # Normalize size to a reasonable range (1-10)
    size_range <- range(igraph::V(g)$size, na.rm = TRUE)
    if (diff(size_range) > 0) {
      igraph::V(g)$size <- 1 + 9 * (igraph::V(g)$size - size_range[1]) / diff(size_range)
    } else {
      igraph::V(g)$size <- 5  # Default if all values are the same
    }
  } else {
    igraph::V(g)$size <- 5  # Default size
  }
  
  # Add node color attribute based on brain areas if specified
  if (!is.null(node_color) && node_color == "area" && !is.null(area_mapping)) {
    # Get node names
    node_names <- igraph::V(g)$name
    if (is.null(node_names)) {
      node_names <- paste0("Node", 1:igraph::vcount(g))
    }
    
    # Determine brain area for each node
    node_areas <- rep("Other", length(node_names))
    for (area in names(area_mapping)) {
      node_areas[node_names %in% area_mapping[[area]]] <- area
    }
    
    # Add area as vertex attribute
    igraph::V(g)$area <- node_areas
  }
  
  # Choose layout
  if (layout == "fr") {
    layout_func <- igraph::layout_with_fr
  } else if (layout == "kk") {
    layout_func <- igraph::layout_with_kk
  } else if (layout == "circle") {
    layout_func <- igraph::layout_in_circle
  } else if (layout == "tree") {
    layout_func <- igraph::layout_as_tree
  } else if (layout == "grid") {
    layout_func <- igraph::layout_on_grid
  } else if (layout == "mds") {
    layout_func <- igraph::layout_with_mds
  } else if (layout == "graphopt") {
    layout_func <- igraph::layout_with_graphopt
  } else if (layout == "drl") {
    layout_func <- igraph::layout_with_drl
  } else {
    # Default to Fruchterman-Reingold
    layout_func <- igraph::layout_with_fr
  }
  
  # Create plot
  p <- ggraph::ggraph(g, layout = layout_func)
  
  # Add edges with alpha proportional to weight
  if (igraph::ecount(g) > 0 && "weight" %in% igraph::edge_attr_names(g)) {
    # Normalize edge weights to [0.1, 1] for alpha
    weights <- igraph::E(g)$weight
    weight_range <- range(weights, na.rm = TRUE)
    if (diff(weight_range) > 0) {
      edge_alpha <- 0.1 + 0.9 * (weights - weight_range[1]) / diff(weight_range)
    } else {
      edge_alpha <- 0.5  # Default if all weights are the same
    }
    
    p <- p + ggraph::geom_edge_link(aes(alpha = edge_alpha, width = edge_alpha), 
                                   color = "grey50")
  } else {
    p <- p + ggraph::geom_edge_link(color = "grey50", alpha = 0.5)
  }
  
  # Add nodes
  if (!is.null(node_color) && node_color == "area" && !is.null(area_mapping)) {
    # Use colors based on brain areas
    p <- p + ggraph::geom_node_point(aes(size = size, color = area))
    
    # Set custom colors if provided
    if (!is.null(area_colors)) {
      unique_areas <- unique(igraph::V(g)$area)
      area_color_subset <- area_colors[names(area_colors) %in% unique_areas]
      
      # Add any missing areas with default colors
      missing_areas <- setdiff(unique_areas, names(area_color_subset))
      if (length(missing_areas) > 0) {
        default_colors <- scales::hue_pal()(length(missing_areas))
        names(default_colors) <- missing_areas
        area_color_subset <- c(area_color_subset, default_colors)
      }
      
      p <- p + scale_color_manual(values = area_color_subset)
    }
  } else {
    # Use default coloring
    p <- p + ggraph::geom_node_point(aes(size = size), color = "dodgerblue3")
  }
  
  # Add node labels if there are fewer than 30 nodes
  if (igraph::vcount(g) < 30) {
    p <- p + ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3)
  }
  
  # Add theme and labels
  p <- p + theme_void() +
    labs(title = "Network Visualization",
         subtitle = paste("Nodes:", igraph::vcount(g), "Edges:", igraph::ecount(g)),
         color = "Brain Area", size = "Centrality") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    ) +
    guides(size = guide_legend(title = node_size, order = 1),
           alpha = "none", width = "none")
  
  return(p)
}

#' Create a correlation heatmap
#' 
#' @param cor_matrix Correlation matrix
#' @param group Group name for the title
#' @param cluster_rows Whether to cluster rows
#' @param cluster_cols Whether to cluster columns
#' @param area_mapping List mapping regions to brain areas
#' @param area_colors Named vector of colors for brain areas
#' @return A ggplot2 object with the heatmap
#' @export
create_correlation_heatmap <- function(cor_matrix, 
                                      group = "All", 
                                      cluster_rows = TRUE,
                                      cluster_cols = TRUE,
                                      area_mapping = NULL,
                                      area_colors = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  # Melt the correlation matrix to long format
  cor_df <- reshape2::melt(cor_matrix, varnames = c("Region1", "Region2"), 
                           value.name = "Correlation")
  
  # Add brain area information if provided
  if (!is.null(area_mapping)) {
    # Function to map region to area
    get_area <- function(region) {
      for (area in names(area_mapping)) {
        if (region %in% area_mapping[[area]]) {
          return(area)
        }
      }
      return("Other")
    }
    
    # Add area columns
    cor_df$Area1 <- sapply(cor_df$Region1, get_area)
    cor_df$Area2 <- sapply(cor_df$Region2, get_area)
  }
  
  # Create a new order if clustering is requested
  if (cluster_rows || cluster_cols) {
    hc <- hclust(dist(cor_matrix))
    region_order <- rownames(cor_matrix)[hc$order]
    
    # Reorder factors in the data frame
    cor_df$Region1 <- factor(cor_df$Region1, levels = region_order)
    cor_df$Region2 <- factor(cor_df$Region2, levels = region_order)
  }
  
  # Create the heatmap
  p <- ggplot2::ggplot(cor_df, aes(x = Region2, y = Region1, fill = Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", 
      midpoint = 0, limits = c(-1, 1)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Correlation Heatmap -", group),
      x = "", y = ""
    ) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid = element_blank()
    )
  
  # Add brain area annotations if provided
  if (!is.null(area_mapping) && !is.null(area_colors)) {
    # Create annotation data frames
    region_order <- levels(cor_df$Region1)
    if (is.null(region_order)) {
      region_order <- unique(cor_df$Region1)
    }
    
    # Function to map region to area
    get_area <- function(region) {
      for (area in names(area_mapping)) {
        if (region %in% area_mapping[[area]]) {
          return(area)
        }
      }
      return("Other")
    }
    
    # Create annotation data frame
    x_annotation <- data.frame(
      Region = region_order,
      Area = sapply(region_order, get_area)
    )
    
    # Add annotation to the plot
    # This is a bit tricky in ggplot2, so we'll add colored strips at the top and left
    # First, create the top annotation
    top_annot <- ggplot2::ggplot(x_annotation, aes(x = Region, y = 1, fill = Area)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_manual(values = area_colors) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = element_blank()
      )
    
    # Create the left annotation
    left_annot <- ggplot2::ggplot(x_annotation, aes(x = 1, y = Region, fill = Area)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_manual(values = area_colors) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.y = element_blank()
      )
    
    # Combine the plots using cowplot or patchwork (if available)
    if (requireNamespace("cowplot", quietly = TRUE)) {
      p <- cowplot::plot_grid(
        top_annot, NULL, 
        left_annot, p, 
        ncol = 2, nrow = 2, 
        rel_widths = c(1, 0.05), 
        rel_heights = c(0.05, 1)
      )
    }
  }
  
  return(p)
}

#' Create a global metrics plot
#' 
#' @param global_metrics Data frame with global network metrics
#' @param metrics_to_plot Vector of metric names to include
#' @param group_colors Named vector of colors for groups
#' @return A ggplot2 object with the global metrics plot
#' @export
create_global_metrics_plot <- function(global_metrics, 
                                      metrics_to_plot = c("Density", "Global_Clustering", 
                                                         "Avg_Path_Length", "Modularity"),
                                      group_colors = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  # Check which metrics are available
  available_metrics <- intersect(metrics_to_plot, names(global_metrics))
  
  if (length(available_metrics) == 0) {
    warning("None of the requested metrics are available in the data")
    return(NULL)
  }
  
  # Select only the metrics we want to plot
  plot_data <- global_metrics[, c("Group", available_metrics)]
  
  # Reshape to long format
  plot_data <- reshape2::melt(plot_data, id.vars = "Group", 
                             variable.name = "Metric", value.name = "Value")
  
  # Create the plot
  p <- ggplot2::ggplot(plot_data, aes(x = Metric, y = Value, fill = Group)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Global Network Metrics",
      x = "", y = "Value"
    ) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  # Set custom colors if provided
  if (!is.null(group_colors)) {
    unique_groups <- unique(plot_data$Group)
    group_color_subset <- group_colors[names(group_colors) %in% unique_groups]
    
    # Add any missing groups with default colors
    missing_groups <- setdiff(unique_groups, names(group_color_subset))
    if (length(missing_groups) > 0) {
      default_colors <- scales::hue_pal()(length(missing_groups))
      names(default_colors) <- missing_groups
      group_color_subset <- c(group_color_subset, default_colors)
    }
    
    p <- p + scale_fill_manual(values = group_color_subset)
  }
  
  return(p)
}

#' Create a node metrics plot
#' 
#' @param node_metrics Data frame with node-level metrics
#' @param metric Name of the metric to plot
#' @param group_column Name of the group column
#' @param top_n Number of top nodes to show (NULL for all)
#' @param area_mapping List mapping regions to brain areas
#' @param area_colors Named vector of colors for brain areas
#' @return A ggplot2 object with the node metrics plot
#' @export
create_node_metrics_plot <- function(node_metrics, 
                                    metric = "Degree", 
                                    group_column = "Group",
                                    top_n = 10,
                                    area_mapping = NULL,
                                    area_colors = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  # Check if the metric is available
  if (!metric %in% names(node_metrics)) {
    warning(paste("Metric", metric, "not found in node metrics data"))
    return(NULL)
  }
  
  # Add brain area information if provided
  if (!is.null(area_mapping)) {
    # Function to map region to area
    get_area <- function(region) {
      for (area in names(area_mapping)) {
        if (region %in% area_mapping[[area]]) {
          return(area)
        }
      }
      return("Other")
    }
    
    # Add area column
    node_metrics$Area <- sapply(node_metrics$Node, get_area)
  } else {
    # Default area
    node_metrics$Area <- "Other"
  }
  
  # Select top nodes if specified
  if (!is.null(top_n) && top_n > 0 && top_n < nrow(node_metrics)) {
    # For each group, select top nodes
    unique_groups <- unique(node_metrics[[group_column]])
    top_nodes <- data.frame()
    
    for (group in unique_groups) {
      group_data <- node_metrics[node_metrics[[group_column]] == group, ]
      group_data <- group_data[order(group_data[[metric]], decreasing = TRUE), ]
      group_data <- group_data[1:min(top_n, nrow(group_data)), ]
      top_nodes <- rbind(top_nodes, group_data)
    }
    
    plot_data <- top_nodes
  } else {
    plot_data <- node_metrics
  }
  
  # Sort by metric value
  plot_data <- plot_data[order(plot_data[[metric]], decreasing = TRUE), ]
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, aes(x = reorder(Node, -get(metric)), 
                                     y = get(metric), fill = Area)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(metric, "by Node"),
      x = "Node", y = metric
    ) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  # If we have multiple groups, facet by group
  if (length(unique(plot_data[[group_column]])) > 1) {
    p <- p + facet_wrap(as.formula(paste("~", group_column)), scales = "free_x")
  }
  
  # Set custom colors if provided
  if (!is.null(area_colors)) {
    unique_areas <- unique(plot_data$Area)
    area_color_subset <- area_colors[names(area_colors) %in% unique_areas]
    
    # Add any missing areas with default colors
    missing_areas <- setdiff(unique_areas, names(area_color_subset))
    if (length(missing_areas) > 0) {
      default_colors <- scales::hue_pal()(length(missing_areas))
      names(default_colors) <- missing_areas
      area_color_subset <- c(area_color_subset, default_colors)
    }
    
    p <- p + scale_fill_manual(values = area_color_subset)
  }
  
  return(p)
}

#' Create an inter-area connectivity heatmap
#' 
#' @param connectivity_data Data frame with inter-area connectivity data
#' @param group Group to display (or "All" for all groups)
#' @param area_colors Named vector of colors for brain areas
#' @return A ggplot2 object with the connectivity heatmap
#' @export
create_inter_area_heatmap <- function(connectivity_data, 
                                     group = "All",
                                     area_colors = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  # Filter by group if specified
  if (group != "All" && "Group" %in% names(connectivity_data)) {
    plot_data <- connectivity_data[connectivity_data$Group == group, ]
  } else {
    plot_data <- connectivity_data
  }
  
  # Create the heatmap
  p <- ggplot2::ggplot(plot_data, aes(x = Area2, y = Area1, fill = Connectivity)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(
      low = "white", high = "darkred", 
      limits = c(0, max(plot_data$Connectivity, na.rm = TRUE))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Inter-Area Connectivity -", group),
      x = "", y = ""
    ) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid = element_blank()
    )
  
  return(p)
}

#' Safely save plots to a file
#' 
#' @param plot ggplot2 object to save
#' @param filename Output filename
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution in DPI
#' @return Invisibly returns TRUE if successful, FALSE if not
#' @export
save_plot_safely <- function(plot, filename, width = 10, height = 8, dpi = 300) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Package 'ggplot2' is required for this function")
    return(invisible(FALSE))
  }
  
  tryCatch({
    ggplot2::ggsave(filename, plot, width = width, height = height, dpi = dpi)
    return(invisible(TRUE))
  }, error = function(e) {
    warning(paste("Error saving plot:", e$message))
    return(invisible(FALSE))
  })
}
