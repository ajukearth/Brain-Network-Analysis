
#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/visualization/network_plot.R - Network plotting functions
#########################################################

#' Generate an interactive network plot
#' 
#' @param graph igraph object to plot
#' @param layout Layout algorithm to use
#' @param color_by Node attribute to use for coloring
#' @param size_by Node attribute to use for sizing
#' @param node_metrics Data frame with node metrics
#' @param area_colors Color palette for brain areas
#' @param show_labels Whether to show node labels
#' @param use_3d Whether to use 3D visualization
#' @return A plotly object
#' @export
plot_network <- function(graph, layout = "fr", color_by = "brain_area", 
                        size_by = NULL, node_metrics = NULL, 
                        area_colors = NULL, show_labels = FALSE, use_3d = FALSE) {
  
  # Check if metrics are provided
  if (is.null(node_metrics)) {
    node_metrics <- data.frame(
      node = 1:igraph::vcount(graph),
      name = igraph::V(graph)$name,
      brain_area = ifelse(is.null(igraph::V(graph)$brain_area), "Other", igraph::V(graph)$brain_area),
      degree = igraph::degree(graph),
      betweenness = igraph::betweenness(graph, normalized = TRUE),
      closeness = igraph::closeness(graph, normalized = TRUE),
      clustering = igraph::transitivity(graph, type = "local", vids = igraph::V(graph))
    )
    
    # Handle NA values in clustering coefficient
    node_metrics$clustering[is.na(node_metrics$clustering)] <- 0
  }
  
  # Generate layout
  if (layout == "fr") {
    layout_coords <- igraph::layout_with_fr(graph)
  } else if (layout == "kk") {
    layout_coords <- igraph::layout_with_kk(graph)
  } else if (layout == "circle") {
    layout_coords <- igraph::layout_in_circle(graph)
  } else if (layout == "grid") {
    layout_coords <- igraph::layout_on_grid(graph)
  } else if (layout == "sphere" && use_3d) {
    layout_coords <- igraph::layout_on_sphere(graph)
  } else {
    layout_coords <- igraph::layout_with_fr(graph)
  }
  
  # Add Z dimension for 3D if needed
  if (use_3d && ncol(layout_coords) < 3) {
    layout_coords <- cbind(layout_coords, rep(0, nrow(layout_coords)))
  }
  
  # Extract edges
  edges <- igraph::as_data_frame(graph, what = "edges")
  
  # Get node colors
  if (color_by == "brain_area") {
    if (is.null(area_colors)) {
      # Default colors if none provided
      all_areas <- unique(node_metrics$brain_area)
      area_colors <- setNames(
        colorRampPalette(RColorBrewer::brewer.pal(min(9, length(all_areas)), "Set1"))(length(all_areas)),
        all_areas
      )
    }
    node_colors <- area_colors[node_metrics$brain_area]
  } else {
    # Color by metric
    metric_values <- node_metrics[[color_by]]
    color_palette <- colorRampPalette(c("blue", "white", "red"))(100)
    color_indices <- cut(metric_values, breaks = 100, labels = FALSE, include.lowest = TRUE)
    node_colors <- color_palette[color_indices]
  }
  
  # Get node sizes
  if (is.null(size_by) || size_by == "none") {
    node_sizes <- rep(10, nrow(node_metrics))
  } else {
    # Size by metric
    metric_values <- node_metrics[[size_by]]
    size_range <- c(5, 20)
    node_sizes <- scales::rescale(metric_values, to = size_range)
  }
  
  # Prepare node data
  nodes <- data.frame(
    id = node_metrics$node,
    label = node_metrics$name,
    x = layout_coords[, 1],
    y = layout_coords[, 2],
    z = if (use_3d && ncol(layout_coords) >= 3) layout_coords[, 3] else rep(0, nrow(layout_coords)),
    color = node_colors,
    size = node_sizes,
    brain_area = node_metrics$brain_area,
    degree = node_metrics$degree,
    betweenness = node_metrics$betweenness,
    closeness = node_metrics$closeness,
    clustering = node_metrics$clustering
  )
  
  # Prepare edge data
  edge_data <- data.frame(
    x = c(rbind(layout_coords[edges$from, 1], layout_coords[edges$to, 1], NA)),
    y = c(rbind(layout_coords[edges$from, 2], layout_coords[edges$to, 2], NA)),
    z = if (use_3d && ncol(layout_coords) >= 3) {
      c(rbind(layout_coords[edges$from, 3], layout_coords[edges$to, 3], NA))
    } else {
      rep(0, 3 * nrow(edges))
    }
  )
  
  # Create plot
  if (use_3d) {
    p <- plotly::plot_ly() %>%
      # Add edges
      plotly::add_trace(
        data = edge_data,
        x = ~x, y = ~y, z = ~z,
        type = "scatter3d", mode = "lines",
        line = list(color = "rgba(190, 190, 190, 0.5)", width = 1),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      # Add nodes
      plotly::add_trace(
        data = nodes,
        x = ~x, y = ~y, z = ~z,
        type = "scatter3d", mode = "markers",
        marker = list(
          color = ~color,
          size = ~size,
          line = list(color = "black", width = 0.5)
        ),
        text = ~paste(
          "Node:", label, "<br>",
          "Brain Area:", brain_area, "<br>",
          "Degree:", degree, "<br>",
          "Betweenness:", round(betweenness, 3), "<br>",
          "Closeness:", round(closeness, 3), "<br>",
          "Clustering:", round(clustering, 3)
        ),
        hoverinfo = "text"
      )
    
    # Add labels if requested
    if (show_labels) {
      p <- p %>% plotly::add_trace(
        data = nodes,
        x = ~x, y = ~y, z = ~z,
        type = "scatter3d", mode = "text",
        text = ~label,
        textposition = "top center",
        textfont = list(size = 10),
        hoverinfo = "none",
        showlegend = FALSE
      )
    }
    
  } else {
    # 2D plot
    p <- plotly::plot_ly() %>%
      # Add edges
      plotly::add_trace(
        data = edge_data,
        x = ~x, y = ~y,
        type = "scatter", mode = "lines",
        line = list(color = "rgba(190, 190, 190, 0.5)", width = 1),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      # Add nodes
      plotly::add_trace(
        data = nodes,
        x = ~x, y = ~y,
        type = "scatter", mode = "markers",
        marker = list(
          color = ~color,
          size = ~size,
          line = list(color = "black", width = 0.5)
        ),
        text = ~paste(
          "Node:", label, "<br>",
          "Brain Area:", brain_area, "<br>",
          "Degree:", degree, "<br>",
          "Betweenness:", round(betweenness, 3), "<br>",
          "Closeness:", round(closeness, 3), "<br>",
          "Clustering:", round(clustering, 3)
        ),
        hoverinfo = "text"
      )
    
    # Add labels if requested
    if (show_labels) {
      p <- p %>% plotly::add_trace(
        data = nodes,
        x = ~x, y = ~y,
        type = "scatter", mode = "text",
        text = ~label,
        textposition = "top center",
        textfont = list(size = 10),
        hoverinfo = "none",
        showlegend = FALSE
      )
    }
  }
  
  # Add layout
  p <- p %>% plotly::layout(
    title = "Brain Network Graph",
    showlegend = FALSE,
    hovermode = "closest",
    margin = list(l = 0, r = 0, b = 0, t = 40)
  )
  
  return(p)
}
#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/visualization/network_plot.R - Network visualization functions
#########################################################

#' Generate network visualization
#' 
#' @param graph igraph object
#' @param node_metrics Data frame with node metrics
#' @param community Community detection results
#' @param show_labels Whether to show node labels
#' @param use_3d Whether to generate 3D plot
#' @return plotly object with network visualization
#' @export
generate_network_plot <- function(graph, node_metrics = NULL, community = NULL,
                                 show_labels = TRUE, use_3d = FALSE) {
  # Check if graph is valid
  if (is.null(graph) || igraph::vcount(graph) == 0) {
    # Return empty plot with message
    p <- plotly::plot_ly() %>%
      plotly::add_annotations(
        text = "No network to display",
        showarrow = FALSE,
        font = list(size = 20)
      )
    return(p)
  }
  
  # Get node names
  node_names <- igraph::V(graph)$name
  if (is.null(node_names)) {
    node_names <- paste0("Node", 1:igraph::vcount(graph))
  }
  
  # Calculate layout
  if (use_3d) {
    layout <- igraph::layout_with_fr(graph, dim = 3)
  } else {
    layout <- igraph::layout_with_fr(graph)
  }
  
  # Add coordinates to graph
  igraph::V(graph)$x <- layout[, 1]
  igraph::V(graph)$y <- layout[, 2]
  if (use_3d) {
    igraph::V(graph)$z <- layout[, 3]
  }
  
  # Determine node colors based on community if available
  if (!is.null(community)) {
    # Get community membership
    membership <- community$membership
    
    # Create color palette for communities
    n_communities <- length(unique(membership))
    community_colors <- viridis::viridis(n_communities)
    
    # Assign colors to nodes
    node_colors <- community_colors[membership]
  } else {
    # Default node color
    node_colors <- rep("#1F78B4", igraph::vcount(graph))
  }
  
  # Determine node sizes based on metrics if available
  if (!is.null(node_metrics) && "degree" %in% names(node_metrics)) {
    # Scale node sizes based on degree
    node_sizes <- scales::rescale(node_metrics$degree, to = c(5, 20))
  } else {
    # Default node size
    node_sizes <- rep(10, igraph::vcount(graph))
  }
  
  # Get edge weights if available
  edge_weights <- rep(1, igraph::ecount(graph))
  if ("weight" %in% igraph::edge_attr_names(graph)) {
    edge_weights <- igraph::E(graph)$weight
  }
  
  # Scale edge widths
  edge_widths <- scales::rescale(abs(edge_weights), to = c(0.5, 3))
  
  # Determine edge colors based on weight sign
  edge_colors <- ifelse(edge_weights >= 0, "#FF5555", "#5555FF")
  
  # Create node data for plotting
  nodes <- data.frame(
    id = 1:igraph::vcount(graph),
    label = node_names,
    x = layout[, 1],
    y = layout[, 2],
    color = node_colors,
    size = node_sizes,
    stringsAsFactors = FALSE
  )
  
  if (use_3d) {
    nodes$z <- layout[, 3]
  }
  
  # Create edge data for plotting
  edges <- data.frame(
    from = igraph::ends(graph, 1:igraph::ecount(graph))[, 1],
    to = igraph::ends(graph, 1:igraph::ecount(graph))[, 2],
    weight = edge_weights,
    width = edge_widths,
    color = edge_colors,
    stringsAsFactors = FALSE
  )
  
  # Create edge coordinates for plotting
  edge_coords <- lapply(1:nrow(edges), function(i) {
    from_idx <- edges$from[i]
    to_idx <- edges$to[i]
    
    if (use_3d) {
      return(list(
        x = c(nodes$x[from_idx], nodes$x[to_idx], NA),
        y = c(nodes$y[from_idx], nodes$y[to_idx], NA),
        z = c(nodes$z[from_idx], nodes$z[to_idx], NA)
      ))
    } else {
      return(list(
        x = c(nodes$x[from_idx], nodes$x[to_idx], NA),
        y = c(nodes$y[from_idx], nodes$y[to_idx], NA)
      ))
    }
  })
  
  # Create plot
  if (use_3d) {
    # 3D plot
    p <- plotly::plot_ly() %>%
      # Add edges
      plotly::add_trace(
        type = "scatter3d",
        mode = "lines",
        x = unlist(lapply(edge_coords, function(e) e$x)),
        y = unlist(lapply(edge_coords, function(e) e$y)),
        z = unlist(lapply(edge_coords, function(e) e$z)),
        line = list(
          color = rep(edges$color, each = 3),
          width = rep(edges$width, each = 3)
        ),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      # Add nodes
      plotly::add_trace(
        type = "scatter3d",
        mode = "markers",
        x = nodes$x,
        y = nodes$y,
        z = nodes$z,
        marker = list(
          color = nodes$color,
          size = nodes$size,
          line = list(color = "black", width = 0.5)
        ),
        text = nodes$label,
        hoverinfo = "text"
      )
    
    if (show_labels) {
      p <- p %>%
        # Add node labels
        plotly::add_trace(
          type = "scatter3d",
          mode = "text",
          x = nodes$x,
          y = nodes$y,
          z = nodes$z,
          text = nodes$label,
          textposition = "top center",
          textfont = list(size = 10),
          hoverinfo = "none",
          showlegend = FALSE
        )
    }
    
  } else {
    # 2D plot
    p <- plotly::plot_ly() %>%
      # Add edges
      plotly::add_trace(
        type = "scatter",
        mode = "lines",
        x = unlist(lapply(edge_coords, function(e) e$x)),
        y = unlist(lapply(edge_coords, function(e) e$y)),
        line = list(
          color = rep(edges$color, each = 3),
          width = rep(edges$width, each = 3)
        ),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      # Add nodes
      plotly::add_trace(
        type = "scatter",
        mode = "markers",
        x = nodes$x,
        y = nodes$y,
        marker = list(
          color = nodes$color,
          size = nodes$size,
          line = list(color = "black", width = 0.5)
        ),
        text = nodes$label,
        hoverinfo = "text"
      )
    
    if (show_labels) {
      p <- p %>%
        # Add node labels
        plotly::add_trace(
          type = "scatter",
          mode = "text",
          x = nodes$x,
          y = nodes$y,
          text = nodes$label,
          textposition = "top center",
          textfont = list(size = 10),
          hoverinfo = "none",
          showlegend = FALSE
        )
    }
  }
  
  # Add layout
  p <- p %>%
    plotly::layout(
      title = "Network Visualization",
      showlegend = FALSE,
      hovermode = "closest",
      margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
  
  # Return the plot
  return(p)
}

#' Generate circular network visualization
#' 
#' @param graph igraph object
#' @param node_metrics Data frame with node metrics
#' @param community Community detection results
#' @param group_by Brain area grouping to use for organizing nodes
#' @param show_labels Whether to show node labels
#' @return plotly object with circular network visualization
#' @export
generate_circular_network_plot <- function(graph, node_metrics = NULL, community = NULL,
                                          group_by = "brain_area", show_labels = TRUE) {
  # Check if graph is valid
  if (is.null(graph) || igraph::vcount(graph) == 0) {
    # Return empty plot with message
    p <- plotly::plot_ly() %>%
      plotly::add_annotations(
        text = "No network to display",
        showarrow = FALSE,
        font = list(size = 20)
      )
    return(p)
  }
  
  # Get node names
  node_names <- igraph::V(graph)$name
  if (is.null(node_names)) {
    node_names <- paste0("Node", 1:igraph::vcount(graph))
  }
  
  # Get node groups if available
  if (group_by == "brain_area" && !is.null(node_metrics) && "Brain_Area" %in% names(node_metrics)) {
    node_groups <- node_metrics$Brain_Area
  } else if (group_by == "community" && !is.null(community)) {
    node_groups <- community$membership
  } else {
    # Default grouping
    node_groups <- rep("Default", igraph::vcount(graph))
  }
  
  # Order nodes by group
  node_order <- order(node_groups)
  
  # Generate circular layout
  n_nodes <- igraph::vcount(graph)
  theta <- seq(0, 2 * pi, length.out = n_nodes + 1)[1:n_nodes]
  
  # Reorder theta based on groups
  theta <- theta[node_order]
  
  # Calculate coordinates
  x <- cos(theta)
  y <- sin(theta)
  
  # Add coordinates to graph
  igraph::V(graph)$x <- x
  igraph::V(graph)$y <- y
  
  # Determine node colors based on group
  unique_groups <- unique(node_groups)
  n_groups <- length(unique_groups)
  
  if (n_groups <= 8) {
    group_colors <- RColorBrewer::brewer.pal(max(n_groups, 3), "Set1")
    if (n_groups < 3) {
      group_colors <- group_colors[1:n_groups]
    }
  } else {
    group_colors <- viridis::viridis(n_groups)
  }
  
  # Create mapping from groups to colors
  group_color_map <- setNames(group_colors, unique_groups)
  
  # Assign colors to nodes
  node_colors <- group_color_map[node_groups]
  
  # Determine node sizes based on metrics if available
  if (!is.null(node_metrics) && "degree" %in% names(node_metrics)) {
    # Scale node sizes based on degree
    node_sizes <- scales::rescale(node_metrics$degree, to = c(5, 15))
  } else {
    # Default node size
    node_sizes <- rep(10, n_nodes)
  }
  
  # Get edge weights if available
  edge_weights <- rep(1, igraph::ecount(graph))
  if ("weight" %in% igraph::edge_attr_names(graph)) {
    edge_weights <- igraph::E(graph)$weight
  }
  
  # Scale edge widths
  edge_widths <- scales::rescale(abs(edge_weights), to = c(0.5, 3))
  
  # Determine edge colors based on weight sign
  edge_colors <- ifelse(edge_weights >= 0, "#FF5555", "#5555FF")
  
  # Create node data for plotting
  nodes <- data.frame(
    id = 1:n_nodes,
    label = node_names,
    x = x,
    y = y,
    color = node_colors,
    size = node_sizes,
    group = node_groups,
    stringsAsFactors = FALSE
  )
  
  # Create edge data for plotting
  edges <- data.frame(
    from = igraph::ends(graph, 1:igraph::ecount(graph))[, 1],
    to = igraph::ends(graph, 1:igraph::ecount(graph))[, 2],
    weight = edge_weights,
    width = edge_widths,
    color = edge_colors,
    stringsAsFactors = FALSE
  )
  
  # Create edge coordinates for plotting
  edge_coords <- lapply(1:nrow(edges), function(i) {
    from_idx <- edges$from[i]
    to_idx <- edges$to[i]
    
    # Calculate control points for curved edges
    # This creates a quadratic Bezier curve
    x0 <- nodes$x[from_idx]
    y0 <- nodes$y[from_idx]
    x1 <- nodes$x[to_idx]
    y1 <- nodes$y[to_idx]
    
    # Midpoint
    xm <- (x0 + x1) / 2
    ym <- (y0 + y1) / 2
    
    # Perpendicular direction from midpoint to center
    d <- sqrt(xm^2 + ym^2)
    
    if (d > 0) {
      # Pull the curve toward center for better visibility
      # Adjust curvature based on distance between nodes
      angle_diff <- abs(atan2(y1, x1) - atan2(y0, x0))
      if (angle_diff > pi) {
        angle_diff <- 2 * pi - angle_diff
      }
      
      curvature <- 0.1 + 0.1 * (1 - angle_diff / pi)
      
      xc <- xm - (curvature * ym / d)
      yc <- ym + (curvature * xm / d)
    } else {
      # Fallback for exact same points
      xc <- xm
      yc <- ym
    }
    
    # Create curve using Bezier interpolation
    t <- seq(0, 1, length.out = 50)
    
    # Quadratic Bezier formula
    bx <- (1-t)^2 * x0 + 2 * (1-t) * t * xc + t^2 * x1
    by <- (1-t)^2 * y0 + 2 * (1-t) * t * yc + t^2 * y1
    
    return(list(
      x = bx,
      y = by
    ))
  })
  
  # Create plot
  p <- plotly::plot_ly() %>%
    # Add edges
    plotly::add_trace(
      type = "scatter",
      mode = "lines",
      x = unlist(lapply(edge_coords, function(e) c(e$x, NA))),
      y = unlist(lapply(edge_coords, function(e) c(e$y, NA))),
      line = list(
        color = rep(edges$color, each = 51),  # 50 points + NA
        width = rep(edges$width, each = 51)
      ),
      hoverinfo = "none",
      showlegend = FALSE
    ) %>%
    # Add nodes
    plotly::add_trace(
      type = "scatter",
      mode = "markers",
      x = nodes$x,
      y = nodes$y,
      marker = list(
        color = nodes$color,
        size = nodes$size,
        line = list(color = "black", width = 0.5)
      ),
      text = nodes$label,
      hoverinfo = "text"
    )
  
  if (show_labels) {
    # Add node labels
    label_positions <- lapply(1:n_nodes, function(i) {
      # Position labels slightly outside the circle
      angle <- theta[i]
      factor <- 1.1  # Push labels outside by 10%
      
      return(list(
        x = factor * x[i],
        y = factor * y[i],
        angle = angle
      ))
    })
    
    p <- p %>%
      plotly::add_trace(
        type = "scatter",
        mode = "text",
        x = sapply(label_positions, function(pos) pos$x),
        y = sapply(label_positions, function(pos) pos$y),
        text = nodes$label,
        textposition = "middle center",
        textfont = list(size = 10),
        hoverinfo = "none",
        showlegend = FALSE
      )
  }
  
  # Add group arcs
  if (n_groups > 1) {
    # Add arcs to separate groups
    unique_groups_ordered <- unique(nodes$group[order(theta)])
    
    # Find start and end points for each group
    group_boundaries <- lapply(unique_groups_ordered, function(group) {
      group_indices <- which(nodes$group == group)
      start_idx <- min(group_indices)
      end_idx <- max(group_indices)
      
      # Get angles
      start_angle <- theta[start_idx]
      end_angle <- theta[end_idx]
      
      # Ensure proper arc direction
      if (end_angle < start_angle) {
        end_angle <- end_angle + 2 * pi
      }
      
      return(list(
        group = group,
        start_angle = start_angle,
        end_angle = end_angle,
        color = group_color_map[group]
      ))
    })
    
    # Add group labels
    for (boundary in group_boundaries) {
      mid_angle <- (boundary$start_angle + boundary$end_angle) / 2
      if (mid_angle > 2 * pi) {
        mid_angle <- mid_angle - 2 * pi
      }
      
      label_r <- 1.25  # Position for group labels
      label_x <- label_r * cos(mid_angle)
      label_y <- label_r * sin(mid_angle)
      
      p <- p %>%
        plotly::add_annotations(
          x = label_x,
          y = label_y,
          text = boundary$group,
          showarrow = FALSE,
          font = list(
            color = boundary$color,
            size = 14,
            family = "Arial"
          )
        )
    }
  }
  
  # Add layout
  p <- p %>%
    plotly::layout(
      title = "Circular Network Visualization",
      showlegend = FALSE,
      hovermode = "closest",
      xaxis = list(
        zeroline = FALSE,
        showgrid = FALSE,
        showticklabels = FALSE,
        range = c(-1.3, 1.3)
      ),
      yaxis = list(
        zeroline = FALSE,
        showgrid = FALSE,
        showticklabels = FALSE,
        range = c(-1.3, 1.3),
        scaleanchor = "x"
      ),
      margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
  
  # Return the plot
  return(p)
}
