#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/advanced/information_flow.R - Information flow analysis
#########################################################

#' Calculate information flow patterns between brain regions
#' 
#' @param time_series Data frame containing time series for brain regions
#' @param region_columns Vector of column names for brain regions
#' @param time_lag Time lag for temporal analysis
#' @param num_bins Number of bins for discretization
#' @return List containing information flow results
#' @export
analyze_information_flow <- function(time_series, region_columns, time_lag = 1, num_bins = 3) {
  # Check required packages
  if (!requireNamespace("infotheo", quietly = TRUE)) {
    stop("Package 'infotheo' is required for information flow analysis")
  }
  
  # Check inputs
  if (is.null(time_series) || nrow(time_series) == 0) {
    stop("Time series data is required")
  }
  
  if (length(region_columns) < 2) {
    stop("At least two brain regions are required for analysis")
  }
  
  # Extract time series for specified regions
  region_data <- time_series[, region_columns, drop = FALSE]
  
  # Initialize results
  n_regions <- length(region_columns)
  results <- list(
    mutual_information = matrix(0, nrow = n_regions, ncol = n_regions),
    transfer_entropy = matrix(0, nrow = n_regions, ncol = n_regions),
    synergy = matrix(0, nrow = n_regions, ncol = n_regions),
    redundancy = matrix(0, nrow = n_regions, ncol = n_regions),
    unique_information = matrix(0, nrow = n_regions, ncol = n_regions),
    information_flow_patterns = list()
  )
  
  # Set row and column names
  rownames(results$mutual_information) <- colnames(results$mutual_information) <- region_columns
  rownames(results$transfer_entropy) <- colnames(results$transfer_entropy) <- region_columns
  rownames(results$synergy) <- colnames(results$synergy) <- region_columns
  rownames(results$redundancy) <- colnames(results$redundancy) <- region_columns
  rownames(results$unique_information) <- colnames(results$unique_information) <- region_columns
  
  # Discretize time series
  discretized_data <- infotheo::discretize(as.matrix(region_data), disc = "equalwidth", 
                                           nbins = num_bins)
  
  # Calculate mutual information between all pairs of regions
  for (i in 1:n_regions) {
    for (j in 1:n_regions) {
      if (i != j) {
        # Calculate mutual information
        mi <- infotheo::mutinformation(discretized_data[, i], discretized_data[, j])
        results$mutual_information[i, j] <- mi
      }
    }
  }
  
  # Calculate transfer entropy (TE) for all pairs of regions
  # TE measures directed information flow from source to target
  n_samples <- nrow(discretized_data)
  for (i in 1:n_regions) {
    for (j in 1:n_regions) {
      if (i != j) {
        # Prepare past and future states
        source_past <- discretized_data[1:(n_samples - time_lag), i]
        target_past <- discretized_data[1:(n_samples - time_lag), j]
        target_future <- discretized_data[(time_lag + 1):n_samples, j]
        
        # Calculate conditional entropy terms for transfer entropy
        # TE = H(target_future | target_past) - H(target_future | target_past, source_past)
        
        # Create joint variables
        joint_past_target <- cbind(target_past)
        joint_both_past_target <- cbind(target_past, source_past)
        
        # Calculate entropies
        h_target_future <- infotheo::entropy(target_future)
        h_joint_past_target_future <- infotheo::entropy(cbind(joint_past_target, target_future))
        h_joint_both_past_target_future <- infotheo::entropy(cbind(joint_both_past_target, target_future))
        h_joint_past_target <- infotheo::entropy(joint_past_target)
        h_joint_both_past <- infotheo::entropy(joint_both_past_target)
        
        # Calculate conditional entropies
        h_target_future_given_target_past <- h_joint_past_target_future - h_joint_past_target
        h_target_future_given_both_past <- h_joint_both_past_target_future - h_joint_both_past
        
        # Calculate transfer entropy
        te <- h_target_future_given_target_past - h_target_future_given_both_past
        results$transfer_entropy[i, j] <- te
      }
    }
  }
  
  # Calculate basic synergy and redundancy for all pairs
  for (i in 1:(n_regions-1)) {
    for (j in (i+1):n_regions) {
      # Calculate synergy and redundancy using mutual information
      mi_i <- results$mutual_information[i, j]
      te_i_to_j <- results$transfer_entropy[i, j]
      te_j_to_i <- results$transfer_entropy[j, i]
      
      # Synergy is when two sources together provide more information than the sum of their individual contributions
      synergy <- max(0, mi_i - (te_i_to_j + te_j_to_i))
      results$synergy[i, j] <- results$synergy[j, i] <- synergy
      
      # Redundancy is when both sources provide the same information
      redundancy <- min(te_i_to_j, te_j_to_i)
      results$redundancy[i, j] <- results$redundancy[j, i] <- redundancy
      
      # Unique information is the information provided by one source that is not provided by the other
      unique_i_to_j <- te_i_to_j - redundancy
      unique_j_to_i <- te_j_to_i - redundancy
      results$unique_information[i, j] <- unique_i_to_j
      results$unique_information[j, i] <- unique_j_to_i
    }
  }
  
  # Identify information flow patterns for each pair of regions
  patterns <- list()
  
  for (i in 1:(n_regions-1)) {
    for (j in (i+1):n_regions) {
      # Extract metrics for this pair
      synergy <- results$synergy[i, j]
      redundancy <- results$redundancy[i, j]
      unique_i_to_j <- results$unique_information[i, j]
      unique_j_to_i <- results$unique_information[j, i]
      
      # Normalize to sum to 1
      total <- synergy + redundancy + unique_i_to_j + unique_j_to_i
      if (total > 0) {
        synergy_norm <- synergy / total
        redundancy_norm <- redundancy / total
        unique_i_to_j_norm <- unique_i_to_j / total
        unique_j_to_i_norm <- unique_j_to_i / total
      } else {
        synergy_norm <- redundancy_norm <- unique_i_to_j_norm <- unique_j_to_i_norm <- 0
      }
      
      # Identify dominant pattern
      values <- c(synergy_norm, redundancy_norm, unique_i_to_j_norm, unique_j_to_i_norm)
      names(values) <- c("Synergy", "Redundancy", "Unique_i_to_j", "Unique_j_to_i")
      dominant <- names(which.max(values))
      
      # Classify information flow pattern
      if (redundancy_norm > 0.5) {
        pattern <- "Redundant Information Storage"
      } else if (synergy_norm > 0.5) {
        pattern <- "Synergistic Processing"
      } else if (unique_i_to_j_norm > 0.5) {
        pattern <- "Information Transfer (i → j)"
      } else if (unique_j_to_i_norm > 0.5) {
        pattern <- "Information Transfer (j → i)"
      } else if (redundancy_norm > 0.3 && synergy_norm > 0.3) {
        pattern <- "Information Integration"
      } else if (unique_i_to_j_norm > 0.3 && unique_j_to_i_norm > 0.3) {
        pattern <- "Bidirectional Exchange"
      } else {
        pattern <- "Mixed Pattern"
      }
      
      # Store pattern information
      patterns[[paste(region_columns[i], region_columns[j], sep = "-")]] <- list(
        regions = c(region_columns[i], region_columns[j]),
        synergy = synergy,
        redundancy = redundancy,
        unique_i_to_j = unique_i_to_j,
        unique_j_to_i = unique_j_to_i,
        synergy_norm = synergy_norm,
        redundancy_norm = redundancy_norm,
        unique_i_to_j_norm = unique_i_to_j_norm,
        unique_j_to_i_norm = unique_j_to_i_norm,
        dominant = dominant,
        pattern = pattern
      )
    }
  }
  
  # Store patterns in results
  results$information_flow_patterns <- patterns
  
  # Calculate information flow type distribution
  pattern_counts <- table(sapply(patterns, function(p) p$pattern))
  results$pattern_distribution <- pattern_counts / sum(pattern_counts)
  
  # Calculate region-level information metrics
  region_metrics <- data.frame(
    Region = region_columns,
    OutgoingTE = colSums(results$transfer_entropy),
    IncomingTE = rowSums(results$transfer_entropy),
    TotalMI = colSums(results$mutual_information),
    SynergyContribution = colSums(results$synergy),
    RedundancyContribution = colSums(results$redundancy),
    stringsAsFactors = FALSE
  )
  
  # Calculate net information flow (outgoing - incoming)
  region_metrics$NetFlow <- region_metrics$OutgoingTE - region_metrics$IncomingTE
  
  # Store region metrics
  results$region_metrics <- region_metrics
  
  return(results)
}

#' Generate information flow network visualization
#' 
#' @param info_flow_results Results from analyze_information_flow function
#' @param metric Information metric to visualize (mutual_information, transfer_entropy, etc.)
#' @param threshold Threshold for including edges
#' @param show_labels Whether to show node labels
#' @return plotly object with information flow network visualization
#' @export
generate_information_flow_network <- function(info_flow_results, metric = "transfer_entropy",
                                              threshold = 0.1, show_labels = TRUE) {
  # Check if info flow results are available
  if (is.null(info_flow_results)) {
    # Return empty plot with message
    return(plotly::plot_ly() %>%
             plotly::add_annotations(
               text = "No information flow results available",
               showarrow = FALSE,
               font = list(size = 20)
             ))
  }
  
  # Get appropriate matrix
  if (metric == "mutual_information") {
    matrix <- info_flow_results$mutual_information
    directed <- FALSE
  } else if (metric == "transfer_entropy") {
    matrix <- info_flow_results$transfer_entropy
    directed <- TRUE
  } else if (metric == "synergy") {
    matrix <- info_flow_results$synergy
    directed <- FALSE
  } else if (metric == "redundancy") {
    matrix <- info_flow_results$redundancy
    directed <- FALSE
  } else if (metric == "unique_information") {
    matrix <- info_flow_results$unique_information
    directed <- TRUE
  } else {
    # Default to mutual information
    matrix <- info_flow_results$mutual_information
    directed <- FALSE
  }
  
  # Apply threshold
  matrix[matrix < threshold] <- 0
  
  # Create igraph object
  if (directed) {
    g <- igraph::graph_from_adjacency_matrix(matrix, mode = "directed", weighted = TRUE)
  } else {
    g <- igraph::graph_from_adjacency_matrix(matrix, mode = "undirected", weighted = TRUE)
  }
  
  # Set node attributes based on region metrics
  if (!is.null(info_flow_results$region_metrics)) {
    # Set node sizes based on total mutual information
    igraph::V(g)$size <- scales::rescale(info_flow_results$region_metrics$TotalMI, to = c(5, 20))
    
    # Set node colors based on net information flow
    net_flow <- info_flow_results$region_metrics$NetFlow
    color_scale <- scales::col_numeric(
      palette = c("blue", "white", "red"),
      domain = c(min(net_flow), 0, max(net_flow))
    )
    igraph::V(g)$color <- color_scale(net_flow)
  } else {
    # Default node attributes
    igraph::V(g)$size <- rep(10, igraph::vcount(g))
    igraph::V(g)$color <- rep("#1F78B4", igraph::vcount(g))
  }
  
  # Set edge attributes
  igraph::E(g)$width <- scales::rescale(igraph::E(g)$weight, to = c(0.5, 3))
  
  # Calculate layout
  layout <- igraph::layout_with_fr(g)
  
  # Add layout coordinates to graph
  igraph::V(g)$x <- layout[, 1]
  igraph::V(g)$y <- layout[, 2]
  
  # Create node data
  nodes <- data.frame(
    id = 1:igraph::vcount(g),
    label = igraph::V(g)$name,
    x = layout[, 1],
    y = layout[, 2],
    size = igraph::V(g)$size,
    color = igraph::V(g)$color,
    stringsAsFactors = FALSE
  )
  
  # Create edge data
  if (directed) {
    # Directed edges
    edges <- data.frame(
      from = igraph::head_of(g, igraph::E(g)),
      to = igraph::tail_of(g, igraph::E(g)),
      weight = igraph::E(g)$weight,
      width = igraph::E(g)$width,
      stringsAsFactors = FALSE
    )
  } else {
    # Undirected edges
    edges <- data.frame(
      from = igraph::get.edges(g, igraph::E(g))[, 1],
      to = igraph::get.edges(g, igraph::E(g))[, 2],
      weight = igraph::E(g)$weight,
      width = igraph::E(g)$width,
      stringsAsFactors = FALSE
    )
  }
  
  # Create edge coordinates for plotting
  edge_coords <- lapply(1:nrow(edges), function(i) {
    from_idx <- edges$from[i]
    to_idx <- edges$to[i]
    
    if (directed) {
      # Add curvature for directed edges
      # This creates a quadratic Bezier curve
      x0 <- nodes$x[from_idx]
      y0 <- nodes$y[from_idx]
      x1 <- nodes$x[to_idx]
      y1 <- nodes$y[to_idx]
      
      # Midpoint with offset for curvature
      xm <- (x0 + x1) / 2
      ym <- (y0 + y1) / 2
      
      # Perpendicular direction
      dx <- x1 - x0
      dy <- y1 - y0
      
      # Normal vector
      length <- sqrt(dx^2 + dy^2)
      if (length > 0) {
        nx <- -dy / length
        ny <- dx / length
        
        # Control point with curvature
        curvature <- 0.2
        xc <- xm + curvature * nx
        yc <- ym + curvature * ny
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
    } else {
      # Straight lines for undirected edges
      return(list(
        x = c(nodes$x[from_idx], nodes$x[to_idx], NA),
        y = c(nodes$y[from_idx], nodes$y[to_idx], NA)
      ))
    }
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
        color = "gray",
        width = rep(edges$width, each = ifelse(directed, 51, 3))  # 50+1 for directed, 3 for undirected
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
    p <- p %>%
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
  
  # Add layout
  p <- p %>%
    plotly::layout(
      title = paste("Information Flow Network -", metric),
      showlegend = FALSE,
      hovermode = "closest",
      xaxis = list(
        title = "",
        zeroline = FALSE,
        showgrid = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        title = "",
        zeroline = FALSE,
        showgrid = FALSE,
        showticklabels = FALSE,
        scaleanchor = "x"
      ),
      margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
  
  return(p)
}

#' Generate information flow pattern distribution plot
#' 
#' @param info_flow_results Results from analyze_information_flow function
#' @return ggplot2 object with pattern distribution plot
#' @export
generate_information_pattern_plot <- function(info_flow_results) {
  # Check if info flow results are available
  if (is.null(info_flow_results) || is.null(info_flow_results$pattern_distribution)) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, 
                               label = "No information flow pattern data available") +
             ggplot2::theme_void() +
             ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
  }
  
  # Create data frame for plotting
  pattern_dist <- data.frame(
    Pattern = names(info_flow_results$pattern_distribution),
    Proportion = as.numeric(info_flow_results$pattern_distribution),
    stringsAsFactors = FALSE
  )
  
  # Sort by proportion
  pattern_dist <- pattern_dist[order(pattern_dist$Proportion, decreasing = TRUE), ]
  
  # Create factor with ordered levels
  pattern_dist$Pattern <- factor(pattern_dist$Pattern, levels = pattern_dist$Pattern)
  
  # Create color map
  pattern_colors <- c(
    "Redundant Information Storage" = "#4DAF4A",
    "Synergistic Processing" = "#377EB8",
    "Information Transfer (i → j)" = "#FF7F00",
    "Information Transfer (j → i)" = "#E41A1C",
    "Information Integration" = "#984EA3",
    "Bidirectional Exchange" = "#FFFF33",
    "Mixed Pattern" = "#A65628"
  )
  
  # Create plot
  p <- ggplot2::ggplot(pattern_dist, ggplot2::aes(x = Pattern, y = Proportion, fill = Pattern)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = pattern_colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Information Flow Pattern Distribution",
      x = "",
      y = "Proportion of Region Pairs"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  
  return(p)
}

#' Generate region information flow metrics plot
#' 
#' @param info_flow_results Results from analyze_information_flow function
#' @return ggplot2 object with region metrics plot
#' @export
generate_region_information_plot <- function(info_flow_results) {
  # Check if info flow results are available
  if (is.null(info_flow_results) || is.null(info_flow_results$region_metrics)) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, 
                               label = "No region information flow data available") +
             ggplot2::theme_void() +
             ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1))
  }
  
  # Create data frame for plotting
  region_metrics <- info_flow_results$region_metrics
  
  # Convert to long format for plotting
  plot_data <- tidyr::pivot_longer(
    region_metrics,
    cols = c("OutgoingTE", "IncomingTE", "NetFlow"),
    names_to = "Metric",
    values_to = "Value"
  )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Region, y = Value, fill = Metric)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(values = c(
      "OutgoingTE" = "#4DAF4A",
      "IncomingTE" = "#377EB8",
      "NetFlow" = "#FF7F00"
    )) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Region Information Flow Metrics",
      x = "Brain Region",
      y = "Information Flow",
      fill = "Metric"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  
  return(p)
}
