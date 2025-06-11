#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/network/metrics.R - Network metrics calculation
#########################################################

#' Calculate global network metrics
#' 
#' @param graph igraph object
#' @param metrics Vector of metric names to calculate
#' @return Named list of global metrics
#' @export
calculate_global_metrics <- function(graph, metrics = NULL) {
  # Initialize results list
  results <- list()
  
  # If no metrics specified, calculate all
  if (is.null(metrics)) {
    metrics <- unlist(NETWORK_METRICS$Global)
  }
  
  # Ensure graph is undirected and has positive weights
  graph <- igraph::as.undirected(graph, mode = "collapse")
  
  # Calculate requested metrics
  for (metric in metrics) {
    results[[metric]] <- switch(
      metric,
      "density" = igraph::edge_density(graph),
      "clustering" = igraph::transitivity(graph, type = "global"),
      "path_length" = mean_shortest_path_length(graph),
      "modularity" = calculate_modularity(graph),
      "assortativity" = igraph::assortativity_degree(graph),
      "small_world" = calculate_small_worldness(graph),
      NA  # Default if metric not recognized
    )
  }
  
  return(results)
}

#' Calculate node-level network metrics
#' 
#' @param graph igraph object
#' @param metrics Vector of metric names to calculate
#' @return Data frame of node metrics
#' @export
calculate_node_metrics <- function(graph, metrics = NULL) {
  # Initialize node names
  node_names <- igraph::V(graph)$name
  if (is.null(node_names)) {
    node_names <- paste0("Node", 1:igraph::vcount(graph))
  }
  
  # Initialize results data frame
  results <- data.frame(Node = node_names, stringsAsFactors = FALSE)
  
  # If no metrics specified, calculate all
  if (is.null(metrics)) {
    metrics <- unlist(NETWORK_METRICSNode-level`)
  }
  
  # Ensure graph is undirected and has positive weights
  graph <- igraph::as.undirected(graph, mode = "collapse")
  
  # Extract edge weights if they exist
  edge_weights <- NULL
  if ("weight" %in% igraph::edge_attr_names(graph)) {
    edge_weights <- igraph::E(graph)$weight
  }
  
  # Calculate requested metrics
  for (metric in metrics) {
    results[[metric]] <- switch(
      metric,
      "degree" = igraph::degree(graph),
      "betweenness" = igraph::betweenness(graph, weights = edge_weights),
      "closeness" = igraph::closeness(graph, weights = edge_weights),
      "eigenvector" = igraph::eigen_centrality(graph, weights = edge_weights)$vector,
      "local_clustering" = igraph::transitivity(graph, type = "local", isolates = "zero"),
      "participation" = calculate_participation_coefficient(graph),
      rep(NA, igraph::vcount(graph))  # Default if metric not recognized
    )
  }
  
  return(results)
}

#' Calculate mean shortest path length for connected components
#' 
#' @param graph igraph object
#' @return Mean shortest path length
#' @export
mean_shortest_path_length <- function(graph) {
  # Check if graph is connected
  if (igraph::is_connected(graph)) {
    # Calculate average path length directly
    return(igraph::average_path_length(graph, directed = FALSE))
  } else {
    # Find connected components
    components <- igraph::decompose(graph)
    
    # Calculate average path length for each component
    component_lengths <- sapply(components, function(c) {
      if (igraph::vcount(c) > 1) {
        return(igraph::average_path_length(c, directed = FALSE))
      } else {
        return(0)  # Single-node components have path length 0
      }
    })
    
    # Calculate weighted average based on component size
    component_sizes <- sapply(components, igraph::vcount)
    total_nodes <- sum(component_sizes)
    
    # Exclude single-node components from calculation
    valid_components <- component_sizes > 1
    
    if (sum(valid_components) > 0) {
      weighted_sum <- sum(component_lengths[valid_components] * 
                          component_sizes[valid_components])
      weighted_avg <- weighted_sum / sum(component_sizes[valid_components])
      return(weighted_avg)
    } else {
      return(NA)  # No valid components with more than one node
    }
  }
}

#' Calculate modularity using Louvain algorithm
#' 
#' @param graph igraph object
#' @return Modularity value
#' @export
calculate_modularity <- function(graph) {
  # Check if graph has at least one edge
  if (igraph::ecount(graph) == 0) {
    return(NA)
  }
  
  # Extract edge weights if they exist
  edge_weights <- NULL
  if ("weight" %in% igraph::edge_attr_names(graph)) {
    edge_weights <- igraph::E(graph)$weight
  }
  
  # Find communities using Louvain algorithm
  communities <- igraph::cluster_louvain(graph, weights = edge_weights)
  
  # Calculate modularity
  modularity <- igraph::modularity(communities)
  
  return(modularity)
}

#' Calculate small-worldness of network
#' 
#' @param graph igraph object
#' @param num_rewired Number of random networks to generate
#' @return Small-worldness coefficient
#' @export
calculate_small_worldness <- function(graph, num_rewired = 10) {
  # Check if graph has at least one edge
  if (igraph::ecount(graph) == 0) {
    return(NA)
  }
  
  # Calculate clustering coefficient and path length of original graph
  C <- igraph::transitivity(graph, type = "global")
  L <- mean_shortest_path_length(graph)
  
  # Initialize values for random networks
  C_rand <- numeric(num_rewired)
  L_rand <- numeric(num_rewired)
  
  # Generate random networks with same degree distribution
  for (i in 1:num_rewired) {
    # Create rewired graph
    rewired <- igraph::rewire(graph, igraph::keeping_degseq(niter = igraph::ecount(graph) * 10))
    
    # Calculate metrics
    C_rand[i] <- igraph::transitivity(rewired, type = "global")
    L_rand[i] <- mean_shortest_path_length(rewired)
  }
  
  # Calculate averages
  C_rand_mean <- mean(C_rand, na.rm = TRUE)
  L_rand_mean <- mean(L_rand, na.rm = TRUE)
  
  # Calculate small-worldness
  if (C_rand_mean > 0 && L_rand_mean > 0) {
    small_worldness <- (C / C_rand_mean) / (L / L_rand_mean)
    return(small_worldness)
  } else {
    return(NA)
  }
}

#' Calculate participation coefficient for nodes
#' 
#' @param graph igraph object
#' @return Vector of participation coefficients
#' @export
calculate_participation_coefficient <- function(graph) {
  # Check if graph has at least one edge
  if (igraph::ecount(graph) == 0) {
    return(rep(0, igraph::vcount(graph)))
  }
  
  # Extract edge weights if they exist
  edge_weights <- NULL
  if ("weight" %in% igraph::edge_attr_names(graph)) {
    edge_weights <- igraph::E(graph)$weight
  }
  
  # Find communities
  communities <- igraph::cluster_louvain(graph, weights = edge_weights)
  
  # Get community membership
  membership <- igraph::membership(communities)
  
  # Calculate participation coefficient for each node
  n_nodes <- igraph::vcount(graph)
  participation <- numeric(n_nodes)
  
  for (i in 1:n_nodes) {
    # Get all neighbors of the current node
    neighbors <- igraph::neighbors(graph, i)
    
    # Get total degree of the node
    k_i <- length(neighbors)
    
    # If node has no connections, participation coefficient is 0
    if (k_i == 0) {
      participation[i] <- 0
      next
    }
    
    # Get communities of neighbors
    neighbor_communities <- membership[neighbors]
    
    # Count connections to each community
    community_counts <- table(neighbor_communities)
    
    # Calculate sum of squared proportions
    sum_squared_proportions <- sum((community_counts / k_i)^2)
    
    # Calculate participation coefficient
    participation[i] <- 1 - sum_squared_proportions
  }
  
  return(participation)
}

#' Detect communities in a network
#' 
#' @param graph igraph object
#' @param method Community detection method
#' @param min_size Minimum community size
#' @return Community object with additional information
#' @export
detect_communities <- function(graph, method = "louvain", min_size = 3) {
  # Check if graph has at least one edge
  if (igraph::ecount(graph) == 0) {
    return(NULL)
  }
  
  # Extract edge weights if they exist
  edge_weights <- NULL
  if ("weight" %in% igraph::edge_attr_names(graph)) {
    edge_weights <- igraph::E(graph)$weight
  }
  
  # Detect communities using the specified method
  communities <- switch(
    method,
    "louvain" = igraph::cluster_louvain(graph, weights = edge_weights),
    "walktrap" = igraph::cluster_walktrap(graph, weights = edge_weights),
    "fast_greedy" = igraph::cluster_fast_greedy(graph, weights = edge_weights),
    "edge_betweenness" = igraph::cluster_edge_betweenness(graph, weights = edge_weights),
    "leading_eigen" = igraph::cluster_leading_eigen(graph, weights = edge_weights),
    "spinglass" = igraph::cluster_spinglass(graph, weights = edge_weights),
    igraph::cluster_louvain(graph, weights = edge_weights)  # Default
  )
  
  # Calculate community sizes
  sizes <- igraph::sizes(communities)
  
  # Create lookup table for community names
  community_names <- rep(NA, length(sizes))
  for (i in 1:length(sizes)) {
    if (sizes[i] >= min_size) {
      community_names[i] <- paste0("Community ", i)
    } else {
      community_names[i] <- "Small"
    }
  }
  
  # Calculate within-community and between-community connections
  membership <- igraph::membership(communities)
  n_communities <- length(sizes)
  
  # Create connection matrix
  connection_matrix <- matrix(0, nrow = n_communities, ncol = n_communities)
  rownames(connection_matrix) <- colnames(connection_matrix) <- paste0("Comm", 1:n_communities)
  
  # Count connections
  for (e in igraph::E(graph)) {
    v1 <- igraph::ends(graph, e)[1]
    v2 <- igraph::ends(graph, e)[2]
    
    comm1 <- membership[v1]
    comm2 <- membership[v2]
    
    # Add connection weight
    weight <- 1
    if (!is.null(edge_weights)) {
      weight <- edge_weights[igraph::get.edge.ids(graph, c(v1, v2))]
    }
    
    connection_matrix[comm1, comm2] <- connection_matrix[comm1, comm2] + weight
    connection_matrix[comm2, comm1] <- connection_matrix[comm2, comm1] + weight
  }
  
  # Calculate community centroids (for visualization)
  centroids <- list()
  for (i in 1:n_communities) {
    # Get nodes in this community
    community_nodes <- which(membership == i)
    
    # Calculate centroid
    if (length(community_nodes) > 0) {
      centroids[[i]] <- list(
        x = mean(igraph::V(graph)[community_nodes]$x),
        y = mean(igraph::V(graph)[community_nodes]$y)
      )
    } else {
      centroids[[i]] <- list(x = 0, y = 0)
    }
  }
  
  # Return results
  result <- list(
    communities = communities,
    membership = membership,
    sizes = sizes,
    names = community_names,
    connection_matrix = connection_matrix,
    centroids = centroids,
    modularity = igraph::modularity(communities)
  )
  
  return(result)
}

#' Identify hub nodes in a network
#' 
#' @param graph igraph object
#' @param node_metrics Data frame with node metrics
#' @param primary_metric Primary metric for hub identification
#' @param threshold Percentile threshold for hub identification
#' @param use_multivariate Whether to use multiple metrics
#' @param secondary_metrics Additional metrics for multivariate hub identification
#' @return List containing hub information
#' @export
identify_hubs <- function(graph, node_metrics, primary_metric = "degree", 
                          threshold = 0.8, use_multivariate = FALSE,
                          secondary_metrics = NULL) {
  # Check if graph is valid
  if (is.null(graph) || igraph::vcount(graph) == 0) {
    return(NULL)
  }
  
  # Check if metrics are available
  if (!primary_metric %in% names(node_metrics)) {
    return(NULL)
  }
  
  # Extract primary metric
  primary_values <- node_metrics[[primary_metric]]
  
  # Calculate threshold value
  threshold_value <- quantile(primary_values, threshold)
  
  # Identify hub nodes based on primary metric
  hub_nodes <- which(primary_values >= threshold_value)
  
  # Refine hubs using multivariate criteria if requested
  if (use_multivariate && !is.null(secondary_metrics) && length(secondary_metrics) > 0) {
    # Check if secondary metrics are available
    available_metrics <- secondary_metrics[secondary_metrics %in% names(node_metrics)]
    
    if (length(available_metrics) > 0) {
      # For each node in the initial hub set, check if it meets secondary criteria
      refined_hubs <- hub_nodes
      
      for (metric in available_metrics) {
        # Calculate threshold value for this metric
        metric_values <- node_metrics[[metric]]
        metric_threshold <- quantile(metric_values, threshold)
        
        # Update hub set
        refined_hubs <- intersect(refined_hubs, which(metric_values >= metric_threshold))
      }
      
      hub_nodes <- refined_hubs
    }
  }
  
  # Create hub scores data frame
  hub_scores <- data.frame(
    Node = node_metrics$Node,
    Primary_Score = primary_values,
    Is_Hub = 1:nrow(node_metrics) %in% hub_nodes,
    stringsAsFactors = FALSE
  )
  
  # Add secondary scores if used
  if (use_multivariate && !is.null(secondary_metrics) && length(secondary_metrics) > 0) {
    for (metric in secondary_metrics) {
      if (metric %in% names(node_metrics)) {
        hub_scores[[paste0(metric, "_Score")]] <- node_metrics[[metric]]
      }
    }
  }
  
  # Create hub subgraph
  hub_subgraph <- NULL
  if (length(hub_nodes) > 0) {
    hub_subgraph <- igraph::induced_subgraph(graph, hub_nodes)
  }
  
  # Calculate hub connectivity statistics
  hub_connectivity <- NULL
  if (!is.null(hub_subgraph) && igraph::vcount(hub_subgraph) > 0) {
    hub_connectivity <- list(
      density = igraph::edge_density(hub_subgraph),
      clustering = igraph::transitivity(hub_subgraph, type = "global"),
      path_length = mean_shortest_path_length(hub_subgraph)
    )
  }
  
  # Return results
  result <- list(
    hub_nodes = hub_nodes,
    hub_scores = hub_scores,
    hub_subgraph = hub_subgraph,
    hub_connectivity = hub_connectivity,
    primary_metric = primary_metric,
    threshold = threshold,
    threshold_value = threshold_value
  )
  
  return(result)
}
