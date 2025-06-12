#########################################################
# Enhanced Brain Network Analysis Shiny App
# utils/network_functions.R - Network analysis utility functions
#########################################################

#' Create a correlation matrix from brain region data
#' 
#' @param data Data frame containing brain region measurements
#' @param region_columns Vector of column names for brain regions
#' @param method Correlation method ("pearson", "spearman", or "kendall")
#' @param use How to handle missing values in the correlation calculation
#' @return A correlation matrix
#' @export
create_correlation_matrix <- function(data, region_columns, 
                                     method = "pearson", 
                                     use = "pairwise.complete.obs") {
  # Extract region data
  region_data <- data[, region_columns, drop = FALSE]
  
  # Calculate correlation matrix
  cor_matrix <- cor(region_data, method = method, use = use)
  
  return(cor_matrix)
}

#' Convert a correlation matrix to an igraph network
#' 
#' @param cor_matrix Correlation matrix
#' @param threshold Correlation threshold for edge inclusion
#' @param use_absolute Whether to use absolute correlation values
#' @return An igraph network object
#' @export
correlation_to_network <- function(cor_matrix, threshold = 0.3, use_absolute = TRUE) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  # Create adjacency matrix from correlation matrix
  adj_matrix <- cor_matrix
  diag(adj_matrix) <- 0  # Remove self-connections
  
  # Apply threshold
  if (use_absolute) {
    adj_matrix <- abs(adj_matrix)
    adj_matrix[adj_matrix < threshold] <- 0
  } else {
    adj_matrix[adj_matrix < threshold] <- 0
  }
  
  # Create weighted graph
  g <- igraph::graph_from_adjacency_matrix(
    adj_matrix,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
  
  return(g)
}

#' Calculate global network metrics
#' 
#' @param g An igraph network object
#' @param group Group name
#' @return A data frame with global network metrics
#' @export
calculate_global_metrics <- function(g, group = "All") {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  # Check if graph is valid
  if (igraph::vcount(g) < 3) {
    warning("Graph has fewer than 3 nodes, some metrics may be undefined")
  }
  
  # Initialize results
  metrics <- data.frame(
    Group = group,
    Node_Count = igraph::vcount(g),
    Edge_Count = igraph::ecount(g),
    Density = 0,
    Global_Clustering = 0,
    Diameter = 0,
    Avg_Path_Length = 0,
    Modularity = 0,
    Assortativity = 0,
    Small_World_Coefficient = 0,
    stringsAsFactors = FALSE
  )
  
  # Calculate metrics, handling potential errors
  tryCatch({
    metrics$Density <- igraph::edge_density(g)
  }, error = function(e) {
    metrics$Density <- NA
  })
  
  tryCatch({
    metrics$Global_Clustering <- igraph::transitivity(g, type = "global")
  }, error = function(e) {
    metrics$Global_Clustering <- NA
  })
  
  # Only calculate diameter and average path length if the graph is connected
  if (igraph::is_connected(g)) {
    tryCatch({
      metrics$Diameter <- igraph::diameter(g)
      metrics$Avg_Path_Length <- igraph::average.path.length(g)
    }, error = function(e) {
      metrics$Diameter <- NA
      metrics$Avg_Path_Length <- NA
    })
  } else {
    # Get largest connected component
    components <- igraph::components(g)
    largest_comp_idx <- which.max(components$csize)
    largest_comp_nodes <- which(components$membership == largest_comp_idx)
    subgraph <- igraph::induced_subgraph(g, largest_comp_nodes)
    
    # Calculate metrics on largest component
    tryCatch({
      metrics$Diameter <- igraph::diameter(subgraph)
      metrics$Avg_Path_Length <- igraph::average.path.length(subgraph)
    }, error = function(e) {
      metrics$Diameter <- NA
      metrics$Avg_Path_Length <- NA
    })
    
    metrics$Largest_Component_Size <- length(largest_comp_nodes)
    metrics$Largest_Component_Percentage <- length(largest_comp_nodes) / igraph::vcount(g)
  }
  
  # Calculate community structure and modularity
  tryCatch({
    comm <- igraph::cluster_louvain(g)
    metrics$Modularity <- igraph::modularity(comm)
    metrics$Community_Count <- length(unique(igraph::membership(comm)))
  }, error = function(e) {
    metrics$Modularity <- NA
    metrics$Community_Count <- NA
  })
  
  # Calculate assortativity
  tryCatch({
    metrics$Assortativity <- igraph::assortativity_degree(g)
  }, error = function(e) {
    metrics$Assortativity <- NA
  })
  
  # Calculate small-world coefficient if possible
  if (requireNamespace("igraph", quietly = TRUE) && igraph::ecount(g) > 0) {
    tryCatch({
      # Generate random graph with same number of nodes and edges
      random_g <- igraph::erdos.renyi.game(igraph::vcount(g), igraph::ecount(g), type = "gnm")
      
      # Calculate clustering and path length ratios
      C_ratio <- igraph::transitivity(g, type = "global") / 
                 igraph::transitivity(random_g, type = "global")
      
      if (igraph::is_connected(g) && igraph::is_connected(random_g)) {
        L_ratio <- igraph::average.path.length(g) / 
                   igraph::average.path.length(random_g)
        
        # Small-world coefficient
        metrics$Small_World_Coefficient <- C_ratio / L_ratio
      } else {
        metrics$Small_World_Coefficient <- NA
      }
    }, error = function(e) {
      metrics$Small_World_Coefficient <- NA
    })
  }
  
  return(metrics)
}

#' Calculate node-level network metrics
#' 
#' @param g An igraph network object
#' @param group Group name
#' @param metrics Vector of metric names to calculate
#' @return A data frame with node-level metrics
#' @export
calculate_node_metrics <- function(g, group = "All", 
                                  metrics = c("degree", "betweenness", "closeness", 
                                              "eigenvector", "local_clustering", 
                                              "participation")) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  # Get node names
  node_names <- igraph::V(g)$name
  if (is.null(node_names)) {
    node_names <- paste0("Node", 1:igraph::vcount(g))
  }
  
  # Initialize results data frame
  results <- data.frame(
    Node = node_names,
    Group = group,
    stringsAsFactors = FALSE
  )
  
  # Calculate metrics
  if ("degree" %in% metrics) {
    results$Degree <- igraph::degree(g)
  }
  
  if ("betweenness" %in% metrics) {
    tryCatch({
      results$Betweenness <- igraph::betweenness(g, normalized = TRUE)
    }, error = function(e) {
      results$Betweenness <- NA
    })
  }
  
  if ("closeness" %in% metrics) {
    tryCatch({
      results$Closeness <- igraph::closeness(g, normalized = TRUE)
    }, error = function(e) {
      results$Closeness <- NA
    })
  }
  
  if ("eigenvector" %in% metrics) {
    tryCatch({
      results$Eigenvector <- igraph::eigen_centrality(g)$vector
    }, error = function(e) {
      results$Eigenvector <- NA
    })
  }
  
  if ("local_clustering" %in% metrics) {
    tryCatch({
      results$Local_Clustering <- igraph::transitivity(g, type = "local", isolates = "zero")
    }, error = function(e) {
      results$Local_Clustering <- NA
    })
  }
  
  # If we want participation coefficient, we need community structure
  if ("participation" %in% metrics) {
    tryCatch({
      # Detect communities using Louvain algorithm
      comm <- igraph::cluster_louvain(g)
      membership <- igraph::membership(comm)
      
      # Calculate participation coefficient for each node
      participation <- numeric(igraph::vcount(g))
      
      for (i in 1:igraph::vcount(g)) {
        # Get neighbors
        neighbors <- igraph::neighbors(g, i)
        if (length(neighbors) > 0) {
          # Get communities of neighbors
          neighbor_comms <- membership[neighbors]
          
          # Count connections to each community
          comm_counts <- table(neighbor_comms)
          
          # Calculate participation coefficient
          k_i <- length(neighbors)
          sum_term <- sum((comm_counts / k_i)^2)
          participation[i] <- 1 - sum_term
        } else {
          participation[i] <- 0
        }
      }
      
      results$Participation_Coefficient <- participation
      results$Community <- membership
      
    }, error = function(e) {
      results$Participation_Coefficient <- NA
      results$Community <- NA
    })
  }
  
  return(results)
}

#' Calculate brain area-level network metrics
#' 
#' @param g An igraph network object
#' @param group Group name
#' @param area_mapping List mapping regions to brain areas
#' @return A data frame with area-level metrics
#' @export
calculate_area_metrics <- function(g, group = "All", area_mapping) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
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
  
  # Get unique areas
  unique_areas <- unique(node_areas)
  
  # Initialize results
  results <- data.frame(
    Area = unique_areas,
    Group = group,
    Node_Count = NA,
    Internal_Edges = NA,
    External_Edges = NA,
    Internal_Density = NA,
    Hub_Score = NA,
    Authority_Score = NA,
    stringsAsFactors = FALSE
  )
  
  # Calculate metrics for each area
  for (i in 1:length(unique_areas)) {
    area <- unique_areas[i]
    area_nodes <- which(node_areas == area)
    
    # Count nodes
    results$Node_Count[i] <- length(area_nodes)
    
    # Skip if area has fewer than 2 nodes
    if (length(area_nodes) < 2) {
      results$Internal_Edges[i] <- 0
      results$External_Edges[i] <- sum(igraph::degree(g, v = area_nodes))
      results$Internal_Density[i] <- NA
      next
    }
    
    # Extract subgraph for this area
    subgraph <- igraph::induced_subgraph(g, area_nodes)
    
    # Count internal edges
    results$Internal_Edges[i] <- igraph::ecount(subgraph)
    
    # Calculate internal density
    results$Internal_Density[i] <- igraph::edge_density(subgraph)
    
    # Count external edges
    total_edges <- sum(igraph::degree(g, v = area_nodes))
    results$External_Edges[i] <- total_edges - (2 * igraph::ecount(subgraph))
  }
  
  # Calculate hub and authority scores if possible
  tryCatch({
    hub_scores <- igraph::hub_score(g)$vector
    auth_scores <- igraph::authority_score(g)$vector
    
    # Average scores by area
    for (i in 1:length(unique_areas)) {
      area <- unique_areas[i]
      area_nodes <- which(node_areas == area)
      
      if (length(area_nodes) > 0) {
        results$Hub_Score[i] <- mean(hub_scores[area_nodes])
        results$Authority_Score[i] <- mean(auth_scores[area_nodes])
      }
    }
  }, error = function(e) {
    # If calculation fails, set to NA
    results$Hub_Score <- NA
    results$Authority_Score <- NA
  })
  
  return(results)
}

#' Calculate inter-area connectivity
#' 
#' @param g An igraph network object
#' @param group Group name
#' @param area_mapping List mapping regions to brain areas
#' @return A data frame with inter-area connectivity metrics
#' @export
calculate_inter_area_connectivity <- function(g, group = "All", area_mapping) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
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
  
  # Get unique areas
  unique_areas <- unique(node_areas)
  
  # Create adjacency matrix
  adj_matrix <- igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
  
  # Initialize results matrix
  n_areas <- length(unique_areas)
  connectivity_matrix <- matrix(0, nrow = n_areas, ncol = n_areas)
  rownames(connectivity_matrix) <- unique_areas
  colnames(connectivity_matrix) <- unique_areas
  
  # Calculate inter-area connectivity
  for (i in 1:n_areas) {
    area_i <- unique_areas[i]
    nodes_i <- which(node_areas == area_i)
    
    for (j in i:n_areas) {
      area_j <- unique_areas[j]
      nodes_j <- which(node_areas == area_j)
      
      if (i == j) {
        # For same area, calculate internal connectivity
        if (length(nodes_i) > 1) {
          # Get the submatrix for this area
          submatrix <- adj_matrix[nodes_i, nodes_i]
          # Sum and divide by number of possible connections
          n_possible <- length(nodes_i) * (length(nodes_i) - 1) / 2
          if (n_possible > 0) {
            connectivity_matrix[i, j] <- sum(submatrix) / (2 * n_possible)
          }
        }
      } else {
        # For different areas, calculate inter-area connectivity
        if (length(nodes_i) > 0 && length(nodes_j) > 0) {
          # Get the cross-area connections
          cross_matrix <- adj_matrix[nodes_i, nodes_j, drop = FALSE]
          # Sum and divide by number of possible connections
          n_possible <- length(nodes_i) * length(nodes_j)
          if (n_possible > 0) {
            connectivity_matrix[i, j] <- sum(cross_matrix) / n_possible
            connectivity_matrix[j, i] <- connectivity_matrix[i, j]  # Make symmetric
          }
        }
      }
    }
  }
  
  # Convert to data frame in long format for easier plotting
  result_df <- reshape2::melt(connectivity_matrix, varnames = c("Area1", "Area2"), 
                              value.name = "Connectivity")
  result_df$Group <- group
  
  return(result_df)
}
