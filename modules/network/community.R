#########################################################
# Enhanced Brain Network Analysis Shiny App
# networks/community.R - Community detection algorithms
#########################################################

#' Detect communities in a brain network
#' 
#' @param graph igraph object representing the brain network
#' @param method Community detection method to use
#' @param weights Edge attribute to use as weights (NULL for unweighted)
#' @param resolution Resolution parameter for modularity-based methods
#' @param seed Random seed for reproducibility
#' @return Community structure object
#' @export
detect_communities <- function(graph, method = "louvain", weights = "weight", 
                             resolution = 1.0, seed = 42) {
  # Check if graph is valid
  if (!igraph::is_igraph(graph)) {
    stop("Input must be an igraph object")
  }
  
  # Set random seed for reproducibility
  set.seed(seed)
  
  # Check if weights exist if specified
  if (!is.null(weights) && !weights %in% igraph::edge_attr_names(graph)) {
    warning("Specified weight attribute not found. Using unweighted graph.")
    weights <- NULL
  }
  
  # Execute the selected community detection method
  community <- switch(
    tolower(method),
    "louvain" = igraph::cluster_louvain(graph, weights = weights, resolution = resolution),
    "leiden" = {
      if (requireNamespace("leidenAlg", quietly = TRUE)) {
        leidenAlg::leiden.community(graph, weights = weights, resolution_parameter = resolution)
      } else {
        warning("Package 'leidenAlg' not available. Using Louvain method instead.")
        igraph::cluster_louvain(graph, weights = weights, resolution = resolution)
      }
    },
    "walktrap" = igraph::cluster_walktrap(graph, weights = weights),
    "infomap" = igraph::cluster_infomap(graph, e.weights = if (is.null(weights)) NULL else 
                                      igraph::E(graph)[[weights]]),
    "fast_greedy" = igraph::cluster_fast_greedy(graph, weights = weights),
    "label_prop" = igraph::cluster_label_prop(graph, weights = weights),
    "edge_betweenness" = igraph::cluster_edge_betweenness(graph, weights = weights),
    "optimal" = {
      if (igraph::vcount(graph) > 50) {
        warning("Optimal community detection is very slow for large networks. Using Louvain method instead.")
        igraph::cluster_louvain(graph, weights = weights)
      } else {
        igraph::cluster_optimal(graph, weights = weights)
      }
    },
    "hierarchical" = {
      # Calculate distance matrix (1 - abs(correlation))
      if (!is.null(weights)) {
        adj_matrix <- igraph::as_adjacency_matrix(graph, attr = weights, sparse = FALSE)
        dist_matrix <- 1 - abs(adj_matrix)
        diag(dist_matrix) <- 0
        
        # Hierarchical clustering
        hc <- hclust(as.dist(dist_matrix), method = "ward.D2")
        
        # Cut tree at a point that optimizes modularity
        optimal_k <- find_optimal_clusters(graph, hc, weights = weights)
        
        # Create communities from hierarchical clustering
        membership <- cutree(hc, k = optimal_k)
        communities <- igraph::make_clusters(graph, membership = membership)
        communities$algorithm <- "hierarchical"
        communities
      } else {
        # Use default edge betweenness for unweighted graphs
        igraph::cluster_edge_betweenness(graph)
      }
    },
    "spinglass" = {
      if (igraph::is_connected(graph)) {
        igraph::cluster_spinglass(graph, weights = weights)
      } else {
        warning("Spinglass requires a connected graph. Using Louvain method instead.")
        igraph::cluster_louvain(graph, weights = weights, resolution = resolution)
      }
    },
    # Default to Louvain method
    igraph::cluster_louvain(graph, weights = weights, resolution = resolution)
  )
  
  return(community)
}

#' Find the optimal number of clusters for hierarchical clustering
#' 
#' @param graph igraph object
#' @param hclust_obj Hierarchical clustering object
#' @param weights Edge attribute to use as weights
#' @param max_k Maximum number of clusters to consider
#' @return Optimal number of clusters
#' @export
find_optimal_clusters <- function(graph, hclust_obj, weights = "weight", max_k = 10) {
  if (!inherits(hclust_obj, "hclust")) {
    stop("hclust_obj must be a hierarchical clustering object")
  }
  
  # Get maximum possible number of clusters
  n <- length(hclust_obj$order)
  max_k <- min(max_k, n)
  
  # Calculate modularity for different numbers of clusters
  modularity_values <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    membership <- cutree(hclust_obj, k = k)
    communities <- igraph::make_clusters(graph, membership = membership)
    modularity_values[k - 1] <- igraph::modularity(graph, communities, weights = weights)
  }
  
  # Find k with maximum modularity
  optimal_k <- which.max(modularity_values) + 1
  
  return(optimal_k)
}

#' Compare multiple community detection methods
#' 
#' @param graph igraph object representing the brain network
#' @param methods Vector of community detection methods to compare
#' @param weights Edge attribute to use as weights
#' @return List with community structures and comparison metrics
#' @export
compare_community_methods <- function(graph, methods = c("louvain", "infomap", "walktrap"), 
                                    weights = "weight") {
  # Check if graph is valid
  if (!igraph::is_igraph(graph)) {
    stop("Input must be an igraph object")
  }
  
  # Initialize results
  results <- list(
    communities = list(),
    metrics = data.frame(
      Method = character(0),
      NumCommunities = integer(0),
      Modularity = numeric(0),
      SilhouetteScore = numeric(0),
      stringsAsFactors = FALSE
    )
  )
  
  # Run each method
  for (method in methods) {
    # Detect communities
    community <- detect_communities(graph, method = method, weights = weights)
    
    # Store community structure
    results$communities[[method]] <- community
    
    # Calculate evaluation metrics
    num_communities <- length(unique(igraph::membership(community)))
    modularity <- igraph::modularity(community)
    
    # Calculate silhouette score if cluster package is available
    silhouette_score <- NA
    if (requireNamespace("cluster", quietly = TRUE)) {
      # Get distance matrix from graph
      if (!is.null(weights)) {
        adj_matrix <- igraph::as_adjacency_matrix(graph, attr = weights, sparse = FALSE)
        dist_matrix <- 1 - abs(adj_matrix)
        diag(dist_matrix) <- 0
        
        # Calculate silhouette
        silhouette <- cluster::silhouette(
          x = igraph::membership(community),
          dist = as.dist(dist_matrix)
        )
        silhouette_score <- mean(silhouette[, "sil_width"])
      }
    }
    
    # Add metrics to results
    results$metrics <- rbind(
      results$metrics,
      data.frame(
        Method = method,
        NumCommunities = num_communities,
        Modularity = modularity,
        SilhouetteScore = silhouette_score,
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(results)
}

#' Evaluate stability of community detection
#' 
#' @param graph igraph object representing the brain network
#' @param method Community detection method
#' @param weights Edge attribute to use as weights
#' @param n_perturbations Number of perturbations to apply
#' @param perturbation_strength Strength of perturbations (0-1)
#' @param resolution Resolution parameter for modularity-based methods
#' @return List with stability metrics
#' @export
evaluate_community_stability <- function(graph, method = "louvain", weights = "weight",
                                       n_perturbations = 100, perturbation_strength = 0.1,
                                       resolution = 1.0) {
  # Check if graph is valid
  if (!igraph::is_igraph(graph)) {
    stop("Input must be an igraph object")
  }
  
  # Get original community structure
  original_community <- detect_communities(graph, method = method, weights = weights, 
                                         resolution = resolution)
  original_membership <- igraph::membership(original_community)
  
  # Initialize results
  ami_scores <- numeric(n_perturbations)
  nmi_scores <- numeric(n_perturbations)
  ari_scores <- numeric(n_perturbations)
  n_communities <- numeric(n_perturbations)
  
  # Get adjacency matrix
  if (!is.null(weights)) {
    adj_matrix <- igraph::as_adjacency_matrix(graph, attr = weights, sparse = FALSE)
  } else {
    adj_matrix <- igraph::as_adjacency_matrix(graph, sparse = FALSE)
  }
  
  # Perform perturbations
  for (i in 1:n_perturbations) {
    # Create perturbed adjacency matrix
    perturbed_matrix <- adj_matrix
    
    # Add random noise to edge weights
    noise <- matrix(rnorm(nrow(adj_matrix) * ncol(adj_matrix), 
                         mean = 0, 
                         sd = perturbation_strength * mean(abs(adj_matrix))),
                   nrow = nrow(adj_matrix))
    perturbed_matrix <- perturbed_matrix + noise
    
    # Ensure matrix is symmetric
    perturbed_matrix <- (perturbed_matrix + t(perturbed_matrix)) / 2
    diag(perturbed_matrix) <- 0
    
    # Create new graph from perturbed matrix
    perturbed_graph <- igraph::graph_from_adjacency_matrix(
      perturbed_matrix,
      mode = "undirected",
      weighted = TRUE
    )
    
    # Detect communities on perturbed graph
    perturbed_community <- detect_communities(perturbed_graph, method = method, 
                                           weights = "weight", resolution = resolution)
    perturbed_membership <- igraph::membership(perturbed_community)
    
    # Calculate similarity metrics if aricode package is available
    if (requireNamespace("aricode", quietly = TRUE)) {
      ami_scores[i] <- aricode::AMI(original_membership, perturbed_membership)
      nmi_scores[i] <- aricode::NMI(original_membership, perturbed_membership)
      ari_scores[i] <- aricode::ARI(original_membership, perturbed_membership)
    } else {
      # Simple agreement calculation if aricode not available
      contingency <- table(original_membership, perturbed_membership)
      ami_scores[i] <- NA
      nmi_scores[i] <- NA
      ari_scores[i] <- NA
    }
    
    # Record number of communities
    n_communities[i] <- length(unique(perturbed_membership))
  }
  
  # Calculate stability metrics
  stability_results <- list(
    method = method,
    ami_scores = ami_scores,
    nmi_scores = nmi_scores,
    ari_scores = ari_scores,
    n_communities = n_communities,
    mean_ami = mean(ami_scores, na.rm = TRUE),
    mean_nmi = mean(nmi_scores, na.rm = TRUE),
    mean_ari = mean(ari_scores, na.rm = TRUE),
    sd_ami = sd(ami_scores, na.rm = TRUE),
    sd_nmi = sd(nmi_scores, na.rm = TRUE),
    sd_ari = sd(ari_scores, na.rm = TRUE),
    mean_n_communities = mean(n_communities),
    sd_n_communities = sd(n_communities),
    original_n_communities = length(unique(original_membership))
  )
  
  return(stability_results)
}
