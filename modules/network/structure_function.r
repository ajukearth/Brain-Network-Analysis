#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/network/structure_function.R - Structure-function coupling
#########################################################

#' Analyze structure-function coupling between matrices
#' 
#' @param functional_matrix Functional connectivity matrix
#' @param structural_matrix Structural connectivity matrix
#' @param model Structure-function coupling model
#' @param include_indirect Whether to include indirect paths
#' @return List containing structure-function analysis results
#' @export
analyze_structure_function <- function(functional_matrix, structural_matrix, 
                                       model = "direct", include_indirect = TRUE) {
  # Check if matrices are compatible
  if (nrow(functional_matrix) != nrow(structural_matrix) || 
      ncol(functional_matrix) != ncol(structural_matrix)) {
    stop("Functional and structural matrices must have the same dimensions")
  }
  
  # Vectorize upper triangular parts of matrices
  n <- nrow(functional_matrix)
  
  functional_vec <- functional_matrix[upper.tri(functional_matrix)]
  structural_vec <- structural_matrix[upper.tri(structural_matrix)]
  
  # Initialize results
  results <- list(
    model = model,
    include_indirect = include_indirect,
    coupling_metrics = list(),
    model_summary = NULL,
    model_coefficients = NULL,
    predicted_functional = NULL
  )
  
  # Calculate direct structure-function coupling
  direct_correlation <- cor(functional_vec, structural_vec, 
                           method = "spearman", use = "pairwise.complete.obs")
  
  results$coupling_metrics$direct_correlation <- direct_correlation
  
  # Execute the appropriate coupling model
  if (model == "direct") {
    # Direct matching model (simple correlation)
    results$model_summary <- list(
      method = "Direct Correlation",
      correlation = direct_correlation,
      r_squared = direct_correlation^2
    )
    
    # Create a simple linear model
    lm_model <- lm(functional_vec ~ structural_vec)
    results$model_coefficients <- coef(lm_model)
    
    # Create predicted functional connectivity
    predicted_vec <- predict(lm_model)
    predicted_matrix <- matrix(0, n, n)
    predicted_matrix[upper.tri(predicted_matrix)] <- predicted_vec
    predicted_matrix <- predicted_matrix + t(predicted_matrix)
    diag(predicted_matrix) <- 1
    
    results$predicted_functional <- predicted_matrix
    
  } else if (model == "euclidean") {
    # Euclidean distance model
    # Calculate Euclidean distance between regions
    euclidean_dist <- as.matrix(dist(rownames(structural_matrix), method = "euclidean"))
    euclidean_vec <- euclidean_dist[upper.tri(euclidean_dist)]
    
    # Create model with Euclidean distance
    lm_model <- lm(functional_vec ~ structural_vec + euclidean_vec)
    results$model_coefficients <- coef(lm_model)
    
    # Model summary
    results$model_summary <- list(
      method = "Euclidean Distance Model",
      r_squared = summary(lm_model)$r.squared,
      adjusted_r_squared = summary(lm_model)$adj.r.squared,
      p_value = summary(lm_model)$coefficients[2, 4]
    )
    
    # Create predicted functional connectivity
    predicted_vec <- predict(lm_model)
    predicted_matrix <- matrix(0, n, n)
    predicted_matrix[upper.tri(predicted_matrix)] <- predicted_vec
    predicted_matrix <- predicted_matrix + t(predicted_matrix)
    diag(predicted_matrix) <- 1
    
    results$predicted_functional <- predicted_matrix
    
  } else if (model == "communicability") {
    # Communicability model
    # Calculate communicability matrix
    structural_graph <- igraph::graph_from_adjacency_matrix(
      structural_matrix,
      mode = "undirected",
      weighted = TRUE
    )
    
    communicability <- igraph::communicability(structural_graph)
    communicability_vec <- communicability[upper.tri(communicability)]
    
    # Create model with communicability
    lm_model <- lm(functional_vec ~ communicability_vec)
    results$model_coefficients <- coef(lm_model)
    
    # Model summary
    results$model_summary <- list(
      method = "Communicability Model",
      r_squared = summary(lm_model)$r.squared,
      adjusted_r_squared = summary(lm_model)$adj.r.squared,
      p_value = summary(lm_model)$coefficients[2, 4]
    )
    
    # Create predicted functional connectivity
    predicted_vec <- predict(lm_model)
    predicted_matrix <- matrix(0, n, n)
    predicted_matrix[upper.tri(predicted_matrix)] <- predicted_vec
    predicted_matrix <- predicted_matrix + t(predicted_matrix)
    diag(predicted_matrix) <- 1
    
    results$predicted_functional <- predicted_matrix
    
  } else if (model == "shortest_path") {
    # Shortest path model
    # Calculate shortest path lengths
    structural_graph <- igraph::graph_from_adjacency_matrix(
      structural_matrix,
      mode = "undirected",
      weighted = TRUE
    )
    
    # Add small constant to zero weights to make them weak connections instead of no connections
    if (include_indirect) {
      # Create distance matrix from structural weights
      # (use inverse weights since higher weights = stronger connections)
      weights <- 1 / (igraph::E(structural_graph)$weight + 0.001)
      
      # Calculate shortest paths
      path_lengths <- igraph::shortest.paths(
        structural_graph,
        weights = weights
      )
      
      # Set diagonal to 0
      diag(path_lengths) <- 0
      
      # Extract upper triangular part
      path_lengths_vec <- path_lengths[upper.tri(path_lengths)]
      
      # Create model with path lengths
      lm_model <- lm(functional_vec ~ path_lengths_vec)
      results$model_coefficients <- coef(lm_model)
      
      # Model summary
      results$model_summary <- list(
        method = "Shortest Path Model",
        r_squared = summary(lm_model)$r.squared,
        adjusted_r_squared = summary(lm_model)$adj.r.squared,
        p_value = summary(lm_model)$coefficients[2, 4]
      )
      
      # Create predicted functional connectivity
      predicted_vec <- predict(lm_model)
      predicted_matrix <- matrix(0, n, n)
      predicted_matrix[upper.tri(predicted_matrix)] <- predicted_vec
      predicted_matrix <- predicted_matrix + t(predicted_matrix)
      diag(predicted_matrix) <- 1
      
      results$predicted_functional <- predicted_matrix
    } else {
      # If not including indirect paths, just use direct connections
      results$model_summary <- list(
        method = "Direct Connections Only",
        correlation = direct_correlation,
        r_squared = direct_correlation^2
      )
      
      # Create a simple linear model
      lm_model <- lm(functional_vec ~ structural_vec)
      results$model_coefficients <- coef(lm_model)
      
      # Create predicted functional connectivity
      predicted_vec <- predict(lm_model)
      predicted_matrix <- matrix(0, n, n)
      predicted_matrix[upper.tri(predicted_matrix)] <- predicted_vec
      predicted_matrix <- predicted_matrix + t(predicted_matrix)
      diag(predicted_matrix) <- 1
      
      results$predicted_functional <- predicted_matrix
    }
    
  } else if (model == "diffusion") {
    # Diffusion model
    # Calculate diffusion matrix (transition probability matrix)
    structural_graph <- igraph::graph_from_adjacency_matrix(
      structural_matrix,
      mode = "undirected",
      weighted = TRUE
    )
    
    # Calculate transition probabilities
    # First, get adjacency matrix with edge weights
    adjacency <- igraph::as_adjacency_matrix(structural_graph, attr = "weight", sparse = FALSE)
    
    # Create transition matrix by normalizing rows
    transition_matrix <- t(apply(adjacency, 1, function(row) {
      row_sum <- sum(row)
      if (row_sum > 0) row / row_sum else row
    }))
    
    # For diffusion, raise transition matrix to powers
    # This simulates multiple steps of a random walk
    diffusion_steps <- 3  # Number of diffusion steps
    diffusion_matrix <- transition_matrix
    
    for (i in 2:diffusion_steps) {
      diffusion_matrix <- diffusion_matrix %*% transition_matrix
    }
    
    # Extract upper triangular part
    diffusion_vec <- diffusion_matrix[upper.tri(diffusion_matrix)]
    
    # Create model with diffusion probabilities
    lm_model <- lm(functional_vec ~ diffusion_vec)
    results$model_coefficients <- coef(lm_model)
    
    # Model summary
    results$model_summary <- list(
      method = "Diffusion Model",
      r_squared = summary(lm_model)$r.squared,
      adjusted_r_squared = summary(lm_model)$adj.r.squared,
      p_value = summary(lm_model)$coefficients[2, 4]
    )
    
    # Create predicted functional connectivity
    predicted_vec <- predict(lm_model)
    predicted_matrix <- matrix(0, n, n)
    predicted_matrix[upper.tri(predicted_matrix)] <- predicted_vec
    predicted_matrix <- predicted_matrix + t(predicted_matrix)
    diag(predicted_matrix) <- 1
    
    results$predicted_functional <- predicted_matrix
    
  } else if (model == "multiple") {
    # Multiple predictors model
    # Calculate different measures
    
    # 1. Direct structural connectivity
    direct_sc <- structural_vec
    
    # 2. Euclidean distance
    euclidean_dist <- as.matrix(dist(rownames(structural_matrix), method = "euclidean"))
    euclidean_vec <- euclidean_dist[upper.tri(euclidean_dist)]
    
    # 3. Communicability
    structural_graph <- igraph::graph_from_adjacency_matrix(
      structural_matrix,
      mode = "undirected",
      weighted = TRUE
    )
    
    communicability <- igraph::communicability(structural_graph)
    communicability_vec <- communicability[upper.tri(communicability)]
    
    # 4. Shortest paths
    # Add small constant to zero weights to make them weak connections instead of no connections
    weights <- 1 / (igraph::E(structural_graph)$weight + 0.001)
    
    # Calculate shortest paths
    path_lengths <- igraph::shortest.paths(
      structural_graph,
      weights = weights
    )
    
    # Set diagonal to 0
    diag(path_lengths) <- 0
    
    # Extract upper triangular part
    path_lengths_vec <- path_lengths[upper.tri(path_lengths)]
    
    # Create model with multiple predictors
    lm_model <- lm(functional_vec ~ direct_sc + euclidean_vec + 
                    communicability_vec + path_lengths_vec)
    
    results$model_coefficients <- coef(lm_model)
    
    # Model summary
    model_summary <- summary(lm_model)
    results$model_summary <- list(
      method = "Multiple Predictors Model",
      r_squared = model_summary$r.squared,
      adjusted_r_squared = model_summary$adj.r.squared,
      p_values = model_summary$coefficients[, 4]
    )
    
    # Create predicted functional connectivity
    predicted_vec <- predict(lm_model)
    predicted_matrix <- matrix(0, n, n)
    predicted_matrix[upper.tri(predicted_matrix)] <- predicted_vec
    predicted_matrix <- predicted_matrix + t(predicted_matrix)
    diag(predicted_matrix) <- 1
    
    results$predicted_functional <- predicted_matrix
    
    # Additional coupling metrics
    results$coupling_metrics$euclidean_correlation <- cor(functional_vec, euclidean_vec, 
                                                         method = "spearman", use = "pairwise.complete.obs")
    results$coupling_metrics$communicability_correlation <- cor(functional_vec, communicability_vec, 
                                                              method = "spearman", use = "pairwise.complete.obs")
    results$coupling_metrics$path_length_correlation <- cor(functional_vec, path_lengths_vec, 
                                                          method = "spearman", use = "pairwise.complete.obs")
  }
  
  # Calculate mean absolute error (MAE)
  error_matrix <- abs(functional_matrix - results$predicted_functional)
  results$coupling_metrics$mean_absolute_error <- mean(error_matrix[upper.tri(error_matrix)])
  
  # Return results
  return(results)
}
