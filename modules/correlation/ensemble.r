#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/correlation/ensemble.R - Ensemble correlation methods
#########################################################

#' Calculate average ensemble of multiple correlation matrices
#' 
#' @param matrices List of correlation matrices
#' @param weights Optional vector of weights for weighted average
#' @return Ensemble correlation matrix
#' @export
calculate_average_ensemble <- function(matrices, weights = NULL) {
  # Check if at least one matrix is provided
  if (length(matrices) == 0) {
    stop("No matrices provided for ensemble")
  }
  
  # Check if all matrices have the same dimensions
  dims <- lapply(matrices, dim)
  if (length(unique(dims)) > 1) {
    stop("All matrices must have the same dimensions")
  }
  
  # Initialize the ensemble matrix
  n <- nrow(matrices[[1]])
  ensemble <- matrix(0, nrow = n, ncol = n)
  
  # Set column and row names
  colnames(ensemble) <- colnames(matrices[[1]])
  rownames(ensemble) <- rownames(matrices[[1]])
  
  # If weights are not provided, use equal weights
  if (is.null(weights)) {
    weights <- rep(1 / length(matrices), length(matrices))
  } else if (length(weights) != length(matrices)) {
    stop("Number of weights must match number of matrices")
  }
  
  # Normalize weights to sum to 1
  weights <- weights / sum(weights)
  
  # Calculate weighted average
  for (i in 1:length(matrices)) {
    ensemble <- ensemble + weights[i] * matrices[[i]]
  }
  
  # Ensure diagonal is 1
  diag(ensemble) <- 1
  
  return(ensemble)
}

#' Calculate rank aggregation ensemble of multiple correlation matrices
#' 
#' @param matrices List of correlation matrices
#' @return Ensemble correlation matrix based on rank aggregation
#' @export
calculate_rank_ensemble <- function(matrices) {
  # Check if at least one matrix is provided
  if (length(matrices) == 0) {
    stop("No matrices provided for ensemble")
  }
  
  # Check if all matrices have the same dimensions
  dims <- lapply(matrices, dim)
  if (length(unique(dims)) > 1) {
    stop("All matrices must have the same dimensions")
  }
  
  # Initialize the ensemble matrix
  n <- nrow(matrices[[1]])
  ensemble <- matrix(0, nrow = n, ncol = n)
  
  # Set column and row names
  colnames(ensemble) <- colnames(matrices[[1]])
  rownames(ensemble) <- rownames(matrices[[1]])
  
  # For each pair of nodes, calculate the rank-based ensemble
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        ensemble[i, j] <- 1  # Diagonal is always 1
      } else {
        # Extract values for this pair from all matrices
        values <- sapply(matrices, function(m) m[i, j])
        
        # Convert to ranks (higher correlation = higher rank)
        ranks <- rank(values)
        
        # Average the ranks and scale to [-1, 1]
        ensemble[i, j] <- (mean(ranks) - 1) / (length(matrices) - 1) * 2 - 1
      }
    }
  }
  
  return(ensemble)
}

#' Calculate consensus ensemble of multiple correlation matrices
#' 
#' @param matrices List of correlation matrices
#' @return Ensemble correlation matrix based on consensus clustering
#' @export
calculate_consensus_ensemble <- function(matrices) {
  # Check if at least one matrix is provided
  if (length(matrices) == 0) {
    stop("No matrices provided for ensemble")
  }
  
  # Check if all matrices have the same dimensions
  dims <- lapply(matrices, dim)
  if (length(unique(dims)) > 1) {
    stop("All matrices must have the same dimensions")
  }
  
  # Initialize the consensus matrix
  n <- nrow(matrices[[1]])
  consensus <- matrix(0, nrow = n, ncol = n)
  
  # Set column and row names
  colnames(consensus) <- colnames(matrices[[1]])
  rownames(consensus) <- rownames(matrices[[1]])
  
  # Threshold each matrix (if value > 0, consider it a connection)
  # Then count how many matrices have a connection for each pair
  for (m in matrices) {
    # Create binary adjacency matrix (1 if connection, 0 if not)
    adj <- ifelse(m > 0, 1, 0)
    diag(adj) <- 0  # Remove self-connections
    
    # Add to consensus matrix
    consensus <- consensus + adj
  }
  
  # Normalize by number of matrices
  consensus <- consensus / length(matrices)
  
  # Set diagonal to 1
  diag(consensus) <- 1
  
  return(consensus)
}
