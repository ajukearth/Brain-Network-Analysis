#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/correlation/distance.R - Distance-based methods
#########################################################

#' Calculate Euclidean distance matrix
#' 
#' @param data Data frame containing brain region data
#' @param settings List of settings for the distance calculation
#' @return Distance matrix (converted to similarity if requested)
#' @export
calculate_euclidean_distance <- function(data, settings) {
  # Extract settings
  normalize <- settings$distance_normalize
  
  # Calculate Euclidean distance matrix
  dist_matrix <- as.matrix(dist(t(as.matrix(data)), method = "euclidean"))
  
  # Set column and row names
  colnames(dist_matrix) <- colnames(data)
  rownames(dist_matrix) <- colnames(data)
  
  # Convert to similarity if requested
  if (normalize) {
    # Find the maximum distance for normalization
    max_dist <- max(dist_matrix)
    
    # Convert to similarity (1 - normalized distance)
    sim_matrix <- 1 - (dist_matrix / max_dist)
    
    # Set diagonal to 1
    diag(sim_matrix) <- 1
    
    return(sim_matrix)
  } else {
    return(dist_matrix)
  }
}

#' Calculate distance correlation matrix
#' 
#' @param data Data frame containing brain region data
#' @param settings List of settings for the distance calculation
#' @return Distance correlation matrix
#' @export
calculate_distance_correlation <- function(data, settings) {
  # Load required packages
  if (!requireNamespace("energy", quietly = TRUE)) {
    stop("Package 'energy' is required for distance correlation")
  }
  
  # Calculate distance correlation matrix
  n <- ncol(data)
  dist_cor <- matrix(0, n, n)
  
  # Set column and row names
  colnames(dist_cor) <- colnames(data)
  rownames(dist_cor) <- colnames(data)
  
  # Calculate distance correlation for each pair
  for (i in 1:n) {
    for (j in i:n) {
      if (i == j) {
        dist_cor[i, j] <- 1
      } else {
        # Calculate distance correlation
        tryCatch({
          dcor_result <- energy::dcor(data[, i], data[, j])
          dist_cor[i, j] <- dist_cor[j, i] <- dcor_result
        }, error = function(e) {
          # If calculation fails, set to NA
          dist_cor[i, j] <- dist_cor[j, i] <- NA
        })
      }
    }
  }
  
  return(dist_cor)
}

#' Calculate Mahalanobis distance matrix
#' 
#' @param data Data frame containing brain region data
#' @param settings List of settings for the distance calculation
#' @return Distance matrix (converted to similarity if requested)
#' @export
calculate_mahalanobis_distance <- function(data, settings) {
  # Extract settings
  normalize <- settings$distance_normalize
  
  # Calculate covariance matrix
  cov_matrix <- cov(data, use = "pairwise.complete.obs")
  
  # Check if covariance matrix is invertible
  tryCatch({
    cov_inv <- solve(cov_matrix)
  }, error = function(e) {
    # If not invertible, use regularized covariance
    message("Covariance matrix is singular, using regularized covariance")
    cov_matrix <- corpcor::cov.shrink(data)
    cov_inv <- solve(cov_matrix)
  })
  
  # Calculate Mahalanobis distance matrix
  n <- ncol(data)
  maha_dist <- matrix(0, n, n)
  
  # Set column and row names
  colnames(maha_dist) <- colnames(data)
  rownames(maha_dist) <- colnames(data)
  
  # Calculate Mahalanobis distance for each pair
  for (i in 1:n) {
    for (j in i:n) {
      if (i == j) {
        maha_dist[i, j] <- 0
      } else {
        # Calculate distance between mean time series
        mean_i <- colMeans(matrix(data[, i], ncol = 1))
        mean_j <- colMeans(matrix(data[, j], ncol = 1))
        diff <- mean_i - mean_j
        
        # Calculate Mahalanobis distance
        tryCatch({
          d <- sqrt(t(diff) %*% cov_inv %*% diff)
          maha_dist[i, j] <- maha_dist[j, i] <- as.numeric(d)
        }, error = function(e) {
          # If calculation fails, set to NA
          maha_dist[i, j] <- maha_dist[j, i] <- NA
        })
      }
    }
  }
  
  # Convert to similarity if requested
  if (normalize) {
    # Find the maximum distance for normalization
    max_dist <- max(maha_dist, na.rm = TRUE)
    
    # Convert to similarity (1 - normalized distance)
    sim_matrix <- 1 - (maha_dist / max_dist)
    
    # Set diagonal to 1
    diag(sim_matrix) <- 1
    
    return(sim_matrix)
  } else {
    return(maha_dist)
  }
}
