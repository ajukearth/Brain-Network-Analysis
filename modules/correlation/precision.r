#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/correlation/precision.R - Precision/partial correlation methods
#########################################################

#' Calculate partial correlation matrix
#' 
#' @param data Data frame containing brain region data
#' @param settings List of settings for the correlation calculation
#' @return Partial correlation matrix
#' @export
calculate_partial_correlation <- function(data, settings) {
  # Extract settings
  partial_method <- settings$partial_method
  shrinkage_lambda <- settings$shrinkage_lambda
  glasso_lambda <- settings$glasso_lambda
  
  # Calculate partial correlation based on the selected method
  if (partial_method == "simple") {
    # Use simple method (pseudo-inverse)
    return(calculate_simple_partial_correlation(data))
  } else if (partial_method == "shrinkage") {
    # Use shrinkage method
    return(calculate_shrinkage_correlation(data, list(shrinkage_lambda = shrinkage_lambda)))
  } else if (partial_method == "glasso") {
    # Use graphical LASSO method
    return(calculate_glasso_correlation(data, list(glasso_lambda = glasso_lambda)))
  } else {
    # Default to simple method
    return(calculate_simple_partial_correlation(data))
  }
}

#' Calculate simple partial correlation using pseudo-inverse
#' 
#' @param data Data frame containing brain region data
#' @return Partial correlation matrix
#' @export
calculate_simple_partial_correlation <- function(data) {
  # Calculate covariance matrix
  cov_matrix <- cov(data, use = "pairwise.complete.obs")
  
  # Calculate precision matrix (inverse of covariance matrix)
  # Use MASS::ginv for pseudo-inverse if matrix is singular
  precision_matrix <- tryCatch({
    solve(cov_matrix)
  }, error = function(e) {
    message("Covariance matrix is singular, using pseudo-inverse")
    MASS::ginv(cov_matrix)
  })
  
  # Calculate partial correlation matrix
  n <- ncol(precision_matrix)
  partial_cor <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      partial_cor[i, j] <- -precision_matrix[i, j] / 
        sqrt(precision_matrix[i, i] * precision_matrix[j, j])
    }
  }
  
  # Set diagonal to 1
  diag(partial_cor) <- 1
  
  # Add column and row names
  colnames(partial_cor) <- colnames(data)
  rownames(partial_cor) <- colnames(data)
  
  return(partial_cor)
}

#' Calculate shrinkage precision matrix and partial correlation
#' 
#' @param data Data frame containing brain region data
#' @param settings List of settings for the correlation calculation
#' @return Partial correlation matrix
#' @export
calculate_shrinkage_correlation <- function(data, settings) {
  # Extract settings
  shrinkage_lambda <- settings$shrinkage_lambda
  
  # Load required packages
  if (!requireNamespace("corpcor", quietly = TRUE)) {
    stop("Package 'corpcor' is required for shrinkage estimation")
  }
  
  # Calculate shrinkage estimate of correlation matrix
  # If lambda is provided, use it; otherwise, let corpcor estimate it
  if (!is.null(shrinkage_lambda)) {
    shrink_cor <- corpcor::cor.shrink(data, lambda = shrinkage_lambda)
  } else {
    shrink_cor <- corpcor::cor.shrink(data)
    # Get the estimated lambda for reporting
    shrinkage_lambda <- attr(shrink_cor, "lambda")
    message(paste("Estimated shrinkage intensity (lambda):", shrinkage_lambda))
  }
  
  # Calculate the precision matrix from the shrinkage correlation
  precision_matrix <- tryCatch({
    solve(shrink_cor)
  }, error = function(e) {
    message("Shrinkage correlation matrix is singular, using pseudo-inverse")
    MASS::ginv(shrink_cor)
  })
  
  # Convert precision matrix to partial correlation
  n <- ncol(precision_matrix)
  partial_cor <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      partial_cor[i, j] <- -precision_matrix[i, j] / 
        sqrt(precision_matrix[i, i] * precision_matrix[j, j])
    }
  }
  
  # Set diagonal to 1
  diag(partial_cor) <- 1
  
  # Add column and row names
  colnames(partial_cor) <- colnames(data)
  rownames(partial_cor) <- colnames(data)
  
  return(partial_cor)
}

#' Calculate graphical LASSO precision matrix and partial correlation
#' 
#' @param data Data frame containing brain region data
#' @param settings List of settings for the correlation calculation
#' @return Partial correlation matrix
#' @export
calculate_glasso_correlation <- function(data, settings) {
  # Extract settings
  glasso_lambda <- settings$glasso_lambda
  glasso_lambda_direct <- settings$glasso_lambda_direct
  max_iter <- settings$glasso_max_iter
  
  # Use direct lambda if provided, otherwise use the general one
  if (!is.null(glasso_lambda_direct)) {
    lambda <- glasso_lambda_direct
  } else {
    lambda <- glasso_lambda
  }
  
  # Set max iterations
  if (is.null(max_iter)) {
    max_iter <- 1000
  }
  
  # Load required packages
  if (!requireNamespace("glasso", quietly = TRUE)) {
    stop("Package 'glasso' is required for graphical LASSO estimation")
  }
  
  # Calculate sample covariance matrix
  cov_matrix <- cov(data, use = "pairwise.complete.obs")
  
  # Apply graphical LASSO
  glasso_result <- glasso::glasso(
    cov_matrix,
    rho = lambda,
    maxit = max_iter,
    penalize.diagonal = FALSE
  )
  
  # Extract precision matrix
  precision_matrix <- glasso_result$wi
  
  # Convert precision matrix to partial correlation
  n <- ncol(precision_matrix)
  partial_cor <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      partial_cor[i, j] <- -precision_matrix[i, j] / 
        sqrt(precision_matrix[i, i] * precision_matrix[j, j])
    }
  }
  
  # Set diagonal to 1
  diag(partial_cor) <- 1
  
  # Add column and row names
  colnames(partial_cor) <- colnames(data)
  rownames(partial_cor) <- colnames(data)
  
  return(partial_cor)
}