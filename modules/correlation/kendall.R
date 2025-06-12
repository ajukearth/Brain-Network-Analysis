#########################################################
# Enhanced Brain Network Analysis Shiny App
# Correlation/kendall.R - Kendall correlation implementation
#########################################################

#' Calculate Kendall rank correlation matrix for brain regions
#' 
#' @param data Data frame containing brain region measurements
#' @param settings List of settings for correlation calculation:
#'   - use_complete: How to handle missing values ("pairwise" or "complete")
#'   - p_threshold: P-value threshold for significance filtering
#'   - apply_fdr: Whether to apply FDR correction for multiple comparisons
#'   - exact: Whether to compute exact p-values (slow for large datasets)
#' @return Correlation matrix
#' @export
calculate_kendall_correlation <- function(data, settings = NULL) {
  # Set default settings if not provided
  if (is.null(settings)) {
    settings <- list(
      use_complete = "pairwise",
      p_threshold = 0.05,
      apply_fdr = TRUE,
      exact = FALSE
    )
  }
  
  # Extract settings with defaults
  use_complete <- ifelse(is.null(settings$use_complete), "pairwise", settings$use_complete)
  p_threshold <- ifelse(is.null(settings$p_threshold), 0.05, settings$p_threshold)
  apply_fdr <- ifelse(is.null(settings$apply_fdr), TRUE, settings$apply_fdr)
  exact <- ifelse(is.null(settings$exact), FALSE, settings$exact)
  
  # Ensure data is a matrix or data frame
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data frame")
  }
  
  # Extract numeric columns if data is a data frame
  if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) == 0) {
      stop("No numeric columns found in data")
    }
    data <- as.matrix(data[, numeric_cols])
  }
  
  # Handle missing values parameter
  use_method <- ifelse(use_complete == "complete", "complete.obs", "pairwise.complete.obs")
  
  # Calculate correlation matrix
  correlation_matrix <- stats::cor(data, method = "kendall", use = use_method)
  
  # Calculate p-values
  n <- nrow(data)
  p_values <- matrix(NA, nrow = nrow(correlation_matrix), ncol = ncol(correlation_matrix))
  
  # If exact is TRUE, calculate exact p-values (computationally intensive)
  if (exact) {
    # Initialize progress bar for long computations
    ncols <- ncol(data)
    pb <- NULL
    try(pb <- utils::txtProgressBar(min = 0, max = ncols * (ncols - 1) / 2, style = 3), silent = TRUE)
    
    count <- 0
    for (i in 1:(ncol(data) - 1)) {
      for (j in (i + 1):ncol(data)) {
        # Complete cases for this pair
        idx <- stats::complete.cases(data[, c(i, j)])
        if (sum(idx) >= 3) {  # Need at least 3 pairs for Kendall
          # Calculate exact p-value using cor.test
          test_result <- stats::cor.test(data[idx, i], data[idx, j], 
                                         method = "kendall", exact = TRUE)
          p_values[i, j] <- p_values[j, i] <- test_result$p.value
        } else {
          p_values[i, j] <- p_values[j, i] <- NA
        }
        
        # Update progress bar
        count <- count + 1
        if (!is.null(pb)) try(utils::setTxtProgressBar(pb, count), silent = TRUE)
      }
    }
    
    # Close progress bar
    if (!is.null(pb)) try(close(pb), silent = TRUE)
    
    # Set diagonal elements to 0
    diag(p_values) <- 0
  } else {
    # Use approximation for large samples
    for (i in 1:nrow(correlation_matrix)) {
      for (j in 1:ncol(correlation_matrix)) {
        if (i != j) {
          # Approximate using normal distribution
          # Formula based on the asymptotic distribution of Kendall's tau
          z <- 3 * correlation_matrix[i, j] * sqrt(n * (n - 1)) / sqrt(2 * (2 * n + 5))
          p_values[i, j] <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
        } else {
          p_values[i, j] <- 0  # Diagonal elements have p-value of 0
        }
      }
    }
  }
  
  # Apply FDR correction if requested
  if (apply_fdr) {
    # Extract upper triangle of p-values (excluding diagonal)
    p_upper <- p_values[upper.tri(p_values)]
    # Apply FDR correction
    p_adjusted <- stats::p.adjust(p_upper, method = "fdr")
    # Replace upper triangle with adjusted p-values
    p_values[upper.tri(p_values)] <- p_adjusted
    # Make matrix symmetric again
    p_values[lower.tri(p_values)] <- t(p_values)[lower.tri(p_values)]
  }
  
  # Create attribute for p-values
  attr(correlation_matrix, "p_values") <- p_values
  
  # Set non-significant correlations to zero if p_threshold is provided
  if (!is.null(p_threshold) && !is.na(p_threshold)) {
    correlation_matrix[p_values > p_threshold] <- 0
  }
  
  # Add column and row names if missing
  if (is.null(colnames(correlation_matrix))) {
    colnames(correlation_matrix) <- paste0("V", 1:ncol(correlation_matrix))
  }
  if (is.null(rownames(correlation_matrix))) {
    rownames(correlation_matrix) <- colnames(correlation_matrix)
  }
  
  # Additional metadata
  attr(correlation_matrix, "method") <- "kendall"
  attr(correlation_matrix, "settings") <- settings
  
  return(correlation_matrix)
}
