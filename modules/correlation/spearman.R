#########################################################
# Enhanced Brain Network Analysis Shiny App
# Correlation/spearman.R - Spearman correlation implementation
#########################################################

#' Calculate Spearman rank correlation matrix for brain regions
#' 
#' @param data Data frame containing brain region measurements
#' @param settings List of settings for correlation calculation:
#'   - use_complete: How to handle missing values ("pairwise" or "complete")
#'   - p_threshold: P-value threshold for significance filtering
#'   - apply_fdr: Whether to apply FDR correction for multiple comparisons
#' @return Correlation matrix
#' @export
calculate_spearman_correlation <- function(data, settings = NULL) {
  # Set default settings if not provided
  if (is.null(settings)) {
    settings <- list(
      use_complete = "pairwise",
      p_threshold = 0.05,
      apply_fdr = TRUE
    )
  }
  
  # Extract settings with defaults
  use_complete <- ifelse(is.null(settings$use_complete), "pairwise", settings$use_complete)
  p_threshold <- ifelse(is.null(settings$p_threshold), 0.05, settings$p_threshold)
  apply_fdr <- ifelse(is.null(settings$apply_fdr), TRUE, settings$apply_fdr)
  
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
  correlation_matrix <- stats::cor(data, method = "spearman", use = use_method)
  
  # Calculate approximate p-values using Fisher's z transformation
  n <- nrow(data)
  p_values <- matrix(NA, nrow = nrow(correlation_matrix), ncol = ncol(correlation_matrix))
  
  for (i in 1:nrow(correlation_matrix)) {
    for (j in 1:ncol(correlation_matrix)) {
      if (i != j) {
        # Approximation for large samples
        t_stat <- correlation_matrix[i, j] * sqrt((n - 2) / (1 - correlation_matrix[i, j]^2))
        p_values[i, j] <- 2 * stats::pt(abs(t_stat), df = n - 2, lower.tail = FALSE)
      } else {
        p_values[i, j] <- 0  # Diagonal elements have p-value of 0
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
  attr(correlation_matrix, "method") <- "spearman"
  attr(correlation_matrix, "settings") <- settings
  
  return(correlation_matrix)
}
