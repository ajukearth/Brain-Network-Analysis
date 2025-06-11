#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/network/stability.R - Network stability analysis
#########################################################

#' Run bootstrap analysis for network stability
#' 
#' @param original_data Original data frame with brain region columns
#' @param iterations Number of bootstrap iterations
#' @param sample_percent Percentage of original samples to include in each bootstrap
#' @param calculate_stability Whether to calculate stability coefficients
#' @param correlation_method Correlation method to use
#' @param correlation_settings Correlation settings
#' @param metrics Network metrics to calculate
#' @return List containing bootstrap results
#' @export
run_bootstrap_analysis <- function(original_data, iterations = 1000, sample_percent = 0.8,
                                  calculate_stability = TRUE, correlation_method = "pearson",
                                  correlation_settings = list(), metrics = NULL) {
  # Initialize results
  bootstrap_results <- list(
    global_values = list(),
    node_values = list(),
    global_ci = list(),
    node_ci = list(),
    stability_coefficients = list()
  )
  
  # Calculate sample size
  n <- nrow(original_data)
  sample_size <- floor(n * sample_percent)
  
  # Calculate original metrics
  original_matrix <- switch(
    correlation_method,
    "pearson" = calculate_pearson_correlation(original_data, correlation_settings),
    "spearman" = calculate_spearman_correlation(original_data, correlation_settings),
    "kendall" = calculate_kendall_correlation(original_data, correlation_settings),
    "partial" = calculate_partial_correlation(original_data, correlation_settings),
    "glasso" = calculate_glasso_correlation(original_data, correlation_settings),
    "shrinkage" = calculate_shrinkage_correlation(original_data, correlation_settings),
    "euclidean" = calculate_euclidean_distance(original_data, correlation_settings),
    "distance" = calculate_distance_correlation(original_data, correlation_settings),
    "mahalanobis" = calculate_mahalanobis_distance(original_data, correlation_settings),
    # Default to Pearson if method not recognized
    calculate_pearson_correlation(original_data, correlation_settings)
  )
  
  # Apply thresholding if specified
  if (!is.null(correlation_settings$threshold_type) && correlation_settings$threshold_type != "none") {
    original_matrix <- apply_thresholding(
      original_matrix,
      correlation_settings$threshold_type,
      switch(correlation_settings$threshold_type,
             "absolute" = correlation_settings$absolute_threshold,
             "proportional" = correlation_settings$proportional_threshold,
             "significance" = correlation_settings$significance_threshold,
             0),
      correlation_settings$use_absolute_values,
      correlation_settings$include_negative
    )
  }
  
  # Create graph from original matrix
  original_graph <- matrix_to_graph(original_matrix)
  
  # Calculate original global metrics
  original_global_metrics <- calculate_global_metrics(original_graph, metrics)
  
  # Calculate original node metrics
  original_node_metrics <- calculate_node_metrics(original_graph, metrics)
  
  # Initialize storage for bootstrap metrics
  global_metric_names <- names(original_global_metrics)
  node_metric_names <- setdiff(names(original_node_metrics), "Node")
  
  # Initialize bootstrap storage
  for (metric in global_metric_names) {
    bootstrap_results$global_values[[metric]] <- numeric(iterations)
  }
  
  for (metric in node_metric_names) {
    bootstrap_results$node_values[[metric]] <- matrix(
      0, 
      nrow = iterations, 
      ncol = nrow(original_node_metrics)
    )
  }
  
  # Run bootstrap iterations
  for (i in 1:iterations) {
    # Sample with replacement
    sample_indices <- sample(1:n, size = sample_size, replace = TRUE)
    bootstrap_data <- original_data[sample_indices, ]
    
    # Calculate correlation matrix
    bootstrap_matrix <- switch(
      correlation_method,
      "pearson" = calculate_pearson_correlation(bootstrap_data, correlation_settings),
      "spearman" = calculate_spearman_correlation(bootstrap_data, correlation_settings),
      "kendall" = calculate_kendall_correlation(bootstrap_data, correlation_settings),
      "partial" = calculate_partial_correlation(bootstrap_data, correlation_settings),
      "glasso" = calculate_glasso_correlation(bootstrap_data, correlation_settings),
      "shrinkage" = calculate_shrinkage_correlation(bootstrap_data, correlation_settings),
      "euclidean" = calculate_euclidean_distance(bootstrap_data, correlation_settings),
      "distance" = calculate_distance_correlation(bootstrap_data, correlation_settings),
      "mahalanobis" = calculate_mahalanobis_distance(bootstrap_data, correlation_settings),
      # Default to Pearson if method not recognized
      calculate_pearson_correlation(bootstrap_data, correlation_settings)
    )
    
    # Apply thresholding if specified
    if (!is.null(correlation_settings$threshold_type) && correlation_settings$threshold_type != "none") {
      bootstrap_matrix <- apply_thresholding(
        bootstrap_matrix,
        correlation_settings$threshold_type,
        switch(correlation_settings$threshold_type,
               "absolute" = correlation_settings$absolute_threshold,
               "proportional" = correlation_settings$proportional_threshold,
               "significance" = correlation_settings$significance_threshold,
               0),
        correlation_settings$use_absolute_values,
        correlation_settings$include_negative
      )
    }
    
    # Create graph from bootstrap matrix
    bootstrap_graph <- matrix_to_graph(bootstrap_matrix)
    
    # Calculate bootstrap global metrics
    bootstrap_global_metrics <- calculate_global_metrics(bootstrap_graph, metrics)
    
    # Store global metrics
    for (metric in global_metric_names) {
      bootstrap_results$global_values[[metric]][i] <- bootstrap_global_metrics[[metric]]
    }
    
    # Calculate bootstrap node metrics
    bootstrap_node_metrics <- calculate_node_metrics(bootstrap_graph, metrics)
    
    # Store node metrics
    for (metric in node_metric_names) {
      bootstrap_results$node_values[[metric]][i, ] <- bootstrap_node_metrics[[metric]]
    }
  }
  
  # Calculate confidence intervals for global metrics
  for (metric in global_metric_names) {
    bootstrap_results$global_ci[[metric]] <- quantile(
      bootstrap_results$global_values[[metric]], 
      probs = c(0.025, 0.975),
      na.rm = TRUE
    )
  }
  
  # Calculate confidence intervals for node metrics
  for (metric in node_metric_names) {
    bootstrap_results$node_ci[[metric]] <- t(apply(
      bootstrap_results$node_values[[metric]],
      2,
      function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
    ))
  }
  
  # Calculate stability coefficients if requested
  if (calculate_stability) {
    # Create subsampling proportions
    subsample_proportions <- seq(0.1, 0.9, by = 0.1)
    
    # Initialize storage for stability coefficients
    for (metric in node_metric_names) {
      # Calculate correlation for each subsample proportion
      correlations <- numeric(length(subsample_proportions))
      
      for (j in seq_along(subsample_proportions)) {
        prop <- subsample_proportions[j]
        subsample_size <- floor(n * prop)
        
        # Generate multiple subsamples at this proportion
        n_subsamples <- min(iterations / 10, 100)  # Reduce computational load
        subsample_correlations <- numeric(n_subsamples)
        
        for (k in 1:n_subsamples) {
          # Generate two subsamples
          subsample1_indices <- sample(1:n, size = subsample_size, replace = FALSE)
          subsample2_indices <- sample(1:n, size = subsample_size, replace = FALSE)
          
          # Calculate correlation matrices
          subsample1_data <- original_data[subsample1_indices, ]
          subsample2_data <- original_data[subsample2_indices, ]
          
          subsample1_matrix <- switch(
            correlation_method,
            "pearson" = calculate_pearson_correlation(subsample1_data, correlation_settings),
            "spearman" = calculate_spearman_correlation(subsample1_data, correlation_settings),
            "kendall" = calculate_kendall_correlation(subsample1_data, correlation_settings),
            "partial" = calculate_partial_correlation(subsample1_data, correlation_settings),
            "glasso" = calculate_glasso_correlation(subsample1_data, correlation_settings),
            "shrinkage" = calculate_shrinkage_correlation(subsample1_data, correlation_settings),
            "euclidean" = calculate_euclidean_distance(subsample1_data, correlation_settings),
            "distance" = calculate_distance_correlation(subsample1_data, correlation_settings),
            "mahalanobis" = calculate_mahalanobis_distance(subsample1_data, correlation_settings),
            # Default to Pearson if method not recognized
            calculate_pearson_correlation(subsample1_data, correlation_settings)
          )
          
          subsample2_matrix <- switch(
            correlation_method,
            "pearson" = calculate_pearson_correlation(subsample2_data, correlation_settings),
            "spearman" = calculate_spearman_correlation(subsample2_data, correlation_settings),
            "kendall" = calculate_kendall_correlation(subsample2_data, correlation_settings),
            "partial" = calculate_partial_correlation(subsample2_data, correlation_settings),
            "glasso" = calculate_glasso_correlation(subsample2_data, correlation_settings),
            "shrinkage" = calculate_shrinkage_correlation(subsample2_data, correlation_settings),
            "euclidean" = calculate_euclidean_distance(subsample2_data, correlation_settings),
            "distance" = calculate_distance_correlation(subsample2_data, correlation_settings),
            "mahalanobis" = calculate_mahalanobis_distance(subsample2_data, correlation_settings),
            # Default to Pearson if method not recognized
            calculate_pearson_correlation(subsample2_data, correlation_settings)
          )
          
          # Apply thresholding if specified
          if (!is.null(correlation_settings$threshold_type) && correlation_settings$threshold_type != "none") {
            subsample1_matrix <- apply_thresholding(
              subsample1_matrix,
              correlation_settings$threshold_type,
              switch(correlation_settings$threshold_type,
                     "absolute" = correlation_settings$absolute_threshold,
                     "proportional" = correlation_settings$proportional_threshold,
                     "significance" = correlation_settings$significance_threshold,
                     0),
              correlation_settings$use_absolute_values,
              correlation_settings$include_negative
            )
            
            subsample2_matrix <- apply_thresholding(
              subsample2_matrix,
              correlation_settings$threshold_type,
              switch(correlation_settings$threshold_type,
                     "absolute" = correlation_settings$absolute_threshold,
                     "proportional" = correlation_settings$proportional_threshold,
                     "significance" = correlation_settings$significance_threshold,
                     0),
              correlation_settings$use_absolute_values,
              correlation_settings$include_negative
            )
          }
          
          # Create graphs
          subsample1_graph <- matrix_to_graph(subsample1_matrix)
          subsample2_graph <- matrix_to_graph(subsample2_matrix)
          
          # Calculate metrics
          subsample1_node_metrics <- calculate_node_metrics(subsample1_graph, metric)
          subsample2_node_metrics <- calculate_node_metrics(subsample2_graph, metric)
          
          # Calculate correlation between metrics
          subsample_correlations[k] <- cor(
            subsample1_node_metrics[[metric]],
            subsample2_node_metrics[[metric]],
            use = "pairwise.complete.obs"
          )
        }
        
        # Average the correlations for this proportion
        correlations[j] <- mean(subsample_correlations, na.rm = TRUE)
      }
      
      # Fit a curve to find the proportion at which correlation drops below 0.7
      # This is the correlation stability coefficient
      if (all(is.na(correlations))) {
        bootstrap_results$stability_coefficients[[metric]] <- NA
      } else {
        # Fit a smooth curve
        fit <- loess(correlations ~ subsample_proportions, span = 0.75)
        
        # Find the proportion at which correlation is 0.7
        predicted <- predict(fit, seq(0.1, 0.9, by = 0.01))
        below_07 <- which(predicted < 0.7)
        
        if (length(below_07) > 0) {
          # CS coefficient is the max proportion where correlation >= 0.7
          cs_coefficient <- (min(below_07) - 1) / 100 + 0.1
        } else {
          # All correlations are >= 0.7
          cs_coefficient <- 0.9
        }
        
        bootstrap_results$stability_coefficients[[metric]] <- cs_coefficient
      }
    }
  }
  
  # Return bootstrap results
  return(bootstrap_results)
}

#' Calculate consensus matrix from bootstrap samples
#' 
#' @param bootstrap_matrices List of bootstrap correlation matrices
#' @return Consensus matrix
#' @export
calculate_consensus_matrix <- function(bootstrap_matrices) {
  # Check if at least one matrix is provided
  if (length(bootstrap_matrices) == 0) {
    stop("No bootstrap matrices provided")
  }
  
  # Get dimensions from first matrix
  n <- nrow(bootstrap_matrices[[1]])
  
  # Initialize consensus matrix
  consensus_matrix <- matrix(0, nrow = n, ncol = n)
  
  # Sum all matrices
  for (matrix in bootstrap_matrices) {
    consensus_matrix <- consensus_matrix + matrix
  }
  
  # Divide by number of matrices to get average
  consensus_matrix <- consensus_matrix / length(bootstrap_matrices)
  
  # Set diagonal to 1
  diag(consensus_matrix) <- 1
  
  # Return consensus matrix
  return(consensus_matrix)
}
