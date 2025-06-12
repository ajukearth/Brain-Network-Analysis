#########################################################
# Enhanced Brain Network Analysis Shiny App
# preprocess/imputation.R - Missing data imputation methods
#########################################################

#' Apply missing data imputation to brain network data
#' 
#' @param data Data frame with missing values
#' @param method Imputation method to use
#' @param columns Columns to impute (default: all numeric columns)
#' @param group_vars Grouping variables for group-specific imputation
#' @param parallel Whether to use parallel processing for MICE
#' @param seed Random seed for reproducibility
#' @param ... Additional parameters for specific imputation methods
#' @return List with imputed data and imputation metrics
#' @export
impute_missing_data <- function(data, method = "mean", columns = NULL, 
                              group_vars = NULL, parallel = FALSE,
                              seed = 42, ...) {
  # Set random seed for reproducibility
  set.seed(seed)
  
  # Check if data is valid
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Check if columns exist and are numeric
  invalid_cols <- setdiff(columns, names(data))
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }
  
  non_numeric <- columns[!sapply(data[, columns, drop = FALSE], is.numeric)]
  if (length(non_numeric) > 0) {
    stop("Non-numeric columns: ", paste(non_numeric, collapse = ", "))
  }
  
  # Check if there are any missing values
  missing_counts <- colSums(is.na(data[, columns, drop = FALSE]))
  total_missing <- sum(missing_counts)
  
  if (total_missing == 0) {
    message("No missing values found in specified columns.")
    return(list(
      imputed_data = data,
      missing_pattern = NULL,
      metrics = data.frame(
        Column = columns,
        Missing = 0,
        Missing_Percent = 0,
        Imputed = 0,
        stringsAsFactors = FALSE
      )
    ))
  }
  
  # Track original missing positions for metrics
  missing_positions <- is.na(data[, columns, drop = FALSE])
  
  # Initialize results
  imputation_result <- list(
    imputed_data = data,
    method = method,
    missing_pattern = NULL,
    metrics = data.frame(
      Column = columns,
      Missing = missing_counts,
      Missing_Percent = round(100 * missing_counts / nrow(data), 2),
      Imputed = 0,
      stringsAsFactors = FALSE
    )
  )
  
  # Visualize missing data pattern if naniar is available
  if (requireNamespace("naniar", quietly = TRUE)) {
    imputation_result$missing_pattern <- naniar::vis_miss(data[, columns, drop = FALSE])
  }
  
  # Apply the selected imputation method
  # For group-specific imputation, apply method within each group
  if (!is.null(group_vars) && method != "mice") {
    # Ensure group variables exist
    invalid_groups <- setdiff(group_vars, names(data))
    if (length(invalid_groups) > 0) {
      stop("Invalid group variables: ", paste(invalid_groups, collapse = ", "))
    }
    
    # Apply imputation within each group
    group_data <- split(data, interaction(data[, group_vars, drop = FALSE], drop = TRUE))
    
    imputed_groups <- lapply(group_data, function(group_df) {
      # Apply the appropriate imputation method within group
      apply_imputation_method(group_df, method, columns, ...)
    })
    
    # Combine imputed groups back into one data frame
    imputation_result$imputed_data <- do.call(rbind, imputed_groups)
    
    # Restore original row order
    imputation_result$imputed_data <- imputation_result$imputed_data[order(match(
      rownames(imputation_result$imputed_data), 
      rownames(data)
    )), ]
  } else {
    # Apply imputation to entire dataset
    imputation_result$imputed_data <- apply_imputation_method(data, method, columns, 
                                                            group_vars = group_vars,
                                                            parallel = parallel, ...)
  }
  
  # Count imputed values
  imputed_positions <- is.na(data[, columns, drop = FALSE]) & 
                     !is.na(imputation_result$imputed_data[, columns, drop = FALSE])
  
  imputed_counts <- colSums(imputed_positions)
  imputation_result$metrics$Imputed <- imputed_counts
  
  return(imputation_result)
}

#' Helper function to apply specific imputation method
#' 
#' @param data Data frame
#' @param method Imputation method
#' @param columns Columns to impute
#' @param ... Additional parameters
#' @return Data frame with imputed values
#' @export
apply_imputation_method <- function(data, method, columns, ...) {
  imputed_data <- data
  
  switch(
    tolower(method),
    "mean" = {
      for (col in columns) {
        if (any(is.na(data[[col]]))) {
          col_mean <- mean(data[[col]], na.rm = TRUE)
          imputed_data[is.na(data[[col]]), col] <- col_mean
        }
      }
    },
    "median" = {
      for (col in columns) {
        if (any(is.na(data[[col]]))) {
          col_median <- median(data[[col]], na.rm = TRUE)
          imputed_data[is.na(data[[col]]), col] <- col_median
        }
      }
    },
    "mode" = {
      for (col in columns) {
        if (any(is.na(data[[col]]))) {
          # Calculate mode (most frequent value)
          col_table <- table(data[[col]])
          col_mode <- as.numeric(names(col_table)[which.max(col_table)])
          imputed_data[is.na(data[[col]]), col] <- col_mode
        }
      }
    },
    "random" = {
      for (col in columns) {
        if (any(is.na(data[[col]]))) {
          # Sample random values from observed distribution
          observed_values <- data[[col]][!is.na(data[[col]])]
          n_missing <- sum(is.na(data[[col]]))
          random_values <- sample(observed_values, size = n_missing, replace = TRUE)
          
          imputed_data[is.na(data[[col]]), col] <- random_values
        }
      }
    },
    "knn" = {
      if (requireNamespace("VIM", quietly = TRUE)) {
        # Extract numeric data for imputation
        numeric_data <- data[, columns, drop = FALSE]
        
        # Default parameters
        k_value <- list(...)$k
        if (is.null(k_value)) k_value <- 5
        
        # KNN imputation
        imputed_result <- VIM::kNN(numeric_data, k = k_value)
        
        # Replace columns in original data
        imputed_data[, columns] <- imputed_result[, columns]
      } else {
        warning("Package 'VIM' not available. Using mean imputation instead.")
        for (col in columns) {
          if (any(is.na(data[[col]]))) {
            col_mean <- mean(data[[col]], na.rm = TRUE)
            imputed_data[is.na(data[[col]]), col] <- col_mean
          }
        }
      }
    },
    "regression" = {
      if (requireNamespace("mice", quietly = TRUE)) {
        # Use single imputation with method 'norm.predict'
        mice_data <- mice::mice(data[, columns, drop = FALSE], 
                             method = "norm.predict", m = 1, printFlag = FALSE)
        imputed_result <- mice::complete(mice_data)
        
        # Replace columns in original data
        imputed_data[, columns] <- imputed_result
      } else {
        warning("Package 'mice' not available. Using mean imputation instead.")
        for (col in columns) {
          if (any(is.na(data[[col]]))) {
            col_mean <- mean(data[[col]], na.rm = TRUE)
            imputed_data[is.na(data[[col]]), col] <- col_mean
          }
        }
      }
    },
    "mice" = {
      if (requireNamespace("mice", quietly = TRUE)) {
        # Extract additional parameters
        m_value <- list(...)$m
        if (is.null(m_value)) m_value <- 5
        
        maxit <- list(...)$maxit
        if (is.null(maxit)) maxit <- 5
        
        method_param <- list(...)$mice_method
        if (is.null(method_param)) method_param <- "pmm"
        
        group_vars <- list(...)$group_vars
        parallel <- list(...)$parallel
        
        # Prepare predictors matrix
        pred_matrix <- mice::make.predictorMatrix(data[, c(columns, group_vars), drop = FALSE])
        
        # If group variables are provided, use them as grouping factors
        if (!is.null(group_vars)) {
          # Set grouping variables as predictors only
          pred_matrix[group_vars, ] <- 0
          
          # Include grouping in imputation model
          method_vector <- rep(method_param, length(columns))
          names(method_vector) <- columns
          
          # For group variables, no imputation needed
          method_vector <- c(method_vector, rep("", length(group_vars)))
          names(method_vector)[(length(columns) + 1):length(method_vector)] <- group_vars
        } else {
          # Standard imputation without grouping
          method_vector <- rep(method_param, length(columns))
          names(method_vector) <- columns
        }
        
        # Run MICE
        mice_data <- mice::mice(
          data[, c(columns, group_vars), drop = FALSE],
          m = m_value,
          method = method_vector,
          predictorMatrix = pred_matrix,
          maxit = maxit,
          printFlag = FALSE,
          parallel = parallel
        )
        
        # Get imputed datasets
        imputed_datasets <- mice::complete(mice_data, "all")
        
        # Average across multiple imputations for final result
        if (m_value > 1) {
          # Create a matrix to store all imputed values
          imputed_values <- array(NA, dim = c(nrow(data), length(columns), m_value))
          
          for (i in 1:m_value) {
            imputed_values[, , i] <- as.matrix(imputed_datasets[[i]][, columns, drop = FALSE])
          }
          
          # Calculate mean across imputations
          for (j in 1:length(columns)) {
            col <- columns[j]
            for (i in 1:nrow(data)) {
              if (is.na(data[i, col])) {
                imputed_data[i, col] <- mean(imputed_values[i, j, ], na.rm = TRUE)
              }
            }
          }
        } else {
          # Just use the single imputation
          imputed_data[, columns] <- imputed_datasets[[1]][, columns]
        }
      } else {
        warning("Package 'mice' not available. Using mean imputation instead.")
        for (col in columns) {
          if (any(is.na(data[[col]]))) {
            col_mean <- mean(data[[col]], na.rm = TRUE)
            imputed_data[is.na(data[[col]]), col] <- col_mean
          }
        }
      }
    },
    # Default to mean imputation
    {
      warning("Unknown imputation method '", method, "'. Using mean imputation instead.")
      for (col in columns) {
        if (any(is.na(data[[col]]))) {
          col_mean <- mean(data[[col]], na.rm = TRUE)
          imputed_data[is.na(data[[col]]), col] <- col_mean
        }
      }
    }
  )
  
  return(imputed_data)
}

#' Compare multiple imputation methods
#' 
#' @param data Data frame with missing values
#' @param methods Vector of imputation methods to compare
#' @param columns Columns to impute
#' @param artificially_remove Percentage of values to artificially remove for testing (0-1)
#' @param seed Random seed for reproducibility
#' @return List with comparison results
#' @export
compare_imputation_methods <- function(data, methods = c("mean", "median", "knn", "mice"), 
                                     columns = NULL, artificially_remove = 0.1, seed = 42) {
  # Set random seed for reproducibility
  set.seed(seed)
  
  # Check if data is valid
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Check if columns exist and are numeric
  invalid_cols <- setdiff(columns, names(data))
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }
  
  non_numeric <- columns[!sapply(data[, columns, drop = FALSE], is.numeric)]
  if (length(non_numeric) > 0) {
    stop("Non-numeric columns: ", paste(non_numeric, collapse = ", "))
  }
  
  # Extract complete cases for artificial missingness experiment
  complete_data <- data[complete.cases(data[, columns]), ]
  
  if (nrow(complete_data) < 10) {
    stop("Insufficient complete cases for comparison. Need at least 10 complete rows.")
  }
  
  # Create artificial missingness for evaluation
  test_data <- complete_data
  
  # Randomly remove values
  n_cells <- nrow(test_data) * length(columns)
  n_remove <- round(artificially_remove * n_cells)
  
  # Create indices of cells to remove
  remove_rows <- sample(1:nrow(test_data), size = n_remove, replace = TRUE)
  remove_cols <- sample(columns, size = n_remove, replace = TRUE)
  
  # Remove values
  for (i in 1:n_remove) {
    test_data[remove_rows[i], remove_cols[i]] <- NA
  }
  
  # Initialize results
  comparison_results <- list(
    methods = methods,
    metrics = data.frame(
      Method = character(0),
      RMSE = numeric(0),
      MAE = numeric(0),
      CorrelationWithTrue = numeric(0),
      stringsAsFactors = FALSE
    ),
    imputed_data = list()
  )
  
  # Apply each imputation method
  for (method in methods) {
    # Impute data
    imputed_result <- impute_missing_data(test_data, method = method, columns = columns, seed = seed)
    imputed_data <- imputed_result$imputed_data
    
    # Store imputed data
    comparison_results$imputed_data[[method]] <- imputed_data
    
    # Calculate metrics
    true_values <- numeric(0)
    imputed_values <- numeric(0)
    
    for (i in 1:n_remove) {
      row <- remove_rows[i]
      col <- remove_cols[i]
      
      true_values <- c(true_values, complete_data[row, col])
      imputed_values <- c(imputed_values, imputed_data[row, col])
    }
    
    # Calculate error metrics
    rmse <- sqrt(mean((true_values - imputed_values)^2))
    mae <- mean(abs(true_values - imputed_values))
    correlation <- cor(true_values, imputed_values, use = "pairwise.complete.obs")
    
    # Add to results
    comparison_results$metrics <- rbind(
      comparison_results$metrics,
      data.frame(
        Method = method,
        RMSE = rmse,
        MAE = mae,
        CorrelationWithTrue = correlation,
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(comparison_results)
}
