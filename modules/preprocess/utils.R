#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/preprocess/utils.R - Preprocessing utility functions
#########################################################

#' Analyze missing data in the dataset
#' 
#' @param data Data frame to analyze
#' @param region_columns Vector of column names for brain regions
#' @return A list with missing data statistics
#' @export
analyze_missing_data <- function(data, region_columns) {
  # Initialize results
  results <- list(
    has_missing = FALSE,
    total_missing = 0,
    percent_missing = 0,
    missing_by_region = NULL,
    missing_by_subject = NULL
  )
  
  # Check if data is valid
  if (!is.data.frame(data) || length(region_columns) == 0) {
    return(results)
  }
  
  # Extract region data
  region_data <- data[, region_columns, drop = FALSE]
  
  # Count missing values
  missing_by_region <- colSums(is.na(region_data))
  missing_by_subject <- rowSums(is.na(region_data))
  
  total_missing <- sum(missing_by_region)
  total_cells <- nrow(data) * length(region_columns)
  percent_missing <- 100 * total_missing / total_cells
  
  # Update results
  results$has_missing <- total_missing > 0
  results$total_missing <- total_missing
  results$percent_missing <- percent_missing
  results$missing_by_region <- missing_by_region
  results$missing_by_subject <- missing_by_subject
  
  return(results)
}

#' Detect outliers in the dataset
#' 
#' @param data Data frame to analyze
#' @param region_columns Vector of column names for brain regions
#' @param method Outlier detection method ('iqr', 'zscore', 'mad')
#' @param threshold Threshold for outlier detection
#' @return A list with outlier information
#' @export
detect_outliers <- function(data, region_columns, method = "iqr", threshold = 2.5) {
  # Initialize results
  results <- list(
    method = method,
    threshold = threshold,
    outliers_by_region = list(),
    total_outliers = 0
  )
  
  # Check if data is valid
  if (!is.data.frame(data) || length(region_columns) == 0 || method == "none") {
    return(results)
  }
  
  # Detect outliers for each region
  outliers_by_region <- list()
  
  for (region in region_columns) {
    values <- data[[region]]
    
    # Skip if all values are NA
    if (all(is.na(values))) {
      outliers_by_region[[region]] <- integer(0)
      next
    }
    
    # Apply detection method
    if (method == "iqr") {
      # IQR method
      q1 <- stats::quantile(values, 0.25, na.rm = TRUE)
      q3 <- stats::quantile(values, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - threshold * iqr
      upper_bound <- q3 + threshold * iqr
      
      outliers <- which(values < lower_bound | values > upper_bound)
    } else if (method == "zscore") {
      # Z-score method
      mean_val <- mean(values, na.rm = TRUE)
      sd_val <- sd(values, na.rm = TRUE)
      
      if (sd_val == 0) {
        outliers <- integer(0)
      } else {
        z_scores <- abs((values - mean_val) / sd_val)
        outliers <- which(z_scores > threshold)
      }
    } else if (method == "mad") {
      # Modified Z-score method using Median Absolute Deviation
      median_val <- median(values, na.rm = TRUE)
      mad_val <- stats::mad(values, na.rm = TRUE)
      
      if (mad_val == 0) {
        outliers <- integer(0)
      } else {
        # Modified Z-scores (using 0.6745 constant for normal distribution)
        mod_z_scores <- abs(0.6745 * (values - median_val) / mad_val)
        outliers <- which(mod_z_scores > threshold)
      }
    } else {
      # Default: no outliers
      outliers <- integer(0)
    }
    
    # Store outliers for this region
    outliers_by_region[[region]] <- outliers
  }
  
  # Calculate total outliers
  total_outliers <- sum(sapply(outliers_by_region, length))
  
  # Update results
  results$outliers_by_region <- outliers_by_region
  results$total_outliers <- total_outliers
  
  return(results)
}

#' Handle outliers in the dataset
#' 
#' @param data Data frame with outliers
#' @param region_columns Vector of column names for brain regions
#' @param outlier_info Outlier information from detect_outliers function
#' @param method Handling method ('winsorize', 'na', 'remove')
#' @return A list with updated data and handling summary
#' @export
handle_outliers <- function(data, region_columns, outlier_info, method = "winsorize") {
  # Check if data is valid
  if (!is.data.frame(data) || length(region_columns) == 0) {
    return(list(data = data, summary = "No valid data provided"))
  }
  
  # Make a copy of the data
  result_data <- data
  
  # Track affected rows
  affected_rows <- integer(0)
  
  # Apply handling method
  if (method == "winsorize") {
    # Winsorize outliers (cap at threshold)
    for (region in names(outlier_info$outliers_by_region)) {
      outliers <- outlier_info$outliers_by_region[[region]]
      
      if (length(outliers) > 0) {
        values <- result_data[[region]]
        
        # Get bounds based on the detection method
        if (outlier_info$method == "iqr") {
          q1 <- stats::quantile(values, 0.25, na.rm = TRUE)
          q3 <- stats::quantile(values, 0.75, na.rm = TRUE)
          iqr <- q3 - q1
          lower_bound <- q1 - outlier_info$threshold * iqr
          upper_bound <- q3 + outlier_info$threshold * iqr
        } else if (outlier_info$method == "zscore") {
          mean_val <- mean(values, na.rm = TRUE)
          sd_val <- sd(values, na.rm = TRUE)
          lower_bound <- mean_val - outlier_info$threshold * sd_val
          upper_bound <- mean_val + outlier_info$threshold * sd_val
        } else if (outlier_info$method == "mad") {
          median_val <- median(values, na.rm = TRUE)
          mad_val <- stats::mad(values, na.rm = TRUE)
          lower_bound <- median_val - outlier_info$threshold * mad_val / 0.6745
          upper_bound <- median_val + outlier_info$threshold * mad_val / 0.6745
        } else {
          # Default: no action
          next
        }
        
        # Replace outliers with bounds
        for (i in outliers) {
          if (!is.na(values[i])) {
            if (values[i] < lower_bound) {
              result_data[i, region] <- lower_bound
            } else if (values[i] > upper_bound) {
              result_data[i, region] <- upper_bound
            }
          }
        }
        
        # Add to affected rows
        affected_rows <- union(affected_rows, outliers)
      }
    }
  } else if (method == "na") {
    # Replace outliers with NA
    for (region in names(outlier_info$outliers_by_region)) {
      outliers <- outlier_info$outliers_by_region[[region]]
      
      if (length(outliers) > 0) {
        result_data[outliers, region] <- NA
        
        # Add to affected rows
        affected_rows <- union(affected_rows, outliers)
      }
    }
  } else if (method == "remove") {
    # Get all rows with outliers
    all_outlier_rows <- integer(0)
    
    for (region in names(outlier_info$outliers_by_region)) {
      outliers <- outlier_info$outliers_by_region[[region]]
      all_outlier_rows <- union(all_outlier_rows, outliers)
    }
    
    # Remove rows with outliers
    if (length(all_outlier_rows) > 0 && length(all_outlier_rows) < nrow(data)) {
      result_data <- result_data[-all_outlier_rows, ]
      affected_rows <- all_outlier_rows
    }
  }
  
  # Return results
  list(
    data = result_data,
    affected_rows = affected_rows,
    num_affected = length(affected_rows)
  )
}

#' Transform data using various methods
#' 
#' @param values Vector of values to transform
#' @param method Transformation method
#' @param log_plus_one Whether to add 1 before log transformation
#' @param boxcox_lambda Lambda parameter for Box-Cox transformation
#' @return Transformed values
#' @export
transform_data <- function(values, method = "none", log_plus_one = TRUE, boxcox_lambda = NULL) {
  # Check if values is valid
  if (length(values) == 0) {
    return(values)
  }
  
  # Remove NA values for calculations
  na_indices <- which(is.na(values))
  valid_values <- values[!is.na(values)]
  
  # Apply transformation method
  if (method == "zscore") {
    # Z-score standardization
    mean_val <- mean(valid_values)
    sd_val <- sd(valid_values)
    
    if (sd_val == 0) {
      transformed <- valid_values
    } else {
      transformed <- (valid_values - mean_val) / sd_val
    }
  } else if (method == "minmax") {
    # Min-max scaling to [0, 1]
    min_val <- min(valid_values)
    max_val <- max(valid_values)
    
    if (max_val == min_val) {
      transformed <- valid_values
    } else {
      transformed <- (valid_values - min_val) / (max_val - min_val)
    }
  } else if (method == "log") {
    # Log transformation
    if (any(valid_values <= 0) && log_plus_one) {
      # log(x + 1) transformation
      transformed <- log(valid_values + 1)
    } else if (!any(valid_values <= 0)) {
      # Regular log transformation
      transformed <- log(valid_values)
    } else {
      # Can't apply log to non-positive values
      transformed <- valid_values
    }
  } else if (method == "sqrt") {
    # Square root transformation
    if (any(valid_values < 0)) {
      # Can't apply sqrt to negative values
      transformed <- valid_values
    } else {
      transformed <- sqrt(valid_values)
    }
  } else if (method == "boxcox") {
    # Box-Cox transformation
    if (requireNamespace("MASS", quietly = TRUE) && all(valid_values > 0)) {
      if (is.null(boxcox_lambda)) {
        # Find optimal lambda
        bc <- MASS::boxcox(valid_values ~ 1, plotit = FALSE)
        lambda <- bc$x[which.max(bc$y)]
      } else {
        lambda <- boxcox_lambda
      }
      
      if (abs(lambda) < 0.01) {
        # Log transformation if lambda is close to 0
        transformed <- log(valid_values)
      } else {
        # Regular Box-Cox transformation
        transformed <- (valid_values^lambda - 1) / lambda
      }
    } else {
      # Can't apply Box-Cox to non-positive values or MASS not available
      transformed <- valid_values
    }
  } else {
    # Default: no transformation
    transformed <- valid_values
  }
  
  # Reconstruct result with NAs in original positions
  result <- values
  result[!is.na(values)] <- transformed
  
  return(result)
}

#' Impute missing values in the dataset
#' 
#' @param data Data frame with missing values
#' @param region_columns Vector of column names for brain regions
#' @param method Imputation method ('mean', 'median', 'knn', 'mice')
#' @param k Number of neighbors for KNN imputation
#' @param mice_method Method for MICE imputation
#' @return A list with imputed data and parameters
#' @export
impute_missing_values <- function(data, region_columns, method = "mean", k = 5, mice_method = "pmm") {
  # Check if data is valid
  if (!is.data.frame(data) || length(region_columns) == 0) {
    return(list(data = data, parameters = NULL))
  }
  
  # Make a copy of the data
  result_data <- data
  parameters <- list(method = method)
  
  # Apply imputation method
  if (method == "mean") {
    # Mean imputation
    for (col in region_columns) {
      values <- result_data[[col]]
      
      if (any(is.na(values))) {
        mean_val <- mean(values, na.rm = TRUE)
        result_data[[col]][is.na(values)] <- mean_val
      }
    }
  } else if (method == "median") {
    # Median imputation
    for (col in region_columns) {
      values <- result_data[[col]]
      
      if (any(is.na(values))) {
        median_val <- median(values, na.rm = TRUE)
        result_data[[col]][is.na(values)] <- median_val
      }
    }
  } else if (method == "knn") {
    # KNN imputation using VIM package
    if (requireNamespace("VIM", quietly = TRUE)) {
      # Extract region data
      region_data <- result_data[, region_columns, drop = FALSE]
      
      # Perform KNN imputation
      imputed_data <- VIM::kNN(region_data, k = k)
      
      # Remove imputation indicator columns
      imputed_cols <- names(imputed_data)[!grepl("_imp$", names(imputed_data))]
      imputed_data <- imputed_data[, imputed_cols, drop = FALSE]
      
      # Update result data
      result_data[, region_columns] <- imputed_data
      
      # Store parameters
      parameters$k <- k
    }
  } else if (method == "mice") {
    # Multiple imputation using mice package
    if (requireNamespace("mice", quietly = TRUE)) {
      # Extract region data
      region_data <- result_data[, region_columns, drop = FALSE]
      
      # Perform MICE imputation (single imputation for this purpose)
      imputed_data <- mice::mice(region_data, m = 1, method = mice_method, printFlag = FALSE)
      complete_data <- mice::complete(imputed_data)
      
      # Update result data
      result_data[, region_columns] <- complete_data
      
      # Store parameters
      parameters$mice_method <- mice_method
    }
  }
  
  # Return results
  list(
    data = result_data,
    parameters = parameters
  )
}

#' Run quality checks on the dataset
#' 
#' @param data Data frame to analyze
#' @param region_columns Vector of column names for brain regions
#' @return A list with quality check results
#' @export
run_quality_checks <- function(data, region_columns) {
  # Initialize results
  results <- list(
    region_summary = NULL,
    variance_check = NULL,
    correlation_check = NULL
  )
  
  # Check if data is valid
  if (!is.data.frame(data) || length(region_columns) == 0) {
    return(results)
  }
  
  # Region summary statistics
  region_summary <- data.frame(
    Region = region_columns,
    Mean = numeric(length(region_columns)),
    Median = numeric(length(region_columns)),
    SD = numeric(length(region_columns)),
    Min = numeric(length(region_columns)),
    Max = numeric(length(region_columns)),
    Missing = numeric(length(region_columns)),
    Missing_Percent = numeric(length(region_columns)),
    CV = numeric(length(region_columns)),  # Coefficient of Variation
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(region_columns)) {
    col <- region_columns[i]
    values <- data[[col]]
    
    region_summary$Mean[i] <- mean(values, na.rm = TRUE)
    region_summary$Median[i] <- median(values, na.rm = TRUE)
    region_summary$SD[i] <- sd(values, na.rm = TRUE)
    region_summary$Min[i] <- min(values, na.rm = TRUE)
    region_summary$Max[i] <- max(values, na.rm = TRUE)
    region_summary$Missing[i] <- sum(is.na(values))
    region_summary$Missing_Percent[i] <- 100 * sum(is.na(values)) / length(values)
    
    # Coefficient of Variation (SD/Mean)
    if (region_summary$Mean[i] != 0) {
      region_summary$CV[i] <- region_summary$SD[i] / abs(region_summary$Mean[i])
    } else {
      region_summary$CV[i] <- NA
    }
  }
  
  # Variance check
  variance_check <- data.frame(
    Region = region_columns,
    Variance = sapply(region_columns, function(col) var(data[[col]], na.rm = TRUE)),
    CV = region_summary$CV,
    Flag = "",
    stringsAsFactors = FALSE
  )
  
  # Flag low variance regions
  low_var_threshold <- quantile(variance_check$Variance, 0.1, na.rm = TRUE)
  variance_check$Flag[variance_check$Variance < low_var_threshold] <- "Low Variance"
  
  # Correlation check
  correlation_check <- data.frame(
    Region1 = character(),
    Region2 = character(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (length(region_columns) > 1) {
    # Calculate correlation matrix
    cor_matrix <- cor(data[, region_columns], use = "pairwise.complete.obs")
    
    # Find high correlations
    high_cor_threshold <- 0.8
    high_cor_indices <- which(abs(cor_matrix) > high_cor_threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
    
    if (nrow(high_cor_indices) > 0) {
      # Create data frame of high correlations
      for (i in 1:nrow(high_cor_indices)) {
        row_idx <- high_cor_indices[i, 1]
        col_idx <- high_cor_indices[i, 2]
        
        # Only include each pair once (upper triangle)
        if (row_idx < col_idx) {
          correlation_check <- rbind(correlation_check, data.frame(
            Region1 = region_columns[row_idx],
            Region2 = region_columns[col_idx],
            Correlation = cor_matrix[row_idx, col_idx],
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Sort by absolute correlation
      correlation_check <- correlation_check[order(-abs(correlation_check$Correlation)), ]
    }
  }
  
  # Update results
  results$region_summary <- region_summary
  results$variance_check <- variance_check
  results$correlation_check <- correlation_check
  
  return(results)
}
