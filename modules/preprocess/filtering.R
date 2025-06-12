#########################################################
# Enhanced Brain Network Analysis Shiny App
# preprocess/filtering.R - Data filtering and cleaning
#########################################################

#' Filter and clean brain network data
#' 
#' @param data Data frame to clean
#' @param options List of filtering options:
#'   - outlier_method: Method for outlier detection ("zscore", "iqr", "none")
#'   - outlier_threshold: Threshold for outlier detection (z-score or IQR multiplier)
#'   - outlier_action: Action for outliers ("remove", "cap", "transform", "none")
#'   - transform_method: Transformation method ("log", "sqrt", "boxcox", "none")
#'   - filter_constant: Whether to filter constant columns
#'   - min_variance: Minimum variance for columns to keep
#'   - exclude_columns: Columns to exclude from filtering
#' @param region_columns Columns containing brain region data
#' @return List with filtered data and filtering metrics
#' @export
filter_brain_data <- function(data, options = NULL, region_columns = NULL) {
  # Set default options if not provided
  if (is.null(options)) {
    options <- list(
      outlier_method = "zscore",
      outlier_threshold = 3,
      outlier_action = "cap",
      transform_method = "none",
      filter_constant = TRUE,
      min_variance = 0.001,
      exclude_columns = NULL
    )
  }
  
  # Check if data is valid
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # If region columns not specified, use all numeric columns
  if (is.null(region_columns)) {
    region_columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Exclude specified columns
  if (!is.null(options$exclude_columns)) {
    region_columns <- setdiff(region_columns, options$exclude_columns)
  }
  
  # Initialize results
  result <- list(
    filtered_data = data,
    original_data = data,
    outliers_detected = list(),
    columns_removed = character(0),
    transformations_applied = character(0),
    metrics = data.frame(
      Column = region_columns,
      Outliers = 0,
      OutlierPercent = 0,
      Variance = 0,
      Skewness = NA,
      KurtosisExcess = NA,
      stringsAsFactors = FALSE
    )
  )
  
  # Calculate column statistics
  for (i in 1:length(region_columns)) {
    col <- region_columns[i]
    col_data <- data[[col]]
    
    # Variance
    result$metrics$Variance[i] <- var(col_data, na.rm = TRUE)
    
    # Skewness and kurtosis if moments package is available
    if (requireNamespace("moments", quietly = TRUE)) {
      result$metrics$Skewness[i] <- moments::skewness(col_data, na.rm = TRUE)
      result$metrics$KurtosisExcess[i] <- moments::kurtosis(col_data, na.rm = TRUE) - 3
    }
  }
  
  # 1. Filter constant or near-constant columns
  if (options$filter_constant) {
    low_var_cols <- region_columns[result$metrics$Variance < options$min_variance]
    
    if (length(low_var_cols) > 0) {
      result$columns_removed <- c(result$columns_removed, low_var_cols)
      region_columns <- setdiff(region_columns, low_var_cols)
    }
  }
  
  # 2. Apply transformations if requested
  if (options$transform_method != "none") {
    for (col in region_columns) {
      col_data <- data[[col]]
      
      # Skip columns with negative or zero values for certain transformations
      if ((options$transform_method %in% c("log", "sqrt", "boxcox")) && any(col_data <= 0, na.rm = TRUE)) {
        next
      }
      
      # Apply transformation
      transformed_data <- switch(
        options$transform_method,
        "log" = log(col_data),
        "sqrt" = sqrt(col_data),
        "boxcox" = {
          if (requireNamespace("MASS", quietly = TRUE)) {
            # Find optimal lambda
            bc <- MASS::boxcox(col_data ~ 1, plotit = FALSE)
            lambda <- bc$x[which.max(bc$y)]
            
            # Apply transformation
            if (abs(lambda) < 0.01) {
              # For lambda near zero, use log
              log(col_data)
            } else {
              # For other lambda values, use power transformation
              (col_data^lambda - 1) / lambda
            }
          } else {
            warning("Package 'MASS' not available for Box-Cox transformation. Skipping.")
            col_data
          }
        },
        # Default: no transformation
        col_data
      )
      
      # Update data
      result$filtered_data[[col]] <- transformed_data
      result$transformations_applied <- c(result$transformations_applied, col)
    }
  }
  
  # 3. Detect and handle outliers
  if (options$outlier_method != "none" && options$outlier_action != "none") {
    for (col in region_columns) {
      col_data <- result$filtered_data[[col]]
      
      # Detect outliers
      outliers <- detect_outliers(
        col_data, 
        method = options$outlier_method, 
        threshold = options$outlier_threshold
      )
      
      # Track outliers
      result$outliers_detected[[col]] <- which(outliers)
      result$metrics$Outliers[result$metrics$Column == col] <- sum(outliers, na.rm = TRUE)
      result$metrics$OutlierPercent[result$metrics$Column == col] <- 
        100 * sum(outliers, na.rm = TRUE) / sum(!is.na(col_data))
      
      # Handle outliers
      if (sum(outliers, na.rm = TRUE) > 0) {
        result$filtered_data[[col]] <- handle_outliers(
          col_data, 
          outliers, 
          action = options$outlier_action
        )
      }
    }
  }
  
  # 4. Update data by removing columns if needed
  if (length(result$columns_removed) > 0) {
    message("Removed ", length(result$columns_removed), " constant or near-constant columns: ", 
           paste(result$columns_removed, collapse = ", "))
  }
  
  return(result)
}

#' Detect outliers in a numeric vector
#' 
#' @param x Numeric vector
#' @param method Method for outlier detection ("zscore", "iqr", "mad")
#' @param threshold Threshold for outlier detection
#' @return Logical vector indicating outliers
#' @export
detect_outliers <- function(x, method = "zscore", threshold = 3) {
  # Handle missing values
  is_na <- is.na(x)
  
  if (all(is_na)) {
    return(rep(FALSE, length(x)))
  }
  
  # Initialize outlier vector
  outliers <- rep(FALSE, length(x))
  outliers[is_na] <- NA  # Keep NA as NA
  
  # Working data without NAs
  x_clean <- x[!is_na]
  
  if (length(x_clean) == 0) {
    return(outliers)
  }
  
  # Detect outliers based on method
  switch(
    tolower(method),
    "zscore" = {
      # Z-score method
      z_scores <- (x_clean - mean(x_clean)) / sd(x_clean)
      outliers_clean <- abs(z_scores) > threshold
    },
    "iqr" = {
      # IQR method
      q1 <- quantile(x_clean, 0.25)
      q3 <- quantile(x_clean, 0.75)
      iqr <- q3 - q1
      lower_bound <- q1 - threshold * iqr
      upper_bound <- q3 + threshold * iqr
      outliers_clean <- x_clean < lower_bound | x_clean > upper_bound
    },
    "mad" = {
      # Median Absolute Deviation
      if (requireNamespace("stats", quietly = TRUE)) {
        med <- median(x_clean)
        mad_val <- stats::mad(x_clean)
        outliers_clean <- abs(x_clean - med) / mad_val > threshold
      } else {
        # Fallback to IQR if stats not available
        q1 <- quantile(x_clean, 0.25)
        q3 <- quantile(x_clean, 0.75)
        iqr <- q3 - q1
        lower_bound <- q1 - threshold * iqr
        upper_bound <- q3 + threshold * iqr
        outliers_clean <- x_clean < lower_bound | x_clean > upper_bound
      }
    },
    # Default to Z-score
    {
      warning("Unknown outlier detection method '", method, "'. Using Z-score method.")
      z_scores <- (x_clean - mean(x_clean)) / sd(x_clean)
      outliers_clean <- abs(z_scores) > threshold
    }
  )
  
  # Set outliers for non-NA values
  outliers[!is_na] <- outliers_clean
  
  return(outliers)
}

#' Handle outliers in a numeric vector
#' 
#' @param x Numeric vector
#' @param outliers Logical vector indicating outliers
#' @param action Action for outliers ("remove", "cap", "winsorize", "transform")
#' @param replacement_method Method for replacing outliers (for "cap" action)
#' @return Numeric vector with handled outliers
#' @export
handle_outliers <- function(x, outliers, action = "cap", replacement_method = "bound") {
  # Check inputs
  if (length(x) != length(outliers)) {
    stop("Length of x and outliers must be the same")
  }
  
  # Copy input to avoid modifying the original
  x_handled <- x
  
  # Handle missing values
  is_na <- is.na(outliers)
  outliers[is_na] <- FALSE  # Don't treat NAs as outliers
  
  # If no outliers, return the original
  if (!any(outliers)) {
    return(x)
  }
  
  # Handle outliers based on action
  switch(
    tolower(action),
    "remove" = {
      # Set outliers to NA
      x_handled[outliers] <- NA
    },
    "cap" = {
      # Cap outliers at threshold
      # Calculate bounds
      if (replacement_method == "bound") {
        # Use Z-score or IQR bounds
        mean_val <- mean(x, na.rm = TRUE)
        sd_val <- sd(x, na.rm = TRUE)
        
        # Use 3 standard deviations as default
        lower_bound <- mean_val - 3 * sd_val
        upper_bound <- mean_val + 3 * sd_val
        
        # Cap values
        x_handled[outliers & x < lower_bound] <- lower_bound
        x_handled[outliers & x > upper_bound] <- upper_bound
      } else if (replacement_method == "percentile") {
        # Use percentile bounds
        lower_bound <- quantile(x, 0.01, na.rm = TRUE)
        upper_bound <- quantile(x, 0.99, na.rm = TRUE)
        
        # Cap values
        x_handled[outliers & x < lower_bound] <- lower_bound
        x_handled[outliers & x > upper_bound] <- upper_bound
      }
    },
    "winsorize" = {
      # Winsorize (similar to cap but uses percentiles)
      if (requireNamespace("DescTools", quietly = TRUE)) {
        # Use DescTools package for winsorization
        x_handled <- DescTools::Winsorize(x, probs = c(0.05, 0.95), na.rm = TRUE)
      } else {
        # Manual winsorization
        lower_bound <- quantile(x, 0.05, na.rm = TRUE)
        upper_bound <- quantile(x, 0.95, na.rm = TRUE)
        
        x_handled[x < lower_bound] <- lower_bound
        x_handled[x > upper_bound] <- upper_bound
      }
    },
    "transform" = {
      # Apply rank-based inverse normal transformation
      if (requireNamespace("bestNormalize", quietly = TRUE)) {
        # Use bestNormalize package
        norm_obj <- bestNormalize::orderNorm(x)
        x_handled <- predict(norm_obj)
      } else {
        # Manual rank-based transformation
        ranks <- rank(x, na.last = "keep", ties.method = "average")
        n <- sum(!is.na(x))
        # Convert ranks to quantiles and then to normal scores
        x_handled <- qnorm((ranks - 0.5) / n)
      }
    },
    # Default: do nothing
    {
      warning("Unknown outlier action '", action, "'. No action taken.")
    }
  )
  
  return(x_handled)
}

#' Normalize brain region data
#' 
#' @param data Data frame to normalize
#' @param method Normalization method ("zscore", "minmax", "robust", "rank")
#' @param columns Columns to normalize
#' @param group_vars Grouping variables for group-specific normalization
#' @return Data frame with normalized values
#' @export
normalize_brain_data <- function(data, method = "zscore", columns = NULL, group_vars = NULL) {
  # Check if data is valid
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Initialize normalized data
  normalized_data <- data
  
  # Apply normalization
  if (!is.null(group_vars)) {
    # Group-specific normalization
    groups <- split(data, interaction(data[, group_vars, drop = FALSE], drop = TRUE))
    
    for (group_name in names(groups)) {
      group_data <- groups[[group_name]]
      group_indices <- which(interaction(data[, group_vars, drop = FALSE], drop = TRUE) == group_name)
      
      # Apply normalization within group
      for (col in columns) {
        col_data <- group_data[[col]]
        
        if (all(is.na(col_data))) {
          next  # Skip if all NA
        }
        
        normalized_data[group_indices, col] <- normalize_column(col_data, method)
      }
    }
  } else {
    # Global normalization
    for (col in columns) {
      col_data <- data[[col]]
      
      if (all(is.na(col_data))) {
        next  # Skip if all NA
      }
      
      normalized_data[[col]] <- normalize_column(col_data, method)
    }
  }
  
  return(normalized_data)
}

#' Normalize a single column
#' 
#' @param x Numeric vector
#' @param method Normalization method
#' @return Normalized vector
#' @export
normalize_column <- function(x, method = "zscore") {
  # Handle missing values
  if (all(is.na(x))) {
    return(x)
  }
  
  # Apply normalization method
  normalized <- switch(
    tolower(method),
    "zscore" = {
      # Z-score normalization
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    },
    "minmax" = {
      # Min-max scaling
      min_val <- min(x, na.rm = TRUE)
      max_val <- max(x, na.rm = TRUE)
      
      if (min_val == max_val) {
        rep(0.5, length(x))  # If all values are the same, return 0.5
      } else {
        (x - min_val) / (max_val - min_val)
      }
    },
    "robust" = {
      # Robust scaling using median and IQR
      med <- median(x, na.rm = TRUE)
      iqr_val <- IQR(x, na.rm = TRUE)
      
      if (iqr_val == 0) {
        # If IQR is 0, use median absolute deviation
        mad_val <- mad(x, na.rm = TRUE)
        
        if (mad_val == 0) {
          # If both IQR and MAD are 0, return 0
          rep(0, length(x))
        } else {
          (x - med) / mad_val
        }
      } else {
        (x - med) / iqr_val
      }
    },
    "rank" = {
      # Rank-based normalization
      ranks <- rank(x, na.last = "keep", ties.method = "average")
      n <- sum(!is.na(x))
      ranks / n
    },
    # Default to Z-score
    {
      warning("Unknown normalization method '", method, "'. Using Z-score method.")
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    }
  )
  
  return(normalized)
}
