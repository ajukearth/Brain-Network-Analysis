#########################################################
# Enhanced Brain Network Analysis Shiny App
# preprocess/utils.R - Preprocessing utility functions
#########################################################

#' Detect data types and suggest preprocessing steps
#' 
#' @param data Data frame to analyze
#' @return List with data type information and preprocessing suggestions
#' @export
detect_data_properties <- function(data) {
  # Check if data exists
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      valid = FALSE,
      message = "No data provided or empty dataset."
    ))
  }
  
  # Initialize results
  results <- list(
    valid = TRUE,
    n_subjects = nrow(data),
    n_variables = ncol(data),
    variable_types = list(
      numeric = character(0),
      factor = character(0),
      character = character(0),
      date = character(0),
      other = character(0)
    ),
    missing_data = list(
      has_missing = FALSE,
      columns_with_missing = character(0),
      missing_pattern = NULL
    ),
    outliers = list(
      has_outliers = FALSE,
      columns_with_outliers = character(0)
    ),
    preprocessing_needed = list(
      imputation = FALSE,
      normalization = FALSE,
      transformation = FALSE,
      outlier_treatment = FALSE
    ),
    preprocessing_suggestions = character(0)
  )
  
  # Analyze variable types
  for (col_name in names(data)) {
    col <- data[[col_name]]
    
    if (is.numeric(col)) {
      results$variable_types$numeric <- c(results$variable_types$numeric, col_name)
    } else if (is.factor(col)) {
      results$variable_types$factor <- c(results$variable_types$factor, col_name)
    } else if (is.character(col)) {
      results$variable_types$character <- c(results$variable_types$character, col_name)
    } else if (inherits(col, "Date") || inherits(col, "POSIXt")) {
      results$variable_types$date <- c(results$variable_types$date, col_name)
    } else {
      results$variable_types$other <- c(results$variable_types$other, col_name)
    }
  }
  
  # Check for missing data
  missing_counts <- colSums(is.na(data))
  
  if (any(missing_counts > 0)) {
    results$missing_data$has_missing <- TRUE
    results$missing_data$columns_with_missing <- names(missing_counts)[missing_counts > 0]
    
    # Calculate missing percentage
    missing_percentage <- 100 * sum(missing_counts) / (nrow(data) * ncol(data))
    results$missing_data$missing_percentage <- missing_percentage
    
    # Suggest imputation if missing data present
    results$preprocessing_needed$imputation <- TRUE
    
    if (missing_percentage > 10) {
      results$preprocessing_suggestions <- c(
        results$preprocessing_suggestions,
        paste0("High missingness (", round(missing_percentage, 1), 
              "%). Consider using multiple imputation (MICE).")
      )
    } else {
      results$preprocessing_suggestions <- c(
        results$preprocessing_suggestions,
        paste0("Missingness detected (", round(missing_percentage, 1), 
              "%). Consider imputation.")
      )
    }
  }
  
  # Check for skewness in numeric variables
  numeric_cols <- results$variable_types$numeric
  
  if (length(numeric_cols) > 0 && requireNamespace("moments", quietly = TRUE)) {
    skewness_values <- sapply(data[, numeric_cols, drop = FALSE], 
                            function(x) moments::skewness(x, na.rm = TRUE))
    highly_skewed <- names(skewness_values)[abs(skewness_values) > 1]
    
    if (length(highly_skewed) > 0) {
      results$preprocessing_needed$transformation <- TRUE
      
      skewed_percentage <- 100 * length(highly_skewed) / length(numeric_cols)
      
      if (skewed_percentage > 25) {
        results$preprocessing_suggestions <- c(
          results$preprocessing_suggestions,
          paste0(round(skewed_percentage, 1), 
                "% of numeric variables are highly skewed. Consider transformation.")
        )
      }
    }
  }
  
  # Check for outliers
  if (length(numeric_cols) > 0) {
    outlier_cols <- character(0)
    
    for (col in numeric_cols) {
      col_data <- data[[col]]
      
      # Calculate z-scores
      if (sd(col_data, na.rm = TRUE) > 0) {
        z_scores <- scale(col_data)
        
        # Check for outliers (|z| > 3)
        if (any(abs(z_scores) > 3, na.rm = TRUE)) {
          outlier_cols <- c(outlier_cols, col)
        }
      }
    }
    
    if (length(outlier_cols) > 0) {
      results$outliers$has_outliers <- TRUE
      results$outliers$columns_with_outliers <- outlier_cols
      results$preprocessing_needed$outlier_treatment <- TRUE
      
      outlier_percentage <- 100 * length(outlier_cols) / length(numeric_cols)
      
      results$preprocessing_suggestions <- c(
        results$preprocessing_suggestions,
        paste0("Outliers detected in ", round(outlier_percentage, 1), 
              "% of numeric variables. Consider outlier treatment.")
      )
    }
  }
  
  # Check for normalization needs
  if (length(numeric_cols) > 0) {
    # Check range variability
    ranges <- sapply(data[, numeric_cols, drop = FALSE], 
                   function(x) diff(range(x, na.rm = TRUE)))
    
    if (max(ranges) / min(ranges) > 10) {
      results$preprocessing_needed$normalization <- TRUE
      
      results$preprocessing_suggestions <- c(
        results$preprocessing_suggestions,
        "Large differences in variable ranges. Consider normalization or standardization."
      )
    }
  }
  
  return(results)
}

#' Check for multicollinearity in numeric variables
#' 
#' @param data Data frame with numeric variables
#' @param threshold Correlation threshold for multicollinearity
#' @return List with multicollinearity information
#' @export
check_multicollinearity <- function(data, threshold = 0.7) {
  # Extract numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  if (length(numeric_cols) < 2) {
    return(list(
      has_multicollinearity = FALSE,
      message = "Less than 2 numeric variables. Multicollinearity check not applicable."
    ))
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(data[, numeric_cols, drop = FALSE], 
                  use = "pairwise.complete.obs")
  
  # Find high correlations (excluding self-correlations)
  high_cors <- which(abs(cor_matrix) > threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
  
  if (nrow(high_cors) == 0) {
    return(list(
      has_multicollinearity = FALSE,
      message = "No multicollinearity detected."
    ))
  }
  
  # Create results
  results <- list(
    has_multicollinearity = TRUE,
    correlation_matrix = cor_matrix,
    high_correlations = data.frame(
      Variable1 = character(0),
      Variable2 = character(0),
      Correlation = numeric(0),
      stringsAsFactors = FALSE
    )
  )
  
  # Add high correlations to results (avoiding duplicates)
  for (i in 1:nrow(high_cors)) {
    row_idx <- high_cors[i, 1]
    col_idx <- high_cors[i, 2]
    
    if (row_idx < col_idx) {  # Avoid duplicates
      results$high_correlations <- rbind(
        results$high_correlations,
        data.frame(
          Variable1 = numeric_cols[row_idx],
          Variable2 = numeric_cols[col_idx],
          Correlation = cor_matrix[row_idx, col_idx],
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # Sort by absolute correlation
  results$high_correlations <- results$high_correlations[
    order(abs(results$high_correlations$Correlation), decreasing = TRUE),
  ]
  
  # Add message
  results$message <- paste0(
    "Multicollinearity detected between ", nrow(results$high_correlations), 
    " variable pairs (|r| > ", threshold, ")."
  )
  
  return(results)
}

#' Apply z-score standardization to numeric variables
#' 
#' @param data Data frame with variables to standardize
#' @param columns Columns to standardize (NULL for all numeric)
#' @param center Whether to center variables (subtract mean)
#' @param scale Whether to scale variables (divide by sd)
#' @return Data frame with standardized variables
#' @export
standardize_variables <- function(data, columns = NULL, center = TRUE, scale = TRUE) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Check if columns exist
  invalid_cols <- setdiff(columns, names(data))
  
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }
  
  # Filter for numeric columns
  non_numeric <- columns[!sapply(data[, columns, drop = FALSE], is.numeric)]
  
  if (length(non_numeric) > 0) {
    warning("Non-numeric columns will be skipped: ", paste(non_numeric, collapse = ", "))
    columns <- setdiff(columns, non_numeric)
  }
  
  if (length(columns) == 0) {
    warning("No numeric columns to standardize")
    return(data)
  }
  
  # Standardize columns
  standardized_data <- data
  
  for (col in columns) {
    col_data <- data[[col]]
    
    if (all(is.na(col_data))) {
      warning("Column '", col, "' contains only missing values. Skipping.")
      next
    }
    
    if (sd(col_data, na.rm = TRUE) == 0) {
      warning("Column '", col, "' has zero standard deviation. Skipping.")
      next
    }
    
    # Standardize
    standardized_data[[col]] <- scale(col_data, center = center, scale = scale)[, 1]
  }
  
  return(standardized_data)
}

#' Apply min-max scaling to numeric variables
#' 
#' @param data Data frame with variables to scale
#' @param columns Columns to scale (NULL for all numeric)
#' @param new_min Minimum value in scaled data
#' @param new_max Maximum value in scaled data
#' @return Data frame with scaled variables
#' @export
minmax_scale_variables <- function(data, columns = NULL, new_min = 0, new_max = 1) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Check if columns exist
  invalid_cols <- setdiff(columns, names(data))
  
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }
  
  # Filter for numeric columns
  non_numeric <- columns[!sapply(data[, columns, drop = FALSE], is.numeric)]
  
  if (length(non_numeric) > 0) {
    warning("Non-numeric columns will be skipped: ", paste(non_numeric, collapse = ", "))
    columns <- setdiff(columns, non_numeric)
  }
  
  if (length(columns) == 0) {
    warning("No numeric columns to scale")
    return(data)
  }
  
  # Scale columns
  scaled_data <- data
  
  for (col in columns) {
    col_data <- data[[col]]
    
    if (all(is.na(col_data))) {
      warning("Column '", col, "' contains only missing values. Skipping.")
      next
    }
    
    # Get min and max
    min_val <- min(col_data, na.rm = TRUE)
    max_val <- max(col_data, na.rm = TRUE)
    
    if (min_val == max_val) {
      warning("Column '", col, "' has constant value. Skipping.")
      next
    }
    
    # Apply min-max scaling
    scaled_data[[col]] <- new_min + (col_data - min_val) * 
                        (new_max - new_min) / (max_val - min_val)
  }
  
  return(scaled_data)
}

#' Apply robust scaling to numeric variables
#' 
#' @param data Data frame with variables to scale
#' @param columns Columns to scale (NULL for all numeric)
#' @param center Whether to center variables (subtract median)
#' @param scale Whether to scale variables (divide by IQR)
#' @return Data frame with scaled variables
#' @export
robust_scale_variables <- function(data, columns = NULL, center = TRUE, scale = TRUE) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Check if columns exist
  invalid_cols <- setdiff(columns, names(data))
  
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }
  
  # Filter for numeric columns
  non_numeric <- columns[!sapply(data[, columns, drop = FALSE], is.numeric)]
  
  if (length(non_numeric) > 0) {
    warning("Non-numeric columns will be skipped: ", paste(non_numeric, collapse = ", "))
    columns <- setdiff(columns, non_numeric)
  }
  
  if (length(columns) == 0) {
    warning("No numeric columns to scale")
    return(data)
  }
  
  # Scale columns
  scaled_data <- data
  
  for (col in columns) {
    col_data <- data[[col]]
    
    if (all(is.na(col_data))) {
      warning("Column '", col, "' contains only missing values. Skipping.")
      next
    }
    
    # Get median and IQR
    med <- median(col_data, na.rm = TRUE)
    iqr_val <- IQR(col_data, na.rm = TRUE)
    
    if (iqr_val == 0) {
      warning("Column '", col, "' has zero IQR. Using MAD instead.")
      # Use median absolute deviation instead
      mad_val <- mad(col_data, na.rm = TRUE)
      
      if (mad_val == 0) {
        warning("Column '", col, "' has zero MAD. Skipping.")
        next
      }
      
      # Apply robust scaling with MAD
      if (center && scale) {
        scaled_data[[col]] <- (col_data - med) / mad_val
      } else if (center) {
        scaled_data[[col]] <- col_data - med
      } else if (scale) {
        scaled_data[[col]] <- col_data / mad_val
      }
    } else {
      # Apply robust scaling with IQR
      if (center && scale) {
        scaled_data[[col]] <- (col_data - med) / iqr_val
      } else if (center) {
        scaled_data[[col]] <- col_data - med
      } else if (scale) {
        scaled_data[[col]] <- col_data / iqr_val
      }
    }
  }
  
  return(scaled_data)
}

#' Apply various data transformations to numeric variables
#' 
#' @param data Data frame with variables to transform
#' @param columns Columns to transform (NULL for all numeric)
#' @param method Transformation method ("log", "sqrt", "boxcox", "yeo-johnson", "quantile")
#' @return Data frame with transformed variables
#' @export
transform_variables <- function(data, columns = NULL, 
                              method = "log") {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Check if columns exist
  invalid_cols <- setdiff(columns, names(data))
  
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }
  
  # Filter for numeric columns
  non_numeric <- columns[!sapply(data[, columns, drop = FALSE], is.numeric)]
  
  if (length(non_numeric) > 0) {
    warning("Non-numeric columns will be skipped: ", paste(non_numeric, collapse = ", "))
    columns <- setdiff(columns, non_numeric)
  }
  
  if (length(columns) == 0) {
    warning("No numeric columns to transform")
    return(data)
  }
  
  # Transform columns
  transformed_data <- data
  
  for (col in columns) {
    col_data <- data[[col]]
    
    if (all(is.na(col_data))) {
      warning("Column '", col, "' contains only missing values. Skipping.")
      next
    }
    
    # Apply transformation
    transformed <- switch(
      tolower(method),
      "log" = {
        if (any(col_data <= 0, na.rm = TRUE)) {
          min_val <- min(col_data, na.rm = TRUE)
          offset <- ifelse(min_val <= 0, abs(min_val) + 1, 0)
          warning("Column '", col, "' contains non-positive values. Adding offset of ", offset, ".")
          log(col_data + offset)
        } else {
          log(col_data)
        }
      },
      "sqrt" = {
        if (any(col_data < 0, na.rm = TRUE)) {
          min_val <- min(col_data, na.rm = TRUE)
          offset <- ifelse(min_val < 0, abs(min_val) + 1e-6, 0)
          warning("Column '", col, "' contains negative values. Adding offset of ", offset, ".")
          sqrt(col_data + offset)
        } else {
          sqrt(col_data)
        }
      },
      "boxcox" = {
        if (requireNamespace("MASS", quietly = TRUE)) {
          if (any(col_data <= 0, na.rm = TRUE)) {
            min_val <- min(col_data, na.rm = TRUE)
            offset <- ifelse(min_val <= 0, abs(min_val) + 1, 0)
            warning("Column '", col, "' contains non-positive values. Adding offset of ", offset, ".")
            col_data <- col_data + offset
          }
          
          # Find optimal lambda
          bc <- MASS::boxcox(col_data ~ 1, plotit = FALSE)
          lambda <- bc$x[which.max(bc$y)]
          
          if (abs(lambda) < 0.01) {
            # For lambda near zero, use log
            log(col_data)
          } else {
            # For other lambda values, use power transformation
            (col_data^lambda - 1) / lambda
          }
        } else {
          warning("Package 'MASS' not available for Box-Cox transformation. Using log transform.")
          if (any(col_data <= 0, na.rm = TRUE)) {
            min_val <- min(col_data, na.rm = TRUE)
            offset <- ifelse(min_val <= 0, abs(min_val) + 1, 0)
            log(col_data + offset)
          } else {
            log(col_data)
          }
        }
      },
      "yeo-johnson" = {
        if (requireNamespace("bestNormalize", quietly = TRUE)) {
          yeojohnson_obj <- bestNormalize::yeojohnson(col_data)
          yeojohnson_obj$x.t
        } else {
          warning("Package 'bestNormalize' not available for Yeo-Johnson transformation. Using log transform.")
          if (any(col_data <= 0, na.rm = TRUE)) {
            min_val <- min(col_data, na.rm = TRUE)
            offset <- ifelse(min_val <= 0, abs(min_val) + 1, 0)
            log(col_data + offset)
          } else {
            log(col_data)
          }
        }
      },
      "quantile" = {
        if (requireNamespace("bestNormalize", quietly = TRUE)) {
          orderNorm_obj <- bestNormalize::orderNorm(col_data)
          orderNorm_obj$x.t
        } else {
          warning("Package 'bestNormalize' not available for quantile transformation. Using rank transform.")
          # Simple rank-based normalization
          ranks <- rank(col_data, na.last = "keep", ties.method = "average")
          n <- sum(!is.na(col_data))
          qnorm((ranks - 0.5) / n)
        }
      },
      # Default to log transform
      {
        warning("Unknown transformation method '", method, "'. Using log transform.")
        if (any(col_data <= 0, na.rm = TRUE)) {
          min_val <- min(col_data, na.rm = TRUE)
          offset <- ifelse(min_val <= 0, abs(min_val) + 1, 0)
          log(col_data + offset)
        } else {
          log(col_data)
        }
      }
    )
    
    # Set transformed values
    transformed_data[[col]] <- transformed
  }
  
  return(transformed_data)
}

#' Suggest optimal data transformation for skewed variables
#' 
#' @param data Data frame with variables to analyze
#' @param columns Columns to analyze (NULL for all numeric)
#' @param skewness_threshold Threshold for detecting skewed distributions
#' @return List with transformation suggestions
#' @export
suggest_transformations <- function(data, columns = NULL, skewness_threshold = 0.8) {
  # Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Check if moments package is available
  if (!requireNamespace("moments", quietly = TRUE)) {
    warning("Package 'moments' not available. Using simplified skewness calculation.")
    has_moments <- FALSE
  } else {
    has_moments <- TRUE
  }
  
  # If no columns specified, use all numeric columns
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Check if columns exist
  invalid_cols <- setdiff(columns, names(data))
  
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }
  
  # Filter for numeric columns
  non_numeric <- columns[!sapply(data[, columns, drop = FALSE], is.numeric)]
  
  if (length(non_numeric) > 0) {
    warning("Non-numeric columns will be skipped: ", paste(non_numeric, collapse = ", "))
    columns <- setdiff(columns, non_numeric)
  }
  
  if (length(columns) == 0) {
    warning("No numeric columns to analyze")
    return(list(
      suggestions = data.frame(
        Column = character(0),
        Skewness = numeric(0),
        Suggestion = character(0),
        stringsAsFactors = FALSE
      ),
      message = "No numeric columns to analyze"
    ))
  }
  
  # Analyze columns
  results <- data.frame(
    Column = character(0),
    Skewness = numeric(0),
    Suggestion = character(0),
    stringsAsFactors = FALSE
  )
  
  for (col in columns) {
    col_data <- data[[col]]
    
    if (all(is.na(col_data))) {
      next  # Skip if all NA
    }
    
    # Calculate skewness
    if (has_moments) {
      skewness <- moments::skewness(col_data, na.rm = TRUE)
    } else {
      # Simple skewness calculation
      n <- sum(!is.na(col_data))
      m <- mean(col_data, na.rm = TRUE)
      s <- sd(col_data, na.rm = TRUE)
      
      if (s == 0) {
        skewness <- 0
      } else {
        skewness <- sum((col_data - m)^3, na.rm = TRUE) / ((n - 1) * s^3)
      }
    }
    
    # Determine suggestion based on skewness
    if (abs(skewness) <= skewness_threshold) {
      suggestion <- "No transformation needed"
    } else if (skewness > skewness_threshold) {
      # Positive skew
      if (any(col_data <= 0, na.rm = TRUE)) {
        suggestion <- "Yeo-Johnson or Quantile transformation"
      } else if (skewness > 3) {
        suggestion <- "Log transformation"
      } else {
        suggestion <- "Square root transformation"
      }
    } else {  # skewness < -skewness_threshold
      # Negative skew
      if (any(col_data <= 0, na.rm = TRUE)) {
        suggestion <- "Yeo-Johnson or Quantile transformation"
      } else {
        suggestion <- "Square transformation"
      }
    }
    
    # Add to results
    results <- rbind(
      results,
      data.frame(
        Column = col,
        Skewness = skewness,
        Suggestion = suggestion,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Sort by absolute skewness
  results <- results[order(abs(results$Skewness), decreasing = TRUE), ]
  
  # Create message
  skewed_cols <- results$Column[abs(results$Skewness) > skewness_threshold]
  
  if (length(skewed_cols) == 0) {
    message <- "No variables with significant skewness detected."
  } else {
    message <- paste0(
      length(skewed_cols), " variable(s) with significant skewness detected. ",
      "Consider transformations for: ", paste(skewed_cols[1:min(3, length(skewed_cols))], collapse = ", "),
      ifelse(length(skewed_cols) > 3, "...", "")
    )
  }
  
  return(list(
    suggestions = results,
    message = message
  ))
}
