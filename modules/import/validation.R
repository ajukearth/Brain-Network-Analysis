#########################################################
# Enhanced Brain Network Analysis Shiny App
# Import/validation.R - Data validation functions
#########################################################

#' Validate uploaded data for brain network analysis
#' 
#' @param data Data frame to validate
#' @param requirements List of validation requirements
#' @return List with validation results
#' @export
validate_brain_data <- function(data, requirements = NULL) {
  # Default requirements
  if (is.null(requirements)) {
    requirements <- list(
      min_subjects = 3,
      min_regions = 3,
      max_missing_pct = 0.3,
      require_id = TRUE,
      require_group = TRUE
    )
  }
  
  # Initialize results
  results <- list(
    valid = TRUE,
    warnings = character(0),
    errors = character(0),
    messages = character(0),
    column_types = NULL
  )
  
  # Check if data exists
  if (is.null(data) || nrow(data) == 0) {
    results$valid <- FALSE
    results$errors <- c(results$errors, "No data provided or empty dataset.")
    return(results)
  }
  
  # Get column types
  results$column_types <- detect_column_types(data)
  
  # 1. Check number of subjects
  if (nrow(data) < requirements$min_subjects) {
    results$warnings <- c(results$warnings, 
                        paste0("Insufficient subjects (found ", nrow(data), 
                              ", minimum ", requirements$min_subjects, " required)."))
  }
  
  # 2. Check ID column
  if (requirements$require_id && length(results$column_types$id_columns) == 0) {
    results$warnings <- c(results$warnings, 
                        "No subject ID column detected. Consider specifying a subject identifier.")
  }
  
  # 3. Check group column
  if (requirements$require_group && length(results$column_types$factor_columns) == 0) {
    results$warnings <- c(results$warnings, 
                        "No grouping variables detected. Comparisons between groups will not be possible.")
  }
  
  # 4. Check number of brain regions
  if (length(results$column_types$region_columns) < requirements$min_regions) {
    if (length(results$column_types$numeric_columns) >= requirements$min_regions) {
      results$messages <- c(results$messages, 
                           paste0("No brain region columns automatically detected. ",
                                 "Using all numeric columns as brain regions."))
      
      # Use all numeric columns as brain regions
      results$column_types$region_columns <- results$column_types$numeric_columns
    } else {
      results$valid <- FALSE
      results$errors <- c(results$errors, 
                         paste0("Insufficient brain regions (found ", 
                               length(results$column_types$region_columns), 
                               ", minimum ", requirements$min_regions, " required)."))
    }
  }
  
  # 5. Check missingness in brain regions
  region_data <- data[, results$column_types$region_columns, drop = FALSE]
  region_missing <- colMeans(is.na(region_data))
  
  high_missing_cols <- names(region_missing)[region_missing > requirements$max_missing_pct]
  if (length(high_missing_cols) > 0) {
    if (length(high_missing_cols) <= 3) {
      high_missing_str <- paste(high_missing_cols, collapse = ", ")
    } else {
      high_missing_str <- paste0(paste(high_missing_cols[1:3], collapse = ", "), 
                               ", and ", length(high_missing_cols) - 3, " more")
    }
    
    results$warnings <- c(results$warnings, 
                        paste0("High missingness (>", 
                              100*requirements$max_missing_pct, 
                              "%) in brain regions: ", high_missing_str, 
                              ". Consider using imputation."))
  }
  
  # 6. Check for constant columns (no variation)
  sd_values <- sapply(region_data, sd, na.rm = TRUE)
  constant_cols <- names(sd_values)[sd_values == 0 | is.na(sd_values)]
  
  if (length(constant_cols) > 0) {
    if (length(constant_cols) <= 3) {
      constant_cols_str <- paste(constant_cols, collapse = ", ")
    } else {
      constant_cols_str <- paste0(paste(constant_cols[1:3], collapse = ", "), 
                                ", and ", length(constant_cols) - 3, " more")
    }
    
    results$warnings <- c(results$warnings, 
                        paste0("Constant columns (no variation): ", constant_cols_str, 
                              ". These will be excluded from analysis."))
  }
  
  # 7. Check for outliers
  z_scores <- scale(region_data)
  extreme_values <- abs(z_scores) > 3
  
  if (any(extreme_values, na.rm = TRUE)) {
    # Count outliers per column
    outlier_counts <- colSums(extreme_values, na.rm = TRUE)
    outlier_cols <- names(outlier_counts)[outlier_counts > 0]
    
    if (length(outlier_cols) > 0) {
      if (length(outlier_cols) <= 3) {
        outlier_cols_str <- paste(outlier_cols, collapse = ", ")
      } else {
        outlier_cols_str <- paste0(paste(outlier_cols[1:3], collapse = ", "), 
                                 ", and ", length(outlier_cols) - 3, " more")
      }
      
      results$warnings <- c(results$warnings, 
                          paste0("Potential outliers detected in columns: ", 
                                outlier_cols_str, 
                                ". Consider reviewing data or using robust methods."))
    }
  }
  
  # 8. Check distributions for normality
  if (requireNamespace("moments", quietly = TRUE)) {
    require(moments)
    
    # Calculate skewness for each region
    skew_values <- sapply(region_data, function(x) moments::skewness(x, na.rm = TRUE))
    highly_skewed <- names(skew_values)[abs(skew_values) > 1.5]
    
    if (length(highly_skewed) > 0) {
      if (length(highly_skewed) <= 3) {
        skewed_cols_str <- paste(highly_skewed, collapse = ", ")
      } else {
        skewed_cols_str <- paste0(paste(highly_skewed[1:3], collapse = ", "), 
                                ", and ", length(highly_skewed) - 3, " more")
      }
      
      results$warnings <- c(results$warnings, 
                          paste0("Highly skewed distributions in columns: ", 
                                skewed_cols_str, 
                                ". Consider transformations or non-parametric methods."))
    }
  }
  
  return(results)
}

#' Validate column selection for analysis
#' 
#' @param data Data frame
#' @param id_cols Selected ID columns
#' @param group_cols Selected group columns
#' @param region_cols Selected brain region columns
#' @param behavior_cols Selected behavioral columns
#' @return List with validation results
#' @export
validate_column_selection <- function(data, id_cols, group_cols, region_cols, behavior_cols = NULL) {
  # Initialize results
  results <- list(
    valid = TRUE,
    warnings = character(0),
    errors = character(0)
  )
  
  # 1. Check if ID columns are specified
  if (length(id_cols) == 0) {
    results$warnings <- c(results$warnings, 
                        "No ID column selected. Recommend selecting at least one for subject identification.")
  }
  
  # 2. Check if group columns are specified
  if (length(group_cols) == 0) {
    results$warnings <- c(results$warnings, 
                        "No group columns selected. Group comparisons will not be possible.")
  }
  
  # 3. Check if enough brain regions are selected
  if (length(region_cols) < 3) {
    results$valid <- FALSE
    results$errors <- c(results$errors, 
                       "At least 3 brain regions must be selected for network analysis.")
  }
  
  # 4. Check if regions are numeric
  if (length(region_cols) > 0) {
    non_numeric <- region_cols[!sapply(data[, region_cols, drop = FALSE], is.numeric)]
    if (length(non_numeric) > 0) {
      results$valid <- FALSE
      results$errors <- c(results$errors, 
                         paste0("Non-numeric brain region columns selected: ", 
                               paste(non_numeric, collapse = ", "), 
                               ". All brain regions must be numeric."))
    }
  }
  
  # 5. Check if behavior columns are numeric
  if (!is.null(behavior_cols) && length(behavior_cols) > 0) {
    non_numeric <- behavior_cols[!sapply(data[, behavior_cols, drop = FALSE], is.numeric)]
    if (length(non_numeric) > 0) {
      results$warnings <- c(results$warnings, 
                          paste0("Non-numeric behavioral columns selected: ", 
                                paste(non_numeric, collapse = ", "), 
                                ". These will be excluded from correlation analyses."))
    }
  }
  
  # 6. Check for column overlap
  all_selected <- c(id_cols, group_cols, region_cols, behavior_cols)
  if (length(all_selected) != length(unique(all_selected))) {
    results$warnings <- c(results$warnings, 
                        "Some columns are assigned to multiple categories. This may cause unexpected behavior.")
  }
  
  return(results)
}

#' Check data completeness and suggest preprocessing steps
#' 
#' @param data Data frame
#' @param region_cols Selected brain region columns
#' @param group_cols Selected group columns
#' @param behavior_cols Selected behavioral columns
#' @return List with preprocessing recommendations
#' @export
check_preprocessing_needs <- function(data, region_cols, group_cols = NULL, behavior_cols = NULL) {
  # Initialize results
  results <- list(
    needs_imputation = FALSE,
    needs_normalization = FALSE,
    needs_outlier_treatment = FALSE,
    imputation_recommended = FALSE,
    normalization_recommended = FALSE,
    outlier_treatment_recommended = FALSE,
    messages = character(0)
  )
  
  # Check for missing values
  region_data <- data[, region_cols, drop = FALSE]
  missing_counts <- colSums(is.na(region_data))
  
  if (any(missing_counts > 0)) {
    missing_pct <- 100 * sum(missing_counts) / (nrow(data) * ncol(region_data))
    results$needs_imputation <- TRUE
    
    if (missing_pct > 10) {
      results$messages <- c(results$messages, 
                          paste0("High missingness (", round(missing_pct, 1), 
                                "%) in brain regions. Imputation strongly recommended."))
      results$imputation_recommended <- TRUE
    } else {
      results$messages <- c(results$messages, 
                          paste0("Some missing values (", round(missing_pct, 1), 
                                "%) in brain regions. Consider imputation."))
    }
  }
  
  # Check distribution properties
  if (requireNamespace("moments", quietly = TRUE)) {
    require(moments)
    
    # Calculate skewness for each region
    skew_values <- sapply(region_data, function(x) moments::skewness(x, na.rm = TRUE))
    highly_skewed <- sum(abs(skew_values) > 1.5)
    
    if (highly_skewed > 0) {
      skew_pct <- 100 * highly_skewed / length(region_cols)
      
      if (skew_pct > 25) {
        results$needs_normalization <- TRUE
        results$normalization_recommended <- TRUE
        results$messages <- c(results$messages, 
                            paste0(round(skew_pct, 1), 
                                  "% of brain regions have skewed distributions. ",
                                  "Consider normalization or transformation."))
      }
    }
  }
  
  # Check for outliers
  z_scores <- scale(region_data)
  extreme_values <- abs(z_scores) > 3
  
  if (any(extreme_values, na.rm = TRUE)) {
    outlier_pct <- 100 * sum(extreme_values, na.rm = TRUE) / sum(!is.na(region_data))
    
    if (outlier_pct > 1) {
      results$needs_outlier_treatment <- TRUE
      results$outlier_treatment_recommended <- TRUE
      results$messages <- c(results$messages, 
                          paste0("Multiple outliers detected (", round(outlier_pct, 1), 
                                "% of data points). Consider outlier treatment."))
    } else {
      results$messages <- c(results$messages, 
                          paste0("Few outliers detected (", round(outlier_pct, 1), 
                                "% of data points). May not require special treatment."))
    }
  }
  
  # Check group balance if groups are provided
  if (!is.null(group_cols) && length(group_cols) > 0) {
    for (group_col in group_cols) {
      group_counts <- table(data[[group_col]])
      min_count <- min(group_counts)
      max_count <- max(group_counts)
      
      if (min_count < 3) {
        results$messages <- c(results$messages, 
                            paste0("Small sample size (n=", min_count, 
                                  ") for group '", names(group_counts)[which.min(group_counts)], 
                                  "' in column '", group_col, 
                                  "'. Statistical power may be limited."))
      }
      
      if (max_count > 3 * min_count) {
        results$messages <- c(results$messages, 
                            paste0("Highly unbalanced groups in column '", group_col, 
                                  "'. Consider using appropriate statistical methods."))
      }
    }
  }
  
  return(results)
}
