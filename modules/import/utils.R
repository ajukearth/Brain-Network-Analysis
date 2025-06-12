#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/import/utils.R - Import module utility functions
#########################################################

#' Validate data structure for network analysis
#' 
#' @param data Data frame to validate
#' @param required_columns Vector of column names that must be present
#' @param id_column Column name containing subject IDs
#' @param group_columns Vector of column names for grouping variables
#' @return A list with validation results and messages
#' @export
validate_data <- function(data, required_columns, id_column, group_columns) {
  # Initialize validation results
  results <- list(
    valid = TRUE,
    messages = character()
  )
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    results$valid <- FALSE
    results$messages <- c(results$messages, "The provided data is not a valid data frame.")
    return(results)
  }
  
  # Check for required columns
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    results$valid <- FALSE
    results$messages <- c(results$messages, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check ID column
  if (id_column %in% names(data)) {
    # Check for duplicate IDs
    id_values <- data[[id_column]]
    if (any(duplicated(id_values))) {
      results$valid <- FALSE
      dup_ids <- unique(id_values[duplicated(id_values)])
      results$messages <- c(results$messages, 
                           paste("Duplicate values found in ID column:", paste(head(dup_ids, 5), collapse = ", "),
                                 if(length(dup_ids) > 5) "... and more."))
    }
  }
  
  # Check group columns
  for (col in group_columns) {
    if (col %in% names(data)) {
      # Check if group column has at least 2 distinct values
      distinct_values <- unique(data[[col]])
      distinct_values <- distinct_values[!is.na(distinct_values)]
      
      if (length(distinct_values) < 2) {
        results$valid <- FALSE
        results$messages <- c(results$messages, 
                             paste("Group column", col, "has fewer than 2 distinct values."))
      }
    }
  }
  
  # Check for numeric columns (potential brain regions)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c(id_column, group_columns))
  
  if (length(numeric_cols) < 3) {
    results$valid <- FALSE
    results$messages <- c(results$messages, 
                         "At least 3 numeric columns (brain regions) are required for network analysis.")
  }
  
  # Check sample size
  if (nrow(data) < 5) {
    results$valid <- FALSE
    results$messages <- c(results$messages, 
                         "Sample size is too small. At least 5 subjects are recommended for reliable analysis.")
  }
  
  # Return validation results
  return(results)
}

#' Generate summary statistics for imported data
#' 
#' @param data Data frame containing the data
#' @param region_columns Vector of column names for brain regions
#' @param group_column Column name for grouping (optional)
#' @return A list with summary statistics
#' @export
generate_data_summary <- function(data, region_columns, group_column = NULL) {
  # Overall summary
  overall_summary <- list(
    n_subjects = nrow(data),
    n_regions = length(region_columns),
    region_names = region_columns
  )
  
  # Summary by region
  region_summary <- lapply(region_columns, function(region) {
    values <- data[[region]]
    list(
      region = region,
      mean = mean(values, na.rm = TRUE),
      median = median(values, na.rm = TRUE),
      sd = sd(values, na.rm = TRUE),
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      missing = sum(is.na(values)),
      missing_percent = round(100 * sum(is.na(values)) / length(values), 1)
    )
  })
  
  # Summary by group (if group_column is provided)
  group_summary <- NULL
  if (!is.null(group_column) && group_column %in% names(data)) {
    groups <- unique(data[[group_column]])
    group_summary <- lapply(groups, function(group) {
      group_data <- data[data[[group_column]] == group, ]
      
      # Region statistics for this group
      region_stats <- lapply(region_columns, function(region) {
        values <- group_data[[region]]
        list(
          region = region,
          mean = mean(values, na.rm = TRUE),
          median = median(values, na.rm = TRUE),
          sd = sd(values, na.rm = TRUE),
          min = min(values, na.rm = TRUE),
          max = max(values, na.rm = TRUE),
          missing = sum(is.na(values)),
          missing_percent = round(100 * sum(is.na(values)) / length(values), 1)
        )
      })
      
      list(
        group = group,
        n_subjects = nrow(group_data),
        region_stats = region_stats
      )
    })
    names(group_summary) <- groups
  }
  
  # Combine all summaries
  list(
    overall = overall_summary,
    by_region = region_summary,
    by_group = group_summary
  )
}

#' Check for common data issues in the imported data
#' 
#' @param data Data frame containing the data
#' @param region_columns Vector of column names for brain regions
#' @return A list with identified issues
#' @export
check_data_issues <- function(data, region_columns) {
  issues <- list(
    missing_data = list(
      has_missing = FALSE,
      total_missing = 0,
      missing_by_region = NULL,
      missing_by_subject = NULL
    ),
    outliers = list(
      has_outliers = FALSE,
      outlier_count = 0,
      outliers_by_region = NULL
    ),
    low_variance = list(
      has_low_var = FALSE,
      low_var_regions = NULL
    ),
    high_correlation = list(
      has_high_corr = FALSE,
      high_corr_pairs = NULL
    )
  )
  
  # Check for missing data
  missing_by_region <- sapply(region_columns, function(col) sum(is.na(data[[col]])))
  missing_by_subject <- rowSums(is.na(data[, region_columns, drop = FALSE]))
  
  total_missing <- sum(missing_by_region)
  issues$missing_data$has_missing <- total_missing > 0
  issues$missing_data$total_missing <- total_missing
  issues$missing_data$missing_by_region <- missing_by_region
  issues$missing_data$missing_by_subject <- missing_by_subject
  
  # Check for outliers using IQR method
  outliers_by_region <- lapply(region_columns, function(col) {
    values <- data[[col]]
    q1 <- quantile(values, 0.25, na.rm = TRUE)
    q3 <- quantile(values, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    outliers <- which(values < lower_bound | values > upper_bound)
    return(outliers)
  })
  names(outliers_by_region) <- region_columns
  
  outlier_count <- sum(sapply(outliers_by_region, length))
  issues$outliers$has_outliers <- outlier_count > 0
  issues$outliers$outlier_count <- outlier_count
  issues$outliers$outliers_by_region <- outliers_by_region
  
  # Check for low variance regions
  region_vars <- sapply(region_columns, function(col) var(data[[col]], na.rm = TRUE))
  low_var_threshold <- quantile(region_vars, 0.1, na.rm = TRUE)
  low_var_regions <- names(region_vars)[region_vars < low_var_threshold]
  
  issues$low_variance$has_low_var <- length(low_var_regions) > 0
  issues$low_variance$low_var_regions <- low_var_regions
  
  # Check for highly correlated regions
  if (length(region_columns) > 1) {
    region_data <- data[, region_columns, drop = FALSE]
    corr_matrix <- cor(region_data, use = "pairwise.complete.obs")
    
    # Find region pairs with correlation > 0.9
    high_corr_threshold <- 0.9
    high_corr_pairs <- which(abs(corr_matrix) > high_corr_threshold & abs(corr_matrix) < 1, arr.ind = TRUE)
    
    if (nrow(high_corr_pairs) > 0) {
      # Convert to named pairs
      high_corr_pairs <- data.frame(
        region1 = region_columns[high_corr_pairs[, 1]],
        region2 = region_columns[high_corr_pairs[, 2]],
        correlation = corr_matrix[high_corr_pairs]
      )
      
      # Remove duplicate pairs (e.g., A-B and B-A)
      high_corr_pairs <- high_corr_pairs[high_corr_pairs$region1 < high_corr_pairs$region2, ]
      
      issues$high_correlation$has_high_corr <- nrow(high_corr_pairs) > 0
      issues$high_correlation$high_corr_pairs <- high_corr_pairs
    }
  }
  
  return(issues)
}
