#########################################################
# Enhanced Brain Network Analysis Shiny App
# utils/data_functions.R - Data handling utility functions
#########################################################

#' Load data from a CSV or Excel file
#' 
#' @param file_path Path to the file to load
#' @param sheet_name Sheet name (for Excel files)
#' @return A data frame containing the loaded data
#' @export
load_data <- function(file_path, sheet_name = NULL) {
  file_ext <- tools::file_ext(file_path)
  
  if (file_ext == "csv") {
    tryCatch({
      data <- readr::read_csv(file_path, col_types = readr::cols(.default = readr::col_guess()))
      return(as.data.frame(data))
    }, error = function(e) {
      stop(paste("Error loading CSV file:", e$message))
    })
  } else if (file_ext %in% c("xlsx", "xls")) {
    tryCatch({
      if (!is.null(sheet_name) && sheet_name != "") {
        data <- openxlsx::read.xlsx(file_path, sheet = sheet_name)
      } else {
        data <- openxlsx::read.xlsx(file_path)
      }
      return(data)
    }, error = function(e) {
      stop(paste("Error loading Excel file:", e$message))
    })
  } else {
    stop("Unsupported file format. Please upload a CSV or Excel file.")
  }
}

#' Get Excel sheet names from a file
#' 
#' @param file_path Path to the Excel file
#' @return Character vector of sheet names
#' @export
get_excel_sheets <- function(file_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for this function")
  }
  
  tryCatch({
    if (tools::file_ext(file_path) %in% c("xlsx", "xls")) {
      sheets <- openxlsx::getSheetNames(file_path)
      return(sheets)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("Error reading Excel sheets:", e$message))
    return(NULL)
  })
}

#' Validate data structure for network analysis
#' 
#' @param data Data frame to validate
#' @param required_columns Vector of column names that must be present
#' @param id_column Name of the ID column
#' @param group_columns Vector of column names containing grouping variables
#' @return List with validation results and messages
#' @export
validate_data <- function(data, required_columns = NULL, id_column = NULL, 
                          group_columns = NULL) {
  results <- list(
    valid = TRUE,
    messages = character(0)
  )
  
  # Check if data exists and is a data frame
  if (is.null(data) || !is.data.frame(data)) {
    results$valid <- FALSE
    results$messages <- c(results$messages, "Data is not a valid data frame.")
    return(results)
  }
  
  # Check minimum number of rows
  if (nrow(data) < 5) {
    results$valid <- FALSE
    results$messages <- c(results$messages, 
                          "Data has fewer than 5 rows. More data is needed for robust analysis.")
  }
  
  # Check required columns
  if (!is.null(required_columns)) {
    missing_cols <- required_columns[!required_columns %in% names(data)]
    if (length(missing_cols) > 0) {
      results$valid <- FALSE
      results$messages <- c(results$messages, 
                            paste("Required columns missing:", 
                                  paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Check ID column
  if (!is.null(id_column) && id_column %in% names(data)) {
    # Check if ID column has unique values
    if (any(duplicated(data[[id_column]]))) {
      results$valid <- FALSE
      results$messages <- c(results$messages, 
                            paste("ID column", id_column, 
                                  "contains duplicate values. Each subject must have a unique ID."))
    }
  }
  
  # Check if there are enough numeric columns for network analysis
  if (!is.null(id_column) && !is.null(group_columns)) {
    # Identify non-grouping columns
    non_group_cols <- setdiff(names(data), c(id_column, group_columns))
    
    # Count numeric columns
    numeric_cols <- sapply(data[, non_group_cols, drop = FALSE], is.numeric)
    num_numeric_cols <- sum(numeric_cols)
    
    if (num_numeric_cols < 3) {
      results$valid <- FALSE
      results$messages <- c(results$messages, 
                            "There are fewer than 3 numeric columns available for network analysis. 
                            Network analysis requires at least 3 brain regions (numeric columns).")
    }
  }
  
  # Check for missing values in critical columns
  if (!is.null(id_column)) {
    if (any(is.na(data[[id_column]]))) {
      results$valid <- FALSE
      results$messages <- c(results$messages, 
                            paste("ID column", id_column, "contains missing values."))
    }
  }
  
  if (!is.null(group_columns)) {
    for (col in group_columns) {
      if (col %in% names(data) && any(is.na(data[[col]]))) {
        results$valid <- FALSE
        results$messages <- c(results$messages, 
                              paste("Group column", col, "contains missing values."))
      }
    }
  }
  
  return(results)
}

#' Generate a summary of data quality
#' 
#' @param data Data frame to analyze
#' @param region_columns Vector of column names containing brain region data
#' @return List with data quality metrics
#' @export
data_quality_summary <- function(data, region_columns) {
  if (!requireNamespace("naniar", quietly = TRUE)) {
    warning("Package 'naniar' is required for comprehensive missing data analysis")
  }
  
  # Initialize results
  summary <- list(
    row_count = nrow(data),
    column_count = ncol(data),
    region_count = length(region_columns),
    missing_values = 0,
    missing_percentage = 0,
    complete_rows = 0,
    complete_row_percentage = 0,
    region_summaries = list()
  )
  
  if (length(region_columns) > 0) {
    # Calculate missing values
    region_data <- data[, region_columns, drop = FALSE]
    missing_values <- sum(is.na(region_data))
    total_values <- nrow(data) * length(region_columns)
    
    summary$missing_values <- missing_values
    summary$missing_percentage <- round(100 * missing_values / total_values, 2)
    
    # Count complete rows
    complete_rows <- sum(complete.cases(region_data))
    summary$complete_rows <- complete_rows
    summary$complete_row_percentage <- round(100 * complete_rows / nrow(data), 2)
    
    # Generate summaries for each region
    summary$region_summaries <- lapply(region_columns, function(col) {
      col_data <- data[[col]]
      list(
        name = col,
        min = min(col_data, na.rm = TRUE),
        max = max(col_data, na.rm = TRUE),
        mean = mean(col_data, na.rm = TRUE),
        median = median(col_data, na.rm = TRUE),
        sd = sd(col_data, na.rm = TRUE),
        missing = sum(is.na(col_data)),
        missing_percentage = round(100 * sum(is.na(col_data)) / length(col_data), 2)
      )
    })
    
    # If naniar is available, add more detailed missing data analysis
    if (requireNamespace("naniar", quietly = TRUE)) {
      miss_var_summary <- naniar::miss_var_summary(region_data)
      summary$missing_by_variable <- miss_var_summary
      
      if (nrow(data) > 1) {
        tryCatch({
          miss_case_summary <- naniar::miss_case_summary(region_data)
          summary$missing_by_case <- miss_case_summary
        }, error = function(e) {
          # Skip this if it fails
        })
      }
    }
  }
  
  return(summary)
}

#' Create sample data for template download
#' 
#' @param n_subjects Number of subjects to include in the sample
#' @param n_regions Number of brain regions to include
#' @param n_groups Number of groups to include
#' @return A data frame containing sample data
#' @export
create_sample_data <- function(n_subjects = 10, n_regions = 15, n_groups = 2) {
  # Create subject IDs
  subjects <- paste0("Sub", sprintf("%03d", 1:n_subjects))
  
  # Create group assignments
  groups <- c("Control", "Treatment")
  if (n_groups > 2) {
    groups <- c(groups, paste0("Group", 3:n_groups))
  }
  group_assignment <- sample(groups[1:min(n_groups, length(groups))], n_subjects, replace = TRUE)
  
  # Create sex variable
  sex <- sample(c("Male", "Female"), n_subjects, replace = TRUE)
  
  # Create age variable
  age <- round(runif(n_subjects, 18, 65))
  
  # Create behavioral variables
  behavior1 <- round(rnorm(n_subjects, 50, 10))
  behavior2 <- round(rnorm(n_subjects, 75, 15))
  
  # Create brain region data
  region_data <- matrix(
    rnorm(n_subjects * n_regions, 3.5, 0.8),
    nrow = n_subjects,
    ncol = n_regions
  )
  
  # Add correlation structure between regions
  for (i in 1:(n_regions-1)) {
    for (j in (i+1):n_regions) {
      if (runif(1) < 0.3) {  # 30% chance of correlation
        corr_strength <- runif(1, 0.3, 0.8)
        shared_variance <- rnorm(n_subjects, 0, 1)
        region_data[, i] <- region_data[, i] + corr_strength * shared_variance
        region_data[, j] <- region_data[, j] + corr_strength * shared_variance
      }
    }
  }
  
  # Create region names
  region_names <- paste0("Region", sprintf("%02d", 1:n_regions))
  
  # Combine everything into a data frame
  result <- data.frame(
    Subject = subjects,
    Group = group_assignment,
    Sex = sex,
    Age = age,
    Behavior1 = behavior1,
    Behavior2 = behavior2
  )
  
  # Add brain region columns
  region_df <- as.data.frame(region_data)
  names(region_df) <- region_names
  
  result <- cbind(result, region_df)
  
  # Add some missing values to make it realistic
  n_missing <- round(n_subjects * n_regions * 0.05)  # 5% missing
  if (n_missing > 0) {
    missing_rows <- sample(1:n_subjects, n_missing, replace = TRUE)
    missing_cols <- sample(7:(6+n_regions), n_missing, replace = TRUE)
    for (i in 1:n_missing) {
      result[missing_rows[i], missing_cols[i]] <- NA
    }
  }
  
  return(result)
}
