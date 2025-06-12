#########################################################
# Enhanced Brain Network Analysis Shiny App
# Import/helper.R - Helper functions for data import
#########################################################

#' Read data from various file formats
#' 
#' @param file_path Path to the data file
#' @param file_type Type of file ("csv", "excel", "tsv", etc.)
#' @param sheet_name Sheet name for Excel files
#' @param sep Separator for delimited files
#' @param decimal Decimal character
#' @param header Whether file has a header row
#' @return Imported data frame
#' @export
read_data_file <- function(file_path, file_type = NULL, sheet_name = NULL, 
                         sep = ",", decimal = ".", header = TRUE) {
  # Determine file type if not provided
  if (is.null(file_type)) {
    file_ext <- tolower(tools::file_ext(file_path))
    file_type <- switch(file_ext,
                       "csv" = "csv",
                       "tsv" = "tsv",
                       "txt" = "txt",
                       "xlsx" = "excel",
                       "xls" = "excel",
                       "rds" = "rds",
                       "rdata" = "rdata",
                       "unknown")
  }
  
  # Import based on file type
  data <- switch(file_type,
                "csv" = utils::read.csv(file_path, sep = sep, dec = decimal, 
                                       header = header, stringsAsFactors = FALSE),
                "tsv" = utils::read.delim(file_path, sep = "\t", dec = decimal, 
                                        header = header, stringsAsFactors = FALSE),
                "txt" = utils::read.table(file_path, sep = sep, dec = decimal, 
                                        header = header, stringsAsFactors = FALSE),
                "excel" = {
                  require(openxlsx)
                  # Get sheet names if not provided
                  if (is.null(sheet_name)) {
                    sheets <- openxlsx::getSheetNames(file_path)
                    sheet_name <- sheets[1]  # Use first sheet by default
                  }
                  openxlsx::read.xlsx(file_path, sheet = sheet_name)
                },
                "rds" = readRDS(file_path),
                "rdata" = {
                  temp_env <- new.env()
                  load(file_path, envir = temp_env)
                  # Return first object in the environment
                  objects <- ls(temp_env)
                  if (length(objects) == 0) {
                    stop("No objects found in RData file")
                  }
                  temp_env[[objects[1]]]
                },
                stop("Unsupported file type")
  )
  
  return(data)
}

#' Detect data columns by type
#' 
#' @param data Data frame to analyze
#' @return List with column names by type
#' @export
detect_column_types <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Initialize result lists
  result <- list(
    id_columns = character(0),
    factor_columns = character(0),
    numeric_columns = character(0),
    region_columns = character(0),
    behavioral_columns = character(0),
    date_columns = character(0),
    other_columns = character(0)
  )
  
  # Detect column types
  for (col_name in names(data)) {
    col <- data[[col_name]]
    
    # Try to determine type
    if (is.numeric(col)) {
      # Numeric columns
      result$numeric_columns <- c(result$numeric_columns, col_name)
      
      # Try to identify if this is likely a brain region column
      # Brain region columns often have names containing "region", "area", "ROI", etc.
      if (grepl("region|area|roi|brodmann|ba|ctx|nucleus|gyrus|matter|cortex|hippocampus|amygdala", 
                tolower(col_name))) {
        result$region_columns <- c(result$region_columns, col_name)
      } 
      # Behavioral columns often have names like "score", "behav", "test", etc.
      else if (grepl("score|test|behav|performance|assessment|cognitive|memory|attention|executive", 
                     tolower(col_name))) {
        result$behavioral_columns <- c(result$behavioral_columns, col_name)
      }
    } 
    else if (is.factor(col) || is.character(col)) {
      # Check if likely ID column (e.g., "ID", "Subject", etc.)
      if (grepl("id$|^id|subject|participant|patient|case", tolower(col_name))) {
        result$id_columns <- c(result$id_columns, col_name)
      }
      # Check if likely group column (e.g., "Group", "Condition", etc.)
      else if (grepl("group|condition|sex|gender|site|diagnosis|status|treatment", 
                    tolower(col_name))) {
        result$factor_columns <- c(result$factor_columns, col_name)
      }
      else {
        # Other categorical columns
        result$factor_columns <- c(result$factor_columns, col_name)
      }
    }
    else if (inherits(col, "Date") || inherits(col, "POSIXt")) {
      result$date_columns <- c(result$date_columns, col_name)
    }
    else {
      result$other_columns <- c(result$other_columns, col_name)
    }
  }
  
  # If no region columns detected based on names, use all numeric columns
  # that are not already classified as behavioral
  if (length(result$region_columns) == 0) {
    potential_regions <- setdiff(result$numeric_columns, result$behavioral_columns)
    
    # If there are many numeric columns, they're likely brain regions
    if (length(potential_regions) > 5) {
      result$region_columns <- potential_regions
    }
  }
  
  return(result)
}

#' Extract summary statistics for data
#' 
#' @param data Data frame to analyze
#' @param columns Optional subset of columns to analyze
#' @return Data frame with summary statistics
#' @export
get_data_summary <- function(data, columns = NULL) {
  if (is.null(columns)) {
    columns <- names(data)
  }
  
  # Initialize results
  summary_df <- data.frame(
    Column = character(0),
    Type = character(0),
    Missing = numeric(0),
    Missing_Percent = numeric(0),
    Mean = numeric(0),
    Median = numeric(0),
    SD = numeric(0),
    Min = numeric(0),
    Max = numeric(0),
    Unique_Values = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Calculate statistics for each column
  for (col_name in columns) {
    if (!col_name %in% names(data)) next
    
    col <- data[[col_name]]
    n <- length(col)
    n_missing <- sum(is.na(col))
    pct_missing <- round(100 * n_missing / n, 2)
    
    # Type-specific statistics
    if (is.numeric(col)) {
      mean_val <- round(mean(col, na.rm = TRUE), 2)
      median_val <- round(median(col, na.rm = TRUE), 2)
      sd_val <- round(sd(col, na.rm = TRUE), 2)
      min_val <- round(min(col, na.rm = TRUE), 2)
      max_val <- round(max(col, na.rm = TRUE), 2)
      uniq_val <- length(unique(col))
      type <- "numeric"
    } 
    else if (is.factor(col) || is.character(col)) {
      mean_val <- NA
      median_val <- NA
      sd_val <- NA
      min_val <- NA
      max_val <- NA
      uniq_val <- length(unique(col))
      type <- ifelse(is.factor(col), "factor", "character")
    }
    else if (inherits(col, "Date") || inherits(col, "POSIXt")) {
      mean_val <- NA
      median_val <- NA
      sd_val <- NA
      min_val <- as.character(min(col, na.rm = TRUE))
      max_val <- as.character(max(col, na.rm = TRUE))
      uniq_val <- length(unique(col))
      type <- "date"
    }
    else {
      mean_val <- NA
      median_val <- NA
      sd_val <- NA
      min_val <- NA
      max_val <- NA
      uniq_val <- NA
      type <- class(col)[1]
    }
    
    # Add to results
    summary_df <- rbind(summary_df, data.frame(
      Column = col_name,
      Type = type,
      Missing = n_missing,
      Missing_Percent = pct_missing,
      Mean = mean_val,
      Median = median_val,
      SD = sd_val,
      Min = min_val,
      Max = max_val,
      Unique_Values = uniq_val,
      stringsAsFactors = FALSE
    ))
  }
  
  return(summary_df)
}

#' Check for potential data issues
#' 
#' @param data Data frame to analyze
#' @param column_types List of column types from detect_column_types
#' @return List of warnings and issues
#' @export
check_data_issues <- function(data, column_types) {
  issues <- list(
    warnings = character(0),
    errors = character(0),
    missing_patterns = NULL
  )
  
  # Check if data exists
  if (is.null(data) || nrow(data) == 0) {
    issues$errors <- c(issues$errors, "No data provided or empty dataset.")
    return(issues)
  }
  
  # Check for too few subjects
  if (nrow(data) < 3) {
    issues$warnings <- c(issues$warnings, 
                        "Very small sample size (less than 3 subjects). Statistical reliability may be limited.")
  }
  
  # Check for missing ID column
  if (length(column_types$id_columns) == 0) {
    issues$warnings <- c(issues$warnings, 
                        "No ID column detected. Consider specifying a subject identifier.")
  }
  
  # Check for missing factor/group columns
  if (length(column_types$factor_columns) == 0) {
    issues$warnings <- c(issues$warnings, 
                        "No grouping variables detected. Comparisons between groups will not be possible.")
  }
  
  # Check for too few brain regions
  if (length(column_types$region_columns) < 3) {
    issues$warnings <- c(issues$warnings, 
                        "Too few brain regions detected (minimum 3 needed). Network analysis may be limited.")
  }
  
  # Check for high missingness in brain regions
  region_data <- data[, column_types$region_columns, drop = FALSE]
  region_missing <- colMeans(is.na(region_data))
  
  if (any(region_missing > 0.3)) {
    high_missing <- names(region_missing)[region_missing > 0.3]
    high_missing_str <- paste(high_missing[1:min(3, length(high_missing))], collapse = ", ")
    
    if (length(high_missing) > 3) {
      high_missing_str <- paste0(high_missing_str, ", and ", length(high_missing) - 3, " more")
    }
    
    issues$warnings <- c(issues$warnings, 
                        paste0("High missingness (>30%) in brain regions: ", high_missing_str))
  }
  
  # Check for constant columns (no variation)
  numeric_cols <- column_types$numeric_columns
  if (length(numeric_cols) > 0) {
    sd_values <- sapply(data[, numeric_cols, drop = FALSE], sd, na.rm = TRUE)
    constant_cols <- names(sd_values)[sd_values == 0 | is.na(sd_values)]
    
    if (length(constant_cols) > 0) {
      constant_cols_str <- paste(constant_cols[1:min(3, length(constant_cols))], collapse = ", ")
      
      if (length(constant_cols) > 3) {
        constant_cols_str <- paste0(constant_cols_str, ", and ", length(constant_cols) - 3, " more")
      }
      
      issues$warnings <- c(issues$warnings, 
                         paste0("Constant columns (no variation): ", constant_cols_str))
    }
  }
  
  # Analyze missing data patterns
  if (requireNamespace("naniar", quietly = TRUE)) {
    require(naniar)
    issues$missing_patterns <- naniar::vis_miss(data)
  }
  
  return(issues)
}
