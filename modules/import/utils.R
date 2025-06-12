#########################################################
# Enhanced Brain Network Analysis Shiny App
# Import/utils.R - Data import utility functions
#########################################################

#' Get file extension from a file path
#' 
#' @param file_path Path to the file
#' @return File extension (lowercase, without dot)
#' @export
get_file_extension <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  return(ext)
}

#' Detect file type based on extension
#' 
#' @param file_path Path to the file
#' @return File type descriptor
#' @export
detect_file_type <- function(file_path) {
  ext <- get_file_extension(file_path)
  
  file_type <- switch(ext,
                     "csv" = "csv",
                     "tsv" = "tsv",
                     "txt" = "text",
                     "xls" = "excel",
                     "xlsx" = "excel",
                     "rds" = "rds",
                     "rdata" = "rdata",
                     "rda" = "rdata",
                     "mat" = "matlab",
                     "sav" = "spss",
                     "json" = "json",
                     "unknown")
  
  return(file_type)
}

#' Get available sheets from an Excel file
#' 
#' @param file_path Path to the Excel file
#' @return Character vector with sheet names
#' @export
get_excel_sheets <- function(file_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' required to read Excel files")
  }
  
  tryCatch({
    sheets <- openxlsx::getSheetNames(file_path)
    return(sheets)
  }, error = function(e) {
    warning("Error reading Excel file: ", e$message)
    return(character(0))
  })
}

#' Read delimited text file with automatic delimiter detection
#' 
#' @param file_path Path to the text file
#' @param n_max Maximum number of rows to read for detection
#' @return List with delimiter and sample data
#' @export
detect_delimiter <- function(file_path, n_max = 5) {
  # Read the first few lines of the file
  lines <- readLines(file_path, n = n_max)
  
  # Remove blank lines
  lines <- lines[nchar(lines) > 0]
  
  if (length(lines) == 0) {
    return(list(delimiter = ",", reason = "Empty file"))
  }
  
  # Common delimiters to check
  delimiters <- c(",", "\t", ";", "|", " ")
  
  # Count occurrences of each delimiter in each line
  counts <- sapply(delimiters, function(d) {
    mean(sapply(lines, function(line) sum(gregexpr(d, line)[[1]] > 0)))
  })
  
  # If no delimiters found, default to comma
  if (all(counts == 0)) {
    return(list(delimiter = ",", reason = "No common delimiters found"))
  }
  
  # Check consistency of column count with each delimiter
  col_counts <- list()
  for (d in delimiters) {
    if (counts[d] > 0) {
      # Split lines by delimiter and count fields
      fields <- strsplit(lines, d, fixed = TRUE)
      col_counts[[d]] <- sapply(fields, length)
    }
  }
  
  # Find delimiter with most consistent column count
  consistency <- sapply(col_counts, function(cc) {
    if (length(cc) == 0) return(0)
    # Calculate consistency as inverse of variance in column count
    if (var(cc) == 0) return(Inf)  # Perfect consistency
    return(1 / var(cc))
  })
  
  # If perfect consistency found, use that delimiter
  perfect <- which(consistency == Inf)
  if (length(perfect) > 0) {
    best_delimiter <- names(perfect)[1]
    reason <- "Perfect consistency in column count"
  } else {
    # Otherwise use delimiter with highest consistency
    best_idx <- which.max(consistency)
    if (length(best_idx) == 0 || consistency[best_idx] == 0) {
      best_delimiter <- ","
      reason <- "Fallback to comma"
    } else {
      best_delimiter <- names(consistency)[best_idx]
      reason <- "Best consistency in column count"
    }
  }
  
  # Check if delimiter is tab character
  if (best_delimiter == "\t") {
    best_delimiter_name <- "tab"
  } else {
    best_delimiter_name <- best_delimiter
  }
  
  return(list(
    delimiter = best_delimiter,
    delimiter_name = best_delimiter_name,
    reason = reason
  ))
}

#' Read first few rows of a file to preview content
#' 
#' @param file_path Path to the file
#' @param file_type Type of file
#' @param n_max Maximum number of rows to read
#' @param ... Additional parameters passed to the read function
#' @return Data frame with preview data
#' @export
preview_file <- function(file_path, file_type = NULL, n_max = 10, ...) {
  # Detect file type if not provided
  if (is.null(file_type)) {
    file_type <- detect_file_type(file_path)
  }
  
  # Read preview based on file type
  preview_data <- switch(file_type,
                        "csv" = {
                          # Try to detect delimiter first
                          delim_info <- detect_delimiter(file_path)
                          utils::read.delim(file_path, sep = delim_info$delimiter, 
                                          header = TRUE, nrows = n_max, ...)
                        },
                        "tsv" = {
                          utils::read.delim(file_path, sep = "\t", header = TRUE, 
                                          nrows = n_max, ...)
                        },
                        "text" = {
                          # Try to detect delimiter first
                          delim_info <- detect_delimiter(file_path)
                          utils::read.delim(file_path, sep = delim_info$delimiter, 
                                          header = TRUE, nrows = n_max, ...)
                        },
                        "excel" = {
                          if (!requireNamespace("openxlsx", quietly = TRUE)) {
                            stop("Package 'openxlsx' required to read Excel files")
                          }
                          
                          # Get first sheet if not specified
                          sheet <- list(...)$sheet
                          if (is.null(sheet)) {
                            sheets <- openxlsx::getSheetNames(file_path)
                            sheet <- sheets[1]
                          }
                          
                          # Read data
                          df <- openxlsx::read.xlsx(file_path, sheet = sheet, rows = 1:(n_max + 1))
                          return(df)
                        },
                        "rds" = {
                          # For RDS, read the entire object but limit rows
                          data <- readRDS(file_path)
                          if (is.data.frame(data)) {
                            return(utils::head(data, n_max))
                          } else {
                            stop("RDS file does not contain a data frame")
                          }
                        },
                        "rdata" = {
                          # For RData, load into a new environment and get the first data frame
                          temp_env <- new.env()
                          load(file_path, envir = temp_env)
                          
                          # Find data frames in the environment
                          objects <- ls(temp_env)
                          data_frames <- objects[sapply(objects, function(obj) {
                            is.data.frame(temp_env[[obj]])
                          })]
                          
                          if (length(data_frames) == 0) {
                            stop("No data frames found in RData file")
                          }
                          
                          # Return the first data frame (limited rows)
                          return(utils::head(temp_env[[data_frames[1]]], n_max))
                        },
                        "matlab" = {
                          if (!requireNamespace("R.matlab", quietly = TRUE)) {
                            stop("Package 'R.matlab' required to read MATLAB files")
                          }
                          
                          # Read MATLAB file
                          mat_data <- R.matlab::readMat(file_path)
                          
                          # Try to find a matrix or data frame
                          data_obj <- NULL
                          for (name in names(mat_data)) {
                            obj <- mat_data[[name]]
                            if (is.matrix(obj) || is.data.frame(obj)) {
                              data_obj <- obj
                              break
                            }
                          }
                          
                          if (is.null(data_obj)) {
                            stop("No suitable data matrix found in MATLAB file")
                          }
                          
                          # Convert to data frame if necessary
                          if (is.matrix(data_obj)) {
                            data_obj <- as.data.frame(data_obj)
                          }
                          
                          # Limit rows
                          return(utils::head(data_obj, n_max))
                        },
                        "spss" = {
                          if (!requireNamespace("haven", quietly = TRUE)) {
                            stop("Package 'haven' required to read SPSS files")
                          }
                          
                          # Read SPSS file
                          spss_data <- haven::read_sav(file_path)
                          
                          # Limit rows
                          return(utils::head(spss_data, n_max))
                        },
                        "json" = {
                          if (!requireNamespace("jsonlite", quietly = TRUE)) {
                            stop("Package 'jsonlite' required to read JSON files")
                          }
                          
                          # Read JSON file
                          json_data <- jsonlite::fromJSON(file_path)
                          
                          # Try to convert to data frame if necessary
                          if (!is.data.frame(json_data)) {
                            if (is.list(json_data) && all(sapply(json_data, length) == 
                                                         length(json_data[[1]]))) {
                              json_data <- as.data.frame(json_data)
                            } else {
                              stop("JSON file does not contain a data frame structure")
                            }
                          }
                          
                          # Limit rows
                          return(utils::head(json_data, n_max))
                        },
                        # Default to CSV
                        utils::read.csv(file_path, header = TRUE, nrows = n_max, ...)
  )
  
  return(preview_data)
}

#' Import data from file with appropriate method
#' 
#' @param file_path Path to the file
#' @param file_type Type of file (auto-detected if NULL)
#' @param import_options List of import options
#' @return Imported data frame
#' @export
import_data <- function(file_path, file_type = NULL, import_options = NULL) {
  # Detect file type if not provided
  if (is.null(file_type)) {
    file_type <- detect_file_type(file_path)
  }
  
  # Set default import options if not provided
  if (is.null(import_options)) {
    import_options <- list(
      header = TRUE,
      skip = 0,
      na.strings = c("NA", "", ".", "N/A"),
      strip.white = TRUE,
      stringsAsFactors = FALSE
    )
  }
  
  # Extract options with defaults
  header <- ifelse(is.null(import_options$header), TRUE, import_options$header)
  skip <- ifelse(is.null(import_options$skip), 0, import_options$skip)
  na.strings <- ifelse(is.null(import_options$na.strings), 
                     c("NA", "", ".", "N/A"), import_options$na.strings)
  strip.white <- ifelse(is.null(import_options$strip.white), TRUE, import_options$strip.white)
  stringsAsFactors <- ifelse(is.null(import_options$stringsAsFactors), 
                           FALSE, import_options$stringsAsFactors)
  sheet <- import_options$sheet  # Excel only
  
  # Import data based on file type
  imported_data <- switch(file_type,
                         "csv" = {
                           # Try to detect delimiter first
                           delim_info <- detect_delimiter(file_path)
                           utils::read.delim(file_path, sep = delim_info$delimiter, 
                                           header = header, skip = skip, 
                                           na.strings = na.strings, 
                                           strip.white = strip.white,
                                           stringsAsFactors = stringsAsFactors)
                         },
                         "tsv" = {
                           utils::read.delim(file_path, sep = "\t", 
                                           header = header, skip = skip, 
                                           na.strings = na.strings, 
                                           strip.white = strip.white,
                                           stringsAsFactors = stringsAsFactors)
                         },
                         "text" = {
                           # Try to detect delimiter first
                           delim_info <- detect_delimiter(file_path)
                           utils::read.delim(file_path, sep = delim_info$delimiter, 
                                           header = header, skip = skip, 
                                           na.strings = na.strings, 
                                           strip.white = strip.white,
                                           stringsAsFactors = stringsAsFactors)
                         },
                         "excel" = {
                           if (!requireNamespace("openxlsx", quietly = TRUE)) {
                             stop("Package 'openxlsx' required to read Excel files")
                           }
                           
                           # Get first sheet if not specified
                           if (is.null(sheet)) {
                             sheets <- openxlsx::getSheetNames(file_path)
                             sheet <- sheets[1]
                           }
                           
                           # Read data
                           openxlsx::read.xlsx(file_path, sheet = sheet, 
                                            startRow = skip + 1,  # +1 because startRow=1 is first row
                                            colNames = header,
                                            na.strings = na.strings)
                         },
                         "rds" = {
                           data <- readRDS(file_path)
                           if (!is.data.frame(data)) {
                             warning("RDS file does not contain a data frame. Attempting to convert.")
                             data <- as.data.frame(data)
                           }
                           return(data)
                         },
                         "rdata" = {
                           # Load into a new environment
                           temp_env <- new.env()
                           load(file_path, envir = temp_env)
                           
                           # Find data frames in the environment
                           objects <- ls(temp_env)
                           data_frames <- objects[sapply(objects, function(obj) {
                             is.data.frame(temp_env[[obj]])
                           })]
                           
                           if (length(data_frames) == 0) {
                             stop("No data frames found in RData file")
                           }
                           
                           # Use specified object if provided
                           if (!is.null(import_options$object) && 
                               import_options$object %in% objects) {
                             data <- temp_env[[import_options$object]]
                             if (!is.data.frame(data)) {
                               warning("Specified object is not a data frame. Attempting to convert.")
                               data <- as.data.frame(data)
                             }
                           } else {
                             # Return the first data frame
                             data <- temp_env[[data_frames[1]]]
                           }
                           
                           return(data)
                         },
                         "matlab" = {
                           if (!requireNamespace("R.matlab", quietly = TRUE)) {
                             stop("Package 'R.matlab' required to read MATLAB files")
                           }
                           
                           # Read MATLAB file
                           mat_data <- R.matlab::readMat(file_path)
                           
                           # Try to find a matrix or data frame
                           data_obj <- NULL
                           for (name in names(mat_data)) {
                             obj <- mat_data[[name]]
                             if (is.matrix(obj) || is.data.frame(obj)) {
                               data_obj <- obj
                               break
                             }
                           }
                           
                           if (is.null(data_obj)) {
                             stop("No suitable data matrix found in MATLAB file")
                           }
                           
                           # Convert to data frame if necessary
                           if (is.matrix(data_obj)) {
                             data_obj <- as.data.frame(data_obj)
                           }
                           
                           return(data_obj)
                         },
                         "spss" = {
                           if (!requireNamespace("haven", quietly = TRUE)) {
                             stop("Package 'haven' required to read SPSS files")
                           }
                           
                           # Read SPSS file
                           haven::read_sav(file_path)
                         },
                         "json" = {
                           if (!requireNamespace("jsonlite", quietly = TRUE)) {
                             stop("Package 'jsonlite' required to read JSON files")
                           }
                           
                           # Read JSON file
                           json_data <- jsonlite::fromJSON(file_path)
                           
                           # Try to convert to data frame if necessary
                           if (!is.data.frame(json_data)) {
                             if (is.list(json_data) && all(sapply(json_data, length) == 
                                                          length(json_data[[1]]))) {
                               json_data <- as.data.frame(json_data)
                             } else {
                               stop("JSON file does not contain a data frame structure")
                             }
                           }
                           
                           return(json_data)
                         },
                         # Default to CSV
                         utils::read.csv(file_path, header = header, skip = skip, 
                                       na.strings = na.strings, 
                                       strip.white = strip.white,
                                       stringsAsFactors = stringsAsFactors)
  )
  
  return(imported_data)
}

#' Export data to various file formats
#' 
#' @param data Data frame to export
#' @param file_path Path to save the file
#' @param file_type Type of file to create
#' @param export_options List of export options
#' @return Logical indicating success
#' @export
export_data <- function(data, file_path, file_type = NULL, export_options = NULL) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  
  # Detect file type from extension if not provided
  if (is.null(file_type)) {
    file_type <- detect_file_type(file_path)
  }
  
  # Set default export options if not provided
  if (is.null(export_options)) {
    export_options <- list(
      row.names = FALSE,
      na = "",
      sep = ",",
      quote = TRUE
    )
  }
  
  # Extract options with defaults
  row.names <- ifelse(is.null(export_options$row.names), FALSE, export_options$row.names)
  na <- ifelse(is.null(export_options$na), "", export_options$na)
  sep <- ifelse(is.null(export_options$sep), ",", export_options$sep)
  quote <- ifelse(is.null(export_options$quote), TRUE, export_options$quote)
  sheet <- ifelse(is.null(export_options$sheet), "Sheet1", export_options$sheet)
  
  # Export data based on file type
  result <- tryCatch({
    switch(file_type,
          "csv" = {
            utils::write.csv(data, file = file_path, row.names = row.names, 
                           na = na, quote = quote)
          },
          "tsv" = {
            utils::write.table(data, file = file_path, row.names = row.names, 
                             na = na, quote = quote, sep = "\t")
          },
          "text" = {
            utils::write.table(data, file = file_path, row.names = row.names, 
                             na = na, quote = quote, sep = sep)
          },
          "excel" = {
            if (!requireNamespace("openxlsx", quietly = TRUE)) {
              stop("Package 'openxlsx' required to write Excel files")
            }
            
            # Create workbook
            wb <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb, sheet)
            openxlsx::writeData(wb, sheet, data, rowNames = row.names)
            openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
          },
          "rds" = {
            saveRDS(data, file = file_path)
          },
          "rdata" = {
            # Create a variable name from the file name
            var_name <- tools::file_path_sans_ext(basename(file_path))
            var_name <- gsub("[^a-zA-Z0-9_]", "_", var_name)
            
            # Ensure it's a valid R name
            if (!grepl("^[a-zA-Z]", var_name)) {
              var_name <- paste0("data_", var_name)
            }
            
            # Assign data to the variable
            assign(var_name, data)
            
            # Save to RData file
            save(list = var_name, file = file_path)
            
            # Clean up
            rm(list = var_name)
          },
          "json" = {
            if (!requireNamespace("jsonlite", quietly = TRUE)) {
              stop("Package 'jsonlite' required to write JSON files")
            }
            
            # Write JSON file
            jsonlite::write_json(data, path = file_path, 
                               pretty = TRUE, auto_unbox = TRUE)
          },
          # Default to CSV
          utils::write.csv(data, file = file_path, row.names = row.names, 
                         na = na, quote = quote)
    )
    TRUE  # Success
  }, error = function(e) {
    warning("Error exporting data: ", e$message)
    FALSE  # Failure
  })
  
  return(result)
}

#' Generate example data for testing or demonstration
#' 
#' @param n_subjects Number of subjects
#' @param n_regions Number of brain regions
#' @param n_groups Number of groups
#' @param missing_rate Rate of missing values (0-1)
#' @param correlation_structure Type of correlation structure
#' @return Data frame with example data
#' @export
generate_example_data <- function(n_subjects = 30, n_regions = 10, n_groups = 2, 
                                missing_rate = 0, correlation_structure = "random") {
  # Set seed for reproducibility
  set.seed(123)
  
  # Create subject IDs
  subject_ids <- paste0("S", sprintf("%03d", 1:n_subjects))
  
  # Create group assignments
  if (n_groups == 2) {
    groups <- factor(rep(c("Control", "Patient"), length.out = n_subjects))
  } else {
    group_names <- paste0("Group", 1:n_groups)
    groups <- factor(sample(group_names, n_subjects, replace = TRUE))
  }
  
  # Create additional demographics
  sex <- factor(sample(c("Male", "Female"), n_subjects, replace = TRUE))
  age <- round(rnorm(n_subjects, mean = 45, sd = 15))
  
  # Create correlation matrix for brain regions
  if (correlation_structure == "random") {
    # Random correlation matrix
    cor_matrix <- matrix(runif(n_regions^2, -0.3, 0.7), nrow = n_regions)
    # Make it symmetric
    cor_matrix[lower.tri(cor_matrix)] <- t(cor_matrix)[lower.tri(cor_matrix)]
    # Set diagonal to 1
    diag(cor_matrix) <- 1
    
    # Ensure it's positive definite
    cor_matrix <- Matrix::nearPD(cor_matrix)$mat
  } else if (correlation_structure == "block") {
    # Block diagonal correlation matrix (network structure)
    cor_matrix <- matrix(0.1, nrow = n_regions, ncol = n_regions)
    
    # Number of blocks (networks)
    n_blocks <- min(5, ceiling(n_regions / 3))
    
    # Assign regions to blocks
    block_sizes <- rep(floor(n_regions / n_blocks), n_blocks)
    # Add any remaining regions to the first block
    block_sizes[1] <- block_sizes[1] + (n_regions - sum(block_sizes))
    
    # Create block assignments
    block_assignments <- rep(1:n_blocks, times = block_sizes)
    
    # Add stronger correlations within blocks
    for (block in 1:n_blocks) {
      block_regions <- which(block_assignments == block)
      for (i in block_regions) {
        for (j in block_regions) {
          if (i != j) {
            cor_matrix[i, j] <- 0.6 + rnorm(1, 0, 0.1)
          }
        }
      }
    }
    
    # Set diagonal to 1
    diag(cor_matrix) <- 1
    
    # Ensure it's positive definite
    cor_matrix <- Matrix::nearPD(cor_matrix)$mat
  } else if (correlation_structure == "distance") {
    # Distance-based correlation (spatial decay)
    
    # Generate random 3D coordinates for regions
    coords <- matrix(rnorm(n_regions * 3), nrow = n_regions)
    
    # Calculate Euclidean distances
    dist_matrix <- as.matrix(dist(coords))
    
    # Convert distances to correlations (decay with distance)
    cor_matrix <- exp(-dist_matrix / max(dist_matrix) * 3)
    
    # Set diagonal to 1
    diag(cor_matrix) <- 1
  } else {
    # Default to identity matrix (no correlation)
    cor_matrix <- diag(n_regions)
  }
  
  # Generate multivariate normal data for brain regions
  region_data <- MASS::mvrnorm(n_subjects, mu = rep(0, n_regions), Sigma = cor_matrix)
  
  # Add group effect for some regions
  effect_regions <- sample(1:n_regions, size = ceiling(n_regions/3))
  
  for (i in 1:n_subjects) {
    if (groups[i] != "Control" && groups[i] != "Group1") {
      # Add effect for non-control groups
      region_data[i, effect_regions] <- region_data[i, effect_regions] + 
                                      rnorm(length(effect_regions), mean = 0.5, sd = 0.2)
    }
  }
  
  # Create region names
  region_names <- paste0("Region", sprintf("%02d", 1:n_regions))
  
  # Add noise and scale to realistic values
  region_data <- region_data * 2 + 5
  
  # Combine all data
  example_data <- data.frame(
    Subject = subject_ids,
    Group = groups,
    Sex = sex,
    Age = age
  )
  
  # Add region data with column names
  region_df <- as.data.frame(region_data)
  names(region_df) <- region_names
  example_data <- cbind(example_data, region_df)
  
  # Add behavioral measures
  example_data$CognitiveScore <- rowMeans(region_data[, 1:min(3, n_regions)]) + 
                               rnorm(n_subjects, 0, 1)
  example_data$MotorScore <- rowMeans(region_data[, (n_regions-min(3, n_regions)+1):n_regions]) + 
                           rnorm(n_subjects, 0, 1)
  
  # Add missing values if requested
  if (missing_rate > 0) {
    # Only add missing values to brain regions
    region_cols <- which(names(example_data) %in% region_names)
    
    # Total number of cells that could be missing
    total_cells <- n_subjects * length(region_cols)
    
    # Number of cells to set to NA
    n_missing <- round(total_cells * missing_rate)
    
    # Randomly select cells to set to NA
    missing_rows <- sample(1:n_subjects, size = n_missing, replace = TRUE)
    missing_cols <- sample(region_cols, size = n_missing, replace = TRUE)
    
    # Set selected cells to NA
    for (i in 1:n_missing) {
      example_data[missing_rows[i], missing_cols[i]] <- NA
    }
  }
  
  return(example_data)
}
