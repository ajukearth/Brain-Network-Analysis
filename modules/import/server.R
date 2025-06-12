#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/import/server.R - Import module server logic
#########################################################

# Initialize reactive values for this module
import_rv <- reactiveValues(
  raw_data = NULL,
  processed_data = NULL,
  validation_complete = FALSE,
  column_info = NULL,
  has_missing_data = FALSE,
  file_type = NULL,
  file_name = NULL
)

# Check if uploaded file is Excel
output$is_excel_file <- reactive({
  req(input$import_file)
  tolower(tools::file_ext(input$import_file$name)) %in% c("xls", "xlsx")
})
outputOptions(output, "is_excel_file", suspendWhenHidden = FALSE)

# Update sheet choices when Excel file is uploaded
observeEvent(input$import_file, {
  file <- input$import_file
  if (is.null(file)) return(NULL)
  
  ext <- tolower(tools::file_ext(file$name))
  import_rv$file_type <- ext
  import_rv$file_name <- file$name
  
  if (ext %in% c("xls", "xlsx")) {
    # Read Excel sheet names
    sheets <- openxlsx::getSheetNames(file$datapath)
    updateSelectInput(session, "import_sheet", choices = sheets, selected = sheets[1])
  }
  
  # Reset processed data and validation
  import_rv$processed_data <- NULL
  import_rv$validation_complete <- FALSE
})

# Load data when file is uploaded
observeEvent(input$import_file, {
  file <- input$import_file
  if (is.null(file)) return(NULL)
  
  ext <- tolower(tools::file_ext(file$name))
  
  # Try to read the file based on extension
  tryCatch({
    if (ext == "csv") {
      # Read CSV file
      data <- read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      # For Excel files, wait for sheet selection
      return(NULL)
    } else {
      # Unsupported file type
      showNotification("Unsupported file type. Please upload a CSV or Excel file.", type = "error")
      return(NULL)
    }
    
    # Store raw data
    import_rv$raw_data <- data
    
    # Update column selection choices
    updateColumnChoices(data)
    
  }, error = function(e) {
    showNotification(paste("Error reading file:", e$message), type = "error")
  })
})

# Load Excel data when sheet is selected
observeEvent(input$import_sheet, {
  file <- input$import_file
  sheet <- input$import_sheet
  
  if (is.null(file) || is.null(sheet)) return(NULL)
  
  ext <- tolower(tools::file_ext(file$name))
  if (!(ext %in% c("xls", "xlsx"))) return(NULL)
  
  # Try to read the Excel file with selected sheet
  tryCatch({
    data <- openxlsx::read.xlsx(file$datapath, sheet = sheet, check.names = FALSE)
    
    # Store raw data
    import_rv$raw_data <- data
    
    # Update column selection choices
    updateColumnChoices(data)
    
  }, error = function(e) {
    showNotification(paste("Error reading Excel sheet:", e$message), type = "error")
  })
})

# Helper function to update column choices
updateColumnChoices <- function(data) {
  if (is.null(data)) return()
  
  # Get column names
  col_names <- names(data)
  
  # Update ID column choices
  updateSelectInput(session, "import_id_column", 
                    choices = c("Select column" = "", col_names),
                    selected = "")
  
  # Update group column choices
  updateSelectizeInput(session, "import_group_columns", 
                       choices = c("Select column(s)" = "", col_names),
                       selected = "")
  
  # Update behavior column choices
  updateSelectizeInput(session, "import_behavior_columns", 
                       choices = c("Select column(s)" = "", col_names),
                       selected = "")
}

# Show data loaded status
output$data_loaded <- reactive({
  !is.null(import_rv$raw_data)
})
outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

# Data preview table
output$import_data_preview <- DT::renderDT({
  req(import_rv$raw_data)
  
  DT::datatable(
    import_rv$raw_data,
    options = list(
      scrollX = TRUE,
      scrollY = "300px",
      paging = TRUE,
      searching = TRUE,
      pageLength = 10,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    filter = 'top',
    class = 'cell-border stripe'
  )
})

# Column summary output
output$import_column_summary <- renderPrint({
  req(import_rv$raw_data)
  
  # Create summary for each column
  col_summary <- lapply(import_rv$raw_data, function(col) {
    if (is.numeric(col)) {
      # Numeric column summary
      c(
        Type = "Numeric",
        Min = min(col, na.rm = TRUE),
        Max = max(col, na.rm = TRUE),
        Mean = mean(col, na.rm = TRUE),
        Median = median(col, na.rm = TRUE),
        SD = sd(col, na.rm = TRUE),
        Missing = sum(is.na(col)),
        "% Missing" = round(100 * sum(is.na(col)) / length(col), 1)
      )
    } else {
      # Character/factor column summary
      unique_values <- unique(col)
      c(
        Type = "Categorical",
        "Unique Values" = length(unique_values),
        "Most Common" = names(sort(table(col), decreasing = TRUE)[1]),
        Missing = sum(is.na(col)),
        "% Missing" = round(100 * sum(is.na(col)) / length(col), 1)
      )
    }
  })
  
  # Convert to data frame for nicer printing
  summary_df <- as.data.frame(do.call(rbind, col_summary))
  summary_df$Column <- rownames(summary_df)
  rownames(summary_df) <- NULL
  summary_df <- summary_df[, c("Column", setdiff(names(summary_df), "Column"))]
  
  # Print summary
  print(summary_df, row.names = FALSE)
})

# Data structure output
output$import_structure <- renderPrint({
  req(import_rv$raw_data)
  
  # Print structure information
  cat("Data Dimensions:", nrow(import_rv$raw_data), "rows,", ncol(import_rv$raw_data), "columns\n\n")
  cat("Column Types:\n")
  
  # Get column types
  col_types <- sapply(import_rv$raw_data, class)
  
  # Count by type
  type_counts <- table(col_types)
  
  for (type in names(type_counts)) {
    cat(" -", type, ":", type_counts[type], "columns\n")
  }
  
  cat("\nFirst few column names:\n")
  cat(paste(" -", head(names(import_rv$raw_data), 10), collapse = "\n"))
  
  if (length(names(import_rv$raw_data)) > 10) {
    cat("\n... and", length(names(import_rv$raw_data)) - 10, "more columns\n")
  }
})

# Configure data when button is clicked
observeEvent(input$import_configure_btn, {
  req(import_rv$raw_data)
  
  # Validate selections
  if (input$import_id_column == "") {
    showNotification("Please select a Subject/ID column.", type = "error")
    return()
  }
  
  if (length(input$import_group_columns) == 0) {
    showNotification("Please select at least one Group column.", type = "error")
    return()
  }
  
  # Check for overlaps in column selections
  all_selected <- c(input$import_id_column, input$import_group_columns, input$import_behavior_columns)
  if (length(all_selected) != length(unique(all_selected))) {
    showNotification("Each column can only be selected for one purpose. Please check your selections.", type = "error")
    return()
  }
  
  # Identify potential region columns (numeric columns not selected for other purposes)
  potential_region_cols <- setdiff(
    names(import_rv$raw_data)[sapply(import_rv$raw_data, is.numeric)],
    all_selected
  )
  
  # Ensure we have at least some region columns
  if (length(potential_region_cols) == 0) {
    showNotification(
      "No numeric columns available for brain regions. Make sure not all numeric columns are selected as ID, group, or behavior columns.", 
      type = "error"
    )
    return()
  }
  
  # Validate the data structure
  validation_results <- validate_data(
    import_rv$raw_data,
    required_columns = c(input$import_id_column, input$import_group_columns),
    id_column = input$import_id_column,
    group_columns = input$import_group_columns
  )
  
  if (!validation_results$valid) {
    # Display validation errors
    error_message <- paste(validation_results$messages, collapse = "\n")
    showNotification(error_message, type = "error", duration = 10)
    return()
  }
  
  # Store column info
  import_rv$column_info <- list(
    id_column = input$import_id_column,
    group_columns = input$import_group_columns,
    behavior_columns = input$import_behavior_columns,
    region_columns = potential_region_cols
  )
  
  # Prepare data for analysis
  processed_data <- import_rv$raw_data
  
  # Transpose data if requested
  if (input$import_transpose) {
    # Save the ID column values
    id_values <- processed_data[[input$import_id_column]]
    
    # Transpose the data matrix (excluding ID and group columns)
    exclude_cols <- c(input$import_id_column, input$import_group_columns, input$import_behavior_columns)
    data_matrix <- t(as.matrix(processed_data[, !names(processed_data) %in% exclude_cols]))
    
    # Create new data frame
    processed_data <- as.data.frame(data_matrix)
    names(processed_data) <- id_values
    
    # Add row names as a new column
    processed_data$Region <- rownames(processed_data)
    
    # Rearrange columns to put Region first
    processed_data <- processed_data[, c("Region", setdiff(names(processed_data), "Region"))]
    
    # Update column info for transposed data
    import_rv$column_info$id_column <- "Region"
    import_rv$column_info$group_columns <- character(0)
    import_rv$column_info$behavior_columns <- character(0)
    import_rv$column_info$region_columns <- setdiff(names(processed_data), "Region")
  } else {
    # Rename ID column to "Subject" for consistency
    names(processed_data)[names(processed_data) == input$import_id_column] <- "Subject"
    import_rv$column_info$id_column <- "Subject"
    
    # Create combined group column if requested
    if (input$import_combine_groups && length(input$import_group_columns) > 1) {
      processed_data$Group <- apply(processed_data[, input$import_group_columns, drop = FALSE], 1, 
                                    function(x) paste(x, collapse = "-"))
      
      # Add Group to column info
      import_rv$column_info$combined_group <- "Group"
    } else if (input$import_combine_groups && length(input$import_group_columns) == 1) {
      processed_data$Group <- processed_data[[input$import_group_columns]]
      import_rv$column_info$combined_group <- "Group"
    }
  }
  
  # Store processed data
  import_rv$processed_data <- processed_data
  
  # Check for missing data
  missing_counts <- sapply(processed_data[, import_rv$column_info$region_columns], function(x) sum(is.na(x)))
  total_missing <- sum(missing_counts)
  import_rv$has_missing_data <- total_missing > 0
  
  # Mark configuration as complete
  import_rv$validation_complete <- TRUE
  
  # Show notification of successful configuration
  showNotification(
    "Data configured successfully! View the Data Quality Summary below.",
    type = "success"
  )
})

# Show configuration complete status
output$config_complete <- reactive({
  import_rv$validation_complete
})
outputOptions(output, "config_complete", suspendWhenHidden = FALSE)

# Show missing data warning
output$has_missing_data <- reactive({
  import_rv$has_missing_data
})
outputOptions(output, "has_missing_data", suspendWhenHidden = FALSE)

# Generate data quality report
output$import_quality_report <- renderUI({
  req(import_rv$processed_data, import_rv$validation_complete)
  
  # Get column info
  col_info <- import_rv$column_info
  
  # Calculate data statistics
  n_subjects <- nrow(import_rv$processed_data)
  n_regions <- length(col_info$region_columns)
  
  # Count groups
  if (!is.null(col_info$combined_group)) {
    groups <- unique(import_rv$processed_data[[col_info$combined_group]])
    n_groups <- length(groups)
    group_counts <- table(import_rv$processed_data[[col_info$combined_group]])
  } else if (length(col_info$group_columns) > 0) {
    # Use first group column if no combined group
    first_group <- col_info$group_columns[1]
    groups <- unique(import_rv$processed_data[[first_group]])
    n_groups <- length(groups)
    group_counts <- table(import_rv$processed_data[[first_group]])
  } else {
    n_groups <- 0
    groups <- NULL
    group_counts <- NULL
  }
  
  # Check for missing data
  missing_data <- import_rv$has_missing_data
  if (missing_data) {
    missing_counts <- colSums(is.na(import_rv$processed_data[, col_info$region_columns, drop = FALSE]))
    total_missing <- sum(missing_counts)
    missing_percent <- round(100 * total_missing / (n_subjects * n_regions), 1)
    regions_with_missing <- sum(missing_counts > 0)
  }
  
  # Generate report
  tagList(
    fluidRow(
      column(
        width = 4,
        div(
          class = "info-box bg-primary",
          div(class = "info-box-icon", icon("users")),
          div(
            class = "info-box-content",
            h4("Subjects"),
            p(n_subjects)
          )
        )
      ),
      column(
        width = 4,
        div(
          class = "info-box bg-primary",
          div(class = "info-box-icon", icon("brain")),
          div(
            class = "info-box-content",
            h4("Brain Regions"),
            p(n_regions)
          )
        )
      ),
      column(
        width = 4,
        div(
          class = "info-box bg-primary",
          div(class = "info-box-icon", icon("layer-group")),
          div(
            class = "info-box-content",
            h4("Groups"),
            p(n_groups)
          )
        )
      )
    ),
    
    if (!is.null(groups) && !is.null(group_counts)) {
      fluidRow(
        column(
          width = 12,
          h4("Group Distribution:"),
          div(
            style = "display: flex; flex-wrap: wrap; gap: 10px;",
            lapply(seq_along(groups), function(i) {
              div(
                style = "padding: 5px 10px; background-color: #f0f0f0; border-radius: 5px;",
                strong(groups[i]), ": ", group_counts[i], " subjects"
              )
            })
          )
        )
      )
    },
    
    if (missing_data) {
      fluidRow(
        column(
          width = 12,
          h4("Missing Data Summary:"),
          p(
            "Total missing values: ", total_missing, 
            " (", missing_percent, "% of all data points)"
          ),
          p(
            "Regions with missing data: ", regions_with_missing,
            " out of ", n_regions, " regions"
          ),
          p("You can handle missing values in the Preprocessing step.")
        )
      )
    }
  )
})

# Template data for download
template_data <- reactive({
  # Create example data frame
  data.frame(
    Subject = 1:10,
    Sex = sample(c("Male", "Female"), 10, replace = TRUE),
    Group = sample(c("Control", "Treatment"), 10, replace = TRUE),
    Age = round(rnorm(10, 35, 5)),
    Region1 = rnorm(10, 3.5, 0.5),
    Region2 = rnorm(10, 2.8, 0.4),
    Region3 = rnorm(10, 4.2, 0.6),
    Region4 = rnorm(10, 3.1, 0.3),
    Region5 = rnorm(10, 3.9, 0.5),
    Behavior1 = rnorm(10, 25, 5),
    Behavior2 = rnorm(10, 75, 10),
    stringsAsFactors = FALSE
  )
})

# Download template handler
output$download_template <- downloadHandler(
  filename = function() {
    "brain_network_template.csv"
  },
  content = function(file) {
    write.csv(template_data(), file, row.names = FALSE)
  }
)

# Proceed to preprocessing when button is clicked
observeEvent(input$import_proceed_btn, {
  req(import_rv$validation_complete)
  
  # Update workflow status
  workflow$data_import_complete <- TRUE
  
  # Switch to preprocessing tab
  updateTabItems(session, "tabs", "preprocessing")
})

# Make data and configuration available to other modules
data_store$raw_data <- reactive({
  import_rv$raw_data
})

data_store$processed_data <- reactive({
  import_rv$processed_data
})

data_store$column_info <- reactive({
  import_rv$column_info
})

data_store$file_info <- reactive({
  list(
    file_name = import_rv$file_name,
    file_type = import_rv$file_type
  )
})
