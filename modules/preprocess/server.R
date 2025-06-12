#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/preprocess/server.R - Preprocessing module server logic
#########################################################

# Initialize reactive values for this module
preprocess_rv <- reactiveValues(
  data = NULL,
  original_data = NULL,
  column_info = NULL,
  missing_data_info = NULL,
  outlier_info = NULL,
  transformation_info = NULL,
  quality_check_info = NULL,
  preprocessing_complete = FALSE,
  preprocessing_steps = list()
)

# Check if import is complete
output$import_complete <- reactive({
  !is.null(data_store$processed_data()) && !is.null(data_store$column_info())
})
outputOptions(output, "import_complete", suspendWhenHidden = FALSE)

# Initialize preprocessing data when import is complete
observe({
  req(data_store$processed_data(), data_store$column_info())
  
  if (is.null(preprocess_rv$original_data)) {
    # Store the original data from import
    preprocess_rv$original_data <- data_store$processed_data()
    preprocess_rv$data <- data_store$processed_data()
    preprocess_rv$column_info <- data_store$column_info()
    
    # Analyze missing data
    preprocess_rv$missing_data_info <- analyze_missing_data(
      preprocess_rv$data,
      preprocess_rv$column_info$region_columns
    )
    
    # Detect outliers
    preprocess_rv$outlier_info <- detect_outliers(
      preprocess_rv$data,
      preprocess_rv$column_info$region_columns,
      method = "iqr",
      threshold = 2.5
    )
    
    # Run quality checks
    preprocess_rv$quality_check_info <- run_quality_checks(
      preprocess_rv$data,
      preprocess_rv$column_info$region_columns
    )
    
    # Update region selection dropdowns
    updateSelectInput(session, "preprocess_transform_region",
                    choices = preprocess_rv$column_info$region_columns)
    
    updateSelectizeInput(session, "preprocess_transform_regions",
                       choices = preprocess_rv$column_info$region_columns)
  }
})

# Check if data has missing values
output$has_missing_data <- reactive({
  req(preprocess_rv$missing_data_info)
  preprocess_rv$missing_data_info$has_missing
})
outputOptions(output, "has_missing_data", suspendWhenHidden = FALSE)

# Missing data visualization
output$preprocess_missing_plot <- renderPlot({
  req(preprocess_rv$data, preprocess_rv$missing_data_info, preprocess_rv$column_info)
  
  # Get relevant data
  data <- preprocess_rv$data
  region_cols <- preprocess_rv$column_info$region_columns
  
  if (preprocess_rv$missing_data_info$has_missing) {
    # Create missing data visualization using naniar
    naniar::vis_miss(data[, region_cols, drop = FALSE], 
                     cluster = TRUE, 
                     sort_miss = TRUE) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(title = "Missing Data Patterns",
                   x = "Brain Regions",
                   y = "Subjects")
  } else {
    # Create a placeholder plot for no missing data
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No missing data in the dataset.") +
      ggplot2::theme_void() +
      ggplot2::xlim(0, 1) +
      ggplot2::ylim(0, 1)
  }
})

# Missing data summary
output$preprocess_missing_summary <- renderPrint({
  req(preprocess_rv$missing_data_info)
  
  info <- preprocess_rv$missing_data_info
  
  if (info$has_missing) {
    cat("Missing Data Summary:\n\n")
    cat("Total missing values:", info$total_missing, "\n")
    cat("Percentage missing:", sprintf("%.2f%%", info$percent_missing), "\n\n")
    
    cat("Regions with highest missing data:\n")
    top_regions <- head(sort(info$missing_by_region, decreasing = TRUE), 5)
    for (i in seq_along(top_regions)) {
      if (top_regions[i] > 0) {
        region_percent <- 100 * top_regions[i] / nrow(preprocess_rv$data)
        cat(sprintf("- %s: %d values (%.1f%%)\n", 
                    names(top_regions)[i], top_regions[i], region_percent))
      }
    }
    
    cat("\nSubjects with highest missing data:\n")
    top_subjects <- head(sort(info$missing_by_subject, decreasing = TRUE), 5)
    for (i in seq_along(top_subjects)) {
      if (top_subjects[i] > 0) {
        subject_percent <- 100 * top_subjects[i] / length(preprocess_rv$column_info$region_columns)
        cat(sprintf("- Subject %d: %d values (%.1f%%)\n", 
                    i, top_subjects[i], subject_percent))
      }
    }
  } else {
    cat("No missing data detected in the dataset.")
  }
})

# Apply missing data handling when method changes
observeEvent(list(
  input$preprocess_missing_method,
  input$preprocess_knn_k,
  input$preprocess_mice_method,
  input$preprocess_exclude_high_missing_regions,
  input$preprocess_region_missing_threshold,
  input$preprocess_exclude_high_missing_subjects,
  input$preprocess_subject_missing_threshold
), {
  req(preprocess_rv$data, preprocess_rv$missing_data_info, preprocess_rv$column_info)
  
  # Only proceed if we have missing data
  if (!preprocess_rv$missing_data_info$has_missing) {
    return()
  }
  
  # Make a copy of the current data
  data <- preprocess_rv$data
  region_cols <- preprocess_rv$column_info$region_columns
  
  # Track modifications
  modifications <- list()
  
  # Handle region exclusion if selected
  if (input$preprocess_exclude_high_missing_regions) {
    threshold <- input$preprocess_region_missing_threshold / 100
    missing_by_region <- preprocess_rv$missing_data_info$missing_by_region
    missing_percent_by_region <- missing_by_region / nrow(data)
    
    # Identify regions to exclude
    regions_to_exclude <- names(missing_percent_by_region)[missing_percent_by_region > threshold]
    
    if (length(regions_to_exclude) > 0) {
      # Update region columns
      region_cols <- setdiff(region_cols, regions_to_exclude)
      
      # Add to modifications
      modifications$excluded_regions <- regions_to_exclude
    }
  }
  
  # Handle subject exclusion if selected
  excluded_subjects <- NULL
  if (input$preprocess_exclude_high_missing_subjects) {
    threshold <- input$preprocess_subject_missing_threshold / 100
    missing_by_subject <- preprocess_rv$missing_data_info$missing_by_subject
    missing_percent_by_subject <- missing_by_subject / length(region_cols)
    
    # Identify subjects to exclude
    subjects_to_exclude <- which(missing_percent_by_subject > threshold)
    
    if (length(subjects_to_exclude) > 0) {
      # Exclude subjects
      if (length(subjects_to_exclude) < nrow(data)) {  # Ensure we don't remove all subjects
        data <- data[-subjects_to_exclude, ]
        
        # Add to modifications
        modifications$excluded_subjects <- subjects_to_exclude
        excluded_subjects <- subjects_to_exclude
      }
    }
  }
  
  # Apply imputation method if not "none"
  if (input$preprocess_missing_method != "none") {
    # Impute missing values
    imputation_result <- impute_missing_values(
      data,
      region_cols,
      method = input$preprocess_missing_method,
      k = if (input$preprocess_missing_method == "knn") input$preprocess_knn_k else NULL,
      mice_method = if (input$preprocess_missing_method == "mice") input$preprocess_mice_method else NULL
    )
    
    # Update data with imputed values
    data <- imputation_result$data
    
    # Add to modifications
    modifications$imputation <- list(
      method = input$preprocess_missing_method,
      parameters = imputation_result$parameters
    )
  }
  
  # Update the data in reactive values
  preprocess_rv$data <- data
  
  # Update column info if regions were excluded
  if (!is.null(modifications$excluded_regions)) {
    preprocess_rv$column_info$region_columns <- region_cols
  }
  
  # Add missing data handling step to preprocessing steps
  preprocess_rv$preprocessing_steps$missing_data <- list(
    applied = TRUE,
    method = input$preprocess_missing_method,
    excluded_regions = modifications$excluded_regions,
    excluded_subjects = modifications$excluded_subjects,
    parameters = modifications$imputation$parameters
  )
  
  # Re-analyze missing data
  preprocess_rv$missing_data_info <- analyze_missing_data(
    preprocess_rv$data,
    preprocess_rv$column_info$region_columns
  )
  
  # Re-run quality checks
  preprocess_rv$quality_check_info <- run_quality_checks(
    preprocess_rv$data,
    preprocess_rv$column_info$region_columns
  )
  
  # Update outlier detection
  preprocess_rv$outlier_info <- detect_outliers(
    preprocess_rv$data,
    preprocess_rv$column_info$region_columns,
    method = input$preprocess_outlier_method,
    threshold = input$preprocess_outlier_threshold
  )
  
  # Update transformation preview if region is in excluded regions
  if (!is.null(modifications$excluded_regions) && 
      input$preprocess_transform_region %in% modifications$excluded_regions) {
    updateSelectInput(session, "preprocess_transform_region",
                     choices = preprocess_rv$column_info$region_columns)
  }
  
  # Update transform regions selection
  updateSelectizeInput(session, "preprocess_transform_regions",
                      choices = preprocess_rv$column_info$region_columns)
})

# Outlier plot
output$preprocess_outlier_plot <- renderPlot({
  req(preprocess_rv$data, preprocess_rv$column_info)
  
  # Get data and settings
  data <- preprocess_rv$data
  region_cols <- preprocess_rv$column_info$region_columns
  plot_type <- input$preprocess_outlier_plot_type
  
  if (length(region_cols) == 0) {
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No region columns available for plotting.") +
             ggplot2::theme_void() +
             ggplot2::xlim(0, 1) +
             ggplot2::ylim(0, 1))
  }
  
  # Reshape data for plotting
  plot_data <- reshape2::melt(data[, region_cols, drop = FALSE], 
                              variable.name = "Region", 
                              value.name = "Value")
  
  # Create plot based on type
  if (plot_type == "boxplot") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Region, y = Value)) +
      ggplot2::geom_boxplot(fill = "lightblue") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(title = "Distribution of Brain Region Values",
                   x = "Brain Region",
                   y = "Value")
    
    # Add outlier highlighting if method is not "none"
    if (input$preprocess_outlier_method != "none" && !is.null(preprocess_rv$outlier_info)) {
      # Extract outliers
      outliers <- preprocess_rv$outlier_info$outliers_by_region
      
      # Create data frame for outlier points
      outlier_df <- data.frame(Region = character(), Value = numeric())
      
      for (region in names(outliers)) {
        if (length(outliers[[region]]) > 0) {
          region_outliers <- data.frame(
            Region = region,
            Value = data[outliers[[region]], region]
          )
          outlier_df <- rbind(outlier_df, region_outliers)
        }
      }
      
      if (nrow(outlier_df) > 0) {
        p <- p + ggplot2::geom_point(data = outlier_df, 
                                     ggplot2::aes(x = Region, y = Value),
                                     color = "red", size = 2)
      }
    }
    
    return(p)
  } else if (plot_type == "histogram") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Value)) +
      ggplot2::geom_histogram(bins = 30, fill = "lightblue", color = "black") +
      ggplot2::facet_wrap(~ Region, scales = "free") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Histograms of Brain Region Values",
                   x = "Value",
                   y = "Count")
    return(p)
  } else if (plot_type == "density") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Value, fill = Region)) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Density Plots of Brain Region Values",
                   x = "Value",
                   y = "Density")
    return(p)
  } else if (plot_type == "qqplot") {
    # Create a multi-panel QQ plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = Value)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line() +
      ggplot2::facet_wrap(~ Region, scales = "free") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "QQ Plots of Brain Region Values",
                   x = "Theoretical Quantiles",
                   y = "Sample Quantiles")
    return(p)
  }
})

# Outlier summary
output$preprocess_outlier_summary <- renderPrint({
  req(preprocess_rv$outlier_info)
  
  info <- preprocess_rv$outlier_info
  
  if (input$preprocess_outlier_method == "none") {
    cat("Outlier detection disabled.")
    return()
  }
  
  cat("Outlier Detection Summary:\n\n")
  cat("Method:", info$method, "\n")
  cat("Threshold:", info$threshold, "\n\n")
  
  outlier_count <- sum(sapply(info$outliers_by_region, length))
  
  if (outlier_count > 0) {
    cat("Total outliers detected:", outlier_count, "\n\n")
    
    cat("Outliers by region:\n")
    for (region in names(info$outliers_by_region)) {
      count <- length(info$outliers_by_region[[region]])
      if (count > 0) {
        cat(sprintf("- %s: %d outliers\n", region, count))
      }
    }
  } else {
    cat("No outliers detected with current settings.")
  }
})

# Apply outlier handling when settings change
observeEvent(list(
  input$preprocess_outlier_method,
  input$preprocess_outlier_threshold,
  input$preprocess_outlier_handling
), {
  req(preprocess_rv$data, preprocess_rv$column_info)
  
  # Skip if method is none
  if (input$preprocess_outlier_method == "none") {
    preprocess_rv$outlier_info <- list(
      method = "none",
      threshold = NA,
      outliers_by_region = list(),
      total_outliers = 0
    )
    
    # Remove outlier handling step if it exists
    if (!is.null(preprocess_rv$preprocessing_steps$outlier_handling)) {
      preprocess_rv$preprocessing_steps$outlier_handling <- NULL
    }
    
    return()
  }
  
  # Detect outliers with current settings
  outlier_info <- detect_outliers(
    preprocess_rv$data,
    preprocess_rv$column_info$region_columns,
    method = input$preprocess_outlier_method,
    threshold = input$preprocess_outlier_threshold
  )
  
  # Store outlier info
  preprocess_rv$outlier_info <- outlier_info
  
  # Apply handling method if not just flagging
  if (input$preprocess_outlier_handling != "flag" && outlier_info$total_outliers > 0) {
    # Handle outliers
    result <- handle_outliers(
      preprocess_rv$data,
      preprocess_rv$column_info$region_columns,
      outlier_info,
      method = input$preprocess_outlier_handling
    )
    
    # Update data
    preprocess_rv$data <- result$data
    
    # Add outlier handling step to preprocessing steps
    preprocess_rv$preprocessing_steps$outlier_handling <- list(
      applied = TRUE,
      detection_method = input$preprocess_outlier_method,
      handling_method = input$preprocess_outlier_handling,
      threshold = input$preprocess_outlier_threshold,
      outliers_handled = outlier_info$total_outliers
    )
    
    # Re-analyze missing data if outliers were replaced with NA
    if (input$preprocess_outlier_handling == "na") {
      preprocess_rv$missing_data_info <- analyze_missing_data(
        preprocess_rv$data,
        preprocess_rv$column_info$region_columns
      )
    }
    
    # Re-run quality checks
    preprocess_rv$quality_check_info <- run_quality_checks(
      preprocess_rv$data,
      preprocess_rv$column_info$region_columns
    )
  } else {
    # Just flagging, add to preprocessing steps
    preprocess_rv$preprocessing_steps$outlier_handling <- list(
      applied = FALSE,
      detection_method = input$preprocess_outlier_method,
      handling_method = "flag",
      threshold = input$preprocess_outlier_threshold,
      outliers_detected = outlier_info$total_outliers
    )
  }
})

# Transformation preview plot
output$preprocess_transform_plot <- renderPlot({
  req(preprocess_rv$data, preprocess_rv$column_info)
  
  # Get selected region and plot type
  region <- input$preprocess_transform_region
  plot_type <- input$preprocess_transform_plot_type
  
  if (is.null(region) || !(region %in% names(preprocess_rv$data))) {
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Please select a valid region.") +
             ggplot2::theme_void() +
             ggplot2::xlim(0, 1) +
             ggplot2::ylim(0, 1))
  }
  
  # Get original data
  original_values <- preprocess_rv$data[[region]]
  
  # Apply transformation for preview
  transformed_values <- transform_data(
    original_values,
    method = input$preprocess_transform_method,
    log_plus_one = input$preprocess_log_plus_one,
    boxcox_lambda = if (input$preprocess_boxcox_auto) NULL else input$preprocess_boxcox_lambda
  )
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Value = c(original_values, transformed_values),
    Type = rep(c("Original", "Transformed"), each = length(original_values))
  )
  
  # Create plot based on type
  if (plot_type == "histogram") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Value, fill = Type)) +
      ggplot2::geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
      ggplot2::facet_wrap(~ Type, scales = "free_x") +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(values = c("Original" = "lightblue", "Transformed" = "lightgreen")) +
      ggplot2::labs(title = paste("Histogram of", region, "Values"),
                   x = "Value",
                   y = "Count")
    return(p)
  } else if (plot_type == "density") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Value, fill = Type)) +
      ggplot2::geom_density(alpha = 0.6) +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(values = c("Original" = "lightblue", "Transformed" = "lightgreen")) +
      ggplot2::labs(title = paste("Density Plot of", region, "Values"),
                   x = "Value",
                   y = "Density")
    return(p)
  } else if (plot_type == "qqplot") {
    # Create separate QQ plots
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = Value)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line() +
      ggplot2::facet_wrap(~ Type, scales = "free") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste("QQ Plot of", region, "Values"),
                   x = "Theoretical Quantiles",
                   y = "Sample Quantiles")
    return(p)
  }
})

# Apply transformation when button is clicked
observeEvent(input$preprocess_apply_transform, {
  req(preprocess_rv$data, preprocess_rv$column_info)
  
  # Skip if method is none
  if (input$preprocess_transform_method == "none") {
    showNotification("No transformation selected.", type = "warning")
    return()
  }
  
  # Determine which regions to transform
  if (input$preprocess_transform_scope == "all") {
    regions_to_transform <- preprocess_rv$column_info$region_columns
  } else {
    regions_to_transform <- input$preprocess_transform_regions
    if (length(regions_to_transform) == 0) {
      showNotification("Please select at least one region to transform.", type = "warning")
      return()
    }
  }
  
  # Apply transformation
  transformed_data <- preprocess_rv$data
  
  # If transforming by group, we need to handle each group separately
  if (input$preprocess_transform_by_group && !is.null(preprocess_rv$column_info$combined_group)) {
    group_col <- preprocess_rv$column_info$combined_group
    groups <- unique(transformed_data[[group_col]])
    
    for (group in groups) {
      group_indices <- which(transformed_data[[group_col]] == group)
      
      for (region in regions_to_transform) {
        transformed_data[group_indices, region] <- transform_data(
          transformed_data[group_indices, region],
          method = input$preprocess_transform_method,
          log_plus_one = input$preprocess_log_plus_one,
          boxcox_lambda = if (input$preprocess_boxcox_auto) NULL else input$preprocess_boxcox_lambda
        )
      }
    }
  } else {
    # Transform all data together
    for (region in regions_to_transform) {
      transformed_data[[region]] <- transform_data(
        transformed_data[[region]],
        method = input$preprocess_transform_method,
        log_plus_one = input$preprocess_log_plus_one,
        boxcox_lambda = if (input$preprocess_boxcox_auto) NULL else input$preprocess_boxcox_lambda
      )
    }
  }
  
  # Update data
  preprocess_rv$data <- transformed_data
  
  # Add transformation step to preprocessing steps
  preprocess_rv$preprocessing_steps$transformation <- list(
    applied = TRUE,
    method = input$preprocess_transform_method,
    scope = if (input$preprocess_transform_scope == "all") "all_regions" else regions_to_transform,
    by_group = input$preprocess_transform_by_group,
    parameters = list(
      log_plus_one = if (input$preprocess_transform_method == "log") input$preprocess_log_plus_one else NULL,
      boxcox_lambda = if (input$preprocess_transform_method == "boxcox" && !input$preprocess_boxcox_auto) 
        input$preprocess_boxcox_lambda else NULL,
      boxcox_auto = if (input$preprocess_transform_method == "boxcox") input$preprocess_boxcox_auto else NULL
    )
  )
  
  # Re-run quality checks
  preprocess_rv$quality_check_info <- run_quality_checks(
    preprocess_rv$data,
    preprocess_rv$column_info$region_columns
  )
  
  # Show notification
  showNotification(
    paste("Transformation applied to", length(regions_to_transform), "regions."),
    type = "success"
  )
})

# Normality tests table
output$preprocess_normality_tests <- DT::renderDT({
  req(preprocess_rv$data, preprocess_rv$column_info)
  
  # Run normality tests on each region
  region_cols <- preprocess_rv$column_info$region_columns
  
  # Create data frame for results
  results <- data.frame(
    Region = region_cols,
    Mean = numeric(length(region_cols)),
    SD = numeric(length(region_cols)),
    Skewness = numeric(length(region_cols)),
    Kurtosis = numeric(length(region_cols)),
    Shapiro_W = numeric(length(region_cols)),
    Shapiro_p = numeric(length(region_cols)),
    Normal = character(length(region_cols)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(region_cols)) {
    region <- region_cols[i]
    values <- preprocess_rv$data[[region]]
    
    # Basic statistics
    results$Mean[i] <- mean(values, na.rm = TRUE)
    results$SD[i] <- sd(values, na.rm = TRUE)
    
    # Skewness and kurtosis
    if (requireNamespace("moments", quietly = TRUE)) {
      results$Skewness[i] <- moments::skewness(values, na.rm = TRUE)
      results$Kurtosis[i] <- moments::kurtosis(values, na.rm = TRUE)
    } else {
      results$Skewness[i] <- NA
      results$Kurtosis[i] <- NA
    }
    
    # Shapiro-Wilk test (for n < 5000)
    if (sum(!is.na(values)) < 5000) {
      shapiro_test <- shapiro.test(values[!is.na(values)])
      results$Shapiro_W[i] <- shapiro_test$statistic
      results$Shapiro_p[i] <- shapiro_test$p.value
      results$Normal[i] <- ifelse(shapiro_test$p.value > 0.05, "Yes", "No")
    } else {
      results$Shapiro_W[i] <- NA
      results$Shapiro_p[i] <- NA
      results$Normal[i] <- "Sample too large"
    }
  }
  
  # Return datatable
  DT::datatable(
    results,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound(c("Mean", "SD", "Skewness", "Kurtosis", "Shapiro_W"), 3) %>%
    DT::formatSignif(c("Shapiro_p"), 3) %>%
    DT::formatStyle(
      "Normal",
      backgroundColor = DT::styleEqual(c("Yes", "No"), c("lightgreen", "lightpink"))
    )
})

# Variance check table
output$preprocess_variance_check <- DT::renderDT({
  req(preprocess_rv$quality_check_info)
  
  variance_check <- preprocess_rv$quality_check_info$variance_check
  
  # Sort by variance
  variance_check <- variance_check[order(variance_check$Variance), ]
  
  # Return datatable
  DT::datatable(
    variance_check,
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      dom = 'frtip'
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound(c("Variance", "CV"), 3) %>%
    DT::formatStyle(
      "Flag",
      backgroundColor = DT::styleEqual(c("Low Variance"), c("lightpink"))
    )
})

# Correlation check table
output$preprocess_correlation_check <- DT::renderDT({
  req(preprocess_rv$quality_check_info)
  
  corr_check <- preprocess_rv$quality_check_info$correlation_check
  
  # Return datatable
  DT::datatable(
    corr_check,
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      dom = 'frtip'
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound("Correlation", 3) %>%
    DT::formatStyle(
      "Correlation",
      backgroundColor = DT::styleInterval(c(0.8, 0.9), c("white", "lightyellow", "lightpink"))
    )
})

# Region summary table
output$preprocess_region_summary <- DT::renderDT({
  req(preprocess_rv$quality_check_info)
  
  region_summary <- preprocess_rv$quality_check_info$region_summary
  
  # Return datatable
  DT::datatable(
    region_summary,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound(c("Mean", "Median", "SD", "Min", "Max", "CV"), 3) %>%
    DT::formatStyle(
      "Missing_Percent",
      backgroundColor = DT::styleInterval(c(5, 20), c("white", "lightyellow", "lightpink"))
    )
})

# Preprocessing summary
output$preprocess_summary <- renderPrint({
  req(preprocess_rv$data, preprocess_rv$original_data, preprocess_rv$preprocessing_steps)
  
  cat("Preprocessing Summary:\n\n")
  
  # Original data dimensions
  cat("Original Data: ", nrow(preprocess_rv$original_data), "subjects,", 
      length(preprocess_rv$column_info$region_columns), "brain regions\n")
  
  # Current data dimensions
  cat("Current Data: ", nrow(preprocess_rv$data), "subjects,", 
      length(preprocess_rv$column_info$region_columns), "brain regions\n\n")
  
  # Missing data handling
  if (!is.null(preprocess_rv$preprocessing_steps$missing_data)) {
    md_step <- preprocess_rv$preprocessing_steps$missing_data
    cat("Missing Data Handling:\n")
    cat("- Method:", md_step$method, "\n")
    
    if (!is.null(md_step$excluded_regions) && length(md_step$excluded_regions) > 0) {
      cat("- Excluded Regions:", length(md_step$excluded_regions), "\n")
    }
    
    if (!is.null(md_step$excluded_subjects) && length(md_step$excluded_subjects) > 0) {
      cat("- Excluded Subjects:", length(md_step$excluded_subjects), "\n")
    }
    
    cat("\n")
  } else {
    cat("Missing Data Handling: None applied\n\n")
  }
  
  # Outlier handling
  if (!is.null(preprocess_rv$preprocessing_steps$outlier_handling)) {
    out_step <- preprocess_rv$preprocessing_steps$outlier_handling
    cat("Outlier Handling:\n")
    cat("- Detection Method:", out_step$detection_method, "\n")
    cat("- Handling Method:", out_step$handling_method, "\n")
    cat("- Threshold:", out_step$threshold, "\n")
    
    if (out_step$applied) {
      cat("- Outliers Handled:", out_step$outliers_handled, "\n")
    } else {
      cat("- Outliers Detected:", out_step$outliers_detected, "(flagging only)\n")
    }
    
    cat("\n")
  } else {
    cat("Outlier Handling: None applied\n\n")
  }
  
  # Transformations
  if (!is.null(preprocess_rv$preprocessing_steps$transformation)) {
    trans_step <- preprocess_rv$preprocessing_steps$transformation
    cat("Transformations:\n")
    cat("- Method:", trans_step$method, "\n")
    
    if (is.character(trans_step$scope) && trans_step$scope == "all_regions") {
      cat("- Applied To: All brain regions\n")
    } else {
      cat("- Applied To:", length(trans_step$scope), "regions\n")
    }
    
    cat("- By Group:", if (trans_step$by_group) "Yes" else "No", "\n")
    cat("\n")
  } else {
    cat("Transformations: None applied\n\n")
  }
  
  # Quality checks
  qc <- preprocess_rv$quality_check_info
  
  cat("Current Data Quality:\n")
  if (!is.null(qc$variance_check)) {
    low_var_count <- sum(qc$variance_check$Flag == "Low Variance")
    cat("- Low Variance Regions:", low_var_count, "\n")
  }
  
  if (!is.null(qc$correlation_check)) {
    high_corr_count <- nrow(qc$correlation_check[qc$correlation_check$Correlation > 0.9, ])
    cat("- Highly Correlated Pairs:", high_corr_count, "\n")
  }
  
  if (!is.null(preprocess_rv$missing_data_info)) {
    if (preprocess_rv$missing_data_info$has_missing) {
      cat("- Missing Values:", preprocess_rv$missing_data_info$total_missing, 
          sprintf("(%.1f%%)", preprocess_rv$missing_data_info$percent_missing), "\n")
    } else {
      cat("- Missing Values: None\n")
    }
  }
})

# Reset preprocessing
observeEvent(input$preprocess_reset, {
  # Confirm reset
  showModal(modalDialog(
    title = "Confirm Reset",
    "Are you sure you want to reset all preprocessing steps? This will revert to the original imported data.",
    footer = tagList(
      modalButton("Cancel"),
      actionButton("preprocess_confirm_reset", "Reset", class = "btn-danger")
    )
  ))
})

# Handle confirmed reset
observeEvent(input$preprocess_confirm_reset, {
  # Reset data to original
  preprocess_rv$data <- preprocess_rv$original_data
  
  # Reset preprocessing steps
  preprocess_rv$preprocessing_steps <- list()
  
  # Re-analyze missing data
  preprocess_rv$missing_data_info <- analyze_missing_data(
    preprocess_rv$data,
    preprocess_rv$column_info$region_columns
  )
  
  # Reset outlier info
  preprocess_rv$outlier_info <- detect_outliers(
    preprocess_rv$data,
    preprocess_rv$column_info$region_columns,
    method = "iqr",
    threshold = 2.5
  )
  
  # Re-run quality checks
  preprocess_rv$quality_check_info <- run_quality_checks(
    preprocess_rv$data,
    preprocess_rv$column_info$region_columns
  )
  
  # Close modal
  removeModal()
  
  # Show notification
  showNotification("All preprocessing steps have been reset.", type = "info")
})

# Proceed to correlation methods
observeEvent(input$preprocess_proceed_btn, {
  req(preprocess_rv$data)
  
  # Mark preprocessing as complete
  preprocess_rv$preprocessing_complete <- TRUE
  
  # Update workflow status
  workflow$preprocessing_complete <- TRUE
  
  # Make preprocessed data available to other modules
  data_store$preprocessed_data <- preprocess_rv$data
  data_store$preprocessing_steps <- preprocess_rv$preprocessing_steps
  
  # Switch to correlation tab
  updateTabItems(session, "tabs", "correlation")
  
  # Show notification
  showNotification("Preprocessing complete. Proceeding to Correlation Methods.", type = "success")
})
