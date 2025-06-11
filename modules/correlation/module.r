# File: modules/correlation/ui.R
#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/correlation/ui.R - Correlation methods UI
#########################################################

fluidPage(
  fluidRow(
    column(
      width = 12,
      box(
        title = "Correlation Methods",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        p("Select and configure correlation methods to generate functional connectivity matrices."),
        p("Different methods capture different aspects of brain connectivity. You can select multiple methods for comparison.")
      )
    )
  ),
  
  # Initial loading message
  uiOutput("correlation_initial_message"),
  
  # Main correlation methods UI - only shown when data is ready
  conditionalPanel(
    condition = "output.preprocessing_complete",
    
    fluidRow(
      # Left column - Method selection
      column(
        width = 4,
        box(
          title = "Select Correlation Methods",
          width = NULL,
          status = "primary",
          
          selectInput(
            "primary_method",
            "Primary Correlation Method:",
            choices = unlist(CORRELATION_METHODS, use.names = TRUE),
            selected = "pearson"
          ),
          
          checkboxGroupInput(
            "additional_methods",
            "Additional Methods (Optional):",
            choices = unlist(CORRELATION_METHODS, use.names = TRUE),
            selected = NULL
          ),
          
          checkboxInput(
            "use_ensemble",
            "Generate ensemble matrix from selected methods",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input.use_ensemble == true",
            selectInput(
              "ensemble_method", 
              "Ensemble Method:",
              choices = c(
                "Average" = "average",
                "Weighted Average" = "weighted",
                "Rank Aggregation" = "rank",
                "Consensus Clustering" = "consensus"
              ),
              selected = "average"
            )
          )
        )
      ),
      
      # Middle column - Method configuration
      column(
        width = 4,
        uiOutput("correlation_method_settings")
      ),
      
      # Right column - Threshold settings
      column(
        width = 4,
        box(
          title = "Thresholding",
          width = NULL,
          status = "primary",
          
          selectInput(
            "threshold_type",
            "Threshold Type:",
            choices = c(
              "None" = "none",
              "Absolute Value" = "absolute",
              "Proportional" = "proportional",
              "Statistical Significance" = "significance"
            ),
            selected = "absolute"
          ),
          
          conditionalPanel(
            condition = "input.threshold_type == 'absolute'",
            sliderInput(
              "absolute_threshold",
              "Absolute Threshold Value:",
              min = 0,
              max = 1,
              value = DEFAULT_THRESHOLD,
              step = 0.05
            )
          ),
          
          conditionalPanel(
            condition = "input.threshold_type == 'proportional'",
            sliderInput(
              "proportional_threshold",
              "Connection Density (%):",
              min = 1,
              max = 100,
              value = 20,
              step = 1
            )
          ),
          
          conditionalPanel(
            condition = "input.threshold_type == 'significance'",
            sliderInput(
              "significance_threshold",
              "P-value Threshold:",
              min = 0.001,
              max = 0.1,
              value = 0.05,
              step = 0.001
            ),
            checkboxInput(
              "fdr_correction",
              "Apply FDR Correction",
              value = TRUE
            )
          ),
          
          checkboxInput(
            "use_absolute_values",
            "Use Absolute Correlation Values",
            value = FALSE
          ),
          
          checkboxInput(
            "include_negative",
            "Include Negative Correlations",
            value = TRUE
          )
        )
      )
    ),
    
    # Correlation matrix preview
    fluidRow(
      column(
        width = 12,
        box(
          title = "Correlation Matrix Preview",
          width = NULL,
          status = "primary",
          
          fluidRow(
            column(
              width = 9,
              plotlyOutput("correlation_heatmap", height = "500px")
            ),
            column(
              width = 3,
              selectInput(
                "preview_method",
                "Preview Method:",
                choices = c("Primary Method"),
                selected = "Primary Method"
              ),
              
              sliderInput(
                "heatmap_text_size",
                "Text Size:",
                min = 6,
                max = 14,
                value = 8,
                step = 1
              ),
              
              checkboxInput(
                "show_dendrogram",
                "Show Dendrogram",
                value = TRUE
              ),
              
              downloadButton(
                "download_correlation_matrix",
                "Download Matrix"
              ),
              
              hr(),
              
              verbatimTextOutput("correlation_summary")
            )
          )
        )
      )
    ),
    
    # Action buttons
    fluidRow(
      column(
        width = 12,
        div(
          style = "text-align: center; margin-top: 20px; margin-bottom: 20px;",
          actionButton(
            "run_correlation",
            "Generate Correlation Matrices",
            icon = icon("play"),
            class = "btn-success btn-lg"
          ),
          actionButton(
            "proceed_to_network",
            "Proceed to Network Analysis",
            icon = icon("arrow-right"),
            class = "btn-primary btn-lg",
            style = "margin-left: 20px;"
          )
        )
      )
    )
  )
)

# File: modules/correlation/server.R
#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/correlation/server.R - Correlation methods server logic
#########################################################

# Initial message based on data state
output$correlation_initial_message <- renderUI({
  if (!workflow$preprocessing_complete) {
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      "Please complete the Preprocessing step first before configuring correlation methods."
    )
  } else {
    return(NULL)
  }
})

# Flag to indicate preprocessing is complete
output$preprocessing_complete <- reactive({
  return(workflow$preprocessing_complete)
})
outputOptions(output, "preprocessing_complete", suspendWhenHidden = FALSE)

# Dynamic UI for correlation method settings
output$correlation_method_settings <- renderUI({
  # Get primary method
  primary_method <- input$primary_method
  
  # Create settings box
  box(
    title = "Method Configuration",
    width = NULL,
    status = "primary",
    
    # Pearson, Spearman, Kendall settings
    conditionalPanel(
      condition = "input.primary_method == 'pearson' || input.primary_method == 'spearman' || input.primary_method == 'kendall'",
      checkboxInput(
        "cor_use_complete",
        "Use complete observations only",
        value = TRUE
      ),
      helpText("For incomplete data, you can use all available data or only complete observations.")
    ),
    
    # Partial correlation settings
    conditionalPanel(
      condition = "input.primary_method == 'partial'",
      selectInput(
        "partial_method",
        "Regularization Method:",
        choices = c(
          "Simple (pseudo-inverse)" = "simple",
          "Shrinkage" = "shrinkage",
          "Graphical LASSO" = "glasso"
        ),
        selected = "shrinkage"
      ),
      conditionalPanel(
        condition = "input.partial_method == 'shrinkage'",
        sliderInput(
          "shrinkage_lambda",
          "Shrinkage Intensity (λ):",
          min = 0,
          max = 1,
          value = 0.2,
          step = 0.05
        ),
        helpText("λ=0 means no shrinkage, λ=1 means complete shrinkage to diagonal.")
      ),
      conditionalPanel(
        condition = "input.partial_method == 'glasso'",
        sliderInput(
          "glasso_lambda",
          "Regularization Parameter (λ):",
          min = 0.01,
          max = 1,
          value = 0.1,
          step = 0.01
        ),
        helpText("Higher values produce sparser networks.")
      )
    ),
    
    # GLASSO specific settings
    conditionalPanel(
      condition = "input.primary_method == 'glasso'",
      sliderInput(
        "glasso_lambda_direct",
        "Regularization Parameter (λ):",
        min = 0.01,
        max = 1,
        value = 0.1,
        step = 0.01
      ),
      numericInput(
        "glasso_max_iter",
        "Maximum Iterations:",
        value = 1000,
        min = 100,
        max = 10000,
        step = 100
      ),
      helpText("GLASSO estimates a sparse inverse covariance matrix using L1 regularization.")
    ),
    
    # Distance-based settings
    conditionalPanel(
      condition = "input.primary_method == 'euclidean' || input.primary_method == 'distance' || input.primary_method == 'mahalanobis'",
      checkboxInput(
        "distance_normalize",
        "Normalize distances (convert to similarity)",
        value = TRUE
      ),
      helpText("Distance measures are converted to similarity for network analysis.")
    ),
    
    # Ensemble settings 
    conditionalPanel(
      condition = "input.use_ensemble == true",
      conditionalPanel(
        condition = "input.ensemble_method == 'weighted'",
        uiOutput("ensemble_weights_ui")
      ),
      helpText("Ensemble methods combine multiple correlation matrices into a consensus network.")
    )
  )
})

# Dynamic UI for ensemble weights
output$ensemble_weights_ui <- renderUI({
  req(input$primary_method)
  req(input$additional_methods)
  
  # Get all selected methods
  all_methods <- c(input$primary_method, input$additional_methods)
  
  # Create sliders for weights
  weights_ui <- lapply(all_methods, function(method) {
    method_name <- names(unlist(CORRELATION_METHODS))[unlist(CORRELATION_METHODS) == method]
    sliderInput(
      inputId = paste0("weight_", method),
      label = paste0("Weight for ", method_name),
      min = 0,
      max = 1,
      value = 1/length(all_methods),
      step = 0.05
    )
  })
  
  # Return a div with all weight sliders
  div(
    weights_ui,
    helpText("Weights should ideally sum to 1.0")
  )
})

# Update preview method choices when correlation methods change
observe({
  req(input$primary_method)
  
  # Get all selected methods
  all_methods <- c(input$primary_method, input$additional_methods)
  method_names <- names(unlist(CORRELATION_METHODS))[match(all_methods, unlist(CORRELATION_METHODS))]
  
  # Create choices list with primary method first
  primary_name <- names(unlist(CORRELATION_METHODS))[unlist(CORRELATION_METHODS) == input$primary_method]
  choices <- c(paste("Primary Method:", primary_name))
  
  # Add additional methods
  if (length(input$additional_methods) > 0) {
    additional_names <- names(unlist(CORRELATION_METHODS))[match(input$additional_methods, unlist(CORRELATION_METHODS))]
    choices <- c(choices, paste("Additional:", additional_names))
  }
  
  # Add ensemble if selected
  if (input$use_ensemble) {
    choices <- c(choices, paste("Ensemble:", input$ensemble_method))
  }
  
  # Update selectInput
  updateSelectInput(session, "preview_method", choices = choices)
})

# Function to generate correlation matrix
generate_correlation_matrix <- function(data, method, settings) {
  # Source necessary method-specific scripts
  switch(method,
         "pearson" = source("modules/correlation/pearson.R", local = TRUE),
         "spearman" = source("modules/correlation/spearman.R", local = TRUE),
         "kendall" = source("modules/correlation/kendall.R", local = TRUE),
         "partial" = source("modules/correlation/precision.R", local = TRUE),
         "glasso" = source("modules/correlation/precision.R", local = TRUE),
         "shrinkage" = source("modules/correlation/precision.R", local = TRUE),
         "euclidean" = source("modules/correlation/distance.R", local = TRUE),
         "distance" = source("modules/correlation/distance.R", local = TRUE),
         "mahalanobis" = source("modules/correlation/distance.R", local = TRUE),
         "consensus" = source("modules/correlation/ensemble.R", local = TRUE),
         "rank" = source("modules/correlation/ensemble.R", local = TRUE)
  )
  
  # Execute the appropriate correlation function
  switch(method,
         "pearson" = calculate_pearson_correlation(data, settings),
         "spearman" = calculate_spearman_correlation(data, settings),
         "kendall" = calculate_kendall_correlation(data, settings),
         "partial" = calculate_partial_correlation(data, settings),
         "glasso" = calculate_glasso_correlation(data, settings),
         "shrinkage" = calculate_shrinkage_correlation(data, settings),
         "euclidean" = calculate_euclidean_distance(data, settings),
         "distance" = calculate_distance_correlation(data, settings),
         "mahalanobis" = calculate_mahalanobis_distance(data, settings),
         "consensus" = calculate_consensus_correlation(data, settings),
         "rank" = calculate_rank_aggregation(data, settings)
  )
}

# Function to apply thresholding
apply_thresholding <- function(matrix, threshold_type, threshold_value, use_absolute, include_negative) {
  # Create a copy of the matrix
  thresholded <- matrix
  
  # Apply thresholding based on type
  if (threshold_type == "absolute") {
    if (use_absolute) {
      thresholded[abs(thresholded) < threshold_value] <- 0
    } else {
      if (include_negative) {
        thresholded[abs(thresholded) < threshold_value] <- 0
      } else {
        thresholded[thresholded < threshold_value] <- 0
        thresholded[thresholded < 0] <- 0
      }
    }
  } else if (threshold_type == "proportional") {
    # Calculate the quantile threshold
    density <- threshold_value / 100
    if (use_absolute) {
      abs_vals <- abs(thresholded[upper.tri(thresholded)])
      quantile_threshold <- quantile(abs_vals, 1 - density)
      thresholded[abs(thresholded) < quantile_threshold] <- 0
    } else {
      if (include_negative) {
        vals <- thresholded[upper.tri(thresholded)]
        pos_threshold <- quantile(vals[vals > 0], 1 - density/2)
        neg_threshold <- quantile(vals[vals < 0], density/2)
        thresholded[thresholded > 0 & thresholded < pos_threshold] <- 0
        thresholded[thresholded < 0 & thresholded > neg_threshold] <- 0
      } else {
        vals <- thresholded[upper.tri(thresholded)]
        vals <- vals[vals > 0]
        pos_threshold <- quantile(vals, 1 - density)
        thresholded[thresholded < pos_threshold] <- 0
      }
    }
  } else if (threshold_type == "significance") {
    # Apply statistical significance thresholding
    # This would require p-values, which would need to be calculated separately
    # For now, we'll just show a notification
    showNotification("Significance thresholding requires p-value calculation. This is a placeholder implementation.", type = "warning")
    
    # Simulate p-values based on correlation values and sample size
    n <- nrow(data_store$processed_data)
    p_values <- matrix(NA, nrow = nrow(matrix), ncol = ncol(matrix))
    for (i in 1:nrow(matrix)) {
      for (j in 1:ncol(matrix)) {
        if (i != j) {
          # Calculate p-value based on correlation and sample size
          t_stat <- matrix[i, j] * sqrt((n - 2) / (1 - matrix[i, j]^2))
          p_values[i, j] <- 2 * pt(-abs(t_stat), df = n - 2)
        } else {
          p_values[i, j] <- 0
        }
      }
    }
    
    # Apply FDR correction if selected
    if (input$fdr_correction) {
      p_values_upper <- p_values[upper.tri(p_values)]
      p_adjusted <- p.adjust(p_values_upper, method = "fdr")
      p_values[upper.tri(p_values)] <- p_adjusted
      p_values[lower.tri(p_values)] <- t(p_values)[lower.tri(p_values)]
    }
    
    # Apply threshold
    thresholded[p_values > threshold_value] <- 0
  }
  
  # If not including negative correlations, set them to zero
  if (!include_negative && threshold_type != "none") {
    thresholded[thresholded < 0] <- 0
  }
  
  return(thresholded)
}

# Generate correlation matrices when button is clicked
observeEvent(input$run_correlation, {
  req(data_store$processed_data)
  req(workflow$preprocessing_complete)
  
  # Show loading message
  withProgress(message = 'Generating correlation matrices...', value = 0, {
    # Get data
    data <- data_store$processed_data
    
    # Extract numeric brain region data
    region_columns <- isolate(data_store$region_columns)
    brain_data <- data[, region_columns, drop = FALSE]
    
    # Increment progress
    incProgress(0.1)
    
    # Generate settings object
    settings <- list(
      # Basic correlation settings
      use_complete = isolate(input$cor_use_complete),
      
      # Partial correlation settings
      partial_method = isolate(input$partial_method),
      shrinkage_lambda = isolate(input$shrinkage_lambda),
      glasso_lambda = isolate(input$glasso_lambda),
      glasso_lambda_direct = isolate(input$glasso_lambda_direct),
      glasso_max_iter = isolate(input$glasso_max_iter),
      
      # Distance settings
      distance_normalize = isolate(input$distance_normalize),
      
      # Threshold settings
      threshold_type = isolate(input$threshold_type),
      absolute_threshold = isolate(input$absolute_threshold),
      proportional_threshold = isolate(input$proportional_threshold),
      significance_threshold = isolate(input$significance_threshold),
      fdr_correction = isolate(input$fdr_correction),
      use_absolute_values = isolate(input$use_absolute_values),
      include_negative = isolate(input$include_negative)
    )
    
    # Clear existing matrices
    data_store$correlation_matrices <- list()
    
    # Calculate primary correlation matrix
    primary_method <- isolate(input$primary_method)
    
    # Generate primary matrix
    tryCatch({
      primary_matrix <- generate_correlation_matrix(brain_data, primary_method, settings)
      data_store$correlation_matrices$primary <- list(
        matrix = primary_matrix,
        method = primary_method,
        settings = settings,
        thresholded = apply_thresholding(
          primary_matrix,
          isolate(input$threshold_type),
          switch(isolate(input$threshold_type),
                 "absolute" = isolate(input$absolute_threshold),
                 "proportional" = isolate(input$proportional_threshold),
                 "significance" = isolate(input$significance_threshold),
                 0
          ),
          isolate(input$use_absolute_values),
          isolate(input$include_negative)
        )
      )
      
      # Increment progress
      incProgress(0.3)
      
      # Calculate additional correlation matrices
      additional_methods <- isolate(input$additional_methods)
      if (length(additional_methods) > 0) {
        data_store$correlation_matrices$additional <- list()
        
        for (i in seq_along(additional_methods)) {
          method <- additional_methods[i]
          
          # Generate matrix
          additional_matrix <- generate_correlation_matrix(brain_data, method, settings)
          data_store$correlation_matrices$additional[[method]] <- list(
            matrix = additional_matrix,
            method = method,
            settings = settings,
            thresholded = apply_thresholding(
              additional_matrix,
              isolate(input$threshold_type),
              switch(isolate(input$threshold_type),
                     "absolute" = isolate(input$absolute_threshold),
                     "proportional" = isolate(input$proportional_threshold),
                     "significance" = isolate(input$significance_threshold),
                     0
              ),
              isolate(input$use_absolute_values),
              isolate(input$include_negative)
            )
          )
          
          # Increment progress
          incProgress(0.4 / length(additional_methods))
        }
      }
      
      # Generate ensemble matrix if selected
      if (isolate(input$use_ensemble)) {
        # Source ensemble module
        source("modules/correlation/ensemble.R", local = TRUE)
        
        # Get all matrices
        all_matrices <- list(data_store$correlation_matrices$primary$matrix)
        all_methods <- c(primary_method)
        
        if (length(additional_methods) > 0) {
          for (method in additional_methods) {
            all_matrices[[length(all_matrices) + 1]] <- data_store$correlation_matrices$additional[[method]]$matrix
            all_methods <- c(all_methods, method)
          }
        }
        
        # Get weights if using weighted average
        weights <- NULL
        if (isolate(input$ensemble_method) == "weighted") {
          weights <- numeric(length(all_methods))
          for (i in seq_along(all_methods)) {
            weights[i] <- isolate(input[[paste0("weight_", all_methods[i])]])
          }
          
          # Normalize weights to sum to 1
          weights <- weights / sum(weights)
        }
        
        # Generate ensemble matrix
        ensemble_settings <- list(
          method = isolate(input$ensemble_method),
          weights = weights,
          original_matrices = all_matrices,
          original_methods = all_methods
        )
        
        ensemble_matrix <- switch(
          isolate(input$ensemble_method),
          "average" = calculate_average_ensemble(all_matrices, weights = NULL),
          "weighted" = calculate_average_ensemble(all_matrices, weights = weights),
          "rank" = calculate_rank_ensemble(all_matrices),
          "consensus" = calculate_consensus_ensemble(all_matrices)
        )
        
        data_store$correlation_matrices$ensemble <- list(
          matrix = ensemble_matrix,
          method = paste0("ensemble_", isolate(input$ensemble_method)),
          settings = ensemble_settings,
          thresholded = apply_thresholding(
            ensemble_matrix,
            isolate(input$threshold_type),
            switch(isolate(input$threshold_type),
                   "absolute" = isolate(input$absolute_threshold),
                   "proportional" = isolate(input$proportional_threshold),
                   "significance" = isolate(input$significance_threshold),
                   0
            ),
            isolate(input$use_absolute_values),
            isolate(input$include_negative)
          )
        )
        
        # Increment progress
        incProgress(0.2)
      }
      
      # Mark correlation step as complete
      workflow$correlation_complete <- TRUE
      
      # Show success message
      showNotification("Correlation matrices generated successfully!", type = "message")
      
    }, error = function(e) {
      # Show error message
      showNotification(paste("Error generating correlation matrices:", e$message), type = "error")
    })
  })
})

# Preview correlation heatmap
output$correlation_heatmap <- renderPlotly({
  req(data_store$correlation_matrices)
  req(input$preview_method)
  
  # Parse preview method selection
  preview_parts <- strsplit(input$preview_method, ": ")[[1]]
  preview_type <- preview_parts[1]
  
  # Get appropriate matrix
  correlation_matrix <- switch(
    preview_type,
    "Primary Method" = data_store$correlation_matrices$primary$thresholded,
    "Additional" = data_store$correlation_matrices$additional[[strsplit(preview_parts[2], " \\(")[[1]][1]]]$thresholded,
    "Ensemble" = data_store$correlation_matrices$ensemble$thresholded
  )
  
  # Generate heatmap
  generate_heatmap(
    correlation_matrix,
    show_dendrogram = input$show_dendrogram,
    text_size = input$heatmap_text_size
  )
})

# Correlation summary
output$correlation_summary <- renderPrint({
  req(data_store$correlation_matrices)
  req(input$preview_method)
  
  # Parse preview method selection
  preview_parts <- strsplit(input$preview_method, ": ")[[1]]
  preview_type <- preview_parts[1]
  
  # Get appropriate matrix
  correlation_matrix <- switch(
    preview_type,
    "Primary Method" = data_store$correlation_matrices$primary$thresholded,
    "Additional" = data_store$correlation_matrices$additional[[strsplit(preview_parts[2], " \\(")[[1]][1]]]$thresholded,
    "Ensemble" = data_store$correlation_matrices$ensemble$thresholded
  )
  
  # Calculate summary statistics
  cat("Matrix Summary:\n")
  cat("Dimensions:", dim(correlation_matrix)[1], "x", dim(correlation_matrix)[2], "\n")
  
  # Calculate density
  total_possible <- (dim(correlation_matrix)[1] * (dim(correlation_matrix)[1] - 1)) / 2
  nonzero_count <- sum(correlation_matrix[upper.tri(correlation_matrix)] != 0)
  density <- nonzero_count / total_possible
  
  cat("Connection Density:", round(density * 100, 2), "%\n")
  cat("Non-zero Connections:", nonzero_count, "/", total_possible, "\n")
  
  # Calculate value range
  range_values <- range(correlation_matrix[correlation_matrix != 0])
  cat("Value Range:", round(range_values[1], 3), "to", round(range_values[2], 3), "\n")
  
  # Calculate mean absolute value
  mean_abs <- mean(abs(correlation_matrix[upper.tri(correlation_matrix)]))
  cat("Mean Abs Value:", round(mean_abs, 3), "\n")
})

# Download correlation matrix
output$download_correlation_matrix <- downloadHandler(
  filename = function() {
    req(input$preview_method)
    
    # Parse preview method selection for filename
    preview_parts <- strsplit(input$preview_method, ": ")[[1]]
    preview_type <- preview_parts[1]
    method_name <- if (length(preview_parts) > 1) gsub(" ", "_", preview_parts[2]) else "primary"
    
    paste0("correlation_matrix_", method_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    req(data_store$correlation_matrices)
    req(input$preview_method)
    
    # Parse preview method selection
    preview_parts <- strsplit(input$preview_method, ": ")[[1]]
    preview_type <- preview_parts[1]
    
    # Get appropriate matrix
    correlation_matrix <- switch(
      preview_type,
      "Primary Method" = data_store$correlation_matrices$primary$thresholded,
      "Additional" = data_store$correlation_matrices$additional[[strsplit(preview_parts[2], " \\(")[[1]][1]]]$thresholded,
      "Ensemble" = data_store$correlation_matrices$ensemble$thresholded
    )
    
    # Write matrix to CSV
    write.csv(correlation_matrix, file, row.names = TRUE)
  }
)

# Navigation to network analysis
observeEvent(input$proceed_to_network, {
  req(workflow$correlation_complete)
  shinydashboard::updateTabItems(session, "tabs", "network")
})

# Function to generate heatmap
generate_heatmap <- function(matrix, show_dendrogram = TRUE, text_size = 8) {
  # Check if matrix exists
  if (is.null(matrix)) {
    return(NULL)
  }
  
  # Create heatmap
  if (show_dendrogram) {
    heatmaply::heatmaply(
      matrix,
      dendrogram = "both",
      xlab = "",
      ylab = "",
      main = "Correlation Matrix",
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        limits = c(-1, 1)
      ),
      fontsize_row = text_size,
      fontsize_col = text_size,
      node_type = "scatter",
      grid_color = "gray",
      grid_width = 0.00001
    )
  } else {
    heatmaply::heatmaply(
      matrix,
      dendrogram = "none",
      xlab = "",
      ylab = "",
      main = "Correlation Matrix",
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        limits = c(-1, 1)
      ),
      fontsize_row = text_size,
      fontsize_col = text_size,
      grid_color = "gray",
      grid_width = 0.00001
    )
  }
}