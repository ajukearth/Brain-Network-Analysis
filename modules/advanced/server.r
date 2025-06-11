#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/advanced/server.R - Advanced analysis server logic
#########################################################

# Initial message based on visualization state
output$advanced_initial_message <- renderUI({
  if (!workflow$visualization_complete) {
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      "Please complete the Visualization step first before performing advanced analysis."
    )
  } else {
    return(NULL)
  }
})

# Flag to indicate visualization is complete
output$visualization_complete <- reactive({
  return(workflow$visualization_complete)
})
outputOptions(output, "visualization_complete", suspendWhenHidden = FALSE)

# Update consensus methods choices
observe({
  req(data_store$correlation_matrices)
  
  # Create choices list
  choices <- c()
  
  # Add primary matrix
  primary_method_name <- names(unlist(CORRELATION_METHODS))[unlist(CORRELATION_METHODS) == data_store$correlation_matrices$primary$method]
  choices <- c(choices, setNames(data_store$correlation_matrices$primary$method, paste("Primary:", primary_method_name)))
  
  # Add additional matrices if they exist
  if (!is.null(data_store$correlation_matrices$additional) && length(data_store$correlation_matrices$additional) > 0) {
    for (method in names(data_store$correlation_matrices$additional)) {
      method_name <- names(unlist(CORRELATION_METHODS))[unlist(CORRELATION_METHODS) == method]
      choices <- c(choices, setNames(method, paste("Additional:", method_name)))
    }
  }
  
  # Update the select input
  updateSelectizeInput(session, "consensus_methods", choices = choices)
  updateSelectInput(session, "stability_method", choices = choices, selected = choices[1])
  updateSelectInput(session, "fingerprinting_method", choices = choices, selected = choices[1])
  updateSelectInput(session, "prediction_method", choices = choices, selected = choices[1])
})

# Dynamic UI for consensus weights
output$consensus_weights_ui <- renderUI({
  req(input$consensus_methods)
  
  if (length(input$consensus_methods) == 0) {
    return(NULL)
  }
  
  # Create a list of sliders for each selected method
  weight_sliders <- lapply(seq_along(input$consensus_methods), function(i) {
    method <- input$consensus_methods[i]
    method_name <- names(which(unlist(CORRELATION_METHODS) == method))
    if (length(method_name) == 0) {
      # Handle case where method doesn't have a name in CORRELATION_METHODS
      method_name <- method
    }
    
    sliderInput(
      inputId = paste0("weight_", i),
      label = paste0("Weight for ", method_name),
      min = 0,
      max = 1,
      value = 1/length(input$consensus_methods),
      step = 0.05
    )
  })
  
  # Return a div with all weight sliders
  do.call(tagList, c(
    weight_sliders,
    list(helpText("Weights should ideally sum to 1.0"))
  ))
})

# Run multi-method consensus analysis
observeEvent(input$run_consensus, {
  req(data_store$correlation_matrices)
  req(input$consensus_methods)
  
  if (length(input$consensus_methods) < 2) {
    showNotification("Please select at least two methods for consensus analysis.", type = "warning")
    return()
  }
  
  # Show loading message
  withProgress(message = 'Generating consensus matrix...', value = 0, {
    
    # Source ensemble module
    source("modules/correlation/ensemble.R", local = TRUE)
    
    # Get all matrices
    all_matrices <- list()
    all_methods <- c()
    
    for (method in input$consensus_methods) {
      if (method == data_store$correlation_matrices$primary$method) {
        all_matrices[[length(all_matrices) + 1]] <- data_store$correlation_matrices$primary$matrix
        all_methods <- c(all_methods, method)
      } else if (!is.null(data_store$correlation_matrices$additional) && 
               method %in% names(data_store$correlation_matrices$additional)) {
        all_matrices[[length(all_matrices) + 1]] <- data_store$correlation_matrices$additional[[method]]$matrix
        all_methods <- c(all_methods, method)
      }
    }
    
    # Get weights if using weighted average
    weights <- NULL
    if (input$consensus_approach == "weighted") {
      weights <- numeric(length(all_methods))
      for (i in seq_along(all_methods)) {
        weights[i] <- input[[paste0("weight_", i)]]
      }
      
      # Normalize weights to sum to 1
      weights <- weights / sum(weights)
    }
    
    # Generate ensemble matrix
    ensemble_matrix <- switch(
      input$consensus_approach,
      "average" = calculate_average_ensemble(all_matrices, weights = NULL),
      "weighted" = calculate_average_ensemble(all_matrices, weights = weights),
      "rank" = calculate_rank_ensemble(all_matrices),
      "consensus" = calculate_consensus_ensemble(all_matrices)
    )
    
    # Apply threshold if specified
    if (!is.null(input$consensus_threshold) && input$consensus_threshold > 0) {
      thresholded_matrix <- ensemble_matrix
      thresholded_matrix[abs(thresholded_matrix) < input$consensus_threshold] <- 0
    } else {
      thresholded_matrix <- ensemble_matrix
    }
    
    # Store ensemble matrix
    data_store$advanced_results$consensus <- list(
      matrix = ensemble_matrix,
      thresholded = thresholded_matrix,
      methods = all_methods,
      weights = weights,
      approach = input$consensus_approach
    )
    
    # Increment progress
    incProgress(0.5)
    
    # Calculate network metrics for consensus matrix
    # Convert to igraph object
    consensus_graph <- matrix_to_graph(thresholded_matrix)
    
    # Calculate global metrics
    global_metrics <- calculate_global_metrics(consensus_graph)
    
    # Calculate node metrics
    node_metrics <- calculate_node_metrics(consensus_graph)
    
    # Store metrics
    data_store$advanced_results$consensus$metrics <- list(
      global = global_metrics,
      node = node_metrics,
      graph = consensus_graph
    )
    
    # Show success message
    showNotification("Consensus matrix generated successfully!", type = "message")
  })
})

# Display consensus matrix heatmap
output$consensus_heatmap <- renderPlotly({
  req(data_store$advanced_results$consensus)
  
  # Generate heatmap
  generate_heatmap(
    data_store$advanced_results$consensus$thresholded,
    show_dendrogram = TRUE,
    text_size = 8
  )
})

# Display consensus metrics table
output$consensus_metrics <- DT::renderDataTable({
  req(data_store$advanced_results$consensus$metrics)
  
  # Create a data frame for the table
  metrics_df <- data.frame(
    Metric = names(data_store$advanced_results$consensus$metrics$global),
    Value = unlist(data_store$advanced_results$consensus$metrics$global),
    stringsAsFactors = FALSE
  )
  
  # Generate table
  DT::datatable(
    metrics_df,
    options = list(
      pageLength = 10,
      searching = FALSE,
      dom = 't',
      ordering = TRUE
    ),
    rownames = FALSE
  )
})

# Download consensus matrix
output$download_consensus_matrix <- downloadHandler(
  filename = function() {
    paste0("consensus_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    req(data_store$advanced_results$consensus)
    
    # Write matrix to CSV
    write.csv(data_store$advanced_results$consensus$thresholded, file, row.names = TRUE)
  }
)

# Run network stability analysis
observeEvent(input$run_stability, {
  req(data_store$correlation_matrices)
  req(input$stability_method)
  req(data_store$processed_data)
  req(data_store$region_columns)
  
  # Show loading message
  withProgress(message = 'Running stability analysis...', value = 0, {
    
    # Source bootstrap module
    source("modules/network/stability.R", local = TRUE)
    
    # Get original data
    region_columns <- data_store$region_columns
    original_data <- data_store$processed_data[, region_columns, drop = FALSE]
    
    # Get correlation method
    if (input$stability_method == data_store$correlation_matrices$primary$method) {
      method <- input$stability_method
      settings <- data_store$correlation_matrices$primary$settings
    } else if (!is.null(data_store$correlation_matrices$additional) && 
              input$stability_method %in% names(data_store$correlation_matrices$additional)) {
      method <- input$stability_method
      settings <- data_store$correlation_matrices$additional[[method]]$settings
    } else {
      showNotification("Selected correlation method not found.", type = "error")
      return()
    }
    
    # Run bootstrap analysis
    bootstrap_result <- run_bootstrap_analysis(
      original_data,
      iterations = input$bootstrap_iterations,
      sample_percent = input$bootstrap_sample_percent / 100,
      calculate_stability = input$calculate_cs_coefficient,
      correlation_method = method,
      correlation_settings = settings,
      metrics = input$stability_metrics
    )
    
    # Store bootstrap results
    data_store$advanced_results$stability <- bootstrap_result
    
    # Increment progress
    incProgress(0.9)
    
    # Show success message
    showNotification("Stability analysis completed successfully!", type = "message")
  })
})

# Display global metrics stability plot
output$stability_global_plot <- renderPlot({
  req(data_store$advanced_results$stability)
  
  # Source stability visualization module
  source("modules/visualization/stability_plot.R", local = TRUE)
  
  # Generate stability plot
  generate_stability_plot(data_store$advanced_results$stability)
})

# Display global metrics stability summary
output$stability_global_summary <- renderPrint({
  req(data_store$advanced_results$stability)
  req(data_store$advanced_results$stability$global_ci)
  
  cat("Global Metrics Stability Summary:\n\n")
  
  for (metric in names(data_store$advanced_results$stability$global_ci)) {
    ci <- data_store$advanced_results$stability$global_ci[[metric]]
    mean_val <- mean(data_store$advanced_results$stability$global_values[[metric]], na.rm = TRUE)
    cat(sprintf("%s: %.4f [%.4f, %.4f]\n", 
                metric, mean_val, ci[1], ci[2]))
    
    # Calculate coefficient of variation
    cv <- sd(data_store$advanced_results$stability$global_values[[metric]], na.rm = TRUE) / mean_val
    cat(sprintf("  Coefficient of Variation: %.2f%%\n", cv * 100))
  }
})

# Display node metrics stability plot
output$stability_node_plot <- renderPlot({
  req(data_store$advanced_results$stability)
  req(data_store$advanced_results$stability$node_ci)
  
  # Source stability visualization module
  source("modules/visualization/stability_plot.R", local = TRUE)
  
  # Select first node metric
  first_metric <- names(data_store$advanced_results$stability$node_ci)[1]
  
  # Generate stability histogram for the first node
  generate_stability_histogram(
    data_store$advanced_results$stability,
    metric = first_metric,
    is_global = FALSE,
    node_index = 1
  )
})

# Display node metrics stability summary
output$stability_node_summary <- renderPrint({
  req(data_store$advanced_results$stability)
  req(data_store$advanced_results$stability$node_ci)
  
  cat("Node Metrics Stability Summary:\n\n")
  
  # Calculate average CI width for each metric
  for (metric in names(data_store$advanced_results$stability$node_ci)) {
    ci_widths <- data_store$advanced_results$stability$node_ci[[metric]][, 2] - 
                data_store$advanced_results$stability$node_ci[[metric]][, 1]
    
    avg_width <- mean(ci_widths, na.rm = TRUE)
    max_width <- max(ci_widths, na.rm = TRUE)
    
    cat(sprintf("%s:\n", metric))
    cat(sprintf("  Average CI Width: %.4f\n", avg_width))
    cat(sprintf("  Maximum CI Width: %.4f\n", max_width))
    cat(sprintf("  Node with Widest CI: %s\n\n", 
                rownames(data_store$advanced_results$stability$node_ci[[metric]])[which.max(ci_widths)]))
  }
})

# Display correlation stability coefficients plot
output$cs_coefficient_plot <- renderPlot({
  req(data_store$advanced_results$stability)
  req(data_store$advanced_results$stability$stability_coefficients)
  
  # Source stability visualization module
  source("modules/visualization/stability_plot.R", local = TRUE)
  
  # Select first node metric
  first_metric <- names(data_store$advanced_results$stability$stability_coefficients)[1]
  
  # Generate stability correlation plot for the first metric
  generate_stability_correlation_plot(
    data_store$advanced_results$stability,
    metric = first_metric
  )
})

# Display correlation stability coefficients summary
output$cs_coefficient_summary <- renderPrint({
  req(data_store$advanced_results$stability)
  req(data_store$advanced_results$stability$stability_coefficients)
  
  cat("Correlation Stability Coefficients (CS):\n\n")
  
  for (metric in names(data_store$advanced_results$stability$stability_coefficients)) {
    cs_value <- data_store$advanced_results$stability$stability_coefficients[[metric]]
    cat(sprintf("%s: %.3f", metric, cs_value))
    
    # Add interpretation
    if (cs_value >= 0.7) {
      cat(" (Excellent stability)\n")
    } else if (cs_value >= 0.5) {
      cat(" (Good stability)\n")
    } else if (cs_value >= 0.3) {
      cat(" (Fair stability)\n")
    } else {
      cat(" (Poor stability)\n")
    }
  }
  
  cat("\nInterpretation:\n")
  cat("CS >= 0.7: Excellent stability\n")
  cat("CS >= 0.5: Good stability\n")
  cat("CS >= 0.3: Fair stability\n")
  cat("CS < 0.3: Poor stability\n\n")
  
  cat("The CS coefficient indicates the maximum proportion of subjects\n")
  cat("that can be dropped while maintaining a correlation of 0.7 or higher\n")
  cat("between the metrics calculated on different subsets of the data.\n")
})

# Display bootstrap consensus heatmap
output$bootstrap_consensus_heatmap <- renderPlotly({
  req(data_store$advanced_results$stability)
  
  # Source stability visualization module
  source("modules/visualization/stability_plot.R", local = TRUE)
  
  # Get bootstrap matrices for first global metric
  first_metric <- names(data_store$advanced_results$stability$global_values)[1]
  
  # Generate consensus matrix plot with threshold 0.5
  generate_consensus_matrix_plot(
    bootstrap_matrices = NULL,  # Would be a list of bootstrap matrices
    threshold = 0.5,
    show_dendrogram = TRUE
  )
})

# The remaining observeEvents and outputs for fingerprinting, information flow analysis, 
# and brain-behavior prediction would be implemented similarly, but are omitted here for brevity
