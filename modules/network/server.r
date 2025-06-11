#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/network/server.R - Network analysis server logic
#########################################################

# Initial message based on correlation state
output$network_initial_message <- renderUI({
  if (!workflow$correlation_complete) {
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      "Please complete the Correlation Methods step first before performing network analysis."
    )
  } else {
    return(NULL)
  }
})

# Flag to indicate correlation is complete
output$correlation_complete <- reactive({
  return(workflow$correlation_complete)
})
outputOptions(output, "correlation_complete", suspendWhenHidden = FALSE)

# Update network matrix choices
observe({
  req(data_store$correlation_matrices)
  req(workflow$correlation_complete)
  
  # Create choices list
  choices <- c()
  
  # Add primary matrix
  primary_method_name <- names(unlist(CORRELATION_METHODS))[unlist(CORRELATION_METHODS) == data_store$correlation_matrices$primary$method]
  choices <- c(choices, paste("Primary:", primary_method_name))
  
  # Add additional matrices if they exist
  if (!is.null(data_store$correlation_matrices$additional) && length(data_store$correlation_matrices$additional) > 0) {
    for (method in names(data_store$correlation_matrices$additional)) {
      method_name <- names(unlist(CORRELATION_METHODS))[unlist(CORRELATION_METHODS) == method]
      choices <- c(choices, paste("Additional:", method_name))
    }
  }
  
  # Add ensemble matrix if it exists
  if (!is.null(data_store$correlation_matrices$ensemble)) {
    ensemble_method <- data_store$correlation_matrices$ensemble$method
    choices <- c(choices, paste("Ensemble:", ensemble_method))
  }
  
  # Update the select input
  updateSelectInput(session, "network_matrix", choices = choices, selected = choices[1])
})

# Function to get selected correlation matrix
get_selected_matrix <- function() {
  req(input$network_matrix)
  req(data_store$correlation_matrices)
  
  # Parse selection
  selection_parts <- strsplit(input$network_matrix, ": ")[[1]]
  matrix_type <- selection_parts[1]
  
  # Get the appropriate matrix
  if (matrix_type == "Primary") {
    return(data_store$correlation_matrices$primary$thresholded)
  } else if (matrix_type == "Additional") {
    method <- names(unlist(CORRELATION_METHODS))[unlist(CORRELATION_METHODS) == selection_parts[2]]
    return(data_store$correlation_matrices$additional[[method]]$thresholded)
  } else if (matrix_type == "Ensemble") {
    return(data_store$correlation_matrices$ensemble$thresholded)
  }
}

# Function to convert correlation matrix to igraph object
matrix_to_graph <- function(matrix) {
  # Create igraph object
  g <- igraph::graph_from_adjacency_matrix(
    matrix,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
  
  return(g)
}

# Run network analysis when button is clicked
observeEvent(input$run_network_analysis, {
  req(workflow$correlation_complete)
  req(input$network_matrix)
  
  # Show loading message
  withProgress(message = 'Running network analysis...', value = 0, {
    
    # Get selected correlation matrix
    corr_matrix <- get_selected_matrix()
    
    # Convert to igraph object
    graph <- matrix_to_graph(corr_matrix)
    
    # Store the graph
    data_store$network_metrics$graph <- graph
    
    # Increment progress
    incProgress(0.2)
    
    # Calculate global metrics
    global_metrics <- calculate_global_metrics(
      graph,
      metrics = input$global_metrics
    )
    
    # Store global metrics
    data_store$network_metrics$global <- global_metrics
    
    # Increment progress
    incProgress(0.2)
    
    # Calculate node metrics
    node_metrics <- calculate_node_metrics(
      graph,
      metrics = input$node_metrics
    )
    
    # Store node metrics
    data_store$network_metrics$node <- node_metrics
    
    # Increment progress
    incProgress(0.2)
    
    # Run community detection
    community_result <- detect_communities(
      graph,
      method = input$community_method,
      min_size = input$min_community_size
    )
    
    # Store community results
    data_store$network_metrics$community <- community_result
    
    # Increment progress
    incProgress(0.1)
    
    # Run hub analysis
    hub_result <- identify_hubs(
      graph,
      node_metrics,
      primary_metric = input$hub_metric,
      threshold = input$hub_threshold / 100,
      use_multivariate = input$use_multivariate_hub_criteria,
      secondary_metrics = input$secondary_hub_metrics
    )
    
    # Store hub results
    data_store$network_metrics$hubs <- hub_result
    
    # Increment progress
    incProgress(0.1)
    
    # Run bootstrap analysis if selected
    if (input$run_bootstrap) {
      # Get original data
      region_columns <- data_store$region_columns
      original_data <- data_store$processed_data[, region_columns, drop = FALSE]
      
      # Source bootstrap module
      source("modules/network/stability.R", local = TRUE)
      
      # Run bootstrap analysis
      bootstrap_result <- run_bootstrap_analysis(
        original_data,
        iterations = input$bootstrap_iterations,
        sample_percent = input$bootstrap_sample_percent / 100,
        calculate_stability = input$calculate_stability_coefficient,
        correlation_method = get_correlation_method_from_selection(input$network_matrix),
        correlation_settings = get_correlation_settings_from_selection(input$network_matrix),
        metrics = c(input$global_metrics, input$node_metrics)
      )
      
      # Store bootstrap results
      data_store$network_metrics$bootstrap <- bootstrap_result
    }
    
    # Increment progress
    incProgress(0.1)
    
    # Run structure-function coupling if selected
    if (input$run_structure_function) {
      # Check if structural connectivity matrix is uploaded
      if (!is.null(input$structural_connectivity)) {
        # Read structural connectivity matrix
        struct_conn <- read.csv(
          input$structural_connectivity$datapath,
          header = TRUE,
          row.names = 1,
          check.names = FALSE
        )
        
        # Source structure-function module
        source("modules/network/structure_function.R", local = TRUE)
        
        # Run structure-function analysis
        sf_result <- analyze_structure_function(
          functional_matrix = corr_matrix,
          structural_matrix = as.matrix(struct_conn),
          model = input$structure_function_model,
          include_indirect = input$include_indirect_paths
        )
        
        # Store structure-function results
        data_store$network_metrics$structure_function <- sf_result
      } else {
        # Show warning about missing structural connectivity
        showNotification(
          "Structural connectivity matrix is required for structure-function analysis.",
          type = "warning"
        )
      }
    }
    
    # Increment progress
    incProgress(0.1)
    
    # Run rich club analysis if selected
    if (input$run_rich_club) {
      # Source rich club module
      source("modules/network/rich_club.R", local = TRUE)
      
      # Run rich club analysis
      rich_club_result <- analyze_rich_club(
        graph,
        levels = input$rich_club_levels,
        iterations = input$randomization_iterations
      )
      
      # Store rich club results
      data_store$network_metrics$rich_club <- rich_club_result
    }
    
    # Mark network step as complete
    workflow$network_complete <- TRUE
    
    # Show success message
    showNotification("Network analysis completed successfully!", type = "message")
  })
})

# Helper function to get correlation method from selection
get_correlation_method_from_selection <- function(selection) {
  # Parse selection
  selection_parts <- strsplit(selection, ": ")[[1]]
  matrix_type <- selection_parts[1]
  
  if (matrix_type == "Primary") {
    return(data_store$correlation_matrices$primary$method)
  } else if (matrix_type == "Additional") {
    return(strsplit(selection_parts[2], " \\(")[[1]][1])
  } else if (matrix_type == "Ensemble") {
    return(paste0("ensemble_", selection_parts[2]))
  }
}

# Helper function to get correlation settings from selection
get_correlation_settings_from_selection <- function(selection) {
  # Parse selection
  selection_parts <- strsplit(selection, ": ")[[1]]
  matrix_type <- selection_parts[1]
  
  if (matrix_type == "Primary") {
    return(data_store$correlation_matrices$primary$settings)
  } else if (matrix_type == "Additional") {
    method <- strsplit(selection_parts[2], " \\(")[[1]][1]
    return(data_store$correlation_matrices$additional[[method]]$settings)
  } else if (matrix_type == "Ensemble") {
    return(data_store$correlation_matrices$ensemble$settings)
  }
}

# Network graph visualization
output$network_graph <- renderPlotly({
  req(data_store$network_metrics$graph)
  
  # Source network visualization module
  source("modules/visualization/network_plot.R", local = TRUE)
  
  # Generate network plot
  generate_network_plot(
    graph = data_store$network_metrics$graph,
    node_metrics = data_store$network_metrics$node,
    community = data_store$network_metrics$community,
    show_labels = TRUE,
    use_3d = FALSE
  )
})

# Global metrics plot and table
output$global_metrics_plot <- renderPlot({
  req(data_store$network_metrics$global)
  
  # Create a bar plot of global metrics
  metrics_df <- data.frame(
    Metric = names(data_store$network_metrics$global),
    Value = unlist(data_store$network_metrics$global),
    stringsAsFactors = FALSE
  )
  
  # Generate plot
  ggplot2::ggplot(metrics_df, ggplot2::aes(x = Metric, y = Value)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Global Network Metrics") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
})

output$global_metrics_table <- DT::renderDataTable({
  req(data_store$network_metrics$global)
  
  # Create a data frame for the table
  metrics_df <- data.frame(
    Metric = names(data_store$network_metrics$global),
    Value = unlist(data_store$network_metrics$global),
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

# Node metrics plot and table
output$node_metrics_plot <- renderPlot({
  req(data_store$network_metrics$node)
  
  # Select the first metric for visualization
  first_metric <- names(data_store$network_metrics$node)[2]  # Skip the first column (Node)
  
  # Create a data frame for plotting
  plot_df <- data.frame(
    Node = data_store$network_metrics$node$Node,
    Value = data_store$network_metrics$node[[first_metric]],
    stringsAsFactors = FALSE
  )
  
  # Sort by metric value
  plot_df <- plot_df[order(plot_df$Value, decreasing = TRUE), ]
  
  # Limit to top 20 nodes for readability
  if (nrow(plot_df) > 20) {
    plot_df <- plot_df[1:20, ]
  }
  
  # Generate plot
  ggplot2::ggplot(plot_df, ggplot2::aes(x = reorder(Node, Value), y = Value)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Top Nodes by", first_metric),
      x = "Node",
      y = first_metric
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
})

output$node_metrics_table <- DT::renderDataTable({
  req(data_store$network_metrics$node)
  
  # Generate table
  DT::datatable(
    data_store$network_metrics$node,
    options = list(
      pageLength = 10,
      searching = TRUE,
      scrollX = TRUE,
      dom = 'frtip'
    ),
    rownames = FALSE
  )
})

# Community structure plot
output$community_plot <- renderPlot({
  req(data_store$network_metrics$community)
  req(data_store$network_metrics$graph)
  
  # Source community visualization module
  source("modules/visualization/community_plot.R", local = TRUE)
  
  # Generate community plot
  generate_community_plot(
    graph = data_store$network_metrics$graph,
    community = data_store$network_metrics$community
  )
})

# Hub analysis plot
output$hub_plot <- renderPlot({
  req(data_store$network_metrics$hubs)
  req(data_store$network_metrics$graph)
  
  # Source hub visualization module
  source("modules/visualization/hub_plot.R", local = TRUE)
  
  # Generate hub plot
  generate_hub_plot(
    graph = data_store$network_metrics$graph,
    hubs = data_store$network_metrics$hubs,
    node_metrics = data_store$network_metrics$node
  )
})

# Network stability plot and results
output$stability_plot <- renderPlot({
  req(input$run_bootstrap)
  req(data_store$network_metrics$bootstrap)
  
  # Source stability visualization module
  source("modules/visualization/stability_plot.R", local = TRUE)
  
  # Generate stability plot
  generate_stability_plot(
    bootstrap_results = data_store$network_metrics$bootstrap
  )
})

output$stability_results <- renderPrint({
  req(input$run_bootstrap)
  req(data_store$network_metrics$bootstrap)
  
  # Print stability coefficients
  cat("Network Stability Results:\n\n")
  
  if (!is.null(data_store$network_metrics$bootstrap$stability_coefficients)) {
    cat("Correlation Stability Coefficients (CS):\n")
    for (metric in names(data_store$network_metrics$bootstrap$stability_coefficients)) {
      cs_value <- data_store$network_metrics$bootstrap$stability_coefficients[[metric]]
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
  }
  
  # Print confidence intervals for global metrics
  if (!is.null(data_store$network_metrics$bootstrap$global_ci)) {
    cat("\nGlobal Metrics (95% Confidence Intervals):\n")
    for (metric in names(data_store$network_metrics$bootstrap$global_ci)) {
      ci <- data_store$network_metrics$bootstrap$global_ci[[metric]]
      cat(sprintf("%s: %.3f [%.3f, %.3f]\n", 
                  metric, 
                  data_store$network_metrics$global[[metric]],
                  ci[1], ci[2]))
    }
  }
})

# Structure-function coupling plot and results
output$structure_function_plot <- renderPlot({
  req(input$run_structure_function)
  req(data_store$network_metrics$structure_function)
  
  # Source structure-function visualization module
  source("modules/visualization/structure_function_plot.R", local = TRUE)
  
  # Generate structure-function plot
  generate_structure_function_plot(
    sf_results = data_store$network_metrics$structure_function
  )
})

output$structure_function_results <- renderPrint({
  req(input$run_structure_function)
  req(data_store$network_metrics$structure_function)
  
  # Print structure-function results
  cat("Structure-Function Coupling Results:\n\n")
  
  if (!is.null(data_store$network_metrics$structure_function$model_summary)) {
    cat("Model Summary:\n")
    print(data_store$network_metrics$structure_function$model_summary)
  }
  
  if (!is.null(data_store$network_metrics$structure_function$coupling_metrics)) {
    cat("\nCoupling Metrics:\n")
    for (metric in names(data_store$network_metrics$structure_function$coupling_metrics)) {
      cat(sprintf("%s: %.3f\n", metric, 
                  data_store$network_metrics$structure_function$coupling_metrics[[metric]]))
    }
  }
})

# Navigation to visualization module
observeEvent(input$proceed_to_visualization, {
  req(workflow$network_complete)
  shinydashboard::updateTabItems(session, "tabs", "visualization")
})
  