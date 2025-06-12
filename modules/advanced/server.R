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

# Update correlation methods choices
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
  
  # Update the select inputs
  updateSelectizeInput(session, "consensus_methods", choices = choices)
  updateSelectInput(session, "stability_method", choices = choices, selected = choices[1])
  updateSelectInput(session, "info_theory_method", choices = choices, selected = choices[1])
  updateSelectInput(session, "prediction_method", choices = choices, selected = choices[1])
})

# Update group selection dropdowns
observe({
  req(data_store$network_metrics$groups)
  
  updateSelectInput(session, "consensus_group", 
                   choices = data_store$network_metrics$groups,
                   selected = data_store$network_metrics$groups[1])
  
  updateSelectInput(session, "stability_group", 
                   choices = data_store$network_metrics$groups,
                   selected = data_store$network_metrics$groups[1])
  
  updateSelectInput(session, "info_theory_group", 
                   choices = data_store$network_metrics$groups,
                   selected = data_store$network_metrics$groups[1])
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
  
  # Show progress notification
  shiny::showNotification("Running consensus analysis...", type = "message", duration = NULL, id = "consensus_progress")
  
  # Collect weights
  weights <- numeric(length(input$consensus_methods))
  for (i in seq_along(input$consensus_methods)) {
    weights[i] <- input[[paste0("weight_", i)]]
  }
  
  # Normalize weights to sum to 1
  weights <- weights / sum(weights)
  
  # Get matrices for selected methods
  matrices <- list()
  for (i in seq_along(input$consensus_methods)) {
    method <- input$consensus_methods[i]
    method_parts <- strsplit(names(input$consensus_methods)[i], ": ")[[1]]
    matrix_type <- method_parts[1]
    
    if (matrix_type == "Primary") {
      matrices[[i]] <- data_store$correlation_matrices$primary$matrices[[input$consensus_group]]
    } else if (matrix_type == "Additional") {
      matrices[[i]] <- data_store$correlation_matrices$additional[[method]]$matrices[[input$consensus_group]]
    }
  }
  
  # Compute consensus matrix
  consensus_matrix <- matrix(0, nrow = nrow(matrices[[1]]), ncol = ncol(matrices[[1]]))
  for (i in seq_along(matrices)) {
    consensus_matrix <- consensus_matrix + weights[i] * matrices[[i]]
  }
  rownames(consensus_matrix) <- rownames(matrices[[1]])
  colnames(consensus_matrix) <- colnames(matrices[[1]])
  
  # Compute similarity between methods
  similarity_matrix <- matrix(NA, nrow = length(matrices), ncol = length(matrices))
  for (i in seq_along(matrices)) {
    for (j in seq_along(matrices)) {
      # Use upper triangle of each matrix
      vec_i <- matrices[[i]][upper.tri(matrices[[i]])]
      vec_j <- matrices[[j]][upper.tri(matrices[[j]])]
      similarity_matrix[i, j] <- cor(vec_i, vec_j, method = "spearman")
    }
  }
  rownames(similarity_matrix) <- names(input$consensus_methods)
  colnames(similarity_matrix) <- names(input$consensus_methods)
  
  # Create consensus network
  threshold <- input$consensus_threshold
  binary_matrix <- consensus_matrix
  binary_matrix[abs(binary_matrix) < threshold] <- 0
  diag(binary_matrix) <- 0
  
  consensus_graph <- igraph::graph_from_adjacency_matrix(
    binary_matrix,
    mode = "undirected",
    weighted = TRUE
  )
  
  # Store results
  data_store$advanced_results$consensus <- list(
    consensus_matrix = consensus_matrix,
    similarity_matrix = similarity_matrix,
    consensus_graph = consensus_graph,
    threshold = threshold,
    methods = input$consensus_methods,
    weights = weights,
    group = input$consensus_group
  )
  
  # Calculate network metrics for consensus network
  data_store$advanced_results$consensus$metrics <- calculate_network_metrics(consensus_graph)
  
  # Remove progress notification
  shiny::removeNotification(id = "consensus_progress")
  shiny::showNotification("Consensus analysis complete!", type = "message")
  
  # Mark advanced analysis as complete
  workflow$advanced_complete <- TRUE
})

# Plot consensus network
output$consensus_network_plot <- renderPlotly({
  req(data_store$advanced_results$consensus)
  
  # Create network plot
  plot_network(
    graph = data_store$advanced_results$consensus$consensus_graph,
    layout = "fr",
    color_by = "degree",
    area_colors = data_store$preferences$area_colors,
    show_labels = TRUE
  )
})

# Plot consensus heatmap
output$consensus_heatmap <- renderPlot({
  req(data_store$advanced_results$consensus)
  
  # Create heatmap
  plot_correlation_heatmap(
    correlation_matrix = data_store$advanced_results$consensus$consensus_matrix,
    show_dendrogram = TRUE,
    title = paste("Consensus Matrix -", data_store$advanced_results$consensus$group),
    area_colors = data_store$preferences$area_colors,
    area_mapping = data_store$preferences$area_mapping
  )
})

# Plot method similarity
output$method_similarity_plot <- renderPlot({
  req(data_store$advanced_results$consensus)
  
  # Create heatmap of method similarities
  similarity_matrix <- data_store$advanced_results$consensus$similarity_matrix
  
  # Convert to long format for ggplot
  sim_df <- reshape2::melt(similarity_matrix)
  colnames(sim_df) <- c("Method1", "Method2", "Similarity")
  
  # Create plot
  ggplot2::ggplot(sim_df, ggplot2::aes(x = Method1, y = Method2, fill = Similarity)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "Method Similarity Matrix")
})

# Display consensus metrics
output$consensus_metrics <- renderPrint({
  req(data_store$advanced_results$consensus)
  
  cat("Consensus Network Metrics:\n\n")
  
  # Print global metrics
  metrics <- data_store$advanced_results$consensus$metrics
  
  cat("Global Metrics:\n")
  for (metric in names(metrics$global)) {
    cat(sprintf("%s: %.3f\n", metric, metrics$global[[metric]]))
  }
  
  cat("\nAverage Node Metrics:\n")
  node_df <- metrics$node
  cat(sprintf("Average Degree: %.3f\n", mean(node_df$degree)))
  cat(sprintf("Average Betweenness: %.3f\n", mean(node_df$betweenness)))
  cat(sprintf("Average Closeness: %.3f\n", mean(node_df$closeness)))
  cat(sprintf("Average Clustering: %.3f\n", mean(node_df$clustering)))
  
  cat("\nConsensus Method Information:\n")
  cat("Methods used:", paste(names(data_store$advanced_results$consensus$methods), collapse = ", "), "\n")
  cat("Weights:", paste(sprintf("%.2f", data_store$advanced_results$consensus$weights), collapse = ", "), "\n")
  cat("Threshold:", data_store$advanced_results$consensus$threshold, "\n")
})

# Run network stability analysis
observeEvent(input$run_stability, {
  req(data_store$correlation_matrices)
  req(input$stability_method)
  req(input$stability_group)
  
  # Show progress notification
  shiny::showNotification("Running stability analysis...", type = "message", duration = NULL, id = "stability_progress")
  
  # Get correlation matrix
  method <- input$stability_method
  method_parts <- strsplit(names(input$stability_method), ": ")[[1]]
  matrix_type <- method_parts[1]
  
  if (matrix_type == "Primary") {
    matrix <- data_store$correlation_matrices$primary$matrices[[input$stability_group]]
  } else if (matrix_type == "Additional") {
    matrix <- data_store$correlation_matrices$additional[[method]]$matrices[[input$stability_group]]
  } else {
    shiny::showNotification("Invalid correlation method selected.", type = "error")
    return()
  }
  
  # Get raw data for resampling
  raw_data <- data_store$processed_data
  group_data <- raw_data[raw_data[[data_store$metadata$group_column]] == input$stability_group, ]
  
  # Parameters
  n_boot <- input$bootstrap_iterations
  subsample_pct <- input$subsample_percentage / 100
  
  # Initialize result containers
  edge_presence <- matrix(0, nrow = nrow(matrix), ncol = ncol(matrix))
  node_metrics <- list(
    degree = matrix(NA, nrow = n_boot, ncol = nrow(matrix)),
    betweenness = matrix(NA, nrow = n_boot, ncol = nrow(matrix)),
    closeness = matrix(NA, nrow = n_boot, ncol = nrow(matrix)),
    clustering = matrix(NA, nrow = n_boot, ncol = nrow(matrix))
  )
  global_metrics <- list(
    density = numeric(n_boot),
    transitivity = numeric(n_boot),
    modularity = numeric(n_boot),
    avg_path_length = numeric(n_boot)
  )
  
  # Bootstrap process
  threshold <- data_store$network_metrics$threshold
  for (i in 1:n_boot) {
    # Subsample the data
    sample_idx <- sample(1:nrow(group_data), size = floor(nrow(group_data) * subsample_pct), replace = TRUE)
    sample_data <- group_data[sample_idx, ]
    
    # Calculate correlation matrix
    region_cols <- data_store$metadata$region_columns
    sample_matrix <- cor(sample_data[, region_cols], use = "pairwise.complete.obs")
    
    # Threshold matrix and create graph
    binary_matrix <- sample_matrix
    binary_matrix[abs(binary_matrix) < threshold] <- 0
    diag(binary_matrix) <- 0
    
    # Keep track of edge presence
    edge_presence <- edge_presence + (abs(binary_matrix) > 0)
    
    # Create graph
    graph <- igraph::graph_from_adjacency_matrix(
      binary_matrix,
      mode = "undirected",
      weighted = TRUE
    )
    
    # Calculate metrics
    metrics <- calculate_network_metrics(graph)
    
    # Store node metrics
    for (node in 1:igraph::vcount(graph)) {
      node_metrics$degree[i, node] <- metrics$node$degree[node]
      node_metrics$betweenness[i, node] <- metrics$node$betweenness[node]
      node_metrics$closeness[i, node] <- metrics$node$closeness[node]
      node_metrics$clustering[i, node] <- metrics$node$clustering[node]
    }
    
    # Store global metrics
    global_metrics$density[i] <- metrics$global$density
    global_metrics$transitivity[i] <- metrics$global$transitivity
    global_metrics$modularity[i] <- metrics$global$modularity
    global_metrics$avg_path_length[i] <- metrics$global$avg_path_length
  }
  
  # Normalize edge presence to [0, 1] range
  edge_presence <- edge_presence / n_boot
  
  # Calculate confidence intervals for global metrics
  global_ci <- list()
  for (metric in names(global_metrics)) {
    global_ci[[metric]] <- quantile(global_metrics[[metric]], c(0.025, 0.975), na.rm = TRUE)
  }
  
  # Calculate confidence intervals for node metrics
  node_ci <- list()
  for (metric in names(node_metrics)) {
    node_ci[[metric]] <- matrix(NA, nrow = ncol(node_metrics[[metric]]), ncol = 2)
    for (node in 1:ncol(node_metrics[[metric]])) {
      node_ci[[metric]][node, ] <- quantile(node_metrics[[metric]][, node], c(0.025, 0.975), na.rm = TRUE)
    }
  }
  
  # Calculate coefficient of stability
  cs_global <- list()
  for (metric in names(global_metrics)) {
    cs_global[[metric]] <- 1 - (global_ci[[metric]][2] - global_ci[[metric]][1]) / mean(global_metrics[[metric]], na.rm = TRUE)
  }
  
  cs_node <- list()
  for (metric in names(node_metrics)) {
    cs_node[[metric]] <- numeric(ncol(node_metrics[[metric]]))
    for (node in 1:ncol(node_metrics[[metric]])) {
      cs_node[[metric]][node] <- 1 - (node_ci[[metric]][node, 2] - node_ci[[metric]][node, 1]) / mean(node_metrics[[metric]][, node], na.rm = TRUE)
    }
  }
  
  # Store results
  data_store$advanced_results$stability <- list(
    edge_presence = edge_presence,
    node_metrics = node_metrics,
    global_metrics = global_metrics,
    global_ci = global_ci,
    node_ci = node_ci,
    cs_global = cs_global,
    cs_node = cs_node,
    method = input$stability_method,
    group = input$stability_group,
    n_boot = n_boot,
    subsample_pct = subsample_pct
  )
  
  # Remove progress notification
  shiny::removeNotification(id = "stability_progress")
  shiny::showNotification("Stability analysis complete!", type = "message")
  
  # Mark advanced analysis as complete
  workflow$advanced_complete <- TRUE
})

# Plot edge stability
output$edge_stability_plot <- renderPlot({
  req(data_store$advanced_results$stability)
  
  # Create heatmap of edge presence probability
  edge_presence <- data_store$advanced_results$stability$edge_presence
  rownames(edge_presence) <- rownames(data_store$correlation_matrices$primary$matrices[[1]])
  colnames(edge_presence) <- colnames(data_store$correlation_matrices$primary$matrices[[1]])
  
  # Convert to long format for ggplot
  edge_df <- reshape2::melt(edge_presence)
  colnames(edge_df) <- c("Region1", "Region2", "Probability")
  
  # Create plot
  ggplot2::ggplot(edge_df, ggplot2::aes(x = Region1, y = Region2, fill = Probability)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "red", limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::labs(title = "Edge Stability (Presence Probability)")
})

# Plot node stability
output$node_stability_plot <- renderPlot({
  req(data_store$advanced_results$stability)
  
  # Prepare data for plotting
  metrics <- c("degree", "betweenness", "closeness", "clustering")
  node_names <- rownames(data_store$correlation_matrices$primary$matrices[[1]])
  
  # Create data frame for confidence intervals
  ci_data <- data.frame(
    Node = rep(node_names, length(metrics)),
    Metric = rep(metrics, each = length(node_names)),
    Lower = numeric(length(node_names) * length(metrics)),
    Upper = numeric(length(node_names) * length(metrics)),
    Mean = numeric(length(node_names) * length(metrics)),
    stringsAsFactors = FALSE
  )
  
  # Fill in data
  idx <- 1
  for (metric in metrics) {
    for (node in 1:length(node_names)) {
      ci_data$Lower[idx] <- data_store$advanced_results$stability$node_ci[[metric]][node, 1]
      ci_data$Upper[idx] <- data_store$advanced_results$stability$node_ci[[metric]][node, 2]
      ci_data$Mean[idx] <- mean(data_store$advanced_results$stability$node_metrics[[metric]][, node], na.rm = TRUE)
      idx <- idx + 1
    }
  }
  
  # Create plot - one facet per metric
  ggplot2::ggplot(ci_data, ggplot2::aes(x = Node, y = Mean, ymin = Lower, ymax = Upper)) +
    ggplot2::geom_pointrange() +
    ggplot2::facet_wrap(~Metric, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)
    ) +
    ggplot2::labs(title = "Node Metric Stability (95% Confidence Intervals)")
})

# Plot global metrics stability
output$global_stability_plot <- renderPlot({
  req(data_store$advanced_results$stability)
  
  # Prepare data for plotting
  metrics <- names(data_store$advanced_results$stability$global_metrics)
  
  # Create data frame for confidence intervals
  ci_data <- data.frame(
    Metric = metrics,
    Lower = numeric(length(metrics)),
    Upper = numeric(length(metrics)),
    Mean = numeric(length(metrics)),
    stringsAsFactors = FALSE
  )
  
  # Fill in data
  for (i in 1:length(metrics)) {
    metric <- metrics[i]
    ci_data$Lower[i] <- data_store$advanced_results$stability$global_ci[[metric]][1]
    ci_data$Upper[i] <- data_store$advanced_results$stability$global_ci[[metric]][2]
    ci_data$Mean[i] <- mean(data_store$advanced_results$stability$global_metrics[[metric]], na.rm = TRUE)
  }
  
  # Create plot
  ggplot2::ggplot(ci_data, ggplot2::aes(x = Metric, y = Mean, ymin = Lower, ymax = Upper)) +
    ggplot2::geom_pointrange(size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(title = "Global Metric Stability (95% Confidence Intervals)")
})

# Display stability metrics
output$stability_metrics <- renderPrint({
  req(data_store$advanced_results$stability)
  
  cat("Network Stability Analysis Results:\n\n")
  
  # Print stability coefficients for global metrics
  cat("Global Metrics Stability Coefficients:\n")
  for (metric in names(data_store$advanced_results$stability$cs_global)) {
    cs_value <- data_store$advanced_results$stability$cs_global[[metric]]
    cat(sprintf("%s: %.3f", metric, cs_value))
    
    # Add qualitative interpretation
    if (cs_value >= 0.9) {
      cat(" (Excellent stability)\n")
    } else if (cs_value >= 0.7) {
      cat(" (Good stability)\n")
    } else if (cs_value >= 0.5) {
      cat(" (Moderate stability)\n")
    } else if (cs_value >= 0.3) {
      cat(" (Fair stability)\n")
    } else {
      cat(" (Poor stability)\n")
    }
  }
  
  # Print confidence intervals for global metrics
  cat("\nGlobal Metrics (95% Confidence Intervals):\n")
  for (metric in names(data_store$advanced_results$stability$global_ci)) {
    ci <- data_store$advanced_results$stability$global_ci[[metric]]
    mean_val <- mean(data_store$advanced_results$stability$global_metrics[[metric]], na.rm = TRUE)
    cat(sprintf("%s: %.3f [%.3f, %.3f]\n", metric, mean_val, ci[1], ci[2]))
  }
  
  # Print average node metric stability
  cat("\nAverage Node Metric Stability Coefficients:\n")
  for (metric in names(data_store$advanced_results$stability$cs_node)) {
    avg_cs <- mean(data_store$advanced_results$stability$cs_node[[metric]], na.rm = TRUE)
    cat(sprintf("%s: %.3f\n", metric, avg_cs))
  }
  
  # Print information about edge stability
  edge_presence <- data_store$advanced_results$stability$edge_presence
  cat("\nEdge Stability:\n")
  cat(sprintf("Average edge presence probability: %.3f\n", mean(edge_presence[upper.tri(edge_presence)])))
  cat(sprintf("Percentage of highly stable edges (>0.9): %.1f%%\n", 
              100 * sum(edge_presence[upper.tri(edge_presence)] > 0.9) / sum(upper.tri(edge_presence))))
  cat(sprintf("Percentage of moderately stable edges (>0.7): %.1f%%\n", 
              100 * sum(edge_presence[upper.tri(edge_presence)] > 0.7) / sum(upper.tri(edge_presence))))
})

# Proceed to results
observeEvent(input$proceed_to_results, {
  # Mark advanced analysis as complete
  workflow$advanced_complete <- TRUE
  
  # Navigate to results tab
  shinydashboard::updateTabItems(session, "tabs", "results")
})

# Helper function to calculate network metrics
calculate_network_metrics <- function(graph) {
  # Initialize results
  results <- list(
    global = list(),
    node = data.frame()
  )
  
  # Calculate global metrics
  results$global$density <- igraph::edge_density(graph)
  results$global$transitivity <- igraph::transitivity(graph, type = "global")
  results$global$avg_path_length <- igraph::mean_distance(graph)
  
  # Detect communities
  community <- igraph::cluster_louvain(graph)
  results$global$modularity <- igraph::modularity(community)
  
  # Calculate node metrics
  node_metrics <- data.frame(
    node = 1:igraph::vcount(graph),
    name = igraph::V(graph)$name,
    degree = igraph::degree(graph),
    betweenness = igraph::betweenness(graph, normalized = TRUE),
    closeness = igraph::closeness(graph, normalized = TRUE),
    clustering = igraph::transitivity(graph, type = "local", vids = igraph::V(graph))
  )
  
  # Handle NA values in clustering coefficient
  node_metrics$clustering[is.na(node_metrics$clustering)] <- 0
  
  # Add community membership
  node_metrics$community <- community$membership
  
  results$node <- node_metrics
  results$community <- community
  
  return(results)
}
