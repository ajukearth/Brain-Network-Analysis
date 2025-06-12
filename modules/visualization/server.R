#########################################################
# Enhanced Brain Network Analysis Shiny App
# modules/visualization/server.R - Visualization server logic
#########################################################

# Initial message based on network analysis state
output$visualization_initial_message <- renderUI({
  if (!workflow$network_complete) {
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      "Please complete the Network Analysis step first before visualizing results."
    )
  } else {
    return(NULL)
  }
})

# Flag to indicate network analysis is complete
output$network_complete <- reactive({
  return(workflow$network_complete)
})
outputOptions(output, "network_complete", suspendWhenHidden = FALSE)

# Update group selection dropdown
observe({
  req(data_store$network_metrics$groups)
  
  updateSelectInput(session, "plot_group", 
                   choices = data_store$network_metrics$groups,
                   selected = data_store$network_metrics$groups[1])
  
  updateSelectInput(session, "heatmap_group", 
                   choices = data_store$network_metrics$groups,
                   selected = data_store$network_metrics$groups[1])
})

# Network graph visualization
output$network_viz_plot <- renderPlotly({
  req(data_store$network_metrics$graphs)
  req(input$plot_group)
  
  # Get selected graph
  graph <- data_store$network_metrics$graphs[[input$plot_group]]
  
  # Get node metrics for this group
  node_metrics <- data_store$network_metrics$node_metrics %>%
    dplyr::filter(Group == input$plot_group)
  
  # Generate network plot
  plot_network(
    graph = graph,
    layout = input$layout_type,
    color_by = input$color_by,
    size_by = if(input$size_by == "none") NULL else input$size_by,
    node_metrics = node_metrics,
    area_colors = data_store$preferences$area_colors,
    show_labels = input$show_labels,
    use_3d = input$use_3d
  )
})

# Correlation heatmap
output$correlation_heatmap <- renderPlot({
  req(data_store$network_metrics$correlation_matrices)
  req(input$heatmap_group)
  
  # Get correlation matrix for selected group
  corr_matrix <- data_store$network_metrics$correlation_matrices[[input$heatmap_group]]
  
  # Generate heatmap
  plot_correlation_heatmap(
    correlation_matrix = corr_matrix,
    show_dendrogram = input$show_dendrogram,
    title = paste("Correlation Heatmap -", input$heatmap_group),
    area_colors = data_store$preferences$area_colors,
    area_mapping = data_store$preferences$area_mapping
  )
})

# Global metrics visualization
output$global_metrics_viz <- renderPlot({
  req(data_store$network_metrics$global_metrics)
  
  # Prepare data for plotting
  metrics_df <- tidyr::pivot_longer(
    data_store$network_metrics$global_metrics,
    cols = -Group,
    names_to = "Metric", 
    values_to = "Value"
  )
  
  # Create plot
  ggplot2::ggplot(metrics_df, ggplot2::aes(x = Group, y = Value, fill = Group)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::facet_wrap(~Metric, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Global Network Metrics by Group")
})

# Global metrics table
output$global_metrics_table <- DT::renderDataTable({
  req(data_store$network_metrics$global_metrics)
  
  DT::datatable(
    data_store$network_metrics$global_metrics,
    options = list(
      pageLength = 5,
      dom = 'tp',
      scrollX = TRUE
    ),
    rownames = FALSE
  )
})

# Node metrics visualization
output$node_metrics_plot <- renderPlot({
  req(data_store$network_metrics$node_metrics)
  req(input$node_metric_plot)
  
  # Prepare data for plotting
  plot_data <- data_store$network_metrics$node_metrics %>%
    dplyr::group_by(Group, brain_area) %>%
    dplyr::summarize(
      mean_value = mean(!!rlang::sym(input$node_metric_plot), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create plot
  ggplot2::ggplot(plot_data, ggplot2::aes(x = brain_area, y = mean_value, fill = Group)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = paste(input$node_metric_plot, "by Brain Area"),
      x = "Brain Area",
      y = paste("Mean", input$node_metric_plot)
    )
})

# Node metrics table
output$node_metrics_table <- DT::renderDataTable({
  req(data_store$network_metrics$node_metrics)
  req(input$plot_group)
  
  # Filter data for selected group
  filtered_data <- data_store$network_metrics$node_metrics %>%
    dplyr::filter(Group == input$plot_group)
  
  DT::datatable(
    filtered_data,
    options = list(
      pageLength = 10,
      scrollX = TRUE
    ),
    rownames = FALSE
  )
})

# Brain area summary visualization
output$brain_area_plot <- renderPlot({
  req(data_store$network_metrics$area_metrics)
  
  # Prepare data for plotting
  plot_data <- data_store$network_metrics$area_metrics %>%
    tidyr::pivot_longer(
      cols = c("mean_degree", "mean_betweenness", "mean_clustering"),
      names_to = "Metric",
      values_to = "Value"
    )
  
  # Create plot
  ggplot2::ggplot(plot_data, ggplot2::aes(x = brain_area, y = Value, fill = Group)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~Metric, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Brain Area Metrics")
})

# Brain area table
output$brain_area_table <- DT::renderDataTable({
  req(data_store$network_metrics$area_metrics)
  
  DT::datatable(
    data_store$network_metrics$area_metrics,
    options = list(
      pageLength = 10,
      scrollX = TRUE
    ),
    rownames = FALSE
  )
})

# Download handlers
output$download_network_plot <- downloadHandler(
  filename = function() {
    paste("network_plot_", input$plot_group, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
  },
  content = function(file) {
    # Create a static version of the plot for download
    p <- plot_network_static(
      graph = data_store$network_metrics$graphs[[input$plot_group]],
      layout = input$layout_type,
      color_by = input$color_by,
      size_by = if(input$size_by == "none") NULL else input$size_by,
      node_metrics = dplyr::filter(data_store$network_metrics$node_metrics, Group == input$plot_group),
      area_colors = data_store$preferences$area_colors,
      show_labels = input$show_labels
    )
    
    ggplot2::ggsave(file, p, width = 10, height = 8, dpi = 300)
  }
)

output$download_heatmap <- downloadHandler(
  filename = function() {
    paste("heatmap_", input$heatmap_group, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
  },
  content = function(file) {
    p <- plot_correlation_heatmap(
      correlation_matrix = data_store$network_metrics$correlation_matrices[[input$heatmap_group]],
      show_dendrogram = input$show_dendrogram,
      title = paste("Correlation Heatmap -", input$heatmap_group),
      area_colors = data_store$preferences$area_colors,
      area_mapping = data_store$preferences$area_mapping
    )
    
    ggplot2::ggsave(file, p, width = 10, height = 8, dpi = 300)
  }
)

output$download_global_metrics <- downloadHandler(
  filename = function() {
    paste("global_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
  },
  content = function(file) {
    utils::write.csv(data_store$network_metrics$global_metrics, file, row.names = FALSE)
  }
)

output$download_node_metrics <- downloadHandler(
  filename = function() {
    paste("node_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
  },
  content = function(file) {
    utils::write.csv(data_store$network_metrics$node_metrics, file, row.names = FALSE)
  }
)

output$download_brain_area_metrics <- downloadHandler(
  filename = function() {
    paste("brain_area_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
  },
  content = function(file) {
    utils::write.csv(data_store$network_metrics$area_metrics, file, row.names = FALSE)
  }
)

# Regenerate visualizations button
observeEvent(input$regenerate_visualizations, {
  req(workflow$network_complete)
  
  # Recalculate necessary visualizations
  shiny::showNotification("Regenerating visualizations...", type = "message")
  
  # Mark visualization as complete
  workflow$visualization_complete <- TRUE
  
  shiny::showNotification("Visualizations regenerated!", type = "message")
})

# Navigation to advanced analysis
observeEvent(input$proceed_to_advanced, {
  req(workflow$visualization_complete)
  shinydashboard::updateTabItems(session, "tabs", "advanced")
})

# Helper function for static network plots (for downloads)
plot_network_static <- function(graph, layout, color_by, size_by, node_metrics, 
                               area_colors, show_labels = FALSE) {
  # Create layout
  if (layout == "fr") {
    graph_layout <- igraph::layout_with_fr(graph)
  } else if (layout == "kk") {
    graph_layout <- igraph::layout_with_kk(graph)
  } else if (layout == "circle") {
    graph_layout <- igraph::layout_in_circle(graph)
  } else if (layout == "grid") {
    graph_layout <- igraph::layout_on_grid(graph)
  } else {
    graph_layout <- igraph::layout_with_fr(graph)
  }
  
  # Prepare node colors
  if (color_by == "brain_area") {
    node_colors <- area_colors[igraph::V(graph)$brain_area]
  } else {
    # Color by metric
    metric_values <- node_metrics[[color_by]]
    color_palette <- colorRampPalette(c("blue", "red"))(100)
    color_indices <- cut(metric_values, breaks = 100, labels = FALSE, include.lowest = TRUE)
    node_colors <- color_palette[color_indices]
  }
  
  # Prepare node sizes
  if (is.null(size_by)) {
    node_sizes <- rep(5, igraph::vcount(graph))
  } else {
    # Size by metric
    metric_values <- node_metrics[[size_by]]
    size_range <- c(3, 15)
    node_sizes <- scales::rescale(metric_values, to = size_range)
  }
  
  # Create plot
  p <- ggraph::ggraph(graph, layout = graph_layout) +
    ggraph::geom_edge_link(alpha = 0.5) +
    ggraph::geom_node_point(size = node_sizes, color = node_colors) +
    ggraph::theme_graph() +
    ggplot2::labs(title = "Brain Network Graph")
  
  # Add labels if requested
  if (show_labels) {
    p <- p + ggraph::geom_node_text(aes(label = name), repel = TRUE)
  }
  
  return(p)
}

# Helper function for correlation heatmaps
plot_correlation_heatmap <- function(correlation_matrix, show_dendrogram = TRUE,
                                    title = "Correlation Heatmap", 
                                    area_colors = NULL, area_mapping = NULL) {
  # Prepare data for heatmap
  matrix_data <- reshape2::melt(correlation_matrix)
  colnames(matrix_data) <- c("Region1", "Region2", "Correlation")
  
  # Add brain area information if available
  if (!is.null(area_mapping)) {
    matrix_data$Area1 <- area_mapping[matrix_data$Region1]
    matrix_data$Area2 <- area_mapping[matrix_data$Region2]
  }
  
  # Create dendrogram if requested
  if (show_dendrogram) {
    hc <- stats::hclust(stats::dist(correlation_matrix))
    region_order <- correlation_matrix[hc$order, hc$order]
    matrix_data$Region1 <- factor(matrix_data$Region1, levels = rownames(region_order))
    matrix_data$Region2 <- factor(matrix_data$Region2, levels = colnames(region_order))
  } else {
    matrix_data$Region1 <- factor(matrix_data$Region1)
    matrix_data$Region2 <- factor(matrix_data$Region2)
  }
  
  # Create plot
  p <- ggplot2::ggplot(matrix_data, ggplot2::aes(x = Region1, y = Region2, fill = Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::labs(title = title) +
    ggplot2::coord_fixed()
  
  return(p)
}
