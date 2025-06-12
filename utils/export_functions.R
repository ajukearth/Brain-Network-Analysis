#########################################################
# Enhanced Brain Network Analysis Shiny App
# utils/export_functions.R - Data export utility functions
#########################################################

#' Format network metrics for export to Excel
#' 
#' @param node_metrics Data frame with node metrics
#' @param global_metrics Data frame with global metrics
#' @param output_file Path to save the Excel file
#' @return Invisibly returns TRUE if successful
#' @export
format_metrics_for_export <- function(node_metrics, global_metrics, output_file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for this function")
  }
  
  # Create workbook
  wb <- openxlsx::createWorkbook()
  
  # Add sheet with all node metrics
  openxlsx::addWorksheet(wb, "All_Node_Metrics")
  openxlsx::writeData(wb, "All_Node_Metrics", node_metrics)
  
  # Add sheet with all global metrics
  openxlsx::addWorksheet(wb, "All_Global_Metrics")
  openxlsx::writeData(wb, "All_Global_Metrics", global_metrics)
  
  # Add sheets for each group's node metrics
  if ("Group" %in% names(node_metrics)) {
    unique_groups <- unique(node_metrics$Group)
    for (group in unique_groups) {
      group_data <- node_metrics[node_metrics$Group == group, ]
      sheet_name <- paste0(gsub("[^a-zA-Z0-9]", "_", group), "_Node_Metrics")
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, group_data)
    }
  }
  
  # Add sheets for each group's global metrics
  if ("Group" %in% names(global_metrics)) {
    for (group in unique(global_metrics$Group)) {
      group_data <- global_metrics[global_metrics$Group == group, ]
      if (nrow(group_data) > 0) {
        sheet_name <- paste0(gsub("[^a-zA-Z0-9]", "_", group), "_Global_Metrics")
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, group_data)
      }
    }
  }
  
  # Save workbook
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  
  return(invisible(TRUE))
}

#' Export all results to a zip file
#' 
#' @param results_list List containing all results to export
#' @param output_file Path to save the zip file
#' @param include_configs Whether to include configuration settings
#' @return Invisibly returns TRUE if successful
#' @export
export_all_results <- function(results_list, output_file, include_configs = TRUE) {
  # Create a temporary directory for results
  temp_dir <- file.path(tempdir(), "brain_network_analysis_export")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  dir.create(temp_dir, recursive = TRUE)
  
  # Create subdirectories
  dirs <- c(
    "metrics" = file.path(temp_dir, "metrics"),
    "plots" = file.path(temp_dir, "plots"),
    "networks" = file.path(temp_dir, "networks"),
    "heatmaps" = file.path(temp_dir, "heatmaps"),
    "configs" = file.path(temp_dir, "configs"),
    "correlation_matrices" = file.path(temp_dir, "correlation_matrices")
  )
  
  # Create all directories
  lapply(dirs, dir.create, recursive = TRUE)
  
  # Export data based on what's available in results_list
  
  # Node metrics
  if (!is.null(results_list$node_metrics)) {
    utils::write.csv(
      results_list$node_metrics,
      file.path(dirs["metrics"], "node_metrics.csv"),
      row.names = FALSE
    )
  }
  
  # Global metrics
  if (!is.null(results_list$global_metrics)) {
    utils::write.csv(
      results_list$global_metrics,
      file.path(dirs["metrics"], "global_metrics.csv"),
      row.names = FALSE
    )
  }
  
  # Area metrics
  if (!is.null(results_list$area_metrics)) {
    utils::write.csv(
      results_list$area_metrics,
      file.path(dirs["metrics"], "brain_area_metrics.csv"),
      row.names = FALSE
    )
  }
  
  # Create Excel file with all metrics
  if (requireNamespace("openxlsx", quietly = TRUE) && 
      !is.null(results_list$node_metrics) && !is.null(results_list$global_metrics)) {
    
    metrics_file <- file.path(dirs["metrics"], "all_metrics.xlsx")
    format_metrics_for_export(
      results_list$node_metrics,
      results_list$global_metrics,
      metrics_file
    )
  }
  
  # Save correlation matrices
  if (!is.null(results_list$networks)) {
    for (group in names(results_list$networks)) {
      network <- results_list$networks[[group]]
      
      if (!is.null(network) && !is.null(network$correlation_matrix)) {
        # Save correlation matrix
        safe_group <- gsub("[^a-zA-Z0-9]", "_", group)
        utils::write.csv(
          network$correlation_matrix,
          file.path(dirs["correlation_matrices"], paste0("correlation_matrix_", safe_group, ".csv")),
          row.names = TRUE
        )
      }
    }
  }
  
  # Save analysis configuration
  if (include_configs && !is.null(results_list$config)) {
    # Convert config list to data frame
    config_df <- data.frame(
      Parameter = names(results_list$config),
      Value = unlist(lapply(results_list$config, as.character)),
      stringsAsFactors = FALSE
    )
    
    utils::write.csv(
      config_df,
      file.path(dirs["configs"], "analysis_config.csv"),
      row.names = FALSE
    )
  }
  
  # Save plots if available
  if (!is.null(results_list$plots)) {
    for (plot_name in names(results_list$plots)) {
      plot_obj <- results_list$plots[[plot_name]]
      if (inherits(plot_obj, "ggplot")) {
        safe_name <- gsub("[^a-zA-Z0-9]", "_", plot_name)
        ggplot2::ggsave(
          file.path(dirs["plots"], paste0(safe_name, ".png")),
          plot_obj,
          width = 10,
          height = 8,
          dpi = 300
        )
      }
    }
  }
  
  # Create README file
  readme_text <- c(
    "# Brain Network Analysis Results",
    "",
    paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "## Contents",
    "",
    "- metrics/: Node and network-level metrics",
    "- plots/: Visualizations",
    "- networks/: Network data",
    "- heatmaps/: Correlation heatmaps",
    "- correlation_matrices/: Raw correlation matrices",
    "- configs/: Analysis configuration",
    "",
    "## Analysis Parameters",
    ""
  )
  
  # Add configuration details to README if available
  if (include_configs && !is.null(results_list$config)) {
    for (param_name in names(results_list$config)) {
      param_value <- results_list$config[[param_name]]
      if (is.list(param_value) || is.vector(param_value) && length(param_value) > 1) {
        param_value <- paste(param_value, collapse = ", ")
      }
      readme_text <- c(readme_text, paste("-", param_name, ":", param_value))
    }
  } else {
    readme_text <- c(readme_text, "Analysis parameters not available")
  }
  
  # Write README file
  writeLines(readme_text, file.path(temp_dir, "README.md"))
  
  # Create ZIP archive of all results
  files_to_zip <- list.files(temp_dir, recursive = TRUE, full.names = TRUE)
  
  # Use different zip methods based on platform
  if (.Platform$OS.type == "windows") {
    utils::zip(output_file, files_to_zip, flags = "-r9X")
  } else {
    # For Unix-like systems
    current_dir <- getwd()
    setwd(temp_dir)
    utils::zip(output_file, list.files(".", recursive = TRUE), flags = "-r")
    setwd(current_dir)
  }
  
  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)
  
  return(invisible(TRUE))
}

#' Export network to GraphML format
#' 
#' @param g An igraph network object
#' @param file Path to save the GraphML file
#' @return Invisibly returns TRUE if successful
#' @export
export_network_to_graphml <- function(g, file) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  tryCatch({
    igraph::write_graph(g, file, format = "graphml")
    return(invisible(TRUE))
  }, error = function(e) {
    warning(paste("Error exporting network to GraphML:", e$message))
    return(invisible(FALSE))
  })
}

#' Export network to Pajek format
#' 
#' @param g An igraph network object
#' @param file Path to save the Pajek file
#' @return Invisibly returns TRUE if successful
#' @export
export_network_to_pajek <- function(g, file) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  tryCatch({
    igraph::write_graph(g, file, format = "pajek")
    return(invisible(TRUE))
  }, error = function(e) {
    warning(paste("Error exporting network to Pajek:", e$message))
    return(invisible(FALSE))
  })
}

#' Export network to GML format
#' 
#' @param g An igraph network object
#' @param file Path to save the GML file
#' @return Invisibly returns TRUE if successful
#' @export
export_network_to_gml <- function(g, file) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  tryCatch({
    igraph::write_graph(g, file, format = "gml")
    return(invisible(TRUE))
  }, error = function(e) {
    warning(paste("Error exporting network to GML:", e$message))
    return(invisible(FALSE))
  })
}

#' Create a formatted report of results in HTML format
#' 
#' @param results_list List containing all results
#' @param file Path to save the HTML report
#' @return Invisibly returns TRUE if successful
#' @export
create_html_report <- function(results_list, file) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for this function")
  }
  
  # Create a temporary directory for the report
  temp_dir <- file.path(tempdir(), "brain_network_report")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  dir.create(temp_dir, recursive = TRUE)
  
  # Create a temporary Rmd file
  rmd_file <- file.path(temp_dir, "report.Rmd")
  
  # Create the Rmd content
  rmd_content <- c(
    "---",
    "title: \"Brain Network Analysis Report\"",
    paste0("date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\""),
    "output: html_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(ggplot2)",
    "library(DT)",
    "```",
    "",
    "## Analysis Parameters",
    "",
    "```{r parameters}"
  )
  
  # Add configuration details if available
  if (!is.null(results_list$config)) {
    rmd_content <- c(rmd_content,
                    "config_df <- data.frame(",
                    "  Parameter = names(results_list$config),",
                    "  Value = unlist(lapply(results_list$config, as.character)),",
                    "  stringsAsFactors = FALSE",
                    ")",
                    "DT::datatable(config_df, options = list(dom = 't'))")
  } else {
    rmd_content <- c(rmd_content, "cat('Analysis parameters not available')")
  }
  
  rmd_content <- c(rmd_content, "```", "", "## Global Network Metrics", "", "```{r global_metrics}")
  
  # Add global metrics if available
  if (!is.null(results_list$global_metrics)) {
    rmd_content <- c(rmd_content, "DT::datatable(results_list$global_metrics)")
  } else {
    rmd_content <- c(rmd_content, "cat('Global metrics not available')")
  }
  
  rmd_content <- c(rmd_content, "```", "", "## Node-Level Metrics", "", "```{r node_metrics}")
  
  # Add node metrics if available
  if (!is.null(results_list$node_metrics)) {
    rmd_content <- c(rmd_content, "DT::datatable(results_list$node_metrics)")
  } else {
    rmd_content <- c(rmd_content, "cat('Node metrics not available')")
  }
  
  rmd_content <- c(rmd_content, "```", "", "## Visualizations", "", "```{r plots, fig.width=10, fig.height=8}")
  
  # Add plots if available
  if (!is.null(results_list$plots)) {
    for (plot_name in names(results_list$plots)) {
      rmd_content <- c(rmd_content,
                      paste0("cat('### ", plot_name, "\\n\\n')"),
                      paste0("print(results_list$plots[['", plot_name, "']])"),
                      "cat('\\n\\n')")
    }
  } else {
    rmd_content <- c(rmd_content, "cat('No plots available')")
  }
  
  rmd_content <- c(rmd_content, "```")
  
  # Write the Rmd file
  writeLines(rmd_content, rmd_file)
  
  # Render the report
  tryCatch({
    rmarkdown::render(
      rmd_file,
      output_file = file,
      envir = new.env()
    )
    return(invisible(TRUE))
  }, error = function(e) {
    warning(paste("Error creating HTML report:", e$message))
    return(invisible(FALSE))
  })
}
