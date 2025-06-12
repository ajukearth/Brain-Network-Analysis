#########################################################
# Enhanced Brain Network Analysis Shiny App
# global.R - Global configuration and constants
#########################################################

# Load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyjs)
  library(DT)
  library(dplyr)
  library(ggplot2)
  library(igraph)
  library(reshape2)
  library(tidyr)
  library(readr)
  library(openxlsx)
  library(colourpicker)
  library(RColorBrewer)
  library(mice)
  library(naniar)
  library(ggraph)
  library(scales)
  library(plotly)
})

# App version
APP_VERSION <- "1.2.0"

# Define correlation methods
CORRELATION_METHODS <- list(
  "Pearson" = "pearson",
  "Spearman" = "spearman",
  "Kendall" = "kendall",
  "Distance Correlation" = "dcor",
  "Mutual Information" = "mi",
  "Partial Correlation" = "partial",
  "Regularized Covariance" = "glasso"
)

# Define network metrics
NETWORK_METRICS <- list(
  "Degree" = "degree",
  "Betweenness Centrality" = "betweenness",
  "Closeness Centrality" = "closeness", 
  "Clustering Coefficient" = "clustering",
  "Eigenvector Centrality" = "eigenvector",
  "PageRank" = "pagerank",
  "Hub Score" = "hub_score",
  "Authority Score" = "authority_score"
)

# Define default color palettes
DEFAULT_AREA_COLORS <- c(
  "Prefrontal" = "#E41A1C",
  "Motor" = "#377EB8",
  "Somatosensory" = "#4DAF4A",
  "Parietal" = "#984EA3",
  "Temporal" = "#FF7F00",
  "Occipital" = "#FFFF33",
  "Limbic" = "#A65628",
  "Subcortical" = "#F781BF",
  "Cerebellum" = "#999999",
  "Other" = "#DDDDDD"
)

DEFAULT_GROUP_COLORS <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
  "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"
)

# Default correlation thresholds
DEFAULT_CORRELATION_THRESHOLD <- 0.3
DEFAULT_P_VALUE_THRESHOLD <- 0.05

# Create directory structure for results export
create_results_directory <- function(base_dir = tempdir()) {
  # Create timestamp for unique directory naming
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  results_dir <- file.path(base_dir, paste0("brain_network_results_", timestamp))
  
  # Create main directory
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Create subdirectories
  subdirs <- c(
    "plots", 
    "metrics", 
    "correlation_matrices", 
    "networks", 
    "configs",
    "advanced"
  )
  
  dirs <- list()
  for (subdir in subdirs) {
    path <- file.path(results_dir, subdir)
    dir.create(path, showWarnings = FALSE)
    dirs[[subdir]] <- path
  }
  
  # Return both the main directory and subdirectories
  dirs[["main"]] <- results_dir
  
  return(dirs)
}

# Function to format metrics for Excel export
format_metrics_for_export <- function(node_metrics, global_metrics, file_path) {
  wb <- openxlsx::createWorkbook()
  
  # Add node metrics sheet
  openxlsx::addWorksheet(wb, "Node Metrics")
  openxlsx::writeData(wb, "Node Metrics", node_metrics)
  
  # Format node metrics sheet
  openxlsx::addStyle(wb, "Node Metrics", openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = 1:ncol(node_metrics))
  openxlsx::setColWidths(wb, "Node Metrics", cols = 1:ncol(node_metrics), widths = "auto")
  
  # Add global metrics sheet
  openxlsx::addWorksheet(wb, "Global Metrics")
  openxlsx::writeData(wb, "Global Metrics", global_metrics)
  
  # Format global metrics sheet
  openxlsx::addStyle(wb, "Global Metrics", openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = 1:ncol(global_metrics))
  openxlsx::setColWidths(wb, "Global Metrics", cols = 1:ncol(global_metrics), widths = "auto")
  
  # Save workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
}
