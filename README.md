# Enhanced Brain Network Analysis Shiny App

A comprehensive Shiny application for analyzing and visualizing functional connectivity networks in neuroscience research. This application provides an extended workflow for in-depth analysis of brain network properties.

## Features

### Comprehensive Analysis Workflow

- **7-Step Process**: Data Import → Preprocessing → Correlation Methods → Network Analysis → Visualization → Advanced Analysis → Results
- **Flexible Data Import**: Support for CSV, Excel, and other common file formats
- **Multiple Correlation Methods**: Various approaches to construct connectivity matrices
- **Graph Theory Analysis**: Comprehensive set of network metrics and analyses
- **Interactive Visualization**: Highly customizable network plots and heatmaps
- **Advanced Analysis Techniques**: Consensus methods, stability analysis, information theory, and brain-behavior prediction
- **Export Options**: Download results, visualizations, and reports in multiple formats

### Key Functionality

- **Data Preprocessing**:
  - Missing value detection and imputation
  - Quality checks and outlier detection
  - Data normalization and transformation

- **Correlation Methods**:
  - Pearson, Spearman, and Kendall correlations
  - Distance correlation for nonlinear relationships
  - Partial correlation to remove indirect effects
  - Mutual information for information-theoretic approach
  - Regularized approaches (GLASSO) for sparse networks

- **Network Analysis**:
  - Node-level metrics (degree, centrality measures, clustering)
  - Global network properties (density, transitivity, path length)
  - Community detection and modular structure
  - Hub and rich-club identification
  - Structure-function coupling analysis

- **Visualization**:
  - Interactive 2D and 3D network graphs
  - Hierarchical clustering heatmaps
  - Node and edge attribute visualization
  - Community structure visualization
  - Brain area-based coloring and analysis

- **Advanced Analysis**:
  - Multi-method consensus networks
  - Network stability through bootstrapping
  - Information theory analysis
  - Brain-behavior prediction models

## Installation

### Prerequisites

- R (version 4.0.0 or higher recommended)
- RStudio (recommended for development and local deployment)
- Required R packages (see below)

### Required Packages

```r
install.packages(c(
  "shiny", "shinydashboard", "shinydashboardPlus", "shinyjs", "DT", 
  "dplyr", "ggplot2", "igraph", "reshape2", "tidyr", "readr", 
  "openxlsx", "colourpicker", "RColorBrewer", "mice", 
  "naniar", "ggraph", "scales", "plotly"
))
