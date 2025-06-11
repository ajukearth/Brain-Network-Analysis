#########################################################
# Enhanced Brain Network Analysis Shiny App
# global.R - Global variables and package loading
#########################################################

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  # Core packages
  shiny,
  shinydashboard,
  shinydashboardPlus,
  shinyjs,
  shinyWidgets,
  shinyBS,
  DT,
  
  # Data manipulation
  dplyr,
  tidyr,
  purrr,
  stringr,
  readr,
  openxlsx,
  
  # Matrix operations
  Matrix,
  matrixcalc,
  
  # Statistics
  corpcor,
  huge,
  glasso,
  energy,
  
  # Visualization
  ggplot2,
  plotly,
  heatmaply,
  RColorBrewer,
  viridis,
  colourpicker,
  
  # Network analysis
  igraph,
  network,
  sna,
  netdiffuseR,
  
  # Advanced methods
  boot,
  kernlab,
  
  # Missing data handling
  mice,
  naniar,
  
  # Other utilities
  patchwork,
  knitr,
  markdown,
  scales
)

# Set global options
options(shiny.maxRequestSize = 100 * 1024^2)  # 100MB file size limit
options(DT.options = list(
  pageLength = 10,
  autoWidth = TRUE,
  dom = 'Blfrtip',
  buttons = c('copy', 'csv', 'excel')
))

# Define global constants
APP_VERSION <- "2.0.0"
DEFAULT_THRESHOLD <- 0.3
DEFAULT_CORR_METHOD <- "pearson"
BOOTSTRAP_ITERATIONS <- 1000

# Define correlation method options
CORRELATION_METHODS <- list(
  "Basic" = c(
    "Pearson correlation" = "pearson",
    "Spearman correlation" = "spearman",
    "Kendall correlation" = "kendall"
  ),
  "Precision-based" = c(
    "Partial correlation" = "partial",
    "Sparse inverse covariance (glasso)" = "glasso",
    "Shrinkage precision" = "shrinkage"
  ),
  "Distance-based" = c(
    "Euclidean distance" = "euclidean",
    "Distance correlation" = "distance",
    "Mahalanobis distance" = "mahalanobis"
  ),
  "Ensemble" = c(
    "Multi-method consensus" = "consensus",
    "Rank aggregation" = "rank"
  )
)

# Define network metrics
NETWORK_METRICS <- list(
  "Global" = c(
    "Density" = "density",
    "Global clustering coefficient" = "clustering",
    "Average path length" = "path_length",
    "Modularity" = "modularity",
    "Assortativity" = "assortativity",
    "Small-worldness" = "small_world"
  ),
  "Node-level" = c(
    "Degree centrality" = "degree",
    "Betweenness centrality" = "betweenness",
    "Closeness centrality" = "closeness",
    "Eigenvector centrality" = "eigenvector",
    "Local clustering coefficient" = "local_clustering",
    "Participation coefficient" = "participation"
  )
)

# Define brain region groupings
DEFAULT_BRAIN_AREAS <- list(
  "Dorsal HPC" = c("dDG", "dCA1", "dCA2", "dCA3"),
  "Ventral HPC" = c("vDG", "vCA1", "vCA3"),
  "Subiculum" = c("dSub", "vSub"),
  "Nucleus Accumbens" = c("NAc", "NAs"),
  "Frontal" = c("ACC", "IL", "PRL"),
  "Amygdala" = c("CeA", "BLA", "LA"),
  "Retrosplenial" = c("RSGab", "RSGc", "RSD")
)

# Default area colors
DEFAULT_AREA_COLORS <- c(
  "Dorsal HPC" = "#D3ADC4",
  "Ventral HPC" = "#C88AB1",
  "Subiculum" = "#9B59B6",
  "Nucleus Accumbens" = "#A3DFD7",
  "Frontal" = "#FAE9BD",
  "Amygdala" = "#F0BC94",
  "Retrosplenial" = "#85C1E9",
  "Other" = "#CCCCCC"
)

# Helper function to safely source all R files in a directory
source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  sapply(files, source)
}

# Source utility functions
source_dir("utils")