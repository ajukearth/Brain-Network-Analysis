#########################################################
# Enhanced Brain Network Analysis Shiny App
# Import/template.R - Functions for creating template data
#########################################################

#' Generate a template data file for the brain network analysis app
#' 
#' @param num_subjects Number of subjects to include
#' @param num_regions Number of brain regions to include
#' @param num_groups Number of groups to include
#' @param add_behavior Whether to add behavioral measures
#' @param add_missing Whether to add some missing values
#' @param save_path Path to save the template file (NULL to not save)
#' @param format File format to save ("csv", "xlsx", or "rds")
#' @return Template data frame
#' @export
generate_template_data <- function(num_subjects = 20, 
                                  num_regions = 10, 
                                  num_groups = 2,
                                  add_behavior = TRUE,
                                  add_missing = FALSE,
                                  save_path = NULL,
                                  format = "csv") {
  
  # Set seed for reproducibility
  set.seed(42)
  
  # Create subject IDs
  subject_ids <- paste0("S", sprintf("%03d", 1:num_subjects))
  
  # Create group assignments
  groups <- list()
  
  # Main experimental group
  group_names <- LETTERS[1:num_groups]
  groups$Condition <- sample(group_names, num_subjects, replace = TRUE)
  
  # Add sex as another grouping variable
  groups$Sex <- sample(c("Male", "Female"), num_subjects, replace = TRUE)
  
  # Create correlated brain regions for realism
  # We'll create a correlation structure with a block diagonal pattern
  # representing functional networks
  
  # Define number of networks
  num_networks <- min(5, ceiling(num_regions / 3))
  
  # Assign regions to networks
  network_sizes <- rep(floor(num_regions / num_networks), num_networks)
  # Add any remaining regions to the first network
  network_sizes[1] <- network_sizes[1] + (num_regions - sum(network_sizes))
  
  # Create network assignments
  network_assignments <- rep(1:num_networks, times = network_sizes)
  
  # Create correlation matrix with block structure
  corr_matrix <- matrix(0.1, nrow = num_regions, ncol = num_regions)
  
  # Add stronger correlations within networks
  for (net in 1:num_networks) {
    net_regions <- which(network_assignments == net)
    for (i in net_regions) {
      for (j in net_regions) {
        if (i != j) {
          corr_matrix[i, j] <- 0.6 + rnorm(1, 0, 0.1)
        }
      }
    }
  }
  
  # Ensure it's positive definite
  corr_matrix <- Matrix::nearPD(corr_matrix)$mat
  
  # Generate correlated data
  region_data <- MASS::mvrnorm(n = num_subjects, 
                              mu = rep(5, num_regions), 
                              Sigma = corr_matrix)
  
  # Add group effects
  for (i in 1:length(groups$Condition)) {
    if (groups$Condition[i] == group_names[1]) {
      # No effect for first group
      next
    }
    
    # Add effect for other groups
    group_idx <- match(groups$Condition[i], group_names)
    
    # Affect specific networks differently
    affected_network <- (group_idx - 1) %% num_networks + 1
    affected_regions <- which(network_assignments == affected_network)
    
    # Add the effect
    effect_size <- 0.5 * group_idx
    region_data[i, affected_regions] <- region_data[i, affected_regions] + effect_size
  }
  
  # Create region names
  region_names <- paste0("Region", sprintf("%02d", 1:num_regions))
  
  # Add behavioral measures if requested
  behavioral_data <- NULL
  if (add_behavior) {
    num_behaviors <- 3
    behavior_names <- c("MemoryScore", "AttentionScore", "ExecutiveScore")
    
    # Create behavioral data correlated with specific networks
    behavioral_data <- matrix(0, nrow = num_subjects, ncol = num_behaviors)
    
    for (b in 1:num_behaviors) {
      # Choose a network that correlates with this behavior
      corr_network <- b %% num_networks + 1
      corr_regions <- which(network_assignments == corr_network)
      
      # Base behavior on the mean of those regions plus noise
      behavioral_data[, b] <- rowMeans(region_data[, corr_regions, drop = FALSE]) + 
                             rnorm(num_subjects, 0, 1)
    }
    
    colnames(behavioral_data) <- behavior_names
  }
  
  # Combine all data
  template_data <- data.frame(
    Subject = subject_ids,
    stringsAsFactors = FALSE
  )
  
  # Add group columns
  for (group_name in names(groups)) {
    template_data[[group_name]] <- groups[[group_name]]
  }
  
  # Add region columns
  colnames(region_data) <- region_names
  template_data <- cbind(template_data, region_data)
  
  # Add behavioral columns if available
  if (!is.null(behavioral_data)) {
    template_data <- cbind(template_data, behavioral_data)
  }
  
  # Add some missing values if requested
  if (add_missing) {
    # Randomly introduce 5% missing values in region data
    for (col in region_names) {
      missing_idx <- sample(1:num_subjects, size = max(1, round(0.05 * num_subjects)))
      template_data[missing_idx, col] <- NA
    }
    
    # Introduce some missing values in behavioral data if it exists
    if (!is.null(behavioral_data)) {
      for (col in colnames(behavioral_data)) {
        missing_idx <- sample(1:num_subjects, size = max(1, round(0.08 * num_subjects)))
        template_data[missing_idx, col] <- NA
      }
    }
  }
  
  # Save the template if requested
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    
    if (format == "csv") {
      utils::write.csv(template_data, file = save_path, row.names = FALSE)
    } else if (format == "xlsx") {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        warning("Package 'openxlsx' not available. Using CSV format instead.")
        utils::write.csv(template_data, file = gsub("\\.xlsx$", ".csv", save_path), 
                        row.names = FALSE)
      } else {
        openxlsx::write.xlsx(template_data, file = save_path)
      }
    } else if (format == "rds") {
      saveRDS(template_data, file = save_path)
    } else {
      warning("Unknown format specified. Using CSV format instead.")
      utils::write.csv(template_data, file = gsub("\\.[^\\.]+$", ".csv", save_path), 
                      row.names = FALSE)
    }
  }
  
  return(template_data)
}

#' Generate template data with spatial information for visualization
#' 
#' @param num_subjects Number of subjects to include
#' @param parcellation_type Type of brain parcellation to simulate
#' @param add_behavior Whether to add behavioral measures
#' @param add_coordinates Whether to add spatial coordinates for visualization
#' @param save_path Path to save the template file (NULL to not save)
#' @return Template data with spatial information
#' @export
generate_template_with_spatial <- function(num_subjects = 20,
                                         parcellation_type = "schaefer",
                                         add_behavior = TRUE,
                                         add_coordinates = TRUE,
                                         save_path = NULL) {
  
  # Set number of regions based on parcellation type
  num_regions <- switch(
    tolower(parcellation_type),
    "schaefer" = 100,  # Schaefer 100 parcellation
    "aal" = 90,        # AAL atlas
    "desikan" = 68,    # Desikan-Killiany atlas
    "power" = 264,     # Power atlas
    "craddock" = 200,  # Craddock atlas
    30                 # Default to a small number for custom
  )
  
  # Generate basic template data without spatial info
  template_data <- generate_template_data(
    num_subjects = num_subjects,
    num_regions = num_regions,
    add_behavior = add_behavior,
    save_path = NULL  # Don't save the basic template
  )
  
  # Add spatial information if requested
  if (add_coordinates) {
    # Create coordinates data frame
    coords <- data.frame(
      Region = colnames(template_data)[-(1:3)],  # Skip Subject, Condition, Sex
      x = numeric(num_regions),
      y = numeric(num_regions),
      z = numeric(num_regions),
      Network = character(num_regions),
      Hemisphere = character(num_regions),
      stringsAsFactors = FALSE
    )
    
    # Define networks
    networks <- c("Default", "Control", "Attention", "Visual", "Limbic", 
                 "Somatomotor", "Frontoparietal")
    
    # Assign regions to networks and generate coordinates
    set.seed(123)  # For reproducibility
    
    # Number of networks to use
    num_networks <- min(length(networks), ceiling(num_regions / 10))
    
    # Assign regions to networks
    network_sizes <- rep(floor(num_regions / num_networks), num_networks)
    # Add any remaining regions to the first network
    network_sizes[1] <- network_sizes[1] + (num_regions - sum(network_sizes))
    
    # Create assignments
    network_assignments <- rep(networks[1:num_networks], times = network_sizes)
    
    # Assign hemispheres (roughly balanced)
    hemispheres <- rep(c("Left", "Right"), length.out = num_regions)
    
    # Generate coordinates based on network
    network_centers <- list(
      "Default" = c(0, 40, 20),
      "Control" = c(40, 30, 10),
      "Attention" = c(-40, 30, 10),
      "Visual" = c(0, -70, 10),
      "Limbic" = c(0, 10, -10),
      "Somatomotor" = c(0, -10, 50),
      "Frontoparietal" = c(35, 10, 30)
    )
    
    for (i in 1:num_regions) {
      network <- network_assignments[i]
      hemi <- hemispheres[i]
      
      # Get base coordinates for this network
      base_coords <- network_centers[[network]]
      
      # Adjust for hemisphere
      if (hemi == "Right" && base_coords[1] < 0) {
        base_coords[1] <- -base_coords[1]
      } else if (hemi == "Left" && base_coords[1] > 0) {
        base_coords[1] <- -base_coords[1]
      }
      
      # Add random jitter
      coords[i, c("x", "y", "z")] <- base_coords + rnorm(3, 0, 5)
      coords[i, "Network"] <- network
      coords[i, "Hemisphere"] <- hemi
    }
    
    # Save the coordinate information
    if (!is.null(save_path)) {
      coords_path <- gsub("\\.[^\\.]+$", "_coordinates.csv", save_path)
      utils::write.csv(coords, file = coords_path, row.names = FALSE)
    }
    
    # Add network information to the template data
    template_info <- data.frame(
      Region = coords$Region,
      Network = coords$Network,
      Hemisphere = coords$Hemisphere,
      stringsAsFactors = FALSE
    )
    
    # Add atlas type information
    attr(template_data, "parcellation") <- parcellation_type
    attr(template_data, "coordinates") <- coords
    attr(template_data, "region_info") <- template_info
  }
  
  # Save the full template if requested
  if (!is.null(save_path)) {
    saveRDS(template_data, file = save_path)
  }
  
  return(template_data)
}
