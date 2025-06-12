#########################################################
# Enhanced Brain Network Analysis Shiny App
# Visualization/region_plot.R - Brain region visualization
#########################################################

#' Create a visualization of brain regions in 2D or 3D space
#' 
#' @param coordinates Data frame with region coordinates (x, y, z)
#' @param values Optional values to map to region colors or sizes
#' @param options List of visualization options:
#'   - type: Plot type ("2d", "glass_brain", "3d", "flatmap", "circle")
#'   - hemisphere: Hemisphere to show ("both", "left", "right")
#'   - view: View angle for 3D plots ("axial", "sagittal", "coronal")
#'   - color_by: Variable to map to colors
#'   - size_by: Variable to map to node sizes
#'   - palette: Color palette for mapping
#'   - background: Background color or image
#'   - labels: Whether to show region labels
#'   - interactive: Whether to create interactive plot
#'   - title: Plot title
#' @return ggplot2, plotly, or other plot object
#' @export
create_region_plot <- function(coordinates, values = NULL, options = NULL) {
  # Check inputs
  if (!is.data.frame(coordinates)) {
    stop("coordinates must be a data frame")
  }
  
  # Check for required columns
  required_cols <- c("Region", "x", "y", "z")
  missing_cols <- setdiff(required_cols, names(coordinates))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns in coordinates: ", paste(missing_cols, collapse = ", "))
  }
  
  # Default options
  if (is.null(options)) {
    options <- list(
      type = "2d",
      hemisphere = "both",
      view = "axial",
      color_by = NULL,
      size_by = NULL,
      palette = "viridis",
      background = "white",
      labels = FALSE,
      interactive = FALSE,
      title = "Brain Region Visualization"
    )
  }
  
  # Extract options with defaults
  plot_type <- ifelse(is.null(options$type), "2d", options$type)
  hemisphere <- ifelse(is.null(options$hemisphere), "both", options$hemisphere)
  view <- ifelse(is.null(options$view), "axial", options$view)
  color_by <- options$color_by
  size_by <- options$size_by
  palette <- ifelse(is.null(options$palette), "viridis", options$palette)
  background <- ifelse(is.null(options$background), "white", options$background)
  show_labels <- ifelse(is.null(options$labels), FALSE, options$labels)
  interactive <- ifelse(is.null(options$interactive), FALSE, options$interactive)
  title <- ifelse(is.null(options$title), "Brain Region Visualization", options$title)
  
  # Filter by hemisphere if requested
  if (hemisphere == "left") {
    coordinates <- coordinates[coordinates$x <= 0, ]
  } else if (hemisphere == "right") {
    coordinates <- coordinates[coordinates$x >= 0, ]
  }
  
  # Combine with values if provided
  if (!is.null(values)) {
    if (is.vector(values)) {
      if (is.null(names(values))) {
        stop("values must be a named vector if not a data frame")
      }
      
      # Convert to data frame
      values_df <- data.frame(
        Region = names(values),
        Value = values,
        stringsAsFactors = FALSE
      )
    } else if (is.data.frame(values)) {
      # Ensure Region column exists
      if (!"Region" %in% names(values)) {
        stop("values data frame must contain a 'Region' column")
      }
      
      values_df <- values
    } else {
      stop("values must be a named vector or data frame")
    }
    
    # Merge with coordinates
    plot_data <- merge(coordinates, values_df, by = "Region", all.x = TRUE)
  } else {
    plot_data <- coordinates
  }
  
  # Create different types of plots
  if (plot_type == "2d") {
    return(create_2d_brain_plot(plot_data, color_by, size_by, palette, 
                               view, show_labels, interactive, title))
  } else if (plot_type == "glass_brain") {
    return(create_glass_brain_plot(plot_data, color_by, size_by, palette, 
                                  view, show_labels, interactive, title))
  } else if (plot_type == "3d") {
    return(create_3d_brain_plot(plot_data, color_by, size_by, palette, 
                               show_labels, interactive, title))
  } else if (plot_type == "flatmap") {
    return(create_flatmap_plot(plot_data, color_by, size_by, palette, 
                              show_labels, interactive, title))
  } else if (plot_type == "circle") {
    return(create_circle_plot(plot_data, color_by, size_by, palette, 
                             show_labels, interactive, title))
  } else {
    stop("Unknown plot type: ", plot_type)
  }
}

#' Create a 2D projection of brain regions
#' 
#' @param data Data frame with region information
#' @param color_by Variable to map to colors
#' @param size_by Variable to map to sizes
#' @param palette Color palette
#' @param view View angle
#' @param show_labels Whether to show labels
#' @param interactive Whether to create interactive plot
#' @param title Plot title
#' @return ggplot2 or plotly plot
#' @export
create_2d_brain_plot <- function(data, color_by = NULL, size_by = NULL, 
                                palette = "viridis", view = "axial", 
                                show_labels = FALSE, interactive = FALSE, 
                                title = "2D Brain Region Plot") {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for 2D brain plots")
  }
  
  # Select coordinates based on view
  if (view == "axial") {
    x_coord <- "x"
    y_coord <- "y"
    subtitle <- "Axial View (Top)"
  } else if (view == "sagittal") {
    x_coord <- "y"
    y_coord <- "z"
    subtitle <- "Sagittal View (Side)"
  } else if (view == "coronal") {
    x_coord <- "x"
    y_coord <- "z"
    subtitle <- "Coronal View (Front)"
  } else {
    stop("Unknown view: ", view)
  }
  
  # Basic plot
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_coord, y = y_coord)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed()
  
  # Add color mapping if requested
  if (!is.null(color_by)) {
    if (color_by %in% names(data)) {
      p <- p + ggplot2::aes_string(color = color_by)
      
      # Add color scale based on palette
      if (palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
        p <- p + viridis::scale_color_viridis()
      } else if (palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
        p <- p + ggplot2::scale_color_brewer(palette = "Set1")
      } else {
        p <- p + ggplot2::scale_color_gradient(low = "blue", high = "red")
      }
    } else {
      warning("color_by variable '", color_by, "' not found in data")
    }
  }
  
  # Add size mapping if requested
  if (!is.null(size_by)) {
    if (size_by %in% names(data)) {
      p <- p + ggplot2::aes_string(size = size_by)
    } else {
      warning("size_by variable '", size_by, "' not found in data")
    }
  }
  
  # Add points
  p <- p + ggplot2::geom_point(alpha = 0.8)
  
  # Add region labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = Region),
      size = 3,
      hjust = -0.2,
      vjust = 0.5,
      check_overlap = TRUE
    )
  }
  
  # Make interactive if requested
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create a glass brain plot
#' 
#' @param data Data frame with region information
#' @param color_by Variable to map to colors
#' @param size_by Variable to map to sizes
#' @param palette Color palette
#' @param view View angle
#' @param show_labels Whether to show labels
#' @param interactive Whether to create interactive plot
#' @param title Plot title
#' @return ggplot2 or plotly plot
#' @export
create_glass_brain_plot <- function(data, color_by = NULL, size_by = NULL, 
                                   palette = "viridis", view = "axial", 
                                   show_labels = FALSE, interactive = FALSE, 
                                   title = "Glass Brain View") {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for glass brain plots")
  }
  
  # Load brain outline data or create simplified outline
  # For a real glass brain, we would load MNI template outlines
  # Here we'll create a simplified outline for demonstration
  
  # Select coordinates based on view
  if (view == "axial") {
    x_coord <- "x"
    y_coord <- "y"
    subtitle <- "Axial View (Top)"
    
    # Create simplified brain outline
    brain_outline <- data.frame(
      x = c(-70, -80, -70, -40, 0, 40, 70, 80, 70, 0),
      y = c(0, 40, 80, 100, 110, 100, 80, 40, 0, -30)
    )
  } else if (view == "sagittal") {
    x_coord <- "y"
    y_coord <- "z"
    subtitle <- "Sagittal View (Side)"
    
    # Create simplified brain outline
    brain_outline <- data.frame(
      x = c(-100, -70, -30, 30, 70, 100, 80, 40, 0, -40),
      y = c(0, 20, 40, 50, 40, 20, -20, -40, -50, -20)
    )
  } else if (view == "coronal") {
    x_coord <- "x"
    y_coord <- "z"
    subtitle <- "Coronal View (Front)"
    
    # Create simplified brain outline
    brain_outline <- data.frame(
      x = c(-60, -40, -20, 0, 20, 40, 60, 70, 60, 40, 20, 0, -20, -40, -60, -70),
      y = c(0, 20, 30, 35, 30, 20, 0, -20, -40, -50, -55, -57, -55, -50, -40, -20)
    )
  } else {
    stop("Unknown view: ", view)
  }
  
  # Create glass brain plot
  p <- ggplot2::ggplot() +
    # Add brain outline
    ggplot2::geom_polygon(
      data = brain_outline,
      ggplot2::aes(x = x, y = y),
      fill = NA,
      color = "gray80",
      size = 1,
      alpha = 0.5
    ) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
  # Add regions as points
  if (!is.null(color_by) && color_by %in% names(data)) {
    p <- p + ggplot2::geom_point(
      data = data,
      ggplot2::aes_string(x = x_coord, y = y_coord, color = color_by, 
                        size = if (is.null(size_by)) NULL else size_by),
      alpha = 0.8
    )
    
    # Add color scale based on palette
    if (palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
      p <- p + viridis::scale_color_viridis()
    } else if (palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
      p <- p + ggplot2::scale_color_brewer(palette = "Set1")
    } else {
      p <- p + ggplot2::scale_color_gradient(low = "blue", high = "red")
    }
  } else {
    p <- p + ggplot2::geom_point(
      data = data,
      ggplot2::aes_string(x = x_coord, y = y_coord, 
                        size = if (is.null(size_by)) NULL else size_by),
      color = "dodgerblue",
      alpha = 0.8
    )
  }
  
  # Add region labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      data = data,
      ggplot2::aes_string(x = x_coord, y = y_coord, label = "Region"),
      size = 3,
      hjust = -0.2,
      vjust = 0.5,
      check_overlap = TRUE
    )
  }
  
  # Make interactive if requested
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create a 3D plot of brain regions
#' 
#' @param data Data frame with region information
#' @param color_by Variable to map to colors
#' @param size_by Variable to map to sizes
#' @param palette Color palette
#' @param show_labels Whether to show labels
#' @param interactive Whether to create interactive plot
#' @param title Plot title
#' @return plotly or rgl plot
#' @export
create_3d_brain_plot <- function(data, color_by = NULL, size_by = NULL, 
                                palette = "viridis", show_labels = FALSE, 
                                interactive = TRUE, title = "3D Brain Regions") {
  # 3D plot requires interactive mode
  interactive <- TRUE
  
  # Check if plotly is available
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' required for 3D brain plots")
  }
  
  # Prepare color mapping
  if (!is.null(color_by) && color_by %in% names(data)) {
    if (is.numeric(data[[color_by]])) {
      # For continuous variables, create a color scale
      color_values <- data[[color_by]]
      
      # Normalize to 0-1 range
      color_norm <- (color_values - min(color_values, na.rm = TRUE)) / 
                   (max(color_values, na.rm = TRUE) - min(color_values, na.rm = TRUE))
      
      # Create color palette
      if (palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
        colors <- viridis::viridis(100)[ceiling(color_norm * 99) + 1]
      } else if (palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
        pal <- RColorBrewer::brewer.pal(9, "YlOrRd")
        colors <- grDevices::colorRampPalette(pal)(100)[ceiling(color_norm * 99) + 1]
      } else {
        colors <- grDevices::colorRampPalette(c("blue", "red"))(100)[ceiling(color_norm * 99) + 1]
      }
    } else {
      # For categorical variables, use discrete colors
      categories <- unique(data[[color_by]])
      
      if (palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
        pal <- RColorBrewer::brewer.pal(min(9, length(categories)), "Set1")
        if (length(categories) > 9) {
          pal <- grDevices::colorRampPalette(pal)(length(categories))
        }
      } else {
        pal <- grDevices::rainbow(length(categories))
      }
      
      color_map <- setNames(pal, categories)
      colors <- color_map[data[[color_by]]]
    }
  } else {
    # Default color
    colors <- rep("dodgerblue", nrow(data))
  }
  
  # Prepare size mapping
  if (!is.null(size_by) && size_by %in% names(data)) {
    if (is.numeric(data[[size_by]])) {
      # Normalize to reasonable range (3-15)
      sizes <- 3 + 12 * (data[[size_by]] - min(data[[size_by]], na.rm = TRUE)) / 
              (max(data[[size_by]], na.rm = TRUE) - min(data[[size_by]], na.rm = TRUE))
    } else {
      # Default size
      sizes <- rep(8, nrow(data))
    }
  } else {
    # Default size
    sizes <- rep(8, nrow(data))
  }
  
  # Create 3D plot with plotly
  p <- plotly::plot_ly(
    data,
    x = ~x,
    y = ~y,
    z = ~z,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      color = colors,
      size = sizes,
      opacity = 0.8,
      line = list(width = 0.5, color = "#FFFFFF")
    ),
    text = ~Region,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = title,
      scene = list(
        xaxis = list(title = "X"),
        yaxis = list(title = "Y"),
        zaxis = list(title = "Z"),
        aspectmode = "data"
      )
    )
  
  # Add text labels if requested
  if (show_labels) {
    p <- p %>% plotly::add_trace(
      type = "scatter3d",
      mode = "text",
      text = ~Region,
      textposition = "top center",
      textfont = list(size = 10),
      showlegend = FALSE
    )
  }
  
  return(p)
}

#' Create a flatmap plot of brain regions
#' 
#' @param data Data frame with region information
#' @param color_by Variable to map to colors
#' @param size_by Variable to map to sizes
#' @param palette Color palette
#' @param show_labels Whether to show labels
#' @param interactive Whether to create interactive plot
#' @param title Plot title
#' @return ggplot2 or plotly plot
#' @export
create_flatmap_plot <- function(data, color_by = NULL, size_by = NULL, 
                               palette = "viridis", show_labels = FALSE, 
                               interactive = FALSE, title = "Brain Region Flatmap") {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for flatmap plots")
  }
  
  # Calculate 2D projection from 3D coordinates
  # We'll use a simple cylindrical projection based on angle and height
  # This is a simplification - real flatmaps use more complex projections
  
  # Calculate projection coordinates
  theta <- atan2(data$y, data$x)
  r <- sqrt(data$x^2 + data$y^2)
  
  # Create x-y coordinates for flatmap
  flatmap_x <- theta
  flatmap_y <- data$z
  
  # Create plot data
  plot_data <- cbind(data, flatmap_x = flatmap_x, flatmap_y = flatmap_y)
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = flatmap_x, y = flatmap_y)) +
    ggplot2::labs(title = title, x = "Angle", y = "Height") +
    ggplot2::theme_minimal()
  
  # Add color mapping if requested
  if (!is.null(color_by)) {
    if (color_by %in% names(plot_data)) {
      p <- p + ggplot2::aes_string(color = color_by)
      
      # Add color scale based on palette
      if (palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
        p <- p + viridis::scale_color_viridis()
      } else if (palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
        p <- p + ggplot2::scale_color_brewer(palette = "Set1")
      } else {
        p <- p + ggplot2::scale_color_gradient(low = "blue", high = "red")
      }
    } else {
      warning("color_by variable '", color_by, "' not found in data")
    }
  }
  
  # Add size mapping if requested
  if (!is.null(size_by)) {
    if (size_by %in% names(plot_data)) {
      p <- p + ggplot2::aes_string(size = size_by)
    } else {
      warning("size_by variable '", size_by, "' not found in data")
    }
  }
  
  # Add points
  p <- p + ggplot2::geom_point(alpha = 0.8)
  
  # Add region labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = Region),
      size = 3,
      hjust = -0.2,
      vjust = 0.5,
      check_overlap = TRUE
    )
  }
  
  # Make interactive if requested
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create a circle plot of brain regions
#' 
#' @param data Data frame with region information
#' @param color_by Variable to map to colors
#' @param size_by Variable to map to sizes
#' @param palette Color palette
#' @param show_labels Whether to show labels
#' @param interactive Whether to create interactive plot
#' @param title Plot title
#' @return ggplot2 or plotly plot
#' @export
create_circle_plot <- function(data, color_by = NULL, size_by = NULL, 
                              palette = "viridis", show_labels = FALSE, 
                              interactive = FALSE, title = "Brain Region Circle Plot") {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for circle plots")
  }
  
  # Arrange regions in a circle
  n_regions <- nrow(data)
  theta <- seq(0, 2 * pi, length.out = n_regions + 1)[-(n_regions + 1)]
  
  # Create circle coordinates
  circle_x <- cos(theta)
  circle_y <- sin(theta)
  
  # Create plot data
  plot_data <- cbind(data, circle_x = circle_x, circle_y = circle_y)
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = circle_x, y = circle_y)) +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
  # Add color mapping if requested
  if (!is.null(color_by)) {
    if (color_by %in% names(plot_data)) {
      p <- p + ggplot2::aes_string(color = color_by)
      
      # Add color scale based on palette
      if (palette == "viridis" && requireNamespace("viridis", quietly = TRUE)) {
        p <- p + viridis::scale_color_viridis()
      } else if (palette == "brewer" && requireNamespace("RColorBrewer", quietly = TRUE)) {
        p <- p + ggplot2::scale_color_brewer(palette = "Set1")
      } else {
        p <- p + ggplot2::scale_color_gradient(low = "blue", high = "red")
      }
    } else {
      warning("color_by variable '", color_by, "' not found in data")
    }
  }
  
  # Add size mapping if requested
  if (!is.null(size_by)) {
    if (size_by %in% names(plot_data)) {
      p <- p + ggplot2::aes_string(size = size_by)
    } else {
      warning("size_by variable '", size_by, "' not found in data")
    }
  }
  
  # Add points
  p <- p + ggplot2::geom_point(alpha = 0.8)
  
  # Add region labels if requested
  if (show_labels) {
    # Calculate label positions (slightly outside the circle)
    label_x <- 1.1 * circle_x
    label_y <- 1.1 * circle_y
    
    plot_data$label_x <- label_x
    plot_data$label_y <- label_y
    
    # Add labels
    p <- p + ggplot2::geom_text(
      data = plot_data,
      ggplot2::aes(x = label_x, y = label_y, label = Region),
      size = 3,
      hjust = ifelse(label_x < 0, 1, 0),
      vjust = 0.5
    )
  }
  
  # Make interactive if requested
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create a region value map for brain atlas
#' 
#' @param values Named vector or data frame with region values
#' @param atlas_data Atlas data (regions, coordinates, etc.)
#' @param options Visualization options
#' @return ggplot2 or plotly plot
#' @export
create_region_value_map <- function(values, atlas_data, options = NULL) {
  # This function would create a more detailed brain atlas visualization
  # with region values mapped to colors
  # For a complete implementation, we would need a real brain atlas template
  
  warning("create_region_value_map requires a brain atlas template. Using simplified visualization.")
  
  # Convert values to data frame if needed
  if (is.vector(values)) {
    if (is.null(names(values))) {
      stop("values must be a named vector if not a data frame")
    }
    
    values_df <- data.frame(
      Region = names(values),
      Value = values,
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(values)) {
    # Ensure Region column exists
    if (!"Region" %in% names(values)) {
      stop("values data frame must contain a 'Region' column")
    }
    
    values_df <- values
  } else {
    stop("values must be a named vector or data frame")
  }
  
  # For now, create a simple glass brain plot with values
  if (is.null(options)) {
    options <- list(
      type = "glass_brain",
      color_by = "Value",
      interactive = TRUE,
      title = "Region Value Map"
    )
  } else {
    options$type <- "glass_brain"
    options$color_by <- "Value"
  }
  
  # Merge with atlas data
  if (!is.null(atlas_data) && is.data.frame(atlas_data)) {
    # Check for required columns
    if (all(c("Region", "x", "y", "z") %in% names(atlas_data))) {
      # Use provided atlas data
      plot_data <- merge(atlas_data, values_df, by = "Region", all.x = TRUE)
    } else {
      warning("atlas_data missing required columns. Creating dummy coordinates.")
      plot_data <- create_dummy_coordinates(values_df)
    }
  } else {
    # Create dummy coordinates for demonstration
    plot_data <- create_dummy_coordinates(values_df)
  }
  
  # Create plot
  return(create_region_plot(plot_data, options = options))
}

#' Helper function to create dummy coordinates for regions
#' 
#' @param values_df Data frame with region values
#' @return Data frame with dummy coordinates
#' @export
create_dummy_coordinates <- function(values_df) {
  n_regions <- nrow(values_df)
  
  # Create circle coordinates
  theta <- seq(0, 2 * pi, length.out = n_regions + 1)[-(n_regions + 1)]
  r <- runif(n_regions, 0.5, 1)
  
  # Create 3D coordinates
  x <- r * cos(theta) * 50
  y <- r * sin(theta) * 50
  z <- rnorm(n_regions, 0, 20)
  
  # Create data frame
  coords_df <- data.frame(
    Region = values_df$Region,
    x = x,
    y = y,
    z = z,
    stringsAsFactors = FALSE
  )
  
  # Merge with values
  result <- merge(coords_df, values_df, by = "Region")
  
  return(result)
}
