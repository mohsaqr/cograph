#' Multilayer Network Heatmap
#'
#' Visualizes multiple network layers as heatmaps on tilted 3D-perspective planes,
#' similar to the plot_mlna network visualization style.
#'
#' @param x A list of matrices (one per layer), a group_tna object, or a single
#'   matrix with layer_list specified.
#' @param layer_list Optional list defining layers (for single matrix input).
#' @param colors Color palette: "viridis", "heat", "blues", "reds", "inferno",
#'   "plasma", or a vector of colors. Default "viridis".
#' @param layer_spacing Vertical spacing between layers. Default 2.5.
#' @param skew Horizontal skew for perspective effect (0-1). Default 0.4.
#' @param compress Vertical compression for perspective (0-1). Default 0.6.
#' @param show_connections Show inter-layer connection lines? Default FALSE.
#' @param connection_color Color for inter-layer connections. Default "#E63946".
#' @param connection_style Line style: "dashed", "solid", "dotted". Default "dashed".
#' @param show_borders Show layer outline borders? Default TRUE.
#' @param border_color Color for layer borders. Default "black".
#' @param border_width Width of layer borders. Default 1.
#' @param cell_border_color Color for cell borders. Default "white".
#' @param cell_border_width Width of cell borders. Default 0.2.
#' @param show_labels Show layer name labels? Default TRUE.
#' @param label_size Size of layer labels. Default 5.
#' @param show_legend Show color legend? Default TRUE.
#' @param legend_title Title for legend. Default "Weight".
#' @param title Plot title. Default NULL.
#' @param limits Color scale limits c(min, max). NULL for auto.
#' @param na_color Color for NA values. Default "grey90".
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun
#' # List of matrices
#' layers <- list(
#'   Layer1 = matrix(runif(16), 4, 4),
#'   Layer2 = matrix(runif(16), 4, 4),
#'   Layer3 = matrix(runif(16), 4, 4)
#' )
#' plot_ml_heatmap(layers)
#'
#' # With connections
#' plot_ml_heatmap(layers, show_connections = TRUE)
#'
#' # Custom styling
#' plot_ml_heatmap(layers,
#'   colors = "plasma",
#'   layer_spacing = 3,
#'   skew = 0.5
#' )
#' }
#'
#' @export
plot_ml_heatmap <- function(
    x,
    layer_list = NULL,
    colors = "viridis",
    layer_spacing = 2.5,
    skew = 0.4,
    compress = 0.6,
    show_connections = FALSE,
    connection_color = "#E63946",
    connection_style = "dashed",
    show_borders = TRUE,
    border_color = "black",
    border_width = 1,
    cell_border_color = "white",
    cell_border_width = 0.2,
    show_labels = TRUE,
    label_size = 5,
    show_legend = TRUE,
    legend_title = "Weight",
    title = NULL,
    limits = NULL,
    na_color = "grey90"
) {

 if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required. Install with: install.packages('ggplot2')")
  }

  # Extract layers from input
  layers <- .extract_ml_layers(x, layer_list)
  n_layers <- length(layers)
  layer_names <- names(layers) %||% paste0("Layer ", seq_len(n_layers))
  names(layers) <- layer_names

  # Get dimensions
  n_rows <- nrow(layers[[1]])
  n_cols <- ncol(layers[[1]])

  # Build cell polygons
  cells_df <- .build_ml_cells(layers, skew, compress, layer_spacing)

  # Build layer shells
  shells_df <- .build_ml_shells(layers, n_rows, n_cols, skew, compress, layer_spacing)

  # Build labels
  labels_df <- .build_ml_labels(layers, n_rows, skew, compress, layer_spacing)

  # Resolve colors
  color_scale <- .resolve_ml_colors(colors)

  # Start plot
  p <- ggplot2::ggplot()

  # Add inter-layer connections if requested
 if (show_connections) {
    conn_df <- .build_ml_connections(layers, n_rows, n_cols, skew, compress, layer_spacing)
    lty <- switch(connection_style,
      "dashed" = "dashed",
      "dotted" = "dotted",
      "solid"
    )
    p <- p + ggplot2::geom_line(
      data = conn_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
      color = connection_color, linewidth = 0.8, linetype = lty, alpha = 0.7
    )
  }

  # Add cells
  p <- p + ggplot2::geom_polygon(
    data = cells_df,
    ggplot2::aes(x = .data$x, y = .data$y, fill = .data$weight, group = .data$cell_id),
    color = cell_border_color, linewidth = cell_border_width
  )

  # Add layer borders
  if (show_borders) {
    p <- p + ggplot2::geom_path(
      data = shells_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$layer),
      color = border_color, linewidth = border_width
    )
  }

  # Add layer labels
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      data = labels_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      fontface = "bold", size = label_size, hjust = 1
    )
  }

  # Color scale
  p <- p + ggplot2::scale_fill_gradientn(
    colors = color_scale,
    limits = limits,
    na.value = na_color,
    name = legend_title
  )

  # Theme
  p <- p +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = if (show_legend) "right" else "none"
    )

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  p
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Extract layers from various input types
#' @keywords internal
.extract_ml_layers <- function(x, layer_list) {
  if (is.list(x) && !inherits(x, "group_tna") && all(sapply(x, is.matrix))) {
    # Already a list of matrices
    return(x)
  }

  if (inherits(x, "group_tna")) {
    # Extract from group_tna
    return(lapply(x, .extract_weights))
  }

  if (is.matrix(x) && !is.null(layer_list)) {
    # Single matrix with layer_list - extract submatrices
    return(lapply(layer_list, function(nodes) {
      x[nodes, nodes, drop = FALSE]
    }))
  }

  stop("x must be a list of matrices, a group_tna object, or a matrix with layer_list")
}


#' Transform coordinates to perspective plane
#' @keywords internal
.transform_to_plane <- function(x, y, layer_idx, n_layers, skew, compress, layer_spacing) {
  y_offset <- (n_layers - layer_idx) * layer_spacing
  x_new <- x + y * skew
 y_new <- y * compress + y_offset
  list(x = x_new, y = y_new)
}


#' Build cell polygon data
#' @keywords internal
.build_ml_cells <- function(layers, skew, compress, layer_spacing) {
  n_layers <- length(layers)
  cell_list <- list()

  for (li in seq_along(layers)) {
    m <- layers[[li]]
    layer_name <- names(layers)[li]
    n_rows <- nrow(m)
    n_cols <- ncol(m)

    for (row in seq_len(n_rows)) {
      for (col in seq_len(n_cols)) {
        corners_x <- c(col - 1, col, col, col - 1)
        corners_y <- c(row - 1, row - 1, row, row)

        transformed <- .transform_to_plane(
          corners_x, corners_y, li, n_layers, skew, compress, layer_spacing
        )

        cell_list[[length(cell_list) + 1]] <- data.frame(
          x = transformed$x,
          y = transformed$y,
          weight = m[row, col],
          layer = layer_name,
          layer_idx = li,
          cell_id = paste(layer_name, row, col, sep = "_")
        )
      }
    }
  }

  df <- do.call(rbind, cell_list)
  df$layer <- factor(df$layer, levels = rev(names(layers)))
  df
}


#' Build layer shell outlines
#' @keywords internal
.build_ml_shells <- function(layers, n_rows, n_cols, skew, compress, layer_spacing) {
  n_layers <- length(layers)

  shell_list <- lapply(seq_along(layers), function(li) {
    corners_x <- c(0, n_cols, n_cols, 0, 0)
    corners_y <- c(0, 0, n_rows, n_rows, 0)
    transformed <- .transform_to_plane(
      corners_x, corners_y, li, n_layers, skew, compress, layer_spacing
    )
    data.frame(
      x = transformed$x,
      y = transformed$y,
      layer = names(layers)[li],
      layer_idx = li
    )
  })

  do.call(rbind, shell_list)
}


#' Build layer label positions
#' @keywords internal
.build_ml_labels <- function(layers, n_rows, skew, compress, layer_spacing) {
  n_layers <- length(layers)

  label_list <- lapply(seq_along(layers), function(li) {
    transformed <- .transform_to_plane(
      -0.8, n_rows / 2, li, n_layers, skew, compress, layer_spacing
    )
    data.frame(
      x = transformed$x,
      y = transformed$y,
      label = names(layers)[li]
    )
  })

  do.call(rbind, label_list)
}


#' Build inter-layer connection lines
#' @keywords internal
.build_ml_connections <- function(layers, n_rows, n_cols, skew, compress, layer_spacing) {
  n_layers <- length(layers)
  conn_list <- list()

  # Connect diagonal cells (identity coupling) between adjacent layers
  n_connect <- min(n_rows, n_cols)

  for (node_idx in seq_len(n_connect)) {
    for (li in seq_len(n_layers - 1)) {
      x_center <- node_idx - 0.5
      y_center <- node_idx - 0.5

      from <- .transform_to_plane(
        x_center, y_center, li, n_layers, skew, compress, layer_spacing
      )
      to <- .transform_to_plane(
        x_center, y_center, li + 1, n_layers, skew, compress, layer_spacing
      )

      conn_list[[length(conn_list) + 1]] <- data.frame(
        x = c(from$x, to$x),
        y = c(from$y, to$y),
        group = paste("conn", li, node_idx, sep = "_")
      )
    }
  }

  do.call(rbind, conn_list)
}


#' Resolve color palette
#' @keywords internal
.resolve_ml_colors <- function(colors) {
  if (length(colors) == 1 && is.character(colors)) {
    switch(colors,
      "viridis" = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
      "inferno" = c("#000004", "#420A68", "#932667", "#DD513A", "#FCA50A", "#FCFFA4"),
      "plasma" = c("#0D0887", "#6A00A8", "#B12A90", "#E16462", "#FCA636", "#F0F921"),
      "heat" = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026"),
      "blues" = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C"),
      "reds" = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15"),
      # Default
      c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725")
    )
  } else {
    colors
  }
}
