#' Multilayer Network Heatmap
#'
#' Visualizes multiple network layers as heatmaps on tilted 3D-perspective planes,
#' similar to the plot_mlna network visualization style.
#'
#' @param x A list of matrices (one per layer), a group_tna object,
#'   cograph_network, or a single matrix with layer_list specified.
#' @param layer_list Optional list defining layers, column name string,
#'   or NULL for auto-detection from cograph_network nodes.
#' @param colors Color palette: "viridis", "heat", "blues", "reds", "inferno",
#'   "plasma", or a vector of colors. Default "viridis".
#' @param layer_spacing Vertical spacing between layers, in data units. A
#'   plane is \code{nrow(x) * compress} units tall, so a fixed spacing that
#'   suits a small network makes a larger one overlap itself. \code{NULL}
#'   (the default) scales the spacing to the plane so planes never collide;
#'   pass a number for the older absolute behaviour.
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
#' @param show_node_labels Show the row and column names of the matrix?
#'   Default TRUE. Without them a plane is an anonymous grid and a reader
#'   cannot tell which cell is which pair. Every plane shares one node
#'   ordering, so the names are drawn once, against the front plane: rows down
#'   its left edge, columns along its lower edge.
#' @param node_label_size Size of the row and column names. Default 3.
#' @param label_size Size of layer labels. Default 5.
#' @param show_legend Show color legend? Default TRUE.
#' @param legend_title Title for legend. Default "Weight".
#' @param title Plot title. Default NULL.
#' @param limits Color scale limits c(min, max). NULL for auto.
#' @param na_color Color for NA values. Default "grey90".
#' @param threshold Minimum absolute value to display. Cells with
#'   \code{abs(value) < threshold} are set to NA (rendered as background).
#'   Default 0.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' set.seed(1)
#' layers <- list(
#'   L1 = matrix(runif(16), 4, 4),
#'   L2 = matrix(runif(16), 4, 4),
#'   L3 = matrix(runif(16), 4, 4))
#' plot_ml_heatmap(layers)
#' plot_ml_heatmap(layers, show_connections = TRUE, colors = "plasma")
#'
#' @export
plot_ml_heatmap <- function(
    x,
    layer_list = NULL,
    colors = "viridis",
    layer_spacing = NULL,
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
    show_node_labels = TRUE,
    node_label_size = 3,
    label_size = 5,
    show_legend = TRUE,
    legend_title = "Weight",
    title = NULL,
    limits = NULL,
    na_color = "grey90",
    threshold = 0
) {

 if (!requireNamespace("ggplot2", quietly = TRUE)) { # nocov start
    stop("Package 'ggplot2' required. Install with: install.packages('ggplot2')")
  } # nocov end

  # Extract layers from input
  layers <- .extract_ml_layers(x, layer_list)
  n_layers <- length(layers)
  layer_names <- names(layers) %||% paste0("Layer ", seq_len(n_layers))
  names(layers) <- layer_names

  # Apply threshold: set cells below threshold to NA
  if (threshold > 0) {
    layers <- lapply(layers, function(m) {
      m[abs(m) < threshold] <- NA
      m
    })
  }

  # Get dimensions
  n_rows <- nrow(layers[[1]])
  layer_spacing <- .ml_layer_spacing(layer_spacing, n_rows, compress)
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

  # Add the row and column names, against the front plane only
  if (show_node_labels) {
    node_labels_df <- .build_ml_node_labels(
      layers, n_rows, n_cols, skew, compress, layer_spacing)
    p <- p + ggplot2::geom_text(
      data = node_labels_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                   hjust = .data$hjust, vjust = .data$vjust,
                   angle = .data$angle),
      size = node_label_size, colour = "grey20"
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
  #
  # Layer labels are right-aligned text anchored inside the panel, and text
  # extent takes no part in scale expansion: the room to the left of the
  # anchor is a fixed fraction of the data span, so a long name or a narrow
  # panel clipped it ("Layer 1" rendered as "/er 1"). Drawing with clipping
  # off and reserving the label's own width as a left margin makes the label
  # legible at any device size, because the margin is in absolute units and
  # so is the text.
  p <- p +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = if (show_legend) "right" else "none",
      plot.margin = ggplot2::margin(
        t = 5.5, r = 5.5,
        l = max(
          if (show_labels) .ml_label_margin(names(layers), label_size) else 5.5,
          if (show_node_labels)
            .ml_label_margin(rownames(layers[[1]]), node_label_size) else 5.5
        ),
        # Column names run below the front plane and need room of their own.
        b = if (show_node_labels)
          .ml_label_margin(colnames(layers[[1]]), node_label_size) else 5.5,
        unit = "pt"
      )
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
#' @noRd
.extract_ml_layers <- function(x, layer_list) {
  if (is.list(x) && !inherits(x, "group_tna") && all(vapply(x, is.matrix, logical(1)))) {
    # Already a list of matrices
    return(x)
  }

  if (inherits(x, "group_tna")) {
    # Extract from group_tna
    return(lapply(x, .extract_weights))
  }

  # Handle cograph_network
  if (inherits(x, "cograph_network")) {
    mat <- to_matrix(x)
    nodes_df <- x$nodes
    lab <- nodes_df$label

    # Handle layer_list: column name string or auto-detect
    if (is.character(layer_list) && length(layer_list) == 1 &&
        layer_list %in% names(nodes_df)) {
      layer_col <- nodes_df[[layer_list]]
      layer_list <- split(lab, layer_col)
      message("Using '", layer_list, "' column for layers")
    } else if (is.null(layer_list)) {
      # Auto-detect from common column names
      layer_cols <- c("layers", "layer", "level", "levels")
      for (col in layer_cols) {
        if (col %in% names(nodes_df)) {
          layer_col <- nodes_df[[col]]
          layer_list <- split(lab, layer_col)
          message("Using '", col, "' column for layers")
          break
        }
      }
    }

    if (is.null(layer_list)) {
      stop("layer_list required: provide a list, column name, or add a ",
           "'layers'/'level' column to nodes")
    }

    return(lapply(layer_list, function(nodes) {
      mat[nodes, nodes, drop = FALSE]
    }))
  }

  if (is.matrix(x) && !is.null(layer_list)) {
    # Single matrix with layer_list - extract submatrices
    return(lapply(layer_list, function(nodes) {
      x[nodes, nodes, drop = FALSE]
    }))
  }

  stop("x must be a list of matrices, a group_tna object, cograph_network, or a matrix with layer_list")
}


#' Transform coordinates to perspective plane
#' @keywords internal
#' @noRd
.transform_to_plane <- function(x, y, layer_idx, n_layers, skew, compress, layer_spacing) {
  y_offset <- (n_layers - layer_idx) * layer_spacing
  x_new <- x + y * skew
 y_new <- y * compress + y_offset
  list(x = x_new, y = y_new)
}


#' Build cell polygon data
#' @keywords internal
#' @noRd
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
#' @noRd
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


#' Row and column names for the front plane
#'
#' Every plane in the stack shares one node ordering, so drawing the names on
#' each would only repeat them and crowd the picture. They go once against the
#' front plane, which is the last layer: `.transform_to_plane()` gives that
#' layer a zero offset, so it sits at the bottom of the stack and nothing
#' overlaps its lower or left edge.
#'
#' Row names sit left of column one at the row's mid-height; column names sit
#' below row one at the column's mid-width, rotated so that long names do not
#' collide with their neighbours. The `skew` shear is already applied by the
#' transform, so both sets follow the plane's edges rather than the page.
#'
#' @param layers The list of layer matrices.
#' @param n_rows,n_cols Dimensions of a layer.
#' @param skew,compress,layer_spacing Projection parameters.
#' @return A data frame of label positions with text justification and angle.
#' @keywords internal
#' @noRd
.build_ml_node_labels <- function(layers, n_rows, n_cols, skew, compress,
                                  layer_spacing) {
  n_layers <- length(layers)
  front <- n_layers
  row_names <- rownames(layers[[1]]) %||% as.character(seq_len(n_rows))
  col_names <- colnames(layers[[1]]) %||% as.character(seq_len(n_cols))

  rows <- .transform_to_plane(-0.25, seq_len(n_rows) - 0.5, front, n_layers,
                              skew, compress, layer_spacing)
  cols <- .transform_to_plane(seq_len(n_cols) - 0.5, -0.25, front, n_layers,
                              skew, compress, layer_spacing)

  rbind(
    data.frame(x = rows$x, y = rows$y, label = row_names,
               hjust = 1, vjust = 0.5, angle = 0),
    data.frame(x = cols$x, y = cols$y, label = col_names,
               hjust = 1, vjust = 0.5, angle = 45)
  )
}

#' Left margin needed to keep layer labels legible
#'
#' Layer names are drawn as right-aligned text just inside the panel, and
#' `ggplot2` does not expand a scale to fit text. The margin reserved here is
#' the label's own estimated width, so a long name stays readable however
#' narrow the panel becomes. The 0.5 factor is the mean advance width of a
#' bold sans glyph relative to its point size; it reproduces the measured
#' width of "Layer 1" at `label_size = 5` to within a percent.
#'
#' @param labels Character vector of layer names.
#' @param label_size Text size, in the units `ggplot2::geom_text()` uses.
#' @return A single number: the margin in points, never below the default.
#' @keywords internal
#' @noRd
.ml_label_margin <- function(labels, label_size) {
  widest <- if (length(labels)) max(nchar(labels), 0L) else 0L
  if (!is.finite(widest)) widest <- 0L
  points_per_size <- 72.27 / 25.4
  # The default 5.5pt margin is kept as padding on top of the estimate, so a
  # font whose glyphs run slightly wider than the average still fits.
  max(5.5, widest * 0.5 * label_size * points_per_size + 5.5)
}

#' Vertical spacing that keeps stacked planes apart
#'
#' A plane drawn by [plot_ml_heatmap()] is `n_rows * compress` data units
#' tall, so an absolute spacing cannot suit both a four-node and a
#' fourteen-node network: at the old fixed default of 2.5 a fourteen-node
#' stack overlapped itself by 5.9 units. Scaling the spacing to the plane
#' leaves a tenth of a plane's height as a visible gap.
#'
#' @param layer_spacing The user's value, or `NULL` to derive one.
#' @param n_rows Rows in a layer matrix.
#' @param compress Vertical compression of the perspective.
#' @return A single positive number.
#' @keywords internal
#' @noRd
.ml_layer_spacing <- function(layer_spacing, n_rows, compress) {
  if (!is.null(layer_spacing)) {
    stopifnot(
      "`layer_spacing` must be one positive number or NULL." =
        length(layer_spacing) == 1L && is.numeric(layer_spacing) &&
        is.finite(layer_spacing) && layer_spacing > 0
    )
    return(layer_spacing)
  }
  max(1, n_rows * compress * 1.1)
}

#' Build layer label positions
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
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
