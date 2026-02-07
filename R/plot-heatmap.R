#' @title Network Heatmap Plots
#' @description Visualize networks as heatmaps with support for multi-cluster
#'   and multi-layer structures.
#' @name plot-heatmap
NULL

#' Plot Network as Heatmap
#'
#' Visualizes a network adjacency/weight matrix as a heatmap. Supports single
#' networks, multi-cluster networks (block diagonal), and multi-layer networks
#' (group_tna).
#'
#' @param x Network input: matrix, CographNetwork, tna, igraph, group_tna,
#'   or any object cograph accepts.
#' @param cluster_list Optional list of character vectors defining node clusters.
#'   Creates a block-structured heatmap with clusters along diagonal.
#' @param cluster_spacing Gap size between clusters (in cell units). Default 0.
#' @param show_legend Logical: display color legend? Default TRUE.
#' @param legend_position Position: "right" (default), "left", "top", "bottom", "none".
#' @param legend_title Title for legend. Default "Weight".
#' @param colors Color palette: vector of colors for gradient, or a palette name
#'   ("viridis", "heat", "blues", "reds", "diverging"). Default "viridis".
#' @param limits Numeric vector c(min, max) for color scale. NULL for auto.
#' @param midpoint Midpoint for diverging scales. NULL for auto (0 if data spans neg/pos).
#' @param na_color Color for NA values. Default "grey90".
#' @param show_values Logical: display values in cells? Default FALSE.
#' @param value_size Text size for cell values. Default 2.5.
#' @param value_color Color for cell value text. Default "black".
#' @param value_digits Decimal places for values. Default 2.
#' @param show_diagonal Logical: show diagonal values? Default TRUE.
#' @param diagonal_color Optional color for diagonal cells. NULL uses scale.
#' @param cluster_labels Logical: show cluster/layer labels? Default TRUE.
#' @param cluster_borders Logical: draw borders around clusters? Default TRUE.
#' @param border_color Color for cluster borders. Default "black".
#' @param border_width Width of cluster borders. Default 0.5.
#' @param row_labels Row labels. NULL for auto (rownames or indices).
#' @param col_labels Column labels. NULL for auto (colnames or indices).
#' @param show_axis_labels Logical: show axis tick labels? Default TRUE.
#' @param axis_text_size Size of axis labels. Default 8.
#' @param axis_text_angle Angle for x-axis labels. Default 45.
#' @param title Plot title. Default NULL.
#' @param subtitle Plot subtitle. Default NULL.
#' @param xlab X-axis label. Default NULL.
#' @param ylab Y-axis label. Default NULL.
#' @param aspect_ratio Aspect ratio. Default 1 (square cells).
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2 object.
#'
#' @details
#' For multi-cluster networks, provide \code{cluster_list} as a named list where
#' each element is a vector of node names belonging to that cluster. The heatmap
#' will be reordered to show clusters as blocks along the diagonal.
#'
#' For group_tna objects (multiple separate networks), each network becomes a
#' diagonal block. Off-diagonal blocks are empty (no inter-layer edges) unless
#' the networks share nodes.
#'
#' @examples
#' \dontrun{
#' # Single network
#' m <- matrix(runif(25), 5, 5)
#' rownames(m) <- colnames(m) <- LETTERS[1:5]
#' plot_heatmap(m)
#'
#' # With clusters
#' clusters <- list(Group1 = c("A", "B"), Group2 = c("C", "D", "E"))
#' plot_heatmap(m, cluster_list = clusters)
#'
#' # Multi-layer (group_tna)
#' plot_heatmap(group_tna_model)
#'
#' # Custom colors and legend
#' plot_heatmap(m, colors = "heat", limits = c(0, 1), show_values = TRUE)
#' }
#'
#' @export
plot_heatmap <- function(x,
                         cluster_list = NULL,
                         cluster_spacing = 0,
                         show_legend = TRUE,
                         legend_position = "right",
                         legend_title = "Weight",
                         colors = "viridis",
                         limits = NULL,
                         midpoint = NULL,
                         na_color = "grey90",
                         show_values = FALSE,
                         value_size = 2.5,
                         value_color = "black",
                         value_digits = 2,
                         show_diagonal = TRUE,
                         diagonal_color = NULL,
                         cluster_labels = TRUE,
                         cluster_borders = TRUE,
                         border_color = "black",
                         border_width = 0.5,
                         row_labels = NULL,
                         col_labels = NULL,
                         show_axis_labels = TRUE,
                         axis_text_size = 8,
                         axis_text_angle = 45,
                         title = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         aspect_ratio = 1,
                         ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required. Install with: install.packages('ggplot2')")
  }

  # Handle group_tna → supra-adjacency block matrix

  if (inherits(x, "group_tna")) {
    return(.plot_heatmap_group_tna(
      x, show_legend, legend_position, legend_title, colors, limits, midpoint,
      na_color, show_values, value_size, value_color, value_digits,
      show_diagonal, diagonal_color, cluster_labels, cluster_borders,
      border_color, border_width, show_axis_labels, axis_text_size,
      axis_text_angle, title, subtitle, xlab, ylab, aspect_ratio
    ))
  }

  # Extract weight matrix
  mat <- .extract_weights(x)

  # Handle cluster_list → reorder and create block structure
  if (!is.null(cluster_list)) {
    return(.plot_heatmap_clustered(
      mat, cluster_list, cluster_spacing, show_legend, legend_position,
      legend_title, colors, limits, midpoint, na_color, show_values,
      value_size, value_color, value_digits, show_diagonal, diagonal_color,
      cluster_labels, cluster_borders, border_color, border_width,
      show_axis_labels, axis_text_size, axis_text_angle, title, subtitle,
      xlab, ylab, aspect_ratio
    ))
  }

  # Single network heatmap
  .plot_heatmap_single(
    mat, show_legend, legend_position, legend_title, colors, limits, midpoint,
    na_color, show_values, value_size, value_color, value_digits, show_diagonal,
    diagonal_color, row_labels, col_labels, show_axis_labels, axis_text_size,
    axis_text_angle, title, subtitle, xlab, ylab, aspect_ratio
  )
}


#' Plot Single Network Heatmap
#' @keywords internal
.plot_heatmap_single <- function(mat, show_legend, legend_position, legend_title,
                                  colors, limits, midpoint, na_color, show_values,
                                  value_size, value_color, value_digits, show_diagonal,
                                  diagonal_color, row_labels, col_labels, show_axis_labels,
                                  axis_text_size, axis_text_angle, title, subtitle,
                                  xlab, ylab, aspect_ratio) {

  # Get labels
  rn <- row_labels %||% rownames(mat) %||% seq_len(nrow(mat))
  cn <- col_labels %||% colnames(mat) %||% seq_len(ncol(mat))

  # Optionally mask diagonal
  if (!show_diagonal) {
    diag(mat) <- NA
  }

  # Convert to long format
  df <- .matrix_to_long(mat, rn, cn)

  # Build base plot
  p <- .build_heatmap_base(df, colors, limits, midpoint, na_color, legend_title,
                           show_legend, legend_position)

  # Add values if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = ifelse(is.na(.data$value), "",
                                  round(.data$value, value_digits))),
      size = value_size, color = value_color
    )
  }

  # Add labels and theme
  th <- .heatmap_theme(show_axis_labels, axis_text_size, axis_text_angle, aspect_ratio)
  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    th$theme

  if (!is.null(th$aspect_ratio)) {
    p <- p + ggplot2::coord_fixed(ratio = th$aspect_ratio)
  }

  p
}


#' Plot Clustered Heatmap
#' @keywords internal
.plot_heatmap_clustered <- function(mat, cluster_list, cluster_spacing, show_legend,
                                     legend_position, legend_title, colors, limits,
                                     midpoint, na_color, show_values, value_size,
                                     value_color, value_digits, show_diagonal,
                                     diagonal_color, cluster_labels, cluster_borders,
                                     border_color, border_width, show_axis_labels,
                                     axis_text_size, axis_text_angle, title, subtitle,
                                     xlab, ylab, aspect_ratio) {

  # Reorder matrix by clusters
  node_order <- unlist(cluster_list)
  missing <- setdiff(node_order, rownames(mat))
  if (length(missing) > 0) {
    stop("Nodes not found in matrix: ", paste(missing, collapse = ", "))
  }

  mat <- mat[node_order, node_order]
  n_total <- length(node_order)
  n_clusters <- length(cluster_list)

  # Get labels
  rn <- rownames(mat)
  cn <- colnames(mat)

  # Optionally mask diagonal
  if (!show_diagonal) {
    diag(mat) <- NA
  }

  # Calculate cluster positions with spacing
  cluster_sizes <- sapply(cluster_list, length)
  cluster_starts <- c(0, cumsum(cluster_sizes[-n_clusters]))
  cluster_offsets <- seq_along(cluster_list) - 1  # Gap multiplier

  # Build data frame with spacing offsets
  df_list <- list()
  for (ci in seq_along(cluster_list)) {
    for (cj in seq_along(cluster_list)) {
      nodes_i <- cluster_list[[ci]]
      nodes_j <- cluster_list[[cj]]

      for (i in seq_along(nodes_i)) {
        for (j in seq_along(nodes_j)) {
          row_idx <- cluster_starts[ci] + i
          col_idx <- cluster_starts[cj] + j

          # Apply spacing offset
          x_pos <- col_idx + cluster_offsets[cj] * cluster_spacing
          y_pos <- (n_total - row_idx + 1) + (n_clusters - ci) * cluster_spacing

          df_list[[length(df_list) + 1]] <- data.frame(
            row = nodes_i[i],
            col = nodes_j[j],
            x = x_pos,
            y = y_pos,
            value = mat[nodes_i[i], nodes_j[j]],
            cluster_row = ci,
            cluster_col = cj
          )
        }
      }
    }
  }
  df <- do.call(rbind, df_list)

  # Build plot with explicit x/y positions
  color_vec <- .resolve_colors(colors, df$value, limits, midpoint)
  vals <- df$value[!is.na(df$value)]
  is_diverging <- !is.null(midpoint) || (length(vals) > 0 && min(vals) < 0 && max(vals) > 0)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1)

  # Add color scale
  if (is_diverging) {
    mid <- midpoint %||% 0
    p <- p + ggplot2::scale_fill_gradient2(
      low = color_vec[1], mid = color_vec[2], high = color_vec[3],
      midpoint = mid, limits = limits, na.value = na_color, name = legend_title
    )
  } else {
    p <- p + ggplot2::scale_fill_gradientn(
      colors = color_vec, limits = limits, na.value = na_color, name = legend_title
    )
  }

  # Legend
  if (!show_legend || legend_position == "none") {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(legend.position = legend_position)
  }

  # Add cluster borders with spacing
  if (cluster_borders) {
    borders <- data.frame(xmin = numeric(), xmax = numeric(),
                          ymin = numeric(), ymax = numeric())
    for (ci in seq_along(cluster_list)) {
      n_cluster <- cluster_sizes[ci]
      x_start <- cluster_starts[ci] + cluster_offsets[ci] * cluster_spacing + 0.5
      x_end <- x_start + n_cluster
      y_start <- (n_total - cluster_starts[ci] - n_cluster) +
                 (n_clusters - ci) * cluster_spacing + 0.5
      y_end <- y_start + n_cluster

      borders <- rbind(borders, data.frame(
        xmin = x_start, xmax = x_end,
        ymin = y_start, ymax = y_end
      ))
    }

    p <- p + ggplot2::geom_rect(
      data = borders,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ymin, ymax = .data$ymax),
      fill = NA, color = border_color, linewidth = border_width,
      inherit.aes = FALSE
    )
  }

  # Add cluster labels with spacing
  if (cluster_labels && !is.null(names(cluster_list))) {
    label_data <- data.frame(
      x = numeric(), y = numeric(), label = character()
    )
    total_height <- n_total + (n_clusters - 1) * cluster_spacing

    for (ci in seq_along(cluster_list)) {
      n_cluster <- cluster_sizes[ci]
      x_center <- cluster_starts[ci] + n_cluster / 2 +
                  cluster_offsets[ci] * cluster_spacing + 0.5
      y_center <- (n_total - cluster_starts[ci] - n_cluster / 2) +
                  (n_clusters - ci) * cluster_spacing + 0.5

      label_data <- rbind(label_data, data.frame(
        x = x_center,
        y = y_center,
        label = names(cluster_list)[ci]
      ))
    }

    # Top labels
    p <- p + ggplot2::annotate("text",
      x = label_data$x, y = total_height + 1,
      label = label_data$label, size = 3, fontface = "bold"
    )
    # Left labels
    p <- p + ggplot2::annotate("text",
      x = -0.3, y = label_data$y,
      label = label_data$label, size = 3, fontface = "bold",
      angle = 90, hjust = 0.5
    )
  }

  # Add values if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = ifelse(is.na(.data$value), "",
                                  round(.data$value, value_digits))),
      size = value_size, color = value_color
    )
  }

  # Theme
  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::coord_fixed(ratio = aspect_ratio)

  p
}


#' Plot Group TNA as Supra-Adjacency Heatmap
#' @keywords internal
.plot_heatmap_group_tna <- function(x, show_legend, legend_position, legend_title,
                                     colors, limits, midpoint, na_color, show_values,
                                     value_size, value_color, value_digits, show_diagonal,
                                     diagonal_color, cluster_labels, cluster_borders,
                                     border_color, border_width, show_axis_labels,
                                     axis_text_size, axis_text_angle, title, subtitle,
                                     xlab, ylab, aspect_ratio) {

  # Extract all weight matrices
  n_groups <- length(x)
  group_names <- names(x) %||% paste0("Layer", seq_len(n_groups))
  matrices <- lapply(x, .extract_weights)

  # Get node labels from first matrix (assuming all have same structure)
  node_labels <- rownames(matrices[[1]])
  if (is.null(node_labels)) {
    node_labels <- seq_len(nrow(matrices[[1]]))
  }
  n_nodes <- length(node_labels)

  # Build supra-adjacency matrix
  supra_size <- n_nodes * n_groups
  supra_mat <- matrix(NA, nrow = supra_size, ncol = supra_size)

  # Fill diagonal blocks
  for (i in seq_len(n_groups)) {
    row_start <- (i - 1) * n_nodes + 1
    row_end <- i * n_nodes
    supra_mat[row_start:row_end, row_start:row_end] <- matrices[[i]]
  }

  # Create combined labels
  supra_labels <- paste0(rep(group_names, each = n_nodes), ":", rep(node_labels, n_groups))
  rownames(supra_mat) <- colnames(supra_mat) <- supra_labels

  # Create cluster_list for borders
  cluster_list <- setNames(
    lapply(seq_len(n_groups), function(i) {
      idx <- ((i - 1) * n_nodes + 1):(i * n_nodes)
      supra_labels[idx]
    }),
    group_names
  )

  # Optionally mask diagonal
  if (!show_diagonal) {
    diag(supra_mat) <- NA
  }

  # Convert to long format
  df <- .matrix_to_long(supra_mat, supra_labels, supra_labels)

  # Build base plot
  p <- .build_heatmap_base(df, colors, limits, midpoint, na_color, legend_title,
                           show_legend, legend_position)

  # Add cluster borders
  if (cluster_borders) {
    p <- .add_cluster_borders(p, cluster_list, supra_labels, border_color, border_width)
  }

  # Add cluster labels (layer names)
  if (cluster_labels) {
    p <- .add_cluster_labels(p, cluster_list, supra_labels)
  }

  # Add values if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = ifelse(is.na(.data$value), "",
                                  round(.data$value, value_digits))),
      size = value_size, color = value_color
    )
  }

  # Theme with smaller labels for supra matrix
  effective_text_size <- max(4, axis_text_size - n_groups)

  th <- .heatmap_theme(show_axis_labels, effective_text_size, axis_text_angle, aspect_ratio)
  p <- p +
    ggplot2::labs(title = title %||% "Supra-Adjacency Heatmap",
                  subtitle = subtitle, x = xlab, y = ylab) +
    th$theme

  if (!is.null(th$aspect_ratio)) {
    p <- p + ggplot2::coord_fixed(ratio = th$aspect_ratio)
  }

  p
}


# ============================================================================
# Helper Functions
# ============================================================================

#' Convert Matrix to Long Format
#' @keywords internal
.matrix_to_long <- function(mat, row_labels, col_labels) {
  df <- expand.grid(
    row = row_labels,
    col = col_labels,
    stringsAsFactors = FALSE
  )
  df$value <- as.vector(mat)

  # Preserve order as factors
  df$row <- factor(df$row, levels = rev(row_labels))
  df$col <- factor(df$col, levels = col_labels)

  df
}


#' Build Base Heatmap
#' @keywords internal
.build_heatmap_base <- function(df, colors, limits, midpoint, na_color,
                                 legend_title, show_legend, legend_position) {

  # Resolve color palette
  color_vec <- .resolve_colors(colors, df$value, limits, midpoint)

  # Determine if diverging
  vals <- df$value[!is.na(df$value)]
  is_diverging <- !is.null(midpoint) || (length(vals) > 0 && min(vals) < 0 && max(vals) > 0)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$col, y = .data$row, fill = .data$value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1)

  # Add color scale
  if (is_diverging) {
    mid <- midpoint %||% 0
    p <- p + ggplot2::scale_fill_gradient2(
      low = color_vec[1], mid = color_vec[2], high = color_vec[3],
      midpoint = mid, limits = limits, na.value = na_color, name = legend_title
    )
  } else {
    p <- p + ggplot2::scale_fill_gradientn(
      colors = color_vec, limits = limits, na.value = na_color, name = legend_title
    )
  }

  # Legend position
  if (!show_legend || legend_position == "none") {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(legend.position = legend_position)
  }

  p
}


#' Resolve Color Palette
#' @keywords internal
.resolve_colors <- function(colors, values, limits, midpoint) {
  if (is.character(colors) && length(colors) == 1) {
    # Named palette
    switch(colors,
      "viridis" = c("#440154", "#21908C", "#FDE725"),
      "heat" = c("#FFFFCC", "#FD8D3C", "#BD0026"),
      "blues" = c("#F7FBFF", "#6BAED6", "#08306B"),
      "reds" = c("#FFF5F0", "#FB6A4A", "#A50F15"),
      "diverging" = c("#2166AC", "#F7F7F7", "#B2182B"),
      "greens" = c("#F7FCF5", "#74C476", "#00441B"),
      # Default: treat as single color, create gradient
      c("white", colors)
    )
  } else {
    colors
  }
}


#' Add Cluster Borders
#' @keywords internal
.add_cluster_borders <- function(p, cluster_list, node_order, border_color, border_width) {
  n_total <- length(node_order)

  # Calculate cluster boundaries
  pos <- 0
  borders <- data.frame(xmin = numeric(), xmax = numeric(),
                        ymin = numeric(), ymax = numeric())

  for (cluster_nodes in cluster_list) {
    n_cluster <- length(cluster_nodes)
    borders <- rbind(borders, data.frame(
      xmin = pos + 0.5,
      xmax = pos + n_cluster + 0.5,
      ymin = n_total - pos - n_cluster + 0.5,
      ymax = n_total - pos + 0.5
    ))
    pos <- pos + n_cluster
  }

  p + ggplot2::geom_rect(
    data = borders,
    ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                 ymin = .data$ymin, ymax = .data$ymax),
    fill = NA, color = border_color, linewidth = border_width,
    inherit.aes = FALSE
  )
}


#' Add Cluster Labels
#' @keywords internal
.add_cluster_labels <- function(p, cluster_list, node_order) {
  n_total <- length(node_order)
  cluster_names <- names(cluster_list)

  # Calculate label positions (center of each cluster)
  pos <- 0
  label_data <- data.frame(x = numeric(), y = numeric(), label = character())

  for (i in seq_along(cluster_list)) {
    n_cluster <- length(cluster_list[[i]])
    center <- pos + n_cluster / 2
    label_data <- rbind(label_data, data.frame(
      x = center + 0.5,
      y = n_total - center + 0.5,
      label = cluster_names[i]
    ))
    pos <- pos + n_cluster
  }

  # Add labels at top and left margins
  p +
    # Top labels
    ggplot2::annotate("text", x = label_data$x, y = n_total + 1,
                      label = label_data$label, size = 3, fontface = "bold") +
    # Left labels
    ggplot2::annotate("text", x = -0.5, y = label_data$y,
                      label = label_data$label, size = 3, fontface = "bold",
                      angle = 90, hjust = 0.5)
}


#' Heatmap Theme
#' @keywords internal
.heatmap_theme <- function(show_axis_labels, axis_text_size, axis_text_angle, aspect_ratio) {
  th <- ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.title = ggplot2::element_text(size = 10)
    )

  if (show_axis_labels) {
    th <- th + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = axis_text_angle, hjust = 1,
                                          size = axis_text_size),
      axis.text.y = ggplot2::element_text(size = axis_text_size)
    )
  } else {
    th <- th + ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  }

  # Return list with theme and coord (coord must be added separately)
  list(theme = th, aspect_ratio = aspect_ratio)
}


#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a
