#' Multilevel Network Visualization
#'
#' Visualizes multilevel/multiplex networks where multiple layers are stacked
#' in a 3D perspective view. Each layer contains nodes connected by solid edges
#' (within-layer), while dashed lines connect nodes between adjacent layers
#' (inter-layer edges). Each layer is enclosed in a parallelogram shell giving
#' a pseudo-3D appearance.
#'
#' \if{html}{\figure{mlna_example.png}{options: width=600 alt="Multilevel network example"}}
#' \if{latex}{\figure{mlna_example.png}{options: width=4in}}
#'
#' @param model A tna object or weight matrix.
#' @param layer_list List of character vectors defining layers. Each element
#'   contains node names belonging to that layer. Layers are displayed from
#'   top to bottom in list order. If NULL and \code{community} is specified,
#'   layers are auto-detected.
#' @param community Community detection method to use for auto-layering.
#'   If specified, overrides \code{layer_list}. See \code{\link{detect_communities}}
#'   for available methods: "louvain", "walktrap", "fast_greedy", "label_prop",
#'   "infomap", "leiden".
#' @param layout Node layout within layers: "horizontal" (default) spreads nodes
#'   horizontally, "circle" arranges nodes in an ellipse, "spring" uses
#'   force-directed placement based on within-layer connections.
#' @param layer_spacing Vertical distance between layer centers. Default 2.2.
#' @param layer_width Horizontal width of each layer shell. Default 4.5.
#' @param layer_depth Depth of each layer (for 3D effect). Default 2.2.
#' @param skew_angle Angle of perspective skew in degrees. Default 25.
#' @param node_spacing Node placement ratio within layer (0-1). Default 0.7.
#'   Higher values spread nodes closer to the layer edges.
#' @param colors Vector of colors for each layer. Default auto-generated.
#' @param shapes Vector of shapes for each layer. Default cycles through
#'   "circle", "square", "diamond", "triangle".
#' @param edge_colors Vector of edge colors by source layer. If NULL (default),
#'   uses darker versions of layer colors.
#' @param within_edges Logical. Show edges within layers (solid lines). Default TRUE.
#' @param between_edges Logical. Show edges between adjacent layers (dashed lines).
#'   Default TRUE.
#' @param between_style Line style for between-layer edges. Default 2 (dashed).
#'   Use 1 for solid, 3 for dotted.
#' @param show_border Logical. Draw parallelogram shells around layers. Default TRUE.
#' @param legend Logical. Whether to show legend. Default TRUE.
#' @param legend_position Position for legend. Default "topright".
#' @param curvature Edge curvature for within-layer edges. Default 0.15.
#' @param node_size Size of nodes. Default 2.5.
#' @param minimum Minimum edge weight threshold. Edges below this are hidden.
#'   Default 0.
#' @param ... Additional parameters (currently unused).
#'
#' @return Invisibly returns NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create multilevel network
#' set.seed(42)
#' nodes <- paste0("N", 1:15)
#' m <- matrix(runif(225, 0, 0.3), 15, 15)
#' diag(m) <- 0
#' colnames(m) <- rownames(m) <- nodes
#'
#' # Define 3 layers
#' layers <- list(
#'   Macro = paste0("N", 1:5),
#'   Meso = paste0("N", 6:10),
#'   Micro = paste0("N", 11:15)
#' )
#'
#' # Basic usage
#' plot_mlna(m, layers)
#'
#' # Customized
#' plot_mlna(m, layers,
#'      layer_spacing = 2.5,
#'      layer_width = 5,
#'      between_style = 2,  # dashed
#'      minimum = 0.1)
#'
#' # Circle layout within layers
#' plot_mlna(m, layers, layout = "circle")
#' }
plot_mlna <- function(
    model,
    layer_list = NULL,
    community = NULL,
    layout = "horizontal",
    layer_spacing = 2.2,
    layer_width = 4.5,
    layer_depth = 2.2,
    skew_angle = 25,
    node_spacing = 0.7,
    colors = NULL,
    shapes = NULL,
    edge_colors = NULL,
    within_edges = TRUE,
    between_edges = TRUE,
    between_style = 2,
    show_border = TRUE,
    legend = TRUE,
    legend_position = "topright",
    curvature = 0.15,
    node_size = 3,
    minimum = 0,
    ...
) {
  # ==========================================================================
  # 1. Input Validation & Setup
  # ==========================================================================

  # Handle community parameter - auto-detect layers
  if (!is.null(community)) {
    comm_df <- detect_communities(model, method = community)
    layer_list <- split(comm_df$node, comm_df$community)
    names(layer_list) <- paste0("Layer_", names(layer_list))
  }

  # Validate layer_list
  if (is.null(layer_list)) {
    stop("Either layer_list or community must be specified", call. = FALSE)
  }
  n_layers <- length(layer_list)
  if (!is.list(layer_list) || n_layers < 2) {
    stop("layer_list must be a list of 2+ character vectors", call. = FALSE)
  }

  # Get labels and weights from model
  if (inherits(model, "tna")) {
    lab <- model$labels
    weights <- model$weights
  } else if (is.matrix(model)) {
    lab <- colnames(model)
    if (is.null(lab)) lab <- as.character(seq_len(ncol(model)))
    weights <- model
  } else {
    stop("model must be a tna object or matrix", call. = FALSE)
  }

  n <- length(lab)

  # Check no overlap between layers
  all_nodes <- unlist(layer_list)
  if (anyDuplicated(all_nodes)) {
    dups <- all_nodes[duplicated(all_nodes)]
    stop("layer_list groups must not overlap. Duplicates: ",
         paste(unique(dups), collapse = ", "), call. = FALSE)
  }

  # Get indices for each layer and validate
  layer_indices <- lapply(layer_list, function(nodes) {
    idx <- match(nodes, lab)
    if (any(is.na(idx))) {
      missing <- nodes[is.na(idx)]
      stop("Nodes not found in model: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    idx
  })

  # Node-to-layer mapping
  node_to_layer <- rep(NA, n)
  for (i in seq_len(n_layers)) {
    node_to_layer[layer_indices[[i]]] <- i
  }

  # ==========================================================================
  # 2. Color & Shape Palettes
  # ==========================================================================

  color_palette <- c("#ffd89d", "#a68ba5", "#7eb5d6", "#98d4a2",
                     "#f4a582", "#92c5de", "#d6c1de", "#b8e186",
                     "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9")

  shape_palette <- c("circle", "square", "diamond", "triangle",
                     "pentagon", "hexagon", "star", "cross")

  edge_color_palette <- c("#e6a500", "#7a5a7a", "#4a90b8", "#5cb85c",
                          "#d9534f", "#5bc0de", "#9b59b6", "#8bc34a",
                          "#ff7043", "#78909c", "#ab47bc", "#aed581")

  layer_colors <- if (is.null(colors)) rep_len(color_palette, n_layers) else colors
  layer_shapes <- if (is.null(shapes)) rep_len(shape_palette, n_layers) else shapes

  if (is.null(edge_colors)) {
    edge_colors <- rep_len(edge_color_palette, n_layers)
  }

  # ==========================================================================
  # 3. Compute 3D Perspective Layer Positions
  # ==========================================================================

  # Convert skew angle to radians
 skew_rad <- skew_angle * pi / 180

  # Layer base y-positions (top to bottom)
  layer_base_y <- seq(0, -(n_layers - 1) * layer_spacing, length.out = n_layers)

  # Node positions in 3D perspective
  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  # Store layer plane info for drawing
  layer_planes <- vector("list", n_layers)

  for (i in seq_len(n_layers)) {
    idx <- layer_indices[[i]]
    n_nodes <- length(idx)
    base_y <- layer_base_y[i]

    # Skew offset for this layer (layers higher up are shifted right)
    skew_offset <- (n_layers - i) * layer_depth * tan(skew_rad) * 0.5

    if (layout == "horizontal") {
      # Spread nodes horizontally within layer
      if (n_nodes > 1) {
        local_x <- seq(-layer_width / 2 * node_spacing,
                       layer_width / 2 * node_spacing,
                       length.out = n_nodes)
      } else {
        local_x <- 0
      }
      local_y <- rep(0, n_nodes)
    } else if (layout == "circle") {
      # Arrange in ellipse within layer (squashed for perspective)
      angles <- pi / 2 - (seq_len(n_nodes) - 1) * 2 * pi / n_nodes
      radius_x <- layer_width / 3 * node_spacing
      radius_y <- layer_depth / 3 * node_spacing
      local_x <- radius_x * cos(angles)
      local_y <- radius_y * sin(angles)
    } else if (layout == "spring") {
      # Force-directed spring layout within layer
      if (n_nodes > 1) {
        # Extract within-layer weights
        layer_weights <- weights[idx, idx, drop = FALSE]

        # Initialize positions randomly
        set.seed(i * 100)  # Reproducible per layer
        local_x <- runif(n_nodes, -1, 1)
        local_y <- runif(n_nodes, -1, 1)

        # Simple force-directed iterations
        k <- 1.0  # optimal distance
        iterations <- 100

        for (iter in seq_len(iterations)) {
          # Calculate repulsive forces (all pairs)
          fx <- rep(0, n_nodes)
          fy <- rep(0, n_nodes)

          for (j in seq_len(n_nodes)) {
            for (m in seq_len(n_nodes)) {
              if (j != m) {
                dx <- local_x[j] - local_x[m]
                dy <- local_y[j] - local_y[m]
                dist <- sqrt(dx^2 + dy^2) + 0.01
                # Repulsive force
                force <- k^2 / dist
                fx[j] <- fx[j] + (dx / dist) * force
                fy[j] <- fy[j] + (dy / dist) * force
              }
            }
          }

          # Calculate attractive forces (connected pairs)
          for (j in seq_len(n_nodes)) {
            for (m in seq_len(n_nodes)) {
              if (j != m) {
                w <- layer_weights[j, m] + layer_weights[m, j]
                if (!is.na(w) && w > 0) {
                  dx <- local_x[j] - local_x[m]
                  dy <- local_y[j] - local_y[m]
                  dist <- sqrt(dx^2 + dy^2) + 0.01
                  # Attractive force
                  force <- dist^2 / k * w * 2
                  fx[j] <- fx[j] - (dx / dist) * force
                  fy[j] <- fy[j] - (dy / dist) * force
                }
              }
            }
          }

          # Apply forces with cooling
          temp <- 0.5 * (1 - iter / iterations)
          local_x <- local_x + pmax(pmin(fx * temp, 0.5), -0.5)
          local_y <- local_y + pmax(pmin(fy * temp, 0.5), -0.5)
        }

        # Scale to fit layer
        x_range <- range(local_x)
        y_range <- range(local_y)
        if (diff(x_range) > 0) {
          local_x <- (local_x - mean(x_range)) / diff(x_range) * layer_width * node_spacing * 0.8
        }
        if (diff(y_range) > 0) {
          local_y <- (local_y - mean(y_range)) / diff(y_range) * layer_depth * node_spacing * 0.6
        }
      } else {
        local_x <- 0
        local_y <- 0
      }
    }

    # Apply perspective transformation
    # x stays mostly the same, y gets shifted based on depth
    x_pos[idx] <- local_x + skew_offset
    y_pos[idx] <- base_y + local_y * cos(skew_rad)

    # Store layer plane corners for drawing the parallelogram
    # Four corners: front-left, front-right, back-right, back-left
    hw <- layer_width / 2
    hd <- layer_depth / 2
    layer_planes[[i]] <- list(
      corners = matrix(c(
        -hw + skew_offset - hd * tan(skew_rad), base_y - hd * cos(skew_rad),  # back-left
        hw + skew_offset - hd * tan(skew_rad), base_y - hd * cos(skew_rad),   # back-right
        hw + skew_offset + hd * tan(skew_rad), base_y + hd * cos(skew_rad),   # front-right
        -hw + skew_offset + hd * tan(skew_rad), base_y + hd * cos(skew_rad)   # front-left
      ), ncol = 2, byrow = TRUE),
      center_y = base_y,
      skew_offset = skew_offset
    )
  }

  # ==========================================================================
  # 4. Set Up Plot
  # ==========================================================================

  # Calculate plot dimensions with padding
  all_x <- c(x_pos, unlist(lapply(layer_planes, function(p) p$corners[, 1])))
  all_y <- c(y_pos, unlist(lapply(layer_planes, function(p) p$corners[, 2])))
  x_range <- range(all_x) + c(-1.5, 2.5)
  y_range <- range(all_y) + c(-1, 1.5)

  # Set up blank plot
  graphics::plot.new()
  graphics::plot.window(xlim = x_range, ylim = y_range, asp = 1)

  # Get max weight for scaling
  max_w <- max(abs(weights), na.rm = TRUE)
  if (is.na(max_w) || max_w == 0) max_w <- 1

  # ==========================================================================
  # 5. Draw from back to front (painter's algorithm)
  # ==========================================================================

  # Draw layers from bottom (back) to top (front)
  for (i in rev(seq_len(n_layers))) {
    idx <- layer_indices[[i]]
    plane <- layer_planes[[i]]
    corners <- plane$corners

    # --- Draw between-layer edges TO this layer (from layer below) ---
    if (isTRUE(between_edges) && i < n_layers) {
      next_layer <- i + 1
      next_idx <- layer_indices[[next_layer]]

      # Edges from next layer (below) to this layer
      for (src_idx in next_idx) {
        for (tgt_idx in idx) {
          weight <- weights[src_idx, tgt_idx]
          if (!is.na(weight) && weight > minimum) {
            lwd <- 0.5 + 2.5 * (abs(weight) / max_w)
            edge_col <- grDevices::adjustcolor(edge_colors[next_layer], alpha.f = 0.6)
            graphics::segments(
              x0 = x_pos[src_idx], y0 = y_pos[src_idx],
              x1 = x_pos[tgt_idx], y1 = y_pos[tgt_idx],
              lty = between_style,
              col = edge_col,
              lwd = lwd
            )
          }
        }
      }

      # Edges from this layer to next layer (below)
      for (src_idx in idx) {
        for (tgt_idx in next_idx) {
          weight <- weights[src_idx, tgt_idx]
          if (!is.na(weight) && weight > minimum) {
            lwd <- 0.5 + 2.5 * (abs(weight) / max_w)
            edge_col <- grDevices::adjustcolor(edge_colors[i], alpha.f = 0.6)
            graphics::segments(
              x0 = x_pos[src_idx], y0 = y_pos[src_idx],
              x1 = x_pos[tgt_idx], y1 = y_pos[tgt_idx],
              lty = between_style,
              col = edge_col,
              lwd = lwd
            )
          }
        }
      }
    }

    # --- Draw layer shell (parallelogram) ---
    if (isTRUE(show_border)) {
      fill_color <- grDevices::adjustcolor(layer_colors[i], alpha.f = 0.3)
      border_color <- grDevices::adjustcolor(layer_colors[i], alpha.f = 0.9)

      graphics::polygon(
        x = c(corners[, 1], corners[1, 1]),
        y = c(corners[, 2], corners[1, 2]),
        border = border_color,
        col = fill_color,
        lwd = 2.5
      )

      # Layer label on the right
      layer_names <- names(layer_list)
      if (!is.null(layer_names)) {
        label_x <- max(corners[, 1]) + 0.3
        label_y <- plane$center_y
        graphics::text(
          x = label_x, y = label_y,
          labels = layer_names[i],
          adj = 0,
          col = layer_colors[i],
          font = 2,
          cex = 1.1
        )
      }
    }

    # --- Draw within-layer edges ---
    if (isTRUE(within_edges)) {
      for (src in idx) {
        for (tgt in idx) {
          if (src != tgt) {
            weight <- weights[src, tgt]
            if (!is.na(weight) && weight > minimum) {
              x0 <- x_pos[src]
              y0 <- y_pos[src]
              x1 <- x_pos[tgt]
              y1 <- y_pos[tgt]

              dx <- x1 - x0
              dy <- y1 - y0
              len <- sqrt(dx^2 + dy^2)

              if (len > 0) {
                # Curve perpendicular to the line
                mid_x <- (x0 + x1) / 2
                mid_y <- (y0 + y1) / 2
                off_x <- -dy / len * curvature * len
                off_y <- dx / len * curvature * len

                edge_col <- grDevices::adjustcolor(
                  layer_colors[i], red.f = 0.6, green.f = 0.6, blue.f = 0.6
                )
                lwd <- 0.8 + 1.5 * (abs(weight) / max_w)

                graphics::xspline(
                  x = c(x0, mid_x + off_x, x1),
                  y = c(y0, mid_y + off_y, y1),
                  shape = 1, open = TRUE,
                  border = edge_col, lwd = lwd
                )

                # Arrowhead
                angle <- atan2(y1 - (mid_y + off_y), x1 - (mid_x + off_x))
                arrow_len <- 0.08
                graphics::polygon(
                  x = x1 + arrow_len * c(0, -cos(angle - pi/7), -cos(angle + pi/7)),
                  y = y1 + arrow_len * c(0, -sin(angle - pi/7), -sin(angle + pi/7)),
                  col = edge_col, border = edge_col
                )
              }
            }
          }
        }
      }
    }

    # --- Draw nodes ---
    pch_val <- switch(layer_shapes[i],
      "circle" = 21, "square" = 22, "diamond" = 23, "triangle" = 24,
      21
    )

    graphics::points(
      x_pos[idx], y_pos[idx],
      pch = pch_val,
      bg = layer_colors[i],
      col = "gray20",
      cex = node_size,
      lwd = 1.5
    )

    # Node labels
    graphics::text(
      x_pos[idx], y_pos[idx],
      labels = lab[idx],
      cex = 0.75,
      pos = 3,
      offset = 0.6,
      font = 1
    )
  }

  # ==========================================================================
  # 6. Draw Legend
  # ==========================================================================

  if (isTRUE(legend)) {
    layer_names <- names(layer_list)
    if (is.null(layer_names)) {
      layer_names <- paste0("Layer ", seq_len(n_layers))
    }

    shape_to_pch <- c(
      "circle" = 21, "square" = 22, "diamond" = 23, "triangle" = 24,
      "pentagon" = 21, "hexagon" = 21, "star" = 8, "cross" = 3
    )

    pch_values <- sapply(layer_shapes, function(s) {
      if (s %in% names(shape_to_pch)) shape_to_pch[s] else 21
    })

    graphics::legend(
      legend_position,
      legend = layer_names,
      pch = pch_values,
      pt.bg = layer_colors,
      col = edge_colors,
      pt.cex = 1.5,
      cex = 0.9,
      bty = "n",
      title = "Layers"
    )
  }

  invisible(NULL)
}

#' @rdname plot_mlna
#' @export
mlna <- plot_mlna
