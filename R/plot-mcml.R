#' Multi-Cluster Multi-Layer Network
#'
#' Two-layer visualization: bottom layer shows detailed multi-cluster network,
#' top layer shows summary network with one node per cluster.
#'
#' @param x Weight matrix, tna object, or cograph_network
#' @param cluster_list Named list of node vectors per cluster, column name
#'   string, or NULL for auto-detection
#' @param layer_spacing Vertical distance between layers. Default 4.
#' @param spacing Cluster spacing. Default 3.
#' @param shape_size Cluster shell size. Default 1.2.
#' @param summary_size Summary node size. Default 4.
#' @param skew_angle Perspective angle in degrees. Default 25.
#' @param aggregation How to aggregate: "sum", "mean", "max". Default "sum".
#' @param minimum Edge threshold. Default 0.
#' @param colors Cluster colors. Default auto.
#' @param legend Show legend. Default TRUE.
#' @param show_labels Logical. Show node labels. Default TRUE.
#' @param node_labels Labels to display. Can be:
#'   \itemize{
#'     \item NULL (default): Use node identifiers
#'     \item Column name string: Use values from that column in nodes data
#'     \item Character vector: Use directly (must match node count)
#'   }
#' @param label_size Label text size. Default NULL (auto-scaled).
#' @param label_abbrev Label abbreviation: NULL (none), integer (max chars),
#'   or "auto" (adaptive based on node count).
#' @param node_size Size of nodes in bottom layer. Default 1.8.
#' @param node_shape Shape of nodes: "circle", "square", "diamond", "triangle".
#'   Can be a single value (applied to all) or a vector (one per node).
#'   Default "circle".
#' @param cluster_shape Shape for cluster summary nodes in top layer.
#'   Can be a single value or vector (one per cluster). Default "circle".
#' @param ... Unused.
#'
#' @export
plot_mcml <- function(
    x,
    cluster_list = NULL,
    layer_spacing = NULL,
    spacing = 3,
    shape_size = 1.2,
    summary_size = 4,
    skew_angle = 60,
    aggregation = c("sum", "mean", "max"),
    minimum = 0,
    colors = NULL,
    legend = TRUE,
    show_labels = TRUE,
    node_labels = NULL,
    label_size = NULL,
    label_abbrev = NULL,
    node_size = 1.8,
    node_shape = "circle",
    cluster_shape = "circle",
    ...
) {
  aggregation <- match.arg(aggregation)

  # Extract weights and labels based on input type
  nodes_df <- NULL
  if (inherits(x, "cograph_network")) {
    weights <- to_matrix(x)
    lab <- x$nodes$label
    nodes_df <- x$nodes
  } else if (inherits(x, "tna")) {
    weights <- x$weights
    lab <- x$labels
  } else {
    weights <- x
    lab <- colnames(x)
    if (is.null(lab)) lab <- seq_len(ncol(x))
  }

  n <- length(lab)

  # Resolve display labels
 display_labels <- if (is.null(node_labels)) {
    lab  # Use identifiers
  } else if (is.character(node_labels) && length(node_labels) == 1 && !is.null(nodes_df)) {
    # Column name
    if (node_labels %in% names(nodes_df)) {
      nodes_df[[node_labels]]
    } else {
      warning("Column '", node_labels, "' not found, using node identifiers")
      lab
    }
  } else {
    # Direct vector
    if (length(node_labels) != n) {
      warning("node_labels length mismatch, using node identifiers")
      lab
    } else {
      node_labels
    }
  }

  # Expand node_shape to vector if needed
  node_shape <- rep_len(node_shape, n)


  # Handle cluster_list: can be list, column name string, or NULL (auto-detect)
  if (is.character(cluster_list) && length(cluster_list) == 1 &&
      !is.null(nodes_df) && cluster_list %in% names(nodes_df)) {
    # Column name provided
    cluster_col <- nodes_df[[cluster_list]]
    cluster_list <- split(lab, cluster_col)
    message("Using '", cluster_list, "' column for clusters")
  } else if (is.null(cluster_list) && !is.null(nodes_df)) {
    # Auto-detect from common column names
    cluster_cols <- c("clusters", "cluster", "groups", "group", "community", "module")
    for (col in cluster_cols) {
      if (col %in% names(nodes_df)) {
        cluster_col <- nodes_df[[col]]
        cluster_list <- split(lab, cluster_col)
        message("Using '", col, "' column for clusters")
        break
      }
    }
  }

  if (is.null(cluster_list)) {
    stop("cluster_list required: provide a list, column name, or add a ",
         "'clusters'/'groups' column to nodes")
  }

  n_clusters <- length(cluster_list)
  cluster_names <- names(cluster_list)
  if (is.null(cluster_names)) cluster_names <- paste0("C", seq_len(n_clusters))

  # Colors
  pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
           "#0072B2", "#D55E00", "#CC79A7", "#999999")
  if (is.null(colors)) colors <- rep_len(pal, n_clusters)

  # Expand cluster_shape to vector if needed
  cluster_shape <- rep_len(cluster_shape, n_clusters)

  # Cluster indices
  cluster_idx <- lapply(cluster_list, function(nodes) match(nodes, lab))

  # Perspective: table view (flat plane seen from above at angle)
  skew_rad <- skew_angle * pi / 180
  compress <- cos(skew_rad)  # flatten y for table-like view
  shear <- 0.15  # subtle lean

  # Bottom layer: cluster centers (flat plane)
  angles <- pi/2 - (seq_len(n_clusters) - 1) * 2 * pi / n_clusters
  bx_base <- spacing * cos(angles)
  by_base <- spacing * sin(angles)
  bx <- bx_base
  by <- by_base * compress

  # Auto-calculate layer_spacing to ensure no overlap
  # Top of bottom layer is max(by) + compressed shell height
  # Bottom of top layer needs to be above that
  bottom_top <- max(by) + shape_size * compress
  bottom_bottom <- min(by) - shape_size * compress

  if (is.null(layer_spacing)) {
    # Ensure gap of at least 1.5 units between layers
    layer_spacing <- (bottom_top - bottom_bottom) + 2
  }

  # Top layer positioned above bottom layer (not too far)
  gap <- spacing * 0.6
  top_base_y <- bottom_top + gap

  # Top layer: oval layout with spaced nodes
  top_radius_x <- spacing * 0.8  # wider horizontally
  top_radius_y <- spacing * 0.25  # oval shape

  tx <- top_radius_x * cos(angles)
  ty <- top_radius_y * sin(angles) + top_base_y

  # Summary weights between clusters
  sw <- matrix(0, n_clusters, n_clusters)
  for (i in seq_len(n_clusters)) {
    for (j in seq_len(n_clusters)) {
      if (i != j) {
        w <- weights[cluster_idx[[i]], cluster_idx[[j]]]
        w <- w[!is.na(w) & w > 0]
        if (length(w) > 0) {
          sw[i, j] <- switch(aggregation, sum = sum(w), mean = mean(w), max = max(w))
        }
      }
    }
  }
  max_sw <- max(sw)
  if (max_sw == 0) max_sw <- 1

  max_w <- max(abs(weights), na.rm = TRUE)
  if (is.na(max_w) || max_w == 0) max_w <- 1

  # Helper: get point on ellipse edge facing target
  shell_edge <- function(cx, cy, tx, ty, rx, ry) {
    a <- atan2((ty - cy) / ry, (tx - cx) / rx)
    c(cx + rx * cos(a), cy + ry * sin(a))
  }

  # Plot limits (tight padding)
  pad <- shape_size * 0.3
  xlim <- range(c(bx, tx)) + c(-shape_size - pad, shape_size + pad)
  ylim <- range(c(by, ty)) + c(-shape_size * compress - pad, shape_size + pad)

  old_par <- graphics::par(mar = c(0.2, 0.2, 0.2, 0.2))
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

  # ============ DRAW INTER-LAYER CONNECTIONS FIRST (behind everything) ============
  # Store node positions for inter-layer connections
  node_positions <- vector("list", n_clusters)
  node_r <- shape_size * 0.55

  for (i in seq_len(n_clusters)) {
    idx <- cluster_idx[[i]]
    n_nodes <- length(idx)
    if (n_nodes == 1) {
      node_positions[[i]] <- list(x = bx[i], y = by[i])
    } else {
      na <- pi/2 - (seq_len(n_nodes) - 1) * 2 * pi / n_nodes
      node_x <- node_r * cos(na)
      node_y <- node_r * sin(na) * compress
      node_positions[[i]] <- list(
        x = bx[i] + node_x,
        y = by[i] + node_y
      )
    }
    # Draw dashed line from each node to summary node
    for (j in seq_along(node_positions[[i]]$x)) {
      graphics::segments(
        node_positions[[i]]$x[j], node_positions[[i]]$y[j],
        tx[i], ty[i],
        col = grDevices::adjustcolor(colors[i], 0.5),
        lty = 2, lwd = 1
      )
    }
  }

  # ============ TOP LAYER (summary network) ============

  # Summary edges
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (i != j && sw[i, j] > minimum) {
          lwd <- 0.5 + 1.5 * sw[i, j] / max_sw
          graphics::segments(tx[i], ty[i], tx[j], ty[j],
                             col = grDevices::adjustcolor(colors[i], 0.7), lwd = lwd)
        }
      }
    }
  }

  # Summary nodes - use per-cluster shapes
  summary_pch <- .shape_to_pch(cluster_shape)
  for (i in seq_len(n_clusters)) {
    graphics::points(tx[i], ty[i], pch = summary_pch[i], bg = colors[i],
                     col = "gray20", cex = summary_size, lwd = 2)
  }

  # ============ BOTTOM LAYER (detailed clusters) ============

  # Between-cluster edges (shell to shell)
  shell_rx <- shape_size
  shell_ry <- shape_size * compress
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (i != j && sw[i, j] > minimum) {
          p1 <- shell_edge(bx[i], by[i], bx[j], by[j], shell_rx, shell_ry)
          p2 <- shell_edge(bx[j], by[j], bx[i], by[i], shell_rx, shell_ry)
          lwd <- 0.5 + 1.5 * sw[i, j] / max_sw
          graphics::segments(p1[1], p1[2], p2[1], p2[2],
                             col = grDevices::adjustcolor(colors[i], 0.6), lwd = lwd)
        }
      }
    }
  }

  # Cluster shells and nodes
  for (i in seq_len(n_clusters)) {
    idx <- cluster_idx[[i]]
    n_nodes <- length(idx)

    # Shell (ellipse for table-view perspective)
    theta <- seq(0, 2 * pi, length.out = 60)
    shell_x <- shape_size * cos(theta)
    shell_y <- shape_size * sin(theta) * compress
    graphics::polygon(
      bx[i] + shell_x,
      by[i] + shell_y,
      border = colors[i],
      col = grDevices::adjustcolor(colors[i], 0.15),
      lwd = 2
    )

    # Node positions (use pre-computed)
    nx <- node_positions[[i]]$x
    ny <- node_positions[[i]]$y

    # Within-cluster edges
    if (n_nodes > 1) {
      for (j in seq_len(n_nodes)) {
        for (k in seq_len(n_nodes)) {
          if (j != k) {
            w <- weights[idx[j], idx[k]]
            if (!is.na(w) && w > minimum) {
              lwd <- 0.3 + 1 * w / max_w
              graphics::segments(nx[j], ny[j], nx[k], ny[k],
                                 col = grDevices::adjustcolor(colors[i], 0.35), lwd = lwd)
            }
          }
        }
      }
    }

    # Nodes - use per-node shapes
    node_pch <- .shape_to_pch(node_shape[idx])
    graphics::points(nx, ny, pch = node_pch, bg = colors[i], col = "gray30",
                     cex = node_size)

    # Node labels
    if (isTRUE(show_labels)) {
      lbl_text <- display_labels[idx]
      if (!is.null(label_abbrev)) {
        lbl_text <- abbrev_label(lbl_text, label_abbrev, n)
      }
      lbl_cex <- if (is.null(label_size)) 0.6 else label_size
      graphics::text(nx, ny, labels = lbl_text, cex = lbl_cex, pos = 3,
                     offset = 0.4, col = "gray20")
    }
  }

  # Legend (positioned inside plot)
  if (legend) {
    graphics::legend(
      x = max(bx) + shape_size * 0.5,
      y = mean(c(max(by), min(ty))),
      legend = cluster_names, pch = 21, pt.bg = colors,
      col = "gray30", pt.cex = 1.2, cex = 0.7, bty = "n",
      xjust = 0, yjust = 0.5
    )
  }

  invisible(NULL)
}

#' @rdname plot_mcml
#' @export
mcml <- plot_mcml
