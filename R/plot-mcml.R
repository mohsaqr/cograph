#' Plot Multi-Cluster Multi-Layer Network
#'
#' Two-layer visualization: bottom layer shows detailed multi-cluster network,
#' top layer shows summary network with one node per cluster.
#'
#' @param x Weight matrix, tna object, cograph_network, or cluster_summary object
#' @param cluster_list Named list of node vectors per cluster, column name
#'   string, or NULL for auto-detection. Ignored if x is cluster_summary.
#' @param mode What values to display on edges: "weights" (default) shows raw
#'   aggregated weights, "tna" shows transition probabilities (row-normalized).
#' @param layer_spacing Vertical distance between layers. Default NULL (auto).
#' @param spacing Cluster spacing. Default 3.
#' @param shape_size Cluster shell size. Default 1.2.
#' @param summary_size Summary node size. Default 4.
#' @param skew_angle Perspective angle in degrees. Default 60.
#' @param aggregation How to aggregate: "sum", "mean", "max". Default "sum".
#'   Ignored if x is cluster_summary.
#' @param minimum Edge threshold. Default 0.
#' @param colors Cluster colors. Default auto.
#' @param legend Show legend. Default TRUE.
#' @param show_labels Logical. Show node labels. Default TRUE.
#' @param nodes Node metadata. Can be:
#'   \itemize{
#'     \item NULL (default): Use existing nodes data from cograph_network
#'     \item Data frame: Must have `label` column for matching; if `labels`
#'       column exists, uses it for display text
#'   }
#'   Display priority: `labels` column > `label` column (identifiers).
#'   Ignored if x is cluster_summary.
#' @param label_size Label text size. Default NULL (auto-scaled).
#' @param label_abbrev Label abbreviation: NULL (none), integer (max chars),
#'   or "auto" (adaptive based on node count).
#' @param node_size Size of nodes in bottom layer. Default 1.8.
#' @param node_shape Shape of nodes: "circle", "square", "diamond", "triangle".
#'   Can be a single value (applied to all) or a vector (one per node).
#'   Default "circle".
#' @param cluster_shape Shape for cluster summary nodes in top layer.
#'   Can be a single value or vector (one per cluster). Default "circle".
#' @param title Main plot title. Default NULL.
#' @param subtitle Subtitle below title. Default NULL.
#' @param title_size Title text size. Default 1.2.
#' @param subtitle_size Subtitle text size. Default 0.9.
#' @param legend_position Legend position: "right", "left", "top", "bottom", "none".
#'   Default "right".
#' @param legend_size Legend text size. Default 0.7.
#' @param legend_pt_size Legend point size. Default 1.2.
#' @param summary_labels Show cluster labels on summary nodes. Default TRUE.
#' @param summary_label_size Summary label text size. Default 0.8.
#' @param summary_label_position Summary label position (1=below, 2=left, 3=above,
#'   4=right). Default 3.
#' @param summary_label_color Summary label color. Default "gray20".
#' @param summary_arrows Show arrows on summary edges. Default TRUE.
#' @param summary_arrow_size Arrow head size. Default 0.15.
#' @param edge_width_range Min/max edge width for within-cluster edges. Default
#'   c(0.3, 1.3).
#' @param between_edge_width_range Min/max edge width for between-cluster edges
#'   in bottom layer. Default c(0.5, 2.0).
#' @param summary_edge_width_range Min/max edge width for summary edges in top
#'   layer. Default c(0.5, 2.0).
#' @param edge_alpha Within-cluster edge transparency. Default 0.35.
#' @param between_edge_alpha Between-cluster edge transparency. Default 0.6.
#' @param summary_edge_alpha Summary layer edge transparency. Default 0.7.
#' @param inter_layer_alpha Inter-layer line transparency. Default 0.5.
#' @param edge_labels Show weight labels on within-cluster edges. Default FALSE.
#' @param edge_label_size Edge label text size. Default 0.5.
#' @param edge_label_color Edge label color. Default "gray40".
#' @param edge_label_digits Decimal places for edge labels. Default 2.
#' @param summary_edge_labels Show weight labels on summary edges. Default
#'   FALSE.
#' @param summary_edge_label_size Summary edge label text size. Default 0.6.
#' @param top_layer_scale Top layer oval x/y scale factors. Default
#'   c(0.8, 0.25).
#' @param inter_layer_gap Gap between layers as multiplier of spacing.
#'   Default 0.6.
#' @param node_radius_scale Node radius within cluster as multiplier of
#'   shape_size. Default 0.55.
#' @param shell_alpha Shell fill transparency. Default 0.15.
#' @param shell_border_width Shell border line width. Default 2.
#' @param node_border_color Detail node border color. Default "gray30".
#' @param summary_border_color Summary node border color. Default "gray20".
#' @param summary_border_width Summary node border width. Default 2.
#' @param label_color Detail label color. Default "gray20".
#' @param label_position Detail label position (1-4). Default 3.
#' @param ... Unused.
#'
#' @return Invisibly returns the cluster_summary data object.
#'
#' @export
#' @seealso \code{\link{cluster_summary}}, \code{\link{plot_mtna}}
#'
#' @examples
#' # Create test matrix
#' mat <- matrix(runif(36), 6, 6)
#' diag(mat) <- 0
#' colnames(mat) <- rownames(mat) <- LETTERS[1:6]
#'
#' # Define clusters
#' clusters <- list(
#'   Cluster1 = c("A", "B"),
#'   Cluster2 = c("C", "D"),
#'   Cluster3 = c("E", "F")
#' )
#'
#' # Plot directly
#' plot_mcml(mat, clusters)
#'
#' # Pre-compute and reuse
#' cs <- cluster_summary(mat, clusters)
#' plot_mcml(cs)  # Uses pre-computed data
plot_mcml <- function(
    x,
    cluster_list = NULL,
    mode = c("weights", "tna"),
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
    nodes = NULL,
    label_size = NULL,
    label_abbrev = NULL,
    node_size = 1.8,
    node_shape = "circle",
    cluster_shape = "circle",
    # Title & Legend
    title = NULL,
    subtitle = NULL,
    title_size = 1.2,
    subtitle_size = 0.9,
    legend_position = "right",
    legend_size = 0.7,
    legend_pt_size = 1.2,
    # Summary labels
    summary_labels = TRUE,
    summary_label_size = 0.8,
    summary_label_position = 3,
    summary_label_color = "gray20",
    # Summary arrows
    summary_arrows = TRUE,
    summary_arrow_size = 0.15,
    # Edge control
    edge_width_range = c(0.3, 1.3),
    between_edge_width_range = c(0.5, 2.0),
    summary_edge_width_range = c(0.5, 2.0),
    edge_alpha = 0.35,
    between_edge_alpha = 0.6,
    summary_edge_alpha = 0.7,
    inter_layer_alpha = 0.5,
    # Edge labels
    edge_labels = FALSE,
    edge_label_size = 0.5,
    edge_label_color = "gray40",
    edge_label_digits = 2,
    summary_edge_labels = FALSE,
    summary_edge_label_size = 0.6,
    # Layout fine-tuning
    top_layer_scale = c(0.8, 0.25),
    inter_layer_gap = 0.6,
    node_radius_scale = 0.55,
    # Shell styling
    shell_alpha = 0.15,
    shell_border_width = 2,
    # Node styling
    node_border_color = "gray30",
    summary_border_color = "gray20",
    summary_border_width = 2,
    # Label styling
    label_color = "gray20",
    label_position = 3,
    ...
) {
  aggregation <- match.arg(aggregation)
  mode <- match.arg(mode)

  # For mode = "tna", show edge labels by default (like tplot/splot with tna)
  # Check if user explicitly set these parameters
 explicit_args <- names(match.call())
  if (mode == "tna") {
    if (!"edge_labels" %in% explicit_args) {
      edge_labels <- TRUE
    }
    if (!"summary_edge_labels" %in% explicit_args) {
      summary_edge_labels <- TRUE
    }
  }

  # ============================================================================
  # Get or compute cluster_summary
  # ============================================================================

  if (inherits(x, "cluster_summary")) {
    cs <- x
  } else {
    # Extract nodes_df for display labels
    nodes_df <- NULL
    if (inherits(x, "cograph_network")) {
      nodes_df <- x$nodes
    }
    if (is.data.frame(nodes)) {
      nodes_df <- nodes
    }

    # Map aggregation to method
    cs <- cluster_summary(x, cluster_list, method = aggregation, type = "tna",
                          compute_within = TRUE)

    # Store nodes_df and display_labels for visualization
    cs$nodes_df <- nodes_df
  }

  # ============================================================================
  # Extract data from cluster_summary
  # ============================================================================

  cluster_list <- cs$clusters
  cluster_names <- names(cluster_list)
  n_clusters <- cs$meta$n_clusters
  n <- cs$meta$n_nodes

  # Get original weight matrix for within-cluster visualization
  # We need raw weights, so re-extract if needed
  if (inherits(x, "cluster_summary")) {
    # Need to get weights from somewhere - use the input
    # For cluster_summary, we stored processed weights, need raw
    # Use within$X$weights which are raw (before normalization)
    weights <- NULL  # Will use within data directly
  } else if (inherits(x, "cograph_network")) {
    weights <- if (!is.null(x$weights)) x$weights else to_matrix(x)
  } else if (inherits(x, "tna")) {
    weights <- x$weights
  } else {
    weights <- x
  }

  # Get node labels
  if (!is.null(weights)) {
    lab <- rownames(weights)
    if (is.null(lab)) lab <- as.character(seq_len(n))
  } else {
    # Reconstruct from cluster_list
    lab <- unlist(cluster_list, use.names = FALSE)
  }

  # Get display labels from nodes_df
  nodes_df <- cs$nodes_df
  display_labels <- if (!is.null(nodes_df)) {
    if ("labels" %in% names(nodes_df)) {
      nodes_df$labels
    } else if ("label" %in% names(nodes_df)) {
      nodes_df$label
    } else {
      lab
    }
  } else {
    lab
  }

  # Get cluster indices
  cluster_idx <- lapply(cluster_list, function(nodes_vec) match(nodes_vec, lab))

  # Between-cluster weights (processed based on type)
  bw <- cs$between$weights

  # Add self-loop values (within-cluster totals) to diagonal
  # This represents the total transition probability staying within each cluster
  if (!is.null(cs$within)) {
    for (cl_name in names(cs$within)) {
      if (cl_name %in% rownames(bw)) {
        within_w <- cs$within[[cl_name]]$weights
        # Sum all within-cluster transitions (normalized)
        diag_val <- sum(within_w, na.rm = TRUE) / nrow(within_w)
        bw[cl_name, cl_name] <- diag_val
      }
    }
  }

  # For edge labels, use processed weights for display
  sw_labels <- bw

  # Expand node_shape to vector if needed
  node_shape <- rep_len(node_shape, n)

  # Colors
  pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
           "#0072B2", "#D55E00", "#CC79A7", "#999999")
  if (is.null(colors)) colors <- rep_len(pal, n_clusters)

  # Expand cluster_shape to vector if needed
  cluster_shape <- rep_len(cluster_shape, n_clusters)

  # ============================================================================
  # Layout computation
  # ============================================================================

  # Perspective: table view (flat plane seen from above at angle)
  skew_rad <- skew_angle * pi / 180
  compress <- cos(skew_rad)  # flatten y for table-like view

  # Bottom layer: cluster centers (flat plane)
  angles <- pi/2 - (seq_len(n_clusters) - 1) * 2 * pi / n_clusters
  bx_base <- spacing * cos(angles)
  by_base <- spacing * sin(angles)
  bx <- bx_base
  by <- by_base * compress

  # Auto-calculate layer_spacing to ensure no overlap
  bottom_top <- max(by) + shape_size * compress
  bottom_bottom <- min(by) - shape_size * compress

  if (is.null(layer_spacing)) {
    layer_spacing <- (bottom_top - bottom_bottom) + 2
  }

  # Top layer positioned above bottom layer
  gap <- spacing * inter_layer_gap
  top_base_y <- bottom_top + gap

  # Top layer: oval layout with spaced nodes
  top_radius_x <- spacing * top_layer_scale[1]
  top_radius_y <- spacing * top_layer_scale[2]

  tx <- top_radius_x * cos(angles)
  ty <- top_radius_y * sin(angles) + top_base_y

  # Edge weight scaling
  max_sw <- max(bw)
  if (max_sw == 0) max_sw <- 1

  # For within-cluster edges, need max from raw weights
  if (!is.null(weights)) {
    max_w <- max(abs(weights), na.rm = TRUE)
    if (is.na(max_w) || max_w == 0) max_w <- 1
  } else {
    # Get from within data
    max_w <- 1
    if (!is.null(cs$within)) {
      all_within_w <- unlist(lapply(cs$within, function(w) w$weights))
      if (length(all_within_w) > 0) {
        max_w <- max(abs(all_within_w), na.rm = TRUE)
        if (is.na(max_w) || max_w == 0) max_w <- 1
      }
    }
  }

  # Helper: get point on ellipse edge facing target
  shell_edge <- function(cx, cy, tx, ty, rx, ry) {
    a <- atan2((ty - cy) / ry, (tx - cx) / rx)
    c(cx + rx * cos(a), cy + ry * sin(a))
  }

  # ============================================================================
  # Plot setup
  # ============================================================================

  # Plot limits (tight padding)
  pad <- shape_size * 0.3
  xlim <- range(c(bx, tx)) + c(-shape_size - pad, shape_size + pad)
  ylim <- range(c(by, ty)) + c(-shape_size * compress - pad, shape_size + pad)

  old_par <- graphics::par(mar = c(0.2, 0.2, 0.2, 0.2))
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

  # ============================================================================
  # DRAW INTER-LAYER CONNECTIONS FIRST (behind everything)
  # ============================================================================

  node_positions <- vector("list", n_clusters)
  node_r <- shape_size * node_radius_scale

  for (i in seq_len(n_clusters)) {
    idx <- cluster_idx[[i]]
    n_nodes <- length(idx)
    if (n_nodes == 1) {
      node_positions[[i]] <- list(x = bx[i], y = by[i], angles = pi/2)
    } else {
      na <- pi/2 - (seq_len(n_nodes) - 1) * 2 * pi / n_nodes
      node_x <- node_r * cos(na)
      node_y <- node_r * sin(na) * compress
      node_positions[[i]] <- list(
        x = bx[i] + node_x,
        y = by[i] + node_y,
        angles = na  # Store original angles for label positioning
      )
    }
    # Draw dashed line from each node to summary node
    for (j in seq_along(node_positions[[i]]$x)) {
      graphics::segments(
        node_positions[[i]]$x[j], node_positions[[i]]$y[j],
        tx[i], ty[i],
        col = grDevices::adjustcolor(colors[i], inter_layer_alpha),
        lty = 2, lwd = 1
      )
    }
  }

  # ============================================================================
  # TOP LAYER (summary network)
  # ============================================================================

  summary_node_r <- summary_size * 0.04
  summary_arrow_sz <- summary_arrow_size

  # 1. Draw regular edges (not self-loops)
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (i != j && bw[i, j] > minimum) {
          lwd <- summary_edge_width_range[1] +
            (summary_edge_width_range[2] - summary_edge_width_range[1]) *
            bw[i, j] / max_sw
          edge_col <- grDevices::adjustcolor(colors[i], summary_edge_alpha)
          angle <- atan2(ty[j] - ty[i], tx[j] - tx[i])

          if (summary_arrows) {
            tip_x <- tx[j] - summary_node_r * cos(angle)
            tip_y <- ty[j] - summary_node_r * sin(angle)
            line_end_x <- tip_x - summary_arrow_sz * cos(angle)
            line_end_y <- tip_y - summary_arrow_sz * sin(angle)
            graphics::segments(tx[i], ty[i], line_end_x, line_end_y,
                               col = edge_col, lwd = lwd)
            draw_arrow_base(tip_x, tip_y, angle, summary_arrow_sz,
                            col = edge_col, border = edge_col, lwd = lwd)
          } else {
            graphics::segments(tx[i], ty[i], tx[j], ty[j],
                               col = edge_col, lwd = lwd)
          }

          if (summary_edge_labels) {
            lbl_x <- tx[i] + (tx[j] - tx[i]) * 0.35
            lbl_y <- ty[i] + (ty[j] - ty[i]) * 0.35
            graphics::text(lbl_x, lbl_y,
                           labels = round(sw_labels[i, j], edge_label_digits),
                           cex = summary_edge_label_size,
                           col = edge_label_color)
          }
        }
      }
    }
  }

  # 2. Draw summary nodes as PIE CHARTS (self vs other, single color)
  pie_radius <- 0.35  # Pie chart radius in plot units

  for (i in seq_len(n_clusters)) {
    # Self-loop proportion (within-cluster) vs between-cluster
    self_val <- bw[i, i]
    other_val <- sum(bw[i, -i])
    total <- self_val + other_val

    if (total > 0) {
      self_prop <- self_val / total
    } else {
      self_prop <- 0
    }

    # Draw "other" slice first (light gray background)
    if (self_prop < 1) {
      theta <- seq(0, 2 * pi, length.out = 60)
      graphics::polygon(tx[i] + pie_radius * cos(theta),
                        ty[i] + pie_radius * sin(theta),
                        col = "gray90", border = NA)
    }

    # Draw "self" slice (cluster color) - starts from top
    if (self_prop > 0.001) {
      start_angle <- pi / 2
      end_angle <- start_angle - self_prop * 2 * pi
      n_pts <- max(10, round(50 * self_prop))
      angles <- seq(start_angle, end_angle, length.out = n_pts)
      slice_x <- c(tx[i], tx[i] + pie_radius * cos(angles), tx[i])
      slice_y <- c(ty[i], ty[i] + pie_radius * sin(angles), ty[i])
      graphics::polygon(slice_x, slice_y, col = colors[i], border = NA)
    }

    # Draw border circle on top
    theta <- seq(0, 2 * pi, length.out = 60)
    graphics::lines(tx[i] + pie_radius * cos(theta),
                    ty[i] + pie_radius * sin(theta),
                    col = summary_border_color, lwd = summary_border_width)
  }

  # 3. Draw loops on summary pies
  if (max_sw > 0) {
    loop_radius <- 0.15
    for (i in seq_len(n_clusters)) {
      if (bw[i, i] > minimum) {
        lwd <- summary_edge_width_range[1] +
          (summary_edge_width_range[2] - summary_edge_width_range[1]) *
          bw[i, i] / max_sw
        edge_col <- grDevices::adjustcolor(colors[i], summary_edge_alpha)

        # Loop rotation pointing outward
        loop_rot <- atan2(ty[i] - mean(ty), tx[i] - mean(tx))

        # Loop center just outside the pie
        loop_cx <- tx[i] + (pie_radius + loop_radius) * cos(loop_rot)
        loop_cy <- ty[i] + (pie_radius + loop_radius) * sin(loop_rot)

        # Draw circular arc
        n_pts <- 40
        arc_start <- loop_rot + pi + 0.5
        arc_end <- loop_rot + pi - 0.5
        if (arc_end < arc_start) arc_end <- arc_end + 2 * pi
        angles <- seq(arc_start, arc_end, length.out = n_pts)
        loop_x <- loop_cx + loop_radius * cos(angles)
        loop_y <- loop_cy + loop_radius * sin(angles)

        graphics::lines(loop_x, loop_y, col = edge_col, lwd = lwd)

        # Arrow at end
        if (summary_arrows) {
          n_lp <- length(loop_x)
          arr_angle <- atan2(loop_y[n_lp] - loop_y[n_lp-1],
                             loop_x[n_lp] - loop_x[n_lp-1])
          draw_arrow_base(loop_x[n_lp], loop_y[n_lp], arr_angle,
                          summary_arrow_sz * 0.8, col = edge_col)
        }

        # Loop label
        if (summary_edge_labels) {
          lbl_x <- loop_cx + loop_radius * 1.3 * cos(loop_rot)
          lbl_y <- loop_cy + loop_radius * 1.3 * sin(loop_rot)
          graphics::text(lbl_x, lbl_y,
                         labels = round(sw_labels[i, i], edge_label_digits),
                         cex = summary_edge_label_size,
                         col = edge_label_color)
        }
      }
    }
  }

  # 4. Summary labels - perpendicular to loop direction (solution 5)
  if (summary_labels) {
    for (i in seq_len(n_clusters)) {
      loop_rot <- atan2(ty[i] - mean(ty), tx[i] - mean(tx))
      perp <- loop_rot + pi/2
      lbl_x <- tx[i] + 0.45 * cos(perp)
      lbl_y <- ty[i] + 0.45 * sin(perp)
      graphics::text(lbl_x, lbl_y,
                     labels = cluster_names[i],
                     cex = summary_label_size,
                     col = summary_label_color)
    }
  }

  # ============================================================================
  # BOTTOM LAYER (detailed clusters)
  # ============================================================================

  # Between-cluster edges (shell to shell)
  shell_rx <- shape_size
  shell_ry <- shape_size * compress
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (i != j && bw[i, j] > minimum) {
          p1 <- shell_edge(bx[i], by[i], bx[j], by[j], shell_rx, shell_ry)
          p2 <- shell_edge(bx[j], by[j], bx[i], by[i], shell_rx, shell_ry)
          lwd <- between_edge_width_range[1] +
            (between_edge_width_range[2] - between_edge_width_range[1]) *
            bw[i, j] / max_sw
          graphics::segments(p1[1], p1[2], p2[1], p2[2],
                             col = grDevices::adjustcolor(colors[i], between_edge_alpha),
                             lwd = lwd)
        }
      }
    }
  }

  # Cluster shells and nodes
  for (i in seq_len(n_clusters)) {
    idx <- cluster_idx[[i]]
    n_nodes <- length(idx)
    cl_name <- cluster_names[i]

    # Shell (ellipse for table-view perspective)
    theta <- seq(0, 2 * pi, length.out = 60)
    shell_x <- shape_size * cos(theta)
    shell_y <- shape_size * sin(theta) * compress
    graphics::polygon(
      bx[i] + shell_x,
      by[i] + shell_y,
      border = colors[i],
      col = grDevices::adjustcolor(colors[i], shell_alpha),
      lwd = shell_border_width
    )

    # Node positions (use pre-computed)
    nx <- node_positions[[i]]$x
    ny <- node_positions[[i]]$y

    # Within-cluster edges
    if (n_nodes > 1) {
      # Get within-cluster weights
      within_w <- if (!is.null(cs$within) && cl_name %in% names(cs$within)) {
        cs$within[[cl_name]]$weights
      } else if (!is.null(weights)) {
        w <- weights[idx, idx]
        diag(w) <- 0
        w
      } else {
        NULL
      }

      if (!is.null(within_w)) {
        # Node visual radius and arrow size
        node_vis_r <- node_size * 0.04
        arrow_size <- 0.06

        for (j in seq_len(n_nodes)) {
          for (k in seq_len(n_nodes)) {
            if (j != k) {
              w <- within_w[j, k]
              if (!is.na(w) && w > minimum) {
                lwd <- edge_width_range[1] +
                  (edge_width_range[2] - edge_width_range[1]) * w / max_w
                edge_col <- grDevices::adjustcolor(colors[i], edge_alpha)

                # Calculate edge angle
                angle <- atan2(ny[k] - ny[j], nx[k] - nx[j])

                # Arrow tip at node edge
                tip_x <- nx[k] - node_vis_r * cos(angle)
                tip_y <- ny[k] - node_vis_r * sin(angle)

                # Line ends at arrow base
                line_end_x <- tip_x - arrow_size * cos(angle)
                line_end_y <- tip_y - arrow_size * sin(angle)

                # Draw edge line
                graphics::segments(nx[j], ny[j], line_end_x, line_end_y,
                                   col = edge_col, lwd = lwd)

                # Draw filled arrow using splot style
                draw_arrow_base(tip_x, tip_y, angle, arrow_size,
                                col = edge_col, border = edge_col, lwd = lwd)

                # Edge label - position at 1/3 along edge (closer to source)
                if (edge_labels) {
                  lbl_x <- nx[j] + (nx[k] - nx[j]) * 0.35
                  lbl_y <- ny[j] + (ny[k] - ny[j]) * 0.35
                  graphics::text(lbl_x, lbl_y,
                                 labels = round(w, edge_label_digits),
                                 cex = edge_label_size,
                                 col = edge_label_color)
                }
              }
            }
          }
        }
      }
    }

    # Nodes as PIE CHARTS showing self-transition proportion
    node_pie_r <- node_size * 0.035  # Pie radius in plot units

    for (ni in seq_along(nx)) {
      # Get self-transition proportion for this node
      self_val <- 0
      other_val <- 1
      if (!is.null(within_w)) {
        node_row <- within_w[ni, ]
        self_val <- within_w[ni, ni]  # Diagonal = self-transition
        other_val <- sum(node_row) - self_val
        total <- self_val + other_val
        if (total > 0) {
          self_prop <- self_val / total
        } else {
          self_prop <- 0
        }
      } else {
        self_prop <- 0
      }

      # Draw "other" slice (light version of cluster color)
      if (self_prop < 1) {
        theta <- seq(0, 2 * pi, length.out = 40)
        graphics::polygon(nx[ni] + node_pie_r * cos(theta),
                          ny[ni] + node_pie_r * sin(theta),
                          col = grDevices::adjustcolor(colors[i], 0.3),
                          border = NA)
      }

      # Draw "self" slice (full cluster color)
      if (self_prop > 0.001) {
        start_angle <- pi / 2
        end_angle <- start_angle - self_prop * 2 * pi
        n_pts <- max(10, round(40 * self_prop))
        angles <- seq(start_angle, end_angle, length.out = n_pts)
        slice_x <- c(nx[ni], nx[ni] + node_pie_r * cos(angles), nx[ni])
        slice_y <- c(ny[ni], ny[ni] + node_pie_r * sin(angles), ny[ni])
        graphics::polygon(slice_x, slice_y, col = colors[i], border = NA)
      }

      # Border
      theta <- seq(0, 2 * pi, length.out = 40)
      graphics::lines(nx[ni] + node_pie_r * cos(theta),
                      ny[ni] + node_pie_r * sin(theta),
                      col = node_border_color, lwd = 1.5)
    }

    # Node labels - position on side (left or right only)
    if (isTRUE(show_labels)) {
      lbl_text <- display_labels[idx]
      if (!is.null(label_abbrev)) {
        lbl_text <- abbrev_label(lbl_text, label_abbrev, n)
      }
      lbl_cex <- if (is.null(label_size)) 0.6 else label_size

      # Use original angles for outward direction, but only left or right
      node_angles <- node_positions[[i]]$angles
      for (ni in seq_along(nx)) {
        angle <- node_angles[ni]
        # Only use left (pos=2) or right (pos=4) based on angle
        if (abs(angle) < pi/2) {
          lbl_pos <- 4  # right
        } else {
          lbl_pos <- 2  # left
        }
        graphics::text(nx[ni], ny[ni], labels = lbl_text[ni], cex = lbl_cex,
                       pos = lbl_pos, offset = 0.4, col = label_color)
      }
    }
  }

  # Title and subtitle
  if (!is.null(title)) {
    graphics::title(main = title, cex.main = title_size)
  }
  if (!is.null(subtitle)) {
    graphics::title(sub = subtitle, cex.sub = subtitle_size, line = -0.5)
  }

  # Legend (positioned based on legend_position)
  if (legend && legend_position != "none") {
    legend_x <- switch(legend_position,
      "right" = max(bx) + shape_size * 0.5,
      "left" = min(bx) - shape_size * 0.5,
      "top" = mean(c(min(bx), max(bx))),
      "bottom" = mean(c(min(bx), max(bx))),
      max(bx) + shape_size * 0.5  # default to right
    )
    legend_y <- switch(legend_position,
      "right" = mean(c(max(by), min(ty))),
      "left" = mean(c(max(by), min(ty))),
      "top" = max(ty) + 1,
      "bottom" = min(by) - 1,
      mean(c(max(by), min(ty)))  # default
    )
    legend_horiz <- legend_position %in% c("top", "bottom")
    legend_xjust <- switch(legend_position,
      "right" = 0,
      "left" = 1,
      "top" = 0.5,
      "bottom" = 0.5,
      0
    )
    legend_yjust <- switch(legend_position,
      "right" = 0.5,
      "left" = 0.5,
      "top" = 0,
      "bottom" = 1,
      0.5
    )

    graphics::legend(
      x = legend_x,
      y = legend_y,
      legend = cluster_names, pch = 21, pt.bg = colors,
      col = "gray30", pt.cex = legend_pt_size, cex = legend_size, bty = "n",
      xjust = legend_xjust, yjust = legend_yjust, horiz = legend_horiz
    )
  }

  invisible(cs)
}

#' mcml - Deprecated alias for cluster_summary
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use \code{\link{cluster_summary}} instead. This function is provided for
#' backward compatibility only.
#'
#' @param x Weight matrix, tna object, cograph_network, or cluster_summary object
#' @param cluster_list Named list of node vectors per cluster
#' @param aggregation How to aggregate edge weights: "sum", "mean", "max"
#' @param as_tna Logical. If TRUE, return a tna-compatible object
#' @param nodes Node metadata
#' @param within Logical. Compute within-cluster matrices
#' @return A cluster_summary object (or tna if as_tna = TRUE)
#' @export
#' @keywords internal
mcml <- function(x,
                 cluster_list = NULL,
                 aggregation = c("sum", "mean", "max"),
                 as_tna = FALSE,
                 nodes = NULL,
                 within = TRUE) {

  aggregation <- match.arg(aggregation)

  # Call cluster_summary with mapped parameters
  cs <- cluster_summary(x, cluster_list, method = aggregation, type = "tna",
                        compute_within = within)

  # Store nodes metadata for display labels (backward compat)
  if (is.data.frame(nodes)) {
    cs$nodes_df <- nodes
  } else if (inherits(x, "cograph_network") && !is.null(x$nodes)) {
    cs$nodes_df <- x$nodes
  }

  if (as_tna) {
    return(as_tna(cs))
  }

  cs
}
