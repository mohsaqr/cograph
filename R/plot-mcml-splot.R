# =============================================================================
# EXPERIMENTAL / SANDBOX — plot_mcml() with the TOP (summary) layer restyled
# as splot-quality DONUT nodes, in the composite two-layer figure.
#
# This file is intentionally SEPARATE from R/plot-mcml.R and does NOT touch,
# import, or modify plot_mcml(). It is a full working copy of plot_mcml()
# renamed to plot_mcml_donut(), where ONLY the top-layer node rendering is
# swapped from the hand-rolled pie polygons to splot's draw_donut_node_base()
# primitive. The bottom layer and everything else are byte-identical to the
# real plot_mcml(). Marked @noRd: no NAMESPACE export, no man page, zero
# public-surface footprint. Call via cograph:::plot_mcml_donut() after
# load_all(). For review/approval before any integration.
#
# Two new args vs plot_mcml():
#   summary_donut_inner_ratio  hole size of the top-layer donut ring (0-1)
#   summary_donut_show_value   print the fill proportion in the donut center
# =============================================================================

#' @noRd
plot_mcml_donut <- function(
    x,
    cluster_list = NULL,
    mode = c("weights", "tna"),
    theme = c("rich", "light", "original"),
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
    node_size = 2.6,
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
    summary_arrow_size = 0.10,
    # Donut top-layer styling (NEW vs plot_mcml)
    summary_donut_inner_ratio = 0.6,
    summary_donut_show_value = FALSE,
    # Donut detail-node styling (NEW vs plot_mcml)
    node_donut = TRUE,
    node_donut_inner_ratio = 0.55,
    # Summary pie semantics
    summary_pie = c("inits", "self"),
    # Edge control
    between_arrows = FALSE,
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
    shell_border_width = 0.75,
    # Node styling
    node_border_color = "gray30",
    node_border_width = 0.4,
    summary_border_color = "gray20",
    summary_border_width = 0.6,
    # Label styling
    label_color = "gray20",
    label_position = 3,
    directed = NULL,
    ...
) {
  aggregation <- match.arg(aggregation)
  mode <- match.arg(mode)
  summary_pie <- match.arg(summary_pie)
  if (!(is.null(directed) ||
        (is.logical(directed) && length(directed) == 1L &&
         !is.na(directed)))) {
    stop("'directed' must be TRUE, FALSE, or NULL (auto-detect).",
         call. = FALSE)
  }

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

  # --- Theme presets --------------------------------------------------------
  # "rich"     curved splot edges + donut nodes (the default look)
  # "light"    same as rich but no cluster-shell outline + softer fill
  # "original" delegate to the real plot_mcml(): pie nodes, straight edges
  theme <- match.arg(theme)
  if (identical(theme, "original")) {
    return(invisible(do.call(plot_mcml, list(
      x = x, cluster_list = cluster_list, mode = mode,
      aggregation = aggregation, summary_pie = summary_pie,
      colors = colors, directed = directed,
      title = title, subtitle = subtitle,
      node_size = node_size,
      node_border_color = node_border_color,
      node_border_width = node_border_width,
      summary_border_color = summary_border_color,
      summary_border_width = summary_border_width,
      shell_border_width = shell_border_width, shell_alpha = shell_alpha
    ))))
  }
  if (identical(theme, "light")) {
    if (!"shell_border_width" %in% explicit_args) shell_border_width <- 0
    if (!"shell_alpha" %in% explicit_args) shell_alpha <- 0.10
  }

  # ============================================================================
  # Get or compute cluster_summary
  # ============================================================================

  # Convert communities object to named list
  if (inherits(cluster_list, "cograph_communities")) {
    cluster_list <- split(cluster_list$node, cluster_list$community)
    names(cluster_list) <- paste0("C", names(cluster_list))
  }

  if (inherits(x, c("cluster_summary", "mcml", "mcml_pc"))) {
    cs <- x
    # directed = NULL: auto-detect from the summary's own metadata
    if (is.null(directed)) {
      directed <- !isFALSE(cs$meta$directed)
    }
  } else {
    # Extract nodes_df for display labels
    nodes_df <- NULL
    if (inherits(x, "cograph_network")) {
      nodes_df <- x$nodes
    }
    if (is.data.frame(nodes)) {
      nodes_df <- nodes
    }

    # directed = NULL: prefer the object's own directedness flag, else
    # fall back to matrix symmetry (same contract as splot()).
    if (is.null(directed)) {
      obj_directed <- if (!is.matrix(x)) x$directed else NULL
      directed <- if (is.logical(obj_directed) &&
                      length(obj_directed) == 1L && !is.na(obj_directed)) {
        obj_directed
      } else {
        wm <- if (is.matrix(x)) x else x$weights
        !(is.matrix(wm) && is_symmetric_matrix(wm))
      }
    }

    # Map aggregation to method. Undirected input aggregates with
    # type = "cooccurrence" (symmetrized counts): the "tna"
    # row-normalization would make even symmetric weights asymmetric,
    # which upper-triangle (undirected) drawing cannot represent.
    cs <- cluster_summary(x, cluster_list, method = aggregation,
                          type = if (directed) "tna" else "cooccurrence",
                          compute_within = TRUE)

    # Store nodes_df and display_labels for visualization
    cs$nodes_df <- nodes_df
  }

  # Undirected rendering: no arrowheads anywhere, and each symmetric pair
  # is drawn once (upper triangle) so edges are not overplotted twice.
  if (!directed) {
    summary_arrows <- FALSE
    between_arrows <- FALSE
  }
  # Edge-label position along the edge (loop-invariant). Directed labels
  # sit off-center so reciprocal labels don't collide; undirected edges
  # are single, so the label sits at the midpoint.
  summary_lbl_frac <- if (directed) 0.7 else 0.5
  within_lbl_frac <- if (directed) 0.35 else 0.5

  # ============================================================================
  # Extract data from cluster_summary
  # ============================================================================

  cluster_list <- cs$cluster_members
  cluster_names <- names(cluster_list)
  n_clusters <- cs$meta$n_clusters
  n <- cs$meta$n_nodes

  # Get original weight matrix for within-cluster visualization
  # We need raw weights, so re-extract if needed
  if (inherits(x, c("cluster_summary", "mcml", "mcml_pc"))) {
    # Use clusters$X$weights directly
    weights <- NULL
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

  # Macro weights (diagonal already contains intra-cluster retention)
  bw <- cs$macro$weights

  # Undirected drawing reads only the upper triangle, so asymmetric
  # weights would be silently misrepresented — warn instead.
  if (!directed) {
    within_symmetric <- vapply(cs$clusters, function(cl) {
      !is.matrix(cl$weights) || is_symmetric_matrix(cl$weights)
    }, logical(1))
    if (!is_symmetric_matrix(bw) || !all(within_symmetric)) {
      warning("directed = FALSE but the aggregated weights are not ",
              "symmetric; only the upper triangle is drawn. Symmetrize ",
              "the weights or use directed = TRUE.", call. = FALSE)
    }
  }

  # Pre-compute rounded weights for edge visibility and labels
  bw_r <- round(bw, edge_label_digits)

  # Format label: drop leading zero (0.35 -> .35, -0.35 -> -.35)
  fmt_lbl <- function(v) {
    if (v == 0) return(NULL) # nocov — callers guard bw_r != 0
    sub("^(-?)0\\.", "\\1.", as.character(v))
  }

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
    if (!is.null(cs$clusters)) {
      all_within_w <- unlist(lapply(cs$clusters, function(w) w$weights))
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

  # Reserve top/bottom margin only when titles/subtitles are set — otherwise
  # graphics::title() clips against the tight 0.2-line edge.
  top_mar <- if (!is.null(title)) max(2.5, title_size * 2) else 0.2
  bot_mar <- if (!is.null(subtitle)) max(1.8, subtitle_size * 2) else 0.2
  old_par <- graphics::par(mar = c(bot_mar, 0.2, top_mar, 0.2))
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

  summary_arrow_sz <- summary_arrow_size
  # Pie chart radius: summary_size default 4 -> radius 0.35 (backwards compat)
  pie_radius <- summary_size * 0.0875

  # Pre-compute pie proportions for each cluster based on summary_pie mode.
  # "inits": colored slice = cluster's share of the initial distribution
  #          (cs$macro$inits[i]), gray = 1 - that value. Sums to 1 across
  #          clusters, so slice answers "how often do sequences start here?".
  # "self":  colored slice = cluster's self-retention share of out-strength
  #          (bw[i, i] / rowSums(bw)[i]), a per-cluster stickiness.
  pie_props <- if (summary_pie == "inits") {
    inits <- cs$macro$inits
    if (is.null(inits)) rep(0, n_clusters) else as.numeric(inits)
  } else {
    row_tot <- rowSums(bw)
    ifelse(row_tot > 0, diag(bw) / row_tot, 0)
  }

  # Top-layer rendering order now follows splot: EDGES first, self-loops,
  # then DONUT nodes on top (opaque rings tuck the edge ends under the node),
  # then labels. This is the key change that makes the summary read as a real
  # splot network rather than the old pie+segment toy.

  # 1. Summary edges via splot's qgraph-style curved-edge engine. Directed
  #    edges get a curvature so reciprocal pairs (i->j and j->i) bow to
  #    opposite sides instead of overplotting; undirected edges stay straight.
  summary_curve <- if (directed) 0.25 else 0
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (i != j && (directed || i < j) &&
            bw[i, j] > minimum && bw_r[i, j] != 0) {
          lwd <- summary_edge_width_range[1] +
            (summary_edge_width_range[2] - summary_edge_width_range[1]) *
            bw[i, j] / max_sw
          edge_col <- grDevices::adjustcolor(colors[i], summary_edge_alpha)
          angle <- atan2(ty[j] - ty[i], tx[j] - tx[i])

          # Boundary-to-boundary endpoints (donut outer radius = pie_radius);
          # draw_curved_edge_base lands the arrow tip exactly at (tip_x, tip_y).
          src_x <- tx[i] + pie_radius * cos(angle)
          src_y <- ty[i] + pie_radius * sin(angle)
          tip_x <- tx[j] - pie_radius * cos(angle)
          tip_y <- ty[j] - pie_radius * sin(angle)

          draw_curved_edge_base(
            src_x, src_y, tip_x, tip_y,
            curve = summary_curve,
            col = edge_col, lwd = lwd,
            arrow = summary_arrows, asize = summary_arrow_sz
          )

          if (summary_edge_labels) {
            lbl_txt <- fmt_lbl(bw_r[i, j])
            if (!is.null(lbl_txt)) {
              # Place the label on the bowed side of the curve.
              mx <- (src_x + tip_x) / 2
              my <- (src_y + tip_y) / 2
              perp <- angle + pi / 2
              seg_len <- sqrt((tip_x - src_x)^2 + (tip_y - src_y)^2)
              off <- summary_curve * seg_len * 0.25 + 0.08
              graphics::text(mx + off * cos(perp), my + off * sin(perp),
                             labels = lbl_txt,
                             cex = summary_edge_label_size,
                             col = edge_label_color)
            }
          }
        }
      }
    }
  }

  # 2. Self-loops via splot's self-loop primitive (same one the bottom layer
  #    uses), pointing outward from the summary arrangement center.
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      if (bw[i, i] > minimum && bw_r[i, i] != 0) {
        lwd <- summary_edge_width_range[1] +
          (summary_edge_width_range[2] - summary_edge_width_range[1]) *
          bw[i, i] / max_sw
        edge_col <- grDevices::adjustcolor(colors[i], summary_edge_alpha)
        loop_rot <- atan2(ty[i] - mean(ty), tx[i] - mean(tx))

        draw_self_loop_base(
          x = tx[i], y = ty[i], node_size = pie_radius,
          col = edge_col, lwd = lwd,
          rotation = loop_rot, arrow = summary_arrows,
          asize = summary_arrow_sz * 0.8
        )

        if (summary_edge_labels) {
          lbl_txt <- fmt_lbl(bw_r[i, i])
          if (!is.null(lbl_txt)) {
            lbl_x <- tx[i] + pie_radius * 2.2 * cos(loop_rot)
            lbl_y <- ty[i] + pie_radius * 2.2 * sin(loop_rot)
            graphics::text(lbl_x, lbl_y, labels = lbl_txt,
                           cex = summary_edge_label_size,
                           col = edge_label_color)
          }
        }
      }
    }
  }

  # 3. Draw the DONUT nodes on top of the edges (opaque ring + white outer
  #    disc hides the edge stubs behind each node, the splot layering).
  #    Fill = the same proportion the old pie encoded (pie_props).
  for (i in seq_len(n_clusters)) {
    self_prop <- pie_props[i]
    if (is.na(self_prop) || self_prop < 0) self_prop <- 0
    if (self_prop > 1) self_prop <- 1

    draw_donut_node_base(
      x = tx[i], y = ty[i], size = pie_radius,
      values = self_prop,
      colors = colors[i],
      inner_ratio = summary_donut_inner_ratio,
      bg_color = "gray90",
      center_color = "white",
      border.col = summary_border_color,
      border.width = summary_border_width,
      show_value = isTRUE(summary_donut_show_value),
      value_cex = summary_label_size * 0.75,
      value_col = summary_label_color,
      value_digits = 2
    )
  }

  # 4. Summary labels - perpendicular to loop direction (solution 5)
  if (summary_labels) {
    lbl_offset <- 0.45
    for (i in seq_len(n_clusters)) {
      if (summary_label_position == 1) {
        lbl_x <- tx[i]; lbl_y <- ty[i] - lbl_offset
      } else if (summary_label_position == 2) {
        lbl_x <- tx[i] - lbl_offset; lbl_y <- ty[i]
      } else if (summary_label_position == 4) {
        lbl_x <- tx[i] + lbl_offset; lbl_y <- ty[i]
      } else {
        lbl_x <- tx[i]; lbl_y <- ty[i] + lbl_offset
      }
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
  between_arrow_sz <- 0.12
  if (max_sw > 0) {
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (i != j && (directed || i < j) &&
            bw[i, j] > minimum && bw_r[i, j] != 0) {
          p1 <- shell_edge(bx[i], by[i], bx[j], by[j], shell_rx, shell_ry)
          p2 <- shell_edge(bx[j], by[j], bx[i], by[i], shell_rx, shell_ry)
          lwd <- between_edge_width_range[1] +
            (between_edge_width_range[2] - between_edge_width_range[1]) *
            bw[i, j] / max_sw
          edge_col <- grDevices::adjustcolor(colors[i], between_edge_alpha)
          if (between_arrows) {
            angle <- atan2(p2[2] - p1[2], p2[1] - p1[1])
            tip_x <- p2[1]
            tip_y <- p2[2]
            line_end_x <- tip_x - between_arrow_sz * cos(angle)
            line_end_y <- tip_y - between_arrow_sz * sin(angle)
            graphics::segments(p1[1], p1[2], line_end_x, line_end_y,
                               col = edge_col, lwd = lwd)
            draw_arrow_base(tip_x, tip_y, angle, between_arrow_sz,
                            col = edge_col, border = edge_col, lwd = lwd)
          } else {
            graphics::segments(p1[1], p1[2], p2[1], p2[2],
                               col = edge_col, lwd = lwd)
          }
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
      border = if (shell_border_width > 0) colors[i] else NA,
      col = grDevices::adjustcolor(colors[i], shell_alpha),
      lwd = if (shell_border_width > 0) shell_border_width else 1
    )

    # Node positions (use pre-computed)
    nx <- node_positions[[i]]$x
    ny <- node_positions[[i]]$y

    # Within-cluster weights (used for edges and pie charts)
    within_w <- NULL
    if (n_nodes > 1) {
      # Get within-cluster weights
      if (!is.null(cs$clusters) && cl_name %in% names(cs$clusters)) {
        within_w <- cs$clusters[[cl_name]]$weights
      } else if (!is.null(weights)) { # nocov start
        within_w <- weights[idx, idx] # nocov end
      }

      if (!is.null(within_w)) {
        # Node visual radius and arrow size
        node_vis_r <- node_size * 0.04
        arrow_size <- 0.06
        edge_col <- grDevices::adjustcolor(colors[i], edge_alpha)

        for (j in seq_len(n_nodes)) {
          for (k in seq_len(n_nodes)) {
            # Undirected: draw each symmetric pair once (upper triangle)
            if (!directed && k < j) next
            w <- within_w[j, k]
            w_r <- round(w, edge_label_digits)
            if (!is.na(w) && w > minimum && w_r != 0) {
              lwd <- edge_width_range[1] +
                (edge_width_range[2] - edge_width_range[1]) * w / max_w

              if (j == k) {
                draw_self_loop_base(
                  x = nx[j], y = ny[j], node_size = node_vis_r,
                  col = edge_col, lwd = lwd,
                  arrow = directed, asize = arrow_size
                )
              } else {
                # Calculate edge angle
                angle <- atan2(ny[k] - ny[j], nx[k] - nx[j])

                # Arrow tip at node edge
                tip_x <- nx[k] - node_vis_r * cos(angle)
                tip_y <- ny[k] - node_vis_r * sin(angle)

                if (directed) {
                  # Line ends at arrow base
                  line_end_x <- tip_x - arrow_size * cos(angle)
                  line_end_y <- tip_y - arrow_size * sin(angle)

                  # Draw edge line
                  graphics::segments(nx[j], ny[j], line_end_x, line_end_y,
                                     col = edge_col, lwd = lwd)

                  # Draw filled arrow using splot style
                  draw_arrow_base(tip_x, tip_y, angle, arrow_size,
                                  col = edge_col, border = edge_col, lwd = lwd)
                } else {
                  # Plain segment from node edge to node edge, no arrowhead
                  src_x <- nx[j] + node_vis_r * cos(angle)
                  src_y <- ny[j] + node_vis_r * sin(angle)
                  graphics::segments(src_x, src_y, tip_x, tip_y,
                                     col = edge_col, lwd = lwd)
                }
              }

              # Edge label
              if (edge_labels) {
                lbl_txt <- fmt_lbl(w_r)
                if (!is.null(lbl_txt)) {
                  if (j == k) {
                    lbl_x <- nx[j]
                    lbl_y <- ny[j] + node_vis_r * 2.5
                  } else {
                    lbl_x <- nx[j] + (nx[k] - nx[j]) * within_lbl_frac
                    lbl_y <- ny[j] + (ny[k] - ny[j]) * within_lbl_frac
                  }
                  graphics::text(lbl_x, lbl_y,
                                 labels = lbl_txt,
                                 cex = edge_label_size,
                                 col = edge_label_color)
                }
              }
            }
          }
        }
      }
    }

    # Detail node rendering. When node_shape is "circle" (default), draw a
    # pie chart encoding self-transition proportion. For any other shape,
    # draw a solid shape in cluster color — the pie semantics only make
    # sense on a circle.
    node_pie_r <- node_size * 0.035  # Radius in plot units

    for (ni in seq_along(nx)) {
      # Global node index (into node_shape vector)
      gi <- idx[ni]
      this_shape <- node_shape[gi]

      if (this_shape == "circle") {
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

        if (node_donut) {
          # Donut detail node: same self-transition proportion, rendered
          # through splot's donut primitive so it matches the top layer.
          # bg ring is a light tint of the cluster color (the old "other"
          # slice look); filled arc is the full cluster color.
          draw_donut_node_base(
            x = nx[ni], y = ny[ni], size = node_pie_r,
            values = self_prop,
            colors = colors[i],
            inner_ratio = node_donut_inner_ratio,
            bg_color = grDevices::adjustcolor(colors[i], 0.3),
            center_color = "white",
            border.col = node_border_color,
            border.width = node_border_width,
            show_value = FALSE
          )
        } else {
          # Original pie detail node (kept for comparison / opt-out).
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
                          col = node_border_color, lwd = node_border_width)
        }
      } else {
        draw_node_base(
          x = nx[ni], y = ny[ni],
          size = node_pie_r,
          shape = this_shape,
          col = colors[i],
          border.col = node_border_color,
          border.width = node_border_width
        )
      }
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
    graphics::title(sub = subtitle, cex.sub = subtitle_size, line = bot_mar - 1)
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
