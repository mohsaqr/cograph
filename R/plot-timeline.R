#' Plot Cluster Timeline Network
#'
#' Horizontal or vertical layout of clusters with within-cluster detail
#' networks and curved between-cluster edges connecting adjacent clusters.
#' Supports multi-layer mode for visualizing the same clustered network
#' across multiple time points or conditions.
#'
#' @param x Weight matrix, tna object, cograph_network, or cluster_summary
#'   object. Used for single-layer mode. Ignored when \code{layers} is provided.
#' @param cluster_list Named list of node vectors per cluster. Required for
#'   matrix input. Ignored if x is cluster_summary.
#' @param layers Named list of weight matrices (or tna/cograph_network objects)
#'   for multi-layer mode. Each element is one layer (e.g., time point).
#'   When provided, \code{x} is ignored. Default NULL (single-layer mode).
#' @param orientation Layout direction: "horizontal" (clusters left-to-right)
#'   or "vertical" (clusters top-to-bottom). Default "horizontal".
#' @param mode What values to use: "weights" (raw aggregated) or "tna"
#'   (row-normalized). Default "weights".
#' @param aggregation How to aggregate: "sum", "mean", "max". Default "sum".
#'   Ignored if x is cluster_summary.
#' @param minimum Edge weight threshold for within-cluster edges. Default 0.02.
#' @param between_minimum Edge weight threshold for between-cluster edges.
#'   Default 0.10. Increase to show fewer between-cluster connections.
#' @param spacing Distance between cluster centers. Default 4.
#' @param shape_size Cluster shell size. Default 1.8.
#' @param node_radius_scale Node ring radius within cluster as proportion of
#'   shape_size. Default 0.85.
#' @param node_size Node circle size. Default 1.8.
#' @param colors Cluster colors. Default NULL (auto palette).
#' @param shell_alpha Shell fill transparency. Default 0.15.
#' @param shell_border_width Shell border line width. Default 2.
#' @param node_border_color Node border color. Default "gray30".
#' @param edge_width_range Min/max within-cluster edge width. Default c(0.3, 1.3).
#' @param edge_alpha Within-cluster edge transparency. Default 0.35.
#' @param arrows Show arrows on within-cluster edges. Default TRUE.
#' @param arrow_size Arrow head size. Default 0.06.
#' @param between_curve_strength How much between-cluster curves bow outward.
#'   Default 0.8.
#' @param between_alpha Between-cluster edge transparency. Default 0.12.
#' @param between_lwd_range Min/max between-cluster edge width. Default
#'   c(0.3, 1.5).
#' @param show_labels Show node labels. Default TRUE.
#' @param label_size Label text size. Default 0.5.
#' @param label_color Label text color. Default "gray20".
#' @param label_abbrev Label abbreviation: NULL (none), integer (max chars),
#'   or "auto". Default NULL.
#' @param cluster_labels Show cluster names. Default TRUE.
#' @param cluster_label_size Cluster label text size. Default 1.1.
#' @param edge_labels Show weight labels on within-cluster edges. Default FALSE.
#' @param edge_label_size Edge label text size. Default 0.5.
#' @param edge_label_color Edge label text color. Default "gray40".
#' @param edge_label_digits Decimal places for edge labels. Default 2.
#' @param layer_gap Vertical gap between layers in multi-layer mode. Default
#'   NULL (auto-calculated).
#' @param layer_label_size Layer name text size (multi-layer). Default 1.2.
#' @param layer_label_color Layer name text color. Default "gray30".
#' @param cross_layer_alpha Cross-layer dashed connection transparency.
#'   Default 0.4.
#' @param cross_layer_lwd Cross-layer dashed connection line width.
#'   Default 0.6.
#' @param global_scale Use global weight scaling across all layers. Default TRUE.
#' @param legend Show legend. Default TRUE.
#' @param legend_position Legend position: "bottom", "top", "right", "left",
#'   "none". Default "bottom".
#' @param legend_size Legend text size. Default 0.9.
#' @param title Main title. Default NULL.
#' @param subtitle Subtitle. Default NULL.
#' @param title_size Title text size. Default 1.4.
#' @param nodes Node metadata data frame. Default NULL.
#' @param ... Unused.
#'
#' @return Invisibly returns a cluster_summary (single-layer) or list of
#'   cluster_summary objects (multi-layer).
#'
#' @export
#' @seealso \code{\link{cluster_summary}}, \code{\link{plot_mcml}}
#'
#' @examples
#' # Create test matrix
#' mat <- matrix(runif(36), 6, 6)
#' diag(mat) <- 0
#' colnames(mat) <- rownames(mat) <- LETTERS[1:6]
#' clusters <- list(Group1 = c("A", "B", "C"), Group2 = c("D", "E", "F"))
#'
#' # Horizontal (default)
#' plot_time_line(mat, clusters)
#'
#' # Vertical
#' plot_time_line(mat, clusters, orientation = "vertical")
#'
#' # Multi-layer
#' mat2 <- matrix(runif(36), 6, 6)
#' diag(mat2) <- 0
#' colnames(mat2) <- rownames(mat2) <- LETTERS[1:6]
#' plot_time_line(layers = list("Time 1" = mat, "Time 2" = mat2),
#'                cluster_list = clusters)
plot_time_line <- function(
    x = NULL,
    cluster_list = NULL,
    layers = NULL,
    orientation = c("horizontal", "vertical"),
    mode = c("weights", "tna"),
    aggregation = c("sum", "mean", "max"),
    minimum = 0.02,
    between_minimum = 0.10,
    # Layout
    spacing = 4,
    shape_size = 1.8,
    node_radius_scale = 0.85,
    node_size = 1.8,
    # Colors
    colors = NULL,
    # Shell styling
    shell_alpha = 0.15,
    shell_border_width = 2,
    node_border_color = "gray30",
    # Within-cluster edges
    edge_width_range = c(0.3, 1.3),
    edge_alpha = 0.35,
    arrows = TRUE,
    arrow_size = 0.06,
    # Between-cluster edges
    between_curve_strength = 0.8,
    between_alpha = 0.12,
    between_lwd_range = c(0.3, 1.5),
    # Labels
    show_labels = TRUE,
    label_size = 0.5,
    label_color = "gray20",
    label_abbrev = NULL,
    cluster_labels = TRUE,
    cluster_label_size = 1.1,
    # Edge labels
    edge_labels = FALSE,
    edge_label_size = 0.5,
    edge_label_color = "gray40",
    edge_label_digits = 2,
    # Multi-layer
    layer_gap = NULL,
    layer_label_size = 1.2,
    layer_label_color = "gray30",
    cross_layer_alpha = 0.4,
    cross_layer_lwd = 0.6,
    global_scale = TRUE,
    # Legend
    legend = TRUE,
    legend_position = "bottom",
    legend_size = 0.9,
    # Title
    title = NULL,
    subtitle = NULL,
    title_size = 1.4,
    # Nodes
    nodes = NULL,
    ...
) {
  orientation <- match.arg(orientation)
  aggregation <- match.arg(aggregation)
  mode <- match.arg(mode)
  is_vertical <- orientation == "vertical"

  # When mode = "tna", show edge labels by default
  explicit_args <- names(match.call())
  if (mode == "tna" && !"edge_labels" %in% explicit_args) {
    edge_labels <- TRUE
  }

  # ==========================================================================
  # Multi-layer vs single-layer
  # ==========================================================================

  multi_layer <- !is.null(layers)
  if (multi_layer) {
    stopifnot(is.list(layers), length(layers) >= 1)
    if (is.null(names(layers))) {
      names(layers) <- paste("Layer", seq_along(layers))
    }
    n_layers <- length(layers)
    layer_names <- names(layers)

    # Extract weight matrices from each layer
    layer_weights <- lapply(layers, function(lx) {
      if (inherits(lx, "cograph_network")) {
        if (!is.null(lx$weights)) lx$weights else to_matrix(lx)
      } else if (inherits(lx, "tna")) {
        lx$weights
      } else if (is.matrix(lx)) {
        lx
      } else {
        stop("Each layer must be a matrix, tna, or cograph_network object",
             call. = FALSE)
      }
    })

    # Validate cluster_list
    if (is.null(cluster_list)) {
      stop("cluster_list is required for multi-layer mode", call. = FALSE)
    }

    # Compute cluster_summary per layer
    all_cs <- lapply(layer_weights, function(mat) {
      cluster_summary(mat, cluster_list, method = aggregation, type = "tna",
                      compute_within = TRUE)
    })
  } else {
    # Single-layer mode
    stopifnot(!is.null(x))
    n_layers <- 1
    layer_names <- NULL

    if (inherits(x, "cluster_summary")) {
      all_cs <- list(x)
      layer_weights <- list(NULL)
    } else {
      nodes_df <- NULL
      if (inherits(x, "cograph_network")) nodes_df <- x$nodes
      if (is.data.frame(nodes)) nodes_df <- nodes

      cs <- cluster_summary(x, cluster_list, method = aggregation, type = "tna",
                            compute_within = TRUE)
      cs$nodes_df <- nodes_df
      all_cs <- list(cs)

      # Extract weight matrix
      wt <- if (inherits(x, "cograph_network")) {
        if (!is.null(x$weights)) x$weights else to_matrix(x)
      } else if (inherits(x, "tna")) {
        x$weights
      } else {
        x
      }
      layer_weights <- list(wt)
    }
  }

  # Use first layer for shared metadata
  cs0 <- all_cs[[1]]
  cluster_list <- cs0$clusters
  cluster_names <- names(cluster_list)
  n_clusters <- cs0$meta$n_clusters
  n_total <- cs0$meta$n_nodes

  # Node labels
  wt0 <- layer_weights[[1]]
  if (!is.null(wt0)) {
    lab <- rownames(wt0)
    if (is.null(lab)) lab <- as.character(seq_len(n_total))
  } else {
    lab <- unlist(cluster_list, use.names = FALSE)
  }

  # Display labels
  nodes_df <- if (!is.null(all_cs[[1]]$nodes_df)) all_cs[[1]]$nodes_df else NULL
  display_labels <- if (!is.null(nodes_df)) {
    if ("labels" %in% names(nodes_df)) nodes_df$labels
    else if ("label" %in% names(nodes_df)) nodes_df$label
    else lab
  } else {
    lab
  }

  if (!is.null(label_abbrev)) {
    if (is.numeric(label_abbrev)) {
      display_labels <- substr(display_labels, 1, label_abbrev)
    } else if (identical(label_abbrev, "auto")) {
      max_chars <- max(6, 12 - n_total %/% 5)
      display_labels <- substr(display_labels, 1, max_chars)
    }
  }

  cluster_idx <- lapply(cluster_list, function(nv) match(nv, lab))

  # Colors
  default_pal <- c("#56B4E9", "#E69F00", "#66C2A5", "#FC8D62", "#8DA0CB",
                   "#E78AC3", "#A6D854", "#FFD92F")
  if (is.null(colors)) colors <- rep_len(default_pal, n_clusters)

  # ==========================================================================
  # Layout: cluster positions (base, before layer offsets)
  # ==========================================================================

  if (is_vertical) {
    # Vertical: clusters stacked top-to-bottom
    bx <- rep(0, n_clusters)
    by <- ((n_clusters + 1) / 2 - seq_len(n_clusters)) * spacing * 1.5
    shell_rx <- shape_size * 0.85
    shell_ry <- shape_size * 1.4
  } else {
    # Horizontal: clusters left-to-right
    bx <- (seq_len(n_clusters) - (n_clusters + 1) / 2) * spacing * 1.5
    by <- rep(0, n_clusters)
    shell_rx <- shape_size * 0.85
    shell_ry <- shape_size * 1.4
  }

  # Node positions within clusters (vertical ellipse)
  node_r <- shape_size * node_radius_scale
  node_positions <- vector("list", n_clusters)
  for (i in seq_len(n_clusters)) {
    nn <- length(cluster_idx[[i]])
    if (nn == 1) {
      node_positions[[i]] <- list(x = bx[i], y = by[i], angles = pi / 2)
    } else {
      na <- pi / 2 - (seq_len(nn) - 1) * 2 * pi / nn
      node_positions[[i]] <- list(
        x = bx[i] + node_r * 0.7 * cos(na),
        y = by[i] + node_r * 1.0 * sin(na),
        angles = na
      )
    }
  }

  # Flat node position vectors (base, no layer offset)
  all_nx_base <- unlist(lapply(seq_len(n_clusters), function(i) {
    node_positions[[i]]$x
  }))
  all_ny_base <- unlist(lapply(seq_len(n_clusters), function(i) {
    node_positions[[i]]$y
  }))
  all_idx <- unlist(cluster_idx)
  node_cluster <- rep(seq_len(n_clusters),
                      vapply(cluster_idx, length, integer(1)))

  # ==========================================================================
  # Global weight scaling
  # ==========================================================================

  if (global_scale) {
    global_max_w <- max(vapply(all_cs, function(cs) {
      aw <- unlist(lapply(cs$within, function(w) w$weights))
      if (length(aw) > 0) max(abs(aw), na.rm = TRUE) else 1
    }, numeric(1)))
  } else {
    global_max_w <- 1
  }
  if (is.na(global_max_w) || global_max_w == 0) global_max_w <- 1

  # Between-cluster max
  between_max <- 0
  for (k in seq_len(n_layers)) {
    wt <- layer_weights[[k]]
    if (!is.null(wt)) {
      for (ni in seq_along(all_idx)) {
        for (nj in seq_along(all_idx)) {
          if (abs(node_cluster[ni] - node_cluster[nj]) == 1) {
            w <- wt[all_idx[ni], all_idx[nj]]
            if (!is.na(w) && w > between_max) between_max <- w
          }
        }
      }
    }
  }
  if (between_max == 0) between_max <- 1

  # ==========================================================================
  # Layer offsets (multi-layer stacking)
  # ==========================================================================

  # Layers always stack vertically (layer 1 at bottom, layer N at top)
  if (is.null(layer_gap)) {
    layer_gap <- 2 * shell_ry + 1.5
  }
  # For vertical orientation, stack layers horizontally instead
  if (is_vertical && multi_layer) {
    layer_x_off <- (seq_len(n_layers) - 1) * (2 * shell_rx + layer_gap)
    layer_y_off <- rep(0, n_layers)
  } else {
    layer_x_off <- rep(0, n_layers)
    layer_y_off <- (seq_len(n_layers) - 1) * (layer_gap)
  }

  # ==========================================================================
  # Plot setup
  # ==========================================================================

  pad <- 1.5
  all_bx <- unlist(lapply(seq_len(n_layers), function(k) bx + layer_x_off[k]))
  all_by <- unlist(lapply(seq_len(n_layers), function(k) by + layer_y_off[k]))
  xlim <- range(all_bx) + c(-shell_rx - pad, shell_rx + pad)
  ylim <- c(min(all_by) - shell_ry - pad - 1.0,
            max(all_by) + shell_ry + pad + 1.5)

  old_par <- graphics::par(mar = c(1, 0.5, 2, 0.5))
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

  # ==========================================================================
  # Render each layer (bottom to top / left to right)
  # ==========================================================================

  for (k in seq_len(n_layers)) {
    x_off <- layer_x_off[k]
    y_off <- layer_y_off[k]
    cs <- all_cs[[k]]
    wt <- layer_weights[[k]]

    # Current layer node positions
    cur_nx <- all_nx_base + x_off
    cur_ny <- all_ny_base + y_off

    # Per-layer within-cluster max (if not global)
    if (!global_scale) {
      aw <- unlist(lapply(cs$within, function(w) w$weights))
      global_max_w <- if (length(aw) > 0) max(abs(aw), na.rm = TRUE) else 1
      if (is.na(global_max_w) || global_max_w == 0) global_max_w <- 1
    }

    # ========================================================================
    # Cross-layer dashed connections
    # ========================================================================

    if (k > 1) {
      prev_x_off <- layer_x_off[k - 1]
      prev_y_off <- layer_y_off[k - 1]
      prev_nx <- all_nx_base + prev_x_off
      prev_ny <- all_ny_base + prev_y_off
      for (ni in seq_along(all_idx)) {
        graphics::segments(
          prev_nx[ni], prev_ny[ni],
          cur_nx[ni], cur_ny[ni],
          col = grDevices::adjustcolor("gray70", cross_layer_alpha),
          lty = 2, lwd = cross_layer_lwd
        )
      }
    }

    # ========================================================================
    # Between-cluster curved edges
    # ========================================================================

    if (!is.null(wt)) {
      for (ni in seq_along(all_idx)) {
        for (nj in seq_along(all_idx)) {
          if (node_cluster[ni] + 1 == node_cluster[nj]) {
            w <- wt[all_idx[ni], all_idx[nj]]
            if (!is.na(w) && w > between_minimum) {
              ci <- node_cluster[ni]
              cj <- node_cluster[nj]
              x0 <- cur_nx[ni]; y0 <- cur_ny[ni]
              x3 <- cur_nx[nj]; y3 <- cur_ny[nj]
              lwd <- between_lwd_range[1] +
                (between_lwd_range[2] - between_lwd_range[1]) * w / between_max

              if (is_vertical) {
                # Vertical orientation: curves go left/right
                mid_y <- (by[ci] + by[cj]) / 2 + y_off
                avg_x <- (x0 + x3) / 2
                if (avg_x >= bx[1] + x_off) {
                  ctrl_x <- max(x0, x3) + between_curve_strength
                } else {
                  ctrl_x <- min(x0, x3) - between_curve_strength
                }
                x1 <- ctrl_x; y1 <- y0 + (mid_y - y0) * 0.5
                x2 <- ctrl_x; y2 <- y3 - (y3 - mid_y) * 0.5
              } else {
                # Horizontal orientation: curves go up/down
                mid_x <- (bx[ci] + bx[cj]) / 2 + x_off
                avg_y <- (y0 + y3) / 2
                if (avg_y >= by[1] + y_off) {
                  ctrl_y <- max(y0, y3) + between_curve_strength
                } else {
                  ctrl_y <- min(y0, y3) - between_curve_strength
                }
                x1 <- x0 + (mid_x - x0) * 0.5; y1 <- ctrl_y
                x2 <- x3 - (x3 - mid_x) * 0.5; y2 <- ctrl_y
              }

              t_seq <- seq(0, 1, length.out = 30)
              bx_t <- (1 - t_seq)^3 * x0 +
                3 * (1 - t_seq)^2 * t_seq * x1 +
                3 * (1 - t_seq) * t_seq^2 * x2 +
                t_seq^3 * x3
              by_t <- (1 - t_seq)^3 * y0 +
                3 * (1 - t_seq)^2 * t_seq * y1 +
                3 * (1 - t_seq) * t_seq^2 * y2 +
                t_seq^3 * y3
              graphics::lines(
                bx_t, by_t,
                col = grDevices::adjustcolor(colors[ci], between_alpha),
                lwd = lwd
              )
            }
          }
        }
      }
    }

    # ========================================================================
    # Cluster shells, within-cluster edges, nodes, labels
    # ========================================================================

    for (i in seq_len(n_clusters)) {
      idx <- cluster_idx[[i]]
      nn <- length(idx)
      cl_name <- cluster_names[i]

      cx <- bx[i] + x_off
      cy <- by[i] + y_off

      # Ellipse shell
      theta <- seq(0, 2 * pi, length.out = 80)
      graphics::polygon(
        cx + shell_rx * cos(theta),
        cy + shell_ry * sin(theta),
        border = colors[i],
        col = grDevices::adjustcolor(colors[i], shell_alpha),
        lwd = shell_border_width
      )

      nx <- node_positions[[i]]$x + x_off
      ny <- node_positions[[i]]$y + y_off

      # Within-cluster edges
      if (nn > 1 && !is.null(cs$within) && cl_name %in% names(cs$within)) {
        within_w <- cs$within[[cl_name]]$weights
        node_vis_r <- node_size * 0.04
        for (j in seq_len(nn)) {
          for (mi in seq_len(nn)) {
            if (j != mi) {
              w <- within_w[j, mi]
              if (!is.na(w) && w > minimum) {
                lwd <- edge_width_range[1] +
                  (edge_width_range[2] - edge_width_range[1]) * w / global_max_w
                ec <- grDevices::adjustcolor(colors[i], edge_alpha)
                angle <- atan2(ny[mi] - ny[j], nx[mi] - nx[j])
                tip_x <- nx[mi] - node_vis_r * cos(angle)
                tip_y <- ny[mi] - node_vis_r * sin(angle)

                if (arrows) {
                  le_x <- tip_x - arrow_size * cos(angle)
                  le_y <- tip_y - arrow_size * sin(angle)
                  graphics::segments(nx[j], ny[j], le_x, le_y,
                                     col = ec, lwd = lwd)
                  draw_arrow_base(tip_x, tip_y, angle, arrow_size,
                                  col = ec, border = ec, lwd = lwd)
                } else {
                  graphics::segments(nx[j], ny[j], tip_x, tip_y,
                                     col = ec, lwd = lwd)
                }

                if (edge_labels) {
                  mid_ex <- (nx[j] + nx[mi]) / 2
                  mid_ey <- (ny[j] + ny[mi]) / 2
                  graphics::text(mid_ex, mid_ey,
                                 labels = formatC(w, format = "f",
                                                  digits = edge_label_digits),
                                 cex = edge_label_size,
                                 col = edge_label_color)
                }
              }
            }
          }
        }
      }

      # Pie chart nodes
      within_w <- if (!is.null(cs$within) && cl_name %in% names(cs$within)) {
        cs$within[[cl_name]]$weights
      } else {
        NULL
      }
      node_pie_r <- node_size * 0.035
      for (ni in seq_along(nx)) {
        self_prop <- 0
        if (!is.null(within_w)) {
          sv <- within_w[ni, ni]
          ov <- sum(within_w[ni, ]) - sv
          tt <- sv + ov
          if (tt > 0) self_prop <- sv / tt
        }
        theta2 <- seq(0, 2 * pi, length.out = 40)
        graphics::polygon(
          nx[ni] + node_pie_r * cos(theta2),
          ny[ni] + node_pie_r * sin(theta2),
          col = grDevices::adjustcolor(colors[i], 0.3),
          border = NA
        )
        if (self_prop > 0.001) {
          sa <- pi / 2
          ea <- sa - self_prop * 2 * pi
          a_seq <- seq(sa, ea, length.out = max(10, round(40 * self_prop)))
          graphics::polygon(
            c(nx[ni], nx[ni] + node_pie_r * cos(a_seq), nx[ni]),
            c(ny[ni], ny[ni] + node_pie_r * sin(a_seq), ny[ni]),
            col = colors[i], border = NA
          )
        }
        graphics::lines(
          nx[ni] + node_pie_r * cos(theta2),
          ny[ni] + node_pie_r * sin(theta2),
          col = node_border_color, lwd = 1.5
        )
      }

      # Node labels
      if (show_labels) {
        node_angles <- node_positions[[i]]$angles
        for (ni in seq_along(nx)) {
          angle <- node_angles[ni]
          lbl_pos <- if (abs(angle) < pi / 2) 4 else 2
          lbl_idx <- idx[ni]
          lbl_text <- display_labels[lbl_idx]
          graphics::text(nx[ni], ny[ni], labels = lbl_text,
                         cex = label_size, pos = lbl_pos, offset = 0.35,
                         col = label_color)
        }
      }

      # Cluster name
      if (cluster_labels) {
        if (is_vertical) {
          graphics::text(cx - shell_rx - 0.4, cy,
                         labels = cl_name, cex = cluster_label_size,
                         col = colors[i], font = 2, srt = 90)
        } else {
          graphics::text(cx, cy + shell_ry + 0.4,
                         labels = cl_name, cex = cluster_label_size,
                         col = colors[i], font = 2)
        }
      }
    }

    # Layer label (multi-layer only)
    if (multi_layer && !is.null(layer_names)) {
      if (is_vertical) {
        # Layer label on top
        graphics::text(
          mean(bx) + x_off, max(by) + y_off + shell_ry + 0.6,
          labels = layer_names[k], cex = layer_label_size,
          col = layer_label_color, font = 2
        )
      } else {
        # Layer label on left, angled
        graphics::text(
          min(bx) + x_off - shell_rx - 0.8,
          mean(by) + y_off + shell_ry + 0.3,
          labels = layer_names[k], cex = layer_label_size,
          col = layer_label_color, font = 2, srt = 45
        )
      }
    }
  }

  # ==========================================================================
  # Title
  # ==========================================================================

  if (!is.null(title)) {
    graphics::title(main = title, cex.main = title_size)
  }
  if (!is.null(subtitle)) {
    graphics::mtext(subtitle, side = 3, line = 0, cex = 0.9 * title_size)
  }

  # ==========================================================================
  # Legend
  # ==========================================================================

  if (legend && legend_position != "none") {
    leg_horiz <- legend_position %in% c("top", "bottom")
    leg_x <- switch(legend_position,
      "bottom" = mean(xlim),
      "top" = mean(xlim),
      "left" = xlim[1] + 0.5,
      "right" = xlim[2] - 0.5
    )
    leg_y <- switch(legend_position,
      "bottom" = ylim[1] + 0.5,
      "top" = ylim[2] - 0.3,
      "left" = mean(ylim),
      "right" = mean(ylim)
    )
    leg_xjust <- if (legend_position %in% c("bottom", "top")) 0.5 else 0
    leg_yjust <- if (legend_position == "bottom") 0 else 1

    graphics::legend(
      leg_x, leg_y,
      legend = cluster_names, pch = 21, pt.bg = colors,
      col = "gray30", pt.cex = 1.5, cex = legend_size, bty = "n",
      horiz = leg_horiz, xjust = leg_xjust, yjust = leg_yjust
    )
  }

  if (multi_layer) invisible(all_cs) else invisible(all_cs[[1]])
}
