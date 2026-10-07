#' Multi-Cluster TNA Network Plot
#'
#' Plots a network whose nodes are grouped into clusters. Each cluster is
#' plotted as a shell shape containing its nodes. By default, edges between
#' clusters are aggregated into summary edges and edges within clusters are
#' plotted individually.
#'
#' @param x A tna object, weight matrix, or cograph_network.
#' @param cluster_list Cluster assignment of the nodes. One of
#'   \itemize{
#'     \item a named list of character vectors with the node names of each
#'       cluster; at least two non-overlapping clusters are required;
#'     \item a single string naming a column of the node data (e.g.,
#'       "groups");
#'     \item NULL, in which case the first node-data column found among
#'       "clusters", "cluster", "groups", "group", "community" and "module"
#'       is used.
#'   }
#' @param community Community detection method used to form the clusters.
#'   When given, it overrides \code{cluster_list}. See
#'   \code{\link{detect_communities}} for available methods.
#' @param layout How to arrange the clusters: "circle" (default),
#'   "grid", "horizontal", "vertical".
#' @param spacing Distance between cluster centers. Default 4.
#' @param shape_size Size of each cluster shape (shell radius). Default 1.8.
#' @param node_spacing Radius for node placement within shapes in summary
#'   mode, as a proportion (0-1) of \code{shape_size}. Default 0.5. When
#'   \code{summary_edges = FALSE}, nodes are placed at radius
#'   \code{shape_size}.
#' @param colors Vector of colors for each cluster. NULL (default) uses a
#'   built-in palette.
#' @param shapes Vector of shapes for each cluster. Defaults cycle through
#'   "circle", "square", "diamond", "triangle", "pentagon", "hexagon",
#'   "star", and "cross". In summary mode, shells other than circle,
#'   square, diamond and triangle are plotted as circles.
#' @param edge_colors Vector of edge colors by source cluster. NULL
#'   (default) uses a built-in palette.
#' @param bundle_edges Logical. Order the nodes around each shell by the
#'   direction of the clusters they connect to, so that edges toward the
#'   same cluster leave from neighbouring nodes. Used when
#'   \code{summary_edges = FALSE}. Default TRUE.
#' @param bundle_strength Currently unused.
#' @param summary_edges Logical. Show aggregated summary edges between
#'   clusters instead of individual node edges. Default TRUE.
#' @param aggregation Method for aggregating edge weights between clusters:
#'   "sum" (total flow), "mean" (average strength), "max" (strongest link),
#'   "min" (weakest link), "median", or "density" (normalized by possible edges).
#'   Default "sum". Only used when summary_edges = TRUE.
#' @param within_edges Logical. When summary_edges is TRUE, also show individual
#'   edges within each cluster. Default TRUE.
#' @param show_border Logical. When \code{summary_edges = FALSE}, plot a
#'   dashed circle around each cluster. Default TRUE.
#' @param legend Logical. Whether to show legend. Default TRUE.
#' @param legend_position Position for legend. Default "topright".
#' @param curvature Edge curvature. Default 0.3.
#' @param node_size Size of nodes inside shapes (summary mode). Default 3.
#' @param layout_margin Margin around the layout as fraction of range
#'   (summary mode). Default 0.15.
#' @param scale Scaling factor for high-resolution output. Values greater than
#'   1 reduce node, edge, label, and legend sizes by \code{sqrt(scale)} while
#'   leaving cluster spacing and shape_size unchanged. Default 1.
#' @param show_labels Logical. Show node labels inside clusters (summary
#'   mode). Default FALSE.
#' @param nodes Node metadata. NULL (default) uses the node data of a
#'   cograph_network. A data frame replaces it and must have one row per node
#'   in the node order of \code{x}. In summary mode, display text is taken
#'   from its \code{labels} column if present, otherwise from its
#'   \code{label} column.
#' @param label_size Label text size (summary mode). Default NULL
#'   (auto-scaled).
#' @param label_abbrev Label abbreviation in summary mode: NULL (none),
#'   integer (max chars), or "auto" (adaptive based on node count).
#' @param cluster_shape Accepted for compatibility; currently unused. Use
#'   \code{shapes} to control cluster shell shapes.
#' @param ... When \code{summary_edges = FALSE}, additional parameters
#'   passed to \code{\link{plot_tna}()}. In summary mode, only
#'   \code{edge.lwd}, \code{edge.labels}, \code{edge.label.cex} and
#'   \code{minimum} are read.
#'
#' @return Invisibly returns a \code{cluster_summary} object when
#'   \code{summary_edges = TRUE}, and otherwise the
#'   \code{\link{plot_tna}()} result (a \code{cograph_network} object).
#'
#' @export
#' @seealso \code{\link{csum}}, \code{\link{plot_mcml}}
#'
#' @examples
#' clusters <- list(Plan = c("Explore", "Plan", "Monitor", "Adapt", "Reflect"),
#'                  Act = c("Discuss", "Synthesize", "Evaluate", "Create", "Share"))
#' plot_mtna(regulation_net, cluster_list = clusters)
plot_mtna <- function(
    x,
    cluster_list = NULL,
    community = NULL,
    layout = "circle",
    spacing = 4,
    shape_size = 1.8,
    node_spacing = 0.5,
    colors = NULL,
    shapes = NULL,
    edge_colors = NULL,
    bundle_edges = TRUE,
    bundle_strength = 0.8,
    summary_edges = TRUE,
    aggregation = c("sum", "mean", "max", "min", "median", "density"),
    within_edges = TRUE,
    show_border = TRUE,
    legend = TRUE,
    legend_position = "topright",
    curvature = 0.3,
    node_size = 3,
    layout_margin = 0.15,
    scale = 1,
    show_labels = FALSE,
    nodes = NULL,
    label_size = NULL,
    label_abbrev = NULL,
    cluster_shape = NULL,
    ...
) {
  # Match aggregation method
  aggregation <- match.arg(aggregation)

  # Apply scale: use sqrt(scale) for gentler compensation at high-resolution
  size_scale <- sqrt(scale)
  node_size <- node_size / size_scale
  edge_scale <- 1 / size_scale

  # Handle cograph_network input
  nodes_df <- NULL
  if (inherits(x, "cograph_network")) {
    nodes_df <- get_nodes(x)
    lab <- if (!is.null(nodes_df$label)) nodes_df$label else as.character(seq_len(nrow(nodes_df)))
    weights <- to_matrix(x)
  } else if (inherits(x, "tna")) {
    lab <- x$labels
    weights <- x$weights
  } else if (is.matrix(x)) {
    lab <- colnames(x)
    if (is.null(lab)) lab <- as.character(seq_len(ncol(x)))
    weights <- x
  } else {
    stop("x must be a cograph_network, tna object, or matrix", call. = FALSE)
  }

  n <- length(lab)

  # Merge nodes parameter with existing nodes_df
  if (is.data.frame(nodes)) {
    nodes_df <- nodes
  }

  # Resolve display labels: priority is labels > label > identifier
  # (labels column = display text, label column = identifier)
  display_labels <- if (!is.null(nodes_df)) {
    if ("labels" %in% names(nodes_df)) {
      nodes_df$labels
    } else if ("label" %in% names(nodes_df)) {
      nodes_df$label
    } else {
      lab  # Fall back to identifiers
    }
  } else {
    lab
  }

  # Handle cluster_list as column name string
  if (is.character(cluster_list) && length(cluster_list) == 1) {
    if (is.null(nodes_df)) {
      stop("To use a column name for clusters, x must be a cograph_network", call. = FALSE)
    }
    if (!cluster_list %in% names(nodes_df)) {
      stop("Column '", cluster_list, "' not found in nodes. Available: ",
           paste(names(nodes_df), collapse = ", "), call. = FALSE)
    }
    cluster_col <- nodes_df[[cluster_list]]
    cluster_list <- split(lab, cluster_col)
  }

  # Auto-detect clusters from common column names
  if (is.null(cluster_list) && is.null(community) && !is.null(nodes_df)) {
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

  # Handle community parameter - auto-detect clusters
  if (!is.null(community)) {
    comm_df <- detect_communities(x, method = community)
    cluster_list <- split(comm_df$node, comm_df$community)
    names(cluster_list) <- paste0("Cluster_", names(cluster_list))
  }

  # Validate cluster_list
  if (is.null(cluster_list)) {
    stop("Either cluster_list or community must be specified", call. = FALSE)
  }
  n_clusters <- length(cluster_list)
  if (!is.list(cluster_list) || n_clusters < 2) {
    stop("cluster_list must be a list of 2+ character vectors", call. = FALSE)
  }

  n <- length(lab)

  # Validate no overlap between clusters
  all_nodes <- unlist(cluster_list)
  if (anyDuplicated(all_nodes)) {
    dups <- all_nodes[duplicated(all_nodes)]
    stop("cluster_list groups must not overlap. Duplicates: ",
         paste(unique(dups), collapse = ", "), call. = FALSE)
  }

  # Get indices for each cluster
  cluster_indices <- lapply(cluster_list, function(nodes) {
    idx <- match(nodes, lab)
    if (any(is.na(idx))) {
      missing <- nodes[is.na(idx)]
      stop("Nodes not found in x: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    idx
  })

  # Color palettes
  color_palette <- c("#ffd89d", "#a68ba5", "#7eb5d6", "#98d4a2",
                     "#f4a582", "#92c5de", "#d6c1de", "#b8e186",
                     "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9")

  shape_palette <- c("circle", "square", "diamond", "triangle",
                     "pentagon", "hexagon", "star", "cross")

  edge_color_palette <- c("#e6a500", "#7a5a7a", "#4a90b8", "#5cb85c",
                          "#d9534f", "#5bc0de", "#9b59b6", "#8bc34a",
                          "#ff7043", "#78909c", "#ab47bc", "#aed581")

  # Set colors and shapes
  cluster_colors <- if (is.null(colors)) rep_len(color_palette, n_clusters) else colors
  cluster_shapes <- if (is.null(shapes)) rep_len(shape_palette, n_clusters) else shapes
  if (is.null(edge_colors)) {
    edge_colors <- rep_len(edge_color_palette, n_clusters)
  }

  # Compute cluster center positions
  cluster_centers <- switch(layout,
    "circle" = {
      angles <- pi/2 - (seq_len(n_clusters) - 1) * 2 * pi / n_clusters
      cbind(
        x = spacing * cos(angles),
        y = spacing * sin(angles)
      )
    },
    "grid" = {
      nc <- ceiling(sqrt(n_clusters))
      nr <- ceiling(n_clusters / nc)
      expand.grid(
        x = seq(0, (nc - 1) * spacing * 2, length.out = nc),
        y = seq(0, -(nr - 1) * spacing * 2, length.out = nr)
      )[seq_len(n_clusters), ]
    },
    "horizontal" = {
      cbind(
        x = seq(0, (n_clusters - 1) * spacing * 2, length.out = n_clusters),
        y = 0
      )
    },
    "vertical" = {
      cbind(
        x = 0,
        y = seq(0, -(n_clusters - 1) * spacing * 2, length.out = n_clusters)
      )
    },
    stop("Unknown layout: ", layout, call. = FALSE)
  )

  # Initialize node positions
  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  # Assign node colors and shapes
  colors <- rep("lightgray", n)
  shapes <- rep("circle", n)

  # Place nodes in circular clusters
  # If bundling, position nodes based on their inter-cluster connectivity
  for (i in seq_len(n_clusters)) {
    idx <- cluster_indices[[i]]
    n_nodes <- length(idx)
    center_x <- cluster_centers[i, 1]
    center_y <- cluster_centers[i, 2]

    if (bundle_edges && n_nodes > 1) {
      # Calculate optimal angle for each node based on connections to other clusters
      node_angles <- numeric(n_nodes)

      for (j in seq_len(n_nodes)) {
        node_idx <- idx[j]
        # Find which other clusters this node connects to most
        target_angles <- numeric(0)
        target_weights <- numeric(0)

        for (k in seq_len(n_clusters)) {
          if (k != i) {
            other_idx <- cluster_indices[[k]]
            # Sum of edge weights to this cluster
            out_weight <- sum(weights[node_idx, other_idx], na.rm = TRUE)
            in_weight <- sum(weights[other_idx, node_idx], na.rm = TRUE)
            total_weight <- out_weight + in_weight

            if (total_weight > 0) {
              # Angle from this cluster center to other cluster center
              dx <- cluster_centers[k, 1] - center_x
              dy <- cluster_centers[k, 2] - center_y
              angle_to_cluster <- atan2(dy, dx)
              target_angles <- c(target_angles, angle_to_cluster)
              target_weights <- c(target_weights, total_weight)
            }
          }
        }

        # Weighted average angle (or default if no connections)
        if (length(target_angles) > 0 && sum(target_weights) > 0) {
          # Use circular mean weighted by connection strength
          wx <- sum(target_weights * cos(target_angles)) / sum(target_weights)
          wy <- sum(target_weights * sin(target_angles)) / sum(target_weights)
          node_angles[j] <- atan2(wy, wx)
        } else {
          # Default: evenly distributed starting angle
          node_angles[j] <- pi/2 - (j - 1) * 2 * pi / n_nodes
        }
      }

      # Sort nodes by their preferred angle
      angle_order <- order(node_angles)

      # Distribute nodes evenly around the circle but in sorted order
      # This keeps nodes with similar targets near each other
      even_angles <- pi/2 - (seq_len(n_nodes) - 1) * 2 * pi / n_nodes

      for (j in seq_len(n_nodes)) {
        orig_idx <- angle_order[j]
        final_angle <- even_angles[j]

        x_pos[idx[orig_idx]] <- center_x + shape_size * cos(final_angle)
        y_pos[idx[orig_idx]] <- center_y + shape_size * sin(final_angle)
      }
    } else if (n_nodes > 1) {
      # No bundling - arrange evenly
      angles <- pi/2 - (seq_len(n_nodes) - 1) * 2 * pi / n_nodes
      x_pos[idx] <- center_x + shape_size * cos(angles)
      y_pos[idx] <- center_y + shape_size * sin(angles)
    } else {
      x_pos[idx] <- center_x
      y_pos[idx] <- center_y
    }

    # Set colors and shapes
    colors[idx] <- cluster_colors[i]
    shapes[idx] <- cluster_shapes[i]
  }

  # Create node-to-cluster mapping for edge colors
  node_to_cluster <- rep(NA, n)
  for (i in seq_len(n_clusters)) {
    node_to_cluster[cluster_indices[[i]]] <- i
  }

  # Build edge color matrix
  edge_color_matrix <- matrix(NA, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    src_cluster <- node_to_cluster[i]
    if (!is.na(src_cluster)) {
      edge_color_matrix[i, ] <- edge_colors[src_cluster]
    }
  }

  layout_mat <- cbind(x = x_pos, y = y_pos)

  # Handle summary edges mode
  if (summary_edges) {
    # Use cluster_summary for aggregation (removes duplicated logic)
    cs <- cluster_summary(weights, cluster_list, method = aggregation,
                          type = "raw", compute_within = FALSE)
    cluster_weights <- cs$macro$weights

    # Create cluster-level layout (centers)
    cluster_layout <- as.matrix(cluster_centers)
    colnames(cluster_layout) <- c("x", "y")

    # Cluster names
    cluster_names <- names(cluster_list)
    if (is.null(cluster_names)) {
      cluster_names <- paste0("Cluster ", seq_len(n_clusters))
    }
    colnames(cluster_weights) <- rownames(cluster_weights) <- cluster_names

    # Build cluster edge colors
    cluster_edge_colors <- matrix(NA, nrow = n_clusters, ncol = n_clusters)
    for (i in seq_len(n_clusters)) {
      cluster_edge_colors[i, ] <- edge_colors[i]
    }

    # Get edge.lwd multiplier from ... (default 1)
    dots <- list(...)
    edge_lwd_mult <- if (!is.null(dots$edge.lwd)) dots$edge.lwd else 1

    # Self-loop sizing: fraction of shape_size (user-coord, device-independent).
    # anchor_ratio < 0.5 gives a full-circle loop; 0.5-1.0 gives a partial
    # arc. 0.6 produces a ~320 deg arc starting close to the node center.
    loop_r_cluster <- shape_size * 0.15
    loop_r_node    <- shape_size * 0.18
    loop_anchor    <- 0.6

    # For summary view, we need to draw manually after setting up the plot
    # First create empty plot with correct dimensions
    all_x <- cluster_centers[, 1]
    all_y <- cluster_centers[, 2]
    # Compute margin: ensure enough room for the shell + any self-loop
    # that extends beyond the border.
    x_base <- range(all_x)
    y_base <- range(all_y)
    loop_extent <- shape_size + loop_r_cluster * 2.5
    x_margin <- max(diff(x_base) * layout_margin * 0.5, loop_extent)
    y_margin <- max(diff(y_base) * layout_margin * 0.5, loop_extent)
    x_range <- c(x_base[1] - x_margin, x_base[2] + x_margin)
    y_range <- c(y_base[1] - y_margin, y_base[2] + y_margin)

    # Set up blank plot with minimal margins
    old_par <- graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::plot.new()
    graphics::plot.window(xlim = x_range, ylim = y_range, asp = 1)

    # Helper function to find edge point on shell border
    get_shell_edge_point <- function(center_x, center_y, target_x, target_y, radius, shape) {
      # Direction from center to target
      dx <- target_x - center_x
      dy <- target_y - center_y
      angle <- atan2(dy, dx)

      if (shape == "circle") {
        # Circle: point on circumference
        return(c(center_x + radius * cos(angle),
                 center_y + radius * sin(angle)))
      } else if (shape == "square") {
        # Square: find intersection with square border
        # Normalize direction
        if (abs(dx) > abs(dy)) {
          # Hits left or right side
          edge_x <- center_x + sign(dx) * radius
          edge_y <- center_y + dy / abs(dx) * radius
        } else {
          # Hits top or bottom side
          edge_y <- center_y + sign(dy) * radius
          edge_x <- center_x + dx / abs(dy) * radius
        }
        return(c(edge_x, edge_y))
      } else if (shape == "diamond") {
        # Diamond: intersection with rotated square
        # For diamond, sum of |x| + |y| = radius (in local coords)
        abs_cos <- abs(cos(angle))
        abs_sin <- abs(sin(angle))
        scale <- radius / (abs_cos + abs_sin)
        return(c(center_x + scale * cos(angle),
                 center_y + scale * sin(angle)))
      } else if (shape == "triangle") {
        # Triangle with vertices at top, bottom-left, bottom-right
        # Vertices at angles: pi/2, pi/2 + 2*pi/3, pi/2 + 4*pi/3
        vertex_angles <- c(pi/2, pi/2 + 2*pi/3, pi/2 + 4*pi/3)

        # Normalize angle to [0, 2*pi)
        norm_angle <- angle %% (2 * pi)
        if (norm_angle < 0) norm_angle <- norm_angle + 2 * pi # nocov (R %% always non-negative)

        # Find which edge we're hitting
        # Edge midpoint angles are between vertices
        edge_mid_angles <- c(
          pi/2 + pi/3,           # between top and bottom-left (5*pi/6)
          pi/2 + pi,             # between bottom-left and bottom-right (3*pi/2)
          pi/2 + 5*pi/3          # between bottom-right and top (pi/6 or 13*pi/6)
        )

        # For regular polygon: distance = r * cos(pi/n) / cos(angle - edge_center_angle)
        # For triangle n=3, cos(pi/3) = 0.5
        apothem_ratio <- cos(pi/3)  # = 0.5

        # Find which sector the angle falls into
        # Sectors are centered on edge midpoints
        if (norm_angle >= pi/6 && norm_angle < 5*pi/6) {
          # Right side of top or left edge
          if (norm_angle < pi/2) {
            # Right edge (from bottom-right to top)
            edge_center <- pi/6
          } else {
            # Left edge (from top to bottom-left)
            edge_center <- 5*pi/6
          }
        } else if (norm_angle >= 5*pi/6 && norm_angle < 3*pi/2) {
          # Left edge or bottom edge
          if (norm_angle < 7*pi/6) {
            edge_center <- 5*pi/6
          } else {
            edge_center <- 3*pi/2
          }
        } else {
          # Bottom or right edge
          if (norm_angle >= 3*pi/2 || norm_angle < pi/6) {
            if (norm_angle >= 3*pi/2 && norm_angle < 11*pi/6) {
              edge_center <- 3*pi/2
            } else {
              edge_center <- pi/6
              if (norm_angle > pi) edge_center <- edge_center + 2*pi # nocov (needs >=7 clusters at 330+ deg)
            }
          } else {
            edge_center <- pi/6 # nocov (tautological: Branch C condition equals inner if)
          }
        }

        # Calculate distance using apothem formula
        angle_diff <- abs(norm_angle - edge_center)
        if (angle_diff > pi) angle_diff <- 2*pi - angle_diff # nocov (sector logic keeps diff < pi/3)

        # Clamp to avoid division issues near vertices
        angle_diff <- min(angle_diff, pi/3 - 0.01)

        scale <- radius * apothem_ratio / cos(angle_diff)
        return(c(center_x + scale * cos(angle),
                 center_y + scale * sin(angle)))
      } else {
        # Default: circle
        return(c(center_x + radius * cos(angle),
                 center_y + radius * sin(angle)))
      }
    }

    shell_radius <- shape_size
    # Use slightly smaller radius for edge endpoints to touch the border
    edge_radius <- shell_radius * 0.98

    # STEP 1: Draw summary edges FIRST (behind everything)
    max_weight <- max(cluster_weights, na.rm = TRUE)
    for (i in seq_len(n_clusters)) {
      for (j in seq_len(n_clusters)) {
        if (cluster_weights[i, j] > 0) {
          weight <- cluster_weights[i, j]
          lwd <- (1 + 5 * (weight / max_weight)) * edge_scale * edge_lwd_mult

          if (i == j) {
            # Cluster self-loop: placed on the shell border, pointing
            # outward from the layout center (away from other clusters).
            cl_cx <- mean(cluster_centers[, 1])
            cl_cy <- mean(cluster_centers[, 2])
            cl_rot <- atan2(cluster_centers[i, 2] - cl_cy,
                            cluster_centers[i, 1] - cl_cx)
            # Use get_shell_edge_point so the loop sits on the actual
            # shape border (matters for triangle/diamond where the border
            # is closer than shell_radius in some directions).
            far_x <- cluster_centers[i, 1] + 100 * cos(cl_rot)
            far_y <- cluster_centers[i, 2] + 100 * sin(cl_rot)
            border_pt <- get_shell_edge_point(
              cluster_centers[i, 1], cluster_centers[i, 2],
              far_x, far_y, shell_radius, cluster_shapes[i]
            )
            border_x <- border_pt[1]
            border_y <- border_pt[2]
            draw_self_loop_base(
              x = border_x, y = border_y,
              node_size = loop_r_cluster,
              col = edge_colors[i], lwd = lwd,
              rotation = cl_rot,
              arrow = TRUE, asize = loop_r_cluster * 0.3,
              anchor_radius = loop_r_cluster * loop_anchor
            )
            next
          }

          # Get edge start point on shell i border (facing cluster j)
          start_pt <- get_shell_edge_point(
            cluster_centers[i, 1], cluster_centers[i, 2],
            cluster_centers[j, 1], cluster_centers[j, 2],
            edge_radius, cluster_shapes[i]
          )
          x0 <- start_pt[1]
          y0 <- start_pt[2]

          # Get edge end point on shell j border (facing cluster i)
          end_pt <- get_shell_edge_point(
            cluster_centers[j, 1], cluster_centers[j, 2],
            cluster_centers[i, 1], cluster_centers[i, 2],
            edge_radius, cluster_shapes[j]
          )
          x1 <- end_pt[1]
          y1 <- end_pt[2]

          # Draw curved line using xspline
          mid_x <- (x0 + x1) / 2
          mid_y <- (y0 + y1) / 2
          # Perpendicular offset for curve
          dx <- x1 - x0
          dy <- y1 - y0
          len <- sqrt(dx^2 + dy^2)
          if (len > 0) {
            # Offset perpendicular to line
            off_x <- -dy / len * curvature * len * 0.3
            off_y <- dx / len * curvature * len * 0.3
          } else { # nocov start
            off_x <- 0
            off_y <- 0
          } # nocov end

          graphics::xspline(
            x = c(x0, mid_x + off_x, x1),
            y = c(y0, mid_y + off_y, y1),
            shape = 1,
            open = TRUE,
            border = edge_colors[i],
            lwd = lwd
          )

          # Draw arrowhead at the end
          if (len > 0) {
            angle <- atan2(y1 - (mid_y + off_y), x1 - (mid_x + off_x))
            arrow_len <- 0.15
            graphics::polygon(
              x = x1 + arrow_len * c(0, -cos(angle - pi/7), -cos(angle + pi/7)),
              y = y1 + arrow_len * c(0, -sin(angle - pi/7), -sin(angle + pi/7)),
              col = edge_colors[i],
              border = edge_colors[i]
            )
          }

          # Draw edge label
          dots <- list(...)
          if (is.null(dots$edge.labels) || !isFALSE(dots$edge.labels)) {
            label_cex <- if (!is.null(dots$edge.label.cex)) dots$edge.label.cex else 0.9
            graphics::text(mid_x + off_x * 1.3, mid_y + off_y * 1.3,
                          labels = round(weight, 2),
                          cex = label_cex / size_scale,
                          col = "gray40")
          }
        }
      }
    }

    # STEP 2: Draw shell fills and borders (on top of summary edges)
    for (i in seq_len(n_clusters)) {
      center_x <- cluster_centers[i, 1]
      center_y <- cluster_centers[i, 2]
      shape <- cluster_shapes[i]
      shell_color <- cluster_colors[i]
      # Use light fill to cover summary edges underneath
      fill_color <- grDevices::adjustcolor(shell_color, alpha.f = 0.2)

      if (shape == "circle") {
        theta <- seq(0, 2 * pi, length.out = 100)
        graphics::polygon(
          x = center_x + shell_radius * cos(theta),
          y = center_y + shell_radius * sin(theta),
          border = shell_color,
          col = fill_color,
          lwd = 3 * edge_scale
        )
      } else if (shape == "square") {
        graphics::rect(
          xleft = center_x - shell_radius,
          ybottom = center_y - shell_radius,
          xright = center_x + shell_radius,
          ytop = center_y + shell_radius,
          border = shell_color,
          col = fill_color,
          lwd = 3 * edge_scale
        )
      } else if (shape == "diamond") {
        graphics::polygon(
          x = center_x + shell_radius * c(0, 1, 0, -1, 0),
          y = center_y + shell_radius * c(1, 0, -1, 0, 1),
          border = shell_color,
          col = fill_color,
          lwd = 3 * edge_scale
        )
      } else if (shape == "triangle") {
        angles <- c(pi/2, pi/2 + 2*pi/3, pi/2 + 4*pi/3, pi/2)
        graphics::polygon(
          x = center_x + shell_radius * cos(angles),
          y = center_y + shell_radius * sin(angles),
          border = shell_color,
          col = fill_color,
          lwd = 3 * edge_scale
        )
      } else {
        theta <- seq(0, 2 * pi, length.out = 100)
        graphics::polygon(
          x = center_x + shell_radius * cos(theta),
          y = center_y + shell_radius * sin(theta),
          border = shell_color,
          col = fill_color,
          lwd = 3 * edge_scale
        )
      }
    }

    # STEP 3: Draw within-cluster edges (if enabled)
    if (isTRUE(within_edges)) {
      dots <- list(...)
      min_weight <- if (!is.null(dots$minimum)) dots$minimum else 0

      for (i in seq_len(n_clusters)) {
        center_x <- cluster_centers[i, 1]
        center_y <- cluster_centers[i, 2]
        idx <- cluster_indices[[i]]
        n_nodes <- length(idx)
        shell_color <- cluster_colors[i]

        if (n_nodes > 1) {
          inner_radius <- shape_size * node_spacing
          node_angles <- pi/2 - (seq_len(n_nodes) - 1) * 2 * pi / n_nodes
          inner_x <- center_x + inner_radius * cos(node_angles)
          inner_y <- center_y + inner_radius * sin(node_angles)

          # Draw edges within this cluster
          edge_col <- grDevices::adjustcolor(shell_color, red.f = 0.7, green.f = 0.7, blue.f = 0.7)
          max_within <- max(weights[idx, idx], na.rm = TRUE)
          for (j in seq_len(n_nodes)) {
            for (k in seq_len(n_nodes)) {
              src_idx <- idx[j]
              tgt_idx <- idx[k]
              weight <- weights[src_idx, tgt_idx]

              if (!is.na(weight) && weight > min_weight) {
                if (max_within > 0) {
                  lwd <- (0.5 + 2 * (weight / max_within)) * edge_scale * edge_lwd_mult
                } else { # nocov start (weight > 0 implies max_within > 0)
                  lwd <- 1 * edge_scale * edge_lwd_mult
                } # nocov end

                if (j == k) {
                  # Loop pointing outward from cluster center.
                  loop_rot <- atan2(inner_y[j] - center_y,
                                    inner_x[j] - center_x)
                  draw_self_loop_base(
                    x = inner_x[j], y = inner_y[j],
                    node_size = loop_r_node,
                    col = edge_col, lwd = lwd,
                    rotation = loop_rot,
                    arrow = TRUE, asize = loop_r_node * 0.3,
                    anchor_radius = loop_r_node * loop_anchor
                  )
                } else {
                  x0 <- inner_x[j]
                  y0 <- inner_y[j]
                  x1 <- inner_x[k]
                  y1 <- inner_y[k]

                  mid_x <- (x0 + x1) / 2
                  mid_y <- (y0 + y1) / 2
                  dx <- x1 - x0
                  dy <- y1 - y0
                  len <- sqrt(dx^2 + dy^2)

                  if (len > 0) {
                    off_x <- -dy / len * curvature * len * 0.4
                    off_y <- dx / len * curvature * len * 0.4

                    graphics::xspline(
                      x = c(x0, mid_x + off_x, x1),
                      y = c(y0, mid_y + off_y, y1),
                      shape = 1,
                      open = TRUE,
                      border = edge_col,
                      lwd = lwd
                    )

                    angle <- atan2(y1 - (mid_y + off_y), x1 - (mid_x + off_x))
                    arrow_len <- 0.06
                    graphics::polygon(
                      x = x1 + arrow_len * c(0, -cos(angle - pi/7), -cos(angle + pi/7)),
                      y = y1 + arrow_len * c(0, -sin(angle - pi/7), -sin(angle + pi/7)),
                      col = edge_col,
                      border = edge_col
                    )
                  }
                }
              }
            }
          }
        }
      }
    }

    # STEP 4: Draw nodes inside shells and labels
    for (i in seq_len(n_clusters)) {
      center_x <- cluster_centers[i, 1]
      center_y <- cluster_centers[i, 2]
      idx <- cluster_indices[[i]]
      n_nodes <- length(idx)
      shape <- cluster_shapes[i]
      shell_color <- cluster_colors[i]

      # Draw nodes inside the shell
      if (n_nodes > 1) {
        inner_radius <- shape_size * node_spacing
        node_angles <- pi/2 - (seq_len(n_nodes) - 1) * 2 * pi / n_nodes
        inner_x <- center_x + inner_radius * cos(node_angles)
        inner_y <- center_y + inner_radius * sin(node_angles)
      } else {
        inner_x <- center_x
        inner_y <- center_y
      }

      # Map shape to pch
      shape_to_pch <- c(
        "circle" = 21, "square" = 22, "diamond" = 23, "triangle" = 24,
        "pentagon" = 21, "hexagon" = 21, "star" = 8, "cross" = 3
      )
      pch_val <- if (shape %in% names(shape_to_pch)) shape_to_pch[shape] else 21

      # Draw nodes
      graphics::points(inner_x, inner_y,
                      pch = pch_val,
                      bg = shell_color,
                      col = "gray30",
                      cex = node_size)

      # Draw node labels if requested
      if (isTRUE(show_labels)) {
        lbl_text <- display_labels[idx]
        if (!is.null(label_abbrev)) {
          lbl_text <- abbrev_label(lbl_text, label_abbrev, n)
        }
        lbl_cex <- if (is.null(label_size)) 0.7 / size_scale else label_size / size_scale
        graphics::text(inner_x, inner_y, labels = lbl_text,
                       cex = lbl_cex, pos = 3, offset = 0.4, col = "gray20")
      }

      # Draw cluster label
      cluster_names <- names(cluster_list)
      if (!is.null(cluster_names)) {
        graphics::text(center_x, center_y - shell_radius - 0.2,
                      labels = cluster_names[i],
                      cex = 1 / size_scale,
                      col = shell_color,
                      font = 2)
      }
    }

    result <- cs
  } else {
    # Regular mode - show all individual edges
    dots <- list(...)
    dots$edge.color <- NULL

    tplot_args <- c(
      list(
        x = x,
        layout = layout_mat,
        color = colors,
        node_shape = shapes,
        curvature = curvature,
        edge.color = edge_color_matrix
      ),
      dots
    )

    result <- do.call(plot_tna, tplot_args)

    # Draw cluster borders
    if (show_border) {
      for (i in seq_len(n_clusters)) {
        center_x <- cluster_centers[i, 1]
        center_y <- cluster_centers[i, 2]

        # Draw border circle
        theta <- seq(0, 2 * pi, length.out = 100)
        border_radius <- shape_size * 1.1
        graphics::polygon(
          x = center_x + border_radius * cos(theta),
          y = center_y + border_radius * sin(theta),
          border = cluster_colors[i],
          col = NA,
          lwd = 2,
          lty = 2
        )
      }
    }
  }

  # Draw legend
  if (isTRUE(legend)) {
    cluster_names <- names(cluster_list)
    if (is.null(cluster_names)) {
      cluster_names <- paste0("Cluster ", seq_len(n_clusters))
    }

    shape_to_pch <- c(
      "circle" = 21, "square" = 22, "diamond" = 23, "triangle" = 24,
      "pentagon" = 21, "hexagon" = 21, "star" = 8, "cross" = 3
    )
    pch_values <- vapply(cluster_shapes, function(s) {
      if (s %in% names(shape_to_pch)) shape_to_pch[[s]] else 21
    }, numeric(1))

    graphics::legend(
      legend_position,
      legend = cluster_names,
      pch = pch_values,
      pt.bg = cluster_colors,
      col = edge_colors,
      pt.cex = 2.5 / size_scale,
      cex = 1.4 / size_scale,
      bty = "n",
      title = "Clusters"
    )
  }

  invisible(result)
}

#' @rdname plot_mtna
#' @return See \code{\link{plot_mtna}}.
#' @export
mtna <- plot_mtna
