#' Plot Heterogeneous TNA Network (Multi-Group Layout)
#'
#' Plots a TNA model with nodes arranged in multiple groups using geometric layouts:
#' \itemize{
#'   \item 2 groups: Bipartite (two vertical columns or horizontal rows)
#'   \item 3+ groups: Polygon (nodes along edges of a regular polygon)
#' }
#' Supports triangle (3), rectangle (4), pentagon (5), hexagon (6), and beyond.
#'
#' @param x A tna object, weight matrix, or cograph_network.
#' @param node_list Node groups can be specified as:
#'   \itemize{
#'     \item A list of character vectors (node names per group)
#'     \item A string column name from nodes data (e.g., "groups")
#'     \item NULL to auto-detect from columns named: groups, cluster, community, etc.
#'     \item NULL with \code{community} specified for algorithmic detection
#'   }
#' @param community Community detection method to use for auto-grouping.
#'   If specified, overrides \code{node_list}. See \code{\link{detect_communities}}
#'   for available methods: "louvain", "walktrap", "fast_greedy", "label_prop",
#'   "infomap", "leiden".
#' @param layout Layout type: "auto" (default), "bipartite", "polygon", or "circular".
#'   When "auto", uses bipartite for 2 groups and polygon for 3+ groups.
#'   "circular" places groups along arcs of a circle.
#'   Legacy values "triangle" and "rectangle" are supported as aliases for "polygon".
#' @param use_list_order Logical. Use node_list order (TRUE) or weight-based order (FALSE).
#'   Only applies to bipartite layout.
#' @param jitter Controls horizontal spread of nodes. Options:
#'   \itemize{
#'     \item FALSE (default) or 0: No jitter (nodes aligned in columns)
#'     \item TRUE: Auto-compute jitter based on edge connectivity
#'     \item Numeric (0-1): Amount of jitter (0.3 = spread nodes 30\% of column width)
#'     \item Named list: Manual per-node offsets by label (e.g., list(Wrong = -0.2))
#'     \item Numeric vector of length n: Direct x-offsets for each node
#'   }
#'   Only applies to bipartite layout.
#' @param jitter_amount Base jitter amount when jitter=TRUE. Default 0.5.
#'   Higher values spread nodes more toward the center. Only applies to bipartite layout.
#' @param jitter_side Which side(s) to apply jitter: "first", "second", "both", or "none".
#'   Default "first" (only first group nodes are jittered toward center).
#'   Only applies to bipartite layout.
#' @param orientation Layout orientation for bipartite: "vertical" (two columns, default),
#'   "horizontal" (two rows), "facing" (both groups on same horizontal line,
#'   group1 left, group2 right, tip-to-tip), or "circular" (two facing semicircles
#'   with a gap between them). Ignored for triangle/rectangle layouts.
#' @param group1_pos Position for first group in bipartite layout. Default -2.
#'   Overridden by \code{group_spacing} if specified.
#' @param group2_pos Position for second group in bipartite layout. Default 2.
#'   Overridden by \code{group_spacing} if specified.
#' @param group_spacing Numeric. Distance between the two groups in bipartite layout.
#'   Overrides \code{group1_pos}/\code{group2_pos}. For example, \code{group_spacing = 6}
#'   places groups at x = -3 and x = 3. Default NULL (uses group1_pos/group2_pos).
#' @param node_spacing Numeric. Vertical (or horizontal) gap between nodes within a group.
#'   Default NULL (auto-computed from the largest group size).
#'   Increase for more space between nodes (e.g., 0.5 or 0.8).
#' @param columns Integer or vector of length 2. Number of sub-columns per group.
#'   A single value applies to both groups. A vector of 2 sets columns per group
#'   independently (e.g., \code{c(2, 1)} puts the first group in 2 columns).
#'   Nodes are distributed evenly across sub-columns. Default 1.
#' @param column_spacing Numeric. Horizontal distance between sub-columns within
#'   a group. Default NULL (auto: \code{node_spacing * 2}).
#' @param layout_margin Margin around the layout (0-1). Default 0.15. Increase if
#'   labels or self-loops are clipped at the edges.
#' @param curvature Edge curvature amount. Default 0.4 for visible curves.
#' @param group1_color Color for first group nodes. Default "#4FC3F7".
#' @param group2_color Color for second group nodes. Default "#fbb550".
#' @param group1_shape Shape for first group nodes. Default "circle".
#' @param group2_shape Shape for second group nodes. Default "square".
#' @param group_colors Vector of colors for each group. Overrides group1_color/group2_color.
#'   Required for 3+ groups if not using defaults.
#' @param group_shapes Vector of shapes for each group. Overrides group1_shape/group2_shape.
#'   Required for 3+ groups if not using defaults.
#' @param angle_spacing Controls empty space at corners (0-1). Default 0.15.
#'   Higher values create larger empty angles at vertices. Only applies to triangle/rectangle layouts.
#' @param edge_colors Vector of colors for edges by source group. If NULL (default),
#'   uses darker versions of group_colors. Set to FALSE to use default edge color.
#' @param intra_curvature Numeric. Curvature amount for intra-group edges (edges
#'   between nodes in the same group). When set, intra-group edges are drawn
#'   separately with curves that arc away from the opposing group. Default NULL
#'   (intra-group edges drawn normally by splot). Typical values: 0.3 to 1.0.
#' @param legend Logical. Whether to show a legend. Default TRUE for polygon layouts.
#' @param legend_position Position for legend: "topright", "topleft", "bottomright",
#'   "bottomleft", "right", "left", "top", "bottom". Default "bottomright".
#' @param extend_lines Logical or numeric. Draw extension lines from nodes.
#'   Only applies to bipartite layout.
#'   \itemize{
#'     \item FALSE (default): No extension lines
#'     \item TRUE: Draw lines extending toward the other group (default length 0.1)
#'     \item Numeric: Length of extension lines
#'   }
#' @param scale Scaling factor for spacing parameters. Use scale > 1 for
#'   high-resolution output (e.g., scale = 4 for 300 dpi). This multiplies
#'   group positions and polygon/circular radius to maintain proper proportions
#'   at higher resolutions. Default 1.
#' @param nodes Node metadata. Can be:
#'   \itemize{
#'     \item NULL (default): Use existing nodes data from cograph_network
#'     \item Data frame: Must have `label` column for matching; if `labels`
#'       column exists, uses it for display text
#'   }
#'   Display priority: `labels` column > `label` column (identifiers).
#' @param label_abbrev Label abbreviation: NULL (none), integer (max chars),
#'   or "auto" (adaptive based on node count). Applied before passing to tplot.
#' @param ... Additional parameters passed to tplot().
#'
#' @return Invisibly returns the result from tplot().
#'
#' @export
#'
#' @examples
#' # Create a 6-node network
#' mat <- matrix(runif(36, 0, 0.3), 6, 6)
#' diag(mat) <- 0
#' colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D", "E", "F")
#'
#' # Bipartite layout (2 groups)
#' groups <- list(Group1 = c("A", "B", "C"), Group2 = c("D", "E", "F"))
#' plot_htna(mat, groups)
#'
#' # Polygon layout (3 groups)
#' groups3 <- list(X = c("A", "B"), Y = c("C", "D"), Z = c("E", "F"))
#' plot_htna(mat, groups3)
plot_htna <- function(
    x,
    node_list = NULL,
    community = NULL,
    layout = "auto",
    use_list_order = TRUE,
    jitter = FALSE,
    jitter_amount = 0.8,
    jitter_side = "first",
    orientation = "vertical",
    group1_pos = -2,
    group2_pos = 2,
    group_spacing = NULL,
    node_spacing = NULL,
    columns = 1,
    column_spacing = NULL,
    layout_margin = 0.15,
    curvature = 0.4,
    group1_color = "#4FC3F7",
    group2_color = "#fbb550",
    group1_shape = "circle",
    group2_shape = "square",
    group_colors = NULL,
    group_shapes = NULL,
    angle_spacing = 0.15,
    edge_colors = NULL,
    intra_curvature = NULL,
    legend = TRUE,
    legend_position = "bottomright",
    extend_lines = FALSE,
    scale = 1,
    nodes = NULL,
    label_abbrev = NULL,
    ...
) {
  # Apply scale: use sqrt(scale) for gentler compensation at high-resolution
  size_scale <- sqrt(scale)


  # Extended color palette for many groups
  color_palette <- c("#4FC3F7", "#fbb550", "#7eb5d6", "#98d4a2",
                     "#f4a582", "#92c5de", "#d6c1de", "#b8e186",
                     "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9")

  # Extended shape palette
  shape_palette <- c("circle", "square", "diamond", "triangle",
                     "pentagon", "hexagon", "star", "cross")

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

  # Handle node_list as column name string
  if (is.character(node_list) && length(node_list) == 1) {
    if (is.null(nodes_df)) {
      stop("To use a column name for node_list, x must be a cograph_network", call. = FALSE)
    }
    if (!node_list %in% names(nodes_df)) {
      stop("Column '", node_list, "' not found in nodes. Available: ",
           paste(names(nodes_df), collapse = ", "), call. = FALSE)
    }
    group_col <- nodes_df[[node_list]]
    node_list <- split(lab, group_col)
  }

  # Auto-detect groups from common column names
  if (is.null(node_list) && is.null(community) && !is.null(nodes_df)) {
    group_cols <- c("groups", "group", "clusters", "cluster", "community", "module", "layer")
    for (col in group_cols) {
      if (col %in% names(nodes_df)) {
        group_col <- nodes_df[[col]]
        node_list <- split(lab, group_col)
        message("Using '", col, "' column for node groups")
        break
      }
    }
  }

  # Handle community parameter - auto-detect groups
  if (!is.null(community)) {
    comm_df <- detect_communities(x, method = community)
    node_list <- split(comm_df$node, comm_df$community)
    names(node_list) <- paste0("Group_", names(node_list))
  }

  # Validate node_list
  if (is.null(node_list)) {
    stop("Either node_list or community must be specified", call. = FALSE)
  }
  n_groups <- length(node_list)
  if (!is.list(node_list) || n_groups < 2) {
    stop("node_list must be a list of 2+ character vectors", call. = FALSE)
  }
  for (i in seq_along(node_list)) {
    if (!is.character(node_list[[i]])) {
      stop("node_list elements must be character vectors", call. = FALSE)
    }
  }

  n <- length(lab)

  # Validate no overlap between groups
  all_nodes <- unlist(node_list)
  if (anyDuplicated(all_nodes)) {
    dups <- all_nodes[duplicated(all_nodes)]
    stop("node_list groups must not overlap. Duplicates: ",
         paste(unique(dups), collapse = ", "), call. = FALSE)
  }

  # Get indices for each group and validate

  group_indices <- lapply(node_list, function(nodes) {
    idx <- match(nodes, lab)
    if (any(is.na(idx))) {
      missing <- nodes[is.na(idx)]
      stop("Nodes not found in x: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    idx
  })

  # Determine layout type
  if (layout == "auto") {
    layout <- if (n_groups == 2) "bipartite" else "polygon"
  }


  # Map legacy layout names to polygon
  if (layout %in% c("triangle", "rectangle", "pentagon", "hexagon")) {
    layout <- "polygon"
  }

  # Validate layout matches group count
  if (layout == "bipartite" && n_groups != 2) {
    stop("Bipartite layout requires exactly 2 groups", call. = FALSE)
  }
  if (layout == "polygon" && n_groups < 3) {
    stop("Polygon layout requires at least 3 groups", call. = FALSE)
  }
  if (layout == "circular" && n_groups < 2) { # nocov start
    stop("Circular layout requires at least 2 groups", call. = FALSE)
  } # nocov end

  # Determine colors and shapes for each group
  if (is.null(group_colors)) {
    if (n_groups == 2) {
      # Use individual parameters for backward compatibility
      group_colors <- c(group1_color, group2_color)
    } else {
      # Cycle through palette if more groups than colors
      group_colors <- rep_len(color_palette, n_groups)
    }
  }
  if (is.null(group_shapes)) {
    if (n_groups == 2) {
      # Use individual parameters for backward compatibility
      group_shapes <- c(group1_shape, group2_shape)
    } else {
      # Cycle through palette if more groups than shapes
      group_shapes <- rep_len(shape_palette, n_groups)
    }
  }

  # Validate color/shape vectors

  if (length(group_colors) != n_groups) {
    stop("group_colors must have ", n_groups, " elements", call. = FALSE)
  }
  if (length(group_shapes) != n_groups) {
    stop("group_shapes must have ", n_groups, " elements", call. = FALSE)
  }

  # Assign colors and shapes to nodes
  colors <- rep("lightgray", n)
  shapes <- rep("circle", n)
  for (i in seq_along(node_list)) {
    idx <- group_indices[[i]]
    colors[idx] <- group_colors[i]
    shapes[idx] <- group_shapes[i]
  }

  # Initialize positions
  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  # Route to appropriate layout computation
  if (layout == "bipartite") {
    # For bipartite, use group_indices directly
    lhs_idx <- group_indices[[1]]
    rhs_idx <- group_indices[[2]]
    n_g1 <- length(lhs_idx)
    n_g2 <- length(rhs_idx)

    # Apply group_spacing: overrides group1_pos/group2_pos
    if (!is.null(group_spacing)) {
      group1_pos <- -group_spacing / 2
      group2_pos <-  group_spacing / 2
    }

    # Resolve columns per group: scalar or vector of length 2
    cols_per_group <- rep_len(as.integer(columns), 2)
    cols_per_group[cols_per_group < 1] <- 1L

    # Auto-compute node_spacing: use rows per column, not total nodes
    n_rows_g1 <- ceiling(n_g1 / cols_per_group[1])
    n_rows_g2 <- ceiling(n_g2 / cols_per_group[2])
    n_max_rows <- max(n_rows_g1, n_rows_g2)
    if (is.null(node_spacing)) {
      node_spacing <- if (n_max_rows <= 4) 0.5 else 2 / (n_max_rows - 1)
    }

    # Auto-compute column_spacing if not specified (2 node sizes ≈ node_spacing)
    if (is.null(column_spacing)) {
      column_spacing <- node_spacing * 2
    }

    # Map jitter_side to internal values
    jitter_side_internal <- switch(jitter_side,
      "first" = "group1",
      "second" = "group2",
      "left" = "group1",
      "right" = "group2",
      jitter_side
    )

    if (orientation == "vertical") {
      # VERTICAL: groups as left/right, nodes stacked vertically
      # Place nodes in sub-columns within each group
      .place_group_vertical <- function(g_idx, center_x, n_cols) {
        ng <- length(g_idx)
        if (ng == 0) return(NULL) # nocov
        n_per_col <- ceiling(ng / n_cols)
        col_assign <- rep(seq_len(n_cols), each = n_per_col)[seq_len(ng)]
        # Sub-column x offsets centered around center_x
        col_offsets <- seq(-(n_cols - 1) / 2, (n_cols - 1) / 2,
                           length.out = n_cols) * column_spacing
        for (cc in seq_len(n_cols)) {
          in_col <- which(col_assign == cc)
          if (length(in_col) == 0) next # nocov
          nodes_in_col <- g_idx[in_col]
          x_pos[nodes_in_col] <<- center_x + col_offsets[cc]
          nr <- length(nodes_in_col)
          if (nr > 1) {
            half_span <- (nr - 1) * node_spacing / 2
            y_pos[nodes_in_col] <<- seq(half_span, -half_span, length.out = nr)
          } else {
            y_pos[nodes_in_col] <<- 0
          }
        }
      }
      .place_group_vertical(lhs_idx, group1_pos, cols_per_group[1])
      .place_group_vertical(rhs_idx, group2_pos, cols_per_group[2])

      # Apply jitter (horizontal direction - toward center)
      if (isTRUE(jitter) && jitter_side != "none") {
        x_jitter <- compute_connectivity_jitter_horizontal(weights, lhs_idx, rhs_idx, jitter_amount, jitter_side_internal)
        x_pos <- x_pos + x_jitter
      } else if (is.numeric(jitter) && length(jitter) == 1 && jitter > 0 && jitter_side != "none") {
        x_jitter <- compute_connectivity_jitter_horizontal(weights, lhs_idx, rhs_idx, jitter, jitter_side_internal)
        x_pos <- x_pos + x_jitter
      } else if (is.list(jitter)) {
        for (label_name in names(jitter)) {
          idx <- match(label_name, lab)
          if (!is.na(idx)) {
            x_pos[idx] <- x_pos[idx] + jitter[[label_name]]
          }
        }
      }

      # Weight-based reordering (if not using list order)
      if (!use_list_order) {
        edges <- weights[lhs_idx, rhs_idx, drop = FALSE]
        out_str <- rowSums(edges, na.rm = TRUE)
        in_str <- colSums(edges, na.rm = TRUE)

        out_str[out_str == 0] <- 1e-10
        in_str[in_str == 0] <- 1e-10

        rank_in <- rank(-in_str, ties.method = "first")
        rank_out <- rank(-out_str, ties.method = "first")

        pos_g1 <- rowSums(edges * rank_in[col(edges)], na.rm = TRUE) / out_str
        pos_g2 <- colSums(edges * rank_out, na.rm = TRUE) / in_str

        g1_order <- rank(pos_g1, ties.method = "first")
        g2_order <- rank(pos_g2, ties.method = "first")

        y_g1_sorted <- sort(y_pos[lhs_idx], decreasing = TRUE)
        y_g2_sorted <- sort(y_pos[rhs_idx], decreasing = TRUE)

        y_pos[lhs_idx] <- y_g1_sorted[g1_order]
        y_pos[rhs_idx] <- y_g2_sorted[g2_order]
      }

    } else if (orientation == "horizontal") {
      # HORIZONTAL: groups as top/bottom, nodes spread horizontally
      .place_group_horizontal <- function(g_idx, center_y, n_cols) {
        ng <- length(g_idx)
        if (ng == 0) return(NULL) # nocov
        n_per_row <- ceiling(ng / n_cols)
        row_assign <- rep(seq_len(n_cols), each = n_per_row)[seq_len(ng)]
        row_offsets <- seq(-(n_cols - 1) / 2, (n_cols - 1) / 2,
                           length.out = n_cols) * column_spacing
        for (rr in seq_len(n_cols)) {
          in_row <- which(row_assign == rr)
          if (length(in_row) == 0) next # nocov
          nodes_in_row <- g_idx[in_row]
          y_pos[nodes_in_row] <<- center_y + row_offsets[rr]
          nr <- length(nodes_in_row)
          if (nr > 1) {
            half_span <- (nr - 1) * node_spacing / 2
            x_pos[nodes_in_row] <<- seq(-half_span, half_span, length.out = nr)
          } else {
            x_pos[nodes_in_row] <<- 0
          }
        }
      }
      .place_group_horizontal(lhs_idx, group1_pos, cols_per_group[1])
      .place_group_horizontal(rhs_idx, group2_pos, cols_per_group[2])

      # Apply jitter (vertical direction - toward center)
      if (isTRUE(jitter) && jitter_side != "none") {
        y_jitter <- compute_connectivity_jitter_vertical(weights, lhs_idx, rhs_idx, jitter_amount, jitter_side_internal)
        y_pos <- y_pos + y_jitter
      } else if (is.numeric(jitter) && length(jitter) == 1 && jitter > 0 && jitter_side != "none") {
        y_jitter <- compute_connectivity_jitter_vertical(weights, lhs_idx, rhs_idx, jitter, jitter_side_internal)
        y_pos <- y_pos + y_jitter
      } else if (is.list(jitter)) {
        for (label_name in names(jitter)) {
          idx <- match(label_name, lab)
          if (!is.na(idx)) {
            y_pos[idx] <- y_pos[idx] + jitter[[label_name]]
          }
        }
      }
    } else if (orientation == "facing") {
      # FACING: both groups on same horizontal level, group1 left, group2 right
      gap <- if (!is.null(group_spacing)) group_spacing else abs(group2_pos - group1_pos)

      if (n_g1 > 1) {
        x_pos[lhs_idx] <- seq(-gap / 2 - (n_g1 - 1) * node_spacing,
                               -gap / 2, length.out = n_g1)
      } else {
        x_pos[lhs_idx] <- -gap / 2
      }
      y_pos[lhs_idx] <- 0

      if (n_g2 > 1) {
        x_pos[rhs_idx] <- seq(gap / 2,
                               gap / 2 + (n_g2 - 1) * node_spacing, length.out = n_g2)
      } else {
        x_pos[rhs_idx] <- gap / 2
      }
      y_pos[rhs_idx] <- 0

    } else if (orientation == "circular") {
      # CIRCULAR: two facing semicircles with a gap
      gap <- if (!is.null(group_spacing)) group_spacing else abs(group2_pos - group1_pos)
      # Radius sized so arc length ~ total node span
      radius <- max(n_g1, n_g2) * node_spacing / pi

      # Group1: left semicircle opening right (angles pi/2 to -pi/2, top to bottom)
      angles_g1 <- seq(pi / 2, -pi / 2, length.out = n_g1)
      x_pos[lhs_idx] <- -gap / 2 + radius * cos(angles_g1)
      y_pos[lhs_idx] <- radius * sin(angles_g1)

      # Group2: right semicircle opening left (angles pi/2 to 3pi/2, top to bottom)
      angles_g2 <- seq(pi / 2, 3 * pi / 2, length.out = n_g2)
      x_pos[rhs_idx] <- gap / 2 + radius * cos(angles_g2)
      y_pos[rhs_idx] <- radius * sin(angles_g2)
    }
  } else if (layout == "polygon") {
    # Polygon layout: n groups along edges of a regular n-sided polygon
    pos <- compute_polygon_layout(node_list, lab, group_indices, n_groups, angle_spacing, scale)
    x_pos <- pos$x
    y_pos <- pos$y
  } else if (layout == "circular") {
    # Circular layout: n groups along arcs of a circle
    pos <- compute_circular_layout(node_list, lab, group_indices, n_groups, angle_spacing, scale)
    x_pos <- pos$x
    y_pos <- pos$y
  }

  layout_mat <- cbind(x = x_pos, y = y_pos)

  # Normalize layout to [-1, 1] using uniform scale (preserves aspect ratio)
  # This ensures node sizes render correctly and intra-edge coordinates match
  x_range <- range(layout_mat[, 1])
  y_range <- range(layout_mat[, 2])
  max_span <- max(diff(x_range), diff(y_range))
  if (max_span > 0) {
    layout_mat[, 1] <- (layout_mat[, 1] - mean(x_range)) / (max_span / 2)
    layout_mat[, 2] <- (layout_mat[, 2] - mean(y_range)) / (max_span / 2)
  }

  # Compute edge colors based on source group
  # Create a mapping from node index to group index
  node_to_group <- rep(NA, n)
  for (i in seq_along(node_list)) {
    node_to_group[group_indices[[i]]] <- i
  }

  # Determine edge colors
  if (is.null(edge_colors)) {
    # Use darker/more saturated versions of group colors for edges
    edge_color_palette <- c("#0288D1", "#E09800", "#4a90b8", "#5cb85c",
                            "#d9534f", "#5bc0de", "#9b59b6", "#8bc34a",
                            "#ff7043", "#78909c", "#ab47bc", "#aed581")
    edge_colors <- rep_len(edge_color_palette, n_groups)
  } else if (isFALSE(edge_colors)) {
    edge_colors <- NULL
  }

  # Build edge color matrix if edge_colors is specified
  edge_color_matrix <- NULL
  if (!is.null(edge_colors)) {
    edge_color_matrix <- matrix(NA, nrow = n, ncol = n)
    for (i in seq_len(n)) {
      src_group <- node_to_group[i]
      if (!is.na(src_group)) {
        edge_color_matrix[i, ] <- edge_colors[src_group]
      }
    }
  }

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

  # Apply label abbreviation if requested
  final_labels <- display_labels
  if (!is.null(label_abbrev)) {
    final_labels <- abbrev_label(display_labels, label_abbrev, n)
  }

  # Call tplot
  # Capture ... args and remove edge.color if we're setting it
  dots <- list(...)
  if (!is.null(edge_color_matrix)) {
    dots$edge.color <- NULL
    dots$`edge.color` <- NULL
  }

  # Extract threshold for intra-edge filtering
  threshold_val <- dots$threshold %||% dots$minimum %||% 0

  # Handle intra-group edges: zero them out from main plot, draw separately
  plot_x <- x
  if (!is.null(intra_curvature) && intra_curvature > 0) {
    cross_weights <- weights
    for (gi in seq_along(group_indices)) {
      g_idx <- group_indices[[gi]]
      pairs <- expand.grid(a = g_idx, b = g_idx)
      pairs <- pairs[pairs$a != pairs$b, ]
      cross_weights[cbind(pairs$a, pairs$b)] <- 0
    }
    colnames(cross_weights) <- lab
    rownames(cross_weights) <- lab
    # Preserve tna object (keeps donuts, styling) — only swap weights
    if (inherits(x, "tna")) {
      plot_x <- x
      plot_x$weights <- cross_weights
    } else {
      plot_x <- cross_weights
    }
  }

  # Set minimal margins for tighter layout
  old_par <- graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(graphics::par(old_par), add = TRUE)

  tplot_base <- list(
    x = plot_x,
    layout = layout_mat,
    color = colors,
    node_shape = shapes,
    curvature = curvature,
    layout_margin = layout_margin
  )
  # We normalize layout ourselves — disable splot's rescaling
  tplot_base$rescale <- FALSE
  tplot_base$layout_scale <- 1
  tplot_args <- c(tplot_base, dots)

  # Add custom labels if nodes data exists or abbreviation requested
  if (!is.null(nodes_df) || !is.null(label_abbrev)) {
    tplot_args$labels <- final_labels
  }

  # Add edge colors if specified (tplot uses edge.color parameter)
  if (!is.null(edge_color_matrix)) {
    tplot_args$edge.color <- edge_color_matrix
  }

  result <- do.call(tplot, tplot_args)

  # Draw legend if requested
  if (isTRUE(legend) && n_groups >= 2) {
    # Get group names
    group_names <- names(node_list)
    if (is.null(group_names)) {
      group_names <- paste0("Group ", seq_len(n_groups))
    }

    # Map shape names to pch values
    shape_to_pch <- c(
      "circle" = 21, "square" = 22, "diamond" = 23, "triangle" = 24,
      "pentagon" = 21, "hexagon" = 21, "star" = 8, "cross" = 3
    )
    pch_values <- sapply(group_shapes, function(s) {
      if (s %in% names(shape_to_pch)) shape_to_pch[s] else 21
    })

    # Draw legend
    graphics::legend(
      legend_position,
      legend = group_names,
      pch = pch_values,
      pt.bg = group_colors,
      col = if (!is.null(edge_colors)) edge_colors else "black",
      pt.cex = 2.5 / size_scale,
      cex = 1.4 / size_scale,
      bty = "n",
      title = "Groups"
    )
  }

  # Draw extension lines if requested (bipartite only)
  if (!isFALSE(extend_lines) && layout == "bipartite") {
    line_len <- if (isTRUE(extend_lines)) 0.1 else extend_lines
    lhs_idx <- group_indices[[1]]
    rhs_idx <- group_indices[[2]]

    if (orientation == "vertical") {
      # Vertical columns: group1 on left extends right, group2 on right extends left
      for (i in lhs_idx) {
        graphics::segments(
          x0 = x_pos[i], y0 = y_pos[i],
          x1 = x_pos[i] + line_len, y1 = y_pos[i],
          col = colors[i], lwd = 1 / size_scale
        )
      }
      for (i in rhs_idx) {
        graphics::segments(
          x0 = x_pos[i], y0 = y_pos[i],
          x1 = x_pos[i] - line_len, y1 = y_pos[i],
          col = colors[i], lwd = 1 / size_scale
        )
      }
    } else {
      # Horizontal rows: group1 on top extends down, group2 on bottom extends up
      for (i in lhs_idx) {
        graphics::segments(
          x0 = x_pos[i], y0 = y_pos[i],
          x1 = x_pos[i], y1 = y_pos[i] - line_len,
          col = colors[i], lwd = 1 / size_scale
        )
      }
      for (i in rhs_idx) {
        graphics::segments(
          x0 = x_pos[i], y0 = y_pos[i],
          x1 = x_pos[i], y1 = y_pos[i] + line_len,
          col = colors[i], lwd = 1 / size_scale
        )
      }
    }
  }

  # Draw intra-group edges with curvature (after main plot)
  if (!is.null(intra_curvature) && intra_curvature > 0) {
    .draw_intra_group_edges(
      layout_mat = layout_mat,
      weights = weights,
      group_indices = group_indices,
      edge_colors = edge_colors,
      intra_curvature = intra_curvature,
      orientation = orientation,
      layout_type = layout,
      threshold = threshold_val,
      directed = TRUE
    )
  }

  invisible(result)
}

#' Draw Intra-Group Edges with Curvature
#'
#' Draws curved edges between nodes within the same group. Uses quadratic
#' bezier arcs with height proportional to distance, producing consistent
#' rounded arcs for both adjacent and distant node pairs.
#'
#' @param layout_mat Layout matrix (n x 2) with x, y coordinates.
#' @param weights Full weight matrix including intra-group edges.
#' @param group_indices List of index vectors per group.
#' @param edge_colors Vector of edge colors per group.
#' @param intra_curvature Curvature amount for intra-group edges.
#' @param orientation Layout orientation ("vertical", "horizontal", "facing").
#' @param threshold Minimum weight threshold for drawing edges.
#' @param directed Logical: draw arrows?
#'
#' @keywords internal
.draw_intra_group_edges <- function(layout_mat, weights, group_indices,
                                     edge_colors, intra_curvature, orientation,
                                     layout_type = "bipartite",
                                     threshold, directed) {
  all_center <- colMeans(layout_mat)
  node_r <- 7 * 0.015

  for (gi in seq_along(group_indices)) {
    g_idx <- group_indices[[gi]]
    if (length(g_idx) < 2) next

    # Curve direction: for bipartite = away from other group (fixed per group),
    # for circular/polygon = computed per-edge toward center
    per_edge_curve <- layout_type %in% c("circular", "polygon")
    curve_sign <- 1  # default, overridden below or per-edge
    if (!per_edge_curve) {
      g_center <- colMeans(layout_mat[g_idx, , drop = FALSE])
      if (orientation == "horizontal") {
        curve_sign <- if (g_center[2] > all_center[2]) 1 else -1
      } else if (orientation == "facing") {
        curve_sign <- if (g_center[1] < all_center[1]) 1 else -1
      } else {
        curve_sign <- if (g_center[1] > all_center[1]) 1 else -1
      }
    }

    e_col <- if (!is.null(edge_colors) && gi <= length(edge_colors)) {
      edge_colors[gi]
    } else {
      "gray50"
    }

    # Max weight for scaling line widths
    intra_w <- weights[g_idx, g_idx, drop = FALSE]
    diag(intra_w) <- 0
    max_w <- max(abs(intra_w), na.rm = TRUE)
    if (max_w == 0) next # nocov

    for (ii in seq_along(g_idx)) {
      for (jj in seq_along(g_idx)) {
        if (ii == jj) next
        src <- g_idx[ii]
        tgt <- g_idx[jj]
        w <- weights[src, tgt]
        if (is.na(w) || abs(w) < threshold) next

        lwd <- 0.5 + (abs(w) / max_w) * 3

        x1 <- layout_mat[src, 1]
        y1 <- layout_mat[src, 2]
        x2 <- layout_mat[tgt, 1]
        y2 <- layout_mat[tgt, 2]

        # Shorten edges by node radius
        edge_angle <- atan2(y2 - y1, x2 - x1)
        x1 <- x1 + node_r * cos(edge_angle)
        y1 <- y1 + node_r * sin(edge_angle)
        x2 <- x2 - node_r * cos(edge_angle)
        y2 <- y2 - node_r * sin(edge_angle)

        # For circular/polygon: compute curve direction per-edge toward center
        if (per_edge_curve) {
          mx <- (x1 + x2) / 2
          my <- (y1 + y2) / 2
          d <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
          if (d > 1e-10) {
            px <- -(y2 - y1) / d
            py <- (x2 - x1) / d
            # Pick perpendicular direction pointing toward center
            d_pos <- (mx + px - all_center[1])^2 + (my + py - all_center[2])^2
            d_neg <- (mx - px - all_center[1])^2 + (my - py - all_center[2])^2
            curve_sign <- if (d_pos < d_neg) 1 else -1
          }
        }

        .draw_intra_arc(x1, y1, x2, y2,
                         intra_curvature = intra_curvature,
                         curve_sign = curve_sign,
                         col = grDevices::adjustcolor(e_col, alpha.f = 0.7),
                         lwd = lwd * 1.5, lty = 3,
                         arrow = directed, asize = 0.03)
      }
    }
  }
}

#' Draw a Smooth Bezier Arc for Intra-Group Edges
#'
#' Uses a quadratic bezier curve with arc height proportional to distance,
#' ensuring consistent rounded arcs for both close and distant node pairs
#' (avoids the narrow spike issue with standard curvature for adjacent nodes).
#'
#' @param x1,y1 Start coordinates.
#' @param x2,y2 End coordinates.
#' @param intra_curvature Base curvature amount (controls arc height ratio).
#' @param curve_sign Direction of curve (+1 or -1).
#' @param col Edge color.
#' @param lwd Line width.
#' @param arrow Draw arrow at target?
#' @param asize Arrow size.
#'
#' @keywords internal
.draw_intra_arc <- function(x1, y1, x2, y2, intra_curvature, curve_sign,
                             col, lwd, lty = 1, arrow, asize) {
  dx <- x2 - x1
  dy <- y2 - y1
  dist <- sqrt(dx^2 + dy^2)
  if (dist < 1e-10) return(invisible())

  # Perpendicular unit vector
  px <- -dy / dist
  py <- dx / dist

  # Arc height proportional to distance — consistent aspect ratio
  # min height ensures very close nodes still have visible arcs
  arc_height <- intra_curvature * max(dist * 0.5, 0.06) * curve_sign

  # Bezier control point at midpoint + perpendicular offset
  mx <- (x1 + x2) / 2
  my <- (y1 + y2) / 2
  cx <- mx + px * arc_height
  cy <- my + py * arc_height

  # Generate smooth quadratic bezier: B(t) = (1-t)^2*P0 + 2(1-t)t*C + t^2*P2
  t <- seq(0, 1, length.out = 40)
  arc_x <- (1 - t)^2 * x1 + 2 * (1 - t) * t * cx + t^2 * x2
  arc_y <- (1 - t)^2 * y1 + 2 * (1 - t) * t * cy + t^2 * y2

  graphics::lines(arc_x, arc_y, col = col, lwd = lwd, lty = lty)

  # Arrow at target: direction of travel = from control point toward target
  if (arrow && asize > 0) {
    arr_angle <- atan2(y2 - cy, x2 - cx)
    draw_arrow_base(x2, y2, arr_angle, asize, col = col)
  }
}

#' Compute Connectivity-Based Jitter (Horizontal Layout)
#'
#' For horizontal layouts (left/right columns). Nodes with more cross-group
#' connections are jittered horizontally toward center.
#'
#' @param weights Weight matrix.
#' @param g1_idx Indices of group 1 nodes.
#' @param g2_idx Indices of group 2 nodes.
#' @param amount Maximum jitter amount. Default 0.8.
#' @param side Which group(s) to jitter: "group1", "group2", or "both".
#'
#' @return Numeric vector of x-offsets for each node.
#'
#' @keywords internal
compute_connectivity_jitter_horizontal <- function(weights, g1_idx, g2_idx, amount = 0.8, side = "group1") {
  n <- nrow(weights)
  jitter <- rep(0, n)

  # Extract cross-group edges
  cross_weights <- weights[g1_idx, g2_idx, drop = FALSE]

  # Compute edge strength for each node
  g1_strength <- rowSums(abs(cross_weights), na.rm = TRUE)
  g2_strength <- colSums(abs(cross_weights), na.rm = TRUE)

  # Normalize to 0-1 range
  g1_max <- max(g1_strength, na.rm = TRUE)
  g2_max <- max(g2_strength, na.rm = TRUE)

  g1_norm <- if (g1_max > 0) g1_strength / g1_max else rep(0, length(g1_idx))
  g2_norm <- if (g2_max > 0) g2_strength / g2_max else rep(0, length(g2_idx))

  # High connectivity = jitter toward center
  # Group1 (left, positive x): negative jitter moves toward center
  # Group2 (right, negative x): positive jitter moves toward center
  if (side %in% c("group1", "both", "first")) {
    jitter[g1_idx] <- -g1_norm * amount
  }
  if (side %in% c("group2", "both", "second")) {
    jitter[g2_idx] <- g2_norm * amount
  }

  jitter
}

#' Compute Connectivity-Based Jitter (Vertical Layout)
#'
#' For vertical layouts (top/bottom rows). Nodes with more cross-group
#' connections are jittered vertically toward center.
#'
#' @param weights Weight matrix.
#' @param g1_idx Indices of group 1 nodes (top).
#' @param g2_idx Indices of group 2 nodes (bottom).
#' @param amount Maximum jitter amount. Default 0.8.
#' @param side Which group(s) to jitter: "group1", "group2", or "both".
#'
#' @return Numeric vector of y-offsets for each node.
#'
#' @keywords internal
compute_connectivity_jitter_vertical <- function(weights, g1_idx, g2_idx, amount = 0.8, side = "group1") {
  n <- nrow(weights)
  jitter <- rep(0, n)

  # Extract cross-group edges
  cross_weights <- weights[g1_idx, g2_idx, drop = FALSE]

  # Compute edge strength for each node
  g1_strength <- rowSums(abs(cross_weights), na.rm = TRUE)
  g2_strength <- colSums(abs(cross_weights), na.rm = TRUE)

  # Normalize to 0-1 range
  g1_max <- max(g1_strength, na.rm = TRUE)
  g2_max <- max(g2_strength, na.rm = TRUE)

  g1_norm <- if (g1_max > 0) g1_strength / g1_max else rep(0, length(g1_idx))
  g2_norm <- if (g2_max > 0) g2_strength / g2_max else rep(0, length(g2_idx))

  # High connectivity = jitter toward center
  # Group1 (top, positive y): negative jitter moves toward center
  # Group2 (bottom, negative y): positive jitter moves toward center
  if (side %in% c("group1", "both", "first")) {
    jitter[g1_idx] <- -g1_norm * amount
  }
  if (side %in% c("group2", "both", "second")) {
    jitter[g2_idx] <- g2_norm * amount
  }

  jitter
}

#' Compute Polygon Layout
#'
#' Positions nodes along edges of a regular n-sided polygon.
#' Each group is placed along one edge. Edges are offset outward from vertices
#' to create empty angles at corners.
#'
#' @param node_list List of n character vectors.
#' @param lab Node labels from model.
#' @param group_indices List of index vectors for each group.
#' @param n_sides Number of sides (groups).
#' @param angle_spacing How far to push edges away from vertices (0-1). Default 0.15.
#' @param scale Scaling factor for radius. Default 1.
#'
#' @return List with x and y position vectors.
#'
#' @keywords internal
compute_polygon_layout <- function(node_list, lab, group_indices, n_sides, angle_spacing = 0.15, scale = 1) {
  n <- length(lab)
  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  # Radius of the polygon (scaled)
  radius <- 2 * scale

  # Compute vertices of regular polygon
  # Start from top (pi/2) and go clockwise
  angles <- pi/2 - (0:n_sides) * 2 * pi / n_sides
  vertices_x <- radius * cos(angles)
  vertices_y <- radius * sin(angles)

  # Edge push distance (outward from center, scaled)
  edge_push <- 0.15 * scale

  # Place each group along its edge

for (i in seq_len(n_sides)) {
    g_idx <- group_indices[[i]]
    n_nodes <- length(g_idx)

    # Edge from vertex i to vertex i+1
    v1 <- c(vertices_x[i], vertices_y[i])
    v2 <- c(vertices_x[i + 1], vertices_y[i + 1])

    # Edge midpoint
    mid <- (v1 + v2) / 2

    # Outward direction (perpendicular to edge, pointing away from center)
    edge_vec <- v2 - v1
    outward <- c(-edge_vec[2], edge_vec[1])  # Perpendicular
    outward <- outward / sqrt(sum(outward^2))  # Normalize

    # Make sure it points outward (away from origin)
    if (sum(outward * mid) < 0) { # nocov start
      outward <- -outward
    } # nocov end

    if (n_nodes > 1) {
      # Distribute nodes along edge with gaps at corners
      t_vals <- seq(angle_spacing, 1 - angle_spacing, length.out = n_nodes)
      base_x <- v1[1] + t_vals * (v2[1] - v1[1])
      base_y <- v1[2] + t_vals * (v2[2] - v1[2])

      # Push outward
      x_pos[g_idx] <- base_x + outward[1] * edge_push
      y_pos[g_idx] <- base_y + outward[2] * edge_push
    } else if (n_nodes == 1) {
      # Single node at midpoint, pushed outward
      x_pos[g_idx] <- mid[1] + outward[1] * edge_push
      y_pos[g_idx] <- mid[2] + outward[2] * edge_push
    }
  }

  list(x = x_pos, y = y_pos)
}

#' Compute Circular Layout
#'
#' Positions nodes along arcs of a circle, with each group occupying one arc.
#' Groups are separated by gaps controlled by angle_spacing.
#'
#' @param node_list List of n character vectors.
#' @param lab Node labels from model.
#' @param group_indices List of index vectors for each group.
#' @param n_groups Number of groups.
#' @param angle_spacing Gap between groups as fraction of arc (0-1). Default 0.15.
#' @param scale Scaling factor for radius. Default 1.
#'
#' @return List with x and y position vectors.
#'
#' @keywords internal
compute_circular_layout <- function(node_list, lab, group_indices, n_groups, angle_spacing = 0.15, scale = 1) {
  n <- length(lab)
  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  # Radius of the circle (scaled)
  radius <- 2 * scale

  # Total angle per group (including gap)
  angle_per_group <- 2 * pi / n_groups

  # Gap angle between groups
  gap_angle <- angle_per_group * angle_spacing

  # Usable arc angle per group
  arc_angle <- angle_per_group - gap_angle

  # Place each group along its arc
  for (i in seq_len(n_groups)) {
    g_idx <- group_indices[[i]]
    n_nodes <- length(g_idx)

    # Start angle for this group (starting from top, going clockwise)
    # Add half gap at start
    start_angle <- pi/2 - (i - 1) * angle_per_group - gap_angle/2
    end_angle <- start_angle - arc_angle

    if (n_nodes > 1) {
      # Distribute nodes along arc
      angles <- seq(start_angle, end_angle, length.out = n_nodes)
      x_pos[g_idx] <- radius * cos(angles)
      y_pos[g_idx] <- radius * sin(angles)
    } else if (n_nodes == 1) {
      # Single node at arc midpoint
      mid_angle <- (start_angle + end_angle) / 2
      x_pos[g_idx] <- radius * cos(mid_angle)
      y_pos[g_idx] <- radius * sin(mid_angle)
    }
  }

  list(x = x_pos, y = y_pos)
}

#' @rdname plot_htna
#' @export
#' @examples
#' \dontrun{
#' mat <- matrix(runif(36, 0, 0.3), 6, 6)
#' diag(mat) <- 0
#' colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D", "E", "F")
#' groups <- list(Group1 = c("A", "B", "C"), Group2 = c("D", "E", "F"))
#' htna(mat, groups)
#' }
htna <- plot_htna
