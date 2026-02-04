#' Plot Heterogeneous TNA Network (Multi-Group Layout)
#'
#' Plots a TNA model with nodes arranged in multiple groups using geometric layouts:
#' \itemize{
#'   \item 2 groups: Bipartite (two vertical columns or horizontal rows)
#'   \item 3+ groups: Polygon (nodes along edges of a regular polygon)
#' }
#' Supports triangle (3), rectangle (4), pentagon (5), hexagon (6), and beyond.
#'
#' @param input A tna object or weight matrix.
#' @param node_list List of 2+ character vectors defining node groups.
#' @param layout Layout type: "auto" (default), "bipartite", "polygon", or "circular".
#'   When "auto", uses bipartite for 2 groups and polygon for 3+ groups.
#'   "circular" places groups along arcs of a circle.
#'   Legacy values "triangle" and "rectangle" are supported as aliases for "polygon".
#' @param use_list_order Logical. Use node_list order (TRUE) or weight-based order (FALSE).
#'   Only applies to bipartite layout.
#' @param jitter Controls horizontal spread of nodes. Options:
#'   \itemize{
#'     \item TRUE (default): Auto-compute jitter based on edge connectivity
#'     \item FALSE or 0: No jitter (nodes aligned in columns)
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
#' @param orientation Layout orientation for bipartite: "vertical" (two columns, default)
#'   or "horizontal" (two rows). Ignored for triangle/rectangle layouts.
#' @param group1_pos Position for first group in bipartite layout. Default -1.2.
#' @param group2_pos Position for second group in bipartite layout. Default 1.2.
#' @param curvature Edge curvature amount. Default 0.4 for visible curves.
#' @param group1_color Color for first group nodes. Default "#ffd89d".
#' @param group2_color Color for second group nodes. Default "#a68ba5".
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
#' @param legend Logical. Whether to show a legend. Default TRUE for polygon layouts.
#' @param legend_position Position for legend: "topright", "topleft", "bottomright",
#'   "bottomleft", "right", "left", "top", "bottom". Default "topright".
#' @param extend_lines Logical or numeric. Draw extension lines from nodes.
#'   Only applies to bipartite layout.
#'   \itemize{
#'     \item FALSE (default): No extension lines
#'     \item TRUE: Draw lines extending toward the other group (default length 0.1)
#'     \item Numeric: Length of extension lines
#'   }
#' @param ... Additional parameters passed to tplot().
#'
#' @return Invisibly returns the result from tplot().
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define node groups (2 groups - bipartite)
#' node_types <- list(
#'   Student = c("Wrong", "Retry", "Right", "Attempt", "Instruction", "Skip"),
#'   AI = c("Order", "Correct", "Hint", "Quit", "Clarify", "Question", "Praise")
#' )
#'
#' # Basic bipartite plot
#' plot_htna(model, node_types)
#'
#' # With custom jitter
#' plot_htna(model, node_types, jitter_amount = 0.5)
#'
#' # Triangle layout (3 groups)
#' node_types_3 <- list(
#'   Teacher = c("Explain", "Question", "Feedback"),
#'   Student = c("Answer", "Ask", "Attempt"),
#'   System = c("Hint", "Score", "Progress")
#' )
#' plot_htna(model, node_types_3)  # Auto-detects triangle
#'
#' # Rectangle layout (4 groups)
#' node_types_4 <- list(
#'   Input = c("Click", "Type", "Scroll"),
#'   Process = c("Validate", "Transform"),
#'   Output = c("Display", "Alert"),
#'   Storage = c("Save", "Load", "Cache")
#' )
#' plot_htna(model, node_types_4)  # Auto-detects rectangle
#'
#' # Explicit layout selection
#' plot_htna(model, node_types_3, layout = "triangle")
#' }
plot_htna <- function(
    input,
    node_list,
    layout = "auto",
    use_list_order = TRUE,
    jitter = TRUE,
    jitter_amount = 0.8,
    jitter_side = "first",
    orientation = "vertical",
    group1_pos = -1.2,
    group2_pos = 1.2,
    curvature = 0.4,
    group1_color = "#ffd89d",
    group2_color = "#a68ba5",
    group1_shape = "circle",
    group2_shape = "square",
    group_colors = NULL,
    group_shapes = NULL,
    angle_spacing = 0.15,
    edge_colors = NULL,
    legend = TRUE,
    legend_position = "topright",
    extend_lines = FALSE,
    ...
) {

  # Extended color palette for many groups
  color_palette <- c("#ffd89d", "#a68ba5", "#7eb5d6", "#98d4a2",
                     "#f4a582", "#92c5de", "#d6c1de", "#b8e186",
                     "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9")

  # Extended shape palette
  shape_palette <- c("circle", "square", "diamond", "triangle",
                     "pentagon", "hexagon", "star", "cross")

  # Validate node_list
  n_groups <- length(node_list)
  if (!is.list(node_list) || n_groups < 2) {
    stop("node_list must be a list of 2+ character vectors", call. = FALSE)
  }
  for (i in seq_along(node_list)) {
    if (!is.character(node_list[[i]])) {
      stop("node_list elements must be character vectors", call. = FALSE)
    }
  }

  # Get labels and weights from input
  if (inherits(input, "tna")) {
    lab <- input$labels
    weights <- input$weights
  } else if (is.matrix(input)) {
    lab <- colnames(input)
    if (is.null(lab)) lab <- as.character(seq_len(ncol(input)))
    weights <- input
  } else {
    stop("input must be a tna object or matrix", call. = FALSE)
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
      stop("Nodes not found in input: ", paste(missing, collapse = ", "), call. = FALSE)
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
  if (layout == "circular" && n_groups < 2) {
    stop("Circular layout requires at least 2 groups", call. = FALSE)
  }

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

    # Map jitter_side to internal values
    jitter_side_internal <- switch(jitter_side,
      "first" = "group1",
      "second" = "group2",
      "left" = "group1",
      "right" = "group2",
      jitter_side
    )

    if (orientation == "vertical") {
      # VERTICAL: Two vertical columns side by side
      # group1 on left, group2 on right, nodes stacked vertically within each
      x_pos[lhs_idx] <- group1_pos
      x_pos[rhs_idx] <- group2_pos

      # Spread nodes vertically within each column
      if (n_g1 > 1) {
        y_pos[lhs_idx] <- seq(1, -1, length.out = n_g1)
      } else if (n_g1 == 1) {
        y_pos[lhs_idx] <- 0
      }
      if (n_g2 > 1) {
        y_pos[rhs_idx] <- seq(1, -1, length.out = n_g2)
      } else if (n_g2 == 1) {
        y_pos[rhs_idx] <- 0
      }

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

    } else {
      # HORIZONTAL: Two horizontal rows stacked top/bottom
      # group1 on top, group2 on bottom, nodes spread horizontally within each
      y_pos[lhs_idx] <- group1_pos
      y_pos[rhs_idx] <- group2_pos

      # Spread nodes horizontally within each row
      if (n_g1 > 1) {
        x_pos[lhs_idx] <- seq(-1, 1, length.out = n_g1)
      } else if (n_g1 == 1) {
        x_pos[lhs_idx] <- 0
      }
      if (n_g2 > 1) {
        x_pos[rhs_idx] <- seq(-1, 1, length.out = n_g2)
      } else if (n_g2 == 1) {
        x_pos[rhs_idx] <- 0
      }

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
    }
  } else if (layout == "polygon") {
    # Polygon layout: n groups along edges of a regular n-sided polygon
    pos <- compute_polygon_layout(node_list, lab, group_indices, n_groups, angle_spacing)
    x_pos <- pos$x
    y_pos <- pos$y
  } else if (layout == "circular") {
    # Circular layout: n groups along arcs of a circle
    pos <- compute_circular_layout(node_list, lab, group_indices, n_groups, angle_spacing)
    x_pos <- pos$x
    y_pos <- pos$y
  }

  layout_mat <- cbind(x = x_pos, y = y_pos)

  # Compute edge colors based on source group
  # Create a mapping from node index to group index
  node_to_group <- rep(NA, n)
  for (i in seq_along(node_list)) {
    node_to_group[group_indices[[i]]] <- i
  }

  # Determine edge colors
  if (is.null(edge_colors)) {
    # Use darker/more saturated versions of group colors for edges
    edge_color_palette <- c("#e6a500", "#7a5a7a", "#4a90b8", "#5cb85c",
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

  # Call tplot
  # Capture ... args and remove edge.color if we're setting it
  dots <- list(...)
  if (!is.null(edge_color_matrix)) {
    dots$edge.color <- NULL
    dots$`edge.color` <- NULL
  }

  tplot_args <- c(
    list(
      input = input,
      layout = layout_mat,
      color = colors,
      node_shape = shapes,
      curvature = curvature
    ),
    dots
  )

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
      pt.cex = 1.5,
      cex = 0.8,
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
          col = colors[i], lwd = 1
        )
      }
      for (i in rhs_idx) {
        graphics::segments(
          x0 = x_pos[i], y0 = y_pos[i],
          x1 = x_pos[i] - line_len, y1 = y_pos[i],
          col = colors[i], lwd = 1
        )
      }
    } else {
      # Horizontal rows: group1 on top extends down, group2 on bottom extends up
      for (i in lhs_idx) {
        graphics::segments(
          x0 = x_pos[i], y0 = y_pos[i],
          x1 = x_pos[i], y1 = y_pos[i] - line_len,
          col = colors[i], lwd = 1
        )
      }
      for (i in rhs_idx) {
        graphics::segments(
          x0 = x_pos[i], y0 = y_pos[i],
          x1 = x_pos[i], y1 = y_pos[i] + line_len,
          col = colors[i], lwd = 1
        )
      }
    }
  }

  invisible(result)
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
#'
#' @return List with x and y position vectors.
#'
#' @keywords internal
compute_polygon_layout <- function(node_list, lab, group_indices, n_sides, angle_spacing = 0.15) {
  n <- length(lab)
  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  # Radius of the polygon

  radius <- 1.2

  # Compute vertices of regular polygon
  # Start from top (pi/2) and go clockwise
  angles <- pi/2 - (0:n_sides) * 2 * pi / n_sides
  vertices_x <- radius * cos(angles)
  vertices_y <- radius * sin(angles)

  # Edge push distance (outward from center)
  edge_push <- 0.15

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
    if (sum(outward * mid) < 0) {
      outward <- -outward
    }

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
#'
#' @return List with x and y position vectors.
#'
#' @keywords internal
compute_circular_layout <- function(node_list, lab, group_indices, n_groups, angle_spacing = 0.15) {
  n <- length(lab)
  x_pos <- rep(0, n)
  y_pos <- rep(0, n)

  # Radius of the circle
  radius <- 1.2

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
