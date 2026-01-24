#' @title Base R Graphics Network Plotting
#' @description Network visualization using base R graphics (similar to qgraph).
#' @name splot
NULL

#' Plot Network with Base R Graphics
#'
#' Creates a network visualization using base R graphics functions (polygon,
#' lines, xspline, etc.) instead of grid graphics. This provides better
#' performance for large networks and uses the same snake_case parameter names
#' as soplot() for consistency.
#'
#' @param x Network input. Can be:
#'   - A square numeric matrix (adjacency/weight matrix)
#'   - A data frame with edge list (from, to, optional weight columns)
#'   - An igraph object
#'   - A sonnet_network object
#' @param layout Layout algorithm: "circle", "spring", "groups", or a matrix
#'   of x,y coordinates, or an igraph layout function. Also supports igraph
#'   two-letter codes: "kk", "fr", "drl", "mds", "ni", etc.
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @param seed Random seed for deterministic layouts. Default 42.
#' @param theme Theme name: "classic", "dark", "minimal", "colorblind", etc.
#'
#' @section Node Aesthetics:
#' @param node_size Node size(s). Single value or vector. Default 3.
#' @param node_size2 Secondary node size for ellipse/rectangle height.
#' @param node_shape Node shape(s): "circle", "square", "triangle", "diamond",
#'   "pentagon", "hexagon", "star", "heart", "ellipse", "cross".
#' @param node_fill Node fill color(s).
#' @param node_border_color Node border color(s).
#' @param node_border_width Node border width(s).
#' @param node_alpha Node transparency (0-1). Default 1.
#' @param labels Node labels: TRUE (use node names/indices), FALSE (none),
#'   or character vector.
#' @param label_size Label character expansion factor.
#' @param label_color Label text color.
#' @param label_position Label position: "center", "above", "below", "left", "right".
#'
#' @section Pie/Donut Nodes:
#' @param pie_values List of numeric vectors for pie chart nodes. Each element
#'   corresponds to a node and contains values for pie segments.
#' @param pie_colors List of color vectors for pie segments.
#' @param donut_values List of values for donut chart nodes. Single value (0-1)
#'   for progress donut, or vector for segmented donut.
#' @param donut_colors List of color vectors for donut segments.
#' @param donut_inner_ratio Inner radius ratio for donut (0-1). Default 0.5.
#' @param donut_bg_color Background color for unfilled donut portion.
#' @param donut_show_value Logical: show value in donut center? Default TRUE.
#' @param donut_value_size Font size for donut center value.
#' @param donut_value_color Color for donut center value.
#'
#' @section Edge Aesthetics:
#' @param edge_color Edge color(s). If NULL, uses positive_color/negative_color based on weight.
#' @param edge_width Edge width(s). If NULL, scales by weight.
#' @param edge_alpha Edge transparency (0-1). Default 0.8.
#' @param edge_labels Edge labels: TRUE (show weights), FALSE (none),
#'   or character vector.
#' @param edge_label_size Edge label size.
#' @param edge_label_color Edge label text color.
#' @param edge_label_bg Edge label background color.
#' @param edge_label_position Position along edge (0-1).
#' @param edge_label_fontface Font face: 1=plain, 2=bold, 3=italic.
#' @param edge_style Line type(s): 1=solid, 2=dashed, 3=dotted, etc.
#' @param curvature Edge curvature. 0 for straight, positive/negative for curves.
#' @param curve_scale Logical: auto-curve reciprocal edges?
#' @param curve_shape Spline tension (-1 to 1). Default 0.
#' @param curve_pivot Position along edge for curve control point (0-1).
#' @param curves Curve mode: TRUE (default) = single edges straight, reciprocal edges
#'   curve as ellipse (two opposing curves); FALSE = all straight; "force" = all curved.
#' @param arrow_size Arrow head size.
#' @param show_arrows Logical or vector: show arrows on directed edges?
#' @param bidirectional Logical or vector: show arrows at both ends?
#' @param loop_rotation Angle(s) in radians for self-loop direction.
#'
#' @section Weight Handling:
#' @param threshold Minimum absolute weight to display.
#' @param maximum Maximum weight for scaling. NULL for auto.
#' @param positive_color Color for positive weights.
#' @param negative_color Color for negative weights.
#'
#' @section Plot Settings:
#' @param title Plot title.
#' @param title_size Title font size.
#' @param margins Margins as c(bottom, left, top, right).
#' @param background Background color.
#' @param rescale Logical: rescale layout to [-1, 1]?
#' @param aspect Logical: maintain aspect ratio?
#' @param usePCH Logical: use points() for simple circles (faster). Default FALSE.
#'
#' @section Legend:
#' @param legend Logical: show legend?
#' @param legend_position Position: "topright", "topleft", "bottomright", "bottomleft".
#' @param legend_size Legend text size.
#' @param legend_edge_colors Logical: show positive/negative edge colors in legend?
#' @param legend_node_sizes Logical: show node size scale in legend?
#' @param groups Group assignments for node coloring/legend.
#' @param node_names Alternative names for legend (separate from labels).
#'
#' @section Output:
#' @param filetype Output format: "default" (screen), "png", "pdf", "svg", "jpeg", "tiff".
#' @param filename Output filename (without extension).
#' @param width Output width in inches.
#' @param height Output height in inches.
#' @param res Resolution in DPI for raster outputs (PNG, JPEG, TIFF). Default 600.
#' @param ... Additional arguments passed to layout functions.
#'
#' @return Invisibly returns the sonnet_network object.
#'
#' @export
#'
#' @examples
#' # Basic network from adjacency matrix
#' adj <- matrix(c(0, 1, 1, 0,
#'                 0, 0, 1, 1,
#'                 0, 0, 0, 1,
#'                 0, 0, 0, 0), 4, 4, byrow = TRUE)
#' splot(adj)
#'
#' # With curved edges
#' splot(adj, curvature = 0.2)
#'
#' # Weighted network with colors
#' w_adj <- matrix(c(0, 0.5, -0.3, 0,
#'                   0.8, 0, 0.4, -0.2,
#'                   0, 0, 0, 0.6,
#'                   0, 0, 0, 0), 4, 4, byrow = TRUE)
#' splot(w_adj, positive_color = "darkgreen", negative_color = "red")
#'
#' # Pie chart nodes
#' splot(adj, pie_values = list(c(1,2,3), c(2,2), c(1,1,1,1), c(3,1)))
#'
#' # Circle layout with labels
#' splot(adj, layout = "circle", labels = c("A", "B", "C", "D"))
#'
splot <- function(
    x,
    layout = "spring",
    directed = NULL,
    seed = 42,
    theme = NULL,

    # Node aesthetics
    node_size = NULL,
    node_size2 = NULL,
    node_shape = "circle",
    node_fill = NULL,
    node_border_color = NULL,
    node_border_width = 1,
    node_alpha = 1,
    labels = TRUE,
    label_size = NULL,
    label_color = "black",
    label_position = "center",

    # Pie/Donut
    pie_values = NULL,
    pie_colors = NULL,
    donut_values = NULL,
    donut_colors = NULL,
    donut_inner_ratio = 0.5,
    donut_bg_color = "gray90",
    donut_show_value = TRUE,
    donut_value_size = 0.8,
    donut_value_color = "black",

    # Edge aesthetics
    edge_color = NULL,
    edge_width = NULL,
    edge_alpha = 0.8,
    edge_labels = FALSE,
    edge_label_size = 0.8,
    edge_label_color = "gray30",
    edge_label_bg = "white",
    edge_label_position = 0.5,
    edge_label_fontface = 1,
    edge_style = 1,
    curvature = 0,
    curve_scale = TRUE,
    curve_shape = 0,
    curve_pivot = 0.5,
    curves = TRUE,
    arrow_size = 1,
    show_arrows = TRUE,
    bidirectional = FALSE,
    loop_rotation = NULL,

    # Weight handling
    threshold = 0,
    maximum = NULL,
    positive_color = "#2E7D32",
    negative_color = "#C62828",

    # Plot settings
    title = NULL,
    title_size = 1.2,
    margins = c(0.1, 0.1, 0.1, 0.1),
    background = "white",
    rescale = TRUE,
    aspect = TRUE,
    usePCH = FALSE,

    # Legend
    legend = FALSE,
    legend_position = "topright",
    legend_size = 0.8,
    legend_edge_colors = TRUE,
    legend_node_sizes = FALSE,
    groups = NULL,
    node_names = NULL,

    # Output
    filetype = "default",
    filename = "splot",
    width = 7,
    height = 7,
    res = 600,
    ...
) {

  # ============================================
  # 1. INPUT PROCESSING
  # ============================================

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Convert to sonnet_network if needed
  network <- ensure_sonnet_network(x, layout = layout, seed = seed, ...)

  # Apply theme if specified
  if (!is.null(theme)) {
    th <- get_theme(theme)
    if (!is.null(th)) {
      # Extract theme colors
      if (is.null(node_fill)) node_fill <- th$get("node_fill")
      if (is.null(node_border_color)) node_border_color <- th$get("node_border_color")
      if (is.null(background)) background <- th$get("background")
      if (label_color == "black") label_color <- th$get("label_color")
      if (positive_color == "#2E7D32") positive_color <- th$get("edge_positive_color")
      if (negative_color == "#C62828") negative_color <- th$get("edge_negative_color")
    }
  }

  nodes <- network$network$get_nodes()
  edges <- network$network$get_edges()
  layout_coords <- network$network$get_layout()

  n_nodes <- nrow(nodes)
  n_edges <- if (!is.null(edges)) nrow(edges) else 0

  # Determine if directed
  if (is.null(directed)) {
    directed <- network$network$is_directed
  }

  # ============================================
  # 2. LAYOUT HANDLING
  # ============================================

  if (is.null(layout_coords)) {
    stop("Layout coordinates not available", call. = FALSE)
  }

  layout_mat <- as.matrix(layout_coords[, c("x", "y")])

  # Rescale to [-1, 1]
  if (rescale) {
    layout_mat <- as.matrix(rescale_layout(layout_mat, mar = 0.1))
  }

  # ============================================
  # 3. PARAMETER VECTORIZATION
  # ============================================

  # Node sizes (qgraph-style)
  if (is.null(node_size)) node_size <- 3
  vsize_usr <- resolve_node_sizes(node_size, n_nodes, default_size = 3, scale_factor = 0.04)
  vsize2_usr <- if (!is.null(node_size2)) {
    resolve_node_sizes(node_size2, n_nodes, default_size = 3, scale_factor = 0.04)
  } else {
    vsize_usr
  }

  # Node shapes
  shapes <- resolve_shapes(node_shape, n_nodes)

  # Node colors
  node_colors <- resolve_node_colors(node_fill, n_nodes, nodes, groups)

  # Apply alpha to node colors
  if (node_alpha < 1) {
    node_colors <- sapply(node_colors, function(c) adjust_alpha(c, node_alpha))
  }

  # Border colors
  if (is.null(node_border_color)) {
    node_border_color <- sapply(node_colors, function(c) {
      tryCatch(adjust_brightness(c, -0.3), error = function(e) "black")
    })
  }
  border_colors <- recycle_to_length(node_border_color, n_nodes)

  # Border widths
  border_widths <- recycle_to_length(node_border_width, n_nodes)

  # Labels
  node_labels <- resolve_labels(labels, nodes, n_nodes)

  # Label sizes
  if (is.null(label_size)) {
    label_size <- pmin(1, vsize_usr * 8)
  }
  label_cex <- recycle_to_length(label_size, n_nodes)
  label_colors <- recycle_to_length(label_color, n_nodes)

  # ============================================
  # 4. EDGE PROCESSING
  # ============================================

  if (n_edges > 0) {
    # Filter by minimum weight (threshold)
    edges <- filter_edges_by_weight(edges, threshold)
    n_edges <- nrow(edges)
  }

  if (n_edges > 0) {
    # Edge colors
    edge_colors <- resolve_edge_colors(edges, edge_color, positive_color, negative_color)

    # Apply edge alpha
    if (edge_alpha < 1) {
      edge_colors <- sapply(edge_colors, function(c) adjust_alpha(c, edge_alpha))
    }

    # Edge widths
    edge_widths <- resolve_edge_widths(edges, edge_width, maximum, threshold)

    # Line types
    ltys <- recycle_to_length(edge_style, n_edges)

    # Handle curves mode:
    # FALSE = all straight
    # TRUE or "mutual" = only reciprocal edges curved (opposite directions)
    # "force" = all edges curved (reciprocals opposite, singles inward)
    curves_vec <- recycle_to_length(curvature, n_edges)
    is_reciprocal <- rep(FALSE, n_edges)

    # Identify reciprocal pairs
    for (i in seq_len(n_edges)) {
      from_i <- edges$from[i]
      to_i <- edges$to[i]
      if (from_i == to_i) next
      for (j in seq_len(n_edges)) {
        if (j != i && edges$from[j] == to_i && edges$to[j] == from_i) {
          is_reciprocal[i] <- TRUE
          break
        }
      }
    }

    if (identical(curves, TRUE) || identical(curves, "mutual")) {
      # Only curve reciprocal edges
      for (i in seq_len(n_edges)) {
        if (is_reciprocal[i] && curves_vec[i] == 0) {
          # Opposite directions: lower index gets positive
          curves_vec[i] <- if (edges$from[i] < edges$to[i]) 0.2 else -0.2
        }
      }
    } else if (identical(curves, "force")) {
      # Curve all edges
      for (i in seq_len(n_edges)) {
        if (edges$from[i] == edges$to[i]) next  # Skip self-loops
        if (curves_vec[i] == 0) {
          if (is_reciprocal[i]) {
            # Reciprocal: opposite directions
            curves_vec[i] <- if (edges$from[i] < edges$to[i]) 0.2 else -0.2
          } else {
            # Single edge: will be curved inward by render_edges_base
            curves_vec[i] <- 0.2
          }
        }
      }
    }
    # If curves = FALSE, curves_vec stays at 0 (straight edges)

    curve_pivots <- recycle_to_length(curve_pivot, n_edges)
    curve_shapes <- recycle_to_length(curve_shape, n_edges)

    # Arrows
    if (is.logical(show_arrows) && length(show_arrows) == 1) {
      arrows_vec <- rep(directed && show_arrows, n_edges)
    } else {
      arrows_vec <- recycle_to_length(show_arrows, n_edges)
    }

    # Arrow size (convert from qgraph scale)
    asize_scaled <- arrow_size * 0.03
    arrow_sizes <- recycle_to_length(asize_scaled, n_edges)

    # Bidirectional
    bidirectionals <- recycle_to_length(bidirectional, n_edges)

    # Loop rotation
    loop_rotations <- resolve_loop_rotation(loop_rotation, edges, layout_mat)

    # Edge labels
    edge_labels_vec <- resolve_edge_labels(edge_labels, edges, n_edges)
  }

  # ============================================
  # 5. DEVICE SETUP
  # ============================================

  # Handle file output
  if (filetype != "default") {
    full_filename <- paste0(filename, ".", filetype)

    if (filetype == "png") {
      grDevices::png(full_filename, width = width, height = height,
                     units = "in", res = res)
    } else if (filetype == "pdf") {
      grDevices::pdf(full_filename, width = width, height = height)
    } else if (filetype == "svg") {
      grDevices::svg(full_filename, width = width, height = height)
    } else if (filetype == "jpeg" || filetype == "jpg") {
      grDevices::jpeg(full_filename, width = width, height = height,
                      units = "in", res = res, quality = 100)
    } else if (filetype == "tiff") {
      grDevices::tiff(full_filename, width = width, height = height,
                      units = "in", res = res, compression = "lzw")
    } else {
      stop("Unknown filetype: ", filetype, call. = FALSE)
    }

    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Set up plot area
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  # Margins
  title_space <- if (!is.null(title)) 0.5 else 0
  graphics::par(mar = c(margins[1], margins[2], margins[3] + title_space, margins[4]))

  # Calculate plot limits
  x_range <- range(layout_mat[, 1], na.rm = TRUE)
  y_range <- range(layout_mat[, 2], na.rm = TRUE)

  # Add margin to limits
  x_margin <- diff(x_range) * 0.15
  y_margin <- diff(y_range) * 0.15

  xlim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  ylim <- c(y_range[1] - y_margin, y_range[2] + y_margin)

  # Create plot
  graphics::plot(
    1, type = "n",
    xlim = xlim,
    ylim = ylim,
    axes = FALSE,
    ann = FALSE,
    asp = if (aspect) 1 else NA,
    xaxs = "i", yaxs = "i"
  )

  # Background
  if (!is.null(background) && background != "transparent") {
    graphics::rect(
      xleft = xlim[1] - 1, ybottom = ylim[1] - 1,
      xright = xlim[2] + 1, ytop = ylim[2] + 1,
      col = background, border = NA
    )
  }

  # Title
  if (!is.null(title)) {
    graphics::title(main = title, cex.main = title_size)
  }

  # ============================================
  # 6. RENDER EDGES
  # ============================================

  if (n_edges > 0) {
    render_edges_splot(
      edges = edges,
      layout = layout_mat,
      node_sizes = vsize_usr,
      shapes = shapes,
      edge_color = edge_colors,
      edge_width = edge_widths,
      edge_style = ltys,
      curvature = curves_vec,
      curve_shape = curve_shapes,
      curve_pivot = curve_pivots,
      show_arrows = arrows_vec,
      arrow_size = arrow_sizes,
      bidirectional = bidirectionals,
      loop_rotation = loop_rotations,
      edge_labels = edge_labels_vec,
      edge_label_size = edge_label_size,
      edge_label_color = edge_label_color,
      edge_label_bg = edge_label_bg,
      edge_label_position = edge_label_position,
      edge_label_fontface = edge_label_fontface
    )
  }

  # ============================================
  # 7. RENDER NODES
  # ============================================

  render_nodes_splot(
    layout = layout_mat,
    node_size = vsize_usr,
    node_size2 = vsize2_usr,
    node_shape = shapes,
    node_fill = node_colors,
    node_border_color = border_colors,
    node_border_width = border_widths,
    pie_values = pie_values,
    pie_colors = pie_colors,
    donut_values = donut_values,
    donut_colors = donut_colors,
    donut_inner_ratio = donut_inner_ratio,
    donut_bg_color = donut_bg_color,
    donut_show_value = donut_show_value,
    donut_value_size = donut_value_size,
    donut_value_color = donut_value_color,
    labels = node_labels,
    label_size = label_cex,
    label_color = label_colors,
    label_position = label_position,
    usePCH = usePCH
  )

  # ============================================
  # 8. LEGEND
  # ============================================

  if (legend) {
    # Determine if we have positive/negative weighted edges
    has_pos_edges <- FALSE
    has_neg_edges <- FALSE
    if (n_edges > 0 && "weight" %in% names(edges)) {
      has_pos_edges <- any(edges$weight > 0, na.rm = TRUE)
      has_neg_edges <- any(edges$weight < 0, na.rm = TRUE)
    }

    render_legend_splot(
      groups = groups,
      node_names = node_names,
      nodes = nodes,
      node_colors = node_colors,
      position = legend_position,
      cex = legend_size,
      show_edge_colors = legend_edge_colors,
      positive_color = positive_color,
      negative_color = negative_color,
      has_pos_edges = has_pos_edges,
      has_neg_edges = has_neg_edges,
      show_node_sizes = legend_node_sizes,
      node_size = vsize_usr
    )
  }

  # ============================================
  # 9. RETURN
  # ============================================

  invisible(network)
}


#' Render Edges for splot
#' @keywords internal
render_edges_splot <- function(edges, layout, node_sizes, shapes,
                               edge_color, edge_width, edge_style, curvature,
                               curve_shape, curve_pivot, show_arrows, arrow_size,
                               bidirectional, loop_rotation, edge_labels,
                               edge_label_size, edge_label_color, edge_label_bg,
                               edge_label_position, edge_label_fontface) {

  m <- nrow(edges)
  if (m == 0) return(invisible())

  n <- nrow(layout)

  # Calculate network center for inward curve direction
  center_x <- mean(layout[, 1])
  center_y <- mean(layout[, 2])

  # Get render order (weakest to strongest)
  order_idx <- get_edge_order(edges)

  # Storage for label positions
  label_positions <- vector("list", m)

  for (i in order_idx) {
    from_idx <- edges$from[i]
    to_idx <- edges$to[i]

    x1 <- layout[from_idx, 1]
    y1 <- layout[from_idx, 2]
    x2 <- layout[to_idx, 1]
    y2 <- layout[to_idx, 2]

    # Self-loop
    if (from_idx == to_idx) {
      draw_self_loop_base(
        x1, y1, node_sizes[from_idx],
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        rotation = loop_rotation[i],
        arrow = show_arrows[i],
        asize = arrow_size[i]
      )

      # Label position for self-loop
      loop_dist <- node_sizes[from_idx] * 2.5
      label_positions[[i]] <- list(
        x = x1 + loop_dist * cos(loop_rotation[i]),
        y = y1 + loop_dist * sin(loop_rotation[i])
      )
      next
    }

    # Calculate edge endpoints
    angle_to <- splot_angle(x1, y1, x2, y2)
    angle_from <- splot_angle(x2, y2, x1, y1)

    start <- cent_to_edge(x1, y1, angle_to, node_sizes[from_idx], NULL, shapes[from_idx])
    end <- cent_to_edge(x2, y2, angle_from, node_sizes[to_idx], NULL, shapes[to_idx])

    # Determine curve direction for inward bending
    # Positive curve values should bend toward the network center
    # Negative curve values (reciprocal edges) keep their direction (bend outward)
    curve_i <- curvature[i]
    if (curve_i > 1e-6) {
      # Calculate midpoint of edge
      mid_x <- (start$x + end$x) / 2
      mid_y <- (start$y + end$y) / 2

      # Edge direction vector
      dx <- end$x - start$x
      dy <- end$y - start$y

      # Vector from midpoint to network center
      to_center_x <- center_x - mid_x
      to_center_y <- center_y - mid_y

      # Cross product determines which side of the edge the center is on
      # Positive cross = center is to the left (counterclockwise from edge direction)
      # Negative cross = center is to the right (clockwise from edge direction)
      cross <- dx * to_center_y - dy * to_center_x

      # Adjust curve sign to bend toward center
      if (cross > 0) {
        curve_i <- abs(curve_i)   # Positive = bend left toward center
      } else {
        curve_i <- -abs(curve_i)  # Negative = bend right toward center
      }
    }
    # Negative curves (reciprocal edges curving outward) keep their sign

    # Draw edge
    if (abs(curve_i) > 1e-6) {
      draw_curved_edge_base(
        start$x, start$y, end$x, end$y,
        curve = curve_i,
        curvePivot = curve_pivot[i],
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        arrow = show_arrows[i],
        asize = arrow_size[i],
        bidirectional = bidirectional[i]
      )
    } else {
      draw_straight_edge_base(
        start$x, start$y, end$x, end$y,
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        arrow = show_arrows[i],
        asize = arrow_size[i],
        bidirectional = bidirectional[i]
      )
    }

    # Store label position (use adjusted curve for correct position)
    label_positions[[i]] <- get_edge_label_position(
      start$x, start$y, end$x, end$y,
      position = edge_label_position,
      curve = curve_i,
      curvePivot = curve_pivot[i]
    )
  }

  # Draw edge labels
  if (!is.null(edge_labels)) {
    for (i in seq_len(m)) {
      if (!is.null(edge_labels[i]) && !is.na(edge_labels[i]) && edge_labels[i] != "") {
        pos <- label_positions[[i]]
        draw_edge_label_base(
          pos$x, pos$y,
          label = edge_labels[i],
          cex = edge_label_size,
          col = edge_label_color,
          bg = edge_label_bg,
          font = edge_label_fontface
        )
      }
    }
  }
}


#' Render Nodes for splot
#' @keywords internal
render_nodes_splot <- function(layout, node_size, node_size2, node_shape, node_fill,
                               node_border_color, node_border_width, pie_values, pie_colors,
                               donut_values, donut_colors, donut_inner_ratio, donut_bg_color,
                               donut_show_value, donut_value_size, donut_value_color,
                               labels, label_size, label_color, label_position,
                               usePCH = FALSE) {

  n <- nrow(layout)
  if (n == 0) return(invisible())

  # Render order: largest to smallest
  order_idx <- get_node_order(node_size)

  for (i in order_idx) {
    x <- layout[i, 1]
    y <- layout[i, 2]

    # Check for pie/donut
    has_pie <- !is.null(pie_values) && length(pie_values) >= i && !is.null(pie_values[[i]]) && length(pie_values[[i]]) > 0
    has_donut <- !is.null(donut_values) && length(donut_values) >= i && !is.null(donut_values[[i]])

    if (has_donut && has_pie) {
      # Donut with inner pie
      donut_val <- if (length(donut_values[[i]]) == 1) donut_values[[i]] else 1
      donut_col <- if (!is.null(donut_colors) && length(donut_colors) >= i) donut_colors[[i]][1] else node_fill[i]
      pie_vals <- pie_values[[i]]
      pie_cols <- if (!is.null(pie_colors) && length(pie_colors) >= i) pie_colors[[i]] else NULL

      draw_donut_pie_node_base(
        x, y, node_size[i],
        donut_value = donut_val,
        donut_color = donut_col,
        pie_values = pie_vals,
        pie_colors = pie_cols,
        inner_ratio = donut_inner_ratio,
        bg_color = donut_bg_color,
        border.col = node_border_color[i],
        border.width = node_border_width[i]
      )

    } else if (has_donut) {
      # Donut only
      donut_vals <- donut_values[[i]]
      donut_cols <- if (!is.null(donut_colors) && length(donut_colors) >= i) donut_colors[[i]] else node_fill[i]

      draw_donut_node_base(
        x, y, node_size[i],
        values = donut_vals,
        colors = donut_cols,
        inner_ratio = donut_inner_ratio,
        bg_color = donut_bg_color,
        border.col = node_border_color[i],
        border.width = node_border_width[i],
        show_value = donut_show_value,
        value_cex = donut_value_size,
        value_col = donut_value_color
      )

    } else if (has_pie) {
      # Pie only
      pie_vals <- pie_values[[i]]
      pie_cols <- if (!is.null(pie_colors) && length(pie_colors) >= i) pie_colors[[i]] else NULL

      draw_pie_node_base(
        x, y, node_size[i],
        values = pie_vals,
        colors = pie_cols,
        border.col = node_border_color[i],
        border.width = node_border_width[i]
      )

    } else {
      # Standard node
      if (usePCH && node_shape[i] == "circle") {
        # Fast point-based rendering
        graphics::points(x, y, pch = 21, cex = node_size[i] * 20,
                         bg = node_fill[i], col = node_border_color[i], lwd = node_border_width[i])
      } else {
        draw_node_base(
          x, y, node_size[i], node_size2[i],
          shape = node_shape[i],
          col = node_fill[i],
          border.col = node_border_color[i],
          border.width = node_border_width[i]
        )
      }
    }
  }

  # Render labels
  if (!is.null(labels)) {
    for (i in seq_len(n)) {
      if (!is.null(labels[i]) && !is.na(labels[i]) && labels[i] != "") {
        lx <- layout[i, 1]
        ly <- layout[i, 2]

        # Adjust position based on label_position
        offset <- node_size[i] * 1.2
        pos <- NULL

        if (label_position == "above") {
          ly <- ly + offset
        } else if (label_position == "below") {
          ly <- ly - offset
        } else if (label_position == "left") {
          lx <- lx - offset
        } else if (label_position == "right") {
          lx <- lx + offset
        }
        # "center" - no offset

        draw_node_label_base(
          lx, ly,
          label = labels[i],
          cex = label_size[i],
          col = label_color[i]
        )
      }
    }
  }
}


#' Render Legend for splot
#'
#' Renders a comprehensive legend showing node groups, edge weight colors,
#' and optionally node sizes.
#'
#' @param groups Group assignments for nodes.
#' @param node_names Names for legend entries.
#' @param nodes Node data frame.
#' @param node_colors Vector of node colors.
#' @param position Legend position.
#' @param cex Text size.
#' @param show_edge_colors Logical: show positive/negative edge color legend?
#' @param positive_color Positive edge color.
#' @param negative_color Negative edge color.
#' @param has_pos_edges Logical: are there positive weighted edges?
#' @param has_neg_edges Logical: are there negative weighted edges?
#' @param show_node_sizes Logical: show node size legend?
#' @param node_size Vector of node sizes.
#' @keywords internal
render_legend_splot <- function(groups, node_names, nodes, node_colors,
                                position = "topright", cex = 0.8,
                                show_edge_colors = FALSE,
                                positive_color = "#2E7D32", negative_color = "#C62828",
                                has_pos_edges = FALSE, has_neg_edges = FALSE,
                                show_node_sizes = FALSE, node_size = NULL) {

  n <- length(node_colors)

  # Collect all legend components
  legend_labels <- character(0)
  legend_colors <- character(0)
  legend_pch <- integer(0)
  legend_lty <- integer(0)
  legend_lwd <- numeric(0)
  legend_pt_cex <- numeric(0)

  # =========================================
  # 1. NODE GROUPS (filled squares)
  # =========================================
  if (!is.null(groups)) {
    unique_groups <- unique(groups)

    # Get color for each group (first node of that group)
    group_colors <- sapply(unique_groups, function(g) {
      idx <- which(groups == g)[1]
      node_colors[idx]
    })

    group_labels <- if (!is.null(node_names)) {
      sapply(unique_groups, function(g) {
        idx <- which(groups == g)[1]
        if (length(node_names) >= idx) node_names[idx] else as.character(g)
      })
    } else {
      as.character(unique_groups)
    }

    legend_labels <- c(legend_labels, group_labels)
    legend_colors <- c(legend_colors, group_colors)
    legend_pch <- c(legend_pch, rep(22, length(unique_groups)))  # filled square
    legend_lty <- c(legend_lty, rep(NA, length(unique_groups)))
    legend_lwd <- c(legend_lwd, rep(NA, length(unique_groups)))
    legend_pt_cex <- c(legend_pt_cex, rep(2, length(unique_groups)))
  }

  # =========================================
  # 2. EDGE COLORS (lines)
  # =========================================
  if (show_edge_colors && (has_pos_edges || has_neg_edges)) {
    # Add separator if we have groups
    if (length(legend_labels) > 0) {
      legend_labels <- c(legend_labels, "")
      legend_colors <- c(legend_colors, NA)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 0)
      legend_lwd <- c(legend_lwd, NA)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }

    if (has_pos_edges) {
      legend_labels <- c(legend_labels, "Positive")
      legend_colors <- c(legend_colors, positive_color)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 1)
      legend_lwd <- c(legend_lwd, 2)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }

    if (has_neg_edges) {
      legend_labels <- c(legend_labels, "Negative")
      legend_colors <- c(legend_colors, negative_color)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 1)
      legend_lwd <- c(legend_lwd, 2)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }
  }

  # =========================================
  # 3. NODE SIZES (circles of different sizes)
  # =========================================
  if (show_node_sizes && !is.null(node_size) && length(unique(node_size)) > 1) {
    # Add separator
    if (length(legend_labels) > 0) {
      legend_labels <- c(legend_labels, "")
      legend_colors <- c(legend_colors, NA)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 0)
      legend_lwd <- c(legend_lwd, NA)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }

    # Show min, median, max sizes
    size_range <- range(node_size)
    size_med <- median(node_size)
    size_vals <- c(size_range[1], size_med, size_range[2])
    size_labels <- c(
      paste0("Small (", round(size_range[1], 1), ")"),
      paste0("Medium (", round(size_med, 1), ")"),
      paste0("Large (", round(size_range[2], 1), ")")
    )

    # Scale for legend display
    scale_factor <- 15  # Adjust for visual appearance
    size_cex <- size_vals * scale_factor

    legend_labels <- c(legend_labels, size_labels)
    legend_colors <- c(legend_colors, rep("gray50", 3))
    legend_pch <- c(legend_pch, rep(21, 3))  # filled circle
    legend_lty <- c(legend_lty, rep(NA, 3))
    legend_lwd <- c(legend_lwd, rep(NA, 3))
    legend_pt_cex <- c(legend_pt_cex, size_cex)
  }

  # =========================================
  # Draw legend if we have entries
  # =========================================
  if (length(legend_labels) == 0) {
    return(invisible())
  }

  # Replace NA colors with transparent for proper rendering
  legend_colors[is.na(legend_colors)] <- "transparent"

  # Determine which elements to show
  has_points <- any(!is.na(legend_pch) & legend_pch > 0)
  has_lines <- any(!is.na(legend_lty) & legend_lty > 0)

  # Build legend
  graphics::legend(
    position,
    legend = legend_labels,
    col = legend_colors,
    pch = if (has_points) legend_pch else NULL,
    lty = if (has_lines) legend_lty else NULL,
    lwd = if (has_lines) legend_lwd else NULL,
    pt.cex = if (has_points) legend_pt_cex else NULL,
    pt.bg = if (has_points) legend_colors else NULL,
    bty = "o",
    bg = "white",
    cex = cex,
    seg.len = 1.5
  )
}
