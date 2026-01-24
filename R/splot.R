#' @title Base R Graphics Network Plotting
#' @description Network visualization using base R graphics (similar to qgraph).
#' @name splot
NULL

#' Plot Network with Base R Graphics
#'
#' Creates a network visualization using base R graphics functions (polygon,
#' lines, xspline, etc.) instead of grid graphics. This provides better
#' performance for large networks and qgraph-familiar parameter names.
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
#' @param vsize Node size(s). Single value or vector. Default 3.
#' @param vsize2 Secondary node size for ellipse/rectangle height.
#' @param shape Node shape(s): "circle", "square", "triangle", "diamond",
#'   "pentagon", "hexagon", "star", "heart", "ellipse", "cross".
#' @param color Node fill color(s).
#' @param border.color Node border color(s).
#' @param border.width Node border width(s).
#' @param alpha Node transparency (0-1). Default 1.
#' @param labels Node labels: TRUE (use node names/indices), FALSE (none),
#'   or character vector.
#' @param label.cex Label character expansion factor.
#' @param label.color Label text color.
#' @param label.position Label position: "center", "above", "below", "left", "right".
#'
#' @section Pie/Donut Nodes:
#' @param pie List of numeric vectors for pie chart nodes. Each element
#'   corresponds to a node and contains values for pie segments.
#' @param pieColor List of color vectors for pie segments.
#' @param donut List of values for donut chart nodes. Single value (0-1)
#'   for progress donut, or vector for segmented donut.
#' @param donutColor List of color vectors for donut segments.
#' @param donut.inner Inner radius ratio for donut (0-1). Default 0.5.
#' @param donut.bg Background color for unfilled donut portion.
#' @param donut.show.value Logical: show value in donut center? Default TRUE.
#' @param donut.value.cex Font size for donut center value.
#' @param donut.value.color Color for donut center value.
#'
#' @section Edge Aesthetics:
#' @param edge.color Edge color(s). If NULL, uses posCol/negCol based on weight.
#' @param edge.width Edge width(s). If NULL, scales by weight.
#' @param edge.alpha Edge transparency (0-1). Default 0.8.
#' @param edge.labels Edge labels: TRUE (show weights), FALSE (none),
#'   or character vector.
#' @param edge.label.cex Edge label size.
#' @param edge.label.color Edge label text color.
#' @param edge.label.bg Edge label background color.
#' @param edge.label.position Position along edge (0-1).
#' @param edge.label.font Font face: 1=plain, 2=bold, 3=italic.
#' @param lty Line type(s): 1=solid, 2=dashed, 3=dotted, etc.
#' @param curve Edge curvature. 0 for straight, positive/negative for curves.
#' @param curveScale Logical: auto-curve reciprocal edges?
#' @param curveShape Spline tension (-1 to 1). Default 0.
#' @param curvePivot Position along edge for curve control point (0-1).
#' @param curves Curve mode: TRUE (default) = single edges straight, reciprocal edges
#'   curve as ellipse (two opposing curves); FALSE = all straight; "force" = all curved.
#' @param asize Arrow head size.
#' @param arrows Logical or vector: show arrows on directed edges?
#' @param bidirectional Logical or vector: show arrows at both ends?
#' @param loopRotation Angle(s) in radians for self-loop direction.
#'
#' @section Weight Handling:
#' @param minimum Minimum absolute weight to display (threshold).
#' @param maximum Maximum weight for scaling. NULL for auto.
#' @param posCol Color for positive weights.
#' @param negCol Color for negative weights.
#'
#' @section Plot Settings:
#' @param title Plot title.
#' @param title.cex Title font size.
#' @param mar Margins as c(bottom, left, top, right).
#' @param background Background color.
#' @param rescale Logical: rescale layout to [-1, 1]?
#' @param aspect Logical: maintain aspect ratio?
#' @param usePCH Logical: use points() for simple circles (faster). Default FALSE.
#'
#' @section Legend:
#' @param legend Logical: show legend?
#' @param legend.position Position: "topright", "topleft", "bottomright", "bottomleft".
#' @param legend.cex Legend text size.
#' @param legend.edge.colors Logical: show positive/negative edge colors in legend?
#' @param legend.node.sizes Logical: show node size scale in legend?
#' @param groups Group assignments for node coloring/legend.
#' @param nodeNames Alternative names for legend (separate from labels).
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
#' splot(adj, curve = 0.2)
#'
#' # Weighted network with colors
#' w_adj <- matrix(c(0, 0.5, -0.3, 0,
#'                   0.8, 0, 0.4, -0.2,
#'                   0, 0, 0, 0.6,
#'                   0, 0, 0, 0), 4, 4, byrow = TRUE)
#' splot(w_adj, posCol = "darkgreen", negCol = "red")
#'
#' # Pie chart nodes
#' splot(adj, pie = list(c(1,2,3), c(2,2), c(1,1,1,1), c(3,1)))
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
    vsize = NULL,
    vsize2 = NULL,
    shape = "circle",
    color = NULL,
    border.color = NULL,
    border.width = 1,
    alpha = 1,
    labels = TRUE,
    label.cex = NULL,
    label.color = "black",
    label.position = "center",

    # Pie/Donut
    pie = NULL,
    pieColor = NULL,
    donut = NULL,
    donutColor = NULL,
    donut.inner = 0.5,
    donut.bg = "gray90",
    donut.show.value = TRUE,
    donut.value.cex = 0.8,
    donut.value.color = "black",

    # Edge aesthetics
    edge.color = NULL,
    edge.width = NULL,
    edge.alpha = 0.8,
    edge.labels = FALSE,
    edge.label.cex = 0.8,
    edge.label.color = "gray30",
    edge.label.bg = "white",
    edge.label.position = 0.5,
    edge.label.font = 1,
    lty = 1,
    curve = 0,
    curveScale = TRUE,
    curveShape = 0,
    curvePivot = 0.5,
    curves = TRUE,
    asize = 1,
    arrows = TRUE,
    bidirectional = FALSE,
    loopRotation = NULL,

    # Weight handling
    minimum = 0,
    maximum = NULL,
    posCol = "#2E7D32",
    negCol = "#C62828",

    # Plot settings
    title = NULL,
    title.cex = 1.2,
    mar = c(0.1, 0.1, 0.1, 0.1),
    background = "white",
    rescale = TRUE,
    aspect = TRUE,
    usePCH = FALSE,

    # Legend
    legend = FALSE,
    legend.position = "topright",
    legend.cex = 0.8,
    legend.edge.colors = TRUE,
    legend.node.sizes = FALSE,
    groups = NULL,
    nodeNames = NULL,

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
      if (is.null(color)) color <- th$get("node_fill")
      if (is.null(border.color)) border.color <- th$get("node_border_color")
      if (is.null(background)) background <- th$get("background")
      if (label.color == "black") label.color <- th$get("label_color")
      if (posCol == "#2E7D32") posCol <- th$get("edge_positive_color")
      if (negCol == "#C62828") negCol <- th$get("edge_negative_color")
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
  if (is.null(vsize)) vsize <- 3
  vsize_usr <- resolve_node_sizes(vsize, n_nodes, default_size = 3, scale_factor = 0.04)
  vsize2_usr <- if (!is.null(vsize2)) {
    resolve_node_sizes(vsize2, n_nodes, default_size = 3, scale_factor = 0.04)
  } else {
    vsize_usr
  }

  # Node shapes
  shapes <- resolve_shapes(shape, n_nodes)

  # Node colors
  node_colors <- resolve_node_colors(color, n_nodes, nodes, groups)

  # Apply alpha to node colors
  if (alpha < 1) {
    node_colors <- sapply(node_colors, function(c) adjust_alpha(c, alpha))
  }

  # Border colors
  if (is.null(border.color)) {
    border.color <- sapply(node_colors, function(c) {
      tryCatch(adjust_brightness(c, -0.3), error = function(e) "black")
    })
  }
  border_colors <- recycle_to_length(border.color, n_nodes)

  # Border widths
  border_widths <- recycle_to_length(border.width, n_nodes)

  # Labels
  node_labels <- resolve_labels(labels, nodes, n_nodes)

  # Label sizes
  if (is.null(label.cex)) {
    label.cex <- pmin(1, vsize_usr * 8)
  }
  label_cex <- recycle_to_length(label.cex, n_nodes)
  label_colors <- recycle_to_length(label.color, n_nodes)

  # ============================================
  # 4. EDGE PROCESSING
  # ============================================

  if (n_edges > 0) {
    # Filter by minimum weight (threshold)
    edges <- filter_edges_by_weight(edges, minimum)
    n_edges <- nrow(edges)
  }

  if (n_edges > 0) {
    # Edge colors
    edge_colors <- resolve_edge_colors(edges, edge.color, posCol, negCol)

    # Apply edge alpha
    if (edge.alpha < 1) {
      edge_colors <- sapply(edge_colors, function(c) adjust_alpha(c, edge.alpha))
    }

    # Edge widths
    edge_widths <- resolve_edge_widths(edges, edge.width, maximum, minimum)

    # Line types
    ltys <- recycle_to_length(lty, n_edges)

    # Handle curves mode:
    # FALSE = all straight
    # TRUE or "mutual" = only reciprocal edges curved (opposite directions)
    # "force" = all edges curved (reciprocals opposite, singles inward)
    curves_vec <- recycle_to_length(curve, n_edges)
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

    curve_pivots <- recycle_to_length(curvePivot, n_edges)
    curve_shapes <- recycle_to_length(curveShape, n_edges)

    # Arrows
    if (is.logical(arrows) && length(arrows) == 1) {
      show_arrows <- rep(directed && arrows, n_edges)
    } else {
      show_arrows <- recycle_to_length(arrows, n_edges)
    }

    # Arrow size (convert from qgraph scale)
    arrow_size <- asize * 0.03
    arrow_sizes <- recycle_to_length(arrow_size, n_edges)

    # Bidirectional
    bidirectionals <- recycle_to_length(bidirectional, n_edges)

    # Loop rotation
    loop_rotations <- resolve_loop_rotation(loopRotation, edges, layout_mat)

    # Edge labels
    edge_labels_vec <- resolve_edge_labels(edge.labels, edges, n_edges)
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
  graphics::par(mar = c(mar[1], mar[2], mar[3] + title_space, mar[4]))

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
    graphics::title(main = title, cex.main = title.cex)
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
      edge.color = edge_colors,
      edge.width = edge_widths,
      lty = ltys,
      curve = curves_vec,
      curveShape = curve_shapes,
      curvePivot = curve_pivots,
      arrows = show_arrows,
      asize = arrow_sizes,
      bidirectional = bidirectionals,
      loopRotation = loop_rotations,
      edge.labels = edge_labels_vec,
      edge.label.cex = edge.label.cex,
      edge.label.color = edge.label.color,
      edge.label.bg = edge.label.bg,
      edge.label.position = edge.label.position,
      edge.label.font = edge.label.font
    )
  }

  # ============================================
  # 7. RENDER NODES
  # ============================================

  render_nodes_splot(
    layout = layout_mat,
    vsize = vsize_usr,
    vsize2 = vsize2_usr,
    shape = shapes,
    color = node_colors,
    border.color = border_colors,
    border.width = border_widths,
    pie = pie,
    pieColor = pieColor,
    donut = donut,
    donutColor = donutColor,
    donut.inner = donut.inner,
    donut.bg = donut.bg,
    donut.show.value = donut.show.value,
    donut.value.cex = donut.value.cex,
    donut.value.color = donut.value.color,
    labels = node_labels,
    label.cex = label_cex,
    label.color = label_colors,
    label.position = label.position,
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
      nodeNames = nodeNames,
      nodes = nodes,
      node_colors = node_colors,
      position = legend.position,
      cex = legend.cex,
      show_edge_colors = legend.edge.colors,
      posCol = posCol,
      negCol = negCol,
      has_pos_edges = has_pos_edges,
      has_neg_edges = has_neg_edges,
      show_node_sizes = legend.node.sizes,
      vsize = vsize_usr
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
                               edge.color, edge.width, lty, curve,
                               curveShape, curvePivot, arrows, asize,
                               bidirectional, loopRotation, edge.labels,
                               edge.label.cex, edge.label.color, edge.label.bg,
                               edge.label.position, edge.label.font) {

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
        col = edge.color[i],
        lwd = edge.width[i],
        lty = lty[i],
        rotation = loopRotation[i],
        arrow = arrows[i],
        asize = asize[i]
      )

      # Label position for self-loop
      loop_dist <- node_sizes[from_idx] * 2.5
      label_positions[[i]] <- list(
        x = x1 + loop_dist * cos(loopRotation[i]),
        y = y1 + loop_dist * sin(loopRotation[i])
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
    curve_i <- curve[i]
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
        curvePivot = curvePivot[i],
        col = edge.color[i],
        lwd = edge.width[i],
        lty = lty[i],
        arrow = arrows[i],
        asize = asize[i],
        bidirectional = bidirectional[i]
      )
    } else {
      draw_straight_edge_base(
        start$x, start$y, end$x, end$y,
        col = edge.color[i],
        lwd = edge.width[i],
        lty = lty[i],
        arrow = arrows[i],
        asize = asize[i],
        bidirectional = bidirectional[i]
      )
    }

    # Store label position (use adjusted curve for correct position)
    label_positions[[i]] <- get_edge_label_position(
      start$x, start$y, end$x, end$y,
      position = edge.label.position,
      curve = curve_i,
      curvePivot = curvePivot[i]
    )
  }

  # Draw edge labels
  if (!is.null(edge.labels)) {
    for (i in seq_len(m)) {
      if (!is.null(edge.labels[i]) && !is.na(edge.labels[i]) && edge.labels[i] != "") {
        pos <- label_positions[[i]]
        draw_edge_label_base(
          pos$x, pos$y,
          label = edge.labels[i],
          cex = edge.label.cex,
          col = edge.label.color,
          bg = edge.label.bg,
          font = edge.label.font
        )
      }
    }
  }
}


#' Render Nodes for splot
#' @keywords internal
render_nodes_splot <- function(layout, vsize, vsize2, shape, color,
                               border.color, border.width, pie, pieColor,
                               donut, donutColor, donut.inner, donut.bg,
                               donut.show.value, donut.value.cex, donut.value.color,
                               labels, label.cex, label.color, label.position,
                               usePCH = FALSE) {

  n <- nrow(layout)
  if (n == 0) return(invisible())

  # Render order: largest to smallest
  order_idx <- get_node_order(vsize)

  for (i in order_idx) {
    x <- layout[i, 1]
    y <- layout[i, 2]

    # Check for pie/donut
    has_pie <- !is.null(pie) && length(pie) >= i && !is.null(pie[[i]]) && length(pie[[i]]) > 0
    has_donut <- !is.null(donut) && length(donut) >= i && !is.null(donut[[i]])

    if (has_donut && has_pie) {
      # Donut with inner pie
      donut_val <- if (length(donut[[i]]) == 1) donut[[i]] else 1
      donut_col <- if (!is.null(donutColor) && length(donutColor) >= i) donutColor[[i]][1] else color[i]
      pie_vals <- pie[[i]]
      pie_cols <- if (!is.null(pieColor) && length(pieColor) >= i) pieColor[[i]] else NULL

      draw_donut_pie_node_base(
        x, y, vsize[i],
        donut_value = donut_val,
        donut_color = donut_col,
        pie_values = pie_vals,
        pie_colors = pie_cols,
        inner_ratio = donut.inner,
        bg_color = donut.bg,
        border.col = border.color[i],
        border.width = border.width[i]
      )

    } else if (has_donut) {
      # Donut only
      donut_vals <- donut[[i]]
      donut_cols <- if (!is.null(donutColor) && length(donutColor) >= i) donutColor[[i]] else color[i]

      draw_donut_node_base(
        x, y, vsize[i],
        values = donut_vals,
        colors = donut_cols,
        inner_ratio = donut.inner,
        bg_color = donut.bg,
        border.col = border.color[i],
        border.width = border.width[i],
        show_value = donut.show.value,
        value_cex = donut.value.cex,
        value_col = donut.value.color
      )

    } else if (has_pie) {
      # Pie only
      pie_vals <- pie[[i]]
      pie_cols <- if (!is.null(pieColor) && length(pieColor) >= i) pieColor[[i]] else NULL

      draw_pie_node_base(
        x, y, vsize[i],
        values = pie_vals,
        colors = pie_cols,
        border.col = border.color[i],
        border.width = border.width[i]
      )

    } else {
      # Standard node
      if (usePCH && shape[i] == "circle") {
        # Fast point-based rendering
        graphics::points(x, y, pch = 21, cex = vsize[i] * 20,
                         bg = color[i], col = border.color[i], lwd = border.width[i])
      } else {
        draw_node_base(
          x, y, vsize[i], vsize2[i],
          shape = shape[i],
          col = color[i],
          border.col = border.color[i],
          border.width = border.width[i]
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

        # Adjust position based on label.position
        offset <- vsize[i] * 1.2
        pos <- NULL

        if (label.position == "above") {
          ly <- ly + offset
        } else if (label.position == "below") {
          ly <- ly - offset
        } else if (label.position == "left") {
          lx <- lx - offset
        } else if (label.position == "right") {
          lx <- lx + offset
        }
        # "center" - no offset

        draw_node_label_base(
          lx, ly,
          label = labels[i],
          cex = label.cex[i],
          col = label.color[i]
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
#' @param nodeNames Names for legend entries.
#' @param nodes Node data frame.
#' @param node_colors Vector of node colors.
#' @param position Legend position.
#' @param cex Text size.
#' @param show_edge_colors Logical: show positive/negative edge color legend?
#' @param posCol Positive edge color.
#' @param negCol Negative edge color.
#' @param has_pos_edges Logical: are there positive weighted edges?
#' @param has_neg_edges Logical: are there negative weighted edges?
#' @param show_node_sizes Logical: show node size legend?
#' @param vsize Vector of node sizes.
#' @keywords internal
render_legend_splot <- function(groups, nodeNames, nodes, node_colors,
                                position = "topright", cex = 0.8,
                                show_edge_colors = FALSE,
                                posCol = "#2E7D32", negCol = "#C62828",
                                has_pos_edges = FALSE, has_neg_edges = FALSE,
                                show_node_sizes = FALSE, vsize = NULL) {

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

    group_labels <- if (!is.null(nodeNames)) {
      sapply(unique_groups, function(g) {
        idx <- which(groups == g)[1]
        if (length(nodeNames) >= idx) nodeNames[idx] else as.character(g)
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
      legend_colors <- c(legend_colors, posCol)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 1)
      legend_lwd <- c(legend_lwd, 2)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }

    if (has_neg_edges) {
      legend_labels <- c(legend_labels, "Negative")
      legend_colors <- c(legend_colors, negCol)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 1)
      legend_lwd <- c(legend_lwd, 2)
      legend_pt_cex <- c(legend_pt_cex, NA)
    }
  }

  # =========================================
  # 3. NODE SIZES (circles of different sizes)
  # =========================================
  if (show_node_sizes && !is.null(vsize) && length(unique(vsize)) > 1) {
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
    size_range <- range(vsize)
    size_med <- median(vsize)
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
