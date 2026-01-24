#' @title Base R Edge Rendering
#' @description Edge drawing functions for splot() using base R graphics.
#' @name splot-edges
#' @keywords internal
NULL

#' Draw Straight Edge
#'
#' Renders a straight edge between two points with optional arrow.
#'
#' @param x1,y1 Start point coordinates.
#' @param x2,y2 End point coordinates.
#' @param col Edge color.
#' @param lwd Line width.
#' @param lty Line type.
#' @param arrow Logical: draw arrow at target?
#' @param asize Arrow size.
#' @param bidirectional Logical: draw arrow at source too?
#' @keywords internal
draw_straight_edge_base <- function(x1, y1, x2, y2, col = "gray50", lwd = 1,
                                    lty = 1, arrow = TRUE, asize = 0.02,
                                    bidirectional = FALSE) {
  # Calculate angle
  angle <- splot_angle(x1, y1, x2, y2)

  # Shorten end point if arrow
  if (arrow && asize > 0) {
    short_end <- shorten_edge_for_arrow(x1, y1, x2, y2, asize * 0.8)
    line_x2 <- short_end$x
    line_y2 <- short_end$y
  } else {
    line_x2 <- x2
    line_y2 <- y2
  }

  # Shorten start if bidirectional
  if (bidirectional && asize > 0) {
    short_start <- shorten_edge_for_arrow(x2, y2, x1, y1, asize * 0.8)
    line_x1 <- short_start$x
    line_y1 <- short_start$y
  } else {
    line_x1 <- x1
    line_y1 <- y1
  }

  # Draw line
  graphics::lines(
    x = c(line_x1, line_x2),
    y = c(line_y1, line_y2),
    col = col,
    lwd = lwd,
    lty = lty
  )

  # Draw arrow at target
  if (arrow && asize > 0) {
    draw_arrow_base(x2, y2, angle, asize, col = col)
  }

  # Draw arrow at source if bidirectional
  if (bidirectional && asize > 0) {
    angle_back <- splot_angle(x2, y2, x1, y1)
    draw_arrow_base(x1, y1, angle_back, asize, col = col)
  }
}

#' Draw Curved Edge with xspline (qgraph-style)
#'
#' Renders a curved edge using xspline() with optional arrow.
#' Uses qgraph-style curve calculation for smooth, natural-looking curves.
#'
#' @param x1,y1 Start point coordinates.
#' @param x2,y2 End point coordinates.
#' @param curve Curvature amount (positive = left, negative = right).
#' @param curvePivot Position along edge for control point (0-1).
#' @param col Edge color.
#' @param lwd Line width.
#' @param lty Line type.
#' @param arrow Logical: draw arrow at target?
#' @param asize Arrow size.
#' @param bidirectional Logical: draw arrow at source too?
#' @keywords internal
draw_curved_edge_base <- function(x1, y1, x2, y2, curve = 0.2, curvePivot = 0.5,
                                  col = "gray50", lwd = 1, lty = 1,
                                  arrow = TRUE, asize = 0.02,
                                  bidirectional = FALSE) {
  if (abs(curve) < 1e-6) {
    # Fall back to straight edge
    draw_straight_edge_base(x1, y1, x2, y2, col, lwd, lty, arrow, asize, bidirectional)
    return(invisible())
  }

  # Edge vector and length
  dx <- x2 - x1
  dy <- y2 - y1
  len <- sqrt(dx^2 + dy^2)

  if (len < 1e-10) {
    return(invisible())
  }

  # Perpendicular unit vector (rotated 90 degrees counterclockwise)
  px <- -dy / len
  py <- dx / len

  # qgraph-style: curve offset is absolute, not proportional to length
  # This gives consistent-looking curves regardless of edge length
  curve_offset <- curve * 0.5  # Scale factor for visual consistency

  # Create smooth curve using multiple control points (qgraph approach)
  # Use 5 points for smoother curve: start, 1/4, mid, 3/4, end
  t_vals <- c(0, 0.25, 0.5, 0.75, 1)
  n_pts <- length(t_vals)

  ctrl_x <- numeric(n_pts)
  ctrl_y <- numeric(n_pts)

  for (i in seq_along(t_vals)) {
    t <- t_vals[i]
    # Base point along edge
    bx <- x1 + t * dx
    by <- y1 + t * dy

    # Parabolic offset - maximum at curvePivot, zero at ends
    # This creates a smooth symmetric curve
    offset_factor <- 4 * t * (1 - t)  # Parabola peaking at t=0.5

    # Adjust for pivot position (shift the peak)
    if (curvePivot != 0.5) {
      # Skewed parabola
      if (t <= curvePivot) {
        offset_factor <- (t / curvePivot)^2 * 4 * curvePivot * (1 - curvePivot)
      } else {
        offset_factor <- ((1 - t) / (1 - curvePivot))^2 * 4 * curvePivot * (1 - curvePivot)
      }
    }

    ctrl_x[i] <- bx + curve_offset * offset_factor * px
    ctrl_y[i] <- by + curve_offset * offset_factor * py
  }

  # Generate smooth xspline through control points
  # shape = 1 for smooth interpolation, 0 for corners at endpoints
  spl <- graphics::xspline(
    x = ctrl_x,
    y = ctrl_y,
    shape = c(0, 1, 1, 1, 0),
    open = TRUE,
    draw = FALSE
  )

  # Draw the curve
  graphics::lines(
    x = spl$x,
    y = spl$y,
    col = col,
    lwd = lwd,
    lty = lty
  )

  # Draw arrow at target - use last segment of spline for angle
  if (arrow && asize > 0) {
    n <- length(spl$x)
    # Use points closer to end for better angle estimation
    idx <- max(1, n - 3)
    angle <- splot_angle(spl$x[idx], spl$y[idx], spl$x[n], spl$y[n])
    draw_arrow_base(x2, y2, angle, asize, col = col)
  }

  # Draw arrow at source if bidirectional
  if (bidirectional && asize > 0) {
    # Use points closer to start for angle
    idx <- min(length(spl$x), 4)
    angle_back <- splot_angle(spl$x[idx], spl$y[idx], spl$x[1], spl$y[1])
    draw_arrow_base(x1, y1, angle_back, asize, col = col)
  }
}

#' Draw Self-Loop Edge (qgraph-style)
#'
#' Renders a self-loop (edge from node to itself) using a teardrop/circular
#' loop shape similar to qgraph.
#'
#' @param x,y Node center coordinates.
#' @param node_size Node radius.
#' @param col Loop color.
#' @param lwd Line width.
#' @param lty Line type.
#' @param rotation Angle in radians for loop direction (default: pi/2 = top).
#' @param arrow Logical: draw arrow?
#' @param asize Arrow size.
#' @keywords internal
draw_self_loop_base <- function(x, y, node_size, col = "gray50", lwd = 1,
                                lty = 1, rotation = pi/2, arrow = TRUE,
                                asize = 0.02) {

  # qgraph-style loop: circular arc outside the node
  # Loop size proportional to node size
  loop_radius <- node_size * 0.8
  loop_dist <- node_size + loop_radius * 0.9  # Center of loop circle

  # Center of the loop arc (outside the node)
  loop_cx <- x + loop_dist * cos(rotation)
  loop_cy <- y + loop_dist * sin(rotation)

  # Generate circular arc (about 300 degrees, leaving gap for arrow)
  n_pts <- 40
  arc_start <- rotation + pi + 0.4  # Start angle (relative to loop center)
  arc_end <- rotation + pi - 0.4    # End angle

  # Handle angle wrapping
  if (arc_end < arc_start) {
    arc_end <- arc_end + 2 * pi
  }

  angles <- seq(arc_start, arc_end, length.out = n_pts)

  loop_x <- loop_cx + loop_radius * cos(angles)
  loop_y <- loop_cy + loop_radius * sin(angles)

  # Draw the loop
  graphics::lines(
    x = loop_x,
    y = loop_y,
    col = col,
    lwd = lwd,
    lty = lty
  )

  # Draw arrow at end of loop
  if (arrow && asize > 0) {
    n <- length(loop_x)
    # Arrow angle tangent to circle at endpoint
    angle <- splot_angle(loop_x[n-1], loop_y[n-1], loop_x[n], loop_y[n])
    draw_arrow_base(loop_x[n], loop_y[n], angle, asize, col = col)
  }
}

#' Draw Edge Label
#'
#' Renders a label on an edge.
#'
#' @param x,y Label position coordinates.
#' @param label Text to display.
#' @param cex Character expansion factor.
#' @param col Text color.
#' @param bg Background color (or NA for none).
#' @param font Font face.
#' @keywords internal
draw_edge_label_base <- function(x, y, label, cex = 0.8, col = "gray30",
                                 bg = "white", font = 1) {
  if (is.null(label) || is.na(label) || label == "") {
    return(invisible())
  }

  # Draw background if specified
  if (!is.na(bg) && !is.null(bg)) {
    # Estimate text size for background
    sw <- graphics::strwidth(label, cex = cex)
    sh <- graphics::strheight(label, cex = cex)
    pad <- 0.2

    graphics::rect(
      xleft = x - sw/2 - sw*pad,
      ybottom = y - sh/2 - sh*pad,
      xright = x + sw/2 + sw*pad,
      ytop = y + sh/2 + sh*pad,
      col = bg,
      border = NA
    )
  }

  graphics::text(
    x = x, y = y,
    labels = label,
    cex = cex,
    col = col,
    font = font
  )
}

#' Get Label Position on Edge
#'
#' Calculates the position for an edge label (matches qgraph-style curves).
#'
#' @param x1,y1 Start point.
#' @param x2,y2 End point.
#' @param position Position along edge (0-1).
#' @param curve Curvature amount.
#' @param curvePivot Curve pivot position.
#' @return List with x, y coordinates.
#' @keywords internal
get_edge_label_position <- function(x1, y1, x2, y2, position = 0.5,
                                    curve = 0, curvePivot = 0.5) {
  if (abs(curve) < 1e-6) {
    # Straight edge
    return(list(
      x = x1 + position * (x2 - x1),
      y = y1 + position * (y2 - y1)
    ))
  }

  # Curved edge - match qgraph-style curve calculation
  dx <- x2 - x1
  dy <- y2 - y1
  len <- sqrt(dx^2 + dy^2)

  if (len < 1e-10) {
    return(list(x = x1, y = y1))
  }

  # Perpendicular unit vector
  px <- -dy / len
  py <- dx / len

  # Same curve offset as draw_curved_edge_base
  curve_offset <- curve * 0.5

  # Base point along edge
  t <- position
  bx <- x1 + t * dx
  by <- y1 + t * dy

  # Parabolic offset
  offset_factor <- 4 * t * (1 - t)

  if (curvePivot != 0.5) {
    if (t <= curvePivot) {
      offset_factor <- (t / curvePivot)^2 * 4 * curvePivot * (1 - curvePivot)
    } else {
      offset_factor <- ((1 - t) / (1 - curvePivot))^2 * 4 * curvePivot * (1 - curvePivot)
    }
  }

  list(
    x = bx + curve_offset * offset_factor * px,
    y = by + curve_offset * offset_factor * py
  )
}

#' Render All Edges
#'
#' Renders all edges in the network.
#'
#' @param edges Edge data frame with from, to columns.
#' @param layout Matrix with x, y columns.
#' @param node_sizes Vector of node sizes.
#' @param shapes Vector of node shapes.
#' @param edge.color Vector of edge colors.
#' @param edge.width Vector of edge widths.
#' @param lty Vector of line types.
#' @param curve Vector of curvatures.
#' @param curvePivot Vector of curve pivot positions.
#' @param arrows Logical or vector: draw arrows?
#' @param asize Arrow size.
#' @param bidirectional Logical or vector: bidirectional arrows?
#' @param loopRotation Vector of loop rotation angles.
#' @param edge.labels Vector of edge labels or NULL.
#' @param edge.label.cex Label size.
#' @param edge.label.bg Label background color.
#' @param edge.label.position Label position along edge.
#' @keywords internal
render_edges_base <- function(edges, layout, node_sizes, shapes = "circle",
                              edge.color = "gray50", edge.width = 1, lty = 1,
                              curve = 0, curvePivot = 0.5, arrows = TRUE,
                              asize = 0.02, bidirectional = FALSE,
                              loopRotation = NULL, edge.labels = NULL,
                              edge.label.cex = 0.8, edge.label.bg = "white",
                              edge.label.position = 0.5) {
  m <- nrow(edges)
  if (m == 0) return(invisible())

  n <- nrow(layout)

  # Vectorize parameters
  edge.color <- recycle_to_length(edge.color, m)
  edge.width <- recycle_to_length(edge.width, m)
  lty <- recycle_to_length(lty, m)
  curve <- recycle_to_length(curve, m)
  curvePivot <- recycle_to_length(curvePivot, m)
  arrows <- recycle_to_length(arrows, m)
  asize <- recycle_to_length(asize, m)
  bidirectional <- recycle_to_length(bidirectional, m)
  node_sizes <- recycle_to_length(node_sizes, n)
  shapes <- recycle_to_length(shapes, n)

  # Loop rotation
  if (is.null(loopRotation)) {
    loopRotation <- resolve_loop_rotation(NULL, edges, layout)
  } else {
    loopRotation <- recycle_to_length(loopRotation, m)
  }

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

      # Label position for self-loop (at top of loop)
      loop_dist <- node_sizes[from_idx] * 2.5
      label_positions[[i]] <- list(
        x = x1 + loop_dist * cos(loopRotation[i]),
        y = y1 + loop_dist * sin(loopRotation[i])
      )
      next
    }

    # Calculate edge endpoints (offset from node centers)
    angle_to <- splot_angle(x1, y1, x2, y2)
    angle_from <- splot_angle(x2, y2, x1, y1)

    start <- cent_to_edge(x1, y1, angle_to, node_sizes[from_idx], NULL, shapes[from_idx])
    end <- cent_to_edge(x2, y2, angle_from, node_sizes[to_idx], NULL, shapes[to_idx])

    # Draw edge
    if (abs(curve[i]) > 1e-6) {
      draw_curved_edge_base(
        start$x, start$y, end$x, end$y,
        curve = curve[i],
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

    # Store label position
    label_positions[[i]] <- get_edge_label_position(
      start$x, start$y, end$x, end$y,
      position = edge.label.position,
      curve = curve[i],
      curvePivot = curvePivot[i]
    )
  }

  # Draw edge labels
  if (!is.null(edge.labels)) {
    edge.labels <- recycle_to_length(edge.labels, m)

    for (i in seq_len(m)) {
      if (!is.null(edge.labels[i]) && !is.na(edge.labels[i]) && edge.labels[i] != "") {
        pos <- label_positions[[i]]
        draw_edge_label_base(
          pos$x, pos$y,
          label = edge.labels[i],
          cex = edge.label.cex,
          col = "gray30",
          bg = edge.label.bg
        )
      }
    }
  }
}
