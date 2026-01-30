#' @title Base R Edge Rendering
#' @description Edge drawing functions for splot() using base R graphics.
#' @name splot-edges
#' @keywords internal
NULL

#' Find Split Index for Curve Based on Arc Length Fraction
#'
#' Calculates the index at which to split a curve's coordinate arrays
#' so that the first segment covers a given fraction of the total arc length.
#'
#' @param x,y Vectors of curve coordinates.
#' @param fraction Desired fraction of total arc length (0-1).
#' @return Index at which to split the arrays.
#' @keywords internal
find_curve_split_index <- function(x, y, fraction) {
  n <- length(x)
  if (n < 2 || fraction <= 0) return(1)
  if (fraction >= 1) return(n)

  # Calculate cumulative arc length
  dx <- diff(x)
  dy <- diff(y)
  segment_lengths <- sqrt(dx^2 + dy^2)
  cumulative_length <- c(0, cumsum(segment_lengths))
  total_length <- cumulative_length[n]

  if (total_length < 1e-10) return(1)

  # Find index where cumulative length crosses target
  target_length <- total_length * fraction
  split_idx <- which(cumulative_length >= target_length)[1]

  # Ensure at least 2 points in each segment
  split_idx <- max(2, min(split_idx, n - 1))

  return(split_idx)
}

#' Draw Curve with Optional Start Segment
#'
#' Draws a curve (as lines) with an optional differently-styled start segment.
#' Used internally to support dashed/dotted start segments for edge direction clarity.
#'
#' @param x,y Vectors of curve coordinates.
#' @param col Line color.
#' @param lwd Line width.
#' @param lty Main line type.
#' @param start_lty Line type for start segment.
#' @param start_fraction Fraction of curve for start segment (0-0.5).
#' @keywords internal
draw_curve_with_start_segment <- function(x, y, col, lwd, lty,
                                          start_lty = 1, start_fraction = 0) {
  n <- length(x)
  if (n < 2) return(invisible())

  # If no split needed, draw single line
  if (start_fraction <= 0 || start_lty == lty) {
    graphics::lines(x, y, col = col, lwd = lwd, lty = lty)
    return(invisible())
  }

  # Convert dotted (lty=3) to tighter pattern for better appearance
  effective_start_lty <- if (identical(start_lty, 3L) || identical(start_lty, 3)) "11" else start_lty

  # Find split index based on arc length
  split_idx <- find_curve_split_index(x, y, start_fraction)

  # Draw start segment (dashed/dotted)
  if (split_idx >= 2) {
    graphics::lines(x[1:split_idx], y[1:split_idx],
                    col = col, lwd = lwd, lty = effective_start_lty)
  }

  # Draw main segment (solid)
  if (split_idx < n) {
    graphics::lines(x[split_idx:n], y[split_idx:n],
                    col = col, lwd = lwd, lty = lty)
  }

  invisible()
}

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
#' @param start_lty Line type for start segment. 1=solid (default), 2=dashed, 3=dotted.
#' @param start_fraction Fraction of edge length for start segment (0-0.5). Default 0.
#' @keywords internal
draw_straight_edge_base <- function(x1, y1, x2, y2, col = "gray50", lwd = 1,
                                    lty = 1, arrow = TRUE, asize = 0.02,
                                    bidirectional = FALSE,
                                    start_lty = 1, start_fraction = 0) {
  # Calculate angle
  angle <- splot_angle(x1, y1, x2, y2)

  # qgraph-style: line ends at arrow base midpoint, arrow TIP at node boundary
  if (arrow && asize > 0) {
    # Get arrow base midpoint (where line should end)
    base_end <- arrow_base_midpoint(x2, y2, angle, asize)
    line_x2 <- base_end$x
    line_y2 <- base_end$y
  } else {
    line_x2 <- x2
    line_y2 <- y2
  }

  # Shorten start if bidirectional
  if (bidirectional && asize > 0) {
    angle_back <- splot_angle(x2, y2, x1, y1)
    base_start <- arrow_base_midpoint(x1, y1, angle_back, asize)
    line_x1 <- base_start$x
    line_y1 <- base_start$y
  } else {
    line_x1 <- x1
    line_y1 <- y1
  }

  # Draw line (ends at arrow base, not at tip)
  # If start_lty differs from main lty and start_fraction > 0, split into two segments
  if (start_fraction > 0 && start_lty != lty) {
    # Calculate split point
    split_x <- line_x1 + start_fraction * (line_x2 - line_x1)
    split_y <- line_y1 + start_fraction * (line_y2 - line_y1)

    # Convert dotted (lty=3) to tighter pattern for better appearance
    effective_start_lty <- if (identical(start_lty, 3L) || identical(start_lty, 3)) "11" else start_lty

    # Draw start segment (dashed/dotted)
    graphics::lines(
      x = c(line_x1, split_x),
      y = c(line_y1, split_y),
      col = col,
      lwd = lwd,
      lty = effective_start_lty
    )

    # Draw main segment (solid)
    graphics::lines(
      x = c(split_x, line_x2),
      y = c(split_y, line_y2),
      col = col,
      lwd = lwd,
      lty = lty
    )
  } else {
    # Single line with uniform style
    graphics::lines(
      x = c(line_x1, line_x2),
      y = c(line_y1, line_y2),
      col = col,
      lwd = lwd,
      lty = lty
    )
  }

  # Draw arrow at target (TIP at node boundary)
  if (arrow && asize > 0) {
    draw_arrow_base(x2, y2, angle, asize, col = col)
  }

  # Draw arrow at source if bidirectional (TIP at node boundary)
  if (bidirectional && asize > 0) {
    angle_back <- splot_angle(x2, y2, x1, y1)
    draw_arrow_base(x1, y1, angle_back, asize, col = col)
  }
}

#' Draw Curved Edge with xspline (qgraph-style)
#'
#' Renders a curved edge using xspline() with optional arrow.
#' Uses qgraph-style curve calculation for smooth, natural-looking curves.
#' Curve direction is normalized so positive curve always bends the same
#' visual direction regardless of edge orientation.
#'
#' @param x1,y1 Start point coordinates.
#' @param x2,y2 End point coordinates.
#' @param curve Curvature amount (positive = clockwise, negative = counterclockwise
#'   when looking from source to target).
#' @param curvePivot Position along edge for control point (0-1).
#' @param col Edge color.
#' @param lwd Line width.
#' @param lty Line type.
#' @param arrow Logical: draw arrow at target?
#' @param asize Arrow size.
#' @param bidirectional Logical: draw arrow at source too?
#' @param start_lty Line type for start segment. 1=solid (default), 2=dashed, 3=dotted.
#' @param start_fraction Fraction of edge length for start segment (0-0.5). Default 0.
#' @keywords internal
draw_curved_edge_base <- function(x1, y1, x2, y2, curve = 0.2, curvePivot = 0.5,
                                  col = "gray50", lwd = 1, lty = 1,
                                  arrow = TRUE, asize = 0.02,
                                  bidirectional = FALSE,
                                  start_lty = 1, start_fraction = 0) {
  if (abs(curve) < 1e-6) {
    # Fall back to straight edge
    draw_straight_edge_base(x1, y1, x2, y2, col, lwd, lty, arrow, asize, bidirectional,
                            start_lty, start_fraction)
    return(invisible())
  }

  # Edge vector and length
  dx <- x2 - x1
  dy <- y2 - y1
  len <- sqrt(dx^2 + dy^2)

  # Defensive check for empty or NA values
  if (length(len) == 0 || is.na(len) || len < 1e-10) {
    return(invisible())
  }


  # Perpendicular unit vector (rotated 90 degrees counter-clockwise)
  # Matches the (-dy, dx) convention used by the curve direction algorithm
  px <- -dy / len
  py <- dx / len

  # Curve offset: proportional to edge length, with minimum so short edges still visibly curve
  curve_offset <- curve * len * 0.25
  min_offset <- abs(curve) * 0.3  # minimum offset ensures reciprocal edges are distinguishable
  if (abs(curve_offset) > 0 && abs(curve_offset) < min_offset) {
    curve_offset <- sign(curve_offset) * min_offset
  }

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

  # qgraph-style arrow positioning:
  # 1. Calculate arrow angle from curve direction
  # 2. Truncate curve to stop at arrow base
  # 3. Draw arrow with TIP at node boundary

  if (arrow && asize > 0) {
    n <- length(spl$x)

    # 1. Calculate arrow angle from last curve segment
    idx <- max(1, n - 3)
    angle <- splot_angle(spl$x[idx], spl$y[idx], x2, y2)

    # 2. Find arrow base midpoint (where curve should end)
    base <- arrow_base_midpoint(x2, y2, angle, asize)

    # 3. Truncate curve: remove points inside arrow radius
    arrow_rad <- asize  # Arrow extends this far back from tip
    dists <- sqrt((spl$x - x2)^2 + (spl$y - y2)^2)
    outside <- dists > arrow_rad

    # Keep only points outside the arrow (qgraph approach)
    keep_idx <- which(rev(cumsum(rev(outside)) > 0))

    # 4. Draw truncated curve + line to arrow base
    if (length(keep_idx) > 0) {
      curve_x <- c(spl$x[keep_idx], base$x)
      curve_y <- c(spl$y[keep_idx], base$y)
      draw_curve_with_start_segment(curve_x, curve_y, col, lwd, lty,
                                    start_lty, start_fraction)
    }

    # 5. Draw arrow with TIP at node boundary (x2, y2)
    draw_arrow_base(x2, y2, angle, asize, col = col)
  } else {
    # No arrow - draw full curve
    draw_curve_with_start_segment(spl$x, spl$y, col, lwd, lty,
                                  start_lty, start_fraction)
  }

  # Draw arrow at source if bidirectional
  if (bidirectional && asize > 0) {
    n <- length(spl$x)

    # Calculate angle from curve start
    idx <- min(n, 4)
    angle_back <- splot_angle(spl$x[idx], spl$y[idx], x1, y1)

    # Find arrow base midpoint at source
    base_start <- arrow_base_midpoint(x1, y1, angle_back, asize)

    # Truncate curve at source: remove points inside arrow radius
    dists_start <- sqrt((spl$x - x1)^2 + (spl$y - y1)^2)
    outside_start <- dists_start > asize

    # Keep only points outside the start arrow
    keep_idx_start <- which(cumsum(outside_start) > 0)

    # Redraw if we need to truncate the start (overwrites previous line)
    if (length(keep_idx_start) > 0 && length(keep_idx_start) < n) {
      # Clear and redraw with both ends truncated
      curve_x <- c(base_start$x, spl$x[keep_idx_start])
      curve_y <- c(base_start$y, spl$y[keep_idx_start])

      # If target also has arrow, truncate that end too
      if (arrow && asize > 0) {
        dists_end <- sqrt((curve_x - x2)^2 + (curve_y - y2)^2)
        outside_end <- dists_end > asize
        keep_end <- which(rev(cumsum(rev(outside_end)) > 0))
        if (length(keep_end) > 0) {
          angle_fwd <- splot_angle(spl$x[n-3], spl$y[n-3], x2, y2)
          base_end <- arrow_base_midpoint(x2, y2, angle_fwd, asize)
          curve_x <- c(curve_x[keep_end], base_end$x)
          curve_y <- c(curve_y[keep_end], base_end$y)
        }
      }

      draw_curve_with_start_segment(curve_x, curve_y, col, lwd, lty,
                                    start_lty, start_fraction)
    }

    # Draw arrow at source
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
#' @param shadow Logical: enable drop shadow?
#' @param shadow_color Shadow color.
#' @param shadow_offset Shadow offset distance.
#' @param shadow_alpha Shadow transparency.
#' @keywords internal
draw_edge_label_base <- function(x, y, label, cex = 0.8, col = "gray30",
                                 bg = "white", font = 1,
                                 shadow = FALSE, shadow_color = "gray40",
                                 shadow_offset = 0.5, shadow_alpha = 0.5) {
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

  # Draw shadow text first (if enabled)
  if (shadow) {
    # Convert points to user coordinate offset
    shadow_off <- shadow_offset * 0.01  # Scale for user coordinates
    shadow_col <- adjust_alpha(shadow_color, shadow_alpha)

    graphics::text(
      x = x + shadow_off, y = y - shadow_off,
      labels = label,
      cex = cex,
      col = shadow_col,
      font = font
    )
  }

  # Draw main text
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
#' For curved edges, the label is offset perpendicular to the edge to avoid
#' overlapping with the edge line.
#'
#' @param x1,y1 Start point.
#' @param x2,y2 End point.
#' @param position Position along edge (0-1).
#' @param curve Curvature amount.
#' @param curvePivot Curve pivot position.
#' @param label_offset Additional perpendicular offset for the label (in user coords).
#'   Positive values offset in the same direction as the curve bulge.
#'   Default 0.03 provides good separation from the edge line.
#' @return List with x, y coordinates.
#' @keywords internal
get_edge_label_position <- function(x1, y1, x2, y2, position = 0.5,
                                    curve = 0, curvePivot = 0.5,
                                    label_offset = 0) {
  # Edge vector and length
  dx <- x2 - x1
  dy <- y2 - y1
  len <- sqrt(dx^2 + dy^2)

  # Defensive check for empty or NA values
  if (length(len) == 0 || is.na(len) || len < 1e-10) {
    return(list(x = x1, y = y1))
  }

  # Perpendicular unit vector (counterclockwise rotation)
  px <- -dy / len
  py <- dx / len

  if (length(curve) == 0 || is.na(curve) || abs(curve) < 1e-6) {
    # Straight edge - position along line with perpendicular offset
    base_x <- x1 + position * dx
    base_y <- y1 + position * dy

    # Offset perpendicular to edge (default: above the line)
    return(list(
      x = base_x + label_offset * px,
      y = base_y + label_offset * py
    ))
  }

  # Curved edge - match qgraph-style curve calculation
  # Same curve offset as draw_curved_edge_base
  curve_offset <- curve * len * 0.25
  min_offset <- abs(curve) * 0.15
  if (abs(curve_offset) > 0 && abs(curve_offset) < min_offset) {
    curve_offset <- sign(curve_offset) * min_offset
  }

  # Base point along edge
  t <- position
  bx <- x1 + t * dx
  by <- y1 + t * dy

  # Parabolic offset for curve position
  offset_factor <- 4 * t * (1 - t)

  if (curvePivot != 0.5) {
    if (t <= curvePivot) {
      offset_factor <- (t / curvePivot)^2 * 4 * curvePivot * (1 - curvePivot)
    } else {
      offset_factor <- ((1 - t) / (1 - curvePivot))^2 * 4 * curvePivot * (1 - curvePivot)
    }
  }

  # Position on the curve
  curve_x <- bx + curve_offset * offset_factor * px
  curve_y <- by + curve_offset * offset_factor * py

  # Add additional offset in the direction of the curve bulge
  # This moves the label to the convex side of the curve
  curve_direction <- sign(curve)
  if (curve_direction == 0) curve_direction <- 1

  list(
    x = curve_x + label_offset * curve_direction * px,
    y = curve_y + label_offset * curve_direction * py
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

  # Calculate network center for inward curve direction
  center_x <- mean(layout[, 1])
  center_y <- mean(layout[, 2])

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

    # Use curve value as-is (direction already calculated by caller)
    curve_i <- curve[i]

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

    # Store label position
    label_positions[[i]] <- get_edge_label_position(
      start$x, start$y, end$x, end$y,
      position = edge.label.position,
      curve = curve_i,
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
