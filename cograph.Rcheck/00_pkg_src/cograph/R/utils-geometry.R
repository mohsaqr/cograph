#' @title Geometry Utilities
#' @description Utility functions for geometric calculations.
#' @name utils-geometry
#' @keywords internal
NULL

#' Calculate Distance Between Two Points
#'
#' @param x1,y1 First point coordinates.
#' @param x2,y2 Second point coordinates.
#' @return Euclidean distance.
#' @keywords internal
point_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Calculate Angle Between Two Points
#'
#' @param x1,y1 Start point coordinates.
#' @param x2,y2 End point coordinates.
#' @return Angle in radians.
#' @keywords internal
point_angle <- function(x1, y1, x2, y2) {
  atan2(y2 - y1, x2 - x1)
}

#' Calculate Point on Circle
#'
#' @param cx,cy Center coordinates.
#' @param r Radius.
#' @param angle Angle in radians.
#' @return List with x, y coordinates.
#' @keywords internal
point_on_circle <- function(cx, cy, r, angle) {
  list(
    x = cx + r * cos(angle),
    y = cy + r * sin(angle)
  )
}

#' Calculate Bezier Curve Points
#'
#' Calculate points along a quadratic Bezier curve.
#'
#' @param x0,y0 Start point.
#' @param x1,y1 Control point.
#' @param x2,y2 End point.
#' @param n Number of points to generate.
#' @return Data frame with x, y coordinates.
#' @keywords internal
bezier_points <- function(x0, y0, x1, y1, x2, y2, n = 50) {
  t <- seq(0, 1, length.out = n)

  # Quadratic Bezier formula
  x <- (1 - t)^2 * x0 + 2 * (1 - t) * t * x1 + t^2 * x2
  y <- (1 - t)^2 * y0 + 2 * (1 - t) * t * y1 + t^2 * y2

  data.frame(x = x, y = y)
}

#' Calculate Control Point for Curved Edge
#'
#' @param x1,y1 Start point.
#' @param x2,y2 End point.
#' @param curvature Curvature amount (0 = straight line).
#' @param pivot Position along edge (0-1) where control point sits. 0 = near source,
#'   0.5 = middle (default), 1 = near target.
#' @param shape Spline tension affecting curvature intensity (-1 to 1).
#'   Negative = sharper curve, Positive = gentler curve. Default 0.
#' @return List with x, y coordinates of control point.
#' @keywords internal
curve_control_point <- function(x1, y1, x2, y2, curvature, pivot = 0.5, shape = 0) {
  # Point along the edge based on pivot (0 = source, 0.5 = midpoint, 1 = target)
  pivot <- max(0, min(1, pivot))  # Clamp to [0, 1]
  mx <- x1 + pivot * (x2 - x1)
  my <- y1 + pivot * (y2 - y1)

  # Perpendicular offset
  dx <- x2 - x1
  dy <- y2 - y1
  len <- sqrt(dx^2 + dy^2)

  if (len == 0) {
    return(list(x = mx, y = my))
  }

  # Perpendicular unit vector
  px <- -dy / len
  py <- dx / len

  # Adjust curvature based on shape parameter
  # shape = 0: no adjustment
  # shape < 0: sharper curve (increase curvature magnitude)
  # shape > 0: gentler curve (decrease curvature magnitude)
  shape <- max(-1, min(1, shape))  # Clamp to [-1, 1]
  adjusted_curvature <- curvature * (1 - shape * 0.5)

  # Control point
  list(
    x = mx + adjusted_curvature * len * px,
    y = my + adjusted_curvature * len * py
  )
}

#' Calculate Arrow Head Points
#'
#' @param x,y Arrow tip position.
#' @param angle Angle of incoming edge (radians).
#' @param size Arrow size.
#' @param width Arrow width ratio (default 0.5).
#' @param x_scale,y_scale Aspect ratio correction factors.
#' @return List with arrow polygon coordinates and midpoint for line connection.
#' @keywords internal
arrow_points <- function(x, y, angle, size, width = 0.5, x_scale = 1, y_scale = 1) {

  # Arrow points relative to tip
  left_angle <- angle + pi - atan(width)
  right_angle <- angle + pi + atan(width)
  back_len <- size / cos(atan(width))

  left_x <- x + back_len * cos(left_angle) * x_scale
  left_y <- y + back_len * sin(left_angle) * y_scale
  right_x <- x + back_len * cos(right_angle) * x_scale
  right_y <- y + back_len * sin(right_angle) * y_scale

  # Midpoint of the arrow base (where line should connect)
  mid_x <- (left_x + right_x) / 2
  mid_y <- (left_y + right_y) / 2

  list(
    x = c(x, left_x, right_x),
    y = c(y, left_y, right_y),
    mid_x = mid_x,
    mid_y = mid_y,
    back_len = back_len
  )
}

#' Offset Point from Center
#'
#' Calculate a point offset from another point by a given distance.
#'
#' @param x,y Original point.
#' @param toward_x,toward_y Point to offset toward.
#' @param offset Distance to offset.
#' @return List with x, y coordinates.
#' @keywords internal
offset_point <- function(x, y, toward_x, toward_y, offset) {
  angle <- point_angle(x, y, toward_x, toward_y)
  list(
    x = x + offset * cos(angle),
    y = y + offset * sin(angle)
  )
}

#' Calculate Edge Endpoint on Node Border
#'
#' Calculates the point where an edge should meet the node border.
#' Uses plain NPC units to match circleGrob borders.
#'
#' @param node_x,node_y Node center in npc.
#' @param other_x,other_y Other endpoint in npc.
#' @param node_size Node radius in npc units.
#' @param shape Node shape.
#' @param x_scale,y_scale Aspect ratio correction factors.
#' @return List with x, y coordinates in npc.
#' @keywords internal
edge_endpoint <- function(node_x, node_y, other_x, other_y, node_size,
                          shape = "circle", x_scale = 1, y_scale = 1) {
  # Calculate angle from node center to other point, accounting for aspect ratio
  dx <- (other_x - node_x) / x_scale
  dy <- (other_y - node_y) / y_scale
  angle <- atan2(dy, dx)

  # Point on node border with aspect correction
  list(
    x = node_x + node_size * cos(angle) * x_scale,
    y = node_y + node_size * sin(angle) * y_scale
  )
}
