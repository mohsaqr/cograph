#' @title Base R Arrow Drawing
#' @description Arrow head drawing functions for splot() edges.
#' @name splot-arrows
#' @keywords internal
NULL

#' Calculate Arrow Base Midpoint
#'
#' Returns the midpoint between the arrow wings (where the curve should end).
#' This is used to connect the edge line to the back of the arrow head.
#'
#' @param x Arrow tip x coordinate.
#' @param y Arrow tip y coordinate.
#' @param angle Angle of incoming edge (radians).
#' @param size Arrow size in user coordinates.
#' @param arrow_angle Arrow head angle in radians. Default pi/6 (30 degrees).
#' @return List with x, y coordinates of the arrow base midpoint.
#' @keywords internal
arrow_base_midpoint <- function(x, y, angle, size, arrow_angle = pi/6) {
  left_angle <- angle + pi - arrow_angle
  right_angle <- angle + pi + arrow_angle
  back_len <- size / cos(arrow_angle)

  left_x <- x + back_len * cos(left_angle)
  left_y <- y + back_len * sin(left_angle)
  right_x <- x + back_len * cos(right_angle)
  right_y <- y + back_len * sin(right_angle)

  list(x = (left_x + right_x) / 2, y = (left_y + right_y) / 2)
}

#' Calculate Arrow Radius
#'
#' Returns the distance from arrow tip to base midpoint.
#' This is how far back from the tip the arrow extends.
#'
#' @param size Arrow size in user coordinates.
#' @param arrow_angle Arrow head angle in radians. Default pi/6 (30 degrees).
#' @return The arrow radius (distance from tip to base).
#' @keywords internal
arrow_radius <- function(size, arrow_angle = pi/6) {
  size  # The arrow extends 'size' units back from tip
}

#' Draw Arrow Head
#'
#' Draws a filled triangular arrow head at the specified position.
#'
#' @param x Arrow tip x coordinate.
#' @param y Arrow tip y coordinate.
#' @param angle Angle of incoming edge (radians).
#' @param size Arrow size in user coordinates.
#' @param arrow_angle Arrow head angle in radians. Default pi/6 (30 degrees).
#' @param col Arrow fill color.
#' @param border Arrow border color (default same as fill).
#' @param lwd Border line width.
#' @keywords internal
draw_arrow_base <- function(x, y, angle, size, arrow_angle = pi/6, col = "black",
                            border = NULL, lwd = 1) {
  if (is.null(border)) border <- col

  # Arrow points relative to tip
  left_angle <- angle + pi - arrow_angle
  right_angle <- angle + pi + arrow_angle
  back_len <- size / cos(arrow_angle)

  left_x <- x + back_len * cos(left_angle)
  left_y <- y + back_len * sin(left_angle)
  right_x <- x + back_len * cos(right_angle)
  right_y <- y + back_len * sin(right_angle)

  # Draw filled polygon
  graphics::polygon(
    x = c(x, left_x, right_x),
    y = c(y, left_y, right_y),
    col = col,
    border = border,
    lwd = lwd
  )
}

#' Calculate Arrow Head Points
#'
#' Returns the vertices for an arrow head polygon without drawing.
#'
#' @param x Arrow tip x coordinate.
#' @param y Arrow tip y coordinate.
#' @param angle Angle of incoming edge (radians).
#' @param size Arrow size.
#' @param arrow_angle Arrow head angle in radians. Default pi/6 (30 degrees).
#' @return List with x, y vectors and midpoint coordinates.
#' @keywords internal
arrow_head_points <- function(x, y, angle, size, arrow_angle = pi/6) {
  # Arrow points relative to tip
  left_angle <- angle + pi - arrow_angle
  right_angle <- angle + pi + arrow_angle
  back_len <- size / cos(arrow_angle)

  left_x <- x + back_len * cos(left_angle)
  left_y <- y + back_len * sin(left_angle)
  right_x <- x + back_len * cos(right_angle)
  right_y <- y + back_len * sin(right_angle)

  # Midpoint of arrow base (where line should connect)
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

#' Draw Curved Arrow Head
#'
#' Draws an arrow head at the end of a curved edge, with angle following
#' the curve direction.
#'
#' @param spline_x X coordinates of the spline.
#' @param spline_y Y coordinates of the spline.
#' @param size Arrow size.
#' @param arrow_angle Arrow head angle in radians. Default pi/6 (30 degrees).
#' @param col Arrow fill color.
#' @param border Arrow border color.
#' @keywords internal
draw_curved_arrow_base <- function(spline_x, spline_y, size, arrow_angle = pi/6,
                                   col = "black", border = NULL) {
  n <- length(spline_x)
  if (n < 2) return(invisible())

  # Get angle from last two points of spline
  angle <- splot_angle(
    spline_x[n - 1], spline_y[n - 1],
    spline_x[n], spline_y[n]
  )

  # Draw arrow at endpoint
  draw_arrow_base(
    x = spline_x[n],
    y = spline_y[n],
    angle = angle,
    size = size,
    arrow_angle = arrow_angle,
    col = col,
    border = border
  )
}

#' Draw Open Arrow Head
#'
#' Draws an open (unfilled) V-shaped arrow head.
#'
#' @param x Arrow tip x coordinate.
#' @param y Arrow tip y coordinate.
#' @param angle Angle of incoming edge (radians).
#' @param size Arrow size.
#' @param arrow_angle Arrow head angle in radians. Default pi/6 (30 degrees).
#' @param col Arrow color.
#' @param lwd Line width.
#' @keywords internal
draw_open_arrow_base <- function(x, y, angle, size, arrow_angle = pi/6,
                                 col = "black", lwd = 1) {
  # Arrow points
  left_angle <- angle + pi - arrow_angle
  right_angle <- angle + pi + arrow_angle
  back_len <- size / cos(arrow_angle)

  left_x <- x + back_len * cos(left_angle)
  left_y <- y + back_len * sin(left_angle)
  right_x <- x + back_len * cos(right_angle)
  right_y <- y + back_len * sin(right_angle)

  # Draw lines only (no fill)
  graphics::lines(
    x = c(left_x, x, right_x),
    y = c(left_y, y, right_y),
    col = col,
    lwd = lwd
  )
}

#' Draw Circle Arrow (Dot)
#'
#' Draws a circular dot at the arrow position (alternative to triangular arrow).
#'
#' @param x Position x coordinate.
#' @param y Position y coordinate.
#' @param size Dot radius.
#' @param col Fill color.
#' @param border Border color.
#' @keywords internal
draw_circle_arrow_base <- function(x, y, size, col = "black", border = NULL) {
  if (is.null(border)) border <- col

  # Use symbols() for perfect circles
  graphics::symbols(
    x = x,
    y = y,
    circles = size,
    inches = FALSE,
    add = TRUE,
    fg = border,
    bg = col
  )
}

#' Calculate Shortened Edge Endpoint
#'
#' Calculates where to stop drawing an edge line so the arrow head
#' doesn't overlap with the line.
#'
#' @param x1,y1 Start point.
#' @param x2,y2 End point (arrow tip).
#' @param arrow_size Arrow size.
#' @return List with x, y coordinates of shortened endpoint.
#' @keywords internal
shorten_edge_for_arrow <- function(x1, y1, x2, y2, arrow_size) {
  angle <- splot_angle(x1, y1, x2, y2)

  # Move endpoint back by arrow length
  list(
    x = x2 - arrow_size * cos(angle),
    y = y2 - arrow_size * sin(angle)
  )
}
