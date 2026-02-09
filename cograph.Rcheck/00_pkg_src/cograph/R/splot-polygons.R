#' @title Base R Polygon Shape Definitions
#' @description Vertex generation functions for polygon-based node shapes.
#' @name splot-polygons
#' @keywords internal
NULL

#' Generate Circle Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius.
#' @param n Number of vertices.
#' @return List with x, y vectors of vertices.
#' @keywords internal
circle_vertices <- function(x, y, r, n = 50) {
  angles <- seq(0, 2 * pi, length.out = n + 1)[-1]
  list(
    x = x + r * cos(angles),
    y = y + r * sin(angles)
  )
}

#' Generate Square Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Half-width (vertex distance from center).
#' @return List with x, y vectors of vertices.
#' @keywords internal
square_vertices <- function(x, y, r) {
  list(
    x = x + r * c(-1, 1, 1, -1),
    y = y + r * c(-1, -1, 1, 1)
  )
}

#' Generate Rectangle Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param w Half-width.
#' @param h Half-height.
#' @return List with x, y vectors of vertices.
#' @keywords internal
rectangle_vertices <- function(x, y, w, h) {
  list(
    x = x + w * c(-1, 1, 1, -1),
    y = y + h * c(-1, -1, 1, 1)
  )
}

#' Generate Triangle Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius (vertex distance from center).
#' @return List with x, y vectors of vertices.
#' @keywords internal
triangle_vertices <- function(x, y, r) {
  angles <- c(pi/2, pi/2 + 2*pi/3, pi/2 + 4*pi/3)
  list(
    x = x + r * cos(angles),
    y = y + r * sin(angles)
  )
}

#' Generate Diamond Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius (vertex distance from center).
#' @return List with x, y vectors of vertices.
#' @keywords internal
diamond_vertices <- function(x, y, r) {
  angles <- c(0, pi/2, pi, 3*pi/2)
  list(
    x = x + r * cos(angles),
    y = y + r * sin(angles)
  )
}

#' Generate Pentagon Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius.
#' @return List with x, y vectors of vertices.
#' @keywords internal
pentagon_vertices <- function(x, y, r) {
  angles <- seq(pi/2, pi/2 + 2*pi * (4/5), length.out = 5)
  list(
    x = x + r * cos(angles),
    y = y + r * sin(angles)
  )
}

#' Generate Hexagon Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius.
#' @return List with x, y vectors of vertices.
#' @keywords internal
hexagon_vertices <- function(x, y, r) {
  angles <- seq(0, 2*pi * (5/6), length.out = 6)
  list(
    x = x + r * cos(angles),
    y = y + r * sin(angles)
  )
}

#' Generate Star Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Outer radius.
#' @param n_points Number of star points.
#' @param inner_ratio Ratio of inner to outer radius.
#' @return List with x, y vectors of vertices.
#' @keywords internal
star_vertices <- function(x, y, r, n_points = 5, inner_ratio = 0.4) {
  n_vertices <- n_points * 2
  angles <- seq(pi/2, pi/2 + 2*pi * (1 - 1/n_vertices), length.out = n_vertices)
  radii <- rep(c(r, r * inner_ratio), n_points)

  list(
    x = x + radii * cos(angles),
    y = y + radii * sin(angles)
  )
}

#' Generate Heart Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Scale (size).
#' @param n Number of vertices.
#' @return List with x, y vectors of vertices.
#' @keywords internal
heart_vertices <- function(x, y, r, n = 100) {
  t <- seq(0, 2*pi, length.out = n)

  # Heart parametric equations
  hx <- 16 * sin(t)^3
  hy <- 13 * cos(t) - 5 * cos(2*t) - 2 * cos(3*t) - cos(4*t)

  # Normalize and scale
  max_extent <- max(abs(c(hx, hy)))
  hx <- hx / max_extent * r * 0.8
  hy <- hy / max_extent * r * 0.8

  list(
    x = x + hx,
    y = y + hy
  )
}

#' Generate Ellipse Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param rx Horizontal radius.
#' @param ry Vertical radius.
#' @param n Number of vertices.
#' @return List with x, y vectors of vertices.
#' @keywords internal
ellipse_vertices <- function(x, y, rx, ry, n = 50) {
  angles <- seq(0, 2 * pi, length.out = n + 1)[-1]
  list(
    x = x + rx * cos(angles),
    y = y + ry * sin(angles)
  )
}

#' Generate Cross/Plus Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Half-size.
#' @param thickness Arm thickness as ratio of r.
#' @return List with x, y vectors of vertices.
#' @keywords internal
cross_vertices <- function(x, y, r, thickness = 0.3) {
  t <- r * thickness

  # 12-point cross shape
  list(
    x = x + c(-t, t, t, r, r, t, t, -t, -t, -r, -r, -t),
    y = y + c(r, r, t, t, -t, -t, -r, -r, -t, -t, t, t)
  )
}

#' Generate Regular Polygon Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius.
#' @param n Number of sides.
#' @param rotation Starting angle in radians (default: first vertex at top).
#' @return List with x, y vectors of vertices.
#' @keywords internal
regular_polygon_vertices <- function(x, y, r, n, rotation = pi/2) {
  angles <- seq(rotation, rotation + 2*pi * (1 - 1/n), length.out = n)
  list(
    x = x + r * cos(angles),
    y = y + r * sin(angles)
  )
}

#' Inset Polygon Vertices
#'
#' Creates an inner polygon by scaling vertices toward the centroid.
#'
#' @param outer List with x, y vectors of outer polygon vertices.
#' @param inner_ratio Ratio to scale vertices toward center (0-1).
#' @return List with x, y vectors of inner polygon vertices.
#' @keywords internal
inset_polygon_vertices <- function(outer, inner_ratio) {
  # Calculate centroid
  cx <- mean(outer$x)
  cy <- mean(outer$y)

  # Scale vertices toward centroid
  list(
    x = cx + (outer$x - cx) * inner_ratio,
    y = cy + (outer$y - cy) * inner_ratio
  )
}

#' Get Polygon Vertices by Shape Name
#'
#' Returns outer polygon vertices for donut ring shapes.
#'
#' @param shape Shape name.
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius/size.
#' @return List with x, y vectors of vertices.
#' @keywords internal
get_donut_base_vertices <- function(shape, x, y, r) {
  switch(shape,
    circle = circle_vertices(x, y, r, n = 100),
    square = square_vertices(x, y, r),
    rectangle = rectangle_vertices(x, y, r, r * 0.7),
    triangle = triangle_vertices(x, y, r),
    diamond = diamond_vertices(x, y, r),
    pentagon = pentagon_vertices(x, y, r),
    hexagon = hexagon_vertices(x, y, r),
    # Default to circle
    circle_vertices(x, y, r, n = 100)
  )
}

#' Generate Gear Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Outer radius.
#' @param n_teeth Number of teeth.
#' @return List with x, y vectors of vertices.
#' @keywords internal
gear_vertices <- function(x, y, r, n_teeth = 8) {
  inner_r <- r * 0.65
  tooth_height <- r * 0.25

  n_pts_per_tooth <- 8
  n_total <- n_teeth * n_pts_per_tooth
  angles <- seq(0, 2 * pi, length.out = n_total + 1)[-1]

  gear_x <- numeric(n_total)
  gear_y <- numeric(n_total)

  for (i in seq_len(n_total)) {
    pos_in_tooth <- (i - 1) %% n_pts_per_tooth

    if (pos_in_tooth < 2 || pos_in_tooth >= 6) {
      rad <- inner_r
    } else {
      rad <- inner_r + tooth_height
    }

    gear_x[i] <- x + rad * cos(angles[i])
    gear_y[i] <- y + rad * sin(angles[i])
  }

  list(x = gear_x, y = gear_y)
}

#' Generate Cloud Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius.
#' @param n Number of vertices.
#' @return List with x, y vectors of vertices.
#' @keywords internal
cloud_vertices <- function(x, y, r, n = 100) {
  t <- seq(0, 2 * pi, length.out = n)
  rad <- r * (0.65 + 0.2 * sin(4 * t) + 0.1 * sin(6 * t))

  list(
    x = x + rad * cos(t),
    y = y + rad * sin(t) * 0.6 + r * 0.1
  )
}

#' Generate Brain Vertices
#'
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius.
#' @param n Number of vertices.
#' @return List with x, y vectors of vertices.
#' @keywords internal
brain_vertices <- function(x, y, r, n = 80) {
  t <- seq(0, 2 * pi, length.out = n)
  rad <- r * (0.7 + 0.15 * sin(3 * t) + 0.1 * sin(5 * t) + 0.05 * cos(7 * t))

  list(
    x = x + rad * cos(t),
    y = y + rad * sin(t) * 0.85
  )
}

#' Get Shape Vertices
#'
#' Dispatch function to get vertices for any supported shape.
#'
#' @param shape Shape name.
#' @param x Center x coordinate.
#' @param y Center y coordinate.
#' @param r Radius/size.
#' @param r2 Secondary radius (for ellipse, rectangle).
#' @param ... Additional shape-specific parameters.
#' @return List with x, y vectors of vertices.
#' @keywords internal
get_shape_vertices <- function(shape, x, y, r, r2 = NULL, ...) {
  if (is.null(r2)) r2 <- r

  switch(shape,
    circle = circle_vertices(x, y, r),
    square = square_vertices(x, y, r),
    rectangle = rectangle_vertices(x, y, r, r2),
    triangle = triangle_vertices(x, y, r),
    diamond = diamond_vertices(x, y, r),
    pentagon = pentagon_vertices(x, y, r),
    hexagon = hexagon_vertices(x, y, r),
    star = star_vertices(x, y, r, ...),
    heart = heart_vertices(x, y, r),
    ellipse = ellipse_vertices(x, y, r, r2),
    cross = cross_vertices(x, y, r, ...),
    gear = gear_vertices(x, y, r, ...),
    cloud = cloud_vertices(x, y, r),
    brain = brain_vertices(x, y, r),
    # Default to circle
    circle_vertices(x, y, r)
  )
}
