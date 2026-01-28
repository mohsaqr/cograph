#' @title Base R Graphics Geometry Utilities
#' @description Coordinate transformation and geometry functions for splot().
#' @name splot-geometry
#' @keywords internal
NULL

#' Convert User Coordinates to Inches (X-axis)
#'
#' @param x Value in user coordinates.
#' @return Value in inches.
#' @keywords internal
usr_to_in_x <- function(x) {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  (x - usr[1]) / (usr[2] - usr[1]) * pin[1]
}

#' Convert User Coordinates to Inches (Y-axis)
#'
#' @param y Value in user coordinates.
#' @return Value in inches.
#' @keywords internal
usr_to_in_y <- function(y) {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  (y - usr[3]) / (usr[4] - usr[3]) * pin[2]
}

#' Convert Inches to User Coordinates (X-axis)
#'
#' @param x Value in inches.
#' @return Value in user coordinates.
#' @keywords internal
in_to_usr_x <- function(x) {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  x / pin[1] * (usr[2] - usr[1]) + usr[1]
}

#' Convert Inches to User Coordinates (Y-axis)
#'
#' @param y Value in inches.
#' @return Value in user coordinates.
#' @keywords internal
in_to_usr_y <- function(y) {
 usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  y / pin[2] * (usr[4] - usr[3]) + usr[3]
}

#' Get X-axis Scale Factor (inches per user unit)
#'
#' @return Scale factor.
#' @keywords internal
get_x_scale <- function() {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  pin[1] / (usr[2] - usr[1])
}

#' Get Y-axis Scale Factor (inches per user unit)
#'
#' @return Scale factor.
#' @keywords internal
get_y_scale <- function() {
  usr <- graphics::par("usr")
  pin <- graphics::par("pin")
  pin[2] / (usr[4] - usr[3])
}

#' Aspect-Corrected atan2
#'
#' Calculate angle accounting for aspect ratio differences.
#'
#' @param dy Change in y (user coordinates).
#' @param dx Change in x (user coordinates).
#' @return Angle in radians.
#' @keywords internal
atan2_usr <- function(dy, dx) {
  # Convert to inches to get visually correct angle
  dy_in <- dy * get_y_scale()
  dx_in <- dx * get_x_scale()
  atan2(dy_in, dx_in)
}

#' Calculate Point on Node Boundary
#'
#' Given a node center, size, and angle, calculates the point on the node
#' boundary. Works with various shapes.
#'
#' @param x Node center x coordinate.
#' @param y Node center y coordinate.
#' @param angle Angle in radians.
#' @param cex Node size (radius in user coordinates).
#' @param cex2 Secondary size for ellipse width (NULL for square aspect).
#' @param shape Node shape: "circle", "square", "ellipse", or polygon name.
#' @return List with x, y coordinates on boundary.
#' @keywords internal
cent_to_edge <- function(x, y, angle, cex, cex2 = NULL, shape = "circle") {

  # Get aspect correction
  x_scale <- get_x_scale()
  y_scale <- get_y_scale()
  asp <- y_scale / x_scale

  if (is.null(cex2)) cex2 <- cex

  if (shape == "circle") {
    # Circle: simple radial point
    list(
      x = x + cex * cos(angle),
      y = y + cex * sin(angle)
    )

  } else if (shape == "square" || shape == "rectangle") {
    # Square/rectangle: find intersection with edges
    # Normalize angle to [0, 2*pi)
    a <- angle %% (2 * pi)

    # Half-widths
    hw <- cex  # half-width
    hh <- cex2 # half-height

    # Determine which edge we hit
    # Using tangent to find intersection
    tan_a <- tan(a)

    if (abs(cos(a)) < 1e-10) {
      # Vertical (top or bottom)
      if (sin(a) > 0) {
        list(x = x, y = y + hh)
      } else {
        list(x = x, y = y - hh)
      }
    } else if (abs(sin(a)) < 1e-10) {
      # Horizontal (left or right)
      if (cos(a) > 0) {
        list(x = x + hw, y = y)
      } else {
        list(x = x - hw, y = y)
      }
    } else {
      # General case
      # Check right/left edge
      edge_x <- if (cos(a) > 0) hw else -hw
      edge_y <- edge_x * tan_a

      if (abs(edge_y) <= hh) {
        list(x = x + edge_x, y = y + edge_y)
      } else {
        # Top/bottom edge
        edge_y <- if (sin(a) > 0) hh else -hh
        edge_x <- edge_y / tan_a
        list(x = x + edge_x, y = y + edge_y)
      }
    }

  } else if (shape == "ellipse") {
    # Ellipse: parametric boundary point
    # For ellipse with semi-axes a (horizontal) and b (vertical)
    a <- cex   # horizontal radius
    b <- cex2  # vertical radius

    # Point on ellipse at angle (not quite the same as the parametric angle)
    # Use Newton's method or direct formula
    # Simple approximation using parametric form
    list(
      x = x + a * cos(angle),
      y = y + b * sin(angle)
    )

  } else {
    # Default to circle for unknown shapes
    list(
      x = x + cex * cos(angle),
      y = y + cex * sin(angle)
    )
  }
}

#' Calculate Perpendicular Midpoint for Curved Edges
#'
#' Computes a control point perpendicular to the line between two nodes,
#' used for xspline() curve generation.
#'
#' @param x0 Start x coordinate.
#' @param y0 Start y coordinate.
#' @param x1 End x coordinate.
#' @param y1 End y coordinate.
#' @param cex Curvature amount (positive = left, negative = right).
#' @param q Position along edge (0 = start, 0.5 = middle, 1 = end).
#' @return List with x, y coordinates of control point.
#' @keywords internal
perp_mid <- function(x0, y0, x1, y1, cex, q = 0.5) {
  # Point along the edge
  mx <- x0 + q * (x1 - x0)
  my <- y0 + q * (y1 - y0)

  # Edge vector
  dx <- x1 - x0
  dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)

  if (len < 1e-10) {
    return(list(x = mx, y = my))
  }

  # Perpendicular unit vector (rotated 90 degrees counterclockwise)
  px <- -dy / len
  py <- dx / len

  # Get aspect correction to make curve look circular
  x_scale <- get_x_scale()
  y_scale <- get_y_scale()

  # Offset distance (scaled by edge length for consistent appearance)
  offset <- cex * len

  list(
    x = mx + offset * px,
    y = my + offset * py
  )
}

#' Calculate Distance Between Two Points
#'
#' @param x1,y1 First point.
#' @param x2,y2 Second point.
#' @return Euclidean distance.
#' @keywords internal
splot_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Calculate Angle Between Two Points
#'
#' @param x1,y1 Start point.
#' @param x2,y2 End point.
#' @return Angle in radians.
#' @keywords internal
splot_angle <- function(x1, y1, x2, y2) {
  atan2(y2 - y1, x2 - x1)
}

#' Rescale Layout to -1 to 1 Range
#'
#' @param layout Matrix or data frame with x, y columns.
#' @param mar Margin to leave (as proportion of range).
#' @return Rescaled layout.
#' @keywords internal
rescale_layout <- function(layout, mar = 0.1) {
  layout <- as.data.frame(layout)

  if (ncol(layout) < 2) {
    stop("Layout must have at least 2 columns", call. = FALSE)
  }

  x <- layout[[1]]
  y <- layout[[2]]

  # Get ranges
  x_range <- range(x, na.rm = TRUE)
  y_range <- range(y, na.rm = TRUE)

  # Handle constant values
  if (diff(x_range) < 1e-10) {
    x_range <- x_range + c(-1, 1)
  }
  if (diff(y_range) < 1e-10) {
    y_range <- y_range + c(-1, 1)
  }

  # Target range with margins
  target <- 1 - mar

  # Rescale using uniform scaling to preserve aspect ratio
  max_range <- max(diff(x_range), diff(y_range))
  x_center <- mean(x_range)
  y_center <- mean(y_range)
  layout[[1]] <- (x - x_center) / max_range * 2 * target
  layout[[2]] <- (y - y_center) / max_range * 2 * target

  layout
}
