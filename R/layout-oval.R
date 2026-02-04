#' @title Oval/Ellipse Layout
#' @description Arrange nodes in an oval (ellipse) shape.
#' @name layout-oval
NULL

#' Oval Layout
#'
#' Arrange nodes evenly spaced around an ellipse. This creates an oval-shaped
#' network layout that is wider than it is tall (or vice versa depending on ratio).
#'
#' @param network A CographNetwork object.
#' @param ratio Aspect ratio (width/height). Values > 1 create horizontal ovals,
#'   values < 1 create vertical ovals. Default 1.5.
#' @param order Optional vector specifying node order (indices or labels).
#' @param start_angle Starting angle in radians (default: pi/2 for top).
#' @param clockwise Logical. Arrange nodes clockwise? Default TRUE.
#' @param rotation Rotation angle in radians to tilt the entire oval. Default 0.
#' @return Data frame with x, y coordinates.
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- CographNetwork$new(adj)
#' coords <- layout_oval(net, ratio = 1.5)
layout_oval <- function(network, ratio = 1.5, order = NULL, start_angle = pi/2,
                        clockwise = TRUE, rotation = 0) {
  n <- network$n_nodes

  if (n == 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  if (n == 1) {
    return(data.frame(x = 0.5, y = 0.5))
  }

  # Determine node order
  if (!is.null(order)) {
    if (is.character(order)) {
      # Convert labels to indices
      labels <- network$node_labels
      order <- match(order, labels)
      if (any(is.na(order))) {
        warning("Some labels not found, using default order")
        order <- seq_len(n)
      }
    }
    if (length(order) != n) {
      warning("Order length doesn't match node count, using default order")
      order <- seq_len(n)
    }
  } else {
    order <- seq_len(n)
  }

  # Calculate angles for each node
  angles <- seq(start_angle, start_angle + 2 * pi * (1 - 1/n),
                length.out = n)
  if (clockwise) {
    angles <- rev(angles)
  }

  # Calculate ellipse radii based on ratio
  # Keep the area roughly similar to a unit circle
  # For ellipse: area = pi * a * b, for circle: area = pi * r^2
  # If we want same area: a * b = r^2 = 0.16 (for r = 0.4)
  # With a = ratio * b: ratio * b^2 = 0.16, so b = sqrt(0.16/ratio)
  base_radius <- 0.4
  radius_x <- base_radius * sqrt(ratio)
  radius_y <- base_radius / sqrt(ratio)

  # Calculate coordinates on ellipse
  x <- 0.5 + radius_x * cos(angles)
  y <- 0.5 + radius_y * sin(angles)

  # Apply rotation if specified

  if (rotation != 0) {
    # Rotate around center (0.5, 0.5)
    x_centered <- x - 0.5
    y_centered <- y - 0.5

    x_rotated <- x_centered * cos(rotation) - y_centered * sin(rotation)
    y_rotated <- x_centered * sin(rotation) + y_centered * cos(rotation)

    x <- x_rotated + 0.5
    y <- y_rotated + 0.5
  }

  # Reorder if needed
  coords <- data.frame(x = x, y = y)
  coords[order, ] <- coords

  coords
}
