#' @title Circular Layout
#' @description Arrange nodes in a circle.
#' @name layout-circle
NULL

#' Circular Layout
#'
#' Arrange nodes evenly spaced around a circle.
#'
#' @param network A CographNetwork object.
#' @param order Optional vector specifying node order (indices or labels).
#' @param start_angle Starting angle in radians (default: pi/2 for top).
#' @param clockwise Logical. Arrange nodes clockwise? Default TRUE.
#' @return Data frame with x, y coordinates.
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- CographNetwork$new(adj)
#' coords <- layout_circle(net)
layout_circle <- function(network, order = NULL, start_angle = pi/2,
                          clockwise = TRUE) {
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

  # Calculate angles
  angles <- seq(start_angle, start_angle + 2 * pi * (1 - 1/n),
                length.out = n)
  if (clockwise) {
    angles <- rev(angles)
  }

  # Calculate coordinates
  x <- 0.5 + 0.4 * cos(angles)
  y <- 0.5 + 0.4 * sin(angles)

  # Reorder if needed
  coords <- data.frame(x = x, y = y)
  coords[order, ] <- coords

  coords
}
