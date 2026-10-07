#' @title Circular Layout
#' @description Arrange nodes in a circle.
#' @name layout-circle
#' @keywords internal
#' @noRd
NULL

#' Circular Layout
#'
#' Places the nodes at evenly spaced positions on a circle of radius 0.4
#' centered at (0.5, 0.5). A single node is placed at the center.
#'
#' @param network A \code{CographNetwork} or \code{cograph_network} object.
#' @param order Optional vector of node indices or labels. Its i-th element is
#'   the node placed at the i-th position. Labels are matched only for a
#'   \code{CographNetwork} object. An order of the wrong length, or with
#'   unmatched labels, raises a warning and the default order is used.
#' @param start_angle Angle in radians of the reference position. Default
#'   \code{pi/2} (top of the circle).
#' @param clockwise Logical. Default \code{TRUE} places the positions
#'   clockwise, with the last position at \code{start_angle} and the first
#'   position one step clockwise of it. \code{FALSE} places the first position
#'   at \code{start_angle} and continues counterclockwise.
#' @param ... Ignored.
#' @return A data frame with columns \code{x} and \code{y} and one row per
#'   node, in node order.
#'
#' @examples
#' layout_circle(CographNetwork$new(regulation_net))
#'
#' @export
layout_circle <- function(network, order = NULL, start_angle = pi/2,
                          clockwise = TRUE, ...) {
  # Get node count (support both R6 and S3 cograph_network)
  n <- if (inherits(network, "cograph_network")) {
    n_nodes(network)
  } else {
    network$n_nodes
  }

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
