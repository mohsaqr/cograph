#' @title Fruchterman-Reingold Spring Layout
#' @description Force-directed layout using the Fruchterman-Reingold algorithm.
#' @name layout-spring
NULL

#' Fruchterman-Reingold Spring Layout
#'
#' Compute node positions using the Fruchterman-Reingold force-directed
#' algorithm. Nodes connected by edges are attracted to each other while
#' all nodes repel each other.
#'
#' @param network A CographNetwork object.
#' @param iterations Number of iterations (default: 500).
#' @param cooling Rate of temperature decrease (default: 0.95).
#' @param repulsion Repulsion constant (default: 1).
#' @param attraction Attraction constant (default: 1).
#' @param seed Random seed for reproducibility.
#' @param initial Optional initial coordinates (matrix or data frame).
#'   For animations, pass the previous frame's layout to ensure smooth transitions.
#' @param max_displacement Maximum distance a node can move from its initial
#'   position (default: NULL = no limit). Useful for animations to prevent
#'   large jumps between frames. Values like 0.05-0.1 work well.
#' @param anchor_strength Strength of force pulling nodes toward initial positions
#'   (default: 0). Higher values (e.g., 0.5-2) keep nodes closer to their starting
#'   positions. Only applies when `initial` is provided.
#' @return Data frame with x, y coordinates.
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow = 4)
#' net <- CographNetwork$new(adj)
#' coords <- layout_spring(net, seed = 42)
#'
#' # For animations: use previous layout as initial with constraints
#' coords2 <- layout_spring(net, initial = coords, max_displacement = 0.05)
layout_spring <- function(network, iterations = 500, cooling = 0.95,
                          repulsion = 1, attraction = 1, seed = NULL,
                          initial = NULL, max_displacement = NULL,
                          anchor_strength = 0) {

  n <- network$n_nodes

  if (n == 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  if (n == 1) {
    return(data.frame(x = 0.5, y = 0.5))
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Initialize positions
  if (!is.null(initial)) {
    if (is.matrix(initial)) {
      pos <- initial
    } else {
      pos <- as.matrix(initial[, c("x", "y")])
    }
    # Store anchor positions for animation constraints
    anchor_pos <- pos
  } else {
    # Random initial positions
    pos <- cbind(
      x = stats::runif(n),
      y = stats::runif(n)
    )
    anchor_pos <- NULL
  }

  # Get edges
  edges <- network$get_edges()
  if (is.null(edges) || nrow(edges) == 0) {
    # No edges: return random positions
    return(data.frame(x = pos[, 1], y = pos[, 2]))
  }

  # Optimal distance
  area <- 1
  k <- sqrt(area / n)

  # Temperature (controls maximum displacement)
  temp <- sqrt(area) * 0.1

  # Fruchterman-Reingold iterations
  for (iter in seq_len(iterations)) {
    # Initialize displacement vectors
    disp <- matrix(0, nrow = n, ncol = 2)

    # Calculate repulsive forces between all pairs
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        delta <- pos[i, ] - pos[j, ]
        dist <- sqrt(sum(delta^2))
        if (dist < 0.001) dist <- 0.001  # Avoid division by zero

        # Repulsive force
        force <- repulsion * k^2 / dist
        disp_vec <- (delta / dist) * force

        disp[i, ] <- disp[i, ] + disp_vec
        disp[j, ] <- disp[j, ] - disp_vec
      }
    }

    # Calculate attractive forces along edges
    for (e in seq_len(nrow(edges))) {
      i <- edges$from[e]
      j <- edges$to[e]

      delta <- pos[i, ] - pos[j, ]
      dist <- sqrt(sum(delta^2))
      if (dist < 0.001) dist <- 0.001

      # Attractive force (weighted)
      weight <- if (!is.null(edges$weight)) abs(edges$weight[e]) else 1
      force <- attraction * dist^2 / k * weight
      disp_vec <- (delta / dist) * force

      disp[i, ] <- disp[i, ] - disp_vec
      disp[j, ] <- disp[j, ] + disp_vec
    }

    # Apply anchor force (pulls nodes toward initial positions)
    if (!is.null(anchor_pos) && anchor_strength > 0) {
      for (i in seq_len(n)) {
        anchor_delta <- anchor_pos[i, ] - pos[i, ]
        disp[i, ] <- disp[i, ] + anchor_delta * anchor_strength
      }
    }

    # Apply displacement with temperature limit
    for (i in seq_len(n)) {
      disp_len <- sqrt(sum(disp[i, ]^2))
      if (disp_len > 0) {
        # Limit displacement to temperature
        scale <- min(disp_len, temp) / disp_len
        pos[i, ] <- pos[i, ] + disp[i, ] * scale
      }
    }

    # Keep within bounds [0, 1]
    pos[, 1] <- pmin(pmax(pos[, 1], 0.05), 0.95)
    pos[, 2] <- pmin(pmax(pos[, 2], 0.05), 0.95)

    # Cool down
    temp <- temp * cooling
  }

  # Apply max_displacement constraint (for animations)
  if (!is.null(max_displacement) && !is.null(anchor_pos)) {
    for (i in seq_len(n)) {
      delta <- pos[i, ] - anchor_pos[i, ]
      dist <- sqrt(sum(delta^2))
      if (dist > max_displacement) {
        # Scale back to max_displacement
        pos[i, ] <- anchor_pos[i, ] + delta * (max_displacement / dist)
      }
    }
  }

  data.frame(x = pos[, 1], y = pos[, 2])
}
