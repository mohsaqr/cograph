#' @title Fruchterman-Reingold Spring Layout
#' @description Force-directed layout using the Fruchterman-Reingold algorithm.
#' @name layout-spring
#' @keywords internal
#' @noRd
NULL

#' Fruchterman-Reingold Spring Layout
#'
#' Computes node positions with the Fruchterman-Reingold force-directed
#' algorithm. Nodes connected by edges attract each other and all nodes repel
#' each other. Attraction along an edge is scaled by the absolute edge weight.
#' Duplicate edges between the same pair of nodes, in either direction, are
#' merged by summing their weights.
#'
#' @param network A \code{CographNetwork} or \code{cograph_network} object.
#' @param iterations Number of iterations (default: 200).
#' @param cooling Factor by which the temperature is multiplied after each
#'   iteration when \code{cooling_mode = "exponential"} (default: 0.95).
#' @param repulsion Repulsion constant (default: 1.5).
#' @param attraction Attraction constant (default: 1).
#' @param seed Random seed for the random initial positions. NULL (default)
#'   uses the current random state. When a seed is given, the caller's random
#'   state is restored on exit.
#' @param initial Optional initial coordinates, a matrix or a data frame with
#'   columns \code{x} and \code{y}. When supplied, \code{init} is ignored.
#'   For animations, pass the previous frame's layout to obtain smooth
#'   transitions.
#' @param max_displacement Maximum distance a node can move from its initial
#'   position (default: NULL, no limit). Applies only when \code{initial} is
#'   supplied. In that case the coordinates are returned without the final
#'   rescaling. Values such as 0.05 to 0.1 suit animations.
#' @param anchor_strength Strength of the force pulling nodes toward their
#'   initial positions (default: 0). Higher values (for example 0.5 to 2) keep
#'   nodes closer to their starting positions. Applies only when
#'   \code{initial} is supplied.
#' @param area Area parameter (default: 1.5). It sets the ideal edge length
#'   \eqn{k = \sqrt{area / n}}{k = sqrt(area / n)} and the initial temperature
#'   \eqn{\sqrt{area} / 10}{sqrt(area) / 10}. Because the final coordinates are rescaled,
#'   \code{area} changes the relative arrangement of the nodes and leaves the
#'   overall extent unchanged.
#' @param gravity Strength of the force pulling nodes toward the centroid of
#'   the layout (default: 0). Higher values (for example 0.5 to 2) give a more
#'   compact layout.
#' @param init Initialization method, \code{"random"} (default) or
#'   \code{"circular"}.
#' @param cooling_mode Cooling schedule. \code{"exponential"} (default)
#'   multiplies the temperature by \code{cooling} after each iteration.
#'   \code{"vcf"} (variable cooling factor) multiplies it by 0.9 when the mean
#'   displacement is below \eqn{0.1k} and by 0.99 otherwise. \code{"linear"}
#'   multiplies it by \eqn{1 - t / T} at iteration \eqn{t} of \eqn{T}.
#' @param ... Additional arguments (ignored).
#' @return A data frame with columns \code{x} and \code{y} and one row per
#'   node. The coordinates are centered on their mean and scaled by a common
#'   factor into \eqn{[0.05, 0.95]}, which preserves the aspect ratio of the
#'   layout. A
#'   network without edges returns the initial positions, and a single node is
#'   placed at (0.5, 0.5).
#'
#' @examples
#' layout_spring(CographNetwork$new(regulation_net), seed = 42)
#'
#' @export
layout_spring <- function(network, iterations = 200, cooling = 0.95,
                          repulsion = 1.5, attraction = 1, seed = NULL,
                          initial = NULL, max_displacement = NULL,
                          anchor_strength = 0, area = 1.5, gravity = 0,
                          init = c("random", "circular"),
                          cooling_mode = c("exponential", "vcf", "linear"), ...) {

  # Match arguments
  init <- match.arg(init)
  cooling_mode <- match.arg(cooling_mode)

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

  # Set seed if provided, restoring RNG state on exit
  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
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
  } else if (init == "circular") {
    # Circular initialization
    angles <- seq(0, 2 * pi, length.out = n + 1)[1:n]
    radius <- 0.4
    pos <- cbind(
      x = 0.5 + radius * cos(angles),
      y = 0.5 + radius * sin(angles)
    )
    anchor_pos <- NULL
  } else {
    # Random initial positions
    pos <- cbind(
      x = stats::runif(n),
      y = stats::runif(n)
    )
    anchor_pos <- NULL
  }

  # Get edges (support both R6 and S3 cograph_network)
  edges <- if (inherits(network, "cograph_network")) {
    get_edges(network)
  } else {
    network$get_edges()
  }
  if (is.null(edges) || nrow(edges) == 0) {
    # No edges: return random positions
    return(data.frame(x = pos[, 1], y = pos[, 2]))
  }

  # Aggregate duplicate edges for layout efficiency
  # (26 nodes with 24k edges -> 26*25 = 650 unique pairs max)
  edge_key <- paste(pmin(edges$from, edges$to), pmax(edges$from, edges$to), sep = "-")
  if (length(unique(edge_key)) < nrow(edges)) {
    # Has duplicates - aggregate by summing weights
    weights <- if (!is.null(edges$weight)) edges$weight else rep(1, nrow(edges))
    agg <- stats::aggregate(weights, by = list(
      from = pmin(edges$from, edges$to),
      to = pmax(edges$from, edges$to)
    ), FUN = sum)
    edges <- data.frame(from = agg$from, to = agg$to, weight = agg$x)
  }

  # Optimal distance (based on area parameter)
  k <- sqrt(area / n)

  # Temperature (controls maximum displacement)
  temp <- sqrt(area) * 0.1

  # Fruchterman-Reingold iterations
  for (iter in seq_len(iterations)) {
    # Initialize displacement vectors
    disp <- matrix(0, nrow = n, ncol = 2)

    # Calculate repulsive forces between all pairs (VECTORIZED)
    # Using outer product for O(n²) computation without nested loops
    dx_mat <- outer(pos[, 1], pos[, 1], "-")
    dy_mat <- outer(pos[, 2], pos[, 2], "-")
    sq_dist <- dx_mat^2 + dy_mat^2
    sq_dist[sq_dist == 0] <- 1e-6  # Avoid division by zero
    dist_mat <- sqrt(sq_dist)

    # Repulsive force: k^2 / dist
    force_mat <- repulsion * k^2 / sq_dist
    disp[, 1] <- rowSums(dx_mat * force_mat / dist_mat)
    disp[, 2] <- rowSums(dy_mat * force_mat / dist_mat)

    # Calculate attractive forces along edges (FULLY VECTORIZED)
    from_idx <- edges$from
    to_idx <- edges$to
    weights <- if (!is.null(edges$weight)) abs(edges$weight) else rep(1, nrow(edges))

    # Delta vectors for all edges at once
    delta_x <- pos[from_idx, 1] - pos[to_idx, 1]
    delta_y <- pos[from_idx, 2] - pos[to_idx, 2]
    edge_dist <- sqrt(delta_x^2 + delta_y^2)
    edge_dist[edge_dist < 0.001] <- 0.001

    # Force magnitude: attraction * dist^2 / k * weight
    force_mag <- attraction * edge_dist^2 / k * weights

    # Displacement vectors per edge
    disp_x <- (delta_x / edge_dist) * force_mag
    disp_y <- (delta_y / edge_dist) * force_mag

    # Aggregate using rowsum (fast C implementation)
    # 'from' nodes: subtract displacement
    from_agg_x <- rowsum(-disp_x, from_idx, reorder = FALSE)
    from_agg_y <- rowsum(-disp_y, from_idx, reorder = FALSE)
    from_nodes <- as.integer(rownames(from_agg_x))
    disp[from_nodes, 1] <- disp[from_nodes, 1] + from_agg_x[, 1]
    disp[from_nodes, 2] <- disp[from_nodes, 2] + from_agg_y[, 1]

    # 'to' nodes: add displacement
    to_agg_x <- rowsum(disp_x, to_idx, reorder = FALSE)
    to_agg_y <- rowsum(disp_y, to_idx, reorder = FALSE)
    to_nodes <- as.integer(rownames(to_agg_x))
    disp[to_nodes, 1] <- disp[to_nodes, 1] + to_agg_x[, 1]
    disp[to_nodes, 2] <- disp[to_nodes, 2] + to_agg_y[, 1]

    # Apply gravity force (pulls nodes toward center)
    if (gravity > 0) {
      center <- colMeans(pos)
      to_center_x <- center[1] - pos[, 1]
      to_center_y <- center[2] - pos[, 2]
      disp[, 1] <- disp[, 1] + to_center_x * gravity * k
      disp[, 2] <- disp[, 2] + to_center_y * gravity * k
    }

    # Apply anchor force (pulls nodes toward initial positions)
    if (!is.null(anchor_pos) && anchor_strength > 0) {
      anchor_delta_x <- anchor_pos[, 1] - pos[, 1]
      anchor_delta_y <- anchor_pos[, 2] - pos[, 2]
      disp[, 1] <- disp[, 1] + anchor_delta_x * anchor_strength
      disp[, 2] <- disp[, 2] + anchor_delta_y * anchor_strength
    }

    # Apply displacement with temperature limit (vectorized)
    disp_len <- sqrt(disp[, 1]^2 + disp[, 2]^2)
    scale <- pmin(disp_len, temp) / pmax(disp_len, 1e-6)
    pos[, 1] <- pos[, 1] + disp[, 1] * scale
    pos[, 2] <- pos[, 2] + disp[, 2] * scale

    # NO box constraint during iteration - let forces determine natural shape!
    # Normalization happens at the end

    # Cool down based on cooling mode
    if (cooling_mode == "vcf") {
      # Variable Cooling Factor - adapts based on movement
      movement <- mean(sqrt(disp[, 1]^2 + disp[, 2]^2))
      cool_factor <- if (movement < k * 0.1) 0.9 else 0.99
      temp <- temp * cool_factor
    } else if (cooling_mode == "linear") {
      # Linear cooling
      temp <- temp * (1 - iter / iterations)
    } else {
      # Exponential cooling (default)
      temp <- temp * cooling
    }
  }

  # Apply max_displacement constraint (for animations)
  # When max_displacement is set with initial layout, skip normalization
  # to preserve relative positions for smooth animations
  if (!is.null(max_displacement) && !is.null(anchor_pos)) {
    for (i in seq_len(n)) {
      delta <- pos[i, ] - anchor_pos[i, ]
      dist <- sqrt(sum(delta^2))
      if (dist > max_displacement) {
        # Scale back to max_displacement
        pos[i, ] <- anchor_pos[i, ] + delta * (max_displacement / dist)
      }
    }
    # Return without normalization - positions stay relative to initial
    return(data.frame(x = pos[, 1], y = pos[, 2]))
  }

  # Normalize to [0.05, 0.95] while PRESERVING ASPECT RATIO
  # (scale both dimensions by the same factor so layout shape is preserved)
  x <- pos[, 1]
  y <- pos[, 2]

  # Center at origin
  x <- x - mean(x)
  y <- y - mean(y)

  # Scale by the SAME factor for both dimensions
  max_extent <- max(abs(c(x, y)))
  if (max_extent > 0) {
    scale_factor <- 0.45 / max_extent  # Fit in [0.05, 0.95] with margin
    x <- x * scale_factor
    y <- y * scale_factor
  }

  # Shift to center at 0.5
  x <- x + 0.5
  y <- y + 0.5

  data.frame(x = x, y = y)
}
