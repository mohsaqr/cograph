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
#' @param iterations Number of iterations (default: 200).
#' @param cooling Rate of temperature decrease for exponential cooling (default: 0.95).
#' @param repulsion Repulsion constant (default: 1.5).
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
#' @param area Area parameter controlling node spread (default: 1.5). Higher values
#'   spread nodes further apart.
#' @param gravity Gravity force pulling nodes toward center (default: 0). Higher
#'   values (e.g., 0.5-2) prevent nodes from drifting apart.
#' @param init Initialization method: "random" (default) or "circular".
#' @param cooling_mode Cooling schedule: "exponential" (default, uses `cooling`
#'   parameter), "vcf" (Variable Cooling Factor - adapts based on movement),
#'   or "linear" (linear decrease over iterations).
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
#'
#' # With gravity to keep nodes centered
#' coords3 <- layout_spring(net, gravity = 0.5, area = 2, seed = 42)
#'
#' # With circular initialization and VCF cooling
#' coords4 <- layout_spring(net, init = "circular", cooling_mode = "vcf", seed = 42)
layout_spring <- function(network, iterations = 200, cooling = 0.95,
                          repulsion = 1.5, attraction = 1, seed = NULL,
                          initial = NULL, max_displacement = NULL,
                          anchor_strength = 0, area = 1.5, gravity = 0,
                          init = c("random", "circular"),
                          cooling_mode = c("exponential", "vcf", "linear")) {

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
