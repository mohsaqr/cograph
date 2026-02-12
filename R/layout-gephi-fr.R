#' Gephi Fruchterman-Reingold Layout
#'
#' Force-directed layout that replicates Gephi's Fruchterman-Reingold algorithm.
#' This is a strict port of the Java implementation from Gephi's source code,
#' with additional improvements for reproducibility and flexibility.
#'
#' @param g An igraph graph object.
#' @param area Area parameter controlling node spread. Default 10000.
#' @param gravity Gravity force pulling nodes toward center. Default 1.0.
#'   (Note: Reduced from Gephi's default of 10.0 to prevent circular layouts)
#' @param speed Speed/cooling parameter. Default 1.0.
#' @param niter Number of iterations. Default 100.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param initial Optional initial coordinates (matrix or data frame).
#'   Useful for warm-starting or animations.
#' @param normalize Logical. If TRUE (default), normalize output to [0,1] range.
#'   If FALSE, return raw Gephi-scale coordinates.
#' @param gravity_mode Gravity behavior: "linear" (default, standard gravity),
#'   "degree" (high-degree nodes feel stronger gravity, creates hub structure),
#'   or "none" (no gravity, like igraph FR).
#' @param cooling_mode Cooling schedule: "constant" (default, no cooling),
#'   "vcf" (Variable Cooling Factor - adapts based on movement),
#'   or "linear" (linear decrease over iterations).
#' @param anchor_strength Strength of force pulling nodes toward initial positions
#'   (default: 0). Only applies when `initial` is provided.
#'
#' @return A matrix with x,y coordinates for each node.
#'
#' @details
#' This layout is a direct port of Gephi's ForceAtlas algorithm variant of
#' Fruchterman-Reingold. Key differences from igraph's layout_with_fr:
#' \itemize{
#'   \item Uses Gephi's specific constants (SPEED_DIVISOR=800, AREA_MULTIPLICATOR=10000)
#'   \item Includes configurable gravity toward center
#'   \item Different cooling/speed mechanism
#'   \item Supports multiple gravity modes for different layout styles
#' }
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' g <- make_ring(10)
#' coords <- layout_gephi_fr(g, seed = 42)
#' plot(g, layout = coords)
#'
#' # Non-circular layout with degree-based gravity
#' coords2 <- layout_gephi_fr(g, gravity_mode = "degree", seed = 42)
#' }
#'
#' @keywords internal
layout_gephi_fr <- function(g, area = 10000, gravity = 1.0, speed = 1.0,
                            niter = 100, seed = NULL, initial = NULL,
                            normalize = TRUE,
                            gravity_mode = c("linear", "degree", "none"),
                            cooling_mode = c("constant", "vcf", "linear"),
                            anchor_strength = 0) {

  # Match arguments
  gravity_mode <- match.arg(gravity_mode)
  cooling_mode <- match.arg(cooling_mode)

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }


  # 1. Setup & Constants
  # Original Gephi values (SPEED_DIVISOR=800) are for interactive animation
  # For static layouts, we need much faster convergence
  SPEED_DIVISOR <- 10.0  # Was 800 - way too slow for static layouts
  AREA_MULTIPLICATOR <- 10000.0

  # Get graph data
  nodes_count <- igraph::vcount(g)
  if (nodes_count == 0) return(matrix(numeric(0), ncol = 2))

  # Initialize positions
  if (!is.null(initial)) {
    if (is.matrix(initial)) {
      coords <- initial
    } else {
      coords <- as.matrix(initial[, c("x", "y")])
    }
    # Scale from [0,1] to Gephi coordinate space if normalized input
    if (all(coords >= 0 & coords <= 1)) {
      coords <- coords * 1000 - 500
    }
    # Store anchor positions for animation constraints
    anchor_coords <- coords
  } else {
    # Random -500 to 500, roughly matching Gephi's random init
    coords <- matrix(runif(nodes_count * 2, min = -500, max = 500), ncol = 2)
    anchor_coords <- NULL
  }

  # Get node degrees for degree-based gravity
  node_degrees <- if (gravity_mode == "degree") igraph::degree(g) else NULL

  # Track initial speed for cooling
  initial_speed <- speed

  # Get Edge List (1-based indices for R)
  edges <- igraph::as_edgelist(g, names = FALSE)
  has_edges <- nrow(edges) > 0

  # 2. Pre-calculate Constants (Java: initAlgo / goAlgo start)
  # Java: float k = (float) Math.sqrt((AREA_MULTIPLICATOR * area) / (1f + nodes.length));
  k <- sqrt((AREA_MULTIPLICATOR * area) / (1.0 + nodes_count))

  # Java: float maxDisplace = (float) (Math.sqrt(AREA_MULTIPLICATOR * area) / 10f);
  max_displace <- sqrt(AREA_MULTIPLICATOR * area) / 10.0

  # 3. Main Algorithm Loop (Replicates 'goAlgo' repeated niter times)
  for (iter in 1:niter) {

    # Initialize displacements to 0 for this pass
    disp <- matrix(0, nrow = nodes_count, ncol = 2)

    # --- REPULSION (All Nodes vs All Nodes) ---
    # We use outer product to get matrix of all xDist and yDist
    dx_mat <- outer(coords[, 1], coords[, 1], "-") # N1.x - N2.x
    dy_mat <- outer(coords[, 2], coords[, 2], "-") # N1.y - N2.y

    sq_dist_mat <- dx_mat^2 + dy_mat^2
    dist_mat <- sqrt(sq_dist_mat)

    # Avoid division by zero
    safe_sq_dist <- sq_dist_mat
    safe_sq_dist[safe_sq_dist == 0] <- Inf

    # Java: float repulsiveF = k * k / dist;
    # Simplified: force = xDist * (k^2 / dist^2)
    factor <- (k^2) / safe_sq_dist

    # Apply forces
    disp[, 1] <- disp[, 1] + rowSums(dx_mat * factor)
    disp[, 2] <- disp[, 2] + rowSums(dy_mat * factor)

    # --- ATTRACTION (Edges Only) ---
    if (has_edges) {
      src_indices <- edges[, 1]
      tgt_indices <- edges[, 2]

      x_dist <- coords[src_indices, 1] - coords[tgt_indices, 1]
      y_dist <- coords[src_indices, 2] - coords[tgt_indices, 2]

      dist <- sqrt(x_dist^2 + y_dist^2)

      mask <- dist > 0

      if (any(mask)) {
        x_d <- x_dist[mask]
        y_d <- y_dist[mask]
        d   <- dist[mask]

        # Java: float attractiveF = dist * dist / k;
        # Simplified: source.dx -= xDist * dist / k
        force_factor <- d / k

        fx <- x_d * force_factor
        fy <- y_d * force_factor

        # Update Sources (subtract) - aggregate by index
        fx_src_accum <- tapply(fx, src_indices[mask], sum)
        fy_src_accum <- tapply(fy, src_indices[mask], sum)

        src_rows <- as.integer(names(fx_src_accum))
        disp[src_rows, 1] <- disp[src_rows, 1] - fx_src_accum
        disp[src_rows, 2] <- disp[src_rows, 2] - fy_src_accum

        # Update Targets (add)
        fx_tgt_accum <- tapply(fx, tgt_indices[mask], sum)
        fy_tgt_accum <- tapply(fy, tgt_indices[mask], sum)

        tgt_rows <- as.integer(names(fx_tgt_accum))
        disp[tgt_rows, 1] <- disp[tgt_rows, 1] + fx_tgt_accum
        disp[tgt_rows, 2] <- disp[tgt_rows, 2] + fy_tgt_accum
      }
    }

    # --- GRAVITY ---
    if (gravity_mode == "degree" && !is.null(node_degrees)) {
      # Degree-based gravity: high-degree nodes feel stronger pull
      # This creates hub-and-spoke structures instead of circular layouts
      max_degree <- max(node_degrees, 1)
      degree_factor <- node_degrees / max_degree
      gravity_factor <- 0.01 * k * gravity * degree_factor
      disp[, 1] <- disp[, 1] - (coords[, 1] * gravity_factor)
      disp[, 2] <- disp[, 2] - (coords[, 2] * gravity_factor)
    } else if (gravity_mode == "none") {
      # No gravity - nodes can drift freely (like igraph FR)
      # Skip gravity entirely
    } else {
      # Linear gravity (default): all nodes pulled equally toward center
      gravity_factor <- 0.01 * k * gravity
      disp[, 1] <- disp[, 1] - (coords[, 1] * gravity_factor)
      disp[, 2] <- disp[, 2] - (coords[, 2] * gravity_factor)
    }

    # --- ANCHOR FORCE (for animations) ---
    if (!is.null(anchor_coords) && anchor_strength > 0) {
      anchor_delta_x <- anchor_coords[, 1] - coords[, 1]
      anchor_delta_y <- anchor_coords[, 2] - coords[, 2]
      disp[, 1] <- disp[, 1] + anchor_delta_x * anchor_strength
      disp[, 2] <- disp[, 2] + anchor_delta_y * anchor_strength
    }

    # --- SPEED (Cooling) ---
    speed_ratio <- speed / SPEED_DIVISOR
    disp <- disp * speed_ratio

    # --- APPLY DISPLACEMENT ---
    disp_dist <- sqrt(disp[, 1]^2 + disp[, 2]^2)

    move_mask <- disp_dist > 0

    if (any(move_mask)) {
      limit_val <- max_displace * speed_ratio

      # Calculate scaling factor
      scale_factors <- rep(1, nodes_count)

      limited_mask <- move_mask & (disp_dist > limit_val)
      scale_factors[limited_mask] <- limit_val / disp_dist[limited_mask]

      coords[move_mask, 1] <- coords[move_mask, 1] + (disp[move_mask, 1] * scale_factors[move_mask])
      coords[move_mask, 2] <- coords[move_mask, 2] + (disp[move_mask, 2] * scale_factors[move_mask])
    }

    # --- COOLING ---
    if (cooling_mode == "vcf") {
      # Variable Cooling Factor - adapts based on movement
      movement <- mean(sqrt(disp[, 1]^2 + disp[, 2]^2))
      if (movement < k * 0.1) {
        speed <- speed * 0.95  # Cool faster when stable
      }
      # Don't cool when still moving much
    } else if (cooling_mode == "linear") {
      # Linear cooling
      speed <- initial_speed * (1 - iter / niter)
    }
    # "constant" = no change (original Gephi behavior)
  }

  # --- NORMALIZATION (preserving aspect ratio) ---
  if (normalize) {
    # Center at origin
    coords[, 1] <- coords[, 1] - mean(coords[, 1])
    coords[, 2] <- coords[, 2] - mean(coords[, 2])

    # Scale by SAME factor for both dimensions (preserve aspect ratio!)
    max_extent <- max(abs(coords))
    if (max_extent > 0) {
      scale_factor <- 0.45 / max_extent
      coords[, 1] <- coords[, 1] * scale_factor
      coords[, 2] <- coords[, 2] * scale_factor
    }

    # Shift to center at 0.5
    coords[, 1] <- coords[, 1] + 0.5
    coords[, 2] <- coords[, 2] + 0.5
  }

  return(coords)
}


#' Wrapper for Gephi FR Layout (for layout registry)
#'
#' @param network A cograph_network object.
#' @param area Area parameter. Default 10000.
#' @param gravity Gravity force. Default 1.0.
#' @param speed Speed parameter. Default 1.0.
#' @param niter Number of iterations. Default 100.
#' @param seed Random seed for reproducibility.
#' @param initial Optional initial coordinates.
#' @param normalize Normalize output to [0,1]. Default TRUE.
#' @param gravity_mode Gravity behavior: "linear", "degree", or "none".
#' @param cooling_mode Cooling schedule: "constant", "vcf", or "linear".
#' @param anchor_strength Anchor force strength for animations.
#' @param ... Additional arguments (ignored).
#'
#' @return Data frame with x, y coordinates.
#' @keywords internal
compute_layout_gephi_fr <- function(network, area = 10000, gravity = 1.0,
                                    speed = 1.0, niter = 100, seed = NULL,
                                    initial = NULL, normalize = TRUE,
                                    gravity_mode = "linear",
                                    cooling_mode = "constant",
                                    anchor_strength = 0, ...) {
  g <- network_to_igraph(network)
  coords <- layout_gephi_fr(g, area = area, gravity = gravity,
                            speed = speed, niter = niter, seed = seed,
                            initial = initial, normalize = normalize,
                            gravity_mode = gravity_mode,
                            cooling_mode = cooling_mode,
                            anchor_strength = anchor_strength)

  data.frame(
    x = coords[, 1],
    y = coords[, 2]
  )
}
