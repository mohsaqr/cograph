#' Gephi Fruchterman-Reingold Layout
#'
#' Force-directed layout that replicates Gephi's Fruchterman-Reingold algorithm.
#' This is a strict port of the Java implementation from Gephi's source code.
#'
#' @param g An igraph graph object.
#' @param area Area parameter controlling node spread. Default 10000.
#' @param gravity Gravity force pulling nodes toward center. Default 10.0.
#' @param speed Speed/cooling parameter. Default 1.0.
#' @param niter Number of iterations. Default 100.
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
#' }
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' g <- make_ring(10)
#' coords <- layout_gephi_fr(g)
#' plot(g, layout = coords)
#' }
#'
#' @keywords internal
layout_gephi_fr <- function(g, area = 10000, gravity = 10.0, speed = 1.0, niter = 100) {


  # 1. Setup & Constants (Directly from Java source)
  SPEED_DIVISOR <- 800.0
  AREA_MULTIPLICATOR <- 10000.0

  # Get graph data

  nodes_count <- igraph::vcount(g)
  if (nodes_count == 0) return(matrix(numeric(0), ncol = 2))

  # Initialize positions (Random -500 to 500, roughly matching Gephi's random init)
  coords <- matrix(runif(nodes_count * 2, min = -500, max = 500), ncol = 2)

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
    # Simplified: 0.01 * k * gravity * x (linear form, robust at center)
    gravity_factor <- 0.01 * k * gravity

    disp[, 1] <- disp[, 1] - (coords[, 1] * gravity_factor)
    disp[, 2] <- disp[, 2] - (coords[, 2] * gravity_factor)

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
  }

  return(coords)
}


#' Wrapper for Gephi FR Layout (for layout registry)
#'
#' @param network A cograph_network object.
#' @param area Area parameter. Default 10000.
#' @param gravity Gravity force. Default 10.0.
#' @param speed Speed parameter. Default 1.0.
#' @param niter Number of iterations. Default 100.
#' @param ... Additional arguments (ignored).
#'
#' @return Data frame with x, y coordinates.
#' @keywords internal
compute_layout_gephi_fr <- function(network, area = 10000, gravity = 10.0,
                                     speed = 1.0, niter = 100, ...) {
  g <- network_to_igraph(network)
  coords <- layout_gephi_fr(g, area = area, gravity = gravity,
                            speed = speed, niter = niter)

  data.frame(
    x = coords[, 1],
    y = coords[, 2]
  )
}
