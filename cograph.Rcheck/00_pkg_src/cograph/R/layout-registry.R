#' @title Layout Registry Functions
#' @description Functions for registering built-in layouts.
#' @name layout-registry
#' @keywords internal
NULL

#' Register Built-in Layouts
#'
#' Register all built-in layout algorithms.
#'
#' @keywords internal
register_builtin_layouts <- function() {
  # Circle layout
  register_layout("circle", layout_circle)

  # Oval/Ellipse layout
  register_layout("oval", layout_oval)
  register_layout("ellipse", layout_oval)  # Alias

  # Spring layout (Fruchterman-Reingold)
  register_layout("spring", layout_spring)
  register_layout("fr", layout_spring)  # Alias
  register_layout("fruchterman-reingold", layout_spring)  # Alias

  # Groups layout
  register_layout("groups", layout_groups)

  # Grid layout
  register_layout("grid", function(network, ncol = NULL, ...) {
    n <- network$n_nodes
    if (n == 0) return(data.frame(x = numeric(0), y = numeric(0)))
    if (n == 1) return(data.frame(x = 0.5, y = 0.5))

    if (is.null(ncol)) {
      ncol <- ceiling(sqrt(n))
    }
    nrow <- ceiling(n / ncol)

    x <- rep(seq(0.1, 0.9, length.out = ncol), times = nrow)[seq_len(n)]
    y <- rep(seq(0.9, 0.1, length.out = nrow), each = ncol)[seq_len(n)]

    data.frame(x = x, y = y)
  })

  # Random layout
  register_layout("random", function(network, seed = NULL, ...) {
    n <- network$n_nodes
    if (!is.null(seed)) set.seed(seed)
    data.frame(x = stats::runif(n, 0.1, 0.9), y = stats::runif(n, 0.1, 0.9))
  })

  # Star layout (one center node, rest in circle)
  register_layout("star", function(network, center = 1, ...) {
    n <- network$n_nodes
    if (n == 0) return(data.frame(x = numeric(0), y = numeric(0)))
    if (n == 1) return(data.frame(x = 0.5, y = 0.5))

    coords <- data.frame(x = numeric(n), y = numeric(n))
    coords$x[center] <- 0.5
    coords$y[center] <- 0.5

    others <- setdiff(seq_len(n), center)
    n_others <- length(others)

    if (n_others > 0) {
      angles <- seq(pi/2, pi/2 + 2 * pi * (1 - 1/n_others),
                    length.out = n_others)
      coords$x[others] <- 0.5 + 0.4 * cos(angles)
      coords$y[others] <- 0.5 + 0.4 * sin(angles)
    }

    coords
  })

  # Bipartite layout
  register_layout("bipartite", function(network, types = NULL, ...) {
    n <- network$n_nodes
    if (n == 0) return(data.frame(x = numeric(0), y = numeric(0)))

    if (is.null(types)) {
      # Default: alternate between two types
      types <- rep(c(0, 1), length.out = n)
    }

    type1 <- which(types == unique(types)[1])
    type2 <- which(types != unique(types)[1])

    coords <- data.frame(x = numeric(n), y = numeric(n))

    # Left side
    if (length(type1) > 0) {
      coords$x[type1] <- 0.2
      coords$y[type1] <- seq(0.9, 0.1, length.out = length(type1))
    }

    # Right side
    if (length(type2) > 0) {
      coords$x[type2] <- 0.8
      coords$y[type2] <- seq(0.9, 0.1, length.out = length(type2))
    }

    coords
  })

  # Custom layout (passthrough)
  register_layout("custom", function(network, coords, ...) {
    if (is.matrix(coords)) {
      coords <- as.data.frame(coords)
    }
    names(coords)[1:2] <- c("x", "y")
    coords
  })

  # Gephi Fruchterman-Reingold layout (full algorithm inline)
  gephi_fr_func <- function(network, area = 10000, gravity = 10.0,
                            speed = 1.0, niter = 100, ...) {
    g <- network_to_igraph(network)

    # Constants from Gephi Java source
    SPEED_DIVISOR <- 800.0
    AREA_MULTIPLICATOR <- 10000.0

    nodes_count <- igraph::vcount(g)
    if (nodes_count == 0) return(data.frame(x = numeric(0), y = numeric(0)))

    # Initialize positions
    coords <- matrix(stats::runif(nodes_count * 2, min = -500, max = 500), ncol = 2)

    # Get edges
    edges <- igraph::as_edgelist(g, names = FALSE)
    has_edges <- nrow(edges) > 0

    # Pre-calculate constants
    k <- sqrt((AREA_MULTIPLICATOR * area) / (1.0 + nodes_count))
    max_displace <- sqrt(AREA_MULTIPLICATOR * area) / 10.0

    # Main loop
    for (iter in 1:niter) {
      disp <- matrix(0, nrow = nodes_count, ncol = 2)

      # Repulsion
      dx_mat <- outer(coords[, 1], coords[, 1], "-")
      dy_mat <- outer(coords[, 2], coords[, 2], "-")
      sq_dist_mat <- dx_mat^2 + dy_mat^2
      safe_sq_dist <- sq_dist_mat
      safe_sq_dist[safe_sq_dist == 0] <- Inf
      factor <- (k^2) / safe_sq_dist
      disp[, 1] <- disp[, 1] + rowSums(dx_mat * factor)
      disp[, 2] <- disp[, 2] + rowSums(dy_mat * factor)

      # Attraction
      if (has_edges) {
        src_indices <- edges[, 1]
        tgt_indices <- edges[, 2]
        x_dist <- coords[src_indices, 1] - coords[tgt_indices, 1]
        y_dist <- coords[src_indices, 2] - coords[tgt_indices, 2]
        dist <- sqrt(x_dist^2 + y_dist^2)
        mask <- dist > 0

        if (any(mask)) {
          x_d <- x_dist[mask]; y_d <- y_dist[mask]; d <- dist[mask]
          force_factor <- d / k
          fx <- x_d * force_factor; fy <- y_d * force_factor

          fx_src <- tapply(fx, src_indices[mask], sum)
          fy_src <- tapply(fy, src_indices[mask], sum)
          src_rows <- as.integer(names(fx_src))
          disp[src_rows, 1] <- disp[src_rows, 1] - fx_src
          disp[src_rows, 2] <- disp[src_rows, 2] - fy_src

          fx_tgt <- tapply(fx, tgt_indices[mask], sum)
          fy_tgt <- tapply(fy, tgt_indices[mask], sum)
          tgt_rows <- as.integer(names(fx_tgt))
          disp[tgt_rows, 1] <- disp[tgt_rows, 1] + fx_tgt
          disp[tgt_rows, 2] <- disp[tgt_rows, 2] + fy_tgt
        }
      }

      # Gravity
      gravity_factor <- 0.01 * k * gravity
      disp[, 1] <- disp[, 1] - (coords[, 1] * gravity_factor)
      disp[, 2] <- disp[, 2] - (coords[, 2] * gravity_factor)

      # Speed
      speed_ratio <- speed / SPEED_DIVISOR
      disp <- disp * speed_ratio

      # Apply displacement
      disp_dist <- sqrt(disp[, 1]^2 + disp[, 2]^2)
      move_mask <- disp_dist > 0
      if (any(move_mask)) {
        limit_val <- max_displace * speed_ratio
        scale_factors <- rep(1, nodes_count)
        limited_mask <- move_mask & (disp_dist > limit_val)
        scale_factors[limited_mask] <- limit_val / disp_dist[limited_mask]
        coords[move_mask, 1] <- coords[move_mask, 1] + (disp[move_mask, 1] * scale_factors[move_mask])
        coords[move_mask, 2] <- coords[move_mask, 2] + (disp[move_mask, 2] * scale_factors[move_mask])
      }
    }

    data.frame(x = coords[, 1], y = coords[, 2])
  }

  register_layout("gephi_fr", gephi_fr_func)
  register_layout("gephi", gephi_fr_func)
}
