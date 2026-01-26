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

  # Gephi Fruchterman-Reingold layout
  register_layout("gephi_fr", compute_layout_gephi_fr)
  register_layout("gephi", compute_layout_gephi_fr)  # Alias
}
