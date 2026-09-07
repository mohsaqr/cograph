# Numerical kernels for volume and maximal clique centrality.

#' Volume of a closed hop neighbourhood
#' @param b Symmetric binary adjacency matrix with zero diagonal.
#' @param radius Nonnegative integer hop radius, or Inf.
#' @return Sum of original-graph degrees in each closed neighbourhood.
#' @keywords internal
#' @noRd
.cg_volume <- function(b, radius = 2) {
  if (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
        radius < 0 || (is.finite(radius) && radius != floor(radius))) {
    stop("volume_radius must be a nonnegative integer or Inf", call. = FALSE)
  }
  deg <- rowSums(b)
  if (radius == 0 || !length(deg)) return(as.numeric(deg))
  d <- .cg_distances(b)
  as.numeric((is.finite(d) & d <= radius) %*% deg)
}

#' Maximal clique centrality on a simple graph
#'
#' Pivoted Bron-Kerbosch enumeration accumulates contributions without
#' retaining every clique. Singleton cliques are excluded by convention.
#' @param b Symmetric binary adjacency matrix with zero diagonal.
#' @return Sum of (clique size minus one) factorial for incident cliques.
#' @keywords internal
#' @noRd
.cg_mcc <- function(b) {
  n <- nrow(b)
  out <- numeric(n)
  nb <- lapply(seq_len(n), function(i) which(b[i, ] != 0))
  visit <- function(clique, candidates, excluded) {
    if (!length(candidates) && !length(excluded)) {
      k <- length(clique)
      if (k < 2L) return(invisible(NULL))
      if (k > 171L) {
        stop("MCC exceeds finite double precision (clique size > 171)",
             call. = FALSE)
      }
      out[clique] <<- out[clique] + factorial(k - 1L)
      return(invisible(NULL))
    }
    union_set <- union(candidates, excluded)
    overlaps <- vapply(union_set, function(u) {
      sum(candidates %in% nb[[u]])
    }, integer(1))
    pivot <- union_set[which.max(overlaps)]
    for (v in setdiff(candidates, nb[[pivot]])) {
      visit(c(clique, v), intersect(candidates, nb[[v]]),
            intersect(excluded, nb[[v]]))
      candidates <- setdiff(candidates, v)
      excluded <- c(excluded, v)
    }
    invisible(NULL)
  }
  visit(integer(0), which(rowSums(b) > 0), integer(0))
  if (any(!is.finite(out))) {
    stop("MCC exceeds finite double precision", call. = FALSE)
  }
  out
}
