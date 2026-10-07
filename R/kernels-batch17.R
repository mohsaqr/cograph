#' Extended neighborhood coreness
#' @param b Simple undirected adjacency matrix.
#' @param core Core numbers in vertex order.
#' @return A squared times the core vector, including backtracking walks.
#' @keywords internal
#' @noRd
.cg_extended_coreness <- function(b, core) {
  as.numeric(b %*% (b %*% core))
}

#' Extended gravity centrality with k-shell masses
#' @param b Simple undirected adjacency matrix.
#' @param core Core numbers in vertex order.
#' @param radius Nonnegative distance cutoff, NULL, or auto.
#' @return Sum of the immediate neighbors' raw gravity scores.
#' @keywords internal
#' @noRd
.cg_extended_gravity <- function(b, core, radius = 3) {
  automatic <- identical(radius, "auto")
  if (!is.null(radius) && !automatic &&
        (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
           radius < 0)) {
    .cg_stop_bad_parameter("gravity_radius must be nonnegative, NULL, or 'auto'")
  }
  if (!nrow(b)) return(numeric(0))
  d <- .cg_distances(b, "all")
  if (automatic) radius <- .cg_gravity_auto_radius(d)
  gravity <- .cg_gravity(d, core, core, radius = radius)
  as.numeric(b %*% gravity)
}
