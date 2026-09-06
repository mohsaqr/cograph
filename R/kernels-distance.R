# ===========================================================================
# Dependency-free distance kernels
# ===========================================================================
# Every distance-based centrality is an arithmetic reduction of the all-pairs
# shortest-path matrix. Computing that matrix in base R removes igraph from
# the whole family at a single point (see the shared_dist_mat block in
# centrality()).
#
# Ported from the snajs TypeScript implementation and verified against
# igraph; see tests/testthat/test-kernels-distance-equivalence.R.

#' Mode-adjusted weight matrix
#'
#' Collapses direction into the weight matrix once, so the shortest-path
#' search never needs to know the mode. Under \code{"all"} a reciprocated
#' dyad takes the minimum of the two weights and an unreciprocated one takes
#' whichever side is positive, matching igraph's undirected collapse.
#'
#' @param m Numeric adjacency matrix.
#' @param mode One of \code{"all"}, \code{"out"}, \code{"in"}.
#' @return A numeric matrix of effective out-edge weights.
#' @keywords internal
#' @noRd
.cg_mode_weights <- function(m, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  if (identical(mode, "out")) return(m)
  if (identical(mode, "in")) return(t(m))
  a <- m
  b <- t(m)
  w <- ifelse(a > 0 & b > 0, pmin(a, b), pmax(a, b))
  dim(w) <- dim(m)
  dimnames(w) <- dimnames(m)
  w
}

#' All-pairs shortest paths without igraph
#'
#' Dijkstra from every source. Weights are distances, and a non-positive
#' entry means "no edge". Negative weights are rejected rather than silently
#' treated as absent, matching igraph, which refuses them outright.
#'
#' @param m Numeric adjacency matrix.
#' @param mode One of \code{"all"}, \code{"out"}, \code{"in"}.
#' @return A numeric matrix of distances, \code{Inf} where unreachable and
#'   \code{0} on the diagonal.
#' @keywords internal
#' @noRd
.cg_distances <- function(m, mode = c("all", "out", "in"), cutoff = -1) {
  mode <- match.arg(mode)
  if (any(m < 0, na.rm = TRUE)) {
    stop(errorCondition(
      "Shortest paths need non-negative weights; found a negative edge.",
      class = "cograph_negative_weights", call = NULL))
  }
  w <- .cg_mode_weights(m, mode)
  n <- nrow(w)
  if (is.null(n) || n == 0L) return(matrix(numeric(0), 0L, 0L))
  d <- t(vapply(seq_len(n), function(s) .cg_dijkstra(w, s, n), numeric(n)))
  if (is.numeric(cutoff) && length(cutoff) == 1L && cutoff >= 0) d[d > cutoff] <- Inf
  dimnames(d) <- dimnames(w)
  d
}

#' Single-source Dijkstra
#' @param w Effective out-edge weight matrix.
#' @param source Integer source index.
#' @param n Vertex count.
#' @return Numeric vector of distances from \code{source}.
#' @keywords internal
#' @noRd
.cg_dijkstra <- function(w, source, n) {
  dist <- rep(Inf, n)
  used <- rep(FALSE, n)
  dist[source] <- 0
  # Settling one vertex depends on every previous settlement, so the outer
  # sweep is irreducibly sequential; the relaxation inside it is vectorised.
  for (step in seq_len(n)) {
    cand <- dist
    cand[used] <- Inf
    if (all(is.infinite(cand))) break
    u <- which.min(cand)
    used[u] <- TRUE
    edge <- w[u, ]
    reach <- !used & edge > 0
    if (!any(reach)) next
    nd <- dist[u] + edge
    upd <- reach & nd < dist
    dist[upd] <- nd[upd]
  }
  dist
}

#' Largest finite distance
#' @param d Distance matrix.
#' @return A single number; \code{0} when nothing is reachable.
#' @keywords internal
#' @noRd
.cg_diameter <- function(d) {
  fin <- d[is.finite(d)]
  if (length(fin) == 0L) return(0)
  max(max(fin), 0)
}

#' Weight matrix for the shortest-path kernels, taken from an igraph object
#'
#' `centrality()` carries path weights as an edge-attribute vector, which may
#' be the inverted weights used by the path-based measures rather than the
#' graph's own. This assembles whichever set is actually in force into the
#' dense matrix the kernels expect.
#'
#' @param g An igraph object.
#' @param weights Edge weight vector, or `NULL` for an unweighted reading.
#' @return A numeric adjacency matrix.
#' @keywords internal
#' @noRd
.cg_path_matrix <- function(g, weights = NULL) {
  n <- igraph::vcount(g)
  m <- matrix(0, n, n)
  if (n == 0L || igraph::ecount(g) == 0L) return(m)
  el <- igraph::as_edgelist(g, names = FALSE)
  w <- if (is.null(weights)) rep(1, nrow(el)) else as.numeric(weights)
  # A repeated endpoint pair keeps the strongest connection, matching the
  # simplify step that precedes this in centrality().
  for (k in seq_len(nrow(el))) {
    i <- el[k, 1L]; j <- el[k, 2L]
    if (w[k] > m[i, j]) m[i, j] <- w[k]
  }
  if (!igraph::is_directed(g)) {
    keep <- m > t(m)
    m[!keep] <- t(m)[!keep]
  }
  m
}
