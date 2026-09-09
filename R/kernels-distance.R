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
#' Weights are distances, and a non-positive entry means "no edge". Negative
#' weights are rejected rather than silently treated as absent, matching
#' igraph, which refuses them outright. Binary graphs use breadth-first
#' layering by boolean matrix products (one product per hop); weighted
#' graphs use Floyd-Warshall with each of the n relaxation sweeps
#' vectorised, which is far faster in R than n interpreted Dijkstra runs.
#' Single-source Dijkstra (`.cg_dijkstra`) remains for callers that need
#' one row or a predecessor structure.
#'
#' @param m Numeric adjacency matrix.
#' @param mode One of \code{"all"}, \code{"out"}, \code{"in"}.
#' @param cutoff Non-negative number: distances above it become \code{Inf};
#'   negative (default) means no cutoff.
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
  has_edge <- w > 0
  d <- if (all(w[has_edge] == 1)) {
    .cg_hop_matrix(has_edge, n)
  } else {
    .cg_floyd_warshall(w, has_edge, n)
  }
  if (is.numeric(cutoff) && length(cutoff) == 1L && cutoff >= 0) d[d > cutoff] <- Inf
  dimnames(d) <- dimnames(w)
  d
}

#' Hop distances of a binary graph by layered boolean products
#' @param has_edge Logical adjacency (row = from, col = to).
#' @param n Vertex count.
#' @return Numeric matrix of hop counts.
#' @keywords internal
#' @noRd
.cg_hop_matrix <- function(has_edge, n) {
  a <- has_edge * 1
  diag_idx <- cbind(seq_len(n), seq_len(n))
  d <- matrix(Inf, n, n)
  d[diag_idx] <- 0
  visited <- diag(TRUE, n)
  frontier <- visited * 1
  k <- 0L
  # Each sweep discovers the next hop layer for every source at once; the
  # loop runs once per unit of the diameter, not once per vertex.
  repeat {
    k <- k + 1L
    nxt <- ((frontier %*% a) > 0) & !visited
    if (!any(nxt)) break
    d[nxt] <- k
    visited <- visited | nxt
    frontier <- nxt * 1
  }
  d
}

#' Floyd-Warshall with vectorised sweeps
#' @param w Effective out-edge weight matrix.
#' @param has_edge Logical adjacency.
#' @param n Vertex count.
#' @return Numeric distance matrix.
#' @keywords internal
#' @noRd
.cg_floyd_warshall <- function(w, has_edge, n) {
  d <- matrix(Inf, n, n)
  d[has_edge] <- w[has_edge]
  d[cbind(seq_len(n), seq_len(n))] <- 0
  # The k-th sweep relaxes every pair through vertex k; the sweeps must run in
  # order (each builds on the previous), but each one is a single vector op.
  for (k in seq_len(n)) {
    via <- d[, k] + rep(d[k, ], each = n)
    d <- pmin(d, via)
  }
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

