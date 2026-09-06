# ===========================================================================
# Batch 11 kernels — parameterised members of families cograph already had
#
# Each measure here sits at 0.90 <= tau < 1 against a measure cograph
# already computed (docs/zoo/parameter_candidates.csv), because it is the
# same family evaluated at a different weight, scope or mass. Pure base-R
# kernels on matrices, validated in
# tests/testthat/test-centrality-batch11.R, following the conventions of
# the earlier kernel batches.
# ===========================================================================

#' Length-scaled betweenness (Brandes 2008, Algorithm 5)
#'
#' Ordinary betweenness with each separated pair weighted by the reciprocal
#' of its distance, so brokering between two nodes that were already close
#' counts for more than brokering across the graph:
#' `sum over s != t of (1 / d(s,t)) * sigma_st(v) / sigma_st`.
#' Borgatti & Everett (2006) propose the measure; Brandes gives the
#' traversal, which is the ordinary accumulation with the unit credit
#' replaced by `1 / d`.
#'
#' @param w Weight matrix, already mode-adjusted.
#' @param n Vertex count.
#' @param directed Whether the graph is directed.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_length_scaled_betweenness <- function(w, n, directed) {
  .cg_betweenness(w, n, directed,
                  pair_weight = function(d) if (d > 0) 1 / d else 0)
}

#' Distance-decayed betweenness (Agneessens, Borgatti & Everett 2017)
#'
#' The same traversal with the pair weight `(d(s,t) - 1)^-delta`. At
#' `delta = 0` every pair counts once and the measure is ordinary
#' betweenness; raising `delta` concentrates the score on the pairs a node
#' separates locally. Adjacent pairs have no intermediary at all, so they
#' contribute nothing and the singularity at `d = 1` never arises.
#'
#' @inheritParams .cg_length_scaled_betweenness
#' @param delta Decay exponent.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_delta_betweenness <- function(w, n, directed, delta = 1) {
  .cg_betweenness(w, n, directed,
                  pair_weight = function(d) if (d > 1) (d - 1)^(-delta) else 0)
}

#' Ego betweenness (Everett & Borgatti 2005)
#'
#' Betweenness computed inside each node's own ego network -- the subgraph
#' induced on the node and its neighbours -- rather than across the whole
#' graph. It asks how much of the brokerage a node could observe from where
#' it stands, which is why it can be estimated from ego-network data alone.
#' A node with fewer than two neighbours brokers nothing.
#'
#' @param a 0/1 matrix. `a[i, j] = 1` when `i` and `j` are adjacent.
#' @param directed Whether the graph is directed.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_ego_betweenness <- function(a, directed = FALSE) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  vapply(seq_len(n), function(v) {
    nbrs <- which(a[v, ] != 0 | a[, v] != 0)
    nbrs <- nbrs[nbrs != v]
    if (length(nbrs) < 2L) return(0)
    ids <- c(v, nbrs)
    sub <- a[ids, ids, drop = FALSE]
    .cg_betweenness(sub, length(ids), directed)[1L]
  }, numeric(1L))
}

#' Geodesic power closeness (Agneessens, Borgatti & Everett 2017, eq. 2)
#'
#' `c_delta(i) = sum_j d_ij^-delta / (n - 1)`, one exponent tuning how far
#' the measure looks. Unreachable nodes contribute nothing but stay in the
#' denominator. The family spans the usual closeness measures: `delta = 1`
#' is harmonic centrality over `n - 1`, `delta = 2` is the sum of inverse
#' squared distances, a large `delta` approaches degree over `n - 1`, and
#' `delta = 0` counts the reachable set.
#'
#' @param d Distance matrix.
#' @param delta Exponent, at least 0.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_delta_closeness <- function(d, delta = 1) {
  n <- nrow(d)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (n == 1L) return(0)
  contrib <- d^(-delta)
  contrib[!is.finite(contrib) | row(d) == col(d) | d == 0] <- 0
  rowSums(contrib) / (n - 1)
}

#' Gravity family (Ma et al. 2016; Li et al. 2019)
#'
#' `G(i) = sum over j of m_i m_j / d_ij^exponent`, optionally truncated at
#' `radius`. The published members differ only in what plays the part of
#' mass and how far the sum reaches: Ma's gravity centrality uses the
#' k-shell of both ends within three steps, Li's gravity model uses the
#' degree of both ends over the whole graph, and their local gravity model
#' truncates that at a radius near half the mean distance.
#'
#' @param d Distance matrix.
#' @param mass_i,mass_j Mass of the focal node and of its partners. They
#'   differ only for the historical cograph form, which carries no mass for
#'   the focal node.
#' @param radius Largest distance to include; `NULL` for no limit.
#' @param exponent Power of the distance in the denominator.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_gravity <- function(d, mass_i, mass_j, radius = NULL, exponent = 2) {
  n <- nrow(d)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (n == 1L) return(0)
  within <- is.finite(d) & d > 0 & row(d) != col(d)
  if (!is.null(radius)) within <- within & d <= radius
  pull <- ifelse(within, outer(mass_i, mass_j) / d^exponent, 0)
  rowSums(pull)
}

#' Truncation radius recommended by Li et al. (2019), eq. 5
#'
#' Half the mean shortest-path length, rounded, and never below 1.
#'
#' @param d Distance matrix.
#' @return Integer radius.
#' @keywords internal
#' @noRd
.cg_gravity_auto_radius <- function(d) {
  off <- row(d) != col(d) & is.finite(d) & d > 0
  if (!any(off)) return(1L)
  max(1L, as.integer(round(mean(d[off]) / 2)))
}
