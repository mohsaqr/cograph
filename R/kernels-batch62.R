#' Hybrid centrality (Christensen, Kenett, Aste, Silvia and Kwapil 2018)
#'
#' Christensen, Kenett, Aste, Silvia and Kwapil (2018), *Behavior Research
#' Methods* 50(6):2531-2550 (read in the accepted manuscript, page 26),
#' building on Pozzi, Di Matteo and Aste (2013):
#'
#' ```
#' HC = (BC^w + BC^u + LC^w + LC^u + k^u + EC^w + EC^u + NS^w - 8) / (8 (N - 1))
#' ```
#'
#' where each term is the rank of the node on one centrality: betweenness
#' `BC`, closeness `LC` and eigenvector centrality `EC`, each on the
#' weighted (`w`) and on the unweighted (`u`) network, the degree `k` and
#' the node strength `NS`. The most central node holds the largest rank
#' and tied nodes share the largest rank of their group, so the score runs
#' from 0 to 1.
#'
#' Betweenness is Freeman's, as the paper states. The first author's
#' `NetworkToolbox::hybrid()` defaults to randomized shortest paths
#' betweenness instead and offers Freeman's as `BC = "standard"`. On the
#' weighted network a path's length is the sum of the reciprocal weights,
#' as in that program.
#'
#' Scores that are equal in exact arithmetic can differ in their last bits,
#' which would break ties at random. Every centrality is therefore rounded
#' to twelve significant digits before it is ranked.
#'
#' @param a Symmetric nonnegative weight matrix with a zero diagonal, of a
#'   connected network.
#' @return Numeric vector in `[0, 1]`.
#' @keywords internal
#' @noRd
.cg_hybrid <- function(a) {
  n <- nrow(a)
  b <- (a > 0) * 1
  lengths <- ifelse(a > 0, 1 / a, 0)
  position <- function(x) rank(signif(x, 12L), ties.method = "max")
  terms <- list(
    .cg_betweenness(lengths, n, FALSE), .cg_betweenness(b, n, FALSE),
    .cg_closeness(.cg_distances(lengths), n), .cg_closeness(.cg_distances(b), n),
    rowSums(b), rowSums(a),
    .cg_eigenvector(a, n), .cg_eigenvector(b, n))
  (Reduce(`+`, lapply(terms, position)) - 8) / (8 * (n - 1))
}
