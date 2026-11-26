#' Calculate hybrid centrality
#' @keywords internal
#' @noRd
calculate_hybrid <- function(cg, weights = NULL) {
  a <- .cg_candidate_adjacency(cg, weights, "hybrid")
  diag(a) <- 0
  n <- nrow(a)
  if (n == 0L) return(numeric(0))
  reason <- if (n < 2L) {
    "the network has one node"
  } else if (!isSymmetric(unname(a))) {
    "the source defines the measure for undirected networks"
  } else if (.cg_n_components((a > 0) * 1) > 1L) {
    "closeness has no value on a disconnected network"
  }
  if (!is.null(reason)) {
    .cg_warn_undefined("`hybrid` has no value on this input, so its column ",
                       "is NA: ", reason, ".")
    return(rep(NA_real_, n))
  }
  .cg_hybrid(a)
}

#' Hybrid Centrality
#'
#' Hybrid centrality (Christensen et al. 2018) combines eight rankings of
#' the nodes into one score:
#' \deqn{HC = \frac{BC^w + BC^u + LC^w + LC^u + k^u + EC^w + EC^u + NS^w - 8}
#'   {8 (N - 1)},}{
#'   HC = (BC^w + BC^u + LC^w + LC^u + k^u + EC^w + EC^u + NS^w - 8) /
#'   (8 (N - 1)),}
#' where each term is the rank of the node on one centrality: betweenness
#' (\eqn{BC}), closeness (\eqn{LC}) and eigenvector centrality (\eqn{EC}),
#' each on the weighted (\eqn{w}) and on the unweighted (\eqn{u}) network,
#' the degree (\eqn{k}) and the strength (\eqn{NS}).
#'
#' @details
#' The most central node holds the largest rank, and tied nodes share the
#' largest rank of their group. The score runs from 0, for a node last on
#' every centrality, to 1, for a node first on every one. Weights are read
#' as strengths of association, and the length of a path on the weighted
#' network is the sum of the reciprocal weights. On an unweighted network
#' the weighted and unweighted rankings coincide. Loops are ignored.
#'
#' Betweenness is Freeman's shortest-path betweenness, as the source
#' states. The function \code{hybrid()} of the NetworkToolbox package, by
#' the first author, uses randomized shortest paths betweenness unless
#' told otherwise, and rounds every centrality to three decimals before
#' ranking, so its values can differ. Here every centrality is rounded to
#' twelve significant digits before ranking, so that scores equal in exact
#' arithmetic are tied.
#'
#' The measure is defined for connected undirected networks of at least
#' two nodes. Otherwise every score is \code{NA} with a
#' \code{cograph_undefined_measure} warning.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{weighted}.
#' @return A named numeric vector with one score per node, in input node
#'   order, between 0 and 1, or \code{NA} at every node when the score is
#'   undefined.
#' @references
#' Christensen, A. P., Kenett, Y. N., Aste, T., Silvia, P. J. and Kwapil,
#'   T. R. (2018). Network structure of the Wisconsin Schizotypy Scales-Short
#'   Forms: examining psychometric network filtering approaches. Behavior
#'   Research Methods, 50(6), 2531-2550. \doi{10.3758/s13428-018-1032-9}.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_closeness}},
#'   \code{\link{centrality_eigenvector}}, \code{\link{centrality}}.
#' @export
#' @examples
#' ring <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
#' ring[cbind(1:5, c(2:5, 1))] <- c(0.9, 0.2, 0.5, 0.7, 0.4)
#' ring <- ring + t(ring)
#' centrality_hybrid(ring)
centrality_hybrid <- function(x, ...) {
  df <- centrality(x, measures = "hybrid", ...)
  stats::setNames(df$hybrid, df$node)
}
