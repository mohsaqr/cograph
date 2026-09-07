#' Calculate weighted LeaderRank on simple directed topology
#' @keywords internal
#' @noRd
calculate_weighted_leaderrank <- function(g, alpha = 1) {
  b <- .cg_path_matrix(g, NULL)
  diag(b) <- 0
  .cg_weighted_leaderrank(b, alpha)
}

#' Weighted LeaderRank centrality
#'
#' Li et al.'s weighted LeaderRank adds a ground node g. Each original
#' directed edge and each edge from an original node to g has weight one.
#' The edge from g to node i has weight \eqn{(k_i^{in})^{\alpha}}, using
#' original in-degree before ground edges are added. Scores follow the
#' stationary distribution of the row-normalized augmented matrix.
#'
#' Raw scores retain total mass N+1 across the augmented graph, following
#' the all-nodes-one initialization in the original paper, section 2.
#' The ground score is omitted from the returned vector without redistribution.
#' The Zoo instead initializes the ground at zero, yielding raw scores
#' smaller by N/(N+1); final max-normalized scores agree. The existing
#' \code{\link{centrality_leaderrank}} uses a different redistribution/scale
#' convention, so raw equality at alpha zero is not asserted.
#'
#' Directed arcs are retained; an undirected edge is treated as two opposite
#' arcs, an explicit extension. Input weights are ignored: weighted refers
#' to the algorithm's ground-edge weights. Loops are removed and parallel
#' arcs count once. Mode, path inversion and cutoff do not change the result.
#'
#' Alpha can be any finite number. Negative values require strictly positive
#' original in-degree at every node. At alpha zero all ground-edge weights
#' are one, including for zero-in-degree nodes. With positive alpha, these
#' nodes receive no ground resource and have zero stationary score; if every
#' in-degree is zero the ground row is undefined and all scores are NaN.
#' Empty input returns an empty vector. These boundary conventions are
#' explicit; no pseudocount is added to the published in-degree weights.
#'
#' A native linear solve eliminates the ground variable and obtains the
#' unique stationary distribution even when ordinary iteration is periodic.
#' This uses O(N^3) time and O(N^2) memory. Ground transition probabilities
#' are calculated with shifted logarithms, avoiding overflow for large
#' exponents; extremely small probabilities may underflow to zero.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param wlr_alpha Finite in-degree exponent, default one, a setting studied
#'   in the source rather than a universal optimum.
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides final scores by their maximum.
#' @return Named numeric vector in input node order.
#' @references Li, Q., Zhou, T., Lu, L., & Chen, D. (2014). Identifying
#'   influential spreaders by weighted LeaderRank. Physica A, 404, 47-55,
#'   section 2, equations 1-2. \doi{10.1016/j.physa.2014.02.041}.
#'   Author preprint: \url{https://arxiv.org/abs/1306.5042}.
#' @export
#' @examples
#' centrality_weighted_leaderrank(igraph::make_ring(4, directed = TRUE))
centrality_weighted_leaderrank <- function(x, wlr_alpha = 1, ...) {
  df <- centrality(x, measures = "weighted_leaderrank", wlr_alpha = wlr_alpha,
                   ...)
  stats::setNames(df$weighted_leaderrank, df$node)
}
