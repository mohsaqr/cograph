#' Calculate weighted LeaderRank on simple directed topology
#' @keywords internal
#' @noRd
calculate_weighted_leaderrank <- function(cg, alpha = 1) {
  b <- .cg_path_matrix(cg, NULL)
  diag(b) <- 0
  .cg_weighted_leaderrank(b, alpha)
}

#' Weighted LeaderRank
#'
#' Weighted LeaderRank (Li et al. 2014) adds a ground node \eqn{g} that is
#' linked in both directions to every node. Each original arc and each arc
#' into the ground has weight 1, and the arc from the ground to node
#' \eqn{i} has weight \eqn{(k_i^{in})^{\alpha}}{(k_i^in)^alpha}, where
#' \eqn{k_i^{in}}{k_i^in} is the in-degree before the ground is added. The
#' score is the stationary resource of the random walk on the row-normalized
#' weights, started with one unit on every node and the ground included.
#'
#' @details
#' Arcs keep their direction, and an undirected edge counts as two opposite
#' arcs. Edge weights, loops and parallel arcs are ignored, and \code{mode}
#' has no effect. The returned scores omit the ground, so they sum to less
#' than \eqn{N+1}{N + 1}. With \eqn{\alpha = 0}{alpha = 0} every ground arc
#' has weight 1. With a positive \eqn{\alpha}{alpha} a node with in-degree
#' 0 scores 0, and when every in-degree is 0 all scores are \code{NaN}
#' without a warning. A negative \eqn{\alpha}{alpha} requires a positive
#' in-degree at every node and raises an error otherwise.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param wlr_alpha Exponent \eqn{\alpha}{alpha} of the in-degree, a finite
#'   number. Default 1, one of the values studied by Li et al. (2014).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Li, Q., Zhou, T., Lu, L., & Chen, D. (2014). Identifying influential
#'   spreaders by weighted LeaderRank. Physica A, 404, 47-55.
#'   \doi{10.1016/j.physa.2014.02.041}.
#' @seealso \code{\link{centrality_leaderrank}},
#'   \code{\link{centrality_adaptive_leaderrank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_weighted_leaderrank(regulation_net)
centrality_weighted_leaderrank <- function(x, wlr_alpha = 1, ...) {
  df <- centrality(x, measures = "weighted_leaderrank", wlr_alpha = wlr_alpha,
                   ...)
  stats::setNames(df$weighted_leaderrank, df$node)
}
