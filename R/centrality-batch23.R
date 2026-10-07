#' Calculate adaptive LeaderRank using original graph H-indices
#' @keywords internal
#' @noRd
calculate_adaptive_leaderrank <- function(cg, h_mode = "all") {
  b <- .cg_path_matrix(cg, NULL)
  diag(b) <- 0
  .cg_adaptive_leaderrank(b, h_mode)
}

#' Adaptive LeaderRank
#'
#' Adaptive LeaderRank (Xu and Wang 2017) adds a ground node with H-index
#' 1 that is linked in both directions to every node, and weights each arc
#' from \eqn{j} to \eqn{i} by the H-index \eqn{h_i} of its target. The
#' H-index of a node is the largest \eqn{h} such that at least \eqn{h} of
#' its neighbors have degree at least \eqn{h}. The score is the stationary
#' resource of the random walk on the row-normalized weights, started with
#' one unit on every node and none on the ground.
#'
#' @details
#' The random walk keeps the direction of the arcs, and an undirected edge
#' counts as two opposite arcs. Edge weights, loops and parallel arcs are
#' ignored, and \code{mode} has no effect. H-indices are computed once on
#' the original network. With \code{alr_h_mode = "all"} they use the
#' simple undirected skeleton, \code{"out"} uses the out-degrees of
#' out-neighbors and \code{"in"} the in-degrees of in-neighbors. The
#' returned scores omit the ground, so they sum to less than \eqn{N}. A
#' node with H-index 0 scores 0, and when every H-index is 0 all scores are
#' \code{NaN} with a \code{cograph_undefined_measure} warning.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param alr_h_mode Neighbors and degrees used for the H-index:
#'   \code{"all"} (default), \code{"out"} or \code{"in"}. On an undirected
#'   network the three agree.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Xu, S., & Wang, P. (2017). Identifying important nodes by adaptive
#'   LeaderRank. Physica A, 469, 654-664. \doi{10.1016/j.physa.2016.11.034}.
#' @seealso \code{\link{centrality_weighted_leaderrank}},
#'   \code{\link{centrality_lobby}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_adaptive_leaderrank(regulation_net)
centrality_adaptive_leaderrank <- function(x, alr_h_mode = "all", ...) {
  df <- centrality(x, measures = "adaptive_leaderrank",
                   alr_h_mode = alr_h_mode, ...)
  stats::setNames(df$adaptive_leaderrank, df$node)
}
