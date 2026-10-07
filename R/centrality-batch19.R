#' Calculate improved closeness on the simple undirected skeleton
#' @keywords internal
#' @noRd
calculate_improved_closeness <- function(cg, alpha = 0.2) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_improved_closeness(b, alpha)
}

#' Improved Closeness Centrality
#'
#' Improved closeness (Luan et al. 2021) divides each hop distance by a
#' power of the number of shortest paths, so that a partner reached along
#' many shortest paths counts as closer:
#' \deqn{ICC(i) = \frac{n-1}{\sum_{j \ne i} d_{ij} / \sigma_{ij}^{\alpha}}.}{
#'   ICC(i) = (n - 1) / sum_{j != i} d_ij / sigma_ij^alpha.}
#' Here \eqn{d_{ij}}{d_ij} is the hop distance and \eqn{\sigma_{ij}}{sigma_ij}
#' the number of shortest paths between \eqn{i} and \eqn{j}.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored, and \code{mode} has no effect.
#' With \eqn{\alpha = 0}{alpha = 0} the score on a connected network is the
#' normalized closeness,
#' and on a tree it does not depend on \eqn{\alpha}{alpha}. Scores can
#' exceed 1. On a disconnected network every node scores 0, because each
#' node has an unreachable partner at infinite distance. An isolated node
#' also scores 0. A value of \code{icc_alpha} outside 0 to 1 raises an
#' error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param icc_alpha Exponent \eqn{\alpha}{alpha} of the number of shortest
#'   paths, between 0 and 1. Default 0.2, one of the values studied by Luan
#'   et al. (2021).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Luan, Y., Bao, Z., & Zhang, H. (2021). Identifying Influential Spreaders
#'   in Complex Networks by Considering the Impact of the Number of Shortest
#'   Paths. Journal of Systems Science and Complexity, 34, 2168-2181.
#'   \doi{10.1007/s11424-021-0111-7}.
#' @seealso \code{\link{centrality_closeness}},
#'   \code{\link{centrality_harmonic}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_improved_closeness(regulation_net)
centrality_improved_closeness <- function(x, icc_alpha = 0.2, ...) {
  df <- centrality(x, measures = "improved_closeness",
                   icc_alpha = icc_alpha, ...)
  stats::setNames(df$improved_closeness, df$node)
}
