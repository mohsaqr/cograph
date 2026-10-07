#' Degree and importance of lines (Liu, Xiong, Shi, Shi and Wang 2016)
#' @keywords internal
#' @noRd
calculate_dil <- function(cg) {
  .cg_dil_terms(.cg_path_matrix(cg, NULL))$dil
}

#' Degree and Importance of Lines
#'
#' The degree and importance of lines (DIL; Liu et al. 2016) adds to the
#' degree of a node a share of the importance of every line incident to it.
#' A line \eqn{e_{ij}}{e_ij} that lies on \eqn{p} triangles has importance
#' \eqn{I_{ij} = (k_i - p - 1)(k_j - p - 1) / \lambda}{
#'   I_ij = (k_i - p - 1)(k_j - p - 1) / lambda}
#' with \eqn{\lambda = p/2 + 1}{lambda = p/2 + 1}, and this importance is
#' split between the endpoints in proportion to their degrees minus one:
#' \deqn{L_i = k_i + \sum_{j \in \Gamma_i} I_{ij}
#'   \frac{k_i - 1}{k_i + k_j - 2}.}{
#'   L_i = k_i + sum_{j in N(i)} I_ij (k_i - 1) / (k_i + k_j - 2).}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Every score is at least the degree of the node, and isolated nodes score
#' 0. The share is undefined only on an isolated edge, whose importance is
#' zero. That share is set to zero, so both endpoints score 1. The text
#' layer of the published article reads \eqn{\lambda}{lambda} as
#' \eqn{2p + 1}. The printed page and the worked example of the article give
#' \eqn{p/2 + 1}, which is the value used here. \code{normalized = TRUE}
#' divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, J., Xiong, Q., Shi, W., Shi, X. and Wang, K. (2016). Evaluating the
#'   importance of nodes in complex networks. Physica A: Statistical Mechanics
#'   and its Applications, 452, 209-219. \doi{10.1016/j.physa.2016.02.049}.
#' @seealso \code{\link{centrality_lhc}}, \code{\link{centrality_hcc}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dil(regulation_net)
centrality_dil <- function(x, ...) {
  df <- centrality(x, measures = "dil", ...)
  stats::setNames(df$dil, df$node)
}
