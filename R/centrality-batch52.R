#' Improved neighbors' k-core (Lin, Guo, Dong, Tang and Liu 2014)
#' @keywords internal
#' @noRd
calculate_ink <- function(cg, alpha = 1) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_ink(b, .cg_mdd(b, lambda = 0), alpha)
}

#' Improved Neighbors' k-core
#'
#' The improved neighbors' k-core (INK; Lin et al. 2014) sums a power of
#' the core number of every neighbor of a node:
#' \deqn{S(i) = \sum_{j \in J(i)} k_s(j)^{\alpha},}{
#'   S(i) = sum_{j in J(i)} k_s(j)^alpha,}
#' where \eqn{k_s(j)}{k_s(j)} is the core number of \eqn{j} in the whole
#' network and \eqn{J(i)} is the set of neighbors of \eqn{i}. It separates
#' nodes that share the same core number by the cores their neighbors
#' belong to.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The node's own core number is not part of its sum. Scores are raw sums.
#' Isolated nodes score 0, the empty sum.
#'
#' With \code{ink_alpha = 0} the score is the degree. With
#' \code{ink_alpha = 1}, the default, it is the sum of the neighbors' core
#' numbers, which is the neighborhood coreness of Bae and Kim (2014). The
#' default is the value Lin et al. use in their experiments. They report
#' that the best-performing exponent depends on the network (0.7, 1, 2.4
#' and 5 on their four networks). Values above one give more weight to
#' neighbors in high cores, and values below one give less.
#'
#' A negative \code{ink_alpha}, which the source does not define, raises a
#' \code{cograph_bad_parameter} error, and so does an exponent so large
#' that a core number raised to it is not a finite number.
#' \code{normalized = TRUE} divides the scores by their maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{ink_alpha} (the exponent \eqn{\alpha}{alpha}, default 1).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Lin, J.-H., Guo, Q., Dong, W.-Z., Tang, L.-Y. and Liu, J.-G. (2014).
#'   Identifying the node spreading influence with largest k-core values.
#'   Physics Letters A, 378(45), 3279-3284.
#'   \doi{10.1016/j.physleta.2014.09.054}.
#'
#' Bae, J. and Kim, S. (2014). Identifying and ranking influential spreaders
#'   in complex networks by neighborhood coreness. Physica A: Statistical
#'   Mechanics and its Applications, 395, 549-559.
#'   \doi{10.1016/j.physa.2013.10.047}.
#' @seealso \code{\link{centrality_extended_coreness}},
#'   \code{\link{centrality_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ink(regulation_net)
#' centrality_ink(regulation_net, ink_alpha = 2)
centrality_ink <- function(x, ...) {
  df <- centrality(x, measures = "ink", ...)
  stats::setNames(df$ink, df$node)
}
