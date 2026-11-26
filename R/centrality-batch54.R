#' X-nonbacktracking centrality on the simple undirected skeleton
#' @keywords internal
#' @noRd
calculate_x_nonbacktracking <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  terms <- .cg_x_nonbacktracking(b)
  undefined <- sum(terms$status != "ok")
  if (undefined > 0L) {
    .cg_warn_undefined(
      "`x_nonbacktracking` has no value at ", undefined, " of ", nrow(terms),
      " nodes, which are NA: after such a node is removed the ",
      "nonbacktracking matrix has no simple largest eigenvalue (the rest ",
      "of the network is a forest, its cyclic parts are single cycles, or ",
      "two parts tie).")
  }
  terms$value
}

#' X-Nonbacktracking Centrality
#'
#' X-nonbacktracking centrality (Torres et al. 2021) estimates how much the
#' largest eigenvalue of the nonbacktracking matrix of a network falls when
#' a node is removed. For a node \eqn{c},
#' \deqn{\alpha_c = \Bigl(\sum_i a_{ci} v^i\Bigr)^2 - \sum_i a_{ci} (v^i)^2,}{
#'   alpha_c = (sum_i a_ci v^i)^2 - sum_i a_ci (v^i)^2,}
#' where \eqn{v^i = \sum_j a_{ij} v_{j \to i}}{v^i = sum_j a_ij v_(j->i)} is
#' the nonbacktracking centrality of node \eqn{i} in the network without
#' \eqn{c}, and \eqn{v} is the principal eigenvector of the nonbacktracking
#' matrix of that network, scaled so that
#' \eqn{v^{\top} P v = 1}{v' P v = 1} with \eqn{P} the reversal of every
#' directed edge.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' The eigenvector is computed again for every node, on the network without
#' that node, and its scaling differs from node to node. The score is the
#' exact quantity of Proposition 3.7 of the source, and differs from the
#' approximation of its section 4, which uses the eigenvector of the whole
#' network. A larger score means a larger fall of the eigenvalue. A node of
#' degree one scores 0 when the score is defined.
#'
#' The score is \code{NA} at a node when the network without it has no
#' principal nonbacktracking eigenvector. This happens when that network is
#' a forest, when each of its cyclic parts is a single cycle, or when two
#' separate parts share the largest eigenvalue. A
#' \code{cograph_undefined_measure} warning reports how many nodes are
#' affected. The source does not discuss these cases.
#'
#' The measure solves one eigenproblem on the directed edges per node, so it
#' is costly and is computed only when requested by name or through
#' \code{include}. \code{normalized = TRUE} divides the scores by their
#' maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, with \code{NA} where the score is undefined.
#' @references
#' Torres, L., Chan, K. S., Tong, H. and Eliassi-Rad, T. (2021).
#'   Nonbacktracking eigenvalues under node removal: X-centrality and
#'   targeted immunization. SIAM Journal on Mathematics of Data Science,
#'   3(2), 656-675. \doi{10.1137/20M1352132}.
#' @seealso \code{\link{centrality_x_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_x_nonbacktracking(regulation_net)
centrality_x_nonbacktracking <- function(x, ...) {
  df <- centrality(x, measures = "x_nonbacktracking", ...)
  stats::setNames(df$x_nonbacktracking, df$node)
}
