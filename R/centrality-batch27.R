#' X-degree on the simple undirected skeleton
#' @keywords internal
#' @noRd
calculate_x_degree <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  neighbors <- .cg_adjlist(b, directed = FALSE)
  excess <- as.numeric(lengths(neighbors)) - 1
  vapply(neighbors, function(nodes) {
    z <- excess[nodes]
    if (length(z) < 2L) return(0)
    # Twice the sum over unordered pairs, without subtracting large squares.
    2 * sum(z[-1L] * head(cumsum(z), -1L))
  }, numeric(1))
}

#' X-Degree Centrality
#'
#' X-degree (Torres et al. 2021, equation 3.15) counts the oriented
#' nonbacktracking walks of four edges whose middle node is \eqn{i}{i}. It
#' depends only on the degrees \eqn{d_j}{d_j} of the neighbors of
#' \eqn{i}{i}:
#' \deqn{Xdeg(i) = \Big(\sum_{j \in N(i)} (d_j - 1)\Big)^2
#'   - \sum_{j \in N(i)} (d_j - 1)^2.}{
#'   Xdeg(i) = (sum_{j in N(i)} (d_j - 1))^2 - sum_{j in N(i)} (d_j - 1)^2.}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored.
#' Isolated nodes and leaves score zero, and every node of a star scores
#' zero. Components are scored independently. The function computes the
#' score on the supplied network. The iterative node-removal immunization
#' procedure of the paper is a separate algorithm.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Torres, L., Chan, K. S., Tong, H., & Eliassi-Rad, T. (2021).
#'   Nonbacktracking Eigenvalues under Node Removal: X-Centrality and Targeted
#'   Immunization. SIAM Journal on Mathematics of Data Science, 3(2), 656-675.
#'   \doi{10.1137/20M1352132}.
#' @seealso \code{\link{centrality_degree}},
#'   \code{\link{centrality_dynamical_importance}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_x_degree(regulation_net)
centrality_x_degree <- function(x, ...) {
  df <- centrality(x, measures = "x_degree", ...)
  stats::setNames(df$x_degree, df$node)
}
