#' Local neighbor contribution (LNC)
#' @keywords internal
#' @noRd
calculate_lnc <- function(cg) {
  .cg_lnc_terms(.cg_path_matrix(cg, NULL))$lnc
}

#' Local Neighbor Contribution Centrality
#'
#' Local neighbor contribution (Dai et al. 2019) multiplies a node's own
#' contribution \eqn{d_i (1 - 1/d_i)^{d_i - 1}}{d_i (1 - 1/d_i)^(d_i - 1)} by
#' its neighbor contribution, the squared degree times the neighbors' degree
#' sum divided by \eqn{n - 1}.
#' \deqn{LNC_i = d_i^3 \left(1 - \frac{1}{d_i}\right)^{d_i - 1}
#'   \frac{\sum_{j \in N(i)} d_j}{n - 1}}{
#'   LNC_i = d_i^3 (1 - 1/d_i)^(d_i - 1) sum_{j in N(i)} d_j / (n - 1)}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the network,
#' so direction, weights, loops and parallel edges are ignored, and it takes
#' no parameters. The count \eqn{n} is the number of nodes in the whole
#' network, so adding a disconnected component rescales every score and
#' leaves the ranking unchanged. Isolates and the node of a single-node graph
#' score zero. The factorization above is the one that reproduces the
#' source's printed intermediates and Table 1. The Centrality Zoo (section
#' 2.238) replaces the degree by the two-hop neighborhood size and does not
#' reproduce the source's values.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Dai, J., Wang, B., Sheng, J., Sun, Z., Khawaja, F. R., Ullah, A., Dejene,
#'   D. A. and Duan, G. (2019). Identifying influential nodes in complex
#'   networks based on local neighbor contribution. IEEE Access, 7,
#'   131719-131731. \doi{10.1109/ACCESS.2019.2939804}.
#' @seealso \code{\link{centrality_semilocal}},
#'   \code{\link{centrality_ked}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_lnc(regulation_net)
centrality_lnc <- function(x, ...) {
  df <- centrality(x, measures = "lnc", ...)
  stats::setNames(df$lnc, df$node)
}
