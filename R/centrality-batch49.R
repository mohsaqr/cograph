#' Immediate effects centrality (Friedkin 1991)
#' @keywords internal
#' @noRd
calculate_iec <- function(cg) {
  terms <- .cg_iec_terms(.cg_path_matrix(cg, NULL))
  status <- attr(terms, "status")
  message <- switch(
    status,
    singleton = paste("`iec` has no value on a one-node graph, so its column",
                      "is NA: equation (20) divides by n - 1, which is zero."),
    reducible = paste("`iec` has no value on this input, so its column is NA:",
                      "with the unit self-loops the source mandates, the",
                      "influence chain is still reducible, so the left",
                      "eigenvector of equation (9) is not determined and the",
                      "mean first passage times of equation (11) are infinite",
                      "between classes. Restrict the input to a connected",
                      "undirected graph or a strongly connected directed one."),
    NULL
  )
  if (!is.null(message)) {
    condition <- warningCondition(message,
                                  class = "cograph_undefined_measure",
                                  call = NULL)
    warning(condition)
  }
  terms$iec
}

#' Immediate Effects Centrality
#'
#' Immediate effects centrality (Friedkin 1991) is the reciprocal of the mean
#' length of the influence sequences that end at a node. The influence matrix
#' \eqn{W} is the adjacency matrix with a unit diagonal, divided by its row
#' sums. With \eqn{c} its stationary vector,
#' \eqn{Z = (I - W + \mathbf{1}c')^{-1}}{Z = (I - W + 1 c')^-1} and the mean
#' first passage times
#' \eqn{M = (I - Z + E Z_{dg})\,\mathrm{diag}(1/c)}{
#'   M = (I - Z + E Z_dg) diag(1/c)}:
#' \deqn{IEC_j = \frac{n - 1}{\sum_{i \ne j} m_{ij}}}{
#'   IEC_j = (n - 1) / sum_{i != j} m_ij}
#'
#' @details
#' Direction is used, and weights, loops and parallel edges are ignored, so
#' the unit diagonal is calibrated against unit edges. \code{mode} has no
#' effect. The measure needs an irreducible chain, that is a connected
#' undirected or a strongly connected directed network. Otherwise, and on a
#' single node, every score is \code{NA} with a
#' \code{cograph_undefined_measure} warning. The measure differs from
#' \code{\link{centrality_markov}}, which omits the unit diagonal and divides
#' by \eqn{n}, and the two can rank nodes differently. The measure is costly
#' and is computed only when requested by name or through \code{include}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order, or \code{NA} at every node when the influence chain is reducible
#'   or the network has one node.
#' @references
#' Friedkin, N. E. (1991). Theoretical foundations for centrality measures.
#'   American Journal of Sociology, 96(6), 1478-1504. \doi{10.1086/229694}.
#' @seealso \code{\link{centrality_markov}},
#'   \code{\link{centrality_random_walk}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_iec(regulation_net)
centrality_iec <- function(x, ...) {
  df <- centrality(x, measures = "iec", ...)
  stats::setNames(df$iec, df$node)
}
