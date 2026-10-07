#' Assemble undirected conductances for resistance curvature
#' @keywords internal
#' @noRd
calculate_resistance_curvature <- function(cg, weights = NULL) {
  if (is.null(weights)) {
    a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  } else {
    a <- .cg_candidate_adjacency(cg, weights, "resistance_curvature")
    if (cg$directed) a <- a + t(a)
    if (any(!is.finite(a))) {
      stop("resistance_curvature conductance sum exceeds double precision",
           call. = FALSE)
    }
  }
  diag(a) <- 0
  .cg_resistance_curvature(a)
}

#' Resistance Curvature
#'
#' The node resistance curvature of Devriendt and Lambiotte (2022) is
#' \deqn{p_i = 1 - \frac{1}{2} \sum_{j \sim i} w_{ij} R_{ij},}{
#'   p_i = 1 - (1/2) sum_{j ~ i} w_ij R_ij,}
#' where the weights \eqn{w_{ij}}{w_ij} are conductances and
#' \eqn{R_{ij}}{R_ij} is the effective resistance. It equals one minus half
#' the expected degree of the node in a random spanning tree of its
#' component.
#'
#' @details
#' Edge weights are conductances, and \code{weighted = FALSE} uses the
#' simple undirected skeleton. On a weighted directed network the two arcs
#' between a pair are added. Self-loops are removed, \code{mode} has no
#' effect, and negative or non-finite weights raise an error. Scores can be
#' negative at tree-like junctions. On a tree the score is one minus half
#' the degree, on an unweighted cycle or complete graph of \eqn{n} nodes it
#' is \eqn{1/n}, and an isolated node scores 1. The raw scores sum to the
#' number of components.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#'   uses \code{weighted} (default \code{TRUE}) and \code{normalized}
#'   (default \code{FALSE}), which divides by the largest score and leaves
#'   negative scores negative.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Devriendt, K., & Lambiotte, R. (2022). Discrete curvature on graphs from
#'   the effective resistance. Journal of Physics: Complexity, 3, 025008.
#'   \doi{10.1088/2632-072X/ac730d}.
#' @seealso \code{\link{centrality_current_flow_closeness}},
#'   \code{\link{centrality_dynamical_importance}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_resistance_curvature(regulation_net)
# nolint start: object_length_linter.
centrality_resistance_curvature <- function(x, ...) {
  df <- centrality(x, measures = "resistance_curvature", ...)
  stats::setNames(df$resistance_curvature, df$node)
}
# nolint end: object_length_linter.
