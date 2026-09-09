#' Calculate improved closeness on the simple undirected skeleton
#' @keywords internal
#' @noRd
calculate_improved_closeness <- function(cg, alpha = 0.2) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_improved_closeness(b, alpha)
}

#' Improved closeness centrality
#'
#' Luan et al.'s improved closeness is
#' \eqn{ICC(i)=(n-1)/\sum_{j\ne i}d_{ij}/\sigma_{ij}^{\alpha}}, where
#' d is the hop distance and sigma counts shortest paths. Multiple shortest
#' paths reduce the effective distance to a partner. At alpha zero this
#' is ordinary normalized closeness on a connected graph; on a tree it is
#' independent of alpha because each pair has one shortest path. Scores
#' need not be bounded by one.
#'
#' Uses the simple undirected unweighted skeleton: either direction creates
#' an edge, parallel edges count once and self-loops are removed. Weights,
#' \code{mode} and path-weight inversion do not affect the result. These
#' are explicit cograph projections to the published domain.
#'
#' In a disconnected graph, every node has an unreachable partner and
#' therefore scores zero under the global infinite-distance convention.
#' Singletons score zero by an explicit cograph convention for the otherwise
#' undefined zero-over-zero expression. For within-component scores, supply
#' each component separately. Empty input returns an empty vector.
#'
#' Breadth-first traversal counts shortest paths in logarithmic form,
#' avoiding overflow when the number of paths exceeds double precision.
#' Extremely small effective-distance terms can underflow to zero, but
#' direct-neighbor terms remain one and keep the denominator positive.
#' Computation costs O(n times (n+m)) with an additional dense adjacency
#' representation. Default alpha 0.2 is a setting studied in the source,
#' not an estimate or a guarantee of optimal spreading predictions.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param icc_alpha Multiplicity exponent between zero and one, default 0.2.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive final scores are divided by their
#'   maximum. The published n-1 factor is present in raw scores already.
#' @return Named numeric vector in input node order.
#' @references
#' Luan, Y., Bao, Z., & Zhang, H. (2021). Identifying Influential Spreaders
#' in Complex Networks by Considering the Impact of the Number of Shortest
#' Paths. Journal of Systems Science and Complexity, 34, 2168-2181,
#' equation 7. \doi{10.1007/s11424-021-0111-7}.
#' @export
#' @examples
#' centrality_improved_closeness(igraph::make_ring(4), icc_alpha = 0.2)
centrality_improved_closeness <- function(x, icc_alpha = 0.2, ...) {
  df <- centrality(x, measures = "improved_closeness",
                   icc_alpha = icc_alpha, ...)
  stats::setNames(df$improved_closeness, df$node)
}
