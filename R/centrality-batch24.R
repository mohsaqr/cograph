#' Assemble undirected weights for graph regularization
#' @keywords internal
#' @noRd
calculate_graph_regularization <- function(g, weights = NULL, gamma = 1) {
  if (is.null(weights)) {
    a <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  } else {
    a <- .cg_candidate_adjacency(g, weights, "graph_regularization")
    if (igraph::is_directed(g)) a <- a + t(a)
    if (any(!is.finite(a))) {
      stop("graph_regularization weight sum exceeds double precision",
           call. = FALSE)
    }
  }
  diag(a) <- 0
  .cg_graph_regularization(a, gamma)
}

#' Graph regularization centrality
#'
#' Dal Col and Petronetto's graph regularization centrality is
#' \eqn{GRC_i = 1/[(I+\gamma L)^{-1}]_{ii}}, where L is the unnormalized
#' weighted graph Laplacian. The ith column of this inverse minimizes
#' \eqn{\|s-e_i\|^2+\gamma s^T Ls}. A larger score indicates that smoothing
#' retains less of a unit impulse at its source vertex. This implements
#' the centrality with unit impulses; the author's separate signal option
#' returns smoothed signal values and is not this centrality.
#'
#' \code{grc_gamma} accepts any finite nonnegative number, default one.
#' At zero every score is one. Isolates also score one. Within a component
#' of n vertices scores lie between one and n, approaching n as gamma
#' grows without bound. Adding disconnected components does not change
#' existing raw scores. Edge weights and gamma act multiplicatively;
#' uniform weight scaling changes scores unless gamma is adjusted inversely.
#'
#' Uses finite nonnegative edge weights when \code{weighted = TRUE}.
#' Zero weights are absent connections. Unweighted inputs use the simple
#' undirected skeleton. Loops are removed. For weighted directed inputs,
#' opposite arcs are added. The generic \code{simplify} argument combines
#' parallel edges first; remaining weighted parallel edges are added.
#' These projections are explicit cograph conventions for the published
#' undirected domain. Generic \code{mode}, shortest-path weight inversion
#' and cutoff do not affect the result.
#'
#' The native dense spectral calculation separates each component's
#' constant eigenvector and evaluates the remaining filter in log space.
#' This supports extreme finite gamma and uniform weight scales without
#' forming their product. Unresolvable weight ranges or positive spectral
#' condition numbers above 1/(64 times machine epsilon) raise an error.
#' Runtime is O(n cubed) and memory
#' O(n squared) per component. Empty graphs return an empty vector.
#'
#' The author software approximates the same filter with ten Chebyshev
#' terms. This function evaluates the defining inverse to numerical
#' precision; default author-software values need not coincide. Optional
#' \code{normalized = TRUE} divides scores by their global maximum.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param grc_gamma Finite nonnegative regularization strength, default one.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Dal Col, A., & Petronetto, F. (2023). Graph regularization
#'   centrality. Physica A, 628, 129188. \doi{10.1016/j.physa.2023.129188}.
#'   Author implementation: GRC, Mendeley Data, version 1.
#'   \doi{10.17632/ns63f5dj86.1}.
#' @export
#' @examples
#' centrality_graph_regularization(igraph::make_ring(4), grc_gamma = 0.5)
# nolint start: object_length_linter.
centrality_graph_regularization <- function(x, grc_gamma = 1, ...) {
  df <- centrality(x, measures = "graph_regularization",
                   grc_gamma = grc_gamma, ...)
  stats::setNames(df$graph_regularization, df$node)
}
# nolint end: object_length_linter.
