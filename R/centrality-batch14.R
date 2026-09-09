#' Assemble a nonnegative adjacency, summing remaining parallel edges
#' @keywords internal
#' @noRd
.cg_candidate_adjacency <- function(cg, weights = NULL, measure) {
  w <- if (is.null(weights)) rep(1, nrow(cg$edges)) else weights
  if (any(!is.finite(w)) || any(w < 0)) {
    stop(measure, " requires finite nonnegative edge weights",
         call. = FALSE)
  }
  # Every canonical edge is a distinct cell, so placing the weights is the
  # same as accumulating them; the undirected mirror fills the lower triangle.
  .cg_path_matrix(cg, w)
}

#' Calculate finite-horizon diffusion
#' @keywords internal
#' @noRd
calculate_finite_diffusion <- function(cg, weights = NULL, q = 1, steps = 3) {
  a <- .cg_candidate_adjacency(cg, weights, "diffusion_centrality")
  .cg_finite_diffusion(a, q, steps)
}

#' Calculate exact relative spectral loss on vertex deletion
#' @keywords internal
#' @noRd
calculate_dynamical_importance <- function(cg, weights = NULL) {
  a <- .cg_candidate_adjacency(cg, weights, "dynamical_importance")
  diag(a) <- 0
  .cg_dynamical_importance(a)
}

#' Finite-horizon diffusion centrality
#'
#' Banerjee et al.'s diffusion centrality is
#' \eqn{DC(A;q,T) = \sum_{t=1}^{T}(qA)^t\mathbf{1}}.
#' It sums weighted walks starting at each node, allowing revisits and
#' returns to the source. Directed edges carry information from their source
#' to their target: the result uses row sums, regardless of \code{mode}.
#' Transpose the input adjacency matrix to measure incoming walks.
#'
#' A is the adjacency matrix with the original edge weights when
#' \code{weighted = TRUE}, or unit edge weights otherwise. Self-loops follow
#' \code{loops}; an undirected self-loop contributes its weight once on the
#' diagonal. The \code{simplify} argument combines parallel edges first;
#' any remaining parallel edges contribute additively to A. Weight inversion
#' for shortest paths does not affect this measure.
#'
#' When every entry of qA is between zero and one, scores have the paper's
#' interpretation as expected total hearings of information. Larger weights
#' are accepted as a mathematical weighted-walk extension of that polynomial,
#' without a probability interpretation. Scores count repeated hearings,
#' not distinct recipients. They need not be bounded by the number of nodes.
#'
#' Default q = 1 and T = 3 are explicit cograph choices, not estimates of
#' a diffusion process or the parameters used by the Zoo. T = 0 returns zero;
#' T = 1 gives q times outgoing strength (degree for a binary graph).
#' A finite horizon requires no spectral convergence condition. Numerical
#' overflow raises an error, including when normalization is requested.
#'
#' This is distinct from \code{\link{centrality_diffusion}}: its default
#' is diffusion degree, and its TNA variant fixes q = 1 and T = n.
#' The existing \code{lambda} and \code{diffusion_method} arguments do not
#' affect this measure. Computation uses T matrix-vector products.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param diffusion_q Finite multiplier between 0 and 1, default 1.
#' @param diffusion_steps Nonnegative integer horizon, default 3. Must be
#'   no larger than \code{.Machine$integer.max}.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive scores are divided by their maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Banerjee, A., Chandrasekhar, A. G., Duflo, E., & Jackson, M. O. (2013).
#' The Diffusion of Microfinance. Science, 341, 1236498, equation 5.
#' \doi{10.1126/science.1236498}.
#'
#' Banerjee, A., Chandrasekhar, A. G., Duflo, E., & Jackson, M. O. (2019).
#' Using Gossips to Spread Information: Theory and Evidence from Two
#' Randomized Controlled Trials. Review of Economic Studies, 86, 2453-2490,
#' section 3.1.2. \doi{10.1093/restud/rdz008}.
#' @export
#' @examples
#' g <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
#' centrality_diffusion_centrality(g, diffusion_q = 0.5, diffusion_steps = 2)
# nolint start: object_length_linter.
centrality_diffusion_centrality <- function(x, diffusion_q = 1,
                                            diffusion_steps = 3, ...) {
  df <- centrality(x, measures = "diffusion_centrality",
                   diffusion_q = diffusion_q, diffusion_steps = diffusion_steps,
                   ...)
  stats::setNames(df$diffusion_centrality, df$node)
}
# nolint end: object_length_linter.

#' Dynamical importance by exact vertex deletion
#'
#' Restrepo, Ott & Hunt's node dynamical importance is the relative drop
#' in adjacency spectral radius on removing that node:
#' \eqn{I_i = (\rho(A)-\rho(A_{-i}))/\rho(A)} (equation 2).
#' This function recomputes the spectral radius after every deletion. The
#' paper's left/right eigenvector product (equation 5) is an approximation
#' and can differ substantially on small networks; it is not used here.
#'
#' Supports directed or undirected nonnegative weighted networks. Self-loops
#' are always removed, as in the paper's zero-diagonal definition. Edge
#' weights, \code{weighted} and \code{simplify} follow the same adjacency
#' conventions as \code{\link{centrality_diffusion_centrality}}. The measure
#' is invariant to reversing all arcs and ignores \code{mode} and path-weight
#' inversion. Disconnected graphs use the spectral radius of the whole graph.
#'
#' When the original spectral radius is zero (including any directed acyclic
#' graph), the ratio is undefined and all vertices receive \code{NaN}.
#' Isolates in a graph with positive spectral radius receive zero. The empty
#' graph returns an empty vector. Strong components are evaluated separately
#' so acyclic parts contribute exactly zero, avoiding numerical eigenvalues
#' of nilpotent blocks. Roundoff in the final ratio is clipped to zero or one.
#'
#' Repeated eigendecomposition is costly. Select this measure explicitly or
#' use \code{include = "dynamical_importance"}; it is held back from the
#' default \code{type = "all"} tier.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}. The default
#'   \code{normalized = FALSE} preserves the published relative loss;
#'   \code{TRUE} additionally divides positive scores by their maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Restrepo, J. G., Ott, E., & Hunt, B. R. (2006). Characterizing the
#' Dynamical Importance of Network Nodes and Links. Physical Review Letters,
#' 97, 094102. \doi{10.1103/PhysRevLett.97.094102}.
#' @export
#' @examples
#' centrality_dynamical_importance(igraph::make_full_graph(4))
# nolint start: object_length_linter.
centrality_dynamical_importance <- function(x, ...) {
  df <- centrality(x, measures = "dynamical_importance", ...)
  stats::setNames(df$dynamical_importance, df$node)
}
# nolint end: object_length_linter.
