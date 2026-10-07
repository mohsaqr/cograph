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

#' Diffusion Centrality
#'
#' Diffusion centrality (Banerjee et al. 2013) counts the walks of length
#' 1 to \eqn{T} that start at a node, each discounted by \eqn{q} per step:
#' \deqn{DC(A; q, T) = \sum_{t=1}^{T} (qA)^t \mathbf{1}.}{
#'   DC(A; q, T) = sum_{t=1}^{T} (qA)^t 1.}
#' Walks may revisit nodes and return to the source, so the score counts
#' repeated hearings of a message.
#'
#' @details
#' \eqn{A} holds the edge weights, or ones with \code{weighted = FALSE}.
#' Directed edges carry information from source to target, so the score
#' uses row sums and \code{mode} has no effect. Transposing the input
#' gives incoming walks. Self-loops follow \code{loops}. Negative or non-finite
#' weights raise an error. \eqn{T = 0} gives 0 and \eqn{T = 1} gives
#' \eqn{q} times the out-strength. When every entry of \eqn{qA} lies
#' between 0 and 1 the score is the expected number of times the
#' information is heard (Banerjee et al. 2013), and it can exceed the
#' number of nodes. The defaults \eqn{q = 1} and \eqn{T = 3} are package
#' choices. This measure differs from \code{\link{centrality_diffusion}}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param diffusion_q Discount \eqn{q}, between 0 and 1. Default 1.
#' @param diffusion_steps Horizon \eqn{T}, a nonnegative integer. Default 3.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#'   uses \code{weighted} (default \code{TRUE}), \code{loops} (default
#'   \code{TRUE}) and \code{normalized} (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Banerjee, A., Chandrasekhar, A. G., Duflo, E., & Jackson, M. O. (2013).
#'   The Diffusion of Microfinance. Science, 341, 1236498.
#'   \doi{10.1126/science.1236498}.
#'
#' Banerjee, A., Chandrasekhar, A. G., Duflo, E., & Jackson, M. O. (2019).
#'   Using Gossips to Spread Information: Theory and Evidence from Two
#'   Randomized Controlled Trials. Review of Economic Studies, 86, 2453-2490.
#'   \doi{10.1093/restud/rdz008}.
#' @seealso \code{\link{centrality_dynamics_sensitive}},
#'   \code{\link{centrality_diffusion}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_diffusion_centrality(regulation_net)
# nolint start: object_length_linter.
centrality_diffusion_centrality <- function(x, diffusion_q = 1,
                                            diffusion_steps = 3, ...) {
  df <- centrality(x, measures = "diffusion_centrality",
                   diffusion_q = diffusion_q, diffusion_steps = diffusion_steps,
                   ...)
  stats::setNames(df$diffusion_centrality, df$node)
}
# nolint end: object_length_linter.

#' Dynamical Importance
#'
#' Dynamical importance (Restrepo et al. 2006) is the relative drop in the
#' spectral radius \eqn{\rho}{rho} of the adjacency matrix when the node is
#' removed:
#' \deqn{I_i = \frac{\rho(A) - \rho(A_{-i})}{\rho(A)}.}{
#'   I_i = (rho(A) - rho(A_-i)) / rho(A).}
#' The spectral radius is recomputed after each deletion, in place of the
#' eigenvector approximation of the paper (eq. 5).
#'
#' @details
#' \eqn{A} holds the edge weights, or ones with \code{weighted = FALSE}, and
#' self-loops are removed. Negative or non-finite weights raise an error.
#' The scores lie between 0 and 1 and are unchanged when every arc is
#' reversed, so \code{mode} has no effect. A network with spectral radius
#' 0, such as any directed acyclic network, gives \code{NaN} for every
#' node without a warning. An isolated node in a network with positive
#' spectral radius scores 0.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure
#'   uses \code{weighted} (default \code{TRUE}) and \code{normalized}
#'   (default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Restrepo, J. G., Ott, E., & Hunt, B. R. (2006). Characterizing the
#' Dynamical Importance of Network Nodes and Links. Physical Review Letters,
#' 97, 094102. \doi{10.1103/PhysRevLett.97.094102}.
#' @seealso \code{\link{centrality_eigenvector}},
#'   \code{\link{centrality_resistance_curvature}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dynamical_importance(regulation_net)
# nolint start: object_length_linter.
centrality_dynamical_importance <- function(x, ...) {
  df <- centrality(x, measures = "dynamical_importance", ...)
  stats::setNames(df$dynamical_importance, df$node)
}
# nolint end: object_length_linter.
