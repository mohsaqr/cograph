#' Calculate dynamics-sensitive centrality on the simple skeleton
#' @keywords internal
#' @noRd
calculate_dynamics_sensitive <- function(cg, beta = 0.1, mu = 1, steps = 5) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_dynamics_sensitive(b, beta, mu, steps)
}

#' Calculate Malatya centrality on the simple skeleton
#' @keywords internal
#' @noRd
calculate_malatya <- function(cg) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  .cg_malatya(b)
}

#' Dynamics-sensitive centrality
#'
#' Liu et al.'s finite-time dynamics-sensitive (DS) centrality is
#' \eqn{S(T)=\sum_{r=0}^{T-1}\beta A[\beta A+(1-\mu)I]^r\mathbf{1}},
#' where beta is the spreading rate and mu the recovery rate (equation 5
#' in the preprint). This is the full recovery-parameter family. For mu=1,
#' it reduces to \eqn{\sum_{t=1}^{T}(\beta A)^t\mathbf{1}} (equation 7),
#' also the form listed in the Centrality Zoo. For mu=0 it gives the
#' paper's susceptible-infected case.
#'
#' Uses the simple undirected unweighted skeleton, as in the source: either
#' direction creates an edge, parallel edges count once and loops are removed.
#' The projection of other inputs is an explicit cograph convention.
#' \code{mode}, edge weights and shortest-path weight inversion do not affect
#' this measure. Isolates score zero. T=0 or beta=0 returns zero; T=1 gives
#' beta times degree. The initial seed itself is not added to the score.
#'
#' This linearized cumulative spreading score allows repeated walks and can
#' exceed the number of nodes. It is not a bounded infection probability or
#' an exact simulation of the nonlinear SIR/SI process. Defaults beta=0.1,
#' mu=1 and T=5 select a parameter setting studied in the paper; they are
#' not fitted to the input network. Any finite horizon is supported without
#' a spectral convergence condition, subject to numerical precision. Overflow
#' raises an error, even if normalization is requested.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ds_beta Finite spreading rate between 0 and 1, default 0.1.
#' @param ds_mu Finite recovery rate between 0 and 1, default 1.
#' @param ds_steps Nonnegative integer horizon, default 5. Must not exceed
#'   \code{.Machine$integer.max}.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive scores are divided by their maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Liu, J. G., Lin, J. H., Guo, Q., & Zhou, T. (2016). Locating influential
#' nodes via dynamics-sensitive centrality. Scientific Reports, 6, 21380.
#' \doi{10.1038/srep21380}. Preprint equations 5 and 7:
#' \url{https://arxiv.org/abs/1504.06672}.
#' @seealso \code{\link{centrality_diffusion_centrality}}.
#' @export
#' @examples
#' g <- igraph::make_ring(5)
#' centrality_dynamics_sensitive(g, ds_beta = 0.1, ds_mu = 1, ds_steps = 5)
#' centrality_dynamics_sensitive(g, ds_mu = 0)
# nolint start: object_length_linter.
centrality_dynamics_sensitive <- function(x, ds_beta = 0.1, ds_mu = 1,
                                          ds_steps = 5, ...) {
  df <- centrality(x, measures = "dynamics_sensitive", ds_beta = ds_beta,
                   ds_mu = ds_mu, ds_steps = ds_steps, ...)
  stats::setNames(df$dynamics_sensitive, df$node)
}
# nolint end: object_length_linter.

#' Malatya centrality
#'
#' The static Malatya score of a node is the sum of its degree divided by
#' each neighbour's degree: \eqn{M(i)=\sum_{j\in N(i)}d_i/d_j}.
#' Computes the score on the original graph. On nonisolated vertices it is
#' exactly the reciprocal of \code{\link{centrality_bridging_coefficient}};
#' this relationship follows from their definitions, not rank correlation.
#'
#' Uses the simple undirected unweighted skeleton: either direction creates
#' an edge, parallel edges count once and self-loops are removed. This is an
#' explicit projection of other inputs to the source's domain. The empty
#' neighbour sum assigns isolates zero. On a regular graph the score equals
#' degree. High scores favour nodes with many neighbours of low degree.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive scores are divided by their maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Karci, A., Yakut, S., & Oztemiz, F. (2022). A New Approach Based on
#' Centrality Value in Solving the Minimum Vertex Cover Problem: Malatya
#' Centrality Algorithm. Journal of Computer Science, 7(2), 81-88,
#' equation 1. \doi{10.53070/bbd.1195501}.
#' @export
#' @examples
#' centrality_malatya(igraph::make_star(5, mode = "undirected"))
centrality_malatya <- function(x, ...) {
  df <- centrality(x, measures = "malatya", ...)
  stats::setNames(df$malatya, df$node)
}
