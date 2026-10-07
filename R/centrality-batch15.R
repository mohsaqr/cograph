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

#' Dynamics-Sensitive Centrality
#'
#' Dynamics-sensitive centrality (Liu et al. 2016) is a linearized
#' cumulative spreading score with spreading rate \eqn{\beta}{beta} and
#' recovery rate \eqn{\mu}{mu} over \eqn{T} steps:
#' \deqn{S(T) = \sum_{r=0}^{T-1} \beta A \left[\beta A + (1-\mu) I\right]^r
#'   \mathbf{1}.}{
#'   S(T) = sum_{r=0}^{T-1} beta A [beta A + (1 - mu) I]^r 1.}
#' With \eqn{\mu = 1}{mu = 1} it reduces to
#' \eqn{\sum_{t=1}^{T} (\beta A)^t \mathbf{1}}{sum_{t=1}^{T} (beta A)^t 1},
#' the form listed in the Centrality Zoo, and \eqn{\mu = 0}{mu = 0} gives the
#' susceptible-infected case.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored. Isolated nodes score 0.
#' \eqn{T = 0} or \eqn{\beta = 0}{beta = 0} gives 0, and \eqn{T = 1} gives
#' \eqn{\beta}{beta} times the degree. The score counts repeated walks and
#' can exceed the number of nodes, so it is not an infection probability.
#' The defaults \eqn{\beta = 0.1}{beta = 0.1}, \eqn{\mu = 1}{mu = 1} and
#' \eqn{T = 5} are one setting studied by Liu et al. (2016). Parameters
#' outside their ranges raise an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ds_beta Spreading rate \eqn{\beta}{beta}, between 0 and 1.
#'   Default 0.1.
#' @param ds_mu Recovery rate \eqn{\mu}{mu}, between 0 and 1. Default 1.
#' @param ds_steps Horizon \eqn{T}, a nonnegative integer. Default 5.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Liu, J. G., Lin, J. H., Guo, Q., & Zhou, T. (2016). Locating influential
#'   nodes via dynamics-sensitive centrality. Scientific Reports, 6, 21380.
#'   \doi{10.1038/srep21380}.
#' @seealso \code{\link{centrality_diffusion_centrality}},
#'   \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dynamics_sensitive(regulation_net)
# nolint start: object_length_linter.
centrality_dynamics_sensitive <- function(x, ds_beta = 0.1, ds_mu = 1,
                                          ds_steps = 5, ...) {
  df <- centrality(x, measures = "dynamics_sensitive", ds_beta = ds_beta,
                   ds_mu = ds_mu, ds_steps = ds_steps, ...)
  stats::setNames(df$dynamics_sensitive, df$node)
}
# nolint end: object_length_linter.

#' Malatya Centrality
#'
#' The Malatya centrality of a node (Karci et al. 2022) is the sum of its
#' degree divided by the degree of each neighbor,
#' \eqn{M(i) = \sum_{j \in N(i)} d_i / d_j}{M(i) = sum_{j in N(i)} d_i / d_j}.
#' High scores mark nodes with many neighbors of low degree.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored. Isolated nodes score 0. On a
#' regular network the score equals the degree. On every node with at least
#' one neighbor it is the reciprocal of
#' \code{\link{centrality_bridging_coefficient}}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Karci, A., Yakut, S., & Oztemiz, F. (2022). A New Approach Based on
#'   Centrality Value in Solving the Minimum Vertex Cover Problem: Malatya
#'   Centrality Algorithm. Journal of Computer Science, 7(2), 81-88.
#'   \doi{10.53070/bbd.1195501}.
#' @seealso \code{\link{centrality_bridging_coefficient}},
#'   \code{\link{centrality_degree}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_malatya(regulation_net)
centrality_malatya <- function(x, ...) {
  df <- centrality(x, measures = "malatya", ...)
  stats::setNames(df$malatya, df$node)
}
