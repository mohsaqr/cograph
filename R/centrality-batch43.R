#' Validate the mass, exponent and stopping parameters shared by IRA/IIRA
#' @keywords internal
#' @noRd
.cg_check_ira_args <- function(alpha, tol, max_iter) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha)) {
    .cg_stop_bad_parameter("ira_alpha must be a single finite number")
  }
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0) {
    .cg_stop_bad_parameter("ira_tol must be a single finite positive number")
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1L ||
        !is.finite(max_iter) || max_iter < 1 || max_iter != trunc(max_iter)) {
    .cg_stop_bad_parameter("ira_max_iter must be a single whole number of at least one")
  }
  invisible(NULL)
}

#' Iterative resource allocation
#' @keywords internal
#' @noRd
calculate_ira <- function(cg, mass = "coreness", alpha = 1, tol = 1e-6,
                          max_iter = 1000) {
  mass <- match.arg(mass, c("coreness", "degree"))
  .cg_check_ira_args(alpha, tol, max_iter)
  a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(a) <- 0
  n <- nrow(a)
  if (!n) return(numeric())
  theta <- .cg_resource_mass(a, n, mass)
  weight <- numeric(n)
  linked <- theta > 0
  weight[linked] <- theta[linked]^alpha
  .cg_resource_settle(.cg_resource_matrix(a, weight), tol, max_iter, "ira")
}

#' Improved iterative resource allocation
#' @keywords internal
#' @noRd
calculate_iira <- function(cg, mass = "coreness", beta = 0.2, steps = 50) {
  mass <- match.arg(mass, c("coreness", "degree"))
  if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta) ||
        beta <= 0 || beta > 1) {
    .cg_stop_bad_parameter("iira_beta must be a single number in (0, 1]")
  }
  if (!is.numeric(steps) || length(steps) != 1L || !is.finite(steps) ||
        steps < 0 || steps != trunc(steps)) {
    .cg_stop_bad_parameter("iira_steps must be a single nonnegative whole number")
  }
  a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(a) <- 0
  n <- nrow(a)
  if (!n) return(numeric())
  theta <- .cg_resource_mass(a, n, mass)
  psi <- 1 - (1 - beta)^rowSums(a)
  .cg_resource_steps(.cg_resource_matrix(a, theta, psi * theta), steps)
}

#' Iterative Resource Allocation
#'
#' Iterative resource allocation (Ren et al. 2014) starts every node with one
#' unit of resource and repeatedly passes it to the neighbors in proportion to
#' the receiver's centrality \eqn{\theta}{theta}. The steady state ranks the
#' spreaders.
#' \deqn{I(t+1) = A\,I(t), \qquad
#'   a_{ij} = \frac{\theta_i^{\alpha}}{\sum_{u \in \Gamma(j)}
#'   \theta_u^{\alpha}}, \qquad I(0) = (1, \dots, 1)}{
#'   I(t+1) = A I(t), a_ij = theta_i^alpha / sum_{u in N(j)} theta_u^alpha,
#'   I(0) = (1, ..., 1)}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the network,
#' so direction, weights, loops and parallel edges are ignored. The total
#' resource is conserved, so on a graph without isolates the scores sum to
#' the number of nodes. An isolate receives nothing and scores zero. The
#' iteration stops when the largest absolute change falls below
#' \code{ira_tol}. On a bipartite component whose two vertex classes differ
#' in size, such as a star, the iteration has a period-two cycle. It then
#' reaches \code{ira_max_iter}, raises a \code{cograph_no_converge} warning
#' and returns the last iterate. The Centrality Zoo states the measure as the
#' left eigenvector of the transposed matrix, which agrees up to scale where
#' the limit exists. The implementation follows the source's iteration.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ira_mass Node centrality \eqn{\theta}{theta}: \code{"coreness"}
#'   (default) or \code{"degree"}.
#' @param ira_alpha Exponent \eqn{\alpha}{alpha} on the mass, a finite
#'   number (default 1).
#' @param ira_tol Positive stopping tolerance on the largest absolute change
#'   between iterates (default \code{1e-6}).
#' @param ira_max_iter Maximum number of iterations, a whole number of at
#'   least one (default 1000).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ren, Z.-M., Zeng, A., Chen, D.-B., Liao, H. and Liu, J.-G. (2014).
#'   Iterative resource allocation for ranking spreaders in complex networks.
#'   EPL (Europhysics Letters), 106(4), 48005.
#'   \doi{10.1209/0295-5075/106/48005}.
#' @seealso \code{\link{centrality_iira}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_ira(regulation_net)
centrality_ira <- function(x, ira_mass = "coreness", ira_alpha = 1,
                           ira_tol = 1e-6, ira_max_iter = 1000, ...) {
  df <- centrality(x, measures = "ira", ira_mass = ira_mass,
                   ira_alpha = ira_alpha, ira_tol = ira_tol,
                   ira_max_iter = ira_max_iter, ...)
  stats::setNames(df$ira, df$node)
}

#' Improved Iterative Resource Allocation
#'
#' Improved iterative resource allocation (Zhong, Liu and Shang 2015) is
#' \code{\link{centrality_ira}} with the receiver's share scaled by its
#' spreading capacity \eqn{1 - (1 - \beta)^{k_i}}{1 - (1 - beta)^k_i}, where
#' \eqn{k_i} is the degree and \eqn{\beta}{beta} the spreading rate. The
#' recursion \eqn{I(t+1) = A\,I(t)}{I(t+1) = A I(t)} runs a fixed number of
#' steps from \eqn{I(0) = (1, \dots, 1)}{I(0) = (1, ..., 1)}.
#' \deqn{a_{ij} = \left[1 - (1 - \beta)^{k_i}\right]
#'   \frac{\theta_i}{\sum_{u \in \Gamma(j)} \theta_u}}{
#'   a_ij = [1 - (1 - beta)^k_i] theta_i / sum_{u in N(j)} theta_u}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the network,
#' so direction, weights, loops and parallel edges are ignored. Every column
#' of \eqn{A} sums to less than one, so the scores decay geometrically and
#' only their order carries meaning. \code{normalized = TRUE} divides them by
#' their maximum. Each connected component decays at its own rate, so raw
#' scores are comparable only within a component, and a large
#' \code{iira_steps} underflows to zero. An isolate scores zero, and
#' \code{iira_steps = 0} returns a vector of ones. The Centrality Zoo
#' (section 2.185) states a different matrix, which is stochastic in neither
#' direction and does not reproduce the source's worked example. The
#' implementation follows the source.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ira_mass Node centrality \eqn{\theta}{theta}: \code{"coreness"}
#'   (default) or \code{"degree"}.
#' @param iira_beta Spreading rate \eqn{\beta}{beta}, a number in
#'   \eqn{(0, 1]}{(0, 1]} (default 0.2).
#' @param iira_steps Number of iterations, a nonnegative whole number
#'   (default 50).
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Zhong, L.-F., Liu, J.-G. and Shang, M.-S. (2015). Iterative resource
#'   allocation based on propagation feature of node for identifying the
#'   influential nodes. Physics Letters A, 379(38), 2272-2276.
#'   \doi{10.1016/j.physleta.2015.05.021}.
#' @seealso \code{\link{centrality_ira}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_iira(regulation_net, normalized = TRUE)
centrality_iira <- function(x, ira_mass = "coreness", iira_beta = 0.2,
                            iira_steps = 50, ...) {
  df <- centrality(x, measures = "iira", ira_mass = ira_mass,
                   iira_beta = iira_beta, iira_steps = iira_steps, ...)
  stats::setNames(df$iira, df$node)
}
