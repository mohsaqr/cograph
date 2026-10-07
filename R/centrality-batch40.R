#' DK-based gravity model
#' @keywords internal
#' @noRd
calculate_dkgm <- function(cg, radius = 2) {
  automatic <- identical(radius, "auto")
  if (is.null(radius)) radius <- Inf
  if (!automatic &&
        (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
           radius < 0)) {
    .cg_stop_bad_parameter("dkgm_radius must be nonnegative, Inf, NULL, or 'auto'")
  }
  a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(a) <- 0
  if (!nrow(a)) return(numeric())
  mass <- .cg_dk_index(a)
  distance <- .cg_distances(a, "all")
  if (automatic) radius <- .cg_gravity_auto_radius(distance)
  .cg_gravity(distance, mass, mass, radius = radius)
}

#' DK-Based Gravity Model
#'
#' The DK-based gravity model (Li and Huang 2021) sums, over the partners
#' within a hop-distance radius \eqn{R}, the product of the two nodes'
#' masses divided by their squared distance. The mass
#' \eqn{DK(i) = k(i) + k_s^*(i)}{DK(i) = k(i) + ks*(i)} adds the degree to an
#' improved k-shell index
#' \eqn{k_s^*(i) = k_s(i) + p(i)/(\max_k q(k) + 1)}{
#'   ks*(i) = ks(i) + p(i) / (max_k q(k) + 1)},
#' where \eqn{p(i)} is the peeling stage at which the node leaves its shell
#' and \eqn{q(k)} the number of stages shell \eqn{k} needs.
#' \deqn{DKGM_i = \sum_{j \ne i,\; d(i,j) \le R}
#'   \frac{DK(i)\,DK(j)}{d(i,j)^2}}{
#'   DKGM_i = sum_{j != i, d(i,j) <= R} DK(i) DK(j) / d(i,j)^2}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the network,
#' so direction, weights, loops and parallel edges are ignored. The k-shell
#' peeling removes nodes of degree at most \eqn{k}, which is the reading that
#' terminates and reproduces the source's Tables 2 to 5, so an isolate falls
#' in the one-shell. Unreachable partners contribute nothing, and isolates
#' and a single-node graph score zero. The stage denominator
#' \eqn{\max_k q(k) + 1}{max_k q(k) + 1} is a global maximum, so adding a
#' disconnected component can change every score. A \code{dkgm_radius} below
#' one gives zero at every node.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param dkgm_radius Hop-distance radius \eqn{R}, a nonnegative number
#'   (default 2, a value the source recommends). \code{NULL} or \code{Inf}
#'   includes every reachable partner. \code{"auto"} uses half the mean
#'   finite hop distance, rounded to the nearest integer and at least one,
#'   following the source's equation 4.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Li, Z. and Huang, X. (2021). Identifying influential spreaders in complex
#'   networks by an improved gravity model. Scientific Reports, 11, 22194.
#'   \doi{10.1038/s41598-021-01218-1}.
#' @seealso \code{\link{centrality_mcgm}},
#'   \code{\link{centrality_mixed_gravity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_dkgm(regulation_net)
centrality_dkgm <- function(x, dkgm_radius = 2, ...) {
  df <- centrality(x, measures = "dkgm", dkgm_radius = dkgm_radius, ...)
  stats::setNames(df$dkgm, df$node)
}
