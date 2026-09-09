#' Project inputs and calculate extended core-based measures
#' @keywords internal
#' @noRd
calculate_extended_core <- function(cg, measure, radius = 3) {
  b <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(b) <- 0
  core <- .cg_mdd(b, lambda = 0)
  if (measure == "extended_coreness") return(.cg_extended_coreness(b, core))
  .cg_extended_gravity(b, core, radius)
}

#' Extended neighborhood coreness
#'
#' Bae and Kim's extended neighborhood coreness sums the neighborhood
#' coreness of every immediate neighbor:
#' \eqn{C_{nc+}(i)=\sum_{j\in N(i)}\sum_{l\in N(j)}k_s(l)}.
#' Equivalently, the score is \eqn{A^2 k_s}. Here k_s is the core-number
#' vector of the original simple undirected graph. Core numbers are not
#' recomputed inside each neighborhood.
#'
#' Every length-two walk contributes its endpoint's core number, including
#' returns to the focal node and repeated endpoints reached via different
#' neighbors. This is not a sum over distinct nodes at distance two. Isolates
#' score zero. On a tree, it equals the sum of neighboring degrees; on a
#' d-regular graph it equals d cubed. A larger score means more access to
#' core-rich neighborhoods; numerical equivalence does not imply superior
#' spreading prediction for every network.
#'
#' Uses the simple undirected unweighted skeleton: either direction creates
#' an edge, parallel edges count once and loops are removed. This projection
#' is a cograph convention for inputs outside the published domain. Weights,
#' \code{mode} and shortest-path weight inversion do not affect the score.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive scores are divided by their maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Bae, J., & Kim, S. (2014). Identifying and ranking influential spreaders
#' in complex networks by neighborhood coreness. Physica A, 395, 549-559.
#' \doi{10.1016/j.physa.2013.10.047}.
#' The equations used here are reproduced as equations 2 and 3 in Ma,
#' Ma, Zhang & Wang (2016), Physica A, 451, 205-212.
#' \doi{10.1016/j.physa.2015.12.162}.
#' @export
#' @examples
#' centrality_extended_coreness(igraph::make_ring(6))
centrality_extended_coreness <- function(x, ...) {
  df <- centrality(x, measures = "extended_coreness", ...)
  stats::setNames(df$extended_coreness, df$node)
}

#' Extended gravity centrality
#'
#' Ma et al.'s extended gravity score is the sum of the immediate neighbors'
#' raw gravity scores:
#' \eqn{G^+(i)=\sum_{j\in N(i)}G(j)}, where
#' \eqn{G(j)=\sum_{l:0<d(j,l)\le r}k_s(j)k_s(l)/d(j,l)^2}.
#' Core numbers and hop distances are calculated on the original simple
#' undirected graph. The radius applies around each neighbor j; it is not
#' a radius around the focal node i. A contribution can therefore reach
#' r+1 hops from i, and paths from a neighbor back to i also contribute.
#'
#' Default radius three is the setting used in the original paper. NULL
#' or infinity includes every reachable partner, excluding the gravity
#' source itself. Radius zero and isolates score zero. The outer neighbor
#' sum has no distance penalty. All inner scores remain raw until the
#' final optional max normalization.
#'
#' Uses the simple undirected unweighted skeleton, with either direction
#' creating an edge, parallel edges counted once and loops removed. This
#' projection is a cograph convention for other inputs. Edge weights,
#' \code{mode}, \code{gravity_mass} and path-weight inversion do not affect
#' this measure: its masses are always k-shell indices. Computation includes
#' all-pairs hop distances, so it can be expensive for large graphs.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param gravity_radius Nonnegative hop-distance cutoff, default 3. NULL
#'   or infinity includes the entire reachable component. The optional
#'   \code{"auto"} setting is a cograph extension: round half the mean
#'   finite positive hop distance to the nearest integer (ties to even),
#'   with minimum one. It is not a parameter rule from Ma et al.
#' @param ... Additional arguments to \code{\link{centrality}}. With
#'   \code{normalized = TRUE}, positive final scores are divided by their
#'   maximum.
#' @return Named numeric vector in input node order.
#' @references
#' Ma, L. L., Ma, C., Zhang, H. F., & Wang, B. H. (2016). Identifying
#' influential spreaders in complex networks based on gravity formula.
#' Physica A, 451, 205-212, equations 6 and 7.
#' \doi{10.1016/j.physa.2015.12.162}.
#' @seealso \code{\link{centrality_gravity}}.
#' @export
#' @examples
#' centrality_extended_gravity(igraph::make_ring(6), gravity_radius = 3)
centrality_extended_gravity <- function(x, gravity_radius = 3, ...) {
  df <- centrality(x, measures = "extended_gravity",
                   gravity_radius = gravity_radius, ...)
  stats::setNames(df$extended_gravity, df$node)
}
