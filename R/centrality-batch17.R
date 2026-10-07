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

#' Extended Neighborhood Coreness
#'
#' Extended neighborhood coreness (Bae and Kim 2014) sums the neighborhood
#' coreness of every neighbor of a node:
#' \deqn{C_{nc+}(i) = \sum_{j \in N(i)} \sum_{l \in N(j)} k_s(l),}{
#'   C_nc+(i) = sum_{j in N(i)} sum_{l in N(j)} k_s(l),}
#' where \eqn{k_s}{k_s} is the core number in the whole network. The score
#' equals \eqn{A^2 k_s}{A^2 k_s}.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored. Every walk of length two adds the
#' core number of its endpoint, including walks that return to the focal
#' node. Isolated nodes score 0. On a tree the score equals the sum of the
#' neighbors' degrees, and on a \eqn{d}-regular network it equals
#' \eqn{d^3}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Bae, J., & Kim, S. (2014). Identifying and ranking influential spreaders
#'   in complex networks by neighborhood coreness. Physica A, 395, 549-559.
#'   \doi{10.1016/j.physa.2013.10.047}.
#' @seealso \code{\link{centrality_coreness}},
#'   \code{\link{centrality_extended_gravity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_extended_coreness(regulation_net)
centrality_extended_coreness <- function(x, ...) {
  df <- centrality(x, measures = "extended_coreness", ...)
  stats::setNames(df$extended_coreness, df$node)
}

#' Extended Gravity Centrality
#'
#' Extended gravity centrality (Ma et al. 2016) is the sum of the gravity
#' scores of a node's neighbors,
#' \deqn{G^+(i) = \sum_{j \in N(i)} G(j), \qquad
#'   G(j) = \sum_{l:\, 0 < d_{jl} \le r} \frac{k_s(j)\, k_s(l)}{d_{jl}^2},}{
#'   G+(i) = sum_{j in N(i)} G(j),
#'   G(j) = sum_{l: 0 < d_jl <= r} k_s(j) k_s(l) / d_jl^2,}
#' where \eqn{k_s}{k_s} is the k-shell index and \eqn{d} the hop distance.
#'
#' @details
#' The measure uses the simple undirected skeleton, so direction, weights,
#' loops and parallel edges are ignored, and the masses are always k-shell
#' indices. The radius applies around each neighbor \eqn{j}, so a
#' contribution can come from \eqn{r+1} hops away from \eqn{i}, including
#' paths back to \eqn{i}. Radius 0 and isolated nodes give 0. The
#' \code{"auto"} radius is half the mean finite positive hop distance,
#' rounded to the nearest integer with a minimum of 1. This rule is a
#' package choice. A negative radius raises an error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param gravity_radius Hop radius \eqn{r}: a nonnegative number (default
#'   3, the value of Ma et al. 2016), \code{"auto"}, or \code{NULL} or
#'   \code{Inf} for the whole component.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized}.
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Ma, L. L., Ma, C., Zhang, H. F., & Wang, B. H. (2016). Identifying
#'   influential spreaders in complex networks based on gravity formula.
#'   Physica A, 451, 205-212. \doi{10.1016/j.physa.2015.12.162}.
#' @seealso \code{\link{centrality_gravity}},
#'   \code{\link{centrality_extended_coreness}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_extended_gravity(regulation_net)
centrality_extended_gravity <- function(x, gravity_radius = 3, ...) {
  df <- centrality(x, measures = "extended_gravity",
                   gravity_radius = gravity_radius, ...)
  stats::setNames(df$extended_gravity, df$node)
}
