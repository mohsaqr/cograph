#' Mixed gravitational centrality and its neighbor extension
#' @keywords internal
#' @noRd
calculate_mixed_gravity <- function(cg, radius = 3, extended = FALSE) {
  automatic <- identical(radius, "auto")
  if (!is.null(radius) && !automatic &&
        (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
           radius < 0)) {
    .cg_stop_bad_parameter("gravity_radius must be nonnegative, NULL, or 'auto'")
  }
  a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
  diag(a) <- 0
  if (!nrow(a)) return(numeric())
  degree <- rowSums(a)
  core <- .cg_mdd(a, lambda = 0)
  distance <- .cg_distances(a, "all")
  if (automatic) radius <- .cg_gravity_auto_radius(distance)
  score <- .cg_gravity(distance, core, degree, radius = radius)
  if (extended) score <- as.numeric(a %*% score)
  score
}

#' Mixed Gravitational Centrality
#'
#' Mixed gravitational centrality (Wang, Li and Xia 2018), also called
#' improved gravitational centrality, uses the core number
#' \eqn{k_s(i)}{ks(i)} of the focal node and the degree \eqn{k(j)}{k(j)} of
#' each partner node as masses:
#' \deqn{MGC_i = k_s(i) \sum_{j : 0 < d(i,j) \le r} \frac{k(j)}{d(i,j)^2}.}{
#'   MGC_i = ks(i) sum_{j: 0 < d(i,j) <= r} k(j) / d(i,j)^2.}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored,
#' and \code{gravity_mass} has no effect. The implementation follows the
#' reproduction of the method in Li and Huang (2022), equations 5-8. The
#' Centrality Zoo writes the inner sum over immediate neighbors, which
#' corresponds to \code{gravity_radius = 1}. Isolated nodes score zero, and
#' unreachable nodes contribute nothing. \code{gravity_radius = "auto"} is
#' a cograph heuristic that rounds half the mean finite distance to the
#' nearest integer, with a minimum of one. A negative radius raises an
#' error.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param gravity_radius Hop-distance cutoff \eqn{r}{r}. Default 3.
#'   \code{NULL} or \code{Inf} includes every reachable node, and a value
#'   below one gives zero scores.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references Wang, J., Li, C. and Xia, C. (2018). Improved centrality
#'   indicators to characterize the nodal spreading capability in complex
#'   networks. Applied Mathematics and Computation, 334, 388-400.
#'   \doi{10.1016/j.amc.2018.04.028}.
#'
#' Li, Z. and Huang, X. (2022). Identifying influential spreaders by gravity
#'   model considering multi-characteristics of nodes. Scientific Reports, 12,
#'   9879. \doi{10.1038/s41598-022-14005-3}.
#' @seealso \code{\link{centrality_extended_mixed_gravity}},
#'   \code{\link{centrality_gravity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_mixed_gravity(regulation_net)
centrality_mixed_gravity <- function(x, gravity_radius = 3, ...) {
  df <- centrality(x, measures = "mixed_gravity",
                   gravity_radius = gravity_radius, ...)
  stats::setNames(df$mixed_gravity, df$node)
}

#' Extended Mixed Gravitational Centrality
#'
#' Extended mixed gravitational centrality (Wang, Li and Xia 2018), also
#' called IGC+, sums the mixed gravitational scores of the immediate
#' neighbors of a node:
#' \deqn{EMGC_i = \sum_{j \in N(i)} MGC_j.}{
#'   EMGC_i = sum_{j in N(i)} MGC_j.}
#'
#' @details
#' Each inner score \eqn{MGC_j}{MGC_j} uses the radius around \eqn{j}{j},
#' so a contribution can come from up to \code{gravity_radius + 1} hops
#' from the focal node. The implementation follows the reproduction in Li
#' and Huang (2022), equation 8. The input handling and radius options of
#' \code{\link{centrality_mixed_gravity}} apply, so direction, weights,
#' loops and parallel edges are ignored. Isolated nodes score zero.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param gravity_radius Hop-distance cutoff of the inner scores. Default
#'   3. \code{NULL} or \code{Inf} includes every reachable node.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Wang, J., Li, C. and Xia, C. (2018). Improved centrality indicators to
#'   characterize the nodal spreading capability in complex networks. Applied
#'   Mathematics and Computation, 334, 388-400.
#'   \doi{10.1016/j.amc.2018.04.028}.
#'
#' Li, Z. and Huang, X. (2022). Identifying influential spreaders by gravity
#'   model considering multi-characteristics of nodes. Scientific Reports, 12,
#'   9879. \doi{10.1038/s41598-022-14005-3}.
#' @seealso \code{\link{centrality_mixed_gravity}},
#'   \code{\link{centrality_gravity}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_extended_mixed_gravity(regulation_net)
# nolint start: object_length_linter.
centrality_extended_mixed_gravity <- function(x, gravity_radius = 3, ...) {
  df <- centrality(x, measures = "extended_mixed_gravity",
                   gravity_radius = gravity_radius, ...)
  stats::setNames(df$extended_mixed_gravity, df$node)
}
# nolint end: object_length_linter.
