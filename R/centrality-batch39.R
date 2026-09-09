#' Mixed gravitational centrality and its neighbor extension
#' @keywords internal
#' @noRd
calculate_mixed_gravity <- function(cg, radius = 3, extended = FALSE) {
  automatic <- identical(radius, "auto")
  if (!is.null(radius) && !automatic &&
        (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
           radius < 0)) {
    stop("gravity_radius must be nonnegative, NULL, or 'auto'",
         call. = FALSE)
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

#' Mixed gravitational centrality
#'
#' Mixed gravitational centrality (MGC), also called improved gravitational
#' centrality (IGC), uses the focal node's core number as its mass and the
#' partner node's degree as its mass:
#' \eqn{MGC_i=k_s(i)\sum_{j:0<d(i,j)\le r}k(j)/d(i,j)^2}.
#' All degrees, core numbers and hop distances are measured on the original
#' simple undirected graph. The masses are asymmetric even though distances
#' are symmetric. This differs from using core numbers on both ends or
#' degree on both ends of each interaction.
#'
#' The implementation follows the explicit reproduction of Wang et al.'s
#' method in Li and Huang (2022), equations 5-8, with default radius three.
#' The original 2018 full equations and author software have not been
#' inspected. The Zoo summary writes an immediate-neighbor inner sum;
#' \code{gravity_radius = 1} reproduces that literal interpretation.
#' Numerical verification establishes agreement with the cited reproduced
#' definition, not parity with unavailable original software or a guarantee
#' of spreading performance.
#'
#' Uses the simple undirected unweighted skeleton: either arc creates an
#' edge, parallel edges count once and loops are removed. This projection
#' is a cograph convention outside the source domain. Edge weights, mode,
#' cutoff, gravity_mass and path-weight inversion are ignored. Isolates
#' and singleton graphs score zero; empty graphs return no scores.
#' Unreachable partners contribute zero. With a fixed radius, adding a
#' disconnected component leaves existing raw scores unchanged. Optional
#' maximum normalization applies to the complete result over all nodes.
#' Dense all-pairs distances cost O(n cubed) time and O(n squared) memory.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param gravity_radius Nonnegative hop-distance cutoff, default three.
#'   NULL or infinity includes every reachable partner. Fractional cutoffs
#'   include exactly integer hop distances not exceeding them; values below
#'   one give zero. The optional \code{"auto"} is a cograph heuristic:
#'   round half the mean finite positive distance to the nearest integer
#'   (ties to even), with minimum one. It is not the cited radius rule and
#'   can change when disconnected components are added.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Wang, J., Li, C. and Xia, C. (2018). Improved centrality
#'   indicators to characterize the nodal spreading capability in complex
#'   networks. Applied Mathematics and Computation, 334, 388-400.
#'   \doi{10.1016/j.amc.2018.04.028}.
#'
#'   Definition read in Li, Z. and Huang, X. (2022). Identifying influential
#'   spreaders by gravity model considering multi-characteristics of nodes.
#'   Scientific Reports, 12, 9879. Equations 5-8 and reference 19.
#'   \doi{10.1038/s41598-022-14005-3}.
#' @seealso \code{\link{centrality_extended_mixed_gravity}}.
#' @export
#' @examples
#' centrality_mixed_gravity(igraph::make_ring(6))
#' centrality_mixed_gravity(igraph::make_star(6), gravity_radius = 1)
centrality_mixed_gravity <- function(x, gravity_radius = 3, ...) {
  df <- centrality(x, measures = "mixed_gravity",
                   gravity_radius = gravity_radius, ...)
  stats::setNames(df$mixed_gravity, df$node)
}

#' Extended mixed gravitational centrality
#'
#' Extended mixed gravitational centrality (EMGC), also called IGC+, sums
#' the raw MGC scores of immediate neighbors:
#' \eqn{EMGC_i=\sum_{j\in N(i)}MGC_j}.
#' Each inner MGC score uses its own source node j's core number, partner
#' degrees, and original-graph hop distances. The inner radius is centered
#' on j, so a contribution can reach r+1 hops from i. Paths from j back to i
#' are included. The outer neighbor sum has no distance or mass factor.
#'
#' Follows the reproduction in Li and Huang (2022), equation 8, attributed
#' to Wang et al. (2018); the original full equations and software have not
#' been inspected. Uses the same skeleton and radius conventions as
#' \code{\link{centrality_mixed_gravity}}. Default inner radius three follows
#' the reproduced definition; radius one matches the Zoo's literal inner
#' neighbor sum. Optional maximum normalization occurs only after summing
#' raw neighbor scores. Isolates and radii below one score zero. Empty and
#' singleton graphs give no scores and zero, respectively. Dense O(n cubed)
#' time and O(n squared) memory. Verification of these numerical equations
#' does not establish author-software parity or predictive superiority.
#'
#' @inheritParams centrality_mixed_gravity
#' @return Named numeric vector in input node order.
#' @references Wang, J., Li, C. and Xia, C. (2018).
#'   \doi{10.1016/j.amc.2018.04.028}.
#'   Definition read in Li, Z. and Huang, X. (2022), Scientific Reports,
#'   12, 9879, equations 5-8 and reference 19.
#'   \doi{10.1038/s41598-022-14005-3}.
#' @export
#' @examples
#' centrality_extended_mixed_gravity(igraph::make_ring(6))
# nolint start: object_length_linter.
centrality_extended_mixed_gravity <- function(x, gravity_radius = 3, ...) {
  df <- centrality(x, measures = "extended_mixed_gravity",
                   gravity_radius = gravity_radius, ...)
  stats::setNames(df$extended_mixed_gravity, df$node)
}
# nolint end: object_length_linter.
