#' DK-based gravity model
#' @keywords internal
#' @noRd
calculate_dkgm <- function(g, radius = 2) {
  automatic <- identical(radius, "auto")
  if (is.null(radius)) radius <- Inf
  if (!automatic &&
        (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
           radius < 0)) {
    stop("dkgm_radius must be nonnegative, Inf, NULL, or 'auto'",
         call. = FALSE)
  }
  a <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(a) <- 0
  if (!nrow(a)) return(numeric())
  mass <- .cg_dk_index(a)
  distance <- .cg_distances(a, "all")
  if (automatic) radius <- .cg_gravity_auto_radius(distance)
  .cg_gravity(distance, mass, mass, radius = radius)
}

#' DK-based gravity model
#'
#' The DK-based gravity model (DKGM) puts the degree k-shell index of both
#' ends into a truncated squared-distance gravity sum:
#' \eqn{DKGM_i=\sum_{j\ne i,\,d(i,j)\le R}DK(i)DK(j)/d(i,j)^2}.
#' The mass \eqn{DK(i)=k(i)+k_s^*(i)} adds the degree to an improved shell
#' index \eqn{k_s^*(i)=k_s(i)+p(i)/(\max_k q(k)+1)}, where \eqn{p(i)} is the
#' removal stage inside the node's shell and \eqn{q(k)} the number of stages
#' the k-level needed. The stage breaks the ties that degree and k-shell
#' leave behind: two nodes of the same shell are separated by how late the
#' peeling reached them.
#'
#' Stages restart at one inside every shell, but the denominator
#' \eqn{\max_k q(k)+1} is a single global maximum. A node's mass therefore
#' depends on the whole graph: adding a disconnected component that peels in
#' more stages lengthens that denominator and changes every raw score. This
#' is a property of the published definition, not a cograph choice, and it
#' distinguishes DKGM from \code{\link{centrality_mixed_gravity}}.
#'
#' The paper's Algorithm 1 says "Find all nodes in G with degree k" while its
#' stage loop ends "until All remaining nodes in G have degree > k" and its
#' Methods define k-shell by removing "nodes whose degree k <= 1 ... Until
#' there are no nodes in the network with degree k <= 1". Strict equality
#' cannot terminate on a three-node path, so cograph follows the at-most
#' reading, which is the only one consistent with the printed termination
#' condition and which reproduces the paper's Tables 2 to 5. Removal inside a
#' stage is simultaneous, matching the printed two-stage two-shell. Because
#' the level starts at one, an isolate falls in the one-shell rather than the
#' zero-shell \code{centrality(measures = "coreness")} reports; isolates carry
#' no edges, so no other node's shell, stage or score is affected.
#'
#' Uses the simple undirected unweighted skeleton, which is the source
#' domain: either arc creates one edge, parallel edges count once and loops
#' are removed. This projection is a cograph convention outside that domain.
#' Edge weights, mode, cutoff, gravity_mass and path-weight inversion are
#' ignored. Unreachable partners contribute nothing. Isolates and singleton
#' graphs score zero; empty graphs return no scores. Optional maximum
#' normalization applies to the complete result over all nodes. Dense
#' all-pairs distances cost O(n cubed) time and O(n squared) memory.
#'
#' Numerical verification establishes agreement with the published equations
#' and the printed nine-node example, not parity with author software, which
#' was not located, nor any claim about spreading performance.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param dkgm_radius Nonnegative hop-distance cutoff, default two, the value
#'   used for the paper's Table 5 and one of the two the paper recommends in
#'   general. \code{NULL} or infinity includes every reachable partner.
#'   Fractional cutoffs include exactly the integer hop distances not
#'   exceeding them; values below one give zero. \code{"auto"} applies the
#'   paper's own equation 4, \eqn{R^*\approx\langle d\rangle/2}, with cograph
#'   conventions: half the mean finite positive hop distance, rounded to the
#'   nearest integer with ties to even, minimum one. Those conventions and
#'   the treatment of disconnected graphs are cograph's, not the paper's.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order.
#' @references Li, Z. and Huang, X. (2021). Identifying influential spreaders
#'   in complex networks by an improved gravity model. Scientific Reports,
#'   11, 22194. Equations 1-3, Algorithm 1 and Tables 2-5.
#'   \doi{10.1038/s41598-021-01218-1}.
#' @seealso \code{\link{centrality_mcgm}} and
#'   \code{\link{centrality_mixed_gravity}} for the other gravity masses.
#' @export
#' @examples
#' centrality_dkgm(igraph::make_ring(6))
#' centrality_dkgm(igraph::make_star(6), dkgm_radius = 1)
centrality_dkgm <- function(x, dkgm_radius = 2, ...) {
  df <- centrality(x, measures = "dkgm", dkgm_radius = dkgm_radius, ...)
  stats::setNames(df$dkgm, df$node)
}
