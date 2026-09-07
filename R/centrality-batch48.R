#' Lhc index (Wang, Yang, Liu and Ma 2021)
#' @keywords internal
#' @noRd
calculate_lhc <- function(g, radius = 2) {
  .cg_lhc_terms(.cg_path_matrix(g, NULL), radius)$lhc
}

#' Lhc Index
#'
#' Wang, Yang, Liu and Ma's Lhc index is a semi-local hybrid: it reads a
#' node's \emph{neighbour information} from degree and its \emph{topological
#' location} from the share of the network's triangles that sit on it, then
#' spreads both over a small ball and collects the result one step out. The
#' influence of a node is
#' \eqn{C(v)=\sum_{u\in\Phi(v)}k_u(1+TP(u))/d^2(uv)}, a sum over the ball
#' \eqn{\Phi(v)} of radius \code{lhc_radius} in which each member
#' contributes its degree, inflated by its triangle share, discounted by the
#' square of its distance; and the index itself is
#' \eqn{Lhc(v)=\sum_{w\in\tau(v)}C(w)}, the influence summed over the open
#' neighbourhood \eqn{\tau(v)=N(v)}. The triangle share is
#' \eqn{TP(u)=NTS(u)/TNTS}, with \eqn{NTS(u)} the number of triangles
#' containing \eqn{u} and \eqn{TNTS=\sum_u NTS(u)}.
#'
#' \strong{The denominator is \eqn{TNTS}, not the number of triangles, and
#' the paper settles it rather than the Zoo.} Immediately after defining
#' \eqn{TNTS} the source writes that "the total number of triangle
#' structure exists in the network are \eqn{\frac{1}{3}*TNTS}", so
#' \eqn{TNTS=3\Delta} for \eqn{\Delta} distinct triangles and \eqn{TP} sums
#' to exactly one over the nodes -- it really is a share. Entry 2.221 of
#' the Centrality Zoo transcribes the structure of both equations correctly
#' but names the denominator "\eqn{\Delta}, the total number of triangular
#' structures in the network", which read literally is three times too
#' small. The two readings are not related by a monotone transform in
#' general, and they differ substantially: on the Krackhardt kite the
#' paper's reading scores node 1 at \eqn{100.15} where the Zoo's literal
#' wording gives \eqn{125.45}. cograph follows the paper.
#'
#' \strong{\code{lhc_radius} is the source's own parameter, exposed with
#' the source's default.} The paper writes it \eqn{d}, states on page 4
#' that "the distance ranged \eqn{d} is set to be 2, namely, only the
#' nearest neighbors and the next-nearest neighbors are taken into
#' consideration", and then sweeps it in section 3 over eleven real
#' networks, reporting that "the optimal value of \eqn{d} is about 2-3" and
#' that the correlation stabilises beyond 3. It is therefore a genuine
#' modelling knob rather than an implementation detail, and it is exposed
#' with the paper's 2 as the default. At \code{lhc_radius = 1} the ball
#' collapses to the neighbours and \eqn{C(v)} becomes
#' \eqn{\sum_{u\in N(v)}k_u(1+TP(u))}; a radius at or above the graph's
#' diameter takes in everything reachable and the score stops moving. The
#' domain is a whole number of at least one; anything else is refused with
#' a \code{cograph_bad_parameter} error.
#'
#' \strong{Both neighbourhoods are open, and a node contributes to its own
#' score.} \eqn{\Phi(v)} is \eqn{1\le d(u,v)\le} \code{lhc_radius}: the
#' focal node is outside it, because \eqn{d^2(vv)=0} would divide by zero,
#' and unreachable nodes fall outside the radius so no infinity arises.
#' \eqn{\tau(v)} is the open neighbourhood. It follows -- the paper does
#' not remark on it, but its equations say so -- that \eqn{v} does enter
#' its own \eqn{Lhc(v)}, since \eqn{v} lies in \eqn{\Phi(w)} at distance 1
#' for every neighbour \eqn{w}.
#'
#' \strong{Triangle-free graphs are a cograph decision, taken explicitly.}
#' Every tree, star, path, even cycle and bipartite graph has
#' \eqn{TNTS=0}, and \eqn{TP(u)} is then \eqn{0/0} everywhere. The source
#' never mentions the case. Since \eqn{TNTS} is a sum of nonnegative
#' counts, it vanishes exactly when every numerator \eqn{NTS(u)} vanishes
#' too, so there is no share to distribute and no node with a claim on
#' one: \eqn{TP} is written as \strong{zero}, and the index reduces to the
#' pure degree-over-squared-distance sum, which is the neighbour and
#' location half of the hybrid with the triangle half contributing
#' nothing. The test is made on \eqn{TNTS} before any division, so no
#' \eqn{0/0} is evaluated; \code{NA} or an error would refuse every tree,
#' which the source's own construction handles perfectly well.
#'
#' \strong{Raw scores are not component-local.} \eqn{TNTS} is a global sum,
#' so attaching a disconnected component that carries a triangle rescales
#' every \eqn{TP} and moves every score. Attaching a component with no
#' triangle -- an isolate included -- changes nothing, since it changes no
#' degree, no triangle and no finite distance inside the existing
#' components. An isolate itself scores zero because \eqn{\tau(v)} is
#' empty and equation (2) is an empty sum; a singleton graph and every node
#' of an edgeless graph score zero for the same reason, and an empty graph
#' returns no scores.
#'
#' Direction, weights, loops and parallel edges are dropped to the simple
#' undirected skeleton the source defines on: \eqn{k_u} is a count,
#' \eqn{d(uv)} a hop count and \eqn{NTS(u)} a combinatorial quantity, and
#' the paper's eleven networks are simple and undirected. There is no
#' in/out/all variant to select, so the measure sits in the no-mode family,
#' and \code{cutoff} and \code{invert_weights} are ignored as well. The
#' source states no normalization, so \code{normalized = TRUE} max-scales
#' the finished vector as elsewhere in \code{\link{centrality}}.
#'
#' \strong{The source prints no numerical example.} There is no toy graph
#' with a table of scores anywhere in the paper -- its Table 1 lists
#' network statistics and its figures are aggregate SIR and Kendall plots
#' -- so there is no published per-node fixture to reproduce. Verification
#' rests instead on independent reference implementations and on
#' hand-derived closed forms for stars, complete graphs, rings and paths.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}, including
#'   \code{lhc_radius}.
#' @return Named numeric vector in input node order.
#' @references Wang, X., Yang, Q., Liu, M. and Ma, X. (2021).
#'   Comprehensive influence of topological location and neighbor
#'   information on identifying influential nodes in complex networks.
#'   PLoS ONE, 16(5), e0251208. Equation (1) and its symbol list on page 3,
#'   equation (2), the \eqn{d=2} statement and Algorithm 1 on page 4, and
#'   the \eqn{d} sweep on page 7. \doi{10.1371/journal.pone.0251208}.
#' @seealso \code{\link{centrality_hcc}} and \code{\link{centrality_ked}}
#'   for other degree-and-position hybrids,
#'   \code{\link{centrality_neighbor_distance}} for another
#'   distance-discounted neighbourhood sum, and
#'   \code{\link{list_centralities}} for the catalogue.
#' @export
#' @examples
#' # The path 1-2-3 is triangle-free, so the triangle share drops out and
#' # the scores are the hand-derived 2, 4.5, 2.
#' centrality_lhc(igraph::make_graph(c(1, 2, 2, 3), directed = FALSE))
#'
#' # On a complete graph every node scores (n-1)^3 (n+1) / n; for n = 5
#' # that is 76.8.
#' centrality_lhc(igraph::make_full_graph(5))
#'
#' # Widening the ball can only raise the score, and it stops moving once
#' # the radius reaches the diameter.
#' ring <- igraph::make_ring(9)
#' centrality_lhc(ring, lhc_radius = 1)
#' centrality_lhc(ring)
#' centrality_lhc(ring, lhc_radius = 4)
centrality_lhc <- function(x, ...) {
  df <- centrality(x, measures = "lhc", ...)
  stats::setNames(df$lhc, df$node)
}
