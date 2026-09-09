#' Hybrid characteristic centrality (Liu and Zheng 2023)
#' @keywords internal
#' @noRd
calculate_hcc <- function(cg, delta = 0.5) {
  .cg_hcc_terms(.cg_path_matrix(cg, NULL), delta)$hcc
}

#' Extended hybrid characteristic centrality (Liu and Zheng 2023)
#' @keywords internal
#' @noRd
calculate_ehcc <- function(cg, delta = 0.5) {
  .cg_hcc_terms(.cg_path_matrix(cg, NULL), delta)$ehcc
}

#' Hybrid characteristic centrality
#'
#' Liu and Zheng's hybrid characteristic centrality adds a local and a
#' global characteristic on the same scale:
#' \eqn{HCC(u)=k^{ex}(u)/k^{ex}_{max}+pos(u)/pos_{max}}. The local half is
#' the \emph{extended degree}
#' \eqn{k^{ex}(u)=\delta k(u)+(1-\delta)\sum_{v\in\phi(u)}k(v)}, the node's
#' own degree blended with its neighbours'; the global half is the
#' \emph{E-shell} position index, the round in which a repeated
#' minimum-extended-degree peel removes the node. Both terms are divided
#' by their largest value, so each lies in \eqn{[0,1]} and the raw score
#' lies in \eqn{[0,2]}.
#'
#' The E-shell hierarchy decomposition is not a k-shell decomposition,
#' although the Centrality Zoo describes it as "a variant of k-shell
#' decomposition". There is no outer loop over a shell index and no
#' repeat-until-stable inner loop: each round removes exactly the set of
#' remaining nodes attaining the current minimum extended degree,
#' recomputes the extended degrees on what is left, and tags the removed
#' nodes with the round number. The position indexes therefore run
#' \eqn{1,\dots,pos_{max}} with every value attained, rather than being
#' shell numbers, and the two procedures disagree on the source's own
#' figure 1.
#'
#' \strong{The source's printed algorithm contains a typo, and cograph
#' implements the correction its own tables require.} Step 3 of the E-shell
#' procedure prints \eqn{S_p=\arg\max_{u\in G_p}\{k^{ex}(u)\}} while the
#' same sentence calls \eqn{S_p} "the set of minimum nodes", the preceding
#' paragraph says "the nodes with minimum extended degree are found and
#' deleted", and the paper's table 2 heads its column "Minimum extended
#' degree" with the increasing values 2, 2.5, 3, 4.5, 5, 6. The minimum
#' reading reproduces every printed row; the literal maximum reading is a
#' different measure.
#'
#' \strong{The peel recomputes but equation (4) does not.} Step 6 updates
#' the extended degrees on the residual graph after every removal, which is
#' what the printed table 2 minima require. The \eqn{k^{ex}(u)} of equation
#' (4), and the \eqn{k^{ex}_{max}} it is divided by, are nevertheless the
#' \strong{original}-graph values: the paper's own worked line
#' \eqn{HCC(a)=4.5/11+4/6} uses the original 4.5 and the original maximum
#' 11, and its node \eqn{d} settles the question, since its original 9.5
#' gives the printed 1.86 while its residual 6 at removal time would give
#' 1.55.
#'
#' \strong{Raw scores are not component-local.} \eqn{k^{ex}_{max}} and
#' \eqn{pos_{max}} are single global constants, so adding a disconnected
#' component -- an isolate included -- can change every score, and not
#' merely by a common factor, because the two terms rescale independently.
#' The source does not discuss disconnected graphs.
#'
#' \strong{Degenerate cases are cograph decisions, not the source's.} An
#' isolate has extended degree zero, which for \eqn{\delta\in[0,1]} is the
#' global minimum, so it always leaves in the first round with
#' \eqn{pos=1}. On an edgeless graph every extended degree is zero and
#' \eqn{k^{ex}_{max}=0}, making the first term \eqn{0/0}; it is written as
#' \strong{zero}, which leaves the E-shell term alone. One round then
#' removes everything, so every node of an edgeless graph -- a singleton
#' included -- scores exactly 1. Empty graphs return no scores.
#'
#' \code{hcc_delta} defaults to the source's 0.5 and is restricted to the
#' source's stated domain \eqn{[0,1]}, where \eqn{\delta=1} recovers the
#' classical degree and \eqn{\delta=0} drops the node's own degree
#' entirely. Values outside that interval are refused with a
#' \code{cograph_bad_parameter} error rather than extended: they make the
#' extended degree negative on some graphs, and then equation (4) divides
#' by a nonpositive maximum, which the source never contemplates.
#'
#' Uses the simple undirected unweighted skeleton, the source's stated
#' domain: either arc creates one edge, parallel edges count once and loops
#' are removed. Edge weights, mode, cutoff and path-weight inversion are
#' ignored, and directed input is symmetrised rather than read as a
#' directed case, which the source does not define. The source states no
#' further normalization; \code{normalized = TRUE} max-scales the finished
#' vector as elsewhere in \code{\link{centrality}}, on top of the two
#' divisions equation (4) already performs. Cost is one dense
#' matrix-vector product per peeling round, so \eqn{O(n^3)} in the worst
#' case rather than the \eqn{O(n+m)} a sparse min-heap would give.
#'
#' Numerical verification establishes agreement with the definition and
#' with the values the source prints for its figure 1, not parity with
#' author software, which does not exist, and not any claim about
#' spreading performance.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}, including
#'   \code{hcc_delta}.
#' @return Named numeric vector in input node order.
#' @references Liu, J. and Zheng, J. (2023). Identifying important nodes in
#'   complex networks based on extended degree and E-shell hierarchy
#'   decomposition. Scientific Reports, 13, 3197. Equations (3) and (4) and
#'   the eight-step E-shell procedure on page 3, with the worked example on
#'   page 4. \doi{10.1038/s41598-023-30308-5}.
#' @seealso \code{\link{centrality_ehcc}} for the neighbourhood sum of this
#'   score, \code{\link{centrality_dkgm}} for another shell-and-degree
#'   hybrid, and \code{\link{list_centralities}} for the catalogue.
#' @export
#' @examples
#' # Every node of a regular graph has the same extended degree, so one
#' # round removes the whole graph and every node scores 1 + 1 = 2.
#' centrality_hcc(igraph::make_ring(6))
#'
#' # A star peels its leaves first and its centre second.
#' centrality_hcc(igraph::make_star(6, mode = "undirected"))
#'
#' # delta = 1 is the classical degree in the extended-degree slot.
#' centrality_hcc(igraph::make_star(6, mode = "undirected"), hcc_delta = 1)
centrality_hcc <- function(x, ...) {
  df <- centrality(x, measures = "hcc", ...)
  stats::setNames(df$hcc, df$node)
}

#' Extended hybrid characteristic centrality
#'
#' The extended hybrid characteristic centrality of Liu and Zheng is the
#' closed-neighbourhood sum of \code{\link{centrality_hcc}}:
#' \eqn{EHCC(u)=HCC(u)+\sum_{v\in\phi(u)}HCC(v)}, the focal node counted
#' once and each neighbour of the open 1-order neighbourhood once. It
#' rewards a node whose neighbours are themselves high in both the extended
#' degree and the E-shell hierarchy, which a node can be without being high
#' itself.
#'
#' Everything recorded on \code{\link{centrality_hcc}} carries over
#' unchanged: the source's \eqn{\arg\max}/\eqn{\arg\min} typo in step 3 of
#' the E-shell procedure, the original-graph reading of \eqn{k^{ex}} and
#' \eqn{k^{ex}_{max}} against the residual-graph peel, the global and
#' therefore not component-local normalisers, the \code{hcc_delta} domain
#' \eqn{[0,1]}, the \eqn{0/0} of an edgeless graph written as zero, the
#' simple undirected unweighted skeleton, and the ignored weights, mode,
#' cutoff and inversion. Because HCC lies in \eqn{[0,2]}, EHCC lies in
#' \eqn{[0,2(1+k_{max})]}, and an isolate scores exactly its own HCC.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}, including
#'   \code{hcc_delta}.
#' @return Named numeric vector in input node order.
#' @references Liu, J. and Zheng, J. (2023). Identifying important nodes in
#'   complex networks based on extended degree and E-shell hierarchy
#'   decomposition. Scientific Reports, 13, 3197. Equation (5) on page 3.
#'   \doi{10.1038/s41598-023-30308-5}.
#' @seealso \code{\link{centrality_hcc}} for the summand and
#'   \code{\link{list_centralities}} for the catalogue.
#' @export
#' @examples
#' # On a regular graph every node scores 2, so EHCC is 2 (1 + k).
#' centrality_ehcc(igraph::make_ring(6))
#'
#' # The star's centre collects every leaf's score as well as its own.
#' centrality_ehcc(igraph::make_star(6, mode = "undirected"))
centrality_ehcc <- function(x, ...) {
  df <- centrality(x, measures = "ehcc", ...)
  stats::setNames(df$ehcc, df$node)
}
