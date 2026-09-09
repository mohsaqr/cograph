#' Degree and importance of lines (Liu, Xiong, Shi, Shi and Wang 2016)
#' @keywords internal
#' @noRd
calculate_dil <- function(cg) {
  .cg_dil_terms(.cg_path_matrix(cg, NULL))$dil
}

#' Degree and Importance of Lines
#'
#' Liu, Xiong, Shi, Shi and Wang rank a node by its degree plus the share
#' it can claim of the importance of the lines that touch it. A line
#' matters when its two endpoints reach far beyond it and when no triangle
#' offers a way round it, so the importance of the line \eqn{e_{mn}} is
#' \eqn{I_{e_{mn}}=U/\lambda} with
#' \eqn{U=(k_m-p-1)(k_n-p-1)} and \eqn{\lambda=p/2+1}, where \eqn{p} is the
#' number of triangles one of whose edges is \eqn{e_{mn}}. That importance
#' is then split between the endpoints in proportion to their own degrees,
#' \eqn{W_{v_iv_j}=I_{e_{ij}}(k_i-1)/(k_i+k_j-2)}, and the score is
#' \eqn{L_{v_i}=k_i+\sum_{v_j\in\Gamma_i}W_{v_iv_j}} over the open
#' neighborhood \eqn{\Gamma_i}. The measure is strictly two-hop local: only
#' the degrees of a node, of its neighbors and the triangles on its
#' incident lines enter, so it costs \eqn{O(n\langle k\rangle^2)} and its
#' raw scores are component-local.
#'
#' \strong{\eqn{\lambda} is \eqn{p/2+1}, and reading it from a text layer
#' gets it wrong.} The stacked fraction extracts from the published PDF as
#' \eqn{\lambda=2p+1}, in Liu et al.'s original as much as in the Almasi
#' and Hu (2019) reproduction of it. The page image shows \eqn{p} over
#' \eqn{2}; so does the paper's own worked example, in printed prose, on
#' page 210: for the seven-line network of its Fig. 1(b) it writes
#' \eqn{p=1}, \eqn{U=4}, "\eqn{\lambda=1/2+1=1.5}" and
#' \eqn{I_{e_{45}}=4/1.5\approx 2.6667}. The wrong reading returns
#' \eqn{4/3} there. cograph reproduces \eqn{8/3}.
#'
#' \strong{\eqn{U} is never negative, so a score never falls below the
#' node's degree.} For a line \eqn{(i,j)}, \eqn{j} belongs to \eqn{N(i)}
#' but to neither \eqn{N(j)} nor the intersection, so
#' \eqn{p=|N(i)\cap N(j)|\le k_i-1} and both factors of \eqn{U} are at
#' least zero. Since \eqn{\lambda\ge 1}, every \eqn{I} and every \eqn{W} is
#' at least zero and \eqn{L_{v_i}\ge k_i}. Equality is common rather than
#' exceptional: every line of a complete graph, of a star, or of any
#' network whose lines all touch a degree-one node has \eqn{U=0}, so
#' \eqn{K_n} scores \eqn{n-1} at every node and a star scores its degree at
#' every node.
#'
#' \strong{The importance of a line is conserved when it is split.} The two
#' shares \eqn{(k_i-1)/(k_i+k_j-2)} and \eqn{(k_j-1)/(k_i+k_j-2)} sum to
#' one, so \eqn{\sum_i (L_{v_i}-k_i)=\sum_{e}I_e}: the network's total
#' excess over degree is exactly the total importance of its lines. That
#' identity is asserted over the package's whole verification collection.
#'
#' \strong{An isolated \eqn{K_2} is the one undefined split, and it is
#' resolved rather than refused.} The denominator \eqn{k_i+k_j-2} vanishes
#' only when \eqn{k_i=k_j=1}, since both endpoints of a line have degree at
#' least one -- that is a two-node component -- and there \eqn{p=0} and
#' \eqn{U=(1-0-1)(1-0-1)=0}, so the importance being divided is exactly
#' zero while the split of it is \eqn{0/0}. Because \eqn{W} is a
#' \emph{share} of \eqn{I}, and the two shares sum to one wherever they are
#' defined, every admissible split of an exactly zero importance gives an
#' exactly zero contribution: the answer does not depend on resolving the
#' indeterminacy. cograph therefore writes the share as zero, taking the
#' test before the division so that no \eqn{0/0} is ever evaluated, and
#' both nodes of a \eqn{K_2} score \eqn{1}. \strong{The source says nothing
#' about this case}; the choice is cograph's, and it follows the precedent
#' of \code{\link{centrality_lhc}}, whose \eqn{0/0} on a triangle-free
#' graph is likewise written as zero because the denominator vanishes
#' exactly where every numerator does. It deliberately does not follow
#' \code{\link{centrality_iec}}, which returns \code{NA} on reducible
#' input: there the closed form returns a finite number in place of an
#' infinite one, so a value would be wrong, where here every candidate
#' value is the same value.
#'
#' \strong{Direction and weights are dropped, because the authors exclude
#' them.} Page 210 opens the derivation with "we assume that a network
#' \eqn{G=(V,E)} is an undirected and unweighted network", and every
#' quantity in the three equations is a count: a degree, a triangle
#' census, a difference of integers. A directed, weighted or multigraph
#' input is therefore projected onto its simple undirected skeleton --
#' arcs symmetrized, weights and parallel edges collapsed to a single line,
#' loops dropped -- rather than refused, which is the convention every
#' other undirected-domain measure in \code{\link{centrality}} already
#' follows, and the projection is silent rather than warned for the same
#' reason. There is no in/out/all reading to choose between, so the measure
#' sits in the no-mode family and \code{cutoff} and \code{invert_weights}
#' are ignored as well. The source states no normalization, so
#' \code{normalized = TRUE} max-scales the finished vector as elsewhere in
#' \code{\link{centrality}}.
#'
#' \strong{Isolates, singletons and disconnected input need no special
#' rule.} An isolate has degree zero and an empty sum, so it scores zero;
#' the single node of a one-node graph and every node of an edgeless graph
#' score zero for the same reason, and an empty graph returns no scores.
#' Because nothing in equations (1)-(3) reaches past a node's second
#' neighbors, the raw scores are component-local: attaching a disjoint
#' component leaves every existing score unchanged.
#'
#' \strong{The source prints three numerical fixtures and all three are
#' reproduced.} Fig. 1 on page 210 prints \eqn{I_{e_{45}}=9} at \eqn{p=0}
#' and \eqn{8/3} at \eqn{p=1}; Fig. 2 on page 211 prints \eqn{L_{v_2}=26/9}
#' and \eqn{L_{v_5}=52/15} on a 27-node tree; and Table 3 on page 217
#' prints a DIL value for every one of the 21 nodes of the ARPA network,
#' whose topology is Fig. 6 on the same page. All 21 printed values are
#' reproduced, and the edge list read off the figure is corroborated
#' independently by the paper's own degree column. See the batch 50
#' published audit in the package's verification directory.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order, one score per node,
#'   each at least the node's degree in the simple undirected skeleton.
#' @references Liu, J., Xiong, Q., Shi, W., Shi, X. and Wang, K. (2016).
#'   Evaluating the importance of nodes in complex networks. Physica A:
#'   Statistical Mechanics and its Applications, 452, 209-219. Equation (1)
#'   and the definitions of \eqn{U}, \eqn{p} and \eqn{\lambda} on page 210,
#'   equations (2) and (3) on page 211, the complexity claim in Table 4 on
#'   page 218, and the ARPA fixture in Table 3 and Fig. 6 on page 217.
#'   \doi{10.1016/j.physa.2016.02.049}. The same three equations are
#'   reproduced as equations (7)-(9) by Almasi, S. and Hu, T. (2019).
#'   Measuring the importance of vertices in the weighted human disease
#'   network. PLoS ONE, 14(3), e0205936. \doi{10.1371/journal.pone.0205936}.
#' @seealso \code{\link{centrality_lhc}} and \code{\link{centrality_hcc}}
#'   for other degree-and-triangle hybrids,
#'   \code{\link{centrality_bridging}} for another measure that scores a
#'   node by the lines it carries, and \code{\link{list_centralities}} for
#'   the catalogue.
#' @export
#' @examples
#' # Every line of a complete graph is shortcut by n - 2 triangles, so U is
#' # zero throughout and the score is the degree.
#' centrality_dil(igraph::make_full_graph(5))
#'
#' # A triangle-free k-regular graph scores k + k(k-1)^2/2 at every node:
#' # 3 for a ring and 9 for the Petersen graph.
#' centrality_dil(igraph::make_ring(6))
#'
#' # The path 1-2-3-4-5 scores 1, 2.5, 3, 2.5, 1: a line to a leaf carries
#' # no importance, and the two interior lines carry one each, split evenly.
#' centrality_dil(igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5),
#'                                   directed = FALSE))
#'
#' # A triangle on two degree-three nodes is the case that needs
#' # lambda = p/2 + 1: I = 1 / 1.5 = 2/3, split evenly, so the two hubs
#' # score 3 + 1/3. Reading lambda as 2p + 1 would give 3 + 1/6.
#' centrality_dil(igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4, 2, 5),
#'                                   directed = FALSE))
centrality_dil <- function(x, ...) {
  df <- centrality(x, measures = "dil", ...)
  stats::setNames(df$dil, df$node)
}
