#' Immediate effects centrality (Friedkin 1991)
#' @keywords internal
#' @noRd
calculate_iec <- function(cg) {
  terms <- .cg_iec_terms(.cg_path_matrix(cg, NULL))
  status <- attr(terms, "status")
  message <- switch(
    status,
    singleton = paste("`iec` has no value on a one-node graph, so its column",
                      "is NA: equation (20) divides by n - 1, which is zero."),
    reducible = paste("`iec` has no value on this input, so its column is NA:",
                      "with the unit self-loops the source mandates, the",
                      "influence chain is still reducible, so the left",
                      "eigenvector of equation (9) is not determined and the",
                      "mean first passage times of equation (11) are infinite",
                      "between classes. Restrict the input to a connected",
                      "undirected graph or a strongly connected directed one."),
    NULL
  )
  if (!is.null(message)) {
    condition <- warningCondition(message,
                                  class = "cograph_undefined_measure",
                                  call = NULL)
    warning(condition)
  }
  terms$iec
}

#' Immediate Effects Centrality
#'
#' Friedkin's immediate effects centrality scores a node by how quickly the
#' rest of the network's influence reaches it. Actors whose effects travel
#' over long sequences of interpersonal influence are more dependent on
#' intervening actors than those whose effects travel over short ones, so
#' the measure is the reciprocal of the mean length of the influence
#' sequences that end at a node. Writing \eqn{W} for the row-stochastic
#' influence matrix, \eqn{c} for its left eigenvector at eigenvalue one,
#' \eqn{Z=(I-W+\mathbf{1}c')^{-1}} for the fundamental matrix, \eqn{Z_{dg}}
#' for \eqn{Z} with its off-diagonal entries set to zero and \eqn{E} for the
#' all-ones matrix, the mean lengths are
#' \eqn{M=(I-Z+EZ_{dg})\,\mathrm{diag}(1/c)} and the score is
#' \eqn{c_{IEC}(j)=(n-1)/\sum_{i\neq j}m_{ij}}. \eqn{M} is the mean first
#' passage time matrix of the chain, so the sum runs \emph{down} column
#' \eqn{j} and a high score marks a node the network reaches fast.
#'
#' \strong{The influence matrix carries a unit self-loop, and the self-loop
#' is load-bearing.} The source builds \eqn{W} by setting the diagonal of
#' the adjacency matrix to one and dividing each row by its sum,
#' \eqn{w_{ij}=a_{ij}/\sum_j a_{ij}} with \eqn{a_{ii}=1}, a construction it
#' attributes to French (1956) and states twice on page 1494, once in the
#' body and once in the note to Table 1. Its footnote 10 says why the
#' diagonal is there: a strong network with \eqn{w_{ii}>0} must be regular,
#' meaning aperiodic, and its footnote 9 gives the two-cycle
#' counterexample that a zero diagonal admits. An implementation that drops
#' the self-loop is not computing this measure on a different scale, it is
#' computing a different measure.
#'
#' \strong{This is not cograph's \code{\link{centrality_markov}}, and the
#' difference is not a rescaling.} The two are both built from mean first
#' passage times and are easy to confuse -- cograph's own candidate ledger
#' confused them for several rounds -- but they differ twice over.
#' \code{markov} normalises \eqn{A} without adding the diagonal, and it
#' divides the column sum by \eqn{n}, counting the excluded diagonal entry,
#' where equation (20) divides by \eqn{n-1}. The second difference is a
#' constant factor \eqn{n/(n-1)} and cannot reorder anything; the first can
#' and does. On the five-node star \code{markov} gives
#' \eqn{1.25, 0.161, 0.161, 0.161, 0.161} where \code{iec} gives
#' \eqn{0.5, 0.08, 0.08, 0.08, 0.08}, and the two rank the nodes
#' differently on 2 of the 21 connected five-node graphs. Both are kept:
#' \code{markov} is the older behaviour that existing results depend on,
#' \code{iec} is Friedkin's published measure.
#'
#' \strong{Reducible input is refused, not extended.} Equation (11) needs an
#' irreducible chain. Without one the eigenvector of equation (9) has a
#' dimension per closed class, so \eqn{c} is not determined, and
#' \eqn{\mathrm{diag}(1/c)} is undefined wherever \eqn{c} vanishes. The
#' danger is that the closed form does not announce the failure: for
#' \eqn{i} and \eqn{j} in different blocks \eqn{z_{ij}=0}, and equation (11)
#' then returns the entirely finite \eqn{m_{ij}=z_{jj}/c_j} in place of an
#' infinite mean first passage time. Rather than publish a finite wrong
#' number, cograph tests the chain first and returns \code{NA} at every node
#' with a \code{cograph_undefined_measure} warning. In practice the test is
#' connectedness of an undirected graph and strong connectedness of a
#' directed one, since the mandated self-loops settle aperiodicity for free.
#' Friedkin restricts his own analysis to regular networks and never defines
#' the measure outside them. \code{\link{centrality_rsp_betweenness}}
#' answers on disconnected input because \emph{its} source states a rule for
#' an unreachable pair; this one states none, and a component-wise reading
#' would additionally have to invent whether the \eqn{n-1} of equation (20)
#' counts the component or the network.
#'
#' \strong{A singleton is \code{NA} and an empty graph returns no scores.}
#' Equation (20) divides by \eqn{n-1}, which is zero when \eqn{n=1}; the
#' same \code{NA} and the same warning follow. An isolate never appears on
#' its own, because a graph containing one is reducible and is already
#' \code{NA} everywhere.
#'
#' \strong{Direction is kept; weights, loops and parallel edges are not.}
#' \eqn{W} is a matrix of directed influence, row \eqn{i} being what actor
#' \eqn{i} attends to, so a directed input is used as it stands and the
#' measure needs a strongly connected one. There is no in/out/all variant to
#' choose between, so \code{mode}, \code{cutoff} and \code{invert_weights}
#' are ignored. Weights are dropped, deliberately: \eqn{a_{ii}=1} is
#' calibrated against \eqn{a_{ij}=1}, so multiplying every weight by a
#' constant would silently re-weight each actor's self-reliance against the
#' network, and the source demonstrates only the binary case. Loops in the
#' input are absorbed by the mandated unit diagonal and parallel edges
#' collapse, since \eqn{a_{ij}=1} "wherever a line exists between two
#' points". The source states no normalization, so \code{normalized = TRUE}
#' max-scales the finished vector as elsewhere in \code{\link{centrality}}.
#'
#' \strong{The source prints a complete numerical fixture.} Table 1, pages
#' 1492-1494, gives this measure to three decimals for every node of all 21
#' connected non-isomorphic five-node graphs. All 105 printed values are
#' reproduced by this implementation; see the batch 49 published audit in
#' the package's verification directory.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order, \code{NA} at every node
#'   when the influence chain is reducible or the graph has one node.
#' @references Friedkin, N. E. (1991). Theoretical foundations for
#'   centrality measures. American Journal of Sociology, 96(6), 1478-1504.
#'   Equation (9) on page 1485, equation (11) on page 1486, equation (20) on
#'   page 1489, the construction of \eqn{W} and Table 1 on pages 1492-1494.
#'   \doi{10.1086/229694}. The fundamental matrix and the mean first passage
#'   form are Kemeny, J. G. and Snell, J. L. (1960). Finite Markov Chains,
#'   page 79.
#' @seealso \code{\link{centrality_markov}} for the older, and different,
#'   mean-first-passage measure, \code{\link{centrality_random_walk}} for
#'   another chain-based score, and \code{\link{list_centralities}} for the
#'   catalogue.
#' @export
#' @examples
#' # On a complete graph W = J/n, so Z = I, every mean first passage time is
#' # n, and the score is (n - 1) / (n (n - 1)) = 1/n. Friedkin's Table 1
#' # prints .200 for the five-node case.
#' centrality_iec(igraph::make_full_graph(5))
#'
#' # The five-node star is row 1 of that table: .500 at the centre and .080
#' # at each leaf.
#' centrality_iec(igraph::make_star(5, mode = "undirected"))
#'
#' # A disconnected graph has no answer: the influence chain is reducible,
#' # so every node is NA and a warning says why.
#' two <- matrix(0, 4, 4)
#' two[1, 2] <- two[2, 1] <- two[3, 4] <- two[4, 3] <- 1
#' tryCatch(centrality_iec(two), warning = conditionMessage)
centrality_iec <- function(x, ...) {
  df <- centrality(x, measures = "iec", ...)
  stats::setNames(df$iec, df$node)
}
