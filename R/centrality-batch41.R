#' Relative-entropy integrated evaluation (Chen, Wang and Luo 2016)
#'
#' Maps each requested index to a discrete distribution with equation (8) or
#' equation (9) and integrates them with the closed form of equation (11),
#' the normalised geometric mean. The geometric mean is taken in logarithms
#' so that a product of `m` small shares cannot underflow; an exact zero
#' survives as `exp(-Inf) = 0`.
#'
#' @param g An igraph object.
#' @param re_indexes Character vector of constituent index names.
#' @param re_negative Character vector of the indexes to treat as negative,
#'   or `NULL` for the source's own declarations.
#' @return Numeric vector summing to one, one entry per node.
#' @keywords internal
#' @noRd
calculate_relative_entropy <- function(g,
                                       re_indexes = c("degree", "closeness",
                                                      "betweenness",
                                                      "constraint"),
                                       re_negative = NULL) {
  vocabulary <- .cg_re_vocabulary()
  named <- is.character(re_indexes) && length(re_indexes) >= 1L &&
    !anyNA(re_indexes)
  flagged <- is.null(re_negative) ||
    (is.character(re_negative) && !anyNA(re_negative))
  stopifnot(
    "`re_indexes` must be a character vector naming at least one index" = named,
    "`re_indexes` must not name the same index twice" =
      !anyDuplicated(re_indexes),
    "`re_negative` must be NULL or a character vector" = flagged
  )
  unknown <- setdiff(re_indexes, vocabulary)
  if (length(unknown)) {
    text <- sprintf("Unknown `re_indexes`: %s. Available: %s.",
                    paste(unknown, collapse = ", "),
                    paste(vocabulary, collapse = ", "))
    stop(errorCondition(text, class = "cograph_unknown_measure", call = NULL))
  }
  outside <- setdiff(re_negative, re_indexes)
  if (length(outside)) {
    text <- sprintf(
      "`re_negative` names %s, which `re_indexes` does not request.",
      paste(outside, collapse = ", ")
    )
    stop(errorCondition(text, class = "cograph_unknown_measure", call = NULL))
  }
  negative <- re_negative %||% intersect(re_indexes, .cg_re_negative_default())
  a <- .cg_undirected_view(.cg_path_matrix(g, NULL))
  diag(a) <- 0
  if (!nrow(a)) return(numeric())
  ctx <- .cg_re_context(a)
  logs <- vapply(re_indexes, function(name) {
    .cg_re_distribution(.cg_re_index(ctx, name), name, name %in% negative)
  }, numeric(ctx$n))
  geometric <- exp(rowMeans(matrix(logs, nrow = ctx$n)))
  total <- sum(geometric)
  # Unreachable with the published vocabulary. For two or more nodes an index
  # is zero at a node only when that node is an isolate (degree, closeness,
  # positive constraint) or carries no shortest path (positive betweenness);
  # a negative index's complement vanishes only on a single node. Zeroing
  # every node therefore needs every non-isolate to have zero betweenness,
  # which makes betweenness identically zero and raises the equation-8
  # condition first. Kept because a future index could break that argument.
  if (!isTRUE(total > 0)) {  # nocov start
    text <- paste(
      "Every node is zero on at least one index, so equation 11 of Chen,",
      "Wang and Luo (2016) divides by zero and `relative_entropy` has no",
      "value on this graph. Choose a different `re_indexes` set."
    )
    stop(errorCondition(text, class = "cograph_undefined_index",
                        call = NULL))
  }  # nocov end
  geometric / total
}

#' Relative-Entropy Integrated Evaluation
#'
#' Integrates several centrality indexes into one score without asking the
#' user to weight them. Each index is first turned into a discrete
#' distribution over the nodes, and the integrated score is the distribution
#' that has the smallest total relative entropy to all of them. Chen, Wang
#' and Luo (2016) show that the minimiser has a closed form, equation (11):
#' \eqn{w_i=\prod_{j=1}^{m}u_{ji}^{1/m}/\sum_{i}\prod_{j=1}^{m}u_{ji}^{1/m}},
#' the normalised geometric mean of the \eqn{m} index distributions. The
#' result sums to one, so it reads as a share of importance rather than a
#' raw score.
#'
#' A *positive* index, where a larger value marks the more important node,
#' becomes a distribution through equation (8),
#' \eqn{C'(i)=C(i)/\sum_j C(j)}. A *negative* index, where a smaller value
#' marks the more important node, becomes one through equation (9),
#' \eqn{C'(i)=(1-C(i)/\sum_j C(j))/\sum_k(1-C(k)/\sum_j C(j))}. Both maps
#' are invariant to rescaling a positive index, so only the shape of an
#' index matters, never its units.
#'
#' The geometric mean is unforgiving: a node that scores exactly zero on any
#' one index scores exactly zero overall. That is the source's own printed
#' behaviour -- its Table 2 gives zero to the three Kite nodes with zero
#' betweenness -- and cograph reproduces it rather than smoothing it away.
#'
#' @section Constituent indexes:
#'
#' \code{re_indexes} accepts the six indexes the source both defines and
#' declares a direction for. Their default directions are the source's own.
#'
#' \describe{
#'   \item{degree}{Number of neighbours (section 3.2). Positive.}
#'   \item{closeness}{Equation (3), \eqn{1/\sum_j l_{ij}}, the reciprocal of
#'     the raw distance sum with no \eqn{|V|-1} factor. Positive.}
#'   \item{betweenness}{Equation (4), summed over ordered pairs
#'     \eqn{j\ne i\ne k}, so twice the usual unnormalised undirected
#'     betweenness. Positive.}
#'   \item{constraint}{Equation (6), the network constraint coefficient,
#'     with the outer sum over every other node rather than over the
#'     neighbours alone. Negative.}
#'   \item{n_components}{Number of connected components left after deleting
#'     the node (section 4.2). Positive.}
#'   \item{largest_component}{Size of the largest component left after
#'     deleting the node (section 4.2). Negative.}
#' }
#'
#' The default is the four-index "distinctiveness" set of the source's Kite
#' study. Passing all six reproduces its six-index column, and passing only
#' \code{n_components} and \code{largest_component} its two-index
#' "destructiveness" column. Equation (2) clustering and equation (5)
#' eigenvector are defined in the source but never used and never declared
#' positive or negative, and equation (7) average path length is infinite
#' as soon as deleting a node disconnects the graph, so none of them is
#' offered.
#'
#' @section Conventions and undefined cases:
#'
#' Equation (6) is not Burt's constraint. Its outer sum runs over all of
#' \eqn{V}, so a node two steps away contributes through the indirect term
#' alone; on the source's Kite this gives node 1 the printed 1.25 where
#' \code{igraph::constraint()} gives 1. The printed outer limit
#' \eqn{j=1\dots|V|} would also include \eqn{j=i} and raise that node to
#' 1.5, so cograph excludes \eqn{j=i}: it is the only reading that
#' reproduces the printed table.
#'
#' Equation (3) sums distances over all of \eqn{V}, which is infinite on a
#' disconnected graph and would leave the index identically zero. cograph
#' sums over the reachable partners instead. This agrees with equation (3)
#' exactly on a connected graph, which is the graph class the source works
#' in, and is a cograph extension outside it. An isolate reaches nobody, so
#' cograph gives it closeness zero, and an isolate invests nowhere, so
#' cograph reads its constraint investment row as zeros; both are cograph
#' conventions.
#'
#' There is no defensible value when a requested index is zero at every node
#' -- betweenness on a complete graph, degree on an edgeless one -- because
#' equation (8) then divides by zero, and none when equation (9)'s
#' denominator \eqn{|V|-1} vanishes on a single node, or when every node is
#' zero on some index and equation (11) divides by zero. All three raise a
#' \code{cograph_undefined_index} error naming the index; none returns
#' zeros. Naming the measure yourself always raises. When a tier such as
#' \code{centrality(x, type = "all")} asked for it instead, that condition
#' becomes a \code{cograph_undefined_measure} warning and an \code{NA}
#' column, so one undefined measure does not take the whole tier down --
#' this is what happens on a complete graph, where the betweenness index of
#' the default set vanishes.
#'
#' Uses the simple undirected unweighted skeleton, which is the source
#' domain: either arc creates one edge, parallel edges count once and loops
#' are removed. Edge weights, \code{mode}, \code{cutoff} and
#' \code{invert_weights} are ignored. Empty graphs return no scores. The
#' base of the logarithm in equation (10) cancels out of equation (11), so
#' the closed form and this implementation are base-free. Raw output already
#' sums to one; \code{normalized = TRUE} divides by the maximum, as
#' elsewhere in \code{\link{centrality}}, so the largest share becomes one
#' and the vector no longer sums to one.
#'
#' Numerical verification establishes agreement with the published equations
#' and the printed Kite tables, not parity with author software, which the
#' source does not offer, nor any claim about spreading performance.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param re_indexes Character vector of constituent indexes, in any order,
#'   without repeats. Default is the source's four-index distinctiveness
#'   set; see the Constituent indexes section for the full vocabulary.
#' @param re_negative Character vector naming which of \code{re_indexes} are
#'   negative, that is, mapped by equation (9). Default \code{NULL} uses the
#'   source's own declarations, which make \code{constraint} and
#'   \code{largest_component} negative and everything else positive. Pass
#'   \code{character(0)} to treat every requested index as positive.
#' @param ... Additional arguments to \code{\link{centrality}}.
#' @return Named numeric vector in input node order, summing to one.
#' @references Chen, B., Wang, Z. and Luo, C. (2016). Integrated evaluation
#'   approach for node importance of complex networks based on relative
#'   entropy. Journal of Systems Engineering and Electronics, 27(6),
#'   1219-1226. Equations 3, 4, 6, 8, 9, 10 and 11 and Tables 1-3.
#'   \doi{10.21629/JSEE.2016.06.10}.
#' @seealso \code{\link{centrality_bridging}} for the nearest existing
#'   cograph measure by rank correlation, and \code{\link{list_centralities}}
#'   for every measure's orientation.
#' @export
#' @examples
#' # The source's own Kite study: four distinctiveness indexes.
#' centrality_relative_entropy(igraph::make_graph("Krackhardt kite"))
#'
#' # Only the two destructiveness indexes of its section 4.2.
#' centrality_relative_entropy(
#'   igraph::make_graph("Krackhardt kite"),
#'   re_indexes = c("n_components", "largest_component")
#' )
#'
#' # Any subset works, and any index can be re-declared negative.
#' centrality_relative_entropy(
#'   igraph::make_tree(7, children = 2, mode = "undirected"),
#'   re_indexes = c("degree", "closeness"), re_negative = "closeness"
#' )
centrality_relative_entropy <- function(x,
                                        re_indexes = c("degree", "closeness",
                                                       "betweenness",
                                                       "constraint"),
                                        re_negative = NULL, ...) {
  df <- centrality(x, measures = "relative_entropy", re_indexes = re_indexes,
                   re_negative = re_negative, ...)
  stats::setNames(df$relative_entropy, df$node)
}
