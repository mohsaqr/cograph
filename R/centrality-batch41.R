#' Relative-entropy integrated evaluation (Chen, Wang and Luo 2016)
#'
#' Maps each requested index to a discrete distribution with equation (8) or
#' equation (9) and integrates them with the closed form of equation (11),
#' the normalized geometric mean. The geometric mean is taken in logarithms
#' so that a product of `m` small shares cannot underflow; an exact zero
#' survives as `exp(-Inf) = 0`.
#'
#' @param cg Internal cograph structure, as built by `centrality()`.
#' @param re_indexes Character vector of constituent index names.
#' @param re_negative Character vector of the indexes to treat as negative,
#'   or `NULL` for the source's own declarations.
#' @return Numeric vector summing to one, one entry per node.
#' @keywords internal
#' @noRd
calculate_relative_entropy <- function(cg,
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
  a <- .cg_undirected_view(.cg_path_matrix(cg, NULL))
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
#' The relative-entropy integrated evaluation (Chen, Wang and Luo 2016)
#' combines several centrality indexes into one score. Each index is turned
#' into a distribution over the nodes, and the score is the distribution with
#' the smallest total relative entropy to all of them, which is their
#' normalized geometric mean:
#' \deqn{w_i = \frac{\prod_{j=1}^{m} u_{ji}^{1/m}}
#'   {\sum_{l} \prod_{j=1}^{m} u_{jl}^{1/m}}}{
#'   w_i = prod_j u_ji^(1/m) / sum_l prod_j u_jl^(1/m)}
#' A positive index (larger is more important) maps to
#' \eqn{u_i = C_i / \sum_j C_j}{u_i = C_i / sum_j C_j}, and a negative index
#' (smaller is more important) maps to the normalized complement
#' \eqn{1 - C_i / \sum_j C_j}{1 - C_i / sum_j C_j}.
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the network,
#' so direction, weights and loops are ignored. The available indexes are
#' \code{"degree"}, \code{"closeness"} (the reciprocal of the distance sum
#' over reachable partners), \code{"betweenness"} (summed over ordered
#' pairs), \code{"constraint"} (the source's equation 6, whose outer sum runs
#' over every other node), \code{"n_components"} and
#' \code{"largest_component"} (the components left after deleting the node).
#' The scores sum to one, and a node that scores zero on any index scores
#' zero overall. An index that is zero at every node, such as betweenness on
#' a complete graph, raises a \code{cograph_undefined_index} error when the
#' measure is requested by name. Inside \code{type = "all"} the same case
#' gives a \code{cograph_undefined_measure} warning and an \code{NA} column.
#' The source's equation 6 differs from Burt's constraint as computed by
#' \code{igraph::constraint()}.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param re_indexes Character vector of constituent indexes, without
#'   repeats. The default \code{c("degree", "closeness", "betweenness",
#'   "constraint")} is the source's four-index set.
#' @param re_negative Character vector naming the members of
#'   \code{re_indexes} treated as negative. The default \code{NULL} uses the
#'   source's declarations, which make \code{constraint} and
#'   \code{largest_component} negative. \code{character(0)} treats every
#'   index as positive.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order, summing to one. With \code{normalized = TRUE} the scores are
#'   divided by their maximum and no longer sum to one.
#' @references
#' Chen, B., Wang, Z. and Luo, C. (2016). Integrated evaluation approach for
#'   node importance of complex networks based on relative entropy. Journal of
#'   Systems Engineering and Electronics, 27(6), 1219-1226.
#'   \doi{10.21629/JSEE.2016.06.10}.
#' @seealso \code{\link{centrality_bridging}},
#'   \code{\link{list_centralities}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_relative_entropy(regulation_net)
centrality_relative_entropy <- function(x,
                                        re_indexes = c("degree", "closeness",
                                                       "betweenness",
                                                       "constraint"),
                                        re_negative = NULL, ...) {
  df <- centrality(x, measures = "relative_entropy", re_indexes = re_indexes,
                   re_negative = re_negative, ...)
  stats::setNames(df$relative_entropy, df$node)
}
