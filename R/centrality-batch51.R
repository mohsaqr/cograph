#' Trust-PageRank (Sheng, Zhu, Wang, Wang and Hou 2020)
#' @keywords internal
#' @noRd
calculate_trust_pagerank <- function(cg, alpha = 0.85, mix = 0.85, decay = 1,
                                     tol = 1e-14, max_iter = 1000L) {
  finite_scalar <- function(v) {
    is.numeric(v) && length(v) == 1L && is.finite(v)
  }
  stopifnot(
    "`tpr_alpha` must be a single number strictly between zero and one" =
      finite_scalar(alpha) && alpha > 0 && alpha < 1,
    "`tpr_k` must be a single number in [0, 1]" =
      finite_scalar(mix) && mix >= 0 && mix <= 1,
    "`tpr_decay` must be a single number in (0, 1]" =
      finite_scalar(decay) && decay > 0 && decay <= 1,
    "`tpr_tol` must be a single positive number" =
      finite_scalar(tol) && tol > 0,
    "`tpr_max_iter` must be a single whole number of at least one" =
      finite_scalar(max_iter) && max_iter >= 1 &&
      isTRUE(all.equal(max_iter, round(max_iter)))
  )

  terms <- .cg_tpr_terms(.cg_path_matrix(cg, NULL), alpha, mix, decay, tol,
                         as.integer(round(max_iter)))
  defined <- attr(terms, "defined")
  converged <- attr(terms, "converged")
  iterations <- attr(terms, "iterations")

  if (!all(converged)) {
    stalled <- names(converged)[!converged]
    warning(warningCondition(
      sprintf(paste0("`trust_pagerank` did not settle: the %s recursion was ",
                     "still moving after %d iterations at relative ",
                     "tolerance %g. The ",
                     "returned scores depend on that bound; raise ",
                     "`tpr_max_iter` or relax `tpr_tol`."),
              paste(stalled, collapse = " and "),
              max(iterations[!converged]), tol),
      class = "cograph_no_converge", call = NULL
    ))
  }
  if (any(!defined)) {
    warning(warningCondition(
      sprintf(paste0("`trust_pagerank` has no value at %d of %d nodes, so ",
                     "those entries are NA: their component has lines but no ",
                     "triangle for the similarity recursion to reach, so the ",
                     "converged similarity is zero on every line there and ",
                     "equation (2) divides zero by zero. Every component ",
                     "that has lines but no triangle is in this class -- a ",
                     "path, a tree, a star, an even cycle, a complete ",
                     "bipartite graph. An isolate is not: it is never a ",
                     "denominator, and it keeps the bare teleport share."),
              sum(!defined), length(defined)),
      class = "cograph_undefined_measure", call = NULL
    ))
  }
  terms$trust_pagerank
}

#' Trust-PageRank
#'
#' Trust-PageRank (Sheng et al. 2020) is a damped PageRank in which a node
#' passes its score to a neighbor in proportion to a trust value. The trust
#' value mixes the SimRank similarity \eqn{s} of the two nodes with a degree
#' ratio:
#' \deqn{T(i,j) = (1-k)\frac{s(i,j)}{\sum_{l \in N_j} s(j,l)}
#'   + k\,\frac{d_i}{\sum_{l \in N_j} d_l}, \qquad
#'   TPR_i = \frac{1-\alpha}{n} + \alpha \sum_{j \in N_i} T(i,j)\,TPR_j .}{
#'   T(i,j) = (1-k) s(i,j) / sum_{l in N_j} s(j,l) + k d_i / sum_{l in N_j} d_l,
#'   TPR_i = (1-alpha)/n + alpha sum_{j in N_i} T(i,j) TPR_j.}
#'
#' @details
#' The measure is computed on the simple undirected skeleton of the
#' network, so direction, weights, loops and parallel edges are ignored. The
#' similarity recursion runs over adjacent pairs and is anchored by
#' triangles. Every node of a component that has edges but no triangle is
#' returned as \code{NA} with a \code{cograph_undefined_measure} warning, and
#' an isolated node scores \eqn{(1-\alpha)/n}{(1-alpha)/n}. Both the similarity and the
#' score iterations stop on a relative tolerance, and an iteration that
#' reaches \code{tpr_max_iter} raises \code{cograph_no_converge}. The
#' Centrality Zoo attributes the measure to Sheng et al., Physica A
#' 541:123262, which defines a different index. The measure is defined in
#' the Algorithms paper cited below.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param ... Further arguments to \code{\link{centrality}}. The measure uses
#'   \code{tpr_alpha} (damping factor, default 0.85), \code{tpr_k} (weight of
#'   the degree ratio, default 0.85), \code{tpr_decay} (SimRank decay
#'   constant, default 1), \code{tpr_tol} (relative tolerance, default
#'   \code{1e-14}) and \code{tpr_max_iter} (default 1000).
#' @return A named numeric vector with one score per node, in input node
#'   order. The scores sum to one on a network with no isolated node and no
#'   undefined component.
#' @references
#' Sheng, J., Zhu, J., Wang, Y., Wang, B. and Hou, Z. (2020). Identifying
#'   Influential Nodes of Complex Networks Based on Trust-Value. Algorithms,
#'   13(11), 280. \doi{10.3390/a13110280}.
#' @seealso \code{\link{centrality_pagerank}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_trust_pagerank(regulation_net)
centrality_trust_pagerank <- function(x, ...) {
  df <- centrality(x, measures = "trust_pagerank", ...)
  stats::setNames(df$trust_pagerank, df$node)
}
