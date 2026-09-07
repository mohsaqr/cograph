#' BG power from column-normalized binary adjacency
#' @keywords internal
#' @noRd
calculate_beta_measure <- function(g, beta_direction = "positive") {
  if (!is.character(beta_direction) || length(beta_direction) != 1L ||
        is.na(beta_direction) ||
        !beta_direction %in% c("positive", "negative")) {
    stop("beta_direction must be 'positive' or 'negative'.", call. = FALSE)
  }
  a <- .cg_path_matrix(g, NULL)
  diag(a) <- 0
  if (beta_direction == "negative") a <- t(a)
  incoming <- colSums(a)
  inverse <- numeric(length(incoming))
  positive <- incoming > 0
  inverse[positive] <- 1 / incoming[positive]
  as.numeric(a %*% inverse)
}

#' BG-index or beta power measure
#'
#' The positive beta-measure of node i is the sum, over its successors j,
#' of one divided by the in-degree of j. Each node with predecessors shares
#' one unit of domination power equally among those predecessors. This is
#' van den Brink and Gilles' BG-measure (1992, definition 2.1), subsequently
#' called the beta-measure (2000, definition 2.1). The negative variant
#' applies the positive measure to the reversed graph (Boldi and Vigna 2014).
#' It sums reciprocal source out-degrees over incoming neighbors.
#'
#' Uses the simple unweighted graph, retaining direction. Loops and duplicate
#' arcs are removed; weights, mode, inversion and cutoff are ignored.
#' Undirected edges represent reciprocal arcs, so both variants coincide
#' with the sum of reciprocal neighbor degrees. This does not implement the
#' separately defined weighted extension of the original paper.
#'
#' Nodes without successors have positive score zero; nodes without
#' predecessors have negative score zero. Isolates score zero, and empty
#' graphs return no scores. There is no division by a zero degree: every
#' contributing successor has at least one predecessor. Raw positive scores
#' sum to the number of nodes with nonzero in-degree; raw negative scores
#' sum to the number with nonzero out-degree. In disconnected graphs this
#' accounting applies independently to each component.
#'
#' Dense matrix preparation and evaluation take O(n squared) time and memory.
#' The score is an expected number of predecessor selections, not a
#' probability distribution or a stationary random-walk centrality.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param beta_direction Either \code{"positive"} (default, credits sources)
#'   or \code{"negative"} (credits destinations).
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides scores by their maximum; all-zero
#'   scores remain zero. This differs from normalizing to unit total mass.
#' @return Named numeric vector in input node order.
#' @references van den Brink, R. and Gilles, R. P. (1992). Measuring
#'   domination in directed graphs. Tilburg Research Memorandum FEW 565,
#'   definition 2.1 and example 2.2, pp. 3-4.
#'   van den Brink, R. and Gilles, R. P. (2000). Measuring domination in
#'   directed networks. Social Networks, 22, 141-157, definition 2.1.
#'   \doi{10.1016/S0378-8733(00)00019-8}.
#'   Boldi, P. and Vigna, S. (2014). Axioms for centrality. Internet
#'   Mathematics, 10, 222-262. \doi{10.1080/15427951.2013.865686}.
#' @export
#' @examples
#' centrality_beta_measure(igraph::make_graph("Zachary"))
#' centrality_beta_measure(igraph::make_star(5, mode = "out"),
#'                         beta_direction = "negative")
centrality_beta_measure <- function(x, beta_direction = "positive", ...) {
  df <- centrality(x, measures = "beta_measure",
                   beta_direction = beta_direction, ...)
  stats::setNames(df$beta_measure, df$node)
}
