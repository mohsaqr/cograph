#' BG power from column-normalized binary adjacency
#' @keywords internal
#' @noRd
calculate_beta_measure <- function(cg, beta_direction = "positive") {
  if (!is.character(beta_direction) || length(beta_direction) != 1L ||
        is.na(beta_direction) ||
        !beta_direction %in% c("positive", "negative")) {
    stop("beta_direction must be 'positive' or 'negative'.", call. = FALSE)
  }
  a <- .cg_path_matrix(cg, NULL)
  diag(a) <- 0
  if (beta_direction == "negative") a <- t(a)
  incoming <- colSums(a)
  inverse <- numeric(length(incoming))
  positive <- incoming > 0
  inverse[positive] <- 1 / incoming[positive]
  as.numeric(a %*% inverse)
}

#' Beta Measure
#'
#' The beta measure, or BG-index (van den Brink and Gilles 2000), gives
#' node \eqn{i}{i} the sum, over its successors \eqn{j}{j}, of one divided
#' by the in-degree of \eqn{j}{j}. Each node with predecessors thus shares
#' one unit of domination power equally among them:
#' \deqn{\beta_i = \sum_{j : i \to j} \frac{1}{d^{in}_j}.}{
#'   beta_i = sum_{j: i -> j} 1 / d_in(j).}
#' The negative variant applies the measure to the reversed network (Boldi
#' and Vigna 2014).
#'
#' @details
#' The measure is computed on the simple unweighted network with direction
#' kept, so weights, loops and parallel edges are ignored. An undirected
#' edge is a pair of reciprocal arcs, so on an undirected network both
#' variants equal the sum of the reciprocal degrees of the neighbors. A
#' node without successors has positive score zero, and a node without
#' predecessors has negative score zero. The positive scores sum to the
#' number of nodes with nonzero in-degree, and the negative scores sum to
#' the number of nodes with nonzero out-degree.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param beta_direction \code{"positive"} (default) credits the sources
#'   of arcs. \code{"negative"} credits their targets.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' van den Brink, R. and Gilles, R. P. (2000). Measuring domination in
#'   directed networks. Social Networks, 22, 141-157.
#'   \doi{10.1016/S0378-8733(00)00019-8}.
#'
#' Boldi, P. and Vigna, S. (2014). Axioms for centrality. Internet
#'   Mathematics, 10, 222-262. \doi{10.1080/15427951.2013.865686}.
#' @seealso \code{\link{centrality_indegree}},
#'   \code{\link{centrality_prestige_domain}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_beta_measure(regulation_net)
centrality_beta_measure <- function(x, beta_direction = "positive", ...) {
  df <- centrality(x, measures = "beta_measure",
                   beta_direction = beta_direction, ...)
  stats::setNames(df$beta_measure, df$node)
}
