#' Proximal betweenness on a simple directed hop graph
#' @keywords internal
#' @noRd
calculate_proximal_betweenness <- function(cg, variant = "source") {
  variant <- match.arg(variant, c("source", "target", "sum", "union"))
  b <- .cg_path_matrix(cg, NULL)
  diag(b) <- 0
  n <- nrow(b)
  neighbors <- .cg_adjlist(b, directed = TRUE, mode = "out")
  source_score <- target_score <- union_score <- numeric(n)
  for (s in seq_len(n)) {
    distance <- rep(-1L, n)
    distance[s] <- 0L
    count <- numeric(n)
    count[s] <- 1
    pred <- vector("list", n)
    queue <- integer(n)
    queue[1L] <- s
    first <- last <- 1L
    while (first <= last) {
      v <- queue[first]
      first <- first + 1L
      for (w in neighbors[[v]]) {
        if (distance[w] < 0L) {
          distance[w] <- distance[v] + 1L
          last <- last + 1L
          queue[last] <- w
        }
        if (distance[w] == distance[v] + 1L) {
          count[w] <- count[w] + count[v]
          if (!is.finite(count[w])) {
            stop("Proximal betweenness shortest-path count overflow.",
                 call. = FALSE)
          }
          pred[[w]] <- c(pred[[w]], v)
        }
      }
    }
    dependency <- numeric(n)
    for (w in rev(queue[seq_len(last)])) {
      p <- pred[[w]]
      if (!length(p)) next
      fraction <- count[p] / count[w]
      dependency[p] <- dependency[p] + fraction * (1 + dependency[w])
      interior <- p != s
      if (any(interior)) {
        v <- p[interior]
        credit <- fraction[interior]
        source_score[v] <- source_score[v] + credit
        # Target credits already cover every two-edge path. Add only
        # longer paths to the union, avoiding subtraction of the overlap.
        if (distance[w] > 2L) union_score[v] <- union_score[v] + credit
      } else {
        target_score[w] <- target_score[w] + dependency[w]
      }
    }
  }
  switch(variant, source = source_score, target = target_score,
         sum = source_score + target_score, union = union_score + target_score)
}

#' Proximal Betweenness Centrality
#'
#' Proximal betweenness (Brandes 2008, section 3.2) is the fraction of
#' shortest paths on which a node is the last intermediate node before the
#' destination (proximal source) or the first intermediate node after the
#' source (proximal target). Each reachable ordered pair of nodes
#' contributes one unit, divided equally among its shortest paths.
#'
#' @details
#' Shortest paths are counted with unit edge lengths on the simple network,
#' so direction is kept and weights, loops and parallel edges are ignored.
#' Ordered pairs are counted on undirected networks as well, so the scores
#' are not halved. Endpoints are excluded, and paths with fewer than two
#' edges contribute nothing. On an undirected network the source and
#' target variants agree, and \code{"sum"} is twice either of them. The
#' \code{"union"} variant counts a node once when a two-edge path makes it
#' both proximal source and proximal target. Isolated nodes and every node
#' of a complete graph score zero.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param proximal_variant \code{"source"} (default), \code{"target"},
#'   \code{"sum"} or \code{"union"}.
#' @param ... Further arguments to \code{\link{centrality}}, such as
#'   \code{normalized} (divide by the maximum, default \code{FALSE}).
#' @return A named numeric vector with one score per node, in input node
#'   order.
#' @references
#' Brandes, U. (2008). On variants of shortest-path betweenness centrality
#'   and their generic computation. Social Networks, 30, 136-145.
#'   \doi{10.1016/j.socnet.2007.11.001}.
#' @seealso \code{\link{centrality_betweenness}},
#'   \code{\link{centrality_stress}}, \code{\link{centrality}}.
#' @export
#' @examples
#' centrality_proximal_betweenness(regulation_net)
# nolint start: object_length_linter.
centrality_proximal_betweenness <- function(x, proximal_variant = "source",
                                            ...) {
  df <- centrality(x, measures = "proximal_betweenness",
                   proximal_variant = proximal_variant, ...)
  stats::setNames(df$proximal_betweenness, df$node)
}
# nolint end: object_length_linter.
