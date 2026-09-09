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

#' Proximal betweenness centrality
#'
#' Fractions of shortest paths on which a node is the first or last
#' intermediate vertex, following Brandes (2008), section 3.2, Algorithm 3.
#' Paths have unit edge lengths. Each reachable ordered source-destination
#' pair contributes equally, divided among all its shortest paths.
#'
#' The original terminology calls the last intermediate vertex the
#' proximal source (a proxy interacting directly with the destination),
#' and the first intermediate vertex the proximal target. The source
#' variant is the default. Endpoints are excluded, so paths with fewer
#' than two edges contribute nothing. The sum variant counts both roles;
#' the union variant counts a vertex only once when a two-edge path places
#' it in both roles. These are the two combination options in the paper.
#'
#' Raw scores sum over ordered pairs, including on undirected graphs,
#' following the displayed definition and Algorithm 3. They are not halved.
#' Source and target scores agree on undirected graphs; sum is twice either
#' score, whereas union removes the two-edge overlap. This convention is
#' distinct from the usual unordered-pair scaling of undirected betweenness.
#'
#' Uses the simple unweighted graph, retaining edge direction. Loops are
#' removed and repeated edges count once after generic input processing.
#' Weights, mode, inversion and cutoff do not affect this measure. Weighted
#' shortest paths and edge-distinct multigraph paths are outside this
#' implementation's verified domain. Unreachable pairs, isolates and
#' complete graphs contribute zero; empty graphs return no scores.
#'
#' Native breadth-first searches and dependency accumulation take
#' O(n(n+m)) time after the current O(n squared) dense graph preparation.
#' Path counts use double precision; a nonfinite count raises an error
#' instead of returning invalid fractions. Counts above the exact-integer
#' range can be rounded, so numerical equivalence is tolerance-based.
#'
#' @param x Network input accepted by \code{\link{centrality}}.
#' @param proximal_variant One of \code{"source"} (default),
#'   \code{"target"}, \code{"sum"}, or \code{"union"}.
#' @param ... Additional arguments to \code{\link{centrality}}.
#'   \code{normalized = TRUE} divides by the maximum score; all-zero
#'   results remain zero.
#' @return Named numeric vector in input node order.
#' @references Brandes, U. (2008). On variants of shortest-path betweenness
#'   centrality and their generic computation. Social Networks, 30, 136-145.
#'   \doi{10.1016/j.socnet.2007.11.001}. Section 3.2, Algorithm 3;
#'   author preprint dated 12 November 2007, pages 7-8.
#' @export
#' @examples
#' centrality_proximal_betweenness(igraph::make_graph("Zachary"))
#' centrality_proximal_betweenness(igraph::make_ring(5),
#'                               proximal_variant = "union")
# nolint start: object_length_linter.
centrality_proximal_betweenness <- function(x, proximal_variant = "source",
                                            ...) {
  df <- centrality(x, measures = "proximal_betweenness",
                   proximal_variant = proximal_variant, ...)
  stats::setNames(df$proximal_betweenness, df$node)
}
# nolint end: object_length_linter.
