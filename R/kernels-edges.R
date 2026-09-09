# ===========================================================================
# Edge-level centralities, dependency-free
# ===========================================================================

#' Edge betweenness by Brandes accumulation
#'
#' The same single-source traversal as `.cg_betweenness()` (Dijkstra with
#' predecessor lists, ties resolved at 1e-15 so path counts stay integral),
#' but the dependency is credited to the edge `(v, w)` that carries it rather
#' than to the inner vertex. Every edge is reported once, so an undirected
#' graph halves the score, exactly as `igraph::edge_betweenness()` does.
#'
#' @param w Weight matrix in canonical orientation: `w[i, j]` is the length of
#'   the arc `i -> j`, and a symmetric matrix describes an undirected graph.
#' @param n Vertex count.
#' @param directed Whether the graph is directed.
#' @param cutoff Longest path to count; `-1` (default) means no limit. A
#'   pair further apart than the cutoff credits no edge at all.
#' @param edges Integer matrix with two columns naming the edges to report,
#'   in the order the caller wants them. Defaults to row-major order over the
#'   non-zero cells (upper triangle including the diagonal when undirected),
#'   which is the canonical order of a `cg_graph`.
#' @return Numeric vector with one value per row of `edges`. A self-loop is
#'   never on a shortest path and scores 0.
#' @references Brandes, U. (2001). A faster algorithm for betweenness
#'   centrality. *Journal of Mathematical Sociology*, 25(2), 163-177.
#' @keywords internal
#' @noRd
.cg_edge_betweenness <- function(w, n, directed, cutoff = -1, edges = NULL) {
  if (is.null(edges)) {
    nz <- which(w != 0, arr.ind = TRUE)
    if (!directed && nrow(nz)) nz <- nz[nz[, 1L] <= nz[, 2L], , drop = FALSE]
    if (nrow(nz)) nz <- nz[order(nz[, 1L], nz[, 2L]), , drop = FALSE]
    edges <- matrix(as.integer(nz), ncol = 2L)
  }
  m <- nrow(edges)
  if (m == 0L || n <= 1L) return(numeric(m))
  # Dense lookup from an ordered pair to its row in `edges`; an undirected
  # edge answers to both orientations.
  index <- matrix(0L, n, n)
  index[edges] <- seq_len(m)
  if (!directed) index[edges[, 2:1, drop = FALSE]] <- seq_len(m)
  eb <- numeric(m)
  eps <- 1e-15
  limited <- is.numeric(cutoff) && length(cutoff) == 1L && cutoff >= 0
  # One accumulation per source, each depending on its own settle order.
  for (s in seq_len(n)) {
    pred <- vector("list", n)
    sigma <- numeric(n); sigma[s] <- 1
    dist <- rep(Inf, n); dist[s] <- 0
    visited <- rep(FALSE, n)
    order_stack <- integer(0)
    # Settling one vertex depends on every previous settlement.
    for (step in seq_len(n)) {
      cand <- dist; cand[visited] <- Inf
      if (all(is.infinite(cand))) break
      u <- which.min(cand)
      visited[u] <- TRUE
      order_stack <- c(order_stack, u)
      edge <- w[u, ]
      move <- which(!visited & edge > 0)
      # Relaxation must see the tie test per neighbour, so it stays a loop.
      for (v in move) {
        nd <- dist[u] + edge[v]
        if (nd < dist[v] - eps) {
          dist[v] <- nd; sigma[v] <- sigma[u]; pred[[v]] <- u
        } else if (abs(nd - dist[v]) < eps) {
          sigma[v] <- sigma[v] + sigma[u]; pred[[v]] <- c(pred[[v]], u)
        }
      }
    }
    delta <- numeric(n)
    # Dependencies flow back from the farthest vertex, one at a time.
    for (wn in rev(order_stack)) {
      if (limited && dist[wn] > cutoff) next
      p <- pred[[wn]]
      if (length(p) == 0L) next
      credit <- (sigma[p] / sigma[wn]) * (1 + delta[wn])
      eb[index[cbind(p, wn)]] <- eb[index[cbind(p, wn)]] + credit
      delta[p] <- delta[p] + credit
    }
  }
  if (!directed) eb / 2 else eb
}
