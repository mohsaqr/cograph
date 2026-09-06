# ===========================================================================
# Path-based centralities, dependency-free
# ===========================================================================

#' Brandes betweenness on a weighted graph
#'
#' Brandes (2001) with Dijkstra settling and predecessor accumulation. Ties
#' between equally short paths are resolved at 1e-15, matching the reference
#' implementation, so shortest-path counts stay integral.
#'
#' @param w Weight matrix, already mode-adjusted.
#' @param n Vertex count.
#' @param directed Whether the graph is directed; undirected halves the score.
#' @param cutoff Longest path to count; `-1` (default) means no limit. A
#'   vertex only earns credit for pairs it separates within that distance.
#' @return Numeric vector.
#' @references Brandes, U. (2001). A faster algorithm for betweenness
#'   centrality. *Journal of Mathematical Sociology*, 25(2), 163-177.
#' @keywords internal
#' @noRd
.cg_betweenness <- function(w, n, directed, cutoff = -1) {
  if (n <= 2L) return(rep(0, n))
  cb <- numeric(n)
  eps <- 1e-15
  # One accumulation per source, each depending on its own settle order.
  for (s in seq_len(n)) {
    pred <- vector("list", n)
    sigma <- numeric(n); sigma[s] <- 1
    dist <- rep(Inf, n); dist[s] <- 0
    visited <- rep(FALSE, n)
    order_stack <- integer(0)
    for (step in seq_len(n)) {
      cand <- dist; cand[visited] <- Inf
      if (all(is.infinite(cand))) break
      u <- which.min(cand)
      visited[u] <- TRUE
      order_stack <- c(order_stack, u)
      edge <- w[u, ]
      move <- which(!visited & edge > 0)
      for (v in move) {
        nd <- dist[u] + edge[v]
        if (nd < dist[v] - eps) {
          dist[v] <- nd; sigma[v] <- sigma[u]; pred[[v]] <- u
        } else if (abs(nd - dist[v]) < eps) {
          sigma[v] <- sigma[v] + sigma[u]; pred[[v]] <- c(pred[[v]], u)
        }
      }
    }
    limited <- is.numeric(cutoff) && length(cutoff) == 1L && cutoff >= 0
    delta <- numeric(n)
    for (wn in rev(order_stack)) {
      if (limited && dist[wn] > cutoff) next
      p <- pred[[wn]]
      if (length(p) > 0L) delta[p] <- delta[p] + (sigma[p] / sigma[wn]) * (1 + delta[wn])
      if (wn != s) cb[wn] <- cb[wn] + delta[wn]
    }
  }
  if (!directed) cb / 2 else cb
}

#' Closeness, harmonic and eccentricity from a distance matrix
#'
#' igraph's conventions: closeness divides by the summed distance to
#' *reachable* vertices only and is `NaN` for a vertex that reaches nothing;
#' eccentricity is the largest finite distance in the row.
#'
#' @param d Distance matrix. @param n Vertex count.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_closeness <- function(d, n) {
  if (n <= 1L) return(rep(NaN, n))
  ok <- .cg_offdiag(d) & is.finite(d)
  total <- rowSums(ifelse(ok, d, 0))
  ifelse(total == 0, NaN, 1 / total)
}

#' @keywords internal
#' @noRd
.cg_harmonic <- function(d, n) {
  if (n <= 1L) return(rep(0, n))
  ok <- .cg_offdiag(d) & is.finite(d) & d > 0
  rowSums(ifelse(ok, 1 / d, 0))
}

#' @keywords internal
#' @noRd
.cg_eccentricity <- function(d, n) {
  if (n <= 1L) return(rep(0, n))
  apply(d, 1L, function(r) {
    fin <- r[is.finite(r)]
    if (length(fin) == 0L) 0 else max(fin)
  })
}

#' Global transitivity (the clustering coefficient of the whole graph)
#'
#' Three times the triangle count over the number of connected triples. This
#' is a single number for the graph, not a per-vertex vector, which is why it
#' cannot be produced by averaging the local coefficients.
#'
#' @param b Binary adjacency matrix.
#' @return A single number; `NaN` when the graph has no connected triple.
#' @keywords internal
#' @noRd
.cg_global_transitivity <- function(b) {
  u <- ((b + t(b)) != 0) * 1
  diag(u) <- 0
  deg <- rowSums(u)
  triples <- sum(deg * (deg - 1) / 2)
  if (triples == 0) return(NaN)
  triangles <- sum(diag(u %*% u %*% u)) / 6
  3 * triangles / triples
}
