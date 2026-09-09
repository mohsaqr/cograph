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
#' @param pair_weight `NULL` for ordinary betweenness, where every separated
#'   pair counts once. Otherwise a function of the pair distance returning
#'   the weight that pair carries, which is what turns the same traversal
#'   into the length-scaled and distance-decayed variants (Brandes 2008,
#'   Algorithm 5). The weight depends only on the distance between the two
#'   endpoints, so it is shared by every inner vertex of a geodesic.
#' @return Numeric vector.
#' @references Brandes, U. (2001). A faster algorithm for betweenness
#'   centrality. *Journal of Mathematical Sociology*, 25(2), 163-177.
#' @keywords internal
#' @noRd
.cg_betweenness <- function(w, n, directed, cutoff = -1,
                            pair_weight = NULL) {
  if (n <= 2L) return(rep(0, n))
  has_edge <- w > 0
  if (is.null(pair_weight) && all(w[has_edge] == 1)) {
    return(.cg_betweenness_batched(has_edge, n, directed, cutoff))
  }
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
      credit <- if (is.null(pair_weight)) 1 else pair_weight(dist[wn])
      if (length(p) > 0L) {
        delta[p] <- delta[p] + (sigma[p] / sigma[wn]) * (credit + delta[wn])
      }
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


#' Brandes betweenness for binary graphs, all sources at once
#'
#' Path counts and dependencies are accumulated one hop layer at a time for
#' every source simultaneously, so the work is a handful of n x n matrix
#' products per unit of diameter instead of n interpreted searches. Exact
#' for unit weights (integer path counts); identical to the per-source
#' kernel, including the `cutoff` rule (nodes beyond the cutoff neither
#' receive nor propagate credit).
#'
#' @param has_edge Logical adjacency, row = from, col = to.
#' @param n Vertex count.
#' @param directed Whether directed (undirected scores are halved).
#' @param cutoff Non-negative path-length cutoff, or negative for none.
#' @return Numeric vector of betweenness scores.
#' @keywords internal
#' @noRd
.cg_betweenness_batched <- function(has_edge, n, directed, cutoff = -1) {
  a <- has_edge * 1
  d <- .cg_hop_matrix(has_edge, n)
  if (is.numeric(cutoff) && length(cutoff) == 1L && cutoff >= 0) d[d > cutoff] <- Inf
  finite <- is.finite(d)
  max_layer <- if (any(finite)) max(d[finite]) else 0
  if (max_layer < 1) return(numeric(n))
  sigma <- diag(1, n)
  # Forward: layer k path counts are sums over layer k-1 predecessors.
  for (k in seq_len(max_layer)) {
    sigma <- sigma + ((sigma * (d == k - 1)) %*% a) * (d == k)
  }
  safe_sigma <- sigma
  safe_sigma[safe_sigma == 0] <- 1
  delta <- matrix(0, n, n)
  # Backward: dependencies flow from layer k to its layer k-1 predecessors.
  for (k in rev(seq_len(max_layer))) {
    term <- ((1 + delta) / safe_sigma) * (d == k)
    delta <- delta + (term %*% t(a)) * sigma * (d == k - 1)
  }
  delta[cbind(seq_len(n), seq_len(n))] <- 0
  cb <- colSums(delta)
  if (!directed) cb / 2 else cb
}
