# ===========================================================================
# Shortest-path accumulation centralities
# ===========================================================================
# load, stress and percolation all sweep the shortest-path DAG back from the
# far end, differing only in what they accumulate. They share one traversal.

#' Shortest-path DAG from one source
#'
#' Returns settle order, predecessor lists, path counts and distances -- the
#' four things every Brandes-style accumulation needs.
#'
#' Ties between equally short paths are judged on a **scale-relative**
#' tolerance, `tol * pmax(|a|, |b|, 1)`, matching the reference. An absolute
#' epsilon silently stops recognising ties once the weights are large: at a
#' distance of 2e9 the reference tolerance is about 30, while a fixed 1.5e-8
#' treats two genuinely tied routes as distinct.
#'
#' @param w Mode-adjusted weight matrix. @param source Source index.
#' @param n Vertex count. @param tol Relative tie tolerance.
#' @return A list with `ordered`, `pred`, `sigma` and `dist`.
#' @keywords internal
#' @noRd
.cg_sp_dag <- function(w, source, n, tol = 0) {
  dist <- rep(Inf, n); dist[source] <- 0
  sigma <- numeric(n); sigma[source] <- 1
  pred <- vector("list", n)
  used <- rep(FALSE, n)
  ordered <- integer(0)
  # Settling is sequential; the relaxation within each step is vectorised.
  for (step in seq_len(n)) {
    cand <- dist; cand[used] <- Inf
    if (all(is.infinite(cand))) break
    u <- which.min(cand)
    used[u] <- TRUE
    ordered <- c(ordered, u)
    edge <- w[u, ]
    for (v in which(!used & edge > 0)) {
      nd <- dist[u] + edge[v]
      # An unreached target is always a strict improvement; scaling by an
      # infinite current distance would give Inf - Inf.
      eps <- if (is.finite(dist[v])) tol * max(abs(nd), abs(dist[v]), 1) else 0
      if (nd < dist[v] - eps) {
        dist[v] <- nd; sigma[v] <- sigma[u]; pred[[v]] <- u
      } else if (abs(nd - dist[v]) <= eps) {
        sigma[v] <- sigma[v] + sigma[u]; pred[[v]] <- c(pred[[v]], u)
      }
    }
  }
  list(ordered = ordered, pred = pred, sigma = sigma, dist = dist)
}

#' Stress centrality: the raw count of shortest paths through a vertex
#'
#' Unlike betweenness this does not divide by the number of shortest paths
#' between each pair, so a vertex on many equally short routes scores high
#' rather than being diluted.
#'
#' @param w Mode-adjusted weight matrix. @param n Vertex count.
#' @param directed Whether directed. @param weighted Whether weighted.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_stress <- function(w, n, directed, weighted) {
  if (n <= 1L) return(rep(0, n))
  tol <- if (weighted) sqrt(.Machine$double.eps) else 0
  stress <- numeric(n)
  for (s in seq_len(n)) {
    dag <- .cg_sp_dag(w, s, n, tol)
    delta <- numeric(n)
    ord <- dag$ordered[order(dag$dist[dag$ordered], dag$ordered, decreasing = TRUE)]
    for (wn in ord) {
      if (!is.finite(dag$dist[wn]) || wn == s || dag$sigma[wn] == 0) next
      p <- dag$pred[[wn]]
      if (length(p) == 0L) next
      factor <- (dag$sigma[wn] + delta[wn]) / dag$sigma[wn]
      delta[p] <- delta[p] + dag$sigma[p] * factor
    }
    delta[s] <- 0
    stress <- stress + delta
  }
  if (!directed) stress / 2 else stress
}

#' Load centrality
#'
#' Follows sna's convention, which differs from betweenness in three ways
#' that all matter: directed graphs are **reversed** first; every reachable
#' vertex is seeded with unit load; and flow divides **equally** among
#' predecessors rather than in proportion to shortest-path counts. There is
#' no undirected halving.
#'
#' @param w Mode-adjusted weight matrix (already reversed if directed).
#' @param n Vertex count. @param directed Whether directed.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_load <- function(w, n, directed) {
  if (n == 0L) return(numeric(0))
  if (n == 1L) return(0)
  d_all <- .cg_distances(w, "out")
  out <- numeric(n)
  for (s in seq_len(n)) {
    dist_s <- d_all[s, ]
    reachable <- which(is.finite(dist_s) & seq_len(n) != s)
    if (length(reachable) == 0L) { out[s] <- out[s] + 1; next }
    ordered_nodes <- reachable[order(dist_s[reachable])]
    pred <- vector("list", n)
    for (wn in ordered_nodes) {
      v <- which(w[, wn] > 0)
      on_path <- v[abs(dist_s[wn] - dist_s[v] - w[cbind(v, wn)]) < 1e-10]
      pred[[wn]] <- on_path
    }
    delta <- numeric(n)
    delta[c(s, ordered_nodes)] <- 1
    for (wn in rev(ordered_nodes)) {
      p <- pred[[wn]]
      if (length(p) > 0L) delta[p] <- delta[p] + delta[wn] / length(p)
    }
    out <- out + delta
  }
  out
}

#' Percolation centrality (Piraveenan, Prokopenko & Hossain 2013)
#'
#' Betweenness weighted by how percolated the source is relative to the rest.
#' The accumulation runs over reachable vertices only -- the source itself is
#' excluded, which is what keeps a vertex from crediting its own state.
#'
#' @param w Mode-adjusted weight matrix. @param n Vertex count.
#' @param states Per-vertex percolation states in `[0, 1]`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_percolation <- function(w, n, states = NULL) {
  if (n == 0L) return(numeric(0))
  if (n <= 2L) return(rep(0, n))
  st <- if (is.null(states)) rep(1, n) else states
  st[is.na(st)] <- 1
  st <- pmax(0, pmin(1, st))
  total <- sum(st)
  if (total == 0) return(rep(0, n))
  d_all <- .cg_distances(w, "out")
  out <- numeric(n)
  for (s in seq_len(n)) {
    if (st[s] == 0) next
    dist_s <- d_all[s, ]
    reachable <- which(is.finite(dist_s) & seq_len(n) != s)
    if (length(reachable) == 0L) next
    ordered_nodes <- reachable[order(dist_s[reachable])]
    sigma <- numeric(n); sigma[s] <- 1
    pred <- vector("list", n)
    for (wn in ordered_nodes) {
      v <- which(w[, wn] > 0)
      on_path <- v[abs(dist_s[wn] - dist_s[v] - w[cbind(v, wn)]) < 1e-10]
      pred[[wn]] <- on_path
      sigma[wn] <- sigma[wn] + sum(sigma[on_path])
    }
    delta <- numeric(n)
    for (wn in rev(ordered_nodes)) {
      if (sigma[wn] > 0) {
        coeff <- (1 + delta[wn]) / sigma[wn]
        p <- pred[[wn]]
        if (length(p) > 0L) delta[p] <- delta[p] + sigma[p] * coeff
      }
      denom <- total - st[wn]
      if (denom > 0) out[wn] <- out[wn] + delta[wn] * (st[s] / denom)
    }
  }
  out / (n - 2)
}

#' Spanning-tree centrality
#'
#' Built from the shifted Laplacian; a disconnected graph has no spanning
#' tree at all, so the whole vector is `NaN` rather than partially filled.
#'
#' @param m Weight matrix. @param n Vertex count.
#' @param directed Whether directed. @param weighted Whether weighted.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_spanning_tree <- function(m, n, directed, weighted) {
  if (n <= 1L) return(rep(1, n))
  if (.cg_n_components((m != 0) * 1) > 1L) return(rep(NaN, n))
  a <- .cg_adj_la(m, directed)
  vals <- if (weighted) a else (a != 0) * 1
  l <- -vals
  diag(l) <- rowSums(vals)
  inv <- tryCatch(solve(l + 1 / n), error = function(e) NULL)
  if (is.null(inv)) return(rep(NaN, n))
  dgv <- diag(inv) - 1 / n
  ifelse(dgv > 1e-15, 1 / dgv, 0)
}
