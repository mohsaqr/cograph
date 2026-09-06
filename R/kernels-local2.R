# ===========================================================================
# Further local, linear-solve and peeling centralities
# ===========================================================================

#' Neighbour lists under igraph's mode semantics
#'
#' On a directed graph with `mode = "all"` a reciprocated dyad appears
#' **twice**, which is what makes degree, coreness and transitivity agree
#' with igraph. Callers that need the distinct neighbour set must `unique()`.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return A list of integer vectors, one per vertex.
#' @keywords internal
#' @noRd
.cg_adjlist <- function(b, directed, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  n <- nrow(b)
  lapply(seq_len(n), function(i) {
    j <- seq_len(n)
    j <- j[j != i]
    if (!directed) return(j[b[i, j] != 0 | b[j, i] != 0])
    if (identical(mode, "out")) return(j[b[i, j] != 0])
    if (identical(mode, "in")) return(j[b[j, i] != 0])
    sort(c(j[b[i, j] != 0], j[b[j, i] != 0]))
  })
}

#' Katz centrality (Katz 1953)
#' @param m Weight matrix. @param alpha Attenuation factor.
#' @return Numeric vector; `NaN` when `I - alpha A^T` is singular.
#' @keywords internal
#' @noRd
.cg_katz <- function(m, alpha = 0.1) {
  n <- nrow(m)
  if (n == 0L) return(numeric(0))
  if (n == 1L) return(0)          # a lone vertex accrues no attenuated walks
  out <- tryCatch(solve(diag(1, n, n) - alpha * t(m), rep(1, n)),
                  error = function(e) NULL)
  if (is.null(out)) rep(NaN, n) else as.numeric(out)
}

#' Hubbell centrality (Hubbell 1965)
#' @param m Weight matrix. @param factor Scaling applied before the solve.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_hubbell <- function(m, factor = 0.5) {
  n <- nrow(m)
  if (n == 0L) return(numeric(0))
  scaled <- m * factor
  # The Neumann series behind Hubbell only converges when the spectral radius
  # is below 1. Past that, (I - W) can still be inverted numerically while the
  # result means nothing, so refuse rather than return a confident number.
  ev <- tryCatch(eigen(scaled, only.values = TRUE)$values, error = function(e) NULL)
  if (is.null(ev) || any(Re(ev) >= 1 - 1e-10)) return(rep(NA_real_, n))
  out <- tryCatch(solve(diag(1, n, n) - scaled, rep(1, n)), error = function(e) NULL)
  if (is.null(out)) rep(NA_real_, n) else as.numeric(out)
}

#' Laplacian centrality, cograph's formulation
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_laplacian <- function(b, directed) {
  deg <- .cg_degree(b, directed, "all")
  out_adj <- .cg_adjlist(b, directed, if (directed) "out" else "all")
  deg^2 + deg + 2 * vapply(out_adj, function(js) sum(deg[js]), numeric(1L))
}

#' Burt's effective size
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_effective_size <- function(b, directed) {
  adj <- .cg_adjlist(b, directed, "all")
  vapply(seq_along(adj), function(i) {
    nbs <- adj[[i]]
    k <- length(nbs)
    if (k == 0L) return(0)
    red <- sum(vapply(nbs, function(j)
      length(intersect(unique(adj[[j]]), nbs)) / k, numeric(1L)))
    k - red
  }, numeric(1L))
}

#' Topological coefficient
#' @inheritParams .cg_effective_size
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_topological_coefficient <- function(b, directed) {
  adj <- .cg_adjlist(b, directed, "all")
  vapply(seq_along(adj), function(v) {
    nbs <- adj[[v]]
    k <- length(nbs)
    if (k == 0L) return(0)
    common <- integer(0)
    tc <- 0
    for (nb in nbs) {
      for (nn in adj[[nb]]) {
        if (nn == v) next
        tc <- tc + 1
        if (!(nn %in% common)) {
          common <- c(common, nn)
          if (nn %in% nbs) tc <- tc + 1
        }
      }
    }
    if (length(common) == 0L) 0 else tc / (length(common) * k)
  }, numeric(1L))
}

#' Weight diversity: normalised Shannon entropy of a vertex's edge weights
#' @param m Weight matrix. @param weighted Whether weights carry information.
#' @param directed Whether directed.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_diversity <- function(m, weighted, directed) {
  n <- nrow(m)
  if (!weighted) {
    b <- (m != 0) * 1
    return(as.numeric(.cg_degree(b, directed, "all") > 1))
  }
  vapply(seq_len(n), function(i) {
    j <- seq_len(n)[seq_len(n) != i]
    vals <- if (directed) {
      c(abs(m[i, j][m[i, j] != 0]), abs(m[j, i][m[j, i] != 0]))
    } else {
      pick <- ifelse(m[i, j] != 0, m[i, j], m[j, i])
      abs(pick[pick != 0])
    }
    if (length(vals) <= 1L) return(0)
    total <- sum(vals)
    if (total == 0) return(0)
    p <- vals / total
    -sum(p * log2(p)) / log2(length(vals))
  }, numeric(1L))
}

#' Local h-index, iterated to a fixed point
#'
#' Both the neighbour set and the seed degrees are taken at `mode`; pinning
#' either to `"all"` silently answers the undirected question on a directed
#' graph.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_local_hindex <- function(b, directed, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  adj <- .cg_neighbors(b, directed, mode)
  h <- .cg_degree(b, directed, mode)
  hidx <- function(v) {
    s <- sort(v, decreasing = TRUE)
    k <- which(s >= seq_along(s))
    if (length(k) == 0L) 0L else max(k)
  }
  # Iterating to a fixed point; the update is a function of the previous pass.
  for (iter in seq_len(100L)) {
    nxt <- vapply(adj, function(js) if (length(js)) hidx(h[js]) else 0L, numeric(1L))
    if (identical(nxt, h)) return(nxt)
    h <- nxt
  }
  h
}

#' Integration centrality
#' @param b Binary adjacency matrix. @param mode Distance mode.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_integration <- function(b, mode = "all") {
  n <- nrow(b)
  if (n <= 1L) return(rep(0, n))
  d <- .cg_distances(b, mode)
  mx <- .cg_diameter(d)
  if (mx <= 0) return(rep(n, n))
  filled <- ifelse(is.finite(d), d, mx + 1)
  rowSums(1 - (filled - 1) / mx)
}

#' Onion decomposition layers
#' @inheritParams .cg_effective_size
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_onion <- function(b, directed) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  layer <- numeric(n)
  active <- rep(TRUE, n)
  current <- 1L
  left <- n
  # Peeling: each stripped layer changes the degrees deciding the next.
  while (left > 0L) {
    mask <- outer(active, active, "&")
    deg <- .cg_degree(b * mask, directed, "all")
    mn <- min(deg[active])
    repeat {
      mask <- outer(active, active, "&")
      deg <- .cg_degree(b * mask, directed, "all")
      batch <- which(active & deg <= mn)
      if (length(batch) == 0L) break
      layer[batch] <- current
      active[batch] <- FALSE
      left <- left - length(batch)
      current <- current + 1L
    }
  }
  layer
}

#' Local bridging coefficient
#' @inheritParams .cg_effective_size
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_local_bridging <- function(b, directed) {
  deg <- .cg_degree(b, directed, "all")
  nbrs <- .cg_neighbors(b, directed)
  vapply(seq_along(nbrs), function(i) {
    if (deg[i] == 0) return(0)
    denom <- sum(ifelse(deg[nbrs[[i]]] > 0, 1 / deg[nbrs[[i]]], 0))
    if (denom == 0) 0 else (1 / deg[i]) * ((1 / deg[i]) / denom)
  }, numeric(1L))
}

#' Neighbour multiset, mode-aware
#'
#' Under `"all"` on a directed graph a reciprocated dyad appears twice, which
#' is what makes the degree-based measures agree with igraph.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return A list of integer vectors.
#' @keywords internal
#' @noRd
.cg_neighbors <- function(b, directed, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  n <- nrow(b)
  lapply(seq_len(n), function(i) {
    j <- seq_len(n)[seq_len(n) != i]
    if (!directed) return(j[b[i, j] != 0 | b[j, i] != 0])
    switch(mode,
      out = j[b[i, j] != 0],
      `in` = j[b[j, i] != 0],
      all = sort(c(j[b[i, j] != 0], j[b[j, i] != 0])))
  })
}

#' Local reaching centrality (Mones, Vicsek & Vicsek 2012)
#'
#' Three regimes, matching the reference: a directed unweighted graph counts
#' the reachable set; an undirected unweighted graph sums inverse distances;
#' a weighted graph averages edge weights along each shortest path.
#'
#' @param b Binary adjacency matrix. @param m Weight matrix.
#' @param directed Whether directed. @param mode Distance mode.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_reaching_local <- function(b, m, directed, mode = "all") {
  n <- nrow(b)
  if (n <= 1L) return(rep(0, n))
  unweighted <- all(m == 0 | m == 1)
  if (!unweighted) return(.cg_reaching_weighted(m, n, directed, mode))
  if (directed) {
    d <- .cg_distances(b, mode)
    return(rowSums(is.finite(d) & .cg_offdiag(d)) / (n - 1))
  }
  d <- .cg_distances(b, "all")
  ok <- .cg_offdiag(d) & is.finite(d) & d > 0
  rowSums(ifelse(ok, 1 / d, 0)) / (n - 1)
}

#' Weighted local reaching centrality
#'
#' Paths are found by minimising total/weight, so a heavy edge is cheap to
#' traverse; the score then averages the traversed weights. Note the
#' reciprocated-dyad rule differs from the distance kernel: here the
#' **larger** of the two directions is taken, not the smaller.
#'
#' @param m Weight matrix. @param n Vertex count.
#' @param directed Whether directed. @param mode Traversal mode.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_reaching_weighted <- function(m, n, directed, mode = "all") {
  total <- sum(pmax(0, m))
  if (total <= 0) return(rep(0, n))
  ow <- .cg_original_weights(m, mode)
  trav <- if (!directed) .cg_original_weights(m, "all")
          else if (identical(mode, "in")) t(m) else m
  cost <- ifelse(ow > 0, total / ow, Inf)
  vapply(seq_len(n), function(src) {
    dist <- rep(Inf, n); dist[src] <- 0
    prev <- rep(NA_integer_, n)
    used <- rep(FALSE, n)
    for (step in seq_len(n)) {
      cand <- dist; cand[used] <- Inf
      if (all(is.infinite(cand))) break
      # Settle the LAST vertex holding the minimum, not the first. When two
      # routes tie, which one survives is decided by the order the reference's
      # binary heap pops equal keys, and that order favours the later-inserted
      # vertex more often than not.
      #
      # This is a PARTIAL match, not parity: across randomised integer-weight
      # graphs (where exact ties are common) it agrees with the reference on
      # roughly 76% of cases, against roughly 50% for which.min(). True parity
      # would mean simulating igraph's two-way indexed heap, whose pop order
      # for equal keys depends on its internal sift operations. Continuous
      # weights essentially never tie, which is why all 99 fixtures pass under
      # either rule.
      best <- min(cand)
      tied <- which(cand <= best + 1e-12 * max(abs(best), 1))
      u <- tied[length(tied)]
      used[u] <- TRUE
      reach <- !used & is.finite(cost[u, ])
      if (!any(reach)) next
      nd <- dist[u] + cost[u, ]
      upd <- reach & nd < dist
      dist[upd] <- nd[upd]; prev[upd] <- u
    }
    acc <- 0
    for (target in seq_len(n)) {
      if (target == src || !is.finite(dist[target])) next
      path <- target
      while (path[1L] != src) {
        p <- prev[path[1L]]
        if (is.na(p)) { path <- integer(0); break }
        path <- c(p, path)
      }
      if (length(path) < 2L) next
      steps <- seq_len(length(path) - 1L)
      acc <- acc + sum(trav[cbind(path[steps], path[steps + 1L])]) / length(steps)
    }
    acc / (n - 1)
  }, numeric(1L))
}

#' Edge weight with reciprocated dyads resolved by the larger side
#' @param m Weight matrix. @param mode One of "all", "out", "in".
#' @return Numeric matrix.
#' @keywords internal
#' @noRd
.cg_original_weights <- function(m, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  if (identical(mode, "out")) return(m)
  if (identical(mode, "in")) return(t(m))
  a <- m; b <- t(m)
  w <- ifelse(a > 0 & b > 0, pmax(a, b), pmax(a, b))
  dim(w) <- dim(m); dimnames(w) <- dimnames(m)
  w
}
