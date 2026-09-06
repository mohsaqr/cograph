# ===========================================================================
# Neighbourhood-aggregate centralities
# ===========================================================================

#' Distinct neighbour set (no reciprocation multiplicity)
#'
#' Distinct from [.cg_adjlist()], which repeats a reciprocated dyad. Measures
#' that aggregate *over vertices* need this one; measures that aggregate over
#' *edges* need the other.
#'
#' @param b Binary adjacency matrix. @param mode One of `"all"`, `"out"`, `"in"`.
#' @return A list of integer vectors.
#' @keywords internal
#' @noRd
.cg_adj_distinct <- function(b, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  n <- nrow(b)
  lapply(seq_len(n), function(i) {
    j <- seq_len(n)[seq_len(n) != i]
    keep <- switch(mode,
      out = b[i, j] != 0,
      `in` = b[j, i] != 0,
      all = b[i, j] != 0 | b[j, i] != 0)
    j[keep]
  })
}

#' h-index of a numeric vector
#' @param v Numeric vector.
#' @return Integer-valued numeric.
#' @keywords internal
#' @noRd
.cg_hindex <- function(v) {
  s <- sort(v, decreasing = TRUE)
  k <- which(s >= seq_along(s))
  if (length(k) == 0L) 0 else max(k)
}

#' k-reach: how many vertices lie within distance k
#' @param d Distance matrix. @param n Vertex count. @param k Radius.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_kreach <- function(d, n, k = 3) {
  rowSums(.cg_offdiag(d) & d <= k)
}

#' Diffusion centrality, cograph's formulation
#'
#' Own scaled degree plus the scaled degrees of its neighbours. Note this is
#' **not** the Banerjee power series: reciprocated dyads are collapsed with a
#' logical OR so a mutual edge is not counted twice in the neighbour sum,
#' even though the degree itself counts it twice.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode Degree mode. @param lambda Scaling applied to degree.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_diffusion <- function(b, directed, mode = "all", lambda = 1) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  d <- .cg_degree(b, directed, mode) * lambda
  adj <- if (directed && identical(mode, "all")) ((b + t(b)) != 0) * 1
         else if (directed && identical(mode, "in")) t(b)
         else b
  as.numeric(d + adj %*% d)
}

#' Lobby index: h-index over the closed neighbourhood's degrees
#'
#' The neighbourhood is *closed* -- the vertex counts itself -- and both the
#' degrees and the neighbour set are taken at `mode`.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_lobby <- function(b, directed = FALSE, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  bb <- (b != 0) * 1
  diag(bb) <- 0
  deg <- .cg_degree(bb, directed, mode)
  nbrs <- .cg_adjlist(bb, directed, mode)
  vapply(seq_along(nbrs), function(i)
    .cg_hindex(c(deg[i], deg[nbrs[[i]]])), numeric(1L))
}

#' h-index over strengths rather than degrees
#' @param nbrs Neighbour lists. @param strength Strength vector.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_hindex_strength <- function(nbrs, strength) {
  vapply(seq_along(nbrs), function(i)
    .cg_hindex(c(strength[i], strength[nbrs[[i]]])), numeric(1L))
}

#' ClusterRank (Chen et al. 2013)
#' @param clust Local transitivity. @param adj Neighbour lists (with multiplicity).
#' @param deg Degree vector.
#' @return Numeric vector; `NaN` wherever local transitivity is undefined.
#' @keywords internal
#' @noRd
.cg_clusterrank <- function(clust, adj, deg) {
  vapply(seq_along(adj), function(i) {
    if (is.nan(clust[i])) return(NaN)
    clust[i] * sum(deg[adj[[i]]] + 1)
  }, numeric(1L))
}

#' Maximum neighbourhood component
#' @param b Binary adjacency matrix. @param nbrs Neighbour lists.
#' @return Numeric vector: the largest connected component among a vertex's
#'   neighbours.
#' @keywords internal
#' @noRd
.cg_mnc <- function(b, nbrs) {
  u <- ((b + t(b)) != 0) * 1
  diag(u) <- 0
  vapply(nbrs, function(raw) {
    nodes <- unique(raw)
    if (length(nodes) == 0L) return(0)
    if (length(nodes) == 1L) return(1)
    sub <- u[nodes, nodes, drop = FALSE]
    .cg_largest_component(sub)
  }, numeric(1L))
}

#' Size of the largest connected component
#' @param u Symmetric binary matrix.
#' @return A single number.
#' @keywords internal
#' @noRd
.cg_largest_component <- function(u) {
  n <- nrow(u)
  if (n <= 1L) return(n)
  seen <- rep(FALSE, n)
  best <- 0
  for (s in seq_len(n)) {
    if (seen[s]) next
    frontier <- s
    seen[s] <- TRUE
    size <- 0
    while (length(frontier) > 0L) {
      v <- frontier[1L]
      frontier <- frontier[-1L]
      size <- size + 1
      nxt <- which(!seen & u[v, ] != 0)
      seen[nxt] <- TRUE
      frontier <- c(frontier, nxt)
    }
    best <- max(best, size)
  }
  best
}

#' Expected degree of a vertex's neighbours
#' @param adj Distinct neighbour lists. @param deg Degree vector.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_expected <- function(adj, deg) {
  vapply(adj, function(js) sum(deg[js]), numeric(1L))
}

#' Collective influence at radius two (Morone & Makse 2015)
#' @param d Distance matrix. @param deg Degree vector.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_collective_influence <- function(d, deg) {
  shell <- d == 2
  (deg - 1) * rowSums(ifelse(shell, rep(deg - 1, each = nrow(d)), 0))
}
