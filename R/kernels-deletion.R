# ===========================================================================
# Deletion-based and neighbourhood centralities
# ===========================================================================
# Several of these score a vertex by what the graph loses when it is removed,
# so each one costs an all-pairs solve per vertex.

#' Graph entropy after deleting each vertex
#' @param b Binary adjacency matrix. @param mode Distance mode.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_entropy <- function(b, mode = "all") {
  n <- nrow(b)
  vapply(seq_len(n), function(drop) {
    keep <- seq_len(n)[seq_len(n) != drop]
    sub <- b[keep, keep, drop = FALSE]
    m <- length(keep)
    if (m <= 1L) return(0)
    d <- .cg_distances(sub, mode)
    total <- (sum(is.finite(d)) - m) / 2
    if (total <= 0) return(0)
    y <- (rowSums(is.finite(d)) - 1) / total
    y <- y[y > 0]
    -sum(y * log2(y))
  }, numeric(1L))
}

#' Semi-local centrality: the two-step neighbourhood mass of one's neighbours
#' @param adj Neighbour lists.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_semilocal <- function(adj) {
  n <- length(adj)
  nb2 <- vapply(seq_len(n), function(i) {
    seen <- unique(c(i, adj[[i]], unlist(adj[adj[[i]]], use.names = FALSE)))
    length(seen) - 1
  }, numeric(1L))
  vapply(seq_len(n), function(v)
    sum(vapply(adj[[v]], function(u) sum(nb2[adj[[u]]]), numeric(1L))), numeric(1L))
}

#' Closeness vitality: the total distance a graph gains when a vertex leaves
#' @param m Weight matrix. @param mode Distance mode. @param full_dist Distances
#'   on the intact graph.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_closeness_vitality <- function(m, mode = "all", full_dist) {
  n <- nrow(m)
  if (n <= 1L) return(rep(NaN, n))
  full <- sum(full_dist[is.finite(full_dist)])
  vapply(seq_len(n), function(drop) {
    keep <- seq_len(n)[seq_len(n) != drop]
    d <- .cg_distances(m[keep, keep, drop = FALSE], mode)
    full - sum(d[is.finite(d)])
  }, numeric(1L))
}

#' Local average connectivity
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode Neighbour mode.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_lac <- function(b, directed, mode = "all") {
  adj <- .cg_adjlist(b, directed, mode)
  vapply(adj, function(nbs) {
    k <- length(nbs)
    if (k == 0L) return(0)
    sub_nodes <- unique(nbs)
    sub <- b[sub_nodes, sub_nodes, drop = FALSE]
    sum(.cg_degree(sub, directed, mode)) / k
  }, numeric(1L))
}

#' Information centrality (Stephenson & Zelen 1989)
#'
#' Isolates are dropped before the solve, because a disconnected vertex makes
#' the information matrix singular and would take the whole result to `NaN`.
#'
#' @param m Weight matrix. @param weighted Whether weights are used.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_information <- function(m, weighted) {
  n <- nrow(m)
  if (n == 0L) return(numeric(0))
  if (n == 1L) return(0)
  a_side <- ifelse(m != 0, if (weighted) m else 1, 0)
  b_side <- t(a_side)
  mm <- (a_side + b_side) / 2
  diag(mm) <- NA_real_
  non_iso <- which(vapply(seq_len(n), function(i)
    any(!is.na(mm[i, ]) & mm[i, ] != 0), logical(1L)))
  if (length(non_iso) == 0L) return(rep(0, n))
  k <- length(non_iso)
  sub <- mm[non_iso, non_iso, drop = FALSE]
  row_sum <- rowSums(sub, na.rm = TRUE)
  a <- ifelse(sub == 0, 1, 1 - sub)
  diag(a) <- 1 + row_sum
  # sna::infocent inverts with a deliberately loose tolerance, so matrices
  # that a default solve() rejects still yield a result. Genuine failure is
  # reported as NA; zero is reserved for a graph with no non-isolates at all.
  cn <- tryCatch(solve(a, tol = 1e-20), error = function(e) NULL)
  if (is.null(cn)) return(rep(NA_real_, n))
  tr <- sum(diag(cn))
  rows <- rowSums(cn)
  out <- numeric(n)
  out[non_iso] <- 1 / (diag(cn) + (tr - 2 * rows) / k)
  out
}

#' Pairwise disconnectivity (Potapov et al. 2008)
#' Counts directed paths, so it is undefined without direction.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @return Numeric vector: the fraction of directed paths a vertex's removal
#'   costs; `NA` throughout on undirected input.
#' @keywords internal
#' @noRd
.cg_pairwisedis <- function(b, directed = TRUE) {
  n <- nrow(b)
  if (!directed) return(rep(NA_real_, n))
  full <- sum(is.finite(.cg_distances(b, "out"))) - n
  if (full <= 0) return(rep(0, n))
  vapply(seq_len(n), function(drop) {
    keep <- seq_len(n)[seq_len(n) != drop]
    sub <- b[keep, keep, drop = FALSE]
    paths <- sum(is.finite(.cg_distances(sub, "out"))) - length(keep)
    (full - paths) / full
  }, numeric(1L))
}

#' VoteRank (Zhang et al. 2016)
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @return Numeric vector, rescaled so the first-elected vertex scores 1.
#' @keywords internal
#' @noRd
.cg_voterank <- function(b, directed) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  avg <- max(1, sum(.cg_degree(b, directed, "all")) / n)
  ability <- rep(1, n)
  selected <- rep(FALSE, n)
  rank <- numeric(n)
  # Each election depends on the previous winner's suppression of its
  # neighbours, so the rounds cannot be vectorised.
  for (r in seq_len(n)) {
    votes <- vapply(seq_len(n), function(v) {
      if (selected[v]) return(-1)
      voters <- if (directed) which(!selected & b[, v] != 0)
                else which(!selected & (b[, v] != 0 | b[v, ] != 0))
      sum(ability[voters])
    }, numeric(1L))
    if (all(votes < 0)) break
    best <- which.max(votes)
    selected[best] <- TRUE
    rank[best] <- r
    hit <- if (directed) which(b[best, ] != 0)
           else which(b[best, ] != 0 | b[, best] != 0)
    ability[hit] <- pmax(0, ability[hit] - 1 / avg)
  }
  mx <- max(rank)
  if (mx == 0) rep(0, n) else (mx + 1 - rank) / mx
}
