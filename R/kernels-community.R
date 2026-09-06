# ===========================================================================
# Community-aware and directed-prestige centralities
# ===========================================================================

#' Expected influence (Robinaugh, Millner & McNally 2016)
#'
#' Keeps the sign of each edge rather than its magnitude, which is the whole
#' point on signed networks: a strong negative tie should pull influence down,
#' not add to it.
#'
#' @param m Weight matrix. @param mode One of `"all"`, `"out"`, `"in"`.
#' @param step 1 for one-step, 2 for two-step influence.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_expected_influence <- function(m, mode = c("all", "out", "in"), step = 1L) {
  mode <- match.arg(mode)
  n <- nrow(m)
  if (n == 0L) return(numeric(0))
  row_s <- rowSums(m)
  col_s <- colSums(m)
  ei1 <- switch(mode, out = row_s, `in` = col_s, all = row_s + col_s - diag(m))
  if (step == 1L) return(as.numeric(ei1))
  out2 <- as.numeric(m %*% ei1)
  in2 <- as.numeric(crossprod(m, ei1))
  as.numeric(ei1 + switch(mode,
    out = out2, `in` = in2, all = out2 + in2 - diag(m) * ei1))
}

#' Participation coefficient (Guimera & Amaral 2005)
#' @param adj Neighbour lists. @param deg Degree vector.
#' @param membership Integer community labels, or `NULL`.
#' @return Numeric vector; all `NaN` when no partition is supplied.
#' @keywords internal
#' @noRd
.cg_participation <- function(adj, deg, membership = NULL) {
  n <- length(adj)
  if (is.null(membership)) return(rep(NaN, n))
  vapply(seq_len(n), function(i) {
    if (deg[i] == 0) return(0)
    counts <- table(membership[adj[[i]]])
    1 - sum((as.numeric(counts) / deg[i])^2)
  }, numeric(1L))
}

#' Within-module degree z-score (Guimera & Amaral 2005)
#' @param adj Neighbour lists. @param membership Integer community labels.
#' @return Numeric vector; `NaN` where a module has no spread.
#' @keywords internal
#' @noRd
.cg_within_module_z <- function(adj, membership = NULL) {
  n <- length(adj)
  if (is.null(membership)) return(rep(NaN, n))
  kw <- vapply(seq_len(n), function(i)
    sum(membership[adj[[i]]] == membership[i]), numeric(1L))
  vapply(seq_len(n), function(i) {
    group <- kw[membership == membership[i]]
    sdv <- stats::sd(group)
    # A module whose members all have the same within-degree has no scale on
    # which to express a z-score.
    if (is.na(sdv) || sdv == 0) return(NaN)
    (kw[i] - mean(group)) / sdv
  }, numeric(1L))
}

#' Domain prestige and proximity prestige (Wasserman & Faust)
#' Prestige is defined by who can reach you, so it is meaningless without
#' direction; an undirected graph yields `NA` rather than a symmetric number
#' dressed up as prestige.
#'
#' @param b Binary adjacency matrix. @param proximity Whether to scale by distance.
#' @param directed Whether the graph is directed.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_prestige_domain <- function(b, proximity = FALSE, directed = TRUE) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  if (!directed) return(rep(NA_real_, n))
  d <- .cg_distances(b, "out")
  vapply(seq_len(n), function(v) {
    incoming <- d[, v][is.finite(d[, v])]
    r <- length(incoming) - 1
    if (!proximity) return(r)
    s <- sum(incoming)
    if (r > 0 && s > 0) (r * r) / (s * (n - 1)) else 0
  }, numeric(1L))
}

#' LeaderRank
#'
#' PageRank with a ground node joined to every vertex in both directions,
#' which removes the need for a damping factor and makes the walk ergodic.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @return Numeric vector; `NA` on undirected input, where the ranking is
#'   not defined.
#' @keywords internal
#' @noRd
.cg_leaderrank <- function(b, directed = TRUE) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  if (!directed) return(rep(NA_real_, n))
  ext <- matrix(0, n + 1L, n + 1L)
  ext[seq_len(n), seq_len(n)] <- b
  ext[n + 1L, seq_len(n)] <- 1
  ext[seq_len(n), n + 1L] <- 1
  outdeg <- rowSums(ext)
  outdeg[outdeg == 0] <- 1
  p <- ext / outdeg
  v <- c(rep(1, n), 0)
  # Power iteration on the ground-extended chain.
  for (iter in seq_len(1000L)) {
    nxt <- as.numeric(crossprod(p, v))
    err <- sum(abs(nxt - v) / pmax(abs(v), 1e-15)) / (n + 1)
    v <- nxt
    if (err < 2e-5) break
  }
  v[seq_len(n)] + v[n + 1L] / n
}

#' Trophic level
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @return Numeric vector; `NA` on undirected input or a singular system.
#' @keywords internal
#' @noRd
.cg_trophic_level <- function(b, directed = TRUE) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  if (!directed) return(rep(NA_real_, n))
  a <- b
  diag(a) <- 0
  indeg <- colSums(a)
  indeg[indeg == 0] <- 1
  wmat <- sweep(a, 2L, indeg, "/")
  out <- tryCatch(solve(diag(1, n, n) - t(wmat), rep(1, n)), error = function(e) NULL)
  if (is.null(out)) rep(NaN, n) else as.numeric(out)
}
