# ===========================================================================
# Current-flow, walk and clique centralities
# ===========================================================================

#' Number of connected components (weak connectivity)
#' @param b Binary adjacency matrix.
#' @return Integer count.
#' @keywords internal
#' @noRd
.cg_n_components <- function(b) {
  n <- nrow(b)
  if (n == 0L) return(0L)
  u <- ((b + t(b)) != 0)
  seen <- rep(FALSE, n)
  count <- 0L
  for (s in seq_len(n)) {
    if (seen[s]) next
    count <- count + 1L
    frontier <- s
    seen[s] <- TRUE
    while (length(frontier) > 0L) {
      v <- frontier[1L]; frontier <- frontier[-1L]
      nxt <- which(!seen & u[v, ])
      seen[nxt] <- TRUE
      frontier <- c(frontier, nxt)
    }
  }
  count
}

#' Adjacency used by the Laplacian routines
#' @param w Weight matrix. @param directed Whether directed.
#' @return Numeric matrix with a zero diagonal.
#' @keywords internal
#' @noRd
.cg_adj_la <- function(w, directed) {
  a <- if (directed) w else ifelse(w != 0, w, t(w))
  dim(a) <- dim(w)
  diag(a) <- 0
  a
}

#' Graph Laplacian
#' @inheritParams .cg_adj_la
#' @return Numeric matrix.
#' @keywords internal
#' @noRd
.cg_laplacian_matrix <- function(w, directed) {
  a <- .cg_adj_la(w, directed)
  l <- -a
  diag(l) <- rowSums(a)
  l
}

#' Moore-Penrose inverse of the shifted Laplacian
#'
#' The Laplacian is singular by construction (its rows sum to zero), so it is
#' shifted by 1/n before inversion and falls back to a pseudo-inverse when
#' even that is rank-deficient.
#'
#' @inheritParams .cg_adj_la
#' @param n Vertex count.
#' @return Numeric matrix, or `NULL` if it cannot be formed.
#' @keywords internal
#' @noRd
.cg_flow_inverse <- function(w, n, directed) {
  shifted <- .cg_laplacian_matrix(w, directed) - 1 / n
  out <- tryCatch(solve(shifted), error = function(e) NULL)
  if (!is.null(out)) return(out)
  s <- tryCatch(svd(shifted), error = function(e) NULL)
  if (is.null(s)) return(NULL)
  keep <- s$d > max(dim(shifted)) * .Machine$double.eps * max(s$d)
  if (!any(keep)) return(NULL)
  s$v[, keep, drop = FALSE] %*% (diag(1 / s$d[keep], sum(keep)) %*%
    t(s$u[, keep, drop = FALSE]))
}

#' Current-flow closeness (information centrality of Brandes & Fleischer)
#' @inheritParams .cg_flow_inverse
#' @return Numeric vector; `NaN` on a disconnected graph.
#' @keywords internal
#' @noRd
.cg_current_flow_closeness <- function(w, n, directed) {
  if (n == 0L) return(numeric(0))
  if (n <= 1L) return(rep(NaN, n))
  if (.cg_n_components((w != 0) * 1) > 1L) return(rep(NaN, n))
  inv <- .cg_flow_inverse(w, n, directed)
  if (is.null(inv)) return(rep(NaN, n))
  dg <- diag(inv)
  vapply(seq_len(n), function(i) {
    j <- seq_len(n)[seq_len(n) != i]
    total <- sum(dg[i] + dg[j] - 2 * inv[i, j])
    if (total != 0) (n - 1) / total else NaN
  }, numeric(1L))
}

#' Current-flow betweenness
#' @inheritParams .cg_flow_inverse
#' @return Numeric vector; `NaN` on a disconnected graph.
#' @keywords internal
#' @noRd
.cg_current_flow_betweenness <- function(w, n, directed) {
  if (n == 0L) return(numeric(0))
  if (n <= 2L) return(rep(0, n))
  if (.cg_n_components((w != 0) * 1) > 1L) return(rep(NaN, n))
  inv <- .cg_flow_inverse(w, n, directed)
  if (is.null(inv)) return(rep(NaN, n))
  a <- .cg_adj_la(w, directed)
  bet <- numeric(n)
  # One unit of current per source-target pair; the pairs cannot be collapsed
  # because each induces a different potential field.
  for (s in seq_len(n - 1L)) {
    for (t in (s + 1L):n) {
      potential <- inv[, s] - inv[, t]
      throughput <- 0.5 * rowSums(a * abs(outer(potential, potential, "-")))
      throughput[c(s, t)] <- 0
      bet <- bet + throughput
    }
  }
  bet * 2 / ((n - 1) * (n - 2))
}

#' Communicability betweenness (Estrada, Higham & Hatano 2009)
#'
#' Unlike `.cg_communicability`, the reference for this one is **correct** --
#' it already uses `solve(V)` on asymmetric input. It is reproduced faithfully,
#' singular-eigenbasis `NA` included.
#'
#' @param w Weight matrix. @param n Vertex count.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_communicability_betweenness <- function(w, n) {
  if (n <= 2L) return(rep(0, n))
  a <- (w != 0) * 1
  diag(a) <- 0
  a <- unname(a)
  sym <- isSymmetric(a)
  g <- .cg_expm_eigen(a, sym)
  if (anyNA(g)) return(rep(NA_real_, n))
  inv_g <- ifelse(g > 1e-15, 1 / g, 0)
  diag_mask <- diag(n) == 1
  # The reference aborts the whole call the first time a reduced eigenbasis is
  # singular, so a partial vector would not be reference-faithful: one failed
  # vertex takes the entire result to NA.
  cb <- vapply(seq_len(n), function(r) {
    a_red <- a
    a_red[r, ] <- 0
    a_red[, r] <- 0
    g_red <- .cg_expm_eigen(a_red, sym)
    if (anyNA(g_red)) return(NA_real_)
    ratio <- (g - g_red) * inv_g
    ratio[diag_mask] <- 0
    ratio[r, ] <- 0
    ratio[, r] <- 0
    sum(ratio)
  }, numeric(1L))
  if (anyNA(cb)) return(rep(NA_real_, n))
  denom <- (n - 1) * (n - 2)
  if (denom > 0) cb / denom else cb
}

#' SALSA authority scores
#' SALSA authority scores
#'
#' The stationary vector is taken as the eigenvector whose eigenvalue is
#' nearest 1, not the dominant one. Power iteration converges to the dominant
#' eigenvector instead, which is why an iterative SALSA disagrees with this
#' one on graphs whose authority matrix is not stochastic.
#'
#' SALSA separates hubs from authorities, so it is undefined without direction.
#'
#' @param w Weight matrix. @param n Vertex count. @param directed Whether directed.
#' @return Numeric vector scaled to a maximum of 1; `NA` on undirected input.
#' @keywords internal
#' @noRd
.cg_salsa <- function(w, n, directed = TRUE) {
  if (n == 0L) return(numeric(0))
  if (!directed) return(rep(NA_real_, n))
  a <- (w != 0) * 1
  diag(a) <- 0
  outdeg <- rowSums(a)
  indeg <- colSums(a)
  a_row <- a
  a_row[outdeg > 0, ] <- a[outdeg > 0, , drop = FALSE] / outdeg[outdeg > 0]
  a_col <- a
  a_col[, indeg > 0] <- sweep(a[, indeg > 0, drop = FALSE], 2L, indeg[indeg > 0], "/")
  auth_mat <- crossprod(a_col, a_row)
  e <- eigen(t(auth_mat))
  idx <- which.min(abs(Re(e$values) - 1))
  auth <- abs(Re(e$vectors[, idx]))
  mx <- max(auth)
  if (!(mx > 0)) rep(0, n) else auth / mx
}

#' Cross-clique connectivity: how many maximal-and-partial cliques a vertex joins
#' @param b Binary adjacency matrix.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_cross_clique <- function(b) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  adj <- .cg_adj_distinct(b, "all")
  count <- numeric(n)
  extend <- function(clique, candidates) {
    if (length(clique) > 0L) count[clique] <<- count[clique] + 1
    for (ci in seq_along(candidates)) {
      v <- candidates[ci]
      nxt <- candidates[seq_len(length(candidates)) > ci]
      nxt <- nxt[nxt %in% adj[[v]]]
      extend(c(clique, v), nxt)
    }
  }
  extend(integer(0), seq_len(n))
  count
}

#' Bridging centrality: betweenness weighted by a local bridging coefficient
#' @param b Binary adjacency matrix. @param bw Betweenness vector.
#' @param directed Whether directed.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_bridging <- function(b, bw, directed) {
  deg <- .cg_degree(b, directed, "all")
  nbrs <- .cg_neighbors(b, directed)
  vapply(seq_along(nbrs), function(i) {
    if (deg[i] == 0) return(0)
    denom <- sum(ifelse(deg[nbrs[[i]]] > 0, 1 / deg[nbrs[[i]]], 0))
    bc <- if (denom != 0) (1 / deg[i]) / denom else 0
    bc * bw[i]
  }, numeric(1L))
}
