# ===========================================================================
# Spectral and linear-solve centralities, dependency-free
# ===========================================================================
# These are matrix algebra, so base R expresses them directly: eigen() and
# solve() replace the hand-rolled Jacobi/Gauss routines a JS port needs.

#' Binary adjacency with the diagonal cleared
#' @param w Weight matrix. @param directed Whether to fold in the transpose.
#' @return A numeric 0/1 matrix.
#' @keywords internal
#' @noRd
.cg_binary <- function(w, directed) {
  a <- (w != 0) * 1
  diag(a) <- 0
  if (directed) a + t(a) else a
}

#' Subgraph centrality (Estrada & Rodriguez-Velazquez 2005)
#' @param w Weight matrix. @param n Vertex count. @param directed Logical.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_subgraph <- function(w, n, directed) {
  if (n == 0L) return(numeric(0))
  if (n == 1L) return(1)
  a <- .cg_binary(w, directed)
  e <- eigen(a, symmetric = TRUE)
  as.numeric((e$vectors^2) %*% exp(e$values))
}

#' Communicability centrality: row sums of the matrix exponential
#'
#' Mirrors `calculate_communicability()` exactly, **including a defect**: the
#' identity `expm(A) = V diag(exp(lambda)) t(V)` holds only for symmetric `A`,
#' yet `t(V)` is applied unconditionally, so directed graphs get a wrong
#' answer. Since these kernels are wired into `centrality()`, deviating here
#' would silently change cograph's public output; the fix belongs in the
#' reference and in this kernel together, not in one of them.
#'
#' @param w Weight matrix. @param n Vertex count.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_communicability <- function(w, n) {
  if (n == 0L) return(numeric(0))
  if (n == 1L) return(1)
  a <- (w != 0) * 1
  diag(a) <- 0
  sym <- isSymmetric(unname(a))
  e <- eigen(a, symmetric = sym)
  vecs <- Re(e$vectors)
  rowSums(vecs %*% (diag(exp(Re(e$values)), n, n) %*% t(vecs)))
}

#' Matrix exponential by eigendecomposition, reference-compatible
#'
#' Uses `t(V)` when the input is symmetric and `solve(V)` otherwise, matching
#' `.expm_sym()` in the reference. A computationally singular eigenbasis
#' propagates as `NA` rather than being rescued by a Pade expansion, because
#' that is what the reference reports.
#'
#' @param mm Numeric matrix. @param symmetric Whether `mm` is symmetric.
#' @return A numeric matrix, or a matrix of `NA` when the basis is singular.
#' @keywords internal
#' @noRd
.cg_expm_eigen <- function(mm, symmetric) {
  e <- eigen(mm, symmetric = symmetric)
  v <- Re(e$vectors)
  ev <- exp(Re(e$values))
  if (symmetric) return(v %*% (ev * t(v)))
  inv <- tryCatch(solve(v), error = function(err) NULL)
  if (is.null(inv)) return(matrix(NA_real_, nrow(mm), ncol(mm)))
  v %*% diag(ev, nrow = nrow(mm)) %*% inv
}

#' Alpha centrality (Bonacich & Lloyd 2001)
#' @param a Adjacency matrix. @param n Vertex count. @param alpha Attenuation.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_alpha <- function(a, n, alpha) {
  if (n == 0L) return(numeric(0))
  if (any(a < 0, na.rm = TRUE)) {
    stop(errorCondition(
      "Alpha centrality needs non-negative weights; found a negative edge.",
      class = "cograph_negative_weights", call = NULL))
  }
  sys <- diag(1, n, n) - alpha * t(a)
  out <- tryCatch(solve(sys, rep(1, n)), error = function(e) NULL)
  if (is.null(out)) rep(NaN, n) else as.numeric(out)
}

#' Bonacich power centrality
#' @param b Binary adjacency matrix. @param n Vertex count. @param alpha Attenuation.
#' @return Numeric vector, rescaled so the sum of squares is `n`.
#' @keywords internal
#' @noRd
.cg_power <- function(b, n, alpha) {
  if (n == 0L) return(numeric(0))
  sys <- diag(1, n, n) - alpha * b
  ev <- tryCatch(solve(sys, rowSums(b)), error = function(e) NULL)
  if (is.null(ev)) return(rep(NaN, n))
  sum_sq <- sum(ev^2)
  # An edgeless graph has nothing to rescale by; igraph propagates the 0/0
  # as NaN rather than reporting a spurious zero centrality.
  if (!(sum_sq > 0)) return(rep(NaN, n))
  as.numeric(ev) * sqrt(n / sum_sq)
}
