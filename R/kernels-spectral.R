# ===========================================================================
# Spectral and linear-solve centralities, dependency-free
# ===========================================================================
# These are matrix algebra, so base R expresses them directly: eigen() and
# solve() replace the hand-rolled Jacobi/Gauss routines a JS port needs.

#' Binary adjacency with the diagonal cleared
#' @param w Weight matrix. @param directed Whether to fold in the transpose.
#' @return A numeric matrix with a zero diagonal. `0/1` when `directed` is
#'   `FALSE`; when `TRUE` the transpose is **added**, so a reciprocated dyad
#'   becomes `2` rather than `1`.
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
#' Total communicability (Benzi & Klymko 2013), `rowSums(expm(A))` on the
#' binary adjacency with the diagonal cleared. `.cg_expm()` handles both
#' symmetric and directed `A`.
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
  rowSums(.cg_expm(unname(a)))
}

#' Matrix exponential of a square numeric matrix
#'
#' A symmetric matrix takes the eigendecomposition fast path
#' `V diag(exp(lambda)) t(V)`, which is exact up to rounding. Any other matrix
#' goes through scaling and squaring with a diagonal Pade approximant of
#' degree 6 (Moler & Van Loan 2003, method 3; Golub & Van Loan, Algorithm
#' 11.3.1): `A` is scaled by `2^-s` until its 1-norm is at most 1/2, the
#' approximant `D^-1 N` is formed, and the result is squared `s` times. The
#' denominator `D` is well conditioned at that norm, so no eigenvector matrix
#' is ever inverted and a defective or nearly defective `A` (common for
#' directed graphs) is handled correctly.
#'
#' @param a Numeric square matrix.
#' @return A numeric matrix of the same dimension.
#' @references Moler, C., & Van Loan, C. (2003). Nineteen dubious ways to
#'   compute the exponential of a matrix, twenty-five years later. SIAM
#'   Review, 45(1), 3-49.
#' @keywords internal
#' @noRd
.cg_expm <- function(a) {
  n <- nrow(a)
  if (n == 0L) return(matrix(0, 0L, 0L))
  if (isSymmetric(unname(a))) {
    e <- eigen(a, symmetric = TRUE)
    return(e$vectors %*% (exp(e$values) * t(e$vectors)))
  }
  norm1 <- max(colSums(abs(a)))
  squarings <- if (norm1 > 0.5) max(0L, as.integer(ceiling(log2(norm1 / 0.5)))) else 0L
  x <- a / 2^squarings
  q <- 6L
  # Pade coefficients c_k = (2q - k)! q! / ((2q)! k! (q - k)!), by recursion.
  coefs <- cumprod(c(1, vapply(seq_len(q), function(k)
    (q - k + 1) / (k * (2 * q - k + 1)), numeric(1L))))
  # Powers X^0 .. X^q; each depends on the previous one.
  powers <- Reduce(function(p, k) p %*% x, seq_len(q),
                   accumulate = TRUE, init = diag(1, n, n))
  signs <- (-1)^(seq_len(q + 1L) - 1L)
  num <- Reduce(`+`, Map(function(p, ck) ck * p, powers, coefs))
  den <- Reduce(`+`, Map(function(p, ck, sg) sg * ck * p, powers, coefs, signs))
  out <- solve(den, num)
  # Undo the scaling: exp(A) = exp(A / 2^s)^(2^s).
  Reduce(function(m, i) m %*% m, seq_len(squarings), init = out)
}

#' Orient a matrix so the walk kernels read the ties `mode` asks for
#'
#' The walk kernels read one fixed direction: `.cg_alpha()` sums incoming
#' ties (`t(a)` inside) and `.cg_power()` outgoing ties (`a` as given).
#' `native` names that direction. Asking for the other one transposes the
#' matrix; `"all"` uses the symmetrized network, `a + t(a)` for weights
#' (the summed collapse of `igraph::as.undirected()`) or the binary skeleton
#' when `binary = TRUE`. Undirected input is returned unchanged. The
#' diagonal is cleared.
#'
#' @param a Square numeric matrix. @param directed Whether directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @param native The direction the kernel reads, `"in"` or `"out"`.
#' @param binary Symmetrize to the 0/1 skeleton instead of summing.
#' @return A numeric matrix with a zero diagonal.
#' @keywords internal
#' @noRd
.cg_mode_matrix <- function(a, directed, mode = c("all", "out", "in"),
                            native = c("in", "out"), binary = FALSE) {
  mode <- match.arg(mode)
  native <- match.arg(native)
  diag(a) <- 0
  if (!directed || identical(mode, native)) return(a)
  if (identical(mode, "all")) {
    s <- a + t(a)
    return(if (binary) (s != 0) * 1 else s)
  }
  t(a)
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
