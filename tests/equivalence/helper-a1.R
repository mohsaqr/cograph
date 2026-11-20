# Reference-side helpers of the spectral, walk and flow centrality tests, in base R. Nothing here calls cograph:
# each function computes a quantity from its published definition, by a route of its own.

#' A vector rescaled so that its largest absolute value is 1 and that value is positive
#'
#' Eigenvector-type scores are defined up to a factor; cograph and igraph report them with maximum 1, NetworkX
#' with unit Euclidean norm (eigenvector) or unit sum (HITS). Rescaling the reference changes no ratio.
#'
#' @param values A named numeric vector.
#' @return `values / values[which.max(abs(values))]`, names kept.
ref_unit_max <- function(values) {
  stopifnot("`values` must be numeric with a non-zero entry" = is.numeric(values) && any(values != 0))
  values / values[[which.max(abs(values))]]
}

#' Mean first-passage times of the simple random walk, one linear system per target
#'
#' For a target j, the hitting times h solve h_j = 0 and h_i = 1 + sum_k P_ik h_k for i != j (Kemeny and Snell
#' 1960, ch. 4), with P the row-normalised 0/1 adjacency. This is a different route from the fundamental-matrix
#' formula m_ij = (Z_jj - Z_ij) / pi_j.
#'
#' @param graph A square matrix with dimnames; its non-zero cells are the steps of the walk.
#' @return A square matrix, m[i, j] the expected number of steps from i to first reach j; zero diagonal.
ref_mfpt <- function(graph) {
  adjacency <- (graph != 0) * 1
  diag(adjacency) <- 0
  transition <- adjacency / rowSums(adjacency)
  n <- nrow(graph)
  passage <- vapply(seq_len(n), \(target) {
    others <- seq_len(n)[-target]
    hitting <- numeric(n)
    hitting[others] <- solve(diag(1, n - 1L) - transition[others, others, drop = FALSE], rep(1, n - 1L))
    hitting
  }, numeric(n))
  dimnames(passage) <- dimnames(graph)
  passage
}

#' Stationary distribution of a Markov chain, from the left eigenvector for eigenvalue 1
#'
#' @param transition A row-stochastic square matrix of an irreducible chain.
#' @return The stationary probabilities, summing to 1.
ref_stationary <- function(transition) {
  decomposition <- eigen(t(transition))
  vector <- Re(decomposition$vectors[, which.min(Mod(decomposition$values - 1))])
  vector / sum(vector)
}

#' The h-index of a set of numbers: the largest h such that h of them are at least h
#'
#' @param values Non-negative numbers.
#' @return One whole number.
ref_hindex <- function(values) {
  sorted <- sort(values, decreasing = TRUE)
  sum(sorted >= seq_along(sorted))
}

#' Moore-Penrose pseudo-inverse of a graph Laplacian from its eigen-decomposition
#'
#' L+ = sum over the non-zero eigenvalues lambda_k of v_k v_k' / lambda_k (a connected graph has one zero
#' eigenvalue, with the constant eigenvector).
#'
#' @param weights A symmetric non-negative weight matrix of a connected graph.
#' @return The pseudo-inverse, with the dimnames of `weights`.
ref_laplacian_pinv <- function(weights) {
  laplacian <- diag(rowSums(weights)) - weights
  decomposition <- eigen(laplacian, symmetric = TRUE)
  keep <- decomposition$values > 1e-9 * max(decomposition$values)
  vectors <- decomposition$vectors[, keep, drop = FALSE]
  pinv <- vectors %*% (t(vectors) / decomposition$values[keep])
  dimnames(pinv) <- dimnames(weights)
  pinv
}

#' Effective resistances of a connected graph, R_ij = L+_ii + L+_jj - 2 L+_ij (Klein and Randic 1993)
#'
#' @inheritParams ref_laplacian_pinv
#' @return A symmetric matrix of resistances, zero diagonal.
ref_resistance <- function(weights) {
  pinv <- ref_laplacian_pinv(weights)
  outer(diag(pinv), diag(pinv), `+`) - 2 * pinv
}
