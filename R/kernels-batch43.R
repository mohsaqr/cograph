# ===========================================================================
# Batch 43 kernels: iterative resource allocation (IRA) and its improved
# variant (IIRA).
#
# Both measures share one recursion, I(t + 1) = A I(t) started from
# I(0) = (1, ..., 1), and differ only in how the allocation matrix A is
# built. The helpers here build A and run the recursion; the measure-level
# choices (which mass, which stopping rule) live in R/centrality-batch43.R.
# ===========================================================================

#' Mass vector for the resource-allocation measures
#'
#' `theta` in Ren et al. (2014) eq. (3) and Zhong et al. (2015) eq. (4) is
#' "a certain centrality of node i". Both worked examples use the k-shell
#' index; both papers also list degree. Isolates have mass zero under either
#' choice, and a zero mass is never raised to `alpha`, because an isolate is
#' in no neighbourhood and so never enters the matrix.
#'
#' @param a Simple undirected 0/1 adjacency matrix with a zero diagonal.
#' @param n Vertex count.
#' @param mass `"coreness"` or `"degree"`.
#' @return Numeric vector of masses, one per node.
#' @keywords internal
#' @noRd
.cg_resource_mass <- function(a, n, mass) {
  if (identical(mass, "degree")) rowSums(a) else .cg_coreness(a, n)
}

#' Allocation matrix for the resource-allocation recursion
#'
#' Entry `(i, j)` is the share of node `j`'s resource that flows to `i`.
#' It exists only when `i` is a neighbour of `j`; the share is
#' `spread[i] / sum(weight[u] for u in neighbours(j))`. For IRA
#' `spread == weight == theta^alpha`, so every non-isolate column sums to
#' one; for IIRA `spread` carries the extra factor `psi`, so columns sum to
#' at most `max(psi) < 1`.
#'
#' Only the adjacency support is touched, so an isolate's undefined column
#' denominator is never evaluated and a non-finite mass outside the support
#' cannot leak into the matrix.
#'
#' @param a Simple undirected 0/1 adjacency matrix with a zero diagonal.
#' @param weight Denominator mass per node.
#' @param spread Numerator mass per node; defaults to `weight`.
#' @return Numeric `n` by `n` allocation matrix.
#' @keywords internal
#' @noRd
.cg_resource_matrix <- function(a, weight, spread = weight) {
  n <- nrow(a)
  out <- matrix(0, n, n)
  support <- which(a != 0, arr.ind = TRUE)
  if (!nrow(support)) return(out)
  denominator <- as.numeric(a %*% weight)
  out[support] <- spread[support[, 1L]] / denominator[support[, 2L]]
  out
}

#' Run the resource recursion to a fixed number of steps
#'
#' `I(t) = A^t I(0)` with `I(0) = (1, ..., 1)`, the initial condition both
#' papers state. Written as a `Reduce()` over the step index rather than a
#' loop, since each step needs the previous one.
#'
#' @param m Allocation matrix.
#' @param steps Number of steps; zero returns `I(0)`.
#' @return Numeric vector `I(steps)`.
#' @keywords internal
#' @noRd
.cg_resource_steps <- function(m, steps) {
  Reduce(function(x, ignored) as.numeric(m %*% x), seq_len(steps),
         init = rep(1, nrow(m)))
}

#' Run the resource recursion until it settles, or until a bound
#'
#' Ren et al. (2014) step iii) terminates when
#' `Delta I(t) = |I(t) - I(t - 1)| < epsilon`. The loop is a `while`, not an
#' apply, because the stopping test reads the value the previous step
#' produced; there is no vectorised form of a fixed-point iteration.
#'
#' The iteration provably has no limit on a bipartite component whose two
#' vertex classes differ in size: `A` is then similar to a symmetric matrix
#' with an eigenvalue of exactly -1 whose coefficient in `I(0) = 1` is the
#' class-size difference, so `I(t)` settles into a period-two cycle and
#' `Delta I(t)` settles at a positive constant. That case cannot be silently
#' reported as an equilibrium, so it raises `cograph_no_converge`.
#'
#' @param m Allocation matrix.
#' @param tol Positive tolerance on the maximum absolute change.
#' @param max_iter Iteration bound.
#' @param measure Measure name, for the condition message.
#' @return Numeric vector, the last iterate computed.
#' @keywords internal
#' @noRd
.cg_resource_settle <- function(m, tol, max_iter, measure) {
  x <- rep(1, nrow(m))
  iterations <- 0L
  delta <- Inf
  while (iterations < max_iter && !(delta < tol)) {
    y <- as.numeric(m %*% x)
    iterations <- iterations + 1L
    delta <- max(abs(y - x))
    x <- y
  }
  if (!(delta < tol)) {
    warning(warningCondition(
      sprintf(paste0("`%s` did not settle: after %d iterations the largest ",
                     "change is %g, still at or above the tolerance %g. The ",
                     "returned scores are I(%d) and depend on that bound. ",
                     "The recursion has a period-two limit cycle, and so no ",
                     "equilibrium at all, on a bipartite component whose two ",
                     "vertex classes differ in size."),
              measure, iterations, delta, tol, iterations),
      class = "cograph_no_converge", call = NULL
    ))
  }
  x
}
