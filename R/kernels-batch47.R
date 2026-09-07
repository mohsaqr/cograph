#' Reachability closure of a directed adjacency matrix
#'
#' `TRUE` at `[s, t]` when `t` is reachable from `s` by a directed walk of
#' length zero or more, so the diagonal is always `TRUE`. Computed by
#' repeated squaring of the clamped indicator, which reaches the fixed
#' point in `ceiling(log2(n))` BLAS-backed products rather than the `n`
#' interpreted rounds a Floyd-Warshall closure would take. Clamping to
#' `0`/`1` after every product keeps the entries bounded, so nothing
#' overflows on a dense graph.
#'
#' The support of the randomized-shortest-paths fundamental matrix
#' `Z = sum_k W^k` is exactly this closure, because `W` is nonnegative and
#' shares its support with the adjacency matrix. Taking the closure
#' structurally rather than testing `Z > 0` matters: a LAPACK solve on a
#' block-diagonal system need not return exact zeros off the blocks, and
#' the reciprocal of a spurious `1e-18` is not a number this measure can
#' survive.
#'
#' @param a Adjacency matrix; any positive entry counts as an arc.
#' @return Logical `n x n` matrix with a `TRUE` diagonal.
#' @keywords internal
#' @noRd
.cg_reach_closure <- function(a) {
  reach <- (a > 0)
  diag(reach) <- TRUE
  # Each squaring at least doubles the reachable radius, so the loop runs
  # at most ceiling(log2(n)) + 1 times; vectorising it away is not possible
  # because every round consumes the previous round's closure.
  repeat {
    nxt <- ((reach * 1) %*% (reach * 1)) > 0
    if (identical(nxt, reach)) break
    reach <- nxt
  }
  reach
}

#' Simple randomized shortest paths betweenness (Kivimaki et al. 2016)
#'
#' Kivimaki, Lebichot, Saramaki and Saerens (2016), *Scientific Reports*
#' 6:19668, put a Boltzmann distribution over the absorbing `s`-`t` walks of
#' a graph, tilted by an inverse temperature `beta` away from the reference
#' random walk and towards low-cost walks. Journal page 5, equation (6),
#' builds the fundamental matrix of that killed random walk,
#'
#' ```
#' P^ref = D^-1 A            (reference transition probabilities)
#' W     = P^ref o exp(-beta C)          o = elementwise product
#' Z     = (I - W)^-1
#' ```
#'
#' and page 6, equations (8) and (14), define the score as the expected
#' number of visits to `i` summed over every ordered source-target pair,
#'
#' ```
#' n_i(s, t) = (z_si / z_st - z_ti / z_tt) z_it              (14)
#' bet_i     = sum_{s = 1..n} sum_{t = 1..n} n_i(s, t)        (8)
#' ```
#'
#' Page 7, equation (15) and Algorithm 1, collapse the double sum into one
#' dense expression, `bet = diag(Z (Z' - n Diag(Z'))^T Z)` with `Z'` the
#' elementwise reciprocal of `Z`. That closed form is what is implemented
#' here, in the masked shape below, and the two agree to machine precision
#' whenever the graph is strongly connected.
#'
#' **The closed form is only valid when every pair is connected, and the
#' paper says what to do otherwise.** Under equation (9) the paper states
#' that its derivation "holds only if there exists a path from `s` to `t`.
#' Otherwise, naturally, `n_ij(s, t) = 0`", and Algorithm 1 takes a
#' strongly connected graph as its input. When some `t` is unreachable from
#' some `s` the entry `z_st` is exactly zero and `Z'` is not defined, so
#' equation (15) cannot be evaluated as printed. This kernel applies the
#' source's own zero rule to the whole term of the pair, giving
#'
#' ```
#' M[s, t] = 1 / z_st  when t is reachable from s, else 0
#' r[t]    = number of s from which t is reachable
#' term1_i = [Z M^T Z]_ii
#' term2_i = sum_t z_it z_ti r[t] / z_tt
#' bet_i   = term1_i - term2_i
#' ```
#'
#' On a strongly connected graph every pair is reachable, `M` is `Z'` and
#' every `r[t]` is `n`, so this reduces to equation (15) exactly. The mask
#' has to be applied to **both** halves of the term, because both come from
#' the same `n_i(s, t)` of equation (14); masking only the reciprocal
#' leaves the `n Diag(Z')` half counting sources that contribute nothing,
#' which is what `NetworkToolbox::rspbc()` does and is why cograph and that
#' function part company on a disconnected graph.
#'
#' The consequence is that the measure becomes **component-local**: a node's
#' score depends only on its own strongly connected reach, so two disjoint
#' triangles score exactly what one triangle scores and adding an isolate
#' changes nothing else.
#'
#' **Zero-out-degree nodes.** `D^-1` is undefined at out-degree zero. The
#' row of `P^ref` is written as **zero**, which is the same object the paper
#' already calls `W` -- a killed random walk, page 5 -- read at a node where
#' the walker dies immediately. `Z` then has `z_ii = 1` and zeros elsewhere
#' in that row, and the score works out to exactly `1 - 1 = 0`. An isolate
#' therefore scores a *derived* zero, not an imputed one, and a singleton
#' and an edgeless graph score zero at every node for the same reason.
#' `NetworkToolbox::rspbc()` raises an error here instead.
#'
#' `(I - W)` is never singular under the parameter domain enforced below.
#' `W` is nonnegative with `rowSums(W) = sum_j P_ij exp(-beta C_ij)`, which
#' is at most `1` and is strictly below `1` on every row carrying an arc,
#' because `beta > 0` and every arc cost is strictly positive under both
#' cost conventions. Rows with no arc are identically zero. The spectral
#' radius of `W` is therefore below one and the Neumann series converges.
#' What can still fail is *underflow*: `z_st` decays roughly like
#' `exp(-beta d(s, t))`, so a large `beta` on a long path drives a
#' structurally positive entry to zero in double precision and its
#' reciprocal to infinity. That is surfaced as a
#' `cograph_rsp_underflow` error naming `beta`, never silently zeroed.
#'
#' **`Z` is set to exactly zero at the unreachable pairs before the
#' reciprocals are taken.** `Z = sum_k W^k` is exactly zero there, but a
#' LAPACK solve on a *reducible* system returns roundoff of either sign, of
#' order `1e-16`, instead. Those entries enter `term1` multiplied by a
#' reciprocal `1 / z_st` that can reach the thousands, which lifts the noise
#' to around `1e-11`; and because the roundoff pattern follows the pivoting,
#' it follows the *node order*, so leaving it in costs permutation
#' invariance on a sparse directed graph. This was not hypothetical: the
#' first full sweep failed on one 35-node directed weighted fixture at
#' scaled error `4.8e-11`, which imposing the known structure drops to
#' `9.1e-14`.
#'
#' @param a Adjacency matrix, read as directed and weighted exactly as
#'   supplied. Loops are dropped; the source discusses none.
#' @param beta Inverse temperature, a single finite number strictly above
#'   zero. Large `beta` concentrates the walk distribution on shortest
#'   paths, `beta` towards zero on the unbiased random walk.
#' @param cost How an edge weight becomes a traversal cost: `"inverse"` for
#'   `C = 1 / w` and `"weight"` for `C = w`. Both give unit cost on a binary
#'   graph. Off-arc entries of `C` are irrelevant because `P^ref` is zero
#'   there, and are written as zero as the reference implementation does.
#' @return A `data.frame` with one row per node in input order and the
#'   columns `term1`, `term2` and `score`. The fundamental matrix is
#'   carried on the result as the `z` attribute, the killed-walk matrix as
#'   `w`, and the reachability closure as `reach`.
#' @keywords internal
#' @noRd
.cg_rsp_terms <- function(a, beta = 0.01, cost = c("inverse", "weight")) {
  if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta) ||
        beta <= 0) {
    message <- paste("`rsp_beta` must be a single finite number strictly",
                     "above zero; it is the inverse temperature of",
                     "equation (6)")
    stop(errorCondition(message, class = "cograph_bad_parameter", call = NULL))
  }
  cost <- match.arg(cost)
  n <- nrow(a)
  if (is.null(n) || n == 0L) {
    empty <- data.frame(term1 = numeric(), term2 = numeric(),
                        score = numeric())
    attr(empty, "z") <- matrix(0, 0, 0)
    attr(empty, "w") <- matrix(0, 0, 0)
    attr(empty, "reach") <- matrix(FALSE, 0, 0)
    return(empty)
  }
  a <- unname(as.matrix(a))
  diag(a) <- 0
  if (any(!is.finite(a))) {
    message <- "randomized shortest paths needs finite edge weights"
    stop(errorCondition(message, class = "cograph_bad_input", call = NULL))
  }
  if (any(a < 0)) {
    message <- paste("randomized shortest paths needs nonnegative edge",
                     "weights; Algorithm 1 of the source takes a",
                     "non-negative cost matrix")
    stop(errorCondition(message, class = "cograph_bad_input", call = NULL))
  }

  # P^ref = D^-1 A, with the row of a zero-out-degree node written as zero.
  strength <- rowSums(a)
  live <- strength > 0
  pref <- matrix(0, n, n)
  if (any(live)) pref[live, ] <- a[live, , drop = FALSE] / strength[live]

  arc <- a > 0
  cmat <- matrix(0, n, n)
  cmat[arc] <- if (identical(cost, "inverse")) 1 / a[arc] else a[arc]
  w <- pref * exp(-beta * cmat)

  z <- solve(diag(1, n) - w)
  reach <- .cg_reach_closure(a)
  # A structurally reachable pair whose fundamental-matrix entry has
  # underflowed to zero would make the reciprocal infinite. Surface it.
  if (any(!is.finite(z)) || any(z[reach] <= 0)) {
    message <- paste("the randomized shortest paths fundamental matrix",
                     "underflowed at rsp_beta =", format(beta),
                     "- a reachable pair reached zero in double precision;",
                     "lower `rsp_beta`")
    stop(errorCondition(message, class = "cograph_rsp_underflow",
                        call = NULL))
  }

  # Z = sum_k W^k is exactly zero at an unreachable pair, but a LAPACK solve
  # on a reducible system returns roundoff there instead -- around 1e-16,
  # and of either sign. Those entries enter term1 multiplied by a reciprocal
  # 1 / z_st that can reach the thousands, so the noise is amplified to
  # around 1e-11 and, worse, its pattern depends on the pivoting, which
  # depends on the node order: leaving it in makes the measure fail
  # permutation invariance on a sparse directed graph. The structure is
  # known exactly, so it is imposed exactly.
  z[!reach] <- 0

  recip <- matrix(0, n, n)
  recip[reach] <- 1 / z[reach]
  # diag(Z M^T Z)_i = sum_k (Z M^T)[i, k] Z[k, i].
  term1 <- rowSums((z %*% t(recip)) * t(z))
  sources <- colSums(reach)
  term2 <- as.numeric((z * t(z)) %*% (sources / diag(z)))
  out <- data.frame(term1 = term1, term2 = term2, score = term1 - term2)
  attr(out, "z") <- z
  attr(out, "w") <- w
  attr(out, "reach") <- reach
  out
}
