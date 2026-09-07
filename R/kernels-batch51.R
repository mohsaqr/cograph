# ===========================================================================
# Batch 51 kernel --- trust-PageRank (TPR).
#
# Sheng, J., Zhu, J., Wang, Y., Wang, B. and Hou, Z. (2020). Identifying
# Influential Nodes of Complex Networks Based on Trust-Value. Algorithms
# 13(11):280. doi:10.3390/a13110280. Equations (2), (4), (5), (6) and (7)
# and Algorithm 1, journal pages 4-7, read from 200 dpi page images as well
# as from the text layer.
#
# `b` is an adjacency matrix; the kernel projects it to the simple
# undirected skeleton the source defines on and touches no igraph object.
# ===========================================================================

#' Fixed point of the source's restricted SimRank recursion
#'
#' Equation (4) on journal page 5 reads
#'
#' ```
#' s(a, b) = 1                                                    a = b
#'         = (C / (|N_a| |N_b|)) sum_{i,j} s(N_a(i), N_b(j))      a != b
#' ```
#'
#' and Algorithm 1 line 4, page 7, restricts the similarity map to
#' **connected** pairs: `for connected node v_u, v_v in V do`. Table 3 on
#' page 6 says the same thing from the other side -- "the short dash
#' indicates that there is no edge between the two nodes" -- so the map
#' holds an entry for every adjacent pair and for the diagonal, and a
#' non-adjacent pair entering the double sum contributes **zero**, not the
#' `0.1` that initialises the adjacent entries. That domain restriction is
#' the whole reason the recursion is usable: the diagonal `s(l, l) = 1` is
#' the only inhomogeneous term, and it enters `s(a, b)` exactly when `l` is
#' a common neighbour of `a` and `b`, so the constant of the affine map is
#' the triangle census of the line.
#'
#' Writing `A` for the binary skeleton and `S` for the similarity carried
#' on the lines, the sweep is one matrix sandwich,
#'
#' ```
#' S <- C * (A (S + I) A) / (d d') masked to the lines of A
#' ```
#'
#' because `(A M A)[a, b] = sum_{l in N_a} sum_{m in N_b} M[l, m]` for a
#' symmetric binary `A`. The iteration starts at `S = 0` rather than at the
#' source's `0.1`: the map is affine and monotone, so from zero the iterates
#' increase to the least nonnegative fixed point, which is the fixed point
#' whenever one is unique, and which stays exactly zero where the recursion
#' is homogeneous instead of drifting to whatever the start happened to be.
#' The start does not matter where the map contracts; it is the whole answer
#' where it does not, and zero is the choice that reports the degeneracy
#' rather than hiding it. **The source's `0.1` initialisation is therefore
#' not used**, and `centrality_trust_pagerank()` records why.
#'
#' Every row sum of the linear part is at most `1 - p/(d_a d_b)` for a line
#' carrying `p` triangles, because the `p` diagonal terms are the part of
#' the `d_a d_b` ordered pairs that leaves the linear part, so the spectral
#' radius never exceeds one and is strictly below one on any block that
#' carries a triangle. The iteration converges for every `C` in `(0, 1]`.
#'
#' **The convergence test is RELATIVE, and it has to be.** The similarities
#' on a single graph span many orders of magnitude -- on a long chain at
#' `C = 0.2` the largest is `2.4e-2` and the smallest positive one
#' `2.7e-20` -- because the mass reaching a line decays geometrically with
#' its distance from the nearest triangle. An absolute test on the largest
#' entrywise change stops while the small entries are still an order of
#' magnitude from their limits, and equation (2) then divides two of those
#' small entries by each other. `.cg_tpr_relative_change()` measures the
#' change against the new value on the lines that have one, so every line
#' must settle in its own scale.
#'
#' @param a Binary symmetric loop-free adjacency matrix.
#' @param decay The source's `C`, in `(0, 1]`.
#' @param tol Positive tolerance on the largest relative change.
#' @param max_iter Iteration bound.
#' @return A list with the similarity matrix `s`, the sweep count
#'   `iterations` and the flag `converged`.
#' @keywords internal
#' @noRd
.cg_tpr_similarity <- function(a, decay, tol, max_iter) {
  n <- nrow(a)
  d <- rowSums(a)
  # d_a d_b is zero only where an endpoint is isolated, and there the mask
  # `* a` already writes zero, so the guard never changes a reported value.
  dd <- outer(d, d)
  dd[dd <= 0] <- 1
  s <- matrix(0, n, n)
  iterations <- 0L
  delta <- Inf
  # A fixed-point sweep consumes the previous sweep, so it cannot be
  # vectorised away; the bound and the residual are both reported.
  while (iterations < max_iter && !(delta < tol)) {
    m <- s
    diag(m) <- 1
    nxt <- decay * (a %*% m %*% a) / dd * a
    delta <- .cg_tpr_relative_change(nxt, s)
    s <- nxt
    iterations <- iterations + 1L
  }
  list(s = s, iterations = iterations, converged = isTRUE(delta < tol))
}

#' Largest relative change between two iterates
#'
#' `max |new - old| / new` over the entries where `new` is positive, and
#' zero when none is. Both iterations here start from a nonnegative vector
#' and stay nonnegative, and an entry that is exactly zero in `new` is
#' exactly zero in `old` too, so the skipped entries have no change to
#' measure.
#'
#' @param nxt,previous Numeric objects of the same shape.
#' @return A single nonnegative number.
#' @keywords internal
#' @noRd
.cg_tpr_relative_change <- function(nxt, previous) {
  live <- nxt > 0
  if (!any(live)) return(0)
  max(abs(nxt[live] - previous[live]) / nxt[live])
}

#' Lines that carry a strictly positive similarity at the fixed point
#'
#' The iterates increase from zero, so a line ends up positive exactly when
#' it can reach a triangle-carrying line through the recursion: `s(a, b)`
#' draws on `s(l, m)` for every `l` in `N_a` and `m` in `N_b`, and the only
#' source of mass is the diagonal term, which is present on a line iff that
#' line lies on a triangle. Deciding this **structurally**, by a boolean
#' closure, rather than by testing the converged doubles against zero,
#' matters for the same reason it does in `.cg_reach_closure()`: a value
#' produced by a long chain of divisions can be a legitimate `1e-300`, and
#' a threshold cannot tell that from a rounding artefact.
#'
#' @param a Binary symmetric loop-free adjacency matrix.
#' @return Logical matrix, `TRUE` on the lines with positive similarity.
#' @keywords internal
#' @noRd
.cg_tpr_support <- function(a) {
  edge <- a > 0
  # A line lies on a triangle iff its endpoints have a common neighbour.
  support <- edge & ((a %*% a) > 0)
  # Each round moves the frontier one hop at each endpoint, so the closure
  # is reached in at most a diameter's worth of rounds; it is monotone and
  # bounded, so the loop terminates.
  repeat {
    grown <- edge & ((a %*% (support * 1) %*% a) > 0)
    nxt <- support | grown
    if (identical(nxt, support)) break
    support <- nxt
  }
  support
}

#' Trust-PageRank and the parts it is built from
#'
#' Sheng et al.'s trust-value replaces PageRank's uniform `1/k_j` split with
#' a mixture of a similarity ratio and a degree ratio. Journal pages 4 to 7
#' give the five equations:
#'
#' ```
#' Rs(i, j) = s(i, j) / sum_{k in N_j} s(j, k)                        (2)
#' s(a, b)  = 1 if a = b, else (C/(|N_a||N_b|)) sum sum s(.,.)        (4)
#' Rd(i, j) = d_i / sum_{k in N_j} d_k                                (5)
#' T(i, j)  = (1 - k) Rs(i, j) + k Rd(i, j)                           (6)
#' TPR_i^t  = (1 - alpha)/n + alpha sum_{j in N_i} T(i, j) TPR_j^{t-1} (7)
#' ```
#'
#' Both ratios in equation (6) are normalised over `N_j`, and `s` is
#' symmetric, so `sum_{i in N_j} Rs(i, j) = 1` and
#' `sum_{i in N_j} Rd(i, j) = 1` for every `k`: **`T` is column-stochastic
#' on the lines**. Equation (7) is therefore an ordinary damped PageRank
#' with a unique fixed point, and its iteration count is a convergence
#' tolerance rather than a modelling choice. The scores sum to one on a
#' graph without isolates; an isolate's column is empty, so it emits
#' nothing and the total falls by the mass it holds.
#'
#' **A node whose lines all carry zero similarity has no trust-value at
#' all.** Equation (2) then divides zero by zero, and unlike the vanishing
#' denominators of `centrality_dil()` and `centrality_lhc()` the quotient is
#' not determined by its numerator: the ratios must sum to one over `N_j`,
#' but nothing in the source says how. Every node of the affected component
#' is returned as `NA`; see `centrality_trust_pagerank()` for why a `1/d_j`
#' fallback was rejected.
#'
#' @param b Adjacency matrix. Direction, weights, loops and parallel edges
#'   are dropped by `.cg_undirected_view()`: page 3 sets the paper in an
#'   undirected network with `a(i, j) = 1`, and every quantity in the five
#'   equations is a count or a ratio of counts.
#' @param alpha The source's jump probability, in `(0, 1)`.
#' @param mix The source's `k`, in `[0, 1]`.
#' @param decay The source's `C`, in `(0, 1]`.
#' @param tol Positive tolerance on the largest relative change, for both
#'   recursions.
#' @param max_iter Iteration bound for both recursions.
#' @return A `data.frame` with one row per node in input order and the
#'   columns `degree` (`d_i`), `similarity_sum` (`S_i`, the row sum of the
#'   similarity over the node's own lines, which is Table 3's `S_v`
#'   column), `neighbor_degree_sum` (`D_i`) and `trust_pagerank` (the
#'   score). Carried as attributes: `similarity` (`s`), `trust` (`T`,
#'   asymmetric where `s` is symmetric), `defined` (a logical vector,
#'   `FALSE` at a node returned as `NA`), `iterations` (a two-element
#'   vector, the similarity and the PageRank sweep counts) and `converged`
#'   (a matching two-element logical vector).
#' @keywords internal
#' @noRd
.cg_tpr_terms <- function(b, alpha = 0.85, mix = 0.85, decay = 1,
                          tol = 1e-14, max_iter = 1000L) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) {
    out <- data.frame(degree = numeric(), similarity_sum = numeric(),
                      neighbor_degree_sum = numeric(),
                      trust_pagerank = numeric())
    attr(out, "similarity") <- matrix(0, 0, 0)
    attr(out, "trust") <- matrix(0, 0, 0)
    attr(out, "defined") <- logical()
    attr(out, "iterations") <- c(similarity = 0L, pagerank = 0L)
    attr(out, "converged") <- c(similarity = TRUE, pagerank = TRUE)
    return(out)
  }

  a <- .cg_undirected_view(b)
  d <- rowSums(a)
  fit <- .cg_tpr_similarity(a, decay, tol, max_iter)
  s <- fit$s
  support <- .cg_tpr_support(a)

  # A node needs a trust column only if it has neighbours at all; an
  # isolate is never a `j` in equation (7), so it is not undefined.
  undefined_node <- d > 0 & rowSums(support) == 0L
  reach <- .cg_reach_closure(a)
  # An undefined column makes equation (7) undefined for every node of its
  # component, because the trust matrix that component solves against has
  # no value in that column.
  defined <- !as.logical(reach %*% (undefined_node * 1) > 0)

  similarity_sum <- rowSums(s)
  neighbor_degree_sum <- as.numeric(a %*% d)

  # Equations (2) and (5). Both denominators run over N_j, so they are
  # column quantities; each is guarded before the division, never after.
  safe_s <- similarity_sum
  safe_s[!(safe_s > 0)] <- 1
  safe_d <- neighbor_degree_sum
  safe_d[!(safe_d > 0)] <- 1
  rs <- s / matrix(safe_s, n, n, byrow = TRUE)
  rd <- matrix(d, n, n) / matrix(safe_d, n, n, byrow = TRUE)
  trust <- ((1 - mix) * rs + mix * rd) * a
  # A column with no value must not contribute mass to the recursion; the
  # component it sits in is reported as NA below regardless.
  trust[, !defined] <- 0

  # Equation (7), iterated from the uniform vector to the same tolerance.
  x <- rep(1 / n, n)
  iterations <- 0L
  delta <- Inf
  while (iterations < max_iter && !(delta < tol)) {
    y <- (1 - alpha) / n + alpha * as.numeric(trust %*% x)
    delta <- .cg_tpr_relative_change(y, x)
    x <- y
    iterations <- iterations + 1L
  }
  x[!defined] <- NA_real_

  out <- data.frame(degree = d, similarity_sum = similarity_sum,
                    neighbor_degree_sum = neighbor_degree_sum,
                    trust_pagerank = x)
  rownames(out) <- NULL
  attr(out, "similarity") <- s
  attr(out, "trust") <- trust
  attr(out, "defined") <- defined
  attr(out, "iterations") <- c(similarity = fit$iterations,
                               pagerank = iterations)
  attr(out, "converged") <- c(similarity = fit$converged,
                              pagerank = isTRUE(delta < tol))
  out
}
