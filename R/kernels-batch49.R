#' Irreducibility of a nonnegative square matrix, by boolean closure
#'
#' A finite Markov chain is irreducible exactly when the digraph of its
#' positive entries is strongly connected. The closure is taken by repeated
#' squaring of the boolean matrix rather than by a traversal: `r` already
#' carries the mandated self-loops, so one squaring doubles the walk length
#' that each entry certifies and `ceiling(log2(n))` squarings certify every
#' walk of length at most `n`, which is all a path on `n` nodes can need.
#' Every intermediate is 0/1, so nothing overflows and no tolerance is
#' involved.
#'
#' @param a Square numeric matrix whose positive entries are the arcs. Its
#'   diagonal is expected to be positive; the caller sets it.
#' @return `TRUE` when every node reaches every node.
#' @keywords internal
#' @noRd
.cg_boolean_irreducible <- function(a) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(FALSE)
  if (n == 1L) return(TRUE)
  steps <- ceiling(log2(n))
  closure <- Reduce(function(r, .) (r %*% r) > 0, seq_len(steps),
                    init = (a > 0))
  all(closure)
}

#' Immediate effects centrality and its parts (Friedkin 1991)
#'
#' Friedkin (1991), *American Journal of Sociology* 96(6):1478-1504,
#' derives three centrality measures from a linear model of interpersonal
#' influence. The second of them, immediate effects centrality, reads a
#' node's prominence off the mean length of the influence sequences that
#' run from it to everyone else. Journal page 1486, equation (11), gives
#' those mean lengths and page 1489, equation (20), turns them into the
#' score:
#'
#' ```
#' M        = (I - Z + E Z_dg) D                                     (11)
#' c_IEC(j) = ( sum_{i != j} m_ij / (n - 1) )^-1                     (20)
#' ```
#'
#' with `D` the diagonal matrix of `d_ii = 1/c_i`, `c` the left eigenvector
#' of `W` for eigenvalue one from page 1485 equation (9), `E` the all-ones
#' matrix, `Z = (I - W + W^Inf)^-1` and `Z_dg` the matrix that results from
#' `Z` by setting the off-diagonal entries to zero. `W^Inf` is the limiting
#' total-effects matrix `V_U` of page 1484, every row of which is `c`, so
#' `W^Inf = 1 c'` and `Z` is the Kemeny-Snell fundamental matrix. `M` is
#' then the mean first passage time matrix in the convention
#' `m_ij = E[steps from i until j is first reached]`, and the sum in
#' equation (20) runs down column `j`: the score is the reciprocal mean
#' first passage time *into* `j`, so a node is central when the rest of the
#' network reaches it quickly.
#'
#' **The influence matrix carries a unit self-loop, and that is not
#' cosmetic.** Page 1494 states the construction twice, once in the body and
#' once in the note to Table 1: "the diagonal entries of its adjacency
#' matrix `A = [a_ij]` were set to one and its influence network was
#' computed as `W = [w_ij] = a_ij / sum_j^n a_ij`", following French (1956).
#' Footnote 10 on page 1484 says why: "Given `w_ii > 0` for any `i`, all
#' strong networks must be regular", regular meaning aperiodic, and
#' footnote 9 on page 1483 gives the periodic counterexample that the
#' self-loop rules out. Dropping it changes the answer rather than the
#' scale: cograph's older `markov` measure drops it, and the two measures
#' disagree on rank order, not merely on units.
#'
#' **The chain must be irreducible, and reducible input is refused rather
#' than computed.** For eigenvalue one of a reducible `W` the eigenspace has
#' one dimension per closed class, so `c` is not determined and `D` is
#' undefined wherever `c` vanishes. Worse, equation (11) does not announce
#' the failure: with `Z` block diagonal, a pair `i`, `j` in different blocks
#' has `z_ij = 0` and equation (11) returns the perfectly finite
#' `m_ij = z_jj / c_j` where the true mean first passage time is infinite.
#' A finite wrong number is the one outcome that must not be produced, so
#' the boolean closure is taken first and every score is `NA` when it fails.
#' Friedkin restricts his own analysis to regular networks (pages
#' 1483-1484) and never defines the measure outside them.
#'
#' @param b Adjacency matrix. Direction is retained, because `W` is a
#'   row-stochastic matrix of directed influence; loops and parallel edges
#'   are dropped, the former because the source mandates its own unit
#'   diagonal and the latter because `a_ij = 1` "wherever a line exists".
#'   Weights are dropped as well: `a_ii = 1` is calibrated against
#'   `a_ij = 1`, so a rescaling of the weights would silently re-weight the
#'   self-loop against the network.
#' @return A `data.frame` with one row per node in input order and the
#'   columns `stationary` (`c_i`), `recurrence` (`m_ii = 1/c_i`, the mean
#'   recurrence time), `mfpt_in` (`sum_{i != j} m_ij`, the denominator of
#'   equation 20) and `iec` (the score). Every column is `NA` when the
#'   measure is undefined. The mean first passage matrix `M` is carried on
#'   the result as the `mfpt` attribute, the influence matrix `W` as
#'   `influence`, and the reason for an undefined result as `status`, one
#'   of `"ok"`, `"empty"`, `"singleton"` or `"reducible"`.
#' @keywords internal
#' @noRd
.cg_iec_terms <- function(b) {
  n <- nrow(b)
  frame <- function(status, values = NULL, mfpt = NULL, influence = NULL) {
    rows <- if (identical(status, "empty")) 0L else n
    na <- rep(NA_real_, rows)
    out <- values %||% data.frame(stationary = na, recurrence = na,
                                  mfpt_in = na, iec = na)
    rownames(out) <- NULL
    attr(out, "status") <- status
    attr(out, "mfpt") <- mfpt %||% matrix(NA_real_, rows, rows)
    attr(out, "influence") <- influence %||% matrix(NA_real_, rows, rows)
    out
  }
  if (is.null(n) || n == 0L) return(frame("empty"))

  # The source's own construction: binary arcs, then a_ii = 1 everywhere.
  # Any loop already in the input is absorbed by that diagonal, and the row
  # sums are at least one, so the normalisation never divides by zero.
  a <- .cg_edge_indicator(b)
  diag(a) <- 1
  w <- a / rowSums(a)

  # Equation (20) divides by n - 1, which is zero on a singleton; and the
  # eigenvector, the inverse and the mean first passage times all need an
  # irreducible chain. Neither case is extended.
  if (n == 1L) return(frame("singleton", influence = w))
  if (!.cg_boolean_irreducible(a)) return(frame("reducible", influence = w))

  ident <- diag(n)
  uniform <- matrix(1 / n, n, n)
  # c W = c with c summing to one, solved as a single nonsingular system
  # rather than by an eigendecomposition. Left-multiplying
  # (I - W' + J/n) x = 1/n by the all-ones row gives sum(x) = 1, whence
  # (I - W') x = 0 and x is the stationary vector; the added rank-one term
  # is scaled by 1/n so the system stays well scaled as n grows.
  stationary <- solve(ident - t(w) + uniform, rep(1 / n, n))
  stationary <- stationary / sum(stationary)

  # W^Inf = 1 c' (page 1484), so Z is the Kemeny-Snell fundamental matrix.
  limit <- matrix(stationary, n, n, byrow = TRUE)
  z <- solve(ident - w + limit)
  # Equation (11). (E Z_dg)[i, j] = z_jj, and the post-multiplication by D
  # divides column j by c_j.
  mfpt <- (ident - z + matrix(diag(z), n, n, byrow = TRUE)) / limit
  # The diagonal of M is the mean recurrence time 1/c_i and equation (20)
  # excludes it, so it is subtracted off rather than zeroed in place.
  recurrence <- diag(mfpt)
  mfpt_in <- colSums(mfpt) - recurrence

  values <- data.frame(stationary = stationary, recurrence = recurrence,
                       mfpt_in = mfpt_in, iec = (n - 1) / mfpt_in)
  frame("ok", values, mfpt, w)
}
