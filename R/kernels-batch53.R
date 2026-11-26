#' Mediative effects centrality (Friedkin 1991)
#'
#' Friedkin (1991), *American Journal of Sociology* 96(6):1478-1504. Page
#' 1486, equation (13), decomposes the mean length of the influence
#' sequences into visits to particular actors,
#'
#' ```
#' m_ij = sum_k t_(j)ik,    T_(j) = (I - W_(j))^-1                    (13)
#' ```
#'
#' "where `t_(j)ik` is the ik-th entry in `T_(j)` and `W_(j)` is a matrix
#' obtained by deleting the j-th row and column from `W`" (Kemeny and Snell
#' 1960, pp. 112-113). Page 1490, equations (22) and (23), build the
#' measure on it:
#'
#' ```
#' tbar_(k)j = sum_{i != j, k} t_(k)ij / ((n - 2) t_(k)jj)            (22)
#' c_MEC(j)  = sum_{k != j} tbar_(k)j / (n - 1)                       (23)
#' ```
#'
#' `T_(k)` is the fundamental matrix of the chain made absorbing at `k`:
#' `t_(k)ij` is the expected number of visits to `j` of a sequence that
#' starts at `i`, before it reaches `k`. The quotient `t_(k)ij / t_(k)jj`
#' is the probability that such a sequence passes through `j` at all, so
#' `tbar_(k)j` is the mean, over the `n - 2` other starting actors, of the
#' probability that the effects of `k` are transmitted by `j`, and the
#' score is its mean over the `n - 1` actors `k`.
#'
#' The influence matrix is the one of `.cg_iec_terms()`: binary arcs, a
#' unit diagonal, rows divided by their sums (page 1494). On an irreducible
#' chain every `I - W_(k)` is nonsingular, because the chain leaves any
#' proper subset of its states.
#'
#' All 105 values of the MEC column of Table 1 (pages 1492-1494) are
#' reproduced to the three decimals printed.
#'
#' @param w Row-stochastic influence matrix of an irreducible chain on at
#'   least three states.
#' @return Numeric vector of scores in `[0, 1]`, one per node, in input
#'   order.
#' @keywords internal
#' @noRd
.cg_mec <- function(w) {
  n <- nrow(w)
  stopifnot("`w` needs at least three states" = n >= 3L)
  # Row j of the result: tbar_(k)j of equation (22), zero at j = k.
  transmitted <- function(k) {
    visits <- solve(diag(n - 1L) - w[-k, -k, drop = FALSE])
    reached <- visits / matrix(diag(visits), n - 1L, n - 1L, byrow = TRUE)
    out <- numeric(n)
    # The column sum includes the start i = j, whose quotient is one.
    out[-k] <- (colSums(reached) - 1) / (n - 2)
    out
  }
  rowSums(vapply(seq_len(n), transmitted, numeric(n))) / (n - 1)
}

#' Total and mediative effects centrality with their status
#'
#' @param b Adjacency matrix, as for `.cg_iec_terms()`.
#' @return A `data.frame` with one row per node and the columns `tec` (the
#'   stationary vector of the influence chain) and `mec`. The `status`
#'   attribute is `"ok"`, `"empty"`, `"singleton"` or `"reducible"` as in
#'   `.cg_iec_terms()`, or `"pair"` for an irreducible chain on two states,
#'   where `tec` has a value and `mec` (equation 22 divides by `n - 2`) has
#'   none.
#' @keywords internal
#' @noRd
.cg_friedkin_terms <- function(b) {
  terms <- .cg_iec_terms(b)
  status <- attr(terms, "status")
  n <- nrow(terms)
  out <- data.frame(tec = terms$stationary, mec = rep(NA_real_, n))
  if (identical(status, "ok") && n == 2L) status <- "pair"
  if (identical(status, "ok")) out$mec <- .cg_mec(attr(terms, "influence"))
  attr(out, "status") <- status
  out
}
