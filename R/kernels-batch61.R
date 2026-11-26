#' Direct influence of the long-range interaction centrality
#'
#' Aleskerov, Meshcheryakova and Shvydun, "Centrality measures in networks
#' based on nodes attributes, long-range interactions and group influence"
#' (arXiv:1610.05892, the working-paper version of their 2017 chapter),
#' Definitions 1-2 and equation (1), pages 16-18. A group of in-neighbors
#' of `j` is critical when its weights sum to at least the quota `q_j`,
#' and a member is pivotal when the group without it falls below the
#' quota. The direct influence of `i` on `j` is
#'
#' ```
#' c_ij = w_ij / min { sum of the weights of a critical group of j in which i is pivotal }
#' ```
#'
#' and zero when `i` is pivotal in no critical group. The quota is the
#' share `quota` of the total weight entering `j`.
#'
#' For a given `i` the smallest such group is `w_ij` plus the smallest
#' sum `R` of other in-neighbors' weights with `q_j - w_ij <= R < q_j`.
#' The attainable sums are built one neighbor at a time and kept distinct,
#' so integer or equal weights stay cheap; with many distinct weights their
#' number doubles with every neighbor, and the construction stops with a
#' classed condition past `limit` sums.
#'
#' Comparisons with the quota allow a relative slack of `1e-12`, so that a
#' group whose weights add up to the quota exactly is critical in floating
#' point as well.
#'
#' @param a Nonnegative weight matrix with a zero diagonal, `a[i, j]` the
#'   influence of `i` on `j`.
#' @param quota Share of the incoming weight that makes a group critical.
#' @param limit Largest number of subset sums per node.
#' @return Matrix of direct influences in `[0, 1]`.
#' @keywords internal
#' @noRd
.cg_lric_direct <- function(a, quota = 0.5, limit = 1e6) {
  n <- nrow(a)
  direct <- matrix(0, n, n)
  into <- function(j) {
    sources <- which(a[, j] > 0)
    if (!length(sources)) return(numeric(n))
    weights <- a[sources, j]
    need <- quota * sum(weights)
    slack <- 1e-12 * sum(weights)
    column <- numeric(n)
    column[sources] <- vapply(seq_along(sources), \(k) {
      sums <- Reduce(\(acc, w) {
        acc <- unique(c(acc, acc + w))
        if (length(acc) > limit) .cg_stop_enumeration()
        # A sum at or above the quota can never be part of a group in
        # which another member is pivotal.
        acc[acc < need - slack]
      }, weights[-k], 0)
      enough <- sums[sums >= need - weights[k] - slack]
      if (length(enough)) weights[k] / (weights[k] + min(enough)) else 0
    }, numeric(1))
    column
  }
  direct[] <- vapply(seq_len(n), into, numeric(n))
  direct
}

#' Long-range interaction centrality with the maximal path (LRIC, max)
#'
#' The influence of `i` on `j` along a path is the product of the direct
#' influences on its arcs (equation 2), and the total influence is that of
#' the best simple path of at most `s` arcs (equation 5, "MaxPath" in
#' Table 5 of the working paper). The score of a node is
#'
#' ```
#' LRIC(i) = sum_j c*_ij u_j,     u_j = sum_k w_jk / sum_l sum_k w_lk,
#' ```
#'
#' the total influence weighted by the share of all weight that leaves
#' `j` (page 27). Direct influences never exceed one, so a walk that
#' repeats a node never beats the path it contains, and the best simple
#' path is found by a max-product closure without enumerating paths.
#'
#' @param a Nonnegative weight matrix with a zero diagonal.
#' @param quota Share of the incoming weight that makes a group critical,
#'   a single number in `(0, 1]`.
#' @param path_length Largest number of arcs of a path, a single whole
#'   number of at least one, or `Inf`.
#' @param limit Largest number of subset sums per node.
#' @return Numeric vector; zeros on a network without arcs.
#' @keywords internal
#' @noRd
.cg_lric_max <- function(a, quota = 0.5, path_length = Inf, limit = 1e6) {
  if (!is.numeric(quota) || length(quota) != 1L || !is.finite(quota) ||
        quota <= 0 || quota > 1) {
    .cg_stop_bad_parameter("`lric_quota` must be a single number in (0, 1]")
  }
  if (!is.numeric(path_length) || length(path_length) != 1L ||
        is.na(path_length) || path_length < 1 ||
        (is.finite(path_length) && path_length != trunc(path_length))) {
    .cg_stop_bad_parameter(
      "`lric_path_length` must be a single whole number of at least one, or Inf")
  }
  .cg_check_enumeration_limit(limit)
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  total <- sum(a)
  if (total <= 0) return(numeric(n))
  direct <- .cg_lric_direct(a, quota, limit)
  best <- direct
  rounds <- min(path_length, n - 1) - 1
  # Each round admits one more arc; it depends on the round before.
  for (round in seq_len(max(rounds, 0))) {
    longer <- vapply(seq_len(n), \(j) {
      apply(best * matrix(direct[, j], n, n, byrow = TRUE), 1L, max)
    }, numeric(n))
    updated <- pmax(best, longer)
    diag(updated) <- 0
    if (identical(updated, best)) break
    best <- updated
  }
  as.numeric(best %*% (rowSums(a) / total))
}
