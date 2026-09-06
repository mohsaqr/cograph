# ===========================================================================
# Distance-derived centralities, dependency-free
# ===========================================================================
# Each reduces the all-pairs matrix from .cg_distances(). Conventions here
# follow igraph/cograph, which differ from the snajs originals in three
# documented places (see the equivalence test).

.cg_offdiag <- function(d) row(d) != col(d)

#' @param d Distance matrix. @param n Vertex count. @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_radiality <- function(d, n, diameter = .cg_diameter(d)) {
  if (n <= 1L) return(rep(NaN, n))
  rowSums(ifelse(is.finite(d), diameter + 1 - d, 0)) / (n - 1)
}

#' @keywords internal
#' @noRd
.cg_lin <- function(d, n) {
  if (n <= 1L) return(rep(NaN, n))
  ok <- is.finite(d) & d > 0
  reachable <- rowSums(ok)
  total <- rowSums(ifelse(ok, d, 0))
  ifelse(reachable == 0 | total == 0, 0, reachable^2 / total)
}

#' @keywords internal
#' @noRd
.cg_decay <- function(d, n, decay_parameter = 0.5) {
  if (n <= 1L) return(rep(1, n))
  rowSums(decay_parameter^d)          # param^Inf is 0 for 0 < param < 1
}

#' @keywords internal
#' @noRd
.cg_residual_closeness <- function(d, n) {
  if (n <= 1L) return(rep(1, n))
  rowSums(ifelse(is.finite(d), 1 / 2^d, 0))
}

#' @keywords internal
#' @noRd
.cg_harary <- function(d, n) {
  if (n <= 1L) return(rep(0, n))
  ok <- .cg_offdiag(d) & is.finite(d) & d > 0
  rowSums(ifelse(ok, 1 / d^2, 0))
}

#' @keywords internal
#' @noRd
.cg_average_distance <- function(d, n) {
  if (n <= 1L) return(rep(NA_real_, n))
  # Unreachable vertices make the row sum infinite, and cograph propagates
  # that Inf rather than masking it. (The JSON fixture set cannot represent
  # Inf and stores it as null, which is why it must not be the oracle here.)
  rowSums(d) / (n + 1)
}

#' @keywords internal
#' @noRd
.cg_barycenter <- function(d, n) {
  if (n <= 1L) return(rep(NaN, n))
  total <- rowSums(ifelse(.cg_offdiag(d) & is.finite(d), d, 0))
  ifelse(total == 0, 0, 1 / total)
}

#' @keywords internal
#' @noRd
.cg_wiener <- function(d, n) {
  if (n <= 1L) return(rep(0, n))
  rowSums(ifelse(.cg_offdiag(d) & is.finite(d), d, 0))
}

#' @keywords internal
#' @noRd
.cg_gilschmidt <- function(d, n) {
  if (n <= 1L) return(rep(0, n))
  ok <- .cg_offdiag(d) & is.finite(d) & d > 0
  rowSums(ifelse(ok, 1 / d, 0)) / (n - 1)
}

#' @keywords internal
#' @noRd
.cg_centroid <- function(d, n) {
  if (n <= 1L) return(rep(0, n))
  vapply(seq_len(n), function(v) {
    dv <- d[v, ]
    min(rowSums(sweep(d, 2L, dv, ">")) - rowSums(sweep(d, 2L, dv, "<")))
  }, numeric(1L))
}
