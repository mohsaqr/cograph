#' Multi-attribute ranking by TOPSIS and relative entropy (Liu et al. 2015)
#'
#' Liu, Jiang, Wang and Yu (2015), *Knowledge-Based Systems* 84:56-66,
#' sections 2.2 and 3, pages 57-61. Four indicators are computed for every
#' node: `DC_i = d_i / (N - 1)` (equation 2), `CC_i = (N - 1) / sum_j l_ij`
#' (3), Freeman's betweenness (4) and the improved k-shell index `IKs`
#' (section 2.2.5). Each column is scaled by its Euclidean norm,
#' `t_ij = v_ij / sqrt(sum_i v_ij^2)` (6), and multiplied by its weight,
#' `r_ij = w_j t_ij` (7), with the weights the paper derives by the
#' analytic hierarchy process and prints as
#' `W = (0.0625, 0.1875, 0.1875, 0.5625)`. With `r_j^+` and `r_j^-` the
#' largest and smallest entry of column `j` (10),
#'
#' ```
#' S_i^+ = { sum_j [ r_j^+ log(r_j^+ / r_ij) + (1 - r_j^+) log((1 - r_j^+) / (1 - r_ij)) ] }^(1/2)   (12a)
#' S_i^- = the same with r_j^-                                                                      (12b)
#' C_i   = S_i^- / (S_i^+ + S_i^-)                                                                  (13)
#' ```
#'
#' The base of the logarithm cancels in `C`.
#'
#' **A zero indicator.** When `r_ij = 0` and the column maximum is
#' positive, the first term of (12a) is infinite, so `S_i^+` is infinite
#' and `C_i` is zero. Every node of degree one has zero betweenness and
#' falls under this. The paper does not discuss the case, and its Fig. 6
#' shows positive values for nodes that its Fig. 5 shows with zero
#' betweenness, so its computation departed from its equation in a way it
#' does not state. The equation is followed here, by the owner's decision
#' of 2026-11-25. A term `0 log(0 / x)` is zero.
#'
#' When all nodes are equal on all four indicators the two distances are
#' both zero and `C` is `0 / 0`; no score is returned.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal, of a
#'   connected network on at least two nodes.
#' @return A list with `value` and `status`, `"ok"` or `"uniform"`.
#' @keywords internal
#' @noRd
.cg_topsis_re <- function(b) {
  n <- nrow(b)
  distance <- .cg_hop_matrix(b > 0, n)
  indicators <- cbind(rowSums(b) / (n - 1), (n - 1) / rowSums(distance),
                      .cg_betweenness(b, n, FALSE), .cg_iksd(b))
  norms <- sqrt(colSums(indicators^2))
  scaled <- sweep(indicators, 2L, ifelse(norms > 0, norms, 1), "/")
  r <- sweep(scaled, 2L, c(0.0625, 0.1875, 0.1875, 0.5625), "*")
  to_ideal <- function(ideal) {
    target <- matrix(ideal, n, 4L, byrow = TRUE)
    first <- ifelse(target > 0, target * log(target / r), 0)
    sqrt(pmax(rowSums(first + (1 - target) * log((1 - target) / (1 - r))),
              0))
  }
  far <- to_ideal(apply(r, 2L, max))
  near <- to_ideal(apply(r, 2L, min))
  if (all(far + near == 0)) {
    return(list(value = rep(NA_real_, n), status = "uniform"))
  }
  list(value = ifelse(is.infinite(far), 0, near / (far + near)), status = "ok")
}
