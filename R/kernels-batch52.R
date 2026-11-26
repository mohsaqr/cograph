#' Improved neighbors' k-core (Lin, Guo, Dong, Tang and Liu 2014)
#'
#' Lin, Guo, Dong, Tang and Liu (2014), *Physics Letters A* 378(45):3279-3284,
#' equation (1), page 3280:
#'
#' ```
#' S(i) = sum_{j in J(i)} k_s(j)^alpha                              (1)
#' ```
#'
#' with `k_s(j)` the k-core value of `j` and `J(i)` the neighbor set of `i`.
#'
#' **`J(i)` is the open neighborhood.** The paper's Fig. 1 prints
#' `S(1) = 15, S(2) = 10, S(3) = 11, S(4) = 13` at `alpha = 1` for the four
#' nodes that form the whole 3-core. Those four induce `K4`, so each already
#' receives `3 * 3 = 9` from the other three; counting a node's own
#' `k_s = 3` as well would force every `S >= 12`, and the paper prints 10.
#'
#' **There is no normalization.** The printed values are raw sums. A
#' third-party MATLAB toolbox divides by the maximum; the source does not.
#'
#' **The exponent sits on each neighbor's core value, inside the sum.**
#' `alpha = 0` gives the degree and `alpha = 1` the sum of the neighbors'
#' core values, both stated by the authors. Their experiments run at
#' `alpha = 1` (section 3.3, Tables 2 and 3); section 3.4 finds the optimal
#' value network-dependent (0.7, 1, 2.4 and 5 on their four networks).
#'
#' An isolate has an empty neighbor set and scores zero, the empty sum; the
#' paper does not discuss the case. `0^alpha` never enters a sum, because a
#' node with `k_s = 0` is an isolate and an isolate is nobody's neighbor.
#' Negative `alpha` is outside the source and is rejected.
#'
#' A power `k_s^alpha` too large for a double is refused rather than
#' returned: `Inf` times the zero of a non-neighbor would be `NaN`.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param core Core numbers in vertex order.
#' @param alpha The exponent of equation (1), a single finite nonnegative
#'   number.
#' @return Numeric vector of raw scores, one per node, in input order.
#'   Raises `cograph_bad_parameter` for an `alpha` outside its domain or one
#'   whose powers overflow.
#' @keywords internal
#' @noRd
.cg_ink <- function(b, core, alpha = 1) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha < 0) {
    .cg_stop_bad_parameter(
      "`ink_alpha` must be a single finite nonnegative number")
  }
  if (!nrow(b)) return(numeric(0))
  powered <- core^alpha
  if (!all(is.finite(powered))) {
    .cg_stop_bad_parameter(
      "`ink_alpha` is too large: a core value raised to it overflows")
  }
  as.numeric(b %*% powered)
}
