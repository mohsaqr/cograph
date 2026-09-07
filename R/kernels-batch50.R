# ===========================================================================
# Batch 50 kernel --- degree and importance of lines (DIL).
#
# Liu, J., Xiong, Q., Shi, W., Shi, X. and Wang, K. (2016). Evaluating the
# importance of nodes in complex networks. Physica A 452:209-219.
# doi:10.1016/j.physa.2016.02.049. Equations (1)-(3), journal pages
# 210-211, read from 300 dpi page images rather than from a text layer.
#
# `b` is an adjacency matrix; the kernel projects it to the simple
# undirected skeleton the source defines on and touches no igraph object.
# ===========================================================================

#' Degree and importance of lines, and the parts it is built from
#'
#' The source ranks a node by its degree plus the share it can claim of the
#' importance of the lines that touch it. A line is important when its two
#' endpoints reach far beyond it and when it is not shortcut by a triangle;
#' the share each endpoint claims is proportional to that endpoint's own
#' degree. Journal pages 210 and 211 give the three equations:
#'
#' ```
#' I_emn = U / lambda,  U = (k_m - p - 1)(k_n - p - 1),  lambda = p/2 + 1  (1)
#' W_vivj = I_eij * (k_i - 1) / (k_i + k_j - 2)                           (2)
#' L_vi   = k_i + sum_{vj in Gamma_i} W_vivj                              (3)
#' ```
#'
#' with `k_m` the degree of `v_m`, `p` the number of triangles one of whose
#' edges is `e_mn`, `Gamma_i` the open neighbourhood of `v_i`, and
#' `W_vivj` "the contribution that `v_i` makes to the importance of
#' `e_ij`". In a simple graph `p` is `|N(m) intersect N(n)|`.
#'
#' **`lambda` is `p/2 + 1`, and this is the one place the measure can be
#' got wrong silently.** The stacked fraction extracts from the PDF's text
#' layer as `lambda = 2p + 1`, in the original as well as in the Almasi and
#' Hu (2019) reproduction of it. The page image shows `p` over `2`, and the
#' paper's own worked example settles it in printed prose: at `p = 1` it
#' writes "`lambda = 1/2 + 1 = 1.5`" and `I_e45 = 4/1.5 = 8/3`. The wrong
#' reading would give `4/3` there.
#'
#' **`U` is never negative.** For an edge `(i, j)`, `j` lies in `N(i)` but
#' in neither `N(j)` nor the intersection, so `p <= k_i - 1` and each
#' factor of `U` is at least zero. It follows that every `I` is at least
#' zero and every score is at least the node's degree.
#'
#' **The only vanishing denominator is an isolated `K2`, and it carries a
#' vanishing numerator with it.** `k_i + k_j - 2 = 0` needs `k_i = k_j = 1`,
#' since both endpoints of an edge have degree at least one; that is a
#' two-node component, where `p = 0` and
#' `U = (1 - 0 - 1)(1 - 0 - 1) = 0`, so the importance being divided is
#' exactly `0`. Equation (2) splits `I` between the endpoints -- the two
#' shares `(k_i - 1)/(k_i + k_j - 2)` and `(k_j - 1)/(k_i + k_j - 2)` sum
#' to one wherever they are defined -- so whatever share of an exactly zero
#' importance an endpoint claims, its contribution is zero. The share is
#' written as zero and the test is made **before** the division, so no
#' `0/0` is ever evaluated. Both nodes of a `K2` therefore score `1`. The
#' source does not discuss the case; this is cograph's decision, and it is
#' recorded in `centrality_dil()`.
#'
#' @param b Adjacency matrix. Direction, weights, loops and parallel edges
#'   are dropped by `.cg_undirected_view()`: the source states on page 210
#'   that its networks are "undirected and unweighted", and every quantity
#'   in equations (1)-(3) is a count.
#' @return A `data.frame` with one row per node in input order and the
#'   columns `degree` (`k_i`), `line_importance` (`sum_j I_eij` over the
#'   incident lines, which is not part of the published measure but is the
#'   quantity equation (3) refuses to use directly), `contribution`
#'   (`sum_j W_vivj`, the sum in equation 3) and `dil` (the score). The
#'   per-edge matrices are carried as attributes: `triangles` (`p`),
#'   `importance` (`I`) and `contributions` (`W`, which is asymmetric where
#'   `I` is symmetric), each zero off the edge set.
#' @keywords internal
#' @noRd
.cg_dil_terms <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) {
    out <- data.frame(degree = numeric(), line_importance = numeric(),
                      contribution = numeric(), dil = numeric())
    attr(out, "triangles") <- matrix(0, 0, 0)
    attr(out, "importance") <- matrix(0, 0, 0)
    attr(out, "contributions") <- matrix(0, 0, 0)
    return(out)
  }

  # The simple undirected skeleton: binary, symmetric, loop-free.
  a <- .cg_undirected_view(b)
  k <- rowSums(a)

  # p_ij, the number of triangles carrying the line e_ij, is the number of
  # common neighbours. (A A)[i, j] counts walks of length two, which on a
  # loop-free binary matrix is exactly that count; it is masked to the edge
  # set because equations (1)-(3) are only ever evaluated on lines.
  triangles <- (a %*% a) * a

  # k_i and k_j as matrices, so equations (1) and (2) are one expression
  # each. Row i of `ki` is k_i; `kj` is its transpose.
  ki <- matrix(k, n, n)
  kj <- t(ki)

  # Equation (1). lambda >= 1 always, so the division is unconditional.
  u <- (ki - triangles - 1) * (kj - triangles - 1)
  importance <- (u / (triangles / 2 + 1)) * a

  # Equation (2). The share is a proportion of an importance, so it is
  # defined as zero exactly where its denominator vanishes -- see the note
  # above -- and the branch is taken before the division, never after it.
  denominator <- (ki + kj - 2) * a
  share <- matrix(0, n, n)
  splittable <- denominator > 0
  share[splittable] <- (ki[splittable] - 1) / denominator[splittable]
  contributions <- importance * share

  out <- data.frame(degree = k,
                    line_importance = rowSums(importance),
                    contribution = rowSums(contributions),
                    dil = k + rowSums(contributions))
  rownames(out) <- NULL
  attr(out, "triangles") <- triangles
  attr(out, "importance") <- importance
  attr(out, "contributions") <- contributions
  out
}
