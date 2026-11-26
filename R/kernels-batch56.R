#' Number of spanning trees, on the log scale
#'
#' Kirchhoff's matrix-tree theorem: the determinant of the Laplacian with
#' one row and column removed.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal, of a
#'   connected graph on at least one node.
#' @return The natural logarithm of the number of spanning trees.
#' @keywords internal
#' @noRd
.cg_log_spanning_trees <- function(b) {
  n <- nrow(b)
  if (n <= 1L) return(0)
  laplacian <- diag(rowSums(b), n) - b
  as.numeric(determinant(laplacian[-n, -n, drop = FALSE],
                         logarithm = TRUE)$modulus)
}

#' Dynamical spanning tree centrality
#'
#' Equation (50) of Lu, Chen, Ren, Zhang, Zhang and Zhou (2016), *Physics
#' Reports* 650:1-63, attributed there to Chen, Hu, Yip, Hu and Zhong
#' (2003), whose paper was not read:
#'
#' ```
#' DST(i) = 1 - t(G \ {v_i}) / t_0
#' ```
#'
#' with `t_0` the number of spanning trees of `G` and `t(G \ {v_i})` that
#' of `G` without `v_i`. When the removal disconnects the graph the count
#' is zero and the score is one. A single remaining node has one spanning
#' tree, so both nodes of a connected pair score zero.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return A list with `value` and `status`, one of `"ok"`,
#'   `"disconnected"` or `"singleton"`.
#' @keywords internal
#' @noRd
.cg_dst <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(list(value = numeric(0), status = "ok"))
  if (n == 1L) return(list(value = NA_real_, status = "singleton"))
  if (.cg_n_components(b) > 1L) {
    return(list(value = rep(NA_real_, n), status = "disconnected"))
  }
  whole <- .cg_log_spanning_trees(b)
  value <- vapply(seq_len(n), \(i) {
    rest <- b[-i, -i, drop = FALSE]
    if (nrow(rest) > 1L && .cg_n_components(rest) > 1L) return(1)
    1 - exp(.cg_log_spanning_trees(rest) - whole)
  }, numeric(1))
  list(value = value, status = "ok")
}

#' The KDEC method (Zhang, Wang, Sheng, Dai, Hu and Chen 2019)
#'
#' Zhang, Wang, Sheng, Dai, Hu and Chen (2019), *Information* 10(10):311,
#' pages 4-6. Each
#' node carries the weight `W(v) = k_s(v) d(v)`, coreness times degree
#' (equation 3). The effective distance from `i` to a neighbor `j` is
#' `eff(i, j) = 1 - log10(1 / d_j)` (equation 4), and along a path of two
#' steps the distances add. The score sums the influence
#' `W(i) W(k) / (n eff^2)` over the neighbors and over the nodes two steps
#' away:
#'
#' ```
#' KDEC(i) = sum_{j in N(i)} W(i) W(j) / (n eff(i,j)^2)
#'         + sum_{j in N(i)} sum_{k in N(j), k != i, k not in N(i)}
#'               W(i) W(k) / (n (eff(i,j) + eff(j,k))^2)
#' ```
#'
#' The factor `1 / n` and the counting of a second neighbor once per path
#' that reaches it are fixed by the paper's worked example, which prints
#' `KDEC(v7) = 28.7212` on its Fig. 1; the equations alone do not state
#' them. `n` is the number of nodes of the whole network.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector of scores; isolates score zero.
#' @keywords internal
#' @noRd
.cg_kdec <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  weight <- .cg_mdd(b, lambda = 0) * degree
  # eff(., j) depends on the degree of j only; isolates are never reached.
  step <- 1 + log10(pmax(degree, 1))
  one_hop <- as.numeric(b %*% (weight / step^2))
  via <- b * matrix(weight, n, n, byrow = TRUE) / outer(step, step, "+")^2
  beyond <- 1 - b
  diag(beyond) <- 0
  two_hop <- rowSums((b %*% via) * beyond)
  weight * (one_hop + two_hop) / n
}

#' Counting betweenness (Bloechl, Theis, Vega-Redondo and Fisher 2011)
#'
#' Bloechl, Theis, Vega-Redondo and Fisher (2011), *Physical Review E*
#' 83:046127, equations (1) and (6)-(8). With `M = K^-1 A` the transition
#' matrix of a random walk and `M_-t` that matrix without row and column
#' `t`, the expected number of times a walk from `s` to its first arrival
#' at `t` uses the edge `(i, j)` is
#' `N^st_ij = m_ij ((I - M_-t)^-1)_si`, and
#'
#' ```
#' N^st(i) = sum_{j != t} (N^st_ij + N^st_ji) / 2        i != s, t     (6)
#' N^st(s) = sum_{j != t} (N^st_sj + N^st_js) / 2 + 1
#' N^st(t) = 1                                                         (7)
#' C_c(i)  = sum_s sum_{t != s} N^st(i) / (n (n - 1))                  (8)
#' ```
#'
#' Writing `F = (I - M_-t)^-1` and `r_i = sum_{j != t} m_ij`, the identity
#' `F M_-t = F - I` gives `sum_j N^st_ji = F_si - [s = i]`, so the whole
#' table of `N^st(i)` for one target is `(F (1 + r) + I) / 2` column by
#' column, with no sum over edges. A self-loop enters through `m_ii` and
#' is counted once, as the paper prescribes.
#'
#' The paper considers strongly connected graphs only, and assigns zero to
#' nodes without incoming or without outgoing ties after removing them
#' (section IV). Both rules are followed; when what remains is not
#' strongly connected no score is returned.
#'
#' @param a Nonnegative weight matrix, `a[i, j]` for the arc `i -> j`,
#'   loops included.
#' @return A list with `value` and `status`, one of `"ok"`,
#'   `"not_strong"` or `"too_small"`.
#' @keywords internal
#' @noRd
.cg_counting_betweenness <- function(a) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(list(value = numeric(0), status = "ok"))
  off <- a
  diag(off) <- 0
  live <- which(rowSums(off) > 0 & colSums(off) > 0)
  size <- length(live)
  if (size < 2L) {
    return(list(value = rep(NA_real_, n), status = "too_small"))
  }
  part <- a[live, live, drop = FALSE]
  reach <- part
  diag(reach) <- 1
  if (!.cg_boolean_irreducible(reach) || any(rowSums(part) <= 0)) {
    return(list(value = rep(NA_real_, n), status = "not_strong"))
  }
  move <- part / rowSums(part)
  visits <- vapply(seq_len(size), \(target) {
    inner <- move[-target, -target, drop = FALSE]
    fundamental <- solve(diag(size - 1L) - inner)
    table <- (fundamental * matrix(1 + rowSums(inner), size - 1L, size - 1L,
                                   byrow = TRUE) + diag(size - 1L)) / 2
    out <- numeric(size)
    out[-target] <- colSums(table)
    out[target] <- size - 1
    out
  }, numeric(size))
  value <- numeric(n)
  value[live] <- rowSums(visits) / (size * (size - 1))
  list(value = value, status = "ok")
}
