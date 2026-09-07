#' Local neighbor contribution terms (Dai et al. 2019, eqs. 1-6)
#'
#' Dai, Wang, Sheng, Sun, Khawaja, Ullah, Dejene and Duan (2019), IEEE
#' Access 7:131719-131731, build the influence of a node from two factors,
#' Definitions 4 and 5 on journal page 131722:
#'
#' ```
#' neiCon(v_i) = D(v_i) sum_{j=1}^{k} P(v_j) DC(v_j)      (4)
#' ownCon(v_i) = d(v_i) sum_{j=1}^{k} C(k,1) P(v_i) (1 - P(v_i))^(k-1)   (5)
#' Influ(v_i)  = neiCon(v_i) ownCon(v_i)                  (6)
#' ```
#'
#' with `DC(v_i) = d(v_i)/(n - 1)` (1), `P(v_i) = 1/d(v_i)` (2) and the
#' cluster degree `D(v_i) = sum_{j in N(i)} d(v_j)` (3). Because
#' `P(v_j) DC(v_j) = (1/d_j)(d_j/(n - 1))` collapses to `1/(n - 1)`, the
#' inner sum of (4) is just a count and the neighbours enter only through
#' `D(v_i)`.
#'
#' The printed equations are self-inconsistent about that count `k`: the
#' prose calls it "the number of the nearest neighbor nodes and the next
#' nearest neighbor nodes", Algorithm 1 line 12 sets it to `G.degree(v)`
#' immediately before evaluating (5), and taken literally (5) carries one
#' factor of `d(v_i)` too many. The printed numbers settle it. For `v_5` of
#' Figure 1 the paper prints `D(v_5) = 12`, `ownCon(v_5) = 1.6875`,
#' `neiCon(v_5) = 19.2` and `Influ(v_5) = 32.4`, and Table 1 prints all
#' eleven influences. The only pair of factors consistent with every printed
#' value is
#'
#' ```
#' ownCon(i) = d_i (1 - 1/d_i)^(d_i - 1)
#' neiCon(i) = d_i^2 (sum_{j in N(i)} d_j) / (n - 1)
#' LNC(i)    = d_i^3 (1 - 1/d_i)^(d_i - 1) (sum_{j in N(i)} d_j) / (n - 1)
#' ```
#'
#' so `k` acts as `d_i` in (5) and as `d_i^2` in (4). The fully literal
#' reading of (4) and (5) moves one factor of `d_i` from `neiCon` to
#' `ownCon` and leaves the product unchanged; the split returned here is the
#' one the paper's own printed intermediates pick.
#'
#' `0^0 = 1` at `d_i = 1`, which is what the three printed influences of
#' 0.4 require and is not stated in the paper. `n` is the vertex count of
#' the whole graph, as equation (1) says, so the scores of a component
#' depend on the size of the rest of the graph.
#'
#' A node of degree zero is outside the definition: `P(v_i) = 1/0`. cograph
#' scores it zero, an extension recorded on `centrality_lnc()`, and the
#' zero is written before the division so a singleton graph, whose only
#' node is an isolate and whose `n - 1` is zero, never divides.
#'
#' @param b Adjacency matrix; direction, weights, loops and parallel edges
#'   are dropped to the simple undirected skeleton the source defines on.
#' @return A `data.frame` with one row per node in input order and the
#'   columns `degree` (`d_i`), `cluster_degree` (`D(v_i)`, equation 3),
#'   `own_con` (equation 5), `nei_con` (equation 4) and `lnc` (equation 6).
#' @keywords internal
#' @noRd
.cg_lnc_terms <- function(b) {
  a <- .cg_undirected_view(b)
  diag(a) <- 0
  n <- nrow(a)
  if (is.null(n) || n == 0L) {
    return(data.frame(degree = numeric(), cluster_degree = numeric(),
                      own_con = numeric(), nei_con = numeric(),
                      lnc = numeric()))
  }
  degree <- rowSums(a)
  cluster <- as.numeric(a %*% degree)
  own <- numeric(n)
  nei <- numeric(n)
  # A degree-zero node has no contribution probability, so it is filled in
  # separately rather than left to (1 - 1/0)^(-1); doing it here also keeps
  # the singleton graph, where n - 1 is zero, away from the division.
  live <- degree > 0
  if (any(live)) {
    d <- degree[live]
    own[live] <- d * (1 - 1 / d)^(d - 1)
    nei[live] <- d^2 * cluster[live] / (n - 1)
  }
  data.frame(degree = degree, cluster_degree = cluster, own_con = own,
             nei_con = nei, lnc = own * nei)
}
