#' KED method terms (Chen, Xiao, Zeng and Zhang 2014, eqs. 1, 2 and 6)
#'
#' Chen, Xiao, Zeng and Zhang (2014), EPL 104(6):68006, rank spreaders by
#' the product of a node's degree, the evenness of its neighbours' degrees
#' and the size of its two-step neighbourhood. Preprint page 2, equations
#' (1) and (2):
#'
#' ```
#' h_i = sum_{j in Gamma_i} -p_j log(p_j),   p_j = k_j / sum_{l in Gamma_i} k_l
#' H_i = h_i / [sum_{j in Gamma_i} -(1/k_i) log(1/k_i)] = h_i / log(k_i)
#' ```
#'
#' and preprint page 4, equation (6) with its accompanying text:
#'
#' ```
#' f_i = k_i E_i D_i,  E_i = 1 + H_i,  D_i = exp(sum_{j in Gamma_i} k_j / N)
#' ```
#'
#' Writing `K_i = sum_{j in N(i)} k_j` for the cluster degree, the measure
#' implemented here is
#'
#' ```
#' KED(i) = k_i (1 + H_i) exp(K_i / N),   N = |V|
#' ```
#'
#' `H_i` is a ratio of two logarithms in the same base -- equation (2)
#' divides the neighbour-degree entropy by its own maximum, the entropy of
#' the uniform distribution on `k_i` outcomes -- so the base cancels and
#' the natural logarithm used here is not a choice. `H_i` lies in `[0, 1]`
#' and equals one exactly when the neighbour degrees are all equal, which
#' is the paper's `1 <= E_i <= 2`.
#'
#' The entropy is not accumulated neighbour by neighbour. Substituting
#' `p_j = k_j / K_i` into equation (1) and separating the logarithm gives
#'
#' ```
#' h_i = log(K_i) - (1 / K_i) sum_{j in N(i)} k_j log(k_j)
#' ```
#'
#' so both the cluster degree and the entropy are single matrix-vector
#' products against the degree vector and against `k log k`.
#'
#' Two cases the paper never mentions are decided here and recorded on
#' `centrality_ked()`. At `k_i = 1` the single neighbour has `p = 1`, so
#' `h_i = 0`, and the normaliser `log(k_i)` is zero as well: `H_i` is
#' `0/0` and is written as **zero**, the value the `k_i = 2` limit of a
#' vanishing minority neighbour approaches and the one that gives a
#' one-path node the least path diversity. At `k_i = 0` both the entropy
#' and its normaliser are empty sums; `H_i` is written as zero there too,
#' and the score is zero whatever finite `E_i` is chosen, because `k_i`
#' multiplies the product.
#'
#' `N` is the vertex count of the whole graph (table 1 of the source
#' defines `N` as "the number of nodes"), so isolates and other components
#' count and raw scores are not comparable across graphs of different
#' order.
#'
#' @param b Adjacency matrix; direction, weights, loops and parallel edges
#'   are dropped to the simple undirected skeleton the source defines on.
#' @return A `data.frame` with one row per node in input order and the
#'   columns `degree` (`k_i`), `cluster_degree` (`K_i`), `entropy`
#'   (`h_i`, equation 1), `diversity` (`H_i`, equation 2), `e_factor`
#'   (`E_i = 1 + H_i`), `d_factor` (`D_i`) and `ked` (equation 6).
#' @keywords internal
#' @noRd
.cg_ked_terms <- function(b) {
  a <- .cg_undirected_view(b)
  diag(a) <- 0
  n <- nrow(a)
  if (is.null(n) || n == 0L) {
    return(data.frame(degree = numeric(), cluster_degree = numeric(),
                      entropy = numeric(), diversity = numeric(),
                      e_factor = numeric(), d_factor = numeric(),
                      ked = numeric()))
  }
  degree <- rowSums(a)
  cluster <- as.numeric(a %*% degree)
  # k log k, with the isolate's undefined 0 log 0 written as zero first so
  # that a zero adjacency entry can never multiply a NaN. An isolate is in
  # nobody's neighbourhood, so this never changes a neighbour sum.
  scaled_log <- numeric(n)
  live <- degree > 0
  scaled_log[live] <- degree[live] * log(degree[live])
  neighbour_log <- as.numeric(a %*% scaled_log)
  entropy <- numeric(n)
  diversity <- numeric(n)
  # Only a node with at least two neighbours has a non-degenerate entropy
  # and a non-zero normaliser; k_i <= 1 is the 0/0 written as zero above.
  plural <- degree > 1
  if (any(plural)) {
    entropy[plural] <- log(cluster[plural]) -
      neighbour_log[plural] / cluster[plural]
    diversity[plural] <- entropy[plural] / log(degree[plural])
  }
  e_factor <- 1 + diversity
  d_factor <- exp(cluster / n)
  ked <- numeric(n)
  ked[live] <- degree[live] * e_factor[live] * d_factor[live]
  if (any(!is.finite(ked))) {
    stop("ked raw scores overflow: the neighbour-degree sum divided by the ",
         "node count exceeds the range of exp()", call. = FALSE)
  }
  data.frame(degree = degree, cluster_degree = cluster, entropy = entropy,
             diversity = diversity, e_factor = e_factor,
             d_factor = d_factor, ked = ked)
}
