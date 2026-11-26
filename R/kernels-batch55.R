#' Common out-neighbor score (Bonato, Eikmeier, Gleich and Malik 2019)
#'
#' Bonato, Eikmeier, Gleich and Malik (2019), "Centrality in dynamic
#' competition networks", Complex Networks and Their Applications VIII,
#' pp. 105-116, section 2: "For a pair of distinct nodes u, v, we define
#' CON(u, v) to be the number of common out-neighbors of u and v", counted
#' with "multiplicities based on the minimum number of interactions",
#'
#' ```
#' CON(u, v) = sum_k min(A_uk, A_vk),    CON(u) = sum_{v != u} CON(u, v).
#' ```
#'
#' The Centrality Zoo's entry 2.41 includes the term `v = u` and writes the
#' product `a_uk a_vk` for the minimum; the paper is followed.
#'
#' @param a Nonnegative matrix of arc multiplicities, `a[u, k]` for the arc
#'   `u -> k`, with a zero diagonal.
#' @return Numeric vector of scores, one per node.
#' @keywords internal
#' @noRd
.cg_con <- function(a) {
  n <- nrow(a)
  if (is.null(n) || n == 0L) return(numeric(0))
  own <- rowSums(a)
  vapply(seq_len(n), \(u) {
    sum(pmin(a, matrix(a[u, ], n, n, byrow = TRUE))) - own[u]
  }, numeric(1))
}

#' Entropy and mutual information-based centrality (Li, Cai, Li and Du 2020)
#'
#' Li, Cai, Li and Du (2020), *Entropy* 22(1):52, equations (22), (24) and
#' (25), page 9, for an undirected unweighted network:
#'
#' ```
#' S_i  = - sum_{j in N_i} (d_j / EXD_i) log(d_j / EXD_i),  EXD_i = sum_{j in N_i} d_j
#' MI_i = sum_{j in N_i} F(N_ij)/F(N_(i,j)) log[(F(N_i) + F(N_j) - F(N_ij)) / F(N_(i,j))]
#' K_i  = S_i + MI_i
#' ```
#'
#' with `N_ij` the common neighbors of `i` and `j`, `F` the size of a set
#' and `F(N_(i,j)) = F(N_i) + F(N_j)`, the value the worked example of
#' page 10 uses. Logarithms are to base 10 (page 10). The Centrality Zoo's
#' entry 2.104 prints the denominator as the product of the two degrees;
#' the paper's Fig. 3 values (-0.03876, -0.05062, -0.05062, 0) are
#' reproduced by the sum and not by the product.
#'
#' Both terms are sums over the open neighborhood, so an isolate scores
#' zero. A neighbor with no common neighbor contributes nothing to `MI`.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return A `data.frame` with the columns `entropy` (`S`), `mutual`
#'   (`MI`, never positive) and `emi`.
#' @keywords internal
#' @noRd
.cg_emi_terms <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) {
    return(data.frame(entropy = numeric(0), mutual = numeric(0),
                      emi = numeric(0)))
  }
  tied <- b > 0
  degree <- rowSums(b)
  neighbor_degree <- matrix(degree, n, n, byrow = TRUE)
  share <- neighbor_degree / as.numeric(b %*% degree)
  entropy <- -rowSums(ifelse(tied, share * log10(share), 0))
  common <- (b %*% b) * b
  both <- outer(degree, degree, "+")
  mutual <- rowSums(ifelse(tied, common / both * log10((both - common) / both),
                           0))
  data.frame(entropy = entropy, mutual = mutual, emi = entropy + mutual)
}

#' NWRank (Wang, Duenas-Osorio and Padgett 2015)
#'
#' Wang, Duenas-Osorio and Padgett (2015), *Scientific Reports* 5:15141,
#' equations (5)-(7), page 3. Nodes and links score each other,
#' `x = W y`, `y = H x`, with `W` the node-link incidence matrix and
#'
#' ```
#' h_bj = alpha k_b / sum_a k_a + (1 - alpha) BC_b / sum_a BC_a          (7)
#' ```
#'
#' for a link `b` at node `j`, the sums running over the links at `j`,
#' `k_b` the degree of the node at the other end of `b` and `BC_b` the
#' betweenness of the link. Both vectors are normalized to sum to one and
#' the node scores are the principal eigenvector of `W H`.
#'
#' `W H` is the identity plus the matrix `G` with
#' `g_ij = h_(ij),j`, whose columns sum to one, so the scores are the
#' vector fixed by `G`. That vector is unique when the nodes that have
#' links form one connected component. With several components the paper
#' speaks of "the linear combination of the principal eigenvectors"
#' without fixing it, and no score is returned. An isolate has no link and
#' scores zero.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param alpha Weight of the degree term, a single number in `[0, 1]`.
#' @return A list with `value` (scores summing to one, or `NA`) and
#'   `status`, one of `"ok"`, `"no_links"` or `"disconnected"`.
#' @keywords internal
#' @noRd
.cg_nwrank <- function(b, alpha = 0.5) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha < 0 || alpha > 1) {
    .cg_stop_bad_parameter("`nwrank_alpha` must be a single number in [0, 1]")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(list(value = numeric(0), status = "ok"))
  degree <- rowSums(b)
  linked <- which(degree > 0)
  if (!length(linked)) {
    return(list(value = rep(NA_real_, n), status = "no_links"))
  }
  part <- b[linked, linked, drop = FALSE]
  if (length(unique(.cg_component_labels(part))) > 1L) {
    return(list(value = rep(NA_real_, n), status = "disconnected"))
  }
  size <- length(linked)
  links <- which(part != 0 & upper.tri(part), arr.ind = TRUE)
  links <- matrix(as.integer(links), ncol = 2L)
  between <- matrix(0, size, size)
  between[links] <- .cg_edge_betweenness(part, size, FALSE, edges = links)
  between <- between + t(between)
  # Column j: the shares node j gives to its links, placed at the far ends.
  by_degree <- part * degree[linked]
  give <- alpha * sweep(by_degree, 2L, colSums(by_degree), "/") +
    (1 - alpha) * sweep(between, 2L, colSums(between), "/")
  fixed <- solve(diag(size) - give + matrix(1 / size, size, size),
                 rep(1 / size, size))
  value <- numeric(n)
  value[linked] <- fixed / sum(fixed)
  list(value = value, status = "ok")
}

#' Biased random walk centrality (Takes and Kosters 2011)
#'
#' Takes and Kosters (2011), "Identifying prominent actors in online social
#' networks using biased random walks", Proceedings of BNAIC 2011,
#' pp. 215-222. A walker at node `v` moves to the neighbor `w` with
#' probability proportional to
#' `alpha f_deg(w) + (1 - alpha) f_nd(w)` (page 5), where
#'
#' ```
#' f_deg(v) = 1 - 1 / |N(v)|
#' f_nd(v)  = 1 - sum_{w in N(v)} |N(w) n N(v)| / ((|N(w)| - 1) |N(v)|)
#' ```
#'
#' and with probability `p` jumps to a node drawn uniformly instead
#' (Algorithm 1). The paper estimates the share of visits by one walk of
#' `10 n` steps from a random start. The value returned is the stationary
#' distribution of the same chain, the limit of that share.
#'
#' Three cases the paper leaves open are settled as follows. A neighbor
#' `w` of degree one has no common neighbor with `v` and its term in
#' `f_nd` is taken as zero. When every neighbor of the current node has
#' weight zero, the move is uniform over the neighbors. From an isolate
#' the walker can only jump. `f_nd` cannot be negative, because `w` and
#' `v` share at most `|N(w)| - 1` neighbors.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param alpha Weight of the degree term, a single number in `[0, 1]`.
#' @param jump Jump probability `p`, a single number in `(0, 1]`.
#' @return Numeric vector of stationary probabilities, summing to one.
#' @keywords internal
#' @noRd
.cg_biased_random_walk <- function(b, alpha = 0.5, jump = 0.15) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha < 0 || alpha > 1) {
    .cg_stop_bad_parameter("`brw_alpha` must be a single number in [0, 1]")
  }
  if (!is.numeric(jump) || length(jump) != 1L || !is.finite(jump) ||
        jump <= 0 || jump > 1) {
    .cg_stop_bad_parameter("`brw_jump` must be a single number in (0, 1]")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  common <- (b %*% b) * b
  # Row w, column v: |N(w) n N(v)| / (|N(w)| - 1), zero when |N(w)| = 1.
  shared <- common / pmax(degree - 1, 1)
  diversity <- 1 - colSums(shared) / pmax(degree, 1)
  weight <- alpha * (1 - 1 / pmax(degree, 1)) + (1 - alpha) * diversity
  move <- b * matrix(weight, n, n, byrow = TRUE)
  total <- rowSums(move)
  stuck <- total <= 0 & degree > 0
  move[stuck, ] <- b[stuck, , drop = FALSE]
  total[stuck] <- degree[stuck]
  move <- move / ifelse(total > 0, total, 1)
  move[degree == 0, ] <- 1 / n
  chain <- (1 - jump) * move + jump / n
  stationary <- solve(diag(n) - t(chain) + matrix(1 / n, n, n), rep(1 / n, n))
  stationary / sum(stationary)
}
