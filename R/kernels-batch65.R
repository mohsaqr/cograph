#' Shannon entropy of the rows of a nonnegative matrix
#'
#' Rows are read as distributions, `0 log 0 = 0`, natural logarithm.
#'
#' @param p Numeric matrix whose rows sum to one or to zero.
#' @return Numeric vector, one entropy per row.
#' @keywords internal
#' @noRd
.cg_row_entropy <- function(p) {
  -rowSums(ifelse(p > 0, p * log(p), 0))
}

#' Number of nodes within two steps of each node, as a logical matrix
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Logical matrix; `TRUE` where the column node is one or two steps
#'   from the row node. The diagonal is `FALSE`.
#' @keywords internal
#' @noRd
.cg_within_two <- function(b) {
  within_two <- (b + b %*% b) > 0
  diag(within_two) <- FALSE
  within_two
}

#' Check a single number between zero and one
#' @keywords internal
#' @noRd
.cg_check_unit <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 0 || value > 1) {
    .cg_stop_bad_parameter(sprintf(
      "`%s` must be a single number between 0 and 1", name))
  }
  invisible(value)
}

#' Check a vector of finite nonnegative weights that are not all zero
#' @keywords internal
#' @noRd
.cg_check_weight_vector <- function(value, name, size) {
  if (!is.numeric(value) || length(value) != size || any(!is.finite(value)) ||
        any(value < 0) || all(value == 0)) {
    .cg_stop_bad_parameter(sprintf(
      "`%s` must be %d finite nonnegative numbers, not all zero", name, size))
  }
  invisible(value)
}

#' Diversity-strength centrality and its two rankings (Zareie et al. 2019)
#'
#' Zareie, Sheikhahmadi and Jalili (2019), *Future Generation Computer
#' Systems* 94:120-129, equations (21)-(26). With `IKs` the improved k-shell
#' index (the round of the minimum-degree peel, `.cg_iksd()`), `N(v)` the
#' neighbors of `v` and `d_v` its degree,
#'
#' ```
#' S(v)    = sum_{u in N(v)} IKs(u)
#' H1(v)   = - sum_{u in N(v)} (IKs(u) / S(v)) log(IKs(u) / S(v))      (21)
#' m(v)    = S(v) / d_v                                                (22)
#' X_u     = shares of the neighbors of u in each shell
#' JSD(v)  = H(mean_{u in N(v)} X_u) - mean_{u in N(v)} H(X_u)         (23)
#' DSC(v)  = m(v) H1(v) JSD(v)                                         (24)
#' DSR(v)  = sum_{u in N(v)} DSC(u)                                    (25)
#' EDSR(v) = sum_{u in N(v)} DSR(u)                                    (26)
#' ```
#'
#' The shells of `X_u` are the improved k-shell rounds: section 3.2 of the
#' paper says the improved index replaces the k-shell in the measure,
#' although the symbols of the definition still read `Ks`. The paper prints
#' `log` without a base; the natural logarithm is used, and another base
#' rescales every score by one constant. The inner sum of (26) is
#' unrestricted, so it counts the focal node once per neighbor.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param level `"dsc"`, `"dsr"` or `"edsr"`.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_diversity_strength <- function(b, level = c("dsc", "dsr", "edsr")) {
  level <- match.arg(level)
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  safe_degree <- pmax(degree, 1)
  shell <- .cg_iksd(b)
  shell_sum <- as.numeric(b %*% shell)
  shares <- b * matrix(shell, n, n, byrow = TRUE) / pmax(shell_sum, 1)
  membership <- outer(shell, seq_len(max(shell)), "==") * 1
  profile <- (b %*% membership) / safe_degree
  mixture <- (b %*% profile) / safe_degree
  divergence <- .cg_row_entropy(mixture) -
    as.numeric(b %*% .cg_row_entropy(profile)) / safe_degree
  # The divergence is nonnegative; rounding can leave it a hair below zero.
  dsc <- (shell_sum / safe_degree) * .cg_row_entropy(shares) *
    pmax(divergence, 0)
  switch(level,
         dsc = dsc,
         dsr = as.numeric(b %*% dsc),
         edsr = as.numeric(b %*% (b %*% dsc)))
}

#' k-shell based gravity centrality (Yang and Xiao 2021)
#'
#' Yang and Xiao (2021), *Knowledge-Based Systems* 227:107198, equations
#' (7)-(10):
#'
#' ```
#' c_ij    = exp((ks_i - ks_j) / (ks_max - ks_min))                    (7)
#' F(i, j) = c_ij k_i k_j / d_ij^2                                     (8)
#' KSGC(i) = sum_{j : d_ij <= 0.5 <d>} F(i, j)                         (10)
#' ```
#'
#' with `ks` the k-shell index, `k` the degree, `d` the hop distance and
#' `<d>` the average shortest-path length. The radius is the real number
#' `0.5 <d>` in a `<=` test, so no rounding is applied; this differs from
#' the rounded `"auto"` radius of cograph's gravity family. A dense network
#' with `<d> < 2` leaves every sum empty.
#'
#' When every node is in one shell the exponent is `0 / 0`. The coefficient
#' is then one: the paper says the method reduces to the gravity model when
#' two nodes have the same k-shell. The paper is written for connected
#' networks; on a disconnected one `<d>` is the mean over the pairs that
#' reach each other.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_ksgc <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  hops <- .cg_hop_matrix(b > 0, n)
  reached <- is.finite(hops) & hops > 0
  if (!any(reached)) return(numeric(n))
  inside <- reached & hops <= 0.5 * mean(hops[reached])
  degree <- rowSums(b)
  shell <- .cg_coreness(b, n)
  spread <- max(shell) - min(shell)
  attraction <- if (spread > 0) exp(outer(shell, shell, "-") / spread) else 1
  force <- attraction * outer(degree, degree) / hops^2
  rowSums(ifelse(inside, force, 0))
}

#' Local structural centrality (Gao et al. 2014)
#'
#' Gao, Ma, Chen, Wang and Xing (2014), *Physica A* 403:130-147, equations
#' (5)-(6). With `G2(u)` the nodes one or two steps from `u` (not `u`) and
#' `c_w` the local clustering coefficient,
#'
#' ```
#' Q(u)   = alpha |G2(u)| + (1 - alpha) sum_{w in G2(u)} c_w
#' LSC(v) = sum_{u in N(v)} Q(u).
#' ```
#'
#' Two conventions are fixed by the ten values of the paper's Table 1 and
#' not by its prose: the two-step neighborhood leaves out its center, and a
#' node of degree below two has clustering zero.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param alpha Weight of the count against the clustering sum, in `[0, 1]`.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_lsc <- function(b, alpha = 0.2) {
  .cg_check_unit(alpha, "lsc_alpha")
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  pairs <- degree * (degree - 1)
  closed_walks <- rowSums((b %*% b) * b)
  clustering <- ifelse(pairs > 0, closed_walks / pmax(pairs, 1), 0)
  within_two <- .cg_within_two(b)
  quality <- alpha * rowSums(within_two) +
    (1 - alpha) * as.numeric(within_two %*% clustering)
  as.numeric(b %*% quality)
}

#' Classified neighbors centrality (Li et al. 2018)
#'
#' Li, Wang, Sun and Xia (2018), *Applied Mathematics and Computation*
#' 320:512-523, equation (1). The k-shell peel removes nodes in batches and
#' numbers the batches; a neighbor `j` of `i` is
#'
#' ```
#' upper        ks(j) > ks(i)
#' equal upper  ks(j) = ks(i), batch(j) >= batch(i)
#' equal lower  ks(j) = ks(i), batch(j) <  batch(i)
#' lower        ks(j) < ks(i)
#' ```
#'
#' and the score is the weighted count of the four classes. A neighbor
#' removed in the same batch is "equal upper": the paper's Table 1 puts it
#' there, while lines 14-15 of its Algorithm 1 would leave it unclassified.
#' The batches are the layers of the onion decomposition.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param weights Weights of the four classes, in the order above.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_classified_neighbors <- function(b, weights = c(0.4, 0.35, 0.25, 0.1)) {
  .cg_check_weight_vector(weights, "cn_weights", 4L)
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  shell <- .cg_coreness(b, n)
  batch <- .cg_onion(b, FALSE)
  shell_gap <- outer(shell, shell, \(own, other) other - own)
  later <- outer(batch, batch, \(own, other) other >= own)
  class_weight <- ifelse(
    shell_gap > 0, weights[1L],
    ifelse(shell_gap < 0, weights[4L],
           ifelse(later, weights[2L], weights[3L])))
  rowSums(b * class_weight)
}

#' Hybrid degree centrality (Ma and Ma 2017)
#'
#' Ma and Ma (2017), *Physica A* 465:312-330, section 3:
#'
#' ```
#' HC(v) = (beta - p) alpha DC(v) + p MLC(v)
#' ```
#'
#' with `DC` the degree, `MLC` the modified local centrality (`.cg_mlc()`)
#' and `p` the spreading probability the ranking is meant for. The paper
#' uses `alpha = 1000`, `beta` between 0.1 and 0.2 and sweeps `p` over
#' `[0.01, 0.1]`; it gives no default for `p`.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param p Spreading probability, in `[0, 1]`.
#' @param alpha Nonnegative scale of the degree term.
#' @param beta Balance of the degree term.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_hybrid_degree <- function(b, p, alpha = 1000, beta = 0.1) {
  .cg_check_unit(p, "hc_p")
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha < 0) {
    .cg_stop_bad_parameter(
      "`hc_alpha` must be a single finite nonnegative number")
  }
  if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta)) {
    .cg_stop_bad_parameter("`hc_beta` must be a single finite number")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  (beta - p) * alpha * rowSums(b) + p * .cg_mlc(b)
}

#' TOPSIS closeness to the ideal alternative (Du et al. 2014)
#'
#' Du, Gao, Hu, Mahadevan and Deng (2014), *Physica A* 399:57-69, equations
#' (6)-(12), for benefit criteria:
#'
#' ```
#' r_ij = x_ij / sqrt(sum_i x_ij^2)                                    (6)
#' v_ij = w_j r_ij                                                     (7)
#' S+_i = sqrt(sum_j (max_i v_ij - v_ij)^2)                            (10)
#' S-_i = sqrt(sum_j (min_i v_ij - v_ij)^2)                            (11)
#' C_i  = S-_i / (S-_i + S+_i)                                         (12)
#' ```
#'
#' A column of zeros stays zero. When every column is constant the ideal
#' and the anti-ideal coincide and the score is `0 / 0`. A column whose
#' range is within 1e-12 of its largest value is constant: an eigenvector
#' of a regular graph is equal across the nodes only up to rounding, and
#' without the rule the score would be a ratio of rounding errors.
#'
#' @param indicators Numeric matrix, one row per alternative and one column
#'   per criterion.
#' @param weights One nonnegative weight per column.
#' @return A list with `value` (the scores, `NA` when undefined) and
#'   `status` (`"ok"` or `"uniform"`).
#' @keywords internal
#' @noRd
.cg_topsis <- function(indicators, weights) {
  norms <- sqrt(colSums(indicators^2))
  weighted <- sweep(sweep(indicators, 2L, ifelse(norms > 0, norms, 1), "/"),
                    2L, weights, "*")
  best <- apply(weighted, 2L, max)
  worst <- apply(weighted, 2L, min)
  constant <- best - worst <= 1e-12 * apply(abs(weighted), 2L, max)
  weighted[, constant] <- 0
  best[constant] <- 0
  worst[constant] <- 0
  to_best <- sqrt(rowSums(sweep(weighted, 2L, best)^2))
  to_worst <- sqrt(rowSums(sweep(weighted, 2L, worst)^2))
  if (all(to_best + to_worst == 0)) {
    return(list(value = rep(NA_real_, nrow(indicators)), status = "uniform"))
  }
  list(value = to_worst / (to_worst + to_best), status = "ok")
}

#' The three attributes of TOPSIS centrality
#'
#' Degree, closeness (the reciprocal of the sum of distances) and either
#' betweenness or eigenvector centrality (Du et al. 2014, section 3).
#' Equation (6) divides each column by its norm, so the scale of an
#' attribute does not matter.
#'
#' @param b Binary symmetric adjacency matrix of a connected network.
#' @param attribute `"betweenness"` or `"eigenvector"`.
#' @return Numeric matrix with three columns.
#' @keywords internal
#' @noRd
.cg_topsis_attributes <- function(b, attribute = "betweenness") {
  n <- nrow(b)
  third <- if (identical(attribute, "betweenness")) {
    .cg_betweenness(b, n, FALSE)
  } else {
    abs(.cg_eigenvector(b, n))
  }
  cbind(rowSums(b), 1 / rowSums(.cg_hop_matrix(b > 0, n)), third)
}

#' All-around score (Hou, Yao and Liao 2012)
#'
#' Hou, Yao and Liao (2012), *Physica A* 391(15):4012-4017, equation (1):
#'
#' ```
#' d(i) = sqrt(||k(i)||^2 + ||C_B(i)||^2 + ||k_s(i)||^2)
#' ```
#'
#' with `k` the degree, `C_B` the betweenness and `k_s` the k-shell index,
#' each rescaled to a common range. The paper gives no formula for the
#' rescaling; each index is divided by its largest value, which keeps zero
#' at zero and is defined for a constant index. An index that is zero for
#' every node adds nothing.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector in `[0, sqrt(3)]`; isolates score zero.
#' @keywords internal
#' @noRd
.cg_all_around <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  indices <- cbind(rowSums(b), .cg_betweenness(b, n, FALSE),
                   .cg_coreness(b, n))
  largest <- apply(indices, 2L, max)
  sqrt(rowSums(sweep(indices, 2L, ifelse(largest > 0, largest, 1), "/")^2))
}

#' Effective gravity model (Li and Xiao 2021)
#'
#' Li and Xiao (2021), *Information Sciences* 578:725-749, equations
#' (16)-(22). With `k` the degree, `ecc_i` the eccentricity of `i` and
#' `k_far` the mean degree of the nodes at that distance,
#'
#' ```
#' R_i    = ecc_i / (1 + sqrt(k_far / k_i))                      (16)-(17)
#' I_i    = - sum_{j in N[i]} p_j log p_j,  p_j = k_j / sum_{u in N[i]} k_u
#' V_i    = I_i k_i                                              (18)-(21)
#' EGM(i) = sum_{j != i : d_ij <= R_i} V_i V_j / d_ij^2          (22)
#' ```
#'
#' `N[i]` is the closed neighborhood. `R_i` is a real number and no
#' rounding is applied, so a node with `R_i < 1` scores zero. The test
#' `d_ij <= R_i` allows a relative slack of 1e-9, because `R_i` is a
#' rounded quotient that can fall a hair below a whole number. The natural
#' logarithm is used; the paper prints `log` without a base.
#'
#' @param b Binary symmetric adjacency matrix of a connected network with
#'   at least two nodes.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_egm <- function(b) {
  n <- nrow(b)
  degree <- rowSums(b)
  hops <- .cg_hop_matrix(b > 0, n)
  eccentricity <- apply(hops, 1L, max)
  farthest <- hops == eccentricity
  far_degree <- as.numeric(farthest %*% degree) / rowSums(farthest)
  radius <- eccentricity / (1 + sqrt(far_degree / degree))
  closed <- b + diag(n)
  share <- closed * matrix(degree, n, n, byrow = TRUE) /
    as.numeric(closed %*% degree)
  value <- .cg_row_entropy(share) * degree
  inside <- hops > 0 & hops <= radius * (1 + 1e-9)
  rowSums(ifelse(inside, outer(value, value) / hops^2, 0))
}

#' Global and local structure (Sheng et al. 2020)
#'
#' Sheng, Dai, Wang, Duan, Long, Zhang, Guan, Hu, Chen and Guan (2020),
#' *Physica A* 541:123262. The measure is computed as the paper's own
#' numbers follow it:
#'
#' ```
#' GLS(i) = [d_i sum_{j != i} 1.1^(c_ij)] [d_i / (n - 1)]
#' ```
#'
#' with `d_i` the degree and `c_ij` the number of common neighbors. The
#' first factor is the global influence of equation (4) summed over all
#' other nodes, as the text and the worked example state (35.13 for the
#' example node; the neighbors-only subscript printed in the equation gives
#' 9.6). The second is what the local influence of equation (6) becomes in
#' the worked example, where every neighbor is given the focal node's `p`:
#' the degree centrality of equation (1). This reading reproduces the
#' worked value 9.58, the rank order of Table 3 and the top ten of Table 4;
#' the literal equations (4)-(7) reproduce none of them.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; isolates score zero, and so does the node of a
#'   one-node network.
#' @keywords internal
#' @noRd
.cg_gls <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (n < 2L) return(0)
  degree <- rowSums(b)
  closeness <- 1.1^(b %*% b)
  diag(closeness) <- 0
  degree * rowSums(closeness) * degree / (n - 1)
}

#' k-shell iteration factor (Wang et al. 2016)
#'
#' Wang, Zhao, Xi and Du (2016), *Physica A* 461:171-181, equations (1)-(2).
#' Stage `k` of the k-shell decomposition removes, in each iteration, every
#' remaining node whose degree is at most `k`, until none is left. For a
#' node removed in iteration `n` of a stage of `m` iterations,
#'
#' ```
#' delta(i) = k (1 + n / m)                                            (1)
#' IC(i)    = delta(i) d_i + sum_{j in N(i)} delta(j) d_j              (2)
#' ```
#'
#' with `d` the degree in the whole network. The paper words the removal as
#' "degree k"; "at most k" is needed, since a node can fall below `k` when
#' two of its neighbors leave in one iteration. The paper's Table 1 cannot
#' come from this peel on any graph, and its Table 2 disagrees with it
#' about one node, so no published value checks the implementation.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_ks_if <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  factor <- numeric(n)
  active <- degree > 0
  stage <- 1
  # Each removal changes the degrees that decide the next one.
  while (any(active)) {
    iteration <- numeric(n)
    count <- 0
    repeat {
      leaving <- active & as.numeric(b %*% active) <= stage
      if (!any(leaving)) break
      count <- count + 1
      iteration[leaving] <- count
      active[leaving] <- FALSE
    }
    removed <- iteration > 0
    factor[removed] <- stage * (1 + iteration[removed] / count)
    stage <- stage + 1
  }
  factor * degree + as.numeric(b %*% (factor * degree))
}

#' NL centrality (Shao et al. 2019)
#'
#' Shao, Liu, Zhao and Liu (2019), *Peer-to-Peer Networking and
#' Applications* 12:1528-1537, equation (5):
#'
#' ```
#' NL(v) = sum_{u in N(v)} [phi(u) + I_uv (k_v - 1) / (k_v + k_u - 2)]
#' ```
#'
#' with `phi(u)` the number of nodes within two steps of `u` and the second
#' term the edge contribution of the DIL measure (`.cg_dil_terms()`).
#' `phi(u)` does not count `u`: the paper's wording and its example list
#' only other nodes. Its tables were not read, so no published value checks
#' this. The share of an isolated edge is `0 / 0` and is taken as zero, as
#' in `dil`.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_nl <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  as.numeric(b %*% rowSums(.cg_within_two(b))) + .cg_dil_terms(b)$contribution
}

#' Entropy-based social influence (Peng et al. 2017)
#'
#' Peng, Yang, Cao, Yu and Xie (2017), *Information Sciences* 379:146-159,
#' equations (2)-(13). With `C_ij` the number of interactions from `i` to
#' `j`,
#'
#' ```
#' friends     W_ij = min(C_ij, C_ji) > 0                          (2)-(4)
#' If_i        = log10(n_i),  n_i the number of friends            (5)
#' Ic_i        = - sum_{j friend} p_ij log10 p_ij,
#'               p_ij = C_ij / sum_{k friend} C_ik                 (6)
#' DI_i        = alpha If_i + (1 - alpha) Ic_i                     (7)
#' II_ik       = DI_i mean_{j common friend of i and k} DI_j       (9)-(11)
#' II_i        = mean of II_ik over the two-step friends k         (12)
#' I_i         = omega DI_i + (1 - omega) II_i                     (13)
#' ```
#'
#' A two-step friend shares a friend with `i` and is not a friend of `i`.
#' Equation (6) uses the outgoing counts, which the paper's Table 4
#' requires. A node without two-step friends has `II = 0`; the paper leaves
#' the `0 / 0` open. The worked example reproduces for all eleven direct
#' values and eight of the eleven indirect ones; the other three follow
#' from slips in the authors' hand calculation.
#'
#' @param counts Nonnegative matrix of interaction counts, row to column.
#' @param alpha Weight of the friend entropy in the direct influence.
#' @param omega Weight of the direct influence in the total.
#' @return A `data.frame` with the columns `direct`, `indirect` and `total`.
#' @keywords internal
#' @noRd
.cg_entropy_influence <- function(counts, alpha = 0.4, omega = 0.6) {
  .cg_check_unit(alpha, "ei_alpha")
  .cg_check_unit(omega, "ei_omega")
  n <- nrow(counts)
  if (is.null(n) || n == 0L) {
    return(data.frame(direct = numeric(), indirect = numeric(),
                      total = numeric()))
  }
  diag(counts) <- 0
  friend <- (pmin(counts, t(counts)) > 0) * 1
  friend_count <- rowSums(friend)
  outgoing <- counts * friend
  share <- outgoing / pmax(rowSums(outgoing), .Machine$double.xmin)
  interaction <- -rowSums(ifelse(share > 0, share * log10(share), 0))
  direct <- alpha * log10(pmax(friend_count, 1)) + (1 - alpha) * interaction
  common <- friend %*% friend
  two_step <- common > 0 & friend == 0 & !diag(n)
  bridge_mean <- (friend %*% (friend * direct)) / pmax(common, 1)
  indirect <- direct * rowSums(ifelse(two_step, bridge_mean, 0)) /
    pmax(rowSums(two_step), 1)
  data.frame(direct = direct, indirect = indirect,
             total = omega * direct + (1 - omega) * indirect)
}

#' Multi-evidence centrality (Mo and Deng 2019)
#'
#' Mo and Deng (2019), *Physica A* 529:121538. Four indices of a node are
#' turned into basic probability assignments over "high", "low" and the
#' whole frame, and fused by Dempster's rule:
#'
#' ```
#' DC(i)  = d_i / (n - 1)                                              (1)
#' BC(i)  = betweenness / ((n - 1)(n - 2) / 2)                         (3)
#' EC(i)  = (1 / n) sum_{j != i} 1 / d_ij                              (4)
#' CoC(i) = correlation centrality                                     (5)
#' m_X(h) = |X_i - X_min| / (X_max - X_min + omega)                (16)-(23)
#' m_X(l) = |X_i - X_max| / (X_max - X_min + omega)
#' MeC(i) = m(h) - m(l)        after fusing the four assignments   (32)-(35)
#' ```
#'
#' `omega > 0` keeps mass on the whole frame, so the conflict of
#' Dempster's rule never reaches one. Betweenness is zero on fewer than
#' three nodes, where the divisor of (3) vanishes.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param omega The constant of the assignments, in `(0, 1]`.
#' @param alpha Distance exponent of correlation centrality.
#' @return Numeric vector in `(-1, 1)`.
#' @keywords internal
#' @noRd
.cg_multi_evidence <- function(b, omega = 1, alpha = 3) {
  if (!is.numeric(omega) || length(omega) != 1L || !is.finite(omega) ||
        omega <= 0 || omega > 1) {
    .cg_stop_bad_parameter(
      "`evidence_omega` must be a single number above 0 and at most 1")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  correlation <- .cg_correlation_centrality(b, alpha)
  if (n < 2L) return(0)
  hops <- .cg_hop_matrix(b > 0, n)
  between <- if (n > 2L) {
    .cg_betweenness(b, n, FALSE) / ((n - 1) * (n - 2) / 2)
  } else {
    numeric(n)
  }
  indices <- list(rowSums(b) / (n - 1), between,
                  rowSums(ifelse(hops > 0, 1 / hops, 0)) / n, correlation)
  assignments <- lapply(indices, \(index) {
    span <- max(index) - min(index) + omega
    high <- (index - min(index)) / span
    low <- (max(index) - index) / span
    cbind(high = high, low = low, frame = 1 - high - low)
  })
  fused <- Reduce(\(first, second) {
    conflict <- first[, "high"] * second[, "low"] +
      first[, "low"] * second[, "high"]
    cbind(
      high = first[, "high"] * (second[, "high"] + second[, "frame"]) +
        first[, "frame"] * second[, "high"],
      low = first[, "low"] * (second[, "low"] + second[, "frame"]) +
        first[, "frame"] * second[, "low"],
      frame = first[, "frame"] * second[, "frame"]
    ) / (1 - conflict)
  }, assignments)
  unname(fused[, "high"] - fused[, "low"])
}
