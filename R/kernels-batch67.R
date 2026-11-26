#' Mapping entropy (Nie, Guo, Zhao and Lu 2016)
#'
#' Nie, Guo, Zhao and Lu (2016), *Physica A* 453:290-297, equation (3):
#'
#' ```
#' ME_i = - DC_i sum_{j in N(i)} log DC_j,   DC = d / (N - 1)
#' ```
#'
#' The paper cites "degree centrality" for `DC` without a formula. `DC` is
#' the degree divided by `N - 1` (owner decision 2026-11-26): only then is
#' its equation (1) an entropy, and only then does a removal of nodes in
#' decreasing score beat a degree attack on the two networks of its Table 1
#' held locally, as the paper reports. With the raw degree the score is
#' never positive and ranks leaves first. Natural logarithm; the base only
#' rescales. A node without neighbors has an empty sum and scores 0, and so
#' does the only node of a one-node network.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector, never negative.
#' @keywords internal
#' @noRd
.cg_mapping_entropy <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (n < 2L) return(0)
  dc <- rowSums(b) / (n - 1)
  log_dc <- log(ifelse(dc > 0, dc, 1))
  -dc * as.numeric(b %*% log_dc)
}

#' Degree and clustering coefficient centrality (Yang et al. 2020)
#'
#' Yang, Wang, Chen, Hu and Ruan (2020), *IEEE Access* 8:58742-58751,
#' equations (7)-(12). With `d` the degree, `C` the local clustering
#' coefficient (0 below degree 2) and `N2(i)` the nodes within two steps of
#' `i`, `i` itself left out,
#'
#' ```
#' I_D(i)  = d_i + sum_{j in N(i)} d_j                              (8)
#' I_C(i)  = exp(-C_i) sum_{j in N2(i)} C_j                         (8)
#' r_kj    = D_kj / sqrt(sum_j D_kj^2)                             (10)
#' E_k     = -(1 / ln n) sum_j r_kj ln r_kj                        (11)
#' w_k     = (1 - E_k) / (2 - E_1 - E_2)                           (12)
#' DCC(i)  = w_1 I_D(i) + w_2 I_C(i)                                (8)
#' ```
#'
#' "Within two steps", not "exactly two": the clustering sum the paper
#' prints for node 8 of its Fig. 1(b) and its karate top five need it. The
#' rows are divided by their Euclidean norm, as the TeX of eq. (10) has it
#' and as the printed `E_1 > 1` requires. The entropy weights the paper
#' prints for its example (`r`, `E_1`, `alpha`, `DCC(1)`) do not follow from
#' the network drawn in its figure; its attribute values do.
#'
#' On a network without a triangle every `C` is 0, the second row of eq.
#' (10) is `0 / 0` and the score has no value (owner decision 2026-11-26).
#' It has none either when `E_1 + E_2 = 2`, where eq. (12) divides by zero.
#' The clustering counts are whole numbers, so the triangle test is exact.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return A list with `value` (the scores, `NA` when undefined) and
#'   `status` (`"ok"`, `"no_triangle"` or `"degenerate"`).
#' @keywords internal
#' @noRd
.cg_dcc <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(list(value = numeric(0), status = "ok"))
  degree <- rowSums(b)
  closed_walks <- rowSums((b %*% b) * b)
  if (all(closed_walks == 0)) {
    return(list(value = rep(NA_real_, n), status = "no_triangle"))
  }
  clustering <- ifelse(degree >= 2,
                       closed_walks / pmax(degree * (degree - 1), 1), 0)
  attributes <- rbind(
    degree + as.numeric(b %*% degree),
    exp(-clustering) * as.numeric(.cg_within_two(b) %*% clustering))
  shares <- attributes / sqrt(rowSums(attributes^2))
  entropy <- -rowSums(ifelse(shares > 0, shares * log(shares), 0)) / log(n)
  denominator <- 2 - sum(entropy)
  if (abs(denominator) <= sqrt(.Machine$double.eps)) {
    return(list(value = rep(NA_real_, n), status = "degenerate"))
  }
  weight <- (1 - entropy) / denominator
  list(value = as.numeric(weight %*% attributes), status = "ok")
}

#' Edge clustering coefficient centrality (Wang, Li, Wang and Pan 2012)
#'
#' Wang, Li, Wang and Pan (2012), *IEEE/ACM Transactions on Computational
#' Biology and Bioinformatics* 9(4):1070-1080, equations (7)-(8). With
#' `z_uv` the number of triangles on the edge `(u, v)`,
#'
#' ```
#' ECC(u, v) = z_uv / min(d_u - 1, d_v - 1)                          (7)
#' NC(u)     = sum_{v in N(u)} ECC(u, v)                             (8)
#' ```
#'
#' There is no `+ 1` in the numerator: the paper's worked value
#' `ECC(1, 4) = 2/3` on its Fig. 1 rules it out. An edge with an end of
#' degree one is in no triangle and its denominator is 0; it contributes 0
#' (owner decision 2026-11-26). Isolated nodes score 0.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_edge_clustering <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  room <- outer(degree, degree, pmin) - 1
  triangles <- b %*% b
  rowSums(ifelse(b > 0 & room > 0, triangles / pmax(room, 1), 0))
}

#' Analytic hierarchy process centrality (Bian, Hu and Deng 2017)
#'
#' Bian, Hu and Deng (2017), *Physica A* 479:422-436, Steps 1-4 of section
#' 4.1, as the worked example of section 4.2 computes them. For each
#' criterion `x_j` (degree, betweenness, closeness `1 / sum of distances`)
#' the pairwise matrix has `b_ih = x_ij / x_hj`, and 0 where either value is
#' 0 (the printed `B(2)`). Its priority vector, the column-normalised row
#' mean with `0 / 0 = 0` (the printed `s(2)` sums to 0.7, not 1), is
#'
#' ```
#' s_j(i) = (N+_j / N) x_ij / sum_l x_lj,  N+_j = #{l : x_lj > 0}
#' AHP(i) = sum_j w_j s_j(i)                                          (12)
#' ```
#'
#' The weights `w` come from a simulation in the paper (Step 2), whose
#' printed spreading values cannot be regenerated. As the authors evaluate
#' it, with the raw spreading count in eq. (15), the rule gives weights
#' within 1e-3 of one third for any realistic count; the weights are an
#' argument with one third each as default (owner decision 2026-11-26).
#' Each share is invariant to the scale of its criterion, so the
#' betweenness normalisation of the paper does not matter. The `N+ / N`
#' factor is kept: the printed `s(2)` and scores follow it.
#'
#' @param b Binary symmetric adjacency matrix of a connected network with
#'   at least two nodes.
#' @param weights Weights of degree, betweenness and closeness.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_ahp <- function(b, weights = c(1, 1, 1) / 3) {
  n <- nrow(b)
  criteria <- cbind(rowSums(b), .cg_betweenness(b, n, FALSE),
                    1 / rowSums(.cg_hop_matrix(b > 0, n)))
  totals <- colSums(criteria)
  positive <- colSums(criteria > 0)
  shares <- sweep(criteria, 2L, ifelse(totals > 0, totals, 1), "/")
  as.numeric(shares %*% (weights * positive / n))
}

#' Check the parameters of the semi-local iterative algorithm
#' @keywords internal
#' @noRd
.cg_check_semi_ia_args <- function(radius, gamma, max_iter) {
  if (!is.null(radius)) .cg_check_radius(radius, "semi_ia_radius")
  .cg_check_unit(gamma, "semi_ia_gamma")
  if (!is.numeric(max_iter) || length(max_iter) != 1L || !is.finite(max_iter) ||
        max_iter < 1 || max_iter != trunc(max_iter)) {
    .cg_stop_bad_parameter(
      "`semi_ia_max_iter` must be a single whole number of at least one")
  }
  invisible(NULL)
}

#' Semi-local iterative algorithm (Luan, Bao and Zhang 2021)
#'
#' Luan, Bao and Zhang (2021), *Journal of Systems Science and Complexity*
#' 34(6):2168-2181, equations (8)-(9). With `d_ij` the distance and `n_ij`
#' the number of shortest paths,
#'
#' ```
#' W_ij     = n_ij^gamma / d_ij   for 1 <= d_ij <= r, else 0
#' X(t + 1) = W X'(t),  X'(t) = X(t) / ||X(t)||,  X(0) = 1        (8)-(9)
#' ```
#'
#' Equation (8) is printed with `X'_i` inside the sum; that form ranks by
#' the row sums of `W` and collapses onto one node, so `X'_j` is used (owner
#' decision 2026-11-26), the reading of the EC and HITS updates the paper
#' says it follows. The norm is Euclidean, as in the HITS normalisation of
#' its eq. (6). The score is `X` at the steady state, so it tends to
#' `lambda_1 v_1` with `v_1` the unit leading eigenvector of `W`.
#'
#' The paper stops when two successive normalised vectors differ by less
#' than 1e-5; here the largest absolute difference must fall to 1e-12, so
#' the score is the steady state to rounding. The number of updates is
#' bounded by `max_iter`.
#'
#' @param b Binary symmetric adjacency matrix of a connected network.
#' @param radius Largest distance summed over, or `NULL` for 3 below 1000
#'   nodes and 5 from there on (section 2.2).
#' @param gamma Exponent of the path count, in `[0, 1]`.
#' @param max_iter Largest number of updates.
#' @return A list with `value` (the scores), `converged`, `iterations` and
#'   `gap` (the last largest absolute difference).
#' @keywords internal
#' @noRd
.cg_semi_ia <- function(b, radius = NULL, gamma = 0.2, max_iter = 10000) {
  .cg_check_semi_ia_args(radius, gamma, max_iter)
  n <- nrow(b)
  radius <- radius %||% if (n < 1000L) 3 else 5
  hops <- .cg_hop_matrix(b > 0, n)
  inside <- is.finite(hops) & hops >= 1 & hops <= radius
  pull <- ifelse(inside, .cg_geodesic_counts(b, hops)^gamma / hops, 0)
  state <- list(value = rep(1, n), unit = rep(1, n) / sqrt(n), gap = Inf,
                iterations = 0L)
  # A fixed-point iteration: the number of updates depends on the spectral
  # gap of `pull` and is only known when the stopping rule is met.
  while (state$iterations < max_iter && !(state$gap < 1e-12)) {
    value <- as.numeric(pull %*% state$unit)
    unit <- value / sqrt(sum(value^2))
    state <- list(value = value, unit = unit, gap = max(abs(unit - state$unit)),
                  iterations = state$iterations + 1L)
  }
  list(value = state$value, converged = state$gap < 1e-12,
       iterations = state$iterations, gap = state$gap)
}

#' Synthesize centrality (Liu, Yu and Lu 2013)
#'
#' Liu, Yu and Lu (2013), *IET International Conference on Smart and
#' Sustainable City 2013*, pp. 268-271, equations (6)-(9):
#'
#' ```
#' d_d(v) = deg(v) / (n - 1)                                          (6)
#' d_b(v) = sum_{s != v != t} sigma_st(v) / sigma_st / (n - 1)^2      (7)
#' d_c(v) = sum_i l(i, v) / (n - 1)^2,   l = 1000 if unreachable      (8)
#' SC(v)  = (d_d(v) + d_b(v)) / d_c(v)                                (9)
#' ```
#'
#' Equation (8) sums distances, so the denominator is a farness. The case
#' formula of eq. (7) says "not containing v"; its prose, used here, counts
#' the shortest paths through `v` over ordered pairs, twice the usual
#' betweenness. The primes of eq. (9), "the normalized" quantities, are
#' eqs. (6)-(8) themselves, with no second rescaling (owner decision
#' 2026-11-26). The unreachable distance 1000 is the paper's constant.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal and at
#'   least two nodes.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_synthesize <- function(b) {
  n <- nrow(b)
  hops <- .cg_hop_matrix(b > 0, n)
  farness <- colSums(ifelse(is.finite(hops), hops, 1000)) / (n - 1)^2
  through <- 2 * .cg_betweenness(b, n, FALSE) / (n - 1)^2
  (rowSums(b) / (n - 1) + through) / farness
}

#' Self-avoiding walk probabilities, by exact enumeration
#'
#' `P[i, j]` is the probability that a self-avoiding random walk started at
#' `i` is at `j` after `steps` steps (Travencolo and Costa 2008, section 2).
#' Each step goes to an unvisited neighbor, all equally likely. A walk with
#' no unvisited neighbor is stopped: with `stuck = "vanish"` it leaves the
#' analysis, with `"stay"` it stays at its last node. Every walk is
#' enumerated, the limit of the paper's sampled walks; the number of walks
#' grows exponentially with `steps`, so their total is bounded by `limit`.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param steps Number of steps, a whole number of at least one.
#' @param stuck `"vanish"` or `"stay"`.
#' @param limit Largest number of walk extensions.
#' @return Numeric `n x n` matrix; raises `cograph_enumeration_limit` when
#'   the enumeration passes `limit`.
#' @keywords internal
#' @noRd
.cg_saw_probabilities <- function(b, steps, stuck = c("vanish", "stay"),
                                  limit = 1e6) {
  stuck <- match.arg(stuck)
  n <- nrow(b)
  arcs <- which(b > 0, arr.ind = TRUE)
  arcs <- arcs[order(arcs[, 1L], arcs[, 2L]), , drop = FALSE]
  degree <- tabulate(arcs[, 1L], n)
  offset <- cumsum(c(0L, degree))[seq_len(n)]
  walks <- list(path = matrix(seq_len(n), ncol = 1L), prob = rep(1, n),
                extensions = 0)
  # One pass per step; within a pass every walk is extended at once.
  for (step in seq_len(steps)) {
    path <- walks$path
    here <- path[, ncol(path)]
    owner <- rep(seq_len(nrow(path)), degree[here])
    target <- arcs[sequence(degree[here], offset[here] + 1L), 2L]
    fresh <- rowSums(path[owner, , drop = FALSE] == target) == 0
    owner <- owner[fresh]
    target <- target[fresh]
    choices <- tabulate(owner, nrow(path))
    extensions <- walks$extensions + length(owner)
    if (extensions > limit) .cg_stop_enumeration()
    kept <- if (identical(stuck, "stay")) which(choices == 0L) else integer(0)
    walks <- list(
      path = rbind(cbind(path[owner, , drop = FALSE], target),
                   cbind(path[kept, , drop = FALSE], here[kept])),
      prob = c(walks$prob[owner] / choices[owner], walks$prob[kept]),
      extensions = extensions)
  }
  out <- matrix(0, n, n)
  if (!length(walks$prob)) return(out)
  cell <- (walks$path[, ncol(walks$path)] - 1L) * n + walks$path[, 1L]
  total <- rowsum(walks$prob, cell)
  out[as.integer(rownames(total))] <- total[, 1L]
  out
}

#' Check the step count and the stopped-walk rule of inward accessibility
#' @keywords internal
#' @noRd
.cg_check_ia_args <- function(steps, stuck) {
  if (!is.null(steps) && (!is.numeric(steps) || length(steps) != 1L ||
                            !is.finite(steps) || steps < 1 ||
                            steps != trunc(steps))) {
    .cg_stop_bad_parameter(
      "`ia_steps` must be NULL or a single whole number of at least one")
  }
  if (!is.character(stuck) || length(stuck) != 1L ||
        !stuck %in% c("vanish", "stay")) {
    .cg_stop_bad_parameter("`ia_stuck` must be \"vanish\" or \"stay\"")
  }
  invisible(NULL)
}

#' Inward accessibility (Travencolo and Costa 2008)
#'
#' Travencolo and Costa (2008), *Physics Letters A* 373(1):89-95, equations
#' (4)-(5). With `P_h(j -> i)` the probability that a self-avoiding walk
#' started at `j` is at `i` after `h` steps,
#'
#' ```
#' E_h(i) = - sum_{j != i} q_ji log q_ji,  q_ji = P_h(j -> i) / (N - 1)   (5)
#' IA_h(i) = exp(E_h(i)) / (N - 1)                                        (4)
#' ```
#'
#' Natural logarithm (the paper's Fig. 2 prints log 3 as 1.1). The paper
#' leaves the treatment of stopped walks to the application; its own
#' application removes them, the default here (owner decision 2026-11-26).
#' A node no walk reaches has `E = 0` and scores `1 / (N - 1)`, the floor
#' of the measure; the largest possible score is 1.
#'
#' @param b Binary symmetric adjacency matrix with at least two nodes.
#' @param steps Number of steps `h`.
#' @param stuck `"vanish"` or `"stay"`.
#' @param limit Largest number of walk extensions.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_inward_accessibility <- function(b, steps, stuck = "vanish", limit = 1e6) {
  n <- nrow(b)
  incoming <- .cg_saw_probabilities(b, steps, stuck, limit) / (n - 1)
  diag(incoming) <- 0
  exp(-colSums(ifelse(incoming > 0, incoming * log(incoming), 0))) / (n - 1)
}

#' Check the confidence strength of the improved entropy-based centrality
#' @keywords internal
#' @noRd
.cg_check_ie_beta <- function(beta) {
  if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta)) {
    .cg_stop_bad_parameter("`ie_beta` must be a single finite number")
  }
  invisible(beta)
}

#' Improved entropy-based centrality (Ni, Yang and Kong 2020)
#'
#' Ni, Yang and Kong (2020), *Physica A* 545:123659, equations (2)-(10).
#' With `w` the edge weights, `k` the degree and `N(i)` the neighbors,
#'
#' ```
#' DI^w_i = - sum_{j in N(i)} p_ij log10 p_ij,  p_ij = w_ij / sum_l w_il   (2)
#' T_ij   = k_j^beta / sum_{l in N(i)} k_l^beta                           (3)
#' DI^c_i = - sum_{j in N(i)} T_ij log10 T_ij                             (4)
#' DI_i   = theta DI^w_i + (1 - theta) DI^c_i                             (5)
#' II_ik  = mean over the common neighbors j of i and k of DI_i DI_j      (6)
#' II_i   = mean of II_ik over the nodes k two steps from i               (8)
#' I_i    = phi DI_i + (1 - phi) II_i                                    (10)
#' ```
#'
#' `beta = 2` and `theta = phi = 0.5` are the paper's basic values (section
#' 5). A node two steps away shares a neighbor with `i` and is not adjacent
#' to it. A node without such a node has `II = 0`, which the paper leaves
#' as `0 / 0`; an isolated node scores 0. With unit weights
#' `DI^w_i = log10 k_i`.
#'
#' @param w Nonnegative symmetric weight matrix with a zero diagonal.
#' @param beta Exponent of the neighbor degree.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_improved_entropy <- function(w, beta = 2) {
  .cg_check_ie_beta(beta)
  n <- nrow(w)
  if (is.null(n) || n == 0L) return(numeric(0))
  b <- (w > 0) * 1
  degree <- rowSums(b)
  entropy10 <- \(p) -rowSums(ifelse(p > 0, p * log10(p), 0))
  weight_share <- w / ifelse(rowSums(w) > 0, rowSums(w), 1)
  # A neighbor has degree at least one, so a negative `beta` stays finite.
  pull <- ifelse(b > 0, matrix(pmax(degree, 1)^beta, n, n, byrow = TRUE), 0)
  confidence <- pull / ifelse(rowSums(pull) > 0, rowSums(pull), 1)
  direct <- 0.5 * entropy10(weight_share) + 0.5 * entropy10(confidence)
  common <- b %*% b
  two_step <- common > 0 & b == 0 & !diag(n)
  bridge_mean <- (b %*% (b * direct)) / pmax(common, 1)
  indirect <- direct * rowSums(ifelse(two_step, bridge_mean, 0)) /
    pmax(rowSums(two_step), 1)
  0.5 * direct + 0.5 * indirect
}
