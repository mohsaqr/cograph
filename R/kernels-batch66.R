#' Pairs within half the average path length
#'
#' The radius of the gravity models of Zhang, Shuai and Lu (2022) and Liu,
#' Wang and Deng (2020) is the real number `0.5 <d>`, compared with `<=`;
#' Zhang et al. say in words that a non-integer radius admits the distances
#' up to its floor. `<d>` is the mean of the finite positive distances, the
#' convention of `.cg_ksgc()` on a disconnected network.
#'
#' @param hops Hop-distance matrix, `Inf` for unreachable pairs.
#' @return Logical matrix; `TRUE` where `0 < d_ij <= <d> / 2`.
#' @keywords internal
#' @noRd
.cg_within_half_mean <- function(hops) {
  reached <- is.finite(hops) & hops > 0
  if (!any(reached)) return(reached)
  reached & hops <= 0.5 * mean(hops[reached])
}

#' K-shell hybrid scores and their improved form
#'
#' Namtirtha, Dutta and Dutta (2018), *Physica A* 499:310-324, equations
#' (1)-(3), and Maji, Namtirtha, Dutta and Curado Malta (2020), *Expert
#' Systems with Applications* 144:113092, equations (16)-(18). With `ks`
#' the k-shell index, `k` the degree, `d` the hop distance and `r` the
#' radius,
#'
#' ```
#' ksh(m)  = sum_{n : 1 <= d_mn <= r} [sqrt(ks_m + ks_n) + mu k_n] / d_mn^2
#' mu(l)   = 2 (r - l + 1) / (r (r + 1))                              (16)
#' kshi(i) = sum_{j : 1 <= d_ij <= r} [sqrt(ks_i ks_j) + mu(d_ij) k_j] / d_ij^2
#' ```
#'
#' The radical covers the shell sum (ksh) or product (kshi) alone and the
#' degree is the partner's: the equation images of both papers, and the 27
#' and 14 values they print for their 13-node example, follow this form.
#' The square distance is fixed by the same values (`1 / d` matches none of
#' them). The level weights of `kshi` sum to one over `l = 1..r`, so its
#' radius must be finite; `ksh` accepts `Inf`, the whole component.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param variant `"ksh"` or `"ikh"`.
#' @param mu Weight of the degree in `ksh`, in `[0, 1]`.
#' @param radius Largest distance summed over.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_kshell_hybrid <- function(b, variant = c("ksh", "ikh"), mu = 0.4,
                              radius = 3) {
  variant <- match.arg(variant)
  .cg_check_radius(radius, "ksh_radius")
  if (identical(variant, "ksh")) {
    .cg_check_unit(mu, "ksh_mu")
  } else if (!is.finite(radius)) {
    .cg_stop_bad_parameter(
      "`ksh_radius` must be finite for \"ikh\" and \"eikh\": their level ",
      "weights are defined for a finite number of levels")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  hops <- .cg_hop_matrix(b > 0, n)
  inside <- is.finite(hops) & hops > 0 & hops <= radius
  shell <- .cg_coreness(b, n)
  partner_degree <- matrix(rowSums(b), n, n, byrow = TRUE)
  pull <- if (identical(variant, "ksh")) {
    sqrt(outer(shell, shell, "+")) + mu * partner_degree
  } else {
    sqrt(outer(shell, shell, "*")) +
      2 * (radius - hops + 1) / (radius * (radius + 1)) * partner_degree
  }
  rowSums(ifelse(inside, pull / hops^2, 0))
}

#' Mixed core, degree and entropy, and its three variants
#'
#' Sheikhahmadi and Nematbakhsh (2017), *Journal of Information Science*
#' 43(3):412-423, equations (8)-(14). With `ks` the k-shell index, `K` its
#' largest value, `d_v` the degree and `p_i(v)` the share of the neighbors
#' of `v` in shell `i`,
#'
#' ```
#' Entropy(v)  = - sum_i p_i(v) log2 p_i(v)                       (9)-(10)
#' WEntropy(v) = - sum_i p_i(v) log2 p_i(v) / (K - i + 1)              (11)
#' MCDE(v)     = ks(v) + d_v  + Entropy(v)                              (8)
#' MCDWE(v)    = ks(v) + d_v  + WEntropy(v)                            (12)
#' MCSDE(v)    = ks~(v) + SL~(v) + Entropy~(v)                         (13)
#' MCSDWE(v)   = ks~(v) + SL~(v) + WEntropy~(v)                        (14)
#' ```
#'
#' The weights alpha, beta and gamma of the paper are 1, as in its sections
#' 3.1 and 4.1. The first two are raw sums: the paper's Table 2 adds the
#' raw core, degree and entropy (for example 3 + 7 + 1.6 = 11.6). The weight
#' of eq. (11) depends on the shell of each summand, as the formula image
#' prints it; the Centrality Zoo's per-node constant is not the paper's.
#'
#' For the semi-local variants the paper says the three amounts are on such
#' different scales that min-max normalisation is used; its wording is
#' `(x - min) / max`, the textbook rule it cites `(x - min) / (max - min)`.
#' The tilde marks the textbook rule applied to each term over the nodes
#' (owner decision 2026-11-26), and a term that is the same for every node
#' becomes 0. `SL` is Chen et al.'s local centrality, the measure
#' `semilocal`: the paper names that method for the semi-local degree.
#' Unscaled, `SL` swamps the other two terms and the score ranks the nodes
#' as `semilocal` does.
#'
#' A node without neighbors has no distribution in eq. (10); its entropy is
#' 0, so an isolate scores 0 in every variant.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param semi_local Use the scaled semi-local form (eqs. 13-14).
#' @param weighted Use the weighted entropy (eqs. 11, 12 and 14).
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_mixed_core <- function(b, semi_local = FALSE, weighted = FALSE) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  shell <- .cg_coreness(b, n)
  degree <- rowSums(b)
  levels <- seq_len(max(shell))
  shares <- (b %*% outer(shell, levels, "==")) / ifelse(degree > 0, degree, 1)
  level_weight <- if (weighted) 1 / (max(shell) - levels + 1) else 1
  entropy <- -as.numeric(ifelse(shares > 0, shares * log2(shares), 0) %*%
                           rep_len(level_weight, length(levels)))
  if (!semi_local) return(shell + degree + entropy)
  scale <- \(x) {
    spread <- max(x) - min(x)
    if (spread > 0) (x - min(x)) / spread else numeric(length(x))
  }
  # Chen et al.'s local centrality, which `semilocal` computes.
  local <- as.numeric(b %*% (b %*% rowSums(.cg_within_two(b))))
  scale(shell) + scale(local) + scale(entropy)
}

#' Laplacian gravity centrality (Zhang, Shuai and Lu 2022)
#'
#' Zhang, Shuai and Lu (2022), *Information Sciences* 618:98-117, equations
#' (10)-(12):
#'
#' ```
#' LC(i)  = E(G) - E(G - i) = k_i^2 + k_i + 2 sum_{j in N(i)} k_j    (10)-(11)
#' LGC(i) = sum_{j != i : d_ij <= <d>/2} LC(i) LC(j) / d_ij^2            (12)
#' ```
#'
#' The mass is the absolute drop in Laplacian energy, the measure
#' `laplacian` on the skeleton; the paper's Table 1 prints it as integers.
#' The relative drop the Centrality Zoo gives changes the scores by one
#' constant. A pair in different components contributes nothing (section
#' 3.1). When `<d> < 2` no pair is within the radius and every score is 0.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_lgc <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  hops <- .cg_hop_matrix(b > 0, n)
  degree <- rowSums(b)
  mass <- degree^2 + degree + 2 * as.numeric(b %*% degree)
  rowSums(ifelse(.cg_within_half_mean(hops), outer(mass, mass) / hops^2, 0))
}

#' Weighted gravity model (Liu, Wang and Deng 2020)
#'
#' Liu, Wang and Deng (2020), *Knowledge-Based Systems* 193:105464,
#' equations (7)-(9):
#'
#' ```
#' C_WG(i) = sum_{j : d_ij <= <d>/2} e_i k_i k_j / d_ij^2              (9)
#' ```
#'
#' with `e` the leading eigenvector of the adjacency matrix and `k` the
#' degree; `j = i` is left out, as in the gravity model the paper extends
#' (its eq. 6). The paper calls `e` normalized without naming a norm; it
#' has unit Euclidean length here (owner decision 2026-11-26), which is a
#' constant factor away from the maximum-one scaling of `eigenvector`.
#'
#' On a disconnected network the leading eigenvector lives on the
#' component of largest spectral radius, so the nodes of the other
#' components score 0. When two components share that spectral radius the
#' eigenvector is not unique and the score has no value. Eigenvalues within
#' a relative 1e-9 of the largest count as equal to it.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return A list with `value` (the scores, `NA` when undefined) and
#'   `status` (`"ok"` or `"tied"`).
#' @keywords internal
#' @noRd
.cg_wgravity <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(list(value = numeric(0), status = "ok"))
  if (!any(b > 0)) return(list(value = numeric(n), status = "ok"))
  spectrum <- eigen(b, symmetric = TRUE, only.values = TRUE)$values
  top <- max(spectrum)
  if (sum(spectrum >= top - 1e-9 * max(top, 1)) > 1L) {
    return(list(value = rep(NA_real_, n), status = "tied"))
  }
  leading <- .cg_eigenvector(b, n)
  leading <- leading / sqrt(sum(leading^2))
  hops <- .cg_hop_matrix(b > 0, n)
  degree <- rowSums(b)
  gravity <- rowSums(ifelse(.cg_within_half_mean(hops),
                            outer(degree, degree) / hops^2, 0))
  list(value = leading * gravity, status = "ok")
}

#' Average shortest path centrality (Lv et al. 2019)
#'
#' Lv, Zhao, Xiong and Chen (2019), *Physica A* 523:488-497, equations
#' (9)-(11):
#'
#' ```
#' ASP[G] = sum_{i != j} d_ij / (N (N - 1))                             (9)
#' AC[k]  = |ASP[G'_k] - ASP[G]| / ASP[G]                             (11)
#' ```
#'
#' with an unreachable pair given `D`, the diameter of `G` before removal
#' (the text before eq. 10). `G'_k` is `G` with every edge of `k` removed
#' and `k` kept as an isolated node: the paper's worked example says so and
#' divides by `9 (9 - 1)` after removing a node of its 9-node network, and
#' its eleven printed values follow this reading and not the `N - 1` nodes
#' of the prose. `N (N - 1)` then cancels, and a substituted `D` is at least
#' every finite distance, so the score is the relative growth of the total
#' distance and is never negative. On a disconnected input the unreachable
#' pairs of `G` also count `D`, the largest finite distance (owner decision
#' 2026-11-26).
#'
#' The distances of `G'_k` are those of `G - k` plus `2 (N - 1)` pairs at
#' `D` for `k` itself. An edgeless network, where `D` does not exist,
#' scores 0: every pair carries the same substitute whatever it is, so
#' nothing changes. So does a one-node network, whose eq. (9) is `0 / 0`.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_asp_centrality <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (n < 2L) return(0)
  hops <- .cg_hop_matrix(b > 0, n)
  finite <- is.finite(hops) & hops > 0
  if (!any(finite)) return(numeric(n))
  diameter <- max(hops[finite])
  # The diagonal is 0, so the sum over all cells is the off-diagonal sum.
  total <- \(d) sum(ifelse(is.finite(d), d, diameter))
  base <- total(hops)
  vapply(seq_len(n), \(k) {
    rest <- b[-k, -k, drop = FALSE]
    (total(.cg_hop_matrix(rest > 0, n - 1L)) + 2 * (n - 1) * diameter - base) /
      base
  }, numeric(1))
}

#' Degree assortativity of a simple undirected graph (Newman 2002)
#'
#' The Pearson correlation of the degrees at the two ends of an edge, each
#' edge counted in both directions. It is `0 / 0`, and `NA` is returned,
#' when every node with a tie has the same degree, and when there is no
#' edge. Degrees are whole numbers, so that test is exact.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return A single number in `[-1, 1]`, or `NA`.
#' @keywords internal
#' @noRd
.cg_degree_assortativity <- function(b) {
  degree <- rowSums(b)
  ends <- which(b > 0, arr.ind = TRUE)
  near <- degree[ends[, 1L]]
  far <- degree[ends[, 2L]]
  if (length(unique(near)) < 2L) return(NA_real_)
  near <- near - mean(near)
  far <- far - mean(far)
  sum(near * far) / sqrt(sum(near^2) * sum(far^2))
}

#' Weight degree centrality and its extension (Liu et al. 2016)
#'
#' Liu, Wei, Du, Xiao and Deng (2016), *Chaos, Solitons & Fractals* 86:1-7,
#' equations (4)-(9):
#'
#' ```
#' Wdc(i)  = (sum_{j in N(i)} d_j - d_i) d_i^alpha                  (4)-(5)
#' EWdc(i) = (sum_{j in N(i)} Wdc(j) - Wdc(i)) Wdc(i)^alpha         (6)-(7)
#' alpha   = |r|, r the degree assortativity                        (8)-(9)
#' ```
#'
#' The paper tries `alpha = |r|` and says other values can be used; one
#' `alpha` serves both levels. `r` is `0 / 0` when every node with a tie has
#' the same degree; `alpha` is then 0 (owner decision 2026-11-26), the
#' paper's neutral case. `x^0 = 1`, also for `x = 0`. `Wdc` with
#' `alpha = 0` is the measure `con` on the skeleton, and `EWdc` can be
#' negative: a star leaf has `sum Wdc(j) = Wdc(center) = 0`.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param alpha Exponent, or `NULL` for `|r|`.
#' @param extended Return `EWdc` instead of `Wdc`.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_weight_degree <- function(b, alpha = NULL, extended = FALSE) {
  if (!is.null(alpha) && (!is.numeric(alpha) || length(alpha) != 1L ||
                            !is.finite(alpha) || alpha < 0)) {
    .cg_stop_bad_parameter(
      "`wdc_alpha` must be NULL or a single finite nonnegative number")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (is.null(alpha)) {
    assortativity <- .cg_degree_assortativity(b)
    alpha <- if (is.na(assortativity)) 0 else abs(assortativity)
  }
  degree <- rowSums(b)
  wdc <- (as.numeric(b %*% degree) - degree) * degree^alpha
  if (!extended) return(wdc)
  (as.numeric(b %*% wdc) - wdc) * wdc^alpha
}
