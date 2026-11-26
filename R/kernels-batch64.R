#' Check a radius-like argument: a whole number of at least one, or Inf
#' @keywords internal
#' @noRd
.cg_check_radius <- function(radius, name) {
  if (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
        radius < 1 || (is.finite(radius) && radius != trunc(radius))) {
    .cg_stop_bad_parameter(sprintf(
      "`%s` must be a single whole number of at least one, or Inf", name))
  }
  invisible(radius)
}

#' RMD-weighted degree and its extension (Yang et al. 2018)
#'
#' Yang, Li, Xu, Liu, Wang, Zhang, Zhang and Yao (2018), *Physics Letters A*
#' 382(34):2361-2371. The remaining-minimum-degree decomposition removes, in
#' each round, every node whose current degree equals the current minimum;
#' `Iter(v)` is the round in which `v` leaves and `MaxIter` the number of
#' rounds. That peel is the one `.cg_iksd()` computes.
#'
#' ```
#' WD(v)  = sum_{u in N(v)} Iter(u) / MaxIter                         (1)
#' EWD(v) = sum_{u in N(v)} WD(u)                                     (2)
#' ```
#'
#' The inner sum of equation (2) is unrestricted, so the focal node is
#' counted once per neighbor. The paper's worked value `EWD(15) = 11.429`
#' on its 15-node example requires it (6.43 without).
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param extended `TRUE` for EWD, `FALSE` for WD.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_rmd_weighted_degree <- function(b, extended = FALSE) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  round_removed <- .cg_iksd(b)
  wd <- as.numeric(b %*% round_removed) / max(round_removed)
  if (extended) as.numeric(b %*% wd) else wd
}

#' Modified local centrality (Ma and Ma 2017)
#'
#' Ma and Ma (2017), *Physica A* 465:312-330, section 3. With `N(w)` the
#' number of nodes within two steps of `w` (not counting `w`) and `G(v)` the
#' neighbors of `v`,
#'
#' ```
#' LC(v)  = sum_{u in G(v)} sum_{w in G(u)} N(w)
#' MLC(v) = LC(v) - 2 sum_{u in G(v)} degree(u).
#' ```
#'
#' `LC` is the semi-local centrality of Chen et al. (2012). The inner sum
#' includes `w = v`. The paper prints no value of the measure.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; it can be negative (both ends of an isolated
#'   edge score -1).
#' @keywords internal
#' @noRd
.cg_mlc <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  within_two <- (b + b %*% b) > 0
  diag(within_two) <- FALSE
  local <- as.numeric(b %*% (b %*% rowSums(within_two)))
  local - 2 * as.numeric(b %*% rowSums(b))
}

#' Spreading probability centrality (Bao, Ma, Xiang and Zhang 2017)
#'
#' Bao, Ma, Xiang and Zhang (2017), *Physica A* 468:391-397, equation (5):
#'
#' ```
#' SP(i) = sum_{j : 1 <= d_ij <= r} sigma_ij (1 / <k>)^d_ij
#' ```
#'
#' with `sigma_ij` the number of shortest paths between `i` and `j`, `<k>`
#' the mean degree of the whole network and `r` the coverage radius, 3 in
#' the paper. The node itself is left out: it would add one to every score.
#' A network without edges has mean degree zero and every sum empty, so
#' every node scores zero.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param radius Coverage radius: a whole number of at least one, or `Inf`.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_spreading_probability <- function(b, radius = 3) {
  .cg_check_radius(radius, "sp_radius")
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  mean_degree <- sum(b) / n
  if (mean_degree == 0) return(numeric(n))
  hops <- .cg_hop_matrix(b > 0, n)
  inside <- is.finite(hops) & hops >= 1 & hops <= radius
  rowSums(ifelse(inside, .cg_geodesic_counts(b, hops) * mean_degree^(-hops), 0))
}

#' ProfitLeader (Yu, Shao, Yang and Sun 2019)
#'
#' Yu, Shao, Yang and Sun (2019), *World Wide Web* 22:533-553, equations
#' (1)-(4). With `N[u]` the closed neighborhood and `d` the degree,
#'
#' ```
#' AR(u -> v) = d_u + 1 + sum_{s in N(u) \ N[v]} d_s      available resource
#' SP(u, v)   = |N[u] n N[v]| / |N[u] u N[v]|             sharing probability
#' PC(u)      = sum_{v in N(u)} AR(u -> v) SP(u, v).      profit capacity
#' ```
#'
#' The closed neighborhoods follow from the paper's equation (5), which
#' writes the sharing probability of adjacent nodes with `c` common
#' neighbors as `(c + 2) / (d_u + 1 + e_v)`. The `+ 1` in `AR` is that of
#' the code the authors released (owner decision 2026-11-26): with it the
#' 40 top-ten positions of the paper's Table 5 on karate, dolphins, adjnoun
#' and lesmis are reproduced, without it 36. The article's equation (2)
#' and the closed forms printed with its Fig. 3 have no `+ 1`.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_profitleader <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  closed <- b + diag(n)
  shared <- closed %*% closed
  joint <- outer(degree + 1, degree + 1, "+") - shared
  # Degrees of the neighbors of u that are also in the closed
  # neighborhood of v, for every pair at once.
  overlap_degree <- b %*% (degree * closed)
  resource <- degree + 1 + as.numeric(b %*% degree) - overlap_degree
  rowSums(b * resource * shared / joint)
}

#' Hierarchical k-shell (Zareie and Sheikhahmadi 2018)
#'
#' Zareie and Sheikhahmadi (2018), *Expert Systems with Applications*
#' 93:200-211, Algorithms 1-2 and equations (10)-(11).
#'
#' `b_i` is the round in which node `i` is removed when the peel starts at
#' shell 1, removes in each round every remaining node whose current degree
#' is at most the shell, and raises the shell only when a round removes
#' nothing. A core is a node none of whose neighbors has a larger `b`.
#' Algorithm 2 spreads the `b` of each core outward, losing one per step,
#' and keeps the largest value that arrives, which is
#'
#' ```
#' f_i    = max_u (b_u - d_iu)        over the nodes u that i can reach
#' S(i)   = sum_{j in N(i)} k_j (b_j + f_j)                          (10)
#' HKS(i) = sum_{j in N(i)} S(j).                                    (11)
#' ```
#'
#' Two slips in the printed pseudo-code (a flag reset inside a loop and a
#' level that is never lowered) are resolved by the prose of the paper.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_hks <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  round_removed <- numeric(n)
  active <- rep(TRUE, n)
  shell <- 1
  round <- 0
  # Each removal changes the degrees that decide the next one.
  while (any(active)) {
    leaving <- active & as.numeric(b %*% active) <= shell
    if (any(leaving)) {
      round <- round + 1
      round_removed[leaving] <- round
      active[leaving] <- FALSE
    } else {
      shell <- shell + 1
    }
  }
  hops <- .cg_hop_matrix(b > 0, n)
  reach <- matrix(round_removed, n, n, byrow = TRUE) - hops
  level <- apply(reach, 1L, max)
  score <- rowSums(b) * (round_removed + level)
  as.numeric(b %*% (b %*% score))
}

#' Node importance by the contribution correlation matrix (Hu, Fan and Mei
#' 2015)
#'
#' Hu, Fan and Mei (2015), *Physica A* 429:169-176, equations (1)-(4). With
#' `I_i = (1 / n) sum_{p != i} 1 / d_ip` the efficiency of node `i`, and
#' `Delta_j(i)` the efficiency node `j` loses when `i` is deleted,
#'
#' ```
#' delta_ij = Delta_j(i) / sum_{k : d_ik = d_ij} Delta_k(i)           (4)
#' H_j      = I_j sum_{i : 1 <= d_ij <= nl} delta_ij I_i / d_ij.      (3)
#' ```
#'
#' Each node hands its efficiency to its first layer, half of it to its
#' second, and so on to layer `nl`, split inside a layer in proportion to
#' the loss its deletion causes. The paper uses `nl = 2`.
#'
#' The efficiency after a deletion keeps the divisor `n`; the paper does not
#' say so, and its Tables 2 and 3 are reproduced only that way. Unreachable
#' pairs contribute `1 / Inf = 0`. The loss of a node that `i` can reach is
#' positive, because deleting `i` removes the term `1 / d_ij` and shortens
#' no other distance, so equation (4) never divides by zero.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param layers Number of layers `nl`: a whole number of at least one, or
#'   `Inf`.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_niccm <- function(b, layers = 2) {
  .cg_check_radius(layers, "niccm_layers")
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  efficiency_of <- function(hops) {
    inverse <- 1 / hops
    diag(inverse) <- 0
    rowSums(inverse) / n
  }
  hops <- .cg_hop_matrix(b > 0, n)
  efficiency <- efficiency_of(hops)
  # One column per contributor; `matrix()` keeps a one-node network a matrix.
  given <- matrix(vapply(seq_len(n), function(i) {
    without <- b
    without[i, ] <- 0
    without[, i] <- 0
    loss <- efficiency - efficiency_of(.cg_hop_matrix(without > 0, n))
    inside <- is.finite(hops[i, ]) & hops[i, ] >= 1 & hops[i, ] <= layers
    layer_loss <- tapply(loss[inside], hops[i, inside], sum)
    share <- numeric(n)
    share[inside] <- loss[inside] /
      layer_loss[as.character(hops[i, inside])] / hops[i, inside]
    share * efficiency[i]
  }, numeric(n)), n, n)
  efficiency * rowSums(given)
}

#' Correlation centrality (Fan, Liu and Hu 2013)
#'
#' Fan, Liu and Hu (2013), *Physica Scripta* 88(6):065201, equations (2)
#' and (4):
#'
#' ```
#' EC(j)  = (1 / n) sum_{k != j} 1 / d_jk
#' CoC(i) = (1 / n) sum_{j != i} EC(j) sigma_ij / d_ij^alpha
#' ```
#'
#' with `sigma_ij` the number of shortest paths between `i` and `j`. The
#' authors set `alpha = 3`. The printed sum runs over every node; the term
#' `j = i` divides by zero and is dropped, which reproduces the paper's
#' Table 1. An unreachable node contributes zero.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param alpha Exponent of the distance, a single finite nonnegative number.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_correlation_centrality <- function(b, alpha = 3) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha < 0) {
    .cg_stop_bad_parameter(
      "`coc_alpha` must be a single finite nonnegative number")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  hops <- .cg_hop_matrix(b > 0, n)
  reached <- is.finite(hops) & hops >= 1
  efficiency <- rowSums(ifelse(reached, 1 / hops, 0)) / n
  kernel <- ifelse(reached, .cg_geodesic_counts(b, hops) / hops^alpha, 0)
  as.numeric(kernel %*% efficiency) / n
}
