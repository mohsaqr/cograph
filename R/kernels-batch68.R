#' Check the step count of PhysarumSpreader
#' @keywords internal
#' @noRd
.cg_check_physarum_steps <- function(steps) {
  if (!is.null(steps) && (!is.numeric(steps) || length(steps) != 1L ||
                            !is.finite(steps) || steps < 1 ||
                            steps != trunc(steps))) {
    .cg_stop_bad_parameter(
      "`physarum_steps` must be NULL or a single whole number of at least one")
  }
  invisible(steps)
}

#' PhysarumSpreader (Wang, Zhang, Zhang, Mahadevan and Deng 2015)
#'
#' Wang, Zhang, Zhang, Mahadevan and Deng (2015), *PLoS ONE* 10(12):
#' e0145028, Section 2, Steps 1-6, equations (7)-(12). A ground node `g` is
#' linked both ways to every node. With `w` the arc weights,
#'
#' ```
#' w_ig = (sum of i's out-arc weights) / (number of i's out-arcs), 1 if none (7)
#' w_gi = 1                                                       (Fig. 3)
#' Q_ij(t) = w_ij D_ij(t) / sum_l w_il D_il(t),  D(0) = 1                (9)
#' D_ij(t + 1) = (Q_ij(t) + D_ij(t)) / 2   on the arcs between nodes    (10)
#' I_i(t + 1) = sum_j Q_ji(t) I_j(t),  I(0) = 1, I_g(0) = 0             (11)
#' S_i = I_i(t) + I_g(t) / N                                            (12)
#' ```
#'
#' The paper gives no stopping rule and the scores tend to 1 at every node,
#' so the number of updates `t` is an argument. Ground links keep `D = 1`
#' (eq. 10 adapts only the arcs between nodes). Loops are removed.
#'
#' @param w Nonnegative weight matrix, `w[i, j]` the weight of the arc
#'   `i -> j`.
#' @param steps Number of updates, a whole number of at least one.
#' @return Numeric vector of `S`.
#' @keywords internal
#' @noRd
.cg_physarum_spreader <- function(w, steps) {
  n <- nrow(w)
  if (is.null(n) || n == 0L) return(numeric(0))
  diag(w) <- 0
  out_arcs <- rowSums(w > 0)
  ground <- n + 1L
  weight <- matrix(0, ground, ground)
  weight[seq_len(n), seq_len(n)] <- w
  weight[seq_len(n), ground] <- ifelse(out_arcs > 0,
                                       rowSums(w) / pmax(out_arcs, 1), 1)
  weight[ground, seq_len(n)] <- 1
  adapts <- matrix(FALSE, ground, ground)
  adapts[seq_len(n), seq_len(n)] <- w > 0
  state <- list(conductivity = (weight > 0) * 1, resource = c(rep(1, n), 0))
  # Each update needs the conductivities and resources of the previous one,
  # so the updates run in sequence; `steps` bounds them.
  for (step in seq_len(steps)) {
    flux <- weight * state$conductivity
    flux <- flux / rowSums(flux)
    conductivity <- state$conductivity
    conductivity[adapts] <- ((flux + conductivity) / 2)[adapts]
    state <- list(conductivity = conductivity,
                  resource = as.numeric(crossprod(flux, state$resource)))
  }
  state$resource[seq_len(n)] + state$resource[ground] / n
}

#' Check the reference counts of ArticleRank
#' @param references `NULL` or one count per node.
#' @param labels Node labels, to match a named vector.
#' @return The counts in node order, or `NULL`.
#' @keywords internal
#' @noRd
.cg_check_ar_references <- function(references, labels) {
  if (is.null(references)) return(NULL)
  n <- length(labels)
  if (!is.numeric(references) || length(references) != n ||
        any(!is.finite(references)) || any(references < 0)) {
    .cg_stop_bad_parameter(
      "`ar_references` must hold one finite nonnegative number per node")
  }
  if (!is.null(names(references))) {
    if (anyDuplicated(names(references)) ||
          !setequal(names(references), labels)) {
      .cg_stop_bad_parameter(
        "the names of `ar_references` must match the node names exactly")
    }
    references <- references[match(labels, names(references))]
  }
  unname(as.numeric(references))
}

#' ArticleRank (Li and Willett 2009)
#'
#' Li and Willett (2009), *Aslib Proceedings* 61(6):605-618, equation (2).
#' With `P -> A` when `P` cites `A`, `NR(P)` the number of references of
#' `P` and `NRbar` their mean over the network,
#'
#' ```
#' AR(A) = 1 - d + d NRbar sum_{P -> A} AR(P) / (NRbar + NR(P))          (2)
#' ```
#'
#' The map is linear, `AR = (1 - d) 1 + M AR` with
#' `M = d NRbar t(B) diag(1 / (NRbar + NR))`. The paper iterates it to a
#' precision of 1e-10 from `AR = 1 - d`; the iteration converges exactly
#' when the spectral radius of `M` is below 1, and its limit is the
#' solution of `(I - M) AR = (1 - d) 1`, computed here directly. Citation
#' networks are acyclic and have radius 0; a cyclic network can have a
#' radius of 1 or more, and then the score has no value (status
#' `"diverges"`). Loops are removed.
#'
#' @param b Binary adjacency matrix, `b[P, A] = 1` when `P` cites `A`.
#' @param references `NR`, one number per node; `NULL` for the out-degree.
#' @param damping The damping factor `d`.
#' @return A list with `value`, `status` (`"ok"` or `"diverges"`) and
#'   `radius`, the spectral radius of `M`.
#' @keywords internal
#' @noRd
.cg_articlerank <- function(b, references = NULL, damping = 0.85) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) {
    return(list(value = numeric(0), status = "ok", radius = 0))
  }
  diag(b) <- 0
  b <- (b != 0) * 1
  references <- references %||% rowSums(b)
  mean_references <- mean(references)
  if (!any(b > 0)) {
    return(list(value = rep(1 - damping, n), status = "ok", radius = 0))
  }
  if (!(mean_references > 0)) {
    .cg_stop_bad_parameter(
      "`ar_references` are all zero on a network with arcs, so equation (2) ",
      "has 0 / 0 in every term")
  }
  share <- damping * mean_references / (mean_references + references)
  transfer <- t(b * share)
  radius <- max(Mod(eigen(transfer, only.values = TRUE)$values))
  if (radius >= 1 - sqrt(.Machine$double.eps)) {
    return(list(value = rep(NA_real_, n), status = "diverges",
                radius = radius))
  }
  # qr.default() by name: a package that registers qr() for arrays
  # (brainGraph) would otherwise take the matrix.
  value <- qr.coef(qr.default(diag(n) - transfer), rep(1 - damping, n))
  list(value = as.numeric(value), status = "ok", radius = radius)
}

#' Min-max scale a column, with a tolerance for rounding
#'
#' A column whose range is within `tolerance` of its largest absolute value
#' is constant (`NULL` is returned); scaled values within `tolerance` of 0
#' are set to 0, so that values equal in exact arithmetic but apart by
#' rounding count as the minimum.
#' @param x Numeric vector.
#' @param tolerance Relative tolerance.
#' @return Numeric vector in `[0, 1]`, or `NULL` for a constant column.
#' @keywords internal
#' @noRd
.cg_minmax_tolerant <- function(x, tolerance = 1e-9) {
  spread <- max(x) - min(x)
  if (spread <= tolerance * max(abs(x))) return(NULL)
  scaled <- (x - min(x)) / spread
  scaled[scaled <= tolerance] <- 0
  scaled
}

#' Entropy-based gravity model (Yan, Cui and Ni 2020)
#'
#' Yan, Cui and Ni (2020), *Chinese Physics B* 29(4):048902, Section 2,
#' with the attributes of Combination 6 (node H-index, closeness,
#' betweenness, PageRank). With `b_ij` the min-max scaled attribute `j` of
#' node `i` (eq. 2),
#'
#' ```
#' P_ij  = b_ij / sum_i b_ij;  (1 + b_ij) / sum_i (1 + b_ij) when P_ij = 1  (3)
#' S_j   = -(1 / ln N) sum_i P_ij ln P_ij                                  (3)
#' w_j   = (1 - S_j) / (m - sum_j S_j)                                     (4)
#' C_i   = sum_j w_j b_ij                                                  (5)
#' G(i)  = sum_{j : 1 <= d_ij <= r} C_i C_j / d_ij^2                       (6)
#' ```
#'
#' The paper prints eq. (3) with the shares taken over the attributes of a
#' node and eq. (5) with `S_j` in place of `w_j`; its prose (the entropy of
#' each attribute, combined "by weighted averaging" with the eq. 4 weights)
#' is followed (owner decision 2026-11-26). The guard of eq. (3) applies to
#' a column with a single nonzero `b`. Closeness is `1 / sum of distances`,
#' PageRank is cograph's with the given damping.
#'
#' @param b Binary symmetric adjacency matrix of a connected network with
#'   at least two nodes.
#' @param radius The radius `r`.
#' @param damping PageRank damping factor.
#' @return A list with `value` (`NA` when an attribute is constant) and
#'   `status` (`"ok"` or `"constant"`).
#' @keywords internal
#' @noRd
.cg_entropy_gravity <- function(b, radius = 3, damping = 0.85) {
  n <- nrow(b)
  hops <- .cg_hop_matrix(b > 0, n)
  # At damping 1 PageRank is the stationary distribution of the random walk,
  # the degree share on a connected undirected network; the power iteration
  # of `.cg_pagerank()` would not settle on a bipartite one.
  pagerank <- if (damping < 1) .cg_pagerank(b, n, damping) else rowSums(b) / sum(b)
  attributes <- list(.cg_lobby(b), 1 / rowSums(hops),
                     .cg_betweenness(b, n, FALSE), pagerank)
  scaled <- lapply(attributes, .cg_minmax_tolerant)
  if (any(vapply(scaled, is.null, logical(1)))) {
    return(list(value = rep(NA_real_, n), status = "constant"))
  }
  scaled <- do.call(cbind, scaled)
  single <- colSums(scaled > 0) == 1L
  shifted <- sweep(scaled, 2L, ifelse(single, 1, 0), "+")
  shares <- sweep(shifted, 2L, colSums(shifted), "/")
  entropy <- -colSums(ifelse(shares > 0, shares * log(shares), 0)) / log(n)
  weight <- (1 - entropy) / (ncol(scaled) - sum(entropy))
  mass <- as.numeric(scaled %*% weight)
  pull <- ifelse(hops >= 1 & hops <= radius, 1 / hops^2, 0)
  list(value = mass * as.numeric(pull %*% mass), status = "ok")
}

#' Global k-shell iteration number of influence capability
#'
#' Wang, Du, Fan and Xing (2017), Section 3.1: at level `k = 1, 2, ...`
#' every remaining node of current degree at most `k` is removed at once;
#' each removal is one iteration of a counter that runs across the shells,
#' from 1. A check that removes nothing raises `k` and does not count.
#' An isolate is removed in the first iteration, with the leaves.
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector of iteration numbers.
#' @keywords internal
#' @noRd
.cg_ic_iteration <- function(b) {
  n <- nrow(b)
  state <- list(alive = rep(TRUE, n), iteration = numeric(n), level = 1,
                counter = 0)
  # Each removal changes the degrees that decide the next one; at most 2n
  # checks are made, since each one removes a node or raises the level.
  while (any(state$alive)) {
    degree <- as.numeric(b %*% state$alive)
    hit <- state$alive & degree <= state$level
    if (any(hit)) {
      state$counter <- state$counter + 1
      state$iteration[hit] <- state$counter
      state$alive[hit] <- FALSE
    } else {
      state$level <- state$level + 1
    }
  }
  state$iteration
}

#' Attributes and entropy weights of influence capability
#'
#' Wang, Du, Fan and Xing (2017), *Neurocomputing* 260:466-477, equations
#' (1)-(3) and (5)-(7). With `Iter` the global iteration number, `Gamma(i)`
#' the neighbors removed in an earlier iteration than `i` and `d` the
#' degree,
#'
#' ```
#' D(j)   = (2 / pi) atan(Iter(j)^(1/3))                                 (1)
#' ICP(i) = (d_i - |Gamma(i)|) + sum_{j in Gamma(i)} D(j)                (2)
#' ICN(i) = sum_{j in N(i)} sum_{k in N(j)} d_k                          (3)
#' H_x    = -(1 / ln n) sum_i r_i ln r_i,  r = x / sum x                 (5)-(6)
#' w_x    = (1 - H_x) / (2 - H_ICP - H_ICN)                              (7)
#' ```
#'
#' Eq. (7) is printed with a leading minus sign, a typo: the printed
#' weights are positive and add to 1. When both entropies are 1 (both
#' attributes constant) eq. (7) is 0 / 0 and the weights are 1/2 each.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal and at
#'   least one edge.
#' @return A list with `icp`, `icn` and `weight` (of ICP and ICN).
#' @keywords internal
#' @noRd
.cg_ic_parts <- function(b) {
  n <- nrow(b)
  degree <- rowSums(b)
  iteration <- .cg_ic_iteration(b)
  earlier <- b * outer(iteration, iteration, ">")
  position <- 2 / pi * atan(iteration^(1 / 3))
  icp <- degree - rowSums(earlier) + as.numeric(earlier %*% position)
  icn <- as.numeric(b %*% (b %*% degree))
  entropy <- vapply(list(icp, icn), \(x) {
    r <- x / sum(x)
    -sum(ifelse(r > 0, r * log(r), 0)) / log(n)
  }, numeric(1))
  denominator <- 2 - sum(entropy)
  weight <- if (denominator > sqrt(.Machine$double.eps)) {
    (1 - entropy) / denominator
  } else {
    c(0.5, 0.5)
  }
  list(icp = icp, icn = icn, weight = weight)
}

#' Influence capability (Wang, Du, Fan and Xing 2017)
#'
#' Equation (4): `IC(i) = w_ICP ICP(i) / sum ICP + w_ICN ICN(i) / sum ICN`.
#' "Normalized" in eq. (4) is division by the total, the `r` of eq. (5)
#' (owner decision 2026-11-26).
#' @param b Binary symmetric adjacency matrix with a zero diagonal and at
#'   least one edge.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_influence_capability <- function(b) {
  parts <- .cg_ic_parts(b)
  parts$weight[1L] * parts$icp / sum(parts$icp) +
    parts$weight[2L] * parts$icn / sum(parts$icn)
}

#' Check the parameters of the extended H-index centrality
#' @keywords internal
#' @noRd
.cg_check_ehc_args <- function(p, r) {
  if (!is.numeric(p) || length(p) != 1L || !is.finite(p) || p <= 0 || p > 1) {
    .cg_stop_bad_parameter("`ehc_p` must be a single number above 0 and at most 1")
  }
  if (!is.numeric(r) || length(r) != 1L || !is.finite(r) || r <= 0) {
    .cg_stop_bad_parameter("`ehc_r` must be a single finite number above 0")
  }
  invisible(NULL)
}

#' Cumulative-function sums of the extended H-index centrality
#'
#' Zareie and Sheikhahmadi (2019), *Physica A* 514:141-155, equations (19)
#' and (21)-(23): `c_k(i)` counts the neighbors of `i` of degree at least
#' `k`, for `k = 1, ..., h` with `h` the largest degree of the network, and
#'
#' ```
#' CMC(i) = sum_{k = 1}^{h} p^(1 + k p / r) c_k(i)                       (23)
#' ```
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param p,r The parameters of eq. (23).
#' @return Numeric vector of `CMC`.
#' @keywords internal
#' @noRd
.cg_ehc_cmc <- function(b, p = 0.8, r = 100) {
  degree <- rowSums(b)
  h <- max(c(degree, 0))
  if (h == 0) return(numeric(nrow(b)))
  at_least <- outer(degree, seq_len(h), ">=") * 1
  as.numeric((b %*% at_least) %*% p^(1 + seq_len(h) * p / r))
}

#' Extended H-index centrality (Zareie and Sheikhahmadi 2019)
#'
#' `EHC(i) = sum_{j in N(i)} CMC(j)`, equation (24); `p = 0.8` and
#' `r = 100` are the authors' values (Section 4.3).
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param p,r The parameters of eq. (23).
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_ehc <- function(b, p = 0.8, r = 100) {
  .cg_check_ehc_args(p, r)
  as.numeric(b %*% .cg_ehc_cmc(b, p, r))
}

#' Entropy-based ranking measure (Zareie, Sheikhahmadi and Fatemi 2017)
#'
#' Zareie, Sheikhahmadi and Fatemi (2017), *Chaos, Solitons and Fractals*
#' 104:485-494, equations (2)-(7). With `d` the degree,
#' `d1_i = sum_{j in N(i)} d_j` and `d2_i = sum_{j in N(i)} d1_j`,
#'
#' ```
#' E1(i)    = - sum_{j in N(i)} (d_j / d1_i) log(d_j / d1_i)             (2)
#' E2(i)    = - sum_{j in N(i)} (d1_j / d2_i) log(d1_j / d2_i)           (3)
#' lambda_i = d2_i / max_k d2_k                                          (5)
#' EC(i)    = E1(i) + lambda_i E2(i)                                     (4)
#' ERM(i)   = sum_{j in N(i)} sum_{k in N(j)} EC(k)                    (6)-(7)
#' ```
#'
#' Eq. (4) is printed with `v_j` on the right, a misprint for `v_i` (the
#' same authors restate it so in 2019). The paper gives no logarithm base;
#' the natural logarithm is used (owner decision 2026-11-26), the base only
#' rescales. An isolated node has empty sums and scores 0.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_erm <- function(b) {
  degree <- rowSums(b)
  first <- as.numeric(b %*% degree)
  second <- as.numeric(b %*% first)
  neighbor_entropy <- \(value, total) {
    shares <- b * outer(ifelse(total > 0, 1 / total, 0), value)
    .cg_row_entropy(shares)
  }
  lambda <- if (max(c(second, 0)) > 0) second / max(second) else second
  ec <- neighbor_entropy(degree, first) + lambda * neighbor_entropy(first, second)
  as.numeric(b %*% (b %*% ec))
}

#' Shell vectors of the shell clustering coefficient
#'
#' Zareie, Sheikhahmadi, Jalili and Fasaei (2020), *Knowledge-Based
#' Systems* 194:105580, equation (2). The hierarchy `IT` is the round in
#' which a node leaves when every round removes all remaining nodes of the
#' lowest degree (the text of Section 3; the peel of `.cg_iksd()`). Row `i`
#' counts the neighbors of `i` in each hierarchy `1, ..., f`.
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric `n x f` matrix.
#' @keywords internal
#' @noRd
.cg_shell_vectors <- function(b) {
  hierarchy <- .cg_iksd(b)
  b %*% outer(hierarchy, seq_len(max(hierarchy)), "==")
}

#' Pearson correlations of the shell vectors, equation (3)
#'
#' `C_ij` is 0 / 0 when either shell vector is constant; it is taken as 0
#' (owner decision 2026-11-26). The shell vectors hold whole numbers, so
#' the test for a constant vector is exact.
#' @param shell Matrix of shell vectors, one row per node.
#' @return Numeric `n x n` matrix.
#' @keywords internal
#' @noRd
.cg_shell_correlation <- function(shell) {
  constant <- rowSums(shell != shell[, 1L]) == 0
  centred <- shell - rowMeans(shell)
  norms <- ifelse(constant, 1, sqrt(rowSums(centred^2)))
  correlation <- tcrossprod(centred / norms)
  correlation[constant, ] <- 0
  correlation[, constant] <- 0
  correlation
}

#' Shell clustering coefficient (Zareie, Sheikhahmadi, Jalili and Fasaei 2020)
#'
#' Equation (4) and Algorithm 1 line 12 (the two brackets are added):
#'
#' ```
#' SCC_i = sum_{j in N(i)} [(2 - C_ij) + (2 d_j / max(d) + 1)]            (4)
#' ```
#'
#' An isolated node, and every node of a network without edges, scores 0
#' (empty sums).
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_shell_clustering <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  if (max(degree) == 0) return(numeric(n))
  correlation <- .cg_shell_correlation(.cg_shell_vectors(b))
  rowSums(b * (2 - correlation)) +
    as.numeric(b %*% (2 * degree / max(degree) + 1))
}

#' Extended cluster coefficient ranking measure (Zareie et al. 2020)
#'
#' Equations (5)-(6) of the same paper: `CRM_i = sum_{j in N(i)} SCC_j` and
#' `ECRM_i = sum_{j in N(i)} CRM_j`.
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_ecrm <- function(b) {
  as.numeric(b %*% (b %*% .cg_shell_clustering(b)))
}

#' Check the parameters of weight neighborhood centrality
#' @keywords internal
#' @noRd
.cg_check_wnc_args <- function(benchmark, alpha) {
  if (!is.character(benchmark) || length(benchmark) != 1L ||
        !benchmark %in% c("degree", "coreness")) {
    .cg_stop_bad_parameter(
      "`wnc_benchmark` must be \"degree\" or \"coreness\"")
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha)) {
    .cg_stop_bad_parameter("`wnc_alpha` must be a single finite number")
  }
  invisible(NULL)
}

#' Weight neighborhood centrality (Wang, Hou, Li and Ding 2017)
#'
#' Wang, Hou, Li and Ding (2017), *Physica A* 475:88-105, equations (9)-(10).
#' With `k` the degree and `phi` the benchmark centrality (degree or
#' coreness),
#'
#' ```
#' w_ij    = (k_i k_j)^alpha                                             (9)
#' <w>     = mean of w_ij over the edges
#' C_i     = phi_i + sum_{j in N(i)} (w_ij / <w>) phi_j                  (10)
#' ```
#'
#' `alpha = 1` is the authors' value. `<w>` is the mean over the edges, the
#' reading the paper's Table 1 fixes. On a network without edges every
#' neighbor sum is empty and the score is the benchmark (0).
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param benchmark `"degree"` or `"coreness"`.
#' @param alpha Exponent of eq. (9).
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_weight_neighborhood <- function(b, benchmark = "degree", alpha = 1) {
  .cg_check_wnc_args(benchmark, alpha)
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  phi <- if (identical(benchmark, "degree")) degree else .cg_coreness(b, n)
  if (!any(b > 0)) return(phi)
  # Both ends of an edge have degree at least one, so any alpha is finite.
  importance <- ifelse(b > 0, outer(pmax(degree, 1), pmax(degree, 1))^alpha, 0)
  mean_importance <- sum(importance) / sum(b)
  phi + as.numeric((importance / mean_importance) %*% phi)
}
