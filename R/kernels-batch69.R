#' Check the parameter of RCFB against equation (14)
#' @param epsilon The conductance of the links to the virtual target.
#' @param w Symmetric nonnegative conductance matrix.
#' @return `epsilon`, invisibly.
#' @keywords internal
#' @noRd
.cg_check_rcfb_epsilon <- function(epsilon, w) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L || !is.finite(epsilon) ||
        epsilon < 0) {
    .cg_stop_bad_parameter(
      "`rcfb_epsilon` must be a single finite number of at least 0")
  }
  edges <- w[upper.tri(w)]
  bound <- 1 / sum(1 / edges[edges > 0])
  if (epsilon > 0 && epsilon >= bound) {
    .cg_stop_bad_parameter(sprintf(paste0(
      "`rcfb_epsilon` must be below 1 / (sum of the edge resistances) = %.6g ",
      "on this network (equation 14 of the source), or 0 for the limit"),
      bound))
  }
  invisible(epsilon)
}

#' RCFB, resized current-flow betweenness (Agryzkov, Tortosa and Vicent 2019)
#'
#' Agryzkov, Tortosa and Vicent (2019), *Applied Mathematics and
#' Computation* 347:600-615, equations (14)-(20) and Algorithm 2. Every node
#' is joined to a virtual target by a link of conductance `epsilon`; a unit
#' of current enters at each source `s` in turn. With `M = (epsilon I + L)^-1`
#' and `q` the conductances, the current on the edge `ij` is
#' `q_ij (M_is - M_js)`, credited to its endpoint of higher potential, and
#'
#' ```
#' b_i = (1 / n) sum_s sum_{edges at i, i higher} q_ij |M_is - M_js|     (18)-(20)
#' ```
#'
#' The loop of Algorithm 2 has no `i != s` guard, so the source is credited
#' with the current it sends out (the second option of eq. 19). At
#' `epsilon = 0` the limit is taken: within a component the term
#' `1 / (epsilon n_c)` of `M` cancels in every difference, so `M` is
#' replaced by `(L + J)^-1`, `J` the block matrix with `1 / n_c` inside each
#' component, whose differences are those of the Laplacian pseudo-inverse.
#'
#' @param w Symmetric nonnegative conductance matrix with a zero diagonal.
#' @param epsilon `0` for the limit, or the conductance `epsilon`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_rcfb <- function(w, epsilon = 0) {
  n <- nrow(w)
  if (is.null(n) || n == 0L) return(numeric(0))
  laplacian <- diag(rowSums(w), n) - w
  shift <- if (epsilon > 0) {
    diag(epsilon, n)
  } else {
    reach <- is.finite(.cg_hop_matrix(w > 0, n))
    reach / rowSums(reach)
  }
  potential <- chol2inv(chol(laplacian + shift))
  edges <- which(upper.tri(w) & w > 0, arr.ind = TRUE)
  if (!nrow(edges)) return(numeric(n))
  # Row e, column s: the current on edge e when the source is s.
  current <- w[edges] * (potential[edges[, 1L], , drop = FALSE] -
                           potential[edges[, 2L], , drop = FALSE])
  credit <- rowsum(c(rowSums(pmax(current, 0)), rowSums(pmax(-current, 0))),
                   c(edges[, 1L], edges[, 2L]))
  value <- numeric(n)
  value[as.integer(rownames(credit))] <- credit[, 1L]
  value / n
}

#' Coreness and degree-variation terms of M-centrality
#'
#' Ibnoulouafi, El Haziti and Cherifi (2018), *J. Stat. Mech.* 073407,
#' equations (6)-(11). With `k` the degree and `Ks` the coreness,
#'
#' ```
#' DeltaD_i = sum_{j in N(i)} |N_i| |(k_j - k_i) / sum_{j in N(i)} k_j|    (7)-(8)
#' r_1j = Ks_j / sum Ks,  r_2j = DeltaD_j / sum DeltaD                     (9)
#' E_x  = -(1 / ln n) sum_j r_xj ln r_xj                                  (10)
#' mu   = (1 - E_1) / (2 - E_1 - E_2)                                     (11)
#' ```
#'
#' An isolated node has an empty sum, `DeltaD = 0`. The weights are 1/2
#' each where eq. (11) has no value: a term that is 0 at every node (as
#' `DeltaD` on a regular network), both entropies 1, or one node (owner
#' decision 2026-11-26).
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return A list with `coreness`, `variation` and `mu`.
#' @keywords internal
#' @noRd
.cg_m_centrality_parts <- function(b) {
  n <- nrow(b)
  degree <- rowSums(b)
  coreness <- .cg_coreness(b, n)
  neighbor_total <- as.numeric(b %*% degree)
  spread <- rowSums(b * abs(outer(degree, degree, \(i, j) j - i)))
  variation <- degree * spread / ifelse(neighbor_total > 0, neighbor_total, 1)
  entropy <- \(x) {
    r <- x / sum(x)
    -sum(ifelse(r > 0, r * log(r), 0)) / log(n)
  }
  defined <- n > 1L && sum(coreness) > 0 && sum(variation) > 0
  mu <- 0.5
  if (defined) {
    e <- c(entropy(coreness), entropy(variation))
    if (2 - sum(e) > sqrt(.Machine$double.eps)) mu <- (1 - e[1L]) / (2 - sum(e))
  }
  list(coreness = coreness, variation = variation, mu = mu)
}

#' M-centrality: `M_i = mu Ks_i + (1 - mu) DeltaD_i`, equation (6)
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_m_centrality <- function(b) {
  if (is.null(nrow(b)) || nrow(b) == 0L) return(numeric(0))
  parts <- .cg_m_centrality_parts(b)
  parts$mu * parts$coreness + (1 - parts$mu) * parts$variation
}

#' Check the two weights of Wksd
#' @keywords internal
#' @noRd
.cg_check_wksd_args <- function(alpha, mu) {
  if (!is.null(alpha)) .cg_check_unit(alpha, "wksd_alpha")
  if (!is.null(mu)) .cg_check_unit(mu, "wksd_mu")
  invisible(NULL)
}

#' Weighted k-shell degree neighborhood (Namtirtha, Dutta and Dutta)
#'
#' Namtirtha, Dutta and Dutta (2018, 2020), as restated by Shetty and
#' Bhattacharjee (2025) eqs. (3)-(4) and Lu and Dong (2020) eqs.
#' (2.5)-(2.6). With `k` the degree and `ks` the k-shell index,
#'
#' ```
#' w_ij    = (alpha k_i + mu ks_i)(alpha k_j + mu ks_j)
#' Wksd(i) = sum_{j in N(i)} w_ij
#' ```
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param alpha,mu The two weights, each in `[0, 1]`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_wksd <- function(b, alpha, mu) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  mass <- alpha * rowSums(b) + mu * .cg_coreness(b, n)
  mass * as.numeric(b %*% mass)
}

#' Mapping entropy betweenness (Gialampoukidis et al. 2016)
#'
#' Gialampoukidis, Kalpakis, Tsikrika, Vrochidis and Kompatsiaris (2016),
#' EISIC 2016, pp. 112-115, with `BC` the betweenness normalised by the
#' number of pairs of other nodes, `(N - 1)(N - 2) / 2`:
#'
#' ```
#' MEB_i = - BC_i sum_{j in N(i), BC_j > 0} log BC_j
#' ```
#'
#' A neighbor of zero betweenness has `log 0`; it is skipped (owner
#' decision 2026-11-26). Natural logarithm, as `mapping_entropy`. Below
#' three nodes every betweenness is 0 and so is every score.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector, never negative.
#' @keywords internal
#' @noRd
.cg_mapping_entropy_betweenness <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  if (n < 3L) return(numeric(n))
  share <- .cg_betweenness(b, n, FALSE) / ((n - 1) * (n - 2) / 2)
  log_share <- log(ifelse(share > 0, share, 1))
  -share * as.numeric(b %*% log_share)
}
