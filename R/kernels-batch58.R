#' Node importance evaluation matrix method (Zhou et al. 2012)
#'
#' Zhou, Zhang, Li, Hui and Wu (2012), *Acta Physica Sinica* 61(5):050201,
#' equations (4)-(6). The efficiency of a node is
#' `I_k = (1/n) sum_{i != k} 1/d_ki`, with `1/d = 0` for a pair without a
#' path (Definition 2). The importance evaluation matrix has `I_i` on its
#' diagonal and `delta_ij D_j I_j / <k>^2` elsewhere, `D` the degree and
#' `<k>` the mean degree, and the importance of a node is
#'
#' ```
#' C_i = I_i * sum_{j != i} delta_ij D_j I_j / <k>^2.                  (6)
#' ```
#'
#' The divisors `n` and `<k>^2` are the printed ones and the only ones that
#' reproduce the paper's Tables 1 and 2. The Centrality Zoo's entry cites a
#' different paper for this measure and prints `<d>` unsquared.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; all zero on a network without edges.
#' @keywords internal
#' @noRd
.cg_niem <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  if (!any(degree > 0)) return(numeric(n))
  hops <- .cg_hop_matrix(b > 0, n)
  efficiency <- rowSums(ifelse(is.finite(hops) & hops > 0, 1 / hops, 0)) / n
  efficiency * as.numeric(b %*% (degree * efficiency)) / mean(degree)^2
}

#' Improved k-shell decomposition (Liu, Jiang, Wang and Yu 2015)
#'
#' Liu, Jiang, Wang and Yu (2015), *Knowledge-Based Systems* 84:56-66,
#' section 2.2.5, page 57: "first find the nodes whose degrees are minimal
#' in G_0 and define their IKs value as Ks^1 = 1, then get a new induced
#' sub-network G_1 by removing those nodes in G_0. Similarly, find the
#' nodes whose degrees are minimal in G_1 and define their IKs value as
#' Ks^2 = Ks^1 + 1, repeat sequentially and recursively until all the nodes
#' have been removed. If the degree of a node becomes 0 in the k th
#' removal, then define its IKs value as (k + 1)."
#'
#' Each iteration removes exactly the nodes at the current minimum degree.
#' Nodes that the removal brings down to or below that degree wait for the
#' next iteration, which sets the method apart from the onion
#' decomposition. A node left without ties has the minimum degree in the
#' next iteration, which is the paper's `k + 1` rule, and a node isolated
#' from the start is removed in the first.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector of iteration numbers, from one.
#' @keywords internal
#' @noRd
.cg_iksd <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  label <- numeric(n)
  active <- rep(TRUE, n)
  iteration <- 0
  # Each removal changes the degrees that decide the next one.
  while (any(active)) {
    iteration <- iteration + 1
    degree <- as.numeric(b %*% active)
    lowest <- active & degree == min(degree[active])
    label[lowest] <- iteration
    active[lowest] <- FALSE
  }
  label
}

#' Density centrality (Ibnoulouafi and El Haziti 2018)
#'
#' Ibnoulouafi and El Haziti (2018), *Chaos, Solitons and Fractals*
#' 114:69-80. The original was not read; the formula is equation (4) of
#' Ullah et al. (2021), *Scientific Reports* 11:6173, which the Centrality
#' Zoo's entry 2.71 matches:
#'
#' ```
#' DNC(i) = sum_{j : 1 <= d_ij <= r} degree_i / (pi d_ij^2).
#' ```
#'
#' The numerator is the degree of the focal node; the ranking printed by
#' Ullah et al. for the dolphin network is reproduced by that reading and
#' by neither the neighbor's degree nor the product of the two.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param radius Largest distance summed over: a single whole number of at
#'   least one, or `Inf`.
#' @return Numeric vector; isolates score zero.
#' @keywords internal
#' @noRd
.cg_density_centrality <- function(b, radius = 3) {
  if (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
        radius < 1 || (is.finite(radius) && radius != trunc(radius))) {
    .cg_stop_bad_parameter(
      "`density_radius` must be a single whole number of at least one, or Inf")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  hops <- .cg_hop_matrix(b > 0, n)
  inside <- is.finite(hops) & hops >= 1 & hops <= radius
  rowSums(b) / pi * rowSums(ifelse(inside, 1 / hops^2, 0))
}

#' Weighted k-shell degree neighborhood (Maji 2020)
#'
#' Maji (2020), *Journal of Computational Science* 39:101055. The original
#' was not read; the equations are those of a later paper that reproduces
#' them (Zhao et al. 2026, *Entropy* 28(8):935, "Measure 5") and of the
#' Centrality Zoo's entry 2.399:
#'
#' ```
#' w_ij     = (ks_i + ks_j) + lambda (k_i + k_j),   lambda = sum(ks) / sum(k)
#' WKSDN(i) = sum_{j in N(i)} w_ij
#' ```
#'
#' with `ks` the k-shell index and `k` the degree.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @return Numeric vector; all zero on a network without edges.
#' @keywords internal
#' @noRd
.cg_wksdn <- function(b) {
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  if (!any(degree > 0)) return(numeric(n))
  shell <- .cg_mdd(b, lambda = 0)
  ratio <- sum(shell) / sum(degree)
  degree * shell + as.numeric(b %*% shell) +
    ratio * (degree^2 + as.numeric(b %*% degree))
}

#' mu-power community index (Papadimitriou, Katsaros and Manolopoulos 2010)
#'
#' Papadimitriou, Katsaros and Manolopoulos (2010), "Social network
#' analysis and its applications in wireless sensor and vehicular
#' networks", page 6: the index of a node is the largest `k` such that at
#' least `mu k` nodes within `mu` hops of it have a degree of at least
#' `k`. The node itself is not counted, as the paper's `PCI(6) = 3` on its
#' Fig. 4 requires. At `mu = 1` this is the h-index of the neighbors'
#' degrees.
#'
#' @param b Binary symmetric adjacency matrix with a zero diagonal.
#' @param mu Whole number of at least one.
#' @return Numeric vector of whole numbers.
#' @keywords internal
#' @noRd
.cg_mu_pci <- function(b, mu = 1) {
  if (!is.numeric(mu) || length(mu) != 1L || !is.finite(mu) || mu < 1 ||
        mu != trunc(mu)) {
    .cg_stop_bad_parameter(
      "`pci_mu` must be a single whole number of at least one")
  }
  n <- nrow(b)
  if (is.null(n) || n == 0L) return(numeric(0))
  degree <- rowSums(b)
  hops <- .cg_hop_matrix(b > 0, n)
  vapply(seq_len(n), \(i) {
    around <- sort(degree[hops[i, ] >= 1 & hops[i, ] <= mu],
                   decreasing = TRUE)
    steps <- seq_len(length(around) %/% mu)
    sum(around[steps * mu] >= steps)
  }, numeric(1))
}
