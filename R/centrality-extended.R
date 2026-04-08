# =============================================================================
# Extended Centrality Measures — Native Implementations
# =============================================================================
#
# All measures implemented from mathematical definitions.
# Equivalence validated against centiserve, sna, tidygraph, brainGraph,
# influenceR, and NetworkX.


# =============================================================================
# Distance-based closeness variants
# =============================================================================

#' Stress centrality (sna-compatible)
#'
#' Number of shortest paths passing through each node as intermediate.
#' Uses sna convention with C-style accumulation.
#' @keywords internal
#' @noRd
calculate_stress <- function(g, weights = NULL, directed = TRUE) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(0, n))

  mode <- if (directed && igraph::is_directed(g)) "out" else "all"
  stress <- numeric(n)

  # For each source, find ALL shortest paths and count intermediate nodes
  for (s in seq_len(n)) {
    asp <- igraph::get.all.shortest.paths(g, from = s, to = igraph::V(g),
                                          mode = mode, weights = NA)
    for (path in asp$res) {
      path_v <- as.integer(path)
      if (length(path_v) > 2) {
        # Count intermediate nodes (exclude source and target)
        intermediates <- path_v[2:(length(path_v) - 1)]
        stress[intermediates] <- stress[intermediates] + 1
      }
    }
  }

  # For undirected, each s-t pair counted from both ends; divide by 2
  if (!igraph::is_directed(g)) {
    stress <- stress / 2
  }

  stress
}


#' Flow betweenness (sna-compatible)
#'
#' Max-flow based betweenness using igraph::max_flow.
#' @keywords internal
#' @noRd
calculate_flow_betweenness <- function(g, weights = NULL, directed = TRUE) {
  n <- igraph::vcount(g)
  if (n <= 2) return(rep(0, n))

  is_dir <- directed && igraph::is_directed(g)
  flow_bet <- numeric(n)
  el <- igraph::as_edgelist(g, names = FALSE)

  for (s in seq_len(n)) {
    targets <- if (is_dir) setdiff(seq_len(n), s) else {
      if (s < n) seq(s + 1L, n) else integer(0)
    }
    for (t_node in targets) {
      mf <- igraph::max_flow(g, source = s, target = t_node,
                             capacity = if (is.null(weights)) NULL else weights)
      if (mf$value == 0) next

      # Compute net inflow at each node from edge flows
      # For each edge, positive flow = src→dst, negative = dst→src
      inflow <- numeric(n)
      for (e_idx in seq_len(nrow(el))) {
        f <- mf$flow[e_idx]
        u <- el[e_idx, 1]; w <- el[e_idx, 2]
        if (f > 1e-12) {
          inflow[w] <- inflow[w] + f   # u→w: w receives
        } else if (f < -1e-12) {
          inflow[u] <- inflow[u] - f   # w→u: u receives
        }
      }
      # Zero out source/target
      inflow[s] <- 0; inflow[t_node] <- 0
      flow_bet <- flow_bet + inflow
    }
  }

  flow_bet
}


#' Lobby index / h-index (centiserve-compatible)
#'
#' Largest k such that node has at least k nodes in its CLOSED neighborhood
#' with degree >= k. Uses closed neighborhood (includes node itself).
#' @keywords internal
#' @noRd
calculate_lobby <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(integer(0))

  deg <- igraph::degree(g, mode = mode)

  vapply(seq_len(n), function(i) {
    # Closed neighborhood: node + its neighbors
    nbs <- c(i, as.integer(igraph::neighbors(g, i, mode = mode)))
    nb_degs <- sort(deg[nbs], decreasing = TRUE)
    h <- 0L
    for (k in seq_along(nb_degs)) {
      if (nb_degs[k] >= k) h <- as.integer(k) else break
    }
    h
  }, integer(1))
}


#' Radiality centrality (centiserve-compatible)
#'
#' sum(diam + 1 - d(v,w)) for ALL w (including self, where d=0),
#' divided by (n - 1).
#' @keywords internal
#' @noRd
calculate_radiality <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(NA_real_, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)
  diam <- igraph::diameter(g, directed = igraph::is_directed(g),
                           weights = if (is.null(weights)) NA else NULL)

  vapply(seq_len(n), function(i) {
    dists <- sp[i, ]
    # Include self (d=0) as in centiserve
    sum(diam + 1 - dists[is.finite(dists)]) / (n - 1)
  }, numeric(1))
}


#' Lin centrality (centiserve-compatible)
#' @keywords internal
#' @noRd
calculate_lin <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(NA_real_, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)

  vapply(seq_len(n), function(i) {
    dists <- sp[i, -i]
    reachable <- dists[is.finite(dists) & dists > 0]
    nr <- length(reachable)
    if (nr == 0) return(0)
    nr^2 / sum(reachable)
  }, numeric(1))
}


#' Decay centrality (centiserve-compatible)
#'
#' rowSums(delta^sp) — INCLUDES self (delta^0 = 1).
#' @keywords internal
#' @noRd
calculate_decay <- function(g, mode = "all", weights = NULL,
                            decay_parameter = 0.5) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(1, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)
  # Include self (diagonal = 0, so delta^0 = 1)
  rowSums(decay_parameter ^ sp)
}


#' Residual closeness (centiserve-compatible)
#'
#' sum(1/2^d) including self = sum(2^(-d)). Self contributes 1.
#' @keywords internal
#' @noRd
calculate_residual_closeness <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(1, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)
  # 1/2^sp including self; Inf distances contribute 0
  sp[!is.finite(sp)] <- Inf
  rowSums(1 / (2^sp))
}


#' Dangalchev closeness (same as residual closeness)
#' @keywords internal
#' @noRd
calculate_dangalchev <- function(g, mode = "all", weights = NULL) {
  calculate_residual_closeness(g, mode = mode, weights = weights)
}


#' Generalized closeness (tidygraph-compatible)
#'
#' sum(alpha^d) including self.
#' @keywords internal
#' @noRd
calculate_generalized_closeness <- function(g, mode = "all", weights = NULL,
                                            alpha = 0.5) {
  calculate_decay(g, mode = mode, weights = weights, decay_parameter = alpha)
}


#' Harary centrality
#'
#' sum(1/d(i,j)^2) over all j != i.
#' @keywords internal
#' @noRd
calculate_harary <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(0, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)
  diag(sp) <- NA

  vapply(seq_len(n), function(i) {
    dists <- sp[i, ]
    valid <- is.finite(dists) & !is.na(dists) & dists > 0
    sum(1 / dists[valid]^2)
  }, numeric(1))
}


#' Average distance centrality (centiserve-compatible)
#'
#' sum(d(v,w)) / (n + 1). Note: centiserve divides by vcount+1.
#' @keywords internal
#' @noRd
calculate_average_distance <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(NA_real_, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)

  # centiserve divides by n+1 (including self which has dist 0)
  rowSums(sp) / (n + 1)
}


#' Barycenter centrality (centiserve-compatible)
#'
#' 1 / sum(distances) for reachable nodes.
#' @keywords internal
#' @noRd
calculate_barycenter <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(NA_real_, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)
  diag(sp) <- NA

  vapply(seq_len(n), function(i) {
    dists <- sp[i, ]
    valid <- is.finite(dists) & !is.na(dists)
    total <- sum(dists[valid])
    if (total == 0) return(0)
    1 / total
  }, numeric(1))
}


#' Closeness vitality (centiserve-compatible)
#'
#' Wiener_full - Wiener_reduced. Wiener = sum of ALL sp values (not /2).
#' @keywords internal
#' @noRd
calculate_closeness_vitality <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(NA_real_, n))

  use_weights <- if (is.null(weights)) NA else NULL

  sp_full <- igraph::distances(g, mode = mode, weights = use_weights)
  sp_full[!is.finite(sp_full)] <- 0
  wiener_full <- sum(sp_full)  # full sum, NOT /2

  vapply(seq_len(n), function(i) {
    g_red <- igraph::delete_vertices(g, i)
    sp_red <- igraph::distances(g_red, mode = mode, weights = use_weights)
    sp_red[!is.finite(sp_red)] <- 0
    wiener_full - sum(sp_red)
  }, numeric(1))
}


#' Wiener index centrality
#'
#' Sum of all shortest path distances from node i.
#' @keywords internal
#' @noRd
calculate_wiener <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(0, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)
  diag(sp) <- 0
  sp[!is.finite(sp)] <- 0
  rowSums(sp)
}


# =============================================================================
# Spectral / walk-based measures
# =============================================================================

#' Communicability centrality (tidygraph-compatible)
#'
#' Row sums of the matrix exponential expm(A). This is the total
#' communicability of each node (not the subgraph centrality which is
#' the diagonal — already available as "subgraph" measure).
#' @keywords internal
#' @noRd
calculate_communicability <- function(g) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (n == 1) return(1)

  A <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  eig <- eigen(A, symmetric = isSymmetric(unname(A)))
  vals <- Re(eig$values)
  vecs <- Re(eig$vectors)
  exp_vals <- exp(vals)

  # expm(A) = V diag(exp(lambda)) V^-1
  # For symmetric: V^-1 = t(V), so expm = V %*% diag(exp_vals) %*% t(V)
  expm_A <- vecs %*% diag(exp_vals, nrow = length(exp_vals)) %*% t(vecs)
  rowSums(expm_A)
}


#' Communicability betweenness (tidygraph-compatible)
#'
#' Based on the ratio of communicability through node r to total.
#' @keywords internal
#' @noRd
calculate_communicability_betweenness <- function(g) {
  n <- igraph::vcount(g)
  if (n <= 2) return(rep(0, n))

  A <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  is_sym <- isSymmetric(unname(A))

  .expm_from_adj <- function(M) {
    eig <- eigen(M, symmetric = isSymmetric(unname(M)))
    v <- Re(eig$vectors)
    v %*% diag(exp(Re(eig$values)), nrow = nrow(M)) %*% t(v)
  }

  G <- .expm_from_adj(A)
  cb <- numeric(n)

  for (r in seq_len(n)) {
    A_red <- A; A_red[r, ] <- 0; A_red[, r] <- 0
    G_red <- .expm_from_adj(A_red)

    total <- 0
    for (s in seq_len(n)) {
      if (s == r) next
      for (t_node in seq_len(n)) {
        if (t_node == r || t_node == s) next
        if (G[s, t_node] > 1e-15) {
          total <- total + (G[s, t_node] - G_red[s, t_node]) / G[s, t_node]
        }
      }
    }
    cb[r] <- total
  }

  denom <- (n - 1) * (n - 2)
  if (denom > 0) cb <- cb / denom
  cb
}


#' Random walk centrality (tidygraph-compatible)
#'
#' Based on random walk distance: d_rw(i,j) = mean first passage times.
#' Returns 1/sum(d_rw) per node (inverse sum aggregation).
#' @keywords internal
#' @noRd
calculate_random_walk <- function(g) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(NA_real_, n))

  if (!igraph::is_connected(g, mode = "weak")) {
    warning("Random walk centrality undefined for disconnected graphs",
            call. = FALSE)
    return(rep(NA_real_, n))
  }

  # Transition matrix
  A <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  deg <- rowSums(A)
  deg[deg == 0] <- 1
  P <- A / deg

  # Stationary distribution
  if (!igraph::is_directed(g)) {
    pi_stat <- deg / sum(deg)
  } else {
    eig <- eigen(t(P))
    idx <- which.min(abs(Re(eig$values) - 1))
    pi_stat <- abs(Re(eig$vectors[, idx]))
    pi_stat <- pi_stat / sum(pi_stat)
  }

  # Fundamental matrix: Z = (I - P + W)^-1
  W <- matrix(pi_stat, n, n, byrow = TRUE)
  Z <- tryCatch(solve(diag(n) - P + W), error = function(e) NULL)
  if (is.null(Z)) return(rep(NA_real_, n))

  # Mean first passage time: m_ij = (Z_jj - Z_ij) / pi_j
  mfpt <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j && pi_stat[j] > 1e-15) {
        mfpt[i, j] <- (Z[j, j] - Z[i, j]) / pi_stat[j]
      }
    }
  }

  # Random walk distance: d_rw(i,j) = (m_ij + m_ji) / 2 for symmetry
  rw_dist <- (mfpt + t(mfpt)) / 2
  diag(rw_dist) <- 0

  # Inverse sum aggregation (matches tidygraph)
  rs <- rowSums(rw_dist)
  ifelse(rs > 0, 1 / rs, NA_real_)
}


# =============================================================================
# Local / neighborhood-based measures
# =============================================================================

#' Entropy centrality (centiserve-compatible)
#'
#' Graph-theoretic entropy: remove node v, count shortest paths in residual,
#' compute entropy of the path distribution. NOT Shannon entropy of degrees.
#' @keywords internal
#' @noRd
calculate_entropy <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  vapply(seq_len(n), function(v) {
    g_red <- igraph::delete_vertices(g, v)
    n_red <- igraph::vcount(g_red)
    if (n_red <= 1) return(0)

    sp <- igraph::distances(g_red, mode = mode, weights = NA)
    # Total number of finite shortest paths (excluding self-pairs)
    total_paths <- (sum(is.finite(sp)) - n_red) / 2
    if (total_paths <= 0) return(0)

    H <- 0
    for (w in seq_len(n_red)) {
      # Number of finite distances from w (excluding self)
      Y <- (sum(is.finite(sp[w, ])) - 1) / total_paths
      if (Y > 0) H <- H + Y * log2(Y)
    }
    -H
  }, numeric(1))
}


#' Semi-local centrality (centiserve-compatible)
#'
#' For each neighbor u of v, for each neighbor w of u, sum the size of
#' w's 2-neighborhood. Triple-nested computation.
#' @keywords internal
#' @noRd
calculate_semilocal <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  # Precompute 2-neighborhood sizes (excluding self)
  nbhood2_size <- vapply(seq_len(n), function(w) {
    length(igraph::neighborhood(g, order = 2, nodes = w, mode = mode)[[1]]) - 1L
  }, integer(1))

  vapply(seq_len(n), function(v) {
    nbs_v <- as.integer(igraph::neighbors(g, v, mode = mode))
    sl <- 0L
    for (u in nbs_v) {
      nbs_u <- as.integer(igraph::neighbors(g, u, mode = mode))
      for (w in nbs_u) {
        sl <- sl + nbhood2_size[w]
      }
    }
    as.numeric(sl)
  }, numeric(1))
}


#' ClusterRank (centiserve-compatible)
#'
#' cc[v] * sum(degree(w) + 1) for neighbors w. Uses clustering coefficient
#' directly, not 10^(-cc).
#' @keywords internal
#' @noRd
calculate_clusterrank <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  deg <- igraph::degree(g, mode = mode)
  cc <- igraph::transitivity(g, type = "local", isolates = "nan")

  vapply(seq_len(n), function(v) {
    if (is.nan(cc[v])) return(NaN)
    nbs <- as.integer(igraph::neighbors(g, v, mode = mode))
    if (length(nbs) == 0) return(0)
    cc[v] * sum(deg[nbs] + 1)
  }, numeric(1))
}


#' Bottleneck centrality (centiserve-compatible)
#'
#' For each source, compute ALL shortest paths. A node v gets +1 if it
#' appears in more than n/4 of those paths.
#' @keywords internal
#' @noRd
calculate_bottleneck <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(1L, n))

  bn <- integer(n)
  for (s in seq_len(n)) {
    # Get all shortest paths from s
    asp <- igraph::get.all.shortest.paths(g, from = s, to = igraph::V(g),
                                          mode = mode, weights = NA)
    # Count how often each node appears across all paths
    node_counts <- tabulate(unlist(asp$res), nbins = n)
    total_nodes <- length(node_counts)

    for (v in which(node_counts > total_nodes / 4)) {
      if (v != s) bn[v] <- bn[v] + 1L
    }
  }

  bn
}


#' Centroid value (centiserve-compatible)
#'
#' For each pair (u,v), gamma[u,v] = count of nodes w where d(u,w) < d(v,w).
#' f[u,v] = gamma[u,v] - gamma[v,u]. Centroid(v) = min f[v,i] over all i.
#' @keywords internal
#' @noRd
calculate_centroid <- function(g, mode = "all", weights = NULL) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(0, n))

  dist_weights <- if (is.null(weights)) NA else weights
  sp <- igraph::distances(g, mode = mode, weights = dist_weights)

  # Compute gamma matrix
  gamma <- matrix(0L, n, n)
  for (u in seq_len(n)) {
    for (v in seq_len(n)) {
      gamma[u, v] <- sum(sp[u, ] < sp[v, ])
    }
  }

  f_mat <- gamma - t(gamma)

  # Include self (f[v,v]=0), matching centiserve convention
  vapply(seq_len(n), function(v) {
    min(f_mat[v, ])
  }, numeric(1))
}


#' Maximum Neighborhood Component (centiserve-compatible)
#' @keywords internal
#' @noRd
calculate_mnc <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(integer(0))

  vapply(seq_len(n), function(v) {
    nbs <- as.integer(igraph::neighbors(g, v, mode = mode))
    if (length(nbs) <= 1) return(as.integer(length(nbs)))
    sub_g <- igraph::induced_subgraph(g, nbs)
    comps <- igraph::components(sub_g)
    as.integer(max(comps$csize))
  }, integer(1))
}


#' DMNC — Density of Maximum Neighborhood Component (centiserve-compatible)
#'
#' ec / max_component_size^epsilon where ec is the edge count of the
#' largest connected component in the neighborhood subgraph.
#' Default epsilon from centiserve is the parameter (default 1.0 I think...
#' actually the centiserve default is between 1 and 2, let me check).
#' @keywords internal
#' @noRd
calculate_dmnc <- function(g, mode = "all", epsilon = 1.7) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  # DMNC = E / N^epsilon where E = edges, N = nodes in the maximum

  # neighborhood component. Lin et al. (2008) recommend epsilon = 1.7
  # (close to 1.67 for four-community assumption). centiserve defaults
  # to 1.67. Both are valid per the original paper.

  vapply(seq_len(n), function(v) {
    nbs <- as.integer(igraph::neighbors(g, v, mode = mode))
    if (length(nbs) == 0) return(0)
    sub_g <- igraph::induced_subgraph(g, nbs)
    comps <- igraph::components(sub_g, mode = "strong")
    if (length(comps$csize) == 0) return(0)
    largest <- which(comps$csize == max(comps$csize))
    mc_nodes <- which(comps$membership %in% largest)
    mc_sub <- igraph::induced_subgraph(g, nbs[mc_nodes])
    ec <- igraph::ecount(mc_sub)
    mc_size <- max(comps$csize)
    if (ec == 0 || mc_size == 0) return(0)
    ec / mc_size^epsilon
  }, numeric(1))
}


#' Topological coefficient (centiserve-compatible)
#'
#' For each node v with neighbors N(v), for each neighbor nb:
#'   - Count distinct neighbors-of-nb that are not v
#'   - Track unique "extended neighbors" across all nb
#'   - Add extra +1 for each extended neighbor that is also in N(v)
#' tc = total / (|extended_set| * |N(v)|)
#' @keywords internal
#' @noRd
calculate_topological_coefficient <- function(g) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  deg <- igraph::degree(g, mode = "all")
  adj_list <- igraph::as_adj_list(g, mode = "all")

  vapply(seq_len(n), function(v) {
    nbs_v <- as.integer(adj_list[[v]])
    k_v <- length(nbs_v)
    if (k_v == 0) return(0)

    com_ne_nodes <- integer(0)
    tc <- 0L

    for (nb in nbs_v) {
      nbs_nb <- as.integer(adj_list[[nb]])
      for (nn in nbs_nb) {
        if (nn != v) {
          tc <- tc + 1L
          if (!nn %in% com_ne_nodes) {
            com_ne_nodes <- c(com_ne_nodes, nn)
            if (nn %in% nbs_v) {
              tc <- tc + 1L
            }
          }
        }
      }
    }

    if (length(com_ne_nodes) == 0) return(0)
    tc / (length(com_ne_nodes) * k_v)
  }, numeric(1))
}


#' Bridging centrality (betweenness * bridging coefficient)
#' @keywords internal
#' @noRd
calculate_bridging <- function(g, weights = NULL, directed = TRUE) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  deg <- igraph::degree(g, mode = "all")
  betw <- igraph::betweenness(g, weights = weights, directed = directed)

  bc <- vapply(seq_len(n), function(v) {
    if (deg[v] == 0) return(0)
    nbs <- as.integer(igraph::neighbors(g, v, mode = "all"))
    inv_deg_v <- 1 / deg[v]
    sum_inv_deg_nbs <- sum(1 / deg[nbs])
    if (sum_inv_deg_nbs == 0) return(0)
    inv_deg_v / sum_inv_deg_nbs
  }, numeric(1))

  betw * bc
}


#' Local bridging centrality (CINNA-compatible)
#'
#' (1/degree) * bridging_coefficient
#' @keywords internal
#' @noRd
calculate_local_bridging <- function(g) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  deg <- igraph::degree(g, mode = "all")

  vapply(seq_len(n), function(v) {
    if (deg[v] == 0) return(0)
    nbs <- as.integer(igraph::neighbors(g, v, mode = "all"))
    inv_deg_v <- 1 / deg[v]
    sum_inv_deg_nbs <- sum(1 / deg[nbs])
    if (sum_inv_deg_nbs == 0) return(0)
    inv_deg_v * (inv_deg_v / sum_inv_deg_nbs)
  }, numeric(1))
}


#' Effective network size (influenceR-compatible)
#'
#' Burt's effective size: degree minus redundancy.
#' @keywords internal
#' @noRd
calculate_effective_size <- function(g) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  deg <- igraph::degree(g, mode = "all")
  adj_list <- igraph::as_adj_list(g, mode = "all")

  vapply(seq_len(n), function(v) {
    nbs <- as.integer(adj_list[[v]])
    k <- length(nbs)
    if (k == 0) return(0)

    redundancy <- 0
    for (j in nbs) {
      nbs_j <- as.integer(adj_list[[j]])
      shared <- length(intersect(nbs, nbs_j))
      redundancy <- redundancy + shared / k
    }

    k - redundancy
  }, numeric(1))
}


#' Diversity centrality (igraph-compatible)
#'
#' Shannon entropy of edge weight distribution per node.
#' @keywords internal
#' @noRd
calculate_diversity <- function(g, weights = NULL) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  w <- if (!is.null(weights)) weights else igraph::E(g)$weight
  if (is.null(w)) {
    # Unweighted: all edges equal weight, so diversity = 1 for deg > 1
    deg <- igraph::degree(g, mode = "all")
    return(ifelse(deg > 1, 1, ifelse(deg == 1, 0, 0)))
  }

  el <- igraph::as_edgelist(g, names = FALSE)
  vapply(seq_len(n), function(v) {
    incident_idx <- which(el[, 1] == v | el[, 2] == v)
    k <- length(incident_idx)
    if (k <= 1) return(0)
    edge_weights <- abs(w[incident_idx])
    total <- sum(edge_weights)
    if (total == 0) return(0)
    p <- edge_weights / total
    p <- p[p > 0]
    # Normalized Shannon entropy (igraph convention): H / log2(degree)
    -sum(p * log2(p)) / log2(k)
  }, numeric(1))
}


#' Cross-clique connectivity (centiserve-compatible)
#'
#' Count of ALL cliques (not just maximal) that each node belongs to.
#' @keywords internal
#' @noRd
calculate_cross_clique <- function(g) {
  n <- igraph::vcount(g)
  if (n == 0) return(integer(0))

  cliques <- igraph::cliques(g)  # ALL cliques, not max_cliques
  as.integer(tabulate(unlist(cliques), nbins = n))
}


#' Markov centrality (centiserve-compatible)
#'
#' Inverse of column means of mean first passage time matrix.
#' @keywords internal
#' @noRd
calculate_markov <- function(g) {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(NA_real_, n))

  if (!igraph::is_connected(g, mode = "weak")) {
    warning("Markov centrality undefined for disconnected graphs",
            call. = FALSE)
    return(rep(NA_real_, n))
  }

  A <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  deg <- rowSums(A)
  deg[deg == 0] <- 1
  P <- A / deg

  if (!igraph::is_directed(g)) {
    pi_stat <- deg / sum(deg)
  } else {
    eig <- eigen(t(P))
    idx <- which.min(abs(Re(eig$values) - 1))
    pi_stat <- abs(Re(eig$vectors[, idx]))
    pi_stat <- pi_stat / sum(pi_stat)
  }

  W <- matrix(pi_stat, n, n, byrow = TRUE)
  Z <- tryCatch(solve(diag(n) - P + W), error = function(e) NULL)
  if (is.null(Z)) return(rep(NA_real_, n))

  mfpt <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j && pi_stat[j] > 1e-15) {
        mfpt[i, j] <- (Z[j, j] - Z[i, j]) / pi_stat[j]
      }
    }
  }

  # centiserve: 1 / column means
  col_means <- colMeans(mfpt)
  ifelse(col_means > 0, 1 / col_means, NA_real_)
}


#' Integration centrality (tidygraph-compatible)
#'
#' For each node, compute distances, then 1 - (d-1)/max(d), sum over all j.
#' @keywords internal
#' @noRd
calculate_integration <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(0, n))

  sp <- igraph::distances(g, mode = mode, weights = NA)
  max_d <- max(sp[is.finite(sp)])
  if (max_d <= 0) return(rep(n, n))

  vapply(seq_len(n), function(i) {
    dists <- sp[i, ]
    dists[!is.finite(dists)] <- max_d + 1
    sum(1 - (dists - 1) / max_d)
  }, numeric(1))
}


#' Expected centrality (based on degree)
#'
#' Sum of neighbor degrees. Simple but effective influence proxy.
#' @keywords internal
#' @noRd
calculate_expected <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  deg <- igraph::degree(g, mode = mode)
  adj <- igraph::as_adjacency_matrix(g, sparse = TRUE)

  if (igraph::is_directed(g) && mode == "in") {
    adj <- Matrix::t(adj)
  } else if (igraph::is_directed(g) && mode == "all") {
    adj <- adj | Matrix::t(adj)
  }

  as.numeric(adj %*% deg)
}


#' Gil-Schmidt power index (sna-compatible)
#'
#' sum(1/d(v,w)) / (n-1) for all reachable w.
#' @keywords internal
#' @noRd
calculate_gilschmidt <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n <= 1) return(rep(0, n))

  sp <- igraph::distances(g, mode = mode, weights = NA)
  diag(sp) <- NA

  vapply(seq_len(n), function(i) {
    dists <- sp[i, ]
    valid <- is.finite(dists) & !is.na(dists) & dists > 0
    if (sum(valid) == 0) return(0)
    sum(1 / dists[valid]) / (n - 1)
  }, numeric(1))
}


#' SALSA centrality (directed only)
#' @keywords internal
#' @noRd
calculate_salsa <- function(g) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (!igraph::is_directed(g)) {
    warning("SALSA requires a directed graph; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  A <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  out_deg <- rowSums(A)
  in_deg <- colSums(A)

  A_row <- A
  for (i in seq_len(n)) {
    if (out_deg[i] > 0) A_row[i, ] <- A_row[i, ] / out_deg[i]
  }
  A_col <- A
  for (j in seq_len(n)) {
    if (in_deg[j] > 0) A_col[, j] <- A_col[, j] / in_deg[j]
  }

  Auth_mat <- t(A_col) %*% A_row
  eig <- eigen(t(Auth_mat))
  idx <- which.min(abs(Re(eig$values) - 1))
  auth <- abs(Re(eig$vectors[, idx]))
  auth / max(auth)
}


#' LeaderRank (directed only)
#' @keywords internal
#' @noRd
calculate_leaderrank <- function(g) {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (!igraph::is_directed(g)) {
    warning("LeaderRank requires a directed graph; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  # Build extended graph with ground node (n+1) bidirectionally connected
  el <- igraph::as_edgelist(g, names = FALSE)
  ground <- n + 1L
  ground_edges <- rbind(cbind(ground, seq_len(n)), cbind(seq_len(n), ground))
  new_el <- rbind(el, ground_edges)
  g_ext <- igraph::graph_from_edgelist(new_el, directed = TRUE)

  # Row-normalized transition matrix (no damping — pure random walk)
  A <- as.matrix(igraph::as_adjacency_matrix(g_ext, sparse = FALSE))
  out_deg <- rowSums(A)
  out_deg[out_deg == 0] <- 1
  P <- A / out_deg

  # Power iteration on t(P) starting from (1,...,1, 0)
  n_ext <- n + 1L
  v <- c(rep(1, n), 0)
  tol <- 2e-05
  max_iter <- 1000L
  Pt <- t(P)
  for (iter in seq_len(max_iter)) {
    v_new <- as.numeric(Pt %*% v)
    err <- mean(abs(v_new - v) / pmax(abs(v), 1e-15))
    v <- v_new
    if (err < tol) break
  }

  # Redistribute ground node score to all nodes
  v[seq_len(n)] + v[ground] / n
}


# =============================================================================
# Local Average Connectivity (LAC) — Li et al. (2011)
# =============================================================================

#' Local Average Connectivity (LAC)
#'
#' For each node v, computes the average degree of v's neighbors within the
#' subgraph induced by those neighbors. Measures how interconnected a node's
#' neighborhood is. High LAC means neighbors interact heavily with each other.
#'
#' @param g igraph object
#' @param mode "all", "in", or "out" for directed graphs
#' @return Numeric vector of LAC values
#' @references
#' Li, M., Wang, J., Chen, X., Wang, H., & Pan, Y. (2011). A local average
#' connectivity-based method for identifying essential proteins from the network
#' level. Computational Biology and Chemistry, 35(3), 143-150.
#' @keywords internal
#' @noRd
calculate_lac <- function(g, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  adj_list <- igraph::as_adj_list(g, mode = mode)

  vapply(seq_len(n), function(v) {
    nbs <- as.integer(adj_list[[v]])
    k <- length(nbs)
    if (k == 0) return(0)

    # Subgraph C_v induced by neighbors of v
    sub_g <- igraph::induced_subgraph(g, nbs)
    # Local connectivity: degree of each neighbor within C_v
    local_deg <- igraph::degree(sub_g, mode = mode)

    # LAC = average local connectivity
    sum(local_deg) / k
  }, numeric(1))
}


# =============================================================================
# Community-aware measures
# =============================================================================

#' Participation coefficient (brainGraph-compatible)
#' @keywords internal
#' @noRd
calculate_participation <- function(g, membership = NULL, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (is.null(membership)) {
    warning("participation requires membership; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }
  stopifnot(length(membership) == n)

  deg <- igraph::degree(g, mode = mode)

  vapply(seq_len(n), function(i) {
    if (deg[i] == 0) return(0)
    nbs <- as.integer(igraph::neighbors(g, i, mode = mode))
    nb_modules <- membership[nbs]
    tab <- tabulate(nb_modules, nbins = max(membership))
    1 - sum((tab / deg[i])^2)
  }, numeric(1))
}


#' Within-module degree z-score (brainGraph-compatible)
#' @keywords internal
#' @noRd
calculate_within_module_z <- function(g, membership = NULL, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (is.null(membership)) {
    warning("within_module_z requires membership; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }
  stopifnot(length(membership) == n)

  k_within <- vapply(seq_len(n), function(i) {
    nbs <- as.integer(igraph::neighbors(g, i, mode = mode))
    sum(membership[nbs] == membership[i])
  }, numeric(1))

  z <- numeric(n)
  for (m in unique(membership)) {
    idx <- which(membership == m)
    kw <- k_within[idx]
    mu <- mean(kw)
    sigma <- stats::sd(kw)
    if (is.na(sigma) || sigma == 0) {
      # Match brainGraph: NaN when sd is 0, Inf→0 otherwise
      z[idx] <- NaN
    } else {
      z[idx] <- (kw - mu) / sigma
    }
  }
  z
}


#' Gateway coefficient (brainGraph-compatible)
#' @keywords internal
#' @noRd
calculate_gateway <- function(g, membership = NULL, mode = "all") {
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))
  if (is.null(membership)) {
    warning("gateway requires membership; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }
  stopifnot(length(membership) == n)

  A <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  Ki <- colSums(A)
  N <- max(membership)
  if (N <= 1) return(rep(0, n))

  cent <- Ki  # default centrality = degree

  # Cn = max module centrality sum
  Cn <- max(vapply(seq_len(N), function(x) sum(cent[membership == x]),
                   numeric(1)))

  # Kis[i,s] = sum of edges from i to module s
  Kis <- matrix(0, n, N)
  for (i in seq_len(n)) {
    for (s in seq_len(N)) {
      Kis[i, s] <- sum(A[i, membership == s])
    }
  }

  # Kjs[s,t] = sum of Kis[j,t] for j in module s = total edges from module s to module t
  Kjs <- matrix(0, N, N)
  for (s in seq_len(N)) {
    for (t_mod in seq_len(N)) {
      Kjs[s, t_mod] <- sum(Kis[membership == s, t_mod])
    }
  }

  result <- numeric(n)
  for (i in seq_len(n)) {
    if (Ki[i] == 0) { result[i] <- 0; next }

    # barKis = Kis[i,s] / Kjs[membership[i], s]
    barKis <- numeric(N)
    for (s in seq_len(N)) {
      if (Kjs[membership[i], s] > 0) {
        barKis[s] <- Kis[i, s] / Kjs[membership[i], s]
      }
    }

    # Cis = sum of centrality of neighbors in module s
    nbs <- which(A[, i] > 0)
    Cis <- numeric(N)
    for (s in seq_len(N)) {
      Cis[s] <- sum(cent[nbs[membership[nbs] == s]])
    }
    barCis <- Cis / Cn

    gis <- 1 - barKis * barCis
    result[i] <- 1 - (1 / Ki[i]^2) * sum(Kis[i, ]^2 * gis^2)
  }

  result
}


# =============================================================================
# Graph-level centralization measures
# =============================================================================

#' Freeman centralization (internal helper)
#' @keywords internal
#' @noRd
.freeman_centralization <- function(scores, theoretical_max) {
  if (length(scores) <= 1 || theoretical_max == 0) return(0)
  scores <- scores[!is.na(scores)]
  max_score <- max(scores)
  sum(max_score - scores) / theoretical_max
}


#' Centralization index
#'
#' Computes Freeman's centralization for degree, betweenness, closeness,
#' or eigenvector centrality.
#'
#' @param x Network input
#' @param measure One of "degree", "betweenness", "closeness", "eigenvector"
#' @param directed Logical or NULL
#' @param mode "all", "in", or "out"
#' @param ... Additional arguments passed to to_igraph()
#' @return Numeric scalar in [0, 1]
#'
#' @export
#' @examples
#' star <- matrix(0, 5, 5)
#' star[1, 2:5] <- 1; star[2:5, 1] <- 1
#' cograph::centralization(star, "degree")
centralization <- function(x, measure = c("degree", "betweenness",
                                          "closeness", "eigenvector"),
                           directed = NULL, mode = "all", ...) {
  measure <- match.arg(measure)
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for centralization()", call. = FALSE)
  }

  g <- to_igraph(x, directed = directed, ...)
  n <- igraph::vcount(g)
  is_dir <- igraph::is_directed(g)
  if (n <= 2) return(0)

  switch(measure,
    "degree" = {
      scores <- igraph::degree(g, mode = mode)
      theo_max <- if (is_dir) (n - 1)^2 else (n - 1) * (n - 2)
      .freeman_centralization(scores, theo_max)
    },
    "betweenness" = {
      scores <- igraph::betweenness(g, directed = is_dir)
      theo_max <- if (is_dir) (n - 1)^2 * (n - 2) else (n - 1)^2 * (n - 2) / 2
      .freeman_centralization(scores, theo_max)
    },
    "closeness" = {
      scores <- igraph::closeness(g, mode = mode, normalized = TRUE)
      theo_max <- (n - 2) * (n - 1) / (2 * n - 3)
      .freeman_centralization(scores, theo_max)
    },
    "eigenvector" = {
      scores <- igraph::eigen_centrality(g, directed = is_dir)$vector
      theo_max <- n - 1
      .freeman_centralization(scores, theo_max)
    }
  )
}


# =============================================================================
# Shared helper
# =============================================================================

#' Build incoming edge list for Brandes-style algorithms
#' @keywords internal
#' @noRd
.build_incoming <- function(el, edge_w, n, directed) {
  incoming <- vector("list", n)
  for (i in seq_len(nrow(el))) {
    incoming[[el[i, 2]]] <- rbind(incoming[[el[i, 2]]], c(el[i, 1], edge_w[i]))
    if (!directed) {
      incoming[[el[i, 1]]] <- rbind(incoming[[el[i, 1]]], c(el[i, 2], edge_w[i]))
    }
  }
  incoming
}
