# Reference side of the test-centrality-zoo-a-*.R files: inputs and measures written from each measure's
# published definition, in base R. Nothing here calls cograph. Small graphs only: distances and shortest-path
# counts come from powers of the adjacency matrix, cliques and subsets from exhaustive enumeration.

#' A graph from an edge list, as a named adjacency matrix
#'
#' @param edges Node pairs, flattened: `c(1, 2, 2, 3)` is the two edges 1-2 and 2-3.
#' @param n Number of nodes.
#' @param weights One weight per edge (recycled).
#' @param directed Whether an edge is an arc from the first node to the second.
#' @param names Node names (default `v1`, `v2`, ...).
#' @return A square numeric matrix with dimnames; `a[i, j]` is the tie from `i` to `j`.
za_graph <- function(edges, n = max(edges), weights = 1, directed = FALSE, names = paste0("v", seq_len(n))) {
  pairs <- matrix(edges, ncol = 2L, byrow = TRUE)
  stopifnot("`edges` must name nodes 1 to n" = all(pairs >= 1 & pairs <= n))
  graph <- matrix(0, n, n, dimnames = list(names, names))
  graph[pairs] <- rep_len(weights, nrow(pairs))
  if (!directed) graph[pairs[, 2:1, drop = FALSE]] <- rep_len(weights, nrow(pairs))
  graph
}

#' Star on `n` nodes, the first being the hub
za_star <- function(n, names = LETTERS[seq_len(n)]) za_graph(c(rbind(1, seq_len(n)[-1L])), n, names = names)

#' Path on `n` nodes
za_path <- function(n, names = LETTERS[seq_len(n)]) za_graph(c(rbind(seq_len(n - 1L), seq_len(n)[-1L])), n, names = names)

#' Cycle on `n` nodes, undirected or as a directed ring
za_ring <- function(n, directed = FALSE, names = paste0("v", seq_len(n))) {
  za_graph(c(rbind(seq_len(n), c(seq_len(n)[-1L], 1L))), n, directed = directed, names = names)
}

#' Complete graph on `n` nodes
za_complete <- function(n, names = paste0("v", seq_len(n))) {
  graph <- matrix(1, n, n, dimnames = list(names, names))
  diag(graph) <- 0
  graph
}

#' Two triangles A-B-C and D-E-F joined by the bridge C-D
za_bridge6 <- function() za_graph(c(1, 2, 1, 3, 2, 3, 4, 5, 4, 6, 5, 6, 3, 4), 6, names = LETTERS[1:6])

#' The block-diagonal union of graphs, with the names of the parts in order
za_union <- function(...) {
  parts <- list(...)
  sizes <- vapply(parts, nrow, integer(1))
  n <- sum(sizes)
  offset <- cumsum(c(0L, sizes))
  graph <- matrix(0, n, n)
  blocks <- lapply(seq_along(parts), \(k) offset[[k]] + seq_len(sizes[[k]]))
  graph <- Reduce(\(g, k) {
    g[blocks[[k]], blocks[[k]]] <- parts[[k]]
    g
  }, seq_along(parts), graph)
  names <- paste0("v", seq_len(n))
  dimnames(graph) <- list(names, names)
  graph
}

#' A random labelled tree on `n` nodes: node k > 1 hangs from a node drawn among 1..k-1
#'
#' @param seed The seed of the draw; the caller's random stream is restored.
za_random_tree <- function(n, seed) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()),
          add = TRUE, after = FALSE)
  set.seed(seed)
  parents <- vapply(seq_len(n)[-1L], \(k) sample.int(k - 1L, 1L), integer(1))
  za_graph(c(rbind(parents, seq_len(n)[-1L])), n)
}

#' The 0/1 skeleton: any non-zero entry is a tie, loops removed
za_binary <- function(graph, symmetric = FALSE) {
  binary <- (graph != 0) * 1
  if (symmetric) binary <- pmax(binary, t(binary))
  diag(binary) <- 0
  binary
}

#' Walk counts: element k + 1 is the matrix of the numbers of walks of length k (k = 0 .. kmax)
za_walks <- function(binary, kmax = nrow(binary) - 1L) {
  Reduce(\(power, k) power %*% binary, seq_len(max(kmax, 0L)), accumulate = TRUE, init = diag(nrow(binary)))
}

#' Hop distances: `d[i, j]` is the fewest arcs from i to j (Inf when j is not reachable)
za_hops <- function(graph) {
  binary <- za_binary(graph)
  n <- nrow(binary)
  walks <- za_walks(binary)
  start <- matrix(Inf, n, n, dimnames = dimnames(graph))
  # the distance is the shortest length at which a walk exists
  Reduce(\(d, k) {
    reached <- walks[[k + 1L]] > 0 & is.infinite(d)
    d[reached] <- k
    d
  }, seq.int(0L, n - 1L), start)
}

#' Numbers of shortest paths: a walk as long as the distance is a shortest path
za_geodesics <- function(graph, d = za_hops(graph)) {
  walks <- za_walks(za_binary(graph))
  finite <- which(is.finite(d), arr.ind = TRUE)
  counts <- matrix(0, nrow(d), ncol(d), dimnames = dimnames(d))
  counts[finite] <- vapply(seq_len(nrow(finite)), \(r) walks[[d[finite[r, , drop = FALSE]] + 1L]][finite[r, 1L], finite[r, 2L]],
                           numeric(1))
  counts
}

#' Share of the shortest s-t paths through node v, for every pair s, t other than v (Freeman 1977)
za_dependency <- function(d, sigma, v) {
  through <- outer(d[, v], d[v, ], "+")
  on_path <- is.finite(d) & is.finite(through) & through == d & sigma > 0
  share <- outer(sigma[, v], sigma[v, ]) / sigma
  share[!on_path] <- 0
  share[v, ] <- 0
  share[, v] <- 0
  diag(share) <- 0
  share
}

#' Betweenness from the definition, each pair weighted by `pair_weight(d)`, pairs further than `cutoff` dropped
#'
#' @return One value per node; unordered pairs on a symmetric graph, ordered pairs otherwise.
za_betweenness <- function(graph, pair_weight = \(d) 1 + 0 * d, cutoff = Inf) {
  d <- za_hops(graph)
  sigma <- za_geodesics(graph, d)
  pairs <- if (isSymmetric(unname(za_binary(graph)))) 2 else 1
  vapply(seq_len(nrow(graph)), \(v) {
    share <- za_dependency(d, sigma, v)
    counted <- share > 0 & d <= cutoff
    sum(share[counted] * pair_weight(d[counted])) / pairs
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

#' Core numbers (Seidman 1983): the largest k such that the node is in a subgraph of minimum degree k
za_core <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  n <- nrow(binary)
  prune <- function(keep, k) {
    degree <- rowSums(binary[keep, keep, drop = FALSE])
    if (all(degree >= k)) keep else prune(keep[degree >= k], k)
  }
  in_core <- vapply(seq_len(max(rowSums(binary), 0) + 1L) - 1L,
                    \(k) seq_len(n) %in% prune(seq_len(n), k), logical(n))
  stats::setNames(as.numeric(rowSums(matrix(in_core, n)) - 1), rownames(graph))
}

#' Every subset of 1..n as a logical matrix, one column per non-empty subset
za_subsets <- function(n) {
  vapply(seq_len(2^n - 1), \(code) as.logical(intToBits(code))[seq_len(n)], logical(n))
}

#' H-index of a set of values: the largest h with at least h values >= h (Hirsch 2005)
za_hindex <- function(values) sum(sort(values, decreasing = TRUE) >= seq_along(values))

#' Stationary distribution of a row-stochastic matrix (the solution of pi P = pi, sum(pi) = 1)
za_stationary <- function(transition) {
  # pi (P - I) = 0 with one balance equation replaced by sum(pi) = 1 (an irreducible chain). solve(), not
  # qr.solve(): in the full run a loaded reference namespace sends qr() to an array method that fails.
  n <- nrow(transition)
  system <- t(transition) - diag(n)
  system[n, ] <- 1
  as.numeric(solve(system, c(numeric(n - 1L), 1)))
}

#' Spectral radius
za_spectral_radius <- function(matrix) max(Mod(eigen(matrix, only.values = TRUE)$values))

#' Moore-Penrose inverse of a symmetric matrix, through its eigendecomposition
za_pinv <- function(matrix) {
  e <- eigen(matrix, symmetric = TRUE)
  keep <- abs(e$values) > 1e-10 * max(1, abs(e$values))
  e$vectors[, keep, drop = FALSE] %*% (t(e$vectors[, keep, drop = FALSE]) / e$values[keep])
}

# -- measures, each from its definition ----------------------------------------------------------------------

#' Rumor centrality on a tree (Shah & Zaman 2011, eq. 10): R(v) = n! / prod_u T_u^v, the product over the
#' sizes of the subtrees rooted at each node u when the tree hangs from v; returned on the log scale.
za_rumor_log <- function(tree) {
  d <- za_hops(tree)
  n <- nrow(tree)
  vapply(seq_len(n), \(v) {
    # w is in u's subtree when u lies on the path from v to w
    sizes <- vapply(seq_len(n), \(u) sum(d[v, u] + d[u, ] == d[v, ]), numeric(1))
    lfactorial(n) - sum(log(sizes))
  }, numeric(1)) |> stats::setNames(rownames(tree))
}

#' Search information S(i, j) (Rosvall et al. 2005): minus log2 of the probability of following one of the
#' shortest i-j paths when every step picks a link at random, never stepping back (degree minus one).
za_search_information <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  d <- za_hops(binary)
  n <- nrow(binary)
  # probability of each shortest path, summed, by dynamic programming over the distance layers from i
  probability <- t(vapply(seq_len(n), \(i) {
    layers <- split(seq_len(n), d[i, ])
    reach <- Reduce(\(p, layer) {
      next_nodes <- layers[[layer]]
      p[next_nodes] <- vapply(next_nodes, \(j) {
        before <- which(binary[, j] > 0 & d[i, ] == d[i, j] - 1)
        sum(p[before] / ifelse(before == i, degree[before], degree[before] - 1))
      }, numeric(1))
      p
    }, setdiff(names(layers), c("0", "Inf")), replace(numeric(n), i, 1))
    reach
  }, numeric(n)))
  info <- -log2(probability)
  diag(info) <- 0
  info
}

#' Entropy, in nats, of a vector of non-negative scores read as a distribution
za_entropy <- function(f) {
  p <- f[f > 0] / sum(f)
  -sum(p * log(p))
}

#' Entropy variation (Ai 2017): entropy of the degree or betweenness distribution minus that entropy once the
#' node is deleted.
za_entropy_variation <- function(graph, of = c("degree", "betweenness")) {
  of <- match.arg(of)
  scores <- switch(of, degree = \(g) rowSums(za_binary(g)), betweenness = \(g) za_betweenness(g))
  whole <- za_entropy(scores(graph))
  vapply(seq_len(nrow(graph)), \(v) whole - za_entropy(scores(graph[-v, -v, drop = FALSE])), numeric(1)) |>
    stats::setNames(rownames(graph))
}

#' Gould-Fernandez brokerage roles (Gould & Fernandez 1989): open two-paths a -> v -> b (no arc a -> b) counted
#' by the groups of a, v and b.
za_brokerage <- function(graph, membership) {
  binary <- za_binary(graph)
  n <- nrow(binary)
  roles <- c("coordinator", "itinerant", "representative", "gatekeeper", "liaison")
  counts <- t(vapply(seq_len(n), \(v) {
    open <- which(outer(binary[, v] > 0, binary[v, ] > 0) & binary == 0, arr.ind = TRUE)
    open <- open[open[, 1L] != open[, 2L], , drop = FALSE]
    group_a <- membership[open[, 1L]]
    group_b <- membership[open[, 2L]]
    own <- membership[[v]]
    role <- ifelse(group_a == own & group_b == own, "coordinator",
                   ifelse(group_a == group_b, "itinerant",
                          ifelse(group_a == own, "representative", ifelse(group_b == own, "gatekeeper", "liaison"))))
    vapply(roles, \(r) sum(role == r), numeric(1))
  }, numeric(5L)))
  dimnames(counts) <- list(rownames(graph), roles)
  counts
}

#' Group betweenness (Everett & Borgatti 1999): over ordered pairs s, t outside C, the share of the shortest
#' s-t paths that meet C. Paths avoiding C are the walks of length d(s, t) in the graph without C.
za_group_betweenness <- function(graph, group) {
  d <- za_hops(graph)
  sigma <- za_geodesics(graph, d)
  outside <- setdiff(seq_len(nrow(graph)), group)
  binary <- za_binary(graph)
  walks <- za_walks(binary[outside, outside, drop = FALSE], kmax = nrow(graph) - 1L)
  pairs <- which(outer(outside, outside, "!=") & is.finite(d[outside, outside]), arr.ind = TRUE)
  avoiding <- vapply(seq_len(nrow(pairs)), \(r) {
    walks[[d[outside[pairs[r, 1L]], outside[pairs[r, 2L]]] + 1L]][pairs[r, 1L], pairs[r, 2L]]
  }, numeric(1))
  total <- sigma[outside, outside][pairs]
  sum((total - avoiding) / total)
}

#' Gravity (Ma et al. 2016): sum over 0 < d(i, j) <= radius of m_i m_j / d(i, j)^2
za_gravity <- function(graph, mass, radius = 3) {
  d <- za_hops(za_binary(graph, symmetric = TRUE))
  inside <- is.finite(d) & d > 0 & d <= radius
  terms <- outer(mass, mass) / d^2
  terms[!inside] <- 0
  stats::setNames(rowSums(terms), rownames(graph))
}

#' Delta closeness (Agneessens et al. 2017, eq. 2): sum over reached j of d(i, j)^-delta, over n - 1
za_delta_closeness <- function(graph, delta) {
  d <- za_hops(graph)
  reached <- is.finite(d) & d > 0
  terms <- d^-delta
  terms[!reached] <- 0
  stats::setNames(rowSums(terms) / (nrow(graph) - 1), rownames(graph))
}

#' Ego betweenness (Everett & Borgatti 2005): the node's betweenness inside its ego network
za_ego_betweenness <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  vapply(seq_len(nrow(binary)), \(v) {
    ego <- sort(c(v, which(binary[v, ] > 0)))
    if (length(ego) < 3L) return(0)
    za_betweenness(binary[ego, ego, drop = FALSE])[[match(v, ego)]]
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

#' Truss number of a node (Cohen 2008; Malliaros et al. 2016): the largest k such that one of its edges lies
#' in the k-truss, the subgraph whose every edge lies in at least k - 2 of its triangles.
za_truss <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  truss_of <- function(edges, k) {
    support <- (edges %*% edges) * edges
    kept <- edges * (support >= k - 2)
    if (identical(kept, edges)) edges else truss_of(kept, k)
  }
  levels <- seq_len(max(nrow(binary), 2L))[-1L]
  node_in <- vapply(levels, \(k) rowSums(truss_of(binary, k)) > 0, logical(nrow(binary)))
  stats::setNames(apply(matrix(node_in, nrow(binary)), 1L, \(row) max(c(0, levels[row]))), rownames(graph))
}

#' Jackson (2020): the godfather index counts pairs of unlinked neighbours, the support counts neighbours with
#' a common neighbour. Hwang et al. (2008): bridging coefficient (1 / d_i) / sum_j 1 / d_j over neighbours.
za_godfather <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  linked <- diag(binary %*% binary %*% binary) / 2
  stats::setNames(choose(degree, 2) - linked, rownames(graph))
}
za_support <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  stats::setNames(rowSums(binary * ((binary %*% binary) > 0)), rownames(graph))
}
za_bridging_coefficient <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  inverse <- ifelse(degree > 0, 1 / degree, 0)
  neighbours <- as.numeric(binary %*% inverse)
  stats::setNames(ifelse(degree > 0, inverse / neighbours, 0), rownames(graph))
}

#' Mixed-degree decomposition (Zeng & Zhang 2013) by exhaustive subsets: a node's index is the largest, over
#' the node sets S that hold it, of the smallest mixed degree k_r + lambda k_e inside S (k_r ties inside S,
#' k_e ties leaving S).
za_mdd <- function(graph, lambda = 0.7) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  subsets <- za_subsets(nrow(binary))
  floor_of <- apply(subsets, 2L, \(s) {
    inside <- rowSums(binary[, s, drop = FALSE])
    min((inside + lambda * (degree - inside))[s])
  })
  stats::setNames(apply(subsets, 1L, \(holds) max(floor_of[holds])), rownames(graph))
}

#' Volume (Wehmuth & Ziviani 2011): the degrees of every node within `radius` hops, the node included
za_volume <- function(graph, radius = 2) {
  binary <- za_binary(graph, symmetric = TRUE)
  d <- za_hops(binary)
  stats::setNames(as.numeric((is.finite(d) & d <= radius) %*% rowSums(binary)), rownames(graph))
}

#' Maximal clique centrality (Chin et al. 2014): sum of (|C| - 1)! over the maximal cliques C (two nodes or
#' more) that hold the node, cliques found by enumerating every node subset.
za_mcc <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  n <- nrow(binary)
  subsets <- za_subsets(n)
  is_clique <- apply(subsets, 2L, \(s) sum(s) >= 2 && all(binary[s, s][upper.tri(diag(sum(s)))] == 1))
  is_maximal <- apply(subsets, 2L, \(s) !any(rowSums(binary[!s, s, drop = FALSE]) == sum(s)))
  cliques <- subsets[, is_clique & is_maximal, drop = FALSE]
  stats::setNames(as.numeric(cliques %*% factorial(colSums(cliques) - 1)), rownames(graph))
}

#' Diffusion centrality (Banerjee et al. 2013): sum over t = 1..T of (qA)^t 1
za_diffusion <- function(graph, q = 1, steps = 3) {
  walks <- Reduce(\(power, t) power %*% (q * graph), seq_len(steps), accumulate = TRUE, init = diag(nrow(graph)))
  stats::setNames(Reduce(`+`, lapply(walks[-1L], rowSums), numeric(nrow(graph))), rownames(graph))
}

#' Dynamics-sensitive centrality (Liu et al. 2016): sum over r = 0..T-1 of beta A (beta A + (1 - mu) I)^r 1
za_dynamics_sensitive <- function(graph, beta = 0.1, mu = 1, steps = 5) {
  binary <- za_binary(graph, symmetric = TRUE)
  step <- beta * binary + (1 - mu) * diag(nrow(binary))
  powers <- Reduce(\(power, r) power %*% step, seq_len(max(steps - 1L, 0L)), accumulate = TRUE, init = diag(nrow(binary)))
  if (steps == 0) return(stats::setNames(numeric(nrow(graph)), rownames(graph)))
  stats::setNames(Reduce(`+`, lapply(powers, \(power) rowSums(beta * binary %*% power))), rownames(graph))
}

#' Dynamical importance (Restrepo et al. 2006), exact: relative drop of the spectral radius on deletion
za_dynamical_importance <- function(graph) {
  diag(graph) <- 0
  whole <- za_spectral_radius(graph)
  vapply(seq_len(nrow(graph)), \(v) (whole - za_spectral_radius(graph[-v, -v, drop = FALSE])) / whole, numeric(1)) |>
    stats::setNames(rownames(graph))
}

#' Malatya centrality (Karci et al. 2022): sum over neighbours of d_i / d_j
za_malatya <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  stats::setNames(degree * as.numeric(binary %*% ifelse(degree > 0, 1 / degree, 0)), rownames(graph))
}

#' Resistance curvature (Devriendt & Lambiotte 2022, eq. 2): 1 - (1/2) sum_j w_ij R_ij, with the effective
#' resistances read from the pseudo-inverse of the Laplacian
za_resistance_curvature <- function(graph) {
  conductance <- graph + t(graph) * !isSymmetric(unname(graph))
  diag(conductance) <- 0
  laplacian <- diag(rowSums(conductance)) - conductance
  inverse <- za_pinv(laplacian)
  resistance <- outer(diag(inverse), diag(inverse), "+") - 2 * inverse
  stats::setNames(1 - rowSums(conductance * resistance) / 2, rownames(graph))
}

#' Extended neighbourhood coreness (Bae & Kim 2014): sum over neighbours j, and their neighbours l, of k_s(l)
za_extended_coreness <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  stats::setNames(as.numeric(binary %*% (binary %*% za_core(binary))), rownames(graph))
}

#' Extended gravity (Ma et al. 2016): sum over neighbours of their k-shell gravity within `radius`
za_extended_gravity <- function(graph, radius = 3) {
  binary <- za_binary(graph, symmetric = TRUE)
  stats::setNames(as.numeric(binary %*% za_gravity(binary, za_core(binary), radius)), rownames(graph))
}

#' Barrat et al. (2004) weighted clustering: sum over ordered neighbour pairs j, h that are linked of
#' (w_ij + w_ih) / 2, over s_i (k_i - 1); 0 below two neighbours
za_barrat <- function(weights) {
  binary <- za_binary(weights)
  degree <- rowSums(binary)
  strength <- rowSums(weights)
  closed <- vapply(seq_len(nrow(weights)), \(i) {
    pairs <- outer(binary[i, ] > 0, binary[i, ] > 0) & binary > 0
    sum(outer(weights[i, ], weights[i, ], "+")[pairs]) / 2
  }, numeric(1))
  ifelse(degree > 1, closed / (strength * (degree - 1)), 0)
}

#' CDA (Wang et al. 2018, eqs. 2-6): CD_i = (alpha d_i + (1 - alpha) s_i) / (1 + exp(-C_i^w)), then
#' PC_i = CD_i + sum_j (w_ij / w_max) CD_j
za_cda <- function(weights, alpha = 0.5) {
  diag(weights) <- 0
  clustering <- za_barrat(weights)
  cd <- (alpha * rowSums(za_binary(weights)) + (1 - alpha) * rowSums(weights)) / (1 + exp(-clustering))
  stats::setNames(as.numeric(cd + (weights / max(weights)) %*% cd), rownames(weights))
}

#' Improved closeness (Luan et al. 2021): (n - 1) / sum_j d_ij / sigma_ij^alpha; 0 on a disconnected graph
za_improved_closeness <- function(graph, alpha = 0.2) {
  binary <- za_binary(graph, symmetric = TRUE)
  d <- za_hops(binary)
  sigma <- za_geodesics(binary, d)
  off <- row(d) != col(d)
  farness <- rowSums(ifelse(off, d / sigma^alpha, 0))
  stats::setNames(ifelse(is.finite(farness), (nrow(binary) - 1) / farness, 0), rownames(graph))
}

#' Exogenous centrality (Everett & Borgatti 2010, eqs. 3 and 8): sum over the other nodes of the drop of a base
#' centrality when the node is deleted. Reverse closeness keeps the original number of nodes N.
za_exogenous <- function(graph, base = c("reverse_closeness", "betweenness", "degree"), mode = c("all", "out", "in")) {
  base <- match.arg(base)
  mode <- match.arg(mode)
  binary <- za_binary(graph, symmetric = mode == "all")
  if (mode == "in") binary <- t(binary)
  n <- nrow(binary)
  score <- switch(base,
    degree = \(b) rowSums(b),
    betweenness = \(b) za_betweenness(b),
    reverse_closeness = \(b) {
      d <- za_hops(b)
      rowSums(ifelse(row(d) != col(d), pmax(n - d, 0), 0))
    })
  whole <- score(binary)
  vapply(seq_len(n), \(v) sum(whole[-v] - score(binary[-v, -v, drop = FALSE])), numeric(1)) |>
    stats::setNames(rownames(graph))
}

#' Global structure model (Ullah et al. 2021): exp(k_s(i) / N) sum_j k_s(j) / d_ij over reached j
za_global_structure <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  d <- za_hops(binary)
  core <- za_core(binary)
  reached <- is.finite(d) & d > 0
  stats::setNames(exp(core / nrow(binary)) * rowSums(ifelse(reached, outer(rep(1, nrow(d)), core) / d, 0)),
                  rownames(graph))
}

#' Hybrid global structure model (Mukhtar et al. 2023): s_i = exp(k_s(i) k_i / N), a = ceiling(log2(mean s)),
#' s_i sum_j s_j / d_ij^a over reached j
za_hybrid_global_structure <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  d <- za_hops(binary)
  self <- exp(za_core(binary) * rowSums(binary) / nrow(binary))
  exponent <- ceiling(log2(mean(self)))
  reached <- is.finite(d) & d > 0
  stats::setNames(self * rowSums(ifelse(reached, outer(rep(1, nrow(d)), self) / d^exponent, 0)), rownames(graph))
}

#' Improved global structure model (Zhu & Wang 2022, as Mukhtar et al. 2023, eq. 5):
#' exp(k_i / N) sum_j k_j / d_ij^a, a = ceiling(log2(mean degree)), over reached j
za_improved_global_structure <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  d <- za_hops(binary)
  degree <- rowSums(binary)
  exponent <- ceiling(log2(mean(degree)))
  reached <- is.finite(d) & d > 0
  stats::setNames(exp(degree / nrow(binary)) * rowSums(ifelse(reached, outer(rep(1, nrow(d)), degree) / d^exponent, 0)),
                  rownames(graph))
}

#' Weighted LeaderRank (Li et al. 2014): a ground node g linked both ways to every node, arc g -> i weighted
#' (k_i^in)^alpha, every other arc 1; the resource of N + 1 units at the stationary state of the row-normalised
#' walk, ground left out.
za_weighted_leaderrank <- function(graph, alpha = 1) {
  binary <- za_binary(graph)
  n <- nrow(binary)
  ground_out <- colSums(binary)^alpha
  augmented <- rbind(cbind(binary, 1), c(ground_out, 0))
  pi <- za_stationary(augmented / rowSums(augmented))
  stats::setNames((n + 1) * pi[seq_len(n)], rownames(graph))
}

#' Adaptive LeaderRank (Xu & Wang 2017): arcs into node i weighted by its H-index h_i (on the undirected
#' skeleton for `h_mode = "all"`), the ground has H-index 1; N units at the stationary state, ground left out.
za_adaptive_leaderrank <- function(graph, h_mode = c("all", "out", "in")) {
  h_mode <- match.arg(h_mode)
  binary <- za_binary(graph)
  n <- nrow(binary)
  view <- switch(h_mode, all = pmax(binary, t(binary)), out = binary, `in` = t(binary))
  degree <- rowSums(view)
  h <- vapply(seq_len(n), \(i) za_hindex(degree[view[i, ] > 0]), numeric(1))
  augmented <- rbind(cbind(binary, 1), 1) * matrix(c(h, 1), n + 1L, n + 1L, byrow = TRUE)
  augmented[n + 1L, n + 1L] <- 0
  pi <- za_stationary(augmented / rowSums(augmented))
  stats::setNames(n * pi[seq_len(n)], rownames(graph))
}

#' Graph regularization centrality (Dal Col & Petronetto 2023): 1 / diag((I + gamma L)^-1)
za_graph_regularization <- function(graph, gamma = 1) {
  weights <- graph + t(graph) * !isSymmetric(unname(graph))
  diag(weights) <- 0
  laplacian <- diag(rowSums(weights)) - weights
  stats::setNames(1 / diag(solve(diag(nrow(weights)) + gamma * laplacian)), rownames(graph))
}

#' Random walk decay (Was et al. 2019, Definition 1, eq. 6): sum over starts u of b_u E_u[a^T_v], T_v the first
#' arrival at v. For each v, h = E_.[a^T_v] solves h_v = 1, h_u = a sum_w P_uw h_w; a node without out-arcs
#' ends the walk.
za_random_walk_decay <- function(graph, decay = 0.5, mass = rep(1, nrow(graph))) {
  out <- rowSums(graph)
  transition <- graph / ifelse(out > 0, out, 1)
  n <- nrow(graph)
  vapply(seq_len(n), \(v) {
    others <- seq_len(n)[-v]
    h <- numeric(n)
    h[v] <- 1
    h[others] <- solve(diag(n - 1L) - decay * transition[others, others, drop = FALSE],
                       decay * transition[others, v])
    sum(mass * h)
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

# -- local and dimension measures ----------------------------------------------------------------------------

#' OLS slope of y on x
za_slope <- function(x, y) sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)

#' Ring sizes n_i(r), r = 1 .. eccentricity, of every node; `d` holds hop distances from the row node
za_rings <- function(d) {
  lapply(seq_len(nrow(d)), \(i) {
    reached <- d[i, is.finite(d[i, ]) & d[i, ] > 0]
    tabulate(reached, nbins = max(c(0, reached)))
  })
}

#' Distance entropy (Stella & De Domenico 2018): entropy of the ring sizes, over the log of the number of
#' distance values between the smallest and the largest (log(M - m + 1); the printed log(M - m) is undefined
#' for two distances); 0 for one distance, NaN when nothing is reached
za_distance_entropy <- function(d) {
  vapply(za_rings(d), \(rings) {
    if (!length(rings)) return(NaN)
    observed <- rings[rings > 0]
    if (length(observed) == 1L) return(0)
    span <- length(rings) - which(rings > 0)[[1L]] + 1L
    p <- observed / sum(observed)
    -sum(p * log(p)) / log(span)
  }, numeric(1)) |> stats::setNames(rownames(d))
}

#' Local dimension (Silva & Costa 2013; Pu et al. 2014): slope of log B_i(r) on log r, the ball counting the
#' centre; with one radius the discretised derivative r n_i(r) / B_i(r) at r = 1
za_local_dimension <- function(d) {
  vapply(za_rings(d), \(rings) {
    if (!length(rings)) return(NaN)
    ball <- 1 + cumsum(rings)
    if (length(ball) == 1L) return(rings[[1L]] / ball[[1L]])
    za_slope(log(seq_along(ball)), log(ball))
  }, numeric(1)) |> stats::setNames(rownames(d))
}

#' Local information dimensionality (Wen & Deng 2020): minus the slope of -p ln p, p = B_i(l) / N, on ln l
#' for l = 1 .. ceiling(eccentricity / 2); with one box (eq. 15) (1 + ln p) n_i(1) / N
za_local_information_dimension <- function(d) {
  n <- nrow(d)
  vapply(za_rings(d), \(rings) {
    if (!length(rings)) return(NaN)
    boxes <- ceiling(length(rings) / 2)
    p <- (1 + cumsum(rings))[seq_len(boxes)] / n
    if (boxes == 1L) return((1 + log(p[[1L]])) * rings[[1L]] / n)
    -za_slope(log(seq_len(boxes)), -p * log(p))
  }, numeric(1)) |> stats::setNames(rownames(d))
}

#' Local dimension at a fixed radius (Silva & Costa 2013, eq. 4): r n_i(r) / B_i(r), 0 when n_i(r) is empty
za_local_dimension_fixed <- function(d, r = 2) {
  shell <- rowSums(d == r)
  ball <- rowSums(d <= r)
  stats::setNames(ifelse(shell > 0, r * shell / ball, 0), rownames(d))
}

#' Fuzzy local dimension (Wen & Jiang 2019): slope of log N_i(r) on log r, N_i(r) the mean of exp(-d^2 / r^2)
#' over the nodes within r (centre included), r = 1 .. eccentricity; NaN below two radii
za_fuzzy_local_dimension <- function(d) {
  vapply(seq_len(nrow(d)), \(i) {
    di <- d[i, is.finite(d[i, ])]
    if (max(di) < 2) return(NaN)
    fuzzy <- vapply(seq_len(max(di)), \(r) mean(exp(-di[di <= r]^2 / r^2)), numeric(1))
    za_slope(log(seq_along(fuzzy)), log(fuzzy))
  }, numeric(1)) |> stats::setNames(rownames(d))
}

#' Local volume dimension (Li & Deng 2021): slope of log V_i(l) on log l, V_i(l) the degrees within l hops
#' (centre included), l = 1 .. eccentricity; NaN below two radii
za_local_volume_dimension <- function(graph) {
  d <- za_hops(graph)
  degree <- rowSums(za_binary(graph, symmetric = TRUE))
  vapply(seq_len(nrow(d)), \(i) {
    reach <- max(d[i, is.finite(d[i, ])])
    if (reach < 2) return(NaN)
    volume <- vapply(seq_len(reach), \(l) sum(degree[d[i, ] <= l]), numeric(1))
    za_slope(log(seq_len(reach)), log(volume))
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

#' Local efficiency (Latora & Marchiori 2001): efficiency of the subgraph induced by the neighbours, the mean
#' of 1 / d over its ordered pairs; 0 below two neighbours
za_local_efficiency <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  vapply(seq_len(nrow(binary)), \(v) {
    neighbours <- which(binary[v, ] > 0)
    if (length(neighbours) < 2L) return(0)
    d <- za_hops(binary[neighbours, neighbours, drop = FALSE])
    off <- row(d) != col(d)
    mean(1 / d[off])
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

#' Local entropy (Nie et al. 2016): -sum over neighbours of k_j log k_j
za_local_entropy <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  stats::setNames(-as.numeric(binary %*% ifelse(degree > 0, degree * log(degree), 0)), rownames(graph))
}

#' Weighted H-index (Gao et al. 2019, eq. 3): the H-index of the link weights k_i k_j, each repeated k_j times
za_weighted_h_index <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  vapply(seq_len(nrow(binary)), \(i) {
    neighbours <- which(binary[i, ] > 0)
    za_hindex(rep(degree[[i]] * degree[neighbours], degree[neighbours]))
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

#' Burt's redundancy on a binary graph (Borgatti 1997): twice the ties among the neighbours over the degree
za_redundancy <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  stats::setNames(ifelse(degree > 0, diag(binary %*% binary %*% binary) / degree, 0), rownames(graph))
}

#' Renewed coreness (Liu et al. 2015): links whose diffusion importance
#' (|N(j) minus N[i]| + |N(i) minus N[j]|) / 2 is below `threshold` are removed; core numbers of what is left
za_renewed_coreness <- function(graph, threshold = 2) {
  binary <- za_binary(graph, symmetric = TRUE)
  closed <- binary + diag(nrow(binary))
  # |N(j) \ N[i]| = number of neighbours of j outside the closed neighbourhood of i
  outside <- binary %*% t(1 - closed)
  importance <- (t(outside) + outside) / 2
  za_core(binary * (importance >= threshold))
}

#' s-core index (Eidsaa & Almaas 2013) by exhaustive subsets: the largest, over the node sets that hold the
#' node, of the smallest strength inside the set
za_s_core <- function(graph) {
  weights <- graph
  diag(weights) <- 0
  subsets <- za_subsets(nrow(weights))
  floor_of <- apply(subsets, 2L, \(s) min(rowSums(weights[s, s, drop = FALSE])))
  stats::setNames(apply(subsets, 1L, \(holds) max(floor_of[holds])), rownames(graph))
}

#' Distance-weighted fragmentation of the graph without the node (Borgatti 2006):
#' 1 - sum of 1 / d over ordered pairs of the remaining nodes, over (n - 1)(n - 2)
za_fragmentation <- function(graph) {
  n <- nrow(graph)
  vapply(seq_len(n), \(v) {
    d <- za_hops(graph[-v, -v, drop = FALSE])
    off <- row(d) != col(d)
    1 - sum(1 / d[off]) / ((n - 1) * (n - 2))
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

#' k-path census (Sade 1989): simple paths of 1 to k links that the node lies on, endpoints included; an
#' undirected path is found from both ends, so its count is halved
za_kpath <- function(graph, k = 3) {
  binary <- za_binary(graph)
  n <- nrow(binary)
  extend <- function(path) {
    if (length(path) > k) return(list(path))
    following <- setdiff(which(binary[path[[length(path)]], ] > 0), path)
    c(list(path), unlist(lapply(following, \(w) extend(c(path, w))), recursive = FALSE))
  }
  paths <- Filter(\(p) length(p) >= 2L, unlist(lapply(seq_len(n), extend), recursive = FALSE))
  counts <- tabulate(unlist(paths), nbins = n)
  stats::setNames(if (isSymmetric(unname(binary))) counts / 2 else counts, rownames(graph))
}

#' Number of geodesics of length 1 to k that start at the node (geodesic k-path; Borgatti & Everett 2006)
za_geodesic_kpath <- function(graph, k = 3) {
  d <- za_hops(graph)
  sigma <- za_geodesics(graph, d)
  stats::setNames(rowSums(ifelse(d > 0 & d <= k, sigma, 0)), rownames(graph))
}

#' Node contraction (Tan et al. 2006; Wang et al. 2011, eq. 2): 1 - a(G) / a(G contracted at v), a = 1 / (n
#' times the mean distance), contracting merging v and its neighbours into one node
za_node_contraction <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  agglomeration <- \(b) {
    d <- za_hops(b)
    off <- row(d) != col(d)
    if (!any(off)) 1 else 1 / (nrow(b) * mean(d[off]))
  }
  whole <- agglomeration(binary)
  vapply(seq_len(nrow(binary)), \(v) {
    merged <- binary[v, ] > 0
    merged[[v]] <- TRUE
    rest <- which(!merged)
    tie <- as.numeric(colSums(binary[merged, rest, drop = FALSE]) > 0)
    contracted <- rbind(cbind(binary[rest, rest, drop = FALSE], tie), c(tie, 0))
    1 - whole / agglomeration(contracted)
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

#' Improved node contraction (Wang et al. 2011, eq. 4): (5/6) IMC(v) + (1/6) sum of IMC over v's links in the
#' line graph
za_node_contraction_improved <- function(graph) {
  binary <- za_binary(graph, symmetric = TRUE)
  links <- which(binary > 0 & upper.tri(binary), arr.ind = TRUE)
  incidence <- matrix(0, nrow(binary), nrow(links))
  incidence[cbind(links[, 1L], seq_len(nrow(links)))] <- 1
  incidence[cbind(links[, 2L], seq_len(nrow(links)))] <- 1
  line <- (crossprod(incidence) > 0) * 1
  diag(line) <- 0
  dimnames(line) <- list(paste0("e", seq_len(nrow(links))), paste0("e", seq_len(nrow(links))))
  5 / 6 * za_node_contraction(binary) + 1 / 6 * as.numeric(incidence %*% za_node_contraction(line))
}

# -- games and greedy selections -----------------------------------------------------------------------------

#' Shapley values of the three coverage games of Michalak et al. (2013), closed forms (Algorithms 1-3):
#' game 1: sum over u in N[v] of 1 / (1 + d_u); game 2: min(1, k / (1 + d_v)) plus, over neighbours u,
#' max(0, (d_u - k + 1) / (d_u (1 + d_u))); game 3: sum over u within `cutoff` of 1 / (1 + |within cutoff of u|)
za_shapley <- function(graph, game = 1, k = 2, cutoff = 2) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  value <- switch(game,
    as.numeric((binary + diag(nrow(binary))) %*% (1 / (1 + degree))),
    pmin(1, k / (1 + degree)) + as.numeric(binary %*% ifelse(degree > 0, pmax(0, (degree - k + 1) / (degree * (1 + degree))), 0)),
    {
      within <- (za_hops(binary) <= cutoff) * 1
      as.numeric(within %*% (1 / rowSums(within)))
    })
  stats::setNames(value, rownames(graph))
}

#' DegreeDiscountIC (Chen et al. 2009) and SingleDiscount: pick the node of largest discounted degree (lowest
#' index on a tie), discount its neighbours (dd = d - 2t - (d - t) t p, or d - t), repeat; the node picked at
#' step s scores (n - s + 1) / n
za_degree_discount <- function(graph, p = 0.01, single = FALSE) {
  binary <- za_binary(graph, symmetric = TRUE)
  degree <- rowSums(binary)
  n <- nrow(binary)
  pick <- function(chosen, picked) {
    if (length(picked) == n) return(picked)
    t <- as.numeric(binary %*% chosen)
    dd <- if (single) degree - t else degree - 2 * t - (degree - t) * t * p
    dd[chosen > 0] <- -Inf
    best <- which.max(dd)
    pick(replace(chosen, best, 1), c(picked, best))
  }
  order_picked <- pick(numeric(n), integer(0))
  stats::setNames((n - match(seq_len(n), order_picked) + 1) / n, rownames(graph))
}

#' Pairwise disconnectivity (Potapov et al. 2008): share of the ordered connected pairs that the deletion of
#' the node disconnects, pairs holding the node included
za_pairwise_disconnectivity <- function(graph) {
  pairs <- \(g) sum(is.finite(za_hops(g))) - nrow(g)
  whole <- pairs(graph)
  vapply(seq_len(nrow(graph)), \(v) (whole - pairs(graph[-v, -v, drop = FALSE])) / whole, numeric(1)) |>
    stats::setNames(rownames(graph))
}
