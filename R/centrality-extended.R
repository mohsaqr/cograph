# =============================================================================
# Extended Centrality Measures — Native Implementations
# =============================================================================
#
# All measures implemented from mathematical definitions.
# Equivalence validated against centiserve, sna, tidygraph, brainGraph,
# influenceR, and NetworkX.


# =============================================================================
# Context helpers shared by the ported measures
# =============================================================================
#
# These measures were written against igraph objects and validated against
# centiserve, sna, brainGraph, influenceR and NetworkX. They now read the
# `cg_graph` context (R/kernels-graph.R). Where the old code leaned on an
# igraph convention -- a self-loop listed twice in a neighbour list, a
# `weights = NULL` default that silently used the weight attribute -- the
# helper below reproduces that convention so the pinned values do not move.

#' Accept an igraph object where a context is expected
#'
#' The measures read a `cg_graph` context. Callers that still hold an igraph
#' object (the equivalence tests do) get it converted once, here, instead of
#' each measure re-growing an igraph code path.
#' @keywords internal
#' @noRd
.ext_context <- function(cg) {
  if (!inherits(cg, "igraph")) return(cg)
  g <- cg
  cg <- .cg_graph(g)
  # igraph keeps edges in insertion order; the context re-canonicalises them
  # row-major. Remember the permutation so a weights vector the caller built
  # in igraph's order can be re-aligned by .ext_weights().
  el <- igraph::as_edgelist(g, names = FALSE)
  if (nrow(el) == nrow(cg$edges)) {
    if (!cg$directed) el <- cbind(pmin(el[, 1L], el[, 2L]), pmax(el[, 1L], el[, 2L]))
    key_igraph <- (el[, 1L] - 1) * cg$n + el[, 2L]
    key_canon <- (cg$edges[, 1L] - 1) * cg$n + cg$edges[, 2L]
    if (!anyDuplicated(key_igraph)) cg$cache$igraph_edge_order <- match(key_canon, key_igraph)
  }
  cg
}

#' Weights in canonical edge order
#'
#' Identity for a context built from a matrix; for one built from an igraph
#' object by `.ext_context()`, permutes a caller-supplied vector from
#' igraph's edge order into the canonical order the kernels expect.
#' @keywords internal
#' @noRd
.ext_weights <- function(cg, weights) {
  perm <- cg$cache$igraph_edge_order
  if (is.null(weights) || is.null(perm) || length(weights) != length(perm)) return(weights)
  weights[perm]
}

#' Path-weight matrix, honouring an igraph-ordered weights vector
#' @keywords internal
#' @noRd
.ext_path_matrix <- function(cg, weights = NULL) {
  .cg_path_matrix(cg, .ext_weights(cg, weights))
}

#' Neighbour lists with igraph's `as_adj_list()` / `neighbors()` semantics
#'
#' Sorted by neighbour id. On a directed graph `mode = "all"` lists a
#' reciprocated dyad twice. A self-loop is listed once under `"out"` or
#' `"in"` and twice on an undirected graph. On a directed graph under
#' `"all"` the two igraph accessors disagree: `neighbors()` lists the loop
#' twice (`loops = "twice"`), `as_adj_list()` once (`loops = "once"`).
#' Use `.cg_adjlist()` when loops must be ignored altogether.
#'
#' @param cg A `cg_graph` context. @param mode One of `"all"`, `"out"`, `"in"`.
#' @param loops How a self-loop is listed under directed `"all"`.
#' @return A list of integer vectors, one per vertex.
#' @keywords internal
#' @noRd
.ext_adjlist <- function(cg, mode = "all", loops = c("twice", "once")) {
  mode <- match.arg(mode, c("all", "out", "in"))
  loops <- match.arg(loops)
  b <- unname(cg$b)
  lapply(seq_len(cg$n), function(i) {
    out <- which(b[i, ] != 0)
    if (!cg$directed) return(sort(c(out, i[b[i, i] != 0])))
    inn <- which(b[, i] != 0)
    switch(mode,
      out = out,
      `in` = inn,
      all = sort(c(out, if (identical(loops, "once")) inn[inn != i] else inn)))
  })
}

#' The weight matrix an igraph `weights = NULL` default would have used
#'
#' `igraph::betweenness()`, `strength()`, `diameter()`, `laplacian_matrix()`
#' and friends fall back to the `weight` attribute when no weights are
#' passed. The context carries that attribute as `cg$w` whenever the input
#' was weighted; an unweighted input gives the binary matrix.
#'
#' @param cg A `cg_graph` context.
#' @param weights Explicit weights in canonical edge order, or NULL.
#' @return A numeric matrix without dimnames.
#' @keywords internal
#' @noRd
.ext_default_weights <- function(cg, weights = NULL) {
  if (!is.null(weights)) return(.ext_path_matrix(cg, weights))
  if (cg$weighted) unname(cg$w) else unname(cg$b)
}

#' Edge count of the subgraph induced by a vertex set
#'
#' Counts as `igraph::ecount()` does on an induced subgraph: every arc when
#' directed, each unordered pair once when undirected, a self-loop once.
#'
#' @param b Binary adjacency matrix. @param nodes Vertex indices.
#' @param directed Whether the graph is directed.
#' @return A single number.
#' @keywords internal
#' @noRd
.ext_edge_count <- function(b, nodes, directed) {
  sub <- b[nodes, nodes, drop = FALSE]
  if (directed) sum(sub != 0) else sum(sub[upper.tri(sub, diag = TRUE)] != 0)
}

#' Bridging coefficient (Hwang et al. 2008)
#'
#' `(1 / deg(v)) / sum_{u in N(v)} 1 / deg(u)` on the undirected degree,
#' with the neighbour multiset as igraph lists it.
#'
#' @param cg A `cg_graph` context.
#' @return Numeric vector; 0 for an isolate.
#' @keywords internal
#' @noRd
.ext_bridging_coefficient <- function(cg) {
  deg <- as.numeric(.cg_degree(cg$b, cg$directed, "all"))
  adj <- .ext_adjlist(cg, "all")
  vapply(seq_len(cg$n), function(v) {
    if (deg[v] == 0) return(0)
    sum_inv_deg_nbs <- sum(1 / deg[adj[[v]]])
    if (sum_inv_deg_nbs == 0) return(0)
    (1 / deg[v]) / sum_inv_deg_nbs
  }, numeric(1))
}

#' Mean first-passage-time matrix of the simple random walk
#'
#' Shared by markov, random_walk and second_order. The walk steps along the
#' binary adjacency, a self-loop included once in the row sum, as the
#' reference implementations do. Returns `NULL` when the fundamental matrix
#' is singular; callers report that as `NA` and guard connectivity first.
#'
#' @param cg A `cg_graph` context.
#' @return An n x n numeric matrix with a zero diagonal, or `NULL`.
#' @keywords internal
#' @noRd
.ext_mfpt <- function(cg) {
  n <- cg$n
  A <- unname(cg$b)
  deg <- rowSums(A)
  deg[deg == 0] <- 1
  P <- A / deg

  # Stationary distribution
  pi_stat <- if (!cg$directed) {
    deg / sum(deg)
  } else {
    eig <- eigen(t(P))
    idx <- which.min(abs(Re(eig$values) - 1))
    v <- abs(Re(eig$vectors[, idx]))
    v / sum(v)
  }

  # Fundamental matrix: Z = (I - P + W)^-1
  W <- matrix(pi_stat, n, n, byrow = TRUE)
  Z <- tryCatch(solve(diag(n) - P + W), error = function(e) NULL)
  if (is.null(Z)) return(NULL)

  # Mean first passage time: m_ij = (Z_jj - Z_ij) / pi_j, 0 where pi_j ~ 0
  pi_row <- matrix(pi_stat, n, n, byrow = TRUE)
  mfpt <- (matrix(diag(Z), n, n, byrow = TRUE) - Z) / pi_row
  mfpt[pi_row <= 1e-15] <- 0
  diag(mfpt) <- 0
  mfpt
}

#' Weighted local reaching centrality
#'
#' Paths minimise `total / w` (a heavy edge is cheap to cross) and the score
#' averages the traversed weights along each path.
#'
#' @param cg A `cg_graph` context. @param w Weights in canonical edge order.
#' @param mode Traversal mode already resolved for direction.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.ext_reaching_weighted <- function(cg, w, mode) {
  n <- cg$n
  # m[a, b] is the weight of arc a -> b (symmetric when undirected). The
  # weight averaged along a path is looked up in path order, a -> b, the way
  # get_edge_ids(directed = TRUE) did: under "in" (or "all" across a
  # one-way dyad) the traversed arc runs the other way and contributes 0.
  m <- .cg_path_matrix(cg, w)
  inc <- .ext_incident_costs(m, cg$directed, mode, sum(w))
  vapply(seq_len(n), function(src) {
    parent <- .ext_dijkstra_parents(inc, src, n)
    reached <- which(parent > 0L)
    if (length(reached) == 0L) return(0)
    avg_ws <- vapply(reached, function(target) {
      path <- target
      # Walk the parent pointers back to the source (sequential by nature).
      while (path[1L] != src) path <- c(parent[path[1L]], path)
      steps <- length(path) - 1L
      sum(m[cbind(path[-length(path)], path[-1L])]) / steps
    }, numeric(1))
    sum(avg_ws) / (n - 1)
  }, numeric(1))
}

#' Incident arcs with traversal costs, in igraph's incidence-list order
#'
#' Neighbours ascending; under directed `"all"` the out-arc precedes the
#' in-arc to the same neighbour. Self-loops are dropped (they can never
#' shorten a path).
#'
#' @param m Weight matrix. @param directed Whether directed.
#' @param mode Traversal mode. @param total Sum of all edge weights.
#' @return A list of `list(to, cost)` per vertex, `cost = total / weight`.
#' @keywords internal
#' @noRd
.ext_incident_costs <- function(m, directed, mode, total) {
  n <- nrow(m)
  lapply(seq_len(n), function(i) {
    j <- seq_len(n)[seq_len(n) != i]
    if (!directed || identical(mode, "out")) {
      keep <- m[i, j] > 0
      return(list(to = j[keep], cost = total / m[i, j[keep]]))
    }
    if (identical(mode, "in")) {
      keep <- m[j, i] > 0
      return(list(to = j[keep], cost = total / m[j[keep], i]))
    }
    out_keep <- m[i, j] > 0
    in_keep <- m[j, i] > 0
    to <- c(j[out_keep], j[in_keep])
    cost <- c(total / m[i, j[out_keep]], total / m[j[in_keep], i])
    rank <- c(rep(0L, sum(out_keep)), rep(1L, sum(in_keep)))
    o <- order(to, rank)
    list(to = to[o], cost = cost[o])
  })
}

#' Single-source Dijkstra with igraph's tie-breaking
#'
#' Reproduces `igraph_get_shortest_paths_dijkstra()`: a binary max-heap on
#' `-distance` (igraph's `2wheap`, equal keys sift up), relaxation only on
#' a strict improvement judged by `igraph_cmp_epsilon()` at 1e-10, and
#' arcs scanned in incidence-list order. Which parent a tied vertex keeps
#' depends on all three, so all three are followed exactly.
#'
#' @param inc Incidence lists from `.ext_incident_costs()`.
#' @param src Source index. @param n Vertex count.
#' @return Integer vector of parents; 0 for the source and for unreached
#'   vertices.
#' @keywords internal
#' @noRd
.ext_dijkstra_parents <- function(inc, src, n) {
  dist <- rep(-1, n)
  parent <- integer(n)
  hdata <- numeric(0)
  hidx <- integer(0)
  hpos <- integer(n)
  swap <- function(a, b) {
    if (a == b) return(invisible(NULL))
    td <- hdata[a]; hdata[a] <<- hdata[b]; hdata[b] <<- td
    ia <- hidx[a]; ib <- hidx[b]
    hidx[a] <<- ib; hidx[b] <<- ia
    hpos[ia] <<- b; hpos[ib] <<- a
    invisible(NULL)
  }
  # Heap maintenance is a walk along one root-to-leaf path; each step
  # depends on the swap before it.
  shift_up <- function(e) {
    while (e > 1L) {
      p <- e %/% 2L
      if (hdata[e] < hdata[p]) break
      swap(e, p)
      e <- p
    }
  }
  sink <- function(h) {
    repeat {
      size <- length(hdata)
      l <- 2L * h
      r <- l + 1L
      if (l > size) break
      child <- if (r > size || hdata[l] >= hdata[r]) l else r
      if (!(hdata[h] < hdata[child])) break
      swap(h, child)
      h <- child
    }
  }
  push <- function(v, key) {
    hdata[length(hdata) + 1L] <<- key
    hidx[length(hidx) + 1L] <<- v
    hpos[v] <<- length(hdata)
    shift_up(length(hdata))
  }
  pop_max <- function() {
    v <- hidx[1L]
    size <- length(hdata)
    swap(1L, size)
    hdata <<- hdata[-size]
    hidx <<- hidx[-size]
    hpos[v] <<- 0L
    if (length(hdata)) sink(1L)
    v
  }
  modify <- function(v, key) {
    p <- hpos[v]
    hdata[p] <<- key
    sink(p)
    shift_up(p)
  }
  cmp_eps <- function(a, b) {
    if (a == b) return(0L)
    diff <- a - b
    total <- abs(a) + abs(b)
    tie <- if (a == 0 || b == 0 || total < .Machine$double.xmin) {
      abs(diff) < 1e-10 * .Machine$double.xmin
    } else {
      abs(diff) / total < 1e-10
    }
    if (tie) 0L else if (diff < 0) -1L else 1L
  }

  dist[src] <- 0
  push(src, 0)
  # Settling is sequential: each pop depends on every relaxation before it.
  while (length(hdata)) {
    u <- pop_max()
    mindist <- dist[u]
    to <- inc[[u]]$to
    cost <- inc[[u]]$cost
    for (k in seq_along(to)) {
      tto <- to[k]
      altdist <- mindist + cost[k]
      curdist <- dist[tto]
      if (curdist < 0) {
        dist[tto] <- altdist
        parent[tto] <- u
        push(tto, -altdist)
      } else if (hpos[tto] > 0L && cmp_eps(altdist, curdist) < 0L) {
        dist[tto] <- altdist
        parent[tto] <- u
        modify(tto, -altdist)
      }
    }
  }
  parent
}

#' Weighted stress by edge scan over the shortest-path DAG
#'
#' Per source, the all-pairs distances identify tight edges
#' (`d[u] + w(u, v) == d[v]` at a scale-relative tolerance floored at 1),
#' which form the predecessor DAG; the stress recurrence then runs by
#' ascending and descending distance. Kept in this form, tolerance floor
#' included, because it is what the pinned values were computed with.
#'
#' @param cg A `cg_graph` context. @param weights Path weights, canonical order.
#' @param mode Distance mode. @param is_dir Whether direction is in force.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.ext_stress_weighted <- function(cg, weights, mode, is_dir) {
  n <- cg$n
  el <- cg$edges
  u_all <- el[, 1L]
  v_all <- el[, 2L]
  w_all <- as.numeric(.ext_weights(cg, weights))

  # For undirected, symmetrize edges so the scan catches predecessors in
  # either orientation. For directed, each arc is scanned once.
  if (is_dir) {
    u_sym <- u_all; v_sym <- v_all; w_sym <- w_all
  } else {
    u_sym <- c(u_all, v_all); v_sym <- c(v_all, u_all); w_sym <- c(w_all, w_all)
  }

  d_all <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  tol_rel <- sqrt(.Machine$double.eps)
  n_seq <- seq_len(n)
  stress <- numeric(n)

  # One DAG per source; the two sweeps below are order-dependent recurrences.
  for (s in n_seq) {
    d <- d_all[s, ]
    reachable <- is.finite(d)
    d_u <- d[u_sym]
    d_v <- d[v_sym]
    lhs <- d_u + w_sym
    tight <- is.finite(d_u) & is.finite(d_v) &
      abs(lhs - d_v) <= tol_rel * pmax(abs(lhs), abs(d_v))
    pred_list <- split(u_sym[tight], factor(v_sym[tight], levels = n_seq))

    ord <- order(d)
    sigma <- numeric(n)
    sigma[s] <- 1
    for (w in ord) {
      if (!reachable[w] || w == s) next
      p <- pred_list[[w]]
      if (length(p)) sigma[w] <- sum(sigma[p])
    }

    delta <- numeric(n)
    for (w in rev(ord)) {
      if (!reachable[w] || w == s || sigma[w] == 0) next
      p <- pred_list[[w]]
      if (!length(p)) next
      factor_w <- (sigma[w] + delta[w]) / sigma[w]
      delta[p] <- delta[p] + sigma[p] * factor_w
    }

    delta[s] <- 0
    stress <- stress + delta
  }

  if (!is_dir) stress / 2 else stress
}


# =============================================================================
# Distance-based closeness variants
# =============================================================================

#' Stress centrality (sna-compatible)
#'
#' Number of shortest paths passing through each node as intermediate.
#' Uses sna convention with C-style accumulation.
#' @keywords internal
#' @noRd
calculate_stress <- function(cg, weights = NULL, directed = TRUE) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(0, n))
  is_dir <- cg$directed && directed
  weights_provided <- !is.null(weights) && !all(is.na(weights))

  mode <- if (is_dir) "out" else "all"
  if (weights_provided) return(.ext_stress_weighted(cg, weights, mode, is_dir))

  # Unweighted: Brandes (2008) BFS accumulation with the stress recurrence
  #   delta(v) += sigma(v) * (sigma(w) + delta(w)) / sigma(w)
  # which counts integer paths rather than the fractions betweenness uses.
  .cg_stress(.cg_mode_weights(unname(cg$b), mode), n, is_dir, FALSE)
}


#' Flow betweenness (sna-compatible)
#'
#' Max-flow based betweenness using igraph::max_flow. Deliberately left on
#' igraph (see R/kernels-final.R): parity needs igraph's flow decomposition.
#' @keywords internal
#' @noRd
calculate_flow_betweenness <- function(cg, weights = NULL, directed = TRUE) {
  cg <- .ext_context(cg)
  .cg_need_igraph("flow_betweenness")
  g <- .cg_igraph(cg)
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
calculate_lobby <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(integer(0))

  deg <- .cg_degree(cg$b, cg$directed, mode)
  adj <- .ext_adjlist(cg, mode)

  # Closed neighborhood: node + its neighbors
  vapply(seq_len(n), function(i) {
    as.integer(.cg_hindex(c(deg[i], deg[adj[[i]]])))
  }, integer(1))
}


#' Radiality centrality (centiserve-compatible)
#'
#' sum(diam + 1 - d(v,w)) for ALL w (including self, where d=0),
#' divided by (n - 1).
#' @keywords internal
#' @noRd
calculate_radiality <- function(cg, mode = "all", weights = NULL,
                                dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(NA_real_, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }
  # The diameter follows the graph's own direction, not `mode`, and when
  # weights are in force it is taken on the raw weight attribute (igraph's
  # `weights = NULL` default), not on the path weights.
  diam_m <- if (is.null(weights)) unname(cg$b) else .ext_default_weights(cg)
  diam <- .cg_diameter(.cg_distances(diam_m, if (cg$directed) "out" else "all"))

  .cg_radiality(dist_mat, n, diam)
}


#' Lin centrality (centiserve-compatible)
#' @keywords internal
#' @noRd
calculate_lin <- function(cg, mode = "all", weights = NULL,
                          dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(NA_real_, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }
  .cg_lin(dist_mat, n)
}


#' Decay centrality (centiserve-compatible)
#'
#' rowSums(delta^sp) — INCLUDES self (delta^0 = 1).
#' @keywords internal
#' @noRd
calculate_decay <- function(cg, mode = "all", weights = NULL,
                            decay_parameter = 0.5, dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(1, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }
  # Include self (diagonal = 0, so delta^0 = 1)
  rowSums(decay_parameter ^ dist_mat)
}


#' Residual closeness (centiserve-compatible)
#'
#' sum(1/2^d) including self = sum(2^(-d)). Self contributes 1.
#' @keywords internal
#' @noRd
calculate_residual_closeness <- function(cg, mode = "all", weights = NULL,
                                         dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(1, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }
  # 1/2^sp including self; Inf distances contribute 0
  rowSums(1 / (2^dist_mat))
}


#' Dangalchev closeness (same as residual closeness)
#' @keywords internal
#' @noRd
calculate_dangalchev <- function(cg, mode = "all", weights = NULL,
                                 dist_mat = NULL) {
  cg <- .ext_context(cg)
  calculate_residual_closeness(cg, mode = mode, weights = weights,
                               dist_mat = dist_mat)
}


#' Generalized closeness (tidygraph-compatible)
#'
#' sum(alpha^d) including self.
#' @keywords internal
#' @noRd
calculate_generalized_closeness <- function(cg, mode = "all", weights = NULL,
                                            alpha = 0.5, dist_mat = NULL) {
  cg <- .ext_context(cg)
  calculate_decay(cg, mode = mode, weights = weights, decay_parameter = alpha,
                  dist_mat = dist_mat)
}


#' Harary centrality
#'
#' sum(1/d(i,j)^2) over all j != i.
#' @keywords internal
#' @noRd
calculate_harary <- function(cg, mode = "all", weights = NULL,
                             dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(0, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }
  .cg_harary(dist_mat, n)
}


#' Average distance centrality (centiserve-compatible)
#'
#' sum(d(v,w)) / (n + 1). Note: centiserve divides by vcount+1.
#' @keywords internal
#' @noRd
calculate_average_distance <- function(cg, mode = "all", weights = NULL,
                                       dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(NA_real_, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }

  # centiserve divides by n+1 (including self which has dist 0)
  rowSums(dist_mat) / (n + 1)
}


#' Barycenter centrality (centiserve-compatible)
#'
#' 1 / sum(distances) for reachable nodes.
#' @keywords internal
#' @noRd
calculate_barycenter <- function(cg, mode = "all", weights = NULL,
                                 dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(NA_real_, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }
  .cg_barycenter(dist_mat, n)
}


#' Closeness vitality (centiserve-compatible)
#'
#' Wiener_full - Wiener_reduced. Wiener = sum of ALL sp values (not /2).
#' @keywords internal
#' @noRd
calculate_closeness_vitality <- function(cg, mode = "all", weights = NULL,
                                         dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(NA_real_, n))

  # The path weights are honoured as-is (NULL forces unweighted); the
  # reduced graphs cannot reuse dist_mat, so the kernel rebuilds them from
  # the same weight matrix with one row and column removed.
  m <- .ext_path_matrix(cg, weights)
  if (is.null(dist_mat)) dist_mat <- .cg_distances(m, mode)
  .cg_closeness_vitality(m, mode, dist_mat)
}


#' Wiener index centrality
#'
#' Sum of all shortest path distances from node i.
#' @keywords internal
#' @noRd
calculate_wiener <- function(cg, mode = "all", weights = NULL,
                             dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(0, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }
  .cg_wiener(dist_mat, n)
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
calculate_communicability <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(1)

  A <- unname(cg$b)
  eig <- eigen(A, symmetric = isSymmetric(A))
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
calculate_communicability_betweenness <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 2) return(rep(0, n))

  unname_A <- unname(cg$b)
  is_sym <- isSymmetric(unname_A)

  # Pre-computed expm: G = V exp(D) V^{-1}; for symmetric A, V^{-1} = V^T.
  # Zeroing row/col r preserves symmetry, so we can reuse symmetric-eigen
  # on A_red too. Caching is_sym saves one isSymmetric() per vertex.
  .expm_sym <- function(M, symmetric) {
    eig <- eigen(M, symmetric = symmetric)
    v <- Re(eig$vectors)
    if (symmetric) {
      v %*% (exp(Re(eig$values)) * t(v))  # tcrossprod-style scaling
    } else {
      v %*% diag(exp(Re(eig$values)), nrow = nrow(M)) %*% solve(v)
    }
  }

  G <- .expm_sym(unname_A, is_sym)

  # Pre-compute 1/G with a zero-tolerance guard; the per-vertex step below
  # collapses to a single mask+sum instead of an O(n^2) double loop per r.
  inv_G <- G
  valid_G <- G > 1e-15
  inv_G[valid_G] <- 1 / G[valid_G]
  inv_G[!valid_G] <- 0
  diag_mask <- diag(n) == 1  # rows where s == t

  cb <- vapply(seq_len(n), function(r) {
    A_red <- unname_A
    A_red[r, ] <- 0
    A_red[, r] <- 0
    G_red <- .expm_sym(A_red, is_sym)

    # ratio[s, t] = (G[s,t] - G_red[s,t]) / G[s,t], with 0 where G[s,t]=0
    ratio <- (G - G_red) * inv_G
    # Exclude diagonal (s == t), row r, col r
    ratio[diag_mask] <- 0
    ratio[r, ] <- 0
    ratio[, r] <- 0
    sum(ratio)
  }, numeric(1))

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
calculate_random_walk <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(NA_real_, n))

  if (.cg_n_components(cg$b) > 1L) {
    warning("Random walk centrality undefined for disconnected graphs",
            call. = FALSE)
    return(rep(NA_real_, n))
  }

  mfpt <- .ext_mfpt(cg)
  if (is.null(mfpt)) return(rep(NA_real_, n))

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
calculate_entropy <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  if (cg$n == 0) return(numeric(0))
  .cg_entropy(cg$b, mode)
}


#' Semi-local centrality (centiserve-compatible)
#'
#' For each neighbor u of v, for each neighbor w of u, sum the size of
#' w's 2-neighborhood. Triple-nested computation.
#' @keywords internal
#' @noRd
calculate_semilocal <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  if (cg$n == 0) return(numeric(0))
  .cg_semilocal(.ext_adjlist(cg, mode))
}


#' ClusterRank (centiserve-compatible)
#'
#' `cc[v] * sum(degree(w) + 1)` for neighbors `w`. Uses clustering coefficient
#' directly, not `10^(-cc)`.
#' @keywords internal
#' @noRd
calculate_clusterrank <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  deg <- .cg_degree(cg$b, cg$directed, mode)
  cc <- .cg_local_transitivity(cg$b, n, cg$directed)
  .cg_clusterrank(cc, .ext_adjlist(cg, mode), deg)
}


#' Bottleneck centrality (centiserve-compatible)
#'
#' For each source, compute ALL shortest paths. A node v gets +1 if it
#' appears in more than n/4 of those paths.
#' @keywords internal
#' @noRd
calculate_bottleneck <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(1L, n))
  as.integer(.cg_bottleneck(cg$b, cg$directed, n, mode))
}


#' Centroid value (centiserve-compatible)
#'
#' For each pair `(u, v)`, `gamma[u, v]` = count of nodes `w` where
#' `d(u, w) < d(v, w)`. `f[u, v] = gamma[u, v] - gamma[v, u]`.
#' `Centroid(v) = min f[v, i]` over all `i`.
#' @keywords internal
#' @noRd
calculate_centroid <- function(cg, mode = "all", weights = NULL,
                               dist_mat = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(0, n))

  if (is.null(dist_mat)) {
    dist_mat <- .cg_distances(.ext_path_matrix(cg, weights), mode)
  }
  # Include self (f[v,v]=0), matching centiserve convention
  .cg_centroid(dist_mat, n)
}


#' Maximum Neighborhood Component (centiserve-compatible)
#' @keywords internal
#' @noRd
calculate_mnc <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  if (cg$n == 0) return(integer(0))
  as.integer(.cg_mnc(cg$b, .ext_adjlist(cg, mode)))
}


#' DMNC — Density of Maximum Neighborhood Component (centiserve-compatible)
#'
#' ec / max_component_size^epsilon where ec is the edge count of the
#' largest connected component in the neighborhood subgraph.
#' Default epsilon from centiserve is the parameter (default 1.0 I think...
#' actually the centiserve default is between 1 and 2, let me check).
#' @keywords internal
#' @noRd
calculate_dmnc <- function(cg, mode = "all", epsilon = 1.7) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  # DMNC = E / N^epsilon where E = edges, N = nodes in the maximum
  # neighborhood component. Lin et al. (2008) recommend epsilon = 1.7
  # (close to 1.67 for four-community assumption). centiserve defaults
  # to 1.67. Both are valid per the original paper.
  b <- unname(cg$b)
  adj <- .ext_adjlist(cg, mode)

  vapply(adj, function(nbs) {
    if (length(nbs) == 0) return(0)
    # An induced subgraph keeps vertices in id order and drops repeats
    sub_nodes <- sort(unique(nbs))
    comps <- .cg_strong_components(b[sub_nodes, sub_nodes, drop = FALSE])
    if (length(comps) == 0) return(0)
    sizes <- lengths(comps)
    mc_size <- max(sizes)
    # Reference quirk kept on purpose: the positions of the largest
    # component index the raw neighbour list, repeats included, not the
    # deduplicated vertex set the components were computed on.
    positions <- sort(unlist(comps[sizes == mc_size], use.names = FALSE))
    mc_nodes <- unique(nbs[positions])
    ec <- .ext_edge_count(b, mc_nodes, cg$directed)
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
calculate_topological_coefficient <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  adj <- .ext_adjlist(cg, "all", loops = "once")

  vapply(seq_len(n), function(v) {
    nbs_v <- adj[[v]]
    k_v <- length(nbs_v)
    if (k_v == 0) return(0)

    # Every neighbour-of-a-neighbour other than v counts once (with
    # multiplicity); each distinct one that is also a neighbour of v counts
    # once more.
    nn <- unlist(adj[nbs_v], use.names = FALSE)
    nn <- nn[nn != v]
    com_ne_nodes <- unique(nn)
    if (length(com_ne_nodes) == 0) return(0)
    tc <- length(nn) + sum(com_ne_nodes %in% nbs_v)
    tc / (length(com_ne_nodes) * k_v)
  }, numeric(1))
}


#' Bridging centrality (betweenness * bridging coefficient)
#' @keywords internal
#' @noRd
calculate_bridging <- function(cg, weights = NULL, directed = TRUE) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  is_dir <- cg$directed && directed
  # igraph::betweenness(weights = NULL) fell back to the weight attribute;
  # .ext_default_weights() reproduces that default.
  w <- .cg_mode_weights(.ext_default_weights(cg, weights),
                        if (is_dir) "out" else "all")
  betw <- .cg_betweenness(w, n, is_dir)

  betw * .ext_bridging_coefficient(cg)
}


#' Local bridging centrality (CINNA-compatible)
#'
#' (1/degree) * bridging_coefficient
#' @keywords internal
#' @noRd
calculate_local_bridging <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  deg <- .cg_degree(cg$b, cg$directed, "all")
  ifelse(deg > 0, 1 / deg, 0) * .ext_bridging_coefficient(cg)
}


#' Effective network size (influenceR-compatible)
#'
#' Burt's effective size: degree minus redundancy.
#' @keywords internal
#' @noRd
calculate_effective_size <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  adj <- .ext_adjlist(cg, "all", loops = "once")

  vapply(seq_len(n), function(v) {
    nbs <- adj[[v]]
    k <- length(nbs)
    if (k == 0) return(0)

    shared <- vapply(nbs, function(j) length(intersect(nbs, adj[[j]])),
                     numeric(1))
    k - sum(shared) / k
  }, numeric(1))
}


#' Diversity centrality (igraph-compatible)
#'
#' Shannon entropy of edge weight distribution per node.
#' @keywords internal
#' @noRd
calculate_diversity <- function(cg, weights = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  w <- .ext_weights(cg, weights) %||% cg$weights
  if (is.null(w)) {
    # Unweighted: all edges equal weight, so diversity = 1 for deg > 1
    deg <- .cg_degree(cg$b, cg$directed, "all")
    return(ifelse(deg > 1, 1, 0))
  }

  el <- cg$edges
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
calculate_cross_clique <- function(cg) {
  cg <- .ext_context(cg)
  if (cg$n == 0) return(integer(0))
  as.integer(.cg_cross_clique(cg$b))  # ALL cliques, not just maximal ones
}


#' Markov centrality (centiserve-compatible)
#'
#' Inverse of column means of mean first passage time matrix.
#' @keywords internal
#' @noRd
calculate_markov <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(NA_real_, n))

  if (.cg_n_components(cg$b) > 1L) {
    warning("Markov centrality undefined for disconnected graphs",
            call. = FALSE)
    return(rep(NA_real_, n))
  }

  mfpt <- .ext_mfpt(cg)
  if (is.null(mfpt)) return(rep(NA_real_, n))

  # centiserve: 1 / column means
  col_means <- colMeans(mfpt)
  ifelse(col_means > 0, 1 / col_means, NA_real_)
}


#' Integration centrality (tidygraph-compatible)
#'
#' For each node, compute distances, then 1 - (d-1)/max(d), sum over all j.
#' @keywords internal
#' @noRd
calculate_integration <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  as.numeric(.cg_integration(cg$b, mode))
}


#' Expected centrality (based on degree)
#'
#' Sum of neighbor degrees. Simple but effective influence proxy.
#' @keywords internal
#' @noRd
calculate_expected <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  deg <- .cg_degree(cg$b, cg$directed, mode)
  adj <- unname(cg$b)

  if (cg$directed && identical(mode, "in")) {
    adj <- t(adj)
  } else if (cg$directed && identical(mode, "all")) {
    adj <- ((adj + t(adj)) != 0) * 1
  }

  as.numeric(adj %*% deg)
}


#' Gil-Schmidt power index (sna-compatible)
#'
#' sum(1/d(v,w)) / (n-1) for all reachable w.
#' @keywords internal
#' @noRd
calculate_gilschmidt <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(0, n))
  .cg_gilschmidt(.cg_hop_distances(cg, mode), n)
}


#' SALSA centrality (directed only)
#' @keywords internal
#' @noRd
calculate_salsa <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (!cg$directed) {
    warning("SALSA requires a directed graph; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  A <- unname(cg$b)
  out_deg <- rowSums(A)
  in_deg <- colSums(A)

  A_row <- A / ifelse(out_deg > 0, out_deg, 1)
  A_col <- t(t(A) / ifelse(in_deg > 0, in_deg, 1))

  Auth_mat <- crossprod(A_col, A_row)
  eig <- eigen(t(Auth_mat))
  idx <- which.min(abs(Re(eig$values) - 1))
  auth <- abs(Re(eig$vectors[, idx]))
  auth / max(auth)
}


#' LeaderRank (directed only)
#' @keywords internal
#' @noRd
calculate_leaderrank <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (!cg$directed) {
    warning("LeaderRank requires a directed graph; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }
  # Ground node joined to every vertex in both directions, then a pure
  # random walk (no damping); the ground score is shared out at the end.
  .cg_leaderrank(cg$b, directed = TRUE)
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
#' @param cg A `cg_graph` context
#' @param mode "all", "in", or "out" for directed graphs
#' @return Numeric vector of LAC values
#' @references
#' Li, M., Wang, J., Chen, X., Wang, H., & Pan, Y. (2011). A local average
#' connectivity-based method for identifying essential proteins from the network
#' level. Computational Biology and Chemistry, 35(3), 143-150.
#' @keywords internal
#' @noRd
calculate_lac <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  b <- unname(cg$b)
  adj <- .ext_adjlist(cg, mode, loops = "once")

  vapply(adj, function(nbs) {
    k <- length(nbs)
    if (k == 0) return(0)

    # Subgraph C_v induced by neighbors of v; local connectivity is each
    # neighbour's degree within C_v
    nodes <- unique(nbs)
    local_deg <- .cg_degree(b[nodes, nodes, drop = FALSE], cg$directed, mode)

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
calculate_participation <- function(cg, membership = NULL, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (is.null(membership)) {
    warning("participation requires membership; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }
  stopifnot(length(membership) == n)

  .cg_participation(.ext_adjlist(cg, mode),
                    .cg_degree(cg$b, cg$directed, mode), membership)
}


#' Within-module degree z-score (brainGraph-compatible)
#' @keywords internal
#' @noRd
calculate_within_module_z <- function(cg, membership = NULL, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (is.null(membership)) {
    warning("within_module_z requires membership; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }
  stopifnot(length(membership) == n)

  # brainGraph convention: NaN where a module's within-degree has no spread
  .cg_within_module_z(.ext_adjlist(cg, mode), membership)
}


#' Gateway coefficient (brainGraph-compatible)
#' @keywords internal
#' @noRd
calculate_gateway <- function(cg, membership = NULL, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (is.null(membership)) {
    warning("gateway requires membership; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }
  stopifnot(length(membership) == n)

  .cg_gateway(unname(cg$b), membership)
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
#' @return Numeric scalar in \eqn{[0, 1]}
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

  cg <- .cg_graph(x, directed = directed)
  n <- cg$n
  is_dir <- cg$directed
  if (n <= 2) return(0)

  # A weighted input carries its weights into betweenness, closeness and
  # eigenvector centrality (igraph's `weights = NULL` default); degree
  # ignores them.
  w <- .ext_default_weights(cg)

  switch(measure,
    "degree" = {
      scores <- .cg_degree(cg$b, is_dir, mode)
      theo_max <- if (is_dir) (n - 1)^2 else (n - 1) * (n - 2)
      .freeman_centralization(scores, theo_max)
    },
    "betweenness" = {
      scores <- .cg_betweenness(.cg_mode_weights(w, if (is_dir) "out" else "all"),
                                n, is_dir)
      theo_max <- if (is_dir) (n - 1)^2 * (n - 2) else (n - 1)^2 * (n - 2) / 2
      .freeman_centralization(scores, theo_max)
    },
    "closeness" = {
      # Normalized closeness: reachable vertices over their summed distance
      d <- .cg_distances(w, mode)
      ok <- .cg_offdiag(d) & is.finite(d)
      reach <- rowSums(ok)
      total <- rowSums(ifelse(ok, d, 0))
      scores <- ifelse(reach > 0, reach / total, NaN)
      theo_max <- (n - 2) * (n - 1) / (2 * n - 3)
      .freeman_centralization(scores, theo_max)
    },
    "eigenvector" = {
      scores <- .cg_eigenvector(w, n)
      theo_max <- n - 1
      .freeman_centralization(scores, theo_max)
    }
  )
}


# =============================================================================
# Batch 2: Zoo of Centralities measures
# =============================================================================

#' Onion decomposition (Hébert-Dufresne et al. 2016)
#'
#' Refined k-shell that assigns nodes to layers within each shell.
#' Layer 1 = outermost (removed first), higher = more central.
#' @keywords internal
#' @noRd
calculate_onion <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(integer(0))
  if (n == 1) return(1L)

  # Onion peeling: within each k-shell, iteratively remove nodes whose
  # degree equals k. After removal, degrees drop and more nodes may reach
  # k -- those form the next layer within the shell.
  as.integer(.cg_onion(cg$b, cg$directed))
}


#' Second-order centrality (Kermarrec et al. 2011)
#'
#' Standard deviation of return times in a random walk. Low values indicate
#' central nodes with regular return times; high values indicate peripheral.
#' Requires a connected graph.
#' @keywords internal
#' @noRd
calculate_second_order <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(NA_real_, n))

  if (.cg_n_components(cg$b) > 1L) {
    warning("second_order requires a connected graph; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  mfpt <- .ext_mfpt(cg)
  if (is.null(mfpt)) return(rep(NA_real_, n))

  # Mean return time for node j = m_jj = 1/pi_j
  # Second-order centrality = SD of return times from all other nodes
  # Following Kermarrec: for each node j, compute SD of {m_ij} over all i != j
  vapply(seq_len(n), function(j) {
    times <- mfpt[-j, j]
    times <- times[times > 0]
    if (length(times) < 2) return(NA_real_)
    stats::sd(times)
  }, numeric(1))
}


#' Collective influence (Morone & Makse 2015)
#'
#' Product of (degree - 1) and sum of (degree - 1) on the boundary
#' of the ball of radius l around the node. Identifies optimal percolation nodes.
#' @keywords internal
#' @noRd
calculate_collective_influence <- function(cg, mode = "all", l = 2L) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  deg <- .cg_degree(cg$b, cg$directed, mode)
  sp <- .cg_hop_distances(cg, mode)

  # Boundary of the ball: nodes at exact distance l
  boundary <- (sp == l) * 1
  as.numeric((deg - 1) * (boundary %*% (deg - 1)))
}


#' Local H-index (Lü et al. 2016)
#'
#' Recursive h-index: h-index computed from the h-indices of neighbors
#' rather than from degrees. Iterates until convergence.
#' @keywords internal
#' @noRd
calculate_local_hindex <- function(cg, mode = "all", max_iter = 100L) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(integer(0))

  adj <- .ext_adjlist(cg, mode, loops = "once")

  # Initialize with degree (h^(0) = degree)
  h <- as.numeric(.cg_degree(cg$b, cg$directed, mode))

  # Fixed-point iteration: each pass is a function of the previous one.
  for (iter in seq_len(max_iter)) {
    h_new <- vapply(adj, function(nbs) {
      if (length(nbs) == 0) 0 else .cg_hindex(h[nbs])
    }, numeric(1))
    if (identical(h_new, h)) break
    h <- h_new
  }

  as.integer(h)
}


#' Infection number (Bauer & Lizier 2012)
#'
#' Expected number of infections from a node as source, approximated using
#' self-avoiding walks (SAWs). Uses SIR model with infection probability beta
#' and removal probability mu.
#' @keywords internal
#' @noRd
calculate_infection <- function(cg, beta = 0.8, mu = 0, max_length = 6L) {
  cg <- .ext_context(cg)
  if (cg$n == 0) return(numeric(0))
  .cg_infection(cg$b, cg$directed, beta = beta, mu = mu,
                max_length = max_length)
}


#' Expected influence (Robinaugh, Millner & McNally 2016)
#'
#' Signed-sum centrality for networks with positive *and* negative edges.
#' Strength takes `|w|` and conflates a node with strong offsetting edges
#' with a genuinely central node; expected influence keeps the sign.
#'
#' Formulas (Robinaugh et al. 2016):
#'   EI1(i) = sum_j W\[i, j\]
#'   EI2(i) = EI1(i) + sum_j W\[i, j\] * EI1(j)
#'
#' `mode` follows the rest of the centrality family: "out" (default) uses
#' row sums (outgoing weights from i), "in" uses column sums, "all" sums
#' both for directed graphs. Undirected graphs ignore `mode` since W is
#' symmetric.
#'
#' @param cg A `cg_graph` context.
#' @param weights Optional numeric vector of edge weights (positive or
#'   negative). If `NULL`, uses the graph's own weights when present, else falls
#'   back to unweighted (edges weighted 1), in which case EI1 reduces to
#'   signed degree.
#' @param step Integer, 1 or 2. Whether to return EI1 or EI2. Default 1.
#' @param mode One of "out", "in", "all". Default "out" (ignored for
#'   undirected graphs).
#' @keywords internal
#' @noRd
calculate_expected_influence <- function(cg, weights = NULL, step = 1L,
                                         mode = c("out", "in", "all")) {
  cg <- .ext_context(cg)
  mode <- match.arg(mode)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  # Signed weight matrix: negative edges stay negative. NULL weights fall
  # back to the graph's own weights, or to 1 when it has none.
  W <- .ext_path_matrix(cg, weights %||% cg$weights)
  .cg_expected_influence(W, mode = mode, step = step)
}


#' Non-backtracking centrality (Martin et al. 2014)
#'
#' Based on the leading eigenvector of the non-backtracking (Hashimoto) matrix.
#' Avoids localization issues of eigenvector centrality on sparse networks.
#' @keywords internal
#' @noRd
calculate_nonbacktracking <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(1)

  el <- cg$edges
  # For undirected: each edge becomes 2 directed edges
  if (!cg$directed) el <- rbind(el, el[, 2:1, drop = FALSE])
  m <- nrow(el)

  # Non-backtracking matrix B: B[(i->j), (k->l)] = 1 if j==k and i!=l
  # This is a 2m x 2m matrix for undirected graphs
  # For efficiency, use the Ihara determinant relationship:
  # Leading eigenvalue of B relates to adjacency spectrum
  # Node centrality = sum of eigenvector components over edges leaving node

  if (m > 5000) {
    # For large graphs, use the reduced 2n x 2n matrix formulation
    A <- unname(cg$b)
    D <- diag(as.numeric(.cg_degree(cg$b, cg$directed, "all")), n, n)
    I_n <- diag(n)

    # Block matrix: [[A, I-D], [I, 0]]
    top <- cbind(A, I_n - D)
    bot <- cbind(I_n, matrix(0, n, n))
    B_red <- rbind(top, bot)

    eig <- eigen(B_red)
    # Leading eigenvalue
    idx <- which.max(Re(eig$values))
    v <- Re(eig$vectors[, idx])
    # Node centrality from first n components
    result <- abs(v[seq_len(n)])
  } else {
    # Direct B matrix construction: edge a = (i->j), edge b = (k->l)
    B <- outer(seq_len(m), seq_len(m), function(a, b) {
      as.numeric(el[a, 2] == el[b, 1] & el[a, 1] != el[b, 2])
    })

    eig <- eigen(B)
    idx <- which.max(Re(eig$values))
    v <- Re(eig$vectors[, idx])

    # Aggregate edge centrality to node centrality
    # Node v = sum of eigenvector components for edges leaving v
    by_source <- tapply(abs(v), factor(el[, 1], levels = seq_len(n)), sum)
    result <- as.numeric(ifelse(is.na(by_source), 0, by_source))
  }

  # Normalize
  max_val <- max(result)
  if (max_val > 0) result <- result / max_val
  result
}


#' Trophic level centrality
#'
#' Trophic level of each node in a directed network, measuring position
#' in the flow hierarchy. Basal nodes (sources) have level 1.
#' Requires a directed graph.
#' @keywords internal
#' @noRd
calculate_trophic_level <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (!cg$directed) {
    warning("trophic_level requires a directed graph; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  # Trophic level s_j = 1 + (1/k_j^in) * sum_{i->j} s_i
  # Solve: (I - W) s = 1, where W_ji = A_ij / k_j^in
  A <- unname(cg$b)
  in_deg <- colSums(A)
  in_deg[in_deg == 0] <- 1  # basal nodes

  W <- t(t(A) / in_deg)  # W_ji = A_ij / in_deg_j
  I_n <- diag(n)

  s <- tryCatch(
    solve(I_n - t(W), rep(1, n)),
    error = function(e) rep(NA_real_, n)
  )

  s
}


#' H-index strength (extended h-index with weighted edges)
#'
#' Like the lobby index but uses strength (weighted degree) of closed
#' neighborhood members instead of unweighted degree.
#' @keywords internal
#' @noRd
calculate_hindex_strength <- function(cg, mode = "all") {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))

  # Strength on the graph's own weights (igraph's default), degree when the
  # input carried none
  str <- .cg_strength(.ext_default_weights(cg), cg$directed, mode)
  as.integer(.cg_hindex_strength(.ext_adjlist(cg, mode), str))
}


#' Spanning tree centrality
#'
#' Based on the number of spanning trees that include each node.
#' Uses the matrix tree theorem (Kirchhoff). For connected graphs,
#' related to the diagonal of the Laplacian pseudoinverse.
#' @keywords internal
#' @noRd
calculate_spanning_tree <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n <= 1) return(rep(1, n))

  if (.cg_n_components(cg$b) > 1L) {
    warning("spanning_tree requires a connected graph; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  # Weighted Laplacian when the input carried weights (igraph's default)
  L <- .cg_laplacian_matrix(.ext_default_weights(cg), cg$directed)

  # Moore-Penrose pseudoinverse of Laplacian: L+ = (L + J/n)^{-1} - J/n
  J <- matrix(1, n, n)
  L_inv <- tryCatch(solve(L + J / n), error = function(e) NULL)
  if (is.null(L_inv)) return(rep(NA_real_, n))
  L_pinv <- L_inv - J / n

  # Node spanning tree centrality = 1 / L+_ii
  # Lower L+_ii = more central (less effective resistance)
  diag_vals <- diag(L_pinv)
  ifelse(diag_vals > 1e-15, 1 / diag_vals, 0)
}


# =============================================================================
# Batch 3: Classical measures with reference-package validation
# =============================================================================
#
# Each measure has an external reference implementation used for equivalence
# tests. Implementations match the references' exact LAPACK call sequences so
# results are bit-exact identical (verified across diverse graph topologies).


#' Katz centrality (Katz 1953)
#'
#' C_Katz = (I - alpha * A^T)^{-1} * 1
#'
#' Mathematically identical to Bonacich alpha centrality with a uniform
#' exogenous vector of ones. Implementation mirrors centiserve::katzcent's
#' exact construction so results are bit-exact identical; also matches
#' igraph::alpha_centrality(exo=1) and networkx.katz_centrality_numpy at
#' floating-point precision.
#'
#' @keywords internal
#' @noRd
calculate_katz <- function(cg, weights = NULL, alpha = 0.1) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  # Match centiserve::katzcent's exact construction so the result is bit-exact
  # identical: take dense adjacency, compute (I - alpha A^T)^{-1}, then
  # multiply by all-ones. NULL weights fall back to the graph's own weights,
  # or to the binary adjacency when it has none.
  A <- .ext_path_matrix(cg, weights %||% cg$weights)

  # Katz converges only for alpha < 1 / rho(A). With exo = 1 every term of
  # the series is non-negative, so a value below 1 is proof that it did not:
  # a free divergence check. The spectral radius is computed only to name the
  # valid bound in the warning, never on the happy path.
  .cg_katz_check <- function(v) {
    if (!any(is.finite(v)) || !any(v < 1 - 1e-8)) return(invisible(NULL))
    rho <- tryCatch(max(abs(eigen(A, only.values = TRUE)$values)),
                    error = function(e) NA_real_)
    bound <- if (is.finite(rho) && rho > 0) sprintf("%.4g", 1 / rho) else "1 / rho(A)"
    warning(warningCondition(
      sprintf(paste0("katz: alpha = %g does not converge on this graph ",
                     "(needs alpha < %s); the values are not Katz scores. ",
                     "Lower `katz_alpha`."), alpha, bound),
      class = "cograph_katz_diverged", call = NULL))
    invisible(NULL)
  }

  res <- tryCatch(
    solve(diag(x = 1, nrow = n) - (alpha * t(A))) %*% matrix(1, nrow = n, ncol = 1),
    error = function(e) {
      warning("katz: linear solve failed (", conditionMessage(e),
              "); returning NA", call. = FALSE)
      matrix(NA_real_, n, 1)
    }
  )
  out <- as.numeric(res[, 1])
  .cg_katz_check(out)
  out
}


#' Hubbell centrality (Hubbell 1965)
#'
#' C_Hubbell = (I - w * W)^{-1} * 1
#'
#' where W is the (weighted) adjacency matrix and w is an attenuation factor.
#' Requires all eigenvalues of (w * W) to be < 1 for solvability. Matches
#' centiserve::hubbell exactly when the same weightfactor is used.
#'
#' @keywords internal
#' @noRd
calculate_hubbell <- function(cg, weights = NULL, weightfactor = 0.5) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (!is.numeric(weightfactor) || length(weightfactor) != 1L || weightfactor <= 0) {
    stop("hubbell: weightfactor must be a positive scalar", call. = FALSE)
  }

  # Use explicit weights if given, else the graph's own weights, else unweighted.
  W <- .ext_path_matrix(cg, weights %||% cg$weights)

  scaledW <- W * weightfactor
  # Solvability: largest eigenvalue of scaledW must be strictly < 1 for
  # (I - scaledW) to be nonsingular. We use a small buffer to catch
  # eigenvalues that land exactly on the unit boundary (e.g. K3 at wf=0.5).
  ev <- tryCatch(eigen(scaledW, only.values = TRUE)$values,
                 error = function(e) NULL)
  if (is.null(ev) || any(Re(ev) >= 1 - 1e-10)) {
    warning("hubbell: not solvable for this graph at weightfactor=",
            format(weightfactor, digits = 4),
            " (spectral radius >= 1); returning NA",
            call. = FALSE)
    return(rep(NA_real_, n))
  }

  # Match centiserve::hubbell's exact LAPACK call path so the result is
  # bit-exact identical: compute the full matrix inverse, then multiply by
  # the all-ones vector. (solve(M, b) is faster but routes through a
  # different LAPACK call and produces ULP-level rounding differences.)
  res <- tryCatch(
    solve(diag(x = 1, nrow = n) - scaledW) %*% matrix(1, nrow = n, ncol = 1),
    error = function(e) {
      warning("hubbell: linear solve failed (", conditionMessage(e),
              "); returning NA", call. = FALSE)
      matrix(NA_real_, n, 1)
    }
  )
  as.numeric(res[, 1])
}


#' Information centrality (Stephenson & Zelen 1989)
#'
#' Information centrality expresses a node's importance in terms of the
#' "information" contained in all paths (not only shortest) passing through
#' it. Defined via the inverse of a Laplacian-like matrix:
#'   A_ij = 1 if i != j and edge absent, 1 - m_ij if edge present
#'   diag(A) = 1 + degree_i
#'   C   = A^{-1}
#'   Tr  = trace(C), R_i = sum of row i of C
#'   IC_i = 1 / (C_ii + (Tr - 2 R_i) / n)
#'
#' Matches sna::infocent exactly on connected undirected graphs.
#' Returns 0 for isolated nodes.
#'
#' @keywords internal
#' @noRd
calculate_information <- function(cg, weights = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  # Symmetrised inside the kernel (Stephenson-Zelen is undirected); the
  # construction mirrors sna::infocent so the result is bit-exact.
  .cg_information(.ext_path_matrix(cg, weights), weighted = !is.null(weights))
}


#' Pairwise Disconnectivity (Potapov, Voss, et al. 2008)
#'
#' For directed graphs: fraction of ordered reachable pairs that become
#' unreachable when node v is removed.
#'
#'   PD(v) = (|P(G)| - |P(G - v)|) / |P(G)|
#'
#' where P(G) is the number of ordered (s,t) pairs with s != t and a directed
#' path from s to t. Matches centiserve::pairwisedis exactly.
#'
#' @keywords internal
#' @noRd
calculate_pairwisedis <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (!cg$directed) {
    warning("pairwisedis requires a directed graph; returning NA",
            call. = FALSE)
    return(rep(NA_real_, n))
  }
  if (n == 1) return(0)

  # Ordered reachable pairs (s != t) on hop distances, before and after
  # deleting each vertex (matches centiserve, which uses w = NA).
  .cg_pairwisedis(cg$b, directed = TRUE)
}


#' Local Reaching Centrality (Mones, Vicsek, Vicsek 2012)
#'
#' Unweighted directed:  LRC(v) = |reachable from v| / (N - 1)
#' Unweighted undirected: LRC(v) = sum_{u != v} (1/d(v,u)) / (N-1)
#'   (equivalent to igraph::harmonic_centrality with normalized = TRUE)
#'
#' Matches networkx.local_reaching_centrality exactly for unweighted graphs.
#' For the weighted branch (edge weights interpreted as strengths), NetworkX
#' uses the average edge weight along each shortest path with distances
#' computed as total_weight / edge_weight; we implement that variant.
#'
#' @keywords internal
#' @noRd
calculate_reaching_local <- function(cg, mode = "all", weights = NULL) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (n == 1) return(0)

  directed <- cg$directed
  dir_mode <- if (!directed) "all" else mode

  # "Effectively unweighted" = no weights arg and either no weight attr
  # or all weights are exactly 1 (cograph parses binary matrices as weighted
  # graphs with w=1, so we must treat those as unweighted).
  edge_w <- if (!is.null(weights)) as.numeric(.ext_weights(cg, weights)) else cg$weights
  unweighted <- is.null(edge_w) || all(edge_w == 1)

  # Directed unweighted: simple proportion of reachable nodes (paper + NetworkX)
  if (directed && unweighted) {
    d <- .cg_hop_distances(cg, dir_mode)
    return(rowSums(is.finite(d) & d > 0) / (n - 1))
  }

  # Undirected unweighted: normalized harmonic (== NetworkX LRC)
  if (unweighted) {
    return(.cg_harmonic(.cg_hop_distances(cg, "all"), n) / (n - 1))
  }

  # Weighted branch: NetworkX uses "average edge weight on shortest path"
  # where shortest path is computed with distances = total_weight / edge_weight
  # (higher weight => shorter path).
  w <- edge_w
  if (any(w < 0)) {
    stop("reaching_local: edge weights must be non-negative", call. = FALSE)
  }
  if (sum(w) <= 0) {
    return(rep(0, n))
  }

  .ext_reaching_weighted(cg, w, dir_mode)
}


#' Domain Prestige (sna::prestige, cmode = "domain")
#'
#' For each node v, the number of OTHER nodes that can reach v via a directed
#' path:
#'   domain(v) = |{u != v : u ->* v}|
#'
#' Classical directed-graph prestige measure (Wasserman & Faust 1994;
#' sna::prestige). Matches sna::prestige(cmode = "domain") bit-exact.
#' Directed-only; returns NA with a warning on undirected input.
#'
#' @keywords internal
#' @noRd
calculate_prestige_domain <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (!cg$directed) {
    warning("prestige_domain requires a directed graph; returning NA",
            call. = FALSE)
    return(rep(NA_real_, n))
  }
  if (n == 1) return(0)

  # Column j of the out-distance matrix holds the distances from every
  # source to j; a finite entry means the source reaches j, minus self.
  .cg_prestige_domain(cg$b, proximity = FALSE, directed = TRUE)
}


#' Domain Proximity Prestige (sna::prestige, cmode = "domain.proximity")
#'
#' Distance-weighted variant of domain prestige. For each node v:
#'   PD(v) = R_v^2 / (D_v * (n - 1))
#' where R_v = number of OTHER nodes that reach v, and D_v = sum of geodesic
#' distances from those reachers to v. Returns 0 when v is unreachable.
#'
#' Matches `sna::prestige(cmode = "domain.proximity")` bit-exact on strongly
#' connected directed graphs. On graphs with any unreachable pair, sna has a
#' known bug: its formula does `(counts > 0) * gdist` element-wise and then
#' sums, but `FALSE * Inf = NaN` in IEEE 754, so the entire denominator becomes
#' `NaN` and sna zeros every node via `p[is.nan(p)] <- 0`. cograph's
#' implementation uses `is.finite()` masking before summing and produces the
#' mathematically correct values on any directed graph.
#'
#' @keywords internal
#' @noRd
calculate_prestige_domain_proximity <- function(cg) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(numeric(0))
  if (!cg$directed) {
    warning("prestige_domain_proximity requires a directed graph; returning NA",
            call. = FALSE)
    return(rep(NA_real_, n))
  }
  if (n == 1) return(0)

  .cg_prestige_domain(cg$b, proximity = TRUE, directed = TRUE)
}


# =============================================================================
# Gould-Fernandez brokerage (Gould & Fernandez 1989)
# =============================================================================
#
# For a directed graph partitioned into groups, each node v is counted as a
# "broker" for every OPEN 2-path a -> v -> c (a != c, no direct edge a -> c).
# The path is classified into one of 5 roles based on the group memberships
# of a, v, c:
#
#   w_I  Coordinator   : all three in v's group           (A -> A -> A)
#   w_O  Itinerant     : a, c same group, v different     (A -> B -> A)
#   b_IO Representative: a, v same group, c different     (A -> A -> B)
#   b_OI Gatekeeper    : v, c same group, a different     (A -> B -> B)
#   b_O  Liaison       : all three in different groups    (A -> B -> C)
#
# Matches sna::brokerage$raw.nli bit-exact (verified across 20 random
# directed graphs). sna's actual counting happens in C via .C("brokerage_R");
# the rule "open 2-paths only" (exclude closed triangles where a -> c exists)
# was derived empirically by working backward from sna's output.


#' Gould-Fernandez brokerage (single role count)
#'
#' Counts open directed 2-paths a -> v -> c where v is the broker, the path
#' is classified by the group memberships of (a, v, c), and only the role
#' matching the requested type is counted. Bit-exact match against
#' sna::brokerage$raw.nli for the corresponding column.
#'
#' @param cg A `cg_graph` context (directed).
#' @param membership Integer or character vector of group assignments, length
#'   equal to the node count.
#' @param role One of "coordinator" (w_I), "itinerant" (w_O),
#'   "representative" (b_IO), "gatekeeper" (b_OI), "liaison" (b_O).
#' @return Integer vector, one entry per node.
#' @keywords internal
#' @noRd
calculate_brokerage <- function(cg, membership, role) {
  cg <- .ext_context(cg)
  n <- cg$n
  if (n == 0) return(integer(0))
  if (is.null(membership)) {
    warning("brokerage requires membership; returning NA", call. = FALSE)
    return(rep(NA_integer_, n))
  }
  if (length(membership) != n) {
    stop(sprintf("membership length (%d) must equal number of nodes (%d)",
                 length(membership), n), call. = FALSE)
  }
  if (!cg$directed) {
    warning("brokerage requires a directed graph; returning NA",
            call. = FALSE)
    return(rep(NA_integer_, n))
  }

  # Presence only (multi-edges collapse), and a node is never its own alter
  b <- unname(cg$b)
  diag(b) <- 0
  cl <- as.integer(as.factor(membership))
  as.integer(.cg_brokerage(b, cl, role, directed = TRUE))
}


# =============================================================================
# Shared helper
# =============================================================================

#' Build incoming edge list for Brandes-style algorithms
#'
#' Returns a length-n list where element w is NULL or a matrix with columns
#' (predecessor, edge_weight). Uses split() to group by target in one pass
#' rather than growing each matrix with rbind inside a loop (O(m) vs O(m^2)).
#' @keywords internal
#' @noRd
.build_incoming <- function(el, edge_w, n, directed) {
  if (directed) {
    target <- el[, 2]
    source <- el[, 1]
    weight <- edge_w
  } else {
    target <- c(el[, 2], el[, 1])
    source <- c(el[, 1], el[, 2])
    weight <- c(edge_w, edge_w)
  }
  idx_by_target <- split(seq_along(target),
                         factor(target, levels = seq_len(n)))
  incoming <- vector("list", n)
  for (w in seq_len(n)) {
    idx <- idx_by_target[[w]]
    if (length(idx) > 0) {
      incoming[[w]] <- matrix(c(source[idx], weight[idx]),
                              ncol = 2, nrow = length(idx))
    }
  }
  incoming
}
