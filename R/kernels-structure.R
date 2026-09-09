# ===========================================================================
# Structural kernels: cut vertices, bridges, ego neighbourhoods,
# shortest-path counts and edge betweenness, dependency-free
# ===========================================================================
# These serve the wrangling verbs (filter_nodes, select_nodes, select_edges)
# and the group-centrality family. Every kernel takes dense matrices; the
# only igraph-aware code is `.cg_as_context()` / `.cg_edge_rows()`, which read
# an igraph object handed in by a caller that has not been ported yet.

#' Coerce a caller's graph to a native context
#'
#' The wrangling verbs still hand their helpers the igraph object built by
#' `to_igraph()`. Until those call sites pass a context, accept either.
#' @param g A `cg_graph`, or any input `.cg_graph()` understands.
#' @return A `cg_graph` context.
#' @keywords internal
#' @noRd
.cg_as_context <- function(g) {
  if (inherits(g, "cg_graph")) return(g)
  .cg_graph(g)
}

#' Edge endpoints in the caller's edge order
#'
#' igraph edge ids follow the order the edges were added, which for a
#' `cograph_network` is the row order of `get_edges()`. A context carries the
#' canonical (row-major) order instead. Callers that index a result per edge
#' need the order the caller sees.
#' @param g A `cg_graph`, igraph object, or `cograph_network`.
#' @return Integer matrix with two columns (`from`, `to`).
#' @keywords internal
#' @noRd
.cg_edge_rows <- function(g) {
  if (inherits(g, "cg_graph")) return(g$edges)
  if (inherits(g, "cograph_network")) {
    e <- get_edges(g)
    return(matrix(as.integer(c(e$from, e$to)), ncol = 2L))
  }
  if (inherits(g, "igraph")) {
    if (!requireNamespace("igraph", quietly = TRUE)) { # nocov start
      stop("Package 'igraph' is required for igraph input", call. = FALSE)
    } # nocov end
    el <- igraph::as_edgelist(g, names = FALSE)
    return(matrix(as.integer(el), ncol = 2L))
  }
  .cg_graph(g)$edges
}

#' Undirected simple support of an adjacency matrix
#' @param b Binary (or weight) matrix.
#' @return 0/1 symmetric matrix with a zero diagonal.
#' @keywords internal
#' @noRd
.cg_support <- function(b) {
  u <- ((b != 0) | (t(b) != 0)) * 1
  diag(u) <- 0
  u
}

#' NA vector plus the classed negative-weight warning
#'
#' Path-based measures are undefined on negative weights. The wrangling verbs
#' report that as `NA` with a warning the caller can catch by class, instead
#' of an igraph error text or a silently swallowed condition.
#' @param n Vertex (or edge) count. @param what Measure name for the message.
#' @return Numeric vector of `NA`, length `n`.
#' @keywords internal
#' @noRd
.cg_na_negative <- function(n, what) {
  warning(warningCondition(
    sprintf("%s cannot be computed with negative weights. Returning NA.", what),
    class = "cograph_negative_weights"))
  rep(NA_real_, n)
}

# ---------------------------------------------------------------------------
# Tarjan low-link: articulation points and bridges
# ---------------------------------------------------------------------------

#' Tarjan's low-link DFS with an explicit stack
#'
#' One depth-first traversal of the undirected simple support yields both
#' cut vertices and bridges (Tarjan 1972; Hopcroft & Tarjan 1973). Recursion
#' is avoided so the kernel survives graphs deeper than R's call stack.
#'
#' @param b Binary adjacency matrix; direction and loops are ignored.
#' @return A list with `articulation` (sorted integer vertex indices) and
#'   `bridges` (integer matrix, two columns, one row per bridge of the
#'   simple support, tree-parent first).
#' @references Tarjan, R. (1972). Depth-first search and linear graph
#'   algorithms. *SIAM Journal on Computing*, 1(2), 146-160.
#' @keywords internal
#' @noRd
.cg_tarjan <- function(b) {
  n <- nrow(b)
  empty <- list(articulation = integer(0), bridges = matrix(integer(0), 0L, 2L))
  if (is.null(n) || n == 0L) return(empty)
  u <- .cg_support(b)
  adj <- lapply(seq_len(n), function(i) which(u[i, ] != 0))
  disc <- integer(n)           # discovery time; 0 = not yet visited
  low <- integer(n)
  parent <- integer(n)         # 0 = DFS root
  ptr <- rep(1L, n)            # next neighbour to examine per vertex
  is_art <- logical(n)
  bridge_from <- integer(n)    # a forest has at most n - 1 tree edges
  bridge_to <- integer(n)
  n_bridges <- 0L
  stack <- integer(n)
  clock <- 0L
  # The DFS is inherently sequential: every low-link value depends on the
  # discovery order of the vertices visited before it, so the walk cannot be
  # expressed as a vectorised operation. The explicit stack replaces
  # recursion; `ptr` remembers where each vertex's neighbour scan stopped.
  for (root in seq_len(n)) {
    if (disc[root] > 0L) next
    clock <- clock + 1L
    disc[root] <- clock
    low[root] <- clock
    top <- 1L
    stack[top] <- root
    root_children <- 0L
    while (top > 0L) {
      v <- stack[top]
      nb <- adj[[v]]
      if (ptr[v] <= length(nb)) {
        w <- nb[ptr[v]]
        ptr[v] <- ptr[v] + 1L
        if (disc[w] == 0L) {
          parent[w] <- v
          clock <- clock + 1L
          disc[w] <- clock
          low[w] <- clock
          top <- top + 1L
          stack[top] <- w
          if (v == root) root_children <- root_children + 1L
        } else if (w != parent[v]) {
          low[v] <- min(low[v], disc[w])
        }
      } else {
        top <- top - 1L
        p <- parent[v]
        if (p > 0L) {
          low[p] <- min(low[p], low[v])
          if (p != root && low[v] >= disc[p]) is_art[p] <- TRUE
          if (low[v] > disc[p]) {
            n_bridges <- n_bridges + 1L
            bridge_from[n_bridges] <- p
            bridge_to[n_bridges] <- v
          }
        }
      }
    }
    if (root_children >= 2L) is_art[root] <- TRUE
  }
  keep <- seq_len(n_bridges)
  list(articulation = which(is_art),
       bridges = cbind(bridge_from[keep], bridge_to[keep]))
}

#' Articulation points (cut vertices)
#'
#' Matches `igraph::articulation_points()`: direction and self-loops are
#' ignored, and a vertex is a cut vertex when removing it splits its
#' component.
#' @param b Binary adjacency matrix.
#' @return Sorted integer vector of vertex indices.
#' @keywords internal
#' @noRd
.cg_articulation_points <- function(b) {
  .cg_tarjan(b)$articulation
}

#' Bridge flag for every row of an edge matrix
#'
#' Matches `igraph::bridges()` on the caller's own edge list: an edge is a
#' bridge when it is the only edge between its endpoints (parallel edges,
#' including a reciprocated directed dyad, are never bridges), it is not a
#' self-loop, and removing it disconnects its component.
#'
#' @param b Binary adjacency matrix of the graph.
#' @param edges Integer matrix with two columns, one row per edge, in the
#'   caller's order.
#' @return Logical vector, one entry per row of `edges`.
#' @keywords internal
#' @noRd
.cg_bridges <- function(b, edges) {
  edges <- matrix(as.integer(edges), ncol = 2L)
  if (nrow(edges) == 0L) return(logical(0))
  n <- nrow(b)
  pair_key <- function(i, j) (pmin(i, j) - 1) * n + pmax(i, j)
  ek <- pair_key(edges[, 1L], edges[, 2L])
  tj <- .cg_tarjan(b)$bridges
  bridge_keys <- if (nrow(tj)) pair_key(tj[, 1L], tj[, 2L]) else numeric(0)
  uk <- unique(ek)
  multiplicity <- tabulate(match(ek, uk), nbins = length(uk))[match(ek, uk)]
  ek %in% bridge_keys & edges[, 1L] != edges[, 2L] & multiplicity == 1L
}

# ---------------------------------------------------------------------------
# Ego neighbourhoods
# ---------------------------------------------------------------------------

#' Vertices within `order` hops of a seed set
#'
#' Breadth-first expansion with a vectorised frontier: each ring is one
#' matrix-vector product. Matches `igraph::ego(g, order, nodes, mode)` taken
#' as a union, seeds included (order 0 returns the seeds themselves).
#'
#' @param b Binary adjacency matrix.
#' @param nodes Integer seed indices; out-of-range indices are ignored.
#' @param order Non-negative integer hop limit.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return Logical vector, one entry per vertex.
#' @keywords internal
#' @noRd
.cg_ego_mask <- function(b, nodes, order = 1L, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  n <- nrow(b)
  reached <- logical(n)
  nodes <- as.integer(nodes)
  nodes <- nodes[!is.na(nodes) & nodes >= 1L & nodes <= n]
  reached[nodes] <- TRUE
  if (length(nodes) == 0L || order < 1) return(reached)
  a <- switch(mode,
    out = (b != 0) * 1,
    `in` = t(b != 0) * 1,
    all = ((b != 0) | (t(b) != 0)) * 1)
  frontier <- reached
  # One ring per iteration; the ring count is the requested order and each
  # ring needs the previous one, so the loop is bounded and sequential.
  for (k in seq_len(order)) {
    nxt <- as.vector(crossprod(a, frontier)) > 0 & !reached
    if (!any(nxt)) break
    reached <- reached | nxt
    frontier <- nxt
  }
  reached
}

# ---------------------------------------------------------------------------
# Shortest-path counts (unweighted) for the group betweenness family
# ---------------------------------------------------------------------------

#' Number of geodesics between every ordered pair
#'
#' Layered counting: `sigma[s, t]` is the number of hop-shortest paths from
#' `s` to `t` in the graph `a`, given the hop-distance matrix `d`. Restricting
#' each product to the previous distance layer keeps the counts exact (walk
#' counts never enter) and lets the same routine count the geodesics that
#' survive in a reduced graph: pass the reduced adjacency with the
#' *original* distances and only paths that are still shortest in the
#' original graph are counted.
#'
#' @param a Binary adjacency matrix (zero diagonal).
#' @param d Hop-distance matrix of the reference graph.
#' @return Numeric matrix of path counts; `0` where unreachable, `1` on the
#'   diagonal.
#' @keywords internal
#' @noRd
.cg_geodesic_counts <- function(a, d) {
  n <- nrow(a)
  sigma <- diag(1, n, n)
  fin <- is.finite(d) & d > 0
  if (!any(fin)) return(sigma)
  kmax <- max(d[fin])
  layer <- sigma
  # Layer k is reached only through layer k - 1, so the expansion is a
  # bounded sequence of matrix products, one per distance value.
  for (k in seq_len(kmax)) {
    layer <- (layer * (d == k - 1)) %*% a
    hit <- d == k
    sigma[hit] <- layer[hit]
  }
  sigma
}

# ---------------------------------------------------------------------------
# Edge betweenness (Brandes accumulation over edges)
# ---------------------------------------------------------------------------

#' Edge betweenness as a matrix
#'
#' Brandes (2001) with the dependency accumulated onto the predecessor edge
#' instead of the predecessor vertex. Equal-length paths are detected with
#' igraph's relative epsilon (`.cg_cmp_epsilon()`), which is what keeps the
#' geodesic counts right when weights are of order 1e-9 -- an absolute
#' tolerance merges paths there. The undirected result is symmetric and
#' halved, matching `igraph::edge_betweenness()`.
#'
#' @param w Weight matrix (weights are path lengths); non-positive means no
#'   edge. Must be symmetric when `directed` is `FALSE`.
#' @param n Vertex count.
#' @param directed Whether the graph is directed.
#' @return Numeric `n x n` matrix; entry `[i, j]` is the betweenness of the
#'   edge `i -> j` (`i -- j` when undirected), `0` where no edge exists.
#' @keywords internal
#' @noRd
.cg_edge_betweenness_structure <- function(w, n, directed) {
  eb <- matrix(0, n, n)
  if (n <= 1L) return(eb)
  if (any(w < 0)) {
    stop(errorCondition(
      "Edge betweenness needs non-negative weights; found a negative edge.",
      class = "cograph_negative_weights", call = NULL))
  }
  # One accumulation per source, each depending on its own settle order.
  for (s in seq_len(n)) {
    pred <- vector("list", n)
    sigma <- numeric(n); sigma[s] <- 1
    dist <- rep(Inf, n); dist[s] <- 0
    visited <- rep(FALSE, n)
    order_stack <- integer(0)
    for (step in seq_len(n)) {
      cand <- dist; cand[visited] <- Inf
      if (all(is.infinite(cand))) break
      u <- which.min(cand)
      visited[u] <- TRUE
      order_stack <- c(order_stack, u)
      edge <- w[u, ]
      move <- which(!visited & edge > 0)
      for (v in move) {
        nd <- dist[u] + edge[v]
        cmp <- .cg_cmp_epsilon(nd, dist[v])
        if (cmp < 0L) {
          dist[v] <- nd; sigma[v] <- sigma[u]; pred[[v]] <- u
        } else if (cmp == 0L) {
          sigma[v] <- sigma[v] + sigma[u]; pred[[v]] <- c(pred[[v]], u)
        }
      }
    }
    delta <- numeric(n)
    for (wn in rev(order_stack)) {
      p <- pred[[wn]]
      if (length(p) > 0L) {
        credit <- (sigma[p] / sigma[wn]) * (1 + delta[wn])
        cells <- cbind(p, wn)
        eb[cells] <- eb[cells] + credit
        delta[p] <- delta[p] + credit
      }
    }
  }
  if (!directed) (eb + t(eb)) / 2 else eb
}

# ---------------------------------------------------------------------------
# Small helpers used by the wrangling verbs
# ---------------------------------------------------------------------------

#' Components numbered the way igraph numbers them
#'
#' `igraph::components()` numbers components by the smallest vertex index they
#' contain (first appearance in vertex order). `.cg_component_labels()` gives
#' each component its smallest member as label; renumbering by first
#' appearance reproduces the igraph ids.
#' @param b Adjacency matrix; direction is ignored (weak components).
#' @return List with `membership` (integer per vertex), `csize` (integer per
#'   component) and `no` (component count).
#' @keywords internal
#' @noRd
.cg_components_numbered <- function(b) {
  lab <- .cg_component_labels(.cg_support(b))
  membership <- match(lab, unique(lab))
  csize <- tabulate(membership, nbins = length(unique(lab)))
  list(membership = membership, csize = csize, no = length(csize))
}

#' Reciprocation flag per edge
#'
#' An edge `i -> j` is mutual when `j -> i` is also listed; a self-loop
#' reciprocates itself.
#' @param from,to Integer endpoint vectors. @param n Vertex count.
#' @return Logical vector, one per edge.
#' @keywords internal
#' @noRd
.cg_reciprocated <- function(from, to, n) {
  key <- (from - 1) * n + to
  reverse <- (to - 1) * n + from
  reverse %in% key
}

#' Edge betweenness in the caller's edge order
#'
#' Looks each `(from, to)` row up in the betweenness matrix; undirected edges
#' read the symmetric cell whichever way they are stored. Negative weights
#' give `NA` with the classed warning, as the node measures do.
#' @param cg A `cg_graph` context.
#' @param edges Data frame (or matrix) with `from` and `to` columns.
#' @return Numeric vector, one per row of `edges`.
#' @keywords internal
#' @noRd
.cg_edge_betweenness_rows <- function(cg, edges) {
  from <- as.integer(edges[, "from"])
  to <- as.integer(edges[, "to"])
  if (any(cg$w < 0)) return(.cg_na_negative(length(from), "Edge betweenness"))
  eb <- .cg_edge_betweenness_structure(cg$w, cg$n, cg$directed)
  eb[cbind(from, to)]
}

#' Path-length comparison with igraph's relative tolerance
#'
#' Transcribes `igraph_cmp_epsilon()` with `IGRAPH_SHORTEST_PATH_EPSILON`
#' (1e-10): two lengths tie when their difference is below 1e-10 of their
#' magnitude, so ties are found at any weight scale.
#' @param a,b Two path lengths (either may be `Inf`).
#' @return `-1L`, `0L` or `1L` as `a` is shorter, tied or longer.
#' @keywords internal
#' @noRd
.cg_cmp_epsilon <- function(a, b, eps = 1e-10) {
  if (a == b) return(0L)
  diff <- a - b
  abs_diff <- abs(diff)
  total <- abs(a) + abs(b)
  tied <- if (a == 0 || b == 0 || total < .Machine$double.xmin) {
    abs_diff < eps * .Machine$double.xmin
  } else {
    abs_diff / min(total, .Machine$double.xmax) < eps
  }
  if (tied) 0L else if (diff < 0) -1L else 1L
}

#' Coreness with igraph's self-loop convention
#'
#' `igraph::coreness()` keeps a vertex's self-loop in its degree for the whole
#' peeling (two under `"all"` and undirected, one under `"out"` / `"in"`),
#' whereas `.cg_coreness()` reads the graph without its diagonal. The loop
#' contribution is a constant offset that only its own vertex can remove, so
#' the peeling below starts from `.cg_degree(loops = TRUE)` and subtracts
#' off-diagonal losses only. Identical to `.cg_coreness()` on a loop-free graph.
#'
#' @param b Binary adjacency matrix (diagonal read, not assumed zero).
#' @param n Vertex count. @param directed Whether directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_coreness_loops <- function(b, n, directed = FALSE, mode = c("all", "out", "in")) {
  # One implementation of igraph's loop convention lives in R/kernels-batch11.R.
  .cg_loop_coreness(b, n, directed = directed, mode = match.arg(mode))
}
