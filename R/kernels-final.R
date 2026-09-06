# ===========================================================================
# Remaining centralities: path enumeration, strong components, max flow
# ===========================================================================

#' Node occurrence counts across all shortest paths from one source
#'
#' Enumerates every shortest path, not just one per pair, so a vertex on many
#' equally short routes is counted many times. Cost is exponential in the
#' worst case; it is bounded here only by graph size.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param source Source index. @param mode Neighbour mode.
#' @return Numeric vector of counts.
#' @keywords internal
#' @noRd
.cg_sp_node_counts <- function(b, directed, source, mode = "all") {
  n <- nrow(b)
  adj <- .cg_adjlist(b, directed, mode)
  dist <- rep(Inf, n); dist[source] <- 0
  pred <- vector("list", n)
  queue <- source
  qi <- 1L
  while (qi <= length(queue)) {
    v <- queue[qi]; qi <- qi + 1L
    for (w in adj[[v]]) {
      if (!is.finite(dist[w])) { dist[w] <- dist[v] + 1; queue <- c(queue, w) }
      if (dist[w] == dist[v] + 1) pred[[w]] <- c(pred[[w]], v)
    }
  }
  counts <- numeric(n)
  visit <- function(v, path) {
    if (v == source) {
      counts[source] <<- counts[source] + 1
      for (node in path) counts[node] <<- counts[node] + 1
      return(invisible(NULL))
    }
    for (p in pred[[v]]) visit(p, c(v, path))
  }
  for (target in which(is.finite(dist))) visit(target, integer(0))
  counts
}

#' Bottleneck centrality
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param n Vertex count. @param mode Neighbour mode.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_bottleneck <- function(b, directed, n, mode = "all") {
  if (n <= 1L) return(rep(1, n))
  out <- numeric(n)
  for (s in seq_len(n)) {
    paths <- .cg_sp_node_counts(b, directed, s, mode)
    hit <- which(seq_len(n) != s & paths > n / 4)
    out[hit] <- out[hit] + 1
  }
  out
}

#' Strongly connected components (Tarjan, iterative)
#'
#' Written with an explicit work stack rather than recursion: the recursive
#' form overflows R's node stack on a few thousand vertices, which is well
#' inside the range this is called on.
#'
#' @param b Binary adjacency matrix.
#' @return A list of integer vectors, each a component.
#' @keywords internal
#' @noRd
.cg_strong_components <- function(b) {
  n <- nrow(b)
  if (n == 0L) return(list())
  succ <- lapply(seq_len(n), function(v) which(b[v, ] != 0))
  index <- rep(NA_integer_, n)
  low <- integer(n)
  on_stack <- rep(FALSE, n)
  stack <- integer(0)
  comps <- list()
  nxt <- 0L
  for (root in seq_len(n)) {
    if (!is.na(index[root])) next
    # Each frame is (vertex, position of the next successor to visit).
    work_v <- root
    work_i <- 1L
    index[root] <- nxt; low[root] <- nxt; nxt <- nxt + 1L
    stack <- c(stack, root); on_stack[root] <- TRUE
    while (length(work_v) > 0L) {
      v <- work_v[length(work_v)]
      i <- work_i[length(work_i)]
      sv <- succ[[v]]
      if (i <= length(sv)) {
        work_i[length(work_i)] <- i + 1L
        w <- sv[i]
        if (is.na(index[w])) {
          index[w] <- nxt; low[w] <- nxt; nxt <- nxt + 1L
          stack <- c(stack, w); on_stack[w] <- TRUE
          work_v <- c(work_v, w); work_i <- c(work_i, 1L)
        } else if (on_stack[w]) {
          low[v] <- min(low[v], index[w])
        }
      } else {
        work_v <- work_v[-length(work_v)]
        work_i <- work_i[-length(work_i)]
        if (length(work_v) > 0L) {
          parent <- work_v[length(work_v)]
          low[parent] <- min(low[parent], low[v])
        }
        if (low[v] == index[v]) {
          comp <- integer(0)
          repeat {
            w <- stack[length(stack)]; stack <- stack[-length(stack)]
            on_stack[w] <- FALSE
            comp <- c(comp, w)
            if (w == v) break
          }
          comps[[length(comps) + 1L]] <- sort(comp)
        }
      }
    }
  }
  comps
}

#' Density of maximum neighbourhood component
#'
#' Reproduces a reference quirk deliberately: the neighbour list carries
#' reciprocation multiplicity (`2 2 3 3`), component membership is computed on
#' the deduplicated subgraph, and the result is then indexed back into the
#' *multiplied* list. That mismatch can select the same vertex twice. It is
#' wrong, but it is what `calculate_dmnc()` and the reference implementation
#' both do, so matching it is the contract.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode Neighbour mode. @param epsilon Exponent on component size.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_dmnc <- function(b, directed, mode = "all", epsilon = 1.7) {
  adj <- .cg_adjlist(b, directed, mode)
  vapply(adj, function(nbs) {
    if (length(nbs) == 0L) return(0)
    sub_nodes <- unique(nbs)
    sub <- b[sub_nodes, sub_nodes, drop = FALSE]
    comps <- .cg_strong_components(sub)
    if (length(comps) == 0L) return(0)
    sizes <- lengths(comps)
    max_size <- max(sizes)
    positions <- sort(unlist(comps[sizes == max_size], use.names = FALSE))
    selected <- unique(nbs[positions])
    selected <- selected[!is.na(selected)]
    e <- if (length(selected) < 2L) 0 else {
      sm <- b[selected, selected, drop = FALSE]
      diag(sm) <- 0
      cnt <- sum(sm != 0)
      if (directed) cnt else cnt / 2
    }
    if (e == 0 || max_size == 0) 0 else e / max_size^epsilon
  }, numeric(1L))
}

#' Gateway coefficient (Vargas & Wahl 2014)
#' @param b Binary adjacency matrix. @param membership Integer community labels.
#' @return Numeric vector; `NaN` without a partition.
#' @keywords internal
#' @noRd
.cg_gateway <- function(b, membership) {
  n <- nrow(b)
  if (is.null(membership) || length(membership) == 0L) return(rep(NaN, n))
  modules <- max(membership)
  if (modules <= 1L) return(rep(0, n))
  ki <- colSums(b)
  cn <- max(vapply(seq_len(modules), function(m) sum(ki[membership == m]), numeric(1L)))
  kis <- t(vapply(seq_len(n), function(i)
    vapply(seq_len(modules), function(s) sum(b[i, membership == s]), numeric(1L)),
    numeric(modules)))
  kjs <- t(vapply(seq_len(modules), function(s)
    colSums(kis[membership == s, , drop = FALSE]), numeric(modules)))
  vapply(seq_len(n), function(i) {
    if (ki[i] == 0) return(0)
    denom <- kjs[membership[i], ]
    bar_kis <- ifelse(denom > 0, kis[i, ] / denom, 0)
    incoming <- which(b[, i] > 0)
    cis <- vapply(seq_len(modules), function(s)
      sum(ki[incoming[membership[incoming] == s]]), numeric(1L))
    bar_cis <- if (cn > 0) cis / cn else rep(0, modules)
    gis <- 1 - bar_kis * bar_cis
    1 - sum(kis[i, ]^2 * gis^2) / ki[i]^2
  }, numeric(1L))
}

# ---------------------------------------------------------------------------
# flow_betweenness (Freeman 1991) is deliberately NOT ported. It keeps its
# igraph route in calculate_flow_betweenness().
#
# It CAN be matched -- that was established, not assumed. igraph's max_flow is
# Goldberg-Tarjan push-relabel, and the decomposition is fixed by a cleanup
# phase after the preflow converges: excess is returned to the source, then
# directed flow cycles are cancelled by DFS. Transcribing that reproduced all
# 99 fixtures bit-exactly. An Edmonds-Karp kernel agrees on every max-flow
# VALUE and still differs on ~40% of decompositions, because the value is
# invariant and the assignment is not.
#
# It is not kept because the price is wrong: ~510 lines, 18% of this entire
# kernel corpus, for 1 of 89 measures -- and unlike every other kernel here it
# transcribes another library's C internals, resting on two arc-ordering
# constants. It is the only component that could break silently on an igraph
# upgrade.
#
# For a flow-based betweenness this package computes from scratch, use
# current_flow_betweenness (.cg_current_flow_betweenness, ~40 lines): it is a
# linear solve on the Laplacian pseudoinverse, so it has exactly one answer,
# and it is validated against both cograph and networkx 3.6.1.
#
# The rule: port a measure when it reproduces the reference from mathematics;
# leave it on igraph when parity requires reproducing igraph's internals.
# ---------------------------------------------------------------------------

#' Infection centrality
#'
#' Sums per-depth transmission weight over every self-avoiding walk of length
#' up to `max_length`. Implemented as backtracking depth-first search with a
#' mutable visited flag: memoising on the visited set is far more expensive
#' than the search it saves, because the key is rebuilt at every call.
#'
#' The neighbour list keeps reciprocation multiplicity, so a mutual dyad is
#' traversed twice.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param beta Per-step transmission probability. @param mu Recovery rate.
#' @param max_length Longest walk considered.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_infection <- function(b, directed, beta = 0.8, mu = 0, max_length = 6L) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  adj <- .cg_adjlist(b, directed, "all")
  depths <- seq_len(max_length) - 1L
  depth_weights <- beta^(depths + 1) * (1 - mu)^depths
  visited <- logical(n)
  count_saws <- function(current, depth) {
    if (depth >= max_length) return(0)
    w <- depth_weights[depth + 1L]
    total <- 0
    for (nb in adj[[current]]) {
      if (!visited[nb]) {
        total <- total + w
        visited[nb] <<- TRUE
        total <- total + count_saws(nb, depth + 1L)
        visited[nb] <<- FALSE
      }
    }
    total
  }
  vapply(seq_len(n), function(src) {
    visited[src] <<- TRUE
    out <- count_saws(src, 0L)
    visited[src] <<- FALSE
    out
  }, numeric(1L))
}
