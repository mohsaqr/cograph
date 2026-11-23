# ===========================================================================
# Neighborhood-aggregate centralities
# ===========================================================================

#' Distinct neighbor set (no reciprocation multiplicity)
#'
#' Distinct from `.cg_adjlist()`, which repeats a reciprocated dyad. Measures
#' that aggregate *over vertices* need this one; measures that aggregate over
#' *edges* need the other.
#'
#' @param b Binary adjacency matrix. @param mode One of `"all"`, `"out"`, `"in"`.
#' @return A list of integer vectors.
#' @keywords internal
#' @noRd
.cg_adj_distinct <- function(b, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  n <- nrow(b)
  lapply(seq_len(n), function(i) {
    j <- seq_len(n)[seq_len(n) != i]
    keep <- switch(mode,
      out = b[i, j] != 0,
      `in` = b[j, i] != 0,
      all = b[i, j] != 0 | b[j, i] != 0)
    j[keep]
  })
}

#' h-index of a numeric vector
#' @param v Numeric vector.
#' @return Integer-valued numeric.
#' @keywords internal
#' @noRd
.cg_hindex <- function(v) {
  s <- sort(v, decreasing = TRUE)
  k <- which(s >= seq_along(s))
  if (length(k) == 0L) 0 else max(k)
}

#' k-reach: how many vertices lie within distance k
#' @param d Distance matrix. @param n Vertex count. @param k Radius.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_kreach <- function(d, n, k = 3) {
  rowSums(.cg_offdiag(d) & d <= k)
}

#' Diffusion centrality, cograph's formulation
#'
#' Own scaled degree plus the scaled degrees of its neighbors. Note this is
#' **not** the Banerjee power series: reciprocated dyads are collapsed with a
#' logical OR so a mutual edge is not counted twice in the neighbor sum,
#' even though the degree itself counts it twice.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode Degree mode. @param lambda Scaling applied to degree.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_diffusion <- function(b, directed, mode = "all", lambda = 1) {
  n <- nrow(b)
  if (n == 0L) return(numeric(0))
  d <- .cg_degree(b, directed, mode) * lambda
  adj <- if (directed && identical(mode, "all")) ((b + t(b)) != 0) * 1
         else if (directed && identical(mode, "in")) t(b)
         else b
  as.numeric(d + adj %*% d)
}

#' Lobby index: h-index over the neighbors' degrees (Korn et al. 2009)
#'
#' Definition 1 of Korn, Schubert and Telcs: the largest k such that the
#' vertex has at least k neighbors of degree at least k. The neighborhood is
#' open (the vertex does not count itself) and each neighbor counts once; the
#' degrees and the neighbor set are taken at `mode`.
#'
#' @param b Binary adjacency matrix. @param directed Whether directed.
#' @param mode One of `"all"`, `"out"`, `"in"`.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_lobby <- function(b, directed = FALSE, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  bb <- (b != 0) * 1
  diag(bb) <- 0
  deg <- .cg_degree(bb, directed, mode)
  nbrs <- .cg_adj_distinct(bb, mode)
  vapply(nbrs, function(js) .cg_hindex(deg[js]), numeric(1L))
}

#' h-index over the neighbors' strengths rather than degrees
#' @param nbrs Distinct neighbor lists. @param strength Strength vector.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_hindex_strength <- function(nbrs, strength) {
  vapply(nbrs, function(js) .cg_hindex(strength[js]), numeric(1L))
}

#' ClusterRank (Chen, Gao, Lu and Zhou 2013)
#'
#' Eq. 4: `s_i = f(c_i) sum_{j in Gamma_i} (k_j + 1)` with Eq. 3
#' `f(c) = 10^-c`. On an undirected graph `Gamma_i` is the neighbor set, `k_j`
#' the degree and `c_i` the local clustering coefficient. On a directed graph
#' (Eq. 1) a link i -> j makes j a follower of i: `Gamma_i` holds the
#' out-neighbors, `k_j` is the out-degree, and
#' `c_i = |{j -> k : j, k in Gamma_i}| / (k_i (k_i - 1))`, a reciprocated pair
#' of followers counting as two links.
#'
#' @param b Binary adjacency matrix without self-loops (transpose it to follow
#'   in-ties). @param directed Whether the graph is directed.
#' @return Numeric vector; `NaN` below two neighbors, where `c_i` is undefined.
#' @keywords internal
#' @noRd
.cg_clusterrank <- function(b, directed) {
  if (!directed) b <- ((b + t(b)) != 0) * 1
  k <- rowSums(b)
  vapply(seq_len(nrow(b)), function(i) {
    if (k[i] < 2) return(NaN)
    followers <- which(b[i, ] != 0)
    links <- sum(b[followers, followers])
    clustering <- links / (k[i] * (k[i] - 1))
    10^(-clustering) * sum(k[followers] + 1)
  }, numeric(1L))
}

#' Maximum neighborhood component
#'
#' The size of the largest component of the subgraph the neighbors of a
#' vertex induce. On a directed graph the component is strongly connected
#' (as `centiserve::mnc()`, `clusters(mode = "strong")`), on an undirected
#' one connected.
#'
#' @param b Binary adjacency matrix. @param nbrs Neighbor lists.
#' @param directed Whether the graph is directed.
#' @return Numeric vector: the largest component among a vertex's neighbors.
#' @keywords internal
#' @noRd
.cg_mnc <- function(b, nbrs, directed = FALSE) {
  u <- ((b + t(b)) != 0) * 1
  diag(u) <- 0
  vapply(nbrs, function(raw) {
    nodes <- unique(raw)
    if (length(nodes) == 0L) return(0)
    if (length(nodes) == 1L) return(1)
    if (directed) {
      return(max(lengths(.cg_strong_components(b[nodes, nodes, drop = FALSE]))))
    }
    .cg_largest_component(u[nodes, nodes, drop = FALSE])
  }, numeric(1L))
}

#' Size of the largest connected component
#' @param u Symmetric binary matrix.
#' @return A single number.
#' @keywords internal
#' @noRd
.cg_largest_component <- function(u) {
  n <- nrow(u)
  if (n <= 1L) return(n)
  seen <- rep(FALSE, n)
  best <- 0
  for (s in seq_len(n)) {
    if (seen[s]) next
    frontier <- s
    seen[s] <- TRUE
    size <- 0
    while (length(frontier) > 0L) {
      v <- frontier[1L]
      frontier <- frontier[-1L]
      size <- size + 1
      nxt <- which(!seen & u[v, ] != 0)
      seen[nxt] <- TRUE
      frontier <- c(frontier, nxt)
    }
    best <- max(best, size)
  }
  best
}

#' Summed degree of a vertex's neighbors
#'
#' The neighbor degrees are added, not averaged -- `centrality()` documents
#' the `expected` measure as "Sum of neighbor degrees".
#'
#' @param adj Distinct neighbor lists. @param deg Degree vector.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_expected <- function(adj, deg) {
  vapply(adj, function(js) sum(deg[js]), numeric(1L))
}

#' Collective influence at radius two (Morone & Makse 2015)
#' @param d Distance matrix. @param deg Degree vector.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
.cg_collective_influence <- function(d, deg) {
  shell <- d == 2
  (deg - 1) * rowSums(ifelse(shell, rep(deg - 1, each = nrow(d)), 0))
}
