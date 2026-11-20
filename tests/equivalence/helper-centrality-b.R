# Reference-side code for the centrality files of slice B (centiserve, sna, influenceR, keyplayer, brainGraph,
# netrankr, tnet and the definitions written out in base R). Nothing here calls cograph.

#' A random tree as an adjacency matrix, the same in every run
#'
#' Node i (i >= 2) is tied to one earlier node drawn uniformly, so every pair is joined by exactly one path.
#'
#' @param nodes Number of nodes.
#' @param seed The seed of the draw.
#' @return A symmetric 0/1 matrix with dimnames `N1`, `N2`, ...
ref_tree <- function(nodes, seed = 1L) {
  stopifnot("`nodes` must be one whole number of at least 2" = length(nodes) == 1L && nodes >= 2)
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()),
          add = TRUE, after = FALSE)
  set.seed(seed)
  parent <- vapply(seq_len(nodes - 1L) + 1L, \(i) sample.int(i - 1L, 1L), integer(1))
  graph <- matrix(0, nodes, nodes)
  graph[cbind(seq_len(nodes - 1L) + 1L, parent)] <- 1
  graph <- graph + t(graph)
  dimnames(graph) <- list(paste0("N", seq_len(nodes)), paste0("N", seq_len(nodes)))
  graph
}

#' Hop distances of a graph, by breadth-first expansion in base R
#'
#' Row i holds the number of ties on a shortest path from i (following the direction of the ties) to each node;
#' `Inf` where there is none.
#'
#' @param graph A square matrix; any non-zero cell is a tie.
#' @return A numeric matrix of the same dimensions and dimnames.
ref_hop_distances <- function(graph) {
  ties <- unname(graph != 0) * 1
  n <- nrow(ties)
  start <- matrix(Inf, n, n)
  diag(start) <- 0
  # One expansion per possible path length: a node first reached at step k is at distance k.
  distances <- Reduce(\(d, k) {
    reached <- (is.finite(d) * 1) %*% ties > 0
    d[reached & !is.finite(d)] <- k
    d
  }, seq_len(max(n - 1L, 1L)), start)
  dimnames(distances) <- dimnames(graph)
  distances
}

#' Centroid value from its definition (Wuchty and Stadler 2003)
#'
#' gamma(u, v) is the number of nodes w closer to u than to v, f(u, v) = gamma(u, v) - gamma(v, u), and the
#' centroid value of u is the smallest f(u, v) over the other nodes v != u.
#'
#' @param graph An adjacency matrix with dimnames.
#' @return A numeric vector named by node.
ref_centroid <- function(graph) {
  d <- ref_hop_distances(graph)
  n <- nrow(d)
  gamma <- outer(seq_len(n), seq_len(n), Vectorize(\(u, v) sum(d[u, ] < d[v, ])))
  f <- gamma - t(gamma)
  diag(f) <- Inf
  stats::setNames(apply(f, 1L, min), rownames(graph))
}

#' DMNC from its definition (Lin et al. 2008), unweighted undirected graph
#'
#' The neighbours of v (v itself left out) induce a subgraph; with N the size of its largest connected component and
#' E the number of ties inside the components of that size, DMNC(v) = E / N^epsilon, and 0 when E is 0. When several
#' components share the largest size, their ties are all counted, as centiserve::dmnc() means to do.
#'
#' @param graph A symmetric adjacency matrix with dimnames.
#' @param epsilon The exponent.
#' @return A numeric vector named by node.
ref_dmnc <- function(graph, epsilon) {
  ties <- unname(graph != 0) * 1
  scores <- vapply(seq_len(nrow(ties)), \(v) {
    neighbours <- which(ties[v, ] != 0)
    if (!length(neighbours)) return(0)
    sub <- ties[neighbours, neighbours, drop = FALSE]
    reach <- is.finite(ref_hop_distances(sub))
    # a component is a set of mutually reachable nodes: one row pattern of `reach` per component
    component <- match(apply(reach, 1L, paste, collapse = ""), unique(apply(reach, 1L, paste, collapse = "")))
    sizes <- tabulate(component)
    in_largest <- component %in% which(sizes == max(sizes))
    edges <- sum(sub[in_largest, in_largest]) / 2
    if (edges == 0) 0 else edges / max(sizes)^epsilon
  }, numeric(1))
  stats::setNames(scores, rownames(graph))
}

#' Domain proximity prestige from its definition (Lin 1976; Wasserman and Faust 1994, p. 203)
#'
#' With I_v the number of nodes that reach v and D_v the sum of their distances to v,
#' P(v) = (I_v / (n - 1))^2 / (D_v / I_v) = I_v^2 / (D_v (n - 1)), and 0 when no node reaches v.
#'
#' @param graph A directed adjacency matrix with dimnames.
#' @return A numeric vector named by node.
ref_domain_proximity <- function(graph) {
  d <- ref_hop_distances(graph)
  diag(d) <- Inf
  reachers <- colSums(is.finite(d))
  total <- colSums(ifelse(is.finite(d), d, 0))
  stats::setNames(ifelse(reachers > 0, reachers^2 / (total * (nrow(d) - 1)), 0), rownames(graph))
}

#' Bridging coefficient (Hwang et al. 2006): the reciprocal degree of v over the sum of its neighbours' reciprocal
#' degrees
#'
#' @param graph A symmetric adjacency matrix with dimnames.
#' @return A numeric vector named by node; 0 for an isolate.
ref_bridging_coefficient <- function(graph) {
  ties <- unname(graph != 0) * 1
  degree <- rowSums(ties)
  inverse <- ifelse(degree > 0, 1 / degree, 0)
  around <- as.vector(ties %*% inverse)
  stats::setNames(ifelse(around > 0, inverse / around, 0), rownames(graph))
}

#' Burt's (1992) constraint from its definition, binary undirected ties
#'
#' With p_ij = a_ij / deg_i the share of i's ties that go to j, c_i = sum over the neighbours j of i of
#' (p_ij + sum_q p_iq p_qj)^2.
#'
#' @param graph A symmetric 0/1 adjacency matrix with dimnames.
#' @return A numeric vector named by node.
ref_constraint <- function(graph) {
  ties <- unname(graph != 0) * 1
  shares <- ties / rowSums(ties)
  scores <- vapply(seq_len(nrow(ties)), \(i) {
    neighbours <- which(ties[i, ] != 0)
    direct <- shares[i, neighbours]
    indirect <- colSums(shares[i, ] * shares[, neighbours, drop = FALSE])
    sum((direct + indirect)^2)
  }, numeric(1))
  stats::setNames(scores, rownames(graph))
}

#' An adjacency matrix as a tnet edge list (columns i, j, w; each undirected tie listed both ways)
#'
#' @param graph A square numeric matrix.
#' @return A numeric matrix with columns `i`, `j`, `w`, sorted by i then j.
ref_tnet_edges <- function(graph) {
  cells <- which(unname(graph) != 0, arr.ind = TRUE)
  cells <- cells[order(cells[, 1L], cells[, 2L]), , drop = FALSE]
  cbind(i = cells[, 1L], j = cells[, 2L], w = unname(graph)[cells])
}
