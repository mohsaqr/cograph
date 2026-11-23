# Reference-side helpers of the graph-level and edge-level files (network_*(), censuses, motifs, assortativity,
# rich club, robustness, edges, paths, degree distribution). Base R and the reference packages only; nothing here
# calls cograph.

#' The 0/1 adjacency of a graph, without self-loops
#'
#' @param graph A square numeric matrix.
#' @return A 0/1 matrix with zero diagonal and the dimnames of `graph`.
d1_binary <- function(graph) {
  stopifnot("`graph` must be a square matrix" = is.matrix(graph) && nrow(graph) == ncol(graph))
  binary <- (graph != 0) * 1
  diag(binary) <- 0
  binary
}

#' Freeman's centralization: the summed gap to the largest score over a stated maximum (Freeman 1979)
#'
#' @param scores Numeric scores, one per node.
#' @param maximum The theoretical maximum of the summed gap.
#' @return One number.
d1_freeman <- function(scores, maximum) {
  sum(max(scores) - scores) / maximum
}

#' Eigenvector centralization from its definition
#'
#' The centrality of a node is the principal eigenvector of the transposed 0/1 adjacency (a node is central when
#' central nodes point to it), scaled to a maximum of 1. The theoretical maximum of the summed gap is n - 1 for a
#' directed graph (a single arc) and n - 2 for an undirected one (a single edge), as in igraph's centr_eigen_tmax().
#'
#' @param graph A square numeric matrix.
#' @param directed Whether the graph is directed.
#' @return One number.
d1_eigen_centralization <- function(graph, directed) {
  binary <- d1_binary(graph)
  decomposition <- eigen(t(binary))
  leading <- abs(Re(decomposition$vectors[, which.max(Re(decomposition$values))]))
  scores <- leading / max(leading)
  n <- nrow(binary)
  d1_freeman(scores, if (directed) n - 1 else n - 2)
}

#' Size of the largest connected component of an undirected 0/1 adjacency, from reachability
#'
#' Reachability is the boolean closure of I + A, by repeated squaring.
#'
#' @param binary A symmetric 0/1 matrix.
#' @return One number (0 for an empty matrix).
d1_largest_component <- function(binary) {
  n <- nrow(binary)
  if (n == 0L) return(0)
  steps <- max(1L, ceiling(log2(n)) + 1L)
  reach <- Reduce(\(r, step) ((r %*% r) > 0) * 1, seq_len(steps), init = ((diag(n) + binary) > 0) * 1)
  max(rowSums(reach))
}

#' Largest component after each step of a sequential degree attack (Albert, Jeong & Barabasi 2000)
#'
#' At each step the node of highest degree in the remaining graph (the first one in node order on a tie) is
#' removed; the largest component is recorded before the first removal and after each removal, and the curve
#' ends at 0 once every node is gone.
#'
#' @param graph A symmetric square matrix.
#' @return A numeric vector of length n + 1: largest component sizes.
d1_degree_attack <- function(graph) {
  binary <- d1_binary(graph)
  n <- nrow(binary)
  remaining <- Reduce(\(keep, step) {
    sub <- binary[keep, keep, drop = FALSE]
    keep[-which.max(rowSums(sub))]
  }, seq_len(n - 1L), accumulate = TRUE, init = seq_len(n))
  c(vapply(remaining, \(keep) d1_largest_component(binary[keep, keep, drop = FALSE]), numeric(1)), 0)
}

#' Weighted rich-club coefficient of Opsahl, Colizza, Panzarasa & Ramasco (2008)
#'
#' For each threshold r, the nodes with degree above r form the club; phi_w(r) is the summed weight of the ties
#' among them divided by the summed weight of the same number of the strongest ties in the whole network.
#'
#' @param graph A symmetric weighted matrix.
#' @param thresholds Degree thresholds.
#' @return A data frame with `threshold`, `n_rich` and `phi`.
d1_weighted_rich_club <- function(graph, thresholds) {
  binary <- d1_binary(graph)
  degree <- rowSums(binary)
  upper <- graph[upper.tri(graph)]
  ranked <- sort(upper[upper != 0], decreasing = TRUE)
  rows <- lapply(thresholds, \(r) {
    club <- degree > r
    ties <- graph[club, club, drop = FALSE]
    inside <- ties[upper.tri(ties)]
    inside <- inside[inside != 0]
    data.frame(threshold = r, n_rich = sum(club), phi = sum(inside) / sum(ranked[seq_along(inside)]))
  })
  do.call(rbind, rows)
}

#' Edge values keyed "u|v" as an edge table in the orientation cograph lists them
#'
#' An undirected edge is listed once, from the node that comes first in the matrix to the later one.
#'
#' @param values A numeric vector named "u|v".
#' @param graph The adjacency matrix the values were computed on.
#' @param directed Whether the graph is directed.
#' @param column Name of the value column.
#' @return A data frame with `from`, `to` and `column`.
d1_edge_table <- function(values, graph, directed, column) {
  ends <- do.call(rbind, strsplit(names(values), "|", fixed = TRUE))
  position <- match(ends, rownames(graph))
  dim(position) <- dim(ends)
  first <- if (directed) position[, 1L] else pmin(position[, 1L], position[, 2L])
  second <- if (directed) position[, 2L] else pmax(position[, 1L], position[, 2L])
  out <- data.frame(from = rownames(graph)[first], to = rownames(graph)[second])
  out[[column]] <- unname(as.numeric(values))
  out
}

#' Every tie of an adjacency matrix as a from/to table, in cograph's orientation
#'
#' @param graph A square matrix with dimnames.
#' @param directed Whether the graph is directed (an undirected tie is listed once, upper triangle).
#' @return A data frame with `from`, `to`, `row`, `col`.
d1_ties <- function(graph, directed) {
  cells <- which(graph != 0, arr.ind = TRUE)
  cells <- cells[cells[, 1L] != cells[, 2L], , drop = FALSE]
  if (!directed) cells <- cells[cells[, 1L] < cells[, 2L], , drop = FALSE]
  data.frame(from = rownames(graph)[cells[, 1L]], to = colnames(graph)[cells[, 2L]],
             row = cells[, 1L], col = cells[, 2L])
}

# Python (NetworkX) expressions used by several files. Weighted efficiencies (Latora & Marchiori 2001) read the
# edge weight as a distance: the mean of 1 / d(u, v) over ordered pairs of distinct nodes, 0 for an unreachable
# pair. Local efficiency, in the form network_local_efficiency() documents (igraph's): for each node, the mean of
# 1 / d(s, t) over ordered pairs of its neighbours (in and out on a directed graph), with d measured in the graph
# without that node (paths may leave the neighbourhood), averaged over nodes; fewer than two neighbours score 0.
# NetworkX's own local_efficiency() restricts d to the subgraph induced on the neighbours.
d1_py_global_efficiency <- paste0(
  "(lambda d: sum(1 / x for s in d.values() for x in s.values() if x > 0) / (len(G) * (len(G) - 1)))",
  "(dict(nx.all_pairs_dijkstra_path_length(G, weight='weight')))")
d1_py_local_efficiency <- paste0(
  "(lambda eff, nbrs: float(np.mean([eff(nbrs(v) - {v}, G.subgraph(nbrs(v) - {v})) for v in G])))(",
  "lambda N, H: 0.0 if len(N) < 2 else sum(1 / x for s in N ",
  "for t, x in nx.single_source_dijkstra_path_length(H, s, weight='weight').items() if t in N and t != s)",
  " / (len(N) * (len(N) - 1)), ",
  "lambda v: (set(G.successors(v)) | set(G.predecessors(v))) if G.is_directed() else set(G[v]))")
# The MAN class of every triple of nodes, as "a|b|c|type" with a < b < c in sorted node names.
d1_py_triad_types <- paste0(
  "[f'{a}|{b}|{c}|' + nx.triad_type(G.subgraph([a, b, c])) ",
  "for a, b, c in __import__('itertools').combinations(sorted(G), 3)]")

#' The triad types NetworkX returned, as a table keyed by the three nodes
#'
#' @param types The list `d1_py_triad_types` evaluated to.
#' @return A data frame with `node1`, `node2`, `node3`, `type`.
d1_triad_table <- function(types) {
  parts <- do.call(rbind, strsplit(unlist(types), "|", fixed = TRUE))
  data.frame(node1 = parts[, 1L], node2 = parts[, 2L], node3 = parts[, 3L], type = parts[, 4L])
}
