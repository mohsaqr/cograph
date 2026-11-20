# Small graphs shared by the test-centrality-zoo-b-*.R files (the measures of cograph's centrality batches 26 to
# 51), in base R. Nothing here calls cograph: each function builds an adjacency matrix both sides read.

#' An undirected (or directed) 0/1 adjacency matrix from an edge list
#'
#' @param n Number of nodes.
#' @param edges A two-column matrix of node indices, one row per edge, or a vector of pairs `c(u1, v1, u2, v2, ...)`.
#' @param directed Whether each row is one arc `u -> v` (default: an undirected edge).
#' @param labels Node names (default `"1"`, `"2"`, ...).
#' @return A square numeric matrix with dimnames `labels`.
zoob_graph <- function(n, edges, directed = FALSE, labels = as.character(seq_len(n))) {
  if (!is.matrix(edges)) edges <- matrix(edges, ncol = 2L, byrow = TRUE)
  stopifnot("`edges` must name nodes 1 to n" = all(edges >= 1 & edges <= n),
            "one label per node is needed" = length(labels) == n)
  graph <- matrix(0, n, n, dimnames = list(labels, labels))
  graph[edges] <- 1
  if (!directed) graph[edges[, 2:1, drop = FALSE]] <- 1
  graph
}

#' The path 1 - 2 - ... - n
#' @param n Number of nodes.
#' @return An undirected adjacency matrix.
zoob_path <- function(n) zoob_graph(n, cbind(seq_len(n - 1L), seq_len(n - 1L) + 1L))

#' The cycle 1 - 2 - ... - n - 1 (directed 1 -> 2 -> ... -> n -> 1 when `directed`)
#' @param n Number of nodes (at least 3).
#' @param directed Whether the cycle is directed.
#' @return An adjacency matrix.
zoob_ring <- function(n, directed = FALSE) {
  zoob_graph(n, cbind(seq_len(n), c(seq_len(n - 1L) + 1L, 1L)), directed = directed)
}

#' A star: node 1 joined to `leaves` leaves
#' @param leaves Number of leaves.
#' @return An undirected adjacency matrix with `leaves + 1` nodes.
zoob_star <- function(leaves) zoob_graph(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))

#' The complete graph on n nodes
#' @param n Number of nodes.
#' @return An undirected adjacency matrix.
zoob_complete <- function(n) {
  graph <- matrix(1, n, n, dimnames = list(as.character(seq_len(n)), as.character(seq_len(n))))
  diag(graph) <- 0
  graph
}

#' Krackhardt's kite, numbered as in cograph's batch 47 to 50 tests
#' @return A 10-node undirected adjacency matrix.
zoob_kite <- function() {
  zoob_graph(10L, c(1, 2, 1, 3, 1, 4, 1, 6, 2, 4, 2, 5, 2, 7, 3, 4, 3, 6, 4, 5, 4, 6, 4, 7, 5, 7, 6, 7,
                    6, 8, 7, 8, 8, 9, 9, 10))
}

#' Zachary's karate club (34 nodes, 78 edges), the edge list igraph's make_graph("Zachary") holds
#' @return A 34-node undirected adjacency matrix.
zoob_karate <- function() {
  zoob_graph(34L, c(1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 1, 7, 1, 8, 1, 9, 1, 11, 1, 12, 1, 13, 1, 14, 1, 18, 1, 20,
                    1, 22, 1, 32, 2, 3, 2, 4, 2, 8, 2, 14, 2, 18, 2, 20, 2, 22, 2, 31, 3, 4, 3, 8, 3, 28, 3, 29,
                    3, 33, 3, 10, 3, 9, 3, 14, 4, 8, 4, 13, 4, 14, 5, 7, 5, 11, 6, 7, 6, 11, 6, 17, 7, 17, 9, 31,
                    9, 33, 9, 34, 10, 34, 14, 34, 15, 33, 15, 34, 16, 33, 16, 34, 19, 33, 19, 34, 20, 34, 21, 33,
                    21, 34, 23, 33, 23, 34, 24, 26, 24, 28, 24, 33, 24, 34, 24, 30, 25, 26, 25, 28, 25, 32, 26, 32,
                    27, 30, 27, 34, 28, 34, 29, 32, 29, 34, 30, 33, 30, 34, 31, 33, 31, 34, 32, 33, 32, 34, 33, 34))
}

#' Graphs side by side as one block-diagonal adjacency matrix
#' @param ... Adjacency matrices.
#' @return A square matrix; nodes are renumbered `"1"`, `"2"`, ... in the order given.
zoob_union <- function(...) {
  parts <- list(...)
  sizes <- vapply(parts, nrow, integer(1))
  n <- sum(sizes)
  ends <- cumsum(sizes)
  starts <- ends - sizes + 1L
  empty <- matrix(0, n, n, dimnames = list(as.character(seq_len(n)), as.character(seq_len(n))))
  Reduce(\(graph, k) {
    block <- seq.int(starts[[k]], ends[[k]])
    graph[block, block] <- unname(parts[[k]])
    graph
  }, seq_along(parts), empty)
}
