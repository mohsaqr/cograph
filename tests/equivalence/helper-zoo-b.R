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

# The three networks below are the netzschleuder copies retained as
# local_testing_and_equivalence/candidate_review57/profitleader_<name>_edges.csv, numbered as there; the node
# labels are in the _labels.csv files beside them.

#' Lusseau's bottlenose dolphins (62 nodes, 159 edges)
#' @return A 62-node undirected adjacency matrix.
zoob_dolphins <- function() {
  zoob_graph(62L, scan(quiet = TRUE, text = "
9 4 10 6 10 7 11 1 11 3 14 6 14 7 14 10 15 1 15 4 16 1 17 15 18 2 18 7 18 10 18 14 19 16 20 2 20 8 21 9
21 17 21 19 22 19 23 18 25 15 25 16 25 19 26 18 27 2 27 26 28 2 28 8 28 18 28 26 28 27 29 2 29 9 29 21
30 11 30 19 30 22 30 25 31 8 31 20 31 29 32 18 33 10 33 14 34 13 34 15 34 17 34 22 35 15 35 34 36 30 37
2 37 21 37 24 38 9 38 15 38 17 38 22 38 34 38 35 38 37 39 15 39 17 39 21 39 34 40 37 41 1 41 8 41 15 41
16 41 34 41 37 41 38 42 2 42 10 42 14 43 1 43 3 43 11 43 31 44 15 44 30 44 34 44 38 44 39 45 3 45 21 45
35 45 39 46 9 46 16 46 19 46 22 46 24 46 25 46 30 46 38 47 44 48 1 48 11 48 21 48 29 48 31 48 43 50 35
50 47 51 15 51 17 51 21 51 34 51 43 51 46 52 5 52 12 52 19 52 22 52 24 52 25 52 30 52 46 52 51 53 15 53
30 53 39 53 41 54 44 55 2 55 7 55 8 55 14 55 20 55 42 56 16 56 52 57 6 57 7 58 6 58 7 58 10 58 14 58 18
58 40 58 42 58 49 58 55 59 39 60 4 60 9 60 16 60 37 60 46 61 33 62 3 62 38 62 54"))
}

#' Newman's adjective-noun adjacencies of David Copperfield (112 nodes, 425 edges)
#' @return A 112-node undirected adjacency matrix.
zoob_adjnoun <- function() {
  zoob_graph(112L, scan(quiet = TRUE, text = "
2 1 3 1 3 2 4 1 4 3 6 5 7 6 8 7 10 2 10 3 10 4 10 9 11 4 12 3 12 11 13 10 13 11 15 2 15 3 15 14 16 8 16
14 17 14 17 15 18 2 18 3 18 6 18 8 18 10 18 13 18 14 18 15 18 16 19 2 19 4 19 13 19 14 20 2 21 20 22 3
22 8 22 18 22 19 22 20 23 3 23 18 23 20 24 3 24 20 25 3 25 10 25 18 25 20 26 3 26 10 26 15 26 18 26 20
27 3 27 10 27 18 27 20 28 3 28 6 28 18 28 20 29 2 29 18 29 22 30 19 30 29 31 26 31 29 32 3 32 18 32 29
33 6 33 7 33 16 33 18 33 29 33 32 34 29 35 3 35 18 35 19 35 29 36 18 36 29 37 3 37 15 37 18 37 25 38 8
38 15 38 18 39 8 39 16 39 18 39 38 40 6 40 10 40 39 41 10 41 12 41 39 42 2 42 3 42 4 42 26 42 32 42 39
43 3 43 10 43 42 44 4 44 6 44 13 44 16 44 18 44 19 44 21 44 22 44 24 44 25 44 27 44 28 44 32 44 37 44 40
44 42 45 18 45 19 45 29 45 44 46 2 46 3 46 23 46 45 47 2 47 26 48 24 48 25 48 38 49 16 49 22 49 26 49 36
49 44 49 48 50 25 50 48 51 3 51 11 51 13 51 18 51 19 51 22 51 45 51 49 52 2 52 3 52 4 52 7 52 8 52 18 52
19 52 22 52 24 52 26 52 27 52 31 52 32 52 33 52 35 52 36 52 39 52 43 52 50 52 51 53 7 53 18 53 28 53 33
53 46 53 51 54 19 54 51 55 3 55 18 55 19 55 28 55 35 55 51 55 52 55 54 56 7 56 37 57 27 57 41 57 43 57
53 58 38 58 52 59 30 59 38 60 10 60 15 60 24 60 26 60 38 60 44 60 50 60 52 61 10 61 19 62 10 63 10 64 12
64 28 64 44 65 64 66 18 66 60 66 64 67 3 67 18 67 44 67 52 68 18 68 52 69 15 69 17 69 25 69 27 69 28 69
50 69 52 69 55 70 12 70 24 71 8 71 18 71 33 71 44 71 55 71 56 71 60 71 69 72 15 72 36 72 60 72 61 72 63
73 18 73 38 73 55 73 60 73 67 74 3 74 46 74 54 74 69 74 72 75 30 76 8 76 16 76 25 76 55 76 58 76 67 76
69 76 71 77 16 77 25 77 28 77 34 77 49 77 61 77 68 77 74 77 76 78 18 78 26 78 76 79 51 79 55 80 16 80 21
80 27 80 28 80 30 80 31 80 32 80 43 80 79 81 18 81 27 81 28 81 44 81 73 81 77 82 44 82 80 82 81 83 18 84
3 84 18 84 44 84 80 85 18 85 31 85 34 85 66 85 69 86 4 86 18 87 18 87 32 88 8 88 13 88 18 88 25 88 28 88
37 88 66 88 68 88 71 88 84 89 13 89 16 89 18 89 26 89 49 90 5 90 10 90 18 90 29 90 54 91 43 92 2 93 29
93 31 93 77 93 90 94 3 94 18 95 21 95 23 95 82 96 93 97 28 97 93 98 3 99 3 99 12 99 18 99 44 99 82 100
44 100 90 101 33 101 44 102 52 102 61 103 2 103 3 103 13 103 14 103 18 103 45 103 51 103 54 103 67 103
71 104 18 104 25 104 28 104 32 104 38 104 40 104 66 104 74 105 3 105 12 105 13 105 16 105 18 105 25 105
26 105 27 105 32 105 34 105 36 105 39 105 47 105 49 105 51 105 60 105 62 105 82 105 85 105 104 106 3 106
15 106 24 106 71 106 73 106 105 107 8 107 52 107 77 107 81 107 89 107 97 108 34 108 35 109 21 109 25 109
32 109 106 110 80 111 80 111 101 112 3"))
}

#' Knuth's Les Miserables co-appearances, unweighted (77 nodes, 254 edges)
#' @return A 77-node undirected adjacency matrix.
zoob_lesmis <- function() {
  zoob_graph(77L, scan(quiet = TRUE, text = "
2 1 3 1 4 1 4 3 5 1 6 1 7 1 8 1 9 1 10 1 12 11 12 4 12 3 12 1 13 12 14 12 15 12 16 12 18 17 19 17 19 18
20 17 20 18 20 19 21 17 21 18 21 19 21 20 22 17 22 18 22 19 22 20 22 21 23 17 23 18 23 19 23 20 23 21 23
22 24 17 24 18 24 19 24 20 24 21 24 22 24 23 24 13 24 12 25 24 25 12 26 25 26 24 26 12 27 25 27 12 27 17
27 26 28 12 28 24 28 26 28 25 28 27 29 12 29 28 30 24 30 28 30 12 31 24 32 31 32 12 32 24 32 28 33 12 34
12 34 28 35 12 35 30 36 12 36 35 36 30 37 35 37 36 37 12 37 30 38 35 38 36 38 37 38 12 38 30 39 35 39 36
39 37 39 38 39 12 39 30 40 26 41 26 42 25 42 26 43 42 43 26 43 25 44 12 44 27 44 28 45 29 45 12 46 29 48
47 49 48 49 26 49 28 49 12 50 27 50 12 51 50 51 25 52 50 52 27 52 12 53 52 53 40 54 52 55 52 55 50 55 27
56 52 56 50 56 40 56 55 56 27 56 12 56 17 56 26 56 42 56 49 57 50 57 56 58 56 58 42 58 49 59 56 59 49 59
28 59 58 59 12 60 59 60 56 60 49 60 58 61 49 61 59 61 60 62 49 62 59 62 61 62 60 62 58 62 56 63 56 63 59
63 60 63 49 63 58 63 42 63 62 63 61 64 60 64 49 64 63 64 58 64 59 64 62 64 61 64 56 65 56 65 63 65 49 65
64 65 59 65 62 65 61 65 60 65 58 65 12 66 64 66 65 66 49 66 63 66 59 66 62 66 61 66 60 66 58 66 56 67 65
67 59 67 60 67 63 67 66 67 49 67 64 67 62 67 61 68 58 69 26 69 12 69 25 69 28 69 49 69 42 70 26 70 69 70
12 70 25 70 28 70 49 70 42 71 26 71 70 71 69 71 12 71 25 71 28 71 42 71 59 72 28 72 70 72 69 72 71 72 12
72 49 72 42 72 26 73 27 73 28 73 12 74 49 75 49 75 74 76 70 76 69 76 26 76 49 76 42 76 71 76 72 77 65 77
66 77 67 77 64 77 63 77 49 77 59"))
}
