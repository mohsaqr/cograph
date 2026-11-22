# Inputs drawn by simulab (the owner's simulation package, installed where the job runs): networks of other
# shapes than ref_graph() gives (scale-free, small-world, block, with isolates, multiplex) and sequences from a
# known transition matrix. simulab draws the input; it is never the reference. Nothing here calls cograph.

#' A simulab network as an adjacency matrix
#'
#' @param nodes Number of nodes.
#' @param seed The seed of the draw.
#' @param ... Passed to `simulab::simulate_network()` (`model`, `directed`, `weight`, `probability`, ...).
#' @return A square numeric matrix with simulab's node labels as dimnames; symmetric when undirected.
sim_graph <- function(nodes, seed, ...) {
  drawn <- simulab::simulate_network(nodes = nodes, seed = seed, ...)
  cells <- as.data.frame(drawn, what = "adjacency")
  labels <- as.character(as.data.frame(drawn, what = "nodes")$node)
  graph <- matrix(0, nodes, nodes, dimnames = list(labels, labels))
  graph[cbind(cells$row, cells$column)] <- cells$weight
  graph
}

#' A simulab block-model network with its blocks
#'
#' The blocks are passed as `node_type`, contiguous, so simulab's block index (`match(node_type, types)`) and
#' igraph::sample_sbm()'s contiguous blocks are the same nodes.
#'
#' @param sizes Block sizes.
#' @param seed The seed of the draw.
#' @param ... Passed to `simulab::simulate_network()` (`within_probability`, `between_probability`, ...).
#' @return A list: `graph` (a symmetric 0/1 matrix) and `blocks` (the block label of each node, named by node).
sim_blocks <- function(sizes, seed, ...) {
  types <- rep(sprintf("b%d", seq_along(sizes)), sizes)
  graph <- sim_graph(sum(sizes), seed, model = "block", directed = FALSE, node_type = types, ...)
  list(graph = graph, blocks = stats::setNames(types, rownames(graph)))
}

#' Binary layers of a simulab multiplex network, one adjacency matrix per layer
#'
#' @param nodes Number of nodes.
#' @param layers Number of layers.
#' @param seed The seed of the draw.
#' @param ... Passed to `simulab::simulate_multiplex_network()`.
#' @return A named list of square 0/1 matrices over the same nodes.
sim_layers <- function(nodes, layers, seed, ...) {
  drawn <- simulab::simulate_multiplex_network(nodes = nodes, layers = layers, seed = seed, ...)
  edges <- as.data.frame(drawn)
  labels <- as.character(as.data.frame(drawn, what = "nodes")$node)
  names_of <- unique(as.character(edges$layer))
  stats::setNames(lapply(names_of, \(layer) {
    graph <- matrix(0, nodes, nodes, dimnames = list(labels, labels))
    rows <- edges[edges$layer == layer, , drop = FALSE]
    graph[cbind(as.character(rows$from), as.character(rows$to))] <- 1
    graph
  }), names_of)
}

#' Sequences drawn by simulab from a known transition matrix, one row per sequence
#'
#' @param n Number of sequences.
#' @param seed The seed of the draw.
#' @param ... Passed to `simulab::simulate_sequences()` (`n_states`, `chain_length`, ...).
#' @return A data frame of states, columns `S1`, `S2`, ... (the wide form tna reads).
sim_sequences <- function(n, seed, ...) {
  wide <- as.data.frame(simulab::simulate_sequences(n = n, seed = seed, ...), what = "wide")
  wide[, grep("^S[0-9]+$", names(wide)), drop = FALSE]
}
