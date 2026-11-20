# Two comparisons moved from tests/testthat/test-centrality-extended.R: weighted stress against an enumeration of
# igraph's shortest paths, and expected influence against qgraph.
#
# Stress (Shimbel 1953) counts, for each node, the shortest paths between other nodes that pass through it.
# The weights are whole numbers 1 to 3 (read as distances, cograph's default for path measures), so that many
# pairs have several shortest paths of exactly equal length: the count then depends on finding all of them.

undirected_weighted <- ceiling(3 * ref_connected_graph(8, density = 0.4, seed = 65L, weighted = TRUE, digits = 6L))
directed_weighted <- ceiling(3 * ref_connected_graph(8, density = 0.3, seed = 66L, directed = TRUE, weighted = TRUE, digits = 6L))

# Expected influence (Robinaugh, Millner and McNally 2016) needs signed weights: a random weight matrix with the
# sign of a second random 0/1 pattern.
signed <- ref_graph(9, density = 0.7, seed = 62L, weighted = TRUE, digits = 6L) * (1 - 2 * ref_graph(9, density = 0.5, seed = 61L))
signed_directed <- ref_graph(9, density = 0.5, seed = 64L, directed = TRUE, weighted = TRUE, digits = 6L) *
  (1 - 2 * ref_graph(9, density = 0.5, seed = 63L, directed = TRUE))

# Counts of paths, and sums of at most n - 1 weights.
exact <- 1e-10

#' Stress from every shortest path igraph lists
#'
#' @param graph A weighted adjacency matrix with dimnames.
#' @param directed Whether the paths follow the direction of the ties.
#' @return The number of shortest paths between other nodes that pass through each node, named by node.
enumerated_stress <- function(graph, directed) {
  network <- ref_igraph(graph, directed = directed, weighted = TRUE)
  pairs <- if (directed) which(row(graph) != col(graph), arr.ind = TRUE) else which(upper.tri(graph), arr.ind = TRUE)
  interior <- unlist(lapply(seq_len(nrow(pairs)), \(k) {
    paths <- igraph::all_shortest_paths(network, from = pairs[k, 1L], to = pairs[k, 2L], mode = "out")$vpaths
    lapply(paths, \(path) as.integer(path)[-c(1L, length(path))])
  }))
  ref_named(tabulate(interior, nbins = nrow(graph)), graph)
}

test_that("weighted stress equals the count of igraph's all_shortest_paths()", {
  skip_without_reference("igraph")
  expect_reference(centrality_stress(undirected_weighted), enumerated_stress(undirected_weighted, directed = FALSE),
                   reference = "igraph", covers = c("centrality()", "centrality_stress()"),
                   quantity = "stress, undirected, integer weights with tied paths", tolerance = exact)
  expect_reference(centrality_stress(directed_weighted), enumerated_stress(directed_weighted, directed = TRUE),
                   reference = "igraph", covers = c("centrality()", "centrality_stress()"),
                   quantity = "stress, directed, integer weights with tied paths", tolerance = exact)
})

test_that("expected influence equals qgraph's centrality() ExpectedInfluence", {
  skip_without_reference("qgraph")
  on_undirected <- qgraph::centrality(signed)
  on_directed <- qgraph::centrality(signed_directed)
  expect_reference(centrality_expected_influence_1(signed), ref_named(on_undirected$OutExpectedInfluence, signed),
                   reference = "qgraph", covers = c("centrality()", "centrality_expected_influence_1()"),
                   quantity = "expected influence 1, signed undirected", tolerance = exact)
  expect_reference(centrality_expected_influence_1(signed_directed, mode = "out"),
                   ref_named(on_directed$OutExpectedInfluence, signed_directed),
                   reference = "qgraph", covers = c("centrality()", "centrality_expected_influence_1()"),
                   quantity = "expected influence 1, mode out, signed directed", tolerance = exact)
  expect_reference(centrality_expected_influence_1(signed_directed, mode = "in"),
                   ref_named(on_directed$InExpectedInfluence, signed_directed),
                   reference = "qgraph", covers = c("centrality()", "centrality_expected_influence_1()"),
                   quantity = "expected influence 1, mode in, signed directed", tolerance = exact)
})

test_that("two-step expected influence equals its definition", {
  # Robinaugh, Millner and McNally (2016): EI2_i = EI1_i + sum_j w_ij EI1_j, with EI1_i = sum_j w_ij, on the
  # signed weights. Written here on the matrix; a few additions and products of doubles.
  one_step <- rowSums(signed)
  expect_reference(centrality_expected_influence_2(signed), one_step + as.vector(signed %*% one_step),
                   reference = "independent: the definition of expected influence (Robinaugh et al. 2016), base R",
                   covers = c("centrality()", "centrality_expected_influence_2()"),
                   quantity = "expected influence 2, signed undirected", tolerance = exact)
})
