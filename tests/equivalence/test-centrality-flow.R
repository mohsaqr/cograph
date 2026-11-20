# centrality() and its wrappers for the electrical-flow measures: current-flow betweenness, current-flow closeness,
# information centrality and spanning-tree centrality. All four come from the pseudo-inverse of the graph
# Laplacian, with a weight read as a conductance (a strength), as NetworkX reads `weight`. cograph computes them
# natively (svd() and solve()), not through igraph.
#
# Scales, read from the sources before the run (R_ij is the effective resistance between i and j):
# - cograph's current-flow closeness is (n - 1) / sum_j R_ij; NetworkX's current_flow_closeness_centrality() is
#   1 / sum_j R_ij. The NetworkX value is multiplied by n - 1 in the test; no ratio between nodes changes.
# - cograph's information centrality is Stephenson and Zelen's (1989) I_i = n / sum_j R_ij, as sna::infocent();
#   NetworkX's information_centrality() is its current_flow_closeness_centrality(), 1 / sum_j R_ij, and is
#   multiplied by n in the test.

undirected <- ref_connected_graph(12, density = 0.25, seed = 51L)
undirected_weighted <- ref_connected_graph(12, density = 0.25, seed = 52L, weighted = TRUE, digits = 6L)
# The 5-node graph of the earlier tests.
small <- matrix(c(0, 1, 1, 0, 0,
                  1, 0, 1, 1, 0,
                  1, 1, 0, 1, 1,
                  0, 1, 1, 0, 1,
                  0, 0, 1, 1, 0), 5, 5, byrow = TRUE, dimnames = list(LETTERS[1:5], LETTERS[1:5]))

# Linear solves and symmetric decompositions of matrices of order 12.
exact <- 1e-10

test_that("current-flow betweenness equals NetworkX's current_flow_betweenness_centrality(normalized = True)", {
  # Brandes and Fleischer (2005); both scale the summed throughput by 2 / ((n - 1)(n - 2)), NetworkX's
  # normalized=True.
  call <- "nx.current_flow_betweenness_centrality(G, normalized=True, weight='weight')"
  on_weighted <- ref_networkx(undirected_weighted, c(cfb = call))
  on_unweighted <- ref_networkx(undirected, c(cfb = call))
  expect_reference(centrality_current_flow_betweenness(undirected_weighted), on_weighted$cfb, reference = "networkx",
                   covers = c("centrality()", "centrality_current_flow_betweenness()"),
                   quantity = "current-flow betweenness, undirected weighted", tolerance = exact)
  expect_reference(centrality_current_flow_betweenness(undirected), on_unweighted$cfb, reference = "networkx",
                   covers = c("centrality()", "centrality_current_flow_betweenness()"),
                   quantity = "current-flow betweenness, undirected", tolerance = exact)
})

test_that("current-flow closeness equals NetworkX's current_flow_closeness_centrality() times n - 1", {
  call <- c(cfc = "nx.current_flow_closeness_centrality(G, weight='weight')")
  on_weighted <- ref_networkx(undirected_weighted, call)
  on_unweighted <- ref_networkx(undirected, call)
  expect_reference(centrality_current_flow_closeness(undirected_weighted), (nrow(undirected_weighted) - 1) * on_weighted$cfc,
                   reference = "networkx", covers = c("centrality()", "centrality_current_flow_closeness()"),
                   quantity = "current-flow closeness, undirected weighted (NetworkX times n - 1)", tolerance = exact)
  expect_reference(centrality_current_flow_closeness(undirected), (nrow(undirected) - 1) * on_unweighted$cfc,
                   reference = "networkx", covers = c("centrality()", "centrality_current_flow_closeness()"),
                   quantity = "current-flow closeness, undirected (NetworkX times n - 1)", tolerance = exact)
})

test_that("current-flow closeness equals (n - 1) over the summed effective resistances", {
  # Brandes and Fleischer (2005): current-flow closeness is information centrality; resistances from the
  # eigen-decomposition of the Laplacian (helper-a1.R), where cograph uses svd().
  expect_reference(centrality_current_flow_closeness(small), (nrow(small) - 1) / rowSums(ref_resistance(small)),
                   reference = "independent: effective resistances from the Laplacian pseudo-inverse by base R eigen()",
                   covers = c("centrality()", "centrality_current_flow_closeness()"),
                   quantity = "current-flow closeness, 5-node graph", tolerance = exact)
  expect_reference(centrality_current_flow_closeness(undirected_weighted),
                   (nrow(undirected_weighted) - 1) / rowSums(ref_resistance(undirected_weighted)),
                   reference = "independent: effective resistances from the Laplacian pseudo-inverse by base R eigen()",
                   covers = c("centrality()", "centrality_current_flow_closeness()"),
                   quantity = "current-flow closeness, undirected weighted", tolerance = exact)
})

test_that("current-flow closeness equals centiserve's closeness.currentflow()", {
  skip_without_reference(c("centiserve", "igraph"))
  expect_reference(centrality_current_flow_closeness(undirected),
                   ref_named(centiserve::closeness.currentflow(ref_igraph(undirected)), undirected),
                   reference = "centiserve", covers = c("centrality()", "centrality_current_flow_closeness()"),
                   quantity = "current-flow closeness, undirected", tolerance = exact)
})

test_that("information centrality equals sna's infocent()", {
  skip_without_reference("sna")
  expect_reference(centrality_information(undirected), ref_named(sna::infocent(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_information()"),
                   quantity = "information, undirected", tolerance = exact)
})

test_that("information centrality equals NetworkX's information_centrality() times n", {
  nx <- ref_networkx(undirected_weighted, c(information = "nx.information_centrality(G, weight='weight')"))
  expect_reference(centrality_information(undirected_weighted), nrow(undirected_weighted) * nx$information,
                   reference = "networkx", covers = c("centrality()", "centrality_information()"),
                   quantity = "information, undirected weighted (NetworkX times n)", tolerance = exact)
})

test_that("spanning-tree centrality equals 1 / L+_ii", {
  # cograph's definition: the inverse diagonal of the Moore-Penrose pseudo-inverse of the (weighted) Laplacian.
  # cograph inverts L + J/n; the reference takes the pseudo-inverse from the eigen-decomposition.
  expect_reference(centrality(undirected_weighted, measures = "spanning_tree"),
                   ref_nodes(spanning_tree = 1 / diag(ref_laplacian_pinv(undirected_weighted))),
                   reference = "independent: Laplacian pseudo-inverse by base R eigen()", covers = "centrality()",
                   quantity = "spanning_tree, undirected weighted", tolerance = exact, key = "node")
  expect_reference(centrality(undirected, measures = "spanning_tree"),
                   ref_nodes(spanning_tree = 1 / diag(ref_laplacian_pinv(undirected))),
                   reference = "independent: Laplacian pseudo-inverse by base R eigen()", covers = "centrality()",
                   quantity = "spanning_tree, undirected", tolerance = exact, key = "node")
})
