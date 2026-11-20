# Distance and degree sums that no reference package computes per node, checked against their definitions written
# out in base R on hop distances from helper-centrality-b.R: Harary centrality (sum of 1 / d^2; Plavsic et al.
# 1993's Harary index split by node, with the squared distances cograph documents), the Wiener sum of distances
# (Wiener 1947), and expected centrality (sum of the neighbours' degrees).

undirected <- ref_connected_graph(12, density = 0.3, seed = 21L)
directed <- ref_connected_graph(12, density = 0.25, seed = 22L, directed = TRUE)

# Sums of at most 11 small rationals: rounding of the last bits only.
exact <- 1e-10

test_that("Harary centrality equals the sum of inverse squared distances", {
  distances <- ref_hop_distances(undirected)
  inverse_squares <- ifelse(is.finite(distances) & distances > 0, 1 / distances^2, 0)
  expect_reference(centrality_harary(undirected), rowSums(inverse_squares),
                   reference = "independent: sum of 1 / d^2 over the other nodes, base R",
                   covers = c("centrality()", "centrality_harary()"), quantity = "harary, undirected", tolerance = exact)
})

test_that("the Wiener sum equals the sum of distances", {
  distances <- ref_hop_distances(undirected)
  distances_out <- ref_hop_distances(directed)
  expect_reference(centrality_wiener(undirected), rowSums(distances),
                   reference = "independent: sum of distances to the other nodes, base R",
                   covers = c("centrality()", "centrality_wiener()"), quantity = "wiener, undirected", tolerance = exact)
  expect_reference(centrality_wiener(directed, mode = "out"), rowSums(distances_out),
                   reference = "independent: sum of distances to the other nodes, base R",
                   covers = c("centrality()", "centrality_wiener()"), quantity = "wiener, mode out, directed",
                   tolerance = exact)
})

test_that("expected centrality equals the sum of the neighbours' degrees", {
  expect_reference(centrality_expected(undirected), as.vector(undirected %*% rowSums(undirected)),
                   reference = "independent: sum of the neighbours' degrees, base R",
                   covers = c("centrality()", "centrality_expected()"), quantity = "expected, undirected", tolerance = exact)
})
