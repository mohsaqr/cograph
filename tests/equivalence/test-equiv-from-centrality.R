# Equivalence tests moved from tests/testthat/test-centrality.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
skip_if_not_installed("igraph")
skip_on_cran()
.test_mat <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(.test_mat) <- colnames(.test_mat) <- LETTERS[1:5]
.test_g <- igraph::graph_from_adjacency_matrix(.test_mat, mode = "undirected")

# ---- equivalence tests ----

test_that("degree matches igraph", {
  expect_equal(
    unname(centrality_degree(.test_mat)),
    unname(igraph::degree(.test_g))
  )
})

test_that("betweenness matches igraph", {
  expect_equal(
    unname(centrality_betweenness(.test_mat)),
    unname(igraph::betweenness(.test_g))
  )
})

test_that("closeness matches igraph", {
  expect_equal(
    unname(centrality_closeness(.test_mat)),
    unname(igraph::closeness(.test_g))
  )
})

test_that("eigenvector matches igraph", {
  expect_equal(
    unname(centrality_eigenvector(.test_mat)),
    unname(igraph::eigen_centrality(.test_g)$vector)
  )
})

test_that("pagerank matches igraph", {
  expect_equal(
    unname(centrality_pagerank(.test_mat)),
    unname(igraph::page_rank(.test_g)$vector)
  )
})

test_that("harmonic matches igraph", {
  expect_equal(
    unname(centrality_harmonic(.test_mat)),
    unname(igraph::harmonic_centrality(.test_g))
  )
})

test_that("alpha (Katz) matches igraph", {
  expect_equal(
    unname(centrality_alpha(.test_mat)),
    unname(igraph::alpha_centrality(.test_g, exo = 1)),
    tolerance = 1e-6
  )
})

test_that("subgraph matches igraph", {
  expect_equal(
    unname(centrality_subgraph(.test_mat)),
    unname(igraph::subgraph_centrality(.test_g, diag = FALSE)),
    tolerance = 1e-6
  )
})

test_that("power (Bonacich) matches igraph", {
  expect_equal(
    unname(centrality_power(.test_mat)),
    unname(igraph::power_centrality(.test_g, exponent = 1)),
    tolerance = 1e-6
  )
})

test_that("edge_betweenness matches igraph", {
  expect_equal(
    unname(edge_centrality(.test_mat)$betweenness),
    unname(igraph::edge_betweenness(.test_g)),
    tolerance = 1e-6
  )
})

test_that("laplacian matches centiserve", {
  skip_if_not_installed("centiserve")
  expect_equal(
    unname(centrality_laplacian(.test_mat)),
    unname(centiserve::laplacian(.test_g)),
    tolerance = 1e-6
  )
})

test_that("current_flow_closeness matches centiserve", {
  skip_if_not_installed("centiserve")
  expect_equal(
    unname(centrality_current_flow_closeness(.test_mat)),
    unname(centiserve::closeness.currentflow(.test_g)),
    tolerance = 1e-6
  )
})

test_that("load matches sna::loadcent", {
  skip_if_not_installed("sna")
  sna_load <- sna::loadcent(.test_mat, gmode = "graph")
  expect_equal(
    unname(centrality_load(.test_mat)),
    unname(sna_load),
    tolerance = 1e-6
  )
})

test_that("diffusion matches centiserve", {
  skip_if_not_installed("centiserve")
  expect_equal(
    unname(centrality_diffusion(.test_mat)),
    unname(centiserve::diffusion.degree(.test_g)),
    tolerance = 1e-6
  )
})

test_that("leverage matches centiserve", {
  skip_if_not_installed("centiserve")
  expect_equal(
    unname(centrality_leverage(.test_mat)),
    unname(centiserve::leverage(.test_g)),
    tolerance = 1e-6
  )
})

test_that("kreach matches centiserve::geokpath", {
  skip_if_not_installed("centiserve")
  expect_equal(
    unname(centrality_kreach(.test_mat, k = 3)),
    unname(centiserve::geokpath(.test_g, k = 3)),
    tolerance = 1e-6
  )
})

test_that("current_flow_betweenness matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_cfb <- nx$current_flow_betweenness_centrality(G)
  nx_cfb_vec <- sapply(LETTERS[1:5], function(x) nx_cfb[[x]])

  expect_equal(
    unname(centrality_current_flow_betweenness(.test_mat)),
    unname(nx_cfb_vec),
    tolerance = 1e-5
  )
})

test_that("percolation matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  states <- reticulate::py_dict(LETTERS[1:5], rep(1.0, 5))
  nx_perc <- nx$percolation_centrality(G, states = states)
  nx_perc_vec <- sapply(LETTERS[1:5], function(x) nx_perc[[x]])

  expect_equal(
    unname(centrality_percolation(.test_mat)),
    unname(nx_perc_vec),
    tolerance = 1e-6
  )
})

test_that("laplacian matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_lap <- nx$laplacian_centrality(G, normalized = FALSE)
  nx_lap_vec <- sapply(LETTERS[1:5], function(x) nx_lap[[x]])

  expect_equal(
    unname(centrality_laplacian(.test_mat)),
    unname(nx_lap_vec),
    tolerance = 1e-6
  )
})

test_that("voterank matches NetworkX ordering", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_vr <- unlist(nx$voterank(G))
  cg_vr <- centrality_voterank(.test_mat)
  cg_order <- names(sort(cg_vr, decreasing = TRUE))

  # Top spreaders should match in order
  expect_equal(cg_order[1:length(nx_vr)], nx_vr)
})
