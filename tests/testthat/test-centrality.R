# Tests for centrality functions

test_that("centrality works with adjacency matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("node" %in% names(result))
})

test_that("centrality works with specific measures", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat, measures = c("degree", "betweenness"))
  expect_true(is.data.frame(result))
  expect_true("degree_all" %in% names(result))
  expect_true("betweenness" %in% names(result))
})

test_that("centrality_degree works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality_degree(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_strength works", {
  mat <- matrix(c(0, 0.5, 0.8, 0.5, 0, 0.3, 0.8, 0.3, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality_strength(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_betweenness works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_betweenness(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_closeness works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_closeness(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_eigenvector works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_eigenvector(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_pagerank works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_pagerank(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
  expect_true(all(result >= 0))
})

test_that("centrality with normalization works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality(mat, normalized = TRUE)
  expect_true(is.data.frame(result))
  # All normalized values should be <= 1
  numeric_cols <- sapply(result, is.numeric)
  for (col in names(result)[numeric_cols]) {
    expect_true(all(result[[col]] <= 1, na.rm = TRUE))
  }
})

test_that("centrality with directed network works", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Use specific measures to avoid alpha centrality singularity on small matrices
  result <- centrality(mat, directed = TRUE, mode = "in",
                       measures = c("degree", "betweenness", "closeness"))
  expect_true(is.data.frame(result))
  expect_true("degree_in" %in% names(result))
})

test_that("centrality with cograph_network works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  result <- centrality(net)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("centrality with igraph object works", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- centrality(g)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
})

test_that("centrality with sorting works", {
  mat <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- centrality(mat, sort_by = "degree_all")
  expect_true(is.data.frame(result))
  # Check descending order
  expect_true(result$degree_all[1] >= result$degree_all[nrow(result)])
})

test_that("centrality with digits rounding works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality(mat, digits = 2)
  expect_true(is.data.frame(result))
})

test_that("centrality errors on invalid measures", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  expect_error(centrality(mat, measures = "invalid_measure"))
})

# ═══════════════════════════════════════════════════════════════════════════════
# Mathematical Equivalence Tests
# ═══════════════════════════════════════════════════════════════════════════════

# Test network used for validation
.test_mat <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(.test_mat) <- colnames(.test_mat) <- LETTERS[1:5]
.test_g <- igraph::graph_from_adjacency_matrix(.test_mat, mode = "undirected")

# --- igraph-backed measures ---

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

# --- centiserve package comparison ---

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

# --- sna package comparison ---

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

# --- NetworkX package comparison (via reticulate) ---

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

# --- Manual verification (for current_flow_closeness) ---

test_that("current_flow_closeness matches manual pseudoinverse calculation", {
  n <- 5
  L <- igraph::laplacian_matrix(.test_g, sparse = FALSE)
  J <- matrix(1, n, n)
  L_tilde <- L - J / n
  svd_result <- svd(L_tilde)
  tol <- max(dim(L_tilde)) * max(svd_result$d) * .Machine$double.eps
  positive <- svd_result$d > tol
  L_pinv <- svd_result$v[, positive, drop = FALSE] %*%
    diag(1 / svd_result$d[positive], nrow = sum(positive)) %*%
    t(svd_result$u[, positive, drop = FALSE])
  diag_L_pinv <- diag(L_pinv)

  manual_cfc <- numeric(n)
  for (i in 1:n) {
    total_resistance <- 0
    for (j in 1:n) {
      if (i != j) {
        R_ij <- diag_L_pinv[i] + diag_L_pinv[j] - 2 * L_pinv[i, j]
        total_resistance <- total_resistance + R_ij
      }
    }
    manual_cfc[i] <- (n - 1) / total_resistance
  }

  expect_equal(
    unname(centrality_current_flow_closeness(.test_mat)),
    manual_cfc,
    tolerance = 1e-6
  )
})

# --- Property-based tests (fallback when NetworkX unavailable) ---

test_that("percolation equals betweenness with uniform states", {
  # When all states are 1, percolation should equal normalized betweenness
  perc <- centrality_percolation(.test_mat)
  betw <- centrality_betweenness(.test_mat)

  # Percolation should be 0 where betweenness is 0
  expect_true(all(perc[betw == 0] == 0))

  # Should equal normalized betweenness (correlation = 1)
  expect_equal(cor(perc, betw), 1, tolerance = 1e-6)
})

test_that("voterank returns valid scores", {
  vr <- centrality_voterank(.test_mat)

  # Node C has highest degree (4), should rank highest
  expect_equal(unname(vr["C"]), max(vr))

  # All values should be in [0, 1]
  expect_true(all(vr >= 0 & vr <= 1))
})
