# Tests for centrality functions

skip_on_cran()

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

test_that("diffusion power_series matches tna::Diffusion byte-for-byte", {
  skip_if_not_installed("tna")
  W <- matrix(c(0.4, 0.3, 0.3,
                0.2, 0.5, 0.3,
                0.1, 0.2, 0.7), 3, 3, byrow = TRUE)
  rownames(W) <- colnames(W) <- c("A", "B", "C")
  t1 <- tna::tna(W)

  tna_diff <- tna::centralities(t1, measures = "Diffusion")$Diffusion

  # tna_network auto-detects from class: tna input -> power_series + loops=FALSE
  cog_auto <- cograph::centrality(t1, measures = "diffusion")$diffusion_all
  expect_equal(cog_auto, tna_diff, tolerance = 1e-12)

  # Explicit method on raw matrix produces the same result.
  cog_explicit <- cograph::centrality(W, measures = "diffusion",
                                       diffusion_method = "power_series",
                                       loops = FALSE)$diffusion_all
  expect_equal(cog_explicit, tna_diff, tolerance = 1e-12)

  # tna_network = TRUE on raw matrix flips both diffusion_method and loops.
  cog_umbrella <- cograph::centrality(W, measures = "diffusion",
                                       tna_network = TRUE)$diffusion_all
  expect_equal(cog_umbrella, tna_diff, tolerance = 1e-12)

  # Default kandhway_kuri on a raw matrix is the binary-degree formula —
  # different by construction from tna's power-series.
  cog_kk <- cograph::centrality(W, measures = "diffusion")$diffusion_all
  expect_false(isTRUE(all.equal(cog_kk, tna_diff, tolerance = 1e-3)))

  # tna_network = FALSE on tna input opts out and returns cograph defaults.
  cog_off <- cograph::centrality(t1, measures = "diffusion",
                                  tna_network = FALSE)$diffusion_all
  expect_equal(cog_off, cog_kk, tolerance = 1e-12)
})

test_that("transitivity onnela matches tna::Clustering byte-for-byte", {
  skip_if_not_installed("tna")
  W <- matrix(c(0.4, 0.3, 0.3,
                0.2, 0.5, 0.3,
                0.1, 0.2, 0.7), 3, 3, byrow = TRUE)
  rownames(W) <- colnames(W) <- c("A", "B", "C")
  t1 <- tna::tna(W)

  tna_clust <- tna::centralities(t1, measures = "Clustering")$Clustering

  # tna input via auto tna_network picks "onnela".
  cog_auto <- cograph::centrality(t1, measures = "transitivity")$transitivity
  expect_equal(cog_auto, tna_clust, tolerance = 1e-12)

  # Explicit type on raw matrix matches.
  cog_explicit <- cograph::centrality(W, measures = "transitivity",
                                       transitivity_type = "onnela")$transitivity
  expect_equal(cog_explicit, tna_clust, tolerance = 1e-12)

  # Umbrella on raw matrix flips transitivity_type.
  cog_umbrella <- cograph::centrality(W, measures = "transitivity",
                                       tna_network = TRUE)$transitivity
  expect_equal(cog_umbrella, tna_clust, tolerance = 1e-12)

  # Default Watts-Strogatz on raw matrix is unweighted and differs.
  cog_local <- cograph::centrality(W, measures = "transitivity")$transitivity
  expect_false(isTRUE(all.equal(cog_local, tna_clust, tolerance = 1e-3)))
})

test_that("tna_network respects user-explicit overrides", {
  skip_if_not_installed("tna")
  W <- matrix(c(0.4, 0.3, 0.3,
                0.2, 0.5, 0.3,
                0.1, 0.2, 0.7), 3, 3, byrow = TRUE)
  rownames(W) <- colnames(W) <- c("A", "B", "C")
  t1 <- tna::tna(W)

  # User says tna_network = TRUE but explicitly passes loops = TRUE.
  # loops should stay TRUE; everything else takes tna defaults.
  out <- cograph::centrality(t1, measures = "diffusion",
                              tna_network = TRUE, loops = TRUE)
  # With loops kept on a row-stochastic matrix, the power-series collapses
  # to a constant n for every node.
  expect_equal(unique(out$diffusion_all), ncol(W))

  # User explicitly sets transitivity_type = "local" — onnela should NOT
  # silently take over even with tna_network = TRUE. Positive assertion:
  # the explicit-override result equals the baseline obtained by manually
  # mirroring the umbrella's other defaults (loops = FALSE,
  # invert_weights = TRUE) under tna_network = FALSE. If the override
  # works, the umbrella adds nothing on top.
  out_explicit_local <- cograph::centrality(t1, measures = "transitivity",
                              tna_network = TRUE, transitivity_type = "local")
  out_baseline_local <- cograph::centrality(t1, measures = "transitivity",
                              tna_network = FALSE, transitivity_type = "local",
                              loops = FALSE, invert_weights = TRUE)
  expect_equal(out_explicit_local$transitivity,
               out_baseline_local$transitivity, tolerance = 1e-12)

  # And it must NOT equal the onnela value the umbrella would have picked.
  out_default_onnela <- cograph::centrality(t1, measures = "transitivity",
                              tna_network = TRUE)
  expect_false(isTRUE(all.equal(out_explicit_local$transitivity,
                                out_default_onnela$transitivity,
                                tolerance = 1e-3)))
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

# ============================================
# Tiered centrality via `type` argument
# ============================================

test_that("centrality default tier is 'basic' with 6 canonical measures", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat)
  cols <- setdiff(names(result), "node")
  # basic resolves to 6 measure names; each measure expands to 1+ columns
  # (e.g. degree -> degree_all), so we check that each measure has a column.
  expect_true(any(grepl("^degree", cols)))
  expect_true(any(grepl("^strength", cols)))
  expect_true(any(grepl("^closeness", cols)))
  expect_true("betweenness" %in% cols)
  expect_true("eigenvector" %in% cols)
  expect_true("pagerank" %in% cols)
  # And confirms the trimming: no extended-only measure like katz/coreness
  expect_false("katz" %in% cols)
  expect_false(any(grepl("^coreness", cols)))
})

test_that("centrality type='extended' is a strict superset of basic", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  basic_cols <- setdiff(names(centrality(mat, type = "basic")), "node")
  ext_cols <- setdiff(names(centrality(mat, type = "extended")), "node")

  expect_true(all(basic_cols %in% ext_cols))
  expect_gt(length(ext_cols), length(basic_cols))
  expect_true(any(grepl("^coreness", ext_cols)))
  expect_true("katz" %in% ext_cols)
})

test_that("centrality type='all' returns every measure", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  all_cols <- setdiff(names(centrality(mat, type = "all")), "node")
  ext_cols <- setdiff(names(centrality(mat, type = "extended")), "node")
  # "all" must include everything in "extended" plus more
  expect_true(all(ext_cols %in% all_cols))
  expect_gt(length(all_cols), length(ext_cols))
})

test_that("centrality explicit measures= overrides type", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Asking for only pagerank should trump the "extended" tier
  result <- centrality(mat, type = "extended", measures = "pagerank")
  cols <- setdiff(names(result), "node")
  expect_equal(cols, "pagerank")
})

test_that("centrality type argument rejects invalid values", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  expect_error(centrality(mat, type = "bogus"), "should be one of")
})
