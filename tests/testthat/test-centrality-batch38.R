# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("MCGM on the primary paper's example keeps its public contracts", {
  edges <- c(1, 2, 2, 3, 2, 7, 3, 4, 3, 7, 4, 5, 4, 6,
             4, 7, 5, 6, 5, 7, 5, 8, 6, 7, 6, 9)
  g <- igraph::make_graph(edges, directed = FALSE)
  # Table 2 itself is an equivalence test
  # (tests/equivalence/test-centrality-zoo-b-gravity.R).
  score <- centrality_mcgm(g)
  expect_equal(centrality(g, measures = "mcgm")$mcgm, unname(score))
  expect_equal(unname(centrality_mcgm(g, normalized = TRUE)),
               unname(score / max(score)))
  expect_false(isTRUE(all.equal(score, centrality_mcgm(g, mcgm_alpha = 1))))
})

test_that("MCGM handles disconnected spectra with a uniform projection", {
  g <- igraph::disjoint_union(igraph::make_ring(4),
                              igraph::make_star(5, mode = "undirected"))
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  # Nonisomorphic components with equal spectral radius two.
  expect_equal(.cg_mcgm_eigenvector(a), c(rep(2 / 3, 4), 1, rep(.5, 4)))
  expected <- c(rep(2.5^2 * 2.25, 4), 4 * (8 / 3) * (17 / 12),
                rep((8 / 3) * (17 / 12) + 3 * (17 / 12)^2 / 4, 4))
  expect_equal(unname(centrality_mcgm(g)), expected)
  g <- igraph::disjoint_union(igraph::make_full_graph(3),
                              igraph::make_full_graph(2))
  expect_equal(unname(centrality_mcgm(g)), c(rep(18, 3), 1, 1))
  g <- igraph::disjoint_union(igraph::make_full_graph(2),
                              igraph::make_empty_graph(3, directed = FALSE))
  expect_error(centrality_mcgm(g), "median coreness is zero")
  expect_equal(unname(centrality_mcgm(g, mcgm_alpha = 1)), c(9, 9, 0, 0, 0))
  expect_equal(unname(centrality_mcgm(g, mcgm_radius = 0)), rep(0, 5))
  for (n in 0:3) {
    expect_equal(unname(centrality_mcgm(igraph::make_empty_graph(n))),
                 numeric(n))
  }
})

test_that("MCGM preserves skeleton, node order and parameter contracts", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7)
  igraph::V(g)$name <- c("c", "a", "b")
  score <- centrality_mcgm(g)
  expect_named(score, c("c", "a", "b"))
  expect_equal(centrality_mcgm(g, simplify = FALSE, mode = "in", loops = TRUE,
                               invert_weights = TRUE, cutoff = 1), score)
  expect_equal(centrality_mcgm(g, weighted = FALSE), score)
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 2)
  expect_equal(centrality_mcgm(a[perm, perm]), score[perm])
  for (r in list(-1, NA_real_, NaN, "2", TRUE, c(1, 2), numeric())) {
    expect_error(centrality_mcgm(g, mcgm_radius = r), "mcgm_radius")
  }
  for (alpha in list(-1, NA_real_, Inf, NaN, "1", TRUE, 1:2, numeric())) {
    expect_error(centrality_mcgm(g, mcgm_alpha = alpha), "mcgm_alpha")
  }
  expect_equal(unname(centrality_mcgm(
    g, mcgm_alpha = 1e300, normalized = TRUE
  )), c(5 / 8, 1, 5 / 8))
  expect_error(centrality_mcgm(g, mcgm_alpha = 1e300), "exceed double")
  meta <- list_centralities()
  expect_false(meta$uses_weights[meta$measure == "mcgm"])
  expect_true("mcgm" %in% .cg_no_mode_measures())
})
