# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("LineRank scores nothing on graphs without edges", {
  # The values on paths, rings, complete graphs and directed paths are compared
  # with NetworkX in tests/equivalence/test-centrality-zoo-b-line-walks.R.
  for (n in 0:3) {
    expect_equal(unname(centrality_linerank(igraph::make_empty_graph(n))),
                 rep(0, n))
  }
})

test_that("LineRank separates weighted transitions and aggregation", {
  g <- igraph::make_graph(c(1, 2, 2, 3, 2, 4), directed = TRUE)
  igraph::E(g)$weight <- c(2, 1, 3)
  igraph::V(g)$name <- c("A", "B", "C", "D")
  # The values themselves are compared with NetworkX in
  # tests/equivalence/test-centrality-zoo-b-line-walks.R; here they anchor invariants.
  expected <- centrality_linerank(g, 0.5)
  weighted <- centrality_linerank(g, 0.5, "weight")
  expect_equal(centrality_linerank(g, 0.5, normalized = TRUE), expected)
  expect_equal(centrality_linerank(g, 0.5, "weight", normalized = TRUE),
               weighted / max(weighted))
  expect_equal(centrality_linerank(g, 0.5, mode = "in", invert_weights = TRUE),
               expected)
  expect_equal(centrality(g, measures = "linerank", damping = 0.5)$linerank,
               unname(expected))
  for (scale in c(1e-300, 10, 1e300)) {
    igraph::E(g)$weight <- scale * c(2, 1, 3)
    expect_equal(centrality_linerank(g, 0.5), expected)
    expect_equal(centrality_linerank(g, 0.5, "weight") / scale, weighted)
  }
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(4, 2, 1, 3)
  expect_equal(centrality_linerank(a[perm, perm], 0.5, directed = TRUE),
               expected[perm])
  meta <- subset(list_centralities(), measure == "linerank")
  expect_true(meta$uses_weights)
  expect_true(meta$costly)
  expect_false(meta$mode_aware)
})

test_that("LineRank handles loops, parallel states and domain errors", {
  loop <- igraph::make_graph(c(1, 1), directed = TRUE)
  expect_equal(unname(centrality_linerank(loop)), 2)
  expect_equal(unname(centrality_linerank(loop, loops = FALSE)), 0)
  igraph::E(loop)$weight <- 1e308
  expect_error(centrality_linerank(loop, linerank_aggregation = "weight"),
               "raw scores overflow")
  expect_equal(unname(centrality_linerank(loop, linerank_aggregation = "weight",
                                          normalized = TRUE)), 1)
  parallel <- igraph::make_graph(c(1, 2, 1, 2, 2, 3), directed = FALSE)
  # The dense graph context combines parallel edges (a line graph cannot
  # keep two nodes for one cell), so simplify = FALSE is a synonym for the
  # default here and the scores are those of the simple path 1 - 2 - 3.
  expect_equal(unname(centrality_linerank(parallel, 0, simplify = FALSE)),
               unname(centrality_linerank(parallel, 0)))
  expect_equal(unname(centrality_linerank(parallel, 0)), c(1, 2, 1) / 2)
  expect_equal(sum(centrality_linerank(parallel, simplify = FALSE)), 2)
  expect_error(centrality_linerank(loop, 1), "damping")
  expect_error(centrality_linerank(loop, linerank_aggregation = "bad"), "arg")
  for (bad in c(-1, NA_real_, Inf)) {
    igraph::E(loop)$weight <- bad
    expect_error(centrality_linerank(loop), "finite nonnegative")
  }
  igraph::E(loop)$weight <- 0
  expect_equal(unname(centrality_linerank(loop)), 0)
  branch <- igraph::make_graph(c(1, 2, 2, 3, 2, 4), directed = TRUE)
  igraph::E(branch)$weight <- c(1, 1e308, 1e-308)
  expect_error(centrality_linerank(branch), "transition range")
})
