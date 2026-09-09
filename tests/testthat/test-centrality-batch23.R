# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("adaptive LeaderRank follows original open-neighbor H-indices", {
  path4 <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = FALSE)
  # Every original H-index is one. Including the focal degree in the
  # H-index list would incorrectly assign two to the middle vertices.
  expect_equal(unname(centrality_adaptive_leaderrank(path4)),
               c(4, 6, 6, 4) / 7)
  path5 <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  # Original H-indices (1,1,2,1,1); reversible augmented conductances.
  expect_equal(unname(centrality_adaptive_leaderrank(path5)),
               c(5 / 12, 5 / 6, 5 / 4, 5 / 6, 5 / 12))
  expect_equal(unname(centrality_adaptive_leaderrank(igraph::make_ring(4))),
               rep(5 / 6, 4))
  clique <- igraph::make_full_graph(4)
  expect_equal(unname(centrality_adaptive_leaderrank(clique)), rep(10 / 11, 4))
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_adaptive_leaderrank(star)),
               c(25 / 18, rep(5 / 9, 4)))
  for (h_mode in c("in", "out")) {
    expect_equal(centrality_adaptive_leaderrank(path5, h_mode),
                 centrality_adaptive_leaderrank(path5))
  }
})

test_that("adaptive LeaderRank separates H-index direction and resource flow", {
  path <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
  expect_equal(unname(centrality_adaptive_leaderrank(path)), c(12, 18, 21) / 29)
  expect_equal(unname(centrality_adaptive_leaderrank(path, "out")),
               c(1.5, 0, 0))
  expect_equal(unname(centrality_adaptive_leaderrank(path, "in")), c(0, 0, 1.5))
  reversed <- igraph::reverse_edges(path)
  expect_equal(unname(centrality_adaptive_leaderrank(reversed)),
               c(21, 18, 12) / 29)
  edge <- igraph::make_graph(c(1, 2), directed = TRUE)
  expect_equal(unname(centrality_adaptive_leaderrank(edge)), c(4 / 9, 2 / 3))
  expect_true(all(is.nan(centrality_adaptive_leaderrank(edge, "out"))))
  expect_true(all(is.nan(centrality_adaptive_leaderrank(edge, "in"))))
  # The ground connects components: adding an isolate changes total mass.
  isolated <- igraph::add_vertices(path, 1)
  expect_equal(unname(centrality_adaptive_leaderrank(isolated)),
               c(16, 24, 28, 0) / 29)
})

test_that("adaptive LeaderRank covers undefined and empty cases", {
  empty <- igraph::make_empty_graph(0)
  expect_equal(centrality_adaptive_leaderrank(empty),
               setNames(numeric(0), character(0)))
  for (n in 1:3) {
    g <- igraph::make_empty_graph(n)
    expect_true(all(is.nan(centrality_adaptive_leaderrank(g))))
    normalized <- centrality_adaptive_leaderrank(g, normalized = TRUE)
    expect_true(all(is.nan(normalized)))
  }
  expect_error(centrality_adaptive_leaderrank(empty, "bad"), "arg")
  expect_error(centrality_adaptive_leaderrank(empty, c("in", "out")), "length")
})

test_that("adaptive LeaderRank handles projection, labels and normalization", {
  a <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3, byrow = TRUE)
  dimnames(a) <- list(c("C", "A", "B"), c("C", "A", "B"))
  weighted <- a
  weighted[a > 0] <- c(0.01, 2, 3, 100)
  diag(weighted) <- 7
  perm <- c(3, 1, 2)
  for (h_mode in c("all", "out", "in")) {
    expected <- centrality_adaptive_leaderrank(a, h_mode)
    expect_identical(names(expected), rownames(a))
    projected <- centrality_adaptive_leaderrank(
      weighted, h_mode, loops = TRUE, mode = "in", invert_weights = TRUE
    )
    expect_equal(projected, expected)
    expect_equal(centrality_adaptive_leaderrank(a[perm, perm], h_mode),
                 expected[perm])
    normalized <- centrality_adaptive_leaderrank(a, h_mode, normalized = TRUE)
    expect_equal(normalized, expected / max(expected))
    main <- centrality(a, measures = "adaptive_leaderrank", alr_h_mode = h_mode)
    expect_equal(unname(expected), main$adaptive_leaderrank)
  }
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 3, 1), directed = TRUE)
  expect_equal(centrality_adaptive_leaderrank(g, simplify = FALSE),
               centrality_adaptive_leaderrank(igraph::simplify(g)))
  meta <- subset(list_centralities(), measure == "adaptive_leaderrank")
  expect_false(meta$uses_weights)
  expect_false(meta$mode_aware)
})
