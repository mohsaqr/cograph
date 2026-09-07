test_that("weighted LeaderRank preserves source scale and direction", {
  edge <- igraph::make_graph(c(1, 2), directed = TRUE)
  expect_equal(unname(centrality_weighted_leaderrank(edge)), c(0, 1.5))
  expect_equal(unname(centrality_weighted_leaderrank(edge, wlr_alpha = 0)),
               c(2 / 3, 1))
  reverse <- igraph::reverse_edges(edge)
  expect_equal(unname(centrality_weighted_leaderrank(reverse)), c(1.5, 0))
  isolated <- igraph::add_vertices(edge, 1)
  expect_equal(unname(centrality_weighted_leaderrank(isolated)), c(0, 2, 0))
  ring <- igraph::make_ring(4, directed = TRUE)
  for (alpha in c(-2, 0, 0.5, 1, 3)) {
    expect_equal(unname(centrality_weighted_leaderrank(ring, alpha)),
                 rep(5 / 6, 4))
  }
  clique <- igraph::make_full_graph(4)
  expect_equal(unname(centrality_weighted_leaderrank(clique)), rep(1, 4))
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_weighted_leaderrank(star)), c(2, rep(0.6, 4)))
  # Alpha zero gives an undirected augmented graph with known stationary mass.
  degree <- igraph::degree(star) + 1
  expected <- 6 * degree / (sum(degree) + 5)
  expect_equal(unname(centrality_weighted_leaderrank(star, 0)),
               unname(expected))
})

test_that("weighted LeaderRank handles zero degree and periodic chains", {
  expect_equal(centrality_weighted_leaderrank(igraph::make_empty_graph(0)),
               setNames(numeric(0), character(0)))
  for (n in 1:4) {
    g <- igraph::make_empty_graph(n)
    expect_true(all(is.nan(centrality_weighted_leaderrank(g))))
    normalized <- centrality_weighted_leaderrank(g, normalized = TRUE)
    expect_true(all(is.nan(normalized)))
    # Ordinary iteration has period two, but the stationary solution exists.
    expect_equal(unname(centrality_weighted_leaderrank(g, 0)),
                 rep((n + 1) / (2 * n), n))
    expect_error(centrality_weighted_leaderrank(g, -1), "positive in-degree")
  }
  g <- igraph::make_graph(c(1, 2), directed = TRUE)
  expect_error(centrality_weighted_leaderrank(g, -0.1), "positive in-degree")
  for (alpha in list(NA_real_, NaN, Inf, -Inf, c(1, 2), "1", NULL)) {
    expect_error(centrality_weighted_leaderrank(g, alpha), "finite number")
  }
})

test_that("weighted LeaderRank preserves labels and binary topology", {
  a <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), 3, 3, byrow = TRUE)
  dimnames(a) <- list(c("C", "A", "B"), c("C", "A", "B"))
  expected <- centrality_weighted_leaderrank(a)
  expect_identical(names(expected), rownames(a))
  altered <- a
  altered[a > 0] <- c(0.01, 2, 3, 100)
  diag(altered) <- 7
  projected <- centrality_weighted_leaderrank(
    altered, loops = TRUE, mode = "in", invert_weights = TRUE, cutoff = 1
  )
  expect_equal(projected, expected)
  perm <- c(3, 1, 2)
  expect_equal(centrality_weighted_leaderrank(a[perm, perm]), expected[perm])
  expect_equal(centrality_weighted_leaderrank(a, normalized = TRUE),
               expected / max(expected))
  main <- centrality(a, measures = "weighted_leaderrank", wlr_alpha = 0.5)
  expect_equal(unname(main$weighted_leaderrank),
               unname(centrality_weighted_leaderrank(a, 0.5)))
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 3, 1), directed = TRUE)
  expect_equal(centrality_weighted_leaderrank(g, simplify = FALSE),
               centrality_weighted_leaderrank(igraph::simplify(g)))
  meta <- subset(list_centralities(), measure == "weighted_leaderrank")
  expect_false(meta$uses_weights)
  expect_false(meta$mode_aware)
})

test_that("extreme exponents avoid ground-weight overflow", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_weighted_leaderrank(star, 1e308)),
               c(2.5, rep(0.5, 4)))
  expect_equal(unname(centrality_weighted_leaderrank(star, -1e308)),
               c(10 / 7, rep(5 / 7, 4)))
})
