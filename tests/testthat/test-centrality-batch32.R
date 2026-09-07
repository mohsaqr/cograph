test_that("BG power reproduces the original diamond and directed variants", {
  g <- igraph::make_graph(c(1, 2, 1, 3, 2, 4, 3, 4), directed = TRUE)
  # Van den Brink and Gilles (1992), Example 2.2, printed pp3-4.
  expect_equal(unname(centrality_beta_measure(g)), c(2, .5, .5, 0))
  expect_equal(unname(centrality_beta_measure(g, "negative")), c(0, .5, .5, 2))
  expect_equal(unname(centrality_beta_measure(g, normalized = TRUE)),
               c(1, .25, .25, 0))
  # One successor shared by four predecessors carries a total of one unit.
  star <- igraph::make_star(5, mode = "in")
  expect_equal(unname(centrality_beta_measure(star)), c(0, rep(.25, 4)))
  expect_equal(unname(centrality_beta_measure(star, "negative")),
               c(4, rep(0, 4)))
  undirected <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_beta_measure(undirected)), c(4, rep(.25, 4)))
  expect_equal(centrality_beta_measure(undirected, "negative"),
               centrality_beta_measure(undirected))
  expect_equal(unname(centrality_beta_measure(igraph::make_full_graph(5))),
               rep(1, 5))
  # Asymmetric source with several successors distinguishes in/out degree.
  h <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 4, 3), n = 5, directed = TRUE)
  expect_equal(unname(centrality_beta_measure(h)), c(4 / 3, 1 / 3, 0, 1 / 3, 0))
  expect_equal(unname(centrality_beta_measure(h, "negative")),
               c(0, .5, 2.5, 0, 0))
  expect_equal(sum(centrality_beta_measure(h)), 2)
  expect_equal(sum(centrality_beta_measure(h, "negative")), 3)
})

test_that("BG power handles disconnected and empty inputs without extra mass", {
  for (n in 0:3) {
    for (direction in c("positive", "negative")) {
      expect_equal(unname(centrality_beta_measure(
        igraph::make_empty_graph(n), direction, normalized = TRUE
      )), rep(0, n))
    }
  }
  h <- igraph::disjoint_union(igraph::make_ring(3),
                              igraph::make_star(4, mode = "undirected"),
                              igraph::make_empty_graph(1, directed = FALSE))
  expect_equal(unname(centrality_beta_measure(h)),
               c(1, 1, 1, 3, 1 / 3, 1 / 3, 1 / 3, 0))
})

test_that("BG public API preserves names, orientation and simple topology", {
  g <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 4, 3), n = 5, directed = TRUE)
  igraph::V(g)$name <- c("a", "b", "c", "d", "isolated")
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  p <- c(5, 2, 4, 1, 3)
  for (direction in c("positive", "negative")) {
    score <- centrality_beta_measure(g, direction)
    expect_identical(names(score), igraph::V(g)$name)
    expect_equal(centrality(g, measures = "beta_measure",
                            beta_direction = direction)$beta_measure,
                 unname(score))
    expect_equal(centrality_beta_measure(a[p, p], direction), score[p])
    h <- igraph::add_edges(g, c(1, 1, 1, 2, 1, 2, 5, 5))
    igraph::E(h)$weight <- seq_len(igraph::ecount(h))
    expect_equal(centrality_beta_measure(h, direction, simplify = FALSE,
                                         mode = "in", invert_weights = TRUE,
                                         cutoff = 1), score)
  }
  expect_equal(centrality_beta_measure(t(a)),
               centrality_beta_measure(a, "negative"))
  meta <- subset(list_centralities(), measure == "beta_measure")
  expect_false(meta$uses_weights)
  expect_false(meta$mode_aware)
  expect_false(meta$costly)
  for (bad in list(NULL, NA, "out", "Positive", 1, c("positive", "negative"))) {
    expect_error(centrality_beta_measure(g, beta_direction = bad),
                 "beta_direction")
  }
})
