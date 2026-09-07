test_that("improved closeness agrees with analytical path multiplicities", {
  ring <- igraph::make_ring(4)
  star <- igraph::make_star(5, mode = "undirected")
  for (alpha in c(0, 0.1, 0.2, 0.5, 1)) {
    expected <- rep(3 / (2 + 2 / 2^alpha), 4)
    expect_equal(unname(centrality_improved_closeness(ring, icc_alpha = alpha)),
                 expected)
    expect_equal(unname(centrality_improved_closeness(star, icc_alpha = alpha)),
                 c(1, rep(4 / 7, 4)))
  }
  clique <- centrality_improved_closeness(igraph::make_full_graph(4))
  expect_equal(unname(clique), rep(1, 4))
  g <- igraph::make_graph("Zachary")
  expect_equal(unname(centrality_improved_closeness(g, icc_alpha = 0)),
               unname(igraph::closeness(g, normalized = TRUE, weights = NA)))
  bipartite <- igraph::make_full_bipartite_graph(3, 3)
  expect_equal(unname(centrality_improved_closeness(bipartite, icc_alpha = 1)),
               rep(15 / 13, 6))
})

test_that("improved closeness documents disconnected and degenerate inputs", {
  for (n in 0:3) {
    score <- centrality_improved_closeness(igraph::make_empty_graph(n))
    expect_equal(unname(score), numeric(n))
  }
  g <- igraph::disjoint_union(igraph::make_ring(4), igraph::make_full_graph(3))
  expect_equal(unname(centrality_improved_closeness(g)), numeric(7))
  expect_equal(unname(centrality_improved_closeness(g, normalized = TRUE)),
               numeric(7))
})

test_that("improved closeness preserves labels and projects other inputs", {
  a <- as.matrix(igraph::as_adjacency_matrix(igraph::make_graph("Zachary")))
  labels <- paste0("person", seq_len(nrow(a)))
  dimnames(a) <- list(labels, labels)
  expected <- centrality_improved_closeness(a)
  expect_identical(names(expected), labels)
  arcs <- a
  arcs[lower.tri(arcs)] <- 0
  arcs[arcs > 0] <- seq_len(sum(arcs > 0))
  diag(arcs) <- 5
  projected <- centrality_improved_closeness(arcs, directed = TRUE,
                                             loops = TRUE)
  expect_equal(projected, expected)
  inverted <- centrality_improved_closeness(a, mode = "in",
                                            invert_weights = TRUE)
  expect_equal(inverted, expected)
  expect_equal(centrality_improved_closeness(a, normalized = TRUE),
               expected / max(expected))
  perm <- rev(seq_len(nrow(a)))
  expect_equal(centrality_improved_closeness(a[perm, perm]), expected[perm])
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 3, 4, 4, 1, 1, 1))
  igraph::E(g)$weight <- c(0.5, 4, 6, 2, 3, 7)
  score <- centrality_improved_closeness(g, simplify = FALSE, loops = TRUE)
  expect_equal(unname(score), rep(3 / (2 + 2 / 2^0.2), 4))
  meta <- list_centralities()
  expect_false(meta$uses_weights[meta$measure == "improved_closeness"])
  expect_false(meta$mode_aware[meta$measure == "improved_closeness"])
})

test_that("improved closeness rejects invalid multiplicity exponents", {
  for (bad in list(NULL, NA_real_, Inf, -0.1, 1.1, "0.2", c(0, 1))) {
    expect_error(centrality_improved_closeness(igraph::make_ring(4),
                                               icc_alpha = bad), "icc_alpha")
  }
  expect_error(centrality_improved_closeness(igraph::make_empty_graph(0),
                                             icc_alpha = -1), "icc_alpha")
})
