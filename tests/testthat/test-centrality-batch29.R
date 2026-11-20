# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("bridging capital removes one entry and counts repeated uses once", {
  # The values on one edge, a loop of 0.5, directed paths and weighted chains
  # are compared with Jackson's definition written out in
  # tests/equivalence/test-centrality-zoo-b-flow-power.R.
  loop <- igraph::make_graph(c(1, 1), directed = TRUE)
  expect_equal(unname(centrality_bridging_capital(loop, 3)), 3)
  igraph::E(loop)$weight <- 0.5
  expect_equal(unname(centrality_bridging_capital(loop, loops = FALSE)), 0)
  path <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
  expect_equal(unname(centrality_bridging_capital(path, 0)), c(0, 0, 0))
  for (n in 0:3) {
    empty <- igraph::make_empty_graph(n)
    expect_equal(unname(centrality_bridging_capital(empty)), rep(0, n))
  }
})

test_that("bridging capital uses source-destination values and labels", {
  a <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  a[1, 2] <- 0.5
  a[2, 3] <- 0.2
  v <- matrix(1, 3, 3, dimnames = dimnames(a))
  v[1, 3] <- 5
  score <- centrality_bridging_capital(a, 2, v)
  expect_equal(centrality_bridging_capital(a, 2, v, normalized = TRUE), score)
  perm <- c(3, 1, 2)
  expect_equal(centrality_bridging_capital(a[perm, perm], 2, v), score[perm])
  expect_equal(centrality_bridging_capital(a, 2, v[perm, perm]), score)
  expect_equal(centrality_bridging_capital(a, 2, v, mode = "in",
                                           invert_weights = TRUE, cutoff = 1),
               score)
  expect_equal(centrality(a, measures = "bridging_capital",
                          bridging_values = v)$bridging_capital, unname(score))
  expect_equal(centrality_bridging_capital(a, 2, v * 0), c(A = 0, B = 0, C = 0))
})

test_that("bridging capital validates probability and numerical domains", {
  g <- igraph::make_graph(c(1, 2, 1, 2), directed = TRUE)
  igraph::E(g)$weight <- c(0.2, 0.3)
  expect_equal(unname(centrality_bridging_capital(g, simplify = FALSE)),
               c(0.5, 0))
  # Parallel edges are summed by the dense context: two 0.6 transmissions
  # add to 1.2, which is not a probability.
  igraph::E(g)$weight <- c(0.6, 0.6)
  expect_error(centrality_bridging_capital(g, simplify = FALSE), "probabilities")
  igraph::E(g)$weight <- c(0.2, 0.3)
  for (bad in c(-1, NA_real_, Inf, 1.1)) {
    igraph::E(g)$weight <- c(bad, 0)
    expect_error(centrality_bridging_capital(g, simplify = FALSE),
                 "finite nonnegative|probabilities")
  }
  pair <- igraph::make_full_graph(2)
  expect_error(centrality_bridging_capital(pair, 1.5), "integer")
  expect_error(centrality_bridging_capital(pair, 2, matrix(1, 3, 3)), "matrix")
  bad_names <- matrix(1, 2, 2, dimnames = list(c("bad", "bad"), c("1", "2")))
  expect_error(centrality_bridging_capital(pair, 2, bad_names), "names")
  expect_error(centrality_bridging_capital(pair, 2, matrix(-1, 2, 2)), "matrix")
  expect_error(centrality_bridging_capital(pair, 2, matrix(1e308, 2, 2)),
               "raw scores overflow")
  expect_equal(unname(centrality_bridging_capital(
    pair, 2, matrix(1e308, 2, 2), normalized = TRUE
  )), c(1, 1))
  tiny <- igraph::make_graph(c(1, 1), directed = TRUE)
  igraph::E(tiny)$weight <- 1e-300
  expect_equal(unname(centrality_bridging_capital(tiny, 1)) / 1e-300, 1)
  expect_error(centrality_bridging_capital(tiny, 2), "underflow")
  meta <- subset(list_centralities(), measure == "bridging_capital")
  expect_true(meta$uses_weights)
  expect_true(meta$costly)
  expect_false(meta$mode_aware)
})
