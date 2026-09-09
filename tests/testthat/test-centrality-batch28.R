# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("Coleman-Theil follows the author's small-network conventions", {
  for (n in 0:3) {
    expect_equal(unname(centrality_coleman_theil(igraph::make_empty_graph(n))),
                 rep(0, n))
  }
  for (n in 3:7) {
    expect_equal(unname(centrality_coleman_theil(igraph::make_full_graph(n))),
                 rep(0, n))
    expect_equal(unname(centrality_coleman_theil(igraph::make_ring(n))),
                 rep(0, n))
  }
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_coleman_theil(star)), c(0, 1, 1, 1, 1))
  expect_equal(unname(centrality_coleman_theil(igraph::make_full_graph(2))),
               c(1, 1))
  expect_equal(unname(centrality_coleman_theil(igraph::make_ring(4),
                                               normalized = TRUE)), rep(0, 4))
})

test_that("Coleman-Theil uses mutual weighted dyadic constraint", {
  g <- igraph::make_graph(c(1, 2, 1, 3), directed = FALSE)
  igraph::E(g)$weight <- c(1, 2)
  igraph::V(g)$name <- c("A", "B", "C")
  h <- 1 + sum(c(1 / 5, 4 / 5) * log(c(1 / 5, 4 / 5))) / log(2)
  expected <- c(A = h, B = 1, C = 1)
  expect_equal(centrality_coleman_theil(g), expected)
  expect_equal(centrality_coleman_theil(g, weighted = FALSE),
               c(A = 0, B = 1, C = 1))
  for (scale in c(1e-300, 1e300)) {
    igraph::E(g)$weight <- c(1, 2) * scale
    expect_equal(centrality_coleman_theil(g), expected)
  }
  directed <- igraph::make_graph(c(1, 2, 3, 1), directed = TRUE)
  igraph::E(directed)$weight <- c(1, 2)
  expect_equal(unname(centrality_coleman_theil(directed)), unname(expected))
  parallel <- igraph::make_graph(c(1, 2, 1, 2, 1, 3), directed = TRUE)
  # Parallel edges are summed by the dense context; with unit weights the
  # doubled edge carries weight 2 and reproduces the weighted expectation.
  igraph::E(parallel)$weight <- rep(1, 3)
  expect_equal(unname(centrality_coleman_theil(parallel, simplify = FALSE)),
               unname(expected))
  expect_equal(unname(centrality_coleman_theil(parallel, weighted = FALSE)), c(0, 1, 1))
  # Positive scores near uniformity must survive entropy cancellation.
  igraph::E(g)$weight <- c(1, 1 + 1e-8)
  small <- centrality_coleman_theil(g)[1]
  expect_gt(small, 0)
  expect_lt(small, 1e-15)
})

test_that("Coleman-Theil preserves public API semantics", {
  a <- matrix(c(0, 1, 2, 1, 0, 4, 2, 4, 0), 3, 3)
  dimnames(a) <- list(c("A", "B", "C"), c("A", "B", "C"))
  score <- centrality_coleman_theil(a)
  expect_identical(names(score), rownames(a))
  expect_equal(centrality(a, measures = "coleman_theil")$coleman_theil,
               unname(score))
  expect_equal(centrality_coleman_theil(a, normalized = TRUE),
               score / max(score))
  perm <- c(3, 1, 2)
  expect_equal(centrality_coleman_theil(a[perm, perm]), score[perm])
  diag(a) <- 10
  expect_equal(centrality_coleman_theil(a, mode = "in", invert_weights = TRUE,
                                        cutoff = 1), score)
  doubled <- matrix(0, 6, 6)
  doubled[1:3, 1:3] <- a
  doubled[4:6, 4:6] <- a * 100
  expect_equal(unname(centrality_coleman_theil(doubled)), rep(unname(score), 2))
  meta <- subset(list_centralities(), measure == "coleman_theil")
  expect_true(meta$uses_weights)
  expect_false(meta$mode_aware)
  expect_false(meta$costly)
  g <- igraph::make_ring(3)
  for (bad in c(-1, NA_real_, Inf)) {
    igraph::E(g)$weight <- c(bad, 1, 2)
    expect_error(centrality_coleman_theil(g), "finite nonnegative")
  }
  igraph::E(g)$weight <- c(0, 0, 1)
  expect_equal(sort(unname(centrality_coleman_theil(g))), c(0, 1, 1))
  igraph::E(g)$weight <- c(1e-308, 1, 1e308)
  expect_error(centrality_coleman_theil(g), "weight range")
})
