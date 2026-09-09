# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("resistance curvature agrees with published analytical families", {
  score <- function(g, ...) unname(centrality_resistance_curvature(g, ...))
  expect_equal(score(igraph::make_star(5, mode = "undirected")),
               c(-1, rep(0.5, 4)))
  expect_equal(score(igraph::make_ring(8)), rep(1 / 8, 8))
  expect_equal(score(igraph::make_full_graph(6)), rep(1 / 6, 6))
  path <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = FALSE)
  expect_equal(score(path), c(0.5, 0, 0, 0.5), tolerance = 1e-14)
  for (n in 0:3) expect_equal(score(igraph::make_empty_graph(n)), rep(1, n))
  disconnected <- igraph::disjoint_union(
    path, igraph::make_empty_graph(2, directed = FALSE)
  )
  expect_equal(score(disconnected), c(0.5, 0, 0, 0.5, 1, 1), tolerance = 1e-14)
  expect_equal(sum(score(disconnected)), 3)
})

test_that("weighted curvature equals an enumerated spanning-tree expectation", {
  # Triangle tree weights are 2, 3, 6. Their degree vectors are
  # (1,2,1), (2,1,1), (1,1,2), so E[degree] = (14,13,17)/11.
  a <- matrix(c(0, 1, 3, 1, 0, 2, 3, 2, 0), 3, 3,
              dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  expected <- stats::setNames(c(4 / 11, 9 / 22, 5 / 22), rownames(a))
  expect_equal(centrality_resistance_curvature(a), expected)
  result <- centrality(a, measures = "resistance_curvature")
  expect_equal(result$resistance_curvature, unname(expected))
  expect_equal(unname(centrality_resistance_curvature(a, weighted = FALSE)),
               rep(1 / 3, 3))
  for (scale in c(1e-200, 0.1, 10, 1e200)) {
    expect_equal(centrality_resistance_curvature(scale * a), expected)
  }
  permutation <- c(3, 1, 2)
  expect_equal(centrality_resistance_curvature(a[permutation, permutation]),
               expected[permutation])
  normalized <- centrality_resistance_curvature(a, normalized = TRUE)
  expect_equal(normalized, expected / max(expected))
})

test_that("curvature conductance projections are explicit", {
  a <- matrix(c(0, 1, 3, 1, 0, 2, 3, 2, 0), 3, 3)
  arcs <- a
  arcs[lower.tri(arcs)] <- 0
  arcs[1, 2] <- 0.25
  arcs[2, 1] <- 0.75
  expected <- centrality_resistance_curvature(a)
  expect_equal(centrality_resistance_curvature(arcs, directed = TRUE), expected)
  diag(arcs) <- 7
  expect_equal(centrality_resistance_curvature(arcs, loops = TRUE), expected)
  expect_equal(centrality_resistance_curvature(arcs, mode = "in"), expected)
  inverted <- centrality_resistance_curvature(arcs, invert_weights = TRUE)
  expect_equal(inverted, expected)
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 1, 3), directed = FALSE)
  igraph::E(g)$weight <- c(0.25, 0.75, 2, 3)
  expect_equal(centrality_resistance_curvature(g), expected)
  expect_equal(centrality_resistance_curvature(g, simplify = FALSE), expected)
  zero <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  igraph::E(zero)$weight <- c(0, 1)
  expect_equal(unname(centrality_resistance_curvature(zero)), c(1, 0.5, 0.5))
  meta <- list_centralities()
  row <- meta[meta$measure == "resistance_curvature", ]
  expect_true(row$uses_weights)
  expect_true(row$costly)
  expect_false(row$mode_aware)
})

test_that("curvature validates conductances and retains negative scores", {
  g <- igraph::make_full_graph(3)
  for (bad in c(-1, Inf, NA_real_)) {
    igraph::E(g)$weight <- c(1, 2, bad)
    expect_error(centrality_resistance_curvature(g), "finite nonnegative")
  }
  igraph::E(g)$weight <- c(1e300, 1e-300, 1)
  expect_error(centrality_resistance_curvature(g), "range exceeds")
  arcs <- igraph::make_graph(c(1, 2, 2, 1), directed = TRUE)
  igraph::E(arcs)$weight <- c(1e308, 1e308)
  expect_error(centrality_resistance_curvature(arcs), "sum exceeds")
  star <- igraph::make_star(5, mode = "undirected")
  normalized <- centrality_resistance_curvature(star, normalized = TRUE)
  expect_equal(unname(normalized), c(-2, rep(1, 4)))
})
