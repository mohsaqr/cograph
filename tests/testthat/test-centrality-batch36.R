# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("ControlRank reproduces the published bi-star example", {
  g <- igraph::make_graph(c(1, 2, 1, 7, 2, 3, 2, 4, 2, 5, 2, 6,
                            7, 8, 7, 9, 7, 10, 7, 11), directed = FALSE)
  published <- c(.17, .09, .05, .05, .05, .05, .09, .05, .05, .05, .05)
  got <- centrality_controlrank(g)
  expect_equal(round(unname(got), 2), published)
  expect_equal(unname(got[1]), 3 - sqrt(8), tolerance = 1e-12)
  expect_equal(unname(got[2]), unname(got[7]), tolerance = 1e-12)
  expect_gt(got[1], got[2])
  expect_gt(got[2], got[3])
  expect_equal(centrality(g, measures = "controlrank")$controlrank,
               unname(got))
})

test_that("ControlRank uses original degrees in the grounded matrix", {
  for (n in 3:8) {
    star <- igraph::make_star(n, mode = "undirected")
    leaf <- 2 / (n + sqrt(n^2 - 4))
    expect_equal(unname(centrality_controlrank(star)),
                 c(1, rep(leaf, n - 1)), tolerance = 1e-12)
    expect_equal(unname(centrality_controlrank(igraph::make_full_graph(n))),
                 rep(1, n), tolerance = 1e-12)
    expect_equal(unname(centrality_controlrank(igraph::make_ring(n))),
                 rep(2 - 2 * cos(pi / n), n), tolerance = 1e-12)
  }
  for (n in 0:3) {
    expect_equal(unname(centrality_controlrank(igraph::make_empty_graph(n))),
                 numeric(n))
  }
  g <- igraph::disjoint_union(igraph::make_ring(5), igraph::make_ring(3))
  expect_identical(unname(centrality_controlrank(g, normalized = TRUE)),
                   numeric(8))
})

test_that("ControlRank retains signed directed scores and row orientation", {
  a <- matrix(0, 3, 3)
  a[1, 2] <- a[2, 3] <- 1
  expected <- c((1 - sqrt(2)) / 2, 0, .5)
  expect_equal(unname(centrality_controlrank(a)), expected)
  expect_equal(unname(centrality_controlrank(t(a))), rev(expected))
  expect_equal(unname(centrality_controlrank(a, normalized = TRUE)),
               expected * 2)
  expect_equal(unname(centrality_controlrank(a, mode = "in", cutoff = 1,
                                             invert_weights = TRUE)), expected)
  # A disjoint union of directed chains has negative minimum modes even
  # after grounding any one node. Nonpositive maxima leave raw units intact.
  b <- matrix(0, 6, 6)
  b[1:3, 1:3] <- a
  b[4:6, 4:6] <- a
  raw <- centrality_controlrank(b)
  expect_true(all(raw < 0))
  expect_equal(centrality_controlrank(b, normalized = TRUE), raw)
  expect_equal(centrality_controlrank(4 * b, normalized = TRUE), 4 * raw)
})

test_that("ControlRank weights, names and parallel conventions are explicit", {
  g <- igraph::graph_from_edgelist(matrix(c("b", "a", "b", "a", "a", "c",
                                            "c", "c"), ncol = 2, byrow = TRUE))
  igraph::E(g)$weight <- c(2, 3, 4, 100)
  a <- matrix(c(0, 5, 0, 0, 0, 4, 0, 0, 0), 3, 3, byrow = TRUE)
  dimnames(a) <- list(c("b", "a", "c"), c("b", "a", "c"))
  expect_equal(centrality_controlrank(g), centrality_controlrank(a))
  expect_equal(centrality_controlrank(g, simplify = FALSE, loops = TRUE),
               centrality_controlrank(a))
  expect_named(centrality_controlrank(g), c("b", "a", "c"))
  expect_equal(unname(centrality_controlrank(a[c(3, 1, 2), c(3, 1, 2)])),
               unname(centrality_controlrank(a)[c(3, 1, 2)]))
  expect_equal(centrality_controlrank(g, weighted = FALSE),
               centrality_controlrank((a > 0) * 1))
  expect_equal(centrality_controlrank(3 * a), 3 * centrality_controlrank(a))
  meta <- list_centralities()
  expect_true(meta$uses_weights[meta$measure == "controlrank"])
  expect_true("controlrank" %in% .cg_costly_measures())
  expect_true("controlrank" %in% .cg_no_mode_measures())
})

test_that("ControlRank reports precision limits and handles uniform scales", {
  a <- matrix(1, 3, 3)
  diag(a) <- 0
  for (scale in c(1e-300, 1e300)) {
    expect_equal(unname(centrality_controlrank(a * scale)) / scale, rep(1, 3))
    expect_equal(unname(centrality_controlrank(a * scale, normalized = TRUE)),
                 rep(1, 3))
  }
  directed <- matrix(c(0, 2, 1, 1, 0, 3, 4, 1, 0), 3, 3, byrow = TRUE)
  expected <- c((9 - sqrt(17)) / 2, (8 - sqrt(29)) / 2,
                (7 - sqrt(10)) / 2)
  for (scale in c(1e-300, 1e-100, 1e300)) {
    expect_equal(unname(centrality_controlrank(
      directed * scale, directed = TRUE
    )) / scale, expected)
    g <- igraph::graph_from_adjacency_matrix(directed * scale,
                                             mode = "directed", weighted = TRUE)
    expect_equal(unname(centrality_controlrank(g)) / scale, expected)
  }
  a[1, 2] <- a[2, 1] <- 1e-300
  a[2, 3] <- a[3, 2] <- 1e300
  expect_error(centrality_controlrank(a), "weight range")
  a <- matrix(0, 3, 3)
  a[1, 2] <- a[2, 1] <- 1e-20
  a[2, 3] <- a[3, 2] <- 1
  expect_error(centrality_controlrank(a), "grounded spectrum")
  g <- igraph::make_ring(3)
  igraph::E(g)$weight <- c(1, -1, 1)
  expect_error(centrality_controlrank(g), "nonnegative")
})

test_that("ControlRank preserves exact zeros before signed normalization", {
  a <- matrix(0, 4, 4)
  a[2, 1] <- .5
  a[2, 4] <- 5.5
  a[4, 2] <- 5.75
  raw <- centrality_controlrank(a)
  expect_identical(max(raw), 0)
  expect_lt(raw[3], 0)
  expect_equal(centrality_controlrank(a, normalized = TRUE), raw)
  cycle <- igraph::make_ring(3, directed = TRUE)
  g <- igraph::disjoint_union(cycle, igraph::make_empty_graph(1))
  expect_identical(unname(centrality_controlrank(g, normalized = TRUE)),
                   numeric(4))
})
