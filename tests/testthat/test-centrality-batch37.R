# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("SpectralRank preserves ground-inclusive normalization", {
  for (n in c(1:5, 16)) {
    g <- igraph::make_empty_graph(n)
    expect_equal(unname(centrality_spectralrank(g)), rep(1 / sqrt(n), n))
    expect_equal(unname(centrality_spectralrank(g, normalized = TRUE)),
                 rep(1, n))
    for (p in c(0, 1, 3, 1e300)) {
      expected <- if (p >= n - 1) 1 else
        (p + sqrt(p^2 + 4 * n)) / (2 * n)
      expect_equal(unname(centrality_spectralrank(g, sr_prior = p)),
                   rep(expected, n))
    }
  }
  expect_length(centrality_spectralrank(igraph::make_empty_graph(0)), 0)
  g <- igraph::make_graph(c(1, 2), directed = FALSE)
  expect_equal(unname(centrality_spectralrank(g)), c(1, 1))
  expect_equal(unname(centrality_spectralrank(g, sr_prior = c(3, 0))),
               c(1, 1 / (1 + sqrt(3))))
})

test_that("SpectralRank uses outgoing scores and diagonal priors", {
  a <- matrix(c(0, 1, 0, 0), 2, 2, byrow = TRUE)
  # Augmented characteristic polynomial lambda^3 - 2lambda - 1.
  root <- (1 + sqrt(5)) / 2
  expected <- c(1, 1 / root)
  expect_equal(unname(centrality_spectralrank(a)), expected)
  expect_equal(unname(centrality_spectralrank(t(a))), rev(expected))
  expect_equal(unname(centrality_spectralrank(a, mode = "in", cutoff = 1,
                                              invert_weights = TRUE)), expected)
  high_second <- centrality_spectralrank(a, sr_prior = c(0, 10))
  expect_gt(high_second[2], high_second[1])
  expect_equal(centrality(a, measures = "spectralrank", sr_prior = c(0, 10))$
                 spectralrank, unname(high_second))
  expect_false(isTRUE(all.equal(centrality_spectralrank(a),
                                centrality_spectralrank(4 * a))))
})

test_that("SpectralRank matches regular-graph analytic eigenvectors", {
  for (n in 3:8) {
    # Ground-class equation gives lambda^2 - d*lambda - n = 0.
    root <- (2 + sqrt(4 + 4 * n)) / 2
    expected <- min(1, root / n)
    expect_equal(unname(centrality_spectralrank(igraph::make_ring(n))),
                 rep(expected, n), tolerance = 1e-12)
    expect_equal(unname(centrality_spectralrank(igraph::make_full_graph(n))),
                 rep(1, n), tolerance = 1e-12)
  }
})

test_that("SpectralRank validates and matches named priors", {
  a <- matrix(c(0, 1, 0, 0), 2, 2, byrow = TRUE,
              dimnames = list(c("b", "a"), c("b", "a")))
  expect_equal(centrality_spectralrank(a, sr_prior = c(a = 10, b = 0)),
               centrality_spectralrank(a, sr_prior = c(0, 10)))
  expect_named(centrality_spectralrank(a), c("b", "a"))
  expect_error(centrality_spectralrank(a, sr_prior = c(x = 1, a = 1)), "names")
  expect_error(centrality_spectralrank(a, sr_prior = c(a = 1, a = 1)), "names")
  for (p in list(-1, NA_real_, NaN, Inf, "1", TRUE, 1:3, numeric())) {
    expect_error(centrality_spectralrank(a, sr_prior = p), "sr_prior")
  }
  expect_error(centrality_spectralrank(igraph::make_empty_graph(0),
                                       sr_prior = -1), "sr_prior")
})

test_that("SpectralRank preprocessing and precision limits are explicit", {
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 3, 4, 8)
  a <- matrix(0, 3, 3)
  a[1, 2] <- 5
  a[2, 3] <- 4
  expect_equal(centrality_spectralrank(g), centrality_spectralrank(a))
  expect_equal(centrality_spectralrank(g, simplify = FALSE, loops = TRUE),
               centrality_spectralrank(a))
  expect_equal(centrality_spectralrank(g, weighted = FALSE),
               centrality_spectralrank((a > 0) * 1))
  perm <- c(3, 1, 2)
  expect_equal(unname(centrality_spectralrank(a[perm, perm], sr_prior = perm)),
               unname(centrality_spectralrank(a, sr_prior = 1:3)[perm]))
  expect_error(centrality_spectralrank(a, sr_prior = c(1e300, 0, 0)),
               "unresolved")
  # the parallel pair (edges 1 and 2) is summed by the dense context, so the
  # negative weight goes on the single 2 -> 3 edge
  igraph::E(g)$weight <- c(1, 1, -1, 1)
  expect_error(centrality_spectralrank(g, simplify = FALSE), "nonnegative")
  meta <- list_centralities()
  expect_true(meta$uses_weights[meta$measure == "spectralrank"])
  expect_true("spectralrank" %in% .cg_no_mode_measures())
})
