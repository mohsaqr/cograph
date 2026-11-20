# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("social capital: a star has no support, a clique no open pair", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_support(star)), rep(0, 5))
  full <- igraph::make_full_graph(4)
  expect_equal(unname(centrality_godfather(full)), rep(0, 4))
})

test_that("MDD rejects a lambda outside [0, 1]", {
  star <- igraph::make_star(5, mode = "undirected")
  for (bad in list(NA_real_, Inf, -0.1, 1.1, c(0, 1), "0.7", NULL)) {
    expect_error(centrality_mdd(star, mdd_lambda = bad), "mdd_lambda")
  }
})

test_that("candidate measures preserve labels and project input explicitly", {
  a <- matrix(c(3, 0, 0, 2, 8, 0, 0, 5, 9), 3, 3,
              dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  b <- (a != 0 | t(a) != 0) * 1
  diag(b) <- 0
  measures <- c("truss", "mdd", "bridging_coefficient", "godfather", "support")
  actual <- centrality(a, measures = measures, directed = TRUE)
  expect_equal(actual, centrality(b, measures = measures))
  expect_identical(actual$node, c("C", "A", "B"))
  normalized <- centrality(a, measures = measures, normalized = TRUE)
  for (m in measures) {
    denom <- max(actual[[m]])
    expected <- if (denom > 0) actual[[m]] / denom else actual[[m]]
    expect_equal(normalized[[m]], expected)
  }
  meta <- list_centralities()
  expect_true(all(measures %in% meta$measure))
  expect_false(any(meta$uses_weights[meta$measure %in% measures]))
  expect_false(any(meta$mode_aware[meta$measure %in% measures]))
})

test_that("new topology measures handle empty graphs and isolates", {
  for (m in c("truss", "mdd", "bridging_coefficient", "godfather", "support")) {
    for (n in 0:3) {
      g <- igraph::make_empty_graph(n)
      expect_equal(centrality(g, measures = m)[[m]], numeric(n))
    }
  }
})
