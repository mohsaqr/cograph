test_that("truss convention distinguishes trees, cycles and complete graphs", {
  expect_equal(unname(centrality_truss(igraph::make_ring(4))), rep(2, 4))
  expect_equal(unname(centrality_truss(igraph::make_full_graph(4))), rep(4, 4))
  # Two triangles sharing an edge do not form a 4-truss: low-support edges
  # disappear first, then their shared edge loses its triangle support.
  g <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4, 2, 4), directed = FALSE)
  expect_equal(unname(centrality_truss(g)), rep(3, 4))
})

test_that("social capital counts open pairs and supported edges correctly", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_godfather(star)), c(6, 0, 0, 0, 0))
  expect_equal(unname(centrality_support(star)), rep(0, 5))
  expect_equal(unname(centrality_bridging_coefficient(star)),
               c(1 / 16, 4, 4, 4, 4))
  full <- igraph::make_full_graph(4)
  expect_equal(unname(centrality_support(full)), rep(3, 4))
  expect_equal(unname(centrality_godfather(full)), rep(0, 4))
})

test_that("MDD freezes the shell threshold through cascading removals", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_mdd(star)), c(2.8, 1, 1, 1, 1))
  expect_equal(unname(centrality_mdd(star, mdd_lambda = 0)), rep(1, 5))
  expect_equal(unname(centrality_mdd(star, mdd_lambda = 1)), c(4, 1, 1, 1, 1))
  path <- igraph::make_tree(5, children = 1, mode = "undirected")
  expect_equal(unname(centrality_mdd(path, mdd_lambda = 0.5)),
               c(1, 1.5, 1.5, 1.5, 1))
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

test_that("LocalRank is the existing semilocal measure on simple graphs", {
  g <- igraph::make_graph("Zachary")
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  d <- igraph::distances(g, weights = NA)
  ref <- as.numeric(a %*% a %*% rowSums(d > 0 & d <= 2))
  expect_equal(centrality(g, measures = "semilocal")$semilocal_all, ref)
})
