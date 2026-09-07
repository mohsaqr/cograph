test_that("graph regularization agrees with analytic graph families", {
  score <- function(g, ...) unname(centrality_graph_regularization(g, ...))
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(score(star), c(3, rep(12 / 7, 4)))
  path <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  expect_equal(score(path), c(8 / 5, 2, 8 / 5))
  for (gamma in c(0, 0.01, 0.5, 1, 10, 1e308)) {
    expected <- if (gamma == 1e308) 4 else (1 + 4 * gamma) / (1 + gamma)
    expect_equal(score(igraph::make_full_graph(4), gamma), rep(expected, 4))
  }
  expect_equal(score(star, 0), rep(1, 5))
  for (n in 0:3) expect_equal(score(igraph::make_empty_graph(n)), rep(1, n))
  g <- igraph::disjoint_union(
    path, igraph::make_empty_graph(2, directed = FALSE)
  )
  expect_equal(score(g), c(8 / 5, 2, 8 / 5, 1, 1))
  expect_equal(score(g, 1e308), c(3, 3, 3, 1, 1))
})

test_that("weighted regularization follows exact determinant ratios", {
  a <- matrix(c(0, 1, 3, 1, 0, 2, 3, 2, 0), 3, 3,
              dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  # det(I+L)=46; principal minors excluding nodes are 20,21,19.
  expected <- stats::setNames(46 / c(20, 21, 19), rownames(a))
  expect_equal(centrality_graph_regularization(a), expected)
  expect_equal(centrality(a, measures = "graph_regularization")$
                 graph_regularization, unname(expected))
  expect_equal(unname(centrality_graph_regularization(a, weighted = FALSE)),
               rep(2, 3))
  for (scale in c(1e-300, 0.1, 10, 1e300)) {
    expect_equal(centrality_graph_regularization(scale * a, 1 / scale),
                 expected)
  }
  # gamma*weight overflows, but the bounded score is well defined.
  expect_equal(unname(centrality_graph_regularization(a * 1e300, 1e300)),
               rep(3, 3))
  expect_equal(unname(centrality_graph_regularization(a * 1e-300, 1e-300)),
               rep(1, 3))
  perm <- c(3, 1, 2)
  expect_equal(centrality_graph_regularization(a[perm, perm]), expected[perm])
  expect_equal(centrality_graph_regularization(a, normalized = TRUE),
               expected / max(expected))
})

test_that("regularization graph projections retain explicit weight semantics", {
  a <- matrix(c(0, 1, 3, 1, 0, 2, 3, 2, 0), 3, 3)
  arcs <- a
  arcs[lower.tri(arcs)] <- 0
  arcs[1, 2] <- 0.25
  arcs[2, 1] <- 0.75
  expected <- centrality_graph_regularization(a)
  expect_equal(centrality_graph_regularization(arcs, directed = TRUE), expected)
  diag(arcs) <- 7
  expect_equal(centrality_graph_regularization(arcs, loops = TRUE), expected)
  expect_equal(centrality_graph_regularization(arcs, mode = "in"), expected)
  expect_equal(centrality_graph_regularization(arcs, invert_weights = TRUE),
               expected)
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 1, 3), directed = FALSE)
  igraph::E(g)$weight <- c(0.25, 0.75, 2, 3)
  expect_equal(centrality_graph_regularization(g), expected)
  expect_equal(centrality_graph_regularization(g, simplify = FALSE), expected)
  zero <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  igraph::E(zero)$weight <- c(0, 1)
  expect_equal(unname(centrality_graph_regularization(zero)), c(1, 1.5, 1.5))
  meta <- subset(list_centralities(), measure == "graph_regularization")
  expect_true(meta$uses_weights)
  expect_false(meta$mode_aware)
})

test_that("regularization validates its parameter and edge weight domain", {
  empty <- igraph::make_empty_graph(0)
  for (bad in list(-1, Inf, NA_real_, NaN, "1", TRUE, numeric(0), c(1, 2))) {
    expect_error(centrality_graph_regularization(empty, bad), "grc_gamma")
  }
  g <- igraph::make_full_graph(3)
  for (bad in c(-1, Inf, NA_real_)) {
    igraph::E(g)$weight <- c(1, 2, bad)
    expect_error(centrality_graph_regularization(g), "finite nonnegative")
  }
  igraph::E(g)$weight <- c(1e300, 1e-300, 1)
  expect_error(centrality_graph_regularization(g), "range exceeds")
  weak <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  igraph::E(weak)$weight <- c(1, 1e-20)
  expect_error(centrality_graph_regularization(weak), "numerically singular")
  arcs <- igraph::make_graph(c(1, 2, 2, 1), directed = TRUE)
  igraph::E(arcs)$weight <- c(1e308, 1e308)
  expect_error(centrality_graph_regularization(arcs), "sum exceeds")
})
