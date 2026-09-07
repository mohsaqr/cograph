test_that("CDA agrees with analytical binary and weighted scores", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_cda(star)), c(4, rep(2.5, 4)))
  triangle <- matrix(c(0, 1, 3, 1, 0, 2, 3, 2, 0), 3, 3)
  expected <- c(22 / 3, 35 / 6, 49 / 6) * stats::plogis(1)
  expect_equal(unname(centrality_cda(triangle)), expected)
  expect_equal(unname(centrality_cda(igraph::make_full_graph(4))),
               rep(12 * stats::plogis(1), 4))
  for (alpha in c(0, 0.25, 0.5, 1)) {
    expect_equal(centrality_cda(star, cda_alpha = alpha), centrality_cda(star))
  }
  strength <- centrality_cda(triangle, cda_alpha = 0)
  degree <- centrality_cda(triangle, cda_alpha = 1)
  expect_equal(centrality_cda(triangle, cda_alpha = 0.25),
               0.25 * degree + 0.75 * strength)
  expect_equal(centrality_cda(10 * triangle, cda_alpha = 0), 10 * strength)
  expect_equal(centrality_cda(10 * triangle, cda_alpha = 1), degree)
})

test_that("CDA uses the global maximum weight and handles absent connections", {
  g <- igraph::make_graph(c(1, 2, 3, 4), n = 5, directed = FALSE)
  igraph::E(g)$weight <- c(1, 2)
  expect_equal(unname(centrality_cda(g, cda_alpha = 1)), c(0.75, 0.75, 1, 1, 0))
  igraph::E(g)$weight <- c(0, 2)
  expect_equal(unname(centrality_cda(g)), c(0, 0, 1.5, 1.5, 0))
  for (n in 0:3) {
    empty_score <- centrality_cda(igraph::make_empty_graph(n))
    expect_equal(unname(empty_score), numeric(n))
  }
})

test_that("CDA projection, labels and normalization are consistent", {
  a <- matrix(c(0, 1, 3, 1, 0, 2, 3, 2, 0), 3, 3,
              dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  expected <- centrality_cda(a)
  expect_identical(names(expected), rownames(a))
  arcs <- a
  arcs[lower.tri(arcs)] <- 0
  arcs[1, 2] <- 0.25
  arcs[2, 1] <- 0.75
  diag(arcs) <- 5
  expect_equal(centrality_cda(arcs, directed = TRUE, loops = TRUE), expected)
  expect_equal(centrality_cda(a, mode = "in", invert_weights = TRUE), expected)
  expect_equal(centrality_cda(a, normalized = TRUE), expected / max(expected))
  order <- c(3, 1, 2)
  expect_equal(centrality_cda(a[order, order]), expected[order])
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 1, 3), directed = FALSE)
  igraph::E(g)$weight <- c(0.25, 0.75, 2, 3)
  expect_equal(unname(centrality_cda(g, simplify = FALSE)), unname(expected))
  unweighted <- centrality_cda(g, weighted = FALSE)
  expect_equal(unweighted, centrality_cda(igraph::make_full_graph(3)))
  meta <- list_centralities()
  expect_true(meta$uses_weights[meta$measure == "cda"])
  expect_false(meta$mode_aware[meta$measure == "cda"])
})

test_that("CDA validates its parameter and numerical domain", {
  g <- igraph::make_full_graph(3)
  for (bad in list(NULL, NA_real_, Inf, -1, 1.1, "0.5", c(0, 1))) {
    expect_error(centrality_cda(g, cda_alpha = bad), "cda_alpha")
  }
  for (bad in c(-1, Inf, NA_real_)) {
    igraph::E(g)$weight <- c(1, 2, bad)
    expect_error(centrality_cda(g), "finite nonnegative")
  }
  igraph::E(g)$weight <- rep(1e308, 3)
  expect_error(centrality_cda(g, normalized = TRUE), "strength exceeds")
  clique <- igraph::make_full_graph(6)
  igraph::E(clique)$weight <- rep(1e307, igraph::ecount(clique))
  # Row-normalized clustering avoids overflow in strength * (degree - 1).
  expect_equal(unname(centrality_cda(clique, cda_alpha = 1)),
               rep(30 * stats::plogis(1), 6))
  expect_error(centrality_cda(clique, cda_alpha = 0, normalized = TRUE),
               "score exceeds")
})
