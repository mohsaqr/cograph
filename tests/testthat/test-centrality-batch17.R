# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("extended coreness on a tree is the sum of neighbour degrees", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_extended_coreness(star)), rep(4, 5))
})

test_that("extended gravity applies its radius around each neighbor", {
  star <- igraph::make_star(5, mode = "undirected")
  score <- function(radius) {
    unname(centrality_extended_gravity(star, gravity_radius = radius))
  }
  expect_equal(score(0), rep(0, 5))
  expect_equal(score(1), rep(4, 5))
  expect_equal(score(3), c(7, rep(4, 4)))
  expect_equal(score(Inf), score(NULL))
  expect_equal(score("auto"), score(1))
  ring <- igraph::make_ring(5)
  expect_equal(unname(centrality_extended_gravity(ring)), rep(20, 5))
})

test_that("extended gravity validates its radius, including empty inputs", {
  for (n in c(0, 3)) {
    g <- igraph::make_empty_graph(n)
    for (bad in list(NA_real_, NaN, -1, -Inf, c(1, 2), numeric(0), "3")) {
      expect_error(centrality_extended_gravity(g, gravity_radius = bad),
                   "gravity_radius")
    }
  }
})

test_that("batch17 projections and normalization preserve the input contract", {
  a <- matrix(c(3, 0, 0, 2, 4, 0, 0, 5, 6), 3, 3,
              dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  b <- (a != 0 | t(a) != 0) * 1
  diag(b) <- 0
  measures <- c("extended_coreness", "extended_gravity")
  result <- centrality(a, measures = measures, directed = TRUE, loops = TRUE)
  expect_equal(result, centrality(b, measures = measures))
  expect_identical(result$node, rownames(a))
  changed <- centrality(a, measures = measures, mode = "in",
                        weighted = FALSE, invert_weights = TRUE,
                        gravity_mass = "degree")
  expect_equal(changed, result)
  normalized <- centrality(a, measures = measures, normalized = TRUE)
  for (m in measures) {
    expect_equal(normalized[[m]], result[[m]] / max(result[[m]]))
    for (n in 0:3) {
      expect_equal(centrality(igraph::make_empty_graph(n), measures = m)[[m]],
                   numeric(n))
    }
  }
  multi <- igraph::make_graph(c(1, 2, 1, 2, 2, 1, 2, 3, 3, 3))
  expect_equal(centrality(multi, measures = measures)[-1], result[-1])
  expect_equal(centrality(multi, measures = measures, simplify = FALSE)[-1],
               result[-1])
  meta <- list_centralities()
  expect_false(any(meta$mode_aware[meta$measure %in% measures]))
  expect_false(any(meta$uses_weights[meta$measure %in% measures]))
})
