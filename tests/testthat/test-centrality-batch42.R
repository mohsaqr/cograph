# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("neighbor distance step sums count non-backtracking walks", {
  b <- as.matrix(igraph::as_adjacency_matrix(
    igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4), directed = FALSE)
  ))
  theta <- rowSums(b)
  steps <- .cg_nb_walk_sums(b, theta, 3)
  expect_length(steps, 3L)
  expect_equal(steps[[1L]], as.numeric(b %*% theta))
  expect_equal(unname(steps[[2L]]), c(4, 6, 6, 4))
  # Three-step walks from A: A-B-C-A and A-C-B-A, both ending on degree 3.
  expect_equal(unname(steps[[3L]])[1], 6)
  expect_identical(.cg_nb_walk_sums(b, theta, 0), list())
  # A walk of length one is just an edge, so the first sum never subtracts.
  ring <- as.matrix(igraph::as_adjacency_matrix(igraph::make_ring(5)))
  expect_equal(unname(.cg_nb_walk_sums(ring, rep(1, 5), 4)[[4L]]), rep(2, 5))
})

test_that("neighbor distance handles isolates, empties and components", {
  for (n in 0:3) {
    expect_equal(unname(centrality_neighbor_distance(
      igraph::make_empty_graph(n, directed = FALSE)
    )), numeric(n))
  }
  # Isolates have empty sums, and both benchmarks give them zero.
  expect_equal(unname(centrality_neighbor_distance(matrix(0, 2, 2),
                                                   nd_mass = "coreness")),
               c(0, 0))
  ring <- igraph::make_ring(5)
  alone <- unname(centrality_neighbor_distance(ring))
  # Walks never leave a component, so raw scores are component-local.
  with_isolate <- igraph::disjoint_union(
    ring, igraph::make_empty_graph(1, directed = FALSE)
  )
  expect_equal(unname(centrality_neighbor_distance(with_isolate)),
               c(alone, 0))
  path <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = FALSE)
  joined <- igraph::disjoint_union(ring, path)
  expect_equal(unname(centrality_neighbor_distance(joined)),
               c(alone, unname(centrality_neighbor_distance(path))))
})

test_that("neighbor distance normalizes by its maximum", {
  g <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4), directed = FALSE)
  raw <- unname(centrality_neighbor_distance(g))
  expect_equal(unname(centrality_neighbor_distance(g, normalized = TRUE)),
               raw / max(raw))
})

test_that("neighbor distance input projections and public contracts hold", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7)
  igraph::V(g)$name <- c("b", "c", "a")
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 2)
  score <- centrality_neighbor_distance(g)
  expect_named(score, c("b", "c", "a"))
  expect_equal(centrality(g, measures = "neighbor_distance")$neighbor_distance,
               unname(score))
  # Either arc makes one edge, parallels count once and the loop is dropped,
  # so the skeleton is the two-edge path b-c-a.
  expect_equal(unname(score), c(1.44, 2.4, 1.44))
  expect_equal(centrality_neighbor_distance(g, simplify = FALSE, mode = "in",
                                            loops = TRUE, invert_weights = TRUE,
                                            cutoff = 1), score)
  expect_equal(centrality_neighbor_distance(g, weighted = FALSE), score)
  expect_equal(centrality_neighbor_distance(a[perm, perm]), score[perm])
})

test_that("neighbor distance validates its parameters and metadata", {
  g <- igraph::make_ring(6)
  for (order in list(-1, 1.5, NA_real_, NaN, Inf, "2", TRUE, 1:2, numeric())) {
    expect_error(centrality_neighbor_distance(g, nd_order = order), "nd_order")
  }
  for (decay in list(NA_real_, NaN, Inf, -Inf, "1", TRUE, c(1, 2), numeric())) {
    expect_error(centrality_neighbor_distance(g, nd_decay = decay), "nd_decay")
  }
  expect_error(centrality_neighbor_distance(g, nd_mass = "betweenness"))
  # Validation happens before the graph is read, so an empty graph still errors.
  expect_error(centrality_neighbor_distance(igraph::make_empty_graph(0),
                                            nd_order = -1), "nd_order")
  meta <- list_centralities()
  expect_true("neighbor_distance" %in% meta$measure)
  expect_false(meta$uses_weights[meta$measure == "neighbor_distance"])
  expect_false(meta$mode_aware[meta$measure == "neighbor_distance"])
  expect_false(meta$costly[meta$measure == "neighbor_distance"])
  expect_false(meta$needs_membership[meta$measure == "neighbor_distance"])
  expect_identical(meta$orientation[meta$measure == "neighbor_distance"],
                   "higher")
  expect_true("neighbor_distance" %in% .cg_no_mode_measures())
})
