# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("neighbor distance follows the source's nested sums, not shells", {
  # Liu, Tang, Zhou & Do arXiv:1511.00441v1 eq. (1): the k-th term sums the
  # benchmark centrality over the endpoints of the non-backtracking walks of
  # length k, once per walk. On the triangle plus a pendant the two-step term
  # of node A revisits B and C, which distance shells exclude entirely.
  g <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4), directed = FALSE)
  walks <- c(4.16, 3.24, 3.24, 1.76)
  shells <- c(4.00, 3.04, 3.04, 1.76)
  expect_equal(unname(centrality_neighbor_distance(g)), walks)
  expect_false(isTRUE(all.equal(walks, shells)))
  # Hand check of node A: theta = 3, the three neighbour degrees sum to 5,
  # and the two non-backtracking two-walks A-B-C and A-C-B end on degree 2.
  expect_equal(unname(centrality_neighbor_distance(g))[1],
               3 + 0.2 * (2 + 2 + 1) + 0.04 * (2 + 2))
  # A four-cycle separates the readings differently: C is reached from A by
  # two distinct non-backtracking two-walks, so its degree counts twice.
  expect_equal(unname(centrality_neighbor_distance(igraph::make_ring(4))),
               rep(2.96, 4))
})

test_that("neighbor distance matches the analytic star and clique families", {
  for (q in c(1, 2, 3, 5, 8, 20)) {
    star <- igraph::make_star(q + 1, mode = "undirected")
    # The centre's walks die after one step; a leaf reaches the other q - 1
    # leaves in two steps.
    expect_equal(unname(centrality_neighbor_distance(star)),
                 c(1.2 * q, rep(1 + 0.2 * q + 0.04 * (q - 1), q)))
    # A star is a tree, so every core number is one when it has an edge.
    expect_equal(unname(centrality_neighbor_distance(star,
                                                     nd_mass = "coreness")),
                 c(1 + 0.2 * q, rep(1.2 + 0.04 * (q - 1), q)))
  }
  for (m in c(2, 3, 5, 7)) {
    # Every step of a clique walk has m - 2 continuations and every endpoint
    # has degree m - 1, which is also its core number.
    clique <- igraph::make_full_graph(m)
    for (order in 0:4) {
      steps <- seq_len(order)
      expected <- rep((m - 1) +
                        sum(0.2^steps * (m - 1)^2 * (m - 2)^(steps - 1)), m)
      expect_equal(unname(centrality_neighbor_distance(clique,
                                                       nd_order = order)),
                   expected)
      expect_equal(unname(centrality_neighbor_distance(clique,
                                                       nd_order = order,
                                                       nd_mass = "coreness")),
                   expected)
    }
  }
})

test_that("neighbor distance matches the analytic ring and path families", {
  for (m in 3:9) {
    # A cycle offers exactly two non-backtracking walks of every length, each
    # ending on a degree-two node.
    for (order in 0:5) {
      expect_equal(unname(centrality_neighbor_distance(igraph::make_ring(m),
                                                       nd_order = order)),
                   rep(2 + 4 * sum(0.2^seq_len(order)), m))
    }
  }
  path <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = FALSE)
  expect_equal(unname(centrality_neighbor_distance(path)),
               c(1.48, 2.64, 2.64, 1.48))
  # A path is a tree, so here the source and the Zoo's distance shells agree.
  expect_equal(unname(centrality_neighbor_distance(path, nd_order = 3)),
               c(1.488, 2.64, 2.64, 1.488))
  # Order zero drops every sum and returns the benchmark centrality itself.
  expect_equal(unname(centrality_neighbor_distance(path, nd_order = 0)),
               c(1, 2, 2, 1))
  expect_equal(unname(centrality_neighbor_distance(path, nd_order = 0,
                                                   nd_mass = "coreness")),
               rep(1, 4))
  expect_equal(unname(centrality_neighbor_distance(path, nd_decay = 0)),
               c(1, 2, 2, 1))
})

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

test_that("neighbor distance normalizes and takes the source's parameters", {
  g <- igraph::make_graph(c(1, 2, 1, 3, 2, 3, 1, 4), directed = FALSE)
  raw <- unname(centrality_neighbor_distance(g))
  expect_equal(unname(centrality_neighbor_distance(g, normalized = TRUE)),
               raw / max(raw))
  # a = 1 removes the decay, so every walk endpoint counts in full.
  expect_equal(unname(centrality_neighbor_distance(g, nd_decay = 1)),
               c(3 + 5 + 4, 2 + 5 + 6, 2 + 5 + 6, 1 + 3 + 4))
  # A negative decay is outside the source's [0, 1] but is accepted.
  expect_equal(unname(centrality_neighbor_distance(g, nd_decay = -0.5,
                                                   nd_order = 3,
                                                   nd_mass = "coreness")),
               c(0, 0.625, 0.625, 0.5))
  expect_equal(unname(centrality_neighbor_distance(g, nd_decay = 0.5,
                                                   nd_order = 1)),
               c(3 + 2.5, 2 + 2.5, 2 + 2.5, 1 + 1.5))
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
