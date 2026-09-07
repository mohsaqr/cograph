test_that("mixed gravity preserves asymmetric masses on stars", {
  for (q in c(2:6, 20)) {
    g <- igraph::make_star(q + 1, mode = "undirected")
    leaf <- q + (q - 1) / 4
    expect_equal(unname(centrality_mixed_gravity(g)), c(q, rep(leaf, q)))
    expect_equal(unname(centrality_extended_mixed_gravity(g)),
                 c(q * leaf, rep(q, q)))
    expect_equal(unname(centrality_mixed_gravity(g, gravity_radius = 1)),
                 rep(q, q + 1))
    expect_equal(unname(centrality_extended_mixed_gravity(
      g, gravity_radius = 1
    )), c(q^2, rep(q, q)))
  }
})

test_that("mixed gravity uses distance shells and raw outer neighbor sums", {
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  mixed <- c(49 / 18, 65 / 18, 9 / 2, 65 / 18, 49 / 18)
  extended <- c(65 / 18, 65 / 9, 65 / 9, 65 / 9, 65 / 18)
  expect_equal(unname(centrality_mixed_gravity(g)), mixed)
  expect_equal(unname(centrality_extended_mixed_gravity(g)), extended)
  expect_equal(unname(centrality_mixed_gravity(g, gravity_radius = 1)),
               c(2, 3, 4, 3, 2))
  expect_equal(unname(centrality_extended_mixed_gravity(g, gravity_radius = 1)),
               c(3, 6, 6, 6, 3))
  expect_equal(unname(centrality_extended_mixed_gravity(g, normalized = TRUE)),
               extended / max(extended))
  # Source node2 reaches node5 in three hops, four hops from focal node1.
  expect_gt(centrality_extended_mixed_gravity(g)[1],
            centrality_extended_mixed_gravity(g, gravity_radius = 2)[1])
  expect_equal(centrality_mixed_gravity(g, gravity_radius = 1.9),
               centrality_mixed_gravity(g, gravity_radius = 1))
  expect_equal(centrality_mixed_gravity(g, gravity_radius = NULL),
               centrality_mixed_gravity(g, gravity_radius = Inf))
  expect_gt(centrality_mixed_gravity(g, gravity_radius = Inf)[1], mixed[1])
  expect_equal(centrality_mixed_gravity(g, gravity_radius = "auto"),
               centrality_mixed_gravity(g, gravity_radius = 1))
})

test_that("mixed gravity agrees with regular graphs and empty sums", {
  wrappers <- list(centrality_mixed_gravity, centrality_extended_mixed_gravity)
  for (j in seq_along(wrappers)) {
    wrapper <- wrappers[[j]]
    for (n in c(3, 5, 8)) {
      expected <- (n - 1)^(j + 2)
      expect_equal(unname(wrapper(igraph::make_full_graph(n))),
                   rep(expected, n))
    }
    for (n in 0:3) {
      expect_equal(unname(wrapper(igraph::make_empty_graph(n))), numeric(n))
    }
    g <- igraph::make_ring(7)
    expect_equal(unname(wrapper(g, gravity_radius = 0)), numeric(7))
    expect_equal(unname(wrapper(g, gravity_radius = .5)), numeric(7))
    bigger <- igraph::disjoint_union(
      g, igraph::make_full_graph(5),
      igraph::make_empty_graph(1, directed = FALSE)
    )
    expect_equal(unname(wrapper(bigger)[1:7]), unname(wrapper(g)))
    expect_equal(unname(tail(wrapper(bigger), 1)), 0)
  }
})

test_that("mixed gravity input projections and public contracts are explicit", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7)
  igraph::V(g)$name <- c("b", "c", "a")
  measures <- c("mixed_gravity", "extended_mixed_gravity")
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 2)
  for (measure in measures) {
    wrapper <- get(paste0("centrality_", measure))
    score <- wrapper(g)
    expect_named(score, c("b", "c", "a"))
    expect_equal(centrality(g, measures = measure)[[measure]], unname(score))
    expect_equal(wrapper(g, simplify = FALSE, mode = "in", loops = TRUE,
                         invert_weights = TRUE, cutoff = 1,
                         gravity_mass = "degree"), score)
    expect_equal(wrapper(g, weighted = FALSE), score)
    expect_equal(wrapper(a[perm, perm]), score[perm])
    for (radius in list(-1, NA_real_, NaN, "3", TRUE, 1:2, numeric())) {
      expect_error(wrapper(g, gravity_radius = radius), "gravity_radius")
    }
    expect_error(wrapper(igraph::make_empty_graph(0), gravity_radius = -1),
                 "gravity_radius")
    meta <- list_centralities()
    expect_false(meta$uses_weights[meta$measure == measure])
    expect_true(measure %in% .cg_no_mode_measures())
  }
})
