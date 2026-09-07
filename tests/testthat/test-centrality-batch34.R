test_that("NINL reproduces every entry in Zhu and Wang's Table 1", {
  edges <- c(1, 3, 2, 4, 3, 4, 3, 5, 3, 6, 4, 5, 4, 7, 4, 8, 4, 9,
             5, 6, 5, 8, 6, 8, 8, 9, 9, 10, 9, 11, 9, 12,
             10, 12, 11, 12, 12, 13)
  g <- igraph::make_graph(edges, n = 13, directed = FALSE)
  expected <- rbind(
    c(29, 37, 37, 38, 37, 37, 37, 38, 38, 37, 37, 37, 24),
    c(37, 38, 141, 224, 150, 112, 38, 150, 187, 75, 75, 136, 37),
    c(141, 224, 523, 704, 627, 441, 224, 673, 660, 323, 323, 374, 136),
    c(523, 704, 1913, 2931, 2341, 1823, 704, 2432, 2397,
      1034, 1034, 1442, 374)
  )
  expect_equal(sum(igraph::distances(g)), 358)
  for (p in 0:3) {
    expect_equal(unname(centrality_ninl(g, ninl_order = p)), expected[p + 1, ])
    expect_equal(centrality(g, measures = "ninl", ninl_order = p)$ninl,
                 expected[p + 1, ])
    expect_equal(unname(centrality_ninl(g, ninl_order = p, normalized = TRUE)),
                 expected[p + 1, ] / max(expected[p + 1, ]))
  }
})

test_that("NINL preserves finite walk parity and handles large powers", {
  g <- igraph::make_star(6, mode = "undirected")
  for (p in c(0:5, 2048, 2049, 2^53 - 2, 2^53 - 1)) {
    expected <- if (p %% 2 == 0) rep(1, 6) else c(1, rep(.2, 5))
    expect_equal(unname(centrality_ninl(g, ninl_order = p, normalized = TRUE)),
                 expected, tolerance = 1e-12)
  }
  expect_error(centrality_ninl(g, ninl_order = 2048), "finite double")
  expect_equal(unname(centrality_ninl(g, ninl_order = 0)), rep(10, 6))
  expect_equal(unname(centrality_ninl(g, ninl_order = 1)), c(50, rep(10, 5)))
  expect_equal(unname(centrality_ninl(g, ninl_order = 0, ninl_radius = 0)),
               c(5, rep(1, 5)))
})

test_that("NINL radius and disconnected extensions are explicit", {
  path <- igraph::make_tree(7, children = 1, mode = "undirected")
  # Mean length is 8/3, so automatic radius3 excludes distant endpoints.
  expect_equal(centrality_ninl(path, ninl_order = 0),
               centrality_volume(path, volume_radius = 3))
  g <- igraph::disjoint_union(
    path, igraph::make_empty_graph(1, directed = FALSE)
  )
  # One unreachable pair makes the global automatic radius infinite.
  expect_equal(unname(centrality_ninl(g, ninl_order = 0)), c(rep(12, 7), 0))
  for (radius in c(0:3, Inf)) {
    expect_equal(centrality_ninl(g, ninl_order = 0, ninl_radius = radius),
                 centrality_volume(g, volume_radius = radius))
  }
  for (n in 0:2) {
    expect_length(centrality_ninl(igraph::make_empty_graph(n)), n)
    expect_equal(unname(centrality_ninl(igraph::make_empty_graph(n))),
                 numeric(n))
  }
})

test_that("NINL uses the undirected simple skeleton through the public API", {
  edges <- c("b", "a", "c", "b", "c", "b", "a", "a")
  g <- igraph::graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE))
  igraph::E(g)$weight <- c(.2, 3, 4, 7)
  plain <- igraph::as_undirected(igraph::simplify(g))
  expect_equal(centrality_ninl(g, mode = "in", cutoff = 1,
                               invert_weights = TRUE, simplify = FALSE,
                               loops = TRUE), centrality_ninl(plain))
  expect_named(centrality_ninl(g), igraph::V(g)$name)
  meta <- list_centralities()
  expect_true("ninl" %in% meta$measure)
  expect_false(meta$uses_weights[meta$measure == "ninl"])
})

test_that("NINL rejects invalid parameters even for empty graphs", {
  g <- igraph::make_empty_graph(0)
  for (p in list(-1, .5, NA_real_, NaN, Inf, numeric(), c(1, 2),
                 "3", TRUE, 2^53)) {
    expect_error(centrality_ninl(g, ninl_order = p), "ninl_order")
  }
  for (r in list(-1, .5, NA_real_, NaN, -Inf, numeric(), c(1, 2), "3", TRUE)) {
    expect_error(centrality_ninl(g, ninl_radius = r), "ninl_radius")
  }
})
