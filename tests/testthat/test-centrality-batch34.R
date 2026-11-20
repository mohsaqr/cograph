# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("NINL refuses a raw power beyond double precision", {
  # The star's closed forms at every order are equivalence tests
  # (tests/equivalence/test-centrality-zoo-b-ninl.R).
  g <- igraph::make_star(6, mode = "undirected")
  expect_error(centrality_ninl(g, ninl_order = 2048), "finite double")
})

test_that("NINL radius and disconnected extensions are explicit", {
  path <- igraph::make_tree(7, children = 1, mode = "undirected")
  # Mean length is 8/3, so automatic radius3 excludes distant endpoints.
  expect_equal(centrality_ninl(path, ninl_order = 0),
               centrality_volume(path, volume_radius = 3))
  g <- igraph::disjoint_union(
    path, igraph::make_empty_graph(1, directed = FALSE)
  )
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
