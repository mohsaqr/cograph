test_that("proximal roles follow Brandes and exclude endpoints", {
  path <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = TRUE)
  score <- function(variant, g = path, ...) {
    unname(centrality_proximal_betweenness(g, variant, ...))
  }
  expect_equal(score("source"), c(0, 1, 2, 0))
  expect_equal(score("target"), c(0, 2, 1, 0))
  expect_equal(score("sum"), c(0, 3, 3, 0))
  expect_equal(score("union"), c(0, 2, 2, 0))
  expect_equal(score("source", normalized = TRUE), c(0, .5, 1, 0))
  pair <- igraph::make_graph(c(1, 2), directed = TRUE)
  for (variant in c("source", "target", "sum", "union")) {
    expect_equal(score(variant, pair), c(0, 0))
    expect_equal(score(variant, igraph::make_full_graph(5)), rep(0, 5))
    expect_equal(score(variant, igraph::make_empty_graph(0)), numeric())
    expect_equal(score(variant, igraph::make_empty_graph(4),
                       normalized = TRUE), rep(0, 4))
  }
  undirected <- igraph::as_undirected(path)
  expect_equal(score("source", undirected), c(0, 3, 3, 0))
  expect_equal(score("target", undirected), c(0, 3, 3, 0))
  expect_equal(score("sum", undirected), c(0, 6, 6, 0))
  expect_equal(score("union", undirected), c(0, 4, 4, 0))
  expect_equal(score("source", igraph::make_star(5, mode = "undirected")),
               c(12, 0, 0, 0, 0))
})

test_that("proximal ties receive fractions and preserve directed paths", {
  # Three equally short paths from 1 to 6: two use penultimate 4,
  # one uses penultimate 5. Other two-edge paths contribute as well.
  g <- igraph::make_graph(c(1, 2, 1, 3, 2, 4, 3, 4, 3, 5, 4, 6, 5, 6),
                          directed = TRUE)
  source <- c(0, .5, 1.5, 13 / 6, 5 / 6, 0)
  target <- c(0, 5 / 6, 13 / 6, 1.5, .5, 0)
  expect_equal(unname(centrality_proximal_betweenness(g)), source)
  expect_equal(unname(centrality_proximal_betweenness(g, "target")), target)
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  expect_equal(unname(centrality_proximal_betweenness(t(a))), target)
  igraph::V(g)$name <- letters[1:6]
  named <- centrality_proximal_betweenness(g)
  expect_identical(names(named), letters[1:6])
  expect_equal(centrality(g, measures = "proximal_betweenness",
                          proximal_variant = "target")$proximal_betweenness,
               target)
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  perm <- c(6, 4, 2, 5, 1, 3)
  expect_equal(centrality_proximal_betweenness(a[perm, perm]), named[perm])
  h <- igraph::add_edges(g, c(1, 1, 1, 2, 1, 2))
  igraph::E(h)$weight <- seq_len(igraph::ecount(h))
  expect_equal(centrality_proximal_betweenness(
    h, simplify = FALSE, invert_weights = TRUE, cutoff = 1, mode = "in"
  ), named)
  expect_equal(centrality_proximal_betweenness(h, weighted = FALSE), named)
  component <- igraph::disjoint_union(
    igraph::delete_vertex_attr(g, "name"),
    igraph::make_full_graph(3, directed = TRUE)
  )
  expect_equal(unname(centrality_proximal_betweenness(component)),
               c(source, 0, 0, 0))
  expect_error(centrality_proximal_betweenness(g, "invalid"), "arg")
  meta <- subset(list_centralities(), measure == "proximal_betweenness")
  expect_false(meta$uses_weights)
  expect_false(meta$mode_aware)
  expect_false(meta$costly)
})

test_that("proximal path-count overflow is explicit", {
  # Two vertices in each of 1030 layers, all four arcs between layers.
  # The first source alone reaches a layer with more than DBL_MAX paths.
  layers <- 1030L
  edges <- c(1, 2, 1, 3)
  for (j in seq_len(layers - 1L)) {
    from <- 2L * j + 0:1
    to <- from + 2L
    edges <- c(edges, as.vector(t(expand.grid(from, to))))
  }
  g <- igraph::make_graph(edges, directed = TRUE)
  expect_error(centrality_proximal_betweenness(g), "path count overflow")
})
