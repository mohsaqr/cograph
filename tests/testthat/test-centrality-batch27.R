test_that("X-degree counts nonbacktracking walks through the middle", {
  path <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  expect_equal(unname(centrality_x_degree(path)), c(0, 0, 2, 0, 0))
  expect_equal(unname(centrality_x_degree(path, normalized = TRUE)),
               c(0, 0, 1, 0, 0))
  for (n in 3:7) {
    expect_equal(unname(centrality_x_degree(igraph::make_ring(n))), rep(2, n))
    expected <- (n - 1) * (n - 2)^3
    expect_equal(unname(centrality_x_degree(igraph::make_full_graph(n))),
                 rep(expected, n))
    expect_equal(unname(centrality_x_degree(igraph::make_star(n))), rep(0, n))
  }
  for (n in 0:3) {
    expect_equal(unname(centrality_x_degree(igraph::make_empty_graph(n))),
                 rep(0, n))
  }
  expect_equal(unname(centrality_x_degree(igraph::make_star(5),
                                          normalized = TRUE)), rep(0, 5))
  triangle_leaf <- igraph::make_graph(c(1, 2, 2, 3, 3, 1, 1, 4),
                                      directed = FALSE)
  expect_equal(unname(centrality_x_degree(triangle_leaf)), c(2, 4, 4, 0))
})

test_that("X-degree preserves labels and documents its skeleton projection", {
  g <- igraph::make_graph("Zachary")
  igraph::V(g)$name <- paste0("v", seq_len(igraph::vcount(g)))
  score <- centrality_x_degree(g)
  expect_identical(names(score), igraph::V(g)$name)
  # The pinned author's README identifies zero-based vertex 2 as maximum.
  expect_identical(names(which.max(score)), "v3")
  expect_equal(centrality(g, measures = "x_degree")$x_degree, unname(score))
  expect_equal(centrality_x_degree(g, normalized = TRUE), score / max(score))
  perm <- rev(seq_along(score))
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  expect_equal(centrality_x_degree(a[perm, perm]), score[perm])
  h <- igraph::as_directed(g, mode = "mutual")
  h <- igraph::add_edges(h, c(1, 1, 1, 2, 1, 2))
  igraph::E(h)$weight <- seq_len(igraph::ecount(h))
  expect_equal(centrality_x_degree(h, simplify = FALSE, mode = "in",
                                   invert_weights = TRUE, cutoff = 1), score)
  expect_equal(centrality_x_degree(h, weighted = FALSE), score)
  components <- igraph::disjoint_union(igraph::delete_vertex_attr(g, "name"),
                                       igraph::make_ring(3))
  expect_equal(unname(centrality_x_degree(components)),
               c(unname(score), 2, 2, 2))
  meta <- subset(list_centralities(), measure == "x_degree")
  expect_false(meta$uses_weights)
  expect_false(meta$mode_aware)
  expect_false(meta$costly)
})
