test_that("exogenous bases agree with analytical undirected scores", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_exogenous(star)), c(52, rep(13, 4)))
  degree <- centrality_exogenous(star, exogenous_base = "degree")
  expect_equal(unname(degree), c(4, rep(1, 4)))
  between <- centrality_exogenous(star, exogenous_base = "betweenness")
  expect_equal(unname(between), c(0, rep(3, 4)))
  ring <- igraph::make_ring(4)
  expect_equal(unname(centrality_exogenous(ring)), rep(8, 4))
  between <- centrality_exogenous(ring, exogenous_base = "betweenness")
  expect_equal(unname(between), rep(0.5, 4))
  clique <- igraph::make_full_graph(4)
  expect_equal(unname(centrality_exogenous(clique)), rep(9, 4))
  between <- centrality_exogenous(clique, exogenous_base = "betweenness")
  expect_equal(unname(between), numeric(4))
})

test_that("exogenous direction applies to the base centrality", {
  g <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
  for (base in c("degree", "betweenness", "reverse_closeness")) {
    expected <- switch(base, degree = c(0, 1, 1), betweenness = c(1, 0, 1),
                       reverse_closeness = c(0, 3, 3))
    out <- centrality_exogenous(g, mode = "out", exogenous_base = base)
    incoming <- centrality_exogenous(g, mode = "in", exogenous_base = base)
    expect_equal(unname(out), expected)
    expect_equal(unname(incoming), rev(expected))
    skeleton <- igraph::as_undirected(g)
    expect_equal(centrality_exogenous(g, exogenous_base = base),
                 centrality_exogenous(skeleton, exogenous_base = base))
  }
})

test_that("exogenous conventions keep original size and signed contributions", {
  g <- igraph::make_graph(c(1, 2), n = 3, directed = FALSE)
  expect_equal(unname(centrality_exogenous(g)), c(2, 2, 0))
  expect_equal(unname(centrality_exogenous(igraph::delete_vertices(g, 3))),
               c(1, 1))
  for (n in 0:3) {
    for (base in c("degree", "betweenness", "reverse_closeness")) {
      score <- centrality_exogenous(igraph::make_empty_graph(n),
                                    exogenous_base = base)
      expect_equal(unname(score), numeric(n))
    }
  }
  # Two universal vertices compete to broker three independent vertices.
  g <- igraph::make_full_bipartite_graph(2, 3)
  between <- centrality_exogenous(g, exogenous_base = "betweenness")
  expect_true(any(between < 0))
  normalized <- centrality_exogenous(g, exogenous_base = "betweenness",
                                     normalized = TRUE)
  expect_equal(normalized, between / max(between))
  expect_true(any(normalized < 0))
})

test_that("exogenous input projections preserve labels and metadata", {
  a <- matrix(c(0, 2, 0, 0, 0, 7, 0, 0, 0), 3, byrow = TRUE,
              dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  expected <- centrality_exogenous(a, mode = "out", directed = TRUE)
  expect_identical(names(expected), rownames(a))
  diag(a) <- 5
  projected <- centrality_exogenous(a, mode = "out", directed = TRUE,
                                    loops = TRUE, invert_weights = TRUE)
  expect_equal(projected, expected)
  order <- c(3, 1, 2)
  permuted <- centrality_exogenous(a[order, order], mode = "out",
                                   directed = TRUE)
  expect_equal(permuted, expected[order])
  normalized <- centrality_exogenous(a, mode = "out", directed = TRUE,
                                     normalized = TRUE)
  expect_equal(normalized, expected / max(expected))
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3), directed = TRUE)
  parallel <- centrality_exogenous(g, mode = "out", simplify = FALSE)
  expect_equal(unname(parallel), unname(expected))
  meta <- list_centralities()
  expect_false(meta$uses_weights[meta$measure == "exogenous"])
  expect_true(meta$mode_aware[meta$measure == "exogenous"])
  expect_true(meta$costly[meta$measure == "exogenous"])
})

test_that("exogenous rejects unsupported or ambiguous base names", {
  g <- igraph::make_empty_graph(0)
  for (bad in list(NULL, NA_character_, "closeness", "eigenvector", "rev",
                   c("degree", "betweenness"), 1)) {
    expect_error(centrality_exogenous(g, exogenous_base = bad),
                 "exogenous_base")
  }
})
