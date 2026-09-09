# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("volume counts full-graph degrees in a closed neighbourhood", {
  path <- igraph::make_tree(5, children = 1, mode = "undirected")
  expect_equal(unname(centrality_volume(path, volume_radius = 0)),
               c(1, 2, 2, 2, 1))
  expect_equal(unname(centrality_volume(path, volume_radius = 1)),
               c(3, 5, 6, 5, 3))
  expect_equal(unname(centrality_volume(path)), c(5, 7, 8, 7, 5))
  expect_equal(unname(centrality_volume(path, volume_radius = Inf)), rep(8, 5))
  disconnected <- igraph::disjoint_union(path, igraph::make_full_graph(3),
                                         igraph::make_empty_graph(1, FALSE))
  expect_equal(unname(centrality_volume(disconnected, volume_radius = Inf)),
               c(rep(8, 5), rep(6, 3), 0))
  for (bad in list(-1, -Inf, NA_real_, NaN, 1.5, "2", c(1, 2), NULL)) {
    expect_error(centrality_volume(path, volume_radius = bad), "volume_radius")
  }
})

test_that("MCC adds maximal cliques without counting contained cliques", {
  # K4 on 1:4, a triangle on 1/4/5, pendant edge 5-6 and isolate 7.
  edges <- c(as.vector(utils::combn(1:4, 2)), 1, 5, 4, 5, 5, 6)
  g <- igraph::make_graph(edges, n = 7, directed = FALSE)
  expect_equal(unname(centrality_mcc(g)), c(8, 6, 6, 8, 3, 1, 0))
  expect_equal(unname(centrality_mcc(igraph::make_full_graph(5))), rep(24, 5))
  tree <- igraph::make_star(8, mode = "undirected")
  expect_equal(unname(centrality_mcc(tree)), as.numeric(igraph::degree(tree)))
  # Clique sizes beyond this boundary cannot have finite raw MCC scores.
  expect_error(centrality_mcc(igraph::make_full_graph(172)), "double precision")
})

test_that("volume and MCC retain input order and document graph projection", {
  a <- matrix(c(7, 0, 0, 2, 8, 0, 3, 5, 9), 3, 3,
              dimnames = list(c("Z", "A", "B"), c("Z", "A", "B")))
  b <- (a != 0 | t(a) != 0) * 1
  diag(b) <- 0
  measures <- c("volume", "mcc")
  got <- centrality(a, measures = measures, directed = TRUE)
  expect_equal(got, centrality(b, measures = measures))
  expect_identical(got$node, c("Z", "A", "B"))
  # Parallel edges and loops do not change either skeleton measure.
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 1, 2, 3, 3, 3), directed = TRUE)
  simple <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  expect_equal(centrality(g, measures = measures),
               centrality(simple, measures = measures))
  for (m in measures) {
    normalized <- centrality(a, measures = m, normalized = TRUE)[[m]]
    expect_equal(normalized, got[[m]] / max(got[[m]]))
    for (n in 0:3) {
      expect_equal(centrality(igraph::make_empty_graph(n), measures = m)[[m]],
                   numeric(n))
    }
  }
  meta <- list_centralities()
  expect_false(any(meta$mode_aware[meta$measure %in% measures]))
  expect_false(any(meta$uses_weights[meta$measure %in% measures]))
  expect_true(meta$costly[meta$measure == "mcc"])
  expect_false(meta$costly[meta$measure == "volume"])
  expect_equal(centrality(simple, type = "basic", include = "mcc")$mcc,
               unname(centrality_mcc(simple)))
})
