# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("Expected Force counts event orders and boundary edges", {
  for (n in 4:7) {
    clique <- igraph::make_full_graph(n)
    expect_equal(unname(centrality_expected_force(clique)),
                 rep(log(2 * (n - 1) * (n - 2)), n))
  }
  star <- igraph::make_star(5, mode = "undirected")
  raw <- c(log(12), rep(log(3), 4))
  expect_equal(unname(centrality_expected_force(star)), raw)
  expect_equal(unname(centrality_modified_expected_force(star)),
               raw * log(2 * c(4, 1, 1, 1, 1)))
  expect_equal(unname(centrality_expected_force(star, normalized = TRUE)),
               raw / max(raw))
  expect_equal(unname(centrality_expected_force(igraph::make_ring(4))),
               rep(log(4), 4))
  # Diamond: identical infected sets arise by distinct edges;
  # boundary edges to the same outside node must each contribute.
  g <- igraph::make_graph(c(1, 2, 1, 3, 2, 4, 3, 4), directed = TRUE)
  # Seed1 outcomes: degrees2,2,1,1; probabilities1/3,1/3,1/6,1/6.
  expected <- -2 * (log(1 / 3) / 3 + log(1 / 6) / 6)
  expect_equal(unname(centrality_expected_force(g))[1], expected)
  expect_equal(unname(centrality_expected_force(
    igraph::make_star(5, mode = "out")
  )), c(log(12), rep(0, 4)))
  expect_equal(unname(centrality_expected_force(
    igraph::make_star(5, mode = "in")
  )), rep(0, 5))
  # This existing public measure is neighbor degree, not Expected Force.
  expect_equal(unname(centrality_expected(star)), c(4, 4, 4, 4, 4))
})

test_that("Expected Force defines empty and exhausted outcomes", {
  for (n in 0:3) {
    g <- igraph::make_empty_graph(n)
    expect_equal(unname(centrality_expected_force(g)), rep(0, n))
    expect_equal(unname(centrality_modified_expected_force(g)), rep(0, n))
  }
  triangle <- igraph::make_full_graph(3)
  expect_equal(unname(centrality_expected_force(triangle)), rep(0, 3))
  expect_equal(unname(centrality_modified_expected_force(triangle,
                                                         normalized = TRUE)),
               rep(0, 3))
  path <- igraph::make_tree(4, children = 1, mode = "out")
  expect_equal(unname(centrality_expected_force(path)), rep(0, 4))
})

test_that("Expected Force retains graph direction and API conventions", {
  g <- igraph::make_graph("Zachary")
  igraph::V(g)$name <- paste0("v", seq_len(igraph::vcount(g)))
  score <- centrality_expected_force(g)
  expect_identical(names(score), igraph::V(g)$name)
  expect_equal(centrality(g, measures = "expected_force")$expected_force,
               unname(score))
  adjusted <- centrality_modified_expected_force(g, exf_alpha = 3)
  expect_equal(adjusted, score * log(3 * igraph::degree(g)))
  expect_equal(centrality(g, measures = "modified_expected_force",
                          exf_alpha = 3)$modified_expected_force,
               unname(adjusted))
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  p <- rev(seq_along(score))
  expect_equal(centrality_expected_force(a[p, p]), score[p])
  h <- igraph::as_directed(g, mode = "mutual")
  h <- igraph::add_edges(h, c(1, 1, 1, 2, 1, 2))
  igraph::E(h)$weight <- seq_len(igraph::ecount(h))
  expect_equal(centrality_expected_force(
    h, simplify = FALSE, mode = "in", invert_weights = TRUE, cutoff = 1
  ), score)
  expect_equal(centrality_modified_expected_force(h, weighted = FALSE),
               centrality_modified_expected_force(g))
  components <- igraph::disjoint_union(igraph::delete_vertex_attr(g, "name"),
                                       igraph::make_ring(3))
  expect_equal(unname(centrality_expected_force(components)),
               c(unname(score), 0, 0, 0))
  meta <- subset(list_centralities(),
                 measure %in% c("expected_force", "modified_expected_force"))
  expect_false(any(meta$uses_weights))
  expect_false(any(meta$mode_aware))
  expect_false(any(meta$costly))
  for (bad in list(0, 1, -1, Inf, NA, "2", c(2, 3))) {
    expect_error(centrality_modified_expected_force(g, exf_alpha = bad),
                 "exf_alpha")
  }
  # Large alpha is valid even when multiplying it by degree would overflow.
  extreme <- centrality_modified_expected_force(g, exf_alpha = 1e308)
  expect_equal(extreme, score * (log(1e308) + log(igraph::degree(g))))
  near_one <- 1 + .Machine$double.eps
  star <- igraph::make_star(5, mode = "undirected")
  got <- centrality_modified_expected_force(star, exf_alpha = near_one)
  expect_equal(unname(got[2]) / log(near_one), log(3))
})
