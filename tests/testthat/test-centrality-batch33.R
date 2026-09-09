# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("localized bridging reproduces Macker's complete synthetic table", {
  edges <- c("A", "B", "A", "C", "A", "D", "A", "E", "B", "J",
             "D", "J", "J", "K", "E", "F", "F", "G", "F", "H",
             "G", "I", "H", "I")
  g <- igraph::graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE),
                                   directed = FALSE)
  # Figure1 and TableI, original MILCOM2016 paper pp2-3, visually read.
  coefficient <- c(.1, 6 / 7, 4, 6 / 7, 6 / 7, 2 / 9,
                   3 / 5, 3 / 5, .5, 1 / 6, 3)
  one_hop <- c(6, 1, 0, 1, 1, 3, 1, 1, 1, 3, 0)
  two_hop <- c(11.5, 3, 0, 3, 12, 6.5, 1, 1, .5, 3.5, 0)
  order <- LETTERS[1:11]
  expect_equal(unname(centrality_localized_bridging(g)[order]),
               one_hop * coefficient)
  expect_equal(unname(centrality_extended_local_bridging(g)[order]),
               two_hop * coefficient)
  a <- as.matrix(igraph::as_adjacency_matrix(g))[order, order]
  for (i in seq_len(11)) {
    first <- which(a[i, ] > 0)
    second <- which(colSums(a[first, , drop = FALSE]) > 0)
    ids <- c(i, setdiff(unique(c(first, second)), i))
    expect_equal(cograph:::.cg_focal_ego_credit(a[ids, ids]), two_hop[i])
  }
  # Published two-hop I credit is .5, lower than its one-hop credit1:
  # adding alter-to-alter paths can reduce brokerage, so never clamp upward.
})

test_that("localized bridging uses induced ego paths and original degrees", {
  g <- igraph::make_tree(5, children = 1, mode = "undirected")
  expect_equal(unname(centrality_localized_bridging(g)),
               c(0, 1 / 3, .5, 1 / 3, 0))
  expect_equal(unname(centrality_extended_local_bridging(g)),
               c(0, 2 / 3, 2, 2 / 3, 0))
  for (r in 2:5) {
    star <- igraph::make_star(r + 1, mode = "undirected")
    expect_equal(unname(centrality_localized_bridging(star)),
                 c((r - 1) / (2 * r), rep(0, r)))
    expect_equal(centrality_extended_local_bridging(star),
                 centrality_localized_bridging(star))
  }
  clique <- igraph::make_full_graph(4)
  expect_equal(unname(centrality_localized_bridging(clique)), rep(0, 4))
  expect_equal(unname(centrality_extended_local_bridging(clique)), rep(0, 4))
  # Preserve the existing degree-only public score, but do not call it LBC.
  expect_equal(unname(centrality_local_bridging(clique)), rep(1 / 9, 4))
})

test_that("localized bridging public API defines topology and empty cases", {
  g <- igraph::make_tree(6, children = 1, mode = "undirected")
  igraph::V(g)$name <- paste0("v", 1:6)
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  p <- 6:1
  for (measure in c("localized_bridging", "extended_local_bridging")) {
    wrapper <- get(paste0("centrality_", measure))
    score <- wrapper(g)
    expect_identical(names(score), igraph::V(g)$name)
    expect_equal(centrality(g, measures = measure)[[measure]], unname(score))
    expect_equal(wrapper(g, normalized = TRUE), score / max(score))
    expect_equal(wrapper(a[p, p]), score[p])
    h <- igraph::as_directed(g, mode = "arbitrary")
    h <- igraph::add_edges(h, c(1, 1, 1, 2, 2, 1, 1, 2))
    igraph::E(h)$weight <- seq_len(igraph::ecount(h))
    expect_equal(wrapper(h, simplify = FALSE, mode = "in",
                         invert_weights = TRUE, cutoff = 1), score)
    for (n in 0:3) {
      expect_equal(unname(wrapper(igraph::make_empty_graph(n),
                                  normalized = TRUE)), rep(0, n))
    }
    disconnected <- igraph::disjoint_union(
      igraph::delete_vertex_attr(g, "name"), igraph::make_full_graph(3)
    )
    expect_equal(unname(wrapper(disconnected)), c(unname(score), 0, 0, 0))
    meta <- list_centralities()
    meta <- meta[meta$measure == measure, ]
    expect_false(meta$uses_weights)
    expect_false(meta$mode_aware)
    expect_identical(meta$costly, measure == "extended_local_bridging")
  }
})
