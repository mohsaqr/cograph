# Equivalence tests moved from tests/testthat/test-audit-fixes.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
# (none)

# ---- equivalence tests ----

test_that("aggregate_duplicate_edges directed matches igraph::simplify", {
  # Build a directed igraph with duplicate 1->2 edges
  g <- igraph::make_empty_graph(n = 4, directed = TRUE)
  g <- igraph::add_edges(g, c(1,2, 1,2, 2,1, 1,3, 3,4))
  igraph::E(g)$weight <- c(0.3, 0.7, 0.5, 0.4, 0.6)

  g_simple <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE,
                                edge.attr.comb = list(weight = "sum"))

  # Extract igraph result as edge list
  ig_edges <- igraph::as_data_frame(g_simple, what = "edges")

  # Build the same edge list for cograph
  co_edges <- data.frame(
    from   = c(1, 1, 2, 1, 3),
    to     = c(2, 2, 1, 3, 4),
    weight = c(0.3, 0.7, 0.5, 0.4, 0.6)
  )
  co_agg <- aggregate_duplicate_edges(co_edges, method = "sum", directed = TRUE)

  # Both should produce 4 edges after merging 1->2 duplicates

  expect_equal(nrow(co_agg), nrow(ig_edges))

  # Compare edge weights — sort both by from,to for stable comparison
  ig_sorted <- ig_edges[order(ig_edges$from, ig_edges$to), ]
  co_sorted <- co_agg[order(co_agg$from, co_agg$to), ]
  expect_equal(co_sorted$weight, ig_sorted$weight)
  expect_equal(co_sorted$from, ig_sorted$from)
  expect_equal(co_sorted$to, ig_sorted$to)
})

test_that("aggregate_duplicate_edges undirected matches igraph::simplify", {
  # Undirected igraph: 1--2 with two edges, 2--1 with another
  g <- igraph::make_empty_graph(n = 3, directed = FALSE)
  g <- igraph::add_edges(g, c(1,2, 1,2, 2,3))
  igraph::E(g)$weight <- c(0.3, 0.7, 0.5)

  g_simple <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE,
                                edge.attr.comb = list(weight = "mean"))
  ig_edges <- igraph::as_data_frame(g_simple, what = "edges")

  co_edges <- data.frame(
    from   = c(1, 1, 2),
    to     = c(2, 2, 3),
    weight = c(0.3, 0.7, 0.5)
  )
  co_agg <- aggregate_duplicate_edges(co_edges, method = "mean", directed = FALSE)

  expect_equal(nrow(co_agg), nrow(ig_edges))

  ig_sorted <- ig_edges[order(ig_edges$from, ig_edges$to), ]
  co_sorted <- co_agg[order(co_agg$from, co_agg$to), ]
  expect_equal(co_sorted$weight, ig_sorted$weight)
})

test_that("simplify.cograph_network directed matches igraph::simplify", {
  # 5-node directed network with some reciprocal edges
  mat <- matrix(0, 5, 5)
  mat[1, 2] <- 0.3; mat[2, 1] <- 0.7
  mat[1, 3] <- 0.5; mat[3, 1] <- 0.2
  mat[2, 3] <- 0.4
  mat[4, 5] <- 0.9; mat[5, 4] <- 0.1
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  # igraph reference
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed", weighted = TRUE)
  g_simple <- igraph::simplify(g, remove.loops = TRUE, remove.multiple = TRUE)
  ig_n <- igraph::ecount(g_simple)

  # cograph
  net <- as_cograph(mat)
  net_s <- simplify(net)
  co_n <- nrow(get_edges(net_s))

  expect_equal(co_n, ig_n)
})
