test_that("centrality works with matrix input", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  result <- centrality(adj)

  expect_s3_class(result, "data.frame")
  expect_true("node" %in% names(result))
  expect_true("degree_all" %in% names(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$node, c("A", "B", "C"))
})

test_that("centrality mode suffix works", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")

  result_in <- centrality(adj, measures = "degree", mode = "in")
  result_out <- centrality(adj, measures = "degree", mode = "out")
  result_all <- centrality(adj, measures = "degree", mode = "all")

  expect_true("degree_in" %in% names(result_in))
  expect_true("degree_out" %in% names(result_out))
  expect_true("degree_all" %in% names(result_all))
})

test_that("centrality matches igraph directly", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")

  result <- centrality(adj, measures = "degree")
  expect_equal(result$degree_all, unname(igraph::degree(g)))

  result_bw <- centrality(adj, measures = "betweenness")
  expect_equal(result_bw$betweenness, unname(igraph::betweenness(g)))
})

test_that("centrality works with igraph input", {
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]
  result <- centrality(g)

  expect_equal(nrow(result), 5)
  expect_equal(result$node, LETTERS[1:5])
})

test_that("normalization works", {
  adj <- matrix(c(0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  result <- centrality(adj, measures = "degree", normalized = TRUE)

  expect_true(max(result$degree_all) == 1)
  expect_true(min(result$degree_all) >= 0)
})

test_that("specific measures selection works", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- centrality(adj, measures = c("degree", "betweenness"))

  expect_true("degree_all" %in% names(result))
  expect_true("betweenness" %in% names(result))
  expect_false("pagerank" %in% names(result))
  expect_false("closeness_all" %in% names(result))
})

test_that("invalid measure throws error", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(
    centrality(adj, measures = "invalid_measure"),
    "Unknown measures"
  )
})

test_that("digits parameter rounds output", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- centrality(adj, measures = "pagerank", digits = 2)

  # Check that values are rounded (no more than 2 decimal places)
  rounded_values <- round(result$pagerank, 2)
  expect_equal(result$pagerank, rounded_values)
})

test_that("sort_by parameter orders results", {
  adj <- matrix(c(0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]

  result <- centrality(adj, measures = "degree", sort_by = "degree_all")

  # First row should have highest degree
  expect_equal(result$node[1], "A")
  # Values should be descending
  expect_true(all(diff(result$degree_all) <= 0))
})

test_that("sort_by with invalid column throws error", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(
    centrality(adj, measures = "degree", sort_by = "nonexistent"),
    "sort_by column"
  )
})

test_that("directed parameter overrides auto-detection", {
  # Symmetric matrix would auto-detect as undirected
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")

  # Force directed
  result_directed <- centrality(adj, measures = "degree",
                                mode = "in", directed = TRUE)
  result_undirected <- centrality(adj, measures = "degree", directed = FALSE)

  # In directed mode with symmetric matrix, in-degree should equal out-degree
  expect_true("degree_in" %in% names(result_directed))
})

test_that("loops parameter removes self-loops when FALSE", {
  adj <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), 3, 3)  # Has self-loops
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")

  result_with_loops <- centrality(adj, measures = "degree", loops = TRUE)
  result_no_loops <- centrality(adj, measures = "degree", loops = FALSE)

  # Degree should be lower without self-loops
  expect_true(all(result_no_loops$degree_all <= result_with_loops$degree_all))
})

test_that("shortcut functions return named vectors", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")

  deg <- centrality_degree(adj)
  expect_true(is.numeric(deg))
  expect_equal(names(deg), c("A", "B", "C"))
  expect_equal(length(deg), 3)

  bw <- centrality_betweenness(adj)
  expect_true(is.numeric(bw))
  expect_equal(names(bw), c("A", "B", "C"))

  pr <- centrality_pagerank(adj)
  expect_true(is.numeric(pr))
  expect_equal(names(pr), c("A", "B", "C"))
})

test_that("all shortcut functions work", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")

  expect_length(centrality_degree(adj), 3)
  expect_length(centrality_strength(adj), 3)
  expect_length(centrality_betweenness(adj), 3)
  expect_length(centrality_closeness(adj), 3)
  expect_length(centrality_eigenvector(adj), 3)
  expect_length(centrality_pagerank(adj), 3)
  expect_length(centrality_authority(adj), 3)
  expect_length(centrality_hub(adj), 3)
  expect_length(centrality_eccentricity(adj), 3)
  expect_length(centrality_coreness(adj), 3)
  expect_length(centrality_constraint(adj), 3)
  expect_length(centrality_transitivity(adj), 3)
})

test_that("centrality works without node names", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- centrality(adj)

  expect_equal(result$node, c("1", "2", "3"))
})

test_that("weighted parameter uses edge weights", {
  adj <- matrix(c(0, 2, 1, 2, 0, 3, 1, 3, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")

  result_weighted <- centrality(adj, measures = "strength", weighted = TRUE)
  result_unweighted <- centrality(adj, measures = "degree", weighted = FALSE)

  # Weighted strength sums edge weights, degree counts edges
  # With weights 2, 3, 1 the strength should differ from degree count
  expect_true(!identical(result_weighted$strength_all,
                         result_unweighted$degree_all))
  # Node A has edges with weights 2 and 1, so strength = 3, degree = 2
  expect_equal(result_weighted$strength_all[1], 3)  # A: 2 + 1
  expect_equal(result_unweighted$degree_all[1], 2)  # A: 2 edges
})

test_that("all measures can be calculated",
  {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")

  result <- centrality(adj, measures = "all")

  expected_cols <- c("node", "degree_all", "strength_all", "closeness_all",
                     "eccentricity_all", "coreness_all", "betweenness",
                     "eigenvector", "pagerank", "authority", "hub",
                     "constraint", "transitivity")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("centrality handles disconnected graph", {
  # Two disconnected nodes
  adj <- matrix(c(0, 0, 0, 0), 2, 2)
  rownames(adj) <- colnames(adj) <- c("A", "B")

  # Should not error
  result <- centrality(adj, measures = c("degree", "betweenness"))
  expect_equal(nrow(result), 2)
  expect_equal(result$degree_all, c(0, 0))
})

test_that("centrality works with network objects", {
  skip_if_not_installed("network")

  # Create a network object
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- network::network(adj, directed = FALSE)
  network::network.vertex.names(net) <- c("A", "B", "C")

  result <- centrality(net, measures = c("degree", "betweenness"))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$node, c("A", "B", "C"))
  expect_true("degree_all" %in% names(result))
  expect_true("betweenness" %in% names(result))
})
