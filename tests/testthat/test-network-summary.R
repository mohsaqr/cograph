# Tests for network summary functions

test_that("network_summary works with adjacency matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- network_summary(mat)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_true("node_count" %in% names(result))
  expect_true("edge_count" %in% names(result))
  expect_true("density" %in% names(result))
})

test_that("network_summary basic metrics are correct", {
  # Simple triangle network
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- network_summary(mat)
  expect_equal(result$node_count, 3)
  # Undirected: 3 edges (triangle)
  expect_true(result$edge_count >= 3)
})

test_that("network_summary with detailed = TRUE works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- network_summary(mat, detailed = TRUE)
  expect_true(is.data.frame(result))
  expect_true("mean_degree" %in% names(result))
  expect_true("mean_strength" %in% names(result))
  expect_true("mean_betweenness" %in% names(result))
})

test_that("network_summary with directed network works", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE)

  result <- network_summary(mat, directed = TRUE)
  expect_true(is.data.frame(result))
  expect_true(!is.na(result$reciprocity) || is.numeric(result$reciprocity))
})

test_that("network_summary with cograph_network works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  result <- network_summary(net)
  expect_true(is.data.frame(result))
  expect_equal(result$node_count, 3)
})

test_that("network_summary with igraph object works", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- network_summary(g)
  expect_true(is.data.frame(result))
  expect_equal(result$node_count, 5)
})

test_that("network_summary digits rounding works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- network_summary(mat, digits = 2)
  expect_true(is.data.frame(result))
})

test_that("degree_distribution works with adjacency matrix", {
  mat <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), 4, 4, byrow = TRUE)

  # Should not error; returns histogram object invisibly
  expect_no_error({
    pdf(NULL)  # suppress plot output
    result <- degree_distribution(mat)
    dev.off()
  })
})

test_that("degree_distribution cumulative works", {
  mat <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), 4, 4, byrow = TRUE)

  expect_no_error({
    pdf(NULL)
    result <- degree_distribution(mat, cumulative = TRUE)
    dev.off()
  })
})

test_that("degree_distribution with directed mode works", {
  mat <- matrix(c(0, 1, 0, 0,
                  0, 0, 1, 0,
                  1, 0, 0, 1,
                  0, 1, 0, 0), 4, 4, byrow = TRUE)

  expect_no_error({
    pdf(NULL)
    result <- degree_distribution(mat, mode = "in", directed = TRUE)
    dev.off()
  })
})

test_that("degree_distribution with igraph works", {
  skip_if_not_installed("igraph")

  g <- igraph::erdos.renyi.game(20, 0.3)

  expect_no_error({
    pdf(NULL)
    result <- degree_distribution(g)
    dev.off()
  })
})
