# Tests for centrality functions

test_that("centrality works with adjacency matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("node" %in% names(result))
})

test_that("centrality works with specific measures", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat, measures = c("degree", "betweenness"))
  expect_true(is.data.frame(result))
  expect_true("degree_all" %in% names(result))
  expect_true("betweenness" %in% names(result))
})

test_that("centrality_degree works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality_degree(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_strength works", {
  mat <- matrix(c(0, 0.5, 0.8, 0.5, 0, 0.3, 0.8, 0.3, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality_strength(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_betweenness works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_betweenness(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_closeness works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_closeness(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_eigenvector works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_eigenvector(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
})

test_that("centrality_pagerank works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality_pagerank(mat)
  expect_length(result, 3)
  expect_true(is.numeric(result))
  expect_true(all(result >= 0))
})

test_that("centrality with normalization works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality(mat, normalized = TRUE)
  expect_true(is.data.frame(result))
  # All normalized values should be <= 1
  numeric_cols <- sapply(result, is.numeric)
  for (col in names(result)[numeric_cols]) {
    expect_true(all(result[[col]] <= 1, na.rm = TRUE))
  }
})

test_that("centrality with directed network works", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  result <- centrality(mat, directed = TRUE, mode = "in")
  expect_true(is.data.frame(result))
  expect_true("degree_in" %in% names(result))
})

test_that("centrality with cograph_network works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  result <- centrality(net)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("centrality with igraph object works", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- centrality(g)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
})

test_that("centrality with sorting works", {
  mat <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  0, 1, 1, 0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- centrality(mat, sort_by = "degree_all")
  expect_true(is.data.frame(result))
  # Check descending order
  expect_true(result$degree_all[1] >= result$degree_all[nrow(result)])
})

test_that("centrality with digits rounding works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- centrality(mat, digits = 2)
  expect_true(is.data.frame(result))
})

test_that("centrality errors on invalid measures", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  expect_error(centrality(mat, measures = "invalid_measure"))
})
