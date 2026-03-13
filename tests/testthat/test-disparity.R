# Tests for disparity_filter()

test_that("disparity_filter.matrix works", {
  # Create a simple weighted network
  mat <- matrix(c(
    0.0, 0.5, 0.1, 0.0,
    0.3, 0.0, 0.4, 0.1,
    0.1, 0.2, 0.0, 0.5,
    0.0, 0.1, 0.3, 0.0
  ), nrow = 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- disparity_filter(mat, level = 0.05)

  # Check output is binary matrix
  expect_true(is.matrix(result))
  expect_true(all(result %in% c(0, 1)))
  expect_equal(dim(result), c(4, 4))

  # Diagonal should be 0
  expect_true(all(diag(result) == 0))
})

test_that("disparity_filter preserves dimnames", {
  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  diag(mat) <- 0

  result <- disparity_filter(mat, level = 0.5)

  expect_equal(rownames(result), rownames(mat))
  expect_equal(colnames(result), colnames(mat))
})

test_that("disparity_filter with different significance levels", {
  mat <- matrix(c(
    0.0, 0.8, 0.1, 0.1,
    0.1, 0.0, 0.7, 0.2,
    0.2, 0.1, 0.0, 0.7,
    0.1, 0.2, 0.1, 0.0
  ), nrow = 4, byrow = TRUE)

  # More stringent level should keep fewer edges
  result_05 <- disparity_filter(mat, level = 0.5)
  result_01 <- disparity_filter(mat, level = 0.01)

  n_edges_05 <- sum(result_05)
  n_edges_01 <- sum(result_01)

  expect_true(n_edges_01 <= n_edges_05)
})

test_that("disparity_filter.tna works", {
  # Create mock TNA model
  weights <- matrix(c(
    0.0, 0.5, 0.1,
    0.3, 0.0, 0.4,
    0.1, 0.2, 0.0
  ), nrow = 3, byrow = TRUE)
  rownames(weights) <- colnames(weights) <- c("A", "B", "C")

  model <- list(
    weights = weights,
    labels = c("A", "B", "C"),
    inits = c(0.33, 0.33, 0.34)
  )
  class(model) <- "tna"

  result <- disparity_filter(model, level = 0.5)

  # Check structure
  expect_s3_class(result, "tna_disparity")
  expect_true("significant" %in% names(result))
  expect_true("weights_orig" %in% names(result))
  expect_true("weights_filtered" %in% names(result))
  expect_equal(result$level, 0.5)
})

test_that("disparity_filter.cograph_network works", {
  mat <- matrix(c(
    0.0, 0.5, 0.1,
    0.3, 0.0, 0.4,
    0.1, 0.2, 0.0
  ), nrow = 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- as_cograph(mat)
  result <- disparity_filter(net, level = 0.5)

  expect_s3_class(result, "tna_disparity")
  expect_equal(dim(result$significant), c(3, 3))
})

test_that("tna_disparity print method works", {
  mat <- matrix(c(
    0.0, 0.5, 0.1, 0.0,
    0.3, 0.0, 0.4, 0.1,
    0.1, 0.2, 0.0, 0.5,
    0.0, 0.1, 0.3, 0.0
  ), nrow = 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- as_cograph(mat)
  result <- disparity_filter(net, level = 0.5)

  expect_output(print(result), "Disparity Filter Result")
  expect_output(print(result), "Significance level")
})

test_that("disparity_filter handles zero matrices", {
  mat <- matrix(0, 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- disparity_filter(mat, level = 0.05)

  expect_true(all(result == 0))
})

test_that("disparity_filter handles single strong edge", {
  # Network where one edge dominates
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 0.99  # Very strong edge
  mat[1, 3] <- 0.005
  mat[1, 4] <- 0.005
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  result <- disparity_filter(mat, level = 0.05)

  # The dominant edge should be significant
  expect_equal(result[1, 2], 1)
})

test_that("disparity_filter tna_disparity contains filtered weights", {
  mat <- matrix(c(
    0.0, 0.5, 0.1,
    0.3, 0.0, 0.4,
    0.1, 0.2, 0.0
  ), nrow = 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- as_cograph(mat)
  result <- disparity_filter(net, level = 0.5)

  # Filtered weights should be original * significant
  expected_filtered <- mat * result$significant
  expect_equal(result$weights_filtered, expected_filtered)
})

test_that("disparity_filter edge counts are correct", {
  mat <- matrix(c(
    0.0, 0.5, 0.1,
    0.3, 0.0, 0.4,
    0.1, 0.2, 0.0
  ), nrow = 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- as_cograph(mat)
  result <- disparity_filter(net, level = 0.5)

  expect_equal(result$n_edges_orig, sum(mat != 0))
  expect_equal(result$n_edges_filtered, sum(result$significant != 0))
})

test_that("disparity_filter.igraph works on simple graph", {
  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- c(10, 1, 1, 1, 1)
  igraph::V(g)$name <- LETTERS[1:5]

  result <- disparity_filter(g, level = 0.5)

  expect_s3_class(result, "tna_disparity")
  expect_equal(nrow(result$weights_orig), 5)
  # The strong edge (weight=10) should be significant
  expect_true(result$n_edges_filtered > 0)
  expect_true(result$n_edges_filtered <= result$n_edges_orig)
})

test_that("disparity_filter.igraph handles multigraph", {
  # Build multigraph with parallel edges
  el <- data.frame(
    from = c("A", "A", "A", "A", "A", "B", "C"),
    to   = c("B", "B", "B", "B", "C", "C", "D"),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(el, directed = FALSE)

  result <- disparity_filter(g, level = 0.5)

  expect_s3_class(result, "tna_disparity")
  # A-B has 4 edges, should dominate A's weight
  expect_true(result$significant["A", "B"] == 1 || result$significant["B", "A"] == 1)
})

test_that("disparity_filter.igraph with no weight attr assigns weight=1", {
  g <- igraph::make_ring(4)
  igraph::V(g)$name <- LETTERS[1:4]
  # No weight attribute

  result <- disparity_filter(g, level = 0.5)

  expect_s3_class(result, "tna_disparity")
  expect_equal(nrow(result$weights_orig), 4)
})
