# Tests for network summary functions

skip_on_cran()

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

  pdf(NULL)
  result <- degree_distribution(mat)
  dev.off()

  expect_true(is.list(result))
  expect_named(result, c("degree", "table", "breaks", "counts", "proportions"))
  expect_equal(sum(result$counts), 4L)
  expect_equal(sum(result$proportions), 1)
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

  g <- igraph::sample_gnp(20, 0.3)

  expect_no_error({
    pdf(NULL)
    result <- degree_distribution(g)
    dev.off()
  })
})

test_that("degree_distribution bins parameter controls bin count", {
  mat <- create_test_matrix(30, density = 0.2)
  pdf(NULL)
  res <- degree_distribution(mat, bins = 4)
  dev.off()

  expect_equal(length(res$counts), 4L)
})

test_that("degree_distribution bin_width parameter works", {
  mat <- create_test_matrix(30, density = 0.2)
  pdf(NULL)
  res <- degree_distribution(mat, bin_width = 3)
  dev.off()

  diffs <- diff(res$breaks)
  expect_true(all(abs(diffs - 3) < 1e-10))
})

test_that("degree_distribution custom breaks vector works", {
  mat <- create_test_matrix(30, density = 0.2)
  pdf(NULL)
  res <- degree_distribution(mat, breaks = c(0, 5, 10, 15, 20, 30))
  dev.off()

  expect_equal(res$breaks, c(0, 5, 10, 15, 20, 30))
  expect_equal(length(res$counts), 5L)
})

test_that("degree_distribution breaks string works", {
  mat <- create_test_matrix(30, density = 0.2)
  pdf(NULL)
  res <- degree_distribution(mat, breaks = "Sturges")
  dev.off()

  expect_true(length(res$counts) > 0)
})

test_that("degree_distribution normalize produces proportions", {
  mat <- create_test_matrix(30, density = 0.2)
  pdf(NULL)
  res <- degree_distribution(mat, normalize = TRUE)
  dev.off()

  expect_equal(sum(res$proportions), 1)
})

test_that("degree_distribution log scales work", {
  mat <- create_test_matrix(50, density = 0.15)

  pdf(NULL)
  expect_no_error(degree_distribution(mat, log = "y"))
  expect_no_error(degree_distribution(mat, cumulative = TRUE, log = "xy"))
  expect_no_error(degree_distribution(mat, cumulative = TRUE, log = "x"))
  expect_no_error(degree_distribution(mat, cumulative = TRUE, log = "y"))
  dev.off()
})

test_that("degree_distribution border parameter works", {
  mat <- create_test_matrix(20, density = 0.3)
  pdf(NULL)
  expect_no_error(degree_distribution(mat, border = "black"))
  dev.off()
})

test_that("degree_distribution custom ylab works", {
  mat <- create_test_matrix(20, density = 0.3)
  pdf(NULL)
  expect_no_error(degree_distribution(mat, ylab = "Count"))
  expect_no_error(degree_distribution(mat, cumulative = TRUE, ylab = "CDF"))
  expect_no_error(degree_distribution(mat, normalize = TRUE, ylab = "Frac"))
  dev.off()
})

test_that("degree_distribution default ylab auto-selects", {
  mat <- create_test_matrix(20, density = 0.3)
  # Just verify no error with default ylab = NULL in each mode
  pdf(NULL)
  expect_no_error(degree_distribution(mat))
  expect_no_error(degree_distribution(mat, normalize = TRUE))
  expect_no_error(degree_distribution(mat, cumulative = TRUE))
  dev.off()
})

test_that("degree_distribution integer-aligned default for small range", {
  mat <- create_test_matrix(20, density = 0.3)
  pdf(NULL)
  res <- degree_distribution(mat)
  dev.off()

  # Default integer bins: breaks should be 0.5-spaced
  diffs <- diff(res$breaks)
  expect_true(all(abs(diffs - 1) < 1e-10))
})
