# Tests for estimate_cs() / estimate_centrality_stability()

test_that("estimate_cs requires appropriate input", {
  mat <- matrix(runif(25), 5, 5)
  expect_error(estimate_cs(mat), "requires")
})

test_that("estimate_centrality_stability is alias for estimate_cs", {
  expect_identical(estimate_centrality_stability, estimate_cs)
})

test_that("estimate_cs.cograph_network works with raw_data", {
  skip_if_not_installed("tna")
  skip("Non-TNA stability not yet implemented")
})

test_that("estimate_cs returns correct structure per measure", {
  skip_if_not_installed("tna")
  skip("Non-TNA stability not yet implemented")
})

test_that("estimate_cs with custom measures", {
  skip_if_not_installed("tna")
  skip("Non-TNA stability not yet implemented")
})

test_that("estimate_cs with custom threshold and certainty", {
  skip_if_not_installed("tna")
  skip("Non-TNA stability not yet implemented")
})

test_that("estimate_cs correlation method options", {
  skip_if_not_installed("tna")
  skip("Non-TNA stability not yet implemented")
})

test_that("estimate_cs print method works", {
  skip_if_not_installed("tna")
  skip("Non-TNA stability not yet implemented")
})

test_that("estimate_cs reproducibility with seed", {
  skip_if_not_installed("tna")
  skip("Non-TNA stability not yet implemented")
})

# TNA-specific tests
test_that("estimate_cs.tna works with TNA objects", {
  skip_if_not_installed("tna")
  skip("Requires tna sample data - skipping for unit tests")
})

test_that("estimate_cs.tna fails without data", {
  # Create mock TNA model without data
  weights <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), nrow = 3, byrow = TRUE)
  rownames(weights) <- colnames(weights) <- c("A", "B", "C")

  model <- list(
    weights = weights,
    labels = c("A", "B", "C"),
    inits = c(0.33, 0.33, 0.34),
    data = NULL
  )
  class(model) <- "tna"

  expect_error(estimate_cs(model), "needs \\$data|requires")
})

test_that("estimate_cs.group_tna works", {
  skip_if_not_installed("tna")
  skip("Requires tna sample data - skipping for unit tests")
})

test_that("estimate_cs.cograph_network works with TNA network", {
  skip_if_not_installed("tna")
  skip("Requires tna sample data - skipping for unit tests")
})

test_that("CS-coefficient interpretation is correct", {
  skip_if_not_installed("tna")
  skip("Non-TNA stability not yet implemented")
})
