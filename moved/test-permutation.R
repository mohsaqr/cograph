# Tests for permutation_test()

test_that("permutation_test requires two networks", {
  mat <- matrix(runif(25), 5, 5)
  expect_error(permutation_test(mat), "requires")
})

test_that("permutation_test.cograph_network works with raw_data", {
  skip_if_not_installed("tna")
  skip("Non-TNA permutation test not yet implemented")

  # Create two datasets
  set.seed(42)
  data1 <- matrix(rnorm(500), nrow = 100, ncol = 5)
  data2 <- matrix(rnorm(500, mean = 0.5), nrow = 100, ncol = 5)
  colnames(data1) <- colnames(data2) <- paste0("V", 1:5)

  # Create networks with raw_data
  net1 <- as_cograph(cor(data1))
  net1 <- set_raw_data(net1, data1, estimator = function(d) cor(d))

  net2 <- as_cograph(cor(data2))
  net2 <- set_raw_data(net2, data2, estimator = function(d) cor(d))

  # Run permutation test
  result <- permutation_test(net1, net2, iter = 10, seed = 123)

  # Check structure
  expect_s3_class(result, "tna_permutation")
  expect_true("edges" %in% names(result))
  expect_true("stats" %in% names(result$edges))
  expect_true("diffs_true" %in% names(result$edges))
  expect_true("diffs_sig" %in% names(result$edges))
})

test_that("permutation_test edge stats are correct", {
  skip_if_not_installed("tna")
  skip("Non-TNA permutation test not yet implemented")
})

test_that("permutation_test p-value adjustment works", {
  skip_if_not_installed("tna")
  skip("Non-TNA permutation test not yet implemented")
})

test_that("permutation_test paired option works", {
  skip_if_not_installed("tna")
  skip("Non-TNA permutation test not yet implemented")
})

test_that("permutation_test paired fails with unequal n", {
  skip_if_not_installed("tna")
  skip("Non-TNA permutation test not yet implemented")
})

test_that("permutation_test print method works", {
  skip_if_not_installed("tna")
  skip("Non-TNA permutation test not yet implemented")
})

test_that("permutation_test reproducibility with seed", {
  skip_if_not_installed("tna")
  skip("Non-TNA permutation test not yet implemented")
})

# TNA-specific tests
test_that("permutation_test.tna works with TNA objects", {
  skip_if_not_installed("tna")
  skip("Requires tna sample data - skipping for unit tests")

  # This test requires actual tna sample data
})

test_that("permutation_test.group_tna works", {
  skip_if_not_installed("tna")
  skip("Requires tna sample data - skipping for unit tests")

  # This test requires actual tna sample data
})

test_that("permutation_test centrality measures work", {
  skip_if_not_installed("tna")
  skip("Requires tna sample data - skipping for unit tests")

  # This test requires actual tna sample data
})
