# Tests for bootstrap()

test_that("bootstrap requires appropriate input", {
  # Matrix without raw_data should fail
  mat <- matrix(runif(25), 5, 5)
  expect_error(bootstrap(mat), "requires a tna or cograph_network")

  # Default method should fail for unsupported types
  expect_error(bootstrap(list(a = 1)), "requires")
})

test_that("bootstrap.cograph_network requires raw_data", {
  mat <- matrix(runif(25), 5, 5)
  colnames(mat) <- rownames(mat) <- letters[1:5]
  net <- as_cograph(mat)

  # Without raw_data
 expect_error(bootstrap(net), "raw_data")
})

test_that("bootstrap.cograph_network works with set_raw_data", {
  skip_if_not_installed("tna")

  # Create data and network
  set.seed(42)
  data <- matrix(rnorm(500), nrow = 100, ncol = 5)
  colnames(data) <- paste0("V", 1:5)

  cor_mat <- cor(data)
  net <- as_cograph(cor_mat)
  net <- set_raw_data(net, data, estimator = function(d) cor(d))

  # Run bootstrap with few iterations
  result <- bootstrap(net, iter = 10, seed = 123)

  # Check structure
  expect_s3_class(result, "cograph_boot")
  expect_equal(dim(result$weights_orig), c(5, 5))
  expect_equal(dim(result$weights_sig), c(5, 5))
  expect_equal(dim(result$weights_mean), c(5, 5))
  expect_equal(dim(result$weights_sd), c(5, 5))
  expect_equal(dim(result$ci_lower), c(5, 5))
  expect_equal(dim(result$ci_upper), c(5, 5))
  expect_equal(dim(result$p_values), c(5, 5))

  # Check values
  expect_true(all(result$p_values >= 0 & result$p_values <= 1))
  expect_equal(result$iter, 10)
  expect_equal(result$level, 0.05)
})

test_that("bootstrap methods work correctly", {
  skip_if_not_installed("tna")

  set.seed(42)
  data <- matrix(rnorm(500), nrow = 100, ncol = 5)
  colnames(data) <- paste0("V", 1:5)

  cor_mat <- cor(data)
  net <- as_cograph(cor_mat)
  net <- set_raw_data(net, data, estimator = function(d) cor(d))

  # Stability method
  result_stab <- bootstrap(net, iter = 10, method = "stability", seed = 123)
  expect_equal(result_stab$method, "stability")
  expect_equal(result_stab$consistency_range, c(0.75, 1.25))

  # Threshold method
  result_thresh <- bootstrap(net, iter = 10, method = "threshold", threshold = 0.1, seed = 123)
  expect_equal(result_thresh$method, "threshold")
  expect_equal(result_thresh$threshold, 0.1)
})

test_that("bootstrap summary data frame is correct", {
  skip_if_not_installed("tna")

  set.seed(42)
  data <- matrix(rnorm(500), nrow = 100, ncol = 5)
  colnames(data) <- paste0("V", 1:5)

  cor_mat <- cor(data)
  net <- as_cograph(cor_mat)
  net <- set_raw_data(net, data, estimator = function(d) cor(d))

  result <- bootstrap(net, iter = 10, seed = 123)

  # Check summary structure
  expect_true(is.data.frame(result$summary))
  expect_true(all(c("from", "to", "weight", "mean", "sd", "ci_lower",
                    "ci_upper", "p_value", "significant") %in% names(result$summary)))

  # Check significant column is logical
  expect_type(result$summary$significant, "logical")
})

test_that("bootstrap print method works", {
  skip_if_not_installed("tna")

  set.seed(42)
  data <- matrix(rnorm(500), nrow = 100, ncol = 5)
  colnames(data) <- paste0("V", 1:5)

  cor_mat <- cor(data)
  net <- as_cograph(cor_mat)
  net <- set_raw_data(net, data, estimator = function(d) cor(d))

  result <- bootstrap(net, iter = 10, seed = 123)

  # Should not error
  expect_output(print(result), "Bootstrap Analysis")
})

test_that("bootstrap reproducibility with seed", {
  skip_if_not_installed("tna")

  set.seed(42)
  data <- matrix(rnorm(500), nrow = 100, ncol = 5)
  colnames(data) <- paste0("V", 1:5)

  cor_mat <- cor(data)
  net <- as_cograph(cor_mat)
  net <- set_raw_data(net, data, estimator = function(d) cor(d))

  # Same seed should give same results
  result1 <- bootstrap(net, iter = 10, seed = 999)
  result2 <- bootstrap(net, iter = 10, seed = 999)

  expect_equal(result1$p_values, result2$p_values)
  expect_equal(result1$weights_sig, result2$weights_sig)
})

# TNA-specific tests (if tna package available)
test_that("bootstrap.tna works with TNA objects", {
  skip_if_not_installed("tna")
  skip("Requires tna sample data - skipping for unit tests")

  # This test requires actual tna sample data like group_regulation
  # which provides proper tna_seq_data objects
})

test_that("bootstrap.tna fails without data", {
  # Create TNA model without data (mock object)
  weights <- matrix(c(0, 0.5, 0.5, 0.3, 0, 0.7, 0.4, 0.6, 0), nrow = 3, byrow = TRUE)
  rownames(weights) <- colnames(weights) <- c("A", "B", "C")

  model <- list(
    weights = weights,
    labels = c("A", "B", "C"),
    inits = c(0.33, 0.33, 0.34),
    data = NULL
  )
  class(model) <- "tna"

  expect_error(bootstrap(model), "no data|requires \\$data")
})

test_that("bootstrap.cograph_network works with TNA network", {
  skip_if_not_installed("tna")
  skip("Requires tna sample data - skipping for unit tests")

  # This test requires actual tna sample data
})

test_that("splot.cograph_boot supports all display modes", {
  skip_if_not_installed("tna")

  set.seed(42)
  data <- matrix(rnorm(500), nrow = 100, ncol = 5)
  colnames(data) <- paste0("V", 1:5)

  cor_mat <- cor(data)
  net <- as_cograph(cor_mat)
  net <- set_raw_data(net, data, estimator = function(d) cor(d))

  result <- bootstrap(net, iter = 10, seed = 123)

  # Test all show modes
  expect_silent(splot(result, show = "styled"))
  expect_silent(splot(result, show = "significant"))
  expect_silent(splot(result, show = "full"))
  expect_silent(splot(result, show = "ci"))

  # Test with stars on/off (default is FALSE, TNA-style)
  expect_silent(splot(result, show_stars = TRUE))
  expect_silent(splot(result, show_stars = FALSE))

  # Test with CI bands
  expect_silent(splot(result, show_ci = TRUE))

  # Test custom styling
  expect_silent(splot(result, nonsig_color = "pink", alpha_nonsig = 0.5))
})

test_that("bootstrap stores original network for styling", {
  skip_if_not_installed("tna")

  set.seed(42)
  data <- matrix(rnorm(500), nrow = 100, ncol = 5)
  colnames(data) <- paste0("V", 1:5)

  cor_mat <- cor(data)
  net <- as_cograph(cor_mat)
  net <- set_raw_data(net, data, estimator = function(d) cor(d))

  result <- bootstrap(net, iter = 10, seed = 123)

  # Should have network stored

  expect_true(!is.null(result$network))
  expect_s3_class(result$network, "cograph_network")
})
