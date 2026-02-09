# Tests for TNA integration in cograph
# Tests is_tna_network() and get_tna_model() functions

test_that("is_tna_network returns FALSE for non-TNA networks", {
  # Matrix input
  mat <- matrix(runif(25), 5, 5)
  net <- as_cograph(mat)
  expect_false(is_tna_network(net))

  # Edge list input
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.5, 0.3, 0.2))
  net2 <- as_cograph(edges)
  expect_false(is_tna_network(net2))
})

test_that("is_tna_network returns TRUE for TNA networks", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  expect_true(is_tna_network(net))
})

test_that("get_tna_model errors for non-TNA networks", {
  mat <- matrix(runif(25), 5, 5)
  net <- as_cograph(mat)

  expect_error(get_tna_model(net), "Not a TNA network")
})

test_that("get_tna_model returns original tna object", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Get model back

  retrieved <- get_tna_model(net)

  # Check class
  expect_s3_class(retrieved, "tna")

  # Check it's the same object (or at least equivalent)
  expect_equal(retrieved$weights, model$weights)
  expect_equal(retrieved$inits, model$inits)
  expect_equal(retrieved$labels, model$labels)
})

test_that("TNA model fields are accessible via get_tna_model", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  retrieved <- get_tna_model(net)

  # Check core fields exist
  expect_true(!is.null(retrieved$weights))
  expect_true(!is.null(retrieved$inits))
  expect_true(!is.null(retrieved$labels))

  # Check weights is a matrix
  expect_true(is.matrix(retrieved$weights))

  # Check inits is numeric
  expect_true(is.numeric(retrieved$inits))

  # Check labels is character
  expect_true(is.character(retrieved$labels))
})

test_that("TNA attributes are preserved", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  retrieved <- get_tna_model(net)

  # Check type attribute
  expect_equal(attr(retrieved, "type"), attr(model, "type"))
})

test_that("cograph_network $tna field has correct structure", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Check $tna field exists
  expect_true(!is.null(net$tna))
  expect_true(is.list(net$tna))

  # Check required fields
  expect_true("model" %in% names(net$tna))
  expect_true("type" %in% names(net$tna))
  expect_equal(net$tna$type, "tna")

  # For single tna, group fields should be NULL
  expect_null(net$tna$group_index)
  expect_null(net$tna$group_name)
})

test_that("source field is 'tna' for TNA networks", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  expect_equal(net$source, "tna")
})

test_that("TNA network can still be plotted", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Should not error
  expect_no_error({
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 400, height = 400)
    splot(net)
    dev.off()
    unlink(tmp)
  })
})

test_that("TNA functions can be used on retrieved model", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Get model and use TNA function on it
  retrieved <- get_tna_model(net)

  # summary should work on retrieved model
  summ <- summary(retrieved)
  expect_true(!is.null(summ))
})
