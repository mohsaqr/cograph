# Tests for TNA integration in cograph
# Tests is_tna_network() function

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

test_that("cograph_network $meta$tna field has correct structure", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Check $meta$tna field exists
  expect_true(!is.null(net$meta$tna))
  expect_true(is.list(net$meta$tna))

  # Check required fields (minimal structure - no model stored)
  expect_true("type" %in% names(net$meta$tna))
  expect_equal(net$meta$tna$type, "tna")

  # For single tna, group fields should be NULL
  expect_null(net$meta$tna$group_index)
  expect_null(net$meta$tna$group_name)
})

test_that("source field is 'tna' for TNA networks", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  expect_equal(net$meta$source, "tna")
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

test_that("TNA weights matrix is preserved in cograph_network", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Check weights matrix is stored
  expect_true(!is.null(net$weights))
  expect_true(is.matrix(net$weights))
  expect_equal(dim(net$weights), dim(model$weights))
  expect_equal(net$weights, model$weights)
})

test_that("TNA inits are preserved in nodes", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Check inits are stored in nodes
  nodes <- get_nodes(net)
  expect_true("inits" %in% names(nodes))
  expect_equal(as.numeric(nodes$inits), as.numeric(model$inits))
})

test_that("TNA colors are extracted if available", {
  skip_if_not_installed("tna")

  library(tna)
  model <- tna(group_regulation)

  # Check if colors exist in model
  has_colors <- !is.null(model$data) && !is.null(attr(model$data, "colors"))

  net <- as_cograph(model)
  nodes <- get_nodes(net)

  if (has_colors) {
    expect_true("color" %in% names(nodes))
  }
})
