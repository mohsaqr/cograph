# test-coverage-class-layout-40.R - Coverage tests for class-layout.R
# Targets uncovered code paths in CographLayout R6 class

# =============================================================================
# CographLayout - Basic Creation Tests
# =============================================================================

test_that("CographLayout$new() creates layout with default type", {
  layout <- CographLayout$new()
  expect_equal(layout$get_type(), "circle")
  expect_equal(layout$get_params(), list())
})

test_that("CographLayout$new() accepts type parameter", {
  layout <- CographLayout$new("spring")
  expect_equal(layout$get_type(), "spring")
})

test_that("CographLayout$new() stores additional parameters", {
  layout <- CographLayout$new("spring", iterations = 100, seed = 42)
  params <- layout$get_params()
  expect_equal(params$iterations, 100)
  expect_equal(params$seed, 42)
})

# =============================================================================
# CographLayout$compute() - Error Handling
# =============================================================================

test_that("CographLayout$compute() errors on non-network input", {
  layout <- CographLayout$new("circle")

  # Test with data frame
  expect_error(
    layout$compute(data.frame(x = 1:3, y = 1:3)),
    "network must be a CographNetwork object"
  )

  # Test with matrix
  expect_error(
    layout$compute(matrix(1:9, 3, 3)),
    "network must be a CographNetwork object"
  )

  # Test with NULL
  expect_error(
    layout$compute(NULL),
    "network must be a CographNetwork object"
  )

  # Test with list
  expect_error(
    layout$compute(list(a = 1, b = 2)),
    "network must be a CographNetwork object"
  )
})

test_that("CographLayout$compute() errors on unknown layout type", {
  layout <- CographLayout$new("nonexistent_layout_xyz")
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)

  expect_error(
    layout$compute(net),
    "Unknown layout type: nonexistent_layout_xyz"
  )
})

# =============================================================================
# CographLayout - Custom Layout Type
# =============================================================================

test_that("CographLayout custom type works with provided coords", {
  custom_coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  layout <- CographLayout$new("custom", coords = custom_coords)

  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
  # Should be normalized
  expect_true(all(coords$x >= 0 & coords$x <= 1))
  expect_true(all(coords$y >= 0 & coords$y <= 1))
})

test_that("CographLayout custom type with matrix coords", {
  custom_coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  colnames(custom_coords) <- c("x", "y")
  layout <- CographLayout$new("custom", coords = custom_coords)

  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("CographLayout custom type errors without coords", {
  layout <- CographLayout$new("custom")
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)

  expect_error(
    layout$compute(net),
    "Custom layout requires 'coords' parameter"
  )
})

test_that("CographLayout custom type with explicit NULL coords errors", {
  layout <- CographLayout$new("custom", coords = NULL)
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)

  expect_error(
    layout$compute(net),
    "Custom layout requires 'coords' parameter"
  )
})

# =============================================================================
# CographLayout$normalize_coords() - Various Input Types
# =============================================================================

test_that("normalize_coords handles matrix input", {
  layout <- CographLayout$new("circle")
  mat <- matrix(c(0, 10, 20, 0, 5, 10), ncol = 2)

  result <- layout$normalize_coords(mat)

  expect_true(is.data.frame(result))
  expect_true(all(c("x", "y") %in% names(result)))
  expect_true(all(result$x >= 0 & result$x <= 1))
  expect_true(all(result$y >= 0 & result$y <= 1))
})

test_that("normalize_coords handles data frame with x,y names", {
  layout <- CographLayout$new("circle")
  df <- data.frame(x = c(-5, 0, 5), y = c(-3, 0, 3))

  result <- layout$normalize_coords(df)

  expect_true(all(c("x", "y") %in% names(result)))
  expect_true(all(result$x >= 0 & result$x <= 1))
  expect_true(all(result$y >= 0 & result$y <= 1))
})

test_that("normalize_coords handles data frame without x,y names", {
  layout <- CographLayout$new("circle")
  # Create df with different column names
  df <- data.frame(col1 = c(-5, 0, 5), col2 = c(-3, 0, 3))

  result <- layout$normalize_coords(df)

  # Should rename first two columns to x, y
  expect_true(all(c("x", "y") %in% names(result)))
  expect_true(all(result$x >= 0 & result$x <= 1))
  expect_true(all(result$y >= 0 & result$y <= 1))
})

test_that("normalize_coords handles identical coordinates (zero spread)", {
  layout <- CographLayout$new("circle")
  # All nodes at the same position
  df <- data.frame(x = c(5, 5, 5), y = c(3, 3, 3))

  result <- layout$normalize_coords(df)

  # Should center at 0.5
  expect_true(all(result$x == 0.5))
  expect_true(all(result$y == 0.5))
})

test_that("normalize_coords handles single point", {
  layout <- CographLayout$new("circle")
  df <- data.frame(x = 100, y = 200)

  result <- layout$normalize_coords(df)

  # Single point should be at center
  expect_equal(result$x, 0.5)
  expect_equal(result$y, 0.5)
})

test_that("normalize_coords handles negative coordinates", {
  layout <- CographLayout$new("circle")
  df <- data.frame(x = c(-100, -50, 0), y = c(-50, 0, 50))

  result <- layout$normalize_coords(df)

  expect_true(all(result$x >= 0 & result$x <= 1))
  expect_true(all(result$y >= 0 & result$y <= 1))
})

test_that("normalize_coords handles very large coordinates", {
  layout <- CographLayout$new("circle")
  df <- data.frame(x = c(1e6, 2e6, 3e6), y = c(1e6, 2e6, 3e6))

  result <- layout$normalize_coords(df)

  expect_true(all(result$x >= 0 & result$x <= 1))
  expect_true(all(result$y >= 0 & result$y <= 1))
})

test_that("normalize_coords respects custom padding", {
  layout <- CographLayout$new("circle")
  df <- data.frame(x = c(0, 10), y = c(0, 10))

  # With default padding (0.1) - use tolerance for floating point
  result_default <- layout$normalize_coords(df, padding = 0.1)
  tol <- 1e-9
  expect_true(all(result_default$x >= 0.1 - tol & result_default$x <= 0.9 + tol))
  expect_true(all(result_default$y >= 0.1 - tol & result_default$y <= 0.9 + tol))

  # With zero padding
  result_zero <- layout$normalize_coords(df, padding = 0)
  expect_true(min(result_zero$x) <= 0.05 || max(result_zero$x) >= 0.95)

  # With large padding - use tolerance for floating point
  result_large <- layout$normalize_coords(df, padding = 0.3)
  expect_true(all(result_large$x >= 0.3 - tol & result_large$x <= 0.7 + tol))
  expect_true(all(result_large$y >= 0.3 - tol & result_large$y <= 0.7 + tol))
})

test_that("normalize_coords preserves aspect ratio", {
  layout <- CographLayout$new("circle")
  # Create rectangle twice as wide as tall
  df <- data.frame(x = c(0, 20), y = c(0, 10))

  result <- layout$normalize_coords(df)

  x_range <- max(result$x) - min(result$x)
  y_range <- max(result$y) - min(result$y)

  # The wider dimension (x) should span more of the plot
  # Aspect ratio should be preserved (2:1)
  expect_equal(x_range / y_range, 2, tolerance = 0.01)
})

# =============================================================================
# CographLayout$print() - Print Method Coverage
# =============================================================================

test_that("CographLayout print method works for simple layout", {
  layout <- CographLayout$new("circle")

  output <- capture.output(layout$print())

  expect_true(any(grepl("CographLayout", output)))
  expect_true(any(grepl("circle", output)))
})

test_that("CographLayout print method shows parameters", {
  layout <- CographLayout$new("spring", iterations = 100, seed = 42)

  output <- capture.output(layout$print())

  expect_true(any(grepl("CographLayout", output)))
  expect_true(any(grepl("spring", output)))
  expect_true(any(grepl("Parameters", output)))
  expect_true(any(grepl("iterations", output)))
  expect_true(any(grepl("100", output)))
  expect_true(any(grepl("seed", output)))
  expect_true(any(grepl("42", output)))
})

test_that("CographLayout print method truncates long parameter values", {
  # Create layout with a long vector parameter
  long_vec <- 1:10
  layout <- CographLayout$new("spring", my_param = long_vec)

  output <- capture.output(layout$print())

  # Should show first 3 values and "..."
  expect_true(any(grepl("\\.\\.\\.", output)))
})

test_that("CographLayout print returns invisible self", {
  layout <- CographLayout$new("circle")

  result <- capture.output(ret <- layout$print())

  # Returns invisibly
  expect_identical(ret, layout)
})

# =============================================================================
# CographLayout$compute() - Integration with Registered Layouts
# =============================================================================

test_that("CographLayout$compute() works with circle layout", {
  layout <- CographLayout$new("circle")
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 5)
  expect_true(all(c("x", "y") %in% names(coords)))
  expect_true(all(coords$x >= 0 & coords$x <= 1))
  expect_true(all(coords$y >= 0 & coords$y <= 1))
})

test_that("CographLayout$compute() works with spring layout", {
  layout <- CographLayout$new("spring", iterations = 10, seed = 42)
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 4)
  expect_true(all(c("x", "y") %in% names(coords)))
  expect_true(all(coords$x >= 0 & coords$x <= 1))
  expect_true(all(coords$y >= 0 & coords$y <= 1))
})

test_that("CographLayout$compute() passes parameters to layout function", {
  # Use spring with specific seed to verify parameters are passed
  layout <- CographLayout$new("spring", iterations = 50, seed = 123)
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords1 <- layout$compute(net)

  # Create another layout with same params
  layout2 <- CographLayout$new("spring", iterations = 50, seed = 123)
  coords2 <- layout2$compute(net)

  # Should produce identical results with same seed
  expect_equal(coords1$x, coords2$x)
  expect_equal(coords1$y, coords2$y)
})

test_that("CographLayout$compute() merges init params with compute params", {
  layout <- CographLayout$new("spring", iterations = 10)
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Pass additional params in compute call
  coords <- layout$compute(net, seed = 42)

  expect_equal(nrow(coords), 4)
  expect_true(all(c("x", "y") %in% names(coords)))
})

# =============================================================================
# CographLayout$get_type() and $get_params()
# =============================================================================

test_that("get_type returns correct layout type", {
  layout1 <- CographLayout$new("circle")
  layout2 <- CographLayout$new("spring")
  layout3 <- CographLayout$new("groups")

  expect_equal(layout1$get_type(), "circle")
  expect_equal(layout2$get_type(), "spring")
  expect_equal(layout3$get_type(), "groups")
})

test_that("get_params returns empty list when no params", {
  layout <- CographLayout$new("circle")

  expect_equal(layout$get_params(), list())
})

test_that("get_params returns all initialization parameters", {
  layout <- CographLayout$new("spring",
                               iterations = 100,
                               seed = 42,
                               k = 0.5,
                               custom = "value")
  params <- layout$get_params()

  expect_equal(params$iterations, 100)
  expect_equal(params$seed, 42)
  expect_equal(params$k, 0.5)
  expect_equal(params$custom, "value")
})

# =============================================================================
# CographLayout - Edge Cases
# =============================================================================

test_that("CographLayout handles single node network", {
  layout <- CographLayout$new("circle")
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 1)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("CographLayout handles two node network", {
  layout <- CographLayout$new("circle")
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 2)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("CographLayout handles large network", {
  layout <- CographLayout$new("circle")
  adj <- create_test_matrix(50, density = 0.1)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 50)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("CographLayout handles empty adjacency (no edges)", {
  layout <- CographLayout$new("circle")
  adj <- matrix(0, 5, 5)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 5)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("CographLayout handles disconnected network", {
  layout <- CographLayout$new("spring", iterations = 10, seed = 42)
  adj <- create_test_topology("disconnected", n = 6)
  net <- CographNetwork$new(adj)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 6)
  expect_true(all(c("x", "y") %in% names(coords)))
})

# =============================================================================
# CographLayout - Input Validation with R6 CographNetwork
# =============================================================================

test_that("CographLayout accepts R6 CographNetwork objects", {
  layout <- CographLayout$new("circle")
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Should work with R6 class
  expect_true(inherits(net, "CographNetwork"))

  coords <- layout$compute(net)
  expect_equal(nrow(coords), 4)
})

# =============================================================================
# CographLayout - normalize_coords with NA values
# =============================================================================

test_that("normalize_coords handles NA in coordinates", {
  layout <- CographLayout$new("circle")
  df <- data.frame(x = c(0, NA, 10), y = c(0, 5, NA))

  # Should handle NAs using na.rm = TRUE
  result <- layout$normalize_coords(df)

  expect_true(all(c("x", "y") %in% names(result)))
  # Non-NA values should be normalized
  expect_true(all(is.na(result$x) | (result$x >= 0 & result$x <= 1)))
  expect_true(all(is.na(result$y) | (result$y >= 0 & result$y <= 1)))
})

# =============================================================================
# CographLayout - Asymmetric coordinate ranges
# =============================================================================

test_that("normalize_coords handles asymmetric x and y ranges", {
  layout <- CographLayout$new("circle")

  # x range is much larger than y range
  df <- data.frame(x = c(0, 100), y = c(0, 10))
  result <- layout$normalize_coords(df)

  # Due to uniform scaling, the smaller range (y) will not span full [0.1, 0.9]
  x_spread <- max(result$x) - min(result$x)
  y_spread <- max(result$y) - min(result$y)

  # x should use more of the space since it's the larger dimension
  expect_true(x_spread > y_spread)
})

test_that("normalize_coords handles tall rectangle (y > x)", {
  layout <- CographLayout$new("circle")

  # y range is larger than x range
  df <- data.frame(x = c(0, 10), y = c(0, 100))
  result <- layout$normalize_coords(df)

  x_spread <- max(result$x) - min(result$x)
  y_spread <- max(result$y) - min(result$y)

  # y should use more of the space
  expect_true(y_spread > x_spread)
})
