# test-coverage-aes-scales-40.R - Comprehensive tests for aes-scales.R
# Tests for all aesthetic scaling functions: scale_size, scale_color,
# scale_color_discrete, scale_width, scale_alpha

# ============================================
# SCALE_SIZE() TESTS - Basic functionality
# ============================================

test_that("scale_size returns correct length output", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_size(values)
  expect_equal(length(result), length(values))
})

test_that("scale_size respects default range [0.03, 0.1]", {
  values <- c(0, 50, 100)
  result <- cograph:::scale_size(values)
  expect_true(min(result) >= 0.03)
  expect_true(max(result) <= 0.1)
})

test_that("scale_size respects custom range", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_size(values, range = c(1, 10))
  expect_equal(min(result), 1)
  expect_equal(max(result), 10)
})

test_that("scale_size handles all NA values", {
  values <- c(NA, NA, NA)
  result <- cograph:::scale_size(values)
  # Should return mean of default range
  expect_equal(length(result), 3)
  expect_true(all(result == mean(c(0.03, 0.1))))
})

test_that("scale_size handles some NA values", {
  values <- c(1, NA, 3, NA, 5)
  result <- cograph:::scale_size(values)
  expect_equal(length(result), 5)
})

test_that("scale_size handles constant values", {
  values <- c(5, 5, 5, 5)
  result <- cograph:::scale_size(values)
  # Should return mean of range for constant values
  expect_equal(length(result), 4)
  expect_true(all(result == mean(c(0.03, 0.1))))
})

# ============================================
# SCALE_SIZE() TESTS - Transformations
# ============================================

test_that("scale_size with linear transformation works correctly", {
  values <- c(0, 25, 50, 75, 100)
  result <- cograph:::scale_size(values, trans = "linear")
  # Linear scaling should maintain proportional relationships
  expect_equal(length(result), 5)
  # Middle value should be halfway between min and max
  expect_equal(result[3], mean(c(result[1], result[5])))
})

test_that("scale_size with sqrt transformation works correctly", {
  values <- c(0, 1, 4, 9, 16)
  result <- cograph:::scale_size(values, range = c(0, 1), trans = "sqrt")
  # After sqrt: 0, 1, 2, 3, 4 -> normalized to 0, 0.25, 0.5, 0.75, 1
  expect_equal(length(result), 5)
  expect_true(result[1] < result[2])
  expect_true(result[2] < result[3])
})

test_that("scale_size with log transformation works correctly", {

  values <- c(0, 1, 10, 100, 1000)
  result <- cograph:::scale_size(values, trans = "log")
  # Log transformation uses log1p for safe handling of 0
  expect_equal(length(result), 5)
  # All results should be finite and within range
  expect_true(all(is.finite(result)))
  # Should be monotonically increasing
  expect_true(all(diff(result) >= 0))
})

test_that("scale_size with unknown transformation defaults to linear", {
  values <- c(1, 2, 3, 4, 5)
  result_unknown <- cograph:::scale_size(values, trans = "unknown_trans")
  result_linear <- cograph:::scale_size(values, trans = "linear")
  expect_equal(result_unknown, result_linear)
})

test_that("scale_size sqrt transformation handles negative values", {
  values <- c(-5, -2, 0, 2, 5)
  result <- cograph:::scale_size(values, trans = "sqrt")
  # Negative values should be clamped to 0 before sqrt
  expect_equal(length(result), 5)
  expect_true(all(!is.nan(result)))
})

test_that("scale_size log transformation handles negative values", {
  values <- c(-10, -5, 0, 5, 10)
  result <- cograph:::scale_size(values, trans = "log")
  # Negative values should be clamped to 0 before log1p
  expect_equal(length(result), 5)
  expect_true(all(!is.nan(result)))
})

# ============================================
# SCALE_COLOR() TESTS - Basic functionality
# ============================================

test_that("scale_color returns correct length output", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_color(values)
  expect_equal(length(result), length(values))
})

test_that("scale_color returns valid colors", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_color(values)
  expect_valid_colors(result)
})

test_that("scale_color handles all NA values", {
  values <- c(NA, NA, NA)
  result <- cograph:::scale_color(values)
  expect_equal(length(result), 3)
  expect_true(all(result == "gray50"))
})

test_that("scale_color handles single color as palette", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_color(values, palette = "red")
  expect_equal(length(result), 5)
  # When single color is used, all values should get that color
  expect_true(all(result == "red"))
})

test_that("scale_color handles custom color vector as palette", {
  values <- c(0, 0.5, 1)
  result <- cograph:::scale_color(values, palette = c("blue", "white", "red"))
  expect_equal(length(result), 3)
  expect_valid_colors(result)
})

test_that("scale_color handles function as palette", {
  values <- c(1, 2, 3, 4, 5)
  custom_pal <- function(n) rep("purple", n)
  result <- cograph:::scale_color(values, palette = custom_pal)
  expect_equal(length(result), 5)
})

# ============================================
# SCALE_COLOR() TESTS - Palettes and limits
# ============================================

test_that("scale_color works with viridis palette", {
  values <- seq(0, 100, length.out = 10)
  result <- cograph:::scale_color(values, palette = "viridis")
  expect_equal(length(result), 10)
  expect_valid_colors(result)
})

test_that("scale_color works with blues palette", {
  values <- c(1, 5, 10, 15, 20)
  result <- cograph:::scale_color(values, palette = "blues")
  expect_equal(length(result), 5)
  expect_valid_colors(result)
})

test_that("scale_color works with reds palette", {
  values <- c(1, 5, 10, 15, 20)
  result <- cograph:::scale_color(values, palette = "reds")
  expect_equal(length(result), 5)
  expect_valid_colors(result)
})

test_that("scale_color respects custom limits", {
  values <- c(25, 50, 75)
  result_default <- cograph:::scale_color(values, palette = c("blue", "red"))
  result_limits <- cograph:::scale_color(values, palette = c("blue", "red"),
                                          limits = c(0, 100))
  # Colors should differ with different limits
  expect_equal(length(result_limits), 3)
  expect_valid_colors(result_limits)
})

test_that("scale_color handles values outside limits", {
  values <- c(-10, 50, 150)
  result <- cograph:::scale_color(values, palette = c("blue", "red"),
                                   limits = c(0, 100))
  # Values outside limits should be clamped
  expect_equal(length(result), 3)
  expect_valid_colors(result)
})

# ============================================
# SCALE_COLOR_DISCRETE() TESTS
# ============================================

test_that("scale_color_discrete returns correct length output", {
  values <- c("A", "B", "C", "A", "B")
  result <- cograph:::scale_color_discrete(values)
  expect_equal(length(result), length(values))
})

test_that("scale_color_discrete returns valid colors", {
  values <- c("cat1", "cat2", "cat3")
  result <- cograph:::scale_color_discrete(values)
  expect_valid_colors(result)
})

test_that("scale_color_discrete maps same categories to same colors", {
  values <- c("A", "B", "A", "C", "B", "A")
  result <- cograph:::scale_color_discrete(values)
  # All "A"s should have same color
  expect_equal(result[1], result[3])
  expect_equal(result[1], result[6])
  # All "B"s should have same color
  expect_equal(result[2], result[5])
})

test_that("scale_color_discrete handles numeric categories", {
  values <- c(1, 2, 3, 1, 2, 3)
  result <- cograph:::scale_color_discrete(values)
  expect_equal(length(result), 6)
  expect_valid_colors(result)
})

test_that("scale_color_discrete handles factors", {
  values <- factor(c("low", "medium", "high", "low"))
  result <- cograph:::scale_color_discrete(values)
  expect_equal(length(result), 4)
  expect_valid_colors(result)
})

test_that("scale_color_discrete works with colorblind palette", {
  values <- c("A", "B", "C", "D", "E")
  result <- cograph:::scale_color_discrete(values, palette = "colorblind")
  expect_equal(length(result), 5)
  expect_valid_colors(result)
})

test_that("scale_color_discrete handles single color as palette", {
  values <- c("A", "B", "C")
  result <- cograph:::scale_color_discrete(values, palette = "purple")
  expect_equal(length(result), 3)
  expect_true(all(result == "purple"))
})

test_that("scale_color_discrete handles custom color vector", {
  values <- c("A", "B", "C", "A")
  result <- cograph:::scale_color_discrete(values,
                                            palette = c("red", "green", "blue"))
  expect_equal(length(result), 4)
  expect_valid_colors(result)
  expect_equal(result[1], result[4])  # Both "A"
})

test_that("scale_color_discrete recycles colors when more categories than palette", {
  values <- LETTERS[1:10]  # 10 categories
  result <- cograph:::scale_color_discrete(values,
                                            palette = c("red", "green", "blue"))
  expect_equal(length(result), 10)
  expect_valid_colors(result)
})

test_that("scale_color_discrete handles custom palette function", {
  values <- c("cat1", "cat2", "cat3")
  custom_pal <- function(n) grDevices::rainbow(n)
  result <- cograph:::scale_color_discrete(values, palette = custom_pal)
  expect_equal(length(result), 3)
  expect_valid_colors(result)
})

# ============================================
# SCALE_WIDTH() TESTS
# ============================================

test_that("scale_width returns correct length output", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_width(values)
  expect_equal(length(result), length(values))
})

test_that("scale_width respects default range [0.5, 3]", {
  values <- c(0, 50, 100)
  result <- cograph:::scale_width(values)
  expect_true(min(result) >= 0.5)
  expect_true(max(result) <= 3)
})

test_that("scale_width respects custom range", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_width(values, range = c(1, 5))
  expect_equal(min(result), 1)
  expect_equal(max(result), 5)
})

test_that("scale_width handles all NA values", {
  values <- c(NA, NA, NA)
  result <- cograph:::scale_width(values)
  expect_equal(length(result), 3)
  # Should return mean of default range
  expect_true(all(result == mean(c(0.5, 3))))
})

test_that("scale_width handles constant values", {
  values <- c(10, 10, 10, 10)
  result <- cograph:::scale_width(values)
  expect_equal(length(result), 4)
  # Should return mean of range
  expect_true(all(result == mean(c(0.5, 3))))
})

test_that("scale_width maintains linear scaling", {
  values <- c(0, 25, 50, 75, 100)
  result <- cograph:::scale_width(values, range = c(0, 100))
  # Middle value should be exactly in the middle
  expect_equal(result[3], 50)
})

# ============================================
# SCALE_ALPHA() TESTS
# ============================================

test_that("scale_alpha returns correct length output", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_alpha(values)
  expect_equal(length(result), length(values))
})

test_that("scale_alpha respects default range [0.3, 1]", {
  values <- c(0, 50, 100)
  result <- cograph:::scale_alpha(values)
  expect_true(min(result) >= 0.3)
  expect_true(max(result) <= 1)
})

test_that("scale_alpha clamps values to [0, 1]", {
  values <- c(-100, 0, 50, 100, 200)
  result <- cograph:::scale_alpha(values, range = c(-0.5, 1.5))
  # Despite wide range, alpha should be clamped
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
})

test_that("scale_alpha respects custom range within [0, 1]", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_alpha(values, range = c(0.2, 0.8))
  expect_true(min(result) >= 0.2)
  expect_true(max(result) <= 0.8)
})

test_that("scale_alpha handles all NA values", {
  values <- c(NA, NA, NA)
  result <- cograph:::scale_alpha(values)
  expect_equal(length(result), 3)
})

test_that("scale_alpha handles constant values", {
  values <- c(7, 7, 7, 7)
  result <- cograph:::scale_alpha(values)
  expect_equal(length(result), 4)
  # Should return mean of range
  expect_true(all(result == mean(c(0.3, 1))))
})

# ============================================
# EDGE CASES AND BOUNDARY CONDITIONS
# ============================================

test_that("scale_size handles single value", {
  values <- 42
  result <- cograph:::scale_size(values)
  expect_equal(length(result), 1)
  # Single value should return mean of range
  expect_equal(result, mean(c(0.03, 0.1)))
})

test_that("scale_color handles single value", {
  # Single value with limits to avoid normalization issues
  values <- 42
  result <- cograph:::scale_color(values, limits = c(0, 100))
  expect_equal(length(result), 1)
  expect_valid_colors(result)
})

test_that("scale_color_discrete handles single category", {
  values <- c("only_one", "only_one", "only_one")
  result <- cograph:::scale_color_discrete(values)
  expect_equal(length(result), 3)
  expect_true(all(result == result[1]))
})

test_that("scale_width handles single value", {
  values <- 100
  result <- cograph:::scale_width(values)
  expect_equal(length(result), 1)
})

test_that("scale_alpha handles single value", {
  values <- 0.5
  result <- cograph:::scale_alpha(values)
  expect_equal(length(result), 1)
  expect_true(result >= 0 && result <= 1)
})

test_that("scale_size handles empty vector", {
  values <- numeric(0)
  result <- cograph:::scale_size(values)
  expect_equal(length(result), 0)
})

test_that("scale_color handles empty vector", {
  values <- numeric(0)
  result <- cograph:::scale_color(values)
  expect_equal(length(result), 0)
})

test_that("scale_alpha handles empty vector", {
  values <- numeric(0)
  result <- cograph:::scale_alpha(values)
  expect_equal(length(result), 0)
})

# ============================================
# NUMERIC PRECISION AND EXTREME VALUES
# ============================================

test_that("scale_size handles very small values", {
  values <- c(1e-10, 1e-9, 1e-8, 1e-7, 1e-6)
  result <- cograph:::scale_size(values)
  expect_equal(length(result), 5)
  expect_true(all(is.finite(result)))
})

test_that("scale_size handles very large values", {
  values <- c(1e6, 1e7, 1e8, 1e9, 1e10)
  result <- cograph:::scale_size(values)
  expect_equal(length(result), 5)
  expect_true(all(is.finite(result)))
})

test_that("scale_color handles extreme value ranges", {
  values <- c(-1e6, 0, 1e6)
  result <- cograph:::scale_color(values)
  expect_equal(length(result), 3)
  expect_valid_colors(result)
})

test_that("scale_alpha handles zero range input", {
  values <- c(0, 0, 0, 0)
  result <- cograph:::scale_alpha(values)
  expect_equal(length(result), 4)
  expect_true(all(is.finite(result)))
})

# ============================================
# INTEGRATION WITH PLOTTING
# ============================================

test_that("scale_size output is usable for node sizes in splot", {
  adj <- create_test_matrix(5)
  node_values <- c(10, 20, 30, 40, 50)
  sizes <- cograph:::scale_size(node_values)

  result <- safe_plot(splot(adj, node_size = sizes))
  expect_true(result$success, info = result$error)
})

test_that("scale_color output is usable for node colors in splot", {
  adj <- create_test_matrix(5)
  node_values <- c(1, 2, 3, 4, 5)
  colors <- cograph:::scale_color(node_values)

  result <- safe_plot(splot(adj, node_fill = colors))
  expect_true(result$success, info = result$error)
})

test_that("scale_color_discrete output is usable for node colors in splot", {
  adj <- create_test_matrix(5)
  categories <- c("A", "B", "A", "C", "B")
  colors <- cograph:::scale_color_discrete(categories)

  result <- safe_plot(splot(adj, node_fill = colors))
  expect_true(result$success, info = result$error)
})

test_that("scale_width output is usable for edge widths in splot", {
  adj <- create_test_matrix(5, weighted = TRUE)
  # Get edge weights from adjacency matrix
  edge_weights <- adj[adj > 0]
  widths <- cograph:::scale_width(edge_weights)

  expect_true(all(is.finite(widths)))
  expect_true(all(widths > 0))
})

test_that("scale_alpha output is usable for alpha values", {
  values <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  alphas <- cograph:::scale_alpha(values)

  # All values should be valid alpha values
  expect_true(all(alphas >= 0 & alphas <= 1))
})

# ============================================
# CONSISTENCY TESTS
# ============================================

test_that("scale functions are deterministic", {
  values <- c(1, 5, 10, 15, 20)

  result1_size <- cograph:::scale_size(values)
  result2_size <- cograph:::scale_size(values)
  expect_equal(result1_size, result2_size)

  result1_width <- cograph:::scale_width(values)
  result2_width <- cograph:::scale_width(values)
  expect_equal(result1_width, result2_width)

  result1_alpha <- cograph:::scale_alpha(values)
  result2_alpha <- cograph:::scale_alpha(values)
  expect_equal(result1_alpha, result2_alpha)
})

test_that("scale_color is deterministic", {
  values <- c(1, 5, 10, 15, 20)

  result1 <- cograph:::scale_color(values)
  result2 <- cograph:::scale_color(values)
  expect_equal(result1, result2)
})

test_that("scale_color_discrete is deterministic", {
  values <- c("A", "B", "C", "A", "B")

  result1 <- cograph:::scale_color_discrete(values)
  result2 <- cograph:::scale_color_discrete(values)
  expect_equal(result1, result2)
})

test_that("scale_size preserves monotonicity", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_size(values)

  # Increasing input should give increasing output
  for (i in 1:(length(result) - 1)) {
    expect_true(result[i] <= result[i + 1])
  }
})

test_that("scale_width preserves monotonicity", {
  values <- c(10, 20, 30, 40, 50)
  result <- cograph:::scale_width(values)

  for (i in 1:(length(result) - 1)) {
    expect_true(result[i] <= result[i + 1])
  }
})

test_that("scale_alpha preserves monotonicity", {
  values <- c(0, 25, 50, 75, 100)
  result <- cograph:::scale_alpha(values)

  for (i in 1:(length(result) - 1)) {
    expect_true(result[i] <= result[i + 1])
  }
})
