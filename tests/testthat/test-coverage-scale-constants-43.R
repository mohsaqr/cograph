# test-coverage-scale-constants-43.R - Comprehensive tests for scale-constants.R
# Tests for QGRAPH_SCALE, COGRAPH_SCALE, COGRAPH_SCALE_LEGACY constants,
# get_scale_constants(), compute_adaptive_esize(), and scale_edge_widths()

# ============================================
# Internal Functions Access
# ============================================

get_scale_constants <- cograph:::get_scale_constants
compute_adaptive_esize <- cograph:::compute_adaptive_esize
scale_edge_widths <- cograph:::scale_edge_widths

# ============================================
# QGRAPH_SCALE CONSTANT TESTS
# ============================================

test_that("QGRAPH_SCALE contains required vsize constants", {
  scale <- cograph:::QGRAPH_SCALE
  expect_true("vsize_base" %in% names(scale))
  expect_true("vsize_decay" %in% names(scale))
  expect_true("vsize_min" %in% names(scale))
  expect_true("vsize_factor" %in% names(scale))
})

test_that("QGRAPH_SCALE contains required esize constants", {
  scale <- cograph:::QGRAPH_SCALE
  expect_true("esize_base" %in% names(scale))
  expect_true("esize_decay" %in% names(scale))
  expect_true("esize_min" %in% names(scale))
  expect_true("esize_unweighted" %in% names(scale))
  expect_true("esize_scale" %in% names(scale))
})

test_that("QGRAPH_SCALE contains cent2edge constants", {
  scale <- cograph:::QGRAPH_SCALE
  expect_true("cent2edge_divisor" %in% names(scale))
  expect_true("cent2edge_reference" %in% names(scale))
  expect_true("cent2edge_plot_ref" %in% names(scale))
})

test_that("QGRAPH_SCALE contains curve and arrow constants", {
  scale <- cograph:::QGRAPH_SCALE
  expect_true("curve_ref_diagonal" %in% names(scale))
  expect_true("arrow_factor" %in% names(scale))
})

test_that("QGRAPH_SCALE values are numeric and positive", {
  scale <- cograph:::QGRAPH_SCALE
  expect_true(is.numeric(scale$vsize_base))
  expect_true(scale$vsize_base > 0)
  expect_true(is.numeric(scale$esize_base))
  expect_true(scale$esize_base > 0)
  expect_true(is.numeric(scale$arrow_factor))
  expect_true(scale$arrow_factor > 0)
})

# ============================================
# COGRAPH_SCALE CONSTANT TESTS
# ============================================

test_that("COGRAPH_SCALE contains node sizing constants", {
  scale <- cograph:::COGRAPH_SCALE
  expect_true("node_factor" %in% names(scale))
  expect_true("node_default" %in% names(scale))
  expect_true(is.numeric(scale$node_factor))
  expect_true(is.numeric(scale$node_default))
})

test_that("COGRAPH_SCALE contains label sizing constants", {
  scale <- cograph:::COGRAPH_SCALE
  expect_true("label_default" %in% names(scale))
  expect_true("label_coupled" %in% names(scale))
  expect_false(scale$label_coupled)  # Default mode has decoupled labels
})

test_that("COGRAPH_SCALE contains edge sizing constants", {
  scale <- cograph:::COGRAPH_SCALE
  expect_true("edge_base" %in% names(scale))
  expect_true("edge_scale" %in% names(scale))
  expect_true("edge_default" %in% names(scale))
  expect_true("edge_width_range" %in% names(scale))
  expect_true("edge_scale_mode" %in% names(scale))
})

test_that("COGRAPH_SCALE edge_width_range is valid two-element vector", {
  scale <- cograph:::COGRAPH_SCALE
  expect_equal(length(scale$edge_width_range), 2)
  expect_true(scale$edge_width_range[1] < scale$edge_width_range[2])
})

test_that("COGRAPH_SCALE contains arrow constants", {
  scale <- cograph:::COGRAPH_SCALE
  expect_true("arrow_factor" %in% names(scale))
  expect_true("arrow_default" %in% names(scale))
  expect_true(scale$arrow_factor > 0)
})

test_that("COGRAPH_SCALE contains soplot-specific factor", {
  scale <- cograph:::COGRAPH_SCALE
  expect_true("soplot_node_factor" %in% names(scale))
  expect_true(scale$soplot_node_factor > 0)
  # soplot factor should be smaller than splot factor (NPC vs user coords)
  expect_true(scale$soplot_node_factor < scale$node_factor)
})

# ============================================
# COGRAPH_SCALE_LEGACY CONSTANT TESTS
# ============================================

test_that("COGRAPH_SCALE_LEGACY has different node factor than default", {
  default_scale <- cograph:::COGRAPH_SCALE
  legacy_scale <- cograph:::COGRAPH_SCALE_LEGACY
  expect_true(legacy_scale$node_factor != default_scale$node_factor)
})

test_that("COGRAPH_SCALE_LEGACY has coupled labels", {
  scale <- cograph:::COGRAPH_SCALE_LEGACY
  expect_true(scale$label_coupled)  # Legacy mode has coupled labels
  expect_null(scale$label_default)  # NULL because coupled to node size
})

test_that("COGRAPH_SCALE_LEGACY has different arrow factors for splot and soplot", {
  scale <- cograph:::COGRAPH_SCALE_LEGACY
  expect_true("arrow_factor" %in% names(scale))
  expect_true("arrow_factor_soplot" %in% names(scale))
  expect_true(scale$arrow_factor != scale$arrow_factor_soplot)
})

test_that("COGRAPH_SCALE_LEGACY edge_width_range differs from default", {
  default_scale <- cograph:::COGRAPH_SCALE
  legacy_scale <- cograph:::COGRAPH_SCALE_LEGACY
  expect_false(identical(legacy_scale$edge_width_range, default_scale$edge_width_range))
})

# ============================================
# GET_SCALE_CONSTANTS() TESTS
# ============================================

test_that("get_scale_constants returns COGRAPH_SCALE for 'default'", {
  result <- get_scale_constants("default")
  expected <- cograph:::COGRAPH_SCALE
  expect_equal(result, expected)
})

test_that("get_scale_constants returns COGRAPH_SCALE_LEGACY for 'legacy'", {
  result <- get_scale_constants("legacy")
  expected <- cograph:::COGRAPH_SCALE_LEGACY
  expect_equal(result, expected)
})

test_that("get_scale_constants returns default for unrecognized scaling", {
  result <- get_scale_constants("unknown_mode")
  expected <- cograph:::COGRAPH_SCALE
  expect_equal(result, expected)
})

test_that("get_scale_constants returns default for NULL scaling", {
  result <- get_scale_constants(NULL)
  expected <- cograph:::COGRAPH_SCALE
  expect_equal(result, expected)
})

test_that("get_scale_constants returns default when called with no arguments", {
  result <- get_scale_constants()
  expected <- cograph:::COGRAPH_SCALE
  expect_equal(result, expected)
})

test_that("get_scale_constants is case sensitive", {
  result_lower <- get_scale_constants("legacy")
  result_upper <- get_scale_constants("LEGACY")
  # "LEGACY" should return default since it's not matching
  expect_equal(result_upper, cograph:::COGRAPH_SCALE)
  expect_equal(result_lower, cograph:::COGRAPH_SCALE_LEGACY)
})

# ============================================
# COMPUTE_ADAPTIVE_ESIZE() TESTS - Basic
# ============================================

test_that("compute_adaptive_esize returns positive numeric", {
  result <- compute_adaptive_esize(10)
  expect_type(result, "double")
  expect_true(result > 0)
})

test_that("compute_adaptive_esize decreases with more nodes", {
  result_few <- compute_adaptive_esize(5)
  result_many <- compute_adaptive_esize(100)
  expect_true(result_few > result_many)
})

test_that("compute_adaptive_esize handles single node", {
  result <- compute_adaptive_esize(1)
  expect_true(is.finite(result))
  expect_true(result > 0)
})

test_that("compute_adaptive_esize handles zero nodes", {
  result <- compute_adaptive_esize(0)
  expect_true(is.finite(result))
  # With 0 nodes, formula gives 4*exp(0) + 1.5 = 5.5
  expect_equal(result, 4 * exp(0) + 1.5)
})

test_that("compute_adaptive_esize is thinner for directed networks", {
  result_undirected <- compute_adaptive_esize(50, directed = FALSE)
  result_directed <- compute_adaptive_esize(50, directed = TRUE)
  expect_true(result_directed < result_undirected)
})

test_that("compute_adaptive_esize directed has minimum of 1", {
  # With many nodes, directed edges get thin, but not below 1
  result <- compute_adaptive_esize(500, directed = TRUE)
  expect_true(result >= 1)
})

test_that("compute_adaptive_esize produces expected values for typical sizes", {
  # Test that formula produces reasonable line widths
  result_3 <- compute_adaptive_esize(3)
  result_50 <- compute_adaptive_esize(50)
  result_200 <- compute_adaptive_esize(200)

  # Based on formula: 4 * exp(-n/150) + 1.5
  expect_true(result_3 > 4)  # ~5.4 for 3 nodes
  expect_true(result_50 > 2.5 && result_50 < 4.5)  # ~4.4 for 50 nodes
  expect_true(result_200 > 1 && result_200 < 3)  # ~2.6 for 200 nodes
})

test_that("compute_adaptive_esize handles very large node count", {
  result <- compute_adaptive_esize(10000)
  expect_true(is.finite(result))
  # Should approach minimum value of 1.5 (as exp(-10000/150) -> 0)
  expect_true(result >= 1.5 && result < 2)
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Empty/Edge Cases
# ============================================

test_that("scale_edge_widths returns empty vector for empty input", {
  result <- scale_edge_widths(numeric(0))
  expect_equal(length(result), 0)
  expect_type(result, "double")
})

test_that("scale_edge_widths handles single weight", {
  result <- scale_edge_widths(0.5)
  expect_equal(length(result), 1)
  expect_true(is.finite(result))
})

test_that("scale_edge_widths handles all zero weights", {
  weights <- c(0, 0, 0)
  result <- scale_edge_widths(weights)
  expect_equal(length(result), 3)
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths handles NA values", {
  weights <- c(0.5, NA, 1.0)
  result <- scale_edge_widths(weights)
  expect_equal(length(result), 3)
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths handles all NA values", {
  weights <- c(NA, NA, NA)
  result <- suppressWarnings(scale_edge_widths(weights))
  expect_equal(length(result), 3)
  expect_true(all(is.finite(result)))
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Linear Mode
# ============================================

test_that("scale_edge_widths linear mode produces proportional scaling", {
  weights <- c(0, 0.5, 1.0)
  result <- scale_edge_widths(weights, mode = "linear", range = c(0, 1))

  expect_equal(result[1], 0)  # 0 weight -> min width
  expect_equal(result[2], 0.5)  # half weight -> half width
  expect_equal(result[3], 1.0)  # max weight -> max width
})

test_that("scale_edge_widths linear mode preserves order", {
  weights <- c(0.1, 0.3, 0.7, 0.9)
  result <- scale_edge_widths(weights, mode = "linear")

  # Result should be monotonically increasing
  expect_true(all(diff(result) >= 0))
})

test_that("scale_edge_widths linear mode respects range parameter", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights, mode = "linear", range = c(2, 8))

  expect_true(min(result) >= 2)
  expect_true(max(result) <= 8)
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Log Mode
# ============================================

test_that("scale_edge_widths log mode handles wide ranges", {
  weights <- c(0.001, 0.01, 0.1, 1, 10, 100)
  result <- scale_edge_widths(weights, mode = "log")

  expect_equal(length(result), 6)
  expect_true(all(is.finite(result)))
  expect_true(all(diff(result) >= 0))  # Monotonically increasing
})

test_that("scale_edge_widths log mode compresses large values", {
  weights <- c(1, 100)
  result_linear <- scale_edge_widths(weights, mode = "linear", range = c(0, 1))
  result_log <- scale_edge_widths(weights, mode = "log", range = c(0, 1))

  # Log mode should compress the range (smaller difference between values)
  linear_diff <- result_linear[2] - result_linear[1]
  log_diff <- result_log[2] - result_log[1]
  expect_true(log_diff < linear_diff)
})

test_that("scale_edge_widths log mode handles zero via log1p", {
  weights <- c(0, 1, 10)
  result <- scale_edge_widths(weights, mode = "log")

  expect_true(all(is.finite(result)))
  expect_true(result[1] < result[2])  # Zero weight should be smallest
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Sqrt Mode
# ============================================

test_that("scale_edge_widths sqrt mode produces valid output", {
  weights <- c(0, 1, 4, 9, 16)
  result <- scale_edge_widths(weights, mode = "sqrt")

  expect_equal(length(result), 5)
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths sqrt mode compresses distribution", {
  weights <- c(0.1, 1.0)
  result_linear <- scale_edge_widths(weights, mode = "linear", range = c(0, 1))
  result_sqrt <- scale_edge_widths(weights, mode = "sqrt", range = c(0, 1))

  # Sqrt mode should give larger result for small values relative to max
  expect_true(result_sqrt[1] > result_linear[1])
})

test_that("scale_edge_widths sqrt mode preserves monotonicity", {
  weights <- c(0.05, 0.2, 0.5, 0.8, 1.0)
  result <- scale_edge_widths(weights, mode = "sqrt")

  expect_true(all(diff(result) >= 0))
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Rank Mode
# ============================================

test_that("scale_edge_widths rank mode produces equal spacing", {
  weights <- c(0.01, 0.02, 100, 1000)
  result <- scale_edge_widths(weights, mode = "rank", range = c(0, 1))

  # Rank mode should space values evenly regardless of actual magnitudes
  diffs <- diff(result)
  expect_true(all(abs(diffs - diffs[1]) < 0.01))  # Near-equal spacing
})

test_that("scale_edge_widths rank mode handles ties", {
  weights <- c(0.5, 0.5, 0.5)
  result <- scale_edge_widths(weights, mode = "rank")

  # All same weight -> all same rank -> all same width (middle of range)
  expect_true(all(result == result[1]))
})

test_that("scale_edge_widths rank mode with single value", {
  weights <- 0.5
  result <- scale_edge_widths(weights, mode = "rank")

  # Single value should be mapped to middle of range
  expect_equal(length(result), 1)
  expect_true(is.finite(result))
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Invalid Mode
# ============================================

test_that("scale_edge_widths throws error for invalid mode", {
  weights <- c(0.5, 1.0)
  expect_error(
    scale_edge_widths(weights, mode = "invalid_mode"),
    "edge_scale_mode must be one of"
  )
})

test_that("scale_edge_widths error message lists valid modes", {
  weights <- c(0.5, 1.0)
  expect_error(
    scale_edge_widths(weights, mode = "wrong"),
    "linear|log|sqrt|rank"
  )
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Maximum Parameter
# ============================================

test_that("scale_edge_widths respects explicit maximum", {
  weights <- c(0.25, 0.5)
  result <- scale_edge_widths(weights, maximum = 1.0, range = c(0, 1))

  # With max=1.0, 0.5 should be scaled to 0.5
  expect_equal(result[2], 0.5, tolerance = 0.001)
})

test_that("scale_edge_widths auto-detects maximum when NULL", {
  weights <- c(0.25, 0.5)
  result <- scale_edge_widths(weights, maximum = NULL, range = c(0, 1))

  # Max weight is 0.5, so 0.5 should scale to 1.0
  expect_equal(result[2], 1.0, tolerance = 0.001)
})

test_that("scale_edge_widths handles maximum = 0", {
  weights <- c(0, 0, 0)
  result <- scale_edge_widths(weights)

  # Should not produce NaN or Inf
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths handles NA maximum", {
  weights <- c(NA, NA)
  result <- suppressWarnings(scale_edge_widths(weights))

  expect_true(all(is.finite(result)))
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Minimum Parameter
# ============================================

test_that("scale_edge_widths applies minimum threshold", {
  weights <- c(0.1, 0.3, 0.5, 0.8)
  result <- scale_edge_widths(weights, minimum = 0.4, range = c(1, 5))

  # Weights below 0.4 should get minimum width
  expect_equal(result[1], 1)  # 0.1 < 0.4
  expect_equal(result[2], 1)  # 0.3 < 0.4
  expect_true(result[3] > 1)  # 0.5 >= 0.4
  expect_true(result[4] > 1)  # 0.8 >= 0.4
})

test_that("scale_edge_widths handles minimum = 0 (no threshold)", {
  weights <- c(0.01, 0.1, 1.0)
  result <- scale_edge_widths(weights, minimum = 0)

  expect_equal(length(result), 3)
  expect_true(result[1] < result[2])
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Negative Weights
# ============================================

test_that("scale_edge_widths uses absolute values", {
  weights_pos <- c(0.5, 1.0)
  weights_neg <- c(-0.5, -1.0)

  result_pos <- scale_edge_widths(weights_pos)
  result_neg <- scale_edge_widths(weights_neg)

  expect_equal(result_pos, result_neg)
})

test_that("scale_edge_widths handles mixed positive/negative weights", {
  weights <- c(-1.0, -0.5, 0, 0.5, 1.0)
  result <- scale_edge_widths(weights)

  # abs(-1) = abs(1) = 1, so first and last should be equal
  expect_equal(result[1], result[5])
  # abs(-0.5) = abs(0.5), so second and fourth should be equal
  expect_equal(result[2], result[4])
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - esize Parameter
# ============================================

test_that("scale_edge_widths uses esize as maximum width", {
  weights <- c(0.5, 1.0)
  result <- scale_edge_widths(weights, esize = 6, range = c(0.5, 4))

  # esize should override range[2]
  expect_true(max(result) <= 6)
})

test_that("scale_edge_widths respects range when esize is NULL", {
  weights <- c(0.5, 1.0)
  result <- scale_edge_widths(weights, esize = NULL, range = c(0.5, 4))

  # Should use range as-is
  expect_true(max(result) <= 4)
  expect_true(min(result) >= 0.5)
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Cut Parameter
# ============================================

test_that("scale_edge_widths handles cut parameter", {
  weights <- c(0.1, 0.3, 0.6, 0.9)
  result <- scale_edge_widths(weights, cut = 0.5)

  # Cut parameter is now for transparency, not width - just verify no errors
  expect_equal(length(result), 4)
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths handles cut = 0 (disabled)", {
  weights <- c(0.1, 0.3, 0.6, 0.9)
  result <- scale_edge_widths(weights, cut = 0)

  expect_equal(length(result), 4)
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths handles cut = NULL (auto)", {
  weights <- c(0.1, 0.3, 0.6, 0.9)
  result <- scale_edge_widths(weights, cut = NULL)

  expect_equal(length(result), 4)
  expect_true(all(is.finite(result)))
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Range Parameter
# ============================================

test_that("scale_edge_widths respects custom range", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights, range = c(1, 10))

  expect_true(min(result) >= 1)
  expect_true(max(result) <= 10)
})

test_that("scale_edge_widths handles narrow range", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights, range = c(2, 3))

  expect_true(min(result) >= 2)
  expect_true(max(result) <= 3)
})

test_that("scale_edge_widths handles single-value range (degenerate)", {
  weights <- c(0.1, 0.5, 1.0)
  result <- scale_edge_widths(weights, range = c(5, 5))

  # All values should be the same
  expect_true(all(result == 5))
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Uniform Weights
# ============================================

test_that("scale_edge_widths handles uniform weights", {
  weights <- c(0.5, 0.5, 0.5, 0.5)
  result <- scale_edge_widths(weights, range = c(1, 5))

  # All same weight should produce all same width
  expect_true(all(result == result[1]))
})

test_that("scale_edge_widths uniform weights linear mode", {
  weights <- c(1, 1, 1)
  result <- scale_edge_widths(weights, mode = "linear", range = c(0, 1))

  expect_true(all(result == result[1]))
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths uniform weights log mode", {
  weights <- c(1, 1, 1)
  result <- scale_edge_widths(weights, mode = "log", range = c(0, 1))

  expect_true(all(result == result[1]))
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths uniform weights sqrt mode", {
  weights <- c(1, 1, 1)
  result <- scale_edge_widths(weights, mode = "sqrt", range = c(0, 1))

  expect_true(all(result == result[1]))
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths uniform weights rank mode", {
  weights <- c(1, 1, 1)
  result <- scale_edge_widths(weights, mode = "rank", range = c(0, 1))

  expect_true(all(result == result[1]))
  expect_true(all(is.finite(result)))
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Extreme Values
# ============================================

test_that("scale_edge_widths handles very small weights", {
  weights <- c(1e-10, 1e-9, 1e-8)
  result <- scale_edge_widths(weights)

  expect_equal(length(result), 3)
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths handles very large weights", {
  weights <- c(1e6, 1e7, 1e8)
  result <- scale_edge_widths(weights)

  expect_equal(length(result), 3)
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths handles mixed extreme values", {
  weights <- c(1e-10, 1, 1e10)
  result <- scale_edge_widths(weights, mode = "log")

  expect_equal(length(result), 3)
  expect_true(all(is.finite(result)))
  expect_true(result[1] < result[2])
  expect_true(result[2] < result[3])
})

# ============================================
# SCALE_EDGE_WIDTHS() TESTS - Integration
# ============================================

test_that("scale_edge_widths output is valid for splot edge widths", {
  # Simulate typical network weights
  set.seed(42)
  weights <- runif(20, 0.1, 1.0)
  result <- scale_edge_widths(weights)

  # All values should be positive and finite
  expect_true(all(result > 0))
  expect_true(all(is.finite(result)))
})

test_that("scale_edge_widths works with all mode/range combinations", {
  weights <- c(0.2, 0.5, 0.8)
  modes <- c("linear", "log", "sqrt", "rank")

  for (mode in modes) {
    result <- scale_edge_widths(weights, mode = mode, range = c(1, 5))
    expect_equal(length(result), 3, info = paste("Mode:", mode))
    expect_true(all(is.finite(result)), info = paste("Mode:", mode))
    expect_true(all(result >= 1), info = paste("Mode:", mode))
    expect_true(all(result <= 5), info = paste("Mode:", mode))
  }
})

test_that("scale_edge_widths with n_nodes and directed is accepted", {
  weights <- c(0.3, 0.6, 0.9)
  # These parameters are allowed but may not affect current implementation
  result <- scale_edge_widths(weights, n_nodes = 10, directed = TRUE)

  expect_equal(length(result), 3)
  expect_true(all(is.finite(result)))
})

# ============================================
# CONSISTENCY TESTS
# ============================================

test_that("scale_edge_widths is deterministic", {
  weights <- c(0.1, 0.5, 0.9)
  result1 <- scale_edge_widths(weights, mode = "linear")
  result2 <- scale_edge_widths(weights, mode = "linear")
  expect_equal(result1, result2)
})

test_that("compute_adaptive_esize is deterministic", {
  result1 <- compute_adaptive_esize(50, directed = TRUE)
  result2 <- compute_adaptive_esize(50, directed = TRUE)
  expect_equal(result1, result2)
})

test_that("get_scale_constants returns identical objects", {
  result1 <- get_scale_constants("default")
  result2 <- get_scale_constants("default")
  expect_identical(result1, result2)
})

# ============================================
# BOUNDARY CONDITION TESTS
# ============================================

test_that("scale_edge_widths clamps normalized values to [0,1]", {
  # Test with weights exceeding explicit maximum
  weights <- c(0.5, 1.0, 2.0)  # 2.0 exceeds max of 1.0
  result <- scale_edge_widths(weights, maximum = 1.0, range = c(0, 1))

  # Result should not exceed range[2]
  expect_true(all(result <= 1))
})

test_that("scale_edge_widths handles negative minimum threshold", {
  weights <- c(-1.0, -0.5, 0.5, 1.0)
  result <- scale_edge_widths(weights, minimum = 0.3)

  # abs(-0.5) = 0.5 >= 0.3, should not be thresholded
  # abs(-1.0) = 1.0 > abs(-0.5) = 0.5, so result[1] > result[2]
  expect_true(result[1] > result[2])
  # result[2] should equal result[3] since abs(-0.5) = abs(0.5)
  expect_equal(result[2], result[3])
})

test_that("compute_adaptive_esize formula matches documentation", {
  # Formula: 4 * exp(-n/150) + 1.5
  n <- 50
  expected <- 4 * exp(-n / 150) + 1.5
  result <- compute_adaptive_esize(n, directed = FALSE)
  expect_equal(result, expected)
})

test_that("compute_adaptive_esize directed reduction is 30%", {
  n <- 50
  undirected <- compute_adaptive_esize(n, directed = FALSE)
  directed <- compute_adaptive_esize(n, directed = TRUE)

  # Directed should be 70% of undirected (30% reduction)
  expected <- max(undirected * 0.7, 1)
  expect_equal(directed, expected)
})
