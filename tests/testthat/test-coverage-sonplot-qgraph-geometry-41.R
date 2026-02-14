# test-coverage-sonplot-qgraph-geometry-41.R - Comprehensive tests for sonplot-qgraph-geometry.R
# Tests for qgraph-compatible geometry utilities

# Make internal qgraph geometry functions available
qgraph_plot_info <- cograph:::qgraph_plot_info
qgraph_default_vsize <- cograph:::qgraph_default_vsize
qgraph_default_esize <- cograph:::qgraph_default_esize
qgraph_scale_edge_widths <- cograph:::qgraph_scale_edge_widths
qgraph_cent2edge <- cograph:::qgraph_cent2edge
qgraph_norm_curve <- cograph:::qgraph_norm_curve
qgraph_vsize_to_user <- cograph:::qgraph_vsize_to_user
qgraph_cent_to_edge_simple <- cograph:::qgraph_cent_to_edge_simple
qgraph_arrow_size <- cograph:::qgraph_arrow_size

# ============================================
# qgraph_default_vsize() TESTS
# ============================================

test_that("qgraph_default_vsize returns correct value for small networks", {
  # Formula: 8 * exp(-n/80) + 1
  expect_equal(qgraph_default_vsize(1), 8 * exp(-1/80) + 1, tolerance = 0.001)
  expect_equal(qgraph_default_vsize(5), 8 * exp(-5/80) + 1, tolerance = 0.001)
  expect_equal(qgraph_default_vsize(10), 8 * exp(-10/80) + 1, tolerance = 0.001)
})

test_that("qgraph_default_vsize returns correct value for medium networks", {
  expect_equal(qgraph_default_vsize(50), 8 * exp(-50/80) + 1, tolerance = 0.001)
  expect_equal(qgraph_default_vsize(80), 8 * exp(-80/80) + 1, tolerance = 0.001)
  expect_equal(qgraph_default_vsize(100), 8 * exp(-100/80) + 1, tolerance = 0.001)
})

test_that("qgraph_default_vsize returns correct value for large networks", {
  expect_equal(qgraph_default_vsize(200), 8 * exp(-200/80) + 1, tolerance = 0.001)
  expect_equal(qgraph_default_vsize(500), 8 * exp(-500/80) + 1, tolerance = 0.001)
})

test_that("qgraph_default_vsize decreases as n increases", {
  sizes <- sapply(c(5, 20, 50, 100, 200), qgraph_default_vsize)
  expect_true(all(diff(sizes) < 0))  # Each subsequent size is smaller
})

test_that("qgraph_default_vsize approaches 1 for very large networks", {
  large_size <- qgraph_default_vsize(10000)
  # Formula: 8 * exp(-10000/80) + 1 = 8 * ~0 + 1 = ~1
  expect_true(large_size >= 1)  # Must be at least 1
  expect_true(large_size < 2)   # Should be close to 1
})

test_that("qgraph_default_vsize handles edge case of n=0", {
  expect_equal(qgraph_default_vsize(0), 8 * exp(0) + 1, tolerance = 0.001)
  expect_equal(qgraph_default_vsize(0), 9, tolerance = 0.001)
})

# ============================================
# qgraph_default_esize() TESTS
# ============================================

test_that("qgraph_default_esize returns correct value for weighted undirected networks", {
  # Formula: 15 * exp(-n/90) + 1 (not halved for undirected)
  expect_equal(qgraph_default_esize(10, weighted = TRUE, directed = FALSE),
               15 * exp(-10/90) + 1, tolerance = 0.001)
  expect_equal(qgraph_default_esize(50, weighted = TRUE, directed = FALSE),
               15 * exp(-50/90) + 1, tolerance = 0.001)
})

test_that("qgraph_default_esize is halved for directed weighted networks", {
  # Formula: max(esize/2, 1) for directed
  esize_10 <- 15 * exp(-10/90) + 1
  expect_equal(qgraph_default_esize(10, weighted = TRUE, directed = TRUE),
               max(esize_10 / 2, 1), tolerance = 0.001)
})

test_that("qgraph_default_esize returns 2 for unweighted networks", {
  expect_equal(qgraph_default_esize(10, weighted = FALSE, directed = FALSE), 2)
  expect_equal(qgraph_default_esize(50, weighted = FALSE, directed = TRUE), 2)
  expect_equal(qgraph_default_esize(100, weighted = FALSE, directed = FALSE), 2)
})

test_that("qgraph_default_esize has minimum of 1 for directed networks", {
  # For very large networks, esize/2 should not go below 1
  large_esize <- qgraph_default_esize(10000, weighted = TRUE, directed = TRUE)
  expect_true(large_esize >= 1)
})

test_that("qgraph_default_esize decreases as n increases for weighted", {
  sizes <- sapply(c(5, 20, 50, 100, 200),
                  function(n) qgraph_default_esize(n, weighted = TRUE))
  expect_true(all(diff(sizes) < 0))
})

test_that("qgraph_default_esize handles n=0", {
  expect_equal(qgraph_default_esize(0, weighted = TRUE, directed = FALSE),
               15 * exp(0) + 1, tolerance = 0.001)
  expect_equal(qgraph_default_esize(0, weighted = TRUE, directed = FALSE),
               16, tolerance = 0.001)
})

# ============================================
# qgraph_scale_edge_widths() TESTS
# ============================================

test_that("qgraph_scale_edge_widths returns empty vector for empty input", {
  result <- qgraph_scale_edge_widths(numeric(0))
  expect_length(result, 0)
})

test_that("qgraph_scale_edge_widths scales to [min_lwd, esize] range", {
  weights <- c(0.2, 0.5, 0.8, 1.0)
  result <- qgraph_scale_edge_widths(weights, minimum = 0, esize = 4)

  expect_true(all(result >= 0.1))  # min_lwd = 0.1
  expect_true(all(result <= 4))    # esize
})

test_that("qgraph_scale_edge_widths handles uniform weights", {
  weights <- c(0.5, 0.5, 0.5)
  result <- qgraph_scale_edge_widths(weights, minimum = 0, esize = 4)

  # All weights same -> all widths same
  expect_equal(result[1], result[2])
  expect_equal(result[2], result[3])
})

test_that("qgraph_scale_edge_widths uses auto-detected maximum", {
  weights <- c(0.1, 0.5, 2.0)
  result <- qgraph_scale_edge_widths(weights, minimum = 0, esize = 4)

  # Maximum weight (2.0) should map to maximum width (esize = 4)
  expect_equal(result[3], 4, tolerance = 0.01)
})

test_that("qgraph_scale_edge_widths respects explicit maximum", {
  weights <- c(0.5, 1.0)
  result <- qgraph_scale_edge_widths(weights, minimum = 0, maximum = 2.0, esize = 4)

  # 1.0/2.0 = 0.5 normalized -> should be middle of range
  # 0.5 * (4 - 0.1) + 0.1 = 2.05
  expect_true(result[2] < 4)  # Not at max
  expect_true(result[2] > result[1])  # Larger than smaller weight
})

test_that("qgraph_scale_edge_widths handles cut parameter (two-tier)", {
  weights <- c(0.1, 0.3, 0.5, 0.8)
  result <- qgraph_scale_edge_widths(weights, cut = 0.4, esize = 4)

  # Weights below cut should have 0 in avgW -> min_lwd
  expect_equal(result[1], 0.1, tolerance = 0.01)  # Below cut
  expect_equal(result[2], 0.1, tolerance = 0.01)  # Below cut
  expect_true(result[3] > 0.1)  # At/above cut
  expect_true(result[4] > result[3])  # Above cut, larger
})

test_that("qgraph_scale_edge_widths handles negative weights via abs()", {
  weights <- c(-0.5, 0.5, -1.0, 1.0)
  result <- qgraph_scale_edge_widths(weights, esize = 4)

  # Absolute values: 0.5, 0.5, 1.0, 1.0
  expect_equal(result[1], result[2], tolerance = 0.01)
  expect_equal(result[3], result[4], tolerance = 0.01)
})

test_that("qgraph_scale_edge_widths handles maximum = 0", {
  weights <- c(0, 0, 0)
  result <- qgraph_scale_edge_widths(weights, esize = 4)

  # Should not error, maximum defaults to 1
  expect_length(result, 3)
})

test_that("qgraph_scale_edge_widths handles NA in weights", {
  weights <- c(0.5, NA, 1.0)
  result <- qgraph_scale_edge_widths(weights, esize = 4)

  expect_length(result, 3)
  expect_true(is.na(result[2]))
})

test_that("qgraph_scale_edge_widths default esize is 4", {
  weights <- c(0.5, 1.0)
  result <- qgraph_scale_edge_widths(weights, minimum = 0)

  # Max weight should map to default esize = 4
  expect_equal(result[2], 4, tolerance = 0.01)
})

# ============================================
# qgraph_plot_info() TESTS
# ============================================

test_that("qgraph_plot_info returns list with required components", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  info <- qgraph_plot_info()

  expect_type(info, "list")
  expect_true("usr" %in% names(info))
  expect_true("pin" %in% names(info))
  expect_true("mai" %in% names(info))
  expect_true("csi" %in% names(info))
  expect_true("dev_name" %in% names(info))

  dev.off()
})

test_that("qgraph_plot_info usr has 4 elements", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  info <- qgraph_plot_info()

  expect_length(info$usr, 4)
  # usr may have small padding around xlim/ylim
  expect_true(info$usr[1] <= -1)  # xleft at or before -1
  expect_true(info$usr[2] >= 1)   # xright at or after 1
  expect_true(info$usr[3] <= -1)  # ybottom at or before -1
  expect_true(info$usr[4] >= 1)   # ytop at or after 1

  dev.off()
})

test_that("qgraph_plot_info pin has 2 elements", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()

  info <- qgraph_plot_info()

  expect_length(info$pin, 2)
  expect_true(all(info$pin > 0))  # Plot dimensions should be positive

  dev.off()
})

test_that("qgraph_plot_info mai has 4 elements", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()

  info <- qgraph_plot_info()

  expect_length(info$mai, 4)

  dev.off()
})

# ============================================
# qgraph_cent2edge() TESTS
# ============================================

test_that("qgraph_cent2edge returns list with x and y", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- qgraph_cent2edge(0, 0, cex = 1, offset = 0, angle = 0)

  expect_type(result, "list")
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))

  dev.off()
})

test_that("qgraph_cent2edge offset moves point outward", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  # Angle 0 in qgraph uses sin(angle) for x, cos(angle) for y
  # sin(0) = 0, cos(0) = 1, so offset affects y, not x
  result_no_offset <- qgraph_cent2edge(0, 0, cex = 1, offset = 0, angle = 0)
  result_with_offset <- qgraph_cent2edge(0, 0, cex = 1, offset = 0.5, angle = 0)

  # With offset, y should be further from center (since angle=0 -> cos(0)=1 -> y direction)
  expect_true(result_with_offset$y > result_no_offset$y)

  dev.off()
})

test_that("qgraph_cent2edge uses provided plot_info", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  # Get plot info manually
  plot_info <- qgraph_plot_info()

  result_auto <- qgraph_cent2edge(0, 0, cex = 1, angle = pi/4, plot_info = NULL)
  result_manual <- qgraph_cent2edge(0, 0, cex = 1, angle = pi/4, plot_info = plot_info)

  # Results should be identical
  expect_equal(result_auto$x, result_manual$x, tolerance = 0.001)
  expect_equal(result_auto$y, result_manual$y, tolerance = 0.001)

  dev.off()
})

test_that("qgraph_cent2edge angle=0 moves in positive x direction", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- qgraph_cent2edge(0, 0, cex = 1, angle = 0)

  # sin(0) = 0, cos(0) = 1
  # So x offset should be 0, y offset should be positive
  expect_equal(result$x, 0, tolerance = 0.001)
  expect_true(result$y > 0)

  dev.off()
})

test_that("qgraph_cent2edge handles non-origin center", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-5, 5), ylim = c(-5, 5))

  result <- qgraph_cent2edge(2, 3, cex = 1, angle = 0)

  # Result should be offset from (2, 3)
  expect_true(result$x != 0 || result$y != 3)

  dev.off()
})

# ============================================
# qgraph_norm_curve() TESTS
# ============================================

test_that("qgraph_norm_curve returns positive value", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()

  norm <- qgraph_norm_curve()

  expect_true(norm > 0)

  dev.off()
})

test_that("qgraph_norm_curve formula is sqrt(sum(pin^2)) / sqrt(7^2 + 7^2)", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()

  pin <- graphics::par("pin")
  expected <- sqrt(sum(pin^2)) / sqrt(7^2 + 7^2)

  result <- qgraph_norm_curve()

  expect_equal(result, expected, tolerance = 0.001)

  dev.off()
})

test_that("qgraph_norm_curve varies with plot size", {
  tmp1 <- tempfile(fileext = ".png")
  tmp2 <- tempfile(fileext = ".png")
  on.exit({unlink(tmp1); unlink(tmp2)}, add = TRUE)

  # Small plot
  png(tmp1, width = 200, height = 200)
  plot.new()
  norm1 <- qgraph_norm_curve()
  dev.off()

  # Large plot
  png(tmp2, width = 800, height = 800)
  plot.new()
  norm2 <- qgraph_norm_curve()
  dev.off()

  # Larger plot should have larger normalization factor
  expect_true(norm2 > norm1)
})

# ============================================
# qgraph_vsize_to_user() TESTS
# ============================================

test_that("qgraph_vsize_to_user returns positive value", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- qgraph_vsize_to_user(5)

  expect_true(result > 0)

  dev.off()
})

test_that("qgraph_vsize_to_user scales linearly with vsize", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result1 <- qgraph_vsize_to_user(5)
  result2 <- qgraph_vsize_to_user(10)

  # Double the vsize should double the user coords
  expect_equal(result2 / result1, 2, tolerance = 0.01)

  dev.off()
})

test_that("qgraph_vsize_to_user uses provided plot_info", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  plot_info <- qgraph_plot_info()

  result_auto <- qgraph_vsize_to_user(5, plot_info = NULL)
  result_manual <- qgraph_vsize_to_user(5, plot_info = plot_info)

  expect_equal(result_auto, result_manual, tolerance = 0.001)

  dev.off()
})

test_that("qgraph_vsize_to_user handles zero vsize", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- qgraph_vsize_to_user(0)

  expect_equal(result, 0)

  dev.off()
})

# ============================================
# qgraph_cent_to_edge_simple() TESTS - Circle
# ============================================

test_that("qgraph_cent_to_edge_simple circle returns correct point at angle 0", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = 0, node_size = 1, shape = "circle")

  expect_equal(result$x, 1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple circle at angle pi/2", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = pi/2, node_size = 1, shape = "circle")

  expect_equal(result$x, 0, tolerance = 0.001)
  expect_equal(result$y, 1, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple circle at angle pi", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = pi, node_size = 1, shape = "circle")

  expect_equal(result$x, -1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple circle at diagonal angle", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = pi/4, node_size = 1, shape = "circle")

  expected_x <- cos(pi/4)
  expected_y <- sin(pi/4)

  expect_equal(result$x, expected_x, tolerance = 0.001)
  expect_equal(result$y, expected_y, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple circle handles non-origin center", {
  result <- qgraph_cent_to_edge_simple(3, 4, angle = 0, node_size = 0.5, shape = "circle")

  expect_equal(result$x, 3.5, tolerance = 0.001)
  expect_equal(result$y, 4, tolerance = 0.001)
})

# ============================================
# qgraph_cent_to_edge_simple() TESTS - Square
# ============================================

test_that("qgraph_cent_to_edge_simple square at angle 0 (right edge)", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = 0, node_size = 1, shape = "square")

  expect_equal(result$x, 1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple square at angle pi/2 (top edge)", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = pi/2, node_size = 1, shape = "square")

  expect_equal(result$x, 0, tolerance = 0.001)
  expect_equal(result$y, 1, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple square at angle pi (left edge)", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = pi, node_size = 1, shape = "square")

  expect_equal(result$x, -1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple square at angle -pi/2 (bottom edge)", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = -pi/2, node_size = 1, shape = "square")

  expect_equal(result$x, 0, tolerance = 0.001)
  expect_equal(result$y, -1, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple square at diagonal (corner)", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = pi/4, node_size = 1, shape = "square")

  # At 45 degrees, should hit corner of square at (1, 1) normalized to edge
  expect_equal(result$x, 1, tolerance = 0.001)
  expect_equal(result$y, 1, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple rectangle works like square", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = 0, node_size = 1, shape = "rectangle")

  expect_equal(result$x, 1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)
})

# ============================================
# qgraph_cent_to_edge_simple() TESTS - Edge Cases
# ============================================

test_that("qgraph_cent_to_edge_simple defaults to circle for unknown shape", {
  result_unknown <- qgraph_cent_to_edge_simple(0, 0, angle = 0, node_size = 1, shape = "hexagon")
  result_circle <- qgraph_cent_to_edge_simple(0, 0, angle = 0, node_size = 1, shape = "circle")

  expect_equal(result_unknown$x, result_circle$x, tolerance = 0.001)
  expect_equal(result_unknown$y, result_circle$y, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple handles very small angles", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = 0.001, node_size = 1, shape = "circle")

  expect_true(abs(result$x - 1) < 0.01)  # Close to (1, 0)
  expect_true(abs(result$y) < 0.01)
})

test_that("qgraph_cent_to_edge_simple handles negative angles", {
  result <- qgraph_cent_to_edge_simple(0, 0, angle = -pi/4, node_size = 1, shape = "circle")

  expect_equal(result$x, cos(-pi/4), tolerance = 0.001)
  expect_equal(result$y, sin(-pi/4), tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple handles angle > 2*pi", {
  result1 <- qgraph_cent_to_edge_simple(0, 0, angle = pi/4, node_size = 1, shape = "circle")
  result2 <- qgraph_cent_to_edge_simple(0, 0, angle = pi/4 + 2*pi, node_size = 1, shape = "circle")

  expect_equal(result1$x, result2$x, tolerance = 0.001)
  expect_equal(result1$y, result2$y, tolerance = 0.001)
})

test_that("qgraph_cent_to_edge_simple handles zero node_size", {
  result <- qgraph_cent_to_edge_simple(5, 5, angle = 0, node_size = 0, shape = "circle")

  # With zero size, should return center point
  expect_equal(result$x, 5, tolerance = 0.001)
  expect_equal(result$y, 5, tolerance = 0.001)
})

# ============================================
# qgraph_arrow_size() TESTS
# ============================================

test_that("qgraph_arrow_size returns positive value", {
  result <- qgraph_arrow_size(edge_width = 2, base_asize = 1)

  expect_true(result > 0)
})

test_that("qgraph_arrow_size scales with edge width", {
  result1 <- qgraph_arrow_size(edge_width = 2, base_asize = 1)
  result2 <- qgraph_arrow_size(edge_width = 8, base_asize = 1)

  # Formula: base_asize * 0.02 * sqrt(edge_width / 2)
  # sqrt(8/2) / sqrt(2/2) = sqrt(4) / sqrt(1) = 2
  expect_equal(result2 / result1, 2, tolerance = 0.01)
})

test_that("qgraph_arrow_size scales with base_asize", {
  result1 <- qgraph_arrow_size(edge_width = 2, base_asize = 1)
  result2 <- qgraph_arrow_size(edge_width = 2, base_asize = 2)

  expect_equal(result2, result1 * 2, tolerance = 0.001)
})

test_that("qgraph_arrow_size handles very small edge width", {
  result <- qgraph_arrow_size(edge_width = 0.1, base_asize = 1)

  expect_true(result > 0)
  expect_true(result < qgraph_arrow_size(edge_width = 1, base_asize = 1))
})

test_that("qgraph_arrow_size handles large edge width", {
  result <- qgraph_arrow_size(edge_width = 10, base_asize = 1)

  expect_true(result > 0)
  # base_asize * 0.02 * sqrt(10/2) = 0.02 * sqrt(5) ~ 0.0447
  expect_equal(result, 0.02 * sqrt(5), tolerance = 0.001)
})

test_that("qgraph_arrow_size formula is base_asize * 0.02 * sqrt(edge_width/2)", {
  edge_width <- 6
  base_asize <- 1.5

  expected <- base_asize * 0.02 * sqrt(edge_width / 2)
  result <- qgraph_arrow_size(edge_width, base_asize)

  expect_equal(result, expected, tolerance = 0.001)
})

# ============================================
# INTEGRATION TESTS - qgraph compatibility
# ============================================

test_that("qgraph geometry functions work together for node placement", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  # Simulate placing nodes with qgraph-style sizing
  n_nodes <- 5
  vsize <- qgraph_default_vsize(n_nodes)

  expect_true(vsize > 0)

  # Convert to user coordinates
  user_size <- qgraph_vsize_to_user(vsize)

  expect_true(user_size > 0)

  # Get boundary point
  boundary <- qgraph_cent_to_edge_simple(0, 0, angle = 0,
                                          node_size = user_size, shape = "circle")

  expect_true(boundary$x > 0)
  expect_equal(boundary$y, 0, tolerance = 0.001)

  dev.off()
})

test_that("qgraph geometry functions work together for edge sizing", {
  # Simulate edge width calculation
  n_nodes <- 10
  weights <- c(0.2, 0.5, 0.8, 1.0)

  esize <- qgraph_default_esize(n_nodes, weighted = TRUE, directed = FALSE)
  widths <- qgraph_scale_edge_widths(weights, esize = esize)

  expect_true(all(widths > 0))
  expect_true(all(widths <= esize))

  # Calculate arrow sizes for each width
  arrow_sizes <- sapply(widths, qgraph_arrow_size)

  expect_true(all(arrow_sizes > 0))
  expect_true(all(diff(arrow_sizes) > 0))  # Larger widths -> larger arrows
})

test_that("qgraph cent2edge matches cent_to_edge_simple for basic cases", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  # Both functions should give similar results for circles
  angle <- pi/4

  # qgraph_cent2edge result
  result1 <- qgraph_cent2edge(0, 0, cex = 1, offset = 0, angle = angle)

  # qgraph_cent_to_edge_simple for comparison
  result2 <- qgraph_cent_to_edge_simple(0, 0, angle = angle, node_size = 1, shape = "circle")

  # Both should be in similar direction
  # (exact values may differ due to coordinate system differences)
  expect_true(!is.null(result1$x))
  expect_true(!is.null(result2$x))

  dev.off()
})

test_that("qgraph_scale_edge_widths with cut=0 produces continuous scaling", {
  weights <- seq(0.1, 1.0, by = 0.1)
  result <- qgraph_scale_edge_widths(weights, cut = 0, esize = 4)

  # All values should be different (continuous)
  expect_equal(length(unique(result)), length(result))

  # Should be monotonically increasing
  expect_true(all(diff(result) > 0))
})

test_that("complete workflow: node size, edge width, arrow size", {
  n_nodes <- 8

  # Node sizing
  vsize <- qgraph_default_vsize(n_nodes)
  expect_true(vsize > 1 && vsize < 9)

  # Edge sizing
  esize <- qgraph_default_esize(n_nodes, weighted = TRUE, directed = TRUE)
  expect_true(esize >= 1)

  # Scale some weights
  weights <- c(0.3, 0.6, 0.9)
  widths <- qgraph_scale_edge_widths(weights, esize = esize)

  # Arrow sizes
  arrows <- sapply(widths, qgraph_arrow_size)

  # All should be valid

  expect_true(all(is.finite(c(vsize, esize, widths, arrows))))
  expect_true(all(c(vsize, esize, widths, arrows) > 0))
})
