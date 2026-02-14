# test-coverage-splot-geometry-40.R - Comprehensive tests for splot-geometry.R
# Tests for coordinate transformations and geometry functions

# Make internal geometry functions available
usr_to_in_x <- cograph:::usr_to_in_x
usr_to_in_y <- cograph:::usr_to_in_y
in_to_usr_x <- cograph:::in_to_usr_x
in_to_usr_y <- cograph:::in_to_usr_y
get_x_scale <- cograph:::get_x_scale
get_y_scale <- cograph:::get_y_scale
atan2_usr <- cograph:::atan2_usr
cent_to_edge <- cograph:::cent_to_edge
perp_mid <- cograph:::perp_mid
splot_distance <- cograph:::splot_distance
splot_angle <- cograph:::splot_angle
rescale_layout <- cograph:::rescale_layout

# ============================================
# splot_distance() TESTS
# ============================================

test_that("splot_distance returns zero for same point", {
  expect_equal(splot_distance(0, 0, 0, 0), 0)
  expect_equal(splot_distance(5, 5, 5, 5), 0)
  expect_equal(splot_distance(-3, 7, -3, 7), 0)
})

test_that("splot_distance calculates horizontal distance correctly", {
  expect_equal(splot_distance(0, 0, 3, 0), 3)
  expect_equal(splot_distance(0, 0, -4, 0), 4)
  expect_equal(splot_distance(2, 5, 7, 5), 5)
})

test_that("splot_distance calculates vertical distance correctly", {
  expect_equal(splot_distance(0, 0, 0, 4), 4)
  expect_equal(splot_distance(0, 0, 0, -5), 5)
  expect_equal(splot_distance(3, 2, 3, 8), 6)
})

test_that("splot_distance calculates diagonal distance correctly", {
  # 3-4-5 triangle

expect_equal(splot_distance(0, 0, 3, 4), 5)
  # 5-12-13 triangle
  expect_equal(splot_distance(0, 0, 5, 12), 13)
  # Unit diagonal
  expect_equal(splot_distance(0, 0, 1, 1), sqrt(2))
})

test_that("splot_distance is symmetric", {
  expect_equal(splot_distance(1, 2, 5, 8), splot_distance(5, 8, 1, 2))
  expect_equal(splot_distance(-3, 4, 7, -2), splot_distance(7, -2, -3, 4))
})

test_that("splot_distance handles negative coordinates", {
  expect_equal(splot_distance(-1, -1, -4, -5), 5)
  expect_equal(splot_distance(-2, -3, 1, 1), 5)
})

# ============================================
# splot_angle() TESTS
# ============================================

test_that("splot_angle returns correct angle for cardinal directions", {
  # Right (0 degrees = 0 radians)
  expect_equal(splot_angle(0, 0, 1, 0), 0)

  # Up (90 degrees = pi/2 radians)
  expect_equal(splot_angle(0, 0, 0, 1), pi/2)

  # Left (180 degrees = pi radians)
  expect_equal(splot_angle(0, 0, -1, 0), pi)

  # Down (-90 degrees = -pi/2 radians)
  expect_equal(splot_angle(0, 0, 0, -1), -pi/2)
})

test_that("splot_angle returns correct angle for diagonals", {
  # Upper right (45 degrees)
  expect_equal(splot_angle(0, 0, 1, 1), pi/4)

  # Upper left (135 degrees)
  expect_equal(splot_angle(0, 0, -1, 1), 3*pi/4)

  # Lower right (-45 degrees)
  expect_equal(splot_angle(0, 0, 1, -1), -pi/4)

  # Lower left (-135 degrees)
  expect_equal(splot_angle(0, 0, -1, -1), -3*pi/4)
})

test_that("splot_angle handles non-origin start points", {
  expect_equal(splot_angle(2, 3, 4, 3), 0)  # Horizontal right
  expect_equal(splot_angle(2, 3, 2, 5), pi/2)  # Vertical up
})

test_that("splot_angle returns 0 for same point", {
  expect_equal(splot_angle(0, 0, 0, 0), 0)
  expect_equal(splot_angle(5, 5, 5, 5), 0)
})

# ============================================
# rescale_layout() TESTS
# ============================================

test_that("rescale_layout scales layout to [-1, 1] range", {
  layout <- data.frame(x = c(0, 10, 20), y = c(0, 10, 20))
  result <- rescale_layout(layout, mar = 0)

  # Check range is within [-1, 1]
  expect_true(all(result[[1]] >= -1 & result[[1]] <= 1))
  expect_true(all(result[[2]] >= -1 & result[[2]] <= 1))
})

test_that("rescale_layout handles margin parameter", {
  layout <- data.frame(x = c(0, 10), y = c(0, 10))
  result <- rescale_layout(layout, mar = 0.1)

  # With margin, range should be smaller
  target <- 1 - 0.1
  expect_true(all(abs(result[[1]]) <= target))
  expect_true(all(abs(result[[2]]) <= target))
})

test_that("rescale_layout preserves aspect ratio", {
  layout <- data.frame(x = c(0, 20), y = c(0, 10))
  result <- rescale_layout(layout, mar = 0)

  # Original aspect ratio: 20:10 = 2:1
  x_range <- diff(range(result[[1]]))
  y_range <- diff(range(result[[2]]))

  # Should preserve ratio
  expect_equal(x_range / y_range, 2, tolerance = 0.01)
})

test_that("rescale_layout handles constant x values", {
  layout <- data.frame(x = c(5, 5, 5), y = c(0, 5, 10))
  result <- rescale_layout(layout, mar = 0)

  # Should not error with constant x
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("rescale_layout handles constant y values", {
  layout <- data.frame(x = c(0, 5, 10), y = c(3, 3, 3))
  result <- rescale_layout(layout, mar = 0)

  # Should not error with constant y
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("rescale_layout handles matrix input", {
  layout <- matrix(c(0, 5, 10, 0, 5, 10), ncol = 2)
  result <- rescale_layout(layout, mar = 0)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("rescale_layout errors on insufficient columns", {
  layout <- data.frame(x = c(1, 2, 3))
  expect_error(rescale_layout(layout), "at least 2 columns")
})

test_that("rescale_layout handles negative coordinates", {
  layout <- data.frame(x = c(-10, 0, 10), y = c(-5, 0, 5))
  result <- rescale_layout(layout, mar = 0)

  expect_true(all(result[[1]] >= -1 & result[[1]] <= 1))
  expect_true(all(result[[2]] >= -1 & result[[2]] <= 1))
})

test_that("rescale_layout centers the result", {
  layout <- data.frame(x = c(100, 110, 120), y = c(100, 110, 120))
  result <- rescale_layout(layout, mar = 0)

  # Center should be at origin
  expect_equal(mean(result[[1]]), 0, tolerance = 0.01)
  expect_equal(mean(result[[2]]), 0, tolerance = 0.01)
})

# ============================================
# COORDINATE TRANSFORMATION TESTS (within graphics context)
# ============================================

test_that("usr_to_in_x and in_to_usr_x are inverse operations", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  # Test round-trip conversion
  test_vals <- c(0, 2.5, 5, 7.5, 10)
  for (val in test_vals) {
    inch_val <- usr_to_in_x(val)
    back_to_usr <- in_to_usr_x(inch_val)
    expect_equal(back_to_usr, val, tolerance = 0.001,
                 info = paste("Value:", val))
  }

  dev.off()
})

test_that("usr_to_in_y and in_to_usr_y are inverse operations", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  # Test round-trip conversion
  test_vals <- c(0, 2.5, 5, 7.5, 10)
  for (val in test_vals) {
    inch_val <- usr_to_in_y(val)
    back_to_usr <- in_to_usr_y(inch_val)
    expect_equal(back_to_usr, val, tolerance = 0.001,
                 info = paste("Value:", val))
  }

  dev.off()
})

test_that("get_x_scale returns positive value", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  scale <- get_x_scale()
  expect_true(scale > 0)

  dev.off()
})

test_that("get_y_scale returns positive value", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  scale <- get_y_scale()
  expect_true(scale > 0)

  dev.off()
})

test_that("get_x_scale and get_y_scale are equal for square plot", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  par(mar = c(0, 0, 0, 0))  # Remove margins for square plot area
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  x_scale <- get_x_scale()
  y_scale <- get_y_scale()

  # Should be approximately equal for square plot with equal ranges
  expect_equal(x_scale, y_scale, tolerance = 0.1)

  dev.off()
})

# ============================================
# atan2_usr() TESTS
# ============================================

test_that("atan2_usr returns angle in expected range", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  # Test various angles
  angles <- c(
    atan2_usr(1, 1),   # Upper right
    atan2_usr(1, -1),  # Upper left
    atan2_usr(-1, 1),  # Lower right
    atan2_usr(-1, -1)  # Lower left
  )

  # All angles should be in [-pi, pi]
  expect_true(all(angles >= -pi & angles <= pi))

  dev.off()
})

test_that("atan2_usr handles zero values", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  # Horizontal
  expect_equal(atan2_usr(0, 1), 0)
  expect_equal(atan2_usr(0, -1), pi)

  dev.off()
})

# ============================================
# cent_to_edge() TESTS - Circle shape
# ============================================

test_that("cent_to_edge returns correct point for circle at angle 0", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- cent_to_edge(0, 0, 0, 1, shape = "circle")

  # At angle 0 (right), x should increase by cex
  expect_equal(result$x, 1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)

  dev.off()
})

test_that("cent_to_edge returns correct point for circle at angle pi/2", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- cent_to_edge(0, 0, pi/2, 1, shape = "circle")

  # At angle pi/2 (up), y should increase by cex
  expect_equal(result$x, 0, tolerance = 0.001)
  expect_equal(result$y, 1, tolerance = 0.001)

  dev.off()
})

test_that("cent_to_edge handles non-origin center for circle", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-5, 5), ylim = c(-5, 5))

  result <- cent_to_edge(3, 4, 0, 0.5, shape = "circle")

  expect_equal(result$x, 3.5, tolerance = 0.001)
  expect_equal(result$y, 4, tolerance = 0.001)

  dev.off()
})

# ============================================
# cent_to_edge() TESTS - Square shape
# ============================================

test_that("cent_to_edge returns correct point for square at angle 0", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- cent_to_edge(0, 0, 0, 1, shape = "square")

  # At angle 0 (right), x should be at the right edge
  expect_equal(result$x, 1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)

  dev.off()
})

test_that("cent_to_edge returns correct point for square at angle pi/2", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- cent_to_edge(0, 0, pi/2, 1, shape = "square")

  # At angle pi/2 (up), y should be at the top edge
  expect_equal(result$x, 0, tolerance = 0.001)
  expect_equal(result$y, 1, tolerance = 0.001)

  dev.off()
})

test_that("cent_to_edge handles rectangle shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- cent_to_edge(0, 0, 0, 2, cex2 = 1, shape = "rectangle")

  # Should hit right edge at x = 2
  expect_equal(result$x, 2, tolerance = 0.001)

  dev.off()
})

# ============================================
# cent_to_edge() TESTS - Ellipse shape
# ============================================

test_that("cent_to_edge returns correct point for ellipse", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-3, 3), ylim = c(-2, 2))

  # Ellipse with a=2 (horizontal), b=1 (vertical)
  result <- cent_to_edge(0, 0, 0, 2, cex2 = 1, shape = "ellipse")

  # At angle 0, point should be at (2, 0)
  expect_equal(result$x, 2, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)

  dev.off()
})

test_that("cent_to_edge ellipse at angle pi/2", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-3, 3), ylim = c(-2, 2))

  result <- cent_to_edge(0, 0, pi/2, 2, cex2 = 1, shape = "ellipse")

  # At angle pi/2, point should be at (0, 1)
  expect_equal(result$x, 0, tolerance = 0.001)
  expect_equal(result$y, 1, tolerance = 0.001)

  dev.off()
})

# ============================================
# cent_to_edge() TESTS - Edge cases
# ============================================

test_that("cent_to_edge handles empty inputs", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- cent_to_edge(numeric(0), numeric(0), numeric(0), numeric(0))

  expect_equal(length(result$x), 0)
  expect_equal(length(result$y), 0)

  dev.off()
})

test_that("cent_to_edge handles NA inputs", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- cent_to_edge(NA, NA, NA, NA)

  expect_true(is.na(result$x))
  expect_true(is.na(result$y))

  dev.off()
})

test_that("cent_to_edge handles NA shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  # Should default to circle
  result <- cent_to_edge(0, 0, 0, 1, shape = NA)

  expect_equal(result$x, 1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)

  dev.off()
})

test_that("cent_to_edge handles unknown shape", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  # Unknown shape should default to circle behavior
  result <- cent_to_edge(0, 0, 0, 1, shape = "unknown_shape")

  expect_equal(result$x, 1, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)

  dev.off()
})

# ============================================
# perp_mid() TESTS
# ============================================

test_that("perp_mid returns midpoint when cex is 0", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  result <- perp_mid(0, 0, 10, 0, cex = 0, q = 0.5)

  expect_equal(result$x, 5, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)

  dev.off()
})

test_that("perp_mid returns point at q=0 (start)", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  result <- perp_mid(0, 0, 10, 0, cex = 0, q = 0)

  expect_equal(result$x, 0, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)

  dev.off()
})

test_that("perp_mid returns point at q=1 (end)", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  result <- perp_mid(0, 0, 10, 0, cex = 0, q = 1)

  expect_equal(result$x, 10, tolerance = 0.001)
  expect_equal(result$y, 0, tolerance = 0.001)

  dev.off()
})

test_that("perp_mid offsets perpendicular to horizontal line", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(-5, 5))

  result <- perp_mid(0, 0, 10, 0, cex = 0.5, q = 0.5)

  # Midpoint is at (5, 0), perpendicular offset should be in y direction
  expect_equal(result$x, 5, tolerance = 0.001)
  expect_true(abs(result$y) > 0)  # y should be offset

  dev.off()
})

test_that("perp_mid offsets perpendicular to vertical line", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-5, 5), ylim = c(0, 10))

  result <- perp_mid(0, 0, 0, 10, cex = 0.5, q = 0.5)

  # Midpoint is at (0, 5), perpendicular offset should be in x direction
  expect_true(abs(result$x) > 0)  # x should be offset
  expect_equal(result$y, 5, tolerance = 0.001)

  dev.off()
})

test_that("perp_mid handles zero-length edge", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))

  # Same start and end point
  result <- perp_mid(5, 5, 5, 5, cex = 0.5, q = 0.5)

  # Should return the point without error
  expect_equal(result$x, 5, tolerance = 0.001)
  expect_equal(result$y, 5, tolerance = 0.001)

  dev.off()
})

test_that("perp_mid handles negative curvature", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(-5, 5))

  result_pos <- perp_mid(0, 0, 10, 0, cex = 0.5, q = 0.5)
  result_neg <- perp_mid(0, 0, 10, 0, cex = -0.5, q = 0.5)

  # Negative curvature should offset in opposite direction
  expect_equal(result_pos$y, -result_neg$y, tolerance = 0.001)

  dev.off()
})

# ============================================
# INTEGRATION TESTS - Geometry in splot context
# ============================================

test_that("geometry functions work together for edge drawing", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  # Simulate drawing an edge from node A to node B
  nodeA <- c(-1, 0)
  nodeB <- c(1, 0)
  node_radius <- 0.2

  # Calculate angle from A to B
  angle_AB <- splot_angle(nodeA[1], nodeA[2], nodeB[1], nodeB[2])
  expect_equal(angle_AB, 0)  # Horizontal to the right

  # Calculate distance
  dist_AB <- splot_distance(nodeA[1], nodeA[2], nodeB[1], nodeB[2])
  expect_equal(dist_AB, 2)

  # Get edge start point (on boundary of node A facing B)
  start <- cent_to_edge(nodeA[1], nodeA[2], angle_AB, node_radius, shape = "circle")
  expect_equal(start$x, nodeA[1] + node_radius, tolerance = 0.001)

  # Get edge end point (on boundary of node B facing A)
  angle_BA <- splot_angle(nodeB[1], nodeB[2], nodeA[1], nodeA[2])
  end <- cent_to_edge(nodeB[1], nodeB[2], angle_BA, node_radius, shape = "circle")
  expect_equal(end$x, nodeB[1] - node_radius, tolerance = 0.001)

  # Get curved midpoint
  mid <- perp_mid(start$x, start$y, end$x, end$y, cex = 0.2, q = 0.5)
  expect_true(!is.null(mid$x) && !is.null(mid$y))

  dev.off()
})

test_that("rescale_layout produces valid splot layout", {
  # Create a random layout
  set.seed(42)
  n <- 10
  layout <- data.frame(
    x = rnorm(n, 100, 20),
    y = rnorm(n, 100, 20)
  )

  # Rescale
  scaled <- rescale_layout(layout, mar = 0.1)

  # Verify it's suitable for splot
  expect_true(all(scaled[[1]] >= -1 & scaled[[1]] <= 1))
  expect_true(all(scaled[[2]] >= -1 & scaled[[2]] <= 1))
  expect_equal(nrow(scaled), n)
})

test_that("coordinate transformations preserve relative positions", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(0, 100), ylim = c(0, 100))

  # Test points
  p1_usr <- c(10, 20)
  p2_usr <- c(50, 80)

  # Convert to inches
  p1_in <- c(usr_to_in_x(p1_usr[1]), usr_to_in_y(p1_usr[2]))
  p2_in <- c(usr_to_in_x(p2_usr[1]), usr_to_in_y(p2_usr[2]))

  # Convert back
  p1_back <- c(in_to_usr_x(p1_in[1]), in_to_usr_y(p1_in[2]))
  p2_back <- c(in_to_usr_x(p2_in[1]), in_to_usr_y(p2_in[2]))

  expect_equal(p1_back, p1_usr, tolerance = 0.001)
  expect_equal(p2_back, p2_usr, tolerance = 0.001)

  dev.off()
})

# ============================================
# EDGE CASE TESTS
# ============================================

test_that("splot_distance handles very small distances", {
  expect_equal(splot_distance(0, 0, 1e-10, 0), 1e-10, tolerance = 1e-15)
})

test_that("splot_distance handles very large distances", {
  expect_equal(splot_distance(0, 0, 1e6, 0), 1e6)
})

test_that("splot_angle handles very small differences", {
  angle <- splot_angle(0, 0, 1e-10, 1e-10)
  expect_equal(angle, pi/4, tolerance = 0.001)
})

test_that("rescale_layout handles single point", {
  layout <- data.frame(x = 5, y = 5)
  result <- rescale_layout(layout)

  # Single point should be centered at origin
  expect_equal(result[[1]], 0, tolerance = 0.001)
  expect_equal(result[[2]], 0, tolerance = 0.001)
})

test_that("rescale_layout handles two points", {
  layout <- data.frame(x = c(0, 10), y = c(0, 10))
  result <- rescale_layout(layout, mar = 0)

  expect_equal(nrow(result), 2)
  # Points should span the range
  expect_true(diff(range(result[[1]])) > 0)
})
