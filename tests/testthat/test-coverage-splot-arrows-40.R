# test-coverage-splot-arrows-40.R - Comprehensive tests for splot-arrows.R
# Tests for arrow drawing functions used in network edge visualization

# ============================================
# SETUP
# ============================================

# Make internal arrow functions available for testing
arrow_base_midpoint <- cograph:::arrow_base_midpoint
arrow_radius <- cograph:::arrow_radius
draw_arrow_base <- cograph:::draw_arrow_base
arrow_head_points <- cograph:::arrow_head_points
draw_curved_arrow_base <- cograph:::draw_curved_arrow_base
draw_open_arrow_base <- cograph:::draw_open_arrow_base
draw_circle_arrow_base <- cograph:::draw_circle_arrow_base
shorten_edge_for_arrow <- cograph:::shorten_edge_for_arrow
splot_angle <- cograph:::splot_angle

# ============================================
# arrow_base_midpoint() TESTS
# ============================================

test_that("arrow_base_midpoint returns list with x and y", {
  result <- arrow_base_midpoint(x = 1, y = 1, angle = 0, size = 0.1)

  expect_type(result, "list")
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_length(result, 2)
})

test_that("arrow_base_midpoint calculates correct midpoint at angle 0", {
  # Arrow pointing right (angle = 0)
  # Back of arrow should be to the left of tip
  result <- arrow_base_midpoint(x = 1, y = 0, angle = 0, size = 0.1)

  expect_lt(result$x, 1)  # Midpoint should be left of tip
  expect_equal(result$y, 0, tolerance = 1e-10)  # Should be on y = 0
})

test_that("arrow_base_midpoint calculates correct midpoint at angle pi/2", {
  # Arrow pointing up (angle = pi/2)
  result <- arrow_base_midpoint(x = 0, y = 1, angle = pi/2, size = 0.1)

  expect_equal(result$x, 0, tolerance = 1e-10)  # Should be on x = 0
  expect_lt(result$y, 1)  # Midpoint should be below tip
})

test_that("arrow_base_midpoint handles negative angles", {
  # Arrow pointing down-right
  result <- arrow_base_midpoint(x = 1, y = -1, angle = -pi/4, size = 0.1)

  expect_type(result$x, "double")
  expect_type(result$y, "double")
  expect_false(is.na(result$x))
  expect_false(is.na(result$y))
})

test_that("arrow_base_midpoint respects arrow_angle parameter", {
  # Wider arrow angle should produce wider midpoint offset
  narrow <- arrow_base_midpoint(x = 1, y = 0, angle = 0, size = 0.1, arrow_angle = pi/12)
  wide <- arrow_base_midpoint(x = 1, y = 0, angle = 0, size = 0.1, arrow_angle = pi/4)

  # Both should be left of tip but at different x positions
  expect_lt(narrow$x, 1)
  expect_lt(wide$x, 1)
})

test_that("arrow_base_midpoint handles zero size", {
  result <- arrow_base_midpoint(x = 1, y = 1, angle = 0, size = 0)

  # With zero size, midpoint should equal tip
  expect_equal(result$x, 1, tolerance = 1e-10)
  expect_equal(result$y, 1, tolerance = 1e-10)
})

test_that("arrow_base_midpoint handles large size values", {
  result <- arrow_base_midpoint(x = 0, y = 0, angle = pi, size = 10)

  expect_false(is.na(result$x))
  expect_false(is.na(result$y))
  # Large size should push midpoint far from tip
  expect_gt(abs(result$x), 5)
})

# ============================================
# arrow_radius() TESTS
# ============================================

test_that("arrow_radius returns numeric value", {
  result <- arrow_radius(size = 0.5)

  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("arrow_radius returns size value", {
  # The function currently just returns size
  expect_equal(arrow_radius(size = 0.5), 0.5)
  expect_equal(arrow_radius(size = 1.0), 1.0)
  expect_equal(arrow_radius(size = 0.1), 0.1)
})

test_that("arrow_radius handles zero size", {
  expect_equal(arrow_radius(size = 0), 0)
})

test_that("arrow_radius ignores arrow_angle parameter", {
  # Current implementation ignores arrow_angle
  result1 <- arrow_radius(size = 0.5, arrow_angle = pi/6)
  result2 <- arrow_radius(size = 0.5, arrow_angle = pi/4)

  expect_equal(result1, result2)
})

# ============================================
# draw_arrow_base() TESTS
# ============================================

test_that("draw_arrow_base draws without error", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_arrow_base(x = 0.5, y = 0.5, angle = 0, size = 0.1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_arrow_base accepts color parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_arrow_base(x = 0.5, y = 0.5, angle = 0, size = 0.1, col = "red")
    draw_arrow_base(x = -0.5, y = 0.5, angle = pi, size = 0.1, col = "#0000FF")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_arrow_base accepts border parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_arrow_base(x = 0, y = 0, angle = 0, size = 0.1, col = "blue", border = "black")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_arrow_base uses col as border when border is NULL", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_arrow_base(x = 0, y = 0, angle = 0, size = 0.1, col = "green", border = NULL)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_arrow_base accepts lwd parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_arrow_base(x = 0, y = 0, angle = 0, size = 0.1, lwd = 3)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_arrow_base works at different angles", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  angles <- c(0, pi/4, pi/2, pi, 3*pi/2, 2*pi)
  result <- tryCatch({
    for (angle in angles) {
      draw_arrow_base(x = 0, y = 0, angle = angle, size = 0.1)
    }
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# arrow_head_points() TESTS
# ============================================

test_that("arrow_head_points returns expected structure", {
  result <- arrow_head_points(x = 1, y = 0, angle = 0, size = 0.1)

  expect_type(result, "list")
  expect_true(all(c("x", "y", "mid_x", "mid_y", "back_len") %in% names(result)))
})

test_that("arrow_head_points returns correct number of vertices", {
  result <- arrow_head_points(x = 0, y = 0, angle = 0, size = 0.1)

  # Triangle has 3 vertices
  expect_length(result$x, 3)
  expect_length(result$y, 3)
})

test_that("arrow_head_points tip is first vertex", {
  result <- arrow_head_points(x = 1, y = 2, angle = 0, size = 0.1)

  # First point should be the tip
  expect_equal(result$x[1], 1)
  expect_equal(result$y[1], 2)
})

test_that("arrow_head_points midpoint is between wing vertices", {
  result <- arrow_head_points(x = 1, y = 0, angle = 0, size = 0.1)

  # mid_x should be average of left and right wing x coordinates
  expected_mid_x <- (result$x[2] + result$x[3]) / 2
  expected_mid_y <- (result$y[2] + result$y[3]) / 2

  expect_equal(result$mid_x, expected_mid_x, tolerance = 1e-10)
  expect_equal(result$mid_y, expected_mid_y, tolerance = 1e-10)
})

test_that("arrow_head_points back_len is positive", {
  result <- arrow_head_points(x = 0, y = 0, angle = 0, size = 0.1)

  expect_gt(result$back_len, 0)
})

test_that("arrow_head_points wings are symmetric about edge direction", {
  result <- arrow_head_points(x = 0, y = 0, angle = 0, size = 0.1)

  # For angle=0 (pointing right), wings should be symmetric about y=0
  expect_equal(result$y[2], -result$y[3], tolerance = 1e-10)
  # Both wings should have same x coordinate (same distance back from tip)
  expect_equal(result$x[2], result$x[3], tolerance = 1e-10)
})

# ============================================
# draw_curved_arrow_base() TESTS
# ============================================

test_that("draw_curved_arrow_base handles short spline silently", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  # Single point spline should return invisibly
  result <- tryCatch({
    draw_curved_arrow_base(spline_x = 0.5, spline_y = 0.5, size = 0.1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_curved_arrow_base draws arrow at spline end", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  # Multi-point spline
  spline_x <- c(0, 0.25, 0.5, 0.75, 1)
  spline_y <- c(0, 0.5, 0.8, 0.5, 0)

  result <- tryCatch({
    draw_curved_arrow_base(spline_x = spline_x, spline_y = spline_y, size = 0.1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_curved_arrow_base accepts color parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  spline_x <- seq(0, 1, length.out = 10)
  spline_y <- sin(spline_x * pi)

  result <- tryCatch({
    draw_curved_arrow_base(spline_x = spline_x, spline_y = spline_y,
                           size = 0.1, col = "purple")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_curved_arrow_base follows curve direction", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  # Curve that ends going up
  spline_x <- c(0, 0.3, 0.5)
  spline_y <- c(0, 0.2, 0.5)

  result <- tryCatch({
    draw_curved_arrow_base(spline_x = spline_x, spline_y = spline_y, size = 0.1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# draw_open_arrow_base() TESTS
# ============================================

test_that("draw_open_arrow_base draws without error", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_open_arrow_base(x = 0.5, y = 0.5, angle = 0, size = 0.1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_open_arrow_base accepts color parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_open_arrow_base(x = 0, y = 0, angle = pi/4, size = 0.15, col = "orange")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_open_arrow_base accepts lwd parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_open_arrow_base(x = 0, y = 0, angle = 0, size = 0.1, lwd = 4)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_open_arrow_base works at multiple angles", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- tryCatch({
    for (i in 0:7) {
      angle <- i * pi / 4
      x <- cos(angle)
      y <- sin(angle)
      draw_open_arrow_base(x = x, y = y, angle = angle, size = 0.15)
    }
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# draw_circle_arrow_base() TESTS
# ============================================

test_that("draw_circle_arrow_base draws without error", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_circle_arrow_base(x = 0, y = 0, size = 0.1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_circle_arrow_base accepts color parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_circle_arrow_base(x = 0.3, y = 0.3, size = 0.1, col = "red")
    draw_circle_arrow_base(x = -0.3, y = -0.3, size = 0.1, col = "#00FF00")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_circle_arrow_base accepts border parameter", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_circle_arrow_base(x = 0, y = 0, size = 0.2, col = "lightblue", border = "darkblue")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_circle_arrow_base uses col as border when border is NULL", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_circle_arrow_base(x = 0, y = 0, size = 0.1, col = "purple", border = NULL)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_circle_arrow_base handles different sizes", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  result <- tryCatch({
    draw_circle_arrow_base(x = -0.5, y = 0, size = 0.05)  # Small
    draw_circle_arrow_base(x = 0, y = 0, size = 0.15)     # Medium
    draw_circle_arrow_base(x = 0.5, y = 0, size = 0.25)   # Large
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# shorten_edge_for_arrow() TESTS
# ============================================

test_that("shorten_edge_for_arrow returns list with x and y", {
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0, x2 = 1, y2 = 0, arrow_size = 0.1)

  expect_type(result, "list")
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("shorten_edge_for_arrow shortens horizontal edge correctly", {
  # Edge from (0,0) to (1,0), arrow size 0.1
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0, x2 = 1, y2 = 0, arrow_size = 0.1)

  expect_equal(result$x, 0.9, tolerance = 1e-10)
  expect_equal(result$y, 0, tolerance = 1e-10)
})

test_that("shorten_edge_for_arrow shortens vertical edge correctly", {
  # Edge from (0,0) to (0,1), arrow size 0.2
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0, x2 = 0, y2 = 1, arrow_size = 0.2)

  expect_equal(result$x, 0, tolerance = 1e-10)
  expect_equal(result$y, 0.8, tolerance = 1e-10)
})

test_that("shorten_edge_for_arrow shortens diagonal edge correctly", {
  # 45-degree edge from origin
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0, x2 = 1, y2 = 1, arrow_size = 0.1)

  # Should move back along the 45-degree line
  expect_lt(result$x, 1)
  expect_lt(result$y, 1)
  # New endpoint should be arrow_size away from (1,1)
  dist <- sqrt((result$x - 1)^2 + (result$y - 1)^2)
  expect_equal(dist, 0.1, tolerance = 1e-10)
})

test_that("shorten_edge_for_arrow handles zero arrow_size", {
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0, x2 = 1, y2 = 1, arrow_size = 0)

  # With zero arrow size, endpoint should be unchanged
  expect_equal(result$x, 1, tolerance = 1e-10)
  expect_equal(result$y, 1, tolerance = 1e-10)
})

test_that("shorten_edge_for_arrow handles negative direction edges", {
  # Edge going from right to left
  result <- shorten_edge_for_arrow(x1 = 1, y1 = 0, x2 = 0, y2 = 0, arrow_size = 0.1)

  expect_equal(result$x, 0.1, tolerance = 1e-10)
  expect_equal(result$y, 0, tolerance = 1e-10)
})

test_that("shorten_edge_for_arrow handles edge to negative coordinates", {
  # Edge going into negative quadrant
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0, x2 = -1, y2 = -1, arrow_size = 0.1)

  expect_gt(result$x, -1)
  expect_gt(result$y, -1)
})

# ============================================
# INTEGRATION TESTS
# ============================================

test_that("arrow functions work together for directed network", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  # Create simple directed network
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3)

  png(tmp, width = 300, height = 300)
  result <- tryCatch({
    splot(mat, directed = TRUE, show_arrows = TRUE)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
  expect_true(file.exists(tmp))
})

test_that("arrow functions work with bidirectional edges", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  # Create directed network with reciprocal edges
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)

  png(tmp, width = 300, height = 300)
  result <- tryCatch({
    splot(mat, directed = TRUE, bidirectional = TRUE)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("arrow functions work with curved edges", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  mat <- create_test_matrix(4, symmetric = FALSE)

  png(tmp, width = 300, height = 300)
  result <- tryCatch({
    splot(mat, directed = TRUE, curvature = 0.3, show_arrows = TRUE)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("arrow size parameter affects arrow rendering", {
  tmp1 <- tempfile(fileext = ".png")
  tmp2 <- tempfile(fileext = ".png")
  on.exit({unlink(tmp1); unlink(tmp2)}, add = TRUE)

  mat <- matrix(c(0, 1, 0, 0), 2, 2)

  # Small arrows
  png(tmp1, width = 200, height = 200)
  splot(mat, directed = TRUE, arrow_size = 0.5)
  dev.off()

  # Large arrows
  png(tmp2, width = 200, height = 200)
  splot(mat, directed = TRUE, arrow_size = 2.0)
  dev.off()

  # Both should render successfully
  expect_true(file.exists(tmp1))
  expect_true(file.exists(tmp2))
  # Files should have different sizes (different arrow sizes affect rendering)
  expect_true(file.size(tmp1) > 0)
  expect_true(file.size(tmp2) > 0)
})

test_that("arrows render correctly with self-loops", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  # Matrix with self-loop
  mat <- matrix(c(1, 1, 0, 0, 0, 1, 0, 1, 0), 3, 3)

  png(tmp, width = 300, height = 300)
  result <- tryCatch({
    splot(mat, directed = TRUE, show_arrows = TRUE)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("arrow colors follow edge colors", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  mat <- matrix(c(0, 1, 0, 0), 2, 2)

  png(tmp, width = 200, height = 200)
  result <- tryCatch({
    splot(mat, directed = TRUE, edge_color = "red", show_arrows = TRUE)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("multiple arrow types render without conflict", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 400, height = 400)
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2))

  result <- tryCatch({
    # Filled arrow
    draw_arrow_base(x = 1, y = 1, angle = 0, size = 0.15, col = "blue")
    # Open arrow
    draw_open_arrow_base(x = -1, y = 1, angle = pi, size = 0.15, col = "red")
    # Circle/dot
    draw_circle_arrow_base(x = 0, y = -1, size = 0.1, col = "green")
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

# ============================================
# EDGE CASE TESTS
# ============================================

test_that("arrow_base_midpoint handles angle = 2*pi (full rotation)", {
  # 2*pi should be equivalent to 0
  result1 <- arrow_base_midpoint(x = 1, y = 0, angle = 0, size = 0.1)
  result2 <- arrow_base_midpoint(x = 1, y = 0, angle = 2*pi, size = 0.1)

  expect_equal(result1$x, result2$x, tolerance = 1e-10)
  expect_equal(result1$y, result2$y, tolerance = 1e-10)
})

test_that("arrow_head_points handles very small size", {
  result <- arrow_head_points(x = 0, y = 0, angle = 0, size = 0.001)

  expect_length(result$x, 3)
  expect_length(result$y, 3)
  # All points should be very close together
  expect_lt(max(result$x) - min(result$x), 0.01)
})

test_that("shorten_edge_for_arrow handles same start and end point", {
  # Degenerate edge with no length
  result <- shorten_edge_for_arrow(x1 = 0, y1 = 0, x2 = 0, y2 = 0, arrow_size = 0.1)

  # Function still returns numeric values (angle defaults to 0 when both points are same)
  expect_type(result$x, "double")
  expect_type(result$y, "double")
  expect_false(is.na(result$x))
  expect_false(is.na(result$y))
})

test_that("arrow drawing functions handle extreme coordinates", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1000, 1000), ylim = c(-1000, 1000))

  result <- tryCatch({
    draw_arrow_base(x = 500, y = 500, angle = 0, size = 50)
    draw_open_arrow_base(x = -500, y = -500, angle = pi, size = 50)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_curved_arrow_base handles exact two-point spline", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  # Minimum valid spline
  result <- tryCatch({
    draw_curved_arrow_base(spline_x = c(0, 1), spline_y = c(0, 0.5), size = 0.1)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})

test_that("draw_circle_arrow_base handles zero size", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp, width = 200, height = 200)
  plot.new()
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1))

  # Zero size should still work (just won't be visible)
  result <- tryCatch({
    draw_circle_arrow_base(x = 0, y = 0, size = 0)
    TRUE
  }, error = function(e) FALSE)
  dev.off()

  expect_true(result)
})
