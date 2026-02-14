# Tests for R/utils-geometry.R
# Coverage for geometry utility functions: point_distance, point_angle,
# point_on_circle, bezier_points, curve_control_point, arrow_points,
# offset_point, and edge_endpoint
# Tests focus on uncovered functions and edge cases

# ============================================
# Import internal functions
# ============================================

point_distance <- cograph:::point_distance
point_angle <- cograph:::point_angle
point_on_circle <- cograph:::point_on_circle
bezier_points <- cograph:::bezier_points
curve_control_point <- cograph:::curve_control_point
arrow_points <- cograph:::arrow_points
offset_point <- cograph:::offset_point
edge_endpoint <- cograph:::edge_endpoint

# ============================================
# point_distance Tests
# ============================================

test_that("point_distance returns correct Euclidean distance for simple cases", {
  # Horizontal distance
  expect_equal(point_distance(0, 0, 3, 0), 3)

  # Vertical distance
  expect_equal(point_distance(0, 0, 0, 4), 4)

  # 3-4-5 triangle
  expect_equal(point_distance(0, 0, 3, 4), 5)
})

test_that("point_distance returns zero for identical points", {
  expect_equal(point_distance(5, 5, 5, 5), 0)
  expect_equal(point_distance(0, 0, 0, 0), 0)
  expect_equal(point_distance(-3.14, 2.71, -3.14, 2.71), 0)
})

test_that("point_distance handles negative coordinates", {
  # Distance from (-1, -1) to (2, 3)
  # dx = 3, dy = 4, distance = 5
  expect_equal(point_distance(-1, -1, 2, 3), 5)

  # Both points in negative quadrant
  expect_equal(point_distance(-5, -5, -2, -1), 5)  # 3-4-5 triangle
})

test_that("point_distance handles decimal values", {
  dist <- point_distance(0.5, 0.5, 1.5, 1.5)
  expected <- sqrt(2)  # sqrt((1)^2 + (1)^2)
  expect_equal(dist, expected, tolerance = 1e-10)
})

test_that("point_distance is symmetric", {
  dist1 <- point_distance(1, 2, 5, 6)
  dist2 <- point_distance(5, 6, 1, 2)
  expect_equal(dist1, dist2)
})

test_that("point_distance handles large values", {
  dist <- point_distance(0, 0, 1e6, 1e6)
  expected <- sqrt(2) * 1e6
  expect_equal(dist, expected, tolerance = 1e-6)
})

# ============================================
# point_angle Tests
# ============================================

test_that("point_angle returns correct angle for cardinal directions", {
  # Right (east)
  expect_equal(point_angle(0, 0, 1, 0), 0)

  # Up (north)
  expect_equal(point_angle(0, 0, 0, 1), pi / 2)

  # Left (west)
  expect_equal(point_angle(0, 0, -1, 0), pi)

  # Down (south)
  expect_equal(point_angle(0, 0, 0, -1), -pi / 2)
})

test_that("point_angle returns correct angle for diagonal directions", {
  # Northeast (45 degrees)
  expect_equal(point_angle(0, 0, 1, 1), pi / 4, tolerance = 1e-10)

  # Northwest (135 degrees)
  expect_equal(point_angle(0, 0, -1, 1), 3 * pi / 4, tolerance = 1e-10)

  # Southwest (-135 degrees)
  expect_equal(point_angle(0, 0, -1, -1), -3 * pi / 4, tolerance = 1e-10)

  # Southeast (-45 degrees)
  expect_equal(point_angle(0, 0, 1, -1), -pi / 4, tolerance = 1e-10)
})

test_that("point_angle handles non-origin start points", {
  # Same as (0,0) to (1,1) but shifted
  angle <- point_angle(5, 5, 6, 6)
  expect_equal(angle, pi / 4, tolerance = 1e-10)
})

test_that("point_angle returns 0 for identical points", {
  # atan2(0, 0) is defined as 0 in R
  expect_equal(point_angle(3, 3, 3, 3), 0)
})

test_that("point_angle handles negative coordinates", {
  # From (-2, -2) to (-1, -1) is like going northeast
  angle <- point_angle(-2, -2, -1, -1)
  expect_equal(angle, pi / 4, tolerance = 1e-10)
})

test_that("point_angle returns values in correct range", {
  # Test various angles are within (-pi, pi]
  angles <- c(
    point_angle(0, 0, 1, 0),
    point_angle(0, 0, 0, 1),
    point_angle(0, 0, -1, 0),
    point_angle(0, 0, 0, -1),
    point_angle(0, 0, 1, 1),
    point_angle(0, 0, -1, -1)
  )

  expect_true(all(angles >= -pi))
  expect_true(all(angles <= pi))
})

# ============================================
# point_on_circle Tests
# ============================================

test_that("point_on_circle returns correct structure", {
  pt <- point_on_circle(0, 0, 1, 0)

  expect_true(is.list(pt))
  expect_true(all(c("x", "y") %in% names(pt)))
})

test_that("point_on_circle returns correct points for cardinal angles", {
  # Angle 0 (right)
  pt <- point_on_circle(0, 0, 1, 0)
  expect_equal(pt$x, 1, tolerance = 1e-10)
  expect_equal(pt$y, 0, tolerance = 1e-10)

  # Angle pi/2 (up)
  pt <- point_on_circle(0, 0, 1, pi / 2)
  expect_equal(pt$x, 0, tolerance = 1e-10)
  expect_equal(pt$y, 1, tolerance = 1e-10)

  # Angle pi (left)
  pt <- point_on_circle(0, 0, 1, pi)
  expect_equal(pt$x, -1, tolerance = 1e-10)
  expect_equal(pt$y, 0, tolerance = 1e-10)

  # Angle -pi/2 (down)
  pt <- point_on_circle(0, 0, 1, -pi / 2)
  expect_equal(pt$x, 0, tolerance = 1e-10)
  expect_equal(pt$y, -1, tolerance = 1e-10)
})

test_that("point_on_circle respects center offset", {
  # Circle centered at (5, 3)
  pt <- point_on_circle(5, 3, 1, 0)
  expect_equal(pt$x, 6, tolerance = 1e-10)
  expect_equal(pt$y, 3, tolerance = 1e-10)

  pt <- point_on_circle(5, 3, 1, pi / 2)
  expect_equal(pt$x, 5, tolerance = 1e-10)
  expect_equal(pt$y, 4, tolerance = 1e-10)
})

test_that("point_on_circle respects radius", {
  # Radius 2
  pt <- point_on_circle(0, 0, 2, 0)
  expect_equal(pt$x, 2, tolerance = 1e-10)
  expect_equal(pt$y, 0, tolerance = 1e-10)

  # Radius 0.5
  pt <- point_on_circle(0, 0, 0.5, 0)
  expect_equal(pt$x, 0.5, tolerance = 1e-10)
  expect_equal(pt$y, 0, tolerance = 1e-10)
})

test_that("point_on_circle handles zero radius", {
  pt <- point_on_circle(5, 3, 0, pi / 4)
  expect_equal(pt$x, 5)
  expect_equal(pt$y, 3)
})

test_that("point_on_circle handles negative radius", {
  # Negative radius points in opposite direction
 pt <- point_on_circle(0, 0, -1, 0)
  expect_equal(pt$x, -1, tolerance = 1e-10)
  expect_equal(pt$y, 0, tolerance = 1e-10)
})

test_that("point_on_circle handles diagonal angles correctly", {
  # 45 degrees
  pt <- point_on_circle(0, 0, 1, pi / 4)
  expected <- sqrt(2) / 2
  expect_equal(pt$x, expected, tolerance = 1e-10)
  expect_equal(pt$y, expected, tolerance = 1e-10)
})

# ============================================
# offset_point Tests
# ============================================

test_that("offset_point returns correct structure", {
  pt <- offset_point(0, 0, 1, 0, 0.5)

  expect_true(is.list(pt))
  expect_true(all(c("x", "y") %in% names(pt)))
})

test_that("offset_point offsets toward target in cardinal directions", {
  # Offset right
  pt <- offset_point(0, 0, 10, 0, 3)
  expect_equal(pt$x, 3, tolerance = 1e-10)
  expect_equal(pt$y, 0, tolerance = 1e-10)

  # Offset up
  pt <- offset_point(0, 0, 0, 10, 3)
  expect_equal(pt$x, 0, tolerance = 1e-10)
  expect_equal(pt$y, 3, tolerance = 1e-10)

  # Offset left
  pt <- offset_point(0, 0, -10, 0, 3)
  expect_equal(pt$x, -3, tolerance = 1e-10)
  expect_equal(pt$y, 0, tolerance = 1e-10)

  # Offset down
  pt <- offset_point(0, 0, 0, -10, 3)
  expect_equal(pt$x, 0, tolerance = 1e-10)
  expect_equal(pt$y, -3, tolerance = 1e-10)
})

test_that("offset_point handles diagonal direction", {
  # From (0,0) toward (1,1) with offset 1
  pt <- offset_point(0, 0, 1, 1, 1)
  expected <- sqrt(2) / 2
  expect_equal(pt$x, expected, tolerance = 1e-10)
  expect_equal(pt$y, expected, tolerance = 1e-10)
})

test_that("offset_point handles non-origin start points", {
  # From (5,5) toward (10, 5) with offset 2
  pt <- offset_point(5, 5, 10, 5, 2)
  expect_equal(pt$x, 7, tolerance = 1e-10)
  expect_equal(pt$y, 5, tolerance = 1e-10)
})

test_that("offset_point handles zero offset", {
  pt <- offset_point(3, 4, 10, 10, 0)
  expect_equal(pt$x, 3)
  expect_equal(pt$y, 4)
})

test_that("offset_point handles negative offset (moves away)", {
  # From (0,0) toward (10, 0) with offset -3 moves backward
  pt <- offset_point(0, 0, 10, 0, -3)
  expect_equal(pt$x, -3, tolerance = 1e-10)
  expect_equal(pt$y, 0, tolerance = 1e-10)
})

test_that("offset_point handles identical points (toward_x = x)", {
  # When toward and origin are the same, angle is 0
  pt <- offset_point(5, 5, 5, 5, 3)
  # Should offset by (3, 0) since atan2(0, 0) = 0
  expect_equal(pt$x, 8, tolerance = 1e-10)
  expect_equal(pt$y, 5, tolerance = 1e-10)
})

# ============================================
# bezier_points Additional Tests
# ============================================

test_that("bezier_points handles default n parameter", {
  pts <- bezier_points(0, 0, 0.5, 0.5, 1, 0)
  expect_equal(nrow(pts), 50)  # default n = 50
})

test_that("bezier_points handles small n values", {
  pts <- bezier_points(0, 0, 0.5, 0.5, 1, 0, n = 2)
  expect_equal(nrow(pts), 2)

  # First point should be start
  expect_equal(pts$x[1], 0)
  expect_equal(pts$y[1], 0)

  # Last point should be end
  expect_equal(pts$x[2], 1)
  expect_equal(pts$y[2], 0)
})

test_that("bezier_points generates straight line when control point is on line", {
  # Control point at midpoint of line from (0,0) to (1,0)
  pts <- bezier_points(0, 0, 0.5, 0, 1, 0, n = 5)

  # All y values should be 0 (straight horizontal line)
  expect_true(all(abs(pts$y) < 1e-10))

  # x values should be evenly spaced from 0 to 1
  expect_equal(pts$x[1], 0)
  expect_equal(pts$x[5], 1)
})

test_that("bezier_points handles vertical curve", {
  pts <- bezier_points(0, 0, 0, 0.5, 0, 1, n = 3)

  # All x values should be 0
  expect_true(all(abs(pts$x) < 1e-10))

  expect_equal(pts$y[1], 0)
  expect_equal(pts$y[3], 1)
})

test_that("bezier_points handles negative coordinates", {
  pts <- bezier_points(-1, -1, 0, 0, 1, 1, n = 3)

  expect_equal(pts$x[1], -1)
  expect_equal(pts$y[1], -1)
  expect_equal(pts$x[3], 1)
  expect_equal(pts$y[3], 1)
})

# ============================================
# curve_control_point Additional Tests
# ============================================

test_that("curve_control_point clamps pivot to valid range", {
  # Pivot < 0 should be clamped to 0
  ctrl_neg <- curve_control_point(0, 0, 1, 0, 0.5, pivot = -0.5)
  ctrl_zero <- curve_control_point(0, 0, 1, 0, 0.5, pivot = 0)
  expect_equal(ctrl_neg$x, ctrl_zero$x)
  expect_equal(ctrl_neg$y, ctrl_zero$y)

  # Pivot > 1 should be clamped to 1
  ctrl_over <- curve_control_point(0, 0, 1, 0, 0.5, pivot = 1.5)
  ctrl_one <- curve_control_point(0, 0, 1, 0, 0.5, pivot = 1)
  expect_equal(ctrl_over$x, ctrl_one$x)
  expect_equal(ctrl_over$y, ctrl_one$y)
})

test_that("curve_control_point clamps shape to valid range", {
  # Shape < -1 should be clamped to -1
  ctrl_neg <- curve_control_point(0, 0, 1, 0, 0.5, shape = -2)
  ctrl_min <- curve_control_point(0, 0, 1, 0, 0.5, shape = -1)
  expect_equal(ctrl_neg$y, ctrl_min$y)

  # Shape > 1 should be clamped to 1
  ctrl_over <- curve_control_point(0, 0, 1, 0, 0.5, shape = 2)
  ctrl_max <- curve_control_point(0, 0, 1, 0, 0.5, shape = 1)
  expect_equal(ctrl_over$y, ctrl_max$y)
})

test_that("curve_control_point handles negative curvature", {
  ctrl_pos <- curve_control_point(0, 0, 1, 0, 0.5)
  ctrl_neg <- curve_control_point(0, 0, 1, 0, -0.5)

  # Negative curvature should produce opposite perpendicular offset
  expect_equal(ctrl_pos$y, -ctrl_neg$y)
})

test_that("curve_control_point handles vertical edge", {
  ctrl <- curve_control_point(0, 0, 0, 1, 0.3)

  # For vertical edge, perpendicular is horizontal
  expect_true(ctrl$x != 0)  # Should have x offset
  expect_equal(ctrl$y, 0.5, tolerance = 1e-10)  # Midpoint y
})

test_that("curve_control_point handles horizontal edge", {
  ctrl <- curve_control_point(0, 0, 1, 0, 0.3)

  # For horizontal edge, perpendicular is vertical
  expect_equal(ctrl$x, 0.5, tolerance = 1e-10)  # Midpoint x
  expect_true(ctrl$y != 0)  # Should have y offset
})

test_that("curve_control_point shape parameter affects curvature intensity", {
  # Shape < 0 increases curvature magnitude
  ctrl_sharp <- curve_control_point(0, 0, 1, 0, 0.5, shape = -0.5)
  ctrl_normal <- curve_control_point(0, 0, 1, 0, 0.5, shape = 0)
  ctrl_gentle <- curve_control_point(0, 0, 1, 0, 0.5, shape = 0.5)

  # Sharp should have larger perpendicular offset than normal
  expect_true(abs(ctrl_sharp$y) > abs(ctrl_normal$y))

  # Gentle should have smaller perpendicular offset than normal
  expect_true(abs(ctrl_gentle$y) < abs(ctrl_normal$y))
})

# ============================================
# arrow_points Additional Tests
# ============================================

test_that("arrow_points returns correct structure with all fields", {
  pts <- arrow_points(0.5, 0.5, 0, 0.03)

  expect_true(is.list(pts))
  expect_true(all(c("x", "y", "mid_x", "mid_y", "back_len") %in% names(pts)))
  expect_equal(length(pts$x), 3)
  expect_equal(length(pts$y), 3)
})

test_that("arrow_points tip is at specified position", {
  pts <- arrow_points(0.7, 0.3, pi / 4, 0.05)

  # First vertex is the tip
  expect_equal(pts$x[1], 0.7)
  expect_equal(pts$y[1], 0.3)
})

test_that("arrow_points handles different width ratios", {
  pts_narrow <- arrow_points(0.5, 0.5, 0, 0.03, width = 0.3)
  pts_wide <- arrow_points(0.5, 0.5, 0, 0.03, width = 0.7)

  # Calculate widths by distance between left and right points
  width_narrow <- abs(pts_narrow$y[2] - pts_narrow$y[3])
  width_wide <- abs(pts_wide$y[2] - pts_wide$y[3])

  expect_true(width_wide > width_narrow)
})

test_that("arrow_points handles aspect ratio scaling", {
  pts_no_scale <- arrow_points(0.5, 0.5, pi / 4, 0.03)
  pts_x_scaled <- arrow_points(0.5, 0.5, pi / 4, 0.03, x_scale = 0.5)
  pts_y_scaled <- arrow_points(0.5, 0.5, pi / 4, 0.03, y_scale = 2)

  # Scaling should affect the back vertices differently
  expect_true(pts_no_scale$x[2] != pts_x_scaled$x[2])
  expect_true(pts_no_scale$y[2] != pts_y_scaled$y[2])
})

test_that("arrow_points midpoint is between left and right vertices", {
  pts <- arrow_points(0.5, 0.5, 0, 0.03)

  expected_mid_x <- (pts$x[2] + pts$x[3]) / 2
  expected_mid_y <- (pts$y[2] + pts$y[3]) / 2

  expect_equal(pts$mid_x, expected_mid_x, tolerance = 1e-10)
  expect_equal(pts$mid_y, expected_mid_y, tolerance = 1e-10)
})

test_that("arrow_points handles angle = 0 (pointing right)", {
  pts <- arrow_points(1, 0.5, 0, 0.05)

  # For angle 0, arrow points right, so back vertices are to the left
  expect_true(pts$x[2] < pts$x[1])
  expect_true(pts$x[3] < pts$x[1])
})

test_that("arrow_points handles angle = pi (pointing left)", {
  pts <- arrow_points(0, 0.5, pi, 0.05)

  # For angle pi, arrow points left, so back vertices are to the right
  expect_true(pts$x[2] > pts$x[1])
  expect_true(pts$x[3] > pts$x[1])
})

test_that("arrow_points back_len increases with arrow size", {
  pts_small <- arrow_points(0.5, 0.5, 0, 0.02)
  pts_large <- arrow_points(0.5, 0.5, 0, 0.08)

  expect_true(pts_large$back_len > pts_small$back_len)
})

# ============================================
# edge_endpoint Additional Tests
# ============================================

test_that("edge_endpoint returns correct structure", {
  pt <- edge_endpoint(0.5, 0.5, 0.8, 0.5, 0.05)

  expect_true(is.list(pt))
  expect_true(all(c("x", "y") %in% names(pt)))
})

test_that("edge_endpoint is on node border at correct distance", {
  node_x <- 0.5
  node_y <- 0.5
  node_size <- 0.1

  pt <- edge_endpoint(node_x, node_y, 1, 0.5, node_size)

  # Distance from center to endpoint should equal node_size
  dist <- sqrt((pt$x - node_x)^2 + (pt$y - node_y)^2)
  expect_equal(dist, node_size, tolerance = 1e-10)
})

test_that("edge_endpoint handles cardinal directions correctly", {
  cx <- 0.5
  cy <- 0.5
  r <- 0.1

  # Other point to the right
  pt_right <- edge_endpoint(cx, cy, cx + 0.3, cy, r)
  expect_equal(pt_right$x, cx + r, tolerance = 1e-10)
  expect_equal(pt_right$y, cy, tolerance = 1e-10)

  # Other point above
  pt_up <- edge_endpoint(cx, cy, cx, cy + 0.3, r)
  expect_equal(pt_up$x, cx, tolerance = 1e-10)
  expect_equal(pt_up$y, cy + r, tolerance = 1e-10)

  # Other point to the left
  pt_left <- edge_endpoint(cx, cy, cx - 0.3, cy, r)
  expect_equal(pt_left$x, cx - r, tolerance = 1e-10)
  expect_equal(pt_left$y, cy, tolerance = 1e-10)

  # Other point below
  pt_down <- edge_endpoint(cx, cy, cx, cy - 0.3, r)
  expect_equal(pt_down$x, cx, tolerance = 1e-10)
  expect_equal(pt_down$y, cy - r, tolerance = 1e-10)
})

test_that("edge_endpoint handles diagonal directions", {
  cx <- 0.5
  cy <- 0.5
  r <- 0.1

  # Other point northeast
  pt <- edge_endpoint(cx, cy, cx + 0.3, cy + 0.3, r)

  # Should be at 45 degrees
  expected_offset <- r * sqrt(2) / 2
  expect_equal(pt$x - cx, expected_offset, tolerance = 1e-10)
  expect_equal(pt$y - cy, expected_offset, tolerance = 1e-10)
})

test_that("edge_endpoint handles aspect ratio correction x_scale", {
  cx <- 0.5
  cy <- 0.5
  r <- 0.1

  # With x_scale < 1, x distances are compressed
  pt_no_scale <- edge_endpoint(cx, cy, cx + 0.3, cy + 0.3, r)
  pt_x_scale <- edge_endpoint(cx, cy, cx + 0.3, cy + 0.3, r, x_scale = 0.5)

  # x_scale affects the endpoint calculation
  expect_true(pt_no_scale$x != pt_x_scale$x)
})

test_that("edge_endpoint handles aspect ratio correction y_scale", {
  cx <- 0.5
  cy <- 0.5
  r <- 0.1

  pt_no_scale <- edge_endpoint(cx, cy, cx + 0.3, cy + 0.3, r)
  pt_y_scale <- edge_endpoint(cx, cy, cx + 0.3, cy + 0.3, r, y_scale = 2)

  # y_scale affects the endpoint calculation
  expect_true(pt_no_scale$y != pt_y_scale$y)
})

test_that("edge_endpoint handles very small node size", {
  pt <- edge_endpoint(0.5, 0.5, 0.8, 0.5, 0.001)

  # Should still calculate correctly
  expect_equal(pt$x, 0.501, tolerance = 1e-10)
  expect_equal(pt$y, 0.5, tolerance = 1e-10)
})

test_that("edge_endpoint handles identical points", {
  # When node center equals other point, angle is 0
  pt <- edge_endpoint(0.5, 0.5, 0.5, 0.5, 0.1)

  # Should return point offset in default direction (right)
  expect_equal(pt$x, 0.6, tolerance = 1e-10)
  expect_equal(pt$y, 0.5, tolerance = 1e-10)
})

# ============================================
# Integration and Edge Case Tests
# ============================================

test_that("point_distance and point_on_circle are consistent", {
  # A point on circle should be exactly radius distance from center
  cx <- 3
  cy <- 4
  r <- 5
  angle <- pi / 3

  pt <- point_on_circle(cx, cy, r, angle)
  dist <- point_distance(cx, cy, pt$x, pt$y)

  expect_equal(dist, r, tolerance = 1e-10)
})

test_that("point_angle and point_on_circle are inverse operations", {
  # Start from center, go to point on circle, angle should match
  cx <- 0
  cy <- 0
  r <- 1
  original_angle <- 2 * pi / 5

  pt <- point_on_circle(cx, cy, r, original_angle)
  recovered_angle <- point_angle(cx, cy, pt$x, pt$y)

  expect_equal(recovered_angle, original_angle, tolerance = 1e-10)
})

test_that("offset_point and point_distance are consistent", {
  # Offset by distance d should produce point at distance d
  start_x <- 2
  start_y <- 3
  toward_x <- 10
  toward_y <- 7
  offset <- 2.5

  pt <- offset_point(start_x, start_y, toward_x, toward_y, offset)
  dist <- point_distance(start_x, start_y, pt$x, pt$y)

  expect_equal(dist, abs(offset), tolerance = 1e-10)
})

test_that("edge_endpoint uses point_angle internally correctly", {
  # Verify edge_endpoint direction matches manual angle calculation
  node_x <- 0.3
  node_y <- 0.4
  other_x <- 0.7
  other_y <- 0.6
  size <- 0.05

  pt <- edge_endpoint(node_x, node_y, other_x, other_y, size)
  angle <- point_angle(node_x, node_y, other_x, other_y)

  # The endpoint should be in the direction of the angle
  endpoint_angle <- point_angle(node_x, node_y, pt$x, pt$y)
  expect_equal(endpoint_angle, angle, tolerance = 1e-10)
})

test_that("curve_control_point produces valid Bezier curves", {
  # Create a curved edge and verify it passes through expected region
  x1 <- 0
  y1 <- 0
  x2 <- 1
  y2 <- 0
  curvature <- 0.3

  ctrl <- curve_control_point(x1, y1, x2, y2, curvature)
  pts <- bezier_points(x1, y1, ctrl$x, ctrl$y, x2, y2, n = 50)

  # Curve should start at (0, 0) and end at (1, 0)
  expect_equal(pts$x[1], x1)
  expect_equal(pts$y[1], y1)
  expect_equal(pts$x[50], x2)
  expect_equal(pts$y[50], y2)

  # With positive curvature, curve should bulge above the line
  mid_idx <- 25
  expect_true(pts$y[mid_idx] > 0)
})

test_that("all geometry functions handle extreme coordinates gracefully", {
  # Very large coordinates
  expect_no_error(point_distance(1e10, 1e10, 1e10 + 3, 1e10 + 4))
  expect_no_error(point_angle(1e10, 1e10, 1e10 + 1, 1e10 + 1))
  expect_no_error(point_on_circle(1e10, 1e10, 1, 0))
  expect_no_error(offset_point(1e10, 1e10, 1e10 + 10, 1e10, 5))
  expect_no_error(curve_control_point(1e10, 1e10, 1e10 + 1, 1e10 + 1, 0.3))
  expect_no_error(arrow_points(1e10, 1e10, 0, 0.03))
  expect_no_error(edge_endpoint(1e10, 1e10, 1e10 + 1, 1e10, 0.1))
  expect_no_error(bezier_points(1e10, 1e10, 1e10 + 0.5, 1e10 + 0.5, 1e10 + 1, 1e10))
})

test_that("all geometry functions handle very small values gracefully", {
  # Very small values (near machine precision)
  small <- 1e-15

  expect_no_error(point_distance(small, small, 2 * small, 2 * small))
  expect_no_error(point_angle(small, small, 2 * small, 2 * small))
  expect_no_error(point_on_circle(small, small, small, 0))
  expect_no_error(offset_point(small, small, 2 * small, small, small))
  expect_no_error(curve_control_point(small, small, 2 * small, 2 * small, small))
  expect_no_error(arrow_points(small, small, 0, small))
  expect_no_error(edge_endpoint(small, small, 2 * small, small, small))
  expect_no_error(bezier_points(small, small, 2 * small, 2 * small, 3 * small, small))
})
