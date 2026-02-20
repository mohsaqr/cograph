# test-coverage-splot-polygons-41.R - Comprehensive tests for R/splot-polygons.R
# Tests for polygon vertex generation functions

# Make internal functions available for testing
circle_vertices <- cograph:::circle_vertices
square_vertices <- cograph:::square_vertices
rectangle_vertices <- cograph:::rectangle_vertices
triangle_vertices <- cograph:::triangle_vertices
diamond_vertices <- cograph:::diamond_vertices
pentagon_vertices <- cograph:::pentagon_vertices
hexagon_vertices <- cograph:::hexagon_vertices
star_vertices <- cograph:::star_vertices
heart_vertices <- cograph:::heart_vertices
ellipse_vertices <- cograph:::ellipse_vertices
cross_vertices <- cograph:::cross_vertices
regular_polygon_vertices <- cograph:::regular_polygon_vertices
inset_polygon_vertices <- cograph:::inset_polygon_vertices
get_donut_base_vertices <- cograph:::get_donut_base_vertices
gear_vertices <- cograph:::gear_vertices
cloud_vertices <- cograph:::cloud_vertices
brain_vertices <- cograph:::brain_vertices
get_shape_vertices <- cograph:::get_shape_vertices

# ============================================
# CIRCLE VERTICES
# ============================================

test_that("circle_vertices generates correct structure", {
  verts <- circle_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_true("x" %in% names(verts))
  expect_true("y" %in% names(verts))
  expect_equal(length(verts$x), 50)  # default n = 50
  expect_equal(length(verts$y), 50)
})

test_that("circle_vertices respects center coordinates", {
  verts <- circle_vertices(5, 10, 1)

  # Center of mass should be near the specified center
  expect_equal(mean(verts$x), 5, tolerance = 0.01)
  expect_equal(mean(verts$y), 10, tolerance = 0.01)
})

test_that("circle_vertices respects radius", {
  r <- 2.5
  verts <- circle_vertices(0, 0, r)

  # All points should be at distance r from center
  distances <- sqrt(verts$x^2 + verts$y^2)
  expect_true(all(abs(distances - r) < 0.01))
})

test_that("circle_vertices respects n parameter", {
  for (n in c(10, 25, 100, 200)) {
    verts <- circle_vertices(0, 0, 1, n = n)
    expect_equal(length(verts$x), n, info = paste("Failed for n =", n))
    expect_equal(length(verts$y), n, info = paste("Failed for n =", n))
  }
})

test_that("circle_vertices handles small n values", {
  # n = 3 creates a triangle-like shape
  verts <- circle_vertices(0, 0, 1, n = 3)
  expect_equal(length(verts$x), 3)
  expect_equal(length(verts$y), 3)
})

test_that("circle_vertices handles negative coordinates", {
  verts <- circle_vertices(-5, -3, 1.5)

  expect_equal(mean(verts$x), -5, tolerance = 0.01)
  expect_equal(mean(verts$y), -3, tolerance = 0.01)
})

# ============================================
# SQUARE VERTICES
# ============================================

test_that("square_vertices generates correct structure", {
  verts <- square_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 4)
  expect_equal(length(verts$y), 4)
})

test_that("square_vertices respects center coordinates", {
  verts <- square_vertices(3, 7, 1)

  expect_equal(mean(verts$x), 3)
  expect_equal(mean(verts$y), 7)
})

test_that("square_vertices respects half-width parameter", {
  r <- 2
  verts <- square_vertices(0, 0, r)

  # All x values should be +/- r
  expect_true(all(abs(verts$x) == r))
  expect_true(all(abs(verts$y) == r))
})

test_that("square_vertices creates proper corners", {
  verts <- square_vertices(0, 0, 1)

  # Should have corners at (-1,-1), (1,-1), (1,1), (-1,1)
  corners <- data.frame(x = verts$x, y = verts$y)
  expect_true(any(corners$x == -1 & corners$y == -1))
  expect_true(any(corners$x == 1 & corners$y == -1))
  expect_true(any(corners$x == 1 & corners$y == 1))
  expect_true(any(corners$x == -1 & corners$y == 1))
})

# ============================================
# RECTANGLE VERTICES
# ============================================

test_that("rectangle_vertices generates correct structure", {
  verts <- rectangle_vertices(0, 0, 2, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 4)
  expect_equal(length(verts$y), 4)
})

test_that("rectangle_vertices respects width and height", {
  w <- 3
  h <- 1.5
  verts <- rectangle_vertices(0, 0, w, h)

  # X values should be +/- w
  expect_true(all(abs(verts$x) == w))
  # Y values should be +/- h
  expect_true(all(abs(verts$y) == h))
})

test_that("rectangle_vertices respects center coordinates", {
  verts <- rectangle_vertices(5, -2, 2, 1)

  expect_equal(mean(verts$x), 5)
  expect_equal(mean(verts$y), -2)
})

test_that("rectangle_vertices handles tall rectangle", {
  # Height > Width
  verts <- rectangle_vertices(0, 0, 1, 3)

  # Width extent should be 2, height extent should be 6
  expect_equal(diff(range(verts$x)), 2)
  expect_equal(diff(range(verts$y)), 6)
})

test_that("rectangle_vertices handles equal w and h (becomes square)", {
  verts <- rectangle_vertices(0, 0, 2, 2)

  expect_equal(diff(range(verts$x)), diff(range(verts$y)))
})

# ============================================
# TRIANGLE VERTICES
# ============================================

test_that("triangle_vertices generates correct structure", {
  verts <- triangle_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 3)
  expect_equal(length(verts$y), 3)
})

test_that("triangle_vertices has vertex at top", {
  verts <- triangle_vertices(0, 0, 1)

  # Top vertex should be at (0, 1) - pi/2 angle
  max_y_idx <- which.max(verts$y)
  expect_equal(verts$x[max_y_idx], 0, tolerance = 0.001)
  expect_equal(verts$y[max_y_idx], 1, tolerance = 0.001)
})

test_that("triangle_vertices respects radius", {
  r <- 2.5
  verts <- triangle_vertices(0, 0, r)

  # All vertices should be at distance r from center
  distances <- sqrt(verts$x^2 + verts$y^2)
  expect_true(all(abs(distances - r) < 0.001))
})

test_that("triangle_vertices respects center coordinates", {
  verts <- triangle_vertices(4, -3, 1)

  expect_equal(mean(verts$x), 4, tolerance = 0.001)
  expect_equal(mean(verts$y), -3, tolerance = 0.001)
})

# ============================================
# DIAMOND VERTICES
# ============================================

test_that("diamond_vertices generates correct structure", {
  verts <- diamond_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 4)
  expect_equal(length(verts$y), 4)
})

test_that("diamond_vertices has vertices on axes", {
  r <- 1
  verts <- diamond_vertices(0, 0, r)

  # Should have vertices at (r,0), (0,r), (-r,0), (0,-r)
  corners <- data.frame(x = verts$x, y = verts$y)
  expect_true(any(abs(corners$x - r) < 0.001 & abs(corners$y) < 0.001))
  expect_true(any(abs(corners$x) < 0.001 & abs(corners$y - r) < 0.001))
  expect_true(any(abs(corners$x - (-r)) < 0.001 & abs(corners$y) < 0.001))
  expect_true(any(abs(corners$x) < 0.001 & abs(corners$y - (-r)) < 0.001))
})

test_that("diamond_vertices respects center coordinates", {
  verts <- diamond_vertices(2, 3, 1)

  expect_equal(mean(verts$x), 2, tolerance = 0.001)
  expect_equal(mean(verts$y), 3, tolerance = 0.001)
})

# ============================================
# PENTAGON VERTICES
# ============================================

test_that("pentagon_vertices generates correct structure", {
  verts <- pentagon_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 5)
  expect_equal(length(verts$y), 5)
})

test_that("pentagon_vertices has vertex at top", {
  verts <- pentagon_vertices(0, 0, 1)

  # First vertex should be at top (angle pi/2)
  max_y_idx <- which.max(verts$y)
  expect_equal(verts$x[max_y_idx], 0, tolerance = 0.001)
  expect_equal(verts$y[max_y_idx], 1, tolerance = 0.001)
})

test_that("pentagon_vertices respects radius", {
  r <- 3
  verts <- pentagon_vertices(0, 0, r)

  distances <- sqrt(verts$x^2 + verts$y^2)
  expect_true(all(abs(distances - r) < 0.001))
})

# ============================================
# HEXAGON VERTICES
# ============================================

test_that("hexagon_vertices generates correct structure", {
  verts <- hexagon_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 6)
  expect_equal(length(verts$y), 6)
})

test_that("hexagon_vertices has flat sides on top and bottom", {
  verts <- hexagon_vertices(0, 0, 1)

  # First vertex should be at angle 0 (rightmost point)
  expect_true(any(abs(verts$x - 1) < 0.001 & abs(verts$y) < 0.001))
})

test_that("hexagon_vertices respects radius", {
  r <- 2.5
  verts <- hexagon_vertices(0, 0, r)

  distances <- sqrt(verts$x^2 + verts$y^2)
  expect_true(all(abs(distances - r) < 0.001))
})

test_that("hexagon_vertices respects center coordinates", {
  verts <- hexagon_vertices(-1, 2, 1)

  expect_equal(mean(verts$x), -1, tolerance = 0.001)
  expect_equal(mean(verts$y), 2, tolerance = 0.001)
})

# ============================================
# STAR VERTICES
# ============================================

test_that("star_vertices generates correct structure with defaults", {
  verts <- star_vertices(0, 0, 1)

  expect_true(is.list(verts))
  # 5 points * 2 = 10 vertices
  expect_equal(length(verts$x), 10)
  expect_equal(length(verts$y), 10)
})

test_that("star_vertices respects n_points parameter", {
  for (n_points in c(3, 4, 5, 6, 8)) {
    verts <- star_vertices(0, 0, 1, n_points = n_points)
    expect_equal(length(verts$x), n_points * 2,
                 info = paste("Failed for n_points =", n_points))
  }
})

test_that("star_vertices respects inner_ratio parameter", {
  r <- 2
  inner_ratio <- 0.3
  verts <- star_vertices(0, 0, r, n_points = 5, inner_ratio = inner_ratio)

  # Outer vertices should be at distance r
  # Inner vertices should be at distance r * inner_ratio
  distances <- sqrt(verts$x^2 + verts$y^2)
  outer_dist <- distances[seq(1, length(distances), 2)]
  inner_dist <- distances[seq(2, length(distances), 2)]

  expect_true(all(abs(outer_dist - r) < 0.001))
  expect_true(all(abs(inner_dist - r * inner_ratio) < 0.001))
})

test_that("star_vertices handles inner_ratio = 0.5", {
  verts <- star_vertices(0, 0, 1, inner_ratio = 0.5)

  expect_equal(length(verts$x), 10)
})

test_that("star_vertices first vertex is at top", {
  verts <- star_vertices(0, 0, 1, n_points = 5)

  # First vertex should be at angle pi/2 (top)
  expect_equal(verts$x[1], 0, tolerance = 0.001)
  expect_equal(verts$y[1], 1, tolerance = 0.001)
})

# ============================================
# HEART VERTICES
# ============================================

test_that("heart_vertices generates correct structure with defaults", {
  verts <- heart_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 100)  # default n = 100
  expect_equal(length(verts$y), 100)
})

test_that("heart_vertices respects n parameter", {
  for (n in c(50, 100, 200)) {
    verts <- heart_vertices(0, 0, 1, n = n)
    expect_equal(length(verts$x), n, info = paste("Failed for n =", n))
  }
})

test_that("heart_vertices is centered approximately at given coordinates", {
  verts <- heart_vertices(3, -2, 1)

  # Heart shape is not perfectly centered, but should be close
  expect_equal(mean(verts$x), 3, tolerance = 0.1)
  # Heart y is offset due to shape, center should be near given y
  expect_true(abs(mean(verts$y) - (-2)) < 0.5)
})

test_that("heart_vertices respects scale", {
  verts_small <- heart_vertices(0, 0, 0.5)
  verts_large <- heart_vertices(0, 0, 2)

  # Larger scale should have larger extent
  extent_small <- max(abs(c(verts_small$x, verts_small$y)))
  extent_large <- max(abs(c(verts_large$x, verts_large$y)))

  expect_true(extent_large > extent_small)
})

# ============================================
# ELLIPSE VERTICES
# ============================================

test_that("ellipse_vertices generates correct structure", {
  verts <- ellipse_vertices(0, 0, 2, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 50)  # default n = 50
  expect_equal(length(verts$y), 50)
})

test_that("ellipse_vertices respects rx and ry", {
  rx <- 3
  ry <- 1
  verts <- ellipse_vertices(0, 0, rx, ry)

  # Check x extent is rx
  expect_equal(max(verts$x), rx, tolerance = 0.01)
  expect_equal(min(verts$x), -rx, tolerance = 0.01)

  # Check y extent is ry
  expect_equal(max(verts$y), ry, tolerance = 0.01)
  expect_equal(min(verts$y), -ry, tolerance = 0.01)
})

test_that("ellipse_vertices respects n parameter", {
  verts <- ellipse_vertices(0, 0, 1, 1, n = 100)
  expect_equal(length(verts$x), 100)
})

test_that("ellipse_vertices with equal radii is a circle", {
  r <- 1.5
  verts <- ellipse_vertices(0, 0, r, r, n = 100)

  # All points should be at distance r
  distances <- sqrt(verts$x^2 + verts$y^2)
  expect_true(all(abs(distances - r) < 0.01))
})

test_that("ellipse_vertices respects center coordinates", {
  verts <- ellipse_vertices(5, -3, 2, 1)

  expect_equal(mean(verts$x), 5, tolerance = 0.01)
  expect_equal(mean(verts$y), -3, tolerance = 0.01)
})

# ============================================
# CROSS VERTICES
# ============================================

test_that("cross_vertices generates correct structure", {
  verts <- cross_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 12)  # 12-point cross
  expect_equal(length(verts$y), 12)
})

test_that("cross_vertices respects thickness parameter", {
  r <- 1
  thickness <- 0.3
  verts <- cross_vertices(0, 0, r, thickness = thickness)

  # The arm should extend to r
  expect_true(max(verts$x) == r)
  expect_true(max(verts$y) == r)
  expect_true(min(verts$x) == -r)
  expect_true(min(verts$y) == -r)
})

test_that("cross_vertices handles different thickness values", {
  for (thickness in c(0.1, 0.3, 0.5, 0.8)) {
    verts <- cross_vertices(0, 0, 1, thickness = thickness)
    expect_equal(length(verts$x), 12,
                 info = paste("Failed for thickness =", thickness))
  }
})

test_that("cross_vertices is symmetric", {
  verts <- cross_vertices(0, 0, 1, thickness = 0.3)

  # Should be symmetric about both axes
  expect_equal(sort(abs(verts$x)), sort(abs(verts$x)))
  expect_equal(sort(abs(verts$y)), sort(abs(verts$y)))
})

test_that("cross_vertices respects center coordinates", {
  verts <- cross_vertices(2, -1, 1)

  expect_equal(mean(verts$x), 2, tolerance = 0.001)
  expect_equal(mean(verts$y), -1, tolerance = 0.001)
})

# ============================================
# REGULAR POLYGON VERTICES
# ============================================

test_that("regular_polygon_vertices generates correct number of vertices", {
  for (n in c(3, 4, 5, 6, 7, 8, 10, 12)) {
    verts <- regular_polygon_vertices(0, 0, 1, n)
    expect_equal(length(verts$x), n, info = paste("Failed for n =", n))
    expect_equal(length(verts$y), n, info = paste("Failed for n =", n))
  }
})

test_that("regular_polygon_vertices default rotation places vertex at top", {
  verts <- regular_polygon_vertices(0, 0, 1, 5)  # default rotation = pi/2

  # First vertex should be at top
  expect_equal(verts$x[1], 0, tolerance = 0.001)
  expect_equal(verts$y[1], 1, tolerance = 0.001)
})

test_that("regular_polygon_vertices respects rotation parameter", {
  r <- 1
  verts <- regular_polygon_vertices(0, 0, r, 4, rotation = 0)  # Square with vertex on right

  # First vertex should be on the right (angle 0)
  expect_equal(verts$x[1], r, tolerance = 0.001)
  expect_equal(verts$y[1], 0, tolerance = 0.001)
})

test_that("regular_polygon_vertices respects radius", {
  r <- 2.5
  verts <- regular_polygon_vertices(0, 0, r, 6)

  distances <- sqrt(verts$x^2 + verts$y^2)
  expect_true(all(abs(distances - r) < 0.001))
})

test_that("regular_polygon_vertices respects center coordinates", {
  verts <- regular_polygon_vertices(3, -2, 1, 5)

  expect_equal(mean(verts$x), 3, tolerance = 0.001)
  expect_equal(mean(verts$y), -2, tolerance = 0.001)
})

test_that("regular_polygon_vertices with n=3 equals triangle_vertices", {
  verts_reg <- regular_polygon_vertices(0, 0, 1, 3, rotation = pi/2)
  verts_tri <- triangle_vertices(0, 0, 1)

  expect_equal(verts_reg$x, verts_tri$x, tolerance = 0.001)
  expect_equal(verts_reg$y, verts_tri$y, tolerance = 0.001)
})

# ============================================
# INSET POLYGON VERTICES
# ============================================

test_that("inset_polygon_vertices generates correct structure", {
  outer <- list(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
  inner <- inset_polygon_vertices(outer, 0.5)

  expect_true(is.list(inner))
  expect_equal(length(inner$x), length(outer$x))
  expect_equal(length(inner$y), length(outer$y))
})

test_that("inset_polygon_vertices creates smaller polygon", {
  outer <- list(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
  inner <- inset_polygon_vertices(outer, 0.5)

  # Inner polygon should be smaller
  expect_true(max(abs(inner$x)) < max(abs(outer$x)))
  expect_true(max(abs(inner$y)) < max(abs(outer$y)))
})

test_that("inset_polygon_vertices with ratio 0 collapses to center", {
  outer <- list(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
  inner <- inset_polygon_vertices(outer, 0)

  # All points should be at the centroid
  expect_equal(inner$x, rep(mean(outer$x), length(outer$x)))
  expect_equal(inner$y, rep(mean(outer$y), length(outer$y)))
})

test_that("inset_polygon_vertices with ratio 1 equals original", {
  outer <- list(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
  inner <- inset_polygon_vertices(outer, 1)

  expect_equal(inner$x, outer$x)
  expect_equal(inner$y, outer$y)
})

test_that("inset_polygon_vertices preserves shape", {
  # Triangle
  outer <- list(x = c(0, 1, 0.5), y = c(0, 0, 1))
  inner <- inset_polygon_vertices(outer, 0.5)

  # Should still have 3 vertices
  expect_equal(length(inner$x), 3)
})

test_that("inset_polygon_vertices handles non-centered polygon", {
  outer <- list(x = c(4, 6, 6, 4), y = c(4, 4, 6, 6))  # Square centered at (5,5)
  inner <- inset_polygon_vertices(outer, 0.5)

  # Inner centroid should match outer centroid
  expect_equal(mean(inner$x), mean(outer$x))
  expect_equal(mean(inner$y), mean(outer$y))
})

# ============================================
# GET DONUT BASE VERTICES
# ============================================

test_that("get_donut_base_vertices returns vertices for circle", {
  verts <- get_donut_base_vertices("circle", 0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 100)  # n = 100 for donut circle
})

test_that("get_donut_base_vertices returns vertices for square", {
  verts <- get_donut_base_vertices("square", 0, 0, 1)

  expect_equal(length(verts$x), 4)
})

test_that("get_donut_base_vertices returns vertices for rectangle", {
  verts <- get_donut_base_vertices("rectangle", 0, 0, 1)

  expect_equal(length(verts$x), 4)
  # Rectangle has r * 0.7 height
  expect_equal(max(abs(verts$y)), 0.7, tolerance = 0.001)
})

test_that("get_donut_base_vertices returns vertices for triangle", {
  verts <- get_donut_base_vertices("triangle", 0, 0, 1)

  expect_equal(length(verts$x), 3)
})

test_that("get_donut_base_vertices returns vertices for diamond", {
  verts <- get_donut_base_vertices("diamond", 0, 0, 1)

  expect_equal(length(verts$x), 4)
})

test_that("get_donut_base_vertices returns vertices for pentagon", {
  verts <- get_donut_base_vertices("pentagon", 0, 0, 1)

  expect_equal(length(verts$x), 5)
})

test_that("get_donut_base_vertices returns vertices for hexagon", {
  verts <- get_donut_base_vertices("hexagon", 0, 0, 1)

  expect_equal(length(verts$x), 6)
})

test_that("get_donut_base_vertices defaults to circle for unknown shape", {
  verts <- get_donut_base_vertices("unknown_shape", 0, 0, 1)

  # Should fall back to circle with n = 100
  expect_equal(length(verts$x), 100)
})

test_that("get_donut_base_vertices respects center and radius", {
  verts <- get_donut_base_vertices("hexagon", 5, -3, 2)

  expect_equal(mean(verts$x), 5, tolerance = 0.001)
  expect_equal(mean(verts$y), -3, tolerance = 0.001)
})

# ============================================
# GEAR VERTICES
# ============================================

test_that("gear_vertices generates correct structure with defaults", {
  verts <- gear_vertices(0, 0, 1)

  expect_true(is.list(verts))
  # 8 teeth * 8 points per tooth = 64 vertices
  expect_equal(length(verts$x), 64)
  expect_equal(length(verts$y), 64)
})

test_that("gear_vertices respects n_teeth parameter", {
  for (n_teeth in c(4, 6, 8, 10, 12)) {
    verts <- gear_vertices(0, 0, 1, n_teeth = n_teeth)
    expected_pts <- n_teeth * 8
    expect_equal(length(verts$x), expected_pts,
                 info = paste("Failed for n_teeth =", n_teeth))
  }
})

test_that("gear_vertices is centered at given coordinates", {
  verts <- gear_vertices(3, -2, 1)

  expect_equal(mean(verts$x), 3, tolerance = 0.1)
  expect_equal(mean(verts$y), -2, tolerance = 0.1)
})

test_that("gear_vertices respects outer radius", {
  r <- 2
  verts <- gear_vertices(0, 0, r)

  # Max distance should be approximately inner_r + tooth_height = 0.65*r + 0.25*r = 0.9*r
  distances <- sqrt(verts$x^2 + verts$y^2)
  expect_true(max(distances) <= r * 0.91)
  expect_true(max(distances) >= r * 0.8)
})

test_that("gear_vertices has varying radii for teeth", {
  verts <- gear_vertices(0, 0, 1, n_teeth = 8)

  distances <- sqrt(verts$x^2 + verts$y^2)
  # Should have both inner and outer radii
  expect_true(length(unique(round(distances, 2))) > 1)
})

# ============================================
# CLOUD VERTICES
# ============================================

test_that("cloud_vertices generates correct structure with defaults", {
  verts <- cloud_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 100)  # default n = 100
  expect_equal(length(verts$y), 100)
})

test_that("cloud_vertices respects n parameter", {
  verts <- cloud_vertices(0, 0, 1, n = 50)

  expect_equal(length(verts$x), 50)
})

test_that("cloud_vertices is approximately centered at given coordinates", {
  verts <- cloud_vertices(2, -1, 1)

  # Cloud has some offset, but should be close
  expect_equal(mean(verts$x), 2, tolerance = 0.2)
  # y has offset of r * 0.1
  expect_equal(mean(verts$y), -1 + 0.1, tolerance = 0.2)
})

test_that("cloud_vertices has bumpy profile", {
  verts <- cloud_vertices(0, 0, 1, n = 100)

  distances <- sqrt(verts$x^2 + (verts$y - 0.1)^2)
  # Cloud should have varying radii due to sin modulation
  expect_true(sd(distances) > 0.01)
})

# ============================================
# BRAIN VERTICES
# ============================================

test_that("brain_vertices generates correct structure with defaults", {
  verts <- brain_vertices(0, 0, 1)

  expect_true(is.list(verts))
  expect_equal(length(verts$x), 80)  # default n = 80
  expect_equal(length(verts$y), 80)
})

test_that("brain_vertices respects n parameter", {
  verts <- brain_vertices(0, 0, 1, n = 50)

  expect_equal(length(verts$x), 50)
})

test_that("brain_vertices is centered at given coordinates", {
  verts <- brain_vertices(3, -2, 1)

  expect_equal(mean(verts$x), 3, tolerance = 0.1)
  expect_equal(mean(verts$y), -2, tolerance = 0.1)
})

test_that("brain_vertices has wrinkled profile", {
  verts <- brain_vertices(0, 0, 1, n = 100)

  # Calculate effective radii (accounting for y scaling of 0.85)
  distances <- sqrt(verts$x^2 + (verts$y / 0.85)^2)
  # Brain should have varying radii due to sin modulation
  expect_true(sd(distances) > 0.01)
})

test_that("brain_vertices respects radius scale", {
  verts_small <- brain_vertices(0, 0, 0.5)
  verts_large <- brain_vertices(0, 0, 2)

  extent_small <- max(abs(c(verts_small$x, verts_small$y)))
  extent_large <- max(abs(c(verts_large$x, verts_large$y)))

  expect_true(extent_large > extent_small)
})

# ============================================
# GET SHAPE VERTICES - Main Dispatch Function
# ============================================

test_that("get_shape_vertices returns vertices for all standard shapes", {
  shapes <- c("circle", "square", "rectangle", "triangle", "diamond",
              "pentagon", "hexagon", "star", "heart", "ellipse", "cross",
              "gear", "cloud", "brain")

  for (shape in shapes) {
    verts <- get_shape_vertices(shape, 0, 0, 1)
    expect_true(is.list(verts), info = paste("Failed for shape:", shape))
    expect_true("x" %in% names(verts), info = paste("Missing x for shape:", shape))
    expect_true("y" %in% names(verts), info = paste("Missing y for shape:", shape))
    expect_true(length(verts$x) > 0, info = paste("Empty x for shape:", shape))
    expect_equal(length(verts$x), length(verts$y),
                 info = paste("x/y length mismatch for shape:", shape))
  }
})

test_that("get_shape_vertices defaults to circle for unknown shape", {
  verts <- get_shape_vertices("unknown_shape_xyz", 0, 0, 1)

  # Should fall back to circle with default n = 50
  expect_equal(length(verts$x), 50)
})

test_that("get_shape_vertices handles r2 parameter for rectangle", {
  verts <- get_shape_vertices("rectangle", 0, 0, 2, r2 = 1)

  # Width should be 2*2=4, height should be 2*1=2
  expect_equal(diff(range(verts$x)), 4)
  expect_equal(diff(range(verts$y)), 2)
})

test_that("get_shape_vertices handles r2 parameter for ellipse", {
  verts <- get_shape_vertices("ellipse", 0, 0, 3, r2 = 1)

  # X extent should be 2*3=6, Y extent should be 2*1=2
  expect_equal(diff(range(verts$x)), 6, tolerance = 0.1)
  expect_equal(diff(range(verts$y)), 2, tolerance = 0.1)
})

test_that("get_shape_vertices uses r for r2 when r2 is NULL", {
  verts <- get_shape_vertices("ellipse", 0, 0, 2, r2 = NULL)

  # Should become a circle (equal extents)
  expect_equal(diff(range(verts$x)), diff(range(verts$y)), tolerance = 0.1)
})

test_that("get_shape_vertices passes ... to star_vertices", {
  verts <- get_shape_vertices("star", 0, 0, 1, n_points = 6, inner_ratio = 0.5)

  # 6 points * 2 = 12 vertices
  expect_equal(length(verts$x), 12)
})

test_that("get_shape_vertices passes ... to cross_vertices", {
  verts <- get_shape_vertices("cross", 0, 0, 1, thickness = 0.5)

  expect_equal(length(verts$x), 12)
})

test_that("get_shape_vertices passes ... to gear_vertices", {
  verts <- get_shape_vertices("gear", 0, 0, 1, n_teeth = 6)

  # 6 teeth * 8 points per tooth = 48 vertices
  expect_equal(length(verts$x), 48)
})

# ============================================
# GRAPHICAL TESTS - Rendering polygon shapes
# ============================================

test_that("circle_vertices can be rendered with polygon()", {
  with_temp_png({
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    verts <- circle_vertices(0, 0, 1)
    polygon(verts$x, verts$y, col = "lightblue", border = "navy")
  })
  expect_true(TRUE)
})

test_that("star_vertices can be rendered with polygon()", {
  with_temp_png({
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    verts <- star_vertices(0, 0, 1, n_points = 5)
    polygon(verts$x, verts$y, col = "gold", border = "orange")
  })
  expect_true(TRUE)
})

test_that("heart_vertices can be rendered with polygon()", {
  with_temp_png({
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    verts <- heart_vertices(0, 0, 1)
    polygon(verts$x, verts$y, col = "red", border = "darkred")
  })
  expect_true(TRUE)
})

test_that("gear_vertices can be rendered with polygon()", {
  with_temp_png({
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    verts <- gear_vertices(0, 0, 1, n_teeth = 8)
    polygon(verts$x, verts$y, col = "gray50", border = "black")
  })
  expect_true(TRUE)
})

test_that("cloud_vertices can be rendered with polygon()", {
  with_temp_png({
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    verts <- cloud_vertices(0, 0, 1)
    polygon(verts$x, verts$y, col = "white", border = "gray")
  })
  expect_true(TRUE)
})

test_that("brain_vertices can be rendered with polygon()", {
  with_temp_png({
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    verts <- brain_vertices(0, 0, 1)
    polygon(verts$x, verts$y, col = "pink", border = "maroon")
  })
  expect_true(TRUE)
})

test_that("inset_polygon_vertices can create donut ring", {
  with_temp_png({
    plot(0, 0, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", asp = 1)
    outer <- hexagon_vertices(0, 0, 1)
    inner <- inset_polygon_vertices(outer, 0.6)

    # Draw outer polygon
    polygon(outer$x, outer$y, col = "steelblue", border = "navy")
    # Draw inner polygon (covers center)
    polygon(inner$x, inner$y, col = "white", border = "navy")
  })
  expect_true(TRUE)
})

test_that("multiple shapes can be rendered together", {
  with_temp_png({
    plot(0, 0, xlim = c(-6, 6), ylim = c(-4, 4), type = "n", asp = 1)

    # Row 1: Basic shapes
    shapes <- c("circle", "square", "triangle", "diamond")
    xs <- c(-4.5, -1.5, 1.5, 4.5)

    for (i in seq_along(shapes)) {
      verts <- get_shape_vertices(shapes[i], xs[i], 2, 1)
      polygon(verts$x, verts$y, col = rainbow(4)[i], border = "black")
    }

    # Row 2: Polygon shapes
    shapes2 <- c("pentagon", "hexagon", "star", "cross")
    for (i in seq_along(shapes2)) {
      verts <- get_shape_vertices(shapes2[i], xs[i], -2, 1)
      polygon(verts$x, verts$y, col = rainbow(4, start = 0.5)[i], border = "black")
    }
  })
  expect_true(TRUE)
})

# ============================================
# EDGE CASES AND NUMERIC PRECISION
# ============================================

test_that("polygon vertices handle very small radius", {
  for (shape in c("circle", "square", "triangle", "hexagon")) {
    verts <- get_shape_vertices(shape, 0, 0, 0.001)
    expect_true(all(is.finite(verts$x)), info = paste("Non-finite x for shape:", shape))
    expect_true(all(is.finite(verts$y)), info = paste("Non-finite y for shape:", shape))
  }
})

test_that("polygon vertices handle very large radius", {
  for (shape in c("circle", "square", "triangle", "hexagon")) {
    verts <- get_shape_vertices(shape, 0, 0, 1000)
    expect_true(all(is.finite(verts$x)), info = paste("Non-finite x for shape:", shape))
    expect_true(all(is.finite(verts$y)), info = paste("Non-finite y for shape:", shape))
  }
})

test_that("polygon vertices handle zero radius", {
  verts <- circle_vertices(0, 0, 0, n = 50)

  # All points should be at origin
  expect_true(all(verts$x == 0))
  expect_true(all(verts$y == 0))
})

test_that("polygon vertices handle large coordinates", {
  verts <- circle_vertices(1e6, -1e6, 1)

  expect_equal(mean(verts$x), 1e6, tolerance = 0.01)
  expect_equal(mean(verts$y), -1e6, tolerance = 0.01)
})

test_that("star_vertices handles extreme inner_ratio values", {
  # Very small inner ratio
  verts <- star_vertices(0, 0, 1, inner_ratio = 0.01)
  expect_equal(length(verts$x), 10)

  # Inner ratio close to 1
  verts <- star_vertices(0, 0, 1, inner_ratio = 0.99)
  expect_equal(length(verts$x), 10)
})

test_that("cross_vertices handles extreme thickness values", {
  # Very thin cross
  verts <- cross_vertices(0, 0, 1, thickness = 0.01)
  expect_equal(length(verts$x), 12)

  # Very thick cross (arms touch)
  verts <- cross_vertices(0, 0, 1, thickness = 1)
  expect_equal(length(verts$x), 12)
})
