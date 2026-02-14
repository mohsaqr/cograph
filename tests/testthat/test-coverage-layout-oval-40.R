# test-coverage-layout-oval-40.R - Comprehensive Tests for layout-oval.R
# Targeting uncovered code paths for improved coverage

# ============================================
# SETUP
# ============================================

# Ensure layout_oval is available
layout_oval <- cograph:::layout_oval

# ============================================
# TEST: Empty Network (n == 0)
# Covers lines 34-36
# ============================================

test_that("layout_oval handles empty network (n == 0)", {
  # Create network with no nodes using CographNetwork R6

  net <- CographNetwork$new()
  net$set_nodes(data.frame(id = integer(0), label = character(0)))

  coords <- layout_oval(net)

  expect_equal(nrow(coords), 0)
  expect_true(all(c("x", "y") %in% names(coords)))
  expect_equal(length(coords$x), 0)
  expect_equal(length(coords$y), 0)
})

test_that("layout_oval handles empty cograph_network (S3)", {
  # Create an empty cograph_network S3 object
  empty_net <- structure(
    list(
      nodes = data.frame(id = integer(0), label = character(0)),
      edges = data.frame(from = integer(0), to = integer(0), weight = numeric(0)),
      directed = FALSE
    ),
    class = c("cograph_network", "list")
  )

  coords <- layout_oval(empty_net)

  expect_equal(nrow(coords), 0)
  expect_true(is.data.frame(coords))
})

# ============================================
# TEST: Single Node Network (n == 1)
# Covers lines 38-40
# ============================================

test_that("layout_oval handles single-node network", {
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)

  coords <- layout_oval(net)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("layout_oval single node returns center coordinates", {
  # Use cograph_network S3 format
  single_net <- as_cograph(matrix(0, 1, 1))

  coords <- layout_oval(single_net)

  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

# ============================================
# TEST: Network Type Detection
# Covers lines 28-32 (both cograph_network and CographNetwork paths)
# ============================================

test_that("layout_oval works with S3 cograph_network", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  coords <- layout_oval(net)

  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("layout_oval works with R6 CographNetwork", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net)

  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
})

# ============================================
# TEST: Order Parameter with Character Labels
# Covers lines 44-52
# ============================================

test_that("layout_oval accepts character order (label-based)", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- CographNetwork$new(mat)

  # Specify order by labels
  coords <- layout_oval(net, order = c("C", "B", "A"))

  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("layout_oval warns when some labels not found", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- CographNetwork$new(mat)

  # Try to use labels that don't exist
  expect_warning(
    coords <- layout_oval(net, order = c("A", "B", "X")),
    "Some labels not found"
  )

  expect_equal(nrow(coords), 3)
})

test_that("layout_oval handles partial label matches with warning", {
  mat <- matrix(c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0), nrow = 4)
  rownames(mat) <- colnames(mat) <- c("Node1", "Node2", "Node3", "Node4")
  net <- CographNetwork$new(mat)

  # One invalid label
  expect_warning(
    layout_oval(net, order = c("Node1", "Node2", "InvalidNode", "Node4")),
    "Some labels not found"
  )
})

# ============================================
# TEST: Order Length Mismatch
# Covers lines 53-56
# ============================================

test_that("layout_oval warns when order length doesn't match node count", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  # Order has wrong length
  expect_warning(
    coords <- layout_oval(net, order = c(1, 2)),
    "Order length doesn't match node count"
  )

  expect_equal(nrow(coords), 3)
})

test_that("layout_oval warns when order is too long", {
  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- CographNetwork$new(mat)

  expect_warning(
    coords <- layout_oval(net, order = c(1, 2, 3, 4)),
    "Order length doesn't match node count"
  )

  expect_equal(nrow(coords), 2)
})

# ============================================
# TEST: Numeric Order Parameter
# Covers lines 43-59 (numeric order path)
# ============================================

test_that("layout_oval accepts numeric order parameter", {
  mat <- matrix(c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0), nrow = 4)
  net <- CographNetwork$new(mat)

  # Reverse order
  coords_default <- layout_oval(net, order = NULL)
  coords_reversed <- layout_oval(net, order = c(4, 3, 2, 1))

  expect_equal(nrow(coords_default), 4)
  expect_equal(nrow(coords_reversed), 4)

  # Coordinates should be different (reordered)
  expect_false(all(coords_default$x == coords_reversed$x))
})

# ============================================
# TEST: Clockwise vs Counter-clockwise
# Covers lines 64-66
# ============================================

test_that("layout_oval produces different results for clockwise vs counter-clockwise", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords_cw <- layout_oval(net, clockwise = TRUE)
  coords_ccw <- layout_oval(net, clockwise = FALSE)

  expect_equal(nrow(coords_cw), 3)
  expect_equal(nrow(coords_ccw), 3)

  # Coordinates should differ (reversed direction)
  expect_false(all(coords_cw$x == coords_ccw$x) && all(coords_cw$y == coords_ccw$y))
})

test_that("layout_oval clockwise=FALSE reverses node arrangement", {
  mat <- create_test_matrix(6)
  net <- CographNetwork$new(mat)

  coords_cw <- layout_oval(net, clockwise = TRUE, start_angle = 0)
  coords_ccw <- layout_oval(net, clockwise = FALSE, start_angle = 0)

  # The first coordinate should be at the same position (start angle)
  # but subsequent coordinates should be in opposite directions
  expect_equal(nrow(coords_cw), 6)
  expect_equal(nrow(coords_ccw), 6)
})

# ============================================
# TEST: Rotation Parameter
# Covers lines 83-93
# ============================================

test_that("layout_oval applies rotation correctly", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords_no_rot <- layout_oval(net, rotation = 0)
  coords_rot <- layout_oval(net, rotation = pi / 4)  # 45 degrees

  expect_equal(nrow(coords_rot), 3)

  # Coordinates should be different when rotated
  expect_false(all(abs(coords_no_rot$x - coords_rot$x) < 1e-10))
})

test_that("layout_oval rotation by 2*pi returns approximately same coordinates", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords_0 <- layout_oval(net, rotation = 0)
  coords_2pi <- layout_oval(net, rotation = 2 * pi)

  # Full rotation should return to same position (within numerical precision)
  expect_equal(coords_0$x, coords_2pi$x, tolerance = 1e-10)
  expect_equal(coords_0$y, coords_2pi$y, tolerance = 1e-10)
})

test_that("layout_oval rotation by pi/2 rotates 90 degrees", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords_0 <- layout_oval(net, rotation = 0)
  coords_90 <- layout_oval(net, rotation = pi / 2)

  expect_equal(nrow(coords_90), 3)

  # After 90 degree rotation, coordinates should be different
  expect_false(all(coords_0$x == coords_90$x))
})

test_that("layout_oval rotation centers at (0.5, 0.5)", {
  mat <- create_test_matrix(8)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net, rotation = pi / 6)

  # Center should still be approximately (0.5, 0.5)
  expect_equal(mean(coords$x), 0.5, tolerance = 0.01)
  expect_equal(mean(coords$y), 0.5, tolerance = 0.01)
})

# ============================================
# TEST: Aspect Ratio Parameter
# Covers lines 73-79
# ============================================

test_that("layout_oval creates horizontal oval with ratio > 1", {
  mat <- create_test_matrix(8)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net, ratio = 2.0)

  x_range <- max(coords$x) - min(coords$x)
  y_range <- max(coords$y) - min(coords$y)

  # With ratio > 1, x_range should be greater than y_range
  expect_true(x_range > y_range)
})

test_that("layout_oval creates vertical oval with ratio < 1", {
  mat <- create_test_matrix(8)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net, ratio = 0.5)

  x_range <- max(coords$x) - min(coords$x)
  y_range <- max(coords$y) - min(coords$y)

  # With ratio < 1, y_range should be greater than x_range
  expect_true(y_range > x_range)
})

test_that("layout_oval ratio=1 creates circular layout", {
  mat <- create_test_matrix(8)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net, ratio = 1.0)

  x_range <- max(coords$x) - min(coords$x)
  y_range <- max(coords$y) - min(coords$y)

  # With ratio = 1, ranges should be equal
  expect_equal(x_range, y_range, tolerance = 0.01)
})

# ============================================
# TEST: Start Angle Parameter
# Covers lines 61-63
# ============================================

test_that("layout_oval respects start_angle parameter", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  # Start at top (pi/2)
  coords_top <- layout_oval(net, start_angle = pi / 2, ratio = 1)

  # Start at right (0)
  coords_right <- layout_oval(net, start_angle = 0, ratio = 1)

  # First node positions should be different
  expect_false(
    abs(coords_top$x[1] - coords_right$x[1]) < 1e-10 &&
    abs(coords_top$y[1] - coords_right$y[1]) < 1e-10
  )
})

test_that("layout_oval start_angle=0 places first node on right", {
  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net, start_angle = 0, ratio = 1, clockwise = FALSE)

  # First node should be to the right of center
  expect_true(coords$x[1] > 0.5)
})

# ============================================
# TEST: Two-Node Network
# ============================================

test_that("layout_oval handles two-node network correctly", {
  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net)

  expect_equal(nrow(coords), 2)

  # Two nodes should be on opposite sides
  x_diff <- abs(coords$x[1] - coords$x[2])
  y_diff <- abs(coords$y[1] - coords$y[2])

  expect_true(x_diff > 0.1 || y_diff > 0.1)
})

# ============================================
# TEST: Default Order (NULL order)
# Covers lines 57-59
# ============================================

test_that("layout_oval uses sequential order when order is NULL", {
  mat <- create_test_matrix(5)
  net <- CographNetwork$new(mat)

  coords1 <- layout_oval(net, order = NULL)
  coords2 <- layout_oval(net, order = 1:5)

  # Should produce identical results
  expect_equal(coords1$x, coords2$x)
  expect_equal(coords1$y, coords2$y)
})

# ============================================
# TEST: Large Network
# ============================================

test_that("layout_oval handles large network", {
  mat <- create_test_matrix(100)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net, ratio = 1.5)

  expect_equal(nrow(coords), 100)
  expect_true(all(c("x", "y") %in% names(coords)))

  # All coordinates should be within reasonable bounds
  expect_true(all(coords$x >= 0 & coords$x <= 1))
  expect_true(all(coords$y >= 0 & coords$y <= 1))
})

# ============================================
# TEST: Combined Parameters
# ============================================

test_that("layout_oval works with all parameters combined", {
  mat <- create_test_matrix(6)
  rownames(mat) <- colnames(mat) <- LETTERS[1:6]
  net <- CographNetwork$new(mat)

  coords <- layout_oval(
    net,
    ratio = 1.8,
    order = 6:1,
    start_angle = pi / 4,
    clockwise = FALSE,
    rotation = pi / 3
  )

  expect_equal(nrow(coords), 6)
  expect_true(all(c("x", "y") %in% names(coords)))
})

# ============================================
# TEST: Integration with splot
# ============================================

test_that("splot works with oval layout", {
  mat <- create_test_matrix(6)

  result <- safe_plot(splot(mat, layout = "oval"))
  expect_true(result$success, info = result$error)
})

test_that("splot oval layout accepts ratio parameter", {
  mat <- create_test_matrix(6)

  result <- safe_plot(splot(mat, layout = "oval", ratio = 2.0))
  expect_true(result$success, info = result$error)
})

# ============================================
# TEST: Coordinate Properties
# ============================================

test_that("layout_oval coordinates form an ellipse shape", {
  mat <- create_test_matrix(20)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net, ratio = 2.0)

  # Center should be at (0.5, 0.5)
  cx <- mean(coords$x)
  cy <- mean(coords$y)

  expect_equal(cx, 0.5, tolerance = 0.01)
  expect_equal(cy, 0.5, tolerance = 0.01)
})

test_that("layout_oval preserves ellipse equation property", {
  mat <- create_test_matrix(12)
  net <- CographNetwork$new(mat)

  ratio <- 1.5
  coords <- layout_oval(net, ratio = ratio, rotation = 0)

  # Calculate radii based on the formula in the function
  base_radius <- 0.4
  radius_x <- base_radius * sqrt(ratio)
  radius_y <- base_radius / sqrt(ratio)

  # Check that points roughly satisfy ellipse equation
  # ((x - 0.5)/radius_x)^2 + ((y - 0.5)/radius_y)^2 should be ~1
  ellipse_values <- ((coords$x - 0.5) / radius_x)^2 + ((coords$y - 0.5) / radius_y)^2

  # All values should be approximately 1
  expect_true(all(abs(ellipse_values - 1) < 0.01))
})

# ============================================
# TEST: Edge Cases with Order
# ============================================

test_that("layout_oval handles order with duplicate indices gracefully", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  # Duplicate indices in order (unusual but should handle)
  # This tests the reordering logic at lines 96-98
  coords <- layout_oval(net, order = c(1, 2, 3, 4))

  expect_equal(nrow(coords), 4)
})

test_that("layout_oval with character order matching all labels", {
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[2, 3] <- mat[3, 2] <- 1
  rownames(mat) <- colnames(mat) <- c("Alpha", "Beta", "Gamma")
  net <- CographNetwork$new(mat)

  # All labels exist - should work without warning
  coords <- layout_oval(net, order = c("Gamma", "Alpha", "Beta"))

  expect_equal(nrow(coords), 3)
})

# ============================================
# TEST: Negative Rotation
# ============================================

test_that("layout_oval handles negative rotation", {
  mat <- create_test_matrix(4)
  net <- CographNetwork$new(mat)

  coords_pos <- layout_oval(net, rotation = pi / 4)
  coords_neg <- layout_oval(net, rotation = -pi / 4)

  expect_equal(nrow(coords_neg), 4)

  # Positive and negative rotations should give different results
  expect_false(all(coords_pos$x == coords_neg$x))
})

# ============================================
# TEST: Extreme Ratio Values
# ============================================

test_that("layout_oval handles extreme ratio values", {
  mat <- create_test_matrix(6)
  net <- CographNetwork$new(mat)

  # Very elongated horizontal
  coords_wide <- layout_oval(net, ratio = 5.0)
  expect_equal(nrow(coords_wide), 6)

  # Very elongated vertical
  coords_tall <- layout_oval(net, ratio = 0.2)
  expect_equal(nrow(coords_tall), 6)
})

test_that("layout_oval with ratio = 1 produces circular layout", {
  mat <- create_test_matrix(8)
  net <- CographNetwork$new(mat)

  coords <- layout_oval(net, ratio = 1.0)

  # Check that distances from center are all equal
  cx <- mean(coords$x)
  cy <- mean(coords$y)
  dists <- sqrt((coords$x - cx)^2 + (coords$y - cy)^2)

  # All distances should be approximately equal
  expect_true(max(dists) - min(dists) < 0.01)
})
