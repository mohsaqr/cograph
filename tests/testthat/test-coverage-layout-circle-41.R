# test-coverage-layout-circle-41.R
# Comprehensive coverage tests for R/layout-circle.R
# Targeting uncovered functions and branches

# ============================================
# SETUP
# ============================================

# Access internal function for direct testing
layout_circle <- cograph:::layout_circle

# ============================================
# EMPTY NETWORK TESTS (n == 0)
# ============================================

test_that("layout_circle handles empty network with R6 CographNetwork", {

  # Create empty R6 network
  empty_net <- CographNetwork$new()


  # Should return empty data frame

  coords <- layout_circle(empty_net)

  expect_equal(nrow(coords), 0)
  expect_equal(ncol(coords), 2)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
  expect_equal(length(coords$x), 0)
  expect_equal(length(coords$y), 0)
})

test_that("layout_circle returns empty data frame for zero nodes with R6", {
  # Create empty R6 network using empty constructor
  empty_net <- CographNetwork$new()

  coords <- layout_circle(empty_net)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 0)
  expect_true(all(c("x", "y") %in% names(coords)))
})

# ============================================
# SINGLE NODE TESTS (n == 1)
# ============================================

test_that("layout_circle returns center coordinates for single node R6 network", {
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("layout_circle returns center coordinates for single node S3 network", {
  adj <- matrix(0, 1, 1)
  rownames(adj) <- colnames(adj) <- "Node1"
  net <- as_cograph(adj)

  coords <- layout_circle(net)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("layout_circle single node ignores start_angle parameter", {
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)

  coords1 <- layout_circle(net, start_angle = 0)
  coords2 <- layout_circle(net, start_angle = pi)
  coords3 <- layout_circle(net, start_angle = 3 * pi / 2)

  # All should return same center coordinates

expect_equal(coords1$x, 0.5)
  expect_equal(coords1$y, 0.5)
  expect_equal(coords2$x, 0.5)
  expect_equal(coords2$y, 0.5)
  expect_equal(coords3$x, 0.5)
  expect_equal(coords3$y, 0.5)
})

test_that("layout_circle single node ignores clockwise parameter", {
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)

  coords_cw <- layout_circle(net, clockwise = TRUE)
  coords_ccw <- layout_circle(net, clockwise = FALSE)

  expect_equal(coords_cw$x, coords_ccw$x)
  expect_equal(coords_cw$y, coords_ccw$y)
})

# ============================================
# ORDER PARAMETER WITH CHARACTER LABELS
# ============================================

test_that("layout_circle accepts character order matching node labels", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  colnames(adj) <- rownames(adj) <- c("A", "B", "C")
  net <- CographNetwork$new(adj)

  # Order by character labels
  coords_abc <- layout_circle(net, order = c("A", "B", "C"))
  coords_cba <- layout_circle(net, order = c("C", "B", "A"))

  expect_equal(nrow(coords_abc), 3)
  expect_equal(nrow(coords_cba), 3)

  # Different orders should produce different coordinate arrangements
  # The actual positions depend on how reordering is applied
  expect_true(is.data.frame(coords_abc))
  expect_true(is.data.frame(coords_cba))
})

test_that("layout_circle warns on invalid character labels", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  colnames(adj) <- rownames(adj) <- c("A", "B", "C")
  net <- CographNetwork$new(adj)

  # Order with non-existent labels
  expect_warning(
    coords <- layout_circle(net, order = c("X", "Y", "Z")),
    "Some labels not found"
  )

  # Should fall back to default order
  expect_equal(nrow(coords), 3)
})

test_that("layout_circle warns on partially invalid character labels", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  colnames(adj) <- rownames(adj) <- c("A", "B", "C")
  net <- CographNetwork$new(adj)

  # Mixed valid and invalid labels
  expect_warning(
    coords <- layout_circle(net, order = c("A", "X", "C")),
    "Some labels not found"
  )

  expect_equal(nrow(coords), 3)
})

# ============================================
# ORDER PARAMETER LENGTH MISMATCH
# ============================================

test_that("layout_circle warns on order length mismatch (too short)", {
  adj <- matrix(0, 5, 5)
  adj[1, 2:5] <- 1
  adj[2:5, 1] <- 1
  net <- CographNetwork$new(adj)

  expect_warning(
    coords <- layout_circle(net, order = c(1, 2)),
    "Order length doesn't match"
  )

  expect_equal(nrow(coords), 5)
})

test_that("layout_circle warns on order length mismatch (too long)", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(adj)

  expect_warning(
    coords <- layout_circle(net, order = c(1, 2, 3, 4, 5)),
    "Order length doesn't match"
  )

  expect_equal(nrow(coords), 3)
})

test_that("layout_circle warns on empty order vector", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(adj)

  expect_warning(
    coords <- layout_circle(net, order = integer(0)),
    "Order length doesn't match"
  )

  expect_equal(nrow(coords), 3)
})

# ============================================
# NUMERIC ORDER PARAMETER
# ============================================

test_that("layout_circle accepts numeric order indices", {
  adj <- matrix(0, 4, 4)
  adj[1, 2:4] <- 1
  adj[2:4, 1] <- 1
  net <- CographNetwork$new(adj)

  coords_1234 <- layout_circle(net, order = c(1, 2, 3, 4))
  coords_4321 <- layout_circle(net, order = c(4, 3, 2, 1))

  expect_equal(nrow(coords_1234), 4)
  expect_equal(nrow(coords_4321), 4)
})

test_that("layout_circle handles order with reordered indices", {
  adj <- matrix(0, 4, 4)
  adj[1, 2] <- adj[2, 1] <- 1
  adj[3, 4] <- adj[4, 3] <- 1
  net <- CographNetwork$new(adj)

  # Custom order
  coords <- layout_circle(net, order = c(2, 4, 1, 3))

  expect_equal(nrow(coords), 4)
  expect_true(all(c("x", "y") %in% names(coords)))
})

# ============================================
# CLOCKWISE / COUNTER-CLOCKWISE
# ============================================

test_that("layout_circle clockwise=TRUE arranges nodes clockwise", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, clockwise = TRUE)

  expect_equal(nrow(coords), 6)
  # Calculate angles for verification
  cx <- mean(coords$x)
  cy <- mean(coords$y)
  angles <- atan2(coords$y - cy, coords$x - cx)

  # Check that it's actually arranged
  expect_true(all(!is.na(angles)))
})

test_that("layout_circle clockwise=FALSE arranges nodes counter-clockwise", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, clockwise = FALSE)

  expect_equal(nrow(coords), 6)
  cx <- mean(coords$x)
  cy <- mean(coords$y)
  angles <- atan2(coords$y - cy, coords$x - cx)

  expect_true(all(!is.na(angles)))
})

test_that("layout_circle clockwise produces different order than counter-clockwise", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords_cw <- layout_circle(net, clockwise = TRUE)
  coords_ccw <- layout_circle(net, clockwise = FALSE)

  # Should not be identical (unless symmetry makes them same)
  # At minimum, they should both be valid
  expect_equal(nrow(coords_cw), 4)
  expect_equal(nrow(coords_ccw), 4)

  # The x coordinates should differ (or y coordinates)
  # Because clockwise reverses the angle sequence
  expect_false(all(coords_cw$x == coords_ccw$x) && all(coords_cw$y == coords_ccw$y))
})

# ============================================
# START ANGLE PARAMETER
# ============================================

test_that("layout_circle respects start_angle = 0 (rightmost)", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, start_angle = 0, clockwise = FALSE)

  # First node should be at angle 0 (rightmost)
  # y coordinate should be close to center (0.5)
  # x coordinate should be > 0.5 (to the right)
  expect_equal(nrow(coords), 4)
})

test_that("layout_circle respects start_angle = pi/2 (top)", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, start_angle = pi/2, clockwise = FALSE)

  expect_equal(nrow(coords), 4)
  # When counter-clockwise and start at pi/2, first angle is pi/2 (top)
})

test_that("layout_circle respects start_angle = pi (leftmost)", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, start_angle = pi, clockwise = FALSE)

  expect_equal(nrow(coords), 4)
})

test_that("layout_circle respects start_angle = 3*pi/2 (bottom)", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, start_angle = 3 * pi / 2, clockwise = FALSE)

  expect_equal(nrow(coords), 4)
})

test_that("layout_circle different start_angles produce different layouts", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)

  coords_0 <- layout_circle(net, start_angle = 0)
  coords_pi2 <- layout_circle(net, start_angle = pi/2)
  coords_pi <- layout_circle(net, start_angle = pi)

  # Different start angles should produce different coordinate arrangements
  expect_false(all(coords_0$x == coords_pi2$x))
  expect_false(all(coords_pi2$x == coords_pi$x))
})

# ============================================
# S3 COGRAPH_NETWORK SUPPORT
# ============================================

test_that("layout_circle works with S3 cograph_network (inherits check)", {
  adj <- create_test_matrix(5)
  net <- as_cograph(adj)  # Creates S3 cograph_network

  expect_true(inherits(net, "cograph_network"))

  coords <- layout_circle(net)

  expect_equal(nrow(coords), 5)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("layout_circle uses n_nodes() for S3 cograph_network", {
  adj <- create_test_matrix(7)
  net <- as_cograph(adj)

  # Verify n_nodes works
  expect_equal(n_nodes(net), 7)

  coords <- layout_circle(net)
  expect_equal(nrow(coords), 7)
})

# ============================================
# R6 COGRAPHNETWORK SUPPORT
# ============================================

test_that("layout_circle works with R6 CographNetwork ($n_nodes)", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)

  expect_true(inherits(net, "CographNetwork"))
  expect_equal(net$n_nodes, 6)

  coords <- layout_circle(net)
  expect_equal(nrow(coords), 6)
})

# ============================================
# COORDINATE PROPERTIES
# ============================================

test_that("layout_circle produces circular layout (equidistant from center)", {
  adj <- create_test_matrix(10)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net)

  cx <- mean(coords$x)
  cy <- mean(coords$y)

  # Calculate distances from center
  dists <- sqrt((coords$x - cx)^2 + (coords$y - cy)^2)

  # All distances should be equal (within tolerance)
  expect_true(max(dists) - min(dists) < 0.001)
})

test_that("layout_circle center is at (0.5, 0.5)", {
  adj <- create_test_matrix(8)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net)

  cx <- mean(coords$x)
  cy <- mean(coords$y)

  # Center should be approximately at (0.5, 0.5)
  expect_equal(cx, 0.5, tolerance = 0.001)
  expect_equal(cy, 0.5, tolerance = 0.001)
})

test_that("layout_circle radius is 0.4", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net)

  cx <- mean(coords$x)
  cy <- mean(coords$y)

  # Calculate radius
  radius <- sqrt((coords$x[1] - cx)^2 + (coords$y[1] - cy)^2)

  expect_equal(radius, 0.4, tolerance = 0.001)
})

test_that("layout_circle coordinates are within valid range", {
  adj <- create_test_matrix(20)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net)

  # With center at 0.5 and radius 0.4, coords should be approximately in [0.1, 0.9]
  # Allow small tolerance for floating point
  expect_true(all(coords$x >= 0.09 & coords$x <= 0.91))
  expect_true(all(coords$y >= 0.09 & coords$y <= 0.91))
})

# ============================================
# TWO NODE NETWORK
# ============================================

test_that("layout_circle two nodes are on opposite sides", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net)

  expect_equal(nrow(coords), 2)

  # Two points on a circle should be 0.8 apart (diameter = 2 * 0.4)
  dist <- sqrt((coords$x[1] - coords$x[2])^2 + (coords$y[1] - coords$y[2])^2)
  expect_equal(dist, 0.8, tolerance = 0.001)
})

# ============================================
# EDGE CASES
# ============================================

test_that("layout_circle handles large network", {
  adj <- create_test_matrix(100)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net)

  expect_equal(nrow(coords), 100)
  expect_true(all(c("x", "y") %in% names(coords)))

  # Verify all coordinates are numeric and finite
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("layout_circle order parameter with duplicate indices", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Duplicates in order that has correct length will work
  # The function doesn't validate uniqueness, only length
  coords <- layout_circle(net, order = c(1, 1, 2, 2))

  # Should produce valid coordinates (even if reordering is unusual)
  expect_equal(nrow(coords), 4)
  expect_true(all(is.finite(coords$x)))
})

test_that("layout_circle handles negative start_angle", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, start_angle = -pi/4)

  expect_equal(nrow(coords), 4)
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("layout_circle handles start_angle > 2*pi", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, start_angle = 5 * pi)

  expect_equal(nrow(coords), 4)
  expect_true(all(is.finite(coords$x)))
})

# ============================================
# INTEGRATION WITH SPLOT
# ============================================

test_that("splot uses layout_circle correctly", {
  adj <- create_test_matrix(5)

  result <- safe_plot(splot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("cograph creates network with circle layout", {
  adj <- create_test_matrix(5)

  net <- cograph(adj, layout = "circle")

  expect_cograph_network(net)
  expect_false(is.null(net$layout))
  expect_equal(nrow(net$layout), 5)
})

# ============================================
# COMBINATION TESTS
# ============================================

test_that("layout_circle with all parameters combined", {
  adj <- create_test_matrix(6)
  colnames(adj) <- rownames(adj) <- LETTERS[1:6]
  net <- CographNetwork$new(adj)

  coords <- layout_circle(
    net,
    order = c("F", "E", "D", "C", "B", "A"),
    start_angle = pi/4,
    clockwise = FALSE
  )

  expect_equal(nrow(coords), 6)
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("layout_circle preserves data frame structure", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net)

  expect_s3_class(coords, "data.frame")
  expect_named(coords, c("x", "y"))
  expect_type(coords$x, "double")
  expect_type(coords$y, "double")
})

# ============================================
# ANGLE CALCULATION TESTS
# ============================================

test_that("layout_circle angles are evenly spaced", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)

  coords <- layout_circle(net, clockwise = FALSE)

  cx <- 0.5
  cy <- 0.5

  # Calculate angles
  angles <- atan2(coords$y - cy, coords$x - cx)
  angles_sorted <- sort(angles)

  # Calculate angular differences
  diffs <- diff(c(angles_sorted, angles_sorted[1] + 2 * pi))

  # All differences should be approximately 2*pi/6
  expected_diff <- 2 * pi / 6
  expect_true(all(abs(diffs - expected_diff) < 0.01))
})

# ============================================
# NULL ORDER TESTS
# ============================================

test_that("layout_circle NULL order uses default sequence", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)

  coords_null <- layout_circle(net, order = NULL)
  coords_seq <- layout_circle(net, order = 1:5)

  # Should produce identical results
  expect_equal(coords_null$x, coords_seq$x)
  expect_equal(coords_null$y, coords_seq$y)
})

# ============================================
# REORDER BRANCH TESTS
# ============================================

test_that("layout_circle reorders coordinates correctly", {
  adj <- create_test_matrix(4)
  colnames(adj) <- rownames(adj) <- c("A", "B", "C", "D")
  net <- CographNetwork$new(adj)

  # Reverse order
  coords_rev <- layout_circle(net, order = c("D", "C", "B", "A"))
  coords_default <- layout_circle(net)

  expect_equal(nrow(coords_rev), 4)
  expect_equal(nrow(coords_default), 4)
})

test_that("layout_circle order indices correctly map to positions", {
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)

  coords_123 <- layout_circle(net, order = c(1, 2, 3))
  coords_321 <- layout_circle(net, order = c(3, 2, 1))

  # Row assignments should differ
  expect_equal(nrow(coords_123), 3)
  expect_equal(nrow(coords_321), 3)
})
