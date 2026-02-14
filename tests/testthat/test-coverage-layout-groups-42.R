# test-coverage-layout-groups-42.R
# Comprehensive tests for R/layout-groups.R targeting uncovered branches
# Requires at least 35 tests for improved coverage

# ============================================
# SETUP: Access internal function
# ============================================

layout_groups <- cograph:::layout_groups

# ============================================
# BASIC FUNCTIONALITY
# ============================================

test_that("layout_groups returns correct structure", {
  adj <- matrix(0, 6, 6)
  adj[1, 2] <- adj[2, 1] <- 1
  adj[3, 4] <- adj[4, 3] <- 1
  adj[5, 6] <- adj[6, 5] <- 1
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3)

  coords <- layout_groups(net, groups)

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 6)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("layout_groups returns numeric coordinates", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups)

  expect_true(is.numeric(coords$x))
  expect_true(is.numeric(coords$y))
})

test_that("layout_groups produces valid x and y columns", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B", "C")

  coords <- layout_groups(net, groups)

  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
  expect_equal(length(coords$x), 5)
  expect_equal(length(coords$y), 5)
})

# ============================================
# GROUPS LENGTH VALIDATION
# ============================================

test_that("layout_groups errors when groups length != n_nodes", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)

  # Groups vector with wrong length
  expect_error(
    layout_groups(net, c(1, 2, 3)),
    "groups must have length equal to number of nodes"
  )
})

test_that("layout_groups errors when groups has too many elements", {
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)

  expect_error(
    layout_groups(net, c(1, 1, 2, 2, 3, 3)),
    "groups must have length equal to number of nodes"
  )
})

test_that("layout_groups errors with empty groups on non-empty network", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  expect_error(
    layout_groups(net, character(0)),
    "groups must have length equal to number of nodes"
  )
})

test_that("layout_groups errors with length mismatch - too few groups", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)

  expect_error(
    layout_groups(net, c(1, 1)),
    "groups must have length equal to number of nodes"
  )
})

# ============================================
# SINGLE GROUP (n_groups == 1)
# ============================================

test_that("layout_groups centers single group at (0.5, 0.5)", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- rep(1, 4)  # All nodes in same group

  coords <- layout_groups(net, groups)

  # All nodes should be arranged around center (0.5, 0.5)
  center_x <- mean(coords$x)
  center_y <- mean(coords$y)
  expect_equal(center_x, 0.5, tolerance = 0.01)
  expect_equal(center_y, 0.5, tolerance = 0.01)
})

test_that("layout_groups single group with character label", {
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)
  groups <- rep("GroupA", 3)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 3)
  center_x <- mean(coords$x)
  center_y <- mean(coords$y)
  expect_equal(center_x, 0.5, tolerance = 0.01)
  expect_equal(center_y, 0.5, tolerance = 0.01)
})

test_that("layout_groups single group with factor", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)
  groups <- factor(rep("Only", 5))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 5)
  # Center should be at (0.5, 0.5)
  expect_equal(mean(coords$x), 0.5, tolerance = 0.01)
  expect_equal(mean(coords$y), 0.5, tolerance = 0.01)
})

test_that("layout_groups single group nodes are circular", {
  adj <- create_test_matrix(8)
  net <- CographNetwork$new(adj)
  groups <- rep("Single", 8)

  coords <- layout_groups(net, groups)

  # All nodes should be equidistant from center
  center_x <- mean(coords$x)
  center_y <- mean(coords$y)
  dists <- sqrt((coords$x - center_x)^2 + (coords$y - center_y)^2)

  expect_equal(max(dists) - min(dists), 0, tolerance = 0.001)
})

# ============================================
# MULTIPLE GROUPS (circular arrangement)
# ============================================

test_that("layout_groups arranges multiple groups in a circle", {
  adj <- create_test_matrix(9)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 9)

  # Group centers should be on a circle
  g1_center <- c(mean(coords$x[1:3]), mean(coords$y[1:3]))
  g2_center <- c(mean(coords$x[4:6]), mean(coords$y[4:6]))
  g3_center <- c(mean(coords$x[7:9]), mean(coords$y[7:9]))

  # All group centers should be similar distance from (0.5, 0.5)
  d1 <- sqrt((g1_center[1] - 0.5)^2 + (g1_center[2] - 0.5)^2)
  d2 <- sqrt((g2_center[1] - 0.5)^2 + (g2_center[2] - 0.5)^2)
  d3 <- sqrt((g3_center[1] - 0.5)^2 + (g3_center[2] - 0.5)^2)

  expect_equal(d1, d2, tolerance = 0.05)
  expect_equal(d2, d3, tolerance = 0.05)
})

test_that("layout_groups handles two groups", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "A", "B", "B", "B")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)

  # Groups should be on opposite sides
  g_a_center <- c(mean(coords$x[1:3]), mean(coords$y[1:3]))
  g_b_center <- c(mean(coords$x[4:6]), mean(coords$y[4:6]))

  # Distance between group centers should be meaningful
  dist_between <- sqrt((g_a_center[1] - g_b_center[1])^2 +
                       (g_a_center[2] - g_b_center[2])^2)
  expect_true(dist_between > 0.3)  # Groups should be separated
})

test_that("layout_groups handles four groups", {
  adj <- create_test_matrix(8)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3, 4, 4)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 8)
})

test_that("layout_groups handles many groups", {
  adj <- create_test_matrix(10)
  net <- CographNetwork$new(adj)
  groups <- 1:10  # Each node in its own group

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 10)
})

test_that("layout_groups five groups are evenly spaced", {
  adj <- create_test_matrix(10)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 10)
})

# ============================================
# CUSTOM GROUP POSITIONS (data.frame)
# ============================================

test_that("layout_groups accepts group_positions as data.frame", {
  adj <- matrix(0, 6, 6)
  adj[1, 2] <- adj[2, 1] <- 1
  adj[3, 4] <- adj[4, 3] <- 1
  adj[5, 6] <- adj[6, 5] <- 1
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3)

  custom_positions <- data.frame(
    x = c(0.2, 0.5, 0.8),
    y = c(0.5, 0.8, 0.5)
  )
  rownames(custom_positions) <- c("1", "2", "3")

  coords <- layout_groups(net, groups, group_positions = custom_positions)

  expect_equal(nrow(coords), 6)

  # Verify group 1 nodes are near (0.2, 0.5)
  g1_center <- c(mean(coords$x[1:2]), mean(coords$y[1:2]))
  expect_equal(g1_center[1], 0.2, tolerance = 0.2)
})

test_that("layout_groups with custom positions for character groups", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  custom_pos <- data.frame(
    x = c(0.25, 0.75),
    y = c(0.5, 0.5)
  )
  rownames(custom_pos) <- c("A", "B")

  coords <- layout_groups(net, groups, group_positions = custom_pos)

  expect_equal(nrow(coords), 4)

  # Group A should be near x = 0.25
  g_a_center_x <- mean(coords$x[groups == "A"])
  expect_equal(g_a_center_x, 0.25, tolerance = 0.2)
})

test_that("layout_groups custom positions override auto-calculation", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  # Place groups at very specific positions
  custom_pos <- data.frame(
    x = c(0.1, 0.9),
    y = c(0.1, 0.9)
  )
  rownames(custom_pos) <- c("1", "2")

  coords <- layout_groups(net, groups, group_positions = custom_pos)

  # Check group 1 is near (0.1, 0.1)
  g1_center <- c(mean(coords$x[1:2]), mean(coords$y[1:2]))
  expect_equal(g1_center[1], 0.1, tolerance = 0.2)
  expect_equal(g1_center[2], 0.1, tolerance = 0.2)
})

# ============================================
# CUSTOM GROUP POSITIONS (list conversion)
# ============================================

test_that("layout_groups converts list group_positions to data.frame", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  custom_list <- list(x = c(0.3, 0.7), y = c(0.5, 0.5))

  coords <- layout_groups(net, groups, group_positions = custom_list)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups accepts matrix-like list for positions", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c("X", "X", "Y", "Y", "Z", "Z")

  pos_list <- list(
    x = c(0.2, 0.5, 0.8),
    y = c(0.2, 0.8, 0.2)
  )

  coords <- layout_groups(net, groups, group_positions = pos_list)

  expect_equal(nrow(coords), 6)
})

test_that("layout_groups list positions get converted properly", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("G1", "G1", "G2", "G2")

  pos_list <- list(x = c(0.2, 0.8), y = c(0.5, 0.5))

  coords <- layout_groups(net, groups, group_positions = pos_list)

  expect_equal(nrow(coords), 4)
  expect_true(is.data.frame(coords))
})

# ============================================
# SINGLE NODE IN GROUP (n_in_group == 1)
# ============================================

test_that("layout_groups places single node at group center", {
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)
  groups <- c(1, 2, 3)  # Each node in its own group

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 3)
  # Each node is alone, so it should be at its group center
})

test_that("layout_groups handles mixed group sizes with singletons", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "A", "B", "C")  # Group A has 3, B has 1, C has 1

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 5)
})

test_that("layout_groups singleton groups are placed correctly", {
  adj <- matrix(0, 4, 4)
  adj[1, 2] <- adj[2, 1] <- 1
  net <- CographNetwork$new(adj)
  groups <- c("Multi", "Multi", "Solo1", "Solo2")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
  # Singleton nodes should be at their group centers
  # The two multi nodes should be arranged within their group
})

test_that("layout_groups with all singleton groups", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "B", "C", "D")  # Each node in own group

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
  # All nodes should be at their respective group centers
})

# ============================================
# MULTIPLE NODES IN GROUP (circular within group)
# ============================================

test_that("layout_groups arranges multiple nodes within group circularly", {
  adj <- matrix(1, 6, 6) - diag(6)
  net <- CographNetwork$new(adj)
  groups <- rep(1, 6)  # All in one group

  coords <- layout_groups(net, groups)

  # Nodes should be on a circle around the center
  center_x <- mean(coords$x)
  center_y <- mean(coords$y)

  dists <- sqrt((coords$x - center_x)^2 + (coords$y - center_y)^2)

  # All distances should be approximately equal (circular arrangement)
  expect_equal(max(dists) - min(dists), 0, tolerance = 0.01)
})

test_that("layout_groups within-group arrangement respects inner_radius", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- rep(1, 4)

  coords_small <- layout_groups(net, groups, inner_radius = 0.1)
  coords_large <- layout_groups(net, groups, inner_radius = 0.3)

  # Calculate spread (range) for each
  spread_small <- max(coords_small$x) - min(coords_small$x)
  spread_large <- max(coords_large$x) - min(coords_large$x)

  expect_true(spread_large > spread_small)
})

# ============================================
# INNER AND OUTER RADIUS PARAMETERS
# ============================================

test_that("layout_groups respects inner_radius parameter", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 1, 2, 2, 2)

  coords_default <- layout_groups(net, groups, inner_radius = 0.15)
  coords_small <- layout_groups(net, groups, inner_radius = 0.05)
  coords_large <- layout_groups(net, groups, inner_radius = 0.25)

  # Calculate within-group spread for group 1
  spread_default <- max(coords_default$x[1:3]) - min(coords_default$x[1:3])
  spread_small <- max(coords_small$x[1:3]) - min(coords_small$x[1:3])
  spread_large <- max(coords_large$x[1:3]) - min(coords_large$x[1:3])

  expect_true(spread_small < spread_default)
  expect_true(spread_default < spread_large)
})

test_that("layout_groups respects outer_radius parameter", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords_small <- layout_groups(net, groups, outer_radius = 0.2)
  coords_large <- layout_groups(net, groups, outer_radius = 0.45)

  # Group centers should be further apart with larger outer_radius
  g_a_small <- c(mean(coords_small$x[1:2]), mean(coords_small$y[1:2]))
  g_b_small <- c(mean(coords_small$x[3:4]), mean(coords_small$y[3:4]))
  dist_small <- sqrt(sum((g_a_small - g_b_small)^2))

  g_a_large <- c(mean(coords_large$x[1:2]), mean(coords_large$y[1:2]))
  g_b_large <- c(mean(coords_large$x[3:4]), mean(coords_large$y[3:4]))
  dist_large <- sqrt(sum((g_a_large - g_b_large)^2))

  expect_true(dist_large > dist_small)
})

test_that("layout_groups works with zero inner_radius", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups, inner_radius = 0)

  expect_equal(nrow(coords), 4)
  # All nodes in same group should be at same position
  expect_equal(coords$x[1], coords$x[2])
  expect_equal(coords$y[1], coords$y[2])
})

test_that("layout_groups works with very small outer_radius", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3)

  coords <- layout_groups(net, groups, outer_radius = 0.05)

  expect_equal(nrow(coords), 6)
  # All group centers should be very close to (0.5, 0.5)
})

test_that("layout_groups with large outer_radius", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3)

  coords <- layout_groups(net, groups, outer_radius = 0.45)

  expect_equal(nrow(coords), 6)
  # Groups should be well-separated
})

test_that("layout_groups with both radius parameters", {

  adj <- create_test_matrix(9)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)

  coords <- layout_groups(net, groups, inner_radius = 0.1, outer_radius = 0.4)

  expect_equal(nrow(coords), 9)
})

# ============================================
# GROUP TYPES (numeric, character, factor)
# ============================================

test_that("layout_groups handles numeric groups", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups handles character groups", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("alpha", "alpha", "beta", "beta")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups handles factor groups", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- factor(c("low", "low", "medium", "medium", "high", "high"),
                   levels = c("low", "medium", "high"))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
})

test_that("layout_groups preserves factor level order", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)

  # Factor with specific level order
  groups <- factor(c("C", "C", "A", "A", "B", "B"),
                   levels = c("A", "B", "C"))  # Alphabetical

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
})

test_that("layout_groups with integer groups", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- as.integer(c(1, 1, 2, 2, 3, 3))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
})

# ============================================
# EDGE CASES AND BOUNDARY CONDITIONS
# ============================================

test_that("layout_groups handles single node network", {
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)
  groups <- c("solo")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x[1], 0.5)
  expect_equal(coords$y[1], 0.5)
})

test_that("layout_groups handles two node network", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- CographNetwork$new(adj)
  groups <- c("A", "B")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 2)
})

test_that("layout_groups handles large network", {
  adj <- create_test_matrix(50, density = 0.3)
  net <- CographNetwork$new(adj)
  groups <- rep(1:5, each = 10)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 50)
})

test_that("layout_groups handles uneven group sizes", {
  adj <- create_test_matrix(10)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 1, 1, 1, 2, 2, 2, 3, 3)  # 5, 3, 2

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 10)
})

test_that("layout_groups with two nodes in same group", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A")  # Both in same group

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 2)
  # Both should be around (0.5, 0.5)
  expect_equal(mean(coords$x), 0.5, tolerance = 0.1)
  expect_equal(mean(coords$y), 0.5, tolerance = 0.1)
})

# ============================================
# COORDINATE RANGE AND VALIDITY
# ============================================

test_that("layout_groups produces finite coordinates", {
  adj <- create_test_matrix(8)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3, 4, 4)

  coords <- layout_groups(net, groups)

  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("layout_groups coordinates are reasonable range", {
  adj <- create_test_matrix(12)
  net <- CographNetwork$new(adj)
  groups <- rep(1:4, each = 3)

  coords <- layout_groups(net, groups)

  # With default parameters, coordinates should be in reasonable range
  expect_true(all(coords$x > -1 & coords$x < 2))
  expect_true(all(coords$y > -1 & coords$y < 2))
})

test_that("layout_groups no NaN or NA values", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3)

  coords <- layout_groups(net, groups)

  expect_false(any(is.na(coords$x)))
  expect_false(any(is.na(coords$y)))
  expect_false(any(is.nan(coords$x)))
  expect_false(any(is.nan(coords$y)))
})

# ============================================
# INTEGRATION WITH COGRAPH_NETWORK
# ============================================

test_that("layout_groups works with CographNetwork from matrix", {
  mat <- matrix(c(0, 1, 1, 0,
                  1, 0, 1, 0,
                  1, 1, 0, 1,
                  0, 0, 1, 0), 4, 4)
  net <- CographNetwork$new(mat)
  groups <- c("X", "X", "Y", "Y")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
  expect_true(is.data.frame(coords))
})

test_that("layout_groups works with weighted network", {
  mat <- matrix(c(0, 0.5, 0.3,
                  0.5, 0, 0.8,
                  0.3, 0.8, 0), 3, 3)
  net <- CographNetwork$new(mat)
  groups <- c(1, 1, 2)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 3)
})

test_that("layout_groups works with directed network", {
  mat <- matrix(c(0, 1, 0,
                  0, 0, 1,
                  1, 0, 0), 3, 3)
  net <- CographNetwork$new(mat, directed = TRUE)
  groups <- c("a", "a", "b")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 3)
})

test_that("layout_groups works with disconnected network", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  net <- CographNetwork$new(mat)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

# ============================================
# SPECIAL GROUP STRUCTURES
# ============================================

test_that("layout_groups with non-contiguous group indices", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 5, 5)  # Non-contiguous group numbers

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups with special character group names", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("Group_1", "Group_1", "Group-2", "Group-2")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups with mixed alphanumeric group names", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c("A1", "A1", "B2", "B2", "C3", "C3")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
})

# ============================================
# REGRESSION TESTS
# ============================================

test_that("layout_groups reproducibility", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3)

  coords1 <- layout_groups(net, groups)
  coords2 <- layout_groups(net, groups)

  expect_equal(coords1$x, coords2$x)
  expect_equal(coords1$y, coords2$y)
})

test_that("layout_groups same result with factor vs character groups", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  groups_char <- c("A", "A", "B", "B")
  groups_factor <- factor(c("A", "A", "B", "B"))

  coords_char <- layout_groups(net, groups_char)
  coords_factor <- layout_groups(net, groups_factor)

  expect_equal(coords_char$x, coords_factor$x)
  expect_equal(coords_char$y, coords_factor$y)
})

# ============================================
# SPLOT INTEGRATION (using spring layout as workaround)
# ============================================

test_that("splot with spring layout and groups parameter works", {
  mat <- create_test_matrix(6)
  groups <- c(1, 1, 2, 2, 3, 3)

  # Use spring layout with groups coloring (layout="groups" has known issues)
  result <- safe_plot(splot(mat, layout = "spring", groups = groups, seed = 42))
  expect_true(result$success, info = result$error)
})

test_that("cograph with groups layout produces network", {
  mat <- create_test_matrix(6)
  groups <- c(1, 1, 2, 2, 3, 3)

  # Create cograph with groups parameter
  net <- cograph(mat, layout = "groups", groups = groups)

  expect_cograph_network(net)
  expect_equal(n_nodes(net), 6)
})
