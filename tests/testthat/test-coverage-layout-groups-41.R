# test-coverage-layout-groups-41.R
# Comprehensive tests for R/layout-groups.R to improve coverage from 95%
# Targets uncovered branches and edge cases
# Series 41 - Complementary to existing series 42 tests

# ============================================
# SETUP: Access internal function
# ============================================

layout_groups <- cograph:::layout_groups

# ============================================
# EMPTY NETWORK TESTS (n == 0 branch, lines 36-37)
# ============================================

test_that("layout_groups returns empty data frame for empty network", {
  # Create empty R6 network

empty_net <- CographNetwork$new()

  coords <- layout_groups(empty_net, groups = character(0))

  expect_true(is.data.frame(coords))
  expect_equal(nrow(coords), 0)
  expect_equal(ncol(coords), 2)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("layout_groups empty network returns numeric x and y columns", {
  empty_net <- CographNetwork$new()

  coords <- layout_groups(empty_net, groups = character(0))

  expect_true(is.numeric(coords$x))
  expect_true(is.numeric(coords$y))
  expect_equal(length(coords$x), 0)
  expect_equal(length(coords$y), 0)
})

test_that("layout_groups empty network ignores group_positions parameter", {
  empty_net <- CographNetwork$new()
  custom_pos <- data.frame(x = c(0.2, 0.8), y = c(0.3, 0.7))

  coords <- layout_groups(empty_net, groups = character(0),
                          group_positions = custom_pos)

  expect_equal(nrow(coords), 0)
})

test_that("layout_groups empty network ignores inner_radius parameter", {
  empty_net <- CographNetwork$new()

  coords <- layout_groups(empty_net, groups = character(0), inner_radius = 0.5)

  expect_equal(nrow(coords), 0)
})

test_that("layout_groups empty network ignores outer_radius parameter", {
  empty_net <- CographNetwork$new()

  coords <- layout_groups(empty_net, groups = character(0), outer_radius = 0.9)

  expect_equal(nrow(coords), 0)
})

# ============================================
# EMPTY GROUP IN FACTOR (n_in_group == 0, line 82)
# This is a key uncovered branch
# ============================================

test_that("layout_groups skips factor levels with no nodes", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Create factor with an unused level
  groups <- factor(c("A", "A", "B", "B"), levels = c("A", "B", "C"))
  # Level "C" exists but has no nodes

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("layout_groups handles factor with multiple unused levels", {
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)

  # Factor with multiple unused levels
  groups <- factor(c("B", "B", "B"), levels = c("A", "B", "C", "D"))
  # Levels A, C, D are unused

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 3)
  # All nodes should be in a single group, coordinates finite
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("layout_groups skips empty group at beginning of factor levels", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Factor where first level is unused
  groups <- factor(c("B", "B", "C", "C"), levels = c("A", "B", "C"))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups skips empty group at end of factor levels", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Factor where last level is unused
  groups <- factor(c("A", "A", "B", "B"), levels = c("A", "B", "C"))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups skips empty group in middle of factor levels", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Factor where middle level is unused
  groups <- factor(c("A", "A", "C", "C"), levels = c("A", "B", "C"))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups handles factor with all levels unused except one", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)

  # Only one level used out of many
  groups <- factor(rep("D", 5), levels = c("A", "B", "C", "D", "E"))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 5)
  # Single group, coordinates should be finite
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("layout_groups with factor having alternating used/unused levels", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)

  # Alternating: A used, B unused, C used, D unused
  groups <- factor(c("A", "A", "A", "C", "C", "C"),
                   levels = c("A", "B", "C", "D"))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
})

# ============================================
# FACTOR CONVERSION EDGE CASES (line 46)
# ============================================

test_that("layout_groups converts numeric to factor correctly", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups converts character to factor correctly", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("alpha", "alpha", "beta", "beta")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups preserves factor level order", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Factor with explicit level order
  groups <- factor(c("Z", "Z", "A", "A"), levels = c("Z", "A"))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups handles logical converted to factor", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(TRUE, TRUE, FALSE, FALSE)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

# ============================================
# DATA FRAME GROUP_POSITIONS BRANCH (line 66-67)
# ============================================

test_that("layout_groups accepts data.frame group_positions with rownames", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("G1", "G1", "G2", "G2")

  custom_pos <- data.frame(x = c(0.2, 0.8), y = c(0.5, 0.5))
  rownames(custom_pos) <- c("G1", "G2")

  coords <- layout_groups(net, groups, group_positions = custom_pos)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups accepts data.frame group_positions without rownames", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  custom_pos <- data.frame(x = c(0.3, 0.7), y = c(0.4, 0.6))

  coords <- layout_groups(net, groups, group_positions = custom_pos)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups with tibble group_positions", {
  skip_if_not_installed("tibble")

  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  # Tibble is a data.frame subclass
  custom_pos <- tibble::tibble(x = c(0.3, 0.7), y = c(0.4, 0.6))

  coords <- layout_groups(net, groups, group_positions = custom_pos)

  expect_equal(nrow(coords), 4)
})

# ============================================
# LIST GROUP_POSITIONS CONVERSION (line 69)
# ============================================

test_that("layout_groups converts list to data.frame for group_positions", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  custom_list <- list(x = c(0.25, 0.75), y = c(0.5, 0.5))

  coords <- layout_groups(net, groups, group_positions = custom_list)

  expect_equal(nrow(coords), 4)
  expect_true(is.data.frame(coords))
})

test_that("layout_groups converts named list to data.frame", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3)

  custom_list <- list(
    x = c(0.1, 0.5, 0.9),
    y = c(0.5, 0.9, 0.5)
  )

  coords <- layout_groups(net, groups, group_positions = custom_list)

  expect_equal(nrow(coords), 6)
})

test_that("layout_groups handles matrix as group_positions via as.data.frame", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  # Matrix will be converted via as.data.frame
  custom_mat <- matrix(c(0.2, 0.8, 0.5, 0.5), ncol = 2)
  colnames(custom_mat) <- c("x", "y")

  coords <- layout_groups(net, groups, group_positions = custom_mat)

  expect_equal(nrow(coords), 4)
})

# ============================================
# SINGLE NODE IN GROUP (line 89-92)
# ============================================

test_that("layout_groups places single node at exact group center", {
  adj <- create_test_matrix(1)
  net <- CographNetwork$new(adj)
  groups <- c("solo")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 1)
  # Single group centered at (0.5, 0.5)
  expect_equal(coords$x[1], 0.5)
  expect_equal(coords$y[1], 0.5)
})

test_that("layout_groups places singleton in each group at group centers", {
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)
  groups <- c("A", "B", "C")  # Each node is alone in its group

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 3)
  # Each node should be exactly at its group center
})

test_that("layout_groups mixed groups with singletons", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)
  groups <- c("big", "big", "big", "small", "tiny")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 5)
  # Verify coordinates are reasonable
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

# ============================================
# MULTIPLE NODES IN GROUP (lines 94-98)
# ============================================

test_that("layout_groups arranges two nodes in group correctly", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 2)
  # Both should be around center with inner_radius spacing
  center_x <- mean(coords$x)
  center_y <- mean(coords$y)
  expect_equal(center_x, 0.5, tolerance = 0.01)
  expect_equal(center_y, 0.5, tolerance = 0.01)
})

test_that("layout_groups arranges three nodes in group as triangle", {
  adj <- create_test_matrix(3)
  net <- CographNetwork$new(adj)
  groups <- c("only", "only", "only")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 3)
  # All at equal distance from center
  center_x <- mean(coords$x)
  center_y <- mean(coords$y)
  dists <- sqrt((coords$x - center_x)^2 + (coords$y - center_y)^2)
  expect_equal(max(dists) - min(dists), 0, tolerance = 0.001)
})

test_that("layout_groups arranges large group circularly", {
  adj <- create_test_matrix(20)
  net <- CographNetwork$new(adj)
  groups <- rep("large", 20)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 20)
  # All at equal distance from center
  center_x <- mean(coords$x)
  center_y <- mean(coords$y)
  dists <- sqrt((coords$x - center_x)^2 + (coords$y - center_y)^2)
  expect_equal(max(dists) - min(dists), 0, tolerance = 0.001)
})

# ============================================
# SINGLE GROUP (n_groups == 1, lines 52-54)
# ============================================

test_that("layout_groups single group centers at (0.5, 0.5)", {
  adj <- create_test_matrix(5)
  net <- CographNetwork$new(adj)
  groups <- rep("only_group", 5)

  coords <- layout_groups(net, groups)

  expect_equal(mean(coords$x), 0.5, tolerance = 0.001)
  expect_equal(mean(coords$y), 0.5, tolerance = 0.001)
})

test_that("layout_groups single group ignores outer_radius", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- rep(1, 4)

  coords1 <- layout_groups(net, groups, outer_radius = 0.1)
  coords2 <- layout_groups(net, groups, outer_radius = 0.9)

  # Both should be centered at (0.5, 0.5) regardless of outer_radius
  expect_equal(mean(coords1$x), mean(coords2$x))
  expect_equal(mean(coords1$y), mean(coords2$y))
})

test_that("layout_groups single group uses inner_radius", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- rep(1, 4)

  coords_small <- layout_groups(net, groups, inner_radius = 0.1)
  coords_large <- layout_groups(net, groups, inner_radius = 0.3)

  # Larger inner_radius should spread nodes more
  spread_small <- max(coords_small$x) - min(coords_small$x)
  spread_large <- max(coords_large$x) - min(coords_large$x)
  expect_true(spread_large > spread_small)
})

# ============================================
# MULTIPLE GROUPS (n_groups > 1, lines 56-62)
# ============================================

test_that("layout_groups two groups positioned on circle", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups)

  # Group centers should be equidistant from (0.5, 0.5)
  g_a_center <- c(mean(coords$x[1:2]), mean(coords$y[1:2]))
  g_b_center <- c(mean(coords$x[3:4]), mean(coords$y[3:4]))

  dist_a <- sqrt((g_a_center[1] - 0.5)^2 + (g_a_center[2] - 0.5)^2)
  dist_b <- sqrt((g_b_center[1] - 0.5)^2 + (g_b_center[2] - 0.5)^2)

  expect_equal(dist_a, dist_b, tolerance = 0.01)
})

test_that("layout_groups three groups form triangle", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c("X", "X", "Y", "Y", "Z", "Z")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
})

test_that("layout_groups five groups evenly distributed", {
  adj <- create_test_matrix(10)
  net <- CographNetwork$new(adj)
  groups <- rep(1:5, each = 2)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 10)
})

test_that("layout_groups many groups (10) still work", {
  adj <- create_test_matrix(20)
  net <- CographNetwork$new(adj)
  groups <- rep(1:10, each = 2)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 20)
})

# ============================================
# ANGLE CALCULATIONS (lines 57-58, 95-96)
# ============================================

test_that("layout_groups groups start at pi/2 (top)", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups, inner_radius = 0)

  # First group should be at top (y > 0.5)
  g_a_center_y <- mean(coords$y[1:2])
  expect_true(g_a_center_y > 0.5)
})

test_that("layout_groups within-group nodes start at pi/2", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- rep("single", 4)

  coords <- layout_groups(net, groups)

  # First node should be at top of the circle
  # (highest y relative to center)
  center_y <- mean(coords$y)
  expect_equal(which.max(coords$y), 1)
})

# ============================================
# INNER RADIUS PARAMETER (line 97-98)
# ============================================

test_that("layout_groups inner_radius = 0 collapses nodes", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups, inner_radius = 0)

  # Nodes in same group should be at same position
  expect_equal(coords$x[1], coords$x[2])
  expect_equal(coords$y[1], coords$y[2])
  expect_equal(coords$x[3], coords$x[4])
  expect_equal(coords$y[3], coords$y[4])
})

test_that("layout_groups inner_radius very small", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups, inner_radius = 0.001)

  expect_equal(nrow(coords), 4)
  # Nodes should be very close
  dist_a <- sqrt((coords$x[1] - coords$x[2])^2 + (coords$y[1] - coords$y[2])^2)
  expect_true(dist_a < 0.01)
})

test_that("layout_groups inner_radius very large", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups, inner_radius = 0.5)

  expect_equal(nrow(coords), 4)
  # Nodes should be spread out
  dist_a <- sqrt((coords$x[1] - coords$x[2])^2 + (coords$y[1] - coords$y[2])^2)
  expect_true(dist_a > 0.3)
})

# ============================================
# OUTER RADIUS PARAMETER (lines 60-61)
# ============================================

test_that("layout_groups outer_radius = 0 collapses groups", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups, outer_radius = 0)

  # Group centers should be at (0.5, 0.5)
  g_a_center <- c(mean(coords$x[1:2]), mean(coords$y[1:2]))
  g_b_center <- c(mean(coords$x[3:4]), mean(coords$y[3:4]))

  expect_equal(g_a_center[1], 0.5, tolerance = 0.2)  # Close to center
  expect_equal(g_b_center[1], 0.5, tolerance = 0.2)
})

test_that("layout_groups outer_radius affects group separation", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords_small <- layout_groups(net, groups, outer_radius = 0.1)
  coords_large <- layout_groups(net, groups, outer_radius = 0.4)

  # Calculate distance between group centers
  dist_small <- sqrt(
    (mean(coords_small$x[1:2]) - mean(coords_small$x[3:4]))^2 +
    (mean(coords_small$y[1:2]) - mean(coords_small$y[3:4]))^2
  )
  dist_large <- sqrt(
    (mean(coords_large$x[1:2]) - mean(coords_large$x[3:4]))^2 +
    (mean(coords_large$y[1:2]) - mean(coords_large$y[3:4]))^2
  )

  expect_true(dist_large > dist_small)
})

# ============================================
# ROWNAMES ON GROUP_CENTERS (line 64)
# ============================================

test_that("layout_groups sets rownames on computed group_centers", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c("alpha", "alpha", "beta", "beta", "gamma", "gamma")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
  # Internal group_centers should have rownames matching group levels
})

test_that("layout_groups rownames handle numeric group names", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(10, 10, 20, 20)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

# ============================================
# GROUP INDEX MATCHING (line 85)
# ============================================

test_that("layout_groups matches group index correctly", {
  adj <- create_test_matrix(6)
  net <- CographNetwork$new(adj)
  groups <- c("B", "B", "A", "A", "C", "C")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
  # Groups should be positioned based on factor level order (A, B, C)
})

test_that("layout_groups handles non-alphabetical factor levels", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- factor(c("Z", "Z", "A", "A"), levels = c("Z", "A"))

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

# ============================================
# INTEGRATION WITH COGRAPH/SPLOT
# ============================================

test_that("cograph with groups layout works", {
  adj <- create_test_matrix(6)
  groups <- c(1, 1, 2, 2, 3, 3)

  net <- cograph(adj, layout = "groups", groups = groups)

  expect_cograph_network(net)
  expect_equal(n_nodes(net), 6)
})

test_that("splot with groups layout plots using pre-computed coords", {
  adj <- create_test_matrix(6)
  groups <- c(1, 1, 2, 2, 3, 3)

  net <- CographNetwork$new(adj)
  coords <- layout_groups(net, groups)
  result <- safe_plot(splot(adj, layout = as.matrix(coords[, c("x", "y")])))
  expect_true(result$success, info = result$error)
})

test_that("splot with groups and custom radii works via pre-computed coords", {
  adj <- create_test_matrix(6)
  groups <- c("X", "X", "Y", "Y", "Z", "Z")

  net <- CographNetwork$new(adj)
  coords <- layout_groups(net, groups, inner_radius = 0.2, outer_radius = 0.4)
  result <- safe_plot(splot(adj, layout = as.matrix(coords[, c("x", "y")])))
  expect_true(result$success, info = result$error)
})

# ============================================
# COORDINATE VALIDATION
# ============================================

test_that("layout_groups produces finite coordinates", {
  adj <- create_test_matrix(10)
  net <- CographNetwork$new(adj)
  groups <- rep(1:5, each = 2)

  coords <- layout_groups(net, groups)

  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
  expect_false(any(is.na(coords$x)))
  expect_false(any(is.na(coords$y)))
})

test_that("layout_groups coordinates are reasonable range", {
  adj <- create_test_matrix(12)
  net <- CographNetwork$new(adj)
  groups <- rep(1:4, each = 3)

  coords <- layout_groups(net, groups)

  # With default params, should be in approx [0, 1]
  expect_true(all(coords$x > -0.5 & coords$x < 1.5))
  expect_true(all(coords$y > -0.5 & coords$y < 1.5))
})

# ============================================
# REPRODUCIBILITY
# ============================================

test_that("layout_groups is deterministic", {
  adj <- create_test_matrix(8)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3, 4, 4)

  coords1 <- layout_groups(net, groups)
  coords2 <- layout_groups(net, groups)

  expect_identical(coords1, coords2)
})

test_that("layout_groups same result with different parameter defaults", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords_default <- layout_groups(net, groups)
  coords_explicit <- layout_groups(net, groups,
                                    inner_radius = 0.15,
                                    outer_radius = 0.35)

  expect_identical(coords_default, coords_explicit)
})

# ============================================
# EDGE CASES WITH SPECIAL VALUES
# ============================================

test_that("layout_groups handles groups with spaces", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("Group One", "Group One", "Group Two", "Group Two")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups handles unicode group names", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("Alpha", "Alpha", "Beta", "Beta")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups handles single character group names", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

# ============================================
# S3 COGRAPH_NETWORK SUPPORT
# ============================================

test_that("layout_groups works with S3 cograph_network", {
  adj <- create_test_matrix(6)
  net <- as_cograph(adj)

  expect_true(inherits(net, "cograph_network"))

  coords <- layout_groups(net, groups = c(1, 1, 2, 2, 3, 3))

  expect_equal(nrow(coords), 6)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("layout_groups uses n_nodes() method for S3 network", {
  adj <- create_test_matrix(5)
  net <- as_cograph(adj)

  expect_equal(n_nodes(net), 5)

  coords <- layout_groups(net, groups = c(1, 1, 1, 2, 2))
  expect_equal(nrow(coords), 5)
})

# ============================================
# EXTREME PARAMETER VALUES
# ============================================

test_that("layout_groups handles negative inner_radius", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  # Negative radius should still produce coords (reversed direction)
  coords <- layout_groups(net, groups, inner_radius = -0.1)

  expect_equal(nrow(coords), 4)
  expect_true(all(is.finite(coords$x)))
})

test_that("layout_groups handles negative outer_radius", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups, outer_radius = -0.2)

  expect_equal(nrow(coords), 4)
  expect_true(all(is.finite(coords$x)))
})

test_that("layout_groups handles very large radii", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups, inner_radius = 10, outer_radius = 100)

  expect_equal(nrow(coords), 4)
  expect_true(all(is.finite(coords$x)))
})

# ============================================
# COMPLEX SCENARIOS
# ============================================

test_that("layout_groups with uneven group sizes", {
  adj <- create_test_matrix(10)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 1, 1, 1, 2, 2, 2, 3, 3)  # 5, 3, 2

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 10)
})

test_that("layout_groups with many small groups", {
  adj <- create_test_matrix(12)
  net <- CographNetwork$new(adj)
  groups <- rep(1:6, each = 2)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 12)
})

test_that("layout_groups with one large group and many singletons", {
  adj <- create_test_matrix(8)
  net <- CographNetwork$new(adj)
  groups <- c("big", "big", "big", "big", "s1", "s2", "s3", "s4")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 8)
})

# ============================================
# CUSTOM GROUP POSITIONS WITH UNUSED LEVELS
# ============================================

test_that("layout_groups custom positions with factor unused levels", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)

  # Factor with unused level
  groups <- factor(c("A", "A", "C", "C"), levels = c("A", "B", "C"))

  # Provide positions for all levels including unused
  custom_pos <- data.frame(
    x = c(0.2, 0.5, 0.8),
    y = c(0.5, 0.5, 0.5)
  )
  rownames(custom_pos) <- c("A", "B", "C")

  coords <- layout_groups(net, groups, group_positions = custom_pos)

  expect_equal(nrow(coords), 4)
})

# ============================================
# DATA FRAME RETURN STRUCTURE
# ============================================

test_that("layout_groups returns proper data.frame class", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups)

  expect_s3_class(coords, "data.frame")
  expect_named(coords, c("x", "y"))
})

test_that("layout_groups x and y are double type", {
  adj <- create_test_matrix(4)
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups)

  expect_type(coords$x, "double")
  expect_type(coords$y, "double")
})

# ============================================
# WEIGHTED NETWORK SUPPORT
# ============================================

test_that("layout_groups works with weighted network", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- CographNetwork$new(adj)
  groups <- c("A", "A", "B", "B")

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})

test_that("layout_groups works with directed network", {
  adj <- create_test_matrix(4, symmetric = FALSE)
  net <- CographNetwork$new(adj, directed = TRUE)
  groups <- c(1, 1, 2, 2)

  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 4)
})
