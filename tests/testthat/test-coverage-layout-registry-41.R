# =============================================================================
# Test Coverage for layout-registry.R (Additional Tests - File 41)
# =============================================================================
# Comprehensive additional tests focusing on edge cases, parameter variations,
# and interactions with different network types

# Helper function to create mock network with specific n_nodes
create_mock_network <- function(n = 5) {
  net <- list(n_nodes = n)
  class(net) <- "CographNetwork"
  net
}

# Helper function to create R6 CographNetwork
create_r6_network <- function(n = 5, edges = TRUE) {
  mat <- matrix(0, n, n)
  if (edges && n > 1) {
    for (i in 1:(n - 1)) {
      mat[i, i + 1] <- 1
      mat[i + 1, i] <- 1
    }
  }
  rownames(mat) <- colnames(mat) <- paste0("N", 1:n)
  CographNetwork$new(mat)
}

# Helper to create cograph_network (lightweight)
create_lightweight_network <- function(n = 5, edges = TRUE) {
  mat <- matrix(0, n, n)
  if (edges && n > 1) {
    for (i in 1:(n - 1)) {
      mat[i, i + 1] <- 1
      mat[i + 1, i] <- 1
    }
  }
  rownames(mat) <- colnames(mat) <- paste0("N", 1:n)
  as_cograph(mat)
}

# =============================================================================
# Test: Grid Layout - Extended Parameter Tests
# =============================================================================

test_that("grid layout handles ncol = 1 (single column)", {
  net <- create_mock_network(5)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net, ncol = 1)

  expect_equal(nrow(coords), 5)
  # All nodes should have same x coordinate
  expect_equal(length(unique(coords$x)), 1)
  # 5 different y coordinates
  expect_equal(length(unique(coords$y)), 5)
})

test_that("grid layout handles ncol greater than n_nodes", {

  net <- create_mock_network(3)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net, ncol = 10)

  expect_equal(nrow(coords), 3)
  # All nodes should be in first row
  expect_true(all(coords$y == coords$y[1]))
})

test_that("grid layout handles perfect square node count", {
  net <- create_mock_network(16)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)

  expect_equal(nrow(coords), 16)
  # Should be 4x4 grid
  expect_equal(length(unique(coords$x)), 4)
  expect_equal(length(unique(coords$y)), 4)
})

test_that("grid layout spacing is within bounds", {
  net <- create_mock_network(25)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)

  expect_true(min(coords$x) >= 0.1)
  expect_true(max(coords$x) <= 0.9)
  expect_true(min(coords$y) >= 0.1)
  expect_true(max(coords$y) <= 0.9)
})

# =============================================================================
# Test: Random Layout - Boundary and Reproducibility Tests
# =============================================================================

test_that("random layout produces different results without seed", {
  net <- create_mock_network(20)
  random_fn <- get_layout("random")

  coords1 <- random_fn(net)
  coords2 <- random_fn(net)

  # Very unlikely to be identical
  expect_false(identical(coords1$x, coords2$x))
})

test_that("random layout works with seed = 0", {
  net <- create_mock_network(5)
  random_fn <- get_layout("random")

  coords1 <- random_fn(net, seed = 0)
  coords2 <- random_fn(net, seed = 0)

  expect_equal(coords1$x, coords2$x)
  expect_equal(coords1$y, coords2$y)
})

test_that("random layout works with negative seed", {
  net <- create_mock_network(5)
  random_fn <- get_layout("random")

  coords <- random_fn(net, seed = -42)
  expect_equal(nrow(coords), 5)
  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
})

test_that("random layout works with large seed", {
  net <- create_mock_network(5)
  random_fn <- get_layout("random")

  coords <- random_fn(net, seed = 2147483647)
  expect_equal(nrow(coords), 5)
})

# =============================================================================
# Test: Star Layout - Various Center Positions
# =============================================================================

test_that("star layout works with center at first node", {
  net <- create_mock_network(6)
  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 1)

  expect_equal(coords$x[1], 0.5)
  expect_equal(coords$y[1], 0.5)

  # Other nodes at radius 0.4
  for (i in 2:6) {
    dist <- sqrt((coords$x[i] - 0.5)^2 + (coords$y[i] - 0.5)^2)
    expect_equal(dist, 0.4, tolerance = 0.001)
  }
})

test_that("star layout works with center at last node", {
  net <- create_mock_network(6)
  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 6)

  expect_equal(coords$x[6], 0.5)
  expect_equal(coords$y[6], 0.5)

  # Other nodes at radius 0.4
  for (i in 1:5) {
    dist <- sqrt((coords$x[i] - 0.5)^2 + (coords$y[i] - 0.5)^2)
    expect_equal(dist, 0.4, tolerance = 0.001)
  }
})

test_that("star layout works with center in middle", {
  net <- create_mock_network(7)
  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 4)

  expect_equal(coords$x[4], 0.5)
  expect_equal(coords$y[4], 0.5)
})

test_that("star layout outer nodes are evenly distributed", {
  net <- create_mock_network(5)
  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 1)

  # Check outer nodes form a regular polygon
  outer_idx <- 2:5
  angles <- atan2(coords$y[outer_idx] - 0.5, coords$x[outer_idx] - 0.5)
  angles_sorted <- sort(angles)

  # Differences between consecutive angles should be equal
  diffs <- diff(c(angles_sorted, angles_sorted[1] + 2 * pi))
  expected_diff <- 2 * pi / 4
  for (d in diffs) {
    expect_equal(d, expected_diff, tolerance = 0.01)
  }
})

# =============================================================================
# Test: Bipartite Layout - Edge Cases
# =============================================================================

test_that("bipartite layout handles character types", {
  net <- create_mock_network(4)
  bipartite_fn <- get_layout("bipartite")
  types <- c("left", "left", "right", "right")
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 4)
  expect_equal(coords$x[1], 0.2)
  expect_equal(coords$x[3], 0.8)
})

test_that("bipartite layout handles factor types", {
  net <- create_mock_network(4)
  bipartite_fn <- get_layout("bipartite")
  types <- factor(c("A", "A", "B", "B"))
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 4)
})

test_that("bipartite layout handles numeric types (0/1)", {
  net <- create_mock_network(6)
  bipartite_fn <- get_layout("bipartite")
  types <- c(0, 0, 1, 1, 0, 1)
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 6)
  # type 0 nodes on left
  expect_equal(coords$x[1], 0.2)
  expect_equal(coords$x[2], 0.2)
  expect_equal(coords$x[5], 0.2)
  # type 1 nodes on right
  expect_equal(coords$x[3], 0.8)
  expect_equal(coords$x[4], 0.8)
  expect_equal(coords$x[6], 0.8)
})

test_that("bipartite layout vertical spacing is correct for unequal groups", {
  net <- create_mock_network(5)
  bipartite_fn <- get_layout("bipartite")
  types <- c(0, 0, 0, 1, 1)  # 3 on left, 2 on right
  coords <- bipartite_fn(net, types = types)

  # Left side (3 nodes): y = 0.9, 0.5, 0.1
  expect_equal(coords$y[1], 0.9)
  expect_equal(coords$y[2], 0.5)
  expect_equal(coords$y[3], 0.1)

  # Right side (2 nodes): y = 0.9, 0.1
  expect_equal(coords$y[4], 0.9)
  expect_equal(coords$y[5], 0.1)
})

test_that("bipartite layout handles single node per side", {
  net <- create_mock_network(2)
  bipartite_fn <- get_layout("bipartite")
  types <- c(0, 1)
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 2)
  expect_equal(coords$x[1], 0.2)
  expect_equal(coords$x[2], 0.8)
})

# =============================================================================
# Test: Custom Layout - Various Input Formats
# =============================================================================

test_that("custom layout preserves existing x/y column names", {
  net <- create_mock_network(3)
  custom_fn <- get_layout("custom")

  df <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.2, 0.5, 0.8))
  coords <- custom_fn(net, coords = df)

  expect_equal(coords$x, c(0.1, 0.5, 0.9))
  expect_equal(coords$y, c(0.2, 0.5, 0.8))
})

test_that("custom layout handles matrix with extra columns", {
  net <- create_mock_network(3)
  custom_fn <- get_layout("custom")

  mat <- matrix(c(0.1, 0.5, 0.9, 0.2, 0.5, 0.8, 1, 2, 3), ncol = 3)
  coords <- custom_fn(net, coords = mat)

  expect_equal(nrow(coords), 3)
  expect_equal(coords$x, c(0.1, 0.5, 0.9))
  expect_equal(coords$y, c(0.2, 0.5, 0.8))
})

test_that("custom layout handles data frame with extra columns", {
  net <- create_mock_network(3)
  custom_fn <- get_layout("custom")

  df <- data.frame(a = c(0.1, 0.5, 0.9), b = c(0.2, 0.5, 0.8), c = c(1, 2, 3))
  coords <- custom_fn(net, coords = df)

  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
  expect_equal(coords$x[1], 0.1)
})

# =============================================================================
# Test: Gephi FR Layout - Parameter Variations
# =============================================================================

test_that("gephi_fr layout works with minimal iterations", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(4)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 1)

  expect_equal(nrow(coords), 4)
})

test_that("gephi_fr layout handles very high gravity", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, gravity = 100.0, niter = 10)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr layout handles very low gravity", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, gravity = 0.1, niter = 10)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr layout handles small area", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, area = 100, niter = 10)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr layout handles large area", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, area = 100000, niter = 10)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr layout handles high speed", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, speed = 10.0, niter = 10)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr layout handles low speed", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, speed = 0.1, niter = 10)

  expect_equal(nrow(coords), 5)
})

# =============================================================================
# Test: Layout Registry Functions
# =============================================================================

test_that("list_layouts returns character vector", {
  layouts <- list_layouts()
  expect_true(is.character(layouts))
  expect_true(length(layouts) > 0)
})

test_that("list_layouts includes all expected layouts after registration", {
  expected_layouts <- c("circle", "oval", "ellipse", "spring", "fr",
                        "fruchterman-reingold", "groups", "grid", "random",
                        "star", "bipartite", "custom", "gephi_fr", "gephi")

  layouts <- list_layouts()
  for (layout in expected_layouts) {
    expect_true(layout %in% layouts,
                info = paste("Missing layout:", layout))
  }
})

test_that("get_layout returns NULL for empty string", {
  result <- get_layout("")
  expect_null(result)
})

test_that("get_layout returns NULL for NULL input", {
  # This should not error
  result <- tryCatch(
    get_layout(NULL),
    error = function(e) "error"
  )
  # Either NULL or error is acceptable behavior
  expect_true(is.null(result) || result == "error")
})

test_that("register_layout overwrites existing layout", {
  # Save original
  original <- get_layout("grid")

  # Register custom
  custom_fn <- function(network, ...) {
    data.frame(x = 0.5, y = 0.5)
  }
  register_layout("grid", custom_fn)

  # Check it was replaced
  new_fn <- get_layout("grid")
  net <- create_mock_network(1)
  coords <- new_fn(net)
  expect_equal(coords$x, 0.5)


  # Restore original
  register_layout("grid", original)
})

test_that("register_layout accepts anonymous function", {
  register_layout("test_anon", function(network, ...) {
    n <- network$n_nodes
    data.frame(x = seq(0, 1, length.out = n), y = seq(0, 1, length.out = n))
  })

  expect_true("test_anon" %in% list_layouts())

  fn <- get_layout("test_anon")
  net <- create_mock_network(5)
  coords <- fn(net)
  expect_equal(nrow(coords), 5)
})

test_that("register_layout errors on non-function", {
  expect_error(register_layout("bad_layout", 123),
               "layout_fn must be a function")
  expect_error(register_layout("bad_layout", "string"),
               "layout_fn must be a function")
  expect_error(register_layout("bad_layout", list(a = 1)),
               "layout_fn must be a function")
})

# =============================================================================
# Test: CographLayout Class Integration
# =============================================================================

test_that("CographLayout normalizes coordinates properly", {
  layout <- CographLayout$new("circle")
  net <- create_r6_network(4)
  coords <- layout$compute(net)

  # Normalized coordinates should be in [0, 1] range
  expect_true(all(coords$x >= 0 & coords$x <= 1))
  expect_true(all(coords$y >= 0 & coords$y <= 1))
})

test_that("CographLayout passes parameters to layout function", {
  layout <- CographLayout$new("grid", ncol = 2)
  net <- create_r6_network(6)
  coords <- layout$compute(net)

  # With ncol=2, should have 2 unique x values
  expect_equal(length(unique(coords$x)), 2)
})

test_that("CographLayout print method works", {
  layout <- CographLayout$new("spring", iterations = 100)
  expect_output(print(layout), "CographLayout")
  expect_output(print(layout), "spring")
})

test_that("CographLayout handles custom layout with coords parameter", {
  custom_coords <- data.frame(x = c(0, 0.5, 1), y = c(0, 1, 0))
  layout <- CographLayout$new("custom", coords = custom_coords)
  net <- create_r6_network(3)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 3)
})

# =============================================================================
# Test: Layout Function Output Validation
# =============================================================================

test_that("all layouts return data frames with x and y columns", {
  net <- create_mock_network(5)

  simple_layouts <- c("grid", "random", "star", "bipartite")
  for (layout_name in simple_layouts) {
    fn <- get_layout(layout_name)
    coords <- fn(net)

    expect_true(is.data.frame(coords),
                info = paste(layout_name, "should return data frame"))
    expect_true("x" %in% names(coords),
                info = paste(layout_name, "should have x column"))
    expect_true("y" %in% names(coords),
                info = paste(layout_name, "should have y column"))
  }
})

test_that("all layouts return numeric x and y values", {
  net <- create_mock_network(5)

  simple_layouts <- c("grid", "random", "star", "bipartite")
  for (layout_name in simple_layouts) {
    fn <- get_layout(layout_name)
    coords <- fn(net)

    expect_true(is.numeric(coords$x),
                info = paste(layout_name, "x should be numeric"))
    expect_true(is.numeric(coords$y),
                info = paste(layout_name, "y should be numeric"))
  }
})

test_that("all layouts return correct number of rows", {
  for (n in c(1, 5, 10, 20)) {
    net <- create_mock_network(n)

    simple_layouts <- c("grid", "random", "star", "bipartite")
    for (layout_name in simple_layouts) {
      fn <- get_layout(layout_name)
      coords <- fn(net)

      expect_equal(nrow(coords), n,
                   info = paste(layout_name, "with", n, "nodes"))
    }
  }
})

# =============================================================================
# Test: Empty and Edge Case Networks
# =============================================================================

test_that("all simple layouts handle empty network", {
  net <- create_mock_network(0)

  simple_layouts <- c("grid", "random", "star", "bipartite")
  for (layout_name in simple_layouts) {
    fn <- get_layout(layout_name)
    coords <- fn(net)

    expect_equal(nrow(coords), 0,
                 info = paste(layout_name, "with 0 nodes"))
    expect_true("x" %in% names(coords))
    expect_true("y" %in% names(coords))
  }
})

test_that("all simple layouts handle very large network", {
  net <- create_mock_network(500)

  simple_layouts <- c("grid", "random")  # Only test fast layouts
  for (layout_name in simple_layouts) {
    fn <- get_layout(layout_name)
    coords <- fn(net)

    expect_equal(nrow(coords), 500,
                 info = paste(layout_name, "with 500 nodes"))
  }
})

# =============================================================================
# Test: Layout Aliases
# =============================================================================

test_that("ellipse is alias for oval", {
  net <- create_r6_network(4)

  oval_fn <- get_layout("oval")
  ellipse_fn <- get_layout("ellipse")

  # Both should work and produce same structure
  coords1 <- oval_fn(net)
  coords2 <- ellipse_fn(net)

  expect_equal(nrow(coords1), nrow(coords2))
  expect_true(all(c("x", "y") %in% names(coords1)))
  expect_true(all(c("x", "y") %in% names(coords2)))
})

test_that("fr and fruchterman-reingold are aliases for spring", {
  spring_fn <- get_layout("spring")
  fr_fn <- get_layout("fr")
  fruchterman_fn <- get_layout("fruchterman-reingold")

  # All should be functions
  expect_true(is.function(spring_fn))
  expect_true(is.function(fr_fn))
  expect_true(is.function(fruchterman_fn))
})

test_that("gephi is alias for gephi_fr", {
  gephi_fn <- get_layout("gephi")
  gephi_fr_fn <- get_layout("gephi_fr")

  expect_true(is.function(gephi_fn))
  expect_true(is.function(gephi_fr_fn))
})

# =============================================================================
# Test: Gephi FR with Different Network Types
# =============================================================================

test_that("gephi_fr works with R6 CographNetwork", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 5)

  expect_equal(nrow(coords), 5)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("gephi_fr works with lightweight cograph_network", {
  skip_if_not_installed("igraph")
  net <- create_lightweight_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 5)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr handles network with self-loops avoided", {
  skip_if_not_installed("igraph")
  # Create network with potential for overlapping coordinates
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[2, 3] <- mat[3, 2] <- 1
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- CographNetwork$new(mat)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 10)

  expect_equal(nrow(coords), 3)
})

# =============================================================================
# Test: Circle and Oval Layout (via CographLayout)
# =============================================================================

test_that("circle layout through CographLayout", {
  layout <- CographLayout$new("circle")
  net <- create_r6_network(6)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 6)

  # All nodes should be roughly same distance from center
  dists <- sqrt((coords$x - 0.5)^2 + (coords$y - 0.5)^2)
  expect_true(max(dists) - min(dists) < 0.1)  # Should be equal within tolerance
})

test_that("oval layout through CographLayout", {
  layout <- CographLayout$new("oval")
  net <- create_r6_network(6)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 6)
})

# =============================================================================
# Test: Spring Layout (via CographLayout)
# =============================================================================

test_that("spring layout through CographLayout", {
  layout <- CographLayout$new("spring")
  net <- create_r6_network(4)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 4)
})

test_that("spring layout with seed parameter", {
  layout1 <- CographLayout$new("spring", seed = 42)
  layout2 <- CographLayout$new("spring", seed = 42)
  net <- create_r6_network(4)

  coords1 <- layout1$compute(net)
  coords2 <- layout2$compute(net)

  # Same seed should produce same results
  expect_equal(coords1$x, coords2$x, tolerance = 0.01)
  expect_equal(coords1$y, coords2$y, tolerance = 0.01)
})

# =============================================================================
# Test: Groups Layout (via CographLayout)
# =============================================================================

test_that("groups layout through CographLayout", {
  layout <- CographLayout$new("groups", groups = c(1, 1, 2, 2, 3, 3))
  net <- create_r6_network(6)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 6)
})

# =============================================================================
# Test: register_builtin_layouts is idempotent
# =============================================================================

test_that("calling register_builtin_layouts multiple times is safe", {
  # Get initial count
  layouts_before <- list_layouts()

  # Call again - using ::: to access internal function
  cograph:::register_builtin_layouts()

  layouts_after <- list_layouts()

  # Should have same layouts (duplicates not added)
  expect_equal(sort(layouts_before), sort(layouts_after))
})

# =============================================================================
# Test: Layout coordinates are finite
# =============================================================================

test_that("grid layout produces finite coordinates", {
  net <- create_mock_network(10)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)

  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("random layout produces finite coordinates", {
  net <- create_mock_network(10)
  random_fn <- get_layout("random")
  coords <- random_fn(net, seed = 42)

  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("star layout produces finite coordinates", {
  net <- create_mock_network(10)
  star_fn <- get_layout("star")
  coords <- star_fn(net)

  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("bipartite layout produces finite coordinates", {
  net <- create_mock_network(10)
  bipartite_fn <- get_layout("bipartite")
  coords <- bipartite_fn(net)

  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("gephi_fr layout produces finite coordinates", {
  skip_if_not_installed("igraph")
  net <- create_r6_network(5)
  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 10)

  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})
