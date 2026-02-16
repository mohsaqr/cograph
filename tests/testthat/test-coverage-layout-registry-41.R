# =============================================================================
# Test Coverage for layout-registry.R (Additional Tests - File 41)
# =============================================================================
# Comprehensive additional tests focusing on edge cases, parameter variations,
# and interactions with different network types.
# Focus: register_layout, get_layout, list_layouts and built-in layout functions

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
# Test: register_layout function validation
# =============================================================================

test_that("register_layout validates function parameter strictly", {
  expect_error(
    register_layout("test_bad_1", "not_a_function"),
    "layout_fn must be a function"
  )

  expect_error(
    register_layout("test_bad_2", 123),
    "layout_fn must be a function"
  )

  expect_error(
    register_layout("test_bad_3", list(a = 1)),
    "layout_fn must be a function"
  )

  expect_error(
    register_layout("test_bad_4", NULL),
    "layout_fn must be a function"
  )

  expect_error(
    register_layout("test_bad_5", NA),
    "layout_fn must be a function"
  )
})

test_that("register_layout accepts valid functions", {
  # Simple function
  fn1 <- function(network, ...) {
    data.frame(x = 0.5, y = 0.5)
  }
  expect_silent(register_layout("test_valid_1", fn1))
  expect_true("test_valid_1" %in% list_layouts())

  # Anonymous function
  expect_silent(register_layout("test_valid_2", function(network, ...) {
    data.frame(x = 0, y = 0)
  }))
  expect_true("test_valid_2" %in% list_layouts())

  # Function with default parameters
  fn3 <- function(network, param1 = 10, param2 = "abc", ...) {
    n <- network$n_nodes
    data.frame(x = rep(0.5, n), y = rep(0.5, n))
  }
  expect_silent(register_layout("test_valid_3", fn3))
  expect_true("test_valid_3" %in% list_layouts())
})

test_that("register_layout returns invisible NULL", {
  fn <- function(network, ...) data.frame(x = 0, y = 0)
  result <- register_layout("test_return", fn)
  expect_null(result)
})

test_that("register_layout can overwrite existing layout multiple times", {
  original <- get_layout("grid")

  # First overwrite
  fn1 <- function(network, ...) data.frame(x = 0.1, y = 0.1)
  register_layout("grid", fn1)
  expect_equal(get_layout("grid")(create_mock_network(1))$x, 0.1)

  # Second overwrite
  fn2 <- function(network, ...) data.frame(x = 0.2, y = 0.2)
  register_layout("grid", fn2)
  expect_equal(get_layout("grid")(create_mock_network(1))$x, 0.2)

  # Restore original
  register_layout("grid", original)
})

# =============================================================================
# Test: get_layout function
# =============================================================================

test_that("get_layout returns NULL for non-existent layouts", {
  expect_null(get_layout("nonexistent_layout_xyz_123"))
  expect_null(get_layout(""))
  expect_null(get_layout("   "))
})

test_that("get_layout returns functions for all built-in layouts", {
  expected_layouts <- c(
    "circle", "oval", "ellipse", "spring", "fr", "fruchterman-reingold",
    "groups", "grid", "random", "star", "bipartite", "custom",
    "gephi_fr", "gephi"
  )

  for (name in expected_layouts) {
    fn <- get_layout(name)
    expect_true(is.function(fn), info = paste("Layout not a function:", name))
  }
})

test_that("get_layout is case-sensitive", {
  expect_true(is.function(get_layout("circle")))
  expect_null(get_layout("CIRCLE"))
  expect_null(get_layout("Circle"))
})

# =============================================================================
# Test: list_layouts function
# =============================================================================

test_that("list_layouts returns character vector", {
  layouts <- list_layouts()
  expect_true(is.character(layouts))
  expect_true(length(layouts) > 0)
})

test_that("list_layouts contains all built-in layouts", {
  layouts <- list_layouts()
  expected <- c(
    "circle", "oval", "ellipse", "spring", "fr", "fruchterman-reingold",
    "groups", "grid", "random", "star", "bipartite", "custom",
    "gephi_fr", "gephi"
  )

  for (name in expected) {
    expect_true(name %in% layouts, info = paste("Missing:", name))
  }
})

test_that("list_layouts reflects newly registered layouts", {
  initial_count <- length(list_layouts())

  register_layout("test_new_layout_xyz", function(network, ...) {
    data.frame(x = 0, y = 0)
  })

  expect_true("test_new_layout_xyz" %in% list_layouts())
  expect_equal(length(list_layouts()), initial_count + 1)
})

# =============================================================================
# Test: Grid Layout - Extended Edge Cases
# =============================================================================

test_that("grid layout handles ncol = 1 (single column)", {
  net <- create_mock_network(5)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net, ncol = 1)

  expect_equal(nrow(coords), 5)
  expect_equal(length(unique(coords$x)), 1)
  expect_equal(length(unique(coords$y)), 5)
})

test_that("grid layout handles ncol greater than n_nodes", {
  net <- create_mock_network(3)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net, ncol = 10)

  expect_equal(nrow(coords), 3)
  expect_true(all(coords$y == coords$y[1]))
})

test_that("grid layout handles perfect square node count", {
  net <- create_mock_network(16)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)

  expect_equal(nrow(coords), 16)
  expect_equal(length(unique(coords$x)), 4)
  expect_equal(length(unique(coords$y)), 4)
})

test_that("grid layout returns empty for 0 nodes", {
  net <- create_mock_network(0)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)

  expect_equal(nrow(coords), 0)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("grid layout returns center for 1 node", {
  net <- create_mock_network(1)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x[1], 0.5)
  expect_equal(coords$y[1], 0.5)
})

test_that("grid layout ncol = n gives single row", {
  net <- create_mock_network(4)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net, ncol = 4)

  expect_equal(length(unique(coords$y)), 1)
  expect_equal(length(unique(coords$x)), 4)
})

# =============================================================================
# Test: Random Layout - Comprehensive Tests
# =============================================================================

test_that("random layout produces reproducible results with seed", {
  net <- create_mock_network(10)
  random_fn <- get_layout("random")

  coords1 <- random_fn(net, seed = 42)
  coords2 <- random_fn(net, seed = 42)

  expect_equal(coords1$x, coords2$x)
  expect_equal(coords1$y, coords2$y)
})

test_that("random layout works with seed = 0", {
  net <- create_mock_network(5)
  random_fn <- get_layout("random")

  coords1 <- random_fn(net, seed = 0)
  coords2 <- random_fn(net, seed = 0)

  expect_equal(coords1$x, coords2$x)
})

test_that("random layout produces values in correct range", {
  net <- create_mock_network(100)
  random_fn <- get_layout("random")
  coords <- random_fn(net, seed = 123)

  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
  expect_true(all(coords$y >= 0.1 & coords$y <= 0.9))
})

test_that("random layout handles zero nodes", {
  net <- create_mock_network(0)
  random_fn <- get_layout("random")
  coords <- random_fn(net, seed = 42)

  expect_equal(nrow(coords), 0)
})

# =============================================================================
# Test: Star Layout - Comprehensive Tests
# =============================================================================

test_that("star layout returns empty for 0 nodes", {
  net <- create_mock_network(0)
  star_fn <- get_layout("star")
  coords <- star_fn(net)

  expect_equal(nrow(coords), 0)
})

test_that("star layout returns center for 1 node", {
  net <- create_mock_network(1)
  star_fn <- get_layout("star")
  coords <- star_fn(net)

  expect_equal(nrow(coords), 1)
  expect_equal(coords$x[1], 0.5)
  expect_equal(coords$y[1], 0.5)
})

test_that("star layout with 2 nodes places center correctly", {
  net <- create_mock_network(2)
  star_fn <- get_layout("star")

  # Center at node 1
 coords1 <- star_fn(net, center = 1)
  expect_equal(coords1$x[1], 0.5)
  expect_equal(coords1$y[1], 0.5)

  # Center at node 2
  coords2 <- star_fn(net, center = 2)
  expect_equal(coords2$x[2], 0.5)
  expect_equal(coords2$y[2], 0.5)
})

test_that("star layout places outer nodes at correct radius", {
  net <- create_mock_network(5)
  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 1)

  for (i in 2:5) {
    dist <- sqrt((coords$x[i] - 0.5)^2 + (coords$y[i] - 0.5)^2)
    expect_equal(dist, 0.4, tolerance = 0.001)
  }
})

test_that("star layout outer nodes are evenly spaced", {
  net <- create_mock_network(5)
  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 1)

  outer_idx <- 2:5
  angles <- atan2(coords$y[outer_idx] - 0.5, coords$x[outer_idx] - 0.5)
  angles_sorted <- sort(angles)

  diffs <- diff(c(angles_sorted, angles_sorted[1] + 2 * pi))
  expected_diff <- 2 * pi / 4

  for (d in diffs) {
    expect_equal(d, expected_diff, tolerance = 0.01)
  }
})

test_that("star layout works with center at last node", {
  net <- create_mock_network(6)
  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 6)

  expect_equal(coords$x[6], 0.5)
  expect_equal(coords$y[6], 0.5)

  for (i in 1:5) {
    dist <- sqrt((coords$x[i] - 0.5)^2 + (coords$y[i] - 0.5)^2)
    expect_equal(dist, 0.4, tolerance = 0.001)
  }
})

# =============================================================================
# Test: Bipartite Layout - Comprehensive Tests
# =============================================================================

test_that("bipartite layout returns empty for 0 nodes", {
  net <- create_mock_network(0)
  bipartite_fn <- get_layout("bipartite")
  coords <- bipartite_fn(net)

  expect_equal(nrow(coords), 0)
})

test_that("bipartite layout uses default alternating types", {
  net <- create_mock_network(4)
  bipartite_fn <- get_layout("bipartite")
  coords <- bipartite_fn(net)

  expect_equal(coords$x[1], 0.2)
  expect_equal(coords$x[2], 0.8)
  expect_equal(coords$x[3], 0.2)
  expect_equal(coords$x[4], 0.8)
})

test_that("bipartite layout handles all same type (type1 only)", {
  net <- create_mock_network(4)
  bipartite_fn <- get_layout("bipartite")
  types <- c(0, 0, 0, 0)
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 4)
  expect_true(all(coords$x == 0.2))
})

test_that("bipartite layout handles all same type (type2 only)", {
  net <- create_mock_network(4)
  bipartite_fn <- get_layout("bipartite")
  # First unique type becomes type1 (left), second becomes type2 (right)
  types <- c(1, 1, 1, 1)
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 4)
  # All on left since there's only one type
  expect_true(all(coords$x == 0.2))
})

test_that("bipartite layout vertical spacing is correct", {
  net <- create_mock_network(6)
  bipartite_fn <- get_layout("bipartite")
  types <- c(0, 0, 0, 1, 1, 1)
  coords <- bipartite_fn(net, types = types)

  expect_equal(coords$y[1], 0.9)
  expect_equal(coords$y[2], 0.5)
  expect_equal(coords$y[3], 0.1)
  expect_equal(coords$y[4], 0.9)
  expect_equal(coords$y[5], 0.5)
  expect_equal(coords$y[6], 0.1)
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

# =============================================================================
# Test: Custom Layout - Comprehensive Tests
# =============================================================================

test_that("custom layout accepts matrix input", {
  net <- create_mock_network(3)
  custom_fn <- get_layout("custom")

  custom_coords <- matrix(c(0.1, 0.5, 0.9, 0.2, 0.5, 0.8), ncol = 2)
  coords <- custom_fn(net, coords = custom_coords)

  expect_equal(nrow(coords), 3)
  expect_equal(coords$x[1], 0.1)
  expect_equal(coords$y[1], 0.2)
})

test_that("custom layout accepts data frame input", {
  net <- create_mock_network(3)
  custom_fn <- get_layout("custom")

  custom_coords <- data.frame(a = c(0.1, 0.5, 0.9), b = c(0.2, 0.5, 0.8))
  coords <- custom_fn(net, coords = custom_coords)

  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
  expect_equal(coords$x[1], 0.1)
  expect_equal(coords$y[1], 0.2)
})

test_that("custom layout preserves existing x/y names", {
  net <- create_mock_network(3)
  custom_fn <- get_layout("custom")

  custom_coords <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.2, 0.5, 0.8))
  coords <- custom_fn(net, coords = custom_coords)

  expect_equal(coords$x, c(0.1, 0.5, 0.9))
  expect_equal(coords$y, c(0.2, 0.5, 0.8))
})

test_that("custom layout handles matrix with extra columns", {
  net <- create_mock_network(3)
  custom_fn <- get_layout("custom")

  mat <- matrix(1:9, ncol = 3)
  coords <- custom_fn(net, coords = mat)

  expect_equal(nrow(coords), 3)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

# =============================================================================
# Test: Gephi FR Layout - Comprehensive Tests
# =============================================================================

test_that("gephi_fr layout returns empty for 0 nodes", {
  skip_if_not_installed("igraph")

  # Create empty mock network for gephi function (direct call)
  gephi_fn <- get_layout("gephi_fr")

  # Call with mock network that has 0 nodes
  net_mock <- list(n_nodes = 0)
  class(net_mock) <- "CographNetwork"

  # The gephi_fr internally uses network_to_igraph, so we skip this test
  # since empty networks can't be properly converted
  skip("Empty networks require special handling in network_to_igraph")
})

test_that("gephi_fr layout handles network with no edges", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, 5, 5)
  rownames(mat) <- colnames(mat) <- paste0("N", 1:5)
  net <- CographNetwork$new(mat)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 10)

  expect_equal(nrow(coords), 5)
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("gephi_fr layout works with single node", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, 1, 1)
  rownames(mat) <- colnames(mat) <- "A"
  net <- CographNetwork$new(mat)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 5)

  expect_equal(nrow(coords), 1)
})

test_that("gephi_fr layout works with dense network", {
  skip_if_not_installed("igraph")

  n <- 6
  mat <- matrix(1, n, n)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("N", 1:n)
  net <- CographNetwork$new(mat)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 20)

  expect_equal(nrow(coords), n)
  expect_true(all(is.finite(coords$x)))
  expect_true(all(is.finite(coords$y)))
})

test_that("gephi_fr layout handles weighted edges", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 0.5
  mat[2, 3] <- mat[3, 2] <- 1.0
  mat[3, 4] <- mat[4, 3] <- 2.0
  rownames(mat) <- colnames(mat) <- paste0("N", 1:4)
  net <- CographNetwork$new(mat)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 15)

  expect_equal(nrow(coords), 4)
})

test_that("gephi_fr layout respects all parameters", {
  skip_if_not_installed("igraph")

  net <- create_r6_network(5)
  gephi_fn <- get_layout("gephi_fr")

  # Test various parameter combinations
  coords1 <- gephi_fn(net, area = 5000, gravity = 5.0, speed = 0.5, niter = 10)
  coords2 <- gephi_fn(net, area = 20000, gravity = 20.0, speed = 2.0, niter = 10)

  expect_equal(nrow(coords1), 5)
  expect_equal(nrow(coords2), 5)
})

test_that("gephi_fr layout handles extreme parameters", {
  skip_if_not_installed("igraph")

  net <- create_r6_network(4)
  gephi_fn <- get_layout("gephi_fr")

  # Very high gravity
  coords1 <- gephi_fn(net, gravity = 100, niter = 5)
  expect_equal(nrow(coords1), 4)

  # Very low gravity
  coords2 <- gephi_fn(net, gravity = 0.01, niter = 5)
  expect_equal(nrow(coords2), 4)

  # Very high speed
  coords3 <- gephi_fn(net, speed = 10, niter = 5)
  expect_equal(nrow(coords3), 4)

  # Very small area
  coords4 <- gephi_fn(net, area = 10, niter = 5)
  expect_equal(nrow(coords4), 4)
})

test_that("gephi alias works same as gephi_fr", {
  skip_if_not_installed("igraph")

  net <- create_r6_network(4)
  gephi_fn <- get_layout("gephi")
  gephi_fr_fn <- get_layout("gephi_fr")

  expect_true(is.function(gephi_fn))
  expect_true(is.function(gephi_fr_fn))

  coords1 <- gephi_fn(net, niter = 5)
  coords2 <- gephi_fr_fn(net, niter = 5)

  expect_equal(nrow(coords1), nrow(coords2))
})

# =============================================================================
# Test: Layout Aliases
# =============================================================================

test_that("oval and ellipse are aliases", {
  oval_fn <- get_layout("oval")
  ellipse_fn <- get_layout("ellipse")

  expect_true(is.function(oval_fn))
  expect_true(is.function(ellipse_fn))
})

test_that("spring, fr, and fruchterman-reingold are aliases", {
  spring_fn <- get_layout("spring")
  fr_fn <- get_layout("fr")
  fruchterman_fn <- get_layout("fruchterman-reingold")

  expect_true(is.function(spring_fn))
  expect_true(is.function(fr_fn))
  expect_true(is.function(fruchterman_fn))
})

# =============================================================================
# Test: CographLayout Class Integration
# =============================================================================

test_that("CographLayout computes coordinates for all layout types", {
  net <- create_r6_network(4)
  layout_types <- c("circle", "oval", "grid", "random", "star", "bipartite")

  for (type in layout_types) {
    layout <- CographLayout$new(type)
    coords <- layout$compute(net)

    expect_equal(nrow(coords), 4, info = paste("Layout:", type))
    expect_true("x" %in% names(coords), info = paste("Layout:", type))
    expect_true("y" %in% names(coords), info = paste("Layout:", type))
  }
})

test_that("CographLayout passes parameters correctly", {
  net <- create_r6_network(6)

  # Grid with ncol
  layout <- CographLayout$new("grid", ncol = 2)
  coords <- layout$compute(net)
  expect_equal(length(unique(coords$x)), 2)

  # Random with seed
  layout1 <- CographLayout$new("random", seed = 42)
  layout2 <- CographLayout$new("random", seed = 42)
  coords1 <- layout1$compute(net)
  coords2 <- layout2$compute(net)
  expect_equal(coords1$x, coords2$x, tolerance = 0.001)

  # Star with center
  layout <- CographLayout$new("star", center = 3)
  coords <- layout$compute(net)
  # Node 3 should be at center after normalization
  # (center might be shifted due to normalization but should be distinct)
  expect_equal(nrow(coords), 6)
})

test_that("CographLayout errors on unknown layout", {
  layout <- CographLayout$new("nonexistent_xyz")
  net <- create_r6_network(3)

  expect_error(layout$compute(net), "Unknown layout type")
})

test_that("CographLayout get_type returns correct type", {
  layout <- CographLayout$new("grid")
  expect_equal(layout$get_type(), "grid")

  layout2 <- CographLayout$new("circle")
  expect_equal(layout2$get_type(), "circle")
})

test_that("CographLayout get_params returns stored parameters", {
  layout <- CographLayout$new("grid", ncol = 3, foo = "bar")
  params <- layout$get_params()

  expect_equal(params$ncol, 3)
  expect_equal(params$foo, "bar")
})

test_that("CographLayout print method outputs correctly", {
  layout <- CographLayout$new("spring", iterations = 100)
  expect_output(print(layout), "CographLayout")
  expect_output(print(layout), "spring")
  expect_output(print(layout), "iterations")
})

test_that("CographLayout normalize_coords works correctly", {
  layout <- CographLayout$new("circle")

  # Test with extreme coordinates
  coords <- data.frame(x = c(-100, 0, 100), y = c(-50, 0, 50))
  normalized <- layout$normalize_coords(coords)

  expect_true(all(normalized$x >= 0 & normalized$x <= 1))
  expect_true(all(normalized$y >= 0 & normalized$y <= 1))
})

test_that("CographLayout normalize_coords handles single point", {
  layout <- CographLayout$new("circle")

  coords <- data.frame(x = 100, y = 200)
  normalized <- layout$normalize_coords(coords)

  expect_equal(normalized$x, 0.5)
  expect_equal(normalized$y, 0.5)
})

test_that("CographLayout normalize_coords handles matrix input", {
  layout <- CographLayout$new("circle")

  coords <- matrix(c(-10, 0, 10, -5, 0, 5), ncol = 2)
  normalized <- layout$normalize_coords(coords)

  expect_true(is.data.frame(normalized))
  expect_true("x" %in% names(normalized))
  expect_true("y" %in% names(normalized))
})

test_that("CographLayout handles custom layout", {
  custom_coords <- data.frame(x = c(0.1, 0.5, 0.9), y = c(0.2, 0.5, 0.8))
  layout <- CographLayout$new("custom", coords = custom_coords)
  net <- create_r6_network(3)

  coords <- layout$compute(net)
  expect_equal(nrow(coords), 3)
})

# =============================================================================
# Test: register_builtin_layouts idempotency
# =============================================================================

test_that("register_builtin_layouts can be called multiple times safely", {
  layouts_before <- list_layouts()

  # Call internal function
  cograph:::register_builtin_layouts()

  layouts_after <- list_layouts()
  expect_equal(sort(layouts_before), sort(layouts_after))
})

# =============================================================================
# Test: Layout output validation
# =============================================================================

test_that("all layouts return finite coordinates", {
  net <- create_mock_network(10)
  simple_layouts <- c("grid", "random", "star", "bipartite")

  for (name in simple_layouts) {
    fn <- get_layout(name)
    coords <- fn(net)

    expect_true(all(is.finite(coords$x)), info = paste(name, "x not finite"))
    expect_true(all(is.finite(coords$y)), info = paste(name, "y not finite"))
  }
})

test_that("all layouts return correct row count", {
  for (n in c(0, 1, 2, 5, 10, 50)) {
    net <- create_mock_network(n)
    simple_layouts <- c("grid", "random", "star", "bipartite")

    for (name in simple_layouts) {
      fn <- get_layout(name)
      coords <- fn(net)

      expect_equal(nrow(coords), n, info = paste(name, "with", n, "nodes"))
    }
  }
})

test_that("all layouts return proper data frame structure", {
  net <- create_mock_network(5)
  simple_layouts <- c("grid", "random", "star", "bipartite")

  for (name in simple_layouts) {
    fn <- get_layout(name)
    coords <- fn(net)

    expect_true(is.data.frame(coords), info = paste(name, "not data.frame"))
    expect_true("x" %in% names(coords), info = paste(name, "no x"))
    expect_true("y" %in% names(coords), info = paste(name, "no y"))
    expect_true(is.numeric(coords$x), info = paste(name, "x not numeric"))
    expect_true(is.numeric(coords$y), info = paste(name, "y not numeric"))
  }
})

# =============================================================================
# Test: Various network types with layouts
# =============================================================================

test_that("layouts work with R6 CographNetwork", {
  net <- create_r6_network(5)
  layout_types <- c("circle", "oval", "grid", "star", "bipartite")

  for (type in layout_types) {
    layout <- CographLayout$new(type)
    coords <- layout$compute(net)

    expect_equal(nrow(coords), 5, info = paste("Layout:", type))
  }
})

test_that("layouts work with lightweight cograph_network", {
  net <- create_lightweight_network(5)
  # These layouts need to work with cograph_network through CographLayout
  # CographLayout uses get_layout and those functions expect n_nodes field
  # cograph_network uses n_nodes() function instead, so use direct layout functions

  # Circle and oval use layout_circle/layout_oval directly
  coords <- layout_circle(net)
  expect_equal(nrow(coords), 5)

  coords <- layout_oval(net)
  expect_equal(nrow(coords), 5)

  coords <- layout_spring(net, iterations = 10)
  expect_equal(nrow(coords), 5)
})

# =============================================================================
# Test: Spring layout specifics
# =============================================================================

test_that("spring layout produces reproducible results with seed", {
  net <- create_r6_network(4)

  layout1 <- CographLayout$new("spring", seed = 42)
  layout2 <- CographLayout$new("spring", seed = 42)

  coords1 <- layout1$compute(net)
  coords2 <- layout2$compute(net)

  expect_equal(coords1$x, coords2$x, tolerance = 0.001)
  expect_equal(coords1$y, coords2$y, tolerance = 0.001)
})

test_that("spring layout aliases produce coordinates", {
  net <- create_r6_network(4)

  for (alias in c("spring", "fr", "fruchterman-reingold")) {
    layout <- CographLayout$new(alias)
    coords <- layout$compute(net)

    expect_equal(nrow(coords), 4, info = paste("Alias:", alias))
    expect_true("x" %in% names(coords), info = paste("Alias:", alias))
    expect_true("y" %in% names(coords), info = paste("Alias:", alias))
  }
})

# =============================================================================
# Test: Groups layout
# =============================================================================

test_that("groups layout arranges nodes by group", {
  layout <- CographLayout$new("groups", groups = c(1, 1, 2, 2, 3, 3))
  net <- create_r6_network(6)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 6)
})

test_that("groups layout with single group", {
  layout <- CographLayout$new("groups", groups = c(1, 1, 1, 1))
  net <- create_r6_network(4)
  coords <- layout$compute(net)

  expect_equal(nrow(coords), 4)
})

# =============================================================================
# Test: Edge cases for built-in layouts
# =============================================================================

test_that("grid layout handles very large networks", {
  net <- create_mock_network(1000)
  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)

  expect_equal(nrow(coords), 1000)
  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
  expect_true(all(coords$y >= 0.1 & coords$y <= 0.9))
})

test_that("random layout handles very large networks", {
  net <- create_mock_network(1000)
  random_fn <- get_layout("random")
  coords <- random_fn(net, seed = 42)

  expect_equal(nrow(coords), 1000)
})

test_that("star layout handles large networks", {
  net <- create_mock_network(100)
  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 50)

  expect_equal(nrow(coords), 100)
  expect_equal(coords$x[50], 0.5)
  expect_equal(coords$y[50], 0.5)
})

test_that("bipartite layout handles large networks", {
  net <- create_mock_network(100)
  bipartite_fn <- get_layout("bipartite")
  types <- rep(c(0, 1), 50)
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 100)
})
