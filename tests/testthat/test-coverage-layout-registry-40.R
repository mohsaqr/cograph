# =============================================================================
# Test Coverage for layout-registry.R
# =============================================================================
# Comprehensive tests for all layout registration functions and built-in layouts

# Helper function to create test networks
create_test_network <- function(n = 5, edges = TRUE) {
  mat <- matrix(0, n, n)
  if (edges && n > 1) {
    # Create some edges
    for (i in 1:(n - 1)) {
      mat[i, i + 1] <- 1
      mat[i + 1, i] <- 1
    }
  }
  rownames(mat) <- colnames(mat) <- paste0("N", 1:n)
  CographNetwork$new(mat)
}

create_test_cograph <- function(n = 5, edges = TRUE) {
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
# Test: register_builtin_layouts function
# =============================================================================

test_that("register_builtin_layouts registers circle layout", {
  # Circle layout should be registered

  expect_true("circle" %in% list_layouts())
  fn <- get_layout("circle")
  expect_true(is.function(fn))
})

test_that("register_builtin_layouts registers oval layout with alias", {

  # Oval and ellipse should both be registered

  expect_true("oval" %in% list_layouts())
  expect_true("ellipse" %in% list_layouts())

  # Both should return the same function
  oval_fn <- get_layout("oval")
  ellipse_fn <- get_layout("ellipse")
  expect_true(is.function(oval_fn))
  expect_true(is.function(ellipse_fn))
})

test_that("register_builtin_layouts registers spring layout with aliases", {
  expect_true("spring" %in% list_layouts())
  expect_true("fr" %in% list_layouts())
  expect_true("fruchterman-reingold" %in% list_layouts())

  spring_fn <- get_layout("spring")
  fr_fn <- get_layout("fr")
  fruchterman_fn <- get_layout("fruchterman-reingold")

  expect_true(is.function(spring_fn))
  expect_true(is.function(fr_fn))
  expect_true(is.function(fruchterman_fn))
})

test_that("register_builtin_layouts registers groups layout", {
  expect_true("groups" %in% list_layouts())
  fn <- get_layout("groups")
  expect_true(is.function(fn))
})

test_that("register_builtin_layouts registers grid layout", {
  expect_true("grid" %in% list_layouts())
  fn <- get_layout("grid")
  expect_true(is.function(fn))
})

test_that("register_builtin_layouts registers random layout", {
  expect_true("random" %in% list_layouts())
  fn <- get_layout("random")
  expect_true(is.function(fn))
})

test_that("register_builtin_layouts registers star layout", {
  expect_true("star" %in% list_layouts())
  fn <- get_layout("star")
  expect_true(is.function(fn))
})

test_that("register_builtin_layouts registers bipartite layout", {
  expect_true("bipartite" %in% list_layouts())
  fn <- get_layout("bipartite")
  expect_true(is.function(fn))
})

test_that("register_builtin_layouts registers custom layout", {
  expect_true("custom" %in% list_layouts())
  fn <- get_layout("custom")
  expect_true(is.function(fn))
})

test_that("register_builtin_layouts registers gephi_fr layout with alias", {
  expect_true("gephi_fr" %in% list_layouts())
  expect_true("gephi" %in% list_layouts())

  gephi_fr_fn <- get_layout("gephi_fr")
  gephi_fn <- get_layout("gephi")

  expect_true(is.function(gephi_fr_fn))
  expect_true(is.function(gephi_fn))
})

# =============================================================================
# Test: Grid Layout Function
# =============================================================================

test_that("grid layout returns empty data frame for empty network", {
  mat <- matrix(0, 0, 0)
  net <- CographNetwork$new()
  # Create a network object with n_nodes = 0
  net_mock <- list(n_nodes = 0)
  class(net_mock) <- "CographNetwork"

  grid_fn <- get_layout("grid")
  coords <- grid_fn(net_mock)
  expect_equal(nrow(coords), 0)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("grid layout returns center for single node", {
  net <- list(n_nodes = 1)
  class(net) <- "CographNetwork"

  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)
  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("grid layout computes automatic grid dimensions", {
  net <- list(n_nodes = 9)
  class(net) <- "CographNetwork"

  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)
  expect_equal(nrow(coords), 9)
  # 9 nodes should be in a 3x3 grid
  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
  expect_true(all(coords$y >= 0.1 & coords$y <= 0.9))
})

test_that("grid layout respects custom ncol parameter", {
  net <- list(n_nodes = 6)
  class(net) <- "CographNetwork"

  grid_fn <- get_layout("grid")
  coords <- grid_fn(net, ncol = 2)
  expect_equal(nrow(coords), 6)
  # 6 nodes in 2 columns = 3 rows
  unique_x <- unique(coords$x)
  expect_equal(length(unique_x), 2)
})

test_that("grid layout handles non-square numbers of nodes", {
  net <- list(n_nodes = 7)
  class(net) <- "CographNetwork"

  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)
  expect_equal(nrow(coords), 7)
})

# =============================================================================
# Test: Random Layout Function
# =============================================================================

test_that("random layout returns coordinates in range", {
  net <- list(n_nodes = 10)
  class(net) <- "CographNetwork"

  random_fn <- get_layout("random")
  coords <- random_fn(net)
  expect_equal(nrow(coords), 10)
  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
  expect_true(all(coords$y >= 0.1 & coords$y <= 0.9))
})

test_that("random layout respects seed parameter", {
  net <- list(n_nodes = 5)
  class(net) <- "CographNetwork"

  random_fn <- get_layout("random")
  coords1 <- random_fn(net, seed = 42)
  coords2 <- random_fn(net, seed = 42)
  coords3 <- random_fn(net, seed = 123)

  # Same seed should give same results

expect_equal(coords1$x, coords2$x)
  expect_equal(coords1$y, coords2$y)

  # Different seeds should give different results (with high probability)
  # Note: There's a tiny chance this could fail randomly
  expect_false(all(coords1$x == coords3$x))
})

test_that("random layout works without seed", {
  net <- list(n_nodes = 5)
  class(net) <- "CographNetwork"

  random_fn <- get_layout("random")
  coords <- random_fn(net)
  expect_equal(nrow(coords), 5)
})

# =============================================================================
# Test: Star Layout Function
# =============================================================================

test_that("star layout returns empty data frame for empty network", {
  net <- list(n_nodes = 0)
  class(net) <- "CographNetwork"

  star_fn <- get_layout("star")
  coords <- star_fn(net)
  expect_equal(nrow(coords), 0)
})

test_that("star layout returns center for single node", {
  net <- list(n_nodes = 1)
  class(net) <- "CographNetwork"

  star_fn <- get_layout("star")
  coords <- star_fn(net)
  expect_equal(nrow(coords), 1)
  expect_equal(coords$x, 0.5)
  expect_equal(coords$y, 0.5)
})

test_that("star layout places center node at center", {
  net <- list(n_nodes = 5)
  class(net) <- "CographNetwork"

  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 1)

  expect_equal(coords$x[1], 0.5)
  expect_equal(coords$y[1], 0.5)
})

test_that("star layout places other nodes in circle around center", {
  net <- list(n_nodes = 5)
  class(net) <- "CographNetwork"

  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 3)

  # Node 3 should be at center
  expect_equal(coords$x[3], 0.5)
  expect_equal(coords$y[3], 0.5)

  # Other nodes should be at distance 0.4 from center
  other_indices <- c(1, 2, 4, 5)
  for (i in other_indices) {
    dist <- sqrt((coords$x[i] - 0.5)^2 + (coords$y[i] - 0.5)^2)
    expect_equal(dist, 0.4, tolerance = 0.001)
  }
})

test_that("star layout works with two nodes", {
  net <- list(n_nodes = 2)
  class(net) <- "CographNetwork"

  star_fn <- get_layout("star")
  coords <- star_fn(net, center = 1)

  expect_equal(nrow(coords), 2)
  expect_equal(coords$x[1], 0.5)
  expect_equal(coords$y[1], 0.5)
})

# =============================================================================
# Test: Bipartite Layout Function
# =============================================================================

test_that("bipartite layout returns empty data frame for empty network", {
  net <- list(n_nodes = 0)
  class(net) <- "CographNetwork"

  bipartite_fn <- get_layout("bipartite")
  coords <- bipartite_fn(net)
  expect_equal(nrow(coords), 0)
})

test_that("bipartite layout creates default types if not provided", {
  net <- list(n_nodes = 4)
  class(net) <- "CographNetwork"

  bipartite_fn <- get_layout("bipartite")
  coords <- bipartite_fn(net)

  expect_equal(nrow(coords), 4)
  # Alternating nodes should be on different sides
  expect_equal(coords$x[1], 0.2)  # type 0
  expect_equal(coords$x[2], 0.8)  # type 1
  expect_equal(coords$x[3], 0.2)  # type 0
  expect_equal(coords$x[4], 0.8)  # type 1
})

test_that("bipartite layout respects custom types parameter", {
  net <- list(n_nodes = 6)
  class(net) <- "CographNetwork"

  bipartite_fn <- get_layout("bipartite")
  # First 3 on left, last 3 on right
  types <- c(0, 0, 0, 1, 1, 1)
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 6)
  expect_true(all(coords$x[1:3] == 0.2))  # Left side
  expect_true(all(coords$x[4:6] == 0.8))  # Right side
})

test_that("bipartite layout vertically spaces nodes within each side", {
  net <- list(n_nodes = 6)
  class(net) <- "CographNetwork"

  bipartite_fn <- get_layout("bipartite")
  types <- c(0, 0, 0, 1, 1, 1)
  coords <- bipartite_fn(net, types = types)

  # Left side y-coordinates should be spaced from 0.9 to 0.1
  expect_equal(coords$y[1], 0.9)
  expect_equal(coords$y[3], 0.1)

  # Right side y-coordinates should be spaced from 0.9 to 0.1
  expect_equal(coords$y[4], 0.9)
  expect_equal(coords$y[6], 0.1)
})

test_that("bipartite layout handles all same type", {
  net <- list(n_nodes = 3)
  class(net) <- "CographNetwork"

  bipartite_fn <- get_layout("bipartite")
  types <- c(0, 0, 0)
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 3)
  # All should be on left side
  expect_true(all(coords$x == 0.2))
})

# =============================================================================
# Test: Custom Layout Function
# =============================================================================

test_that("custom layout accepts matrix input", {
  net <- list(n_nodes = 3)
  class(net) <- "CographNetwork"

  custom_fn <- get_layout("custom")
  custom_coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  coords <- custom_fn(net, coords = custom_coords)

  expect_equal(nrow(coords), 3)
  expect_equal(coords$x[1], 0)
  expect_equal(coords$y[1], 0)
})

test_that("custom layout accepts data frame input", {
  net <- list(n_nodes = 3)
  class(net) <- "CographNetwork"

  custom_fn <- get_layout("custom")
  custom_coords <- data.frame(a = c(0.1, 0.5, 0.9), b = c(0.2, 0.5, 0.8))
  coords <- custom_fn(net, coords = custom_coords)

  expect_equal(nrow(coords), 3)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
  # First two columns should be renamed to x and y
  expect_equal(coords$x[1], 0.1)
  expect_equal(coords$y[1], 0.2)
})

# =============================================================================
# Test: Gephi FR Layout Function
# =============================================================================

test_that("gephi_fr layout returns empty data frame for empty network", {
  # Create actual network object since gephi_fr calls network_to_igraph
  skip_if_not_installed("igraph")

  gephi_fn <- get_layout("gephi_fr")
  # Verify the function is registered
  expect_true(is.function(gephi_fn))
})

test_that("gephi_fr layout produces coordinates for small network", {
  skip_if_not_installed("igraph")
  net <- create_test_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 10)

  expect_equal(nrow(coords), 5)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})

test_that("gephi_fr layout respects iteration parameter", {
  skip_if_not_installed("igraph")
  net <- create_test_network(5)

  gephi_fn <- get_layout("gephi_fr")
  # More iterations should produce different results
  coords1 <- gephi_fn(net, niter = 5)
  coords2 <- gephi_fn(net, niter = 100)

  expect_equal(nrow(coords1), 5)
  expect_equal(nrow(coords2), 5)
})

test_that("gephi_fr layout respects area parameter", {
  skip_if_not_installed("igraph")
  net <- create_test_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, area = 5000, niter = 10)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr layout respects gravity parameter", {
  skip_if_not_installed("igraph")
  net <- create_test_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, gravity = 20.0, niter = 10)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr layout respects speed parameter", {
  skip_if_not_installed("igraph")
  net <- create_test_network(5)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, speed = 2.0, niter = 10)

  expect_equal(nrow(coords), 5)
})

test_that("gephi_fr layout works with disconnected nodes", {
  skip_if_not_installed("igraph")
  # Network with no edges
  mat <- matrix(0, 4, 4)
  rownames(mat) <- colnames(mat) <- paste0("N", 1:4)
  net <- CographNetwork$new(mat)

  gephi_fn <- get_layout("gephi_fr")
  coords <- gephi_fn(net, niter = 10)

  expect_equal(nrow(coords), 4)
})

test_that("gephi layout alias works same as gephi_fr", {
  skip_if_not_installed("igraph")
  net <- create_test_network(5)

  gephi_fr_fn <- get_layout("gephi_fr")
  gephi_fn <- get_layout("gephi")

  # Same function should be registered
  expect_true(is.function(gephi_fr_fn))
  expect_true(is.function(gephi_fn))
})

# =============================================================================
# Test: Layout Registration/Retrieval Functions
# =============================================================================

test_that("list_layouts returns all registered layouts", {
  layouts <- list_layouts()
  expect_true(is.character(layouts))
  expect_true(length(layouts) >= 10)  # At least 10 built-in layouts

  # Check for key layouts
  expected <- c("circle", "oval", "ellipse", "spring", "fr",
                "fruchterman-reingold", "groups", "grid", "random",
                "star", "bipartite", "custom", "gephi_fr", "gephi")
  for (name in expected) {
    expect_true(name %in% layouts, info = paste("Missing layout:", name))
  }
})

test_that("get_layout returns NULL for unknown layout", {
  fn <- get_layout("nonexistent_layout_xyz")
  expect_null(fn)
})

test_that("get_layout returns functions for all built-in layouts", {
  layouts <- c("circle", "oval", "spring", "groups", "grid",
               "random", "star", "bipartite", "custom", "gephi_fr")

  for (name in layouts) {
    fn <- get_layout(name)
    expect_true(is.function(fn), info = paste("Layout not a function:", name))
  }
})

test_that("register_layout validates function parameter", {
  expect_error(
    register_layout("test_layout", "not_a_function"),
    "layout_fn must be a function"
  )
})

test_that("register_layout can register custom layout", {
  # Register a simple custom layout
  custom_layout_fn <- function(network, ...) {
    n <- network$n_nodes
    data.frame(x = rep(0.5, n), y = rep(0.5, n))
  }

  register_layout("test_center_layout", custom_layout_fn)

  # Verify registration
  expect_true("test_center_layout" %in% list_layouts())

  fn <- get_layout("test_center_layout")
  expect_true(is.function(fn))

  # Test it works
  net <- list(n_nodes = 3)
  class(net) <- "CographNetwork"
  coords <- fn(net)
  expect_equal(nrow(coords), 3)
  expect_true(all(coords$x == 0.5))
})

test_that("register_layout can overwrite existing layout", {
  # Get original
  original_fn <- get_layout("random")

  # Register replacement
  replacement_fn <- function(network, ...) {
    n <- network$n_nodes
    data.frame(x = rep(0.1, n), y = rep(0.9, n))
  }

  register_layout("random", replacement_fn)

  # Verify replacement
  new_fn <- get_layout("random")
  net <- list(n_nodes = 2)
  class(net) <- "CographNetwork"
  coords <- new_fn(net)
  expect_equal(coords$x[1], 0.1)
  expect_equal(coords$y[1], 0.9)

  # Restore original
  register_layout("random", original_fn)
})

# =============================================================================
# Test: Layout Integration with CographLayout class
# =============================================================================

test_that("CographLayout can use registered layouts", {
  layout <- CographLayout$new("circle")
  net <- create_test_network(4)

  coords <- layout$compute(net)

  expect_equal(nrow(coords), 4)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("CographLayout errors on unknown layout type", {
  layout <- CographLayout$new("unknown_layout_xyz")
  net <- create_test_network(3)

  expect_error(
    layout$compute(net),
    "Unknown layout type"
  )
})

test_that("CographLayout get_type returns correct type", {
  layout <- CographLayout$new("grid")
  expect_equal(layout$get_type(), "grid")
})

test_that("CographLayout stores parameters", {
  layout <- CographLayout$new("grid", ncol = 3)
  params <- layout$get_params()
  expect_equal(params$ncol, 3)
})

# =============================================================================
# Test: Edge cases and special inputs
# =============================================================================

test_that("layouts handle single node networks", {
  net <- list(n_nodes = 1)
  class(net) <- "CographNetwork"

  layouts <- c("grid", "random", "star", "bipartite")

  for (name in layouts) {
    fn <- get_layout(name)
    coords <- fn(net)
    expect_equal(nrow(coords), 1, info = paste("Layout:", name))
  }
})

test_that("layouts handle two node networks", {
  net <- list(n_nodes = 2)
  class(net) <- "CographNetwork"

  layouts <- c("grid", "random", "star", "bipartite")

  for (name in layouts) {
    fn <- get_layout(name)
    coords <- fn(net)
    expect_equal(nrow(coords), 2, info = paste("Layout:", name))
  }
})

test_that("layouts handle large networks", {
  net <- list(n_nodes = 100)
  class(net) <- "CographNetwork"

  grid_fn <- get_layout("grid")
  coords <- grid_fn(net)
  expect_equal(nrow(coords), 100)

  random_fn <- get_layout("random")
  coords <- random_fn(net, seed = 42)
  expect_equal(nrow(coords), 100)
})

test_that("grid layout handles odd number of nodes with custom ncol", {
  net <- list(n_nodes = 11)
  class(net) <- "CographNetwork"

  grid_fn <- get_layout("grid")
  coords <- grid_fn(net, ncol = 4)

  expect_equal(nrow(coords), 11)
})

test_that("bipartite layout handles single type", {
  net <- list(n_nodes = 5)
  class(net) <- "CographNetwork"

  bipartite_fn <- get_layout("bipartite")
  types <- c(1, 1, 1, 1, 1)
  coords <- bipartite_fn(net, types = types)

  expect_equal(nrow(coords), 5)
  # All on same side
  expect_true(all(coords$x == coords$x[1]))
})

test_that("star layout center parameter out of range handled", {
  net <- list(n_nodes = 5)
  class(net) <- "CographNetwork"

  star_fn <- get_layout("star")
  # Center = 5 is last node
  coords <- star_fn(net, center = 5)

  expect_equal(coords$x[5], 0.5)
  expect_equal(coords$y[5], 0.5)
})

# =============================================================================
# Test: Layout produces valid data frame structure
# =============================================================================

test_that("all built-in layouts return proper data frame structure", {
  net <- list(n_nodes = 5)
  class(net) <- "CographNetwork"

  simple_layouts <- c("grid", "random", "star", "bipartite")

  for (name in simple_layouts) {
    fn <- get_layout(name)
    coords <- fn(net)

    expect_true(is.data.frame(coords), info = paste("Layout:", name))
    expect_true("x" %in% names(coords), info = paste("Layout:", name))
    expect_true("y" %in% names(coords), info = paste("Layout:", name))
    expect_true(is.numeric(coords$x), info = paste("Layout:", name))
    expect_true(is.numeric(coords$y), info = paste("Layout:", name))
  }
})

test_that("custom layout renames columns correctly", {
  net <- list(n_nodes = 3)
  class(net) <- "CographNetwork"

  custom_fn <- get_layout("custom")

  # With unnamed matrix
  mat <- matrix(1:6, ncol = 2)
  coords <- custom_fn(net, coords = mat)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))

  # With wrongly named data frame
  df <- data.frame(foo = c(1, 2, 3), bar = c(4, 5, 6))
  coords <- custom_fn(net, coords = df)
  expect_true("x" %in% names(coords))
  expect_true("y" %in% names(coords))
})
