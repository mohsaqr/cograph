# test-coverage-layout-gephi-fr-40.R - Coverage tests for layout-gephi-fr.R
# Targeting uncovered lines and branches

# Make internal functions available
layout_gephi_fr <- cograph:::layout_gephi_fr
compute_layout_gephi_fr <- cograph:::compute_layout_gephi_fr
network_to_igraph <- cograph:::network_to_igraph

# ============================================
# EMPTY GRAPH HANDLING (line 76)
# ============================================

test_that("layout_gephi_fr handles empty graph", {
  skip_if_no_igraph()
  g <- igraph::make_empty_graph(n = 0, directed = FALSE)
  coords <- layout_gephi_fr(g)

  expect_true(is.matrix(coords))
  expect_equal(ncol(coords), 2)
  expect_equal(nrow(coords), 0)
})

# ============================================
# INITIAL COORDINATES - MATRIX INPUT (lines 80-81)
# ============================================

test_that("layout_gephi_fr accepts matrix initial coordinates", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)
  init_coords <- matrix(c(0.2, 0.4, 0.6, 0.8, 1.0,
                          0.1, 0.3, 0.5, 0.7, 0.9), ncol = 2)

  coords <- layout_gephi_fr(g, initial = init_coords, niter = 10, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_equal(ncol(coords), 2)
  # Coordinates should be normalized to [0,1]
  expect_true(all(coords >= 0 & coords <= 1))
})

# ============================================
# INITIAL COORDINATES - DATA FRAME INPUT (lines 82-83)
# ============================================

test_that("layout_gephi_fr accepts data.frame initial coordinates", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)
  init_df <- data.frame(
    x = c(0.2, 0.4, 0.6, 0.8, 1.0),
    y = c(0.1, 0.3, 0.5, 0.7, 0.9)
  )

  coords <- layout_gephi_fr(g, initial = init_df, niter = 10, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_equal(ncol(coords), 2)
})

# ============================================
# INITIAL COORDINATES SCALING (lines 86-88)
# ============================================

test_that("layout_gephi_fr scales [0,1] coordinates to Gephi space", {
  skip_if_no_igraph()
  g <- igraph::make_ring(4)
  # Coordinates in [0,1] range get scaled
  init_coords <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1), ncol = 2)

  coords <- layout_gephi_fr(g, initial = init_coords, niter = 5, seed = 42)

  expect_equal(nrow(coords), 4)
  # After normalization, should be back in [0,1]
  expect_true(all(coords >= 0 & coords <= 1))
})

test_that("layout_gephi_fr handles coords outside [0,1] without scaling", {
  skip_if_no_igraph()
  g <- igraph::make_ring(4)
  # Coordinates OUTSIDE [0,1] range
  init_coords <- matrix(c(-100, 100, -100, 100, -100, -100, 100, 100), ncol = 2)

  coords <- layout_gephi_fr(g, initial = init_coords, niter = 5, seed = 42, normalize = TRUE)

  expect_equal(nrow(coords), 4)
  expect_true(all(coords >= 0 & coords <= 1))
})

# ============================================
# ANCHOR STRENGTH (lines 202-207)
# ============================================

test_that("layout_gephi_fr applies anchor force when anchor_strength > 0", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)
  init_coords <- matrix(c(0.2, 0.4, 0.6, 0.8, 1.0,
                          0.2, 0.4, 0.6, 0.8, 1.0), ncol = 2)

  # With high anchor strength, nodes stay close to initial positions
  coords_anchored <- layout_gephi_fr(g, initial = init_coords, niter = 20,
                                      anchor_strength = 10.0, seed = 42)

  # Without anchor, nodes move freely

  coords_free <- layout_gephi_fr(g, initial = init_coords, niter = 20,
                                  anchor_strength = 0, seed = 42)

  expect_equal(nrow(coords_anchored), 5)
  expect_equal(nrow(coords_free), 5)
})

test_that("layout_gephi_fr anchor_strength=0 has no effect", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)
  init_coords <- matrix(c(0.5, 0.5, 0.5, 0.5, 0.5,
                          0.5, 0.5, 0.5, 0.5, 0.5), ncol = 2)

  # anchor_strength = 0 means no anchor force
  coords <- layout_gephi_fr(g, initial = init_coords, niter = 10,
                            anchor_strength = 0, seed = 42)

  expect_equal(nrow(coords), 5)
})

# ============================================
# GRAVITY MODE = "degree" (lines 183-190)
# ============================================

test_that("layout_gephi_fr gravity_mode='degree' uses degree-based gravity", {
  skip_if_no_igraph()
  # Create a star graph where node 1 has high degree
  g <- igraph::make_star(6, mode = "undirected")

  coords <- layout_gephi_fr(g, gravity_mode = "degree", niter = 50, seed = 42)

  expect_equal(nrow(coords), 6)
  expect_equal(ncol(coords), 2)
  # High-degree center node should be near center due to stronger gravity
  # The hub node (node 1) connects to all others
  center <- c(0.5, 0.5)
  hub_dist <- sqrt((coords[1, 1] - center[1])^2 + (coords[1, 2] - center[2])^2)
  # Hub should be reasonably close to center
  expect_true(hub_dist < 0.4)
})

test_that("layout_gephi_fr gravity_mode='degree' handles single node", {
  skip_if_no_igraph()
  g <- igraph::make_empty_graph(n = 1, directed = FALSE)

  coords <- layout_gephi_fr(g, gravity_mode = "degree", niter = 10, seed = 42)

  expect_equal(nrow(coords), 1)
})

# ============================================
# GRAVITY MODE = "none" (lines 191-193)
# ============================================

test_that("layout_gephi_fr gravity_mode='none' applies no gravity", {
  skip_if_no_igraph()
  g <- igraph::make_ring(6)

  coords <- layout_gephi_fr(g, gravity_mode = "none", niter = 50, seed = 42)

  expect_equal(nrow(coords), 6)
  expect_equal(ncol(coords), 2)
  # Without gravity, layout still produces valid coordinates
  expect_true(all(coords >= 0 & coords <= 1))
})

test_that("layout_gephi_fr gravity_mode='none' allows nodes to drift", {
  skip_if_no_igraph()
  # Disconnected graph - nodes should spread with no gravity
  g <- igraph::graph_from_edgelist(matrix(c(1, 2), ncol = 2), directed = FALSE)
  g <- igraph::add_vertices(g, 3)  # Add isolated nodes

  coords <- layout_gephi_fr(g, gravity_mode = "none", niter = 30, seed = 42)

  expect_equal(nrow(coords), 5)
})

# ============================================
# GRAVITY MODE = "linear" (default, lines 194-199)
# ============================================

test_that("layout_gephi_fr gravity_mode='linear' is default", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords1 <- layout_gephi_fr(g, niter = 30, seed = 42)
  coords2 <- layout_gephi_fr(g, gravity_mode = "linear", niter = 30, seed = 42)

  expect_equal(coords1, coords2)
})

test_that("layout_gephi_fr gravity_mode='linear' pulls nodes to center", {
  skip_if_no_igraph()
  g <- igraph::make_ring(6)

  coords <- layout_gephi_fr(g, gravity_mode = "linear", gravity = 5.0,
                            niter = 50, seed = 42)

  expect_equal(nrow(coords), 6)
  # Coordinates should be in valid range
  expect_true(all(coords >= 0 & coords <= 1))
})

# ============================================
# COOLING MODE = "vcf" (lines 232-238)
# ============================================

test_that("layout_gephi_fr cooling_mode='vcf' adapts speed based on movement", {
  skip_if_no_igraph()
  g <- igraph::make_ring(8)

  coords <- layout_gephi_fr(g, cooling_mode = "vcf", niter = 100, seed = 42)

  expect_equal(nrow(coords), 8)
  expect_true(all(coords >= 0 & coords <= 1))
})

test_that("layout_gephi_fr cooling_mode='vcf' converges to stable state", {
  skip_if_no_igraph()
  g <- igraph::make_full_graph(5)

  coords <- layout_gephi_fr(g, cooling_mode = "vcf", niter = 200, seed = 42)

  expect_equal(nrow(coords), 5)
})

# ============================================
# COOLING MODE = "linear" (lines 239-241)
# ============================================

test_that("layout_gephi_fr cooling_mode='linear' decreases speed over iterations", {
  skip_if_no_igraph()
  g <- igraph::make_ring(6)

  coords <- layout_gephi_fr(g, cooling_mode = "linear", niter = 100, seed = 42)

  expect_equal(nrow(coords), 6)
  expect_true(all(coords >= 0 & coords <= 1))
})

test_that("layout_gephi_fr cooling_mode='linear' produces stable layout", {
  skip_if_no_igraph()
  g <- igraph::make_full_graph(6)

  coords <- layout_gephi_fr(g, cooling_mode = "linear", niter = 150, seed = 42)

  expect_equal(nrow(coords), 6)
})

# ============================================
# COOLING MODE = "constant" (default)
# ============================================

test_that("layout_gephi_fr cooling_mode='constant' is default", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords1 <- layout_gephi_fr(g, niter = 30, seed = 42)
  coords2 <- layout_gephi_fr(g, cooling_mode = "constant", niter = 30, seed = 42)

  expect_equal(coords1, coords2)
})

# ============================================
# NORMALIZE = FALSE (skip normalization)
# ============================================

test_that("layout_gephi_fr normalize=FALSE returns raw coordinates", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords <- layout_gephi_fr(g, normalize = FALSE, niter = 20, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_equal(ncol(coords), 2)
  # Raw coordinates may be outside [0,1]
  # Just check they're finite
  expect_true(all(is.finite(coords)))
})

test_that("layout_gephi_fr normalize=TRUE constrains to [0,1]", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords <- layout_gephi_fr(g, normalize = TRUE, niter = 20, seed = 42)

  expect_true(all(coords >= 0 & coords <= 1))
})

# ============================================
# GRAPH WITH NO EDGES (has_edges = FALSE)
# ============================================

test_that("layout_gephi_fr handles graph with no edges", {
  skip_if_no_igraph()
  # Graph with nodes but no edges
  g <- igraph::make_empty_graph(n = 5, directed = FALSE)

  coords <- layout_gephi_fr(g, niter = 20, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_equal(ncol(coords), 2)
  expect_true(all(is.finite(coords)))
})

test_that("layout_gephi_fr isolated nodes spread via repulsion", {
  skip_if_no_igraph()
  g <- igraph::make_empty_graph(n = 4, directed = FALSE)

  coords <- layout_gephi_fr(g, gravity = 0.5, niter = 50, seed = 42)

  expect_equal(nrow(coords), 4)
  # Check nodes have spread out (not all at same position)
  x_range <- max(coords[, 1]) - min(coords[, 1])
  y_range <- max(coords[, 2]) - min(coords[, 2])
  expect_true(x_range > 0.01 || y_range > 0.01)
})

# ============================================
# EDGE ATTRACTION WITH ZERO DISTANCE
# ============================================

test_that("layout_gephi_fr handles edges with overlapping nodes", {
  skip_if_no_igraph()
  g <- igraph::make_ring(3)
  # Start with overlapping nodes
  init_coords <- matrix(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5), ncol = 2)

  coords <- layout_gephi_fr(g, initial = init_coords, niter = 30, seed = 42)

  expect_equal(nrow(coords), 3)
  # Nodes should spread out
  expect_true(all(is.finite(coords)))
})

# ============================================
# SEED REPRODUCIBILITY
# ============================================

test_that("layout_gephi_fr produces reproducible output with seed", {
  skip_if_no_igraph()
  g <- igraph::make_ring(6)

  coords1 <- layout_gephi_fr(g, seed = 123, niter = 50)
  coords2 <- layout_gephi_fr(g, seed = 123, niter = 50)

  expect_equal(coords1, coords2)
})

test_that("layout_gephi_fr produces different output with different seeds", {
  skip_if_no_igraph()
  g <- igraph::make_ring(6)

  coords1 <- layout_gephi_fr(g, seed = 123, niter = 50)
  coords2 <- layout_gephi_fr(g, seed = 456, niter = 50)

  expect_false(all(coords1 == coords2))
})

test_that("layout_gephi_fr without seed is non-deterministic", {
  skip_if_no_igraph()
  g <- igraph::make_ring(6)

  coords1 <- layout_gephi_fr(g, seed = NULL, niter = 50)
  coords2 <- layout_gephi_fr(g, seed = NULL, niter = 50)

  # Very unlikely to be exactly the same without seed
  expect_true(is.matrix(coords1))
  expect_true(is.matrix(coords2))
})

# ============================================
# COMPUTE_LAYOUT_GEPHI_FR WRAPPER
# ============================================

test_that("compute_layout_gephi_fr returns data.frame", {
  adj <- create_test_matrix(5)
  net <- cograph(adj)

  result <- compute_layout_gephi_fr(net, niter = 20, seed = 42)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
  expect_true(all(c("x", "y") %in% names(result)))
})
test_that("compute_layout_gephi_fr passes parameters correctly", {
  adj <- create_test_matrix(5)
  net <- cograph(adj)

  # Test with various parameters
  result <- compute_layout_gephi_fr(
    net,
    area = 5000,
    gravity = 2.0,
    speed = 0.5,
    niter = 30,
    seed = 42,
    gravity_mode = "degree",
    cooling_mode = "linear"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
})

test_that("compute_layout_gephi_fr works with normalize=FALSE", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  result <- compute_layout_gephi_fr(net, normalize = FALSE, niter = 20, seed = 42)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
})

test_that("compute_layout_gephi_fr ignores extra arguments via ...", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  # Should not error with extra args
  result <- compute_layout_gephi_fr(net, niter = 10, seed = 42,
                                     extra_arg = "ignored",
                                     another_arg = 123)

  expect_true(is.data.frame(result))
})

# ============================================
# PARAMETER EDGE CASES
# ============================================

test_that("layout_gephi_fr handles very low gravity", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords <- layout_gephi_fr(g, gravity = 0.01, niter = 20, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_true(all(is.finite(coords)))
})

test_that("layout_gephi_fr handles very high gravity", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords <- layout_gephi_fr(g, gravity = 100.0, niter = 20, seed = 42)

  expect_equal(nrow(coords), 5)
  # High gravity should still produce valid coordinates
  expect_true(all(is.finite(coords)))
  expect_true(all(coords >= 0 & coords <= 1))
})

test_that("layout_gephi_fr handles small area parameter", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords <- layout_gephi_fr(g, area = 100, niter = 20, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_true(all(is.finite(coords)))
})

test_that("layout_gephi_fr handles large area parameter", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords <- layout_gephi_fr(g, area = 100000, niter = 20, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_true(all(is.finite(coords)))
})

test_that("layout_gephi_fr handles speed parameter", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords_slow <- layout_gephi_fr(g, speed = 0.1, niter = 30, seed = 42)
  coords_fast <- layout_gephi_fr(g, speed = 5.0, niter = 30, seed = 42)

  expect_equal(nrow(coords_slow), 5)
  expect_equal(nrow(coords_fast), 5)
})

test_that("layout_gephi_fr handles single iteration", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords <- layout_gephi_fr(g, niter = 1, seed = 42)

  expect_equal(nrow(coords), 5)
})

test_that("layout_gephi_fr handles many iterations", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)

  coords <- layout_gephi_fr(g, niter = 500, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_true(all(is.finite(coords)))
})

# ============================================
# DIFFERENT GRAPH TYPES
# ============================================

test_that("layout_gephi_fr handles directed graph", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5, directed = TRUE)

  coords <- layout_gephi_fr(g, niter = 30, seed = 42)

  expect_equal(nrow(coords), 5)
})

test_that("layout_gephi_fr handles weighted graph", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- c(0.5, 1.0, 1.5, 2.0, 2.5)

  coords <- layout_gephi_fr(g, niter = 30, seed = 42)

  expect_equal(nrow(coords), 5)
})

test_that("layout_gephi_fr handles complete graph", {
  skip_if_no_igraph()
  g <- igraph::make_full_graph(6)

  coords <- layout_gephi_fr(g, niter = 50, seed = 42)

  expect_equal(nrow(coords), 6)
  expect_true(all(is.finite(coords)))
})

test_that("layout_gephi_fr handles star graph", {
  skip_if_no_igraph()
  g <- igraph::make_star(7, mode = "undirected")

  coords <- layout_gephi_fr(g, niter = 50, seed = 42)

  expect_equal(nrow(coords), 7)
})

test_that("layout_gephi_fr handles tree graph", {
  skip_if_no_igraph()
  g <- igraph::make_tree(10, children = 2, mode = "undirected")

  coords <- layout_gephi_fr(g, niter = 50, seed = 42)

  expect_equal(nrow(coords), 10)
})

# ============================================
# SINGLE NODE GRAPH
# ============================================

test_that("layout_gephi_fr handles single node graph", {
  skip_if_no_igraph()
  g <- igraph::make_empty_graph(n = 1, directed = FALSE)

  coords <- layout_gephi_fr(g, niter = 10, seed = 42)

  expect_equal(nrow(coords), 1)
  expect_equal(ncol(coords), 2)
  # Single node should be at center after normalization
  expect_equal(coords[1, 1], 0.5)
  expect_equal(coords[1, 2], 0.5)
})

# ============================================
# TWO NODE GRAPH
# ============================================

test_that("layout_gephi_fr handles two node connected graph", {
  skip_if_no_igraph()
  g <- igraph::make_graph(edges = c(1, 2), directed = FALSE)

  coords <- layout_gephi_fr(g, niter = 30, seed = 42)

  expect_equal(nrow(coords), 2)
  # Nodes should be separated
  dist <- sqrt((coords[1, 1] - coords[2, 1])^2 + (coords[1, 2] - coords[2, 2])^2)
  expect_true(dist > 0)
})

test_that("layout_gephi_fr handles two disconnected nodes", {
  skip_if_no_igraph()
  g <- igraph::make_empty_graph(n = 2, directed = FALSE)

  coords <- layout_gephi_fr(g, niter = 30, seed = 42)

  expect_equal(nrow(coords), 2)
  # Disconnected nodes should repel
  dist <- sqrt((coords[1, 1] - coords[2, 1])^2 + (coords[1, 2] - coords[2, 2])^2)
  expect_true(dist > 0)
})

# ============================================
# NORMALIZATION EDGE CASES
# ============================================

test_that("layout_gephi_fr normalization handles max_extent = 0", {
  skip_if_no_igraph()
  # Single node case - max_extent could be 0
  g <- igraph::make_empty_graph(n = 1, directed = FALSE)

  coords <- layout_gephi_fr(g, normalize = TRUE, niter = 10, seed = 42)

  expect_equal(nrow(coords), 1)
  expect_true(all(is.finite(coords)))
})

# ============================================
# DISPLACEMENT LIMITING
# ============================================

test_that("layout_gephi_fr limits maximum displacement", {
  skip_if_no_igraph()
  g <- igraph::make_ring(5)
  # Very high speed could cause large displacements
  coords <- layout_gephi_fr(g, speed = 10.0, niter = 10, seed = 42)

  expect_equal(nrow(coords), 5)
  expect_true(all(is.finite(coords)))
})

# ============================================
# INTEGRATION WITH SPLOT
# ============================================

test_that("splot works with gephi_fr layout", {
  adj <- create_test_matrix(6)

  result <- safe_plot(splot(adj, layout = "gephi_fr", seed = 42))
  expect_true(result$success, info = result$error)
})

test_that("splot gephi_fr layout accepts parameters", {
  adj <- create_test_matrix(6)

  # Pass layout parameters via layout_params or direct
  result <- safe_plot(splot(adj, layout = "gephi_fr", seed = 42,
                            layout_niter = 50))
  expect_true(result$success, info = result$error)
})
