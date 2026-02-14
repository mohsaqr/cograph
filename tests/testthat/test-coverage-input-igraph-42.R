# =============================================================================
# Test Coverage for input-igraph.R
# =============================================================================
# Comprehensive tests targeting uncovered functions and branches
# Tests all igraph input parsing and layout functions

# Make internal functions available for testing
parse_igraph <- cograph:::parse_igraph
apply_igraph_layout <- cograph:::apply_igraph_layout
apply_igraph_layout_by_name <- cograph:::apply_igraph_layout_by_name
network_to_igraph <- cograph:::network_to_igraph
normalize_coords <- cograph:::normalize_coords
create_nodes_df <- cograph:::create_nodes_df
create_edges_df <- cograph:::create_edges_df

# =============================================================================
# Test: parse_igraph() - Basic Functionality
# =============================================================================

test_that("parse_igraph parses simple undirected graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- parse_igraph(g)

  expect_true(is.list(result))
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_true("directed" %in% names(result))
  expect_true("weights" %in% names(result))
  expect_equal(nrow(result$nodes), 5)
  expect_false(result$directed)
})

test_that("parse_igraph parses directed graph correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4, directed = TRUE)
  result <- parse_igraph(g)

  expect_true(result$directed)
  expect_equal(nrow(result$nodes), 4)
})

test_that("parse_igraph respects forced directed parameter", {
  skip_if_not_installed("igraph")

  # Create undirected graph but force directed = TRUE
  g <- igraph::make_ring(3)
  result <- parse_igraph(g, directed = TRUE)

  expect_true(result$directed)
})

test_that("parse_igraph respects forced undirected parameter", {
  skip_if_not_installed("igraph")

  # Create directed graph but force directed = FALSE
  g <- igraph::make_ring(3, directed = TRUE)
  result <- parse_igraph(g, directed = FALSE)

  expect_false(result$directed)
})

# =============================================================================
# Test: parse_igraph() - Node Labels Handling
# =============================================================================

test_that("parse_igraph uses default labels when names are null", {
  skip_if_not_installed("igraph")

  # Create graph without vertex names
  g <- igraph::make_empty_graph(4)
  result <- parse_igraph(g)

  expect_equal(result$nodes$label, c("1", "2", "3", "4"))
})

test_that("parse_igraph uses vertex names as labels", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("Alice", "Bob", "Carol")
  result <- parse_igraph(g)

  expect_equal(result$nodes$label, c("Alice", "Bob", "Carol"))
})

test_that("parse_igraph handles NA vertex names", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c(NA, NA, NA)
  result <- parse_igraph(g)

  # Should fall back to numeric labels

  expect_equal(result$nodes$label, c("1", "2", "3"))
})

test_that("parse_igraph handles mixed NA/valid vertex names", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  # Setting partial NA names - igraph keeps them
  igraph::V(g)$name <- c("A", NA, "C")
  result <- parse_igraph(g)

  # Names vector exists but has some NAs - should use names
  expect_true(!all(is.na(result$nodes$label)))
})

# =============================================================================
# Test: parse_igraph() - Edge Weights
# =============================================================================

test_that("parse_igraph extracts edge weights", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  igraph::E(g)$weight <- c(0.5, 1.0, 1.5, 2.0)
  result <- parse_igraph(g)

  expect_equal(result$weights, c(0.5, 1.0, 1.5, 2.0))
  expect_true("weight" %in% names(result$edges))
})

test_that("parse_igraph uses default weights when not present", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  result <- parse_igraph(g)

  # All weights should be 1
  expect_true(all(result$weights == 1))
})

test_that("parse_igraph handles graph with zero edges", {
  skip_if_not_installed("igraph")

  g <- igraph::make_empty_graph(5)
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 5)
  expect_equal(nrow(result$edges), 0)
  expect_equal(length(result$weights), 0)
})

# =============================================================================
# Test: parse_igraph() - Vertex Attributes
# =============================================================================

test_that("parse_igraph copies additional vertex attributes", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::V(g)$color <- c("red", "green", "blue")
  igraph::V(g)$size <- c(10, 20, 30)
  result <- parse_igraph(g)

  expect_true("color" %in% names(result$nodes))
  expect_true("size" %in% names(result$nodes))
  expect_equal(result$nodes$color, c("red", "green", "blue"))
  expect_equal(result$nodes$size, c(10, 20, 30))
})

test_that("parse_igraph skips 'name' vertex attribute (already used as label)", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("A", "B", "C")
  igraph::V(g)$group <- c(1, 1, 2)
  result <- parse_igraph(g)

  # 'group' should be copied, but 'name' should not appear twice
  expect_true("group" %in% names(result$nodes))
  expect_equal(result$nodes$label, c("A", "B", "C"))
})

# =============================================================================
# Test: parse_igraph() - Edge Attributes
# =============================================================================

test_that("parse_igraph copies additional edge attributes", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::E(g)$color <- c("red", "green", "blue")
  igraph::E(g)$lty <- c(1, 2, 3)
  result <- parse_igraph(g)

  expect_true("color" %in% names(result$edges))
  expect_true("lty" %in% names(result$edges))
  expect_equal(result$edges$color, c("red", "green", "blue"))
})

test_that("parse_igraph skips 'weight' edge attribute (already extracted)", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::E(g)$weight <- c(0.5, 1.0, 1.5)
  igraph::E(g)$type <- c("A", "B", "C")
  result <- parse_igraph(g)

  # 'type' should be copied
  expect_true("type" %in% names(result$edges))
  # Weight should be in edges df and also in weights vector
  expect_equal(result$edges$weight, c(0.5, 1.0, 1.5))
})

# =============================================================================
# Test: parse_igraph() - Error Handling
# =============================================================================

test_that("parse_igraph errors on non-igraph input", {
  skip_if_not_installed("igraph")

  expect_error(parse_igraph("not an igraph"), "Input must be an igraph object")
  expect_error(parse_igraph(list(a = 1)), "Input must be an igraph object")
  expect_error(parse_igraph(matrix(1:4, 2, 2)), "Input must be an igraph object")
})

# =============================================================================
# Test: network_to_igraph() - Conversion
# =============================================================================

test_that("network_to_igraph converts cograph_network to igraph", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  g <- network_to_igraph(net)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 3)
})

test_that("network_to_igraph converts CographNetwork (R6) to igraph", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  g <- network_to_igraph(net)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 3)
})

test_that("network_to_igraph handles empty edges", {
  skip_if_not_installed("igraph")

  # Create network with no edges
  mat <- matrix(0, 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  g <- network_to_igraph(net)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 3)
  expect_equal(igraph::ecount(g), 0)
})

test_that("network_to_igraph preserves edge weights", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
  net <- as_cograph(mat)

  g <- network_to_igraph(net)

  expect_true("weight" %in% igraph::edge_attr_names(g))
})

test_that("network_to_igraph preserves directedness", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- as_cograph(mat, directed = TRUE)

  g <- network_to_igraph(net)

  expect_true(igraph::is_directed(g))
})

test_that("network_to_igraph preserves node labels", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  rownames(mat) <- colnames(mat) <- c("Node1", "Node2")
  net <- as_cograph(mat)

  g <- network_to_igraph(net)

  expect_equal(igraph::V(g)$name, c("Node1", "Node2"))
})

test_that("network_to_igraph errors on invalid input", {
  skip_if_not_installed("igraph")

  expect_error(network_to_igraph("not a network"),
               "network must be a CographNetwork or cograph_network object")
  expect_error(network_to_igraph(list(a = 1)),
               "network must be a CographNetwork or cograph_network object")
})

# =============================================================================
# Test: normalize_coords() - Coordinate Normalization
# =============================================================================

test_that("normalize_coords normalizes to 0.1-0.9 range", {
  coords <- matrix(c(0, 100, 50, 0, 100, 50), ncol = 2)
  result <- normalize_coords(coords)

  expect_true(all(result >= 0.1))
  expect_true(all(result <= 0.9))
  # Check the range is properly normalized
  expect_equal(min(result[, 1]), 0.1)
  expect_equal(max(result[, 1]), 0.9)
})

test_that("normalize_coords handles single node", {
  coords <- matrix(c(5, 10), nrow = 1)
  result <- normalize_coords(coords)

  expect_equal(nrow(result), 1)
  expect_equal(result[1, 1], 0.5)
  expect_equal(result[1, 2], 0.5)
})

test_that("normalize_coords handles zero range in one dimension", {
  # All x coordinates are the same
  coords <- matrix(c(5, 5, 5, 0, 50, 100), ncol = 2)
  result <- normalize_coords(coords)

  # X should be 0.5 for all (zero range)
  expect_equal(result[, 1], c(0.5, 0.5, 0.5))
  # Y should be normalized
  expect_equal(result[, 2], c(0.1, 0.5, 0.9))
})

test_that("normalize_coords handles negative coordinates", {
  coords <- matrix(c(-100, 0, 100, -50, 0, 50), ncol = 2)
  result <- normalize_coords(coords)

  expect_true(all(result >= 0.1))
  expect_true(all(result <= 0.9))
})

test_that("normalize_coords handles all same values", {
  coords <- matrix(c(5, 5, 5, 5, 5, 5), ncol = 2)
  result <- normalize_coords(coords)

  # All should be centered at 0.5
  expect_equal(unique(result[, 1]), 0.5)
  expect_equal(unique(result[, 2]), 0.5)
})

# =============================================================================
# Test: apply_igraph_layout_by_name() - Layout Name Mapping
# =============================================================================

test_that("apply_igraph_layout_by_name works with two-letter aliases", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  # Test circle layout
  coords_ci <- apply_igraph_layout_by_name(net, "ci", seed = 42)
  expect_equal(nrow(coords_ci), 3)
  expect_true(all(c("x", "y") %in% names(coords_ci)))

  # Test star layout
  coords_st <- apply_igraph_layout_by_name(net, "st", seed = 42)
  expect_equal(nrow(coords_st), 3)
})

test_that("apply_igraph_layout_by_name works with full names", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout_by_name(net, "layout_nicely", seed = 42)
  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("apply_igraph_layout_by_name works with igraph_ prefix aliases", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout_by_name(net, "igraph_circle", seed = 42)
  expect_equal(nrow(coords), 3)
})

test_that("apply_igraph_layout_by_name errors on unknown layout", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(apply_igraph_layout_by_name(net, "unknown_layout"),
               "Unknown igraph layout")
})

test_that("apply_igraph_layout_by_name respects seed for determinism", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0), nrow = 4)
  net <- as_cograph(mat)

  # Same seed should give same results
  coords1 <- apply_igraph_layout_by_name(net, "fr", seed = 123)
  coords2 <- apply_igraph_layout_by_name(net, "fr", seed = 123)

  expect_equal(coords1$x, coords2$x, tolerance = 1e-10)
  expect_equal(coords1$y, coords2$y, tolerance = 1e-10)
})

test_that("apply_igraph_layout_by_name works with NULL seed", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  # NULL seed should work without error
  coords <- apply_igraph_layout_by_name(net, "ci", seed = NULL)
  expect_equal(nrow(coords), 3)
})

# =============================================================================
# Test: apply_igraph_layout() - Direct Layout Function
# =============================================================================

test_that("apply_igraph_layout applies layout function directly", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout(net, igraph::layout_in_circle)

  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
  expect_true(all(coords$y >= 0.1 & coords$y <= 0.9))
})

test_that("apply_igraph_layout passes additional arguments", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  # layout_with_fr accepts niter parameter
  coords <- apply_igraph_layout(net, igraph::layout_with_fr, niter = 10)

  expect_equal(nrow(coords), 3)
})

# =============================================================================
# Test: Layout Aliases Comprehensive
# =============================================================================

test_that("all two-letter layout aliases are valid", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  aliases <- c("kk", "fr", "mds", "st", "gr", "rd", "ni", "ci")

  for (alias in aliases) {
    coords <- apply_igraph_layout_by_name(net, alias, seed = 42)
    expect_equal(nrow(coords), 3,
                 info = paste("Failed for alias:", alias))
  }
})

test_that("layout_with_drl works with larger network", {
  skip_if_not_installed("igraph")

  # DRL needs more nodes to work properly
  mat <- create_test_matrix(n = 10, density = 0.3, seed = 42)
  net <- as_cograph(mat)

  # DRL may warn on small graphs but should work
  coords <- suppressWarnings(
    apply_igraph_layout_by_name(net, "drl", seed = 42)
  )

  expect_equal(nrow(coords), 10)
})

test_that("layout_with_lgl works with larger connected network", {
  skip_if_not_installed("igraph")

  # LGL needs connected graph
  mat <- create_test_topology("complete", n = 6)
  net <- as_cograph(mat)

  # LGL may produce warnings on some configurations
  coords <- suppressWarnings(
    apply_igraph_layout_by_name(net, "lgl", seed = 42)
  )

  expect_equal(nrow(coords), 6)
})

test_that("layout_as_tree works with tree-like structure", {
  skip_if_not_installed("igraph")

  # Create a star topology (tree-like)
  mat <- create_test_topology("star", n = 5)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout_by_name(net, "tr", seed = 42)

  expect_equal(nrow(coords), 5)
})

test_that("layout_on_grid arranges nodes in grid", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(n = 9, density = 0.3)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout_by_name(net, "gr", seed = 42)

  expect_equal(nrow(coords), 9)
})

test_that("layout_randomly produces random layout", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(n = 5, density = 0.5)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout_by_name(net, "rd", seed = 42)

  expect_equal(nrow(coords), 5)
})

test_that("layout_on_sphere works", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(n = 5, density = 0.5)
  net <- as_cograph(mat)

  # Sphere layout returns 3D coords, but we only use x,y
  coords <- apply_igraph_layout_by_name(net, "sp", seed = 42)

  expect_equal(nrow(coords), 5)
})

test_that("layout_with_graphopt works", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(n = 6, density = 0.4)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout_by_name(net, "go", seed = 42)

  expect_equal(nrow(coords), 6)
})

# =============================================================================
# Test: Round-Trip Conversion
# =============================================================================

test_that("igraph to cograph and back preserves structure", {
  skip_if_not_installed("igraph")

  # Create original igraph
  g_orig <- igraph::make_ring(5)
  igraph::V(g_orig)$name <- LETTERS[1:5]
  igraph::E(g_orig)$weight <- c(0.1, 0.2, 0.3, 0.4, 0.5)

  # Parse to cograph
  parsed <- parse_igraph(g_orig)

  # Verify structure preserved
  expect_equal(nrow(parsed$nodes), 5)
  expect_equal(nrow(parsed$edges), 5)
  expect_equal(parsed$nodes$label, LETTERS[1:5])
  expect_equal(parsed$weights, c(0.1, 0.2, 0.3, 0.4, 0.5))
})

# =============================================================================
# Test: Edge Cases and Special Graphs
# =============================================================================

test_that("parse_igraph handles complete graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_graph(5)
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 5)
  expect_equal(nrow(result$edges), 10)  # 5 choose 2 = 10 edges
})

test_that("parse_igraph handles star graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_star(5, mode = "undirected")
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 5)
  expect_equal(nrow(result$edges), 4)  # n-1 edges
})

test_that("parse_igraph handles single node graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_empty_graph(1)
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 1)
  expect_equal(nrow(result$edges), 0)
  expect_equal(result$nodes$label, "1")
})

test_that("parse_igraph handles graph with self-loops", {
  skip_if_not_installed("igraph")

  g <- igraph::make_empty_graph(3)
  g <- igraph::add_edges(g, c(1, 1, 1, 2, 2, 3))  # self-loop on 1
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 3)
})

test_that("network_to_igraph handles network without labels", {
  skip_if_not_installed("igraph")

  # Create network where nodes have no label column
  nodes_df <- data.frame(id = 1:3, x = NA_real_, y = NA_real_)
  # Explicitly add NULL-ish label behavior
  nodes_df$label <- NA_character_

  net <- list(
    nodes = nodes_df,
    edges = data.frame(from = c(1L, 2L), to = c(2L, 3L), weight = c(1, 1)),
    directed = FALSE
  )
  class(net) <- c("cograph_network", "list")

  # Should handle gracefully
  g <- network_to_igraph(net)
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 3)
})

# =============================================================================
# Test: Integration with CographNetwork (R6) Class
# =============================================================================

test_that("apply_igraph_layout works with CographNetwork R6 object", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- apply_igraph_layout(net, igraph::layout_in_circle)

  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
})

test_that("apply_igraph_layout_by_name works with CographNetwork R6 object", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- apply_igraph_layout_by_name(net, "kk", seed = 42)

  expect_equal(nrow(coords), 3)
})

# =============================================================================
# Test: Coordinate Normalization Edge Cases
# =============================================================================

test_that("normalize_coords handles very small range", {
  coords <- matrix(c(0.001, 0.002, 0.003, 0.001, 0.002, 0.003), ncol = 2)
  result <- normalize_coords(coords)

  expect_true(all(result >= 0.1))
  expect_true(all(result <= 0.9))
})

test_that("normalize_coords handles very large range", {
  coords <- matrix(c(-1e6, 0, 1e6, -1e6, 0, 1e6), ncol = 2)
  result <- normalize_coords(coords)

  expect_true(all(result >= 0.1 - 1e-10))
  expect_true(all(result <= 0.9 + 1e-10))
})

test_that("normalize_coords handles NA values gracefully", {
  # NA values should be handled by na.rm = TRUE in range()
  coords <- matrix(c(0, NA, 100, 0, 50, 100), ncol = 2)
  result <- normalize_coords(coords)

  # Non-NA values should be normalized
  expect_true(!is.na(result[1, 1]))
  expect_true(!is.na(result[3, 1]))
})

# =============================================================================
# Test: Error Messages Quality
# =============================================================================

test_that("error message for unknown layout lists available options", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- as_cograph(mat)

  err <- tryCatch(
    apply_igraph_layout_by_name(net, "nonexistent"),
    error = function(e) conditionMessage(e)
  )

  expect_true(grepl("Unknown igraph layout", err))
  expect_true(grepl("Available", err))
})

test_that("parse_igraph error message is clear for invalid input", {
  skip_if_not_installed("igraph")

  err <- tryCatch(
    parse_igraph(data.frame(a = 1:3)),
    error = function(e) conditionMessage(e)
  )

  expect_true(grepl("igraph object", err))
})
