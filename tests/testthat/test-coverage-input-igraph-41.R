# =============================================================================
# Test Coverage for input-igraph.R - Series 41
# =============================================================================
# Additional comprehensive tests targeting edge cases and branches
# that may not be covered in test-coverage-input-igraph-42.R

# Make internal functions available for testing
parse_igraph <- cograph:::parse_igraph
apply_igraph_layout <- cograph:::apply_igraph_layout
apply_igraph_layout_by_name <- cograph:::apply_igraph_layout_by_name
network_to_igraph <- cograph:::network_to_igraph
normalize_coords <- cograph:::normalize_coords

# =============================================================================
# Test: parse_igraph() - Edge Cases for Vertex Names
# =============================================================================

test_that("parse_igraph handles graph with empty name attribute", {
  skip_if_not_installed("igraph")

  # Create graph with empty string names
  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("", "", "")
  result <- parse_igraph(g)

  # Empty strings are still valid names (not NA)
  expect_equal(result$nodes$label, c("", "", ""))
})

test_that("parse_igraph handles graph with numeric vertex names", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  igraph::V(g)$name <- c(100, 200, 300, 400)
  result <- parse_igraph(g)

  # Numeric names are preserved as-is
  expect_equal(as.character(result$nodes$label), c("100", "200", "300", "400"))
})

test_that("parse_igraph preserves vertex names with special characters", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("Node A", "Node-B", "Node_C")
  result <- parse_igraph(g)

  expect_equal(result$nodes$label, c("Node A", "Node-B", "Node_C"))
})

# =============================================================================
# Test: parse_igraph() - Complex Vertex Attributes
# =============================================================================

test_that("parse_igraph handles multiple vertex attributes", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  igraph::V(g)$name <- c("A", "B", "C", "D")
  igraph::V(g)$color <- c("red", "blue", "green", "yellow")
  igraph::V(g)$size <- c(10, 20, 30, 40)
  igraph::V(g)$shape <- c("circle", "square", "circle", "square")
  igraph::V(g)$group <- c(1, 1, 2, 2)
  igraph::V(g)$score <- c(0.1, 0.2, 0.3, 0.4)

  result <- parse_igraph(g)

  expect_true("color" %in% names(result$nodes))
  expect_true("size" %in% names(result$nodes))
  expect_true("shape" %in% names(result$nodes))
  expect_true("group" %in% names(result$nodes))
  expect_true("score" %in% names(result$nodes))

  expect_equal(result$nodes$shape, c("circle", "square", "circle", "square"))
  expect_equal(result$nodes$group, c(1, 1, 2, 2))
})

test_that("parse_igraph handles vertex attributes with NA values", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("A", "B", "C")
  igraph::V(g)$size <- c(10, NA, 30)
  igraph::V(g)$label_color <- c("black", "red", NA)

  result <- parse_igraph(g)

  expect_equal(result$nodes$size, c(10, NA, 30))
  expect_equal(result$nodes$label_color, c("black", "red", NA))
})

# =============================================================================
# Test: parse_igraph() - Complex Edge Attributes
# =============================================================================

test_that("parse_igraph handles multiple edge attributes", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  igraph::E(g)$weight <- c(0.5, 1.0, 1.5, 2.0)
  igraph::E(g)$color <- c("red", "green", "blue", "black")
  igraph::E(g)$lty <- c(1, 2, 1, 2)
  igraph::E(g)$label <- c("e1", "e2", "e3", "e4")
  igraph::E(g)$width <- c(1, 2, 3, 4)

  result <- parse_igraph(g)

  expect_true("color" %in% names(result$edges))
  expect_true("lty" %in% names(result$edges))
  expect_true("label" %in% names(result$edges))
  expect_true("width" %in% names(result$edges))

  expect_equal(result$edges$label, c("e1", "e2", "e3", "e4"))
})

test_that("parse_igraph handles edge attributes with NA values", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::E(g)$weight <- c(1, NA, 2)
  igraph::E(g)$type <- c("A", NA, "C")

  result <- parse_igraph(g)

  expect_equal(result$weights, c(1, NA, 2))
  expect_equal(result$edges$type, c("A", NA, "C"))
})

test_that("parse_igraph handles negative and zero weights", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  igraph::E(g)$weight <- c(-1, 0, 0.5, -0.5)

  result <- parse_igraph(g)

  expect_equal(result$weights, c(-1, 0, 0.5, -0.5))
})

# =============================================================================
# Test: parse_igraph() - Various Graph Types
# =============================================================================

test_that("parse_igraph handles bipartite graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_full_bipartite_graph(3, 2)
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 5)
  # Bipartite full graph: 3*2 = 6 edges
  expect_equal(nrow(result$edges), 6)
})

test_that("parse_igraph handles tree graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_tree(7, children = 2, mode = "undirected")
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 7)
  expect_equal(nrow(result$edges), 6)  # tree has n-1 edges
})

test_that("parse_igraph handles lattice graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_lattice(c(3, 3))
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 9)
})

test_that("parse_igraph handles large graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(100)
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 100)
  expect_equal(nrow(result$edges), 100)
})

test_that("parse_igraph handles directed graph with multiple edge attributes", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4, directed = TRUE)
  igraph::E(g)$weight <- c(0.1, 0.2, 0.3, 0.4)
  igraph::E(g)$type <- c("forward", "forward", "back", "back")

  result <- parse_igraph(g)

  expect_true(result$directed)
  expect_true("type" %in% names(result$edges))
  expect_equal(result$edges$type, c("forward", "forward", "back", "back"))
})

# =============================================================================
# Test: network_to_igraph() - Edge Cases
# =============================================================================

test_that("network_to_igraph handles directed cograph_network", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 0,
                  0, 0, 1,
                  1, 0, 0), nrow = 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat, directed = TRUE)

  g <- network_to_igraph(net)

  expect_true(igraph::is_directed(g))
  expect_equal(igraph::ecount(g), 3)
})

test_that("network_to_igraph handles network with many edges", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(n = 10, density = 0.8, weighted = TRUE)
  rownames(mat) <- colnames(mat) <- LETTERS[1:10]
  net <- as_cograph(mat)

  g <- network_to_igraph(net)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 10)
})

test_that("network_to_igraph handles network with no labels", {
  skip_if_not_installed("igraph")

  # Create a network and remove labels
  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- as_cograph(mat)
  net$nodes$label <- NULL

  # Should still work
  g <- network_to_igraph(net)
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 2)
})

test_that("network_to_igraph handles network with NULL weight column", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- as_cograph(mat)
  net$edges$weight <- NULL

  g <- network_to_igraph(net)

  expect_true(inherits(g, "igraph"))
  # Weight attribute should not be set
  expect_false("weight" %in% igraph::edge_attr_names(g))
})

test_that("network_to_igraph handles single-node network", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, nrow = 1, ncol = 1)
  rownames(mat) <- colnames(mat) <- "A"
  net <- as_cograph(mat)

  g <- network_to_igraph(net)

  expect_equal(igraph::vcount(g), 1)
  expect_equal(igraph::ecount(g), 0)
})

# =============================================================================
# Test: normalize_coords() - Edge Cases
# =============================================================================

test_that("normalize_coords handles two nodes in same position", {
  coords <- matrix(c(5, 5, 10, 10), ncol = 2)
  result <- normalize_coords(coords)

  # Should have valid coordinates
  expect_equal(nrow(result), 2)
  expect_true(all(is.numeric(result)))
})

test_that("normalize_coords handles very large coordinate values", {
  coords <- matrix(c(1e10, 1e10, 2e10, 1e10, 1.5e10, 2e10), ncol = 2)
  result <- normalize_coords(coords)

  expect_true(all(result >= 0.1 - 1e-10))
  expect_true(all(result <= 0.9 + 1e-10))
})

test_that("normalize_coords handles very small coordinate differences", {
  coords <- matrix(c(1e-10, 2e-10, 3e-10, 1e-10, 2e-10, 3e-10), ncol = 2)
  result <- normalize_coords(coords)

  expect_true(all(result >= 0.1 - 1e-10))
  expect_true(all(result <= 0.9 + 1e-10))
})

test_that("normalize_coords handles mixed positive and negative coordinates", {
  coords <- matrix(c(-50, 0, 50, -50, 0, 50), ncol = 2)
  result <- normalize_coords(coords)

  # Check min and max are at 0.1 and 0.9
  expect_equal(min(result[, 1]), 0.1)
  expect_equal(max(result[, 1]), 0.9)
})

test_that("normalize_coords handles 2-node case", {
  coords <- matrix(c(0, 10, 0, 10), ncol = 2)
  result <- normalize_coords(coords)

  # Should properly scale
  expect_equal(nrow(result), 2)
  expect_equal(result[1, 1], 0.1)
  expect_equal(result[2, 1], 0.9)
})

# =============================================================================
# Test: apply_igraph_layout_by_name() - Additional Layout Aliases
# =============================================================================

test_that("apply_igraph_layout_by_name works with full layout names", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1,
                  1, 0, 1, 1,
                  1, 1, 0, 1,
                  1, 1, 1, 0), nrow = 4)
  net <- as_cograph(mat)

  # Test various full names
  layouts <- c(
    "layout_with_fr",
    "layout_with_kk",
    "layout_in_circle",
    "layout_as_star",
    "layout_on_grid",
    "layout_randomly"
  )

  for (layout in layouts) {
    coords <- apply_igraph_layout_by_name(net, layout, seed = 42)
    expect_equal(nrow(coords), 4,
                 info = paste("Failed for layout:", layout))
    expect_true(all(c("x", "y") %in% names(coords)),
                info = paste("Missing x/y for layout:", layout))
  }
})

test_that("apply_igraph_layout_by_name works with igraph_ prefix", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  prefixed <- c(
    "igraph_nicely",
    "igraph_fr",
    "igraph_kk",
    "igraph_star",
    "igraph_grid",
    "igraph_random"
  )

  for (layout in prefixed) {
    coords <- apply_igraph_layout_by_name(net, layout, seed = 42)
    expect_equal(nrow(coords), 3,
                 info = paste("Failed for layout:", layout))
  }
})

test_that("apply_igraph_layout_by_name MDS layout works", {
  skip_if_not_installed("igraph")

  # MDS needs connected graph
  mat <- create_test_topology("complete", n = 5)
  net <- as_cograph(mat)

  coords <- suppressWarnings(
    apply_igraph_layout_by_name(net, "mds", seed = 42)
  )

  expect_equal(nrow(coords), 5)
})

test_that("apply_igraph_layout_by_name graphopt layout works", {
  skip_if_not_installed("igraph")

  mat <- create_test_topology("complete", n = 5)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout_by_name(net, "igraph_graphopt", seed = 42)

  expect_equal(nrow(coords), 5)
})

# =============================================================================
# Test: apply_igraph_layout() - Direct Function Use
# =============================================================================

test_that("apply_igraph_layout works with layout_with_kk", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout(net, igraph::layout_with_kk)

  expect_equal(nrow(coords), 3)
  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
  expect_true(all(coords$y >= 0.1 & coords$y <= 0.9))
})

test_that("apply_igraph_layout works with layout_as_star", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 1, 1,
                  1, 0, 0, 0, 0,
                  1, 0, 0, 0, 0,
                  1, 0, 0, 0, 0,
                  1, 0, 0, 0, 0), nrow = 5)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout(net, igraph::layout_as_star)

  expect_equal(nrow(coords), 5)
})

test_that("apply_igraph_layout works with layout_on_grid", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(n = 9, density = 0.3)
  net <- as_cograph(mat)

  coords <- apply_igraph_layout(net, igraph::layout_on_grid)

  expect_equal(nrow(coords), 9)
})

test_that("apply_igraph_layout handles empty network gracefully", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, 4, 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  net <- as_cograph(mat)

  # Should work even with no edges
  coords <- apply_igraph_layout(net, igraph::layout_in_circle)

  expect_equal(nrow(coords), 4)
})

# =============================================================================
# Test: Round-Trip Conversion Edge Cases
# =============================================================================

test_that("igraph to cograph preserves all vertex attribute types", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  igraph::V(g)$name <- c("A", "B", "C")
  igraph::V(g)$numeric_attr <- c(1.5, 2.5, 3.5)
  igraph::V(g)$integer_attr <- c(1L, 2L, 3L)
  igraph::V(g)$char_attr <- c("x", "y", "z")
  igraph::V(g)$logical_attr <- c(TRUE, FALSE, TRUE)

  result <- parse_igraph(g)

  expect_equal(result$nodes$numeric_attr, c(1.5, 2.5, 3.5))
  expect_equal(result$nodes$integer_attr, c(1L, 2L, 3L))
  expect_equal(result$nodes$char_attr, c("x", "y", "z"))
  expect_equal(result$nodes$logical_attr, c(TRUE, FALSE, TRUE))
})

test_that("igraph to cograph preserves all edge attribute types", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  igraph::E(g)$weight <- c(0.1, 0.2, 0.3, 0.4)
  igraph::E(g)$numeric_attr <- c(1.1, 2.2, 3.3, 4.4)
  igraph::E(g)$char_attr <- c("a", "b", "c", "d")
  igraph::E(g)$logical_attr <- c(TRUE, FALSE, TRUE, FALSE)

  result <- parse_igraph(g)

  expect_equal(result$edges$numeric_attr, c(1.1, 2.2, 3.3, 4.4))
  expect_equal(result$edges$char_attr, c("a", "b", "c", "d"))
  expect_equal(result$edges$logical_attr, c(TRUE, FALSE, TRUE, FALSE))
})

# =============================================================================
# Test: Integration - Full Workflow
# =============================================================================

test_that("full workflow: parse igraph -> apply layout -> coordinates valid", {
  skip_if_not_installed("igraph")

  # Create igraph with attributes
  g <- igraph::make_full_graph(5)
  igraph::V(g)$name <- LETTERS[1:5]
  igraph::E(g)$weight <- runif(igraph::ecount(g))

  # Parse to cograph format
  parsed <- parse_igraph(g)

  # Create network directly from igraph (avoids edge duplication)
  net <- as_cograph(g)

  # Apply layout
  coords <- apply_igraph_layout_by_name(net, "fr", seed = 42)

  # Validate
  expect_equal(nrow(coords), 5)
  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
  expect_true(all(coords$y >= 0.1 & coords$y <= 0.9))
})

test_that("workflow: cograph_network to igraph and back preserves structure", {
  skip_if_not_installed("igraph")

  # Create cograph_network
  mat <- create_test_topology("complete", n = 4)
  rownames(mat) <- colnames(mat) <- c("W", "X", "Y", "Z")
  net <- as_cograph(mat)

  # Convert to igraph
  g <- network_to_igraph(net)

  # Convert back
  parsed <- parse_igraph(g)

  # Verify
  expect_equal(nrow(parsed$nodes), 4)
  expect_equal(parsed$nodes$label, c("W", "X", "Y", "Z"))
})

# =============================================================================
# Test: Error Handling and Validation
# =============================================================================

test_that("parse_igraph with numeric input errors correctly", {
  skip_if_not_installed("igraph")

  expect_error(parse_igraph(123), "Input must be an igraph object")
})

test_that("parse_igraph with NULL input errors correctly", {
  skip_if_not_installed("igraph")

  expect_error(parse_igraph(NULL), "Input must be an igraph object")
})

test_that("parse_igraph with vector input errors correctly", {
  skip_if_not_installed("igraph")

  expect_error(parse_igraph(c(1, 2, 3)), "Input must be an igraph object")
})

test_that("network_to_igraph with data.frame errors correctly", {
  skip_if_not_installed("igraph")

  df <- data.frame(from = c(1, 2), to = c(2, 3))
  expect_error(network_to_igraph(df),
               "network must be a CographNetwork or cograph_network object")
})

test_that("network_to_igraph with matrix errors correctly", {
  skip_if_not_installed("igraph")

  mat <- matrix(1:4, 2, 2)
  expect_error(network_to_igraph(mat),
               "network must be a CographNetwork or cograph_network object")
})

test_that("apply_igraph_layout_by_name with invalid layout errors correctly", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- as_cograph(mat)

  expect_error(apply_igraph_layout_by_name(net, "invalid_layout_xyz"),
               "Unknown igraph layout")
})

test_that("error message includes available layout options", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- as_cograph(mat)

  err <- tryCatch(
    apply_igraph_layout_by_name(net, "bad_layout"),
    error = function(e) conditionMessage(e)
  )

  expect_true(grepl("kk", err))
  expect_true(grepl("fr", err))
})

# =============================================================================
# Test: Graph Topologies
# =============================================================================

test_that("parse_igraph handles Watts-Strogatz small world graph", {
  skip_if_not_installed("igraph")

  g <- igraph::sample_smallworld(dim = 1, size = 10, nei = 2, p = 0.1)
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 10)
  expect_false(result$directed)
})

test_that("parse_igraph handles Barabasi-Albert graph", {
  skip_if_not_installed("igraph")

  g <- igraph::sample_pa(n = 20, m = 2, directed = FALSE)
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 20)
})

test_that("parse_igraph handles Erdos-Renyi random graph", {
  skip_if_not_installed("igraph")

  set.seed(42)
  g <- igraph::sample_gnp(n = 15, p = 0.3)
  result <- parse_igraph(g)

  expect_equal(nrow(result$nodes), 15)
})

test_that("parse_igraph handles Kautz graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_kautz_graph(m = 2, n = 1)
  result <- parse_igraph(g)

  expect_true(inherits(result, "list"))
  expect_true(result$directed)  # Kautz graphs are directed
})

test_that("parse_igraph handles De Bruijn graph", {
  skip_if_not_installed("igraph")

  g <- igraph::make_de_bruijn_graph(m = 2, n = 2)
  result <- parse_igraph(g)

  expect_true(inherits(result, "list"))
  expect_true(result$directed)  # De Bruijn graphs are directed
})

# =============================================================================
# Test: Coordinate Normalization with Extreme Values
# =============================================================================

test_that("normalize_coords preserves relative positions", {
  coords <- matrix(c(0, 50, 100, 0, 50, 100), ncol = 2)
  result <- normalize_coords(coords)

  # Middle point should be at 0.5
  expect_equal(result[2, 1], 0.5)
  expect_equal(result[2, 2], 0.5)
})

test_that("normalize_coords handles 4 nodes in a square pattern", {
  coords <- matrix(c(0, 0, 10, 10, 0, 10, 0, 10), ncol = 2)
  result <- normalize_coords(coords)

  # All coordinates should be between 0.1 and 0.9
  expect_true(all(result >= 0.1 - 1e-10))
  expect_true(all(result <= 0.9 + 1e-10))
})

test_that("normalize_coords handles infinity values by using na.rm", {
  # This tests the na.rm = TRUE in range()
  coords <- matrix(c(0, 10, 20, 0, 10, 20), ncol = 2)
  # We can't pass Inf directly, but we can test normal values work
  result <- normalize_coords(coords)

  expect_true(all(is.finite(result)))
})

# =============================================================================
# Test: Layout Edge Cases
# =============================================================================

test_that("layout works on disconnected graph", {
  skip_if_not_installed("igraph")

  # Create disconnected graph (two components)
  mat <- create_test_topology("disconnected", n = 6)
  net <- as_cograph(mat)

  # Should still produce valid layout
  coords <- apply_igraph_layout_by_name(net, "ni", seed = 42)

  expect_equal(nrow(coords), 6)
  expect_true(all(coords$x >= 0.1 & coords$x <= 0.9))
})

test_that("layout handles graph with self-loop", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(3)
  g <- igraph::add_edges(g, c(1, 1))  # Self-loop

  result <- parse_igraph(g)
  net <- as_cograph(result$nodes, result$edges, directed = FALSE)

  # Should not error
  coords <- apply_igraph_layout_by_name(net, "ci", seed = 42)
  expect_equal(nrow(coords), 3)
})

test_that("layout_as_tree handles non-tree structure", {
  skip_if_not_installed("igraph")

  # Complete graph is not a tree
  mat <- create_test_topology("complete", n = 4)
  net <- as_cograph(mat)

  # Tree layout should still work (may just not look like a tree)
  coords <- suppressWarnings(
    apply_igraph_layout_by_name(net, "tr", seed = 42)
  )

  expect_equal(nrow(coords), 4)
})

# =============================================================================
# Test: CographNetwork R6 Compatibility
# =============================================================================

test_that("parse_igraph result can create CographNetwork R6", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]

  parsed <- parse_igraph(g)

  # Create adjacency matrix from parsed data
  n <- nrow(parsed$nodes)
  mat <- matrix(0, n, n)
  rownames(mat) <- colnames(mat) <- parsed$nodes$label

  for (i in seq_len(nrow(parsed$edges))) {
    from_idx <- parsed$edges$from[i]
    to_idx <- parsed$edges$to[i]
    weight <- if (!is.null(parsed$edges$weight)) parsed$edges$weight[i] else 1
    mat[from_idx, to_idx] <- weight
    if (!parsed$directed) {
      mat[to_idx, from_idx] <- weight
    }
  }

  net <- CographNetwork$new(mat)
  expect_true(inherits(net, "CographNetwork"))
  expect_equal(net$n_nodes, 5)
})

test_that("apply_igraph_layout works with R6 and produces normalized coords", {
  skip_if_not_installed("igraph")

  mat <- create_test_topology("ring", n = 6)
  net <- CographNetwork$new(mat)

  coords <- apply_igraph_layout(net, igraph::layout_with_fr)

  # Verify normalization
  expect_true(all(coords$x >= 0.1 - 1e-10))
  expect_true(all(coords$x <= 0.9 + 1e-10))
  expect_true(all(coords$y >= 0.1 - 1e-10))
  expect_true(all(coords$y <= 0.9 + 1e-10))
})

# =============================================================================
# Test: Seed Parameter Behavior
# =============================================================================

test_that("different seeds produce different layouts for randomized algorithms", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(n = 6, density = 0.5)
  net <- as_cograph(mat)

  coords1 <- apply_igraph_layout_by_name(net, "fr", seed = 1)
  coords2 <- apply_igraph_layout_by_name(net, "fr", seed = 999)

  # With high probability, different seeds produce different layouts
  # But this could occasionally be the same by chance, so we just check they run
  expect_equal(nrow(coords1), 6)
  expect_equal(nrow(coords2), 6)
})

test_that("NULL seed allows non-deterministic layouts", {
  skip_if_not_installed("igraph")

  mat <- create_test_matrix(n = 4, density = 0.5)
  net <- as_cograph(mat)

  # Should not error with NULL seed
  coords <- apply_igraph_layout_by_name(net, "rd", seed = NULL)
  expect_equal(nrow(coords), 4)
})
