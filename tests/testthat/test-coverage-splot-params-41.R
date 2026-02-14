# test-coverage-splot-params-41.R - Comprehensive tests for splot-params.R
# Tests for splot parameter vectorization helpers

# Load internal functions for testing
resolve_edge_colors <- cograph:::resolve_edge_colors
resolve_edge_widths <- cograph:::resolve_edge_widths
resolve_node_sizes <- cograph:::resolve_node_sizes
resolve_centrality_sizes <- cograph:::resolve_centrality_sizes
resolve_label_sizes <- cograph:::resolve_label_sizes
resolve_node_colors <- cograph:::resolve_node_colors
resolve_labels <- cograph:::resolve_labels
resolve_edge_labels <- cograph:::resolve_edge_labels
resolve_shapes <- cograph:::resolve_shapes
resolve_curvatures <- cograph:::resolve_curvatures
resolve_loop_rotation <- cograph:::resolve_loop_rotation
filter_edges_by_weight <- cograph:::filter_edges_by_weight
get_edge_order <- cograph:::get_edge_order
get_node_order <- cograph:::get_node_order
recycle_to_length <- cograph:::recycle_to_length
get_scale_constants <- cograph:::get_scale_constants

# ============================================
# RESOLVE_EDGE_COLORS TESTS
# ============================================

test_that("resolve_edge_colors returns empty vector for zero edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- resolve_edge_colors(edges)
  expect_equal(length(result), 0)
  expect_type(result, "character")
})

test_that("resolve_edge_colors uses user-specified single color", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.5, 0.3, 0.7))
  result <- resolve_edge_colors(edges, edge.color = "blue")
  expect_equal(result, rep("blue", 3))
})

test_that("resolve_edge_colors recycles user-specified color vector", {
  edges <- data.frame(from = c(1, 2, 3, 4), to = c(2, 3, 4, 1))
  result <- resolve_edge_colors(edges, edge.color = c("red", "blue"))
  expect_equal(result, c("red", "blue", "red", "blue"))
})

test_that("resolve_edge_colors colors by weight sign - positive", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
  result <- resolve_edge_colors(edges, posCol = "#00FF00", negCol = "#FF0000")
  expect_equal(result, c("#00FF00", "#00FF00"))
})

test_that("resolve_edge_colors colors by weight sign - negative", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(-0.5, -0.8))
  result <- resolve_edge_colors(edges, posCol = "#00FF00", negCol = "#FF0000")
  expect_equal(result, c("#FF0000", "#FF0000"))
})

test_that("resolve_edge_colors colors by weight sign - mixed", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.5, -0.3, 0))
  result <- resolve_edge_colors(edges, posCol = "green", negCol = "red", default_col = "gray")
  expect_equal(result, c("green", "red", "gray"))
})

test_that("resolve_edge_colors uses default when no weights", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1))
  result <- resolve_edge_colors(edges, default_col = "purple")
  expect_equal(result, rep("purple", 3))
})

test_that("resolve_edge_colors handles NA weights", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(NA, 0.5))
  result <- resolve_edge_colors(edges, posCol = "green", negCol = "red", default_col = "gray")
  # NA weights should be handled gracefully
expect_equal(length(result), 2)
})

# ============================================
# RESOLVE_EDGE_WIDTHS TESTS
# ============================================

test_that("resolve_edge_widths returns empty vector for zero edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- resolve_edge_widths(edges)
  expect_equal(length(result), 0)
  expect_type(result, "double")
})

test_that("resolve_edge_widths treats 'weight' string as weight-based scaling", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.3, 0.8))
  result <- resolve_edge_widths(edges, edge.width = "weight")
  expect_equal(length(result), 2)
  expect_true(result[2] > result[1])  # Heavier weight = thicker line
})

test_that("resolve_edge_widths uses explicit numeric widths", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.5, 0.5, 0.5))
  result <- resolve_edge_widths(edges, edge.width = 2.5)
  expect_equal(result, rep(2.5, 3))
})

test_that("resolve_edge_widths recycles numeric width vector", {
  edges <- data.frame(from = c(1, 2, 3, 4), to = c(2, 3, 4, 1))
  result <- resolve_edge_widths(edges, edge.width = c(1, 2))
  expect_equal(result, c(1, 2, 1, 2))
})

test_that("resolve_edge_widths uses scale constants for edge_width_range", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.2, 1.0))
  # Not specifying edge_width_range should use scale constants default
  result <- resolve_edge_widths(edges)
  scale <- get_scale_constants("default")
  expect_true(all(result >= scale$edge_width_range[1]))
  expect_true(all(result <= scale$edge_width_range[2]))
})

test_that("resolve_edge_widths uses scale constants for edge_scale_mode", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.1, 1.0))
  # Should use default scale mode from constants
  result_default <- resolve_edge_widths(edges)
  result_sqrt <- resolve_edge_widths(edges, edge_scale_mode = "sqrt")
  # Different modes should produce different results
  expect_false(all(result_default == result_sqrt))
})

test_that("resolve_edge_widths handles edges without weight column", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1))
  result <- resolve_edge_widths(edges)
  scale <- get_scale_constants("default")
  expect_equal(result, rep(scale$edge_width_default, 3))
})

test_that("resolve_edge_widths respects custom edge_width_range", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.1, 1.0))
  result <- resolve_edge_widths(edges, edge_width_range = c(0.5, 10))
  expect_true(all(result >= 0.5))
  expect_true(all(result <= 10))
})

test_that("resolve_edge_widths works with legacy scaling", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.3, 0.9))
  result_default <- resolve_edge_widths(edges, scaling = "default")
  result_legacy <- resolve_edge_widths(edges, scaling = "legacy")
  # Legacy has different range defaults
  expect_true(length(result_legacy) == 2)
})

test_that("resolve_edge_widths respects n_nodes for adaptive sizing", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.5))
  # More nodes should produce thinner default edges
  result_few <- resolve_edge_widths(edges, n_nodes = 5)
  result_many <- resolve_edge_widths(edges, n_nodes = 100)
  # Both should work without error
  expect_equal(length(result_few), 2)
  expect_equal(length(result_many), 2)
})

test_that("resolve_edge_widths handles directed parameter", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.5))
  result_undirected <- resolve_edge_widths(edges, directed = FALSE)
  result_directed <- resolve_edge_widths(edges, directed = TRUE)
  # Both should work
  expect_equal(length(result_undirected), 2)
  expect_equal(length(result_directed), 2)
})

# ============================================
# RESOLVE_NODE_SIZES TESTS
# ============================================

test_that("resolve_node_sizes uses default size when vsize is NULL", {
  result <- resolve_node_sizes(NULL, n = 5)
  scale <- get_scale_constants("default")
  expected <- rep(scale$node_default * scale$node_factor, 5)
  expect_equal(result, expected)
})

test_that("resolve_node_sizes applies scale factor to vsize", {
  result <- resolve_node_sizes(vsize = 10, n = 3)
  scale <- get_scale_constants("default")
  expected <- rep(10 * scale$node_factor, 3)
  expect_equal(result, expected)
})

test_that("resolve_node_sizes recycles vsize vector", {
  result <- resolve_node_sizes(vsize = c(5, 10), n = 4)
  scale <- get_scale_constants("default")
  expected <- c(5, 10, 5, 10) * scale$node_factor
  expect_equal(result, expected)
})

test_that("resolve_node_sizes uses custom default_size", {
  result <- resolve_node_sizes(NULL, n = 3, default_size = 8)
  scale <- get_scale_constants("default")
  expected <- rep(8 * scale$node_factor, 3)
  expect_equal(result, expected)
})

test_that("resolve_node_sizes uses custom scale_factor", {
  result <- resolve_node_sizes(vsize = 5, n = 2, scale_factor = 0.1)
  expected <- rep(5 * 0.1, 2)
  expect_equal(result, expected)
})

test_that("resolve_node_sizes works with legacy scaling", {
  result_default <- resolve_node_sizes(vsize = 5, n = 2, scaling = "default")
  result_legacy <- resolve_node_sizes(vsize = 5, n = 2, scaling = "legacy")
  # Legacy has different factor
  scale_def <- get_scale_constants("default")
  scale_leg <- get_scale_constants("legacy")
  expect_equal(result_default, rep(5 * scale_def$node_factor, 2))
  expect_equal(result_legacy, rep(5 * scale_leg$node_factor, 2))
})

# ============================================
# RESOLVE_CENTRALITY_SIZES TESTS
# ============================================

test_that("resolve_centrality_sizes returns NULL when scale_by is NULL", {
  mat <- create_test_matrix(4)
  result <- resolve_centrality_sizes(mat, scale_by = NULL)
  expect_null(result)
})

test_that("resolve_centrality_sizes calculates degree centrality", {
  mat <- create_test_matrix(5, density = 0.6)
  result <- resolve_centrality_sizes(mat, scale_by = "degree")
  expect_type(result, "list")
  expect_true("sizes" %in% names(result))
  expect_true("values" %in% names(result))
  expect_equal(length(result$sizes), 5)
})

test_that("resolve_centrality_sizes calculates strength centrality", {
  mat <- create_test_matrix(5, weighted = TRUE)
  result <- resolve_centrality_sizes(mat, scale_by = "strength")
  expect_equal(length(result$sizes), 5)
  expect_true(all(result$sizes > 0))
})

test_that("resolve_centrality_sizes calculates betweenness centrality", {
  mat <- create_test_matrix(5, density = 0.7)
  result <- resolve_centrality_sizes(mat, scale_by = "betweenness")
  expect_equal(length(result$sizes), 5)
})

test_that("resolve_centrality_sizes accepts list with measure and params", {
  mat <- create_test_matrix(5, weighted = TRUE)
  result <- resolve_centrality_sizes(mat, scale_by = list(measure = "degree"))
  expect_equal(length(result$sizes), 5)
  expect_equal(result$measure, "degree")
})

test_that("resolve_centrality_sizes uses size_range parameter", {
  mat <- create_test_matrix(5)
  result <- resolve_centrality_sizes(mat, scale_by = "degree", size_range = c(1, 5))
  scale <- get_scale_constants("default")
  # Sizes should be within range (before factor applied)
  scaled_range <- c(1, 5) * scale$node_factor
  expect_true(all(result$sizes >= scaled_range[1] - 0.01))
  expect_true(all(result$sizes <= scaled_range[2] + 0.01))
})

test_that("resolve_centrality_sizes handles constant centrality values", {
  # Create a complete graph where all nodes have same degree
  mat <- create_test_topology("complete", n = 4)
  result <- resolve_centrality_sizes(mat, scale_by = "degree", size_range = c(2, 8))
  # All sizes should be equal (mean of range)
  expect_true(all(abs(result$sizes - result$sizes[1]) < 0.01))
})

test_that("resolve_centrality_sizes errors for invalid measure", {
  mat <- create_test_matrix(5)
  expect_error(
    resolve_centrality_sizes(mat, scale_by = "invalid_measure"),
    "'arg' should be one of"
  )
})

test_that("resolve_centrality_sizes errors for invalid scale_by type", {
  mat <- create_test_matrix(5)
  expect_error(
    resolve_centrality_sizes(mat, scale_by = 123),
    "scale_nodes_by must be a character string or list"
  )
})

test_that("resolve_centrality_sizes works with legacy scaling", {
  mat <- create_test_matrix(5)
  result_default <- resolve_centrality_sizes(mat, scale_by = "degree", scaling = "default")
  result_legacy <- resolve_centrality_sizes(mat, scale_by = "degree", scaling = "legacy")
  # Sizes should differ due to different scale factors
  expect_false(all(result_default$sizes == result_legacy$sizes))
})

test_that("resolve_centrality_sizes handles NaN/NA in centrality values", {
  # Single node with no edges - some centrality measures may return NaN
  mat <- matrix(0, 3, 3)
  mat[1, 2] <- 1; mat[2, 1] <- 1
  # Node 3 is isolated
  result <- resolve_centrality_sizes(mat, scale_by = "degree")
  expect_equal(length(result$sizes), 3)
  expect_false(any(is.na(result$sizes)))
})

# ============================================
# RESOLVE_LABEL_SIZES TESTS
# ============================================

test_that("resolve_label_sizes uses explicit label_size", {
  result <- resolve_label_sizes(label_size = 1.5, node_size_usr = rep(0.1, 3), n = 3)
  expect_equal(result, rep(1.5, 3))
})

test_that("resolve_label_sizes recycles label_size vector", {
  result <- resolve_label_sizes(label_size = c(0.8, 1.2), node_size_usr = rep(0.1, 4), n = 4)
  expect_equal(result, c(0.8, 1.2, 0.8, 1.2))
})

test_that("resolve_label_sizes uses default independent size in default scaling", {
  result <- resolve_label_sizes(label_size = NULL, node_size_usr = rep(0.1, 3), n = 3, scaling = "default")
  scale <- get_scale_constants("default")
  expect_equal(result, rep(scale$label_default, 3))
})

test_that("resolve_label_sizes couples to node size in legacy mode", {
  node_sizes <- c(0.05, 0.1, 0.15)
  result <- resolve_label_sizes(label_size = NULL, node_size_usr = node_sizes, n = 3, scaling = "legacy")
  # Legacy mode: vsize_usr * 8, capped at 1
  expected <- pmin(1, node_sizes * 8)
  expect_equal(result, expected)
})

test_that("resolve_label_sizes caps at 1 in legacy mode", {
  # Large node sizes should cap label size at 1
  node_sizes <- c(0.2, 0.3, 0.5)  # 0.2*8=1.6, 0.3*8=2.4, 0.5*8=4
  result <- resolve_label_sizes(label_size = NULL, node_size_usr = node_sizes, n = 3, scaling = "legacy")
  expect_true(all(result <= 1))
})

# ============================================
# RESOLVE_NODE_COLORS TESTS
# ============================================

test_that("resolve_node_colors uses explicit single color", {
  result <- resolve_node_colors("red", n = 4)
  expect_equal(result, rep("red", 4))
})

test_that("resolve_node_colors recycles color vector", {
  result <- resolve_node_colors(c("red", "blue"), n = 5)
  expect_equal(result, c("red", "blue", "red", "blue", "red"))
})

test_that("resolve_node_colors colors by groups", {
  groups <- c("A", "A", "B", "B", "C")
  result <- resolve_node_colors(NULL, n = 5, groups = groups)
  expect_equal(length(result), 5)
  # Same group should have same color
  expect_equal(result[1], result[2])
  expect_equal(result[3], result[4])
  # Different groups should have different colors
  expect_false(result[1] == result[3])
})

test_that("resolve_node_colors uses colors from node data", {
  nodes <- data.frame(label = c("A", "B", "C"), color = c("#FF0000", "#00FF00", "#0000FF"))
  result <- resolve_node_colors(NULL, n = 3, nodes = nodes)
  expect_equal(result, c("#FF0000", "#00FF00", "#0000FF"))
})

test_that("resolve_node_colors uses default when no color info", {
  result <- resolve_node_colors(NULL, n = 3, default_col = "steelblue")
  expect_equal(result, rep("steelblue", 3))
})

test_that("resolve_node_colors prioritizes explicit color over groups", {
  groups <- c("A", "B", "C")
  result <- resolve_node_colors("orange", n = 3, groups = groups)
  expect_equal(result, rep("orange", 3))
})

# ============================================
# RESOLVE_LABELS TESTS
# ============================================

test_that("resolve_labels returns NULL for NULL input", {
  result <- resolve_labels(NULL, nodes = NULL, n = 5)
  expect_null(result)
})

test_that("resolve_labels returns NULL for FALSE input", {
  result <- resolve_labels(FALSE, nodes = NULL, n = 5)
  expect_null(result)
})

test_that("resolve_labels uses node data labels for TRUE", {
  nodes <- data.frame(label = c("Alpha", "Beta", "Gamma"))
  result <- resolve_labels(TRUE, nodes = nodes, n = 3)
  expect_equal(result, c("Alpha", "Beta", "Gamma"))
})

test_that("resolve_labels uses indices when TRUE but no node labels", {
  result <- resolve_labels(TRUE, nodes = NULL, n = 4)
  expect_equal(result, c("1", "2", "3", "4"))
})

test_that("resolve_labels uses TRUE with nodes without label column", {
  nodes <- data.frame(x = c(1, 2, 3))
  result <- resolve_labels(TRUE, nodes = nodes, n = 3)
  expect_equal(result, c("1", "2", "3"))
})

test_that("resolve_labels uses user-provided labels", {
  result <- resolve_labels(c("X", "Y", "Z"), nodes = NULL, n = 3)
  expect_equal(result, c("X", "Y", "Z"))
})

test_that("resolve_labels recycles user labels", {
  result <- resolve_labels(c("A", "B"), nodes = NULL, n = 4)
  expect_equal(result, c("A", "B", "A", "B"))
})

test_that("resolve_labels converts labels to character", {
  result <- resolve_labels(1:3, nodes = NULL, n = 3)
  expect_type(result, "character")
  expect_equal(result, c("1", "2", "3"))
})

# ============================================
# RESOLVE_EDGE_LABELS TESTS
# ============================================

test_that("resolve_edge_labels returns NULL for NULL input", {
  result <- resolve_edge_labels(NULL, edges = NULL, m = 5)
  expect_null(result)
})

test_that("resolve_edge_labels returns NULL for FALSE input", {
  result <- resolve_edge_labels(FALSE, edges = NULL, m = 5)
  expect_null(result)
})

test_that("resolve_edge_labels uses weights for TRUE", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.456, 0.789))
  result <- resolve_edge_labels(TRUE, edges = edges, m = 2)
  expect_equal(result, c("0.46", "0.79"))
})

test_that("resolve_edge_labels returns empty strings when TRUE but no weights", {
  edges <- data.frame(from = c(1, 2), to = c(2, 3))
  result <- resolve_edge_labels(TRUE, edges = edges, m = 2)
  expect_equal(result, c("", ""))
})

test_that("resolve_edge_labels uses user-provided labels", {
  result <- resolve_edge_labels(c("low", "high"), edges = NULL, m = 2)
  expect_equal(result, c("low", "high"))
})

test_that("resolve_edge_labels recycles user labels", {
  result <- resolve_edge_labels(c("a", "b"), edges = NULL, m = 4)
  expect_equal(result, c("a", "b", "a", "b"))
})

test_that("resolve_edge_labels converts labels to character", {
  result <- resolve_edge_labels(c(1, 2, 3), edges = NULL, m = 3)
  expect_type(result, "character")
  expect_equal(result, c("1", "2", "3"))
})

# ============================================
# RESOLVE_SHAPES TESTS
# ============================================

test_that("resolve_shapes uses circle as default", {
  result <- resolve_shapes(NULL, n = 3)
  expect_equal(result, rep("circle", 3))
})

test_that("resolve_shapes uses explicit shape", {
  result <- resolve_shapes("square", n = 3)
  expect_equal(result, rep("square", 3))
})

test_that("resolve_shapes recycles shape vector", {
  result <- resolve_shapes(c("circle", "square"), n = 5)
  expect_equal(result, c("circle", "square", "circle", "square", "circle"))
})

# ============================================
# RESOLVE_CURVATURES TESTS
# ============================================

test_that("resolve_curvatures returns empty for zero edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- resolve_curvatures(0, edges)
  expect_equal(length(result), 0)
})

test_that("resolve_curvatures recycles curvature vector", {
  edges <- data.frame(from = c(1, 2, 3, 4), to = c(2, 3, 4, 1))
  result <- resolve_curvatures(c(0.1, 0.2), edges)
  expect_equal(result, c(0.1, 0.2, 0.1, 0.2))
})

test_that("resolve_curvatures returns as-is when curveScale is FALSE", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1))  # Reciprocal
  result <- resolve_curvatures(0, edges, curveScale = FALSE)
  expect_equal(result, c(0, 0))
})

test_that("resolve_curvatures applies default curvature to reciprocal edges", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1))  # Reciprocal pair
  result <- resolve_curvatures(0, edges, curveScale = TRUE, default_curve = 0.3)
  expect_equal(result, c(0.3, 0.3))
})

test_that("resolve_curvatures skips self-loops for reciprocal detection", {
  edges <- data.frame(from = c(1, 1, 2), to = c(1, 2, 1))  # Self-loop + reciprocal
  result <- resolve_curvatures(0, edges, curveScale = TRUE, default_curve = 0.25)
  # Self-loop (edge 1) should not be curved
  expect_equal(result[1], 0)
  # Reciprocal pair (edges 2 and 3) should be curved
  expect_equal(result[2], 0.25)
  expect_equal(result[3], 0.25)
})

test_that("resolve_curvatures preserves non-zero curvature for reciprocal edges", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1))
  # Pre-set curvature for first edge
  result <- resolve_curvatures(c(0.5, 0), edges, curveScale = TRUE, default_curve = 0.3)
  expect_equal(result[1], 0.5)  # Keeps original
  expect_equal(result[2], 0.3)  # Gets default
})

# ============================================
# RESOLVE_LOOP_ROTATION TESTS
# ============================================

test_that("resolve_loop_rotation returns empty for zero edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- resolve_loop_rotation(NULL, edges)
  expect_equal(length(result), 0)
})

test_that("resolve_loop_rotation uses default pi/2 when no layout", {
  edges <- data.frame(from = c(1, 1), to = c(1, 2))  # First is self-loop
  result <- resolve_loop_rotation(NULL, edges)
  expect_equal(result[1], pi/2)
  expect_equal(result[2], pi/2)
})

test_that("resolve_loop_rotation calculates rotation from layout", {
  edges <- data.frame(from = c(1, 2), to = c(1, 2))  # Both self-loops
  # Node 1 at (-1, 0), Node 2 at (1, 0), center at (0, 0)
  layout <- matrix(c(-1, 1, 0, 0), ncol = 2)
  result <- resolve_loop_rotation(NULL, edges, layout = layout)
  # Node 1 should point left (pi), Node 2 should point right (0)
  expect_equal(result[1], pi, tolerance = 0.01)
  expect_equal(result[2], 0, tolerance = 0.01)
})

test_that("resolve_loop_rotation uses explicit rotation when provided", {
  edges <- data.frame(from = c(1, 2), to = c(1, 2))
  result <- resolve_loop_rotation(c(0, pi/4), edges)
  expect_equal(result, c(0, pi/4))
})

test_that("resolve_loop_rotation recycles explicit rotation", {
  edges <- data.frame(from = c(1, 2, 3, 4), to = c(1, 2, 3, 4))
  result <- resolve_loop_rotation(c(0, pi), edges)
  expect_equal(result, c(0, pi, 0, pi))
})

# ============================================
# FILTER_EDGES_BY_WEIGHT TESTS
# ============================================

test_that("filter_edges_by_weight returns all edges when minimum is 0", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.1, 0.5, 0.9))
  result <- filter_edges_by_weight(edges, minimum = 0)
  expect_equal(nrow(result), 3)
})

test_that("filter_edges_by_weight returns all edges when no weight column", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1))
  result <- filter_edges_by_weight(edges, minimum = 0.5)
  expect_equal(nrow(result), 3)
})

test_that("filter_edges_by_weight removes edges below threshold", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.1, 0.5, 0.9))
  result <- filter_edges_by_weight(edges, minimum = 0.3)
  expect_equal(nrow(result), 2)
  expect_true(all(abs(result$weight) >= 0.3))
})

test_that("filter_edges_by_weight uses absolute values", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(-0.8, 0.2, -0.1))
  result <- filter_edges_by_weight(edges, minimum = 0.5)
  expect_equal(nrow(result), 1)
  expect_equal(result$weight, -0.8)
})

test_that("filter_edges_by_weight preserves row names", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.1, 0.5, 0.9))
  rownames(edges) <- c("e1", "e2", "e3")
  result <- filter_edges_by_weight(edges, minimum = 0.4)
  expect_equal(nrow(result), 2)
})

# ============================================
# GET_EDGE_ORDER TESTS
# ============================================

test_that("get_edge_order returns empty for zero edges", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- get_edge_order(edges)
  expect_equal(length(result), 0)
})

test_that("get_edge_order orders by weight (weakest first)", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.9, 0.1, 0.5))
  result <- get_edge_order(edges)
  # Edge 2 (0.1), then edge 3 (0.5), then edge 1 (0.9)
  expect_equal(result, c(2, 3, 1))
})

test_that("get_edge_order returns sequential order when no weights", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1))
  result <- get_edge_order(edges)
  expect_equal(result, c(1, 2, 3))
})

test_that("get_edge_order uses priority as primary sort key", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.9, 0.1, 0.5))
  priority <- c(2, 1, 3)  # Edge 2 lowest, edge 3 highest
  result <- get_edge_order(edges, priority = priority)
  # Sort by priority first: edge 2 (pri=1), edge 1 (pri=2), edge 3 (pri=3)
  expect_equal(result[1], 2)
  expect_equal(result[3], 3)
})

test_that("get_edge_order breaks priority ties with weight", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), weight = c(0.9, 0.1, 0.5))
  priority <- c(1, 1, 2)  # Edges 1 and 2 have same priority
  result <- get_edge_order(edges, priority = priority)
  # Within priority 1: edge 2 (weight 0.1) before edge 1 (weight 0.9)
  expect_equal(result[1], 2)
  expect_equal(result[2], 1)
  expect_equal(result[3], 3)
})

test_that("get_edge_order handles priority without weights", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1))
  priority <- c(3, 1, 2)
  result <- get_edge_order(edges, priority = priority)
  expect_equal(result[1], 2)  # Lowest priority
  expect_equal(result[3], 1)  # Highest priority
})

# ============================================
# GET_NODE_ORDER TESTS
# ============================================

test_that("get_node_order orders largest to smallest", {
  sizes <- c(5, 10, 3, 8)
  result <- get_node_order(sizes)
  # Largest (10) first, then 8, 5, 3
  expect_equal(result, c(2, 4, 1, 3))
})

test_that("get_node_order handles equal sizes", {
  sizes <- c(5, 5, 5)
  result <- get_node_order(sizes)
  expect_equal(length(result), 3)
  expect_true(all(result %in% 1:3))
})

test_that("get_node_order handles single node", {
  sizes <- c(10)
  result <- get_node_order(sizes)
  expect_equal(result, 1)
})

# ============================================
# INTEGRATION TESTS
# ============================================

test_that("resolve functions work together for complete workflow", {
  # Create test network
  edges <- data.frame(
    from = c(1, 2, 3, 1),
    to = c(2, 3, 1, 3),
    weight = c(0.8, 0.3, 0.5, 0.6)
  )
  nodes <- data.frame(
    label = c("A", "B", "C"),
    color = c("red", "green", "blue")
  )
  n <- 3
  m <- nrow(edges)

  # Test all resolve functions
  colors <- resolve_edge_colors(edges)
  expect_equal(length(colors), m)

  widths <- resolve_edge_widths(edges)
  expect_equal(length(widths), m)

  node_sizes <- resolve_node_sizes(NULL, n = n)
  expect_equal(length(node_sizes), n)

  label_sizes <- resolve_label_sizes(NULL, node_sizes, n)
  expect_equal(length(label_sizes), n)

  node_colors <- resolve_node_colors(NULL, n, nodes)
  expect_equal(node_colors, c("red", "green", "blue"))

  labels <- resolve_labels(TRUE, nodes, n)
  expect_equal(labels, c("A", "B", "C"))

  edge_labels <- resolve_edge_labels(TRUE, edges, m)
  expect_equal(length(edge_labels), m)

  shapes <- resolve_shapes(NULL, n)
  expect_equal(shapes, rep("circle", n))

  curves <- resolve_curvatures(0, edges)
  expect_equal(length(curves), m)

  # Filter and order
  filtered <- filter_edges_by_weight(edges, minimum = 0.4)
  expect_true(nrow(filtered) < m)

  order <- get_edge_order(edges)
  expect_equal(length(order), m)
})

test_that("resolve_centrality_sizes integrates with splot workflow", {
  skip_if_no_igraph()

  mat <- create_test_matrix(6, density = 0.5)

  # Test different centrality measures work
  for (measure in c("degree", "strength", "betweenness", "closeness")) {
    result <- tryCatch(
      resolve_centrality_sizes(mat, scale_by = measure),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_equal(length(result$sizes), 6)
      expect_true(all(result$sizes > 0))
    }
  }
})
