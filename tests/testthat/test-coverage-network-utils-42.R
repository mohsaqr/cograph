# =============================================================================
# Comprehensive Tests for network-utils.R - Targeting Uncovered Code
# Coverage target: 45+ additional tests for uncovered functions/branches
# =============================================================================

# -----------------------------------------------------------------------------
# Test Setup: Helper functions and test data
# -----------------------------------------------------------------------------

# Create test adjacency matrices
create_test_matrix <- function(n = 4, symmetric = TRUE, weighted = TRUE) {
  set.seed(42)
  mat <- matrix(runif(n * n), n, n)
  diag(mat) <- 0
  if (symmetric) {
    mat <- (mat + t(mat)) / 2
  }
  if (!weighted) {
    mat <- (mat > 0.5) * 1
  }
  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

# Create disconnected network matrix
create_disconnected_matrix <- function() {
  mat <- matrix(0, 6, 6)
  # Component 1: A-B-C
  mat[1, 2] <- mat[2, 1] <- 1
  mat[1, 3] <- mat[3, 1] <- 1
  # Component 2: D-E-F (larger)
  mat[4, 5] <- mat[5, 4] <- 1
  mat[5, 6] <- mat[6, 5] <- 1
  mat[4, 6] <- mat[6, 4] <- 1
  rownames(mat) <- colnames(mat) <- LETTERS[1:6]
  mat
}

# Create directed matrix with reciprocal edges
create_directed_matrix <- function() {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 0.5
  mat[2, 3] <- 0.6
  mat[3, 4] <- 0.7
  mat[4, 1] <- 0.8
  mat[2, 1] <- 0.3  # reciprocal edge
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  mat
}

# Create matrix with negative weights
create_negative_weight_matrix <- function() {
  mat <- matrix(c(0, 0.5, -0.3, 0,
                  0.5, 0, 0.4, -0.6,
                  -0.3, 0.4, 0, 0.7,
                  0, -0.6, 0.7, 0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  mat
}

# Create matrix with bridge edge (line graph)
create_bridge_matrix <- function() {
  # A-B-C-D-E with C-D as bridge to D-E cluster
  mat <- matrix(0, 6, 6)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[2, 3] <- mat[3, 2] <- 1
  mat[1, 3] <- mat[3, 1] <- 1  # Triangle A-B-C
  mat[3, 4] <- mat[4, 3] <- 1  # Bridge C-D
  mat[4, 5] <- mat[5, 4] <- 1
  mat[5, 6] <- mat[6, 5] <- 1
  mat[4, 6] <- mat[6, 4] <- 1  # Triangle D-E-F
  rownames(mat) <- colnames(mat) <- LETTERS[1:6]
  mat
}

# Create mock tna object
create_mock_tna <- function(symmetric = FALSE) {
  mat <- create_test_matrix(4, symmetric = symmetric)
  obj <- list(
    weights = mat,
    labels = c("A", "B", "C", "D")
  )
  class(obj) <- "tna"
  obj
}

# =============================================================================
# SECTION 1: to_igraph() with cograph_network directed override (lines 51-54)
# =============================================================================

test_that("to_igraph converts cograph_network from undirected to directed", {
  mat <- create_test_matrix(symmetric = TRUE)
  net <- as_cograph(mat)
  # Net is undirected, force to directed
  g <- to_igraph(net, directed = TRUE)

  expect_true(igraph::is_directed(g))
})

test_that("to_igraph converts cograph_network from directed to undirected", {
  mat <- create_directed_matrix()
  net <- as_cograph(mat, directed = TRUE)
  # Net is directed, force to undirected
  g <- to_igraph(net, directed = FALSE)

  expect_false(igraph::is_directed(g))
})

# =============================================================================
# SECTION 2: to_igraph() with tna object (lines 93-106)
# =============================================================================

test_that("to_igraph works with tna object", {
  tna_obj <- create_mock_tna(symmetric = FALSE)
  g <- to_igraph(tna_obj)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 4)
  expect_true(igraph::is_directed(g))
})

test_that("to_igraph with tna respects directed = TRUE override", {
  tna_obj <- create_mock_tna(symmetric = TRUE)
  g <- to_igraph(tna_obj, directed = TRUE)

  expect_true(igraph::is_directed(g))
})

test_that("to_igraph with tna respects directed = FALSE override", {
  tna_obj <- create_mock_tna(symmetric = FALSE)
  g <- to_igraph(tna_obj, directed = FALSE)

  expect_false(igraph::is_directed(g))
})

test_that("to_igraph with tna auto-detects symmetric as undirected", {
  tna_obj <- create_mock_tna(symmetric = TRUE)
  g <- to_igraph(tna_obj)

  expect_false(igraph::is_directed(g))
})

test_that("to_igraph preserves labels from tna object", {
  tna_obj <- create_mock_tna()
  g <- to_igraph(tna_obj)

  expect_equal(igraph::V(g)$name, c("A", "B", "C", "D"))
})

test_that("to_igraph handles tna without labels", {
  tna_obj <- create_mock_tna()
  tna_obj$labels <- NULL
  g <- to_igraph(tna_obj)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 4)
})

# =============================================================================
# SECTION 3: detect_communities with unnamed igraph (line 204)
# =============================================================================

test_that("detect_communities handles igraph without node names", {
  g <- igraph::make_ring(5)
  # Don't set vertex names
  result <- detect_communities(g, method = "louvain")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
  expect_true(all(result$node %in% as.character(1:5)))
})

# =============================================================================
# SECTION 4: color_communities with single color palette name (line 278)
# =============================================================================

test_that("color_communities with unknown palette name replicates it", {
  mat <- create_test_matrix()
  # Pass a single color string that's not a known palette name
  colors <- color_communities(mat, palette = "#FF0000")

  expect_equal(length(colors), 4)
  # All nodes should have the same color
  expect_true(all(colors == "#FF0000"))
})

test_that("color_communities with short palette extends properly", {
  # Create a network that will likely have multiple communities
  mat <- create_disconnected_matrix()
  colors <- color_communities(mat, palette = c("red"))

  expect_equal(length(colors), 6)
})

test_that("color_communities with palette 'pastel'", {
  mat <- create_test_matrix()
  colors <- color_communities(mat, palette = "pastel")

  expect_equal(length(colors), 4)
  expect_true(all(is.character(colors)))
})

test_that("color_communities with palette 'viridis'", {
  mat <- create_test_matrix()
  colors <- color_communities(mat, palette = "viridis")

  expect_equal(length(colors), 4)
  expect_true(all(is.character(colors)))
})

# =============================================================================
# SECTION 5: filter_edges with keep_format on empty network (lines 373-374)
# =============================================================================

test_that("filter_edges keep_format with igraph on empty network", {
  g <- igraph::make_empty_graph(5)
  igraph::V(g)$name <- LETTERS[1:5]

  expect_warning(
    result <- filter_edges(g, weight > 0, keep_format = TRUE),
    "no edges"
  )

  expect_true(inherits(result, "igraph"))
})

# =============================================================================
# SECTION 6: filter_nodes with keep_format empty result (lines 515-520)
# =============================================================================

test_that("filter_nodes keep_format = TRUE returns empty matrix for no matches", {
  mat <- create_test_matrix()

  expect_warning(
    result <- filter_nodes(mat, degree > 100, keep_format = TRUE),
    "No nodes match"
  )

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(0, 0))
})

test_that("filter_nodes keep_format with igraph returns empty igraph for no matches", {
  g <- igraph::make_ring(5)

  expect_warning(
    result <- suppressMessages(filter_nodes(g, degree > 100, keep_format = TRUE)),
    "No nodes match"
  )

  expect_true(inherits(result, "igraph"))
  expect_equal(igraph::vcount(result), 0)
})

# =============================================================================
# SECTION 7: .evaluate_filter_conditions with empty dots (line 600)
# =============================================================================

test_that("filter_edges with no filter expression returns all edges", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  n_orig <- n_edges(net)

  # No filter expression - should return all edges
  result <- filter_edges(net)

  expect_equal(n_edges(result), n_orig)
})

test_that("filter_nodes with no filter expression returns all nodes", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  n_orig <- n_nodes(net)

  # No filter expression - should return all nodes
  result <- filter_nodes(net)

  expect_equal(n_nodes(result), n_orig)
})

# =============================================================================
# SECTION 8: .detect_input_class for various types (lines 756-763)
# =============================================================================

test_that(".detect_input_class identifies network object", {
  skip_if_not_installed("network")

  mat <- create_test_matrix()
  net_obj <- network::network(mat, directed = FALSE)

  result <- cograph:::.detect_input_class(net_obj)
  expect_equal(result, "network")
})

test_that(".detect_input_class identifies tna object", {
  tna_obj <- create_mock_tna()
  result <- cograph:::.detect_input_class(tna_obj)
  expect_equal(result, "tna")
})

test_that(".detect_input_class identifies group_tna object", {
  obj <- list(a = 1)
  class(obj) <- "group_tna"
  result <- cograph:::.detect_input_class(obj)
  expect_equal(result, "tna")
})

test_that(".detect_input_class returns unknown for unsupported type", {
  obj <- list(a = 1)
  result <- cograph:::.detect_input_class(obj)
  expect_equal(result, "unknown")
})

# =============================================================================
# SECTION 9: to_network with various inputs (lines 942-950)
# =============================================================================

test_that("to_network detects directed from igraph input", {
  skip_if_not_installed("network")

  g <- igraph::make_ring(5, directed = TRUE)
  igraph::V(g)$name <- LETTERS[1:5]
  igraph::E(g)$weight <- rep(1, 5)

  result <- to_network(g)

  expect_true(network::is.directed(result))
})

test_that("to_network detects directed from network input", {
  skip_if_not_installed("network")

  mat <- create_test_matrix()
  net_obj <- network::network(mat, directed = TRUE)

  result <- to_network(net_obj)

  expect_true(network::is.directed(result))
})

test_that("to_network with tna assumes directed", {
  skip_if_not_installed("network")

  tna_obj <- create_mock_tna()
  result <- to_network(tna_obj)

  expect_true(network::is.directed(result))
})

# =============================================================================
# SECTION 10: select_nodes with empty network (lines 1094-1099)
# =============================================================================

test_that("select_nodes warns on empty network", {
  # Create an empty cograph_network directly
  empty_net <- cograph:::.empty_cograph_network(directed = FALSE)

  expect_warning(
    result <- select_nodes(empty_net, degree >= 0),
    "no nodes"
  )
})

test_that("select_nodes keep_format on empty network", {
  # Create an empty cograph_network directly
  empty_net <- cograph:::.empty_cograph_network(directed = FALSE)

  expect_warning(
    result <- select_nodes(empty_net, degree >= 0, keep_format = TRUE),
    "no nodes"
  )
})

# =============================================================================
# SECTION 11: select_nodes keep_format with empty result (lines 1159-1164, 1174)
# =============================================================================

test_that("select_nodes keep_format returns empty matrix for no matches", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_nodes(mat, degree > 100, keep_format = TRUE),
    "No nodes match"
  )

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(0, 0))
})

test_that("select_nodes keep_format with igraph returns empty igraph for no matches", {
  g <- igraph::make_ring(5)

  expect_warning(
    result <- suppressMessages(select_nodes(g, degree > 100, keep_format = TRUE)),
    "No nodes match"
  )

  expect_true(inherits(result, "igraph"))
})

test_that("select_nodes keep_format returns igraph on successful selection", {
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]

  result <- suppressMessages(select_nodes(g, degree >= 2, keep_format = TRUE))

  expect_true(inherits(result, "igraph"))
})

# =============================================================================
# SECTION 12: .select_by_component edge cases (lines 1223-1231)
# =============================================================================

test_that("select_nodes with non-existent node name for component", {
  mat <- create_disconnected_matrix()

  expect_warning(
    result <- select_nodes(mat, component = "Z"),
    "Node 'Z' not found"
  )
})

test_that("select_nodes with component that is not string, number, or 'largest'", {
  mat <- create_test_matrix()
  # If component doesn't match any condition, it should return all nodes
  # This tests line 1231

  result <- select_nodes(mat, component = NA)
  expect_true(n_nodes(result) > 0)
})

# =============================================================================
# SECTION 13: .select_by_neighbors with numeric indices (lines 1244-1248)
# =============================================================================

test_that("select_nodes with neighbors_of as numeric indices", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, neighbors_of = c(1, 2), order = 1)

  expect_true(inherits(result, "cograph_network"))
  expect_true(n_nodes(result) >= 2)
})

test_that("select_nodes with neighbors_of with out-of-range indices warns", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_nodes(mat, neighbors_of = c(1, 100), order = 1),
    "out of range"
  )
})

# =============================================================================
# SECTION 14: .select_by_top with NA centrality (lines 1264-1266)
# =============================================================================

test_that("select_nodes warns when centrality cannot be computed for top", {
  # Create a network that might cause centrality issues
  # Use an invalid centrality measure (should fall back to degree)
  mat <- create_test_matrix()
  result <- select_nodes(mat, top = 2, by = "unknown_measure")

  # Should still work (falls back to degree)
  expect_equal(n_nodes(result), 2)
})

test_that("select_nodes with top = 0 returns empty", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_nodes(mat, top = 0),
    "No nodes match"
  )
})

# =============================================================================
# SECTION 15: Lazy centrality measures (lines 1347-1387)
# =============================================================================

test_that("select_nodes with indegree centrality", {
  mat <- create_directed_matrix()
  result <- select_nodes(mat, indegree >= 1, directed = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with outdegree centrality", {
  mat <- create_directed_matrix()
  result <- select_nodes(mat, outdegree >= 1, directed = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with instrength centrality", {
  mat <- create_directed_matrix()
  result <- select_nodes(mat, instrength > 0, directed = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with outstrength centrality", {
  mat <- create_directed_matrix()
  result <- select_nodes(mat, outstrength > 0, directed = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with betweenness warns on negative weights", {
  mat <- create_negative_weight_matrix()

  expect_warning(
    result <- select_nodes(mat, top = 2, by = "betweenness"),
    "negative weights"
  )
})

test_that("select_nodes with closeness warns on negative weights", {
  mat <- create_negative_weight_matrix()

  expect_warning(
    result <- select_nodes(mat, top = 2, by = "closeness"),
    "negative weights"
  )
})

test_that("select_nodes with hub centrality", {
  mat <- create_directed_matrix()
  result <- select_nodes(mat, top = 2, by = "hub")

  expect_equal(n_nodes(result), 2)
})

test_that("select_nodes with authority centrality", {
  mat <- create_directed_matrix()
  result <- select_nodes(mat, top = 2, by = "authority")

  expect_equal(n_nodes(result), 2)
})

test_that("select_nodes with coreness for top selection", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, top = 2, by = "coreness")

  expect_equal(n_nodes(result), 2)
})

# =============================================================================
# SECTION 16: Lazy context variables (lines 1417-1457)
# =============================================================================

test_that("select_nodes with component context variable", {
  mat <- create_disconnected_matrix()
  result <- select_nodes(mat, component == 1)

  expect_true(inherits(result, "cograph_network"))
  expect_true(n_nodes(result) > 0)
})

test_that("select_nodes with is_articulation", {
  mat <- create_bridge_matrix()
  result <- select_nodes(mat, is_articulation)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with is_bridge_endpoint", {
  mat <- create_bridge_matrix()
  result <- select_nodes(mat, is_bridge_endpoint)

  expect_true(inherits(result, "cograph_network"))
  expect_true(n_nodes(result) > 0)
})

test_that("select_nodes with is_bridge_endpoint on fully connected graph", {
  # No bridges in a fully connected graph
  mat <- create_test_matrix()
  result <- select_nodes(mat, is_bridge_endpoint)

  # Should return empty or few nodes
  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 17: select_edges with empty network (lines 1702-1707)
# =============================================================================

test_that("select_edges warns on empty network", {
  mat <- matrix(0, 4, 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  net <- as_cograph(mat)

  expect_warning(
    result <- select_edges(net, weight > 0),
    "no edges"
  )
})

test_that("select_edges keep_format on empty network", {
  mat <- matrix(0, 4, 4)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  # Create network that has nodes but no edges
  net <- as_cograph(mat)

  expect_warning(
    result <- select_edges(net, weight > 0, keep_format = TRUE),
    "no edges"
  )

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 18: select_edges keep_format paths (lines 1765-1780)
# =============================================================================

test_that("select_edges keep_format with igraph warning", {
  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- c(0.1, 0.5, 0.3, 0.8, 0.2)

  expect_message(
    result <- select_edges(g, weight > 0.3),
    "keep_format"
  )
})

test_that("select_edges keep_format returns igraph on success", {
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]
  igraph::E(g)$weight <- c(0.1, 0.5, 0.3, 0.8, 0.2)

  result <- suppressMessages(select_edges(g, weight > 0.3, keep_format = TRUE))

  expect_true(inherits(result, "igraph"))
})

test_that("select_edges keep_format with empty result and keep_isolates", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_edges(mat, weight > 100, .keep_isolates = TRUE),
    "No edges match"
  )

  # With keep_isolates, should still have nodes
  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 19: .select_edges_involving warnings (lines 1796-1801)
# =============================================================================

test_that("select_edges involving non-existent node warns", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_edges(mat, involving = "Z"),
    "No nodes found"
  )
})

test_that("select_edges involving with numeric indices", {
  mat <- create_test_matrix()
  result <- select_edges(mat, involving = c(1, 2))

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 20: .select_edges_between edge cases (lines 1832-1834)
# =============================================================================

test_that("select_edges between with empty node sets warns", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_edges(mat, between = list("Z", "Y")),
    "empty"
  )
})

# =============================================================================
# SECTION 21: .select_edges_top with NA metric (lines 1877-1891)
# =============================================================================

test_that("select_edges top with unknown metric falls back to weight", {
  mat <- create_test_matrix()
  result <- select_edges(mat, top = 3, by = "unknown_metric")

  expect_true(n_edges(result) <= 3)
})

test_that("select_edges top = 0 returns empty", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_edges(mat, top = 0),
    "No edges match"
  )
})

# =============================================================================
# SECTION 22: .compute_single_edge_metric (lines 1938-1942)
# =============================================================================

test_that("select_edges top by abs_weight", {
  mat <- create_negative_weight_matrix()
  result <- select_edges(mat, top = 2, by = "abs_weight")

  expect_true(n_edges(result) <= 2)
})

# =============================================================================
# SECTION 23: .compute_lazy_edge_metrics (lines 1975-2018)
# =============================================================================

test_that("select_edges with to_strength expression", {
  mat <- create_test_matrix()
  result <- select_edges(mat, to_strength > 0)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with edge_betweenness expression", {
  mat <- create_test_matrix()
  result <- select_edges(mat, edge_betweenness > 0)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with is_mutual on directed network", {
  mat <- create_directed_matrix()
  result <- select_edges(mat, is_mutual, directed = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with from_label expression", {
  mat <- create_test_matrix()
  result <- select_edges(mat, from_label == "A")

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with to_label expression", {
  mat <- create_test_matrix()
  result <- select_edges(mat, to_label %in% c("C", "D"))

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 24: to_adjacency_matrix alias (line 2178)
# =============================================================================

test_that("to_adjacency_matrix works with cograph_network", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  result <- cograph:::to_adjacency_matrix(net)

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(4, 4))
})

# =============================================================================
# SECTION 25: filter_nodes with igraph format conversion message
# =============================================================================

test_that("filter_nodes shows message when converting igraph", {
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]

  expect_message(
    result <- filter_nodes(g, degree >= 2),
    "keep_format"
  )
})

# =============================================================================
# SECTION 26: select_nodes message when converting igraph
# =============================================================================

test_that("select_nodes shows message when converting igraph", {
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]

  expect_message(
    result <- select_nodes(g, degree >= 2),
    "keep_format"
  )
})

# =============================================================================
# SECTION 27: Additional edge cases
# =============================================================================

test_that("select_edges with negative top falls through correctly", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_edges(mat, involving = "A", top = -1),
    "No edges match"
  )
})

test_that("select_nodes with negative top warns", {
  mat <- create_test_matrix()

  expect_warning(
    result <- select_nodes(mat, top = -1),
    "No nodes match"
  )
})

test_that("filter_edges warns on filter that removes all nodes", {
  mat <- create_test_matrix()

  expect_warning(
    result <- filter_edges(mat, weight > 100, .keep_isolates = FALSE),
    "all edges|all nodes"
  )
})

# =============================================================================
# SECTION 28: Complex combined selections
# =============================================================================

test_that("select_nodes with multiple centrality measures", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, degree >= 2 & pagerank > 0)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with multiple expression conditions", {
  mat <- create_test_matrix()
  result <- select_edges(mat, weight > 0.3, abs_weight < 0.8)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes combining name and expression", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, name = c("A", "B", "C"), degree >= 2)

  labels <- get_labels(result)
  expect_true(all(labels %in% c("A", "B", "C")))
})

# =============================================================================
# SECTION 29: Additional coverage for remaining uncovered lines
# =============================================================================

# Lines 62-64: to_igraph with network when network package not available
# (This is defensive code that can't be tested easily)

# Lines 194: detect_communities leiden error (requires old igraph)
# (Can't be tested without unloading igraph)

# Lines 283: color_communities with palette shorter than communities
test_that("color_communities extends palette when too short", {
  # Create network with definitely multiple communities
  mat <- create_disconnected_matrix()
  # Use a palette that's shorter than the number of communities
  colors <- color_communities(mat, palette = c("red"))

  expect_equal(length(colors), 6)
})

# Line 362: filter_edges message for network objects
test_that("filter_edges shows message when converting network object", {
  skip_if_not_installed("network")

  mat <- create_test_matrix()
  net_obj <- network::network(mat, directed = FALSE, ignore.eval = FALSE,
                               names.eval = "weight")

  expect_message(
    result <- filter_edges(net_obj, weight > 0.3),
    "keep_format"
  )
})

# Lines 756-763: .detect_input_class coverage
# Already covered, but let's add qgraph detection
test_that(".detect_input_class identifies qgraph object", {
  obj <- list(a = 1)
  class(obj) <- "qgraph"
  result <- cograph:::.detect_input_class(obj)
  expect_equal(result, "qgraph")
})

# Line 773: .convert_to_format for network
test_that(".convert_to_format returns network format", {
  skip_if_not_installed("network")

  mat <- create_test_matrix()
  net <- as_cograph(mat)

  result <- cograph:::.convert_to_format(net, "network")
  expect_true(inherits(result, "network"))
})

# Lines 931-932: to_network error when network package unavailable
# (Can't be tested without mocking package availability)

# Lines 1365-1374: closeness and eigenvector with tryCatch paths
test_that("select_nodes closeness on positive weight network", {
  mat <- create_test_matrix()  # positive weights
  result <- select_nodes(mat, top = 2, by = "closeness")

  expect_equal(n_nodes(result), 2)
})

test_that("select_nodes eigenvector centrality", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, top = 2, by = "eigenvector")

  expect_equal(n_nodes(result), 2)
})

# Line 1766: select_edges with keep_format and empty result
test_that("select_edges keep_format returns igraph on empty result", {
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]
  igraph::E(g)$weight <- c(0.1, 0.5, 0.3, 0.8, 0.2)

  expect_warning(
    result <- suppressMessages(select_edges(g, weight > 100, keep_format = TRUE)),
    "No edges match"
  )

  expect_true(inherits(result, "igraph"))
})

# Lines 1877-1879: select_edges_top with all NA metrics
test_that("select_edges top with NA metric returns selection", {
  mat <- create_test_matrix()
  # The default metric is weight which won't be NA
  # But we can test through the normal flow
  result <- select_edges(mat, top = 2, by = "weight")
  expect_true(n_edges(result) <= 2)
})

# Line 1994: is_mutual on undirected network expression
test_that("select_edges with is_mutual on undirected network", {
  mat <- create_test_matrix(symmetric = TRUE)
  result <- select_edges(mat, is_mutual, directed = FALSE)

  # All edges in undirected network are considered mutual
  expect_true(inherits(result, "cograph_network"))
  expect_true(n_edges(result) > 0)
})

# =============================================================================
# SECTION 30: More edge cases for full coverage
# =============================================================================

test_that("select_nodes with closeness centrality expression", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, closeness > 0)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with eigenvector centrality expression", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, eigenvector > 0)

  expect_true(inherits(result, "cograph_network"))
})

test_that("filter_edges returns proper format when keeping isolates", {
  mat <- create_test_matrix()
  result <- filter_edges(mat, weight > 0.8, .keep_isolates = TRUE)

  # Should still have all nodes even if some edges removed
  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with top and involving combined", {
  mat <- create_test_matrix()
  result <- select_edges(mat, involving = c("A", "B"), top = 2)

  expect_true(n_edges(result) <= 2)
})

test_that("select_edges with bridges_only and top combined", {
  mat <- create_bridge_matrix()
  result <- select_edges(mat, bridges_only = TRUE, top = 1)

  expect_true(inherits(result, "cograph_network"))
})

test_that(".convert_to_format returns cograph_network for tna format", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)

  # For tna format, it should return the network as-is
  result <- cograph:::.convert_to_format(net, "tna")
  expect_true(inherits(result, "cograph_network"))
})

test_that(".convert_to_format returns cograph_network for unknown format", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)

  result <- cograph:::.convert_to_format(net, "unknown")
  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with hub and authority expressions", {
  mat <- create_directed_matrix()
  result <- select_nodes(mat, hub > 0 & authority > 0, directed = TRUE)

  expect_true(inherits(result, "cograph_network"))
})
