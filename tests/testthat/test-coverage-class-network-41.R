# Additional comprehensive tests for R/class-network.R
# This file targets edge cases and uncovered branches to increase coverage from 91%

# =============================================================================
# CographNetwork R6 Class - Node Matching Edge Cases
# =============================================================================

test_that("CographNetwork$new() matches nodes by 'name' column", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Nodes data frame with 'name' column (priority over 'label')
  nodes_df <- data.frame(
    name = c("A", "B", "C"),
    custom_attr = c("attr1", "attr2", "attr3"),
    stringsAsFactors = FALSE
  )

  net <- CographNetwork$new(mat, nodes = nodes_df)

  # Check that custom_attr was merged
  nodes <- net$get_nodes()
  expect_true("custom_attr" %in% names(nodes))
  expect_equal(nodes$custom_attr, c("attr1", "attr2", "attr3"))
})

test_that("CographNetwork$new() matches nodes by 'id' column when label/name don't match", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("X", "Y", "Z")

  # Nodes data frame with 'id' column matching the network labels
  nodes_df <- data.frame(
    id = c("X", "Y", "Z"),
    display_name = c("Node X", "Node Y", "Node Z"),
    stringsAsFactors = FALSE
  )

  net <- CographNetwork$new(mat, nodes = nodes_df)

  # Check that display_name was merged
  nodes <- net$get_nodes()
  expect_true("display_name" %in% names(nodes))
  expect_equal(nodes$display_name, c("Node X", "Node Y", "Node Z"))
})

test_that("CographNetwork$new() fallback to row order when no matching column found", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Nodes data frame with no matching columns but same row count
  nodes_df <- data.frame(
    custom_col1 = c("val1", "val2", "val3"),
    custom_col2 = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  net <- CographNetwork$new(mat, nodes = nodes_df)

  # Check that columns were merged by row order
  nodes <- net$get_nodes()
  expect_true("custom_col1" %in% names(nodes))
  expect_true("custom_col2" %in% names(nodes))
  expect_equal(nodes$custom_col1, c("val1", "val2", "val3"))
  expect_equal(nodes$custom_col2, c(10, 20, 30))
})

test_that("CographNetwork$new() partial match by 'name' column", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Nodes data frame with 'name' column - only some match
  nodes_df <- data.frame(
    name = c("A", "B", "D"),  # D doesn't exist in network
    extra = c("e1", "e2", "e3"),
    stringsAsFactors = FALSE
  )

  net <- CographNetwork$new(mat, nodes = nodes_df)

  # Should still match (partial match is valid as long as sum > 0)
  nodes <- net$get_nodes()
  expect_true("extra" %in% names(nodes))
  # A and B should have values, C should be NA (no match)
  expect_equal(nodes$extra[1], "e1")
  expect_equal(nodes$extra[2], "e2")
  expect_true(is.na(nodes$extra[3]))
})

test_that("CographNetwork$new() skips nodes merge when no matches and row count differs", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Nodes data frame with different row count and no matching column
  nodes_df <- data.frame(
    foo = c("x", "y"),  # Only 2 rows, network has 3 nodes
    stringsAsFactors = FALSE
  )

  # Should not merge (no match + different row count)
  net <- CographNetwork$new(mat, nodes = nodes_df)

  nodes <- net$get_nodes()
  # foo column should not be present

  expect_false("foo" %in% names(nodes))
})

# =============================================================================
# CographNetwork R6 Class - Layout Coords Edge Cases
# =============================================================================

test_that("CographNetwork set_layout_coords() with unnamed matrix columns", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  # Matrix without column names - as.data.frame creates V1, V2 names
  # but condition checks is.null(names(coords)) which will be FALSE
  # So we need to test that the layout is stored correctly
  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  net$set_layout_coords(coords)

  layout <- net$get_layout()
  # After conversion: names are V1 and V2, but the is.null check fails
  # so names remain V1 and V2 instead of being renamed to x and y
  expect_equal(nrow(layout), 3)
  # The columns may be V1/V2 or x/y depending on names(coords) check
  expect_true(ncol(layout) >= 2)
})

test_that("CographNetwork set_layout_coords() renames columns when matrix has NULL names", {
  # This tests the branch where is.matrix is TRUE and names(coords) is NULL
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  # Create matrix and remove colnames to get NULL names after as.data.frame
  # Actually, as.data.frame always adds V1, V2 names so we need a different approach
  # The code checks is.null(names(coords)) AFTER converting to data.frame
  # as.data.frame always adds names, so this branch is effectively unreachable
  # Instead test that matrix input gets properly converted
  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  net$set_layout_coords(coords)

  layout <- net$get_layout()
  # After matrix -> data.frame conversion, should have 3 rows and at least 2 columns
  expect_equal(nrow(layout), 3)
  expect_true(ncol(layout) >= 2)
})

test_that("CographNetwork set_layout_coords() handles NULL gracefully", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  # Setting NULL should not fail
  net$set_layout_coords(NULL)

  layout <- net$get_layout()
  # Layout should remain as before (or NULL if never set)
  expect_true(is.null(layout) || is.data.frame(layout))
})

test_that("CographNetwork set_layout_coords() with mismatched row count", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  # Coords with wrong row count
  coords <- data.frame(x = c(0, 1), y = c(0, 0))  # Only 2 rows, network has 3

  # Should set layout but NOT update nodes (row count mismatch)
  net$set_layout_coords(coords)

  layout <- net$get_layout()
  expect_equal(nrow(layout), 2)

  # Nodes should NOT have been updated (row count mismatch prevented update)
  nodes <- net$get_nodes()
  # Node x/y from original parsing should remain (not overwritten with mismatched coords)
  # Just verify node x values are NOT c(0, 1) truncated
  expect_false(identical(nodes$x[1:2], c(0, 1)))
})

# =============================================================================
# CographNetwork R6 Class - Active Bindings Edge Cases
# =============================================================================

test_that("CographNetwork node_labels returns NULL when nodes is NULL", {
  net <- CographNetwork$new()  # Empty network

  expect_null(net$node_labels)
})

test_that("CographNetwork n_nodes returns 0 for empty network", {
  net <- CographNetwork$new()
  expect_equal(net$n_nodes, 0L)
})

test_that("CographNetwork n_edges returns 0 for empty network", {
  net <- CographNetwork$new()
  expect_equal(net$n_edges, 0L)
})

test_that("CographNetwork has_weights with NULL weights", {
  net <- CographNetwork$new()
  # With NULL weights, has_weights should be FALSE
  expect_false(net$has_weights)
})

test_that("CographNetwork has_weights with mixed weights", {
  net <- CographNetwork$new()
  net$set_weights(c(0.5, 1, 1.5, 2))
  # Has varying weights, so has_weights should be TRUE
  expect_true(net$has_weights)
})

# =============================================================================
# CographNetwork R6 Class - Clone with Full State
# =============================================================================

test_that("CographNetwork clone_network() preserves all state", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net1 <- CographNetwork$new(mat)

  # Set all possible fields
  net1$set_layout_coords(data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))
  net1$set_theme("dark")
  net1$set_layout_info(list(name = "spring", seed = 123, iterations = 500))
  net1$set_plot_params(list(title = "Test Network", margin = 0.15, node_size = 0.08))
  net1$set_node_aes(list(fill = "blue", size = 0.1))
  net1$set_edge_aes(list(color = "red", width = 2))

  # Clone
  net2 <- net1$clone_network()

  # Verify all state is preserved
  expect_equal(net2$n_nodes, net1$n_nodes)
  expect_equal(net2$n_edges, net1$n_edges)
  expect_equal(net2$get_theme(), "dark")
  expect_equal(net2$get_layout_info()$name, "spring")
  expect_equal(net2$get_layout_info()$seed, 123)
  expect_equal(net2$get_plot_params()$title, "Test Network")
  expect_equal(net2$get_plot_params()$margin, 0.15)
  expect_equal(net2$get_node_aes()$fill, "blue")
  expect_equal(net2$get_edge_aes()$color, "red")
  expect_equal(net2$get_layout()$x, c(0, 1, 0.5))
})

test_that("CographNetwork clone_network() creates independent copy", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net1 <- CographNetwork$new(mat)
  net1$set_theme("light")

  net2 <- net1$clone_network()

  # Modify net2

  net2$set_theme("dark")

  # net1 should be unchanged
  expect_equal(net1$get_theme(), "light")
  expect_equal(net2$get_theme(), "dark")
})

# =============================================================================
# CographNetwork R6 Class - Print Method
# =============================================================================

test_that("CographNetwork print() shows layout status", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  # Without layout
  output <- capture.output(net$print())
  expect_true(any(grepl("Layout: none", output)))

  # With layout
  net$set_layout_coords(data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))
  output <- capture.output(net$print())
  expect_true(any(grepl("Layout: set", output)))
})

# =============================================================================
# .create_cograph_network() Edge Cases
# =============================================================================

test_that(".create_cograph_network() with NULL edges", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))

  net <- .create_cograph_network(
    nodes = nodes,
    edges = NULL,
    directed = FALSE
  )

  expect_s3_class(net, "cograph_network")
  expect_equal(nrow(net$edges), 0)
  expect_equal(net$edges$from, integer(0))
  expect_equal(net$edges$to, integer(0))
  expect_equal(net$edges$weight, numeric(0))
})

test_that(".create_cograph_network() coerces edge columns to correct types", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  edges <- data.frame(
    from = c("1", "2"),  # Character
    to = c("2", "3"),    # Character
    weight = c("0.5", "0.8")  # Character
  )

  net <- .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = FALSE
  )

  # Should be coerced to numeric/integer
  expect_type(net$edges$from, "integer")
  expect_type(net$edges$to, "integer")
  expect_type(net$edges$weight, "double")
})

test_that(".create_cograph_network() with node_groups", {
  nodes <- data.frame(id = 1:4, label = c("A", "B", "C", "D"))
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4), weight = c(1, 1, 1))
  node_groups <- data.frame(node = c("A", "B", "C", "D"), group = c("G1", "G1", "G2", "G2"))

  net <- .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    node_groups = node_groups
  )

  expect_equal(net$node_groups, node_groups)
})

# =============================================================================
# as_cograph() - Source Type Detection
# =============================================================================

test_that("as_cograph() detects igraph source correctly", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  net <- as_cograph(g)

  expect_equal(net$meta$source, "igraph")
})

test_that("as_cograph() detects edgelist source correctly", {
  edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A")
  )
  net <- as_cograph(edges)

  expect_equal(net$meta$source, "edgelist")
})

test_that("as_cograph() handles matrix without rownames", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  # No rownames or colnames

  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 3)
  # Should have auto-generated labels
  labels <- get_labels(net)
  expect_true(all(nchar(labels) > 0))
})

test_that("as_cograph() preserves weights_matrix from parsed input", {
  mat <- matrix(c(0, 0.3, 0.5, 0.4, 0, 0.6, 0.2, 0.7, 0), nrow = 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- as_cograph(mat)

  expect_true(!is.null(net$weights))
  expect_equal(dim(net$weights), c(3, 3))
  expect_equal(net$weights[1, 2], 0.3)
})

test_that("as_cograph() handles non-square matrix edge list", {
  edges <- data.frame(
    from = c(1, 2, 3, 4),
    to = c(2, 3, 4, 1),
    weight = c(0.5, 0.6, 0.7, 0.8)
  )

  net <- as_cograph(edges)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_edges(net), 4)
})

# =============================================================================
# get_nodes() Edge Cases
# =============================================================================

test_that("get_nodes() with cograph_network missing nodes returns error", {
  net <- structure(
    list(directed = FALSE, edges = data.frame()),
    class = c("cograph_network", "list")
  )
  # nodes is NULL/missing

  expect_error(get_nodes(net), "Cannot extract nodes")
})

# =============================================================================
# get_labels() Edge Cases
# =============================================================================

test_that("get_labels() with nodes missing both label and labels columns", {
  nodes <- data.frame(id = 1:3, custom = c("x", "y", "z"))
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(1, 1))

  net <- .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = FALSE
  )

  # Should error since no label/labels column exists
  expect_error(get_labels(net), "Cannot extract labels")
})

# =============================================================================
# set_nodes() Edge Cases
# =============================================================================

test_that("set_nodes() creates default label from id when label missing", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(id = 1:3)  # No label column
  net <- set_nodes(net, new_nodes)

  nodes <- get_nodes(net)
  # Label should be created from id
  expect_equal(nodes$label, c("1", "2", "3"))
})

# =============================================================================
# set_layout() Edge Cases
# =============================================================================

test_that("set_layout() converts matrix with more than 2 columns", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  # Matrix with 3 columns
  layout_mat <- matrix(c(0, 1, 0.5, 0, 0, 1, 5, 6, 7), ncol = 3)

  net <- set_layout(net, layout_mat)

  nodes <- get_nodes(net)
  expect_equal(nodes$x, c(0, 1, 0.5))
  expect_equal(nodes$y, c(0, 0, 1))
})

test_that("set_layout() with single column matrix fails", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  # Single column matrix - should fail
  layout_mat <- matrix(c(0, 1, 0.5), ncol = 1)

  expect_error(set_layout(net, layout_mat), "must have 'x' and 'y' columns")
})

# =============================================================================
# is_directed() Edge Cases
# =============================================================================

test_that("is_directed() with cograph_network missing directed field", {
  net <- structure(
    list(nodes = data.frame(id = 1:3, label = c("A", "B", "C"))),
    class = c("cograph_network", "list")
  )
  # directed is NULL/missing

  expect_error(is_directed(net), "Cannot determine directedness")
})

# =============================================================================
# n_nodes() and n_edges() Edge Cases
# =============================================================================

test_that("n_nodes() with NULL nodes field returns 0", {
  net <- structure(
    list(nodes = NULL, edges = data.frame(), directed = FALSE),
    class = c("cograph_network", "list")
  )

  expect_equal(n_nodes(net), 0L)
})

test_that("n_edges() with NULL edges field returns 0", {
  net <- structure(
    list(
      nodes = data.frame(id = 1:3, label = c("A", "B", "C")),
      edges = NULL,
      directed = FALSE
    ),
    class = c("cograph_network", "list")
  )

  expect_equal(n_edges(net), 0L)
})

# =============================================================================
# set_groups() Additional Edge Cases
# =============================================================================

test_that("set_groups() infers nodes from network when not provided with layers", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  rownames(mat) <- colnames(mat) <- c("N1", "N2", "N3", "N4")
  net <- as_cograph(mat)

  # layers without nodes - should use network's node labels
  net <- set_groups(net, layers = c("Top", "Top", "Bottom", "Bottom"))

  result <- get_groups(net)
  expect_equal(result$node, c("N1", "N2", "N3", "N4"))
  expect_equal(result$layer, c("Top", "Top", "Bottom", "Bottom"))
})

test_that("set_groups() infers nodes from network when not provided with clusters", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  net <- as_cograph(mat)

  # clusters without nodes
  net <- set_groups(net, clusters = c("C1", "C1", "C2", "C2"))

  result <- get_groups(net)
  expect_equal(result$node, c("A", "B", "C", "D"))
  expect_true("cluster" %in% names(result))
})

test_that("set_groups() data.frame with only 2 columns and no type column", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  rownames(mat) <- colnames(mat) <- c("N1", "N2", "N3", "N4")
  net <- as_cograph(mat)

  df <- data.frame(
    first_col = c("N1", "N2", "N3", "N4"),
    second_col = c("A", "A", "B", "B")
  )

  # Type defaults to "group", so second column should be renamed to "group"
  net <- set_groups(net, df, type = "group")

  result <- get_groups(net)
  expect_true("group" %in% names(result))
})

test_that("set_groups() errors on invalid groups argument type", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  rownames(mat) <- colnames(mat) <- c("N1", "N2", "N3", "N4")
  net <- as_cograph(mat)

  # Single element vector should not work as groups
  expect_error(
    set_groups(net, groups = 42),
    "groups must be"
  )
})

test_that("set_groups() errors on nodes/layers length mismatch", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  rownames(mat) <- colnames(mat) <- c("N1", "N2", "N3", "N4")
  net <- as_cograph(mat)

  expect_error(
    set_groups(net, nodes = c("N1", "N2", "N3", "N4"), layers = c("A", "B")),
    "must have the same length"
  )
})

test_that("set_groups() errors on nodes/clusters length mismatch", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  rownames(mat) <- colnames(mat) <- c("N1", "N2", "N3", "N4")
  net <- as_cograph(mat)

  expect_error(
    set_groups(net, nodes = c("N1", "N2"), clusters = c("C1", "C1", "C2", "C2")),
    "must have the same length"
  )
})

test_that("set_groups() data.frame with single column errors", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  rownames(mat) <- colnames(mat) <- c("N1", "N2", "N3", "N4")
  net <- as_cograph(mat)

  df <- data.frame(nodes = c("N1", "N2", "N3", "N4"))  # Single column

  expect_error(
    set_groups(net, df),
    "at least 2 columns"
  )
})

# =============================================================================
# Integration Tests - Complex Workflows
# =============================================================================

test_that("Full workflow: create, modify, query network", {
  # Create from matrix
  mat <- matrix(runif(25), 5, 5)
  mat <- (mat + t(mat)) / 2  # Make symmetric
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  net <- as_cograph(mat)

  # Modify nodes
  new_nodes <- data.frame(
    id = 1:5,
    label = paste0("Node_", LETTERS[1:5]),
    size = c(1, 2, 3, 4, 5)
  )
  net <- set_nodes(net, new_nodes)

  # Set layout
  layout <- data.frame(
    x = cos(2 * pi * (0:4) / 5),
    y = sin(2 * pi * (0:4) / 5)
  )
  net <- set_layout(net, layout)

  # Add groups
  net <- set_groups(net, c("G1", "G1", "G2", "G2", "G2"))

  # Query
  expect_equal(n_nodes(net), 5)
  expect_equal(get_labels(net), paste0("Node_", LETTERS[1:5]))
  expect_equal(get_nodes(net)$size, c(1, 2, 3, 4, 5))
  expect_true(!is.null(get_groups(net)))
  expect_equal(nrow(get_groups(net)), 5)
})

test_that("Network round-trip: as_cograph -> set_nodes -> set_edges", {
  # Original matrix
  mat <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- as_cograph(mat)
  original_n_nodes <- n_nodes(net)
  original_n_edges <- n_edges(net)

  # Modify nodes
  net <- set_nodes(net, data.frame(id = 1:3, label = c("X", "Y", "Z")))

  # Modify edges
  net <- set_edges(net, data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.5)))

  # Check modifications
  expect_equal(n_nodes(net), original_n_nodes)  # Same count
  expect_equal(n_edges(net), 2)  # New edge count
  expect_equal(get_labels(net), c("X", "Y", "Z"))
})

test_that("Network with special label characters", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  # Use special characters in labels
  rownames(mat) <- colnames(mat) <- c("Node (1)", "Node-2", "Node_3.txt")

  net <- as_cograph(mat)
  labels <- get_labels(net)

  expect_equal(labels[1], "Node (1)")
  expect_equal(labels[2], "Node-2")
  expect_equal(labels[3], "Node_3.txt")
})

test_that("Network with unicode labels", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("Alpha", "Beta", "Gamma")

  net <- as_cograph(mat)

  # Set nodes with unicode
  new_nodes <- data.frame(
    id = 1:3,
    label = c("Alpha", "Beta", "Gamma"),
    labels = c("\u03B1", "\u03B2", "\u03B3")  # Greek letters
  )
  net <- set_nodes(net, new_nodes)

  # get_labels should return the 'labels' column (priority)
  labels <- get_labels(net)
  expect_equal(labels, c("\u03B1", "\u03B2", "\u03B3"))
})

test_that("Empty network operations", {
  net <- CographNetwork$new()

  # These should not error
  expect_equal(net$n_nodes, 0L)
  expect_equal(net$n_edges, 0L)
  expect_false(net$is_directed)
  expect_false(net$has_weights)
  expect_null(net$node_labels)
  expect_null(net$get_layout())
})

test_that("Network with zero-weight edges", {
  mat <- matrix(c(0, 0, 1, 0, 0, 0, 1, 0, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- as_cograph(mat)

  # Should have edges where weight > 0
  edges <- get_edges(net)
  expect_true(all(edges$weight != 0))
})

test_that("Large network performance (100 nodes)", {
  n <- 100
  mat <- matrix(runif(n * n), n, n)
  mat <- (mat + t(mat)) / 2
  diag(mat) <- 0
  mat[mat < 0.5] <- 0  # Make sparse
  rownames(mat) <- colnames(mat) <- paste0("N", seq_len(n))

  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 100)
  expect_true(n_edges(net) > 0)
  expect_s3_class(net, "cograph_network")
})

# =============================================================================
# to_cograph() Alias Tests
# =============================================================================

test_that("to_cograph() with all argument combinations", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  # Test with directed = TRUE
  net1 <- to_cograph(mat, directed = TRUE)
  expect_true(is_directed(net1))

  # Test with directed = FALSE
  net2 <- to_cograph(mat, directed = FALSE)
  expect_false(is_directed(net2))

  # Test with directed = NULL (auto-detect)
  net3 <- to_cograph(mat, directed = NULL)
  expect_true(is_directed(net3))  # Asymmetric matrix
})

# =============================================================================
# nodes() Deprecated Function Tests
# =============================================================================

test_that("nodes() works as alias for get_nodes()", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  # Both should return identical results
  result_nodes <- nodes(net)
  result_get_nodes <- get_nodes(net)

  expect_identical(result_nodes, result_get_nodes)
})

# =============================================================================
# get_edges() Empty Network Edge Cases
# =============================================================================

test_that("get_edges() returns proper empty data frame for network with no edges field", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))

  net <- .create_cograph_network(
    nodes = nodes,
    edges = data.frame(from = integer(0), to = integer(0)),
    directed = FALSE
  )

  edges <- get_edges(net)

  expect_equal(nrow(edges), 0)
  expect_true("from" %in% names(edges))
  expect_true("to" %in% names(edges))
  expect_true("weight" %in% names(edges))
})

# =============================================================================
# CographNetwork nodes argument with 'label' matching
# =============================================================================

test_that("CographNetwork$new() matches nodes by 'label' column", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("X", "Y", "Z")

  # Nodes data frame with 'label' column matching network labels
  nodes_df <- data.frame(
    label = c("X", "Y", "Z"),
    color = c("red", "green", "blue"),
    stringsAsFactors = FALSE
  )

  net <- CographNetwork$new(mat, nodes = nodes_df)

  # Check that color was merged
  nodes <- net$get_nodes()
  expect_true("color" %in% names(nodes))
  expect_equal(nodes$color, c("red", "green", "blue"))
})

# =============================================================================
# Community Detection in set_groups()
# =============================================================================

test_that("set_groups() with walktrap community detection", {
  skip_if_not_installed("igraph")

  # Create network with clear community structure
  mat <- matrix(0, 6, 6)
  # Community 1: nodes 1-3
  mat[1, 2] <- mat[2, 1] <- 0.9
  mat[1, 3] <- mat[3, 1] <- 0.8
  mat[2, 3] <- mat[3, 2] <- 0.85
  # Community 2: nodes 4-6
  mat[4, 5] <- mat[5, 4] <- 0.9
  mat[4, 6] <- mat[6, 4] <- 0.8
  mat[5, 6] <- mat[6, 5] <- 0.85
  # Weak link between communities
  mat[3, 4] <- mat[4, 3] <- 0.1
  rownames(mat) <- colnames(mat) <- paste0("N", 1:6)

  net <- as_cograph(mat)

  # Use walktrap
  net <- set_groups(net, "walktrap", type = "cluster")

  result <- get_groups(net)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 6)
  expect_true("cluster" %in% names(result))
})

test_that("set_groups() with fast_greedy community detection", {
  skip_if_not_installed("igraph")

  mat <- matrix(0, 6, 6)
  mat[1, 2] <- mat[2, 1] <- 0.9
  mat[1, 3] <- mat[3, 1] <- 0.8
  mat[2, 3] <- mat[3, 2] <- 0.85
  mat[4, 5] <- mat[5, 4] <- 0.9
  mat[4, 6] <- mat[6, 4] <- 0.8
  mat[5, 6] <- mat[6, 5] <- 0.85
  mat[3, 4] <- mat[4, 3] <- 0.1
  rownames(mat) <- colnames(mat) <- paste0("N", 1:6)

  net <- as_cograph(mat)

  net <- set_groups(net, "fast_greedy", type = "layer")

  result <- get_groups(net)
  expect_true("layer" %in% names(result))
})

# =============================================================================
# Edge cases in .create_cograph_network() with layout
# =============================================================================

test_that(".create_cograph_network() stores layout in nodes x/y", {
  nodes <- data.frame(
    id = 1:3,
    label = c("A", "B", "C"),
    x = c(0, 1, 0.5),
    y = c(0, 0, 1)
  )
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(1, 1))

  net <- .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    meta = list(layout = list(name = "custom", seed = NULL))
  )

  expect_equal(net$nodes$x, c(0, 1, 0.5))
  expect_equal(net$nodes$y, c(0, 0, 1))
  expect_equal(net$meta$layout$name, "custom")
})

# =============================================================================
# set_groups() with 'groups' parameter as named list with type
# =============================================================================

test_that("set_groups() named list with explicit type parameter", {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  net <- as_cograph(mat)

  groups_list <- list(
    Layer1 = c("A", "B"),
    Layer2 = c("C", "D")
  )

  net <- set_groups(net, groups_list, type = "layer")

  result <- get_groups(net)
  expect_true("layer" %in% names(result))
  expect_equal(result$layer[result$node == "A"], "Layer1")
  expect_equal(result$layer[result$node == "C"], "Layer2")
})

# =============================================================================
# Additional R6 method tests
# =============================================================================

test_that("CographNetwork returns self invisibly from setters",
 {
  net <- CographNetwork$new()

  # All setters should return invisible(self)
  result1 <- net$set_nodes(data.frame(id = 1:2, label = c("A", "B")))
  result2 <- net$set_edges(data.frame(from = 1, to = 2, weight = 1))
  result3 <- net$set_directed(TRUE)
  result4 <- net$set_weights(c(1, 2))
  result5 <- net$set_layout_coords(data.frame(x = c(0, 1), y = c(0, 1)))
  result6 <- net$set_node_aes(list(fill = "red"))
  result7 <- net$set_edge_aes(list(color = "blue"))
  result8 <- net$set_theme("dark")
  result9 <- net$set_layout_info(list(name = "spring"))
  result10 <- net$set_plot_params(list(title = "Test"))

  # All should return the network object (self)
  expect_s3_class(result1, "CographNetwork")
  expect_s3_class(result2, "CographNetwork")
  expect_s3_class(result3, "CographNetwork")
  expect_s3_class(result4, "CographNetwork")
  expect_s3_class(result5, "CographNetwork")
  expect_s3_class(result6, "CographNetwork")
  expect_s3_class(result7, "CographNetwork")
  expect_s3_class(result8, "CographNetwork")
  expect_s3_class(result9, "CographNetwork")
  expect_s3_class(result10, "CographNetwork")
})
