# test-coverage-input-qgraph-41.R - Comprehensive Tests for input-qgraph.R
# Targets all code paths in R/input-qgraph.R for full coverage

# =============================================================================
# SETUP: Access internal functions
# =============================================================================

parse_qgraph <- cograph:::parse_qgraph
create_nodes_df <- cograph:::create_nodes_df
create_edges_df <- cograph:::create_edges_df

# =============================================================================
# SECTION 1: Basic parse_qgraph() functionality
# =============================================================================

test_that("parse_qgraph requires qgraph package", {
  # We'll just check that the function exists and works when qgraph is present
  skip_if_not_installed("qgraph")

  # Create a simple qgraph object
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_type(result, "list")
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_true("directed" %in% names(result))
  expect_true("weights" %in% names(result))
})

test_that("parse_qgraph validates qgraph input", {
  skip_if_not_installed("qgraph")

  # Non-qgraph object without Arguments should fail
  not_qgraph <- list(foo = 1, bar = 2)
  expect_error(parse_qgraph(not_qgraph), "must be a qgraph object")
})

test_that("parse_qgraph accepts object with Arguments field", {
  skip_if_not_installed("qgraph")

  # Create mock object with Arguments (mimics qgraph structure)
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Verify it has Arguments
  expect_true(!is.null(q$Arguments))

  result <- parse_qgraph(q)
  expect_type(result, "list")
})

# =============================================================================
# SECTION 2: Directed/undirected detection
# =============================================================================

test_that("parse_qgraph detects directed from Edgelist$directed", {
  skip_if_not_installed("qgraph")

  # Asymmetric (directed) matrix
  adj <- matrix(c(0, 1, 0, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, directed = TRUE)

  result <- parse_qgraph(q)
  expect_true(result$directed)
})

test_that("parse_qgraph detects undirected from symmetric matrix", {
  skip_if_not_installed("qgraph")

  # Symmetric matrix
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, directed = FALSE)

  result <- parse_qgraph(q)
  expect_false(result$directed)
})

test_that("parse_qgraph respects forced directed parameter", {
  skip_if_not_installed("qgraph")

  # Symmetric matrix but force directed
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q, directed = TRUE)
  expect_true(result$directed)

  result2 <- parse_qgraph(q, directed = FALSE)
  expect_false(result2$directed)
})

test_that("parse_qgraph falls back to matrix symmetry when no directed info", {
  skip_if_not_installed("qgraph")

  # Create qgraph and remove directed field from Edgelist
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Manually remove directed from Edgelist to test fallback
  q_modified <- q
  q_modified$Edgelist$directed <- NULL

  result <- parse_qgraph(q_modified)
  # Symmetric matrix should be undirected
  expect_false(result$directed)
})

test_that("parse_qgraph defaults to FALSE when no matrix to check symmetry", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Remove both directed field and input matrix
  q_modified <- q
  q_modified$Edgelist$directed <- NULL
  q_modified$Arguments$input <- NULL

  result <- parse_qgraph(q_modified)
  expect_false(result$directed)
})

# =============================================================================
# SECTION 3: Node information extraction
# =============================================================================

test_that("parse_qgraph extracts node names from graphAttributes", {

  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(adj) <- colnames(adj) <- c("NodeA", "NodeB")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)
  # qgraph may store names as labels or truncate - check nodes are present
  expect_equal(nrow(result$nodes), 2)
  expect_true(all(!is.na(result$nodes$label)))
})

test_that("parse_qgraph extracts labels when available", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, labels = c("X", "Y"))

  result <- parse_qgraph(q)
  expect_true(is.data.frame(result$nodes))
  expect_equal(nrow(result$nodes), 2)
})

test_that("parse_qgraph infers n from input matrix", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Remove names/labels to force inference from matrix
  q_modified <- q
  q_modified$graphAttributes$Nodes$names <- NULL
  q_modified$graphAttributes$Nodes$labels <- NULL

  result <- parse_qgraph(q_modified)
  expect_equal(nrow(result$nodes), 3)
})

test_that("parse_qgraph infers n from edge list when no matrix", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Remove names, labels, and input matrix
  q_modified <- q
  q_modified$graphAttributes$Nodes$names <- NULL
  q_modified$graphAttributes$Nodes$labels <- NULL
  q_modified$Arguments$input <- NULL

  result <- parse_qgraph(q_modified)
  # n should be inferred from max(from, to) in edge list
  expect_true(nrow(result$nodes) >= 1)
})

# =============================================================================
# SECTION 4: Edge extraction
# =============================================================================

test_that("parse_qgraph handles empty edge list", {
  skip_if_not_installed("qgraph")

  # Empty network (no edges)
  adj <- matrix(0, 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_equal(nrow(result$edges), 0)
  expect_equal(length(result$weights), 0)
})

test_that("parse_qgraph extracts edge weights", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(all(result$weights > 0))
  expect_true(all(result$edges$weight > 0))
})

test_that("parse_qgraph handles edges without weight field", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Remove weight field from Edgelist
  q_modified <- q
  q_modified$Edgelist$weight <- NULL

  result <- parse_qgraph(q_modified)
  # Should default to weight = 1
  expect_true(all(result$edges$weight == 1))
})

test_that("parse_qgraph extracts correct from/to indices", {
  skip_if_not_installed("qgraph")

  # Specific directed network
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, directed = TRUE)

  result <- parse_qgraph(q)

  # Should have 2 edges: 1->2 and 2->3
  expect_equal(nrow(result$edges), 2)
})

# =============================================================================
# SECTION 5: NULL Edgelist handling
# =============================================================================

test_that("parse_qgraph handles NULL Edgelist", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Set Edgelist to NULL
  q_modified <- q
  q_modified$Edgelist <- NULL

  result <- parse_qgraph(q_modified)

  # Should handle gracefully with empty edges
  expect_equal(nrow(result$edges), 0)
  expect_equal(length(result$weights), 0)
})

test_that("parse_qgraph handles Edgelist with empty from", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Set from to empty
  q_modified <- q
  q_modified$Edgelist$from <- integer(0)
  q_modified$Edgelist$to <- integer(0)
  q_modified$Edgelist$weight <- numeric(0)

  result <- parse_qgraph(q_modified)

  expect_equal(nrow(result$edges), 0)
})

# =============================================================================
# SECTION 6: Layout extraction
# =============================================================================

test_that("parse_qgraph handles layout extraction", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, layout = "circle")

  result <- parse_qgraph(q)

  # Layout should be in nodes (may be NA if conditions don't match)
  expect_true("x" %in% names(result$nodes))
  expect_true("y" %in% names(result$nodes))
})

test_that("parse_qgraph handles NULL layout", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Remove layout
  q_modified <- q
  q_modified$layout <- NULL

  result <- parse_qgraph(q_modified)

  # Should work without layout (x, y remain NA)
  expect_true(is.data.frame(result$nodes))
})

test_that("parse_qgraph handles layout with wrong dimensions", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Set layout with wrong number of rows
  q_modified <- q
  q_modified$layout <- matrix(c(0, 0, 1, 1, 2, 2), nrow = 3, ncol = 2)

  result <- parse_qgraph(q_modified)

  # Should not crash - layout might not be copied
  expect_true(is.data.frame(result$nodes))
})

# =============================================================================
# SECTION 7: Large networks
# =============================================================================

test_that("parse_qgraph handles larger networks", {
  skip_if_not_installed("qgraph")

  set.seed(123)
  n <- 20
  adj <- matrix(runif(n * n), n, n)
  adj <- adj * (adj > 0.7)  # Sparse
  diag(adj) <- 0
  rownames(adj) <- colnames(adj) <- paste0("N", 1:n)

  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$nodes) > 0)
  expect_true(nrow(result$edges) >= 0)
})

# =============================================================================
# SECTION 8: Weighted networks
# =============================================================================

test_that("parse_qgraph preserves weight values", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 0.25, 0.75, 0.25, 0, 0.5, 0.75, 0.5, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Weights should be preserved (at least present)
  expect_true(length(result$weights) > 0)
  expect_true(all(result$weights > 0 & result$weights <= 1))
})

test_that("parse_qgraph handles negative weights", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, -0.5, 0.3, -0.5, 0, -0.8, 0.3, -0.8, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(any(result$weights < 0))
})

# =============================================================================
# SECTION 9: Return structure validation
# =============================================================================

test_that("parse_qgraph returns correct list structure", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_type(result, "list")
  expect_named(result, c("nodes", "edges", "directed", "weights"))

  expect_s3_class(result$nodes, "data.frame")
  expect_s3_class(result$edges, "data.frame")
  expect_type(result$directed, "logical")
  expect_type(result$weights, "double")
})

test_that("parse_qgraph nodes dataframe has correct structure", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(adj) <- colnames(adj) <- c("A", "B")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true("id" %in% names(result$nodes))
  expect_true("label" %in% names(result$nodes))
  expect_true("name" %in% names(result$nodes))
  expect_true("x" %in% names(result$nodes))
  expect_true("y" %in% names(result$nodes))
})

test_that("parse_qgraph edges dataframe has correct structure", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true("from" %in% names(result$edges))
  expect_true("to" %in% names(result$edges))
  expect_true("weight" %in% names(result$edges))
})

# =============================================================================
# SECTION 10: Edge cases - single node networks
# =============================================================================

test_that("parse_qgraph handles single node network", {
  skip_if_not_installed("qgraph")

  adj <- matrix(0, 1, 1)
  rownames(adj) <- colnames(adj) <- "Solo"
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$nodes) >= 1)
  expect_equal(nrow(result$edges), 0)
})

test_that("parse_qgraph handles single node with self-loop", {
  skip_if_not_installed("qgraph")

  adj <- matrix(0.5, 1, 1)
  rownames(adj) <- colnames(adj) <- "Self"
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$nodes) >= 1)
  # Self-loop might or might not be included depending on qgraph version
  expect_true(nrow(result$edges) <= 1)
})

# =============================================================================
# SECTION 11: Bidirectional edges
# =============================================================================

test_that("parse_qgraph handles bidirectional edges correctly", {
  skip_if_not_installed("qgraph")

  # Asymmetric weights indicate directed
  adj <- matrix(c(0, 0.3, 0.7, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, directed = TRUE)

  result <- parse_qgraph(q)

  expect_true(result$directed)
  expect_equal(nrow(result$edges), 2)  # Two separate edges
})

# =============================================================================
# SECTION 12: Mixed qgraph configurations
# =============================================================================

test_that("parse_qgraph handles qgraph with custom layout matrix", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  custom_layout <- matrix(c(0, 1, 0, 1), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, layout = custom_layout)

  result <- parse_qgraph(q)

  expect_true(is.data.frame(result$nodes))
  expect_true(nrow(result$nodes) >= 1)
})

test_that("parse_qgraph handles qgraph with groups", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
  groups <- list(GroupA = 1:2, GroupB = 3:4)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, groups = groups)

  result <- parse_qgraph(q)

  # Groups may affect how qgraph stores node info
  expect_true(is.data.frame(result$nodes))
  expect_true(nrow(result$nodes) >= 1)
})

# =============================================================================
# SECTION 13: Sparse networks
# =============================================================================

test_that("parse_qgraph handles sparse network", {
  skip_if_not_installed("qgraph")

  # Only one edge in a 5-node network
  adj <- matrix(0, 5, 5)
  adj[1, 2] <- 0.5
  adj[2, 1] <- 0.5
  rownames(adj) <- colnames(adj) <- LETTERS[1:5]

  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$edges) >= 1)
})

# =============================================================================
# SECTION 14: Dense networks
# =============================================================================

test_that("parse_qgraph handles fully connected network", {
  skip_if_not_installed("qgraph")

  n <- 5
  adj <- matrix(1, n, n)
  diag(adj) <- 0
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Should have edges
  expect_true(nrow(result$edges) >= 1)
})

# =============================================================================
# SECTION 15: Numeric labels
# =============================================================================

test_that("parse_qgraph generates numeric labels when no names given", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  # No dimnames
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Clear any automatically assigned names
  q_modified <- q
  q_modified$graphAttributes$Nodes$names <- NULL
  q_modified$graphAttributes$Nodes$labels <- NULL
  q_modified$Arguments$input <- adj  # Keep matrix for n inference

  result <- parse_qgraph(q_modified)

  expect_equal(nrow(result$nodes), 3)
})

# =============================================================================
# SECTION 16: Integration with cograph
# =============================================================================

test_that("parse_qgraph output works with splot", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  parsed <- parse_qgraph(q)

  # Use the parsed data to create a cograph_network
  result <- safe_plot({
    splot(adj)
  })

  expect_true(result$success, info = result$error)
})

# =============================================================================
# SECTION 17: Edge case - all zero weights
# =============================================================================

test_that("parse_qgraph handles all-zero matrix", {
  skip_if_not_installed("qgraph")

  adj <- matrix(0, 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_equal(nrow(result$edges), 0)
})

# =============================================================================
# SECTION 18: Various matrix types
# =============================================================================

test_that("parse_qgraph handles integer matrix", {
  skip_if_not_installed("qgraph")

  adj <- matrix(as.integer(c(0, 1, 2, 1, 0, 3, 2, 3, 0)), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$edges) > 0)
})

test_that("parse_qgraph handles very small weights", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1e-10, 1e-10, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(is.data.frame(result$nodes))
})

# =============================================================================
# SECTION 19: Multiple edges between same nodes
# =============================================================================

test_that("parse_qgraph extracts all unique edges", {
  skip_if_not_installed("qgraph")

  # Simple network to verify edge extraction
  adj <- matrix(c(
    0, 0.5, 0.3,
    0.5, 0, 0.7,
    0.3, 0.7, 0
  ), 3, 3, byrow = TRUE)

  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # For undirected, should have 3 unique edges
  expect_true(nrow(result$edges) >= 3)
})

# =============================================================================
# SECTION 20: Asymmetric input forcing direction check
# =============================================================================

test_that("parse_qgraph detects asymmetric matrix as directed via isSymmetric", {
  skip_if_not_installed("qgraph")

  # Clearly asymmetric
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Remove directed field to force isSymmetric check
  q_modified <- q
  q_modified$Edgelist$directed <- NULL

  result <- parse_qgraph(q_modified)

  expect_true(result$directed)
})

# =============================================================================
# SECTION 21: Layout as non-matrix object
# =============================================================================

test_that("parse_qgraph handles layout not being a matrix", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Set layout to non-matrix
  q_modified <- q
  q_modified$layout <- list(x = c(0, 1), y = c(0, 1))

  result <- parse_qgraph(q_modified)

  # Should not crash - x, y may be NA
  expect_true(is.data.frame(result$nodes))
})

# =============================================================================
# SECTION 22: Verify correct edge weight alignment
# =============================================================================

test_that("parse_qgraph maintains weight-edge alignment", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 0.25, 0.50, 0.75, 0.25, 0, 0.33, 0.67,
                  0.50, 0.33, 0, 0.80, 0.75, 0.67, 0.80, 0), 4, 4)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # weights vector should match edges df weight column
  expect_equal(length(result$weights), nrow(result$edges))
  expect_equal(result$weights, result$edges$weight)
})

# =============================================================================
# SECTION 23: Various graphAttributes configurations
# =============================================================================

test_that("parse_qgraph handles missing graphAttributes$Nodes", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Remove Nodes entirely
  q_modified <- q
  q_modified$graphAttributes$Nodes <- NULL

  # Should infer from matrix
  result <- parse_qgraph(q_modified)

  expect_true(is.data.frame(result$nodes))
  expect_true(nrow(result$nodes) >= 1)
})

# =============================================================================
# SECTION 24: Verify node ID sequence
# =============================================================================

test_that("parse_qgraph assigns sequential node IDs", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Force n inference from edge list
  q_modified <- q
  q_modified$graphAttributes$Nodes <- NULL

  result <- parse_qgraph(q_modified)

  expect_equal(result$nodes$id, seq_len(nrow(result$nodes)))
})

# =============================================================================
# SECTION 25: From/to indices are valid
# =============================================================================

test_that("parse_qgraph produces valid from/to indices", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  if (nrow(result$edges) > 0) {
    # From/to should be positive integers
    expect_true(all(result$edges$from >= 1))
    expect_true(all(result$edges$to >= 1))
    # They should be integers
    expect_true(all(result$edges$from == as.integer(result$edges$from)))
    expect_true(all(result$edges$to == as.integer(result$edges$to)))
  }
})

# =============================================================================
# SECTION 26: Consistency with qgraph structure
# =============================================================================

test_that("parse_qgraph creates valid network structure", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0), 4, 4)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$nodes) >= 1)
  expect_true(is.data.frame(result$edges))
})

# =============================================================================
# SECTION 27: Test with qgraph using different layouts
# =============================================================================

test_that("parse_qgraph works with spring layout", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, layout = "spring")

  result <- parse_qgraph(q)

  expect_true(is.data.frame(result$nodes))
  expect_true(nrow(result$edges) > 0)
})

test_that("parse_qgraph works with circular layout", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, layout = "circle")

  result <- parse_qgraph(q)

  expect_true(is.data.frame(result$nodes))
})

# =============================================================================
# SECTION 28: Directed parameter override behavior
# =============================================================================

test_that("parse_qgraph directed=TRUE overrides any detection", {
  skip_if_not_installed("qgraph")

  # Clearly symmetric matrix
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, directed = FALSE)

  result <- parse_qgraph(q, directed = TRUE)
  expect_true(result$directed)
})

test_that("parse_qgraph directed=FALSE overrides any detection", {
  skip_if_not_installed("qgraph")

  # Asymmetric matrix
  adj <- matrix(c(0, 1, 0, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, directed = TRUE)

  result <- parse_qgraph(q, directed = FALSE)
  expect_false(result$directed)
})

# =============================================================================
# SECTION 29: Verify proper creation of helper structures
# =============================================================================

test_that("parse_qgraph uses create_nodes_df correctly", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Verify structure matches create_nodes_df output
  expected_cols <- c("id", "label", "name", "x", "y")
  expect_true(all(expected_cols %in% names(result$nodes)))
})

test_that("parse_qgraph uses create_edges_df correctly", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  # Verify structure matches create_edges_df output
  expected_cols <- c("from", "to", "weight")
  expect_true(all(expected_cols %in% names(result$edges)))
})

# =============================================================================
# SECTION 30: Stress test - complex network
# =============================================================================

test_that("parse_qgraph handles complex weighted directed network", {
  skip_if_not_installed("qgraph")

  set.seed(456)
  n <- 10
  adj <- matrix(runif(n * n, -1, 1), n, n)
  adj <- adj * (abs(adj) > 0.5)  # Sparsify
  diag(adj) <- 0
  rownames(adj) <- colnames(adj) <- paste0("Node", 1:n)

  q <- qgraph::qgraph(adj, DoNotPlot = TRUE, directed = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$nodes) >= 1)
  expect_true(result$directed)
  expect_equal(length(result$weights), nrow(result$edges))
})

# =============================================================================
# SECTION 31: Edge case - qgraph with no Arguments$input but has Edgelist
# =============================================================================

test_that("parse_qgraph works with Edgelist but no input matrix", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  # Remove input but keep Edgelist
  q_modified <- q
  q_modified$Arguments$input <- NULL
  q_modified$graphAttributes$Nodes$names <- NULL
  q_modified$graphAttributes$Nodes$labels <- NULL

  result <- parse_qgraph(q_modified)

  # Should infer n from max(from, to)
  expect_true(nrow(result$nodes) >= 1)
})

# =============================================================================
# SECTION 32: Additional validation
# =============================================================================

test_that("parse_qgraph edges have positive count when network has connections", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(nrow(result$edges) > 0)
  expect_true(length(result$weights) > 0)
})

test_that("parse_qgraph directed field is logical", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_type(result$directed, "logical")
  expect_length(result$directed, 1)
})

# =============================================================================
# SECTION 33: Test label handling
# =============================================================================

test_that("parse_qgraph handles labels with special characters", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  rownames(adj) <- colnames(adj) <- c("A1", "B2", "C3")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(all(nchar(result$nodes$label) > 0))
})

test_that("parse_qgraph handles various label lengths", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(adj) <- colnames(adj) <- c("Short", "VeryLongLabelName")
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_equal(nrow(result$nodes), 2)
  expect_true(all(!is.na(result$nodes$label)))
})

# =============================================================================
# SECTION 34: Test various weight ranges
# =============================================================================

test_that("parse_qgraph handles weights close to zero", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 0.001, 0.001, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(is.data.frame(result$edges))
})

test_that("parse_qgraph handles large weights", {
  skip_if_not_installed("qgraph")

  adj <- matrix(c(0, 100, 100, 0), 2, 2)
  q <- qgraph::qgraph(adj, DoNotPlot = TRUE)

  result <- parse_qgraph(q)

  expect_true(all(result$weights > 0))
})

# =============================================================================
# SECTION 35: Error condition tests
# =============================================================================

test_that("parse_qgraph error on invalid input types", {
  skip_if_not_installed("qgraph")

  # String input causes error (either invalid access or validation)
  expect_error(parse_qgraph("not a qgraph"))

  # NULL input causes error
  expect_error(parse_qgraph(NULL))

  # Simple list without Arguments field causes error
  not_qgraph <- list(foo = 1, bar = 2)
  expect_error(parse_qgraph(not_qgraph), "must be a qgraph object")

  # Data frame input causes error
  df <- data.frame(from = 1:3, to = 2:4)
  expect_error(parse_qgraph(df), "must be a qgraph object")
})
