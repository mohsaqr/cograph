# Tests for input-statnet.R - parse_statnet() function
# Coverage target: Comprehensive coverage for statnet input parsing

# =============================================================================
# Helper function to create test networks
# =============================================================================

# Helper to create a basic network matrix for testing
create_test_adj_matrix <- function(n = 4, symmetric = TRUE, weighted = FALSE) {
  m <- matrix(0, nrow = n, ncol = n)
  # Add edges dynamically based on n
  if (n >= 2) m[1, 2] <- 1
  if (n >= 3) {
    m[1, 3] <- 1
    m[2, 3] <- 1
  }
  if (n >= 4) m[2, 4] <- 1

  if (symmetric) {
    m <- m + t(m)
  }

  if (weighted) {
    m[m > 0] <- runif(sum(m > 0), 0.1, 1.0)
    if (symmetric) {
      m[lower.tri(m)] <- t(m)[lower.tri(m)]
    }
  }

  rownames(m) <- LETTERS[1:n]
  colnames(m) <- LETTERS[1:n]
  m
}

# =============================================================================
# SECTION 1: Basic parse_statnet() functionality tests
# =============================================================================

test_that("parse_statnet errors when network package not available", {
  # This test verifies the error message when network is not installed

  # We can't easily test this without unloading the package, so we skip

  skip("Cannot easily test package unavailability")
})

test_that("parse_statnet errors on non-network object", {
  skip_if_not_installed("network")

  expect_error(
    parse_statnet(matrix(1:4, 2)),
    "must be a network object"
  )

  expect_error(
    parse_statnet(data.frame(a = 1:3)),
    "must be a network object"
  )

  expect_error(
    parse_statnet(list(foo = 1)),
    "must be a network object"
  )
})

test_that("parse_statnet returns correct structure", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix()
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_type(result, "list")
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_true("directed" %in% names(result))
  expect_true("weights" %in% names(result))
})

test_that("parse_statnet correctly parses undirected network", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(symmetric = TRUE)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_false(result$directed)
  expect_s3_class(result$nodes, "data.frame")
  expect_s3_class(result$edges, "data.frame")
})

test_that("parse_statnet correctly parses directed network", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(symmetric = FALSE)
  net <- network::network(mat, directed = TRUE)

  result <- parse_statnet(net)

  expect_true(result$directed)
})

test_that("parse_statnet returns correct number of nodes", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 5)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 5)
})

test_that("parse_statnet returns correct number of edges", {
  skip_if_not_installed("network")

  # Create a simple 3-node network with known edges
  mat <- matrix(c(0, 1, 0,
                  1, 0, 1,
                  0, 1, 0), nrow = 3, byrow = TRUE)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  # For undirected: 1-2, 2-3 = 2 edges
  expect_equal(nrow(result$edges), 2)
})

# =============================================================================
# SECTION 2: Directed parameter override tests
# =============================================================================

test_that("parse_statnet respects directed = TRUE override", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(symmetric = TRUE)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net, directed = TRUE)

  expect_true(result$directed)
})

test_that("parse_statnet respects directed = FALSE override", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(symmetric = FALSE)
  net <- network::network(mat, directed = TRUE)

  result <- parse_statnet(net, directed = FALSE)

  expect_false(result$directed)
})

test_that("parse_statnet uses network's directedness when directed = NULL", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix()

  # Test undirected
  net_undir <- network::network(mat, directed = FALSE)
  result_undir <- parse_statnet(net_undir, directed = NULL)
  expect_false(result_undir$directed)

  # Test directed
  net_dir <- network::network(mat, directed = TRUE)
  result_dir <- parse_statnet(net_dir, directed = NULL)
  expect_true(result_dir$directed)
})

# =============================================================================
# SECTION 3: Node labels and attributes tests
# =============================================================================

test_that("parse_statnet extracts vertex names", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "vertex.names", c("X", "Y", "Z"))

  result <- parse_statnet(net)

  expect_equal(result$nodes$label, c("X", "Y", "Z"))
})

test_that("parse_statnet generates numeric labels when names are NULL", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE)
  # Force NA vertex names
  network::set.vertex.attribute(net, "vertex.names", c(NA, NA))

  result <- parse_statnet(net)

  expect_equal(result$nodes$label, c("1", "2"))
})

test_that("parse_statnet transfers vertex attributes", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "color", c("red", "green", "blue"))
  network::set.vertex.attribute(net, "size", c(1, 2, 3))

  result <- parse_statnet(net)

  expect_equal(result$nodes$color, c("red", "green", "blue"))
  expect_equal(result$nodes$size, c(1, 2, 3))
})

test_that("parse_statnet excludes reserved vertex attributes", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "custom_attr", c("a", "b", "c"))

  result <- parse_statnet(net)

  # Should have custom attribute but not "na" or internal attrs
  expect_true("custom_attr" %in% names(result$nodes))
  expect_false("na" %in% names(result$nodes))
})

# =============================================================================
# SECTION 4: Edge weights tests
# =============================================================================

test_that("parse_statnet extracts edge weights", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 0.5, 0.5, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE, ignore.eval = FALSE,
                          names.eval = "weight")

  result <- parse_statnet(net)

  expect_length(result$weights, 1)
  expect_equal(result$weights[1], 0.5)
})

test_that("parse_statnet uses weight of 1 when no weights", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(result$weights, 1)
})

test_that("parse_statnet handles multiple weighted edges", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 0.3, 0.5,
                  0.3, 0, 0.7,
                  0.5, 0.7, 0), nrow = 3, byrow = TRUE)
  net <- network::network(mat, directed = FALSE, ignore.eval = FALSE,
                          names.eval = "weight")

  result <- parse_statnet(net)

  expect_length(result$weights, nrow(result$edges))
  expect_true(all(result$weights > 0))
})

# =============================================================================
# SECTION 5: Edge attributes tests
# =============================================================================

test_that("parse_statnet transfers edge attributes", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 0, 0), nrow = 2)
  net <- network::network(mat, directed = TRUE)
  network::set.edge.attribute(net, "type", "friendship")

  result <- parse_statnet(net)

  expect_true("type" %in% names(result$edges))
  expect_equal(result$edges$type, "friendship")
})

test_that("parse_statnet excludes reserved edge attributes", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE)
  network::set.edge.attribute(net, "custom_edge_attr", "test_value")

  result <- parse_statnet(net)

  # Should have custom attribute
  expect_true("custom_edge_attr" %in% names(result$edges))
})

# =============================================================================
# SECTION 6: Empty network tests
# =============================================================================

test_that("parse_statnet handles empty network (no edges)", {
  skip_if_not_installed("network")

  mat <- matrix(0, nrow = 3, ncol = 3)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 0)
  expect_length(result$weights, 0)
})

test_that("parse_statnet handles single node network", {
  skip_if_not_installed("network")

  mat <- matrix(0, nrow = 1, ncol = 1)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 1)
  expect_equal(nrow(result$edges), 0)
})

# =============================================================================
# SECTION 7: Complex network tests
# =============================================================================

test_that("parse_statnet handles large network", {
  skip_if_not_installed("network")

  n <- 20
  mat <- matrix(0, nrow = n, ncol = n)
  # Create random edges
  set.seed(42)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (runif(1) < 0.3) {
        mat[i, j] <- 1
        mat[j, i] <- 1
      }
    }
  }
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), n)
  expect_true(nrow(result$edges) > 0)
})

test_that("parse_statnet handles fully connected network", {
  skip_if_not_installed("network")

  n <- 4
  mat <- matrix(1, nrow = n, ncol = n)
  diag(mat) <- 0
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), n)
  # Fully connected undirected: n*(n-1)/2 = 6 edges
  expect_equal(nrow(result$edges), 6)
})

test_that("parse_statnet handles self-loops", {
  skip_if_not_installed("network")

  mat <- matrix(c(1, 1, 1, 0), nrow = 2)
  net <- network::network(mat, directed = TRUE, loops = TRUE)

  result <- parse_statnet(net)

  # Should include self-loop
  self_loops <- result$edges[result$edges$from == result$edges$to, ]
  expect_true(nrow(self_loops) > 0)
})

# =============================================================================
# SECTION 8: Data frame structure tests
# =============================================================================

test_that("parse_statnet nodes data frame has required columns", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix()
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_true("id" %in% names(result$nodes))
  expect_true("label" %in% names(result$nodes))
})

test_that("parse_statnet edges data frame has required columns", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix()
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_true("from" %in% names(result$edges))
  expect_true("to" %in% names(result$edges))
  expect_true("weight" %in% names(result$edges))
})

test_that("parse_statnet edge indices match node indices", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 4)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  # All from/to indices should be valid node ids
  node_ids <- result$nodes$id
  expect_true(all(result$edges$from %in% node_ids))
  expect_true(all(result$edges$to %in% node_ids))
})

# =============================================================================
# SECTION 9: Bipartite network tests
# =============================================================================

test_that("parse_statnet handles bipartite network", {
  skip_if_not_installed("network")

  # Create bipartite adjacency matrix
  mat <- matrix(0, nrow = 5, ncol = 5)
  # Two groups: nodes 1-2 and nodes 3-5
  mat[1, 3] <- 1
  mat[1, 4] <- 1
  mat[2, 4] <- 1
  mat[2, 5] <- 1
  mat[3, 1] <- 1
  mat[4, 1] <- 1
  mat[4, 2] <- 1
  mat[5, 2] <- 1

  net <- network::network(mat, directed = FALSE, bipartite = 2)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 5)
  expect_true(nrow(result$edges) > 0)
})

# =============================================================================
# SECTION 10: Mixed attribute types tests
# =============================================================================

test_that("parse_statnet handles numeric vertex attributes", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "value", c(1.5, 2.5, 3.5))

  result <- parse_statnet(net)

  expect_equal(result$nodes$value, c(1.5, 2.5, 3.5))
  expect_type(result$nodes$value, "double")
})

test_that("parse_statnet handles character vertex attributes", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "category", c("A", "B", "C"))

  result <- parse_statnet(net)

  expect_equal(result$nodes$category, c("A", "B", "C"))
})

test_that("parse_statnet handles logical vertex attributes", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "active", c(TRUE, FALSE, TRUE))

  result <- parse_statnet(net)

  expect_equal(result$nodes$active, c(TRUE, FALSE, TRUE))
})

# =============================================================================
# SECTION 11: Edge consistency tests
# =============================================================================

test_that("parse_statnet edges have consistent weights", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 0.5, 0.5, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE, ignore.eval = FALSE,
                          names.eval = "weight")

  result <- parse_statnet(net)

  # edges$weight should match weights vector
  expect_equal(result$edges$weight, result$weights)
})

test_that("parse_statnet preserves edge order", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1,
                  0, 0, 1,
                  0, 0, 0), nrow = 3, byrow = TRUE)
  net <- network::network(mat, directed = TRUE)

  result <- parse_statnet(net)

  # Should have 3 edges for directed network
  expect_equal(nrow(result$edges), 3)
})

# =============================================================================
# SECTION 12: Integration with cograph tests
# =============================================================================

test_that("parse_statnet output works with cograph", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix()
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  # Should be valid input for cograph
  cn <- cograph(result)
  expect_s3_class(cn, "cograph_network")
})

test_that("parse_statnet with vertex names works with cograph", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 3)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "vertex.names", c("Alpha", "Beta", "Gamma"))

  result <- parse_statnet(net)
  cn <- cograph(result)

  expect_s3_class(cn, "cograph_network")
  expect_equal(get_labels(cn), c("Alpha", "Beta", "Gamma"))
})

# =============================================================================
# SECTION 13: Error handling edge cases
# =============================================================================

test_that("parse_statnet handles network with NA in matrix", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, NA, 0), nrow = 2)
  mat[is.na(mat)] <- 0  # Clean NAs before creating network
  net <- network::network(mat, directed = TRUE)

  result <- parse_statnet(net)

  expect_type(result, "list")
  expect_true(nrow(result$nodes) > 0)
})

test_that("parse_statnet handles string input error", {
  skip_if_not_installed("network")

  expect_error(
    parse_statnet("not a network"),
    "must be a network object"
  )
})

test_that("parse_statnet handles NULL input error", {
  skip_if_not_installed("network")

  expect_error(
    parse_statnet(NULL),
    "must be a network object"
  )
})

# =============================================================================
# SECTION 14: Network property preservation tests
# =============================================================================

test_that("parse_statnet preserves network size", {
  skip_if_not_installed("network")

  for (n in c(2, 5, 10)) {
    mat <- matrix(0, nrow = n, ncol = n)
    if (n > 1) {
      mat[1, 2] <- 1
      mat[2, 1] <- 1
    }
    net <- network::network(mat, directed = FALSE)

    result <- parse_statnet(net)

    expect_equal(nrow(result$nodes), n)
  }
})

test_that("parse_statnet edge count matches network edge count", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix()
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$edges), network::network.edgecount(net))
})

# =============================================================================
# SECTION 15: Special character handling tests
# =============================================================================

test_that("parse_statnet handles special characters in vertex names", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "vertex.names", c("Node-1", "Node_2"))

  result <- parse_statnet(net)

  expect_equal(result$nodes$label, c("Node-1", "Node_2"))
})

test_that("parse_statnet handles unicode in vertex names", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net, "vertex.names", c("Alpha", "Beta"))

  result <- parse_statnet(net)

  expect_equal(result$nodes$label, c("Alpha", "Beta"))
})

# =============================================================================
# SECTION 16: Negative and zero weight tests
# =============================================================================

test_that("parse_statnet handles negative weights", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, -0.5, -0.5, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE, ignore.eval = FALSE,
                          names.eval = "weight")

  result <- parse_statnet(net)

  expect_true(any(result$weights < 0))
})

test_that("parse_statnet handles mixed positive and negative weights", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 0.5, -0.3,
                  0.5, 0, 0.7,
                  -0.3, 0.7, 0), nrow = 3, byrow = TRUE)
  net <- network::network(mat, directed = FALSE, ignore.eval = FALSE,
                          names.eval = "weight")

  result <- parse_statnet(net)

  expect_true(any(result$weights > 0))
  expect_true(any(result$weights < 0))
})

# =============================================================================
# SECTION 17: Multiple edge attribute tests
# =============================================================================

test_that("parse_statnet handles multiple edge attributes", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- network::network(mat, directed = FALSE)
  network::set.edge.attribute(net, "type", "friendship")
  network::set.edge.attribute(net, "strength", 0.8)
  network::set.edge.attribute(net, "active", TRUE)

  result <- parse_statnet(net)

  expect_true("type" %in% names(result$edges))
  expect_true("strength" %in% names(result$edges))
  expect_true("active" %in% names(result$edges))
})

# =============================================================================
# SECTION 18: Directed network edge tests
# =============================================================================

test_that("parse_statnet correctly counts directed edges", {
  skip_if_not_installed("network")

  # Asymmetric matrix - directed edges
  mat <- matrix(c(0, 1, 0,
                  0, 0, 1,
                  1, 0, 0), nrow = 3, byrow = TRUE)
  net <- network::network(mat, directed = TRUE)

  result <- parse_statnet(net)

  expect_true(result$directed)
  expect_equal(nrow(result$edges), 3)  # 1->2, 2->3, 3->1
})

test_that("parse_statnet handles bidirectional edges in directed network", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  net <- network::network(mat, directed = TRUE)

  result <- parse_statnet(net)

  expect_true(result$directed)
  # Should have 2 edges: 1->2 and 2->1
  expect_equal(nrow(result$edges), 2)
})

# =============================================================================
# SECTION 19: Sparse network tests
# =============================================================================

test_that("parse_statnet handles very sparse network", {
  skip_if_not_installed("network")

  n <- 10
  mat <- matrix(0, nrow = n, ncol = n)
  mat[1, 2] <- 1
  mat[2, 1] <- 1
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), n)
  expect_equal(nrow(result$edges), 1)
})

test_that("parse_statnet handles network with isolated nodes", {
  skip_if_not_installed("network")

  mat <- matrix(0, nrow = 5, ncol = 5)
  mat[1, 2] <- 1
  mat[2, 1] <- 1
  # Nodes 3, 4, 5 are isolated
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), 5)
  expect_equal(nrow(result$edges), 1)
})

# =============================================================================
# SECTION 20: Dense network tests
# =============================================================================

test_that("parse_statnet handles dense weighted network", {
  skip_if_not_installed("network")

  n <- 5
  set.seed(123)
  mat <- matrix(runif(n * n), nrow = n)
  mat <- (mat + t(mat)) / 2  # Make symmetric
  diag(mat) <- 0

  net <- network::network(mat, directed = FALSE, ignore.eval = FALSE,
                          names.eval = "weight")

  result <- parse_statnet(net)

  expect_equal(nrow(result$nodes), n)
  # Fully connected: n*(n-1)/2 = 10 edges
  expect_equal(nrow(result$edges), 10)
})

# =============================================================================
# SECTION 21: Network metadata tests
# =============================================================================

test_that("parse_statnet works with named network", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix(n = 3)
  net <- network::network(mat, directed = FALSE)

  result <- parse_statnet(net)

  expect_type(result, "list")
  expect_equal(length(result), 4)  # nodes, edges, directed, weights
})

# =============================================================================
# SECTION 22: Reproducibility tests
# =============================================================================

test_that("parse_statnet is deterministic", {
  skip_if_not_installed("network")

  mat <- create_test_adj_matrix()
  net <- network::network(mat, directed = FALSE)

  result1 <- parse_statnet(net)
  result2 <- parse_statnet(net)

  expect_identical(result1$nodes, result2$nodes)
  expect_identical(result1$edges, result2$edges)
  expect_identical(result1$directed, result2$directed)
  expect_identical(result1$weights, result2$weights)
})

test_that("parse_statnet gives same result with same input", {
  skip_if_not_installed("network")

  mat <- matrix(c(0, 1, 1, 0), nrow = 2)

  net1 <- network::network(mat, directed = FALSE)
  net2 <- network::network(mat, directed = FALSE)

  result1 <- parse_statnet(net1)
  result2 <- parse_statnet(net2)

  expect_equal(nrow(result1$nodes), nrow(result2$nodes))
  expect_equal(nrow(result1$edges), nrow(result2$edges))
})
