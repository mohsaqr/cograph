# Comprehensive coverage tests for input-tna.R
# Tests uncovered code paths: error handling, edge cases, group_tna parsing

# =============================================================================
# parse_tna() Error Handling Tests
# =============================================================================

test_that("parse_tna errors on non-tna object", {
  # Regular matrix should error
  mat <- matrix(1:9, 3, 3)
  expect_error(parse_tna(mat), "Input must be a tna object")

  # Data frame should error
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(parse_tna(df), "Input must be a tna object")

  # List without tna class should error
  fake_list <- list(weights = matrix(1:9, 3, 3))
  expect_error(parse_tna(fake_list), "Input must be a tna object")

  # NULL should error
  expect_error(parse_tna(NULL), "Input must be a tna object")
})

test_that("parse_tna reads directed from tna_obj$directed field", {
  skip_if_not_installed("tna")
  library(tna)

  # Create a tna object
  model <- tna(group_regulation)

  # Manually set $directed field to FALSE
  model$directed <- FALSE
  parsed <- parse_tna(model)
  expect_false(parsed$directed)

  # Manually set $directed field to TRUE
  model$directed <- TRUE
  parsed <- parse_tna(model)
  expect_true(parsed$directed)
})

test_that("parse_tna reads directed from attr(tna_obj, 'directed')", {
  skip_if_not_installed("tna")
  library(tna)

  # Create a tna object
  model <- tna(group_regulation)

  # Remove $directed field, set attribute instead
  model$directed <- NULL
  attr(model, "directed") <- FALSE
  parsed <- parse_tna(model)
  expect_false(parsed$directed)

  # Set attribute to TRUE
  attr(model, "directed") <- TRUE
  parsed <- parse_tna(model)
  expect_true(parsed$directed)
})

test_that("parse_tna defaults to directed=TRUE when no directed info", {
  skip_if_not_installed("tna")
  library(tna)

  # Create a tna object
  model <- tna(group_regulation)

  # Remove all directed indicators
  model$directed <- NULL
  attr(model, "directed") <- NULL

  parsed <- parse_tna(model)
  expect_true(parsed$directed)
})

test_that("parse_tna respects explicit directed parameter", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)

  # Explicit FALSE overrides default

  parsed <- parse_tna(model, directed = FALSE)
  expect_false(parsed$directed)

  # Explicit TRUE
  parsed <- parse_tna(model, directed = TRUE)
  expect_true(parsed$directed)
})

test_that("parse_tna handles NULL labels", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)
  n <- nrow(model$weights)

  # Remove labels
  model$labels <- NULL
  parsed <- parse_tna(model)

  # Should generate numeric labels
  expect_equal(parsed$nodes$label, as.character(seq_len(n)))
})

test_that("parse_tna handles all NA labels", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)
  n <- nrow(model$weights)

  # Set all labels to NA
  model$labels <- rep(NA, n)
  parsed <- parse_tna(model)

  # Should generate numeric labels
  expect_equal(parsed$nodes$label, as.character(seq_len(n)))
})

test_that("parse_tna handles empty network (all zero weights)", {
  skip_if_not_installed("tna")
  library(tna)

  # Create a mock tna object with all zero weights
  fake_tna <- list(
    weights = matrix(0, 3, 3),
    labels = c("A", "B", "C"),
    inits = c(0.3, 0.3, 0.4)
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Should have nodes but no edges
  expect_equal(nrow(parsed$nodes), 3)
  expect_equal(nrow(parsed$edges), 0)
})

test_that("parse_tna handles tna without inits", {
  skip_if_not_installed("tna")
  library(tna)

  # Create a mock tna object without inits
  fake_tna <- list(
    weights = matrix(c(0, 0.5, 0.3, 0, 0, 0.2, 0.7, 0, 0), 3, 3),
    labels = c("A", "B", "C")
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Should have nodes without inits column
  expect_false("inits" %in% names(parsed$nodes))
})

test_that("parse_tna handles tna with inits", {
  skip_if_not_installed("tna")
  library(tna)

  # Create mock tna with inits
  fake_tna <- list(
    weights = matrix(c(0, 0.5, 0.3, 0, 0, 0.2, 0.7, 0, 0), 3, 3),
    labels = c("A", "B", "C"),
    inits = c(0.4, 0.35, 0.25)
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Should have inits in nodes
  expect_true("inits" %in% names(parsed$nodes))
  expect_equal(parsed$nodes$inits, c(0.4, 0.35, 0.25))
})

test_that("parse_tna handles tna with data containing colors", {
  skip_if_not_installed("tna")
  library(tna)

  # Create mock tna with data containing colors attribute
  fake_data <- data.frame(state = c("A", "B", "C"))
  attr(fake_data, "colors") <- c("#FF0000", "#00FF00", "#0000FF")

  fake_tna <- list(
    weights = matrix(c(0, 0.5, 0.3, 0, 0, 0.2, 0.7, 0, 0), 3, 3),
    labels = c("A", "B", "C"),
    data = fake_data
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Should extract colors to nodes
  expect_true("color" %in% names(parsed$nodes))
  expect_equal(parsed$nodes$color, c("#FF0000", "#00FF00", "#0000FF"))
})

test_that("parse_tna handles tna with data but wrong color count", {
  skip_if_not_installed("tna")
  library(tna)

  # Create mock tna with mismatched color count
  fake_data <- data.frame(state = c("A", "B", "C"))
  attr(fake_data, "colors") <- c("#FF0000", "#00FF00")  # Only 2 colors for 3 nodes

  fake_tna <- list(
    weights = matrix(c(0, 0.5, 0.3, 0, 0, 0.2, 0.7, 0, 0), 3, 3),
    labels = c("A", "B", "C"),
    data = fake_data
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Should NOT extract colors when count doesn't match
  expect_false("color" %in% names(parsed$nodes))
})

test_that("parse_tna handles tna with data but no colors attribute", {
  skip_if_not_installed("tna")
  library(tna)

  fake_data <- data.frame(state = c("A", "B", "C"))
  # No colors attribute set

  fake_tna <- list(
    weights = matrix(c(0, 0.5, 0.3, 0, 0, 0.2, 0.7, 0, 0), 3, 3),
    labels = c("A", "B", "C"),
    data = fake_data
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Should NOT have color column
  expect_false("color" %in% names(parsed$nodes))
})

test_that("parse_tna returns correct tna metadata structure", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)
  parsed <- parse_tna(model)

  expect_true(is.list(parsed$tna))
  expect_equal(parsed$tna$type, "tna")
  expect_null(parsed$tna$group_index)
  expect_null(parsed$tna$group_name)
})

# =============================================================================
# parse_group_tna() Tests
# =============================================================================

test_that("parse_group_tna errors on non-group_tna object", {
  # Regular list should error
  fake_list <- list(a = 1, b = 2)
  expect_error(parse_group_tna(fake_list), "Input must be a group_tna object")

  # tna object (not group_tna) should error
  skip_if_not_installed("tna")
  library(tna)
  model <- tna(group_regulation)
  expect_error(parse_group_tna(model), "Input must be a group_tna object")
})

test_that("parse_group_tna errors on index out of bounds", {
  skip_if_not_installed("tna")
  library(tna)

  # Create group_tna with 2 groups
  data(engagement, package = "tna")
  groups <- sample(c("A", "B"), nrow(engagement), replace = TRUE)
  group_model <- group_tna(engagement, group = groups)

  n_groups <- length(group_model)

  # Index 0 should error
  expect_error(parse_group_tna(group_model, i = 0),
               "Index i must be between 1 and")

  # Index beyond n_groups should error
  expect_error(parse_group_tna(group_model, i = n_groups + 1),
               "Index i must be between 1 and")

  # Negative index should error
  expect_error(parse_group_tna(group_model, i = -1),
               "Index i must be between 1 and")
})

test_that("parse_group_tna extracts correct group", {
  skip_if_not_installed("tna")
  library(tna)

  data(engagement, package = "tna")
  groups <- sample(c("GroupA", "GroupB"), nrow(engagement), replace = TRUE)
  group_model <- group_tna(engagement, group = groups)

  # Parse first group
  parsed1 <- parse_group_tna(group_model, i = 1)
  expect_equal(parsed1$tna$group_index, 1)
  expect_equal(parsed1$tna$group_name, names(group_model)[1])
  expect_equal(parsed1$tna$type, "group_tna")

  # Parse second group
  parsed2 <- parse_group_tna(group_model, i = 2)
  expect_equal(parsed2$tna$group_index, 2)
  expect_equal(parsed2$tna$group_name, names(group_model)[2])
})

test_that("parse_group_tna respects directed parameter", {
  skip_if_not_installed("tna")
  library(tna)

  data(engagement, package = "tna")
  groups <- sample(c("A", "B"), nrow(engagement), replace = TRUE)
  group_model <- group_tna(engagement, group = groups)

  # Explicit directed = FALSE
  parsed <- parse_group_tna(group_model, i = 1, directed = FALSE)
  expect_false(parsed$directed)

  # Explicit directed = TRUE
  parsed <- parse_group_tna(group_model, i = 1, directed = TRUE)
  expect_true(parsed$directed)
})

test_that("parse_group_tna preserves weights matrix", {
  skip_if_not_installed("tna")
  library(tna)

  data(engagement, package = "tna")
  groups <- sample(c("A", "B"), nrow(engagement), replace = TRUE)
  group_model <- group_tna(engagement, group = groups)

  parsed <- parse_group_tna(group_model, i = 1)

  # Weights matrix should match the group's weights
  expect_equal(parsed$weights_matrix, group_model[[1]]$weights)
})

# =============================================================================
# is_tna_network() Edge Cases
# =============================================================================

test_that("is_tna_network returns FALSE for NULL input", {
  expect_false(is_tna_network(NULL))
})

test_that("is_tna_network returns FALSE for non-network objects", {
  expect_false(is_tna_network("string"))
  expect_false(is_tna_network(123))
  expect_false(is_tna_network(list(a = 1)))
  expect_false(is_tna_network(data.frame(x = 1:3)))
})

test_that("is_tna_network returns FALSE for cograph_network without tna field", {
  mat <- matrix(runif(16), 4, 4)
  net <- as_cograph(mat)

  # Verify it's FALSE (matrix-based network has no tna field)
  expect_false(is_tna_network(net))
})

test_that("is_tna_network returns FALSE for cograph_network with NULL tna$type", {
  mat <- matrix(runif(16), 4, 4)
  net <- as_cograph(mat)

  # Manually add tna field with NULL type
  net$tna <- list(type = NULL)
  expect_false(is_tna_network(net))
})

test_that("is_tna_network works with CographNetwork R6 class", {
  # Create R6 CographNetwork
  mat <- matrix(runif(9), 3, 3)
  net <- CographNetwork$new(mat)

  # Without tna field, should return FALSE
  expect_false(is_tna_network(net))
})

test_that("is_tna_network returns TRUE for properly structured tna network", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)
  net <- as_cograph(model)

  expect_true(is_tna_network(net))
  expect_equal(net$tna$type, "tna")
})

test_that("is_tna_network returns TRUE for group_tna network", {
  skip_if_not_installed("tna")
  library(tna)

  data(engagement, package = "tna")
  groups <- sample(c("A", "B"), nrow(engagement), replace = TRUE)
  group_model <- group_tna(engagement, group = groups)

  # Parse first group
  parsed <- parse_group_tna(group_model, i = 1)

  # Create cograph_network manually
  net <- .create_cograph_network(
    nodes = parsed$nodes,
    edges = parsed$edges,
    directed = parsed$directed,
    source = "group_tna",
    tna = parsed$tna,
    weights = parsed$weights_matrix
  )

  expect_true(is_tna_network(net))
  expect_equal(net$tna$type, "group_tna")
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("parse_tna creates valid nodes data frame", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)
  parsed <- parse_tna(model)

  nodes <- parsed$nodes
  expect_true(is.data.frame(nodes))
  expect_true("id" %in% names(nodes))
  expect_true("label" %in% names(nodes))
  expect_true("x" %in% names(nodes))
  expect_true("y" %in% names(nodes))
})

test_that("parse_tna creates valid edges data frame", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)
  parsed <- parse_tna(model)

  edges <- parsed$edges
  expect_true(is.data.frame(edges))
  expect_true("from" %in% names(edges))
  expect_true("to" %in% names(edges))
  expect_true("weight" %in% names(edges))
})

test_that("parse_tna extracts edges correctly from weight matrix", {
  skip_if_not_installed("tna")
  library(tna)

  # Create known weight matrix
  weights <- matrix(c(
    0, 0.5, 0,
    0.3, 0, 0.2,
    0.4, 0, 0
  ), 3, 3, byrow = TRUE)

  fake_tna <- list(
    weights = weights,
    labels = c("A", "B", "C")
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Should have exactly 4 edges (non-zero entries)
  expect_equal(nrow(parsed$edges), 4)

  # Check specific edges exist
  edges <- parsed$edges
  # Edge from 1 to 2 with weight 0.5
  expect_true(any(edges$from == 1 & edges$to == 2 & edges$weight == 0.5))
  # Edge from 2 to 1 with weight 0.3
  expect_true(any(edges$from == 2 & edges$to == 1 & edges$weight == 0.3))
})

test_that("parse_tna preserves full weight matrix", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)
  parsed <- parse_tna(model)

  expect_equal(parsed$weights_matrix, model$weights)
})

test_that("tna network round-trip through as_cograph preserves structure", {
  skip_if_not_installed("tna")
  library(tna)

  model <- tna(group_regulation)
  net <- as_cograph(model)

  # Check all components are preserved
  expect_equal(n_nodes(net), nrow(model$weights))
  expect_equal(net$weights, model$weights)
  expect_equal(net$source, "tna")
  expect_true(is_tna_network(net))
})

test_that("group_tna network includes group metadata", {
  skip_if_not_installed("tna")
  library(tna)

  data(engagement, package = "tna")
  groups <- sample(c("First", "Second"), nrow(engagement), replace = TRUE)
  group_model <- group_tna(engagement, group = groups)

  parsed <- parse_group_tna(group_model, i = 1)

  expect_equal(parsed$tna$type, "group_tna")
  expect_equal(parsed$tna$group_index, 1)
  expect_equal(parsed$tna$group_name, names(group_model)[1])
})

# =============================================================================
# Edge Cases and Boundary Conditions
# =============================================================================

test_that("parse_tna handles single-node network", {
  skip_if_not_installed("tna")

  # Single node network
  fake_tna <- list(
    weights = matrix(0, 1, 1),
    labels = "A"
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  expect_equal(nrow(parsed$nodes), 1)
  expect_equal(nrow(parsed$edges), 0)
  expect_equal(parsed$nodes$label, "A")
})

test_that("parse_tna handles self-loops", {
  skip_if_not_installed("tna")

  # Network with self-loop
  weights <- matrix(c(
    0.5, 0.3,
    0.2, 0.4
  ), 2, 2, byrow = TRUE)

  fake_tna <- list(
    weights = weights,
    labels = c("A", "B")
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Self-loops should be included (diagonal elements)
  edges <- parsed$edges
  expect_true(any(edges$from == 1 & edges$to == 1))  # Self-loop A->A
  expect_true(any(edges$from == 2 & edges$to == 2))  # Self-loop B->B
})

test_that("parse_tna handles negative weights", {
  skip_if_not_installed("tna")

  # Network with negative weights
  weights <- matrix(c(
    0, 0.5, -0.3,
    -0.2, 0, 0.4,
    0.1, -0.6, 0
  ), 3, 3, byrow = TRUE)

  fake_tna <- list(
    weights = weights,
    labels = c("A", "B", "C")
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # All non-zero weights should be extracted
  expect_equal(nrow(parsed$edges), 6)

  # Check negative weights are preserved
  edges <- parsed$edges
  expect_true(any(edges$weight < 0))
})

test_that("parse_tna handles very small weights", {
  skip_if_not_installed("tna")

  # Network with tiny weights
  weights <- matrix(c(
    0, 1e-10, 0,
    0, 0, 1e-15,
    0, 0, 0
  ), 3, 3, byrow = TRUE)

  fake_tna <- list(
    weights = weights,
    labels = c("A", "B", "C")
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Tiny non-zero weights should be extracted
  expect_equal(nrow(parsed$edges), 2)
})

test_that("parse_tna correctly creates directed edges", {
  skip_if_not_installed("tna")

  # Asymmetric weight matrix
  weights <- matrix(c(
    0, 0.5, 0,
    0, 0, 0.3,
    0.4, 0, 0
  ), 3, 3, byrow = TRUE)

  fake_tna <- list(
    weights = weights,
    labels = c("A", "B", "C")
  )
  class(fake_tna) <- c("tna", "list")

  parsed <- parse_tna(fake_tna)

  # Should be directed by default
  expect_true(parsed$directed)

  # Check edge directions
  edges <- parsed$edges
  # 1->2 exists but not 2->1
  expect_true(any(edges$from == 1 & edges$to == 2))
  expect_false(any(edges$from == 2 & edges$to == 1))
})
