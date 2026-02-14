# Tests for input-parse.R and related input parsing functions
# Coverage target: 40% improvement for input-parse.R

# =============================================================================
# parse_input() tests - Main dispatch function
# =============================================================================

test_that("parse_input dispatches to parse_matrix for matrix input", {
  m <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  result <- parse_input(m)

  expect_type(result, "list")
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
  expect_true("directed" %in% names(result))
})

test_that("parse_input dispatches to parse_edgelist for data.frame input", {
  df <- data.frame(from = c("A", "B"), to = c("B", "C"), weight = c(0.5, 0.8))
  result <- parse_input(df)

  expect_type(result, "list")
  expect_true("nodes" %in% names(result))
  expect_true("edges" %in% names(result))
})

test_that("parse_input returns already parsed format unchanged", {
  already_parsed <- list(
    nodes = data.frame(id = 1:3, label = c("A", "B", "C"), stringsAsFactors = FALSE),
    edges = data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8), stringsAsFactors = FALSE),
    directed = FALSE

  )
  result <- parse_input(already_parsed)

  expect_identical(result, already_parsed)
})

test_that("parse_input errors on unsupported input type", {
  expect_error(
    parse_input("not a valid input"),
    "Unsupported input type"
  )
  expect_error(
    parse_input(123),
    "Unsupported input type"
  )
  expect_error(
    parse_input(list(foo = 1, bar = 2)),
    "Unsupported input type"
  )
})

test_that("parse_input passes directed parameter correctly", {
  # Symmetric matrix but force directed = TRUE
  m <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  result <- parse_input(m, directed = TRUE)
  expect_true(result$directed)

  # Same matrix with directed = FALSE
  result2 <- parse_input(m, directed = FALSE)
  expect_false(result2$directed)
})

# =============================================================================
# is_symmetric_matrix() tests
# =============================================================================

test_that("is_symmetric_matrix returns TRUE for symmetric matrix", {
  m <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3)
  expect_true(is_symmetric_matrix(m))
})
test_that("is_symmetric_matrix returns FALSE for asymmetric matrix", {
  m <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3)
  expect_false(is_symmetric_matrix(m))
})

test_that("is_symmetric_matrix returns FALSE for non-square matrix", {
  m <- matrix(1:6, nrow = 2, ncol = 3)
  expect_false(is_symmetric_matrix(m))
})

test_that("is_symmetric_matrix returns FALSE for non-matrix input", {
  expect_false(is_symmetric_matrix(c(1, 2, 3)))
  expect_false(is_symmetric_matrix(data.frame(a = 1:3)))
  expect_false(is_symmetric_matrix(NULL))
})

test_that("is_symmetric_matrix handles numeric tolerance", {
  m <- matrix(c(0, 1, 1 + 1e-10, 1, 0, 1, 1, 1, 0), nrow = 3)
  # With default tolerance, should be symmetric

expect_true(is_symmetric_matrix(m))

  # With very strict tolerance, should be asymmetric
  expect_false(is_symmetric_matrix(m, tol = 1e-15))
})

# =============================================================================
# create_nodes_df() tests
# =============================================================================

test_that("create_nodes_df creates correct structure", {
  result <- create_nodes_df(5)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_true(all(c("id", "label", "name", "x", "y") %in% names(result)))
  expect_equal(result$id, 1:5)
  expect_equal(result$label, as.character(1:5))
})

test_that("create_nodes_df uses provided labels", {
  result <- create_nodes_df(3, labels = c("A", "B", "C"))

  expect_equal(result$label, c("A", "B", "C"))
  expect_equal(result$name, c("A", "B", "C"))
})

test_that("create_nodes_df uses provided names", {
  result <- create_nodes_df(3, labels = c("A", "B", "C"), names = c("Alpha", "Beta", "Gamma"))

  expect_equal(result$label, c("A", "B", "C"))
  expect_equal(result$name, c("Alpha", "Beta", "Gamma"))
})

test_that("create_nodes_df initializes x and y as NA", {
  result <- create_nodes_df(3)

  expect_true(all(is.na(result$x)))
  expect_true(all(is.na(result$y)))
})

test_that("create_nodes_df handles single node", {
  result <- create_nodes_df(1)

  expect_equal(nrow(result), 1)
  expect_equal(result$id, 1)
  expect_equal(result$label, "1")
})

# =============================================================================
# create_edges_df() tests
# =============================================================================

test_that("create_edges_df creates correct structure", {
  result <- create_edges_df(from = c(1, 2, 3), to = c(2, 3, 1))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("from", "to", "weight") %in% names(result)))
})

test_that("create_edges_df uses default weight of 1", {
  result <- create_edges_df(from = c(1, 2), to = c(2, 3))

  expect_equal(result$weight, c(1, 1))
})

test_that("create_edges_df uses provided weights", {
  result <- create_edges_df(from = c(1, 2), to = c(2, 3), weight = c(0.5, 1.5))

  expect_equal(result$weight, c(0.5, 1.5))
})

test_that("create_edges_df handles empty edges", {
  result <- create_edges_df(from = integer(0), to = integer(0))

  expect_equal(nrow(result), 0)
  expect_true(all(c("from", "to", "weight") %in% names(result)))
})

# =============================================================================
# detect_duplicate_edges() tests
# =============================================================================

test_that("detect_duplicate_edges returns FALSE for no duplicates", {
  edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4), weight = c(0.5, 0.6, 0.7))
  result <- detect_duplicate_edges(edges)

  expect_false(result$has_duplicates)
  expect_null(result$info)
})

test_that("detect_duplicate_edges detects simple duplicates", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(0.5, 0.3, 0.7))
  result <- detect_duplicate_edges(edges)

  expect_true(result$has_duplicates)
  expect_length(result$info, 1)
  expect_equal(result$info[[1]]$count, 2)
  expect_equal(result$info[[1]]$weights, c(0.5, 0.3))
})

test_that("detect_duplicate_edges detects reversed duplicates in undirected mode", {
  edges <- data.frame(from = c(1, 2, 2), to = c(2, 1, 3), weight = c(0.5, 0.3, 0.7))
  result <- detect_duplicate_edges(edges)

  expect_true(result$has_duplicates)
  expect_length(result$info, 1)
  expect_equal(result$info[[1]]$nodes, c(1, 2))
  expect_equal(result$info[[1]]$count, 2)
})

test_that("detect_duplicate_edges handles NULL edges", {
  result <- detect_duplicate_edges(NULL)

  expect_false(result$has_duplicates)
  expect_null(result$info)
})

test_that("detect_duplicate_edges handles empty data frame", {
  edges <- data.frame(from = integer(0), to = integer(0))
  result <- detect_duplicate_edges(edges)

  expect_false(result$has_duplicates)
  expect_null(result$info)
})

test_that("detect_duplicate_edges handles edges without weight column", {
  edges <- data.frame(from = c(1, 1), to = c(2, 2))
  result <- detect_duplicate_edges(edges)

  expect_true(result$has_duplicates)
  expect_equal(result$info[[1]]$weights, c(1, 1))
})

test_that("detect_duplicate_edges handles multiple duplicate groups", {
  edges <- data.frame(
    from = c(1, 1, 2, 2, 3),
    to = c(2, 2, 3, 3, 4),
    weight = c(0.1, 0.2, 0.3, 0.4, 0.5)
  )
  result <- detect_duplicate_edges(edges)

  expect_true(result$has_duplicates)
  expect_length(result$info, 2)
})

# =============================================================================
# aggregate_duplicate_edges() tests
# =============================================================================

test_that("aggregate_duplicate_edges with method='sum'", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(0.5, 0.3, 0.7))
  result <- aggregate_duplicate_edges(edges, method = "sum")

  expect_equal(nrow(result), 2)
  expect_equal(result$weight[result$from == 1 & result$to == 2], 0.8)
})

test_that("aggregate_duplicate_edges with method='mean'", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(0.5, 0.3, 0.7))
  result <- aggregate_duplicate_edges(edges, method = "mean")

  expect_equal(result$weight[result$from == 1 & result$to == 2], 0.4)
})

test_that("aggregate_duplicate_edges with method='max'", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(0.5, 0.3, 0.7))
  result <- aggregate_duplicate_edges(edges, method = "max")

  expect_equal(result$weight[result$from == 1 & result$to == 2], 0.5)
})

test_that("aggregate_duplicate_edges with method='min'", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(0.5, 0.3, 0.7))
  result <- aggregate_duplicate_edges(edges, method = "min")

  expect_equal(result$weight[result$from == 1 & result$to == 2], 0.3)
})

test_that("aggregate_duplicate_edges with method='first'", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(0.5, 0.3, 0.7))
  result <- aggregate_duplicate_edges(edges, method = "first")

  expect_equal(result$weight[result$from == 1 & result$to == 2], 0.5)
})

test_that("aggregate_duplicate_edges with custom function", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3), weight = c(3, 4, 5))
  result <- aggregate_duplicate_edges(edges, method = function(x) sqrt(sum(x^2)))

  expect_equal(result$weight[result$from == 1 & result$to == 2], 5)  # sqrt(9+16) = 5
})

test_that("aggregate_duplicate_edges errors on unknown method", {
  edges <- data.frame(from = c(1, 1), to = c(2, 2), weight = c(0.5, 0.3))

  expect_error(
    aggregate_duplicate_edges(edges, method = "unknown"),
    "Unknown aggregation method"
  )
})

test_that("aggregate_duplicate_edges handles NULL edges", {
  result <- aggregate_duplicate_edges(NULL)
  expect_null(result)
})

test_that("aggregate_duplicate_edges handles empty data frame", {
  edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  result <- aggregate_duplicate_edges(edges)

  expect_equal(nrow(result), 0)
})

test_that("aggregate_duplicate_edges handles reversed duplicates", {
  edges <- data.frame(from = c(1, 2), to = c(2, 1), weight = c(0.5, 0.3))
  result <- aggregate_duplicate_edges(edges, method = "sum")

  expect_equal(nrow(result), 1)
  expect_equal(result$weight, 0.8)
  # Should use canonical order (1 -> 2)
  expect_equal(result$from, 1)
  expect_equal(result$to, 2)
})

# =============================================================================
# parse_matrix() tests - Extended coverage
# =============================================================================

test_that("parse_matrix errors on non-matrix input", {
  expect_error(parse_matrix(data.frame(a = 1:3)), "must be a matrix")
})

test_that("parse_matrix errors on non-numeric matrix", {
  m <- matrix(c("a", "b", "c", "d"), nrow = 2)
  expect_error(parse_matrix(m), "must be numeric")
})

test_that("parse_matrix uses colnames when rownames are NULL", {
  m <- matrix(c(0, 1, 1, 0), nrow = 2)
  colnames(m) <- c("X", "Y")
  result <- parse_matrix(m)

  expect_equal(result$nodes$label, c("X", "Y"))
})

test_that("parse_matrix generates labels when no dimnames", {
  m <- matrix(c(0, 1, 1, 0), nrow = 2)
  result <- parse_matrix(m)

  expect_equal(result$nodes$label, c("1", "2"))
})

test_that("parse_matrix correctly extracts directed edges", {
  m <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3, byrow = TRUE)
  result <- parse_matrix(m, directed = TRUE)

  expect_true(result$directed)
  # All non-zero entries should be edges
  expect_equal(nrow(result$edges), 2)
})

test_that("parse_matrix correctly extracts undirected edges from upper triangle", {
  m <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  result <- parse_matrix(m, directed = FALSE)

  expect_false(result$directed)
  # Only upper triangle
  expect_equal(nrow(result$edges), 3)
})

test_that("parse_matrix handles weighted matrix", {
  m <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
  result <- parse_matrix(m)

  expect_true(all(result$edges$weight %in% c(0.3, 0.5, 0.8)))
})

test_that("parse_matrix handles sparse matrix", {
  m <- matrix(0, nrow = 5, ncol = 5)
  m[1, 2] <- 1
  m[2, 1] <- 1
  result <- parse_matrix(m)

  expect_equal(nrow(result$nodes), 5)
  expect_equal(nrow(result$edges), 1)  # Only upper triangle for symmetric
})

test_that("parse_matrix handles single node network", {
  m <- matrix(0, nrow = 1, ncol = 1)
  result <- parse_matrix(m)

  expect_equal(nrow(result$nodes), 1)
  expect_equal(nrow(result$edges), 0)
})

test_that("parse_matrix handles self-loops in directed network", {
  m <- matrix(c(0.5, 1, 0, 0.5), nrow = 2)
  result <- parse_matrix(m, directed = TRUE)

  # Self-loops on diagonal
  self_loops <- result$edges[result$edges$from == result$edges$to, ]
  expect_true(nrow(self_loops) > 0)
})

# =============================================================================
# parse_edgelist() tests - Extended coverage
# =============================================================================

test_that("parse_edgelist errors on non-data.frame input", {
  expect_error(parse_edgelist(matrix(1:4, 2)), "must be a data frame")
})

test_that("parse_edgelist auto-detects column names: source/target", {
  df <- data.frame(source = c("A", "B"), target = c("B", "C"))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$edges), 2)
})

test_that("parse_edgelist auto-detects column names: src/tgt", {
  df <- data.frame(src = c("A", "B"), tgt = c("B", "C"))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$edges), 2)
})

test_that("parse_edgelist auto-detects column names: v1/v2", {
  df <- data.frame(v1 = c("A", "B"), v2 = c("B", "C"))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$edges), 2)
})

test_that("parse_edgelist auto-detects column names: node1/node2", {
  df <- data.frame(node1 = c("A", "B"), node2 = c("B", "C"))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$edges), 2)
})

test_that("parse_edgelist auto-detects column names: i/j", {
  df <- data.frame(i = c("A", "B"), j = c("B", "C"))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$edges), 2)
})

test_that("parse_edgelist falls back to columns 1 and 2", {
  df <- data.frame(col1 = c("A", "B"), col2 = c("B", "C"))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$edges), 2)
})

test_that("parse_edgelist auto-detects weight column: w", {
  df <- data.frame(from = c("A", "B"), to = c("B", "C"), w = c(0.5, 0.8))
  result <- parse_edgelist(df)

  expect_equal(result$weights, c(0.5, 0.8))
})

test_that("parse_edgelist auto-detects weight column: value", {
  df <- data.frame(from = c("A", "B"), to = c("B", "C"), value = c(0.5, 0.8))
  result <- parse_edgelist(df)

  expect_equal(result$weights, c(0.5, 0.8))
})

test_that("parse_edgelist auto-detects weight column: strength", {
  df <- data.frame(from = c("A", "B"), to = c("B", "C"), strength = c(0.5, 0.8))
  result <- parse_edgelist(df)

  expect_equal(result$weights, c(0.5, 0.8))
})

test_that("parse_edgelist handles numeric node IDs", {
  df <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$nodes), 3)
})

test_that("parse_edgelist auto-detects directed for bidirectional edges", {
  # Edges 1->2 and 2->1 should indicate directed
  df <- data.frame(from = c(1, 2), to = c(2, 1))
  result <- parse_edgelist(df)

  expect_true(result$directed)
})

test_that("parse_edgelist auto-detects undirected for simple edges", {
  # No bidirectional edges
  df <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4))
  result <- parse_edgelist(df)

  expect_false(result$directed)
})

test_that("parse_edgelist respects explicit directed parameter", {
  df <- data.frame(from = c(1, 2), to = c(2, 3))

  result_directed <- parse_edgelist(df, directed = TRUE)
  expect_true(result_directed$directed)

  result_undirected <- parse_edgelist(df, directed = FALSE)
  expect_false(result_undirected$directed)
})

test_that("parse_edgelist handles mixed character and numeric nodes", {
  df <- data.frame(from = c("A", "B", "1"), to = c("B", "1", "A"))
  result <- parse_edgelist(df)

  expect_equal(nrow(result$nodes), 3)
  expect_true(all(c("A", "B", "1") %in% result$nodes$label))
})

# =============================================================================
# Integration tests - parse_input with different formats
# =============================================================================

test_that("parse_input correctly identifies igraph objects", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  result <- parse_input(g)

  expect_equal(nrow(result$nodes), 5)
  expect_false(result$directed)
})

test_that("parse_input correctly identifies tna objects", {
  # Create mock tna object
  mock_tna <- list(
    weights = matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3),
    labels = c("A", "B", "C"),
    inits = c(0.3, 0.4, 0.3)
  )
  class(mock_tna) <- "tna"

  result <- parse_input(mock_tna)

  expect_equal(nrow(result$nodes), 3)
  expect_true(result$directed)
})

# =============================================================================
# Edge cases and error handling
# =============================================================================

test_that("parse_matrix handles negative weights", {
  m <- matrix(c(0, -0.5, 0.3, -0.5, 0, -0.8, 0.3, -0.8, 0), nrow = 3)
  result <- parse_matrix(m)

  expect_true(any(result$edges$weight < 0))
})

test_that("parse_matrix handles very small weights", {
  m <- matrix(c(0, 1e-10, 1e-10, 0), nrow = 2)
  result <- parse_matrix(m)

  expect_equal(nrow(result$edges), 1)
})

test_that("parse_edgelist handles duplicate edges gracefully", {
  df <- data.frame(
    from = c("A", "A", "B"),
    to = c("B", "B", "C"),
    weight = c(0.5, 0.3, 0.8)
  )
  result <- parse_edgelist(df)

  expect_equal(nrow(result$edges), 3)  # Keeps all edges
})

test_that("create_nodes_df handles large number of nodes", {
  result <- create_nodes_df(1000)

  expect_equal(nrow(result), 1000)
  expect_equal(result$id, 1:1000)
})

test_that("aggregate_duplicate_edges preserves row attributes", {
  edges <- data.frame(
    from = c(1, 1, 2),
    to = c(2, 2, 3),
    weight = c(0.5, 0.3, 0.7)
  )
  result <- aggregate_duplicate_edges(edges, method = "sum")

  expect_equal(ncol(result), 3)  # from, to, weight preserved
})

test_that("detect_duplicate_edges info contains correct node pairs", {
  edges <- data.frame(
    from = c(5, 10, 5),
    to = c(10, 5, 10),
    weight = c(0.1, 0.2, 0.3)
  )
  result <- detect_duplicate_edges(edges)

  expect_true(result$has_duplicates)
  # All three should map to 5-10
  expect_equal(result$info[[1]]$nodes, c(5, 10))
  expect_equal(result$info[[1]]$count, 3)
})

# =============================================================================
# Complex scenarios
# =============================================================================

test_that("parse_matrix handles fully connected network", {
  n <- 4
  m <- matrix(1, nrow = n, ncol = n)
  diag(m) <- 0  # No self-loops
  result <- parse_matrix(m)

  expect_equal(nrow(result$nodes), 4)
  # For undirected, upper triangle has n*(n-1)/2 edges
  expect_equal(nrow(result$edges), 6)
})

test_that("parse_edgelist handles single edge network", {
  df <- data.frame(from = "A", to = "B")
  result <- parse_edgelist(df)

  expect_equal(nrow(result$nodes), 2)
  expect_equal(nrow(result$edges), 1)
})

test_that("parse_matrix with all zero weights returns empty edges", {
  m <- matrix(0, nrow = 3, ncol = 3)
  result <- parse_matrix(m)

  expect_equal(nrow(result$nodes), 3)
  expect_equal(nrow(result$edges), 0)
})

test_that("aggregate_duplicate_edges handles edges without weight column", {
  edges <- data.frame(from = c(1, 1, 2), to = c(2, 2, 3))
  result <- aggregate_duplicate_edges(edges, method = "sum")

  # Should work even without weight column
  expect_equal(nrow(result), 2)
})
