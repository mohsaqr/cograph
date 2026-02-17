# Comprehensive coverage tests for input-tna.R
# All tests use mock tna/group_tna objects — no tna package dependency needed.
# tna structure: list(weights, labels, inits, data) with class c("tna", "list")
# group_tna structure: named list of tna objects with class c("group_tna", "list")

# =============================================================================
# Helper: Mock tna and group_tna constructors
# =============================================================================

mock_tna <- function(
    weights = matrix(c(0, 0.5, 0.3, 0.4, 0, 0.2, 0.1, 0.6, 0), 3, 3),
    labels = c("A", "B", "C"),
    inits = c(0.4, 0.35, 0.25),
    data = NULL,
    directed = NULL
) {
  obj <- list(weights = weights, labels = labels, inits = inits, data = data)
  if (!is.null(directed)) obj$directed <- directed
  class(obj) <- c("tna", "list")
  obj
}

mock_group_tna <- function(
    n_groups = 2,
    group_names = c("GroupA", "GroupB")
) {
  groups <- lapply(seq_len(n_groups), function(i) {
    w <- matrix(runif(9), 3, 3)
    diag(w) <- 0
    w <- w / rowSums(w)
    mock_tna(weights = w)
  })
  names(groups) <- group_names[seq_len(n_groups)]
  class(groups) <- c("group_tna", "list")
  groups
}

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
  model <- mock_tna(directed = FALSE)
  parsed <- parse_tna(model)
  expect_false(parsed$directed)

  model$directed <- TRUE
  parsed <- parse_tna(model)
  expect_true(parsed$directed)
})

test_that("parse_tna reads directed from attr(tna_obj, 'directed')", {
  model <- mock_tna()
  # No $directed field, set attribute instead
  attr(model, "directed") <- FALSE
  parsed <- parse_tna(model)
  expect_false(parsed$directed)

  attr(model, "directed") <- TRUE
  parsed <- parse_tna(model)
  expect_true(parsed$directed)
})

test_that("parse_tna defaults to directed=TRUE when no directed info", {
  model <- mock_tna()
  # No $directed field, no attribute
  parsed <- parse_tna(model)
  expect_true(parsed$directed)
})

test_that("parse_tna respects explicit directed parameter", {
  model <- mock_tna()

  parsed <- parse_tna(model, directed = FALSE)
  expect_false(parsed$directed)

  parsed <- parse_tna(model, directed = TRUE)
  expect_true(parsed$directed)
})

test_that("parse_tna handles NULL labels", {
  model <- mock_tna(labels = NULL)
  n <- nrow(model$weights)
  parsed <- parse_tna(model)
  expect_equal(parsed$nodes$label, as.character(seq_len(n)))
})

test_that("parse_tna handles all NA labels", {
  model <- mock_tna()
  n <- nrow(model$weights)
  model$labels <- rep(NA, n)
  parsed <- parse_tna(model)
  expect_equal(parsed$nodes$label, as.character(seq_len(n)))
})

test_that("parse_tna handles empty network (all zero weights)", {
  model <- mock_tna(
    weights = matrix(0, 3, 3),
    inits = c(0.3, 0.3, 0.4)
  )
  parsed <- parse_tna(model)
  expect_equal(nrow(parsed$nodes), 3)
  expect_equal(nrow(parsed$edges), 0)
})

test_that("parse_tna handles tna without inits", {
  model <- mock_tna(inits = NULL)
  parsed <- parse_tna(model)
  expect_false("inits" %in% names(parsed$nodes))
})

test_that("parse_tna handles tna with inits", {
  model <- mock_tna(inits = c(0.4, 0.35, 0.25))
  parsed <- parse_tna(model)
  expect_true("inits" %in% names(parsed$nodes))
  expect_equal(parsed$nodes$inits, c(0.4, 0.35, 0.25))
})

test_that("parse_tna handles tna with data containing colors", {
  fake_data <- data.frame(state = c("A", "B", "C"))
  attr(fake_data, "colors") <- c("#FF0000", "#00FF00", "#0000FF")
  model <- mock_tna(data = fake_data)

  parsed <- parse_tna(model)
  expect_true("color" %in% names(parsed$nodes))
  expect_equal(parsed$nodes$color, c("#FF0000", "#00FF00", "#0000FF"))
})

test_that("parse_tna handles tna with data but wrong color count", {
  fake_data <- data.frame(state = c("A", "B", "C"))
  attr(fake_data, "colors") <- c("#FF0000", "#00FF00")  # Only 2 colors for 3 nodes
  model <- mock_tna(data = fake_data)

  parsed <- parse_tna(model)
  expect_false("color" %in% names(parsed$nodes))
})

test_that("parse_tna handles tna with data but no colors attribute", {
  fake_data <- data.frame(state = c("A", "B", "C"))
  model <- mock_tna(data = fake_data)

  parsed <- parse_tna(model)
  expect_false("color" %in% names(parsed$nodes))
})

test_that("parse_tna returns correct tna metadata structure", {
  model <- mock_tna()
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
  fake_list <- list(a = 1, b = 2)
  expect_error(parse_group_tna(fake_list), "Input must be a group_tna object")

  # tna object (not group_tna) should error
  model <- mock_tna()
  expect_error(parse_group_tna(model), "Input must be a group_tna object")
})

test_that("parse_group_tna errors on index out of bounds", {
  group_model <- mock_group_tna(n_groups = 2)

  expect_error(parse_group_tna(group_model, i = 0),
               "Index i must be between 1 and")
  expect_error(parse_group_tna(group_model, i = 3),
               "Index i must be between 1 and")
  expect_error(parse_group_tna(group_model, i = -1),
               "Index i must be between 1 and")
})

test_that("parse_group_tna extracts correct group", {
  group_model <- mock_group_tna(n_groups = 2, group_names = c("GroupA", "GroupB"))

  parsed1 <- parse_group_tna(group_model, i = 1)
  expect_equal(parsed1$tna$group_index, 1)
  expect_equal(parsed1$tna$group_name, "GroupA")
  expect_equal(parsed1$tna$type, "group_tna")

  parsed2 <- parse_group_tna(group_model, i = 2)
  expect_equal(parsed2$tna$group_index, 2)
  expect_equal(parsed2$tna$group_name, "GroupB")
})

test_that("parse_group_tna respects directed parameter", {
  group_model <- mock_group_tna()

  parsed <- parse_group_tna(group_model, i = 1, directed = FALSE)
  expect_false(parsed$directed)

  parsed <- parse_group_tna(group_model, i = 1, directed = TRUE)
  expect_true(parsed$directed)
})

test_that("parse_group_tna preserves weights matrix", {
  group_model <- mock_group_tna()
  parsed <- parse_group_tna(group_model, i = 1)
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
  expect_false(is_tna_network(net))
})

test_that("is_tna_network returns FALSE for cograph_network with NULL meta$tna$type", {
  mat <- matrix(runif(16), 4, 4)
  net <- as_cograph(mat)
  net$meta$tna <- list(type = NULL)
  expect_false(is_tna_network(net))
})

test_that("is_tna_network works with CographNetwork R6 class", {
  mat <- matrix(runif(9), 3, 3)
  net <- CographNetwork$new(mat)
  expect_false(is_tna_network(net))
})

test_that("is_tna_network returns TRUE for properly structured tna network", {
  model <- mock_tna()
  net <- as_cograph(model)
  expect_true(is_tna_network(net))
  expect_equal(net$meta$tna$type, "tna")
})

test_that("is_tna_network returns TRUE for group_tna network", {
  group_model <- mock_group_tna()
  parsed <- parse_group_tna(group_model, i = 1)

  net <- .create_cograph_network(
    nodes = parsed$nodes,
    edges = parsed$edges,
    directed = parsed$directed,
    meta = list(source = "group_tna", tna = parsed$tna),
    weights = parsed$weights_matrix
  )

  expect_true(is_tna_network(net))
  expect_equal(net$meta$tna$type, "group_tna")
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("parse_tna creates valid nodes data frame", {
  model <- mock_tna()
  parsed <- parse_tna(model)

  nodes <- parsed$nodes
  expect_true(is.data.frame(nodes))
  expect_true("id" %in% names(nodes))
  expect_true("label" %in% names(nodes))
  expect_true("x" %in% names(nodes))
  expect_true("y" %in% names(nodes))
})

test_that("parse_tna creates valid edges data frame", {
  model <- mock_tna()
  parsed <- parse_tna(model)

  edges <- parsed$edges
  expect_true(is.data.frame(edges))
  expect_true("from" %in% names(edges))
  expect_true("to" %in% names(edges))
  expect_true("weight" %in% names(edges))
})

test_that("parse_tna extracts edges correctly from weight matrix", {
  weights <- matrix(c(
    0, 0.5, 0,
    0.3, 0, 0.2,
    0.4, 0, 0
  ), 3, 3, byrow = TRUE)
  model <- mock_tna(weights = weights)

  parsed <- parse_tna(model)
  expect_equal(nrow(parsed$edges), 4)

  edges <- parsed$edges
  expect_true(any(edges$from == 1 & edges$to == 2 & edges$weight == 0.5))
  expect_true(any(edges$from == 2 & edges$to == 1 & edges$weight == 0.3))
})

test_that("parse_tna preserves full weight matrix", {
  model <- mock_tna()
  parsed <- parse_tna(model)
  expect_equal(parsed$weights_matrix, model$weights)
})

test_that("tna network round-trip through as_cograph preserves structure", {
  model <- mock_tna()
  net <- as_cograph(model)

  expect_equal(n_nodes(net), nrow(model$weights))
  expect_equal(net$weights, model$weights)
  expect_equal(net$meta$source, "tna")
  expect_true(is_tna_network(net))
})

test_that("group_tna network includes group metadata", {
  group_model <- mock_group_tna(group_names = c("First", "Second"))
  parsed <- parse_group_tna(group_model, i = 1)

  expect_equal(parsed$tna$type, "group_tna")
  expect_equal(parsed$tna$group_index, 1)
  expect_equal(parsed$tna$group_name, "First")
})

# =============================================================================
# Edge Cases and Boundary Conditions
# =============================================================================

test_that("parse_tna handles single-node network", {
  model <- mock_tna(
    weights = matrix(0, 1, 1),
    labels = "A",
    inits = 1.0
  )
  parsed <- parse_tna(model)
  expect_equal(nrow(parsed$nodes), 1)
  expect_equal(nrow(parsed$edges), 0)
  expect_equal(parsed$nodes$label, "A")
})

test_that("parse_tna handles self-loops", {
  weights <- matrix(c(0.5, 0.3, 0.2, 0.4), 2, 2, byrow = TRUE)
  model <- mock_tna(weights = weights, labels = c("A", "B"), inits = c(0.5, 0.5))

  parsed <- parse_tna(model)
  edges <- parsed$edges
  expect_true(any(edges$from == 1 & edges$to == 1))
  expect_true(any(edges$from == 2 & edges$to == 2))
})

test_that("parse_tna handles negative weights", {
  weights <- matrix(c(
    0, 0.5, -0.3,
    -0.2, 0, 0.4,
    0.1, -0.6, 0
  ), 3, 3, byrow = TRUE)
  model <- mock_tna(weights = weights)

  parsed <- parse_tna(model)
  expect_equal(nrow(parsed$edges), 6)
  expect_true(any(parsed$edges$weight < 0))
})

test_that("parse_tna handles very small weights", {
  weights <- matrix(c(
    0, 1e-10, 0,
    0, 0, 1e-15,
    0, 0, 0
  ), 3, 3, byrow = TRUE)
  model <- mock_tna(weights = weights)

  parsed <- parse_tna(model)
  expect_equal(nrow(parsed$edges), 2)
})

test_that("parse_tna correctly creates directed edges", {
  weights <- matrix(c(
    0, 0.5, 0,
    0, 0, 0.3,
    0.4, 0, 0
  ), 3, 3, byrow = TRUE)
  model <- mock_tna(weights = weights)

  parsed <- parse_tna(model)
  expect_true(parsed$directed)

  edges <- parsed$edges
  expect_true(any(edges$from == 1 & edges$to == 2))
  expect_false(any(edges$from == 2 & edges$to == 1))
})
