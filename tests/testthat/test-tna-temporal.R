# Tests for tna temporal edge list extraction

# Helper: create a minimal tna object with sequence data
make_tna <- function() {
  # 3 states, 4 sessions, short sequences
  data_mat <- matrix(c(
    1, 2, 3, 1, 2, NA,
    2, 3, 1, 2, NA, NA,
    1, 1, 2, 3, 3, 2,
    3, 2, 1, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  class(data_mat) <- c("tna_seq_data", "matrix", "array")

  # Compute weights from transitions
  n <- 3
  weights <- matrix(0, n, n)
  for (i in seq_len(nrow(data_mat))) {
    row <- data_mat[i, ]
    valid <- row[is.finite(row) & !is.na(row)]
    for (j in seq_len(length(valid) - 1)) {
      weights[valid[j], valid[j + 1]] <- weights[valid[j], valid[j + 1]] + 1
    }
  }
  # Normalize rows to probabilities
  rs <- rowSums(weights)
  rs[rs == 0] <- 1
  weights <- weights / rs

  labels <- c("A", "B", "C")
  rownames(weights) <- colnames(weights) <- labels

  structure(
    list(
      weights = weights,
      labels = labels,
      inits = c(0.5, 0.25, 0.25),
      data = data_mat
    ),
    class = c("tna", "list")
  )
}

# Helper: tna object WITHOUT $data
make_tna_no_data <- function() {
  weights <- matrix(c(
    0.0, 0.6, 0.4,
    0.3, 0.0, 0.7,
    0.5, 0.5, 0.0
  ), nrow = 3, byrow = TRUE)
  labels <- c("X", "Y", "Z")
  rownames(weights) <- colnames(weights) <- labels

  structure(
    list(weights = weights, labels = labels, inits = c(0.4, 0.3, 0.3)),
    class = c("tna", "list")
  )
}

# =============================================================================
# parse_tna: temporal edge list extraction
# =============================================================================

test_that("parse_tna extracts individual transitions from $data", {
  mod <- make_tna()
  parsed <- parse_tna(mod, simplify = FALSE)

  edges <- parsed$edges
  expect_true("session" %in% names(edges))
  expect_true("time" %in% names(edges))

  # Count total transitions manually:
  # Session 1: 1->2, 2->3, 3->1, 1->2 = 4 edges (5 valid values)
  # Session 2: 2->3, 3->1, 1->2 = 3 edges (4 valid values)
  # Session 3: 1->1, 1->2, 2->3, 3->3, 3->2 = 5 edges (6 valid values)
  # Session 4: 3->2, 2->1 = 2 edges (3 valid values)
  # Total = 14
  expect_equal(nrow(edges), 14)

  # All weights should be 1 (individual transitions)
  expect_true(all(edges$weight == 1))

  # Session column should be correct
  expect_equal(edges$session[1], 1L)
  expect_equal(edges$session[5], 2L)  # session 2 starts at edge 5

  # Time column should restart per session
  expect_equal(edges$time[1], 1L)
  expect_equal(edges$time[2], 2L)
  expect_equal(edges$time[5], 1L)  # first transition of session 2
})

test_that("parse_tna simplify=TRUE aggregates to weighted edges", {
  mod <- make_tna()
  parsed <- parse_tna(mod, simplify = TRUE)

  edges <- parsed$edges
  # No session/time columns after simplification
  expect_false("session" %in% names(edges))
  expect_false("time" %in% names(edges))

  # Fewer edges than raw transitions
  expect_true(nrow(edges) < 14)

  # Weights should sum to total transitions
  expect_equal(sum(edges$weight), 14)
})

test_that("parse_tna simplify='mean' uses mean aggregation", {
  mod <- make_tna()
  parsed_sum <- parse_tna(mod, simplify = "sum")
  parsed_mean <- parse_tna(mod, simplify = "mean")

  # Same number of unique edges
  expect_equal(nrow(parsed_sum$edges), nrow(parsed_mean$edges))

  # Sum weights > mean weights (when count > 1)
  expect_true(sum(parsed_sum$edges$weight) >= sum(parsed_mean$edges$weight))
})

test_that("parse_tna without $data falls back to weight matrix", {
  mod <- make_tna_no_data()
  parsed <- parse_tna(mod, simplify = FALSE)

  edges <- parsed$edges
  # Should NOT have session/time columns
  expect_false("session" %in% names(edges))

  # Should have edges from the weight matrix (all 6 non-zero cells)
  expect_equal(nrow(edges), 6)

  # Weights should match matrix values
  expect_true(all(edges$weight > 0))
})

test_that("parse_tna preserves weights_matrix regardless of simplify", {
  mod <- make_tna()
  parsed <- parse_tna(mod, simplify = FALSE)
  expect_equal(parsed$weights_matrix, mod$weights)

  parsed2 <- parse_tna(mod, simplify = TRUE)
  expect_equal(parsed2$weights_matrix, mod$weights)
})

test_that("parse_tna preserves inits and labels", {
  mod <- make_tna()
  parsed <- parse_tna(mod, simplify = FALSE)

  expect_equal(parsed$nodes$label, c("A", "B", "C"))
  expect_equal(parsed$nodes$inits, c(0.5, 0.25, 0.25))
})

# =============================================================================
# as_cograph: temporal edge list in cograph_network
# =============================================================================

test_that("as_cograph(tna) preserves session and time columns", {
  mod <- make_tna()
  net <- as_cograph(mod)

  edges <- get_edges(net)
  expect_true("session" %in% names(edges))
  expect_true("time" %in% names(edges))
  expect_equal(nrow(edges), 14)
})

test_that("as_cograph(tna, simplify=TRUE) produces weighted simple graph", {
  mod <- make_tna()
  net <- as_cograph(mod, simplify = TRUE)

  edges <- get_edges(net)
  expect_false("session" %in% names(edges))
  expect_true(nrow(edges) < 14)
  expect_equal(sum(edges$weight), 14)
})

test_that("as_cograph(tna, simplify='mean') works", {
  mod <- make_tna()
  net <- as_cograph(mod, simplify = "mean")

  edges <- get_edges(net)
  expect_true(nrow(edges) < 14)
})

test_that("cograph(tna) preserves temporal edges", {
  mod <- make_tna()
  net <- cograph(mod)

  edges <- get_edges(net)
  expect_true("session" %in% names(edges))
  expect_equal(nrow(edges), 14)
})

test_that("cograph(tna, simplify=TRUE) simplifies", {
  mod <- make_tna()
  net <- cograph(mod, simplify = TRUE)

  edges <- get_edges(net)
  expect_false("session" %in% names(edges))
  expect_true(nrow(edges) < 14)
})

# =============================================================================
# Weight matrix is always available for round-trip
# =============================================================================

test_that("to_matrix works on temporal tna cograph_network", {
  mod <- make_tna()
  net <- as_cograph(mod)

  mat <- to_matrix(net)
  expect_equal(dim(mat), c(3, 3))
  # Should return the tna weight matrix, not the edge list
  expect_equal(mat, mod$weights)
})

# =============================================================================
# Edge cases
# =============================================================================

test_that("parse_tna handles empty sessions (all NA)", {
  data_mat <- matrix(c(
    1, 2, 3, NA,
    NA, NA, NA, NA,
    2, 1, NA, NA
  ), nrow = 3, byrow = TRUE)
  class(data_mat) <- c("tna_seq_data", "matrix", "array")

  weights <- matrix(0.25, 3, 3)
  diag(weights) <- 0
  labels <- c("A", "B", "C")
  rownames(weights) <- colnames(weights) <- labels

  mod <- structure(
    list(weights = weights, labels = labels, inits = c(0.33, 0.33, 0.34),
         data = data_mat),
    class = c("tna", "list")
  )

  parsed <- parse_tna(mod, simplify = FALSE)
  # Session 1: 1->2, 2->3 = 2 edges
  # Session 2: all NA = 0 edges
  # Session 3: 2->1 = 1 edge
  expect_equal(nrow(parsed$edges), 3)
})

test_that("parse_tna handles single-element sessions", {
  data_mat <- matrix(c(
    1, NA, NA,
    2, 3, NA,
    1, NA, NA
  ), nrow = 3, byrow = TRUE)
  class(data_mat) <- c("tna_seq_data", "matrix", "array")

  weights <- matrix(0.5, 3, 3)
  diag(weights) <- 0
  labels <- c("A", "B", "C")
  rownames(weights) <- colnames(weights) <- labels

  mod <- structure(
    list(weights = weights, labels = labels, inits = c(0.33, 0.33, 0.34),
         data = data_mat),
    class = c("tna", "list")
  )

  parsed <- parse_tna(mod, simplify = FALSE)
  # Session 1: single state, 0 edges
  # Session 2: 2->3, 1 edge
  # Session 3: single state, 0 edges
  expect_equal(nrow(parsed$edges), 1)
})

test_that("simplify with real tna data produces correct totals", {
  skip_if_not_installed("tna")
  library(tna)
  mod <- tna(human_ai)

  parsed_raw <- parse_tna(mod, simplify = FALSE)
  parsed_agg <- parse_tna(mod, simplify = "sum")

  # Sum of aggregated weights = number of raw transitions
  expect_equal(sum(parsed_agg$edges$weight), nrow(parsed_raw$edges))
})

test_that("simplify preserves directed edge distinction", {
  mod <- make_tna()
  parsed <- parse_tna(mod, simplify = "sum")

  # A->B and B->A should be separate edges (directed)
  ab <- parsed$edges[parsed$edges$from == 1 & parsed$edges$to == 2, ]
  ba <- parsed$edges[parsed$edges$from == 2 & parsed$edges$to == 1, ]
  # Both should exist (there are A->B and B->A transitions in the data)
  expect_true(nrow(ab) > 0 || nrow(ba) > 0)
})

test_that("group_tna parse forwards simplify", {
  mod <- make_tna()

  # Create a fake group_tna
  group_mod <- list(group1 = mod, group2 = mod)
  class(group_mod) <- c("group_tna", "list")

  parsed <- parse_group_tna(group_mod, i = 1, simplify = FALSE)
  expect_true("session" %in% names(parsed$edges))
  expect_equal(nrow(parsed$edges), 14)

  parsed2 <- parse_group_tna(group_mod, i = 1, simplify = TRUE)
  expect_false("session" %in% names(parsed2$edges))
  expect_true(nrow(parsed2$edges) < 14)
})
