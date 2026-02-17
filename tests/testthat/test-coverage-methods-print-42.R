# Test file: test-coverage-methods-print-42.R
# Additional tests for S3 print methods in methods-print.R
# Target: Cover uncovered branches and edge cases

# Helper to create a proper fake cograph_network for print testing.
# The new print method uses getters: n_nodes(x) reads nrow(x$nodes),
# n_edges(x) reads nrow(x$edges), is_directed(x) reads x$directed,
# get_edges(x)$weight reads x$edges$weight, get_nodes(x) reads x$nodes.
make_test_net42 <- function(n_nodes = 3, n_edges = 3, directed = FALSE,
                            weights = NULL, coords = TRUE,
                            meta = list(), data = NULL) {
  nodes <- data.frame(
    id = seq_len(n_nodes),
    label = LETTERS[seq_len(n_nodes)],
    name = LETTERS[seq_len(n_nodes)],
    stringsAsFactors = FALSE
  )
  if (coords) {
    nodes$x <- seq(0, 1, length.out = n_nodes)
    nodes$y <- seq(0, 1, length.out = n_nodes)
  } else {
    nodes$x <- rep(NA_real_, n_nodes)
    nodes$y <- rep(NA_real_, n_nodes)
  }

  if (n_edges > 0) {
    from_idx <- seq_len(n_edges)
    to_idx <- from_idx + 1L
    from_idx <- ((from_idx - 1L) %% n_nodes) + 1L
    to_idx <- ((to_idx - 1L) %% n_nodes) + 1L
    w <- if (!is.null(weights)) weights else rep(1, n_edges)
    edges <- data.frame(from = from_idx, to = to_idx, weight = w)
  } else {
    edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  }

  net <- list(
    nodes = nodes,
    edges = edges,
    directed = directed,
    weights = NULL,
    meta = meta,
    data = data
  )
  class(net) <- c("cograph_network", "list")
  net
}

# ==============================================================================
# Tests for print.cograph_network - Unified Format Edge Cases
# ==============================================================================

test_that("print.cograph_network handles nodes missing x column", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2)
  # Remove x column to trigger "no layout" path
  net$nodes$x <- NULL

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network handles directed TRUE", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2, directed = TRUE,
                         weights = c(0.5, 0.8))

  output <- capture.output(print(net))
  expect_true(any(grepl("directed", output)))
  expect_false(any(grepl("undirected", output)))
})

test_that("print.cograph_network handles directed FALSE", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2, directed = FALSE)

  output <- capture.output(print(net))
  expect_true(any(grepl("undirected", output)))
})

test_that("print.cograph_network handles edges with weight column", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2, weights = c(0.2, 0.8))

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("0\\.2.*to.*0\\.8", output)))
})

test_that("print.cograph_network handles edges with equal weights", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2, weights = c(1, 1))

  output <- capture.output(print(net))
  expect_true(any(grepl("all equal", output)))
})

test_that("print.cograph_network handles zero edges", {
  net <- make_test_net42(n_nodes = 3, n_edges = 0)

  output <- capture.output(print(net))
  expect_true(any(grepl("0.*edges", output)))
  expect_false(any(grepl("Weights:", output)))
})

test_that("print.cograph_network handles layout set", {
  net <- make_test_net42(n_nodes = 2, n_edges = 1, coords = TRUE)

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*set", output)))
})

test_that("print.cograph_network handles layout none (all NAs)", {
  net <- make_test_net42(n_nodes = 2, n_edges = 1, coords = FALSE)

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network shows source when set in meta", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         meta = list(source = "matrix"))

  output <- capture.output(print(net))
  expect_true(any(grepl("Source:.*matrix", output)))
})

test_that("print.cograph_network hides source when unknown", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         meta = list(source = "unknown"))

  output <- capture.output(print(net))
  expect_false(any(grepl("Source:", output)))
})

test_that("print.cograph_network hides source when NULL", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         meta = list())

  output <- capture.output(print(net))
  expect_false(any(grepl("Source:", output)))
})

test_that("print.cograph_network shows data matrix info", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         data = matrix(1:12, nrow = 3))

  output <- capture.output(print(net))
  expect_true(any(grepl("Data:.*matrix", output)))
  expect_true(any(grepl("3 x 4", output)))
})

test_that("print.cograph_network shows data frame info", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         data = data.frame(a = 1:5, b = 6:10))

  output <- capture.output(print(net))
  expect_true(any(grepl("Data:.*data\\.frame", output)))
  expect_true(any(grepl("5 x 2", output)))
})

test_that("print.cograph_network shows data vector info", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         data = c(1, 2, 3, 4, 5))

  output <- capture.output(print(net))
  expect_true(any(grepl("Data:.*numeric", output)))
  expect_true(any(grepl("length 5", output)))
})

test_that("print.cograph_network returns invisible x", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2)

  result <- print(net)
  expect_identical(result, net)
})

# ==============================================================================
# Tests for print.cograph_network - Weight Edge Cases
# ==============================================================================

test_that("print.cograph_network handles weight range with NA values", {
  net <- make_test_net42(n_nodes = 3, n_edges = 3,
                         weights = c(0.2, NA, 0.8))

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
})

test_that("print.cograph_network handles negative weights", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         weights = c(-0.5, 0.3))

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("-0\\.5", output)))
})

test_that("print.cograph_network handles very small weights", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         weights = c(0.0001, 0.0002))

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
})

test_that("print.cograph_network handles very large weights", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         weights = c(1000, 5000))

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("1000", output)))
})

test_that("print.cograph_network handles Inf weights", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         weights = c(Inf, 1))

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
})

# ==============================================================================
# Tests for Large Network Display
# ==============================================================================

test_that("print.cograph_network handles large network", {
  n <- 100
  nodes <- data.frame(
    id = seq_len(n),
    label = paste0("node_", seq_len(n)),
    name = paste0("node_", seq_len(n)),
    x = runif(n),
    y = runif(n)
  )
  n_e <- n * 2
  edges <- data.frame(
    from = sample(n, n_e, replace = TRUE),
    to = sample(n, n_e, replace = TRUE),
    weight = runif(n_e)
  )
  net <- list(
    nodes = nodes,
    edges = edges,
    directed = TRUE,
    weights = NULL,
    meta = list(source = "test")
  )
  class(net) <- c("cograph_network", "list")

  output <- capture.output(print(net))
  expect_true(any(grepl("100.*nodes", output)))
  expect_true(any(grepl("200.*edges", output)))
})

test_that("print.cograph_network handles single node network", {
  net <- make_test_net42(n_nodes = 1, n_edges = 0)

  output <- capture.output(print(net))
  expect_true(any(grepl("1.*nodes", output)))
  expect_true(any(grepl("0.*edges", output)))
})

# ==============================================================================
# Tests for Partial/Mixed Coordinates
# ==============================================================================

test_that("print.cograph_network handles partial x/y coordinates", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2)
  net$nodes$x <- c(0, NA, 0.5)  # Partial NAs

  output <- capture.output(print(net))
  # Should show "set" because not ALL are NA
  expect_true(any(grepl("Layout:.*set", output)))
})

test_that("print.cograph_network handles all-NA coordinates as no layout", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2, coords = FALSE)

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

# ==============================================================================
# Tests for Output Structure
# ==============================================================================

test_that("print.cograph_network output order is consistent", {
  net <- make_test_net42(n_nodes = 3, n_edges = 3, directed = TRUE,
                         weights = c(0.2, 0.5, 0.8))

  output <- capture.output(print(net))

  # First line should contain "Cograph network:"
  expect_true(grepl("Cograph network:", output[1]))
  # Second line should contain "Weights:"
  expect_true(grepl("Weights:", output[2]))
  # Layout line should be present
  expect_true(any(grepl("Layout:", output)))
})

test_that("print.cograph_network output with source and data", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         weights = c(0.3, 0.7),
                         meta = list(source = "edgelist"),
                         data = matrix(1:6, nrow = 2))

  output <- capture.output(print(net))

  # Check all sections present
  expect_true(any(grepl("Cograph network:", output)))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("Source:.*edgelist", output)))
  expect_true(any(grepl("Layout:", output)))
  expect_true(any(grepl("Data:", output)))
})

# ==============================================================================
# Tests for Edge Cases with Real cograph() Output
# ==============================================================================

test_that("print works with cograph() from matrix input", {
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(print(net))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Cograph", output)))
})

test_that("print works with cograph() from symmetric matrix", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph", output)))
})

test_that("print works with cograph() from asymmetric matrix", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- cograph(adj, directed = TRUE)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph", output)))
  expect_true(any(grepl("directed", output)))
})

test_that("print works with cograph() from edgelist", {
  edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    weight = c(1, 2, 3)
  )
  net <- cograph(edges)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph", output)))
})

test_that("print works with cograph() weighted network", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
})

test_that("print works with cograph() empty network", {
  adj <- matrix(0, nrow = 3, ncol = 3)
  net <- cograph(adj)

  output <- capture.output(print(net))
  expect_true(any(grepl("0.*edges", output)))
})

# ==============================================================================
# Tests for Numeric Precision in Weight Display
# ==============================================================================

test_that("print.cograph_network rounds weights to 3 decimal places", {
  net <- make_test_net42(n_nodes = 3, n_edges = 2,
                         weights = c(0.123456789, 0.987654321))

  output <- capture.output(print(net))
  combined <- paste(output, collapse = " ")

  # Should show 3 decimal places
  expect_true(grepl("0\\.123", combined))
  expect_true(grepl("0\\.988", combined))  # Rounded
})

# ==============================================================================
# Tests Ensuring All Print Paths Return Properly
# ==============================================================================

test_that("print.cograph_network returns invisibly for all configurations", {
  # With edges
  net1 <- make_test_net42(n_nodes = 3, n_edges = 2, weights = c(0.5, 0.8))
  result1 <- print(net1)
  expect_identical(result1, net1)

  # Without edges
  net2 <- make_test_net42(n_nodes = 3, n_edges = 0)
  result2 <- print(net2)
  expect_identical(result2, net2)

  # With meta source
  net3 <- make_test_net42(n_nodes = 3, n_edges = 2,
                          meta = list(source = "matrix"))
  result3 <- print(net3)
  expect_identical(result3, net3)

  # With data
  net4 <- make_test_net42(n_nodes = 3, n_edges = 2,
                          data = data.frame(a = 1:3))
  result4 <- print(net4)
  expect_identical(result4, net4)

  # No layout
  net5 <- make_test_net42(n_nodes = 3, n_edges = 2, coords = FALSE)
  result5 <- print(net5)
  expect_identical(result5, net5)
})

test_that("print.cograph_network handles all output branches", {
  # Branch: e > 0 with different weights
  net1 <- make_test_net42(n_nodes = 3, n_edges = 2, weights = c(0.1, 0.9))
  out1 <- capture.output(print(net1))
  expect_true(any(grepl("to", out1)))

  # Branch: e > 0 with equal weights
  net2 <- make_test_net42(n_nodes = 3, n_edges = 2, weights = c(0.5, 0.5))
  out2 <- capture.output(print(net2))
  expect_true(any(grepl("all equal", out2)))

  # Branch: e == 0 (no weights line)
  net3 <- make_test_net42(n_nodes = 3, n_edges = 0)
  out3 <- capture.output(print(net3))
  expect_false(any(grepl("Weights:", out3)))

  # Branch: source present and not "unknown"
  net4 <- make_test_net42(n_nodes = 3, n_edges = 2,
                          meta = list(source = "tna"))
  out4 <- capture.output(print(net4))
  expect_true(any(grepl("Source:.*tna", out4)))

  # Branch: source "unknown" (hidden)
  net5 <- make_test_net42(n_nodes = 3, n_edges = 2,
                          meta = list(source = "unknown"))
  out5 <- capture.output(print(net5))
  expect_false(any(grepl("Source:", out5)))

  # Branch: has_layout TRUE
  net6 <- make_test_net42(n_nodes = 3, n_edges = 2, coords = TRUE)
  out6 <- capture.output(print(net6))
  expect_true(any(grepl("Layout:.*set", out6)))

  # Branch: has_layout FALSE
  net7 <- make_test_net42(n_nodes = 3, n_edges = 2, coords = FALSE)
  out7 <- capture.output(print(net7))
  expect_true(any(grepl("Layout:.*none", out7)))

  # Branch: data present (matrix)
  net8 <- make_test_net42(n_nodes = 3, n_edges = 2,
                          data = matrix(1:6, nrow = 2))
  out8 <- capture.output(print(net8))
  expect_true(any(grepl("Data:", out8)))

  # Branch: data present (vector, no dim)
  net9 <- make_test_net42(n_nodes = 3, n_edges = 2,
                          data = c(1.0, 2.0, 3.0))
  out9 <- capture.output(print(net9))
  expect_true(any(grepl("Data:.*numeric", out9)))
  expect_true(any(grepl("length 3", out9)))

  # Branch: data NULL (no Data line)
  net10 <- make_test_net42(n_nodes = 3, n_edges = 2)
  out10 <- capture.output(print(net10))
  expect_false(any(grepl("Data:", out10)))
})
