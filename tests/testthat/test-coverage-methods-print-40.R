# Test file: test-coverage-methods-print-40.R
# Tests for S3 print methods in methods-print.R and related files
# Target: 100% coverage for print methods

# Helper to create a proper fake cograph_network for print testing.
# The new print method uses getters: n_nodes(x) reads nrow(x$nodes),
# n_edges(x) reads nrow(x$edges), is_directed(x) reads x$directed,
# get_edges(x)$weight reads x$edges$weight, get_nodes(x) reads x$nodes.
make_test_net <- function(n_nodes = 3, n_edges = 3, directed = FALSE,
                          weights = NULL, coords = TRUE,
                          meta = list()) {
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
    # Wrap around
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
    meta = meta
  )
  class(net) <- c("cograph_network", "list")
  net
}

# ==============================================================================
# Tests for print.cograph_network (methods-print.R)
# ==============================================================================

# --- Unified format with proper nodes/edges/directed structure ---

test_that("print.cograph_network shows node and edge counts", {
  net <- make_test_net(n_nodes = 3, n_edges = 3, directed = FALSE)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network:", output)))
  expect_true(any(grepl("3.*nodes", output)))
  expect_true(any(grepl("3.*edges", output)))
  expect_true(any(grepl("undirected", output)))
})

test_that("print.cograph_network shows directed status", {
  net <- make_test_net(n_nodes = 3, n_edges = 2, directed = TRUE,
                       weights = c(0.5, 0.8))

  output <- capture.output(print(net))
  expect_true(any(grepl("directed", output)))
})

test_that("print.cograph_network shows weight range with different weights", {
  net <- make_test_net(n_nodes = 3, n_edges = 3,
                       weights = c(0.2, 0.5, 0.8))

  output <- capture.output(print(net))
  expect_true(any(grepl("Weights:", output)))
  expect_true(any(grepl("0\\.2.*to.*0\\.8", output)))
})

test_that("print.cograph_network shows equal weights message", {
  net <- make_test_net(n_nodes = 3, n_edges = 3,
                       weights = c(1, 1, 1))

  output <- capture.output(print(net))
  expect_true(any(grepl("all equal", output)))
})

test_that("print.cograph_network shows layout status when set", {
  net <- make_test_net(n_nodes = 3, n_edges = 2, coords = TRUE)

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*set", output)))
})

test_that("print.cograph_network shows layout: none when not set", {
  net <- make_test_net(n_nodes = 3, n_edges = 2, coords = FALSE)

  output <- capture.output(print(net))
  expect_true(any(grepl("Layout:.*none", output)))
})

test_that("print.cograph_network returns invisible x", {
  net <- make_test_net(n_nodes = 3, n_edges = 2)

  result <- print(net)
  expect_identical(result, net)
})

test_that("print.cograph_network handles network with zero edges", {
  net <- make_test_net(n_nodes = 3, n_edges = 0)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network:", output)))
  expect_true(any(grepl("0.*edges", output)))
})

test_that("print.cograph_network shows source when set", {
  net <- make_test_net(n_nodes = 3, n_edges = 2,
                       meta = list(source = "matrix"))

  output <- capture.output(print(net))
  expect_true(any(grepl("Source:.*matrix", output)))
})

test_that("print.cograph_network hides source when unknown", {
  net <- make_test_net(n_nodes = 3, n_edges = 2,
                       meta = list(source = "unknown"))

  output <- capture.output(print(net))
  expect_false(any(grepl("Source:", output)))
})

test_that("print.cograph_network shows data info when present", {
  net <- make_test_net(n_nodes = 3, n_edges = 2)
  net$data <- matrix(1:12, nrow = 3, ncol = 4)

  output <- capture.output(print(net))
  expect_true(any(grepl("Data:.*matrix", output)))
  expect_true(any(grepl("3 x 4", output)))
})

test_that("print.cograph_network works with actual cograph() output", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(print(net))
  expect_true(any(grepl("Cograph network", output)))
  expect_true(any(grepl("3.*nodes", output)))
})

# ==============================================================================
# Tests for print.cograph_communities (communities.R)
# ==============================================================================
test_that("print.cograph_communities shows basic info", {
  skip_if_not_installed("igraph")

  # Create a proper igraph communities object
  g <- igraph::make_ring(6)
  igraph::V(g)$name <- letters[1:6]

  # Use actual community detection to get a proper object
  communities <- igraph::cluster_louvain(g)
  result <- communities
  result$algorithm <- "louvain"
  result$names <- letters[1:6]
  class(result) <- c("cograph_communities", class(communities))

  output <- capture.output(print(result))
  expect_true(any(grepl("Community structure", output)) ||
                any(grepl("louvain", output)))
})

test_that("print.cograph_communities without node names", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(4)
  communities <- igraph::cluster_louvain(g)
  result <- communities
  result$algorithm <- "fast_greedy"
  result$names <- NULL
  class(result) <- c("cograph_communities", class(communities))

  # Should not error when names is NULL
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("all print methods return invisible self", {
  # Test cograph_network
  net <- make_test_net(n_nodes = 3, n_edges = 2)
  result <- print(net)
  expect_s3_class(result, "cograph_network")
})

test_that("print output is human readable", {
  net <- make_test_net(n_nodes = 5, n_edges = 6, directed = TRUE,
                       weights = c(0.1, 0.5, 0.3, 0.8, 0.2, 0.9))

  output <- capture.output(print(net))
  combined <- paste(output, collapse = " ")

  # Output should contain recognizable network information
  expect_true(grepl("Cograph", combined))
  expect_true(grepl("5", combined))  # n_nodes
  expect_true(grepl("6", combined))  # n_edges
})
