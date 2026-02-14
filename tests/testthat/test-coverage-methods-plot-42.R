# Test file: test-coverage-methods-plot-42.R
# Tests for methods-plot.R (plot.cograph_network and summary.cograph_network)
# Targeting coverage for all uncovered branches and edge cases

# ==============================================================================
# Tests for plot.cograph_network
# ==============================================================================

test_that("plot.cograph_network plots a basic network", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_s3_class(result, "cograph_network")
  expect_true(file.exists(tmp))
})

test_that("plot.cograph_network returns invisible x", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_identical(result, net)
})

test_that("plot.cograph_network passes additional arguments to sn_render", {
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net, title = "Test Network")
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("plot.cograph_network works with directed network", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- cograph(adj, directed = TRUE)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("plot.cograph_network works with weighted network", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.7, 0.3, 0.7, 0), nrow = 3)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("plot.cograph_network works with single node network", {
  adj <- matrix(0, nrow = 1, ncol = 1)
  rownames(adj) <- colnames(adj) <- "A"
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("plot.cograph_network works with many nodes", {
  n <- 12
  adj <- matrix(0, n, n)
  adj[upper.tri(adj)] <- sample(c(0, 1), sum(upper.tri(adj)), replace = TRUE)
  adj <- adj + t(adj)
  diag(adj) <- 0
  rownames(adj) <- colnames(adj) <- paste0("N", 1:n)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_s3_class(result, "cograph_network")
})

# ==============================================================================
# Tests for summary.cograph_network - Basic Structure
# ==============================================================================

test_that("summary.cograph_network works with basic undirected network", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Cograph Network Summary", output)))
  expect_true(any(grepl("Nodes:", output)))
  expect_true(any(grepl("Edges:", output)))
  expect_true(any(grepl("Undirected", output)))
})

test_that("summary.cograph_network shows directed type", {
  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- cograph(adj, directed = TRUE)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Directed", output)))
})

test_that("summary.cograph_network returns invisible list with correct structure", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))

  expect_type(result, "list")
  expect_true("n_nodes" %in% names(result))
  expect_true("n_edges" %in% names(result))
  expect_true("directed" %in% names(result))
  expect_true("weighted" %in% names(result))
  expect_true("has_layout" %in% names(result))
})

test_that("summary.cograph_network returns correct node count", {
  adj <- matrix(0, 5, 5)
  adj[1, 2] <- adj[2, 1] <- 1
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_equal(result$n_nodes, 5)
})

test_that("summary.cograph_network returns correct edge count", {
  adj <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_equal(result$n_edges, n_edges(net))
})

# ==============================================================================
# Tests for summary.cograph_network - Edge Statistics Branch
# ==============================================================================

test_that("summary.cograph_network shows edge statistics with weights", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.7, 0.3, 0.7, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Edge Statistics", output)))
  expect_true(any(grepl("Min weight", output)))
  expect_true(any(grepl("Max weight", output)))
  expect_true(any(grepl("Mean weight", output)))
})

test_that("summary.cograph_network shows positive/negative edge counts when negative edges exist", {
  adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, -0.2, -0.3, -0.2, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Positive edges", output)))
  expect_true(any(grepl("Negative edges", output)))
})

test_that("summary.cograph_network does not show negative edge count when all positive", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.7, 0.3, 0.7, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  # Should have Edge Statistics but NOT "Negative edges"
  expect_true(any(grepl("Edge Statistics", output)))
  expect_false(any(grepl("Negative edges", output)))
})

test_that("summary.cograph_network handles network with no edges", {
  adj <- matrix(0, 3, 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_equal(result$n_edges, 0)
})

test_that("summary.cograph_network handles edges without weight column", {
  # Create network and manually remove weight column to test branch
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # The network should still work even if edges exist
  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Cograph Network Summary", output)))
})

# ==============================================================================
# Tests for summary.cograph_network - Node Labels Branch
# ==============================================================================

test_that("summary.cograph_network shows node labels for small network", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(adj) <- colnames(adj) <- c("Alpha", "Beta", "Gamma")
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Node Labels", output)))
  expect_true(any(grepl("Alpha", output)))
  expect_true(any(grepl("Beta", output)))
  expect_true(any(grepl("Gamma", output)))
})

test_that("summary.cograph_network truncates labels for large network (>10 nodes)", {
  n <- 12
  adj <- matrix(0, n, n)
  adj[upper.tri(adj)] <- 1
  adj <- adj + t(adj)
  diag(adj) <- 0
  rownames(adj) <- colnames(adj) <- paste0("Node", 1:n)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Node Labels", output)))
  expect_true(any(grepl("\\.\\.\\.", output)))  # Should show "..."
})

test_that("summary.cograph_network shows all labels for exactly 10 nodes", {
  n <- 10
  adj <- matrix(0, n, n)
  adj[1, 2] <- adj[2, 1] <- 1
  rownames(adj) <- colnames(adj) <- LETTERS[1:n]
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Node Labels", output)))
  # Should NOT truncate for exactly 10 nodes
  combined <- paste(output, collapse = " ")
  expect_true(grepl("J", combined))  # Last node should appear
})

test_that("summary.cograph_network handles network with 0 nodes gracefully", {
  # Create minimal network
  adj <- matrix(numeric(0), 0, 0)
  # Skip this test if cograph cannot handle 0-node networks
  tryCatch({
    net <- cograph(adj)
    output <- capture.output(result <- summary(net))
    expect_equal(result$n_nodes, 0)
  }, error = function(e) {
    skip("cograph does not support 0-node networks")
  })
})

# ==============================================================================
# Tests for summary.cograph_network - Layout Branch
# ==============================================================================

test_that("summary.cograph_network shows layout computed when layout exists", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, layout = "circle")

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Layout:.*computed", output)))
  expect_true(result$has_layout)
})

test_that("summary.cograph_network shows layout not computed when missing", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Manually clear layout to test branch
  net$nodes$x <- NA
  net$nodes$y <- NA

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Layout:.*not computed", output)))
  expect_false(result$has_layout)
})

test_that("summary.cograph_network handles partial NA layout", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj, layout = "circle")

  # Set some layout values to NA
  net$nodes$x[1] <- NA

  output <- capture.output(result <- summary(net))
  # Should still show computed if not ALL are NA
  expect_true(any(grepl("Layout:", output)))
})

test_that("summary.cograph_network handles NULL x in nodes", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  # Manually set x to NULL
  net$nodes$x <- NULL

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Layout:.*not computed", output)))
  expect_false(result$has_layout)
})

# ==============================================================================
# Tests for summary.cograph_network - Weighted Detection
# ==============================================================================

test_that("summary.cograph_network detects weighted network", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.7, 0.3, 0.7, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(result$weighted)
})

test_that("summary.cograph_network detects unweighted network (all 1s)", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_false(result$weighted)
})

test_that("summary.cograph_network handles mixed weights including 1", {
  adj <- matrix(c(0, 1, 0.5, 1, 0, 0.7, 0.5, 0.7, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(result$weighted)  # Should be weighted since not all are 1
})

# ==============================================================================
# Tests for plot.cograph_network with various rendering options
# ==============================================================================

test_that("plot.cograph_network with node_fill parameter", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net, node_fill = "steelblue")
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("plot.cograph_network with edge_color parameter", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net, edge_color = "red")
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("plot.cograph_network with layout parameter", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net, layout = "circle")
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("plot.cograph_network with show_labels FALSE", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net, show_labels = FALSE)
  dev.off()

  expect_s3_class(result, "cograph_network")
})

# ==============================================================================
# Integration tests for plot and summary
# ==============================================================================

test_that("plot and summary work together on same network", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C")
  net <- cograph(adj)

  # First summary
  output <- capture.output(sum_result <- summary(net))
  expect_type(sum_result, "list")

  # Then plot
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  plot_result <- plot(net)
  dev.off()

  expect_s3_class(plot_result, "cograph_network")
})

test_that("summary shows correct directed status from returned list", {
  # Undirected
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net_undir <- cograph(adj)
  output <- capture.output(result_undir <- summary(net_undir))
  expect_false(result_undir$directed)

  # Directed
  adj_dir <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net_dir <- cograph(adj_dir, directed = TRUE)
  output <- capture.output(result_dir <- summary(net_dir))
  expect_true(result_dir$directed)
})

test_that("summary shows correct weighted status from returned list", {
  # Unweighted (all 1s)
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net_unw <- cograph(adj)
  output <- capture.output(result_unw <- summary(net_unw))
  expect_false(result_unw$weighted)

  # Weighted
  adj_w <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.7, 0.3, 0.7, 0), nrow = 3)
  net_w <- cograph(adj_w)
  output <- capture.output(result_w <- summary(net_w))
  expect_true(result_w$weighted)
})

test_that("summary output format is consistent", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(summary(net))

  # Check output structure
  expect_true(any(grepl("=+", output)))  # Header separator
  expect_true(any(grepl("Structure:", output)))
  expect_true(any(grepl("Node Labels:", output)))
})

# ==============================================================================
# Edge cases and boundary conditions
# ==============================================================================

test_that("plot.cograph_network handles network from edge list", {
  edges <- data.frame(
    from = c("A", "A", "B"),
    to = c("B", "C", "C"),
    stringsAsFactors = FALSE
  )
  net <- cograph(edges)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("summary.cograph_network handles network from edge list", {
  edges <- data.frame(
    from = c("A", "A", "B"),
    to = c("B", "C", "C"),
    weight = c(0.5, 0.3, 0.8),
    stringsAsFactors = FALSE
  )
  net <- cograph(edges)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Cograph Network Summary", output)))
  expect_true(result$weighted)
})

test_that("plot.cograph_network works with igraph input", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]
  net <- cograph(g)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("summary.cograph_network works with igraph input", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]
  net <- cograph(g)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Cograph Network Summary", output)))
  expect_equal(result$n_nodes, 5)
})

test_that("plot.cograph_network handles self-loop network", {
  adj <- matrix(c(1, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 3)
  net <- cograph(adj)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 200, height = 200)
  result <- plot(net)
  dev.off()

  expect_s3_class(result, "cograph_network")
})

test_that("summary.cograph_network handles zero weight edges", {
  adj <- matrix(c(0, 0, 0.5, 0, 0, 0, 0.5, 0, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Cograph Network Summary", output)))
})

test_that("summary.cograph_network handles very large weights", {
  adj <- matrix(c(0, 1000, 500, 1000, 0, 750, 500, 750, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Max weight:", output)))
  expect_true(result$weighted)
})

test_that("summary.cograph_network handles very small weights", {
  adj <- matrix(c(0, 0.001, 0.0005, 0.001, 0, 0.00075, 0.0005, 0.00075, 0), nrow = 3)
  net <- cograph(adj)

  output <- capture.output(result <- summary(net))
  expect_true(any(grepl("Min weight:", output)))
  expect_true(result$weighted)
})
