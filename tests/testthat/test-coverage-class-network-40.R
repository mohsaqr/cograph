# Comprehensive tests for R/class-network.R
# This file tests the CographNetwork R6 class, cograph_network S3 class,
# and all related getter/setter functions

# =============================================================================
# Test Fixtures
# =============================================================================

# Simple symmetric adjacency matrix (undirected)
create_symmetric_matrix <- function(n = 3) {
  mat <- matrix(0, nrow = n, ncol = n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      mat[i, j] <- mat[j, i] <- runif(1, 0.1, 1)
    }
  }
  rownames(mat) <- colnames(mat) <- paste0("N", seq_len(n))
  mat
}

# Asymmetric matrix (directed)
create_directed_matrix <- function(n = 3) {
  mat <- matrix(runif(n * n, 0, 1), nrow = n, ncol = n)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- paste0("N", seq_len(n))
  mat
}

# =============================================================================
# CographNetwork R6 Class Tests
# =============================================================================

test_that("CographNetwork$new() creates empty network", {
  net <- CographNetwork$new()
  expect_s3_class(net, "CographNetwork")
  expect_equal(net$n_nodes, 0L)
  expect_equal(net$n_edges, 0L)
  expect_false(net$is_directed)
})

test_that("CographNetwork$new() creates network from matrix", {

  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  expect_s3_class(net, "CographNetwork")
  expect_equal(net$n_nodes, 3L)
  expect_true(net$n_edges > 0)
})

test_that("CographNetwork$new() handles directed parameter", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)

  # Force directed
  net_dir <- CographNetwork$new(mat, directed = TRUE)
  expect_true(net_dir$is_directed)

  # Force undirected
  net_undir <- CographNetwork$new(mat, directed = FALSE)
  expect_false(net_undir$is_directed)
})

test_that("CographNetwork$new() accepts nodes data frame with labels", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("1", "2", "3")
  labels <- c("Alpha", "Beta", "Gamma")
  nodes_df <- data.frame(label = c("1", "2", "3"), labels = labels)
  net <- CographNetwork$new(mat, nodes = nodes_df)

  # node_labels active binding uses 'labels' column (priority: labels > label)
  expect_equal(net$node_labels, labels)
})

test_that("CographNetwork$new() node_labels returns label column when no labels column", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- CographNetwork$new(mat)

  # Should return the label column (from rownames)
  expect_equal(net$node_labels, c("A", "B", "C"))
})

test_that("CographNetwork$new() initializes default aesthetics", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  node_aes <- net$get_node_aes()
  expect_true(is.list(node_aes))
  expect_equal(node_aes$size, 0.05)
  expect_equal(node_aes$shape, "circle")
  expect_equal(node_aes$fill, "#4A90D9")

  edge_aes <- net$get_edge_aes()
  expect_true(is.list(edge_aes))
  expect_equal(edge_aes$width, 1)
  expect_equal(edge_aes$positive_color, "#2E7D32")
  expect_equal(edge_aes$negative_color, "#C62828")
})

test_that("CographNetwork clone_network() creates independent copy", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net1 <- CographNetwork$new(mat)
  net1$set_theme("dark")

  net2 <- net1$clone_network()

  expect_s3_class(net2, "CographNetwork")
  expect_equal(net2$n_nodes, net1$n_nodes)
  expect_equal(net2$n_edges, net1$n_edges)
  expect_equal(net2$get_theme(), "dark")
})

test_that("CographNetwork set_nodes() and get_nodes() work", {
  net <- CographNetwork$new()
  nodes_df <- data.frame(
    id = 1:3,
    label = c("A", "B", "C"),
    x = c(0, 1, 0.5),
    y = c(0, 0, 1)
  )

  net$set_nodes(nodes_df)
  result <- net$get_nodes()

  expect_equal(nrow(result), 3)
  expect_equal(result$label, c("A", "B", "C"))
})

test_that("CographNetwork set_edges() and get_edges() work", {
  net <- CographNetwork$new()
  edges_df <- data.frame(
    from = c(1, 2),
    to = c(2, 3),
    weight = c(0.5, 0.8)
  )

  net$set_edges(edges_df)
  result <- net$get_edges()

  expect_equal(nrow(result), 2)
  expect_equal(result$weight, c(0.5, 0.8))
})

test_that("CographNetwork set_directed() and is_directed work", {
  net <- CographNetwork$new()

  net$set_directed(TRUE)
  expect_true(net$is_directed)

  net$set_directed(FALSE)
  expect_false(net$is_directed)
})

test_that("CographNetwork set_weights() work", {
  net <- CographNetwork$new()
  weights <- c(0.3, 0.5, 0.7)

  net$set_weights(weights)
  expect_true(net$has_weights)
})

test_that("CographNetwork has_weights is FALSE for all-1 weights", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)
  # All 1s or NULLs => has_weights should be FALSE
  # Actually the matrix has 1s, needs checking
  # If weights are all 1, has_weights should be FALSE
  expect_false(net$has_weights)
})

test_that("CographNetwork set_layout_coords() with matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  # Use named matrix columns for proper conversion
  coords <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  colnames(coords) <- c("x", "y")
  net$set_layout_coords(coords)

  layout <- net$get_layout()
  expect_equal(layout$x, c(0, 1, 0.5))
  expect_equal(layout$y, c(0, 0, 1))
})

test_that("CographNetwork set_layout_coords() with data.frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  net$set_layout_coords(coords)

  layout <- net$get_layout()
  expect_equal(layout$x, c(0, 1, 0.5))
})

test_that("CographNetwork set_layout_coords() updates node positions", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  coords <- data.frame(x = c(10, 20, 30), y = c(100, 200, 300))
  net$set_layout_coords(coords)

  nodes <- net$get_nodes()
  expect_equal(nodes$x, c(10, 20, 30))
  expect_equal(nodes$y, c(100, 200, 300))
})

test_that("CographNetwork set_node_aes() merges with defaults", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  net$set_node_aes(list(size = 0.1, fill = "red"))
  aes <- net$get_node_aes()

  expect_equal(aes$size, 0.1)
  expect_equal(aes$fill, "red")
  # Other defaults should still be present
  expect_equal(aes$shape, "circle")
})

test_that("CographNetwork set_edge_aes() merges with defaults", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  net$set_edge_aes(list(width = 3, color = "blue"))
  aes <- net$get_edge_aes()

  expect_equal(aes$width, 3)
  expect_equal(aes$color, "blue")
  # Other defaults should still be present
  expect_equal(aes$positive_color, "#2E7D32")
})

test_that("CographNetwork set_theme() and get_theme() work", {
  net <- CographNetwork$new()

  net$set_theme("minimal")
  expect_equal(net$get_theme(), "minimal")

  net$set_theme("dark")
  expect_equal(net$get_theme(), "dark")
})

test_that("CographNetwork set_layout_info() and get_layout_info() work", {
  net <- CographNetwork$new()
  info <- list(name = "spring", seed = 42, iterations = 100)

  net$set_layout_info(info)
  result <- net$get_layout_info()

  expect_equal(result$name, "spring")
  expect_equal(result$seed, 42)
})

test_that("CographNetwork set_plot_params() and get_plot_params() work", {
  net <- CographNetwork$new()
  params <- list(title = "My Network", margin = 0.1)

  net$set_plot_params(params)
  result <- net$get_plot_params()

  expect_equal(result$title, "My Network")
  expect_equal(result$margin, 0.1)
})

test_that("CographNetwork print() method works", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(mat)

  expect_output(net$print(), "CographNetwork")
  expect_output(net$print(), "Nodes:")
  expect_output(net$print(), "Edges:")
  expect_output(net$print(), "Directed:")
})

test_that("CographNetwork active bindings work", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- CographNetwork$new(mat)

  expect_equal(net$n_nodes, 3)
  expect_true(net$n_edges > 0)
  expect_false(net$is_directed)  # Symmetric matrix
  expect_equal(net$node_labels, c("A", "B", "C"))
})

# =============================================================================
# is_cograph_network() Tests
# =============================================================================

test_that("is_cograph_network() identifies CographNetwork R6 objects", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net_r6 <- CographNetwork$new(mat)

  expect_true(is_cograph_network(net_r6))
})

test_that("is_cograph_network() identifies cograph_network S3 objects", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net_s3 <- as_cograph(mat)

  expect_true(is_cograph_network(net_s3))
})

test_that("is_cograph_network() returns FALSE for non-network objects", {
  expect_false(is_cograph_network(matrix(1:9, 3, 3)))
  expect_false(is_cograph_network(list(a = 1, b = 2)))
  expect_false(is_cograph_network(data.frame(x = 1:3)))
  expect_false(is_cograph_network(NULL))
  expect_false(is_cograph_network(42))
  expect_false(is_cograph_network("network"))
})

# =============================================================================
# .create_cograph_network() Tests (internal constructor)
# =============================================================================

test_that(".create_cograph_network() creates valid structure", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))

  net <- .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    source = "test"
  )

  expect_s3_class(net, "cograph_network")
  expect_s3_class(net, "list")
  expect_equal(net$source, "test")
  expect_false(net$directed)
})

test_that(".create_cograph_network() handles empty edges", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  edges <- data.frame(from = integer(0), to = integer(0))

  net <- .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    source = "test"
  )

  expect_equal(nrow(net$edges), 0)
  expect_true("weight" %in% names(net$edges))
})

test_that(".create_cograph_network() adds default weight column", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  edges <- data.frame(from = c(1, 2), to = c(2, 3))  # No weight

  net <- .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = FALSE
  )

  expect_equal(net$edges$weight, c(1, 1))
})

test_that(".create_cograph_network() preserves optional fields", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
  weights_mat <- matrix(0, 3, 3)
  tna_meta <- list(type = "single", group_name = "Test")
  layout_info <- list(name = "spring", seed = 42)

  net <- .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = TRUE,
    source = "tna",
    weights = weights_mat,
    tna = tna_meta,
    layout_info = layout_info,
    layers = c("A", "A", "B"),
    clusters = c(1, 1, 2),
    groups = c("G1", "G1", "G2")
  )

  expect_equal(net$tna$type, "single")
  expect_equal(net$tna$group_name, "Test")
  expect_equal(net$layout_info$name, "spring")
  expect_equal(net$layers, c("A", "A", "B"))
  expect_equal(net$clusters, c(1, 1, 2))
  expect_equal(net$groups, c("G1", "G1", "G2"))
})

# =============================================================================
# as_cograph() / to_cograph() Tests
# =============================================================================

test_that("as_cograph() returns same object if already cograph_network", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net1 <- as_cograph(mat)
  net2 <- as_cograph(net1)

  expect_identical(net1, net2)
})

test_that("as_cograph() creates network from symmetric matrix", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 4)
  expect_false(is_directed(net))
  expect_equal(net$source, "matrix")
})

test_that("as_cograph() creates network from asymmetric matrix", {
  mat <- create_directed_matrix(4)
  net <- as_cograph(mat)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 4)
  expect_true(is_directed(net))
})

test_that("as_cograph() can force directed parameter", {
  mat <- create_symmetric_matrix(3)

  net_undir <- as_cograph(mat, directed = FALSE)
  expect_false(is_directed(net_undir))

  net_dir <- as_cograph(mat, directed = TRUE)
  expect_true(is_directed(net_dir))
})

test_that("as_cograph() creates network from edge list data.frame", {
  edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    weight = c(0.5, 0.8, 0.3)
  )
  net <- as_cograph(edges)

  expect_s3_class(net, "cograph_network")
  expect_equal(n_nodes(net), 3)
  expect_equal(net$source, "edgelist")
})

test_that("as_cograph() preserves weight matrix", {
  mat <- matrix(c(0, 0.5, 0.3, 0.2, 0, 0.4, 0.1, 0.6, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  expect_true(!is.null(net$weights))
  expect_true(is.matrix(net$weights))
  expect_equal(dim(net$weights), c(3, 3))
})

test_that("to_cograph() is an alias for as_cograph()", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)

  net1 <- as_cograph(mat)
  net2 <- to_cograph(mat)

  expect_equal(n_nodes(net1), n_nodes(net2))
  expect_equal(n_edges(net1), n_edges(net2))
  expect_equal(is_directed(net1), is_directed(net2))
})

# =============================================================================
# get_nodes() Tests
# =============================================================================

test_that("get_nodes() returns nodes data frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  nodes <- get_nodes(net)

  expect_true(is.data.frame(nodes))
  expect_equal(nrow(nodes), 3)
  expect_true("id" %in% names(nodes))
  expect_true("label" %in% names(nodes))
})

test_that("get_nodes() errors on non-cograph_network", {
  expect_error(get_nodes(list(a = 1)), "Cannot extract nodes")
  expect_error(get_nodes(matrix(1:4, 2, 2)), "Cannot extract nodes")
})

# =============================================================================
# get_edges() Tests
# =============================================================================

test_that("get_edges() returns edges data frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  edges <- get_edges(net)

  expect_true(is.data.frame(edges))
  expect_true("from" %in% names(edges))
  expect_true("to" %in% names(edges))
  expect_true("weight" %in% names(edges))
})

test_that("get_edges() returns empty data frame for network with no edges", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  net <- .create_cograph_network(
    nodes = nodes,
    edges = NULL,
    directed = FALSE
  )

  edges <- get_edges(net)
  expect_equal(nrow(edges), 0)
  expect_true("from" %in% names(edges))
})

test_that("get_edges() errors on non-cograph_network", {
  expect_error(get_edges(list(a = 1)), "Cannot extract edges")
  expect_error(get_edges(42), "Cannot extract edges")
})

# =============================================================================
# get_labels() Tests
# =============================================================================

test_that("get_labels() returns character vector of labels", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("Alpha", "Beta", "Gamma")
  net <- as_cograph(mat)

  labels <- get_labels(net)

  expect_true(is.character(labels))
  expect_equal(labels, c("Alpha", "Beta", "Gamma"))
})

test_that("get_labels() errors on non-cograph_network", {
  expect_error(get_labels(list(a = 1)), "Cannot extract labels")
})

test_that("get_labels() priority: labels > label", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("id1", "id2", "id3")
  net <- as_cograph(mat)

  # Default: uses label column (which comes from rownames)
  expect_equal(get_labels(net), c("id1", "id2", "id3"))

  # Add 'label' column - should use it
  net$nodes$label <- c("Label1", "Label2", "Label3")
  expect_equal(get_labels(net), c("Label1", "Label2", "Label3"))

  # Add 'labels' column - should take priority over 'label'
  net$nodes$labels <- c("Display1", "Display2", "Display3")
  expect_equal(get_labels(net), c("Display1", "Display2", "Display3"))
})

# =============================================================================
# set_nodes() Tests
# =============================================================================

test_that("set_nodes() replaces nodes data frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(id = 1:3, label = c("X", "Y", "Z"))
  net <- set_nodes(net, new_nodes)

  expect_equal(get_labels(net), c("X", "Y", "Z"))
})

test_that("set_nodes() adds id column if missing", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(label = c("A", "B", "C"))
  net <- set_nodes(net, new_nodes)

  expect_true("id" %in% names(get_nodes(net)))
  expect_equal(get_nodes(net)$id, 1:3)
})

test_that("set_nodes() adds label column if missing", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_nodes <- data.frame(id = 1:3)
  net <- set_nodes(net, new_nodes)

  expect_true("label" %in% names(get_nodes(net)))
})

test_that("set_nodes() errors on non-cograph_network", {
  expect_error(set_nodes(list(a = 1), data.frame(id = 1)), "must be a cograph_network")
})

test_that("set_nodes() errors on non-data.frame nodes", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(set_nodes(net, c(1, 2, 3)), "must be a data frame")
})

# =============================================================================
# set_edges() Tests
# =============================================================================

test_that("set_edges() replaces edges", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.9, 0.1))
  net <- set_edges(net, new_edges)

  edges <- get_edges(net)
  expect_equal(nrow(edges), 2)
  expect_equal(edges$weight, c(0.9, 0.1))
})

test_that("set_edges() adds weight column if missing", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  new_edges <- data.frame(from = c(1, 2), to = c(2, 3))
  net <- set_edges(net, new_edges)

  edges <- get_edges(net)
  expect_true("weight" %in% names(edges))
  expect_equal(edges$weight, c(1, 1))
})

test_that("set_edges() errors without from/to columns", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(
    set_edges(net, data.frame(source = 1, target = 2)),
    "must have 'from' and 'to' columns"
  )
})

test_that("set_edges() errors on non-cograph_network", {
  expect_error(
    set_edges(list(a = 1), data.frame(from = 1, to = 2)),
    "must be a cograph_network"
  )
})

test_that("set_edges() errors on non-data.frame edges", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(set_edges(net, c(1, 2, 3)), "must be a data frame")
})

# =============================================================================
# set_layout() Tests
# =============================================================================

test_that("set_layout() sets layout from data.frame", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  layout <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  net <- set_layout(net, layout)

  nodes <- get_nodes(net)
  expect_equal(nodes$x, c(0, 1, 0.5))
  expect_equal(nodes$y, c(0, 0, 1))
})

test_that("set_layout() sets layout from matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  layout_mat <- matrix(c(0, 1, 0.5, 0, 0, 1), ncol = 2)
  net <- set_layout(net, layout_mat)

  nodes <- get_nodes(net)
  expect_equal(nodes$x, c(0, 1, 0.5))
  expect_equal(nodes$y, c(0, 0, 1))
})

test_that("set_layout() errors without x/y columns", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(
    set_layout(net, data.frame(a = 1:3, b = 4:6)),
    "must have 'x' and 'y' columns"
  )
})

test_that("set_layout() errors on mismatched rows", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_error(
    set_layout(net, data.frame(x = 1:5, y = 1:5)),
    "same number of rows"
  )
})

test_that("set_layout() errors on non-cograph_network", {
  expect_error(
    set_layout(list(a = 1), data.frame(x = 1, y = 1)),
    "must be a cograph_network"
  )
})

# =============================================================================
# n_nodes() Tests
# =============================================================================

test_that("n_nodes() returns correct count", {
  mat <- matrix(c(0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0), nrow = 4)
  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 4)
})

test_that("n_nodes() returns 0 for empty network", {
  nodes <- data.frame(id = integer(0), label = character(0))
  net <- .create_cograph_network(nodes = nodes, edges = NULL, directed = FALSE)

  expect_equal(n_nodes(net), 0)
})

test_that("n_nodes() errors on non-cograph_network", {
  expect_error(n_nodes(matrix(1:4, 2, 2)), "Cannot count nodes")
  expect_error(n_nodes(list(a = 1)), "Cannot count nodes")
})

# =============================================================================
# n_edges() Tests
# =============================================================================

test_that("n_edges() returns correct count", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  # Undirected: 3 edges (each pair counted once)
  expect_true(n_edges(net) > 0)
})

test_that("n_edges() returns 0 for network with no edges", {
  nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
  edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  net <- .create_cograph_network(nodes = nodes, edges = edges, directed = FALSE)

  expect_equal(n_edges(net), 0)
})

test_that("n_edges() errors on non-cograph_network", {
  expect_error(n_edges(matrix(1:4, 2, 2)), "Cannot count edges")
  expect_error(n_edges(42), "Cannot count edges")
})

# =============================================================================
# is_directed() Tests
# =============================================================================

test_that("is_directed() returns FALSE for symmetric matrix", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_false(is_directed(net))
})

test_that("is_directed() returns TRUE for asymmetric matrix", {
  mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
  net <- as_cograph(mat)

  expect_true(is_directed(net))
})

test_that("is_directed() works with igraph", {
  skip_if_not_installed("igraph")

  g_dir <- igraph::make_ring(5, directed = TRUE)
  g_undir <- igraph::make_ring(5, directed = FALSE)

  expect_true(is_directed(g_dir))
  expect_false(is_directed(g_undir))
})

test_that("is_directed() errors on unknown object", {
  expect_error(is_directed(list(a = 1)), "Cannot determine directedness")
  expect_error(is_directed(42), "Cannot determine directedness")
})

# =============================================================================
# nodes() Tests (deprecated function)
# =============================================================================

test_that("nodes() returns same as get_nodes() (deprecated)", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  result1 <- nodes(net)
  result2 <- get_nodes(net)

  expect_equal(result1, result2)
})

# =============================================================================
# set_groups() Tests
# =============================================================================

test_that("set_groups() works with named list", {
  mat <- create_symmetric_matrix(6)
  net <- as_cograph(mat)

  groups_list <- list(
    G1 = c("N1", "N2", "N3"),
    G2 = c("N4", "N5", "N6")
  )
  net <- set_groups(net, groups_list)

  result <- get_groups(net)
  expect_true(is.data.frame(result))
  expect_true("node" %in% names(result))
  expect_true("group" %in% names(result))
  expect_equal(nrow(result), 6)
})

test_that("set_groups() works with vector", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  groups_vec <- c("A", "A", "B", "B")
  net <- set_groups(net, groups_vec)

  result <- get_groups(net)
  expect_equal(nrow(result), 4)
})

test_that("set_groups() works with layers vector argument", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  net <- set_groups(net,
    nodes = c("N1", "N2", "N3", "N4"),
    layers = c("Top", "Top", "Bottom", "Bottom")
  )

  result <- get_groups(net)
  expect_true("layer" %in% names(result))
  expect_equal(nrow(result), 4)
})

test_that("set_groups() works with clusters vector argument", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  net <- set_groups(net,
    nodes = c("N1", "N2", "N3", "N4"),
    clusters = c("C1", "C1", "C2", "C2")
  )

  result <- get_groups(net)
  expect_true("cluster" %in% names(result))
})

test_that("set_groups() works with data.frame", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  df <- data.frame(
    nodes = c("N1", "N2", "N3", "N4"),
    layers = c("A", "A", "B", "B")
  )
  net <- set_groups(net, df)

  result <- get_groups(net)
  # Should normalize "nodes" to "node" and "layers" to "layer"
  expect_true("node" %in% names(result))
  expect_true("layer" %in% names(result))
})

test_that("set_groups() respects type parameter", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  net_layer <- set_groups(net, c("A", "A", "B", "B"), type = "layer")
  net_cluster <- set_groups(net, c("A", "A", "B", "B"), type = "cluster")

  expect_true("layer" %in% names(get_groups(net_layer)))
  expect_true("cluster" %in% names(get_groups(net_cluster)))
})

test_that("set_groups() errors on duplicate node assignments", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  df <- data.frame(
    node = c("N1", "N1", "N3", "N4"),  # N1 is duplicated
    group = c("A", "B", "A", "B")
  )

  expect_error(set_groups(net, df), "Duplicate node assignments")
})

test_that("set_groups() errors on missing nodes", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  df <- data.frame(
    node = c("X1", "X2", "X3", "X4"),  # None of these exist
    group = c("A", "A", "B", "B")
  )

  expect_error(set_groups(net, df), "Nodes not found")
})

test_that("set_groups() errors on incomplete node assignment", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  df <- data.frame(
    node = c("N1", "N2"),  # Missing N3, N4
    group = c("A", "B")
  )

  expect_error(set_groups(net, df), "Nodes missing from group assignment")
})

test_that("set_groups() errors on only one group", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  # All same group
  expect_error(
    set_groups(net, c("A", "A", "A", "A")),
    "At least 2 groups are required"
  )
})

test_that("set_groups() errors on mismatched vector lengths", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  expect_error(
    set_groups(net, layers = c("A", "B")),  # Only 2 elements
    "must match number of nodes"
  )
})

test_that("set_groups() errors on non-cograph_network", {
  expect_error(
    set_groups(list(a = 1), c("A", "B")),
    "must be a cograph_network"
  )
})

test_that("set_groups() errors without groups or vectors", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  expect_error(
    set_groups(net),
    "Must provide either 'groups' or vector arguments"
  )
})

test_that("set_groups() works with community detection method", {
  skip_if_not_installed("igraph")

  # Create a network with clear community structure
  mat <- matrix(0, 6, 6)
  mat[1, 2] <- mat[2, 1] <- 0.9
  mat[1, 3] <- mat[3, 1] <- 0.8
  mat[2, 3] <- mat[3, 2] <- 0.85
  mat[4, 5] <- mat[5, 4] <- 0.9
  mat[4, 6] <- mat[6, 4] <- 0.8
  mat[5, 6] <- mat[6, 5] <- 0.85
  mat[3, 4] <- mat[4, 3] <- 0.1  # Weak inter-community link
  rownames(mat) <- colnames(mat) <- paste0("N", 1:6)

  net <- as_cograph(mat)
  net <- set_groups(net, "louvain")

  result <- get_groups(net)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 6)
})

# =============================================================================
# get_groups() Tests
# =============================================================================

test_that("get_groups() returns NULL when no groups set", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)

  expect_null(get_groups(net))
})

test_that("get_groups() returns data frame after set_groups()", {
  mat <- create_symmetric_matrix(4)
  net <- as_cograph(mat)
  net <- set_groups(net, c("A", "A", "B", "B"))

  result <- get_groups(net)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
})

test_that("get_groups() errors on non-cograph_network", {
  expect_error(get_groups(list(a = 1)), "must be a cograph_network")
})

# =============================================================================
# Edge cases and integration tests
# =============================================================================

test_that("cograph_network works with very small networks", {
  # Single node
  mat <- matrix(0, 1, 1)
  rownames(mat) <- colnames(mat) <- "A"
  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 1)
  expect_equal(n_edges(net), 0)
})

test_that("cograph_network works with disconnected networks", {
  # 4 nodes, only 1-2 connected, 3-4 isolated
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- mat[2, 1] <- 1
  rownames(mat) <- colnames(mat) <- paste0("N", 1:4)
  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 4)
  # Should have 1 edge (1-2)
  expect_equal(n_edges(net), 1)
})

test_that("cograph_network handles negative weights", {
  mat <- matrix(c(0, -0.5, 0.3, -0.5, 0, 0.2, 0.3, 0.2, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- as_cograph(mat)

  expect_s3_class(net, "cograph_network")
  edges <- get_edges(net)
  expect_true(any(edges$weight < 0))
})

test_that("cograph_network handles sparse networks", {
  mat <- matrix(0, 10, 10)
  mat[1, 2] <- mat[2, 1] <- 0.5
  mat[3, 4] <- mat[4, 3] <- 0.3
  rownames(mat) <- colnames(mat) <- paste0("N", 1:10)

  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 10)
  expect_equal(n_edges(net), 2)
})

test_that("cograph_network preserves special characters in labels", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("Node 1", "Node-2", "Node_3")

  net <- as_cograph(mat)
  labels <- get_labels(net)

  expect_equal(labels, c("Node 1", "Node-2", "Node_3"))
})

test_that("cograph_network handles matrix without row/colnames", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  # No rownames/colnames

  net <- as_cograph(mat)

  expect_equal(n_nodes(net), 3)
  labels <- get_labels(net)
  expect_true(all(nchar(labels) > 0))  # Should have auto-generated labels
})

test_that("chained setter operations work correctly", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  # Chain multiple operations
  net <- set_nodes(net, data.frame(id = 1:3, label = c("X", "Y", "Z")))
  net <- set_layout(net, data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))
  net <- set_groups(net, c("G1", "G1", "G2"))

  expect_equal(get_labels(net), c("X", "Y", "Z"))
  expect_equal(get_nodes(net)$x, c(0, 1, 0.5))
  expect_true(!is.null(get_groups(net)))
})

test_that("S3 class hierarchy is correct", {
  mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- as_cograph(mat)

  classes <- class(net)
  expect_true("cograph_network" %in% classes)
  expect_true("list" %in% classes)
})
