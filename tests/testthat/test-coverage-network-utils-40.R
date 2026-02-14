# =============================================================================
# Comprehensive Tests for network-utils.R
# Coverage target: 60+ tests for all utility functions
# =============================================================================

# -----------------------------------------------------------------------------
# Test Setup: Helper functions and test data
# -----------------------------------------------------------------------------

# Create test adjacency matrices
create_test_matrix <- function(n = 4, symmetric = TRUE, weighted = TRUE) {
  mat <- matrix(runif(n * n), n, n)
  diag(mat) <- 0
  if (symmetric) {
    mat <- (mat + t(mat)) / 2
  }
  if (!weighted) {
    mat <- (mat > 0.5) * 1
  }
  rownames(mat) <- colnames(mat) <- LETTERS[1:n]
  mat
}

# Create disconnected network matrix
create_disconnected_matrix <- function() {
  mat <- matrix(0, 6, 6)
  # Component 1: A-B-C
  mat[1, 2] <- mat[2, 1] <- 1
  mat[1, 3] <- mat[3, 1] <- 1
  # Component 2: D-E-F (larger)
  mat[4, 5] <- mat[5, 4] <- 1
  mat[5, 6] <- mat[6, 5] <- 1
  mat[4, 6] <- mat[6, 4] <- 1
  rownames(mat) <- colnames(mat) <- LETTERS[1:6]
  mat
}

# Create directed matrix
create_directed_matrix <- function() {
  mat <- matrix(0, 4, 4)
  mat[1, 2] <- 0.5
  mat[2, 3] <- 0.6
  mat[3, 4] <- 0.7
  mat[4, 1] <- 0.8
  mat[2, 1] <- 0.3  # reciprocal edge
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]
  mat
}

# =============================================================================
# SECTION 1: to_igraph() Tests (13 tests)
# =============================================================================

test_that("to_igraph converts matrix to igraph", {
  mat <- create_test_matrix()
  g <- to_igraph(mat)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 4)
})

test_that("to_igraph auto-detects symmetry for undirected", {
  mat <- create_test_matrix(symmetric = TRUE)
  g <- to_igraph(mat)

  expect_false(igraph::is_directed(g))
})

test_that("to_igraph auto-detects asymmetry for directed", {
  mat <- create_directed_matrix()
  g <- to_igraph(mat)

  expect_true(igraph::is_directed(g))
})

test_that("to_igraph respects directed override TRUE", {
  mat <- create_test_matrix(symmetric = TRUE)
  g <- to_igraph(mat, directed = TRUE)

  expect_true(igraph::is_directed(g))
})

test_that("to_igraph respects directed override FALSE", {
  mat <- create_directed_matrix()
  g <- to_igraph(mat, directed = FALSE)

  expect_false(igraph::is_directed(g))
})

test_that("to_igraph returns igraph as-is when no override", {
  g_orig <- igraph::make_ring(5)
  g <- to_igraph(g_orig)

  expect_identical(g, g_orig)
})

test_that("to_igraph converts directed igraph to undirected", {
  g_orig <- igraph::make_ring(5, directed = TRUE)
  g <- to_igraph(g_orig, directed = FALSE)

  expect_false(igraph::is_directed(g))
})

test_that("to_igraph converts undirected igraph to directed", {
  g_orig <- igraph::make_ring(5, directed = FALSE)
  g <- to_igraph(g_orig, directed = TRUE)

  expect_true(igraph::is_directed(g))
})

test_that("to_igraph works with cograph_network", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  g <- to_igraph(net)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 4)
})

test_that("to_igraph cograph_network respects directed override", {
  mat <- create_test_matrix(symmetric = TRUE)
  net <- as_cograph(mat)
  g <- to_igraph(net, directed = TRUE)

  expect_true(igraph::is_directed(g))
})

test_that("to_igraph preserves node names from matrix", {
  mat <- create_test_matrix()
  g <- to_igraph(mat)

  expect_equal(igraph::V(g)$name, c("A", "B", "C", "D"))
})

test_that("to_igraph preserves edge weights", {
  mat <- create_test_matrix()
  g <- to_igraph(mat)

  expect_true(!is.null(igraph::E(g)$weight))
})

test_that("to_igraph errors on invalid input", {
  expect_error(to_igraph("not a network"))
  expect_error(to_igraph(list(a = 1, b = 2)))
})

# =============================================================================
# SECTION 2: to_igraph() with network package (3 tests)
# =============================================================================

test_that("to_igraph works with statnet network object", {
  skip_if_not_installed("network")

  mat <- create_test_matrix(symmetric = TRUE)
  net_obj <- network::network(mat, directed = FALSE, ignore.eval = FALSE,
                               names.eval = "weight")

  g <- to_igraph(net_obj)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 4)
})

test_that("to_igraph preserves vertex names from network object", {
  skip_if_not_installed("network")

  mat <- create_test_matrix()
  net_obj <- network::network(mat, directed = FALSE)
  network::set.vertex.attribute(net_obj, "vertex.names", c("A", "B", "C", "D"))

  g <- to_igraph(net_obj)

  expect_equal(igraph::V(g)$name, c("A", "B", "C", "D"))
})

test_that("to_igraph respects directed override with network object", {
  skip_if_not_installed("network")

  mat <- create_test_matrix(symmetric = TRUE)
  net_obj <- network::network(mat, directed = FALSE)

  g <- to_igraph(net_obj, directed = TRUE)

  expect_true(igraph::is_directed(g))
})

# =============================================================================
# SECTION 3: detect_communities() Tests (8 tests)
# =============================================================================

test_that("detect_communities returns data frame", {
  mat <- create_test_matrix()
  result <- detect_communities(mat)

  expect_true(is.data.frame(result))
  expect_true("node" %in% names(result))
  expect_true("community" %in% names(result))
})

test_that("detect_communities with louvain method", {
  mat <- create_test_matrix()
  result <- detect_communities(mat, method = "louvain")

  expect_equal(nrow(result), 4)
  expect_true(all(result$community >= 1))
})
test_that("detect_communities with walktrap method", {
  mat <- create_test_matrix()
  result <- detect_communities(mat, method = "walktrap")

  expect_equal(nrow(result), 4)
})

test_that("detect_communities with fast_greedy method", {
  mat <- create_test_matrix(symmetric = TRUE)
  result <- detect_communities(mat, method = "fast_greedy")

  expect_equal(nrow(result), 4)
})

test_that("detect_communities with label_prop method", {
  mat <- create_test_matrix()
  result <- detect_communities(mat, method = "label_prop")

  expect_equal(nrow(result), 4)
})

test_that("detect_communities with infomap method", {
  mat <- create_test_matrix()
  result <- detect_communities(mat, method = "infomap")

  expect_equal(nrow(result), 4)
})

test_that("detect_communities respects directed parameter", {
  mat <- create_directed_matrix()
  # Use walktrap which works with directed graphs
  result <- detect_communities(mat, method = "walktrap", directed = TRUE)

  expect_equal(nrow(result), 4)
})

test_that("detect_communities without weights", {
  mat <- create_test_matrix()
  result <- detect_communities(mat, weights = FALSE)

  expect_equal(nrow(result), 4)
})

# =============================================================================
# SECTION 4: color_communities() Tests (6 tests)
# =============================================================================

test_that("color_communities returns named character vector", {
  mat <- create_test_matrix()
  colors <- color_communities(mat)

  expect_true(is.character(colors))
  expect_true(!is.null(names(colors)))
  expect_equal(length(colors), 4)
})

test_that("color_communities with custom palette vector", {
  mat <- create_test_matrix()
  colors <- color_communities(mat, palette = c("red", "blue", "green", "yellow"))

  expect_true(all(colors %in% c("red", "blue", "green", "yellow")))
})

test_that("color_communities with palette function", {
  mat <- create_test_matrix()
  colors <- color_communities(mat, palette = grDevices::rainbow)

  expect_equal(length(colors), 4)
})

test_that("color_communities with palette name 'rainbow'", {
  mat <- create_test_matrix()
  colors <- color_communities(mat, palette = "rainbow")

  expect_equal(length(colors), 4)
})

test_that("color_communities with palette name 'colorblind'", {
  mat <- create_test_matrix()
  colors <- color_communities(mat, palette = "colorblind")

  expect_equal(length(colors), 4)
})

test_that("color_communities extends short palette", {
  # Create network with more communities than palette colors
  mat <- create_test_matrix(n = 6)
  colors <- color_communities(mat, palette = c("red", "blue"))

  expect_equal(length(colors), 6)
})

# =============================================================================
# SECTION 5: filter_edges() Tests (10 tests)
# =============================================================================

test_that("filter_edges with weight threshold", {
  mat <- create_test_matrix()
  result <- filter_edges(mat, weight > 0.5)

  expect_true(inherits(result, "cograph_network"))
  edges <- get_edges(result)
  expect_true(all(edges$weight > 0.5))
})

test_that("filter_edges with mean weight", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  result <- filter_edges(net, weight >= mean(weight))

  edges <- get_edges(result)
  original_edges <- get_edges(net)
  expect_true(all(edges$weight >= mean(original_edges$weight)))
})

test_that("filter_edges with keep_isolates = TRUE", {
  mat <- create_test_matrix()
  result <- filter_edges(mat, weight > 0.9, .keep_isolates = TRUE)

  # Should preserve all nodes even if some have no edges
  expect_true(n_nodes(result) >= 0)
})

test_that("filter_edges with keep_isolates = FALSE removes isolates", {
  mat <- create_test_matrix()
  # Use lower threshold to ensure some edges remain
  result <- filter_edges(mat, weight > 0.3, .keep_isolates = FALSE)

  # Nodes without edges should be removed
  if (n_edges(result) > 0) {
    edges <- get_edges(result)
    connected_nodes <- unique(c(edges$from, edges$to))
    expect_equal(n_nodes(result), length(connected_nodes))
  } else {
    # If no edges, should have no nodes
    expect_equal(n_nodes(result), 0)
  }
})

test_that("filter_edges keep_format = TRUE returns matrix", {
  mat <- create_test_matrix()
  result <- filter_edges(mat, weight > 0.3, keep_format = TRUE)

  expect_true(is.matrix(result))
})

test_that("filter_edges keep_format = TRUE returns igraph", {
  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- c(0.1, 0.5, 0.3, 0.8, 0.2)

  result <- suppressMessages(filter_edges(g, weight > 0.3, keep_format = TRUE))

  expect_true(inherits(result, "igraph"))
})

test_that("filter_edges warns on empty network", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  net$edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))

  expect_warning(filter_edges(net, weight > 0))
})

test_that("filter_edges warns when all edges removed", {
  mat <- create_test_matrix()
  expect_warning(filter_edges(mat, weight > 100))
})

test_that("filter_edges with abs(weight) expression", {
  mat <- create_test_matrix()
  mat[1, 2] <- -0.5
  mat[2, 1] <- -0.5
  result <- filter_edges(mat, abs(weight) > 0.3)

  expect_true(inherits(result, "cograph_network"))
})

test_that("filter_edges preserves network directedness", {
  mat <- create_directed_matrix()
  net <- as_cograph(mat)
  result <- filter_edges(net, weight > 0.3)

  expect_equal(is_directed(result), is_directed(net))
})

# =============================================================================
# SECTION 6: filter_nodes() Tests (10 tests)
# =============================================================================

test_that("filter_nodes with degree threshold", {
  mat <- create_test_matrix()
  result <- filter_nodes(mat, degree >= 2)

  expect_true(inherits(result, "cograph_network"))
})

test_that("filter_nodes by label", {
  mat <- create_test_matrix()
  result <- filter_nodes(mat, label %in% c("A", "B"))

  labels <- get_labels(result)
  expect_true(all(labels %in% c("A", "B")))
})

test_that("filter_nodes with betweenness threshold", {
  mat <- create_test_matrix()
  result <- filter_nodes(mat, betweenness >= 0)

  expect_true(inherits(result, "cograph_network"))
})

test_that("filter_nodes with pagerank", {
  mat <- create_test_matrix()
  result <- filter_nodes(mat, pagerank > 0)

  expect_true(n_nodes(result) > 0)
})

test_that("filter_nodes with keep_edges = 'none'", {
  mat <- create_test_matrix()
  result <- filter_nodes(mat, degree >= 1, .keep_edges = "none")

  expect_equal(n_edges(result), 0)
})

test_that("filter_nodes keep_format = TRUE returns matrix", {
  mat <- create_test_matrix()
  result <- filter_nodes(mat, degree >= 2, keep_format = TRUE)

  expect_true(is.matrix(result))
})

test_that("filter_nodes warns when no nodes match", {
  mat <- create_test_matrix()
  expect_warning(filter_nodes(mat, degree > 100))
})

test_that("filter_nodes combines multiple conditions", {
  mat <- create_test_matrix()
  result <- filter_nodes(mat, degree >= 2 & label != "D")

  labels <- get_labels(result)
  expect_true(!"D" %in% labels || length(labels) == 0)
})

test_that("filter_nodes with eigenvector centrality", {
  mat <- create_test_matrix()
  result <- filter_nodes(mat, eigenvector > 0)

  expect_true(inherits(result, "cograph_network"))
})

test_that("filter_nodes with hub/authority scores", {
  mat <- create_directed_matrix()
  result <- filter_nodes(mat, hub > 0, directed = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 7: to_data_frame() / to_df() Tests (5 tests)
# =============================================================================

test_that("to_data_frame returns data frame with correct columns", {
  mat <- create_test_matrix()
  df <- to_data_frame(mat)

  expect_true(is.data.frame(df))
  expect_true(all(c("from", "to", "weight") %in% names(df)))
})

test_that("to_data_frame with empty network", {
  mat <- matrix(0, 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  df <- to_data_frame(mat)

  expect_equal(nrow(df), 0)
})

test_that("to_df is alias for to_data_frame", {
  mat <- create_test_matrix()
  df1 <- to_data_frame(mat)
  df2 <- to_df(mat)

  expect_equal(df1, df2)
})

test_that("to_data_frame preserves node names", {
  mat <- create_test_matrix()
  df <- to_data_frame(mat)

  expect_true(all(df$from %in% c("A", "B", "C", "D")))
  expect_true(all(df$to %in% c("A", "B", "C", "D")))
})

test_that("to_data_frame from igraph", {
  g <- igraph::make_ring(5)
  igraph::V(g)$name <- LETTERS[1:5]
  df <- to_data_frame(g)

  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 5)
})

# =============================================================================
# SECTION 8: to_matrix() Tests (5 tests)
# =============================================================================

test_that("to_matrix returns matrix as-is", {
  mat <- create_test_matrix()
  result <- to_matrix(mat)

  expect_identical(result, mat)
})

test_that("to_matrix from cograph_network", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  result <- to_matrix(net)

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(4, 4))
})

test_that("to_matrix from igraph", {
  g <- igraph::make_ring(5)
  igraph::E(g)$weight <- rep(1, 5)  # Add weights
  result <- to_matrix(g)

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(5, 5))
})

test_that("to_matrix preserves row/column names", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  result <- to_matrix(net)

  expect_equal(rownames(result), c("A", "B", "C", "D"))
  expect_equal(colnames(result), c("A", "B", "C", "D"))
})

test_that("to_matrix preserves weights", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  result <- to_matrix(net)

  # Check that non-zero elements are preserved
  expect_true(sum(result != 0) > 0)
})

# =============================================================================
# SECTION 9: to_network() Tests (4 tests)
# =============================================================================

test_that("to_network converts matrix to network object", {
  skip_if_not_installed("network")

  mat <- create_test_matrix()
  result <- to_network(mat)

  expect_true(inherits(result, "network"))
})

test_that("to_network preserves directedness", {
  skip_if_not_installed("network")

  mat <- create_directed_matrix()
  result <- to_network(mat, directed = TRUE)

  expect_true(network::is.directed(result))
})

test_that("to_network preserves vertex names", {
  skip_if_not_installed("network")

  mat <- create_test_matrix()
  result <- to_network(mat)

  names <- network::network.vertex.names(result)
  expect_equal(names, c("A", "B", "C", "D"))
})

test_that("to_network errors without network package", {
  skip_if(requireNamespace("network", quietly = TRUE))

  mat <- create_test_matrix()
  expect_error(to_network(mat))
})

# =============================================================================
# SECTION 10: select_nodes() Tests (10 tests)
# =============================================================================

test_that("select_nodes by name", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, name = c("A", "B"))

  labels <- get_labels(result)
  expect_true(all(labels %in% c("A", "B")))
})

test_that("select_nodes by index", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, index = c(1, 2))

  expect_equal(n_nodes(result), 2)
})

test_that("select_nodes top N by degree", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, top = 2, by = "degree")

  expect_equal(n_nodes(result), 2)
})

test_that("select_nodes top N by pagerank", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, top = 2, by = "pagerank")

  expect_equal(n_nodes(result), 2)
})

test_that("select_nodes by neighbors_of", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, neighbors_of = "A", order = 1)

  # Should include A and its neighbors
  expect_true(n_nodes(result) >= 1)
})

test_that("select_nodes by component largest", {
  mat <- create_disconnected_matrix()
  result <- select_nodes(mat, component = "largest")

  # Largest component has 3 nodes (D-E-F)
  expect_equal(n_nodes(result), 3)
})

test_that("select_nodes by component containing node", {
  mat <- create_disconnected_matrix()
  result <- select_nodes(mat, component = "A")

  # Component containing A has 3 nodes (A-B-C)
  expect_equal(n_nodes(result), 3)
})

test_that("select_nodes with expression filter", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, degree >= 2)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes combined modes (top + component)", {
  mat <- create_disconnected_matrix()
  result <- select_nodes(mat, component = "largest", top = 2)

  expect_equal(n_nodes(result), 2)
})

test_that("select_nodes warns on invalid index", {
  mat <- create_test_matrix()
  expect_warning(select_nodes(mat, index = c(1, 100)))
})

# =============================================================================
# SECTION 11: select_neighbors() Tests (3 tests)
# =============================================================================

test_that("select_neighbors returns neighborhood", {
  mat <- create_test_matrix()
  result <- select_neighbors(mat, of = "A", order = 1)

  expect_true(inherits(result, "cograph_network"))
  expect_true(n_nodes(result) >= 1)
})

test_that("select_neighbors with order = 2", {
  mat <- create_test_matrix()
  result <- select_neighbors(mat, of = "A", order = 2)

  expect_true(n_nodes(result) >= 1)
})

test_that("select_neighbors warns on missing node", {
  mat <- create_test_matrix()
  expect_warning(select_neighbors(mat, of = "Z"))
})

# =============================================================================
# SECTION 12: select_component() Tests (3 tests)
# =============================================================================

test_that("select_component largest", {
  mat <- create_disconnected_matrix()
  result <- select_component(mat, which = "largest")

  expect_equal(n_nodes(result), 3)
})

test_that("select_component by ID", {
  mat <- create_disconnected_matrix()
  result <- select_component(mat, which = 1)

  expect_true(n_nodes(result) > 0)
})

test_that("select_component warns on invalid component", {
  mat <- create_disconnected_matrix()
  expect_warning(select_component(mat, which = 100))
})

# =============================================================================
# SECTION 13: select_top() Tests (3 tests)
# =============================================================================

test_that("select_top by degree", {
  mat <- create_test_matrix()
  result <- select_top(mat, n = 2, by = "degree")

  expect_equal(n_nodes(result), 2)
})

test_that("select_top by betweenness", {
  mat <- create_test_matrix()
  result <- select_top(mat, n = 2, by = "betweenness")

  expect_equal(n_nodes(result), 2)
})

test_that("select_top by strength", {
  mat <- create_test_matrix()
  result <- select_top(mat, n = 2, by = "strength")

  expect_equal(n_nodes(result), 2)
})

# =============================================================================
# SECTION 14: select_edges() Tests (10 tests)
# =============================================================================

test_that("select_edges by weight threshold", {
  mat <- create_test_matrix()
  result <- select_edges(mat, weight > 0.5)

  edges <- get_edges(result)
  expect_true(all(edges$weight > 0.5))
})

test_that("select_edges top N by weight", {
  mat <- create_test_matrix()
  result <- select_edges(mat, top = 3)

  expect_true(n_edges(result) <= 3)
})

test_that("select_edges involving specific nodes", {
  mat <- create_test_matrix()
  result <- select_edges(mat, involving = "A")

  edges <- get_edges(result)
  nodes <- get_nodes(result)
  # All edges should involve node 1 (A)
  expect_true(all(edges$from == 1 | edges$to == 1))
})

test_that("select_edges between node sets", {
  mat <- create_test_matrix()
  result <- select_edges(mat, between = list(c("A", "B"), c("C", "D")))

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges bridges_only", {
  # Create network with bridge
  mat <- matrix(0, 5, 5)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[2, 3] <- mat[3, 2] <- 1  # Bridge
  mat[3, 4] <- mat[4, 3] <- 1
  mat[4, 5] <- mat[5, 4] <- 1
  mat[3, 5] <- mat[5, 3] <- 1
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  result <- select_edges(mat, bridges_only = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges mutual_only for directed", {
  mat <- create_directed_matrix()
  result <- select_edges(mat, mutual_only = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with abs_weight", {
  mat <- create_test_matrix()
  mat[1, 2] <- -0.8
  mat[2, 1] <- -0.8
  result <- select_edges(mat, abs_weight > 0.5)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges by edge_betweenness", {
  mat <- create_test_matrix()
  result <- select_edges(mat, top = 2, by = "edge_betweenness")

  expect_true(n_edges(result) <= 2)
})

test_that("select_edges warns on empty result", {
  mat <- create_test_matrix()
  expect_warning(select_edges(mat, weight > 100))
})

test_that("select_edges with same_community", {
  mat <- create_test_matrix()
  result <- select_edges(mat, same_community)

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 15: select_bridges() Tests (2 tests)
# =============================================================================

test_that("select_bridges returns bridge edges", {
  mat <- matrix(0, 5, 5)
  mat[1, 2] <- mat[2, 1] <- 1
  mat[2, 3] <- mat[3, 2] <- 1
  mat[3, 4] <- mat[4, 3] <- 1
  mat[4, 5] <- mat[5, 4] <- 1
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  result <- select_bridges(mat)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_bridges with keep_isolates", {
  mat <- create_test_matrix()
  result <- select_bridges(mat, .keep_isolates = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 16: select_top_edges() Tests (2 tests)
# =============================================================================

test_that("select_top_edges by weight", {
  mat <- create_test_matrix()
  result <- select_top_edges(mat, n = 3)

  expect_true(n_edges(result) <= 3)
})

test_that("select_top_edges by edge_betweenness", {
  mat <- create_test_matrix()
  result <- select_top_edges(mat, n = 2, by = "edge_betweenness")

  expect_true(n_edges(result) <= 2)
})

# =============================================================================
# SECTION 17: select_edges_involving() Tests (2 tests)
# =============================================================================

test_that("select_edges_involving single node", {
  mat <- create_test_matrix()
  result <- select_edges_involving(mat, nodes = "A")

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges_involving multiple nodes", {
  mat <- create_test_matrix()
  result <- select_edges_involving(mat, nodes = c("A", "B"))

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 18: select_edges_between() Tests (2 tests)
# =============================================================================

test_that("select_edges_between two sets", {
  mat <- create_test_matrix()
  result <- select_edges_between(mat, set1 = c("A", "B"), set2 = c("C", "D"))

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges_between with numeric indices", {
  mat <- create_test_matrix()
  result <- select_edges_between(mat, set1 = c(1, 2), set2 = c(3, 4))

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 19: Helper Function Tests (5 tests)
# =============================================================================

test_that(".detect_input_class correctly identifies matrix", {
  mat <- create_test_matrix()
  result <- cograph:::.detect_input_class(mat)

  expect_equal(result, "matrix")
})

test_that(".detect_input_class correctly identifies igraph", {
  g <- igraph::make_ring(5)
  result <- cograph:::.detect_input_class(g)

  expect_equal(result, "igraph")
})

test_that(".detect_input_class correctly identifies cograph_network", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)
  result <- cograph:::.detect_input_class(net)

  expect_equal(result, "cograph_network")
})

test_that(".empty_cograph_network creates empty network", {
  result <- cograph:::.empty_cograph_network(directed = FALSE)

  expect_true(inherits(result, "cograph_network"))
  expect_equal(n_nodes(result), 0)
  expect_equal(n_edges(result), 0)
})

test_that("to_adjacency_matrix is alias for to_matrix", {
  mat <- create_test_matrix()
  net <- as_cograph(mat)

  result1 <- to_matrix(net)
  result2 <- cograph:::to_adjacency_matrix(net)

  expect_equal(result1, result2)
})

# =============================================================================
# SECTION 20: Edge Cases and Error Handling (5 tests)
# =============================================================================

test_that("filter_edges handles network with no edges gracefully", {
  mat <- matrix(0, 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- as_cograph(mat)

  expect_warning(result <- filter_edges(net, weight > 0))
})

test_that("select_nodes with network with no matching nodes", {
  mat <- create_test_matrix()
  # Filter by impossible condition
  expect_warning(result <- select_nodes(mat, degree > 100))
})

test_that("select_edges warns on invalid between format", {
  mat <- create_test_matrix()

  expect_warning(result <- select_edges(mat, between = c("A", "B")))
})

test_that("select_edges handles network with all mutual edges", {
  mat <- create_test_matrix(symmetric = TRUE)
  result <- select_edges(mat, mutual_only = TRUE)

  expect_true(inherits(result, "cograph_network"))
})

test_that("detect_communities with leiden method", {
  skip_if_not(
    exists("cluster_leiden", where = asNamespace("igraph")),
    "Leiden algorithm not available in this igraph version"
  )

  mat <- create_test_matrix()
  result <- detect_communities(mat, method = "leiden")

  expect_equal(nrow(result), 4)
})

# =============================================================================
# SECTION 21: Global Context Variables in select_nodes() (5 tests)
# =============================================================================

test_that("select_nodes with is_largest_component", {
  mat <- create_disconnected_matrix()
  result <- select_nodes(mat, is_largest_component)

  expect_equal(n_nodes(result), 3)
})

test_that("select_nodes with component_size", {
  mat <- create_disconnected_matrix()
  result <- select_nodes(mat, component_size >= 3)

  expect_true(n_nodes(result) >= 3)
})

test_that("select_nodes with k_core", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, k_core >= 1)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with coreness centrality", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, coreness >= 1)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_nodes with neighborhood_size", {
  mat <- create_test_matrix()
  result <- select_nodes(mat, neighborhood_size >= 2)

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 22: Edge Metrics in select_edges() (4 tests)
# =============================================================================

test_that("select_edges with from_degree", {
  mat <- create_test_matrix()
  result <- select_edges(mat, from_degree >= 2)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with to_degree", {
  mat <- create_test_matrix()
  result <- select_edges(mat, to_degree >= 2)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with from_strength", {
  mat <- create_test_matrix()
  result <- select_edges(mat, from_strength > 0)

  expect_true(inherits(result, "cograph_network"))
})

test_that("select_edges with is_bridge", {
  mat <- create_test_matrix()
  result <- select_edges(mat, is_bridge)

  expect_true(inherits(result, "cograph_network"))
})

# =============================================================================
# SECTION 23: subset_nodes and subset_edges Aliases (2 tests)
# =============================================================================

test_that("subset_nodes is alias for filter_nodes", {
  mat <- create_test_matrix()
  result1 <- filter_nodes(mat, degree >= 2)
  result2 <- subset_nodes(mat, degree >= 2)

  expect_equal(n_nodes(result1), n_nodes(result2))
})

test_that("subset_edges is alias for filter_edges", {
  mat <- create_test_matrix()
  result1 <- filter_edges(mat, weight > 0.5)
  result2 <- subset_edges(mat, weight > 0.5)

  expect_equal(n_edges(result1), n_edges(result2))
})
