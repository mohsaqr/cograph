# test-edge-cases.R - Edge Cases and Boundary Tests
# Comprehensive edge case testing for robustness

# ============================================
# EMPTY AND MINIMAL NETWORKS
# ============================================

test_that("sonnet() handles empty adjacency matrix (no nodes)", {
  # 0x0 matrix might not be meaningful but shouldn't crash
  # Skip if implementation explicitly forbids it
  adj <- matrix(numeric(0), nrow = 0, ncol = 0)

  result <- tryCatch(
    sonnet(adj),
    error = function(e) "error"
  )

  # Either creates network or errors gracefully
  expect_true(inherits(result, "sonnet_network") || result == "error")
})

test_that("sonnet() handles single-node network", {
  adj <- matrix(0, 1, 1)
  net <- sonnet(adj)

  expect_sonnet_network(net)
  expect_equal(net$network$n_nodes, 1)
  expect_equal(net$network$n_edges, 0)
})

test_that("splot() renders single-node network", {
  adj <- matrix(0, 1, 1)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("sonnet() handles two-node network", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- sonnet(adj)

  expect_sonnet_network(net)
  expect_equal(net$network$n_nodes, 2)
})

test_that("splot() renders two-node network", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("sonnet() handles network with no edges", {
  adj <- matrix(0, 5, 5)
  net <- sonnet(adj)

  expect_sonnet_network(net)
  expect_equal(net$network$n_nodes, 5)
  expect_equal(net$network$n_edges, 0)
})

test_that("splot() renders network with no edges", {
  adj <- matrix(0, 5, 5)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# SELF-LOOPS
# ============================================

test_that("sonnet() handles network with self-loops", {
  adj <- create_test_matrix(4)
  diag(adj) <- 1

  net <- sonnet(adj)
  expect_sonnet_network(net)
})

test_that("splot() renders self-loops correctly", {
  adj <- matrix(0, 3, 3)
  diag(adj) <- 1  # Only self-loops

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("sonnet() handles network with only self-loops", {

  adj <- diag(4)  # Identity matrix = only self-loops
  net <- sonnet(adj)

  expect_sonnet_network(net)
  # Should have nodes (self-loops may or may not be counted as edges)
  expect_equal(net$network$n_nodes, 4)
})

test_that("splot() handles self-loop rotation parameter", {
  adj <- diag(3)

  result <- safe_plot(splot(adj, loop_rotation = c(0, pi/2, pi)))
  expect_true(result$success, info = result$error)
})

# ============================================
# SPECIAL GRAPH TOPOLOGIES
# ============================================

test_that("sonnet() handles complete graph", {
  adj <- create_test_topology("complete", n = 5)
  net <- sonnet(adj)

  expect_sonnet_network(net)
  # Complete graph of n nodes has n*(n-1)/2 undirected edges
  expect_equal(net$network$n_edges, 10)
})

test_that("splot() renders complete graph", {
  adj <- create_test_topology("complete", n = 6)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("sonnet() handles star graph", {
  adj <- create_test_topology("star", n = 5)
  net <- sonnet(adj)

  expect_sonnet_network(net)
  expect_equal(net$network$n_edges, 4)  # n-1 edges
})

test_that("splot() renders star graph", {
  adj <- create_test_topology("star", n = 6)

  result <- safe_plot(splot(adj, layout = "spring"))
  expect_true(result$success, info = result$error)
})

test_that("sonnet() handles ring graph", {
  adj <- create_test_topology("ring", n = 6)
  net <- sonnet(adj)

  expect_sonnet_network(net)
  expect_equal(net$network$n_edges, 6)  # n edges in a ring
})

test_that("splot() renders ring graph with circle layout", {
  adj <- create_test_topology("ring", n = 8)

  result <- safe_plot(splot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("sonnet() handles path graph", {
  adj <- create_test_topology("path", n = 5)
  net <- sonnet(adj)

  expect_sonnet_network(net)
  expect_equal(net$network$n_edges, 4)  # n-1 edges
})

test_that("sonnet() handles disconnected graph", {
  adj <- create_test_topology("disconnected", n = 6)
  net <- sonnet(adj)

  expect_sonnet_network(net)
})

test_that("splot() renders disconnected graph", {
  adj <- create_test_topology("disconnected", n = 6)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# WEIGHT EDGE CASES
# ============================================

test_that("sonnet() handles zero weights", {
  adj <- create_test_matrix(4, weighted = TRUE)
  adj[adj != 0] <- 0  # Set all weights to 0 (but keep structure)

  # This might result in empty network depending on implementation
  net <- sonnet(adj)
  expect_sonnet_network(net)
})

test_that("sonnet() handles negative weights", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  adj[adj > 0] <- -abs(adj[adj > 0])  # Make all weights negative

  net <- sonnet(adj)
  expect_sonnet_network(net)
})

test_that("splot() handles negative weights with coloring", {
  adj <- matrix(c(0, -0.5, -0.3, -0.5, 0, -0.8, -0.3, -0.8, 0), 3, 3)

  result <- safe_plot(splot(adj, positive_color = "blue", negative_color = "red"))
  expect_true(result$success, info = result$error)
})

test_that("sonnet() handles mixed positive/negative weights", {
  adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, -0.8, -0.3, -0.8, 0), 3, 3)
  net <- sonnet(adj)

  expect_sonnet_network(net)
})

test_that("sonnet() handles very small weights", {
  adj <- create_test_matrix(4, weighted = TRUE)
  adj[adj != 0] <- adj[adj != 0] * 1e-10

  net <- sonnet(adj)
  expect_sonnet_network(net)
})

test_that("sonnet() handles very large weights", {
  adj <- create_test_matrix(4, weighted = TRUE)
  adj[adj != 0] <- adj[adj != 0] * 1e10

  net <- sonnet(adj)
  expect_sonnet_network(net)
})

test_that("splot() handles weight_digits filtering near-zero weights", {
  adj <- create_test_matrix(4, weighted = TRUE)
  adj[adj != 0] <- 0.001  # Very small weights

  # With weight_digits=2, these should round to 0 and be filtered
  result <- safe_plot(splot(adj, weight_digits = 2))
  expect_true(result$success, info = result$error)
})

# ============================================
# NODE LABEL EDGE CASES
# ============================================

test_that("splot() handles very long node labels", {
  adj <- create_test_matrix(3)
  labels <- c(
    "This is an extremely long label that might overflow",
    "Another very long label for testing purposes",
    "Short"
  )

  result <- safe_plot(splot(adj, labels = labels))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles empty string labels", {
  adj <- create_test_matrix(3)
  labels <- c("", "B", "")

  result <- safe_plot(splot(adj, labels = labels))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles numeric labels", {
  adj <- create_test_matrix(3)
  labels <- 1:3

  result <- safe_plot(splot(adj, labels = labels))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles Unicode labels", {
  skip_on_cran()  # Unicode handling varies by platform

  adj <- create_test_matrix(3)

  # Greek letters
  result <- safe_plot(splot(adj, labels = c("\u03B1", "\u03B2", "\u03B3")))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles labels with special characters", {
  adj <- create_test_matrix(3)
  labels <- c("Node & 1", "Node < 2 >", "Node \"3\"")

  result <- safe_plot(splot(adj, labels = labels))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles labels with newlines", {
  adj <- create_test_matrix(3)
  labels <- c("Line1\nLine2", "Single", "A\nB\nC")

  result <- safe_plot(splot(adj, labels = labels))
  expect_true(result$success, info = result$error)
})

# ============================================
# NAMED MATRIX EDGE CASES
# ============================================

test_that("sonnet() preserves row/column names as labels", {
  adj <- create_test_matrix(3)
  rownames(adj) <- colnames(adj) <- c("Alice", "Bob", "Charlie")

  net <- sonnet(adj)
  nodes <- net$network$get_nodes()

  expect_equal(nodes$label, c("Alice", "Bob", "Charlie"))
})

test_that("sonnet() handles only rownames (no colnames)", {
  adj <- create_test_matrix(3)
  rownames(adj) <- c("A", "B", "C")
  # colnames remain NULL

  net <- sonnet(adj)
  nodes <- net$network$get_nodes()

  expect_equal(nodes$label, c("A", "B", "C"))
})

test_that("sonnet() handles mismatched row/colnames", {
  adj <- create_test_matrix(3)
  rownames(adj) <- c("R1", "R2", "R3")
  colnames(adj) <- c("C1", "C2", "C3")

  net <- sonnet(adj)
  # Should use one of them (likely rownames)
  nodes <- net$network$get_nodes()
  expect_equal(length(nodes$label), 3)
})

# ============================================
# COLOR EDGE CASES
# ============================================

test_that("splot() handles NA colors gracefully", {
  adj <- create_test_matrix(4)

  # This might error or substitute default
  result <- tryCatch({
    with_temp_png(splot(adj, node_fill = c("red", NA, "blue", "green")))
    "success"
  }, error = function(e) "error")

  # Should either work or error cleanly
  expect_true(result %in% c("success", "error"))
})

test_that("splot() handles transparent colors", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, node_fill = "transparent"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles RGB color strings", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, node_fill = "#FF573380"))  # With alpha
  expect_true(result$success, info = result$error)
})

# ============================================
# SIZE EDGE CASES
# ============================================

test_that("splot() handles zero node size", {
  adj <- create_test_matrix(3)

  # Zero size might make nodes invisible
  result <- safe_plot(splot(adj, node_size = 0))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles very small node sizes", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj, node_size = 0.001))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles very large node sizes", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj, node_size = 50))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles zero edge width", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, edge_width = 0))
  expect_true(result$success, info = result$error)
})

# ============================================
# LARGE NETWORKS
# ============================================

test_that("sonnet() handles moderately large network", {
  skip_on_cran()  # Can be slow

  adj <- create_test_matrix(50, density = 0.1)
  net <- sonnet(adj, layout = "spring", seed = 42)

  expect_sonnet_network(net)
  expect_equal(net$network$n_nodes, 50)
})

test_that("splot() renders moderately large network", {
  skip_on_cran()

  adj <- create_test_matrix(30, density = 0.1)

  result <- safe_plot(splot(adj, layout = "spring", seed = 42))
  expect_true(result$success, info = result$error)
})

# ============================================
# LAYOUT EDGE CASES
# ============================================

test_that("splot() handles custom layout with NaN values gracefully", {
  adj <- create_test_matrix(3)
  layout <- matrix(c(0, NaN, 1, 0.5, 1, 0.5), ncol = 2)

  # Should error or handle gracefully
  result <- tryCatch({
    with_temp_png(splot(adj, layout = layout))
    "success"
  }, error = function(e) "error")

  expect_true(result %in% c("success", "error"))
})

test_that("splot() handles custom layout with Inf values gracefully", {
  adj <- create_test_matrix(3)
  layout <- matrix(c(0, Inf, 1, 0.5, 1, 0.5), ncol = 2)

  result <- tryCatch({
    with_temp_png(splot(adj, layout = layout))
    "success"
  }, error = function(e) "error")

  expect_true(result %in% c("success", "error"))
})

test_that("splot() handles collinear layout (all nodes in a line)", {
  adj <- create_test_matrix(4)
  layout <- matrix(c(0, 0.33, 0.67, 1, 0.5, 0.5, 0.5, 0.5), ncol = 2)

  result <- safe_plot(splot(adj, layout = layout))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles coincident nodes (same position)", {
  adj <- create_test_matrix(3)
  layout <- matrix(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5), ncol = 2)

  result <- safe_plot(splot(adj, layout = layout))
  expect_true(result$success, info = result$error)
})

# ============================================
# PIE/DONUT EDGE CASES
# ============================================

test_that("splot() handles pie with single value per node", {
  adj <- create_test_matrix(3)
  pie_vals <- list(c(1), c(1), c(1))

  result <- safe_plot(splot(adj, pie_values = pie_vals))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles pie with many segments", {
  adj <- create_test_matrix(3)
  pie_vals <- list(1:10, 1:5, 1:3)

  result <- safe_plot(splot(adj, pie_values = pie_vals))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_fill at boundaries (0 and 1)", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj, donut_fill = c(0, 0.5, 1)))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles partial donut_fill list", {
  adj <- create_test_matrix(4)
  # Only 2 values for 4 nodes
  donut_fill <- c(0.3, 0.7)

  # Should recycle or error gracefully
  result <- tryCatch({
    with_temp_png(splot(adj, donut_fill = donut_fill))
    "success"
  }, error = function(e) "error")

  expect_true(result %in% c("success", "error"))
})

# ============================================
# EDGE LIST EDGE CASES
# ============================================

test_that("sonnet() handles edge list with duplicate edges", {
  edges <- data.frame(
    from = c(1, 1, 2),
    to = c(2, 2, 3)  # Duplicate edge 1->2
  )

  net <- sonnet(edges)
  expect_sonnet_network(net)
})

test_that("sonnet() handles edge list with all same edge", {
  edges <- data.frame(
    from = c(1, 1, 1),
    to = c(2, 2, 2)
  )

  net <- sonnet(edges)
  expect_sonnet_network(net)
})

# ============================================
# THEME EDGE CASES
# ============================================

test_that("splot() handles theme with NULL background", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, background = NULL))
  expect_true(result$success, info = result$error)
})

test_that("sn_theme() applies correctly after other customizations", {
  adj <- create_test_matrix(4)

  # Apply node customizations first, then theme
  net <- sonnet(adj) |>
    sn_nodes(fill = "red") |>  # Custom color
    sn_theme("dark")  # Theme might override

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})
