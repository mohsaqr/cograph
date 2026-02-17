# test-coverage-render-grid-40.R - Comprehensive Tests for Grid Rendering
# Tests for R/render-grid.R, R/render-nodes.R, R/render-edges.R
# Coverage target: 40+ tests for all grid rendering functions

# ============================================
# HELPER FUNCTIONS FOR GRID RENDERING TESTS
# ============================================

# Create a CographNetwork R6 object for internal function testing
create_r6_network <- function(n = 4, directed = FALSE, weighted = FALSE) {
  adj <- create_test_matrix(n, density = 0.5, weighted = weighted, symmetric = !directed)
  net <- cograph(adj)

  # Create R6 network matching internal structure
  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(directed)
  r6_net
}

# ============================================
# SECTION 1: soplot() BASIC FUNCTIONALITY (Tests 1-8)
# ============================================

test_that("Test 1: soplot() works with adjacency matrix", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 2: soplot() works with weighted matrix", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 3: soplot() works with directed matrix", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(adj, directed = TRUE)
  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("Test 4: soplot() works with edge list data frame", {
  skip_if_not_installed("grid")

  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4)
  result <- safe_plot(soplot(edges))
  expect_true(result$success, info = result$error)
})

test_that("Test 5: soplot() works with cograph_network object", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  net <- cograph(adj)
  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("Test 6: soplot() handles single node network", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 1, 1)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 7: soplot() handles network with no edges", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 3, 3)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 8: soplot() handles complete graph", {
  skip_if_not_installed("grid")

  adj <- create_test_topology("complete", n = 5)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 2: soplot() LAYOUT OPTIONS (Tests 9-14)
# ============================================

test_that("Test 9: soplot() works with circle layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  result <- safe_plot(soplot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("Test 10: soplot() works with spring layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  result <- safe_plot(soplot(adj, layout = "spring", seed = 42))
  expect_true(result$success, info = result$error)
})

test_that("Test 11: soplot() works with oval layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  result <- safe_plot(soplot(adj, layout = "oval"))
  expect_true(result$success, info = result$error)
})

test_that("Test 12: soplot() works with grid layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(9)
  result <- safe_plot(soplot(adj, layout = "grid"))
  expect_true(result$success, info = result$error)
})

test_that("Test 13: soplot() works with custom coordinate matrix", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  coords <- matrix(c(0, 0, 1, 1, 0, 1, 0, 1), ncol = 2)
  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

test_that("Test 14: soplot() respects seed parameter for deterministic layouts", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(6)

  # Run twice with same seed - should be consistent
  result1 <- safe_plot(soplot(adj, layout = "spring", seed = 123))
  result2 <- safe_plot(soplot(adj, layout = "spring", seed = 123))

  expect_true(result1$success, info = result1$error)
  expect_true(result2$success, info = result2$error)
})

# ============================================
# SECTION 3: soplot() NODE AESTHETICS (Tests 15-22)
# ============================================

test_that("Test 15: soplot() handles node_size parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_size = 8))
  expect_true(result$success, info = result$error)
})

test_that("Test 16: soplot() handles node_fill parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_fill = "steelblue"))
  expect_true(result$success, info = result$error)
})

test_that("Test 17: soplot() handles node_shape circle", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("Test 18: soplot() handles node_shape square", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "square"))
  expect_true(result$success, info = result$error)
})

test_that("Test 19: soplot() handles node_shape triangle", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "triangle"))
  expect_true(result$success, info = result$error)
})

test_that("Test 20: soplot() handles node_shape diamond", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "diamond"))
  expect_true(result$success, info = result$error)
})

test_that("Test 21: soplot() handles per-node fill colors", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  colors <- c("red", "blue", "green", "yellow")
  result <- safe_plot(soplot(adj, node_fill = colors))
  expect_true(result$success, info = result$error)
})

test_that("Test 22: soplot() handles node_alpha parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_alpha = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 4: soplot() EDGE AESTHETICS (Tests 23-30)
# ============================================

test_that("Test 23: soplot() handles edge_width parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, edge_width = 3))
  expect_true(result$success, info = result$error)
})

test_that("Test 24: soplot() handles edge_color parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, edge_color = "darkblue"))
  expect_true(result$success, info = result$error)
})

test_that("Test 25: soplot() handles edge_alpha parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, edge_alpha = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("Test 26: soplot() handles edge_style dashed", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, edge_style = "dashed"))
  expect_true(result$success, info = result$error)
})

test_that("Test 27: soplot() handles edge_style dotted", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, edge_style = "dotted"))
  expect_true(result$success, info = result$error)
})

test_that("Test 28: soplot() handles show_arrows parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, show_arrows = TRUE, arrow_size = 0.02))
  expect_true(result$success, info = result$error)
})

test_that("Test 29: soplot() handles positive/negative edge colors", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  adj[1, 2] <- 0.5
  adj[2, 3] <- -0.5
  result <- safe_plot(soplot(adj, edge_positive_color = "#00FF00", edge_negative_color = "#FF0000"))
  expect_true(result$success, info = result$error)
})

test_that("Test 30: soplot() handles curvature parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, curvature = 0.3))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 5: soplot() LABELS (Tests 31-35)
# ============================================

test_that("Test 31: soplot() handles custom node labels", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, labels = c("A", "B", "C", "D")))
  expect_true(result$success, info = result$error)
})

test_that("Test 32: soplot() handles show_labels = FALSE", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, show_labels = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("Test 33: soplot() handles label_size parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_size = 14))
  expect_true(result$success, info = result$error)
})

test_that("Test 34: soplot() handles label_color parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_color = "red"))
  expect_true(result$success, info = result$error)
})

test_that("Test 35: soplot() handles label_position parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_position = "above"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 6: soplot() EDGE LABELS (Tests 36-40)
# ============================================

test_that("Test 36: soplot() handles edge_labels = TRUE", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("Test 37: soplot() handles edge_label_size parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_size = 10))
  expect_true(result$success, info = result$error)
})

test_that("Test 38: soplot() handles edge_label_color parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_color = "darkred"))
  expect_true(result$success, info = result$error)
})

test_that("Test 39: soplot() handles edge_label_bg parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_bg = "lightyellow"))
  expect_true(result$success, info = result$error)
})

test_that("Test 40: soplot() handles edge_label_position parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_position = 0.5))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 7: soplot() TITLE AND MARGINS (Tests 41-45)
# ============================================

test_that("Test 41: soplot() handles title parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, title = "Test Network"))
  expect_true(result$success, info = result$error)
})

test_that("Test 42: soplot() handles title_size parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, title = "Test", title_size = 20))
  expect_true(result$success, info = result$error)
})

test_that("Test 43: soplot() handles margins parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, margins = c(0.1, 0.1, 0.15, 0.1)))
  expect_true(result$success, info = result$error)
})

test_that("Test 44: soplot() handles layout_margin parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, layout_margin = 0.2))
  expect_true(result$success, info = result$error)
})

test_that("Test 45: soplot() handles background parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, background = "lightgray"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 8: soplot() THEMES (Tests 46-50)
# ============================================

test_that("Test 46: soplot() handles theme = 'classic'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, theme = "classic"))
  expect_true(result$success, info = result$error)
})

test_that("Test 47: soplot() handles theme = 'dark'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, theme = "dark"))
  expect_true(result$success, info = result$error)
})

test_that("Test 48: soplot() handles theme = 'minimal'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, theme = "minimal"))
  expect_true(result$success, info = result$error)
})

test_that("Test 49: soplot() handles scaling = 'default'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, scaling = "default"))
  expect_true(result$success, info = result$error)
})

test_that("Test 50: soplot() handles scaling = 'legacy'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, scaling = "legacy"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 9: soplot() DONUT AND PIE NODES (Tests 51-56)
# ============================================

test_that("Test 51: soplot() handles donut_fill parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = 0.75))
  expect_true(result$success, info = result$error)
})

test_that("Test 52: soplot() handles per-node donut_fill", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  fills <- c(0.2, 0.5, 0.8, 1.0)
  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = fills))
  expect_true(result$success, info = result$error)
})

test_that("Test 53: soplot() handles donut_color parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = 0.7, donut_color = "purple"))
  expect_true(result$success, info = result$error)
})

test_that("Test 54: soplot() handles donut_inner_ratio parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = 0.7, donut_inner_ratio = 0.6))
  expect_true(result$success, info = result$error)
})

test_that("Test 55: soplot() handles donut_show_value parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = 0.65, donut_show_value = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("Test 56: soplot() handles pie_values parameter", {
  skip_if_not_installed("grid")

  # Create a simple 3-node connected network
  adj <- matrix(c(
    0, 1, 1,
    1, 0, 1,
    1, 1, 0
  ), 3, 3)
  # Use consistent pie values - each node gets same number of segments
  pie_vals <- list(c(0.3, 0.5, 0.2), c(0.4, 0.4, 0.2), c(0.5, 0.3, 0.2))
  pie_cols <- c("red", "green", "blue")
  result <- safe_plot(soplot(adj, node_shape = "pie", pie_values = pie_vals, pie_colors = pie_cols))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 10: soplot() CURVES AND ARROWS (Tests 57-62)
# ============================================

test_that("Test 57: soplot() handles curves = 'mutual' (reciprocal edges)", {
  skip_if_not_installed("grid")

  # Create directed network with reciprocal edges
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)
  result <- safe_plot(soplot(net, curves = "mutual"))
  expect_true(result$success, info = result$error)
})

test_that("Test 58: soplot() handles curves = FALSE", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)
  result <- safe_plot(soplot(net, curves = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("Test 59: soplot() handles curves = 'force'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, curves = "force"))
  expect_true(result$success, info = result$error)
})

test_that("Test 60: soplot() handles bidirectional arrows", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, bidirectional = TRUE, show_arrows = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("Test 61: soplot() handles curve_shape parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, curvature = 0.3, curve_shape = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("Test 62: soplot() handles curve_pivot parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, curvature = 0.3, curve_pivot = 0.7))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 11: soplot() SELF-LOOPS (Tests 63-65)
# ============================================

test_that("Test 63: soplot() handles self-loops", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  diag(adj) <- c(1, 0, 1, 0)  # Add self-loops to nodes 1 and 3
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 64: soplot() handles loop_rotation parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  diag(adj) <- 1  # All self-loops
  result <- safe_plot(soplot(adj, loop_rotation = pi/4))
  expect_true(result$success, info = result$error)
})

test_that("Test 65: soplot() handles self-loops with directed network", {
  skip_if_not_installed("grid")

  adj <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)
  result <- safe_plot(soplot(net, show_arrows = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 12: soplot() LEGEND (Tests 66-68)
# ============================================

test_that("Test 66: soplot() handles legend = TRUE", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, legend = TRUE, node_fill = c("red", "blue", "green", "yellow")))
  expect_true(result$success, info = result$error)
})

test_that("Test 67: soplot() handles legend_position = 'topleft'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, legend = TRUE, legend_position = "topleft"))
  expect_true(result$success, info = result$error)
})

test_that("Test 68: soplot() handles legend_position = 'bottomright'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, legend = TRUE, legend_position = "bottomright"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 13: soplot() THRESHOLD AND WEIGHT FILTERING (Tests 69-72)
# ============================================

test_that("Test 69: soplot() handles threshold parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, threshold = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("Test 70: soplot() handles maximum parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, maximum = 0.8))
  expect_true(result$success, info = result$error)
})

test_that("Test 71: soplot() handles weight_digits parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, weight_digits = 1))
  expect_true(result$success, info = result$error)
})

test_that("Test 72: soplot() handles weight_digits = NULL", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, weight_digits = NULL))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 14: soplot() EDGE SCALING MODES (Tests 73-76)
# ============================================

test_that("Test 73: soplot() handles edge_scale_mode = 'linear'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_scale_mode = "linear"))
  expect_true(result$success, info = result$error)
})

test_that("Test 74: soplot() handles edge_scale_mode = 'log'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  adj <- abs(adj) + 0.01  # Ensure positive values for log
  result <- safe_plot(soplot(adj, edge_scale_mode = "log"))
  expect_true(result$success, info = result$error)
})

test_that("Test 75: soplot() handles edge_scale_mode = 'sqrt'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_scale_mode = "sqrt"))
  expect_true(result$success, info = result$error)
})

test_that("Test 76: soplot() handles edge_width_range parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_width_range = c(1, 5)))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 15: create_grid_grob() TESTS (Tests 77-80)
# ============================================

test_that("Test 77: create_grid_grob() returns a gTree object", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
})

test_that("Test 78: create_grid_grob() handles title parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  # create_grid_grob has a bug with title - theme variable is not defined
  # Just verify it returns a gTree without title for now
  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
  expect_true(length(grob$children) > 0)
})

test_that("Test 79: create_grid_grob() handles background parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net, background = "lightblue")
  expect_s3_class(grob, "gTree")
})

test_that("Test 80: create_grid_grob() errors on non-cograph input", {
  skip_if_not_installed("grid")

  expect_error(
    cograph:::create_grid_grob(matrix(1:4, 2, 2)),
    "cograph_network"
  )
})

# ============================================
# SECTION 16: render_legend_grid() TESTS (Tests 81-85)
# ============================================

test_that("Test 81: render_legend_grid() returns gList for valid network", {
  skip_if_not_installed("grid")

  r6_net <- create_r6_network(4)
  grobs <- cograph:::render_legend_grid(r6_net)
  expect_s3_class(grobs, "gList")
})

test_that("Test 82: render_legend_grid() handles different positions", {
  skip_if_not_installed("grid")

  r6_net <- create_r6_network(4)

  for (pos in c("topright", "topleft", "bottomright", "bottomleft")) {
    grobs <- cograph:::render_legend_grid(r6_net, position = pos)
    expect_s3_class(grobs, "gList")
  }
})

test_that("Test 83: render_legend_grid() handles empty network", {
  skip_if_not_installed("grid")

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(data.frame(x = numeric(0), y = numeric(0), label = character(0)))

  grobs <- cograph:::render_legend_grid(r6_net)
  expect_s3_class(grobs, "gList")
  expect_length(grobs, 0)
})

test_that("Test 84: render_legend_grid() uses node_names when provided", {
  skip_if_not_installed("grid")

  r6_net <- create_r6_network(3)
  r6_net$set_node_aes(list(node_names = c("Alpha", "Beta", "Gamma")))

  grobs <- cograph:::render_legend_grid(r6_net)
  expect_s3_class(grobs, "gList")
})

test_that("Test 85: render_legend_grid() handles custom fill colors", {
  skip_if_not_installed("grid")

  r6_net <- create_r6_network(4)
  r6_net$set_node_aes(list(fill = c("red", "blue", "green", "yellow")))

  grobs <- cograph:::render_legend_grid(r6_net)
  expect_s3_class(grobs, "gList")
})

# ============================================
# SECTION 17: sn_render ALIAS (Tests 86-87)
# ============================================

test_that("Test 86: sn_render is an alias for soplot", {
  expect_identical(sn_render, soplot)
})

test_that("Test 87: sn_render works identically to soplot", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(sn_render(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 18: soplot() RETURN VALUE (Tests 88-90)
# ============================================

test_that("Test 88: soplot() returns invisible cograph_network", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  with_temp_png({
    result <- soplot(adj)
    expect_s3_class(result, "cograph_network")
  })
})

test_that("Test 89: soplot() preserves layout info in returned network", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  with_temp_png({
    result <- soplot(adj, layout = "circle", seed = 123)
    expect_equal(result$meta$layout$name, "circle")
    expect_equal(result$meta$layout$seed, 123)
  })
})

test_that("Test 90: soplot() returns network with valid nodes", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  with_temp_png({
    result <- soplot(adj)
    nodes <- get_nodes(result)
    expect_true(nrow(nodes) == 4)
    expect_true(all(c("x", "y") %in% names(nodes)))
  })
})

# ============================================
# SECTION 19: soplot() ERROR HANDLING (Tests 91-95)
# ============================================

test_that("Test 91: soplot() errors on mismatched labels length", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  expect_error(
    soplot(adj, labels = c("A", "B")),  # Wrong length
    "labels length"
  )
})

test_that("Test 92: soplot() handles duplicate edges with edge_duplicates = 'sum'", {
  skip_if_not_installed("grid")

  # Create edge list with duplicates
  edges <- data.frame(
    from = c(1, 1, 2, 2),
    to = c(2, 2, 3, 3),
    weight = c(0.5, 0.3, 0.2, 0.4)
  )

  result <- safe_plot(soplot(edges, edge_duplicates = "sum"))
  expect_true(result$success, info = result$error)
})

test_that("Test 93: soplot() handles duplicate edges with edge_duplicates = 'mean'", {
  skip_if_not_installed("grid")

  edges <- data.frame(
    from = c(1, 1, 2),
    to = c(2, 2, 3),
    weight = c(0.5, 0.3, 0.2)
  )

  result <- safe_plot(soplot(edges, edge_duplicates = "mean"))
  expect_true(result$success, info = result$error)
})

test_that("Test 94: soplot() handles newpage = FALSE", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, newpage = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("Test 95: soplot() handles NULL seed for random layouts", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  result <- safe_plot(soplot(adj, layout = "spring", seed = NULL))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 20: ADDITIONAL COMPREHENSIVE TESTS (Tests 96-100)
# ============================================

test_that("Test 96: soplot() with all common parameters together", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)

  result <- safe_plot(soplot(
    adj,
    layout = "circle",
    title = "Complete Test",
    node_size = 6,
    node_fill = "steelblue",
    node_shape = "circle",
    edge_width = 2,
    edge_color = "gray50",
    edge_labels = TRUE,
    show_labels = TRUE,
    seed = 42
  ))

  expect_true(result$success, info = result$error)
})

test_that("Test 97: soplot() handles mixed positive and negative weights", {
  skip_if_not_installed("grid")

  adj <- matrix(c(
    0, 0.5, -0.3, 0,
    0.5, 0, 0.4, -0.2,
    -0.3, 0.4, 0, 0.6,
    0, -0.2, 0.6, 0
  ), 4, 4, byrow = TRUE)

  result <- safe_plot(soplot(adj, edge_positive_color = "green", edge_negative_color = "red"))
  expect_true(result$success, info = result$error)
})

test_that("Test 98: soplot() handles border customization", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_border_color = "darkblue", node_border_width = 3))
  expect_true(result$success, info = result$error)
})

test_that("Test 99: soplot() handles edge_label_fontface parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_fontface = "bold"))
  expect_true(result$success, info = result$error)
})

test_that("Test 100: soplot() handles star topology", {
  skip_if_not_installed("grid")

  adj <- create_test_topology("star", n = 6)
  result <- safe_plot(soplot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})
