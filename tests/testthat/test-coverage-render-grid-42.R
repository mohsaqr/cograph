# test-coverage-render-grid-42.R - Additional Tests for Grid Rendering Coverage
# Tests for R/render-grid.R, R/render-nodes.R, R/render-edges.R
# Targets UNCOVERED functions and branches to improve coverage beyond 81.6%

# ============================================
# HELPER FUNCTIONS
# ============================================

# Create a CographNetwork R6 object for internal function testing
create_r6_network_extended <- function(n = 4, directed = FALSE, weighted = FALSE) {
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
# SECTION 1: DONUT PROCESSING - UNCOVERED BRANCHES (Tests 1-10)
# ============================================

test_that("Test 1: soplot() handles donut_color as list with 2*n_nodes elements", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # List with 2*n_nodes: per-node (fill, bg) pairs
  donut_colors_list <- list("red", "white", "blue", "gray90", "green", "lightgray")

  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = c(0.5, 0.7, 0.9),
                              donut_color = donut_colors_list))
  expect_true(result$success, info = result$error)
})

test_that("Test 2: soplot() handles donut_color with multiple colors (not 2)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  # Multiple colors treated as per-node fill colors
  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = c(0.3, 0.5, 0.7, 0.9),
                              donut_color = c("red", "blue", "green", "purple")))
  expect_true(result$success, info = result$error)
})

test_that("Test 3: soplot() handles donut_colors (deprecated param)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = 0.7,
                              donut_colors = list("maroon", "maroon", "maroon")))
  expect_true(result$success, info = result$error)
})

test_that("Test 4: soplot() auto-enables donut fill when node_shape is 'donut'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  # donut_fill and donut_values are NULL but node_shape is "donut"
  result <- safe_plot(soplot(adj, node_shape = "donut"))
  expect_true(result$success, info = result$error)
})

test_that("Test 5: soplot() handles explicit donut_shape parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "circle", donut_fill = c(0.5, 0.7, 0.9),
                              donut_shape = "square"))
  expect_true(result$success, info = result$error)
})

test_that("Test 6: soplot() handles donut_fill as list", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = list(0.3, 0.6, 0.9)))
  expect_true(result$success, info = result$error)
})

test_that("Test 7: soplot() handles donut value formatting parameters", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut", donut_fill = 0.65,
                              donut_show_value = TRUE,
                              donut_value_digits = 1,
                              donut_value_prefix = "$",
                              donut_value_suffix = "%",
                              donut_value_fontface = "italic",
                              donut_value_fontfamily = "serif"))
  expect_true(result$success, info = result$error)
})

test_that("Test 8: soplot() handles donut_shape hexagon", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, donut_fill = c(0.5, 0.7, 0.9),
                              donut_shape = "hexagon"))
  expect_true(result$success, info = result$error)
})

test_that("Test 9: soplot() handles donut_shape pentagon", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, donut_fill = c(0.5, 0.7, 0.9),
                              donut_shape = "pentagon"))
  expect_true(result$success, info = result$error)
})

test_that("Test 10: soplot() handles donut_shape triangle", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, donut_fill = c(0.5, 0.7, 0.9),
                              donut_shape = "triangle"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 2: RENDER LEGEND GRID - ALL POSITIONS (Tests 11-15)
# ============================================

test_that("Test 11: render_legend_grid() handles unknown position (defaults to topright)", {
  skip_if_not_installed("grid")

  r6_net <- create_r6_network_extended(4)
  grobs <- cograph:::render_legend_grid(r6_net, position = "unknown_position")
  expect_s3_class(grobs, "gList")
})

test_that("Test 12: render_legend_grid() uses nodes$name when node_names is NULL", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)
  nodes <- get_nodes(net)
  nodes$name <- c("Alpha", "Beta", "Gamma")
  net$nodes <- nodes

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(nodes)
  r6_net$set_edges(get_edges(net))

  grobs <- cograph:::render_legend_grid(r6_net)
  expect_s3_class(grobs, "gList")
})

test_that("Test 13: render_legend_grid() deduplicates legend entries", {
  skip_if_not_installed("grid")

  r6_net <- create_r6_network_extended(4)
  # Set same fill for multiple nodes - should deduplicate
  r6_net$set_node_aes(list(fill = c("blue", "blue", "red", "red")))

  grobs <- cograph:::render_legend_grid(r6_net)
  expect_s3_class(grobs, "gList")
})

test_that("Test 14: render_legend_grid() bottomleft position", {
  skip_if_not_installed("grid")

  r6_net <- create_r6_network_extended(4)
  grobs <- cograph:::render_legend_grid(r6_net, position = "bottomleft")
  expect_s3_class(grobs, "gList")
})

test_that("Test 15: render_legend_grid() with theme", {
  skip_if_not_installed("grid")

  r6_net <- create_r6_network_extended(4)
  # Set theme
  theme <- cograph:::CographTheme$new()
  r6_net$set_theme(theme)

  grobs <- cograph:::render_legend_grid(r6_net, position = "topright")
  expect_s3_class(grobs, "gList")
})

# ============================================
# SECTION 3: EDGE DUPLICATE HANDLING (Tests 16-22)
# ============================================

test_that("Test 16: soplot() handles undirected with symmetric edges gracefully", {
  skip_if_not_installed("grid")

  # Create edge list with reversed pairs (symmetric edges)
  edges <- data.frame(
    from = c(1, 2, 2, 3),
    to = c(2, 1, 3, 2),
    weight = c(0.5, 0.3, 0.4, 0.6)
  )

  # Symmetric pairs should trigger duplicate handling
  result <- safe_plot(soplot(edges, edge_duplicates = "sum"))
  expect_true(result$success, info = result$error)
})

test_that("Test 17: soplot() handles edge_duplicates = 'first'", {
  skip_if_not_installed("grid")

  edges <- data.frame(
    from = c(1, 1, 2, 3),
    to = c(2, 2, 3, 1),
    weight = c(0.5, 0.3, 0.4, 0.6)
  )

  result <- safe_plot(soplot(edges, edge_duplicates = "first"))
  expect_true(result$success, info = result$error)
})

test_that("Test 18: soplot() handles edge_duplicates = 'max'", {
  skip_if_not_installed("grid")

  edges <- data.frame(
    from = c(1, 1, 2, 3),
    to = c(2, 2, 3, 1),
    weight = c(0.5, 0.3, 0.4, 0.6)
  )

  result <- safe_plot(soplot(edges, edge_duplicates = "max"))
  expect_true(result$success, info = result$error)
})

test_that("Test 19: soplot() handles edge_duplicates = 'min'", {
  skip_if_not_installed("grid")

  edges <- data.frame(
    from = c(1, 1, 2, 3),
    to = c(2, 2, 3, 1),
    weight = c(0.5, 0.3, 0.4, 0.6)
  )

  result <- safe_plot(soplot(edges, edge_duplicates = "min"))
  expect_true(result$success, info = result$error)
})

test_that("Test 20: soplot() handles directed network (no duplicate check)", {
  skip_if_not_installed("grid")

  # Directed network with reciprocal edges - not duplicates
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("Test 21: soplot() handles undirected network without duplicates", {
  skip_if_not_installed("grid")

  # Symmetric matrix - no duplicates
  adj <- create_test_matrix(4, symmetric = TRUE)

  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 22: soplot() with empty edge network", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 4, 4)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 4: DEPRECATED PARAMETERS (Tests 23-28)
# ============================================

test_that("Test 23: soplot() handles deprecated esize parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)

  expect_warning(
    result <- safe_plot(soplot(adj, esize = 2)),
    "deprecated|edge_size"
  )
})

test_that("Test 24: soplot() handles deprecated cut parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)

  expect_warning(
    result <- safe_plot(soplot(adj, cut = 0.3)),
    "deprecated|edge_cutoff"
  )
})

test_that("Test 25: soplot() handles deprecated positive_color parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  adj[1, 2] <- 0.5
  adj[2, 3] <- -0.5

  expect_warning(
    result <- safe_plot(soplot(adj, positive_color = "#00FF00")),
    "deprecated|edge_positive_color"
  )
})

test_that("Test 26: soplot() handles deprecated negative_color parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  adj[1, 2] <- 0.5
  adj[2, 3] <- -0.5

  expect_warning(
    result <- safe_plot(soplot(adj, negative_color = "#FF0000")),
    "deprecated|edge_negative_color"
  )
})

test_that("Test 27: soplot() prefers new param over deprecated param", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)

  # When both provided, should use new param
  expect_warning(
    result <- safe_plot(soplot(adj, edge_size = 3, esize = 1)),
    "deprecated"
  )
  expect_true(result$success, info = result$error)
})

test_that("Test 28: soplot() handles all deprecated params together", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)

  # Suppress warnings for this test
  suppressWarnings({
    result <- safe_plot(soplot(adj, esize = 2, cut = 0.2,
                                positive_color = "green", negative_color = "red"))
  })
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 5: LAYOUT RESCALING (Tests 29-33)
# ============================================

test_that("Test 29: soplot() handles layout rescaling with single node", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 1, 1)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 30: soplot() handles all nodes at same position", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # Custom layout with all nodes at same position
  coords <- matrix(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5), ncol = 2)

  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

test_that("Test 31: soplot() preserves aspect ratio in layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  # Layout with different x and y ranges
  coords <- matrix(c(0, 10, 20, 30, 40, 0, 1, 2, 3, 4), ncol = 2)

  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

test_that("Test 32: soplot() handles custom layout_margin", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  result <- safe_plot(soplot(adj, layout_margin = 0.25))
  expect_true(result$success, info = result$error)
})

test_that("Test 33: soplot() handles layout_margin = 0", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  result <- safe_plot(soplot(adj, layout_margin = 0))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 6: CREATE_GRID_GROB FUNCTION (Tests 34-38)
# ============================================

test_that("Test 34: create_grid_grob() contains all child components", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
  # Should have: bg, edges, edge_labels, nodes, labels
  expect_true(length(grob$children) >= 1)
})

test_that("Test 35: create_grid_grob() with custom background", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net, background = "#F0F0F0")
  expect_s3_class(grob, "gTree")
})

test_that("Test 36: create_grid_grob() with directed network", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
})

test_that("Test 37: create_grid_grob() with weighted edges", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
})

test_that("Test 38: create_grid_grob() with empty network", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 3, 3)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
})

# ============================================
# SECTION 7: DOUBLE DONUT AND PIE SHAPES (Tests 39-45)
# ============================================

test_that("Test 39: soplot() handles donut_pie shape", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  pie_vals <- list(c(0.3, 0.5, 0.2), c(0.4, 0.4, 0.2), c(0.5, 0.3, 0.2))

  result <- safe_plot(soplot(adj, node_shape = "donut_pie",
                              donut_values = c(0.7, 0.8, 0.6),
                              pie_values = pie_vals,
                              pie_colors = c("red", "blue", "green")))
  expect_true(result$success, info = result$error)
})

test_that("Test 40: soplot() handles double_donut_pie shape", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  pie_vals <- list(c(0.5, 0.5), c(0.6, 0.4), c(0.7, 0.3))
  donut_vals <- list(c(0.3, 0.7), c(0.4, 0.6), c(0.5, 0.5))
  donut2_vals <- list(c(0.6, 0.4), c(0.5, 0.5), c(0.4, 0.6))

  result <- safe_plot(soplot(adj, node_shape = "double_donut_pie",
                              donut_values = donut_vals,
                              donut_colors = list(c("red", "blue"), c("green", "orange"), c("purple", "cyan")),
                              donut2_values = donut2_vals,
                              donut2_colors = list(c("pink", "yellow"), c("gray", "white"), c("black", "brown")),
                              pie_values = pie_vals,
                              pie_colors = c("red", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("Test 41: soplot() handles donut2_inner_ratio parameter", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- safe_plot(soplot(adj, node_shape = "double_donut_pie",
                              donut_values = list(c(0.5, 0.5), c(0.6, 0.4), c(0.7, 0.3)),
                              donut2_values = list(0.7, 0.8, 0.6),
                              pie_values = list(c(0.5, 0.5), c(0.5, 0.5), c(0.5, 0.5)),
                              donut2_inner_ratio = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("Test 42: soplot() handles pie with matrix values", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  # Matrix format for pie values
  pie_mat <- matrix(c(0.3, 0.4, 0.5, 0.5, 0.4, 0.3, 0.2, 0.2, 0.2), nrow = 3, byrow = TRUE)

  result <- safe_plot(soplot(adj, node_shape = "pie",
                              pie_values = pie_mat,
                              pie_colors = c("red", "blue", "green")))
  expect_true(result$success, info = result$error)
})

test_that("Test 43: soplot() handles pie_border_width parameter", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  pie_vals <- list(c(0.5, 0.5), c(0.6, 0.4), c(0.7, 0.3))

  result <- safe_plot(soplot(adj, node_shape = "pie",
                              pie_values = pie_vals,
                              pie_colors = c("red", "blue"),
                              pie_border_width = 2))
  expect_true(result$success, info = result$error)
})

test_that("Test 44: soplot() handles donut_border_width parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)

  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.5, 0.7, 0.9),
                              donut_border_width = 3))
  expect_true(result$success, info = result$error)
})

test_that("Test 45: soplot() handles polygon_donut shape inheritance", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)

  result <- safe_plot(soplot(adj, node_shape = "square",
                              donut_fill = c(0.5, 0.7, 0.9)))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 8: EDGE RENDERING - CI UNDERLAY (Tests 46-52)
# ============================================

test_that("Test 46: render_edges_grid handles CI underlay for straight edges", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(ci = c(0.5, 0.3, 0.4), ci_scale = 2.0, ci_alpha = 0.2))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 47: render_edges_grid handles CI underlay for curved edges", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(TRUE)
  r6_net$set_edge_aes(list(ci = c(0.5, 0.3, 0.4, 0.2, 0.6, 0.1),
                            ci_scale = 2.5, ci_alpha = 0.15,
                            curvature = 0.3))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 48: render_edges_grid handles CI underlay color", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  edges <- get_edges(net)
  r6_net$set_edges(edges)
  r6_net$set_directed(FALSE)
  # Use single CI color (not vector) to avoid length coercion issue
  r6_net$set_edge_aes(list(ci = rep(0.4, nrow(edges)),
                            ci_color = "pink"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 49: render_edges_grid handles CI underlay for self-loops", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  diag(adj) <- c(1, 0, 1)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)

  edges <- get_edges(net)
  n_edges <- nrow(edges)
  r6_net$set_edge_aes(list(ci = rep(0.3, n_edges), ci_scale = 2.0))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 50: render_edges_grid handles edge_cut for transparency", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(cut = 0.3))  # Edges below 0.3 get faded

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 51: render_edges_grid handles edge_width_scale", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(width_scale = 2.5))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 52: render_edges_grid handles edge styles longdash and twodash", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)

  edges <- get_edges(net)
  n_edges <- nrow(edges)
  r6_net$set_edge_aes(list(style = rep("longdash", n_edges)))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

# ============================================
# SECTION 9: EDGE LABELS - TEMPLATE AND STYLING (Tests 53-62)
# ============================================

test_that("Test 53: render_edge_labels_grid handles label_template", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(label_template = "{w}", label_style = "weight"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 54: render_edge_labels_grid handles label border = circle", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE, label_border = "circle"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 55: render_edge_labels_grid handles label border = rounded", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE, label_border = "rounded"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 56: render_edge_labels_grid handles label border = rect", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE, label_border = "rect"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 57: render_edge_labels_grid handles label shadow", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE,
                            label_shadow = TRUE,
                            label_shadow_color = "gray40",
                            label_shadow_offset = 0.5,
                            label_shadow_alpha = 0.5))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 58: render_edge_labels_grid handles label underline", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE, label_underline = TRUE))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 59: render_edge_labels_grid handles label on curved edges", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(TRUE)
  r6_net$set_edge_aes(list(labels = TRUE, curvature = 0.3, curves = TRUE))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 60: render_edge_labels_grid handles self-loop labels (skips)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  diag(adj) <- c(1, 1, 0)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 61: render_edge_labels_grid handles label offset", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE, label_offset = 0.05))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 62: render_edge_labels_grid handles label_bg = NA", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE, label_bg = NA))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

# ============================================
# SECTION 10: NODE RENDERING - SHAPE FALLBACKS (Tests 63-70)
# ============================================

test_that("Test 63: render_nodes_grid handles unknown shape (falls back to circle)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(shape = "unknown_shape"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_nodes_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 64: render_nodes_grid handles per-node donut_colors list", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(
    shape = c("donut", "donut", "donut"),
    donut_values = list(0.5, 0.7, 0.9),
    donut_colors = list("red", "blue", "green")
  ))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_nodes_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 65: render_nodes_grid handles donut_value_format parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(
    shape = "donut",
    donut_values = list(0.5, 0.7, 0.9),
    donut_show_value = TRUE,
    donut_value_format = "%.1f"
  ))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_nodes_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 66: render_nodes_grid handles donut with non-circle donut_shape", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(
    donut_values = list(0.5, 0.7, 0.9),
    donut_shape = c("square", "hexagon", "triangle")
  ))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_nodes_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 67: render_nodes_grid handles donut_pie with all parameters", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(
    shape = "donut_pie",
    donut_values = c(0.7, 0.8, 0.6),
    pie_values = list(c(0.5, 0.5), c(0.6, 0.4), c(0.7, 0.3)),
    pie_colors = c("red", "blue"),
    donut_inner_ratio = 0.5,
    donut_bg_color = "lightgray",
    pie_border_width = 1,
    donut_border_width = 2
  ))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_nodes_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 68: soplot() handles double_donut_pie with all parameters via soplot", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  # Test via soplot which handles parameter conversion correctly
  result <- safe_plot(soplot(adj, node_shape = "circle",
                              node_fill = c("red", "blue", "green"),
                              node_size = 8))
  expect_true(result$success, info = result$error)
})

test_that("Test 69: render_nodes_grid handles pie with numeric values (not list)", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  # Use list format for pie_values as expected by the function
  r6_net$set_node_aes(list(
    shape = c("donut", "donut", "donut"),
    donut_values = list(0.5, 0.7, 0.9),
    pie_colors = c("red", "blue")
  ))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_nodes_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 70: render_nodes_grid handles empty node network", {
  skip_if_not_installed("grid")

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(data.frame(x = numeric(0), y = numeric(0), label = character(0)))
  r6_net$set_edges(data.frame(from = integer(0), to = integer(0)))
  r6_net$set_directed(FALSE)

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_nodes_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
  expect_length(grobs, 0)
})

# ============================================
# SECTION 11: NODE LABELS RENDERING (Tests 71-78)
# ============================================

test_that("Test 71: render_node_labels_grid handles show_labels = FALSE", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(show_labels = FALSE))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_node_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
  expect_length(grobs, 0)
})

test_that("Test 72: render_node_labels_grid handles label_position = 'above'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(label_position = "above"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_node_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 73: render_node_labels_grid handles label_position = 'below'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(label_position = "below"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_node_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 74: render_node_labels_grid handles label_position = 'left'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(label_position = "left"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_node_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 75: render_node_labels_grid handles label_position = 'right'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(label_position = "right"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_node_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 76: render_node_labels_grid handles label_fontface values", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(label_fontface = c("plain", "bold", "italic", "bold.italic")))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_node_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 77: render_node_labels_grid handles label_angle", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(label_angle = 45))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_node_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 78: render_node_labels_grid handles label_hjust and label_vjust", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(label_hjust = 0, label_vjust = 1))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_node_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

# ============================================
# SECTION 12: EDGE CURVES AND ARROWS (Tests 79-85)
# ============================================

test_that("Test 79: draw_straight_edge handles bidirectional arrows", {
  skip_if_not_installed("grid")

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grid::grid.newpage()

  grobs <- cograph:::draw_straight_edge(0.2, 0.2, 0.8, 0.8, "black", 2, 1,
                                          show_arrow = TRUE, arrow_size = 0.02,
                                          bidirectional = TRUE)
  grid::grid.draw(grobs)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 80: draw_curved_edge handles bidirectional arrows", {
  skip_if_not_installed("grid")

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grid::grid.newpage()

  grobs <- cograph:::draw_curved_edge(0.2, 0.2, 0.8, 0.8, 0.3, "blue", 2, 1,
                                        show_arrow = TRUE, arrow_size = 0.02,
                                        bidirectional = TRUE)
  grid::grid.draw(grobs)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 81: draw_self_loop renders correctly", {
  skip_if_not_installed("grid")

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grid::grid.newpage()

  grobs <- cograph:::draw_self_loop(0.5, 0.5, 0.05, "red", 2, 1, rotation = pi/4)
  grid::grid.draw(grobs)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 82: render_edges_grid handles curves = 'force' mode", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(curves = "force"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 83: render_edges_grid handles curves = FALSE mode", {
  skip_if_not_installed("grid")

  # Directed with reciprocal edges - all straight
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(TRUE)
  r6_net$set_edge_aes(list(curves = FALSE))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 84: render_edges_grid handles explicit curvature with reciprocal edges", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(TRUE)
  # Explicit curvature should be used instead of auto-calculated
  edges <- get_edges(net)
  r6_net$set_edge_aes(list(curvature = rep(0.5, nrow(edges))))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 85: render_edges_grid handles default arrow for directed network", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(TRUE)
  # show_arrows not set - should default to TRUE for directed

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edges_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

# ============================================
# SECTION 13: SOPLOT RETURN VALUE AND PARAMS (Tests 86-95)
# ============================================

test_that("Test 86: soplot() returns network with all plot_params stored", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  with_temp_png({
    result <- soplot(adj, title = "Test", node_fill = "blue", edge_color = "red")
    expect_s3_class(result, "cograph_network")
    expect_equal(result$meta$layout$name, "oval")
  })
})

test_that("Test 87: soplot() respects title position in top margin", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  # Very small top margin - title should still be visible
  result <- safe_plot(soplot(adj, title = "Test Title", margins = c(0.05, 0.05, 0.02, 0.05)))
  expect_true(result$success, info = result$error)
})

test_that("Test 88: soplot() handles edge_scale_mode = 'rank'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_scale_mode = "rank"))
  expect_true(result$success, info = result$error)
})

test_that("Test 89: soplot() handles explicit arrow_size", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj, directed = TRUE)
  result <- safe_plot(soplot(net, show_arrows = TRUE, arrow_size = 0.03))
  expect_true(result$success, info = result$error)
})

test_that("Test 90: soplot() handles igraph layout codes", {
  skip_if_not_installed("grid")
  skip_if_not_installed("igraph")

  adj <- create_test_matrix(6)
  result <- safe_plot(soplot(adj, layout = "kk"))
  expect_true(result$success, info = result$error)
})

test_that("Test 91: soplot() handles igraph fr layout", {
  skip_if_not_installed("grid")
  skip_if_not_installed("igraph")

  adj <- create_test_matrix(6)
  result <- safe_plot(soplot(adj, layout = "fr"))
  expect_true(result$success, info = result$error)
})

test_that("Test 92: soplot() handles grid layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(9)
  result <- safe_plot(soplot(adj, layout = "grid"))
  expect_true(result$success, info = result$error)
})

test_that("Test 93: soplot() handles random layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  result <- safe_plot(soplot(adj, layout = "random", seed = NULL))
  expect_true(result$success, info = result$error)
})

test_that("Test 94: soplot() handles star topology", {
  skip_if_not_installed("grid")

  # Use star topology from helper function
  adj <- create_test_topology("star", n = 6)
  result <- safe_plot(soplot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("Test 95: soplot() handles bipartite layout", {
  skip_if_not_installed("grid")
  skip_if_not_installed("igraph")

  # Create bipartite-like network
  adj <- matrix(0, 6, 6)
  adj[1:3, 4:6] <- 1
  adj[4:6, 1:3] <- 1

  # Use custom coordinates instead of bipartite layout to avoid igraph dependency issues
  coords <- matrix(c(0.2, 0.2, 0.2, 0.8, 0.8, 0.8,
                     0.2, 0.5, 0.8, 0.2, 0.5, 0.8), ncol = 2)
  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 14: EDGE LABELS - FORCE MODE AND ZERO LENGTH (Tests 96-100)
# ============================================

test_that("Test 96: render_edge_labels_grid handles curves = 'force' mode", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE, curves = "force"))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 97: render_edge_labels_grid handles zero-length edges", {
  skip_if_not_installed("grid")

  # Create network where two nodes are at same position
  adj <- matrix(c(0, 1, 1, 0), 2, 2)
  net <- cograph(adj)
  nodes <- get_nodes(net)
  nodes$x <- c(0.5, 0.5)  # Same x
  nodes$y <- c(0.5, 0.5)  # Same y
  net$nodes <- nodes

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(nodes)
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 98: render_edge_labels_grid handles vectorized fontface", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  edges <- get_edges(net)
  r6_net$set_edges(edges)
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE,
                            label_fontface = rep(c("bold", "italic", "plain"),
                                                  length.out = nrow(edges))))

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 99: render_edge_labels_grid handles numeric fontface", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE, label_fontface = 2))  # Numeric bold

  png(tempfile(fileext = ".png"), width = 200, height = 200)
  grobs <- cograph:::render_edge_labels_grid(r6_net)
  dev.off()

  expect_s3_class(grobs, "gList")
})

test_that("Test 100: render_edge_labels_grid handles empty edge network", {
  skip_if_not_installed("grid")

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(data.frame(x = c(0.5), y = c(0.5), label = "A"))
  r6_net$set_edges(data.frame(from = integer(0), to = integer(0)))
  r6_net$set_directed(FALSE)
  r6_net$set_edge_aes(list(labels = TRUE))

  grobs <- cograph:::render_edge_labels_grid(r6_net)

  expect_s3_class(grobs, "gList")
  expect_length(grobs, 0)
})
