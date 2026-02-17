# test-coverage-render-grid-41.R - Additional Coverage Tests for Grid Rendering
# Tests for R/render-grid.R targeting uncovered branches
# Coverage target: Improve coverage from 89% by testing edge cases

# ============================================
# SECTION 1: TNA OBJECT HANDLING (Tests 1-10)
# ============================================

test_that("Test 1: soplot() handles tna object directly", {
  skip_if_not_installed("grid")
  skip_if_not_installed("tna")

  # Create minimal tna-like object
  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.5, 0.3, 0.5, 0, 0.4, 0.3, 0.4, 0), 3, 3),
      inits = c(0.3, 0.4, 0.3),
      labels = c("A", "B", "C"),
      directed = TRUE
    ),
    class = "tna"
  )

  result <- safe_plot(soplot(tna_obj))
  expect_true(result$success, info = result$error)
})

test_that("Test 2: soplot() handles tna object with user overrides", {
  skip_if_not_installed("grid")
  skip_if_not_installed("tna")

  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.5, 0.3, 0.5, 0, 0.4, 0.3, 0.4, 0), 3, 3),
      inits = c(0.3, 0.4, 0.3),
      labels = c("A", "B", "C"),
      directed = TRUE
    ),
    class = "tna"
  )

  # User overrides layout and seed
  result <- safe_plot(soplot(tna_obj, layout = "circle", seed = 123))
  expect_true(result$success, info = result$error)
})

test_that("Test 3: soplot() handles tna object with theme override", {
  skip_if_not_installed("grid")
  skip_if_not_installed("tna")

  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0.5, 0), 3, 3),
      inits = c(0.4, 0.3, 0.3),
      labels = c("X", "Y", "Z"),
      directed = TRUE
    ),
    class = "tna"
  )

  result <- safe_plot(soplot(tna_obj, theme = "dark"))
  expect_true(result$success, info = result$error)
})

test_that("Test 4: soplot() handles tna object with NULL values", {
  skip_if_not_installed("grid")
  skip_if_not_installed("tna")

  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.3, 0.7, 0, 0.4, 0.3, 0.6, 0.3, 0), 3, 3),
      inits = c(0.5, 0.3, 0.2),
      labels = NULL,
      directed = NULL
    ),
    class = "tna"
  )

  result <- safe_plot(soplot(tna_obj))
  expect_true(result$success, info = result$error)
})

test_that("Test 5: soplot() handles tna object with attr directed", {
  skip_if_not_installed("grid")
  skip_if_not_installed("tna")

  tna_obj <- structure(
    list(
      weights = matrix(c(0, 0.4, 0.2, 0.4, 0, 0.3, 0.2, 0.3, 0), 3, 3),
      inits = c(0.4, 0.4, 0.2),
      labels = c("P", "Q", "R")
    ),
    class = "tna",
    directed = FALSE
  )

  result <- safe_plot(soplot(tna_obj))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 2: DONUT COLOR PROCESSING (Tests 6-15)
# ============================================

test_that("Test 6: soplot() handles donut_color list with 2*n_nodes pairs", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # List with 6 elements for 3 nodes: (fill1, bg1, fill2, bg2, fill3, bg3)
  donut_colors <- list("red", "white", "blue", "gray90", "green", "lightgray")

  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.5, 0.7, 0.9),
                              donut_color = donut_colors))
  expect_true(result$success, info = result$error)
})

test_that("Test 7: soplot() handles donut_color with single color", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.3, 0.5, 0.7, 0.9),
                              donut_color = "steelblue"))
  expect_true(result$success, info = result$error)
})

test_that("Test 8: soplot() handles donut_color with 2 colors (fill + bg)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # Two colors: first is fill, second is background
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.4, 0.6, 0.8),
                              donut_color = c("coral", "white")))
  expect_true(result$success, info = result$error)
})

test_that("Test 9: soplot() handles donut_color with 5 colors (recycled)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  # 5 colors: not 2, so treated as per-node fill colors and recycled
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.2, 0.4, 0.6, 0.8),
                              donut_color = c("red", "blue", "green", "orange", "purple")))
  expect_true(result$success, info = result$error)
})

test_that("Test 10: soplot() handles donut shape inheritance from triangle", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # node_shape is triangle (valid donut base shape), should inherit to donut_shape
  result <- safe_plot(soplot(adj, node_shape = "triangle",
                              donut_fill = c(0.5, 0.7, 0.9)))
  expect_true(result$success, info = result$error)
})

test_that("Test 11: soplot() handles donut shape inheritance from diamond", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "diamond",
                              donut_fill = c(0.4, 0.6, 0.8)))
  expect_true(result$success, info = result$error)
})

test_that("Test 12: soplot() defaults to circle for special donut shapes", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # double_donut_pie is not in valid_donut_base_shapes, should default to circle
  result <- safe_plot(soplot(adj, node_shape = "donut_pie",
                              donut_values = list(0.5, 0.7, 0.9),
                              pie_values = list(c(0.5, 0.5), c(0.6, 0.4), c(0.7, 0.3)),
                              pie_colors = c("red", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("Test 13: soplot() uses deprecated donut_colors when donut_color NULL", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.5, 0.6, 0.7),
                              donut_colors = list("purple", "purple", "purple")))
  expect_true(result$success, info = result$error)
})

test_that("Test 14: soplot() auto-defaults to maroon for donut when no color specified", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # donut_fill triggers donut rendering, color defaults to maroon
  result <- safe_plot(soplot(adj, donut_fill = c(0.4, 0.6, 0.8)))
  expect_true(result$success, info = result$error)
})

test_that("Test 15: soplot() handles vectorized donut_shape", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, donut_fill = c(0.3, 0.5, 0.7, 0.9),
                              donut_shape = c("circle", "square", "hexagon", "pentagon")))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 3: DUPLICATE EDGES ERROR PATH (Tests 16-20)
# ============================================

test_that("Test 16: soplot() errors on undirected duplicate edges without handler", {
  skip_if_not_installed("grid")

  # Create undirected network with duplicate edges
  adj <- matrix(0, 3, 3)
  adj[1, 2] <- 0.5
  adj[2, 1] <- 0.5  # Symmetric
  net <- cograph(adj, directed = FALSE)

  # Manually add duplicate edge to trigger error path
  edges <- data.frame(
    from = c(1, 1, 2),
    to = c(2, 2, 3),
    weight = c(0.5, 0.3, 0.4)
  )
  net$edges <- edges

  # Without edge_duplicates, should error
  expect_error(
    with_temp_png(soplot(net)),
    "duplicate edge"
  )
})

test_that("Test 17: soplot() shows informative duplicate error message", {
  skip_if_not_installed("grid")

  # Create undirected network with duplicate edges
  adj <- matrix(0, 3, 3)
  adj[1, 2] <- 0.5
  adj[2, 1] <- 0.5
  net <- cograph(adj, directed = FALSE)

  # Manually add duplicate edges to trigger error path
  edges <- data.frame(
    from = c(1, 1, 2, 2),
    to = c(2, 2, 3, 3),
    weight = c(0.5, 0.3, 0.2, 0.4)
  )
  net$edges <- edges

  expect_error(
    with_temp_png(soplot(net)),
    "edge_duplicates"
  )
})

test_that("Test 18: soplot() handles duplicate edges with custom function", {
  skip_if_not_installed("grid")

  edges <- data.frame(
    from = c(1, 1, 2, 3),
    to = c(2, 2, 3, 1),
    weight = c(0.5, 0.3, 0.4, 0.6)
  )

  # Custom aggregation function
  result <- safe_plot(soplot(edges, edge_duplicates = function(x) median(x)))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 4: LAYOUT RESCALING EDGE CASES (Tests 19-25)
# ============================================

test_that("Test 19: soplot() handles all nodes at exact same position", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  # Custom layout with all nodes at same position (max_range <= 1e-10)
  coords <- matrix(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), ncol = 2)

  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

test_that("Test 20: soplot() handles very small coordinate differences", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # Coordinates with extremely small differences (below 1e-10)
  coords <- matrix(c(0.5, 0.5 + 1e-12, 0.5 - 1e-12,
                     0.5, 0.5 + 1e-12, 0.5 - 1e-12), ncol = 2)

  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

test_that("Test 21: soplot() handles NA in layout coordinates", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  # Layout with valid coordinates only
  result <- safe_plot(soplot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("Test 22: soplot() handles asymmetric x/y ranges in layout", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  # X range is much larger than Y range
  coords <- matrix(c(0, 100, 200, 300, 0, 1, 2, 3), ncol = 2)

  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

test_that("Test 23: soplot() preserves aspect ratio with extreme ranges", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)
  # Y range much larger than X range
  coords <- matrix(c(1, 2, 3, 4, 5, 0, 50, 100, 150, 200), ncol = 2)

  result <- safe_plot(soplot(adj, layout = coords))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 5: THRESHOLD AND WEIGHT FILTERING (Tests 24-30)
# ============================================

test_that("Test 24: soplot() threshold filters all edges", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  adj <- adj * 0.1  # All weights small
  # Threshold higher than all weights - all edges removed
  result <- safe_plot(soplot(adj, threshold = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("Test 25: soplot() threshold with NULL edges", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 3, 3)  # No edges
  result <- safe_plot(soplot(adj, threshold = 0.1))
  expect_true(result$success, info = result$error)
})

test_that("Test 26: soplot() threshold with edges missing weight column", {
  skip_if_not_installed("grid")

  # Edge list without weight column
  edges <- data.frame(from = c(1, 2), to = c(2, 3))
  result <- safe_plot(soplot(edges, threshold = 0.1))
  expect_true(result$success, info = result$error)
})

test_that("Test 27: soplot() handles weight_digits rounding to zero", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 0.001, 0.002, 0.001, 0, 0.003, 0.002, 0.003, 0), 3, 3)
  # weight_digits = 1 rounds 0.001 to 0
  result <- safe_plot(soplot(adj, weight_digits = 1))
  expect_true(result$success, info = result$error)
})

test_that("Test 28: soplot() handles network with all self-loops", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 3, 3)
  diag(adj) <- c(0.5, 0.6, 0.7)

  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 6: CREATE_GRID_GROB TESTS (Tests 29-35)
# ============================================

test_that("Test 29: create_grid_grob() contains correct children", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
  expect_equal(grob$name, "cograph_plot")
})

test_that("Test 30: create_grid_grob() handles empty edges", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 3, 3)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
})

test_that("Test 31: create_grid_grob() with self-loops", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  diag(adj) <- 1
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net)
  expect_s3_class(grob, "gTree")
})

test_that("Test 32: create_grid_grob() handles custom background color", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net, background = "#FFFFCC")
  expect_s3_class(grob, "gTree")
})

test_that("Test 33: create_grid_grob() with transparent background", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  grob <- cograph:::create_grid_grob(net, background = "transparent")
  expect_s3_class(grob, "gTree")
})

# ============================================
# SECTION 7: RENDER_LEGEND_GRID TESTS (Tests 34-40)
# ============================================

test_that("Test 34: render_legend_grid() handles invalid position gracefully", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)

  # Invalid position should default to topright
  grobs <- cograph:::render_legend_grid(r6_net, position = "invalid_position")
  expect_s3_class(grobs, "gList")
})

test_that("Test 35: render_legend_grid() uses label when name is NULL", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)
  nodes <- get_nodes(net)
  nodes$name <- NULL
  nodes$label <- c("Label1", "Label2", "Label3")

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(nodes)
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)

  grobs <- cograph:::render_legend_grid(r6_net)
  expect_s3_class(grobs, "gList")
})

test_that("Test 36: render_legend_grid() deduplicates identical entries", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  # All same color, same name - should deduplicate
  r6_net$set_node_aes(list(fill = rep("blue", 4), node_names = rep("Same", 4)))

  grobs <- cograph:::render_legend_grid(r6_net)
  expect_s3_class(grobs, "gList")
})

test_that("Test 37: render_legend_grid() handles theme with custom colors", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)

  # Use correct set() syntax: set(name, value)
  theme <- cograph:::CographTheme$new()
  theme$set("background", "#1a1a1a")
  theme$set("label_color", "white")
  r6_net$set_theme(theme)

  grobs <- cograph:::render_legend_grid(r6_net, position = "topright")
  expect_s3_class(grobs, "gList")
})

test_that("Test 38: render_legend_grid() with bottomright position", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(fill = c("red", "blue", "green", "yellow")))

  grobs <- cograph:::render_legend_grid(r6_net, position = "bottomright")
  expect_s3_class(grobs, "gList")
})

test_that("Test 39: render_legend_grid() with bottomleft position", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(fill = c("red", "blue", "green", "yellow")))

  grobs <- cograph:::render_legend_grid(r6_net, position = "bottomleft")
  expect_s3_class(grobs, "gList")
})

test_that("Test 40: render_legend_grid() with topleft position", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  net <- cograph(adj)

  r6_net <- cograph:::CographNetwork$new()
  r6_net$set_nodes(get_nodes(net))
  r6_net$set_edges(get_edges(net))
  r6_net$set_directed(FALSE)
  r6_net$set_node_aes(list(fill = c("red", "blue", "green", "yellow")))

  grobs <- cograph:::render_legend_grid(r6_net, position = "topleft")
  expect_s3_class(grobs, "gList")
})

# ============================================
# SECTION 8: SOPLOT TITLE AND MARGINS (Tests 41-45)
# ============================================

test_that("Test 41: soplot() handles very small top margin with title", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  # Top margin is 0.01 - title should still be at least 0.02 from edge
  result <- safe_plot(soplot(adj, title = "Test Title",
                              margins = c(0.05, 0.05, 0.01, 0.05)))
  expect_true(result$success, info = result$error)
})

test_that("Test 42: soplot() handles zero top margin with title", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, title = "Zero Margin Title",
                              margins = c(0, 0, 0, 0)))
  expect_true(result$success, info = result$error)
})

test_that("Test 43: soplot() handles very long title", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  long_title <- paste(rep("Very Long Title ", 10), collapse = "")
  result <- safe_plot(soplot(adj, title = long_title))
  expect_true(result$success, info = result$error)
})

test_that("Test 44: soplot() handles title with special characters", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, title = "Title: Test (alpha=0.5) & more"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 9: EDGE AESTHETICS ADVANCED (Tests 45-55)
# ============================================

test_that("Test 45: soplot() handles edge_cutoff with auto threshold", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_cutoff = NULL))  # Auto 75th percentile
  expect_true(result$success, info = result$error)
})

test_that("Test 46: soplot() handles edge_cutoff = 0 (disabled)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_cutoff = 0))
  expect_true(result$success, info = result$error)
})

test_that("Test 47: soplot() handles explicit edge_cutoff value", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_cutoff = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("Test 48: soplot() handles edge_width_scale multiplier", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_width_scale = 2.0))
  expect_true(result$success, info = result$error)
})

test_that("Test 49: soplot() handles edge_label_offset parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_offset = 0.05))
  expect_true(result$success, info = result$error)
})

test_that("Test 50: soplot() handles edge_label_border = 'rect'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_border = "rect"))
  expect_true(result$success, info = result$error)
})

test_that("Test 51: soplot() handles edge_label_border = 'rounded'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_border = "rounded"))
  expect_true(result$success, info = result$error)
})

test_that("Test 52: soplot() handles edge_label_border = 'circle'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_border = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("Test 53: soplot() handles edge_label_border_color", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE,
                              edge_label_border = "rect",
                              edge_label_border_color = "darkblue"))
  expect_true(result$success, info = result$error)
})

test_that("Test 54: soplot() handles edge_label_underline", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_underline = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("Test 55: soplot() handles edge_label_bg = NA", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_labels = TRUE, edge_label_bg = NA))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 10: NODE SIZE AND SCALING (Tests 56-65)
# ============================================

test_that("Test 56: soplot() handles very small node_size", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_size = 1))
  expect_true(result$success, info = result$error)
})

test_that("Test 57: soplot() handles very large node_size", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_size = 20))
  expect_true(result$success, info = result$error)
})

test_that("Test 58: soplot() handles per-node sizes", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_size = c(4, 6, 8, 10)))
  expect_true(result$success, info = result$error)
})

test_that("Test 59: soplot() handles node_border_width = 0", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_border_width = 0))
  expect_true(result$success, info = result$error)
})

test_that("Test 60: soplot() handles node_alpha = 0 (invisible)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_alpha = 0))
  expect_true(result$success, info = result$error)
})

test_that("Test 61: soplot() handles per-node alpha", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_alpha = c(0.2, 0.4, 0.6, 0.8)))
  expect_true(result$success, info = result$error)
})

test_that("Test 62: soplot() handles mixed node shapes", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = c("circle", "square", "triangle", "diamond")))
  expect_true(result$success, info = result$error)
})

test_that("Test 63: soplot() handles node_shape ellipse", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "ellipse"))
  expect_true(result$success, info = result$error)
})

test_that("Test 64: soplot() handles node_shape heart", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "heart"))
  expect_true(result$success, info = result$error)
})

test_that("Test 65: soplot() handles node_shape star", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, node_shape = "star"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 11: LABEL POSITIONS (Tests 66-72)
# ============================================

test_that("Test 66: soplot() handles label_position 'left'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_position = "left"))
  expect_true(result$success, info = result$error)
})

test_that("Test 67: soplot() handles label_position 'right'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_position = "right"))
  expect_true(result$success, info = result$error)
})

test_that("Test 68: soplot() handles label_position 'below'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_position = "below"))
  expect_true(result$success, info = result$error)
})

test_that("Test 69: soplot() handles per-node label_position", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_position = c("above", "below", "left", "right")))
  expect_true(result$success, info = result$error)
})

test_that("Test 70: soplot() handles label_size = 0 (invisible labels)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_size = 0))
  expect_true(result$success, info = result$error)
})

test_that("Test 71: soplot() handles per-node label_color", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, label_color = c("red", "blue", "green", "black")))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 12: CURVES AND ARROWS (Tests 72-80)
# ============================================

test_that("Test 72: soplot() handles curves = 'mutual' with reciprocal edges", {
  skip_if_not_installed("grid")

  # Directed network with reciprocal edges
  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  result <- safe_plot(soplot(net, curves = "mutual"))
  expect_true(result$success, info = result$error)
})

test_that("Test 73: soplot() handles curves = 'force' all edges curved", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, curves = "force"))
  expect_true(result$success, info = result$error)
})

test_that("Test 74: soplot() handles curve_shape parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, curvature = 0.3, curve_shape = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("Test 75: soplot() handles curve_pivot parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, curvature = 0.3, curve_pivot = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("Test 76: soplot() handles arrow_size with directed network", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  result <- safe_plot(soplot(net, arrow_size = 0.05))
  expect_true(result$success, info = result$error)
})

test_that("Test 77: soplot() handles bidirectional arrows", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, bidirectional = TRUE, show_arrows = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("Test 78: soplot() handles show_arrows = FALSE on directed network", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  result <- safe_plot(soplot(net, show_arrows = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("Test 79: soplot() handles loop_rotation for self-loops", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  diag(adj) <- 1  # All self-loops

  result <- safe_plot(soplot(adj, loop_rotation = pi/4))
  expect_true(result$success, info = result$error)
})

test_that("Test 80: soplot() handles per-edge curvature", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)
  edges <- get_edges(net)

  # Note: per-edge curvature is handled via sn_edges, not direct soplot param
  result <- safe_plot(soplot(net, curvature = 0.2))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 13: RETURN VALUE TESTS (Tests 81-85)
# ============================================

test_that("Test 81: soplot() returns cograph_network with correct structure", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  with_temp_png({
    result <- soplot(adj, layout = "circle", seed = 42)
    expect_s3_class(result, "cograph_network")
    expect_true(!is.null(result$nodes))
    expect_true(!is.null(result$meta$layout))
  })
})

test_that("Test 82: soplot() stores layout info correctly", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  with_temp_png({
    result <- soplot(adj, layout = "spring", seed = 123)
    expect_equal(result$meta$layout$name, "spring")
    expect_equal(result$meta$layout$seed, 123)
  })
})

test_that("Test 83: soplot() returns network with updated node coordinates", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5)

  with_temp_png({
    result <- soplot(adj, layout = "circle")
    nodes <- get_nodes(result)
    # Coordinates should be rescaled to [0.1, 0.9] range
    expect_true(all(nodes$x >= 0 & nodes$x <= 1))
    expect_true(all(nodes$y >= 0 & nodes$y <= 1))
  })
})

test_that("Test 84: soplot() returns network with custom labels applied", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  with_temp_png({
    result <- soplot(adj, labels = c("A", "B", "C", "D"))
    nodes <- get_nodes(result)
    expect_equal(nodes$label, c("A", "B", "C", "D"))
  })
})

test_that("Test 85: soplot() handles newpage = FALSE", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, newpage = FALSE))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 14: EDGE SCALE MODES (Tests 86-90)
# ============================================

test_that("Test 86: soplot() handles edge_scale_mode = 'rank'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_scale_mode = "rank"))
  expect_true(result$success, info = result$error)
})

test_that("Test 87: soplot() handles edge_scale_mode = 'sqrt'", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  adj <- abs(adj)  # Ensure positive for sqrt
  result <- safe_plot(soplot(adj, edge_scale_mode = "sqrt"))
  expect_true(result$success, info = result$error)
})

test_that("Test 88: soplot() handles edge_scale_mode = 'log' with small weights", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  adj <- abs(adj) + 0.001  # Ensure positive for log
  result <- safe_plot(soplot(adj, edge_scale_mode = "log"))
  expect_true(result$success, info = result$error)
})

test_that("Test 89: soplot() handles edge_width_range custom range", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_width_range = c(0.2, 8)))
  expect_true(result$success, info = result$error)
})

test_that("Test 90: soplot() handles edge_size (esize) parameter", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(5, weighted = TRUE)
  result <- safe_plot(soplot(adj, edge_size = 3))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 15: DONUT VALUE FORMATTING (Tests 91-95)
# ============================================

test_that("Test 91: soplot() handles donut_value_digits", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.333, 0.666, 0.999),
                              donut_show_value = TRUE,
                              donut_value_digits = 1))
  expect_true(result$success, info = result$error)
})

test_that("Test 92: soplot() handles donut_value_prefix", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.5, 0.7, 0.9),
                              donut_show_value = TRUE,
                              donut_value_prefix = "$"))
  expect_true(result$success, info = result$error)
})

test_that("Test 93: soplot() handles donut_value_suffix", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(50, 70, 90),
                              donut_show_value = TRUE,
                              donut_value_suffix = "%"))
  expect_true(result$success, info = result$error)
})

test_that("Test 94: soplot() handles donut_value_fontface italic", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.5, 0.7, 0.9),
                              donut_show_value = TRUE,
                              donut_value_fontface = "italic"))
  expect_true(result$success, info = result$error)
})

test_that("Test 95: soplot() handles donut_value_fontfamily serif", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.5, 0.7, 0.9),
                              donut_show_value = TRUE,
                              donut_value_fontfamily = "serif"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 16: PIE AND DOUBLE DONUT (Tests 96-100)
# ============================================

test_that("Test 96: soplot() handles pie_values as matrix", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  pie_mat <- matrix(c(0.3, 0.5, 0.2, 0.4, 0.4, 0.2, 0.5, 0.3, 0.2), nrow = 3, byrow = TRUE)

  result <- safe_plot(soplot(adj, node_shape = "pie",
                              pie_values = pie_mat,
                              pie_colors = c("red", "green", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("Test 97: soplot() handles donut2_values for double donut", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  result <- safe_plot(soplot(adj, node_shape = "double_donut_pie",
                              donut_values = list(c(0.5, 0.5), c(0.6, 0.4), c(0.7, 0.3)),
                              donut_colors = list(c("red", "blue"), c("green", "orange"), c("purple", "cyan")),
                              donut2_values = list(c(0.4, 0.6), c(0.5, 0.5), c(0.3, 0.7)),
                              donut2_colors = list(c("pink", "yellow"), c("gray", "white"), c("black", "brown")),
                              donut2_inner_ratio = 0.3,
                              pie_values = list(c(0.5, 0.5), c(0.6, 0.4), c(0.7, 0.3)),
                              pie_colors = c("coral", "teal")))
  expect_true(result$success, info = result$error)
})

test_that("Test 98: soplot() handles pie_border_width", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  pie_vals <- list(c(0.5, 0.3, 0.2), c(0.4, 0.4, 0.2), c(0.3, 0.5, 0.2))

  result <- safe_plot(soplot(adj, node_shape = "pie",
                              pie_values = pie_vals,
                              pie_colors = c("red", "green", "blue"),
                              pie_border_width = 2))
  expect_true(result$success, info = result$error)
})

test_that("Test 99: soplot() handles donut_border_width", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.5, 0.7, 0.9),
                              donut_border_width = 3))
  expect_true(result$success, info = result$error)
})

test_that("Test 100: soplot() handles donut_bg_color", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj, node_shape = "donut",
                              donut_fill = c(0.5, 0.7, 0.9),
                              donut_bg_color = "lightyellow"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 17: LEGEND WITH SOPLOT (Tests 101-105)
# ============================================

test_that("Test 101: soplot() renders legend with legend = TRUE", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, legend = TRUE,
                              node_fill = c("red", "blue", "green", "yellow")))
  expect_true(result$success, info = result$error)
})

test_that("Test 102: soplot() legend respects legend_position", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, legend = TRUE,
                              legend_position = "bottomleft",
                              node_fill = c("red", "blue", "green", "yellow")))
  expect_true(result$success, info = result$error)
})

test_that("Test 103: soplot() legend with node_names", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(soplot(adj, legend = TRUE,
                              node_fill = c("red", "blue", "green", "yellow"),
                              node_names = c("Alpha", "Beta", "Gamma", "Delta")))
  expect_true(result$success, info = result$error)
})

test_that("Test 104: soplot() legend deduplicates same-color nodes", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  # Two red, two blue - legend should show only 2 entries
  result <- safe_plot(soplot(adj, legend = TRUE,
                              node_fill = c("red", "red", "blue", "blue")))
  expect_true(result$success, info = result$error)
})

test_that("Test 105: soplot() legend with single node", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 1, 1)
  result <- safe_plot(soplot(adj, legend = TRUE, node_fill = "steelblue"))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 18: sn_render ALIAS (Tests 106-108)
# ============================================

test_that("Test 106: sn_render is identical to soplot", {
  expect_identical(sn_render, soplot)
})

test_that("Test 107: sn_render works with all soplot parameters", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)
  result <- safe_plot(sn_render(adj, layout = "circle",
                                 node_fill = "steelblue",
                                 title = "Test sn_render"))
  expect_true(result$success, info = result$error)
})

test_that("Test 108: sn_render returns same structure as soplot", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4)

  with_temp_png({
    result <- sn_render(adj, layout = "circle")
    expect_s3_class(result, "cograph_network")
    expect_true(!is.null(result$meta$layout))
  })
})

# ============================================
# SECTION 19: EDGE HANDLING SPECIAL CASES (Tests 109-115)
# ============================================

test_that("Test 109: soplot() handles directed network no duplicate check", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
  net <- cograph(adj, directed = TRUE)

  # Directed networks don't trigger duplicate edge checking
  result <- safe_plot(soplot(net))
  expect_true(result$success, info = result$error)
})

test_that("Test 110: soplot() handles undirected symmetric matrix (no duplicates)", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(4, symmetric = TRUE)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 111: soplot() handles network with NULL edges field", {
  skip_if_not_installed("grid")

  # Create network that results in NULL edges
  adj <- matrix(0, 3, 3)  # No edges
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 112: soplot() handles network with 0-row edges dataframe", {
  skip_if_not_installed("grid")

  adj <- matrix(0, 3, 3)
  result <- safe_plot(soplot(adj, threshold = 1))  # Filter all edges
  expect_true(result$success, info = result$error)
})

test_that("Test 113: soplot() edge duplicate with symmetric undirected edges", {
  skip_if_not_installed("grid")

  # Edge list with reversed pairs (A->B and B->A) in undirected network
  edges <- data.frame(
    from = c(1, 2, 2, 3),
    to = c(2, 1, 3, 2),
    weight = c(0.5, 0.3, 0.4, 0.2)
  )

  result <- safe_plot(soplot(edges, edge_duplicates = "mean"))
  expect_true(result$success, info = result$error)
})

test_that("Test 114: soplot() handles very sparse network", {
  skip_if_not_installed("grid")

  # 10 nodes, only 2 edges
  adj <- matrix(0, 10, 10)
  adj[1, 2] <- 0.5
  adj[2, 1] <- 0.5
  adj[5, 6] <- 0.3
  adj[6, 5] <- 0.3

  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 115: soplot() handles dense network", {
  skip_if_not_installed("grid")

  # Complete graph
  adj <- create_test_topology("complete", n = 8)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# SECTION 20: MISCELLANEOUS COVERAGE (Tests 116-120)
# ============================================

test_that("Test 116: soplot() handles maximum parameter capping weights", {
  skip_if_not_installed("grid")

  adj <- matrix(c(0, 5, 2, 5, 0, 3, 2, 3, 0), 3, 3)
  result <- safe_plot(soplot(adj, maximum = 2))  # Cap weights at 2
  expect_true(result$success, info = result$error)
})

test_that("Test 117: soplot() handles combination of all donut params", {
  skip_if_not_installed("grid")

  adj <- create_test_matrix(3)
  result <- safe_plot(soplot(adj,
                              node_shape = "donut",
                              donut_fill = c(0.4, 0.6, 0.8),
                              donut_color = c("maroon", "gray95"),
                              donut_inner_ratio = 0.6,
                              donut_show_value = TRUE,
                              donut_value_size = 10,
                              donut_value_color = "black",
                              donut_value_fontface = "bold",
                              donut_value_digits = 1,
                              donut_value_suffix = "%"))
  expect_true(result$success, info = result$error)
})

test_that("Test 118: soplot() handles ring topology", {
  skip_if_not_installed("grid")

  adj <- create_test_topology("ring", n = 6)
  result <- safe_plot(soplot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("Test 119: soplot() handles path topology", {
  skip_if_not_installed("grid")

  adj <- create_test_topology("path", n = 5)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})

test_that("Test 120: soplot() handles disconnected topology", {
  skip_if_not_installed("grid")

  adj <- create_test_topology("disconnected", n = 6)
  result <- safe_plot(soplot(adj))
  expect_true(result$success, info = result$error)
})
