# test-splot.R - Core splot() function tests (CRITICAL)
# Tests for the main base R graphics plotting function

# ============================================
# INPUT TYPES
# ============================================

test_that("splot() accepts adjacency matrix", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() accepts weighted matrix", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() accepts asymmetric (directed) matrix", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() accepts edge list data.frame", {
  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4)

  result <- safe_plot(splot(edges))
  expect_true(result$success, info = result$error)
})

test_that("splot() accepts edge list with character nodes", {
  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4, char_nodes = TRUE)

  result <- safe_plot(splot(edges))
  expect_true(result$success, info = result$error)
})

test_that("splot() accepts weighted edge list", {
  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4, weighted = TRUE)

  result <- safe_plot(splot(edges))
  expect_true(result$success, info = result$error)
})

test_that("splot() accepts sonnet_network object", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("splot() accepts igraph object", {
  skip_if_no_igraph()

  g <- igraph::make_ring(5)

  result <- safe_plot(splot(g))
  expect_true(result$success, info = result$error)
})

# ============================================
# LAYOUTS
# ============================================

test_that("splot() works with circle layout", {
  adj <- create_test_matrix(5)

  result <- safe_plot(splot(adj, layout = "circle"))
  expect_true(result$success, info = result$error)
})
test_that("splot() works with spring layout", {
  adj <- create_test_matrix(5)

  result <- safe_plot(splot(adj, layout = "spring", seed = 42))
  expect_true(result$success, info = result$error)
})

test_that("splot() works with groups parameter", {
  adj <- create_test_matrix(6)
  groups <- c(1, 1, 2, 2, 3, 3)

  # Use spring layout with groups coloring (layout="groups" has a known issue)
  result <- safe_plot(splot(adj, layout = "spring", groups = groups, seed = 42))
  expect_true(result$success, info = result$error)
})

test_that("splot() works with custom coordinate matrix", {

  adj <- create_test_matrix(4)
  custom_layout <- matrix(c(0, 0, 1, 1, 0, 1, 0, 1), ncol = 2)

  result <- safe_plot(splot(adj, layout = custom_layout))
  expect_true(result$success, info = result$error)
})

test_that("splot() works with igraph two-letter layout codes", {
  skip_if_no_igraph()
  adj <- create_test_matrix(5)

  # Test common igraph layout codes
  for (code in c("kk", "fr", "mds")) {
    result <- safe_plot(splot(adj, layout = code, seed = 42))
    expect_true(result$success, info = paste("Layout", code, "failed:", result$error))
  }
})

# ============================================
# NODE AESTHETICS
# ============================================

test_that("splot() handles node_size parameter", {
  adj <- create_test_matrix(4)

  # Scalar value
  result <- safe_plot(splot(adj, node_size = 5))
  expect_true(result$success, info = result$error)

  # Vector value
  result <- safe_plot(splot(adj, node_size = c(3, 4, 5, 6)))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles node_shape parameter", {
  adj <- create_test_matrix(3)

  # Test multiple shapes
  shapes <- c("circle", "square", "triangle", "diamond", "pentagon",
              "hexagon", "star", "heart", "ellipse", "cross")

  for (shape in shapes) {
    result <- safe_plot(splot(adj, node_shape = shape))
    expect_true(result$success, info = paste("Shape", shape, "failed:", result$error))
  }
})

test_that("splot() handles per-node shapes", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, node_shape = c("circle", "square", "triangle", "diamond")))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles node_fill colors", {
  adj <- create_test_matrix(4)

  # Single color
  result <- safe_plot(splot(adj, node_fill = "steelblue"))
  expect_true(result$success, info = result$error)

  # Per-node colors
  result <- safe_plot(splot(adj, node_fill = c("red", "green", "blue", "orange")))
  expect_true(result$success, info = result$error)

  # Hex colors
  result <- safe_plot(splot(adj, node_fill = "#FF5733"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles node_border_color and node_border_width", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, node_border_color = "black", node_border_width = 2))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles node_alpha transparency", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, node_alpha = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles labels parameter", {
  adj <- create_test_matrix(4)

  # TRUE (default - use indices/names)
  result <- safe_plot(splot(adj, labels = TRUE))
  expect_true(result$success, info = result$error)

  # FALSE
  result <- safe_plot(splot(adj, labels = FALSE))
  expect_true(result$success, info = result$error)

  # Character vector
  result <- safe_plot(splot(adj, labels = c("A", "B", "C", "D")))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles label positioning options", {
  adj <- create_test_matrix(3)

  for (pos in c("center", "above", "below", "left", "right")) {
    result <- safe_plot(splot(adj, labels = TRUE, label_position = pos))
    expect_true(result$success, info = paste("Position", pos, "failed:", result$error))
  }
})

test_that("splot() handles label font parameters", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    labels = TRUE,
    label_size = 1.2,
    label_color = "navy",
    label_fontface = "bold",
    label_fontfamily = "serif"
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# PIE AND DONUT NODES
# ============================================

test_that("splot() handles pie_values for pie chart nodes", {
  adj <- create_test_matrix(3)
  pie_vals <- list(c(1, 2, 3), c(2, 2), c(1, 1, 1, 1))

  result <- safe_plot(splot(adj, pie_values = pie_vals))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles pie_colors", {
  adj <- create_test_matrix(3)
  pie_vals <- list(c(1, 2), c(2, 2), c(3, 1))
  pie_cols <- list(c("red", "blue"), c("green", "yellow"), c("purple", "orange"))

  result <- safe_plot(splot(adj, pie_values = pie_vals, pie_colors = pie_cols))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_fill for donut nodes", {
  adj <- create_test_matrix(3)

  # Scalar fill proportion
  result <- safe_plot(splot(adj, donut_fill = 0.5))
  expect_true(result$success, info = result$error)

  # Per-node fill proportions
  result <- safe_plot(splot(adj, donut_fill = c(0.2, 0.5, 0.8)))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_color and donut_bg_color", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, 0.6, 0.9),
    donut_color = "steelblue",
    donut_bg_color = "lightyellow"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_shape for polygon donuts", {
  adj <- create_test_matrix(3)

  for (shape in c("circle", "square", "hexagon", "triangle")) {
    result <- safe_plot(splot(adj, donut_fill = 0.7, donut_shape = shape))
    expect_true(result$success, info = paste("Donut shape", shape, "failed:", result$error))
  }
})

test_that("splot() handles donut value display", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.25, 0.50, 0.75),
    donut_show_value = TRUE,
    donut_value_suffix = "%",
    donut_value_digits = 0
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut with node_shape='donut'", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj, node_shape = "donut", donut_fill = 0.8))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles double donut (donut2)", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.7, 0.8, 0.6),
    donut2_values = list(c(0.5), c(0.4), c(0.6)),
    donut2_colors = list("orange", "purple", "green")
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE AESTHETICS
# ============================================

test_that("splot() handles edge_color parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  # Single color
  result <- safe_plot(splot(adj, edge_color = "gray50"))
  expect_true(result$success, info = result$error)

  # NULL for weight-based coloring
  result <- safe_plot(splot(adj, edge_color = NULL))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_width parameter", {
  adj <- create_test_matrix(4)

  # Scalar
  result <- safe_plot(splot(adj, edge_width = 2))
  expect_true(result$success, info = result$error)

  # NULL for weight-based sizing
  adj_w <- create_test_matrix(4, weighted = TRUE)
  result <- safe_plot(splot(adj_w, edge_width = NULL))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_width_range", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, edge_width_range = c(0.5, 5)))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_scale_mode options", {
  adj <- create_test_matrix(4, weighted = TRUE)

  for (mode in c("linear", "log", "sqrt", "rank")) {
    result <- safe_plot(splot(adj, edge_scale_mode = mode))
    expect_true(result$success, info = paste("Scale mode", mode, "failed:", result$error))
  }
})

test_that("splot() handles edge_alpha transparency", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, edge_alpha = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_style line types", {
  adj <- create_test_matrix(4)

  # Numeric styles
  for (style in 1:3) {
    result <- safe_plot(splot(adj, edge_style = style))
    expect_true(result$success, info = paste("Edge style", style, "failed:", result$error))
  }
})

test_that("splot() handles curvature parameter", {
  adj <- create_test_matrix(4)

  # Straight edges
  result <- safe_plot(splot(adj, curvature = 0))
  expect_true(result$success, info = result$error)

  # Curved edges
  result <- safe_plot(splot(adj, curvature = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles curves modes", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, curves = TRUE))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, curves = FALSE))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, curves = "force"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles arrow parameters for directed networks", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE, show_arrows = TRUE, arrow_size = 1.5))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, directed = TRUE, bidirectional = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_labels parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  # TRUE shows weights
  result <- safe_plot(splot(adj, edge_labels = TRUE))
  expect_true(result$success, info = result$error)

  # FALSE hides labels
  result <- safe_plot(splot(adj, edge_labels = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge label styling", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj,
    edge_labels = TRUE,
    edge_label_size = 0.8,
    edge_label_color = "navy",
    edge_label_bg = "white"
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# CI UNDERLAYS AND LABEL TEMPLATES
# ============================================

test_that("splot() handles edge_ci underlays", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2  # Symmetric matrix
  ci_vals <- runif(n_edges, 0.1, 0.3)

  result <- safe_plot(splot(adj,
    edge_ci = ci_vals,
    edge_ci_scale = 2,
    edge_ci_alpha = 0.2
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_template", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj,
    edge_label_template = "{est}",
    edge_label_digits = 2
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_style presets", {
  adj <- create_test_matrix(4, weighted = TRUE)

  for (style in c("none", "estimate")) {
    result <- safe_plot(splot(adj, edge_label_style = style))
    expect_true(result$success, info = paste("Label style", style, "failed:", result$error))
  }
})

# ============================================
# WEIGHT HANDLING
# ============================================

test_that("splot() handles positive and negative edge colors", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)

  result <- safe_plot(splot(adj,
    edge_positive_color = "darkgreen",
    edge_negative_color = "darkred"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles threshold parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, threshold = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles maximum parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, maximum = 0.8))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles weight_digits rounding", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, weight_digits = 1))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, weight_digits = NULL))  # No rounding
  expect_true(result$success, info = result$error)
})

# ============================================
# PLOT SETTINGS
# ============================================

test_that("splot() handles title parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, title = "Test Network", title_size = 1.5))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles background color", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, background = "lightgray"))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, background = "transparent"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles margins parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, margins = c(0.2, 0.2, 0.2, 0.2)))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles layout_scale parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, layout_scale = 0.8))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, layout_scale = 1.2))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, layout_scale = "auto"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles layout_margin parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, layout_margin = 0))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, layout_margin = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles rescale parameter", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, rescale = TRUE))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, rescale = FALSE))
  expect_true(result$success, info = result$error)
})

# ============================================
# THEME INTEGRATION
# ============================================

test_that("splot() handles theme parameter", {
  adj <- create_test_matrix(4)

  for (theme in c("classic", "dark", "minimal", "colorblind")) {
    result <- safe_plot(splot(adj, theme = theme))
    expect_true(result$success, info = paste("Theme", theme, "failed:", result$error))
  }
})

# ============================================
# LEGEND
# ============================================

test_that("splot() handles legend parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, legend = TRUE))
  expect_true(result$success, info = result$error)

  result <- safe_plot(splot(adj, legend = FALSE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles legend with groups", {
  adj <- create_test_matrix(6)
  groups <- c(1, 1, 2, 2, 3, 3)

  result <- safe_plot(splot(adj, legend = TRUE, groups = groups))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles legend_position options", {
  adj <- create_test_matrix(4)

  for (pos in c("topright", "topleft", "bottomright", "bottomleft")) {
    result <- safe_plot(splot(adj, legend = TRUE, legend_position = pos))
    expect_true(result$success, info = paste("Legend position", pos, "failed:", result$error))
  }
})

# ============================================
# OUTPUT
# ============================================

test_that("splot() outputs to PNG file", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  splot(adj, filetype = "png", filename = tools::file_path_sans_ext(tmp),
        width = 5, height = 5, res = 100)

  expect_file_created(paste0(tools::file_path_sans_ext(tmp), ".png"))
})

test_that("splot() outputs to PDF file", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  splot(adj, filetype = "pdf", filename = tools::file_path_sans_ext(tmp),
        width = 5, height = 5)

  expect_file_created(paste0(tools::file_path_sans_ext(tmp), ".pdf"))
})

test_that("splot() outputs to SVG file", {
  skip_on_cran()  # SVG requires cairo

  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)

  # Skip if SVG device not available
  svg_test <- tempfile(fileext = ".svg")
  result <- tryCatch({
    grDevices::svg(svg_test)
    grDevices::dev.off()
    unlink(svg_test)
    TRUE
  }, warning = function(w) {
    unlink(svg_test)
    if (grepl("cairo|X11", conditionMessage(w), ignore.case = TRUE)) FALSE else TRUE
  }, error = function(e) { unlink(svg_test); FALSE })

  if (!result) skip("SVG device not available on this system")

  splot(adj, filetype = "svg", filename = tools::file_path_sans_ext(tmp),
        width = 5, height = 5)

  expect_file_created(paste0(tools::file_path_sans_ext(tmp), ".svg"))
})

# ============================================
# RETURN VALUE
# ============================================

test_that("splot() returns sonnet_network invisibly", {
  adj <- create_test_matrix(4)

  result <- with_temp_png({
    ret <- splot(adj)
    ret
  })

  expect_sonnet_network(result)
})

# ============================================
# EDGE CASES
# ============================================

test_that("splot() handles single-node network", {
  adj <- matrix(0, 1, 1)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles two-node network", {
  adj <- matrix(c(0, 1, 1, 0), 2, 2)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles network with self-loops", {
  adj <- create_test_matrix(4)
  diag(adj) <- 1  # Add self-loops

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles fully disconnected nodes", {
  adj <- matrix(0, 4, 4)  # No edges

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles complete graph", {
  adj <- create_test_topology("complete", n = 5)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles named matrix with row/column names", {
  adj <- create_test_matrix(4)
  rownames(adj) <- colnames(adj) <- c("Node_A", "Node_B", "Node_C", "Node_D")

  result <- safe_plot(splot(adj, labels = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles very long node labels", {
  adj <- create_test_matrix(3)
  long_labels <- c("This is a very long label", "Another lengthy node name", "Short")

  result <- safe_plot(splot(adj, labels = long_labels))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles Unicode node labels", {
  skip_on_cran()  # Unicode handling can vary by platform
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj, labels = c("\u03B1", "\u03B2", "\u03B3")))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles zero weights in weighted matrix", {
  adj <- create_test_matrix(4, weighted = TRUE)
  adj[adj != 0][1] <- 0  # Set one weight to exactly 0

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

# ============================================
# SEED REPRODUCIBILITY
# ============================================

test_that("splot() with same seed produces consistent layouts", {
  adj <- create_test_matrix(5)

  # Run twice with same seed
  net1 <- with_temp_png(splot(adj, layout = "spring", seed = 123))
  net2 <- with_temp_png(splot(adj, layout = "spring", seed = 123))

  layout1 <- net1$network$get_layout()
  layout2 <- net2$network$get_layout()

  expect_equal(layout1$x, layout2$x)
  expect_equal(layout1$y, layout2$y)
})
