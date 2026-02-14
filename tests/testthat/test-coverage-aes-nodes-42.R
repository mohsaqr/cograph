# test-coverage-aes-nodes-42.R - Comprehensive coverage tests for aes-nodes.R
# Tests targeting uncovered functions/branches: map_node_colors, scale_node_sizes,
# SVG registration, deprecated parameters, error handling, and edge cases

# ============================================
# MAP_NODE_COLORS() TESTS
# ============================================

test_that("map_node_colors returns correct length output", {
  groups <- c("A", "B", "C", "A", "B")
  result <- cograph:::map_node_colors(groups)
  expect_equal(length(result), length(groups))
})

test_that("map_node_colors uses default colorblind palette when palette is NULL", {
  groups <- c("A", "B", "C")
  result <- cograph:::map_node_colors(groups, palette = NULL)
  expect_equal(length(result), 3)
  expect_valid_colors(result)
})

test_that("map_node_colors uses function palette", {
  groups <- c("A", "B", "C", "D")
  custom_palette <- function(n) rep("purple", n)
  result <- cograph:::map_node_colors(groups, palette = custom_palette)
  expect_equal(length(result), 4)
  expect_true(all(result == "purple"))
})

test_that("map_node_colors uses character vector palette", {
  groups <- c("A", "B", "C", "A", "B")
  result <- cograph:::map_node_colors(groups, palette = c("red", "green", "blue"))
  expect_equal(length(result), 5)
  expect_valid_colors(result)
  # Same group should have same color
  expect_equal(result[1], result[4])
  expect_equal(result[2], result[5])
})

test_that("map_node_colors recycles palette when fewer colors than groups", {
  groups <- c("A", "B", "C", "D", "E")  # 5 groups
  result <- cograph:::map_node_colors(groups, palette = c("red", "blue"))  # 2 colors
  expect_equal(length(result), 5)
  expect_valid_colors(result)
})

test_that("map_node_colors handles numeric groups", {
  groups <- c(1, 2, 3, 1, 2)
  result <- cograph:::map_node_colors(groups)
  expect_equal(length(result), 5)
  expect_valid_colors(result)
})

test_that("map_node_colors handles factor groups", {
  groups <- factor(c("low", "medium", "high", "low"))
  result <- cograph:::map_node_colors(groups)
  expect_equal(length(result), 4)
  expect_valid_colors(result)
})

test_that("map_node_colors handles single group", {
  groups <- c("A", "A", "A")
  result <- cograph:::map_node_colors(groups)
  expect_equal(length(result), 3)
  expect_true(all(result == result[1]))
})

test_that("map_node_colors handles many groups", {
  groups <- LETTERS[1:20]
  result <- cograph:::map_node_colors(groups)
  expect_equal(length(result), 20)
  expect_valid_colors(result)
})

# ============================================
# SCALE_NODE_SIZES() TESTS
# ============================================

test_that("scale_node_sizes returns correct length output", {
  values <- c(1, 2, 3, 4, 5)
  result <- cograph:::scale_node_sizes(values)
  expect_equal(length(result), length(values))
})

test_that("scale_node_sizes uses default range [0.03, 0.1]", {
  values <- c(0, 50, 100)
  result <- cograph:::scale_node_sizes(values)
  expect_true(min(result) >= 0.03)
  expect_true(max(result) <= 0.1)
})

test_that("scale_node_sizes handles all NA values", {
  values <- c(NA, NA, NA)
  result <- cograph:::scale_node_sizes(values)
  expect_equal(length(result), 3)
  # Should return mean of default range
  expect_true(all(result == mean(c(0.03, 0.1))))
})

test_that("scale_node_sizes handles constant values (zero range)", {
  values <- c(5, 5, 5, 5)
  result <- cograph:::scale_node_sizes(values)
  expect_equal(length(result), 4)
  # When diff is 0, should return mean of range
  expect_true(all(result == mean(c(0.03, 0.1))))
})

test_that("scale_node_sizes respects custom range", {
  values <- c(0, 50, 100)
  result <- cograph:::scale_node_sizes(values, range = c(0.05, 0.2))
  expect_equal(min(result), 0.05)
  expect_equal(max(result), 0.2)
})

test_that("scale_node_sizes preserves linear scaling", {
  values <- c(0, 25, 50, 75, 100)
  result <- cograph:::scale_node_sizes(values)
  # Middle value should be at middle of range
  expect_equal(result[3], mean(c(result[1], result[5])))
})

test_that("scale_node_sizes handles negative values", {
  values <- c(-10, -5, 0, 5, 10)
  result <- cograph:::scale_node_sizes(values)
  expect_equal(length(result), 5)
  expect_true(all(is.finite(result)))
})

test_that("scale_node_sizes handles mixed NA values", {
  values <- c(1, NA, 3, NA, 5)
  result <- cograph:::scale_node_sizes(values)
  expect_equal(length(result), 5)
  # Non-NA values should be scaled properly
  expect_true(is.finite(result[1]))
  expect_true(is.finite(result[3]))
  expect_true(is.finite(result[5]))
})

test_that("scale_node_sizes handles single value", {
  values <- 42
  result <- cograph:::scale_node_sizes(values)
  expect_equal(length(result), 1)
  # Single value should return mean of range
  expect_equal(result, mean(c(0.03, 0.1)))
})

# ============================================
# SN_NODES() SVG PARAMETER TESTS
# ============================================

test_that("sn_nodes handles node_svg parameter with inline SVG", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # Simple inline SVG
  svg_content <- '<svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="40"/></svg>'

  result <- sn_nodes(net, node_svg = svg_content)
  expect_cograph_network(result)
  expect_true(!is.null(result$node_aes$shape))
})

test_that("sn_nodes sets svg_preserve_aspect parameter", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  result <- sn_nodes(net, svg_preserve_aspect = FALSE)
  aes <- result$node_aes

  expect_equal(aes$svg_preserve_aspect, FALSE)
})

test_that("sn_nodes sets svg_preserve_aspect to TRUE", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  result <- sn_nodes(net, svg_preserve_aspect = TRUE)
  aes <- result$node_aes

  expect_equal(aes$svg_preserve_aspect, TRUE)
})

# ============================================
# DEPRECATED PARAMETER TESTS
# ============================================

test_that("sn_nodes handles deprecated donut_values parameter", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # donut_values is deprecated, but should still work
  result <- sn_nodes(net, donut_values = c(0.3, 0.5, 0.7))
  aes <- result$node_aes

  expect_equal(aes$donut_values, c(0.3, 0.5, 0.7))
})

test_that("sn_nodes prefers donut_fill over donut_values when both provided", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # donut_fill takes precedence
  result <- sn_nodes(net, donut_fill = c(0.1, 0.2, 0.3), donut_values = c(0.9, 0.8, 0.7))
  aes <- result$node_aes

  # donut_fill should set donut_values internally
  expect_equal(aes$donut_fill, c(0.1, 0.2, 0.3))
  expect_equal(aes$donut_values, c(0.1, 0.2, 0.3))
})

test_that("sn_nodes handles deprecated donut_colors parameter", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # donut_colors is deprecated, should use donut_color
  result <- sn_nodes(net, donut_colors = c("red", "green", "blue"))
  aes <- result$node_aes

  expect_equal(aes$donut_color, c("red", "green", "blue"))
})

test_that("sn_nodes prefers donut_color over donut_colors when both provided", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  result <- sn_nodes(net, donut_color = "steelblue", donut_colors = c("red", "green"))
  aes <- result$node_aes

  expect_equal(aes$donut_color, "steelblue")
})

# ============================================
# DONUT_SHAPE VALIDATION TESTS
# ============================================

test_that("sn_nodes validates donut_shape with invalid values", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, donut_shape = "invalid"),
    "donut_shape must be one of"
  )
})

test_that("sn_nodes validates vectorized donut_shape with some invalid values", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, donut_shape = c("circle", "invalid", "square")),
    "donut_shape must be one of"
  )
})

test_that("sn_nodes accepts vectorized valid donut_shapes", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  shapes <- c("circle", "square", "hexagon")
  result <- sn_nodes(net, donut_shape = shapes)
  aes <- result$node_aes

  expect_equal(aes$donut_shape, shapes)
})

test_that("sn_nodes accepts all valid donut_shape values", {
  adj <- create_test_matrix(6)
  net <- cograph(adj)

  valid_shapes <- c("circle", "square", "hexagon", "triangle", "diamond", "pentagon")
  for (shape in valid_shapes) {
    result <- sn_nodes(net, donut_shape = shape)
    expect_equal(result$node_aes$donut_shape, shape)
  }
})

# ============================================
# LABEL_POSITION VALIDATION TESTS
# ============================================

test_that("sn_nodes validates label_position with invalid value", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, label_position = "diagonal"),
    "label_position must be one of"
  )
})

test_that("sn_nodes validates vectorized label_position with invalid values", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, label_position = c("above", "invalid", "below")),
    "label_position must be one of"
  )
})

# ============================================
# FONTFACE VALIDATION TESTS
# ============================================

test_that("sn_nodes validates donut_value_fontface with invalid value", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, donut_value_fontface = "extra_bold"),
    "donut_value_fontface must be one of"
  )
})

test_that("sn_nodes validates label_fontface with invalid value", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, label_fontface = "super_italic"),
    "label_fontface must be one of"
  )
})

# ============================================
# DONUT_VALUE_FORMAT VALIDATION TESTS
# ============================================

test_that("sn_nodes validates donut_value_format must be a function", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, donut_value_format = "not_a_function"),
    "donut_value_format must be a function"
  )
})

test_that("sn_nodes validates donut_value_format rejects numeric", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, donut_value_format = 123),
    "donut_value_format must be a function"
  )
})

# ============================================
# ALPHA VALIDATION TESTS
# ============================================

test_that("sn_nodes validates alpha must be numeric", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(
    sn_nodes(net, alpha = "opaque"),
    "alpha must be numeric"
  )
})

test_that("sn_nodes validates alpha range with vector", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  expect_error(sn_nodes(net, alpha = c(0.5, 1.5, 0.3)))
  expect_error(sn_nodes(net, alpha = c(-0.1, 0.5, 0.8)))
})

# ============================================
# NODE_AES MERGING TESTS
# ============================================

test_that("sn_nodes creates node_aes when it does not exist", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # Ensure node_aes starts as NULL
  net$node_aes <- NULL

  result <- sn_nodes(net, fill = "red")

  expect_true(!is.null(result$node_aes))
  expect_true(all(result$node_aes$fill == "red"))
})

test_that("sn_nodes merges with existing node_aes", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # First call sets size
  net <- sn_nodes(net, size = 0.08)

  # Second call sets fill - should merge
  net <- sn_nodes(net, fill = "blue")

  expect_true(all(net$node_aes$size == 0.08))
  expect_true(all(net$node_aes$fill == "blue"))
})

test_that("sn_nodes overwrites existing aesthetics when called again", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  net <- sn_nodes(net, fill = "red")
  net <- sn_nodes(net, fill = "green")

  expect_true(all(net$node_aes$fill == "green"))
})

# ============================================
# DONUT VALUE FORMATTING TESTS
# ============================================

test_that("sn_nodes sets donut_value_fontfamily", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  result <- sn_nodes(net, donut_value_fontfamily = "serif")
  aes <- result$node_aes

  expect_equal(aes$donut_value_fontfamily, "serif")
})

test_that("sn_nodes sets donut_border_width", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  result <- sn_nodes(net, donut_border_width = 1.5)
  aes <- result$node_aes

  expect_equal(aes$donut_border_width, 1.5)
})

# ============================================
# COLUMN NAME RESOLUTION TESTS
# ============================================

test_that("sn_nodes resolves fill from column name in nodes data", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # Add a custom column to nodes
  net$nodes$my_color <- c("red", "green", "blue")

  result <- sn_nodes(net, fill = "my_color")
  aes <- result$node_aes

  expect_equal(aes$fill, c("red", "green", "blue"))
})

test_that("sn_nodes resolves size from column name in nodes data", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  net$nodes$importance <- c(0.05, 0.1, 0.15)

  result <- sn_nodes(net, size = "importance")
  aes <- result$node_aes

  expect_equal(aes$size, c(0.05, 0.1, 0.15))
})

test_that("sn_nodes uses literal value when column name does not exist", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # "coral" is not a column name, should be used as literal color
  result <- sn_nodes(net, fill = "coral")
  aes <- result$node_aes

  expect_true(all(aes$fill == "coral"))
})

# ============================================
# PIE AND DONUT COMBINATION TESTS
# ============================================

test_that("sn_nodes handles pie_values and pie_colors together", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  pie_vals <- list(c(30, 40, 30), c(50, 50), c(25, 25, 25, 25))
  pie_cols <- c("red", "green", "blue", "orange")

  result <- sn_nodes(net, pie_values = pie_vals, pie_colors = pie_cols)
  aes <- result$node_aes

  expect_equal(aes$pie_values, pie_vals)
  expect_equal(aes$pie_colors, pie_cols)
})

test_that("sn_nodes handles double donut parameters", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  donut2_vals <- list(c(0.4), c(0.6), c(0.8))
  donut2_cols <- list("orange", "purple", "cyan")

  result <- sn_nodes(net,
                     donut2_values = donut2_vals,
                     donut2_colors = donut2_cols,
                     donut2_inner_ratio = 0.35)
  aes <- result$node_aes

  expect_equal(aes$donut2_values, donut2_vals)
  expect_equal(aes$donut2_colors, donut2_cols)
  expect_equal(aes$donut2_inner_ratio, 0.35)
})

# ============================================
# EDGE CASES AND BOUNDARY CONDITIONS
# ============================================

test_that("sn_nodes handles single node network", {
  adj <- matrix(0, 1, 1)
  net <- cograph(adj)

  result <- sn_nodes(net, size = 0.1, fill = "red")
  aes <- result$node_aes

  expect_equal(length(aes$size), 1)
  expect_equal(length(aes$fill), 1)
})

test_that("sn_nodes handles large network", {
  n <- 100
  adj <- create_test_matrix(n, density = 0.1)
  net <- cograph(adj)

  result <- sn_nodes(net, size = 0.05)
  aes <- result$node_aes

  expect_equal(length(aes$size), n)
})

test_that("sn_nodes recycling works correctly for vectors", {
  adj <- create_test_matrix(6)
  net <- cograph(adj)

  # 3 colors for 6 nodes - should recycle
  result <- sn_nodes(net, fill = c("red", "green", "blue"))
  aes <- result$node_aes

  expect_equal(length(aes$fill), 6)
  expect_equal(aes$fill, rep(c("red", "green", "blue"), 2))
})

# ============================================
# COMPLEX INTEGRATION TESTS
# ============================================

test_that("sn_nodes with all donut parameters renders without error", {
  adj <- create_test_matrix(3)

  net <- cograph(adj) |>
    sn_nodes(
      donut_fill = c(0.25, 0.5, 0.75),
      donut_color = "steelblue",
      donut_bg_color = "gray90",
      donut_inner_ratio = 0.6,
      donut_shape = "circle",
      donut_show_value = TRUE,
      donut_value_size = 1.0,
      donut_value_color = "black",
      donut_value_fontface = "bold",
      donut_value_fontfamily = "sans",
      donut_value_digits = 1,
      donut_value_prefix = "",
      donut_value_suffix = "%",
      donut_border_width = 0.5
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_nodes with all label parameters renders without error", {
  adj <- create_test_matrix(4)

  net <- cograph(adj) |>
    sn_nodes(
      label_size = 1.2,
      label_color = "navy",
      label_position = "above",
      show_labels = TRUE,
      label_fontface = "bold",
      label_fontfamily = "serif",
      label_hjust = 0.5,
      label_vjust = 0.5,
      label_angle = 0
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_nodes multiple calls chain correctly", {
  adj <- create_test_matrix(4)

  net <- cograph(adj) |>
    sn_nodes(size = 0.08) |>
    sn_nodes(fill = "coral") |>
    sn_nodes(border_color = "black") |>
    sn_nodes(alpha = 0.9)

  aes <- net$node_aes

  expect_true(all(aes$size == 0.08))
  expect_true(all(aes$fill == "coral"))
  expect_true(all(aes$border_color == "black"))
  expect_true(all(aes$alpha == 0.9))
})

# ============================================
# AUTO-CONVERSION INPUT TESTS
# ============================================

test_that("sn_nodes auto-converts igraph object", {
  skip_if_no_igraph()

  g <- igraph::make_ring(5)
  result <- sn_nodes(g, fill = "blue")

  expect_cograph_network(result)
  expect_equal(n_nodes(result), 5)
})

test_that("sn_nodes auto-converts adjacency matrix", {
  adj <- create_test_matrix(4)

  result <- sn_nodes(adj, size = 0.1, fill = "red")

  expect_cograph_network(result)
  expect_equal(n_nodes(result), 4)
  expect_true(all(result$node_aes$size == 0.1))
})

test_that("sn_nodes auto-converts data.frame edge list", {
  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4)

  result <- sn_nodes(edges, shape = "square")

  expect_cograph_network(result)
  expect_true(all(result$node_aes$shape == "square"))
})

# ============================================
# COMPLEX AESTHETIC COMBINATIONS
# ============================================

test_that("sn_nodes handles all basic parameters together", {
  adj <- create_test_matrix(5)
  net <- cograph(adj)

  result <- sn_nodes(net,
    size = c(0.05, 0.06, 0.07, 0.08, 0.09),
    shape = c("circle", "square", "triangle", "diamond", "pentagon"),
    fill = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
    border_color = "black",
    border_width = 1.5,
    alpha = 0.85
  )

  aes <- result$node_aes

  expect_equal(length(aes$size), 5)
  expect_equal(length(aes$shape), 5)
  expect_equal(length(aes$fill), 5)
  expect_true(all(aes$border_color == "black"))
  expect_true(all(aes$border_width == 1.5))
  expect_true(all(aes$alpha == 0.85))
})

test_that("sn_nodes handles node_names parameter", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  names <- c("Alpha", "Beta", "Gamma")
  result <- sn_nodes(net, node_names = names)
  aes <- result$node_aes

  expect_equal(aes$node_names, names)
})

test_that("sn_nodes handles node_names from column", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  # Add legend_name column
  net$nodes$legend_name <- c("First", "Second", "Third")

  result <- sn_nodes(net, node_names = "legend_name")
  aes <- result$node_aes

  expect_equal(aes$node_names, c("First", "Second", "Third"))
})

# ============================================
# DONUT_VALUE_FORMAT FUNCTION TESTS
# ============================================

test_that("sn_nodes stores custom donut_value_format function", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  fmt_fn <- function(x) sprintf("%.1f%%", x * 100)
  result <- sn_nodes(net, donut_value_format = fmt_fn)
  aes <- result$node_aes

  expect_true(is.function(aes$donut_value_format))
  expect_equal(aes$donut_value_format(0.5), "50.0%")
})

test_that("sn_nodes allows anonymous function for donut_value_format", {
  adj <- create_test_matrix(3)
  net <- cograph(adj)

  result <- sn_nodes(net, donut_value_format = function(x) paste0("Val:", round(x, 2)))
  aes <- result$node_aes

  expect_true(is.function(aes$donut_value_format))
  expect_equal(aes$donut_value_format(0.123), "Val:0.12")
})
