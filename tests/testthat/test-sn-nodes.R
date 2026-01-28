# test-sn-nodes.R - Node Aesthetics Function Tests
# Dedicated tests for sn_nodes()

# ============================================
# BASIC FUNCTIONALITY
# ============================================

test_that("sn_nodes() returns sonnet_network object", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, size = 5)

  expect_sonnet_network(result)
})

test_that("sn_nodes() preserves network structure", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, fill = "red")

  expect_equal(result$network$n_nodes, net$network$n_nodes)
  expect_equal(result$network$n_edges, net$network$n_edges)
})

test_that("sn_nodes() can be chained in pipes", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_nodes(size = 5) |>
    sn_nodes(fill = "blue")

  expect_sonnet_network(net)
})

test_that("sn_nodes() accepts matrix input directly", {
  adj <- create_test_matrix(4)

  result <- sn_nodes(adj, fill = "red")

  expect_sonnet_network(result)
})

test_that("sn_nodes() accepts edge list input directly", {
  edges <- create_test_edgelist(n_edges = 6, n_nodes = 4)

  result <- sn_nodes(edges, fill = "blue")

  expect_sonnet_network(result)
})

# ============================================
# SIZE PARAMETER
# ============================================

test_that("sn_nodes() sets scalar size", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, size = 0.1)
  aes <- result$network$get_node_aes()

  expect_true(all(aes$size == 0.1))
})

test_that("sn_nodes() sets vector size", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  sizes <- c(0.05, 0.08, 0.1, 0.12)
  result <- sn_nodes(net, size = sizes)
  aes <- result$network$get_node_aes()

  expect_equal(aes$size, sizes)
})

test_that("sn_nodes() recycles size if shorter than node count", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, size = c(0.05, 0.1))
  aes <- result$network$get_node_aes()

  expect_equal(length(aes$size), 4)
  expect_equal(aes$size, c(0.05, 0.1, 0.05, 0.1))
})

# ============================================
# SHAPE PARAMETER
# ============================================

test_that("sn_nodes() sets scalar shape", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, shape = "square")
  aes <- result$network$get_node_aes()

  expect_true(all(aes$shape == "square"))
})

test_that("sn_nodes() sets per-node shapes", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  shapes <- c("circle", "square", "triangle", "diamond")
  result <- sn_nodes(net, shape = shapes)
  aes <- result$network$get_node_aes()

  expect_equal(aes$shape, shapes)
})

test_that("sn_nodes() accepts all built-in shapes", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  shapes <- c("circle", "square", "triangle", "diamond", "pentagon",
              "hexagon", "ellipse", "heart", "star", "cross", "rectangle")

  for (shape in shapes) {
    result <- sn_nodes(net, shape = shape)
    aes <- result$network$get_node_aes()
    expect_true(all(aes$shape == shape))
  }
})

# ============================================
# FILL PARAMETER
# ============================================

test_that("sn_nodes() sets scalar fill color", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, fill = "steelblue")
  aes <- result$network$get_node_aes()

  expect_true(all(aes$fill == "steelblue"))
})

test_that("sn_nodes() sets per-node fill colors", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  colors <- c("red", "green", "blue", "orange")
  result <- sn_nodes(net, fill = colors)
  aes <- result$network$get_node_aes()

  expect_equal(aes$fill, colors)
})

test_that("sn_nodes() accepts hex colors", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, fill = "#FF5733")
  aes <- result$network$get_node_aes()

  expect_true(all(aes$fill == "#FF5733"))
})

# ============================================
# BORDER PARAMETERS
# ============================================

test_that("sn_nodes() sets border_color", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, border_color = "darkblue")
  aes <- result$network$get_node_aes()

  expect_true(all(aes$border_color == "darkblue"))
})

test_that("sn_nodes() sets border_width", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, border_width = 2.5)
  aes <- result$network$get_node_aes()

  expect_true(all(aes$border_width == 2.5))
})

test_that("sn_nodes() sets per-node border attributes", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  border_colors <- c("red", "green", "blue", "black")
  border_widths <- c(1, 2, 3, 4)

  result <- sn_nodes(net, border_color = border_colors, border_width = border_widths)
  aes <- result$network$get_node_aes()

  expect_equal(aes$border_color, border_colors)
  expect_equal(aes$border_width, border_widths)
})

# ============================================
# ALPHA PARAMETER
# ============================================

test_that("sn_nodes() sets alpha", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, alpha = 0.7)
  aes <- result$network$get_node_aes()

  expect_true(all(aes$alpha == 0.7))
})

test_that("sn_nodes() validates alpha range", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, alpha = 1.5))
  expect_error(sn_nodes(net, alpha = -0.1))
})

# ============================================
# LABEL PARAMETERS
# ============================================

test_that("sn_nodes() sets label_size", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, label_size = 1.2)
  aes <- result$network$get_node_aes()

  expect_true(!is.null(aes$label_size))
})

test_that("sn_nodes() sets label_color", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, label_color = "navy")
  aes <- result$network$get_node_aes()

  expect_true(all(aes$label_color == "navy"))
})

test_that("sn_nodes() sets label_position", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  for (pos in c("center", "above", "below", "left", "right")) {
    result <- sn_nodes(net, label_position = pos)
    aes <- result$network$get_node_aes()
    expect_true(all(aes$label_position == pos))
  }
})

test_that("sn_nodes() validates label_position", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, label_position = "invalid"))
})

test_that("sn_nodes() sets show_labels", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, show_labels = FALSE)
  aes <- result$network$get_node_aes()

  expect_equal(aes$show_labels, FALSE)
})

test_that("sn_nodes() sets label_fontface", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  for (face in c("plain", "bold", "italic", "bold.italic")) {
    result <- sn_nodes(net, label_fontface = face)
    aes <- result$network$get_node_aes()
    expect_equal(aes$label_fontface, face)
  }
})

test_that("sn_nodes() validates label_fontface", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, label_fontface = "extra_bold"))
})

test_that("sn_nodes() sets label_fontfamily", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, label_fontfamily = "serif")
  aes <- result$network$get_node_aes()

  expect_equal(aes$label_fontfamily, "serif")
})

test_that("sn_nodes() sets label justification parameters", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, label_hjust = 0, label_vjust = 1)
  aes <- result$network$get_node_aes()

  expect_equal(aes$label_hjust, 0)
  expect_equal(aes$label_vjust, 1)
})

test_that("sn_nodes() sets label_angle", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net, label_angle = 45)
  aes <- result$network$get_node_aes()

  expect_equal(aes$label_angle, 45)
})

# ============================================
# PIE PARAMETERS
# ============================================

test_that("sn_nodes() sets pie_values", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  pie_vals <- list(c(1, 2), c(3, 1), c(2, 2, 1))
  result <- sn_nodes(net, pie_values = pie_vals)
  aes <- result$network$get_node_aes()

  expect_equal(aes$pie_values, pie_vals)
})

test_that("sn_nodes() sets pie_colors", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  pie_cols <- list(c("red", "blue"), c("green", "yellow"), c("purple", "orange", "pink"))
  result <- sn_nodes(net, pie_colors = pie_cols)
  aes <- result$network$get_node_aes()

  expect_equal(aes$pie_colors, pie_cols)
})

test_that("sn_nodes() sets pie_border_width", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  result <- sn_nodes(net, pie_values = list(c(1, 2), c(2, 1), c(1, 1)),
                     pie_border_width = 0.5)
  aes <- result$network$get_node_aes()

  expect_equal(aes$pie_border_width, 0.5)
})

# ============================================
# DONUT PARAMETERS
# ============================================

test_that("sn_nodes() sets donut_fill", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  result <- sn_nodes(net, donut_fill = 0.7)
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut_fill, 0.7)
})

test_that("sn_nodes() sets per-node donut_fill", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  fills <- c(0.2, 0.5, 0.8)
  result <- sn_nodes(net, donut_fill = fills)
  aes <- result$network$get_node_aes()

  # donut_fill should be set
  expect_equal(aes$donut_fill, fills)
})

test_that("sn_nodes() sets donut_color", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  result <- sn_nodes(net, donut_color = "steelblue")
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut_color, "steelblue")
})

test_that("sn_nodes() sets donut_bg_color", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  result <- sn_nodes(net, donut_bg_color = "lightyellow")
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut_bg_color, "lightyellow")
})

test_that("sn_nodes() sets donut_inner_ratio", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  result <- sn_nodes(net, donut_inner_ratio = 0.6)
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut_inner_ratio, 0.6)
})

test_that("sn_nodes() sets donut_shape", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  for (shape in c("circle", "square", "hexagon", "triangle")) {
    result <- sn_nodes(net, donut_shape = shape)
    aes <- result$network$get_node_aes()
    expect_equal(aes$donut_shape, shape)
  }
})

test_that("sn_nodes() validates donut_shape", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, donut_shape = "invalid_shape"))
})

test_that("sn_nodes() sets donut_show_value", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  result <- sn_nodes(net, donut_show_value = TRUE)
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut_show_value, TRUE)
})

test_that("sn_nodes() sets donut value formatting parameters", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  result <- sn_nodes(net,
    donut_show_value = TRUE,
    donut_value_size = 1.2,
    donut_value_color = "navy",
    donut_value_digits = 1,
    donut_value_prefix = "$",
    donut_value_suffix = "%"
  )
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut_value_size, 1.2)
  expect_equal(aes$donut_value_color, "navy")
  expect_equal(aes$donut_value_digits, 1)
  expect_equal(aes$donut_value_prefix, "$")
  expect_equal(aes$donut_value_suffix, "%")
})

test_that("sn_nodes() sets donut_value_fontface", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  for (face in c("plain", "bold", "italic", "bold.italic")) {
    result <- sn_nodes(net, donut_value_fontface = face)
    aes <- result$network$get_node_aes()
    expect_equal(aes$donut_value_fontface, face)
  }
})

test_that("sn_nodes() validates donut_value_fontface", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, donut_value_fontface = "invalid"))
})

test_that("sn_nodes() sets donut_value_format function", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  fmt_fn <- function(x) paste0(round(x * 100), "%")
  result <- sn_nodes(net, donut_value_format = fmt_fn)
  aes <- result$network$get_node_aes()

  expect_true(is.function(aes$donut_value_format))
})

test_that("sn_nodes() validates donut_value_format is function", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  expect_error(sn_nodes(net, donut_value_format = "not_a_function"))
})

# ============================================
# DOUBLE DONUT (DONUT2) PARAMETERS
# ============================================

test_that("sn_nodes() sets donut2_values", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  donut2_vals <- list(c(0.3), c(0.5), c(0.7))
  result <- sn_nodes(net, donut2_values = donut2_vals)
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut2_values, donut2_vals)
})

test_that("sn_nodes() sets donut2_colors", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  donut2_cols <- list("orange", "purple", "green")
  result <- sn_nodes(net, donut2_colors = donut2_cols)
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut2_colors, donut2_cols)
})

test_that("sn_nodes() sets donut2_inner_ratio", {
  adj <- create_test_matrix(3)
  net <- sonnet(adj)

  result <- sn_nodes(net, donut2_inner_ratio = 0.3)
  aes <- result$network$get_node_aes()

  expect_equal(aes$donut2_inner_ratio, 0.3)
})

# ============================================
# NODE NAMES
# ============================================

test_that("sn_nodes() sets node_names", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  names <- c("Node A", "Node B", "Node C", "Node D")
  result <- sn_nodes(net, node_names = names)
  aes <- result$network$get_node_aes()

  expect_equal(aes$node_names, names)
})

# ============================================
# MULTIPLE PARAMETERS AT ONCE
# ============================================

test_that("sn_nodes() sets multiple parameters at once", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_nodes(net,
    size = 0.1,
    shape = "square",
    fill = "coral",
    border_color = "black",
    border_width = 2,
    alpha = 0.9,
    label_size = 1.1,
    label_color = "navy"
  )

  aes <- result$network$get_node_aes()

  expect_true(all(aes$size == 0.1))
  expect_true(all(aes$shape == "square"))
  expect_true(all(aes$fill == "coral"))
  expect_true(all(aes$border_color == "black"))
  expect_true(all(aes$border_width == 2))
  expect_true(all(aes$alpha == 0.9))
})

# ============================================
# INTEGRATION WITH SPLOT
# ============================================

test_that("sn_nodes() customizations render in splot()", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_nodes(
      size = 5,
      shape = "diamond",
      fill = "steelblue",
      border_color = "darkblue",
      alpha = 0.8
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_nodes() pie customizations render in splot()", {
  adj <- create_test_matrix(3)

  net <- sonnet(adj) |>
    sn_nodes(
      pie_values = list(c(1, 2), c(2, 1), c(1, 1, 1)),
      pie_colors = list(c("red", "blue"), c("green", "yellow"), c("purple", "orange", "pink"))
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_nodes() donut customizations render in splot()", {
  adj <- create_test_matrix(3)

  net <- sonnet(adj) |>
    sn_nodes(
      donut_fill = c(0.3, 0.6, 0.9),
      donut_color = "steelblue",
      donut_bg_color = "gray90",
      donut_show_value = TRUE
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})
