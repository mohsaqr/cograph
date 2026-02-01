# test-sn-edges.R - Edge Aesthetics Function Tests
# Dedicated tests for sn_edges()

# ============================================
# BASIC FUNCTIONALITY
# ============================================

test_that("sn_edges() returns sonnet_network object", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, width = 2)

  expect_sonnet_network(result)
})

test_that("sn_edges() preserves network structure", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, color = "gray")

  expect_equal(result$network$n_nodes, net$network$n_nodes)
  expect_equal(result$network$n_edges, net$network$n_edges)
})

test_that("sn_edges() can be chained in pipes", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_edges(width = 2) |>
    sn_edges(color = "blue")

  expect_sonnet_network(net)
})

test_that("sn_edges() accepts matrix input directly", {
  adj <- create_test_matrix(4)

  result <- sn_edges(adj, color = "gray")

  expect_sonnet_network(result)
})

test_that("sn_edges() accepts weighted matrix", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- sn_edges(adj, width = "weight")

  expect_sonnet_network(result)
})

# ============================================
# WIDTH PARAMETER
# ============================================

test_that("sn_edges() sets scalar width", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, width = 2.5)
  aes <- result$network$get_edge_aes()

  expect_true(all(aes$width == 2.5))
})

test_that("sn_edges() sets width from 'weight'", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- sonnet(adj)

  result <- sn_edges(net, width = "weight")
  aes <- result$network$get_edge_aes()

  # Width should be scaled from weights
  expect_true(!is.null(aes$width))
  expect_true(length(aes$width) > 0)
})

test_that("sn_edges() respects maximum parameter with width='weight'", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- sonnet(adj)

  result <- sn_edges(net, width = "weight", maximum = 0.5)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$maximum, 0.5)
})

# ============================================
# EDGE WIDTH SCALING PARAMETERS
# ============================================

test_that("sn_edges() sets edge_size parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, edge_size = 10)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$esize, 10)
})

test_that("sn_edges() deprecated esize parameter works with warning", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_warning(
    result <- sn_edges(net, esize = 10),
    "deprecated"
  )
  aes <- result$network$get_edge_aes()
  expect_equal(aes$esize, 10)
})

test_that("sn_edges() sets edge_width_range parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, edge_width_range = c(1, 5))
  aes <- result$network$get_edge_aes()

  expect_equal(aes$edge_width_range, c(1, 5))
})

test_that("sn_edges() sets edge_scale_mode parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  for (mode in c("linear", "log", "sqrt", "rank")) {
    result <- sn_edges(net, edge_scale_mode = mode)
    aes <- result$network$get_edge_aes()
    expect_equal(aes$edge_scale_mode, mode)
  }
})

test_that("sn_edges() validates edge_scale_mode", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, edge_scale_mode = "invalid_mode"))
})

test_that("sn_edges() sets edge_cutoff parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, edge_cutoff = 0.3)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$cut, 0.3)
})

test_that("sn_edges() deprecated cut parameter works with warning", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_warning(
    result <- sn_edges(net, cut = 0.3),
    "deprecated"
  )
  aes <- result$network$get_edge_aes()
  expect_equal(aes$cut, 0.3)
})

test_that("sn_edges() sets width_scale parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, width_scale = 1.5)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$width_scale, 1.5)
})

# ============================================
# COLOR PARAMETER
# ============================================

test_that("sn_edges() sets scalar color", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, color = "gray50")
  aes <- result$network$get_edge_aes()

  expect_true(all(aes$color == "gray50"))
})

test_that("sn_edges() sets color from 'weight'", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  net <- sonnet(adj)

  result <- sn_edges(net, color = "weight")
  aes <- result$network$get_edge_aes()

  # Color should be assigned based on weight sign
  expect_true(!is.null(aes$color))
})

test_that("sn_edges() sets edge_positive_color and edge_negative_color", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- sonnet(adj)

  result <- sn_edges(net, edge_positive_color = "darkgreen", edge_negative_color = "darkred")
  aes <- result$network$get_edge_aes()

  expect_equal(aes$positive_color, "darkgreen")
  expect_equal(aes$negative_color, "darkred")
})

test_that("sn_edges() deprecated positive_color and negative_color work with warning", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- sonnet(adj)

  expect_warning(
    result <- sn_edges(net, positive_color = "darkgreen", negative_color = "darkred"),
    "deprecated"
  )
  aes <- result$network$get_edge_aes()
  expect_equal(aes$positive_color, "darkgreen")
  expect_equal(aes$negative_color, "darkred")
})

test_that("sn_edges() uses edge_positive/edge_negative colors with color='weight'", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  net <- sonnet(adj)

  result <- sn_edges(net, color = "weight",
                     edge_positive_color = "blue", edge_negative_color = "red")
  aes <- result$network$get_edge_aes()

  expect_true(!is.null(aes$color))
})

# ============================================
# ALPHA PARAMETER
# ============================================

test_that("sn_edges() sets alpha", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, alpha = 0.5)
  aes <- result$network$get_edge_aes()

  expect_true(all(aes$alpha == 0.5))
})

test_that("sn_edges() validates alpha range", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, alpha = 1.5))
  expect_error(sn_edges(net, alpha = -0.1))
})

# ============================================
# STYLE PARAMETER
# ============================================

test_that("sn_edges() sets style", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  for (style in c("solid", "dashed", "dotted", "longdash", "twodash")) {
    result <- sn_edges(net, style = style)
    aes <- result$network$get_edge_aes()
    expect_true(all(aes$style == style))
  }
})

test_that("sn_edges() validates style", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, style = "wavy"))
})

# ============================================
# CURVATURE PARAMETER
# ============================================

test_that("sn_edges() sets curvature", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, curvature = 0.3)
  aes <- result$network$get_edge_aes()

  expect_true(all(aes$curvature == 0.3))
})

test_that("sn_edges() sets per-edge curvature", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  n_edges <- net$network$n_edges

  curvs <- seq(0, 0.5, length.out = n_edges)
  result <- sn_edges(net, curvature = curvs)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$curvature, curvs)
})

# ============================================
# CURVES PARAMETER
# ============================================

test_that("sn_edges() sets curves parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, curves = FALSE)
  aes <- result$network$get_edge_aes()
  expect_equal(aes$curves, FALSE)

  result <- sn_edges(net, curves = "mutual")
  aes <- result$network$get_edge_aes()
  expect_equal(aes$curves, "mutual")

  result <- sn_edges(net, curves = "force")
  aes <- result$network$get_edge_aes()
  expect_equal(aes$curves, "force")
})

test_that("sn_edges() validates curves parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, curves = "all"))
})

test_that("sn_edges() sets curve_shape", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, curve_shape = 0.5)
  aes <- result$network$get_edge_aes()

  expect_true(!is.null(aes$curve_shape))
})

test_that("sn_edges() sets curve_pivot", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, curve_pivot = 0.3)
  aes <- result$network$get_edge_aes()

  expect_true(!is.null(aes$curve_pivot))
})

# ============================================
# ARROW PARAMETERS
# ============================================

test_that("sn_edges() sets arrow_size", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, arrow_size = 1.5)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$arrow_size, 1.5)
})

test_that("sn_edges() sets show_arrows", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, show_arrows = FALSE)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$show_arrows, FALSE)
})

test_that("sn_edges() sets bidirectional", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, bidirectional = TRUE)
  aes <- result$network$get_edge_aes()

  expect_true(all(aes$bidirectional == TRUE))
})

# ============================================
# LOOP ROTATION
# ============================================

test_that("sn_edges() sets loop_rotation", {
  adj <- create_test_matrix(4)
  diag(adj) <- 1  # Add self-loops
  net <- sonnet(adj)

  result <- sn_edges(net, loop_rotation = pi/4)
  aes <- result$network$get_edge_aes()

  expect_true(!is.null(aes$loop_rotation))
})

# ============================================
# LABEL PARAMETERS
# ============================================

test_that("sn_edges() sets labels=TRUE to show weights", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- sonnet(adj)

  result <- sn_edges(net, labels = TRUE)
  aes <- result$network$get_edge_aes()

  expect_true(!is.null(aes$labels))
})

test_that("sn_edges() sets custom edge labels", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  n_edges <- net$network$n_edges

  custom_labels <- paste0("E", 1:n_edges)
  result <- sn_edges(net, labels = custom_labels)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$labels, custom_labels)
})

test_that("sn_edges() sets label_size", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_size = 0.8)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_size, 0.8)
})

test_that("sn_edges() sets label_color", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_color = "navy")
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_color, "navy")
})

test_that("sn_edges() sets label_position", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_position = 0.3)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_position, 0.3)
})

test_that("sn_edges() sets label_offset", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_offset = 0.1)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_offset, 0.1)
})

test_that("sn_edges() sets label_bg", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_bg = "lightyellow")
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_bg, "lightyellow")
})

test_that("sn_edges() sets label_fontface", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  for (face in c("plain", "bold", "italic", "bold.italic")) {
    result <- sn_edges(net, label_fontface = face)
    aes <- result$network$get_edge_aes()
    expect_equal(aes$label_fontface, face)
  }
})

test_that("sn_edges() validates label_fontface", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, label_fontface = "extra_bold"))
})

test_that("sn_edges() sets label_border", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  for (border in c("rect", "rounded", "circle")) {
    result <- sn_edges(net, label_border = border)
    aes <- result$network$get_edge_aes()
    expect_equal(aes$label_border, border)
  }
})

test_that("sn_edges() validates label_border", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, label_border = "hexagon"))
})

test_that("sn_edges() sets label shadow parameters", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net,
    label_shadow = TRUE,
    label_shadow_color = "gray40",
    label_shadow_offset = 0.8,
    label_shadow_alpha = 0.3
  )
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_shadow, TRUE)
  expect_equal(aes$label_shadow_color, "gray40")
  expect_equal(aes$label_shadow_offset, 0.8)
  expect_equal(aes$label_shadow_alpha, 0.3)
})

test_that("sn_edges() validates label_shadow_alpha range", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, label_shadow_alpha = 1.5))
})

# ============================================
# CI PARAMETERS
# ============================================

test_that("sn_edges() sets ci parameter", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  n_edges <- net$network$n_edges

  ci_vals <- runif(n_edges, 0.1, 0.3)
  result <- sn_edges(net, ci = ci_vals)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$ci, ci_vals)
})

test_that("sn_edges() sets ci_scale", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, ci_scale = 3)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$ci_scale, 3)
})

test_that("sn_edges() sets ci_alpha", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, ci_alpha = 0.2)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$ci_alpha, 0.2)
})

test_that("sn_edges() validates ci_alpha range", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, ci_alpha = 1.5))
})

test_that("sn_edges() sets ci_color", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, ci_color = "lightblue")
  aes <- result$network$get_edge_aes()

  expect_equal(aes$ci_color, "lightblue")
})

test_that("sn_edges() sets ci_style", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, ci_style = 2)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$ci_style, 2)
})

test_that("sn_edges() sets ci_arrows", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, ci_arrows = TRUE)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$ci_arrows, TRUE)
})

# ============================================
# LABEL TEMPLATE PARAMETERS
# ============================================

test_that("sn_edges() sets ci_lower and ci_upper", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- sonnet(adj)
  n_edges <- net$network$n_edges

  lower <- runif(n_edges, 0, 0.5)
  upper <- runif(n_edges, 0.5, 1)

  result <- sn_edges(net, ci_lower = lower, ci_upper = upper)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$ci_lower, lower)
  expect_equal(aes$ci_upper, upper)
})

test_that("sn_edges() sets label_style", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  for (style in c("none", "estimate", "full", "range", "stars")) {
    result <- sn_edges(net, label_style = style)
    aes <- result$network$get_edge_aes()
    expect_equal(aes$label_style, style)
  }
})

test_that("sn_edges() validates label_style", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, label_style = "fancy"))
})

test_that("sn_edges() sets label_template", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_template = "{est} [{low}, {up}]")
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_template, "{est} [{low}, {up}]")
})

test_that("sn_edges() sets label_digits", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_digits = 3)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_digits, 3)
})

test_that("sn_edges() sets label_ci_format", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_ci_format = "bracket")
  aes <- result$network$get_edge_aes()
  expect_equal(aes$label_ci_format, "bracket")

  result <- sn_edges(net, label_ci_format = "dash")
  aes <- result$network$get_edge_aes()
  expect_equal(aes$label_ci_format, "dash")
})

test_that("sn_edges() validates label_ci_format", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  expect_error(sn_edges(net, label_ci_format = "parenthesis"))
})

test_that("sn_edges() sets label_p parameters", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)
  n_edges <- net$network$n_edges

  p_vals <- runif(n_edges, 0, 0.1)
  result <- sn_edges(net,
    label_p = p_vals,
    label_p_digits = 4,
    label_p_prefix = "p = "
  )
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_p, p_vals)
  expect_equal(aes$label_p_digits, 4)
  expect_equal(aes$label_p_prefix, "p = ")
})

test_that("sn_edges() sets label_stars", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net, label_stars = TRUE)
  aes <- result$network$get_edge_aes()

  expect_equal(aes$label_stars, TRUE)
})

# ============================================
# MULTIPLE PARAMETERS AT ONCE
# ============================================

test_that("sn_edges() sets multiple parameters at once", {
  adj <- create_test_matrix(4)
  net <- sonnet(adj)

  result <- sn_edges(net,
    width = 2,
    color = "gray50",
    alpha = 0.7,
    style = "dashed",
    curvature = 0.2,
    arrow_size = 1.2
  )

  aes <- result$network$get_edge_aes()

  expect_true(all(aes$width == 2))
  expect_true(all(aes$color == "gray50"))
  expect_true(all(aes$alpha == 0.7))
  expect_true(all(aes$style == "dashed"))
  expect_true(all(aes$curvature == 0.2))
  expect_equal(aes$arrow_size, 1.2)
})

# ============================================
# INTEGRATION WITH SPLOT
# ============================================

test_that("sn_edges() customizations render in splot()", {
  adj <- create_test_matrix(4, weighted = TRUE)

  net <- sonnet(adj) |>
    sn_edges(
      width = "weight",
      color = "weight",
      alpha = 0.8,
      edge_positive_color = "darkgreen",
      edge_negative_color = "darkred"
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_edges() curvature customizations render in splot()", {
  adj <- create_test_matrix(4)

  net <- sonnet(adj) |>
    sn_edges(curvature = 0.3, style = "dashed")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_edges() CI underlay customizations render in splot()", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- sonnet(adj)
  n_edges <- net$network$n_edges

  net <- net |>
    sn_edges(
      ci = runif(n_edges, 0.1, 0.3),
      ci_scale = 2,
      ci_alpha = 0.2
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})
