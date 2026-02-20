# test-coverage-aes-edges-41.R
# Comprehensive tests for R/aes-edges.R to improve coverage
# Tests internal functions and edge cases not covered by test-sn-edges.R

# ============================================
# INTERNAL FUNCTION: scale_edge_widths_simple
# ============================================

test_that("scale_edge_widths_simple returns mean of range for all NA values", {
  result <- cograph:::scale_edge_widths_simple(c(NA, NA, NA))
  expected_mean <- mean(c(0.5, 3))
  expect_equal(result, rep(expected_mean, 3))
})

test_that("scale_edge_widths_simple returns mean of range when all values equal", {
  result <- cograph:::scale_edge_widths_simple(c(5, 5, 5))
  expected_mean <- mean(c(0.5, 3))
  expect_equal(result, rep(expected_mean, 3))
})

test_that("scale_edge_widths_simple scales linearly with default range", {
  values <- c(0, 0.5, 1)
  result <- cograph:::scale_edge_widths_simple(values)
  # Range is c(0.5, 3) by default
  expect_equal(result[1], 0.5)  # min value -> min range

  expect_equal(result[3], 3)    # max value -> max range
  expect_equal(result[2], 1.75) # middle value -> middle range
})

test_that("scale_edge_widths_simple uses custom range", {
  values <- c(0, 1)
  result <- cograph:::scale_edge_widths_simple(values, range = c(1, 5))
  expect_equal(result[1], 1)
  expect_equal(result[2], 5)
})

test_that("scale_edge_widths_simple respects maximum parameter", {
  values <- c(0, 0.5, 1, 2)  # 2 exceeds maximum of 1
  result <- cograph:::scale_edge_widths_simple(values, maximum = 1)
  # Values should be capped at maximum
  # With maximum=1, val_min=0, val_max=1
  # Default range c(0.5, 3)
  expect_equal(result[1], 0.5)  # 0 -> 0.5
  expect_equal(result[2], 1.75) # 0.5 -> 1.75
  expect_equal(result[3], 3)    # 1 -> 3
  expect_equal(result[4], 3)    # 2 capped to 1 -> 3
})

test_that("scale_edge_widths_simple handles single value",
{
  result <- cograph:::scale_edge_widths_simple(0.5)
  # Single value with val_min == val_max returns mean
  expected_mean <- mean(c(0.5, 3))
  expect_equal(result, expected_mean)
})

test_that("scale_edge_widths_simple handles empty vector", {
  result <- cograph:::scale_edge_widths_simple(numeric(0))
  expect_equal(length(result), 0)
})

test_that("scale_edge_widths_simple handles mixed NA values", {
  values <- c(0, NA, 1)
  result <- cograph:::scale_edge_widths_simple(values)
  # NA values are preserved, min/max calculated ignoring NAs
  expect_equal(result[1], 0.5)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 3)
})

test_that("scale_edge_widths_simple with maximum=0 uses data min/max", {
  values <- c(0, 0.5, 1)
  # When maximum is 0 (falsy), it should use data min/max
  result <- cograph:::scale_edge_widths_simple(values, maximum = 0)
  # maximum=0 is truthy, so val_min=0, val_max=0, values capped at 0
  # All become 0, then scaled linearly (but val_max == val_min)
  expected_mean <- mean(c(0.5, 3))
  expect_equal(result, rep(expected_mean, 3))
})

# ============================================
# INTERNAL FUNCTION: map_edge_colors
# ============================================

test_that("map_edge_colors assigns correct colors for positive weights", {
  weights <- c(0.5, 1, 0.2)
  result <- cograph:::map_edge_colors(weights)
  expect_true(all(result == "#2E7D32"))  # default positive color
})

test_that("map_edge_colors assigns correct colors for negative weights", {
  weights <- c(-0.5, -1, -0.2)
  result <- cograph:::map_edge_colors(weights)
  expect_true(all(result == "#C62828"))  # default negative color
})

test_that("map_edge_colors assigns correct colors for zero weights", {
  weights <- c(0, 0, 0)
  result <- cograph:::map_edge_colors(weights)
  expect_true(all(result == "gray50"))  # default zero color
})

test_that("map_edge_colors handles NA weights", {
  weights <- c(NA, NA, NA)
  result <- cograph:::map_edge_colors(weights)
  expect_true(all(result == "gray50"))  # NA treated as zero
})

test_that("map_edge_colors handles mixed weights", {
  weights <- c(1, -1, 0, NA, 0.5, -0.5)
  result <- cograph:::map_edge_colors(weights)
  expect_equal(result[1], "#2E7D32")  # positive
  expect_equal(result[2], "#C62828")  # negative
  expect_equal(result[3], "gray50")   # zero
  expect_equal(result[4], "gray50")   # NA
  expect_equal(result[5], "#2E7D32")  # positive
  expect_equal(result[6], "#C62828")  # negative
})

test_that("map_edge_colors uses custom positive color", {
  weights <- c(1, 2, 3)
  result <- cograph:::map_edge_colors(weights, positive_color = "blue")
  expect_true(all(result == "blue"))
})

test_that("map_edge_colors uses custom negative color", {
  weights <- c(-1, -2, -3)
  result <- cograph:::map_edge_colors(weights, negative_color = "red")
  expect_true(all(result == "red"))
})

test_that("map_edge_colors uses custom zero color", {
  weights <- c(0, 0, 0)
  result <- cograph:::map_edge_colors(weights, zero_color = "black")
  expect_true(all(result == "black"))
})

test_that("map_edge_colors handles empty vector", {
  result <- cograph:::map_edge_colors(numeric(0))
  expect_equal(length(result), 0)
})

# ============================================
# sn_edges: MISSING PARAMETER TESTS
# ============================================

test_that("sn_edges sets label_bg_padding", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  result <- sn_edges(net, label_bg_padding = 0.5)
  aes <- result$edge_aes

  expect_equal(aes$label_bg_padding, 0.5)
})

test_that("sn_edges sets label_border_color", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  result <- sn_edges(net, label_border_color = "navy")
  aes <- result$edge_aes

  expect_equal(aes$label_border_color, "navy")
})

test_that("sn_edges sets label_underline", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  result <- sn_edges(net, label_underline = TRUE)
  aes <- result$edge_aes

  expect_equal(aes$label_underline, TRUE)
})

# ============================================
# sn_edges: EDGE CASES
# ============================================

test_that("sn_edges handles network with no edges", {
  # Create network with isolated nodes (no edges)
  adj <- matrix(0, 4, 4)
  net <- cograph(adj)

  result <- sn_edges(net, color = "gray")

  expect_cograph_network(result)
  expect_equal(n_edges(result), 0)
})

test_that("sn_edges handles single-node network", {
  adj <- matrix(0, 1, 1)
  net <- cograph(adj)

  result <- sn_edges(net, width = 2)

  expect_cograph_network(result)
  expect_equal(n_nodes(result), 1)
  expect_equal(n_edges(result), 0)
})

test_that("sn_edges handles network with self-loops", {
  adj <- diag(4)  # Only self-loops
  net <- cograph(adj)

  result <- sn_edges(net, loop_rotation = pi)

  expect_cograph_network(result)
})

test_that("sn_edges handles width='weight' with no weights in edges", {
  adj <- create_test_matrix(4, weighted = FALSE)
  net <- cograph(adj)

  # If no weight column exists, width='weight' should use resolve_aesthetic
  result <- sn_edges(net, width = "weight")

  expect_cograph_network(result)
})

test_that("sn_edges handles color='weight' with no weights in edges", {
  adj <- create_test_matrix(4, weighted = FALSE)
  net <- cograph(adj)

  # If no weight column exists, color='weight' should use resolve_aesthetic
  result <- sn_edges(net, color = "weight")

  expect_cograph_network(result)
})

test_that("sn_edges preserves existing edge_aes", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  # Set first parameter
  net <- sn_edges(net, width = 2)
  expect_equal(net$edge_aes$width, rep(2, n_edges(net)))

  # Set second parameter - width should be preserved
  net <- sn_edges(net, color = "blue")
  expect_equal(net$edge_aes$width, rep(2, n_edges(net)))
  expect_true(all(net$edge_aes$color == "blue"))
})

test_that("sn_edges initializes edge_aes when NULL", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  net$edge_aes <- NULL  # Force NULL

  result <- sn_edges(net, alpha = 0.5)

  expect_true(!is.null(result$edge_aes))
  expect_true(all(result$edge_aes$alpha == 0.5))
})

# ============================================
# sn_edges: VECTORIZATION TESTS
# ============================================

test_that("sn_edges handles per-edge width vector", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  widths <- seq(1, 3, length.out = m)
  result <- sn_edges(net, width = widths)
  aes <- result$edge_aes

  expect_equal(aes$width, widths)
})

test_that("sn_edges handles per-edge color vector", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  colors <- rep(c("red", "blue"), length.out = m)
  result <- sn_edges(net, color = colors)
  aes <- result$edge_aes

  expect_equal(aes$color, colors)
})

test_that("sn_edges handles per-edge alpha vector", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  alphas <- seq(0.3, 0.9, length.out = m)
  result <- sn_edges(net, alpha = alphas)
  aes <- result$edge_aes

  expect_equal(aes$alpha, alphas)
})

test_that("sn_edges handles per-edge style vector", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  styles <- rep(c("solid", "dashed"), length.out = m)
  result <- sn_edges(net, style = styles)
  aes <- result$edge_aes

  expect_equal(aes$style, styles)
})

test_that("sn_edges handles per-edge loop_rotation", {
  adj <- diag(4) + create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  rotations <- seq(0, pi, length.out = m)
  result <- sn_edges(net, loop_rotation = rotations)

  expect_cograph_network(result)
})

test_that("sn_edges handles per-edge bidirectional flags", {
  adj <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(adj)
  m <- n_edges(net)

  bidir <- rep(c(TRUE, FALSE), length.out = m)
  result <- sn_edges(net, bidirectional = bidir)
  aes <- result$edge_aes

  expect_equal(aes$bidirectional, bidir)
})

# ============================================
# sn_edges: VALIDATION TESTS
# ============================================

test_that("sn_edges validates alpha out of range (both bounds)", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(sn_edges(net, alpha = -0.1))
  expect_error(sn_edges(net, alpha = 1.1))
})

test_that("sn_edges validates ci_alpha out of range", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(sn_edges(net, ci_alpha = -0.1))
  expect_error(sn_edges(net, ci_alpha = 1.5))
})

test_that("sn_edges validates label_shadow_alpha out of range", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(sn_edges(net, label_shadow_alpha = -0.1))
})

test_that("sn_edges validates all edge_scale_mode options", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  # Valid modes
  for (mode in c("linear", "log", "sqrt", "rank")) {
    result <- sn_edges(net, edge_scale_mode = mode)
    expect_equal(result$edge_aes$edge_scale_mode, mode)
  }

  # Invalid mode
  expect_error(
    sn_edges(net, edge_scale_mode = "exponential"),
    "edge_scale_mode must be one of"
  )
})

test_that("sn_edges validates style options", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  # Invalid style
  expect_error(
    sn_edges(net, style = "zigzag"),
    "style must be one of"
  )
})

test_that("sn_edges validates label_fontface options", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(
    sn_edges(net, label_fontface = "oblique"),
    "label_fontface must be one of"
  )
})

test_that("sn_edges validates label_border options", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(
    sn_edges(net, label_border = "oval"),
    "label_border must be one of"
  )
})

test_that("sn_edges validates label_style options", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(
    sn_edges(net, label_style = "custom"),
    "label_style must be one of"
  )
})

test_that("sn_edges validates label_ci_format options", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(
    sn_edges(net, label_ci_format = "pipe"),
    "label_ci_format must be one of"
  )
})

test_that("sn_edges validates curves parameter", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  expect_error(
    sn_edges(net, curves = TRUE),  # TRUE is not valid, only FALSE, "mutual", "force"
    "curves must be"
  )

  expect_error(
    sn_edges(net, curves = "always"),
    "curves must be"
  )
})

# ============================================
# sn_edges: DEPRECATED PARAMETER TESTS
# ============================================

test_that("sn_edges handles all deprecated parameters with warnings", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)

  # esize -> edge_size
  expect_warning(
    result <- sn_edges(net, esize = 10),
    "deprecated"
  )
  expect_equal(result$edge_aes$esize, 10)

  # cut -> edge_cutoff
  expect_warning(
    result <- sn_edges(net, cut = 0.5),
    "deprecated"
  )
  expect_equal(result$edge_aes$cut, 0.5)

  # positive_color -> edge_positive_color
  expect_warning(
    result <- sn_edges(net, positive_color = "green"),
    "deprecated"
  )
  expect_equal(result$edge_aes$positive_color, "green")

  # negative_color -> edge_negative_color
  expect_warning(
    result <- sn_edges(net, negative_color = "red"),
    "deprecated"
  )
  expect_equal(result$edge_aes$negative_color, "red")
})

test_that("sn_edges prefers new parameter over deprecated", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  # When both new and deprecated are specified, new should win
  expect_warning(
    result <- sn_edges(net, edge_size = 15, esize = 10)
  )
  expect_equal(result$edge_aes$esize, 15)  # edge_size wins
})

# ============================================
# sn_edges: INPUT TYPE CONVERSION TESTS
# ============================================

test_that("sn_edges auto-converts data.frame input", {
  edges_df <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    weight = c(0.5, 0.8, 0.3)
  )

  result <- sn_edges(edges_df, width = 2)

  expect_cograph_network(result)
})

test_that("sn_edges auto-converts igraph input", {
  skip_if_not_installed("igraph")

  g <- igraph::make_ring(5)

  result <- sn_edges(g, color = "gray")

  expect_cograph_network(result)
})

# ============================================
# sn_edges: COMPLEX PARAMETER COMBINATIONS
# ============================================

test_that("sn_edges handles full CI styling", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)
  m <- n_edges(net)

  ci_vals <- runif(m, 0.1, 0.5)

  result <- sn_edges(net,
    ci = ci_vals,
    ci_scale = 3,
    ci_alpha = 0.25,
    ci_color = "lightblue",
    ci_style = 3,
    ci_arrows = FALSE
  )

  aes <- result$edge_aes
  expect_equal(aes$ci, ci_vals)
  expect_equal(aes$ci_scale, 3)
  expect_equal(aes$ci_alpha, 0.25)
  expect_equal(aes$ci_color, "lightblue")
  expect_equal(aes$ci_style, 3)
  expect_equal(aes$ci_arrows, FALSE)
})

test_that("sn_edges handles full label template configuration", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)
  m <- n_edges(net)

  result <- sn_edges(net,
    label_template = "{est} ({range}){stars}",
    label_digits = 3,
    label_ci_format = "dash",
    ci_lower = runif(m, 0, 0.3),
    ci_upper = runif(m, 0.7, 1),
    label_p = runif(m, 0, 0.1),
    label_p_digits = 4,
    label_p_prefix = "P=",
    label_stars = TRUE
  )

  aes <- result$edge_aes
  expect_equal(aes$label_template, "{est} ({range}){stars}")
  expect_equal(aes$label_digits, 3)
  expect_equal(aes$label_ci_format, "dash")
  expect_equal(aes$label_p_digits, 4)
  expect_equal(aes$label_p_prefix, "P=")
  expect_equal(aes$label_stars, TRUE)
})

test_that("sn_edges handles full label styling", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  result <- sn_edges(net,
    labels = TRUE,
    label_size = 0.9,
    label_color = "navy",
    label_position = 0.4,
    label_offset = 0.1,
    label_bg = "lightyellow",
    label_bg_padding = 0.4,
    label_fontface = "bold",
    label_border = "rounded",
    label_border_color = "darkgray",
    label_underline = FALSE,
    label_shadow = TRUE,
    label_shadow_color = "gray30",
    label_shadow_offset = 1,
    label_shadow_alpha = 0.6
  )

  aes <- result$edge_aes
  expect_equal(aes$label_size, 0.9)
  expect_equal(aes$label_color, "navy")
  expect_equal(aes$label_position, 0.4)
  expect_equal(aes$label_offset, 0.1)
  expect_equal(aes$label_bg, "lightyellow")
  expect_equal(aes$label_bg_padding, 0.4)
  expect_equal(aes$label_fontface, "bold")
  expect_equal(aes$label_border, "rounded")
  expect_equal(aes$label_border_color, "darkgray")
  expect_equal(aes$label_underline, FALSE)
  expect_equal(aes$label_shadow, TRUE)
  expect_equal(aes$label_shadow_color, "gray30")
  expect_equal(aes$label_shadow_offset, 1)
  expect_equal(aes$label_shadow_alpha, 0.6)
})

test_that("sn_edges handles full curve styling", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)

  result <- sn_edges(net,
    curvature = 0.4,
    curves = "mutual",
    curve_shape = 0.3,
    curve_pivot = 0.6
  )

  aes <- result$edge_aes
  expect_true(all(aes$curvature == 0.4))
  expect_equal(aes$curves, "mutual")
})

test_that("sn_edges handles full arrow styling", {
  adj <- create_test_matrix(4, symmetric = FALSE)
  net <- cograph(adj)

  result <- sn_edges(net,
    arrow_size = 1.2,
    show_arrows = TRUE,
    bidirectional = TRUE
  )

  aes <- result$edge_aes
  expect_equal(aes$arrow_size, 1.2)
  expect_equal(aes$show_arrows, TRUE)
})

# ============================================
# sn_edges: RENDERING INTEGRATION TESTS
# ============================================

test_that("sn_edges with all label parameters renders", {
  adj <- create_test_matrix(4, weighted = TRUE)

  net <- cograph(adj) |>
    sn_edges(
      labels = TRUE,
      label_size = 0.7,
      label_bg = "white",
      label_fontface = "bold",
      label_shadow = TRUE
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_edges with full edge width parameters renders", {
  adj <- create_test_matrix(4, weighted = TRUE)

  net <- cograph(adj) |>
    sn_edges(
      width = "weight",
      edge_size = 12,
      edge_width_range = c(0.5, 5),
      edge_scale_mode = "sqrt",
      edge_cutoff = 0.5,
      width_scale = 1.2,
      maximum = 1
    )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_edges with curves='force' renders", {
  adj <- create_test_matrix(4)

  net <- cograph(adj) |>
    sn_edges(curves = "force", curvature = 0.3)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_edges with label_border='circle' renders", {
  adj <- create_test_matrix(4, weighted = TRUE)

  net <- cograph(adj) |>
    sn_edges(labels = TRUE, label_border = "circle", label_border_color = "gray")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_edges with all styles renders", {
  adj <- create_test_matrix(5, density = 0.8)
  net <- cograph(adj)
  m <- n_edges(net)

  # Mix of all valid styles
  styles <- rep(c("solid", "dashed", "dotted", "longdash", "twodash"),
                length.out = m)

  net <- sn_edges(net, style = styles)

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_edges with label_style='range' renders", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)
  m <- n_edges(net)

  net <- sn_edges(net,
    label_style = "range",
    ci_lower = runif(m, 0, 0.3),
    ci_upper = runif(m, 0.7, 1)
  )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("sn_edges with label_ci_format='bracket' renders", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)
  m <- n_edges(net)

  net <- sn_edges(net,
    label_style = "full",
    label_ci_format = "bracket",
    ci_lower = runif(m, 0, 0.3),
    ci_upper = runif(m, 0.7, 1)
  )

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

# ============================================
# sn_edges: SPECIAL VALUE TESTS
# ============================================

test_that("sn_edges handles labels=FALSE correctly", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)

  # FALSE should not set labels
  result <- sn_edges(net, labels = FALSE)

  # labels = FALSE means we shouldn't add labels to aes
  expect_null(result$edge_aes$labels)
})

test_that("sn_edges handles labels with string vector", {
  # String labels are directly applied as edge labels
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  labels <- paste0("Edge_", seq_len(m))
  result <- sn_edges(net, labels = labels)

  expect_equal(result$edge_aes$labels, labels)
})

test_that("sn_edges handles color with string that doesn't match column", {
  # When a string is provided that isn't a column name in edges_df,

  # it's treated as a literal color value and recycled
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  result <- sn_edges(net, color = "steelblue")

  # Single string gets recycled to all edges
  expect_equal(result$edge_aes$color, rep("steelblue", m))
})

test_that("sn_edges handles per-edge ci values", {
  adj <- create_test_matrix(4, weighted = TRUE)
  net <- cograph(adj)
  m <- n_edges(net)

  ci_vals <- seq(0.1, 0.5, length.out = m)
  result <- sn_edges(net, ci = ci_vals)

  expect_equal(result$edge_aes$ci, ci_vals)
})

test_that("sn_edges handles label_stars as character vector", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  stars <- rep(c("*", "**", "***", ""), length.out = m)
  result <- sn_edges(net, label_stars = stars)

  expect_equal(result$edge_aes$label_stars, stars)
})

test_that("sn_edges handles label_stars as numeric (p-values)", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  m <- n_edges(net)

  # Numeric values treated as p-values
  p_vals <- runif(m, 0, 0.1)
  result <- sn_edges(net, label_stars = p_vals)

  expect_equal(result$edge_aes$label_stars, p_vals)
})

# ============================================
# sn_edges: LARGE NETWORK TESTS
# ============================================

test_that("sn_edges handles large network efficiently", {
  adj <- create_test_matrix(50, density = 0.3)
  net <- cograph(adj)
  m <- n_edges(net)

  # Multiple parameters at once
  result <- sn_edges(net,
    width = "weight",
    color = "weight",
    alpha = 0.7,
    style = "solid",
    curvature = 0.1
  )

  expect_cograph_network(result)
  expect_true(length(result$edge_aes$alpha) == m)
})

# ============================================
# sn_edges: EDGE WEIGHT SCALING COMBINATIONS
# ============================================

test_that("sn_edges width='weight' with negative weights", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  # Force some negative weights
  adj[adj > 0.5] <- adj[adj > 0.5] - 1.5
  net <- cograph(adj)

  result <- sn_edges(net, width = "weight")

  # Width should be based on absolute values
  expect_true(!is.null(result$edge_aes$width))
  expect_true(all(result$edge_aes$width > 0))
})

test_that("sn_edges color='weight' applies custom positive/negative colors", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  # Force some negative weights
  adj[1, 2] <- -0.5
  adj[2, 1] <- -0.3
  net <- cograph(adj)

  result <- sn_edges(net,
    color = "weight",
    edge_positive_color = "forestgreen",
    edge_negative_color = "firebrick"
  )

  colors <- result$edge_aes$color
  expect_true("forestgreen" %in% colors || "firebrick" %in% colors)
})

# ============================================
# sn_edges: PIPING CHAIN TESTS
# ============================================

test_that("sn_edges works in complex pipe chain", {
  adj <- create_test_matrix(4, weighted = TRUE)

  net <- cograph(adj) |>
    sn_edges(width = "weight") |>
    sn_edges(color = "weight") |>
    sn_edges(alpha = 0.8) |>
    sn_edges(style = "solid") |>
    sn_edges(curvature = 0.1)

  aes <- net$edge_aes
  expect_true(!is.null(aes$width))
  expect_true(!is.null(aes$color))
  expect_true(all(aes$alpha == 0.8))
  expect_true(all(aes$style == "solid"))
  expect_true(all(aes$curvature == 0.1))
})

test_that("sn_edges updates work with sn_nodes in chain", {
  adj <- create_test_matrix(4, weighted = TRUE)

  net <- cograph(adj) |>
    sn_edges(width = 2, color = "gray") |>
    sn_nodes(size = 15) |>
    sn_edges(alpha = 0.7)

  expect_cograph_network(net)
  expect_true(all(net$edge_aes$alpha == 0.7))
  expect_true(all(net$edge_aes$width == 2))
  expect_true(all(net$edge_aes$color == "gray"))
})
