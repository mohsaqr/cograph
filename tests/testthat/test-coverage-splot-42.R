# test-coverage-splot-42.R - Comprehensive coverage tests for splot.R
# Tests targeting uncovered branches and functions

# ============================================
# DEPRECATED PARAMETER HANDLING
# ============================================

test_that("splot() warns when using deprecated esize parameter", {
  adj <- create_test_matrix(4)
  expect_warning(
    with_temp_png(splot(adj, esize = 5)),
    "esize.*deprecated"
  )
})

test_that("splot() warns when using deprecated cut parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)
  expect_warning(
    with_temp_png(splot(adj, cut = 0.5)),
    "cut.*deprecated"
  )
})

test_that("splot() warns when using deprecated usePCH parameter", {
  adj <- create_test_matrix(4)
  expect_warning(
    with_temp_png(splot(adj, usePCH = TRUE)),
    "usePCH.*deprecated"
  )
})

test_that("splot() warns when using deprecated positive_color parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)
  expect_warning(
    with_temp_png(splot(adj, positive_color = "green")),
    "positive_color.*deprecated"
  )
})

test_that("splot() warns when using deprecated negative_color parameter", {
  adj <- create_test_matrix(4, weighted = TRUE)
  expect_warning(
    with_temp_png(splot(adj, negative_color = "red")),
    "negative_color.*deprecated"
  )
})

test_that("splot() warns when using deprecated donut_border_lty parameter", {
  adj <- create_test_matrix(4)
  expect_warning(
    with_temp_png(splot(adj, donut_fill = 0.5, donut_border_lty = "dashed")),
    "donut_border_lty.*deprecated"
  )
})

# ============================================
# FILE OUTPUT FORMATS (JPEG/TIFF)
# ============================================

test_that("splot() outputs to JPEG file", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".jpeg")
  on.exit(unlink(tmp), add = TRUE)

  splot(adj, filetype = "jpeg", filename = tools::file_path_sans_ext(tmp),
        width = 5, height = 5, res = 100)

  expect_file_created(paste0(tools::file_path_sans_ext(tmp), ".jpeg"))
})

test_that("splot() outputs to TIFF file", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".tiff")
  on.exit(unlink(tmp), add = TRUE)

  splot(adj, filetype = "tiff", filename = tools::file_path_sans_ext(tmp),
        width = 5, height = 5, res = 100)

  expect_file_created(paste0(tools::file_path_sans_ext(tmp), ".tiff"))
})

test_that("splot() outputs to JPG file (alias)", {
  adj <- create_test_matrix(4)
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp), add = TRUE)

  splot(adj, filetype = "jpg", filename = tools::file_path_sans_ext(tmp),
        width = 5, height = 5, res = 100)

  expect_file_created(paste0(tools::file_path_sans_ext(tmp), ".jpg"))
})

test_that("splot() errors on unknown filetype", {
  adj <- create_test_matrix(4)
  expect_error(
    splot(adj, filetype = "unknown_format"),
    "Unknown filetype"
  )
})

# ============================================
# EDGE START STYLE VARIATIONS
# ============================================

test_that("splot() handles edge_start_style = 'dashed'", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE, edge_start_style = "dashed"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_start_style = 'dotted'", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE, edge_start_style = "dotted"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles numeric edge_start_style", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE, edge_start_style = 2))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_start_style = 3 (dotted)", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE, edge_start_style = 3))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_start_length parameter", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE,
                            edge_start_style = "dashed",
                            edge_start_length = 0.25))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_start_dot_density parameter", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE,
                            edge_start_style = "dotted",
                            edge_start_dot_density = "11"))
  expect_true(result$success, info = result$error)
})

test_that("splot() warns on invalid numeric edge_start_style", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  expect_warning(
    with_temp_png(splot(adj, directed = TRUE, edge_start_style = 5)),
    "edge_start_style"
  )
})

test_that("splot() errors on invalid string edge_start_style", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  expect_error(
    splot(adj, directed = TRUE, edge_start_style = "invalid"),
    "edge_start_style"
  )
})

# ============================================
# CI UNDERLAY SYSTEM
# ============================================

test_that("splot() handles edge_ci with custom colors", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_ci = rep(0.2, n_edges),
    edge_ci_color = "blue"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_ci with custom style (solid)", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_ci = rep(0.2, n_edges),
    edge_ci_style = 1
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_ci with arrows enabled", {
  adj <- create_test_matrix(4, weighted = TRUE, symmetric = FALSE)
  n_edges <- sum(adj != 0)

  result <- safe_plot(splot(adj,
    directed = TRUE,
    edge_ci = rep(0.2, n_edges),
    edge_ci_arrows = TRUE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge edge_ci_scale vector", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_ci = rep(0.2, n_edges),
    edge_ci_scale = seq(1, 3, length.out = n_edges)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge edge_ci_alpha vector", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_ci = rep(0.2, n_edges),
    edge_ci_alpha = seq(0.1, 0.3, length.out = n_edges)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles CI on self-loops", {
  adj <- create_test_matrix(4, weighted = TRUE)
  diag(adj) <- c(0.5, 0.3, 0.2, 0.4)  # Add self-loops

  n_edges <- sum(adj != 0)

  result <- safe_plot(splot(adj,
    edge_ci = rep(0.2, n_edges)
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# DONUT COLOR VARIATIONS
# ============================================

test_that("splot() handles donut_color with two colors (fill + bg)", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    donut_fill = 0.5,
    donut_color = c("steelblue", "lightgray")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_color with per-node colors vector", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, 0.5, 0.7, 0.9),
    donut_color = c("red", "green", "blue", "orange")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles deprecated donut_colors parameter", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, 0.5, 0.7),
    donut_colors = list("red", "green", "blue")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_border_color per-node", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, 0.5, 0.7),
    donut_border_color = c("red", "green", "blue")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_outer_border_color (double border)", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, 0.5, 0.7),
    donut_outer_border_color = "black"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_line_type per-node", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, 0.5, 0.7),
    donut_line_type = c("solid", "dashed", "dotted")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_empty = FALSE", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, NA, 0.7),
    donut_empty = FALSE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut value display with prefix and suffix", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.25, 0.50, 0.75),
    donut_show_value = TRUE,
    donut_value_prefix = "$",
    donut_value_suffix = "K"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_value_fontfamily", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.25, 0.50, 0.75),
    donut_show_value = TRUE,
    donut_value_fontfamily = "mono"
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE LABEL TEMPLATE SYSTEM
# ============================================

test_that("splot() handles edge_label_style = 'full'", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_style = "full",
    edge_ci_lower = runif(n_edges, -0.2, 0),
    edge_ci_upper = runif(n_edges, 0, 0.2)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_style = 'range'", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_style = "range",
    edge_ci_lower = runif(n_edges, -0.2, 0),
    edge_ci_upper = runif(n_edges, 0, 0.2)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_style = 'stars'", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_style = "stars",
    edge_label_p = runif(n_edges, 0, 0.1)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles custom edge_label_template", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_template = "{est}{stars}",
    edge_label_p = runif(n_edges, 0, 0.1)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_ci_format = 'dash'", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_style = "full",
    edge_ci_lower = rep(-0.1, n_edges),
    edge_ci_upper = rep(0.1, n_edges),
    edge_label_ci_format = "dash"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_leading_zero = FALSE", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj,
    edge_label_style = "estimate",
    edge_label_leading_zero = FALSE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_p_prefix", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_template = "{p}",
    edge_label_p = runif(n_edges, 0, 0.1),
    edge_label_p_prefix = "P="
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_stars as character vector", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_template = "{est}{stars}",
    edge_label_stars = rep("***", n_edges)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_stars as numeric p-values", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_template = "{est}{stars}",
    edge_label_stars = runif(n_edges, 0, 0.1)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_stars = TRUE with p-values", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    edge_label_template = "{est}{stars}",
    edge_label_p = runif(n_edges, 0, 0.1),
    edge_label_stars = TRUE
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE LABEL STYLING
# ============================================

test_that("splot() handles edge_label_shadow = TRUE", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj,
    edge_labels = TRUE,
    edge_label_shadow = TRUE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_shadow with custom settings", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj,
    edge_labels = TRUE,
    edge_label_shadow = TRUE,
    edge_label_shadow_color = "black",
    edge_label_shadow_offset = 1,
    edge_label_shadow_alpha = 0.3
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_halo = TRUE", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj,
    edge_labels = TRUE,
    edge_label_halo = TRUE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge edge_label_size", {
  edges <- create_test_edgelist(n_edges = 5, n_nodes = 4, weighted = TRUE)

  result <- safe_plot(splot(edges,
    edge_labels = TRUE,
    edge_label_size = c(0.6, 0.7, 0.8, 0.9, 1.0)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge edge_label_color", {
  edges <- create_test_edgelist(n_edges = 5, n_nodes = 4, weighted = TRUE)

  result <- safe_plot(splot(edges,
    edge_labels = TRUE,
    edge_label_color = c("red", "green", "blue", "purple", "orange")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_label_fontface options", {
  adj <- create_test_matrix(4, weighted = TRUE)

  for (ff in c("plain", "bold", "italic", "bold.italic")) {
    result <- safe_plot(splot(adj, edge_labels = TRUE, edge_label_fontface = ff))
    expect_true(result$success, info = paste("Fontface", ff, "failed:", result$error))
  }
})

test_that("splot() handles edge_label_position values", {
  adj <- create_test_matrix(4, weighted = TRUE)

  for (pos in c(0.3, 0.5, 0.7)) {
    result <- safe_plot(splot(adj, edge_labels = TRUE, edge_label_position = pos))
    expect_true(result$success, info = paste("Position", pos, "failed:", result$error))
  }
})

test_that("splot() handles edge_label_offset values", {
  adj <- create_test_matrix(4, weighted = TRUE)

  for (offset in c(-0.1, 0, 0.1)) {
    result <- safe_plot(splot(adj, edge_labels = TRUE, edge_label_offset = offset))
    expect_true(result$success, info = paste("Offset", offset, "failed:", result$error))
  }
})

# ============================================
# LEGEND RENDERING BRANCHES
# ============================================

test_that("splot() legend shows edge colors with positive only edges", {
  adj <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.4, 0.3, 0.4, 0), 3, 3)  # All positive

  result <- safe_plot(splot(adj, legend = TRUE, legend_edge_colors = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() legend shows edge colors with negative only edges", {
  adj <- matrix(c(0, -0.5, -0.3, -0.5, 0, -0.4, -0.3, -0.4, 0), 3, 3)  # All negative

  result <- safe_plot(splot(adj, legend = TRUE, legend_edge_colors = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() legend shows node sizes", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    legend = TRUE,
    legend_node_sizes = TRUE,
    node_size = c(2, 4, 6, 8)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() legend with groups and edge colors", {
  adj <- create_test_matrix(6, weighted = TRUE)
  groups <- c(1, 1, 2, 2, 3, 3)

  result <- safe_plot(splot(adj,
    legend = TRUE,
    groups = groups,
    legend_edge_colors = TRUE
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() legend with node_names", {
  adj <- create_test_matrix(4)
  groups <- c(1, 1, 2, 2)

  result <- safe_plot(splot(adj,
    legend = TRUE,
    groups = groups,
    node_names = c("Alpha", "Beta", "Gamma", "Delta")
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE STYLE VARIATIONS
# ============================================

test_that("splot() handles string edge_style values", {
  adj <- create_test_matrix(4)

  for (style in c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) {
    result <- safe_plot(splot(adj, edge_style = style))
    expect_true(result$success, info = paste("Style", style, "failed:", result$error))
  }
})

test_that("splot() handles numeric edge_style 4-6", {
  adj <- create_test_matrix(4)

  for (style in 4:6) {
    result <- safe_plot(splot(adj, edge_style = style))
    expect_true(result$success, info = paste("Style", style, "failed:", result$error))
  }
})

test_that("splot() handles per-edge edge_style", {
  edges <- create_test_edgelist(n_edges = 5, n_nodes = 4)

  result <- safe_plot(splot(edges,
    edge_style = c(1, 2, 3, 1, 2)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_cutoff with cutoff values", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, edge_cutoff = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_cutoff = 0 (disabled)", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, edge_cutoff = 0))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_priority for render order", {
  edges <- create_test_edgelist(n_edges = 5, n_nodes = 4, weighted = TRUE)

  result <- safe_plot(splot(edges,
    edge_priority = c(1, 5, 3, 2, 4)
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# PIE AND DOUBLE DONUT COMBINATIONS
# ============================================

test_that("splot() handles donut with pie (donut_pie)", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.7, 0.8, 0.9),
    pie_values = list(c(1, 2), c(3, 1), c(1, 1, 1))
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles polygon donut with pie (non-circular)", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.7, 0.8, 0.9),
    donut_shape = "hexagon",
    pie_values = list(c(1, 2), c(3, 1), c(1, 1, 1))
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut2 with different inner_ratio", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.7, 0.8, 0.6),
    donut2_values = list(c(0.5), c(0.4), c(0.6)),
    donut2_inner_ratio = 0.3
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles pie_border_width", {
  adj <- create_test_matrix(3)
  pie_vals <- list(c(1, 2, 3), c(2, 2), c(1, 1, 1, 1))

  result <- safe_plot(splot(adj, pie_values = pie_vals, pie_border_width = 2))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_border_width", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, 0.5, 0.7),
    donut_border_width = 3
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# LABEL PARAMETERS
# ============================================

test_that("splot() handles label_hjust and label_vjust", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    labels = TRUE,
    label_hjust = 0,
    label_vjust = 1
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles label_angle", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    labels = TRUE,
    label_angle = 45
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-node label_position", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    labels = TRUE,
    label_position = c("center", "above", "below", "right")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-node label_fontface", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    labels = TRUE,
    label_fontface = c("plain", "bold", "italic", "bold.italic")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-node label_fontfamily", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    labels = TRUE,
    label_fontfamily = c("sans", "serif", "mono")
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# NODE SIZE AND SHAPE VARIATIONS
# ============================================

test_that("splot() handles node_size2 for ellipse height", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    node_shape = "ellipse",
    node_size = 5,
    node_size2 = 3
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles use_pch = TRUE for fast rendering", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, use_pch = TRUE, node_shape = "circle"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles scaling = 'legacy'", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, scaling = "legacy"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles aspect = FALSE", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, aspect = FALSE))
  expect_true(result$success, info = result$error)
})

# ============================================
# CENTRALITY-BASED NODE SIZING
# ============================================

test_that("splot() handles scale_nodes_by with string measure", {
  skip_if_no_igraph()
  adj <- create_test_matrix(5)

  result <- safe_plot(splot(adj, scale_nodes_by = "degree"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles scale_nodes_by with list parameters", {
  skip_if_no_igraph()
  adj <- create_test_matrix(5)

  result <- safe_plot(splot(adj,
    scale_nodes_by = list(measure = "betweenness"),
    node_size_range = c(3, 10)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles scale_nodes_by with different measures", {
  skip_if_no_igraph()
  adj <- create_test_matrix(5)

  for (measure in c("strength", "closeness", "eigenvector")) {
    result <- safe_plot(splot(adj, scale_nodes_by = measure))
    expect_true(result$success, info = paste("Measure", measure, "failed:", result$error))
  }
})

# ============================================
# ARROW PARAMETERS
# ============================================

test_that("splot() handles arrow_angle parameter", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, directed = TRUE, arrow_angle = pi/4))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge show_arrows vector", {
  edges <- create_test_edgelist(n_edges = 5, n_nodes = 4)

  result <- safe_plot(splot(edges,
    directed = TRUE,
    show_arrows = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge arrow_size", {
  edges <- create_test_edgelist(n_edges = 5, n_nodes = 4)

  result <- safe_plot(splot(edges,
    directed = TRUE,
    arrow_size = c(0.5, 1, 1.5, 0.8, 1.2)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge bidirectional", {
  edges <- create_test_edgelist(n_edges = 5, n_nodes = 4)

  result <- safe_plot(splot(edges,
    directed = TRUE,
    bidirectional = c(TRUE, FALSE, TRUE, FALSE, FALSE)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles loop_rotation parameter", {
  adj <- create_test_matrix(4)
  diag(adj) <- 1  # Add self-loops

  result <- safe_plot(splot(adj, loop_rotation = pi/4))
  expect_true(result$success, info = result$error)
})

# ============================================
# ALPHA AND COLOR VARIATIONS
# ============================================

test_that("splot() handles per-node node_alpha", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    node_alpha = c(0.3, 0.5, 0.7, 1.0)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-edge edge_alpha", {
  edges <- create_test_edgelist(n_edges = 5, n_nodes = 4)

  result <- safe_plot(splot(edges,
    edge_alpha = c(0.3, 0.5, 0.7, 0.9, 1.0)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-node node_border_color", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    node_border_color = c("red", "green", "blue", "orange")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-node node_border_width", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    node_border_width = c(1, 2, 3, 4)
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# WEIGHT HANDLING
# ============================================

test_that("splot() handles minimum parameter (qgraph compat)", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, minimum = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot() uses max of threshold and minimum", {
  adj <- create_test_matrix(4, weighted = TRUE)

  result <- safe_plot(splot(adj, threshold = 0.2, minimum = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_duplicates = 'sum'", {
  # Create undirected network with duplicate edges
  edges <- data.frame(
    from = c(1, 2, 1, 2),
    to = c(2, 1, 3, 3),
    weight = c(0.5, 0.3, 0.4, 0.2)
  )

  result <- safe_plot(splot(edges, directed = FALSE, edge_duplicates = "sum"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_duplicates = 'mean'", {
  edges <- data.frame(
    from = c(1, 2, 1, 2),
    to = c(2, 1, 3, 3),
    weight = c(0.5, 0.3, 0.4, 0.2)
  )

  result <- safe_plot(splot(edges, directed = FALSE, edge_duplicates = "mean"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_duplicates = 'max'", {
  edges <- data.frame(
    from = c(1, 2, 1, 2),
    to = c(2, 1, 3, 3),
    weight = c(0.5, 0.3, 0.4, 0.2)
  )

  result <- safe_plot(splot(edges, directed = FALSE, edge_duplicates = "max"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_duplicates = 'min'", {
  edges <- data.frame(
    from = c(1, 2, 1, 2),
    to = c(2, 1, 3, 3),
    weight = c(0.5, 0.3, 0.4, 0.2)
  )

  result <- safe_plot(splot(edges, directed = FALSE, edge_duplicates = "min"))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge_duplicates = 'first'", {
  edges <- data.frame(
    from = c(1, 2, 1, 2),
    to = c(2, 1, 3, 3),
    weight = c(0.5, 0.3, 0.4, 0.2)
  )

  result <- safe_plot(splot(edges, directed = FALSE, edge_duplicates = "first"))
  expect_true(result$success, info = result$error)
})

# ============================================
# CURVE MODES AND SHAPES
# ============================================

test_that("splot() handles curve_pivot parameter", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, curvature = 0.3, curve_pivot = 0.3))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles curve_shape parameter", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, curvature = 0.3, curve_shape = 0.5))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles curves = 'mutual'", {
  adj <- create_test_matrix(4, symmetric = FALSE)

  result <- safe_plot(splot(adj, curves = "mutual"))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE CASES AND SPECIAL INPUTS
# ============================================

test_that("splot() handles network with all zero weights after rounding", {
  adj <- matrix(c(0, 0.001, 0.001, 0.001, 0, 0.001, 0.001, 0.001, 0), 3, 3)

  # weight_digits = 1 should round to zero
  result <- safe_plot(splot(adj, weight_digits = 1))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles edge labels being subset after filtering", {
  adj <- create_test_matrix(4, weighted = TRUE)
  n_edges <- sum(adj != 0) / 2

  result <- safe_plot(splot(adj,
    threshold = 0.3,
    edge_labels = rep("label", n_edges)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles NULL node_fill with groups", {
  adj <- create_test_matrix(6)
  groups <- c(1, 1, 2, 2, 3, 3)

  result <- safe_plot(splot(adj, node_fill = NULL, groups = groups))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles nodes$color from cograph_network", {
  adj <- create_test_matrix(4)
  net <- cograph(adj)
  net$nodes$color <- c("red", "green", "blue", "orange")

  result <- safe_plot(splot(net))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles per-node donut_shape inheritance", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    node_shape = c("hexagon", "square", "circle", "diamond"),
    donut_fill = c(0.3, 0.5, 0.7, 0.9)
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles NULL background", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj, background = NULL))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles NA in donut_values with donut_empty = TRUE", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.3, NA, 0.7),
    donut_empty = TRUE
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# SPECIAL NODE SHAPES
# ============================================

test_that("splot() handles mixed polygon donut shapes", {
  adj <- create_test_matrix(4)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.5, 0.6, 0.7, 0.8),
    donut_shape = c("circle", "square", "hexagon", "triangle")
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_shape = 'diamond'", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.5, 0.6, 0.7),
    donut_shape = "diamond"
  ))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles donut_shape = 'pentagon'", {
  adj <- create_test_matrix(3)

  result <- safe_plot(splot(adj,
    donut_fill = c(0.5, 0.6, 0.7),
    donut_shape = "pentagon"
  ))
  expect_true(result$success, info = result$error)
})

# ============================================
# TITLE HANDLING
# ============================================

test_that("splot() handles title with various title_size", {
  adj <- create_test_matrix(4)

  for (size in c(0.8, 1.0, 1.5, 2.0)) {
    result <- safe_plot(splot(adj, title = "Test Network", title_size = size))
    expect_true(result$success, info = paste("Title size", size, "failed:", result$error))
  }
})

# ============================================
# RECIPROCAL EDGE HANDLING
# ============================================

test_that("splot() handles reciprocal edges with default curves = TRUE", {
  # Create a network with reciprocal edges
  edges <- data.frame(
    from = c(1, 2, 1, 3),
    to = c(2, 1, 3, 1),
    weight = c(0.5, 0.3, 0.4, 0.6)
  )

  result <- safe_plot(splot(edges, directed = TRUE, curves = TRUE))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles all reciprocal edges network", {
  # Every edge has a reciprocal
  edges <- data.frame(
    from = c(1, 2, 2, 3, 1, 3),
    to = c(2, 1, 3, 2, 3, 1),
    weight = c(0.5, 0.5, 0.3, 0.3, 0.4, 0.4)
  )

  result <- safe_plot(splot(edges, directed = TRUE))
  expect_true(result$success, info = result$error)
})

# ============================================
# EDGE WEIGHT EDGE CASES
# ============================================

test_that("splot() handles network with exact zero weights", {
  adj <- create_test_matrix(4, weighted = TRUE)
  adj[1, 2] <- 0
  adj[2, 1] <- 0

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles network with very small weights", {
  adj <- matrix(c(0, 1e-10, 1e-10, 1e-10, 0, 1e-10, 1e-10, 1e-10, 0), 3, 3)

  result <- safe_plot(splot(adj, weight_digits = NULL))
  expect_true(result$success, info = result$error)
})

test_that("splot() handles network with very large weights", {
  adj <- matrix(c(0, 1000, 500, 1000, 0, 800, 500, 800, 0), 3, 3)

  result <- safe_plot(splot(adj))
  expect_true(result$success, info = result$error)
})
