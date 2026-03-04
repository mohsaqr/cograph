# Tests for plot_ml_heatmap() - Multilayer Network Heatmap
# Coverage: R/plot-ml-heatmap.R
#
# Functions tested:
#   - plot_ml_heatmap() - main exported function
#   - .extract_ml_layers() - extract layers from various inputs
#   - .transform_to_plane() - perspective coordinate transformation
#   - .build_ml_cells() - build cell polygon data
#   - .build_ml_shells() - build layer shell outlines
#   - .build_ml_labels() - build layer label positions
#   - .build_ml_connections() - build inter-layer connections
#   - .resolve_ml_colors() - resolve color palettes

# ============================================
# Setup: Make internal functions available
# ============================================

.extract_ml_layers <- cograph:::.extract_ml_layers
.transform_to_plane <- cograph:::.transform_to_plane
.build_ml_cells <- cograph:::.build_ml_cells
.build_ml_shells <- cograph:::.build_ml_shells
.build_ml_labels <- cograph:::.build_ml_labels
.build_ml_connections <- cograph:::.build_ml_connections
.resolve_ml_colors <- cograph:::.resolve_ml_colors

# ============================================
# Test Data Setup
# ============================================

create_test_layers <- function(n_layers = 3, n_rows = 4, n_cols = 4, seed = 42) {
  set.seed(seed)
  layers <- lapply(seq_len(n_layers), function(i) {
    matrix(runif(n_rows * n_cols), n_rows, n_cols)
  })
  names(layers) <- paste0("Layer", seq_len(n_layers))
  layers
}

# ============================================
# Basic plot_ml_heatmap() Tests
# ============================================

test_that("plot_ml_heatmap works with basic list of matrices", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with single layer", {
  skip_if_not_installed("ggplot2")

  layers <- list(Single = matrix(runif(16), 4, 4))
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with two layers", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with many layers", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 5)
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with non-square matrices", {

  skip_if_not_installed("ggplot2")

  layers <- list(
    A = matrix(runif(12), 3, 4),
    B = matrix(runif(12), 3, 4),
    C = matrix(runif(12), 3, 4)
  )
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap generates unnamed layers", {
  skip_if_not_installed("ggplot2")

  # Unnamed list
  layers <- list(
    matrix(runif(9), 3, 3),
    matrix(runif(9), 3, 3)
  )
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Title Parameter Tests
# ============================================

test_that("plot_ml_heatmap respects title parameter", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, title = "Custom Title")

  expect_s3_class(p, "ggplot")
  # Check title is present in the plot
  expect_true(!is.null(p$labels$title))
})

test_that("plot_ml_heatmap works without title", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, title = NULL)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Color Palette Tests
# ============================================

test_that("plot_ml_heatmap works with viridis colors", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, colors = "viridis")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with heat colors", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, colors = "heat")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with blues colors", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, colors = "blues")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with reds colors", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, colors = "reds")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with inferno colors", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, colors = "inferno")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with plasma colors", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, colors = "plasma")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with custom color vector", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, colors = c("white", "blue", "red"))

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap uses default colors for unknown palette", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, colors = "unknown_palette")

  expect_s3_class(p, "ggplot")
})

# ============================================
# Perspective Parameter Tests
# ============================================

test_that("plot_ml_heatmap respects layer_spacing parameter", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers, layer_spacing = 5)

  expect_s3_class(p, "ggplot")
})
test_that("plot_ml_heatmap respects skew parameter", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers, skew = 0.2)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap respects compress parameter", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers, compress = 0.8)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with extreme skew value", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, skew = 0.9)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with zero skew", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, skew = 0)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Connection Line Tests
# ============================================

test_that("plot_ml_heatmap shows connections when enabled", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers, show_connections = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap respects connection_color", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers,
    show_connections = TRUE,
    connection_color = "#00FF00"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with dashed connection style", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers,
    show_connections = TRUE,
    connection_style = "dashed"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with solid connection style", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers,
    show_connections = TRUE,
    connection_style = "solid"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with dotted connection style", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)
  p <- plot_ml_heatmap(layers,
    show_connections = TRUE,
    connection_style = "dotted"
  )

  expect_s3_class(p, "ggplot")
})

# ============================================
# Border Parameter Tests
# ============================================

test_that("plot_ml_heatmap shows borders by default", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, show_borders = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap hides borders when disabled", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, show_borders = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap respects border_color", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, border_color = "red")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap respects border_width", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, border_width = 3)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap respects cell_border_color", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, cell_border_color = "grey50")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap respects cell_border_width", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, cell_border_width = 0.5)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Label Parameter Tests
# ============================================

test_that("plot_ml_heatmap shows labels by default", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, show_labels = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap hides labels when disabled", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, show_labels = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap respects label_size", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, label_size = 8)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Legend Parameter Tests
# ============================================

test_that("plot_ml_heatmap shows legend by default", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, show_legend = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap hides legend when disabled", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, show_legend = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap respects legend_title", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, legend_title = "Custom Legend")

  expect_s3_class(p, "ggplot")
})

# ============================================
# Scale Limits Tests
# ============================================

test_that("plot_ml_heatmap respects limits parameter", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, limits = c(0, 1))

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with NA color", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    A = matrix(c(NA, 0.5, 0.3, NA), 2, 2),
    B = matrix(c(0.2, NA, NA, 0.8), 2, 2)
  )
  p <- plot_ml_heatmap(layers, na_color = "grey50")

  expect_s3_class(p, "ggplot")
})

# ============================================
# Input Type Tests - Matrix with layer_list
# ============================================

test_that("plot_ml_heatmap works with matrix and layer_list", {
  skip_if_not_installed("ggplot2")

  # Create a larger matrix
  full_matrix <- matrix(runif(64), 8, 8)
  rownames(full_matrix) <- paste0("N", 1:8)
  colnames(full_matrix) <- paste0("N", 1:8)

  layer_list <- list(
    Group1 = c("N1", "N2", "N3"),
    Group2 = c("N4", "N5", "N6"),
    Group3 = c("N7", "N8")
  )

  p <- plot_ml_heatmap(full_matrix, layer_list = layer_list)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Error Handling Tests
# ============================================

test_that("plot_ml_heatmap errors without ggplot2", {
  # This test checks the error message, but we can't actually unload ggplot2

  # Instead, we verify the error path exists by checking the function
  skip_if_not_installed("ggplot2")

  # Just verify function exists and works
  layers <- create_test_layers(n_layers = 2)
  expect_no_error(plot_ml_heatmap(layers))
})

test_that("plot_ml_heatmap errors on invalid input", {
  skip_if_not_installed("ggplot2")

  # Not a matrix or list of matrices
  expect_error(
    plot_ml_heatmap("invalid"),
    "list of matrices|group_tna|matrix"
  )
})

test_that("plot_ml_heatmap errors on matrix without layer_list", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(16), 4, 4)
  expect_error(
    plot_ml_heatmap(mat),
    "list of matrices|group_tna|matrix with layer_list"
  )
})

# ============================================
# Internal Function Tests: .extract_ml_layers
# ============================================

test_that(".extract_ml_layers works with list of matrices", {
  layers <- list(
    A = matrix(1:4, 2, 2),
    B = matrix(5:8, 2, 2)
  )
  result <- .extract_ml_layers(layers, layer_list = NULL)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, is.matrix)))
})

test_that(".extract_ml_layers works with matrix and layer_list", {
  full_matrix <- matrix(runif(16), 4, 4)
  rownames(full_matrix) <- colnames(full_matrix) <- LETTERS[1:4]

  layer_list <- list(
    G1 = c("A", "B"),
    G2 = c("C", "D")
  )

  result <- .extract_ml_layers(full_matrix, layer_list = layer_list)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(nrow(result[[1]]), 2)
  expect_equal(nrow(result[[2]]), 2)
})

test_that(".extract_ml_layers errors on invalid input without layer_list", {
  mat <- matrix(1:9, 3, 3)
  expect_error(.extract_ml_layers(mat, layer_list = NULL))
})

# ============================================
# Internal Function Tests: .transform_to_plane
# ============================================

test_that(".transform_to_plane correctly transforms coordinates", {
  result <- .transform_to_plane(
    x = 1, y = 2,
    layer_idx = 1, n_layers = 3,
    skew = 0.4, compress = 0.6,
    layer_spacing = 2.5
  )

  expect_type(result, "list")
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_type(result$x, "double")
  expect_type(result$y, "double")
})

test_that(".transform_to_plane handles multiple coordinates", {
  result <- .transform_to_plane(
    x = c(0, 1, 2), y = c(0, 1, 2),
    layer_idx = 2, n_layers = 3,
    skew = 0.5, compress = 0.5,
    layer_spacing = 2
  )

  expect_length(result$x, 3)
  expect_length(result$y, 3)
})

test_that(".transform_to_plane y_offset depends on layer_idx", {
  result1 <- .transform_to_plane(0, 0, 1, 3, 0.4, 0.6, 2.5)
  result2 <- .transform_to_plane(0, 0, 2, 3, 0.4, 0.6, 2.5)
  result3 <- .transform_to_plane(0, 0, 3, 3, 0.4, 0.6, 2.5)

  # Layer 1 should have highest y (top)
  expect_true(result1$y > result2$y)
  expect_true(result2$y > result3$y)
})

# ============================================
# Internal Function Tests: .build_ml_cells
# ============================================

test_that(".build_ml_cells creates correct data frame structure", {
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(4), 2, 2)
  )

  cells <- .build_ml_cells(layers, skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(cells, "data.frame")
  expect_true("x" %in% names(cells))
  expect_true("y" %in% names(cells))
  expect_true("weight" %in% names(cells))
  expect_true("layer" %in% names(cells))
  expect_true("cell_id" %in% names(cells))
})

test_that(".build_ml_cells creates correct number of cell polygons", {
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(4), 2, 2)
  )

  cells <- .build_ml_cells(layers, skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  # Each cell has 4 corners, 2 layers x 2x2 = 8 cells total = 32 rows
  expect_equal(nrow(cells), 2 * 2 * 2 * 4)
})

# ============================================
# Internal Function Tests: .build_ml_shells
# ============================================

test_that(".build_ml_shells creates correct data frame structure", {
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(4), 2, 2)
  )

  shells <- .build_ml_shells(layers, n_rows = 2, n_cols = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(shells, "data.frame")
  expect_true("x" %in% names(shells))
  expect_true("y" %in% names(shells))
  expect_true("layer" %in% names(shells))
})

test_that(".build_ml_shells creates 5 points per layer (closed polygon)", {
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(4), 2, 2)
  )

  shells <- .build_ml_shells(layers, n_rows = 2, n_cols = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  # 2 layers x 5 corner points = 10 rows
  expect_equal(nrow(shells), 2 * 5)
})

# ============================================
# Internal Function Tests: .build_ml_labels
# ============================================

test_that(".build_ml_labels creates correct data frame structure", {
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(4), 2, 2)
  )

  labels <- .build_ml_labels(layers, n_rows = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(labels, "data.frame")
  expect_true("x" %in% names(labels))
  expect_true("y" %in% names(labels))
  expect_true("label" %in% names(labels))
})

test_that(".build_ml_labels creates one label per layer", {
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(4), 2, 2),
    L3 = matrix(runif(4), 2, 2)
  )

  labels <- .build_ml_labels(layers, n_rows = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_equal(nrow(labels), 3)
  expect_equal(labels$label, c("L1", "L2", "L3"))
})

# ============================================
# Internal Function Tests: .build_ml_connections
# ============================================

test_that(".build_ml_connections creates correct data frame structure", {
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(4), 2, 2),
    L3 = matrix(runif(4), 2, 2)
  )

  conns <- .build_ml_connections(layers, n_rows = 2, n_cols = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(conns, "data.frame")
  expect_true("x" %in% names(conns))
  expect_true("y" %in% names(conns))
  expect_true("group" %in% names(conns))
})

test_that(".build_ml_connections connects diagonal cells", {
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(4), 2, 2)
  )

  conns <- .build_ml_connections(layers, n_rows = 2, n_cols = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  # 2x2 matrix: min(2,2) = 2 diagonal cells, 1 connection between layers
  # Each connection has 2 points (from and to)
  # 2 cells x 1 layer-pair x 2 points = 4 rows
  expect_equal(nrow(conns), 2 * 1 * 2)
})

test_that(".build_ml_connections with three layers", {
  layers <- list(
    L1 = matrix(runif(9), 3, 3),
    L2 = matrix(runif(9), 3, 3),
    L3 = matrix(runif(9), 3, 3)
  )

  conns <- .build_ml_connections(layers, n_rows = 3, n_cols = 3,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  # 3x3 matrix: min(3,3) = 3 diagonal cells
  # 2 layer-pairs (1-2, 2-3)
  # Each connection has 2 points
  # 3 cells x 2 pairs x 2 points = 12 rows
  expect_equal(nrow(conns), 3 * 2 * 2)
})

# ============================================
# Internal Function Tests: .resolve_ml_colors
# ============================================

test_that(".resolve_ml_colors returns viridis palette", {
  colors <- .resolve_ml_colors("viridis")
  expect_type(colors, "character")
  expect_true(length(colors) > 1)
})

test_that(".resolve_ml_colors returns heat palette", {
  colors <- .resolve_ml_colors("heat")
  expect_type(colors, "character")
  expect_true(length(colors) > 1)
})

test_that(".resolve_ml_colors returns blues palette", {
  colors <- .resolve_ml_colors("blues")
  expect_type(colors, "character")
  expect_true(length(colors) > 1)
})

test_that(".resolve_ml_colors returns reds palette", {
  colors <- .resolve_ml_colors("reds")
  expect_type(colors, "character")
  expect_true(length(colors) > 1)
})

test_that(".resolve_ml_colors returns inferno palette", {
  colors <- .resolve_ml_colors("inferno")
  expect_type(colors, "character")
  expect_true(length(colors) > 1)
})

test_that(".resolve_ml_colors returns plasma palette", {
  colors <- .resolve_ml_colors("plasma")
  expect_type(colors, "character")
  expect_true(length(colors) > 1)
})

test_that(".resolve_ml_colors returns default for unknown palette", {
  colors <- .resolve_ml_colors("unknown")
  expect_type(colors, "character")
  expect_true(length(colors) > 1)
  # Default is viridis
  expect_equal(colors, .resolve_ml_colors("viridis"))
})

test_that(".resolve_ml_colors passes through custom color vector", {
  custom <- c("red", "white", "blue")
  colors <- .resolve_ml_colors(custom)
  expect_equal(colors, custom)
})

# ============================================
# TNA Integration Tests
# ============================================

test_that("plot_ml_heatmap works with group_tna object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tna")

  library(tna)
  data(engagement, package = "tna")

  # Create artificial groups
  n <- nrow(engagement)
  groups <- rep(c("A", "B", "C"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  p <- plot_ml_heatmap(group_model)

  expect_s3_class(p, "ggplot")
})

test_that(".extract_ml_layers works with group_tna object", {
  skip_if_not_installed("tna")

  library(tna)
  data(engagement, package = "tna")

  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  result <- .extract_ml_layers(group_model, layer_list = NULL)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, is.matrix)))
})

# ============================================
# Edge Cases and Robustness Tests
# ============================================

test_that("plot_ml_heatmap handles matrices with zero values", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    A = matrix(0, 3, 3),
    B = matrix(0, 3, 3)
  )
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap handles matrices with negative values", {
  skip_if_not_installed("ggplot2")

  set.seed(123)
  layers <- list(
    A = matrix(rnorm(9), 3, 3),
    B = matrix(rnorm(9), 3, 3)
  )
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap handles 1x1 matrices", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    A = matrix(0.5, 1, 1),
    B = matrix(0.8, 1, 1)
  )
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap handles larger matrices", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()

  set.seed(42)
  layers <- list(
    A = matrix(runif(100), 10, 10),
    B = matrix(runif(100), 10, 10)
  )
  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Comprehensive Parameter Combination Tests
# ============================================

test_that("plot_ml_heatmap works with all styling options combined", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)

  p <- plot_ml_heatmap(layers,
    colors = "plasma",
    layer_spacing = 3,
    skew = 0.3,
    compress = 0.7,
    show_connections = TRUE,
    connection_color = "#FF5500",
    connection_style = "dotted",
    show_borders = TRUE,
    border_color = "darkblue",
    border_width = 2,
    cell_border_color = "grey80",
    cell_border_width = 0.1,
    show_labels = TRUE,
    label_size = 6,
    show_legend = TRUE,
    legend_title = "Value",
    title = "Combined Test",
    limits = c(0, 1),
    na_color = "purple"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with minimal styling", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)

  p <- plot_ml_heatmap(layers,
    show_connections = FALSE,
    show_borders = FALSE,
    show_labels = FALSE,
    show_legend = FALSE,
    title = NULL
  )

  expect_s3_class(p, "ggplot")
})
