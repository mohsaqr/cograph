# Tests for plot_ml_heatmap() - Extended Coverage
# Coverage: R/plot-ml-heatmap.R
#
# This file extends test-coverage-plot-ml-heatmap-40.R with additional tests
# focusing on:
#   - cograph_network input with layer column detection
#   - Message output verification
#   - Error handling edge cases
#   - Graphics rendering with with_temp_png()
#   - Additional internal helper edge cases

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
# cograph_network Input Tests
# ============================================

test_that(".extract_ml_layers works with cograph_network with 'layers' column", {
  skip_if_not_installed("ggplot2")

  # Create a simple cograph_network with layers column
  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- cograph(mat, layout = "circle")
  net$nodes$layers <- c("L1", "L1", "L2", "L2")

  # Auto-detect layers from "layers" column
  expect_message(
    result <- .extract_ml_layers(net, layer_list = NULL),
    "Using 'layers' column for layers"
  )

  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, is.matrix)))
})

test_that(".extract_ml_layers works with cograph_network with 'layer' column", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- cograph(mat, layout = "circle")
  net$nodes$layer <- c("GroupA", "GroupA", "GroupB", "GroupB")

  expect_message(
    result <- .extract_ml_layers(net, layer_list = NULL),
    "Using 'layer' column for layers"
  )

  expect_type(result, "list")
  expect_length(result, 2)
})

test_that(".extract_ml_layers works with cograph_network with 'level' column", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("N1", "N2", "N3")

  net <- cograph(mat, layout = "circle")
  net$nodes$level <- c("Top", "Middle", "Bottom")

  expect_message(
    result <- .extract_ml_layers(net, layer_list = NULL),
    "Using 'level' column for layers"
  )

  expect_type(result, "list")
  expect_length(result, 3)
})

test_that(".extract_ml_layers works with cograph_network with 'levels' column", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("N1", "N2", "N3")

  net <- cograph(mat, layout = "circle")
  net$nodes$levels <- c("Level1", "Level2", "Level1")

  expect_message(
    result <- .extract_ml_layers(net, layer_list = NULL),
    "Using 'levels' column for layers"
  )

  expect_type(result, "list")
  expect_length(result, 2)
})

test_that(".extract_ml_layers works with cograph_network and explicit column name", {

  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- cograph(mat, layout = "circle")
  net$nodes$my_groups <- c("G1", "G1", "G2", "G2")

  # Pass the column name as layer_list
  # Note: message prints the split result, not the column name (line 201 in source)
  expect_message(
    result <- .extract_ml_layers(net, layer_list = "my_groups"),
    "Using '.*' column for layers"
  )

  expect_type(result, "list")
  expect_length(result, 2)
})

test_that(".extract_ml_layers errors on cograph_network without layer info", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- cograph(mat, layout = "circle")
  # No layers/layer/level/levels column

  expect_error(
    .extract_ml_layers(net, layer_list = NULL),
    "layer_list required"
  )
})

test_that(".extract_ml_layers works with cograph_network and list layer_list", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- cograph(mat, layout = "circle")

  layer_list <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D")
  )

  result <- .extract_ml_layers(net, layer_list = layer_list)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(names(result), c("Group1", "Group2"))
})

# ============================================
# plot_ml_heatmap with cograph_network
# ============================================

test_that("plot_ml_heatmap works with cograph_network with layers column", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- cograph(mat, layout = "circle")
  net$nodes$layers <- c("L1", "L1", "L2", "L2")

  expect_message(
    p <- plot_ml_heatmap(net),
    "Using 'layers' column for layers"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap works with cograph_network and explicit layer_list", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")

  net <- cograph(mat, layout = "circle")

  layer_list <- list(
    Layer1 = c("A", "B"),
    Layer2 = c("C", "D")
  )

  p <- plot_ml_heatmap(net, layer_list = layer_list)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap errors on cograph_network without layer info", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  net <- cograph(mat, layout = "circle")

  expect_error(
    plot_ml_heatmap(net),
    "layer_list required"
  )
})

# ============================================
# Graphics Rendering Tests with with_temp_png
# ============================================

test_that("plot_ml_heatmap renders correctly with basic list", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)

  with_temp_png({
    p <- plot_ml_heatmap(layers)
    print(p)
  })

  expect_true(TRUE)
})

test_that("plot_ml_heatmap renders with show_connections = TRUE", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)

  with_temp_png({
    p <- plot_ml_heatmap(layers, show_connections = TRUE)
    print(p)
  })

  expect_true(TRUE)
})

test_that("plot_ml_heatmap renders with all visual parameters", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)

  with_temp_png({
    p <- plot_ml_heatmap(
      layers,
      colors = "plasma",
      layer_spacing = 3,
      skew = 0.5,
      compress = 0.7,
      show_connections = TRUE,
      connection_color = "#00FF00",
      connection_style = "solid",
      show_borders = TRUE,
      border_color = "navy",
      border_width = 2,
      cell_border_color = "grey",
      cell_border_width = 0.3,
      show_labels = TRUE,
      label_size = 6,
      show_legend = TRUE,
      legend_title = "Custom Legend",
      title = "Test Title",
      limits = c(0, 1),
      na_color = "yellow"
    )
    print(p)
  })

  expect_true(TRUE)
})

test_that("plot_ml_heatmap renders without legend", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)

  with_temp_png({
    p <- plot_ml_heatmap(layers, show_legend = FALSE)
    print(p)
  })

  expect_true(TRUE)
})

test_that("plot_ml_heatmap renders without borders", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)

  with_temp_png({
    p <- plot_ml_heatmap(layers, show_borders = FALSE)
    print(p)
  })

  expect_true(TRUE)
})

test_that("plot_ml_heatmap renders without labels", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)

  with_temp_png({
    p <- plot_ml_heatmap(layers, show_labels = FALSE)
    print(p)
  })

  expect_true(TRUE)
})

test_that("plot_ml_heatmap renders with cograph_network", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C", "D")
  net <- cograph(mat, layout = "circle")
  net$nodes$layers <- c("L1", "L1", "L2", "L2")

  with_temp_png({
    p <- suppressMessages(plot_ml_heatmap(net))
    print(p)
  })

  expect_true(TRUE)
})

# ============================================
# Additional Edge Cases for Internal Helpers
# ============================================

test_that(".transform_to_plane handles zero layer_spacing", {
  result <- .transform_to_plane(
    x = 1, y = 1,
    layer_idx = 1, n_layers = 3,
    skew = 0.4, compress = 0.6,
    layer_spacing = 0
  )

  expect_type(result, "list")
  expect_true(is.numeric(result$x))
  expect_true(is.numeric(result$y))
})

test_that(".transform_to_plane handles single layer", {
  result <- .transform_to_plane(
    x = 0, y = 0,
    layer_idx = 1, n_layers = 1,
    skew = 0.4, compress = 0.6,
    layer_spacing = 2.5
  )

  expect_type(result, "list")
  expect_equal(result$x, 0)
  # y should be 0 * 0.6 + 0 = 0 (n_layers - layer_idx = 1 - 1 = 0)
  expect_equal(result$y, 0)
})

test_that(".transform_to_plane handles extreme skew = 1", {
  result <- .transform_to_plane(
    x = 1, y = 1,
    layer_idx = 1, n_layers = 2,
    skew = 1, compress = 0.6,
    layer_spacing = 2.5
  )

  expect_type(result, "list")
  # x_new = x + y * skew = 1 + 1 * 1 = 2
  expect_equal(result$x, 2)
})

test_that(".transform_to_plane handles compress = 1", {
  result <- .transform_to_plane(
    x = 1, y = 2,
    layer_idx = 2, n_layers = 3,
    skew = 0, compress = 1,
    layer_spacing = 2.5
  )

  expect_type(result, "list")
  # y_new = y * compress + y_offset = 2 * 1 + (3 - 2) * 2.5 = 2 + 2.5 = 4.5
  expect_equal(result$y, 4.5)
})

test_that(".build_ml_cells handles single cell matrix", {
  layers <- list(L1 = matrix(0.5, 1, 1))

  cells <- .build_ml_cells(layers, skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(cells, "data.frame")
  # 1 layer x 1 cell x 4 corners = 4 rows

  expect_equal(nrow(cells), 4)
})

test_that(".build_ml_cells preserves weight values", {
  layers <- list(
    L1 = matrix(c(0.1, 0.2, 0.3, 0.4), 2, 2)
  )

  cells <- .build_ml_cells(layers, skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  # Each cell appears 4 times (4 corners)
  unique_weights <- unique(cells$weight)
  expect_setequal(unique_weights, c(0.1, 0.2, 0.3, 0.4))
})

test_that(".build_ml_cells handles layers with different dimensions is not supported", {
  # This tests that if layers have different sizes, we still get output
  # (the function uses the first layer's dimensions)
  layers <- list(
    L1 = matrix(runif(4), 2, 2),
    L2 = matrix(runif(9), 3, 3)
  )

  # This should work but may not render correctly visually
  cells <- .build_ml_cells(layers, skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(cells, "data.frame")
})

test_that(".build_ml_shells handles single layer", {
  layers <- list(Single = matrix(runif(4), 2, 2))

  shells <- .build_ml_shells(layers, n_rows = 2, n_cols = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(shells, "data.frame")
  # 1 layer x 5 corners = 5 rows
  expect_equal(nrow(shells), 5)
})

test_that(".build_ml_labels handles single layer", {
  layers <- list(Single = matrix(runif(4), 2, 2))

  labels <- .build_ml_labels(layers, n_rows = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(labels, "data.frame")
  expect_equal(nrow(labels), 1)
  expect_equal(labels$label, "Single")
})

test_that(".build_ml_connections returns empty-like structure for single layer", {
  layers <- list(Single = matrix(runif(4), 2, 2))

  conns <- .build_ml_connections(layers, n_rows = 2, n_cols = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  # With single layer, n_layers - 1 = 0 layer pairs, so no connections
  expect_null(conns)
})

test_that(".build_ml_connections handles non-square matrices", {
  layers <- list(
    L1 = matrix(runif(6), 2, 3),
    L2 = matrix(runif(6), 2, 3)
  )

  conns <- .build_ml_connections(layers, n_rows = 2, n_cols = 3,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(conns, "data.frame")
  # min(2, 3) = 2 diagonal cells x 1 layer pair x 2 points = 4 rows
  expect_equal(nrow(conns), 4)
})

test_that(".resolve_ml_colors returns single-color as named palette default", {
  single_color <- c("red")
  result <- .resolve_ml_colors(single_color)

  # Single element string that doesn't match known palettes returns viridis default
  # because the switch() falls through to default
  expect_type(result, "character")
  expect_true(length(result) > 1)
})

test_that(".resolve_ml_colors handles two-color gradient", {
  colors <- c("white", "red")
  result <- .resolve_ml_colors(colors)

  expect_equal(result, colors)
  expect_length(result, 2)
})

# ============================================
# Connection Style Edge Cases
# ============================================

test_that("plot_ml_heatmap handles unknown connection_style", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)

  # Unknown style should default to "solid"
  p <- plot_ml_heatmap(layers,
    show_connections = TRUE,
    connection_style = "unknown_style"
  )

  expect_s3_class(p, "ggplot")
})

# ============================================
# NA Values in Matrices
# ============================================

test_that("plot_ml_heatmap handles matrices with all NA values", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    A = matrix(NA, 2, 2),
    B = matrix(NA, 2, 2)
  )

  p <- plot_ml_heatmap(layers, na_color = "purple")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap renders matrices with NA values", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    A = matrix(c(0.5, NA, 0.3, NA), 2, 2),
    B = matrix(c(NA, 0.7, NA, 0.9), 2, 2)
  )

  with_temp_png({
    p <- plot_ml_heatmap(layers, na_color = "grey50")
    print(p)
  })

  expect_true(TRUE)
})

# ============================================
# Limits Parameter Edge Cases
# ============================================

test_that("plot_ml_heatmap handles limits outside data range", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  layers <- list(
    A = matrix(runif(4, 0.2, 0.8), 2, 2),
    B = matrix(runif(4, 0.2, 0.8), 2, 2)
  )

  # Limits extend beyond data range
  p <- plot_ml_heatmap(layers, limits = c(0, 2))

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap handles limits narrower than data range", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  layers <- list(
    A = matrix(runif(4, 0, 1), 2, 2),
    B = matrix(runif(4, 0, 1), 2, 2)
  )

  # Limits narrower than data - values outside will be clamped
  p <- plot_ml_heatmap(layers, limits = c(0.3, 0.7))

  expect_s3_class(p, "ggplot")
})

# ============================================
# Very Small/Large Values
# ============================================

test_that("plot_ml_heatmap handles very small values", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    A = matrix(1e-10, 2, 2),
    B = matrix(1e-10, 2, 2)
  )

  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap handles very large values", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    A = matrix(1e10, 2, 2),
    B = matrix(1e10, 2, 2)
  )

  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap handles mixed positive/negative values", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  layers <- list(
    A = matrix(rnorm(4), 2, 2),
    B = matrix(rnorm(4), 2, 2)
  )

  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Multiple Layers Stress Test
# ============================================

test_that("plot_ml_heatmap handles many layers", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()

  set.seed(42)
  layers <- lapply(1:10, function(i) {
    matrix(runif(9), 3, 3)
  })
  names(layers) <- paste0("L", 1:10)

  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap renders many layers", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()

  set.seed(42)
  layers <- lapply(1:5, function(i) {
    matrix(runif(16), 4, 4)
  })
  names(layers) <- paste0("Layer", 1:5)

  with_temp_png(width = 400, height = 400, {
    p <- plot_ml_heatmap(layers, show_connections = TRUE)
    print(p)
  })

  expect_true(TRUE)
})

# ============================================
# Unicode and Special Characters in Layer Names
# ============================================

test_that("plot_ml_heatmap handles layer names with spaces", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    "Layer One" = matrix(runif(4), 2, 2),
    "Layer Two" = matrix(runif(4), 2, 2)
  )

  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

test_that("plot_ml_heatmap handles layer names with special characters", {
  skip_if_not_installed("ggplot2")

  layers <- list(
    "Layer-1" = matrix(runif(4), 2, 2),
    "Layer_2" = matrix(runif(4), 2, 2),
    "Layer.3" = matrix(runif(4), 2, 2)
  )

  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Different Matrix Sizes per Layer
# ============================================

test_that("plot_ml_heatmap works with varying matrix sizes", {
  skip_if_not_installed("ggplot2")

  # Note: This may not render correctly but should not error
  layers <- list(
    Small = matrix(runif(4), 2, 2),
    Large = matrix(runif(9), 3, 3)
  )

  p <- plot_ml_heatmap(layers)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Zero Dimension Handling
# ============================================

test_that(".build_ml_connections handles very small n_connect", {
  layers <- list(
    L1 = matrix(runif(2), 1, 2),
    L2 = matrix(runif(2), 1, 2)
  )

  conns <- .build_ml_connections(layers, n_rows = 1, n_cols = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_s3_class(conns, "data.frame")
  # min(1, 2) = 1 diagonal cell x 1 layer pair x 2 points = 2 rows
  expect_equal(nrow(conns), 2)
})

# ============================================
# Layer Index Calculations
# ============================================

test_that(".build_ml_cells correctly assigns layer_idx", {
  layers <- list(
    First = matrix(0.1, 2, 2),
    Second = matrix(0.2, 2, 2),
    Third = matrix(0.3, 2, 2)
  )

  cells <- .build_ml_cells(layers, skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  # Check layer_idx values
  expect_equal(sort(unique(cells$layer_idx)), c(1, 2, 3))
})

test_that(".build_ml_shells correctly assigns layer names", {
  layers <- list(
    Alpha = matrix(runif(4), 2, 2),
    Beta = matrix(runif(4), 2, 2),
    Gamma = matrix(runif(4), 2, 2)
  )

  shells <- .build_ml_shells(layers, n_rows = 2, n_cols = 2,
    skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_true("Alpha" %in% shells$layer)
  expect_true("Beta" %in% shells$layer)
  expect_true("Gamma" %in% shells$layer)
})

# ============================================
# Cell ID Uniqueness
# ============================================

test_that(".build_ml_cells generates unique cell_ids", {
  layers <- create_test_layers(n_layers = 3, n_rows = 4, n_cols = 4)

  cells <- .build_ml_cells(layers, skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  # Each cell has 4 corners, cell_id is repeated 4 times
  cell_counts <- table(cells$cell_id)
  expect_true(all(cell_counts == 4))

  # Total unique cells = 3 layers x 4 rows x 4 cols = 48
  expect_equal(length(unique(cells$cell_id)), 48)
})

# ============================================
# Layer Factor Levels
# ============================================

test_that(".build_ml_cells creates factor with reversed layer levels", {
  layers <- list(
    A = matrix(runif(4), 2, 2),
    B = matrix(runif(4), 2, 2),
    C = matrix(runif(4), 2, 2)
  )

  cells <- .build_ml_cells(layers, skew = 0.4, compress = 0.6, layer_spacing = 2.5)

  expect_true(is.factor(cells$layer))
  expect_equal(levels(cells$layer), c("C", "B", "A"))
})

# ============================================
# Empty Layer List Handling
# ============================================

test_that(".extract_ml_layers returns empty list for empty input", {
  # An empty list of matrices returns empty list (not an error)
  result <- .extract_ml_layers(list(), layer_list = NULL)
  expect_type(result, "list")
  expect_length(result, 0)
})

# ============================================
# Color Scale Integration
# ============================================

test_that("plot_ml_heatmap color scales render correctly", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)

  palettes <- c("viridis", "inferno", "plasma", "heat", "blues", "reds")

  for (pal in palettes) {
    with_temp_png({
      p <- plot_ml_heatmap(layers, colors = pal)
      print(p)
    })
  }

  expect_true(TRUE)
})

# ============================================
# Combined Parameters Stress Test
# ============================================

test_that("plot_ml_heatmap handles all off options", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)

  p <- plot_ml_heatmap(
    layers,
    show_connections = FALSE,
    show_borders = FALSE,
    show_labels = FALSE,
    show_legend = FALSE,
    title = NULL
  )

  expect_s3_class(p, "ggplot")

  with_temp_png({
    print(p)
  })

  expect_true(TRUE)
})

test_that("plot_ml_heatmap handles all on options with connections", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 3)

  p <- plot_ml_heatmap(
    layers,
    show_connections = TRUE,
    connection_color = "#FF0000",
    connection_style = "dotted",
    show_borders = TRUE,
    border_color = "blue",
    border_width = 3,
    cell_border_color = "white",
    cell_border_width = 0.5,
    show_labels = TRUE,
    label_size = 7,
    show_legend = TRUE,
    legend_title = "Intensity",
    title = "Full Features"
  )

  expect_s3_class(p, "ggplot")

  with_temp_png({
    print(p)
  })

  expect_true(TRUE)
})

# ============================================
# Matrix Input with layer_list
# ============================================

test_that("plot_ml_heatmap handles matrix with overlapping node names", {
  skip_if_not_installed("ggplot2")

  full_matrix <- matrix(runif(36), 6, 6)
  rownames(full_matrix) <- colnames(full_matrix) <- paste0("N", 1:6)

  # Non-overlapping groups
  layer_list <- list(
    G1 = c("N1", "N2"),
    G2 = c("N3", "N4"),
    G3 = c("N5", "N6")
  )

  p <- plot_ml_heatmap(full_matrix, layer_list = layer_list)

  expect_s3_class(p, "ggplot")
})

test_that(".extract_ml_layers preserves submatrix structure", {
  full_matrix <- matrix(1:25, 5, 5)
  rownames(full_matrix) <- colnames(full_matrix) <- LETTERS[1:5]

  layer_list <- list(
    First = c("A", "B"),
    Second = c("C", "D", "E")
  )

  result <- .extract_ml_layers(full_matrix, layer_list = layer_list)

  expect_equal(dim(result$First), c(2, 2))
  expect_equal(dim(result$Second), c(3, 3))

  # Check that values are correctly extracted
  expect_equal(result$First, full_matrix[c("A", "B"), c("A", "B")])
  expect_equal(result$Second, full_matrix[c("C", "D", "E"), c("C", "D", "E")])
})

# ============================================
# Return Value Tests
# ============================================

test_that("plot_ml_heatmap returns ggplot with expected layers", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers, show_connections = TRUE, show_borders = TRUE)

  # Check plot has expected structure

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2)  # At least cells + borders
})

test_that("plot_ml_heatmap coordinates are applied", {
  skip_if_not_installed("ggplot2")

  layers <- create_test_layers(n_layers = 2)
  p <- plot_ml_heatmap(layers)

  # Check that coordinates are applied (CoordCartesian or CoordFixed depending on ggplot2 version)
  expect_true(inherits(p$coordinates, "Coord"))
})
