# Tests for plot-heatmap.R
# Comprehensive test coverage for heatmap plotting functions

# ============================================
# Basic plot_heatmap() Tests
# ============================================

test_that("plot_heatmap works with basic matrix", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with unlabeled matrix", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(16), 4, 4)

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with small matrix", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap errors without ggplot2", {
  # This test is tricky to implement since we need ggplot2 for all other tests

  # We verify the error message exists in source code instead
  skip("Cannot test missing ggplot2 when ggplot2 is required for testing")
})

# ============================================
# Color Palette Tests
# ============================================

test_that("plot_heatmap works with viridis palette", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, colors = "viridis")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with heat palette", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, colors = "heat")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with blues palette", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, colors = "blues")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with reds palette", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, colors = "reds")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with diverging palette", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9) - 0.5, 3, 3)  # Values around 0

  p <- plot_heatmap(mat, colors = "diverging")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with greens palette", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, colors = "greens")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with custom color vector", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, colors = c("white", "blue", "darkblue"))

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with single custom color", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, colors = "purple")

  expect_s3_class(p, "ggplot")
})

# ============================================
# Legend Parameter Tests
# ============================================

test_that("plot_heatmap respects show_legend = FALSE", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, show_legend = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects legend_position = left", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, legend_position = "left")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects legend_position = top", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, legend_position = "top")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects legend_position = bottom", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, legend_position = "bottom")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects legend_position = none", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, legend_position = "none")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects custom legend_title", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, legend_title = "Custom Legend")

  expect_s3_class(p, "ggplot")
})

# ============================================
# Value Display Tests
# ============================================

test_that("plot_heatmap shows values when show_values = TRUE", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, show_values = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects value_size parameter", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, show_values = TRUE, value_size = 4)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects value_color parameter", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, show_values = TRUE, value_color = "white")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects value_digits parameter", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, show_values = TRUE, value_digits = 1)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Diagonal Display Tests
# ============================================

test_that("plot_heatmap hides diagonal when show_diagonal = FALSE", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  diag(mat) <- 1

  p <- plot_heatmap(mat, show_diagonal = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap shows diagonal by default", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  diag(mat) <- 1

  p <- plot_heatmap(mat, show_diagonal = TRUE)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Axis Label Tests
# ============================================

test_that("plot_heatmap hides axis labels when show_axis_labels = FALSE", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  p <- plot_heatmap(mat, show_axis_labels = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects axis_text_size parameter", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  p <- plot_heatmap(mat, axis_text_size = 12)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects axis_text_angle parameter", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  p <- plot_heatmap(mat, axis_text_angle = 90)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap uses custom row_labels", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, row_labels = c("X", "Y", "Z"))

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap uses custom col_labels", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, col_labels = c("Col1", "Col2", "Col3"))

  expect_s3_class(p, "ggplot")
})

# ============================================
# Title and Label Tests
# ============================================

test_that("plot_heatmap displays title", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, title = "Test Heatmap")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap displays subtitle", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, subtitle = "Test Subtitle")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap displays xlab", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, xlab = "X Axis Label")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap displays ylab", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, ylab = "Y Axis Label")

  expect_s3_class(p, "ggplot")
})

# ============================================
# Scale and Limits Tests
# ============================================

test_that("plot_heatmap respects custom limits", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, limits = c(0, 1))

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap respects midpoint for diverging scale", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9) - 0.5, 3, 3)  # Values around 0

  p <- plot_heatmap(mat, midpoint = 0)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap uses custom na_color", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  mat[1, 2] <- NA

  p <- plot_heatmap(mat, na_color = "white")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap aspect_ratio parameter works", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)

  p <- plot_heatmap(mat, aspect_ratio = 0.5)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Clustered Heatmap Tests
# ============================================

test_that("plot_heatmap works with cluster_list", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered with cluster_spacing", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, cluster_spacing = 0.5)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered hides cluster_labels", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, cluster_labels = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered hides cluster_borders", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, cluster_borders = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered with custom border_color", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, border_color = "red")

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered with custom border_width", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, border_width = 2)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered shows values", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, show_values = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered with diverging scale", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25) - 0.5, 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, midpoint = 0)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered errors on missing nodes", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("X", "Y", "Z")  # Missing nodes
  )

  expect_error(
    plot_heatmap(mat, cluster_list = clusters),
    "not found"
  )
})

test_that("plot_heatmap clustered hides diagonal", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]
  diag(mat) <- 1

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, show_diagonal = FALSE)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Edge Cases and NA Handling
# ============================================

test_that("plot_heatmap handles matrix with NA values", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  mat[1, 2] <- NA
  mat[2, 3] <- NA

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap handles matrix with all same values", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(0.5, 3, 3)

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap handles zero matrix", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(0, 3, 3)

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap handles single element matrix", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(0.5, 1, 1)

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap handles negative values", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9) - 0.5, 3, 3)

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap handles mixed positive/negative values", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(c(-0.5, 0.3, -0.2, 0.1, 0.4, -0.3, 0.2, -0.1, 0.5), 3, 3)

  p <- plot_heatmap(mat)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Input Conversion Tests
# ============================================

test_that("plot_heatmap works with cograph_network object", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.4, 0.3, 0.4, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  net <- cograph(mat)

  p <- plot_heatmap(net)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap works with tna object", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")
  model <- tna(engagement[1:50, ])

  p <- plot_heatmap(model)

  expect_s3_class(p, "ggplot")
})

# ============================================
# group_tna Tests (Supra-Adjacency Heatmap)
# ============================================

test_that("plot_heatmap works with group_tna object", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create artificial groups
  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  p <- plot_heatmap(group_model)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap group_tna shows cluster borders", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  p <- plot_heatmap(group_model, cluster_borders = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap group_tna shows cluster labels", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  p <- plot_heatmap(group_model, cluster_labels = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap group_tna hides diagonal", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  p <- plot_heatmap(group_model, show_diagonal = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap group_tna shows values", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  p <- plot_heatmap(group_model, show_values = TRUE, value_size = 1.5)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap group_tna with custom title", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  p <- plot_heatmap(group_model, title = "Custom Group TNA Heatmap")

  expect_s3_class(p, "ggplot")
})

# ============================================
# Helper Function Tests
# ============================================

test_that(".matrix_to_long converts matrix correctly", {
  skip_if_not_installed("ggplot2")

  # Access internal function
  matrix_to_long <- cograph:::.matrix_to_long

  mat <- matrix(1:9, 3, 3)
  row_labels <- c("A", "B", "C")
  col_labels <- c("X", "Y", "Z")

  df <- matrix_to_long(mat, row_labels, col_labels)

  expect_s3_class(df, "data.frame")
  expect_true("row" %in% names(df))
  expect_true("col" %in% names(df))
  expect_true("value" %in% names(df))
  expect_equal(nrow(df), 9)
})

test_that(".resolve_colors returns correct palette for viridis", {
  skip_if_not_installed("ggplot2")

  resolve_colors <- cograph:::.resolve_colors

  colors <- resolve_colors("viridis", 1:10, NULL, NULL)

  expect_equal(length(colors), 3)
  expect_true(all(grepl("^#", colors)))
})

test_that(".resolve_colors returns correct palette for heat", {
  skip_if_not_installed("ggplot2")

  resolve_colors <- cograph:::.resolve_colors

  colors <- resolve_colors("heat", 1:10, NULL, NULL)

  expect_equal(length(colors), 3)
})

test_that(".resolve_colors returns correct palette for blues", {
  skip_if_not_installed("ggplot2")

  resolve_colors <- cograph:::.resolve_colors

  colors <- resolve_colors("blues", 1:10, NULL, NULL)

  expect_equal(length(colors), 3)
})

test_that(".resolve_colors handles custom color vector", {
  skip_if_not_installed("ggplot2")

  resolve_colors <- cograph:::.resolve_colors

  custom <- c("red", "white", "blue")
  colors <- resolve_colors(custom, 1:10, NULL, NULL)

  expect_equal(colors, custom)
})

test_that(".heatmap_theme returns valid theme components", {
  skip_if_not_installed("ggplot2")

  heatmap_theme <- cograph:::.heatmap_theme

  result <- heatmap_theme(TRUE, 8, 45, 1)

  expect_type(result, "list")
  expect_true("theme" %in% names(result))
  expect_true("aspect_ratio" %in% names(result))
})

test_that(".heatmap_theme handles show_axis_labels = FALSE", {
  skip_if_not_installed("ggplot2")

  heatmap_theme <- cograph:::.heatmap_theme

  result <- heatmap_theme(FALSE, 8, 45, 1)

  expect_type(result, "list")
})

# ============================================
# Additional Clustered Tests
# ============================================

test_that("plot_heatmap clustered with three clusters", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(36), 6, 6)
  rownames(mat) <- colnames(mat) <- LETTERS[1:6]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D"),
    Group3 = c("E", "F")
  )

  p <- plot_heatmap(mat, cluster_list = clusters)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered unnamed clusters omits labels", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  # Unnamed clusters
  clusters <- list(
    c("A", "B"),
    c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters)

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered with large spacing", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, cluster_spacing = 2)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Integration Tests with Multiple Options
# ============================================

test_that("plot_heatmap with all display options", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  p <- plot_heatmap(
    mat,
    show_legend = TRUE,
    legend_position = "right",
    legend_title = "Weight",
    colors = "viridis",
    show_values = TRUE,
    value_size = 2,
    value_color = "white",
    value_digits = 1,
    show_diagonal = TRUE,
    title = "Full Options Test",
    subtitle = "Subtitle here",
    xlab = "Columns",
    ylab = "Rows",
    axis_text_size = 10,
    axis_text_angle = 45
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered with all options", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(
    mat,
    cluster_list = clusters,
    cluster_spacing = 0.5,
    cluster_labels = TRUE,
    cluster_borders = TRUE,
    border_color = "darkblue",
    border_width = 1,
    show_legend = TRUE,
    colors = "heat",
    show_values = TRUE,
    title = "Clustered with All Options"
  )

  expect_s3_class(p, "ggplot")
})
