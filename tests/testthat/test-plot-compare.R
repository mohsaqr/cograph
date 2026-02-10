# Tests for plot_compare() and related comparison functions
# Network difference visualization

# ============================================
# Basic plot_compare() Tests
# ============================================

test_that("plot_compare works with basic matrices", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.7, 0), 2, 2)

  expect_no_error(with_temp_png(plot_compare(mat1, mat2)))
})

test_that("plot_compare returns invisibly", {
  mat1 <- matrix(c(0, 0.5, 0.3, 0), 2, 2)
  mat2 <- matrix(c(0, 0.2, 0.4, 0), 2, 2)

  result <- with_temp_png(plot_compare(mat1, mat2))

  expect_type(result, "list")
  expect_true("weights" %in% names(result))
  expect_true(is.matrix(result$weights))
})

test_that("plot_compare computes correct difference", {
  mat1 <- matrix(c(0, 0.6, 0.4, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.5, 0), 2, 2)

  result <- with_temp_png(plot_compare(mat1, mat2))

  expected_diff <- mat1 - mat2
  expect_equal(result$weights, expected_diff)
})

test_that("plot_compare works with labeled matrices", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2,
                 dimnames = list(c("A", "B"), c("A", "B")))
  mat2 <- matrix(c(0, 0.3, 0.7, 0), 2, 2,
                 dimnames = list(c("A", "B"), c("A", "B")))

  expect_no_error(with_temp_png(plot_compare(mat1, mat2)))
})

test_that("plot_compare preserves labels in output", {
  mat1 <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  mat2 <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  mat1[1, 2] <- 0.5
  mat2[1, 2] <- 0.3

  result <- with_temp_png(plot_compare(mat1, mat2))

  expect_equal(rownames(result$weights), LETTERS[1:3])
  expect_equal(colnames(result$weights), LETTERS[1:3])
})

# ============================================
# Color Parameter Tests
# ============================================

test_that("plot_compare respects pos_color parameter", {
  mat1 <- matrix(c(0, 0.6, 0.3, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.6, 0), 2, 2)

  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2, pos_color = "#00FF00")
  ))
})

test_that("plot_compare respects neg_color parameter", {
  mat1 <- matrix(c(0, 0.3, 0.6, 0), 2, 2)
  mat2 <- matrix(c(0, 0.6, 0.3, 0), 2, 2)

  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2, neg_color = "#FF0000")
  ))
})

test_that("plot_compare respects both color parameters", {
  mat1 <- matrix(c(0, 0.5, 0.3, 0), 2, 2)
  mat2 <- matrix(c(0, 0.2, 0.6, 0), 2, 2)

  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2,
                 pos_color = "darkgreen",
                 neg_color = "darkred")
  ))
})

# ============================================
# Title Parameter Tests
# ============================================

test_that("plot_compare uses custom title", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2, title = "Custom Title")
  ))
})

test_that("plot_compare auto-generates title", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  # Default title should be generated
  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2)
  ))
})

# ============================================
# Labels Parameter Tests
# ============================================

test_that("plot_compare respects custom labels", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2, labels = c("Node1", "Node2"))
  ))
})

# ============================================
# Input Validation Tests
# ============================================

test_that("plot_compare errors when y is missing", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  expect_error(
    with_temp_png(plot_compare(mat1)),
    "y is required"
  )
})

test_that("plot_compare errors on dimension mismatch", {
  mat1 <- matrix(0, 2, 2)
  mat2 <- matrix(0, 3, 3)

  expect_error(
    with_temp_png(plot_compare(mat1, mat2)),
    "same dimensions"
  )
})

test_that("plot_compare errors on label mismatch", {
  mat1 <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  mat2 <- matrix(0, 2, 2, dimnames = list(c("X", "Y"), c("X", "Y")))
  mat1[1, 2] <- 0.5
  mat2[1, 2] <- 0.3

  expect_error(
    with_temp_png(plot_compare(mat1, mat2)),
    "same node labels"
  )
})

# ============================================
# Inits/Donut Display Tests
# ============================================

test_that("plot_compare works with inits_x and inits_y", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  inits1 <- c(0.6, 0.4)
  inits2 <- c(0.4, 0.6)

  result <- with_temp_png(
    plot_compare(mat1, mat2, inits_x = inits1, inits_y = inits2)
  )

  expect_equal(result$inits, inits1 - inits2)
})

test_that("plot_compare show_inits = FALSE hides donuts", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  inits1 <- c(0.6, 0.4)
  inits2 <- c(0.4, 0.6)

  # Should not error even with inits provided
  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2,
                 inits_x = inits1,
                 inits_y = inits2,
                 show_inits = FALSE)
  ))
})

test_that("plot_compare donut_inner_ratio is respected", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  inits1 <- c(0.6, 0.4)
  inits2 <- c(0.4, 0.6)

  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2,
                 inits_x = inits1,
                 inits_y = inits2,
                 donut_inner_ratio = 0.5)
  ))
})

test_that("plot_compare warns on inits length mismatch", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  inits1 <- c(0.6, 0.4, 0.5)  # Wrong length
  inits2 <- c(0.4, 0.6)

  expect_warning(
    with_temp_png(
      plot_compare(mat1, mat2, inits_x = inits1, inits_y = inits2)
    ),
    "length doesn't match"
  )
})

# ============================================
# List Input Tests
# ============================================

test_that("plot_compare works with list of matrices", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  net_list <- list(first = mat1, second = mat2)

  expect_no_error(with_temp_png(
    plot_compare(net_list)
  ))
})

test_that("plot_compare list with i and j parameters", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  mat3 <- matrix(c(0, 0.7, 0.7, 0), 2, 2)

  net_list <- list(A = mat1, B = mat2, C = mat3)

  # Compare specific elements
  expect_no_error(with_temp_png(
    plot_compare(net_list, i = 1, j = 3)
  ))

  expect_no_error(with_temp_png(
    plot_compare(net_list, i = "A", j = "C")
  ))
})

test_that("plot_compare errors on single-element list", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  net_list <- list(only = mat1)

  expect_error(
    with_temp_png(plot_compare(net_list)),
    "at least 2"
  )
})

# ============================================
# TNA Integration Tests
# ============================================

test_that("plot_compare works with tna objects", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create simple tna models from different subsets
  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  expect_no_error(with_temp_png(
    plot_compare(model1, model2)
  ))
})

test_that("plot_compare auto-extracts inits from tna objects", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  result <- with_temp_png(
    plot_compare(model1, model2)
  )

  # Should have extracted inits difference
  expect_true(!is.null(result$inits))
})

test_that("plot_compare works with group_tna objects", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create artificial groups
  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  # Should compare the two groups
  expect_no_error(with_temp_png(
    plot_compare(group_model)
  ))
})

test_that("plot_compare group_tna with specific i, j", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create artificial groups
  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  expect_no_error(with_temp_png(
    plot_compare(group_model, i = 1, j = 2)
  ))
})

test_that("plot_compare errors on group_tna with < 2 groups", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Single group
  single_group <- group_tna(
    engagement,
    group = rep("A", nrow(engagement))
  )

  expect_error(
    with_temp_png(plot_compare(single_group)),
    "at least 2 groups"
  )
})

# ============================================
# Edge Case Tests
# ============================================

test_that("plot_compare handles identical matrices", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  result <- with_temp_png(plot_compare(mat, mat))

  # All differences should be zero
  expect_true(all(result$weights == 0))
})

test_that("plot_compare handles zero matrices", {
  mat1 <- matrix(0, 3, 3)
  mat2 <- matrix(0, 3, 3)

  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2)
  ))
})

test_that("plot_compare handles negative differences", {
  mat1 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  mat2 <- matrix(c(0, 0.7, 0.7, 0), 2, 2)

  result <- with_temp_png(plot_compare(mat1, mat2))

  # Differences should be negative
  expect_true(all(result$weights[result$weights != 0] < 0))
})

test_that("plot_compare handles larger networks", {
  skip_on_cran()

  n <- 10
  set.seed(42)
  mat1 <- matrix(runif(n * n), n, n)
  mat2 <- matrix(runif(n * n), n, n)
  diag(mat1) <- 0
  diag(mat2) <- 0

  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2),
    width = 400, height = 400
  ))
})

# ============================================
# plot_comparison_heatmap() Tests
# ============================================

test_that("plot_comparison_heatmap works with basic matrices", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(runif(9), 3, 3)
  mat2 <- matrix(runif(9), 3, 3)

  p <- plot_comparison_heatmap(mat1, mat2)

  expect_s3_class(p, "ggplot")
})

test_that("plot_comparison_heatmap type = 'difference'", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  p <- plot_comparison_heatmap(mat1, mat2, type = "difference")

  expect_s3_class(p, "ggplot")
})

test_that("plot_comparison_heatmap type = 'x'", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  p <- plot_comparison_heatmap(mat1, type = "x")

  expect_s3_class(p, "ggplot")
})

test_that("plot_comparison_heatmap type = 'y'", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  p <- plot_comparison_heatmap(mat1, mat2, type = "y")

  expect_s3_class(p, "ggplot")
})

test_that("plot_comparison_heatmap errors when y required but missing", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  expect_error(
    plot_comparison_heatmap(mat1, type = "difference"),
    "y is required"
  )

  expect_error(
    plot_comparison_heatmap(mat1, type = "y"),
    "y is required"
  )
})

test_that("plot_comparison_heatmap respects color parameters", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  p <- plot_comparison_heatmap(mat1, mat2,
                               low_color = "green",
                               mid_color = "yellow",
                               high_color = "purple")

  expect_s3_class(p, "ggplot")
})

test_that("plot_comparison_heatmap show_values = TRUE", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  p <- plot_comparison_heatmap(mat1, mat2, show_values = TRUE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_comparison_heatmap respects custom limits", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  p <- plot_comparison_heatmap(mat1, mat2, limits = c(-1, 1))

  expect_s3_class(p, "ggplot")
})

test_that("plot_comparison_heatmap respects name_x and name_y", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  p <- plot_comparison_heatmap(mat1, mat2,
                               name_x = "Model A",
                               name_y = "Model B")

  expect_s3_class(p, "ggplot")
})

test_that("plot_comparison_heatmap with labeled matrices", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2,
                 dimnames = list(c("A", "B"), c("A", "B")))
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2,
                 dimnames = list(c("A", "B"), c("A", "B")))

  p <- plot_comparison_heatmap(mat1, mat2)

  expect_s3_class(p, "ggplot")
})

# ============================================
# Passthrough Argument Tests
# ============================================

test_that("plot_compare passes additional arguments to splot", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  # Test layout passthrough
  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2, layout = "circle")
  ))

  # Test node_size passthrough
  expect_no_error(with_temp_png(
    plot_compare(mat1, mat2, node_size = 10)
  ))
})
