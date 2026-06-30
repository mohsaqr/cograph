# Tests for plot_difference() and related comparison functions
# Network difference visualization

# ============================================
# Basic plot_difference() Tests
# ============================================

test_that("plot_difference works with basic matrices", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.7, 0), 2, 2)

  expect_no_error(with_temp_png(cograph::plot_difference(mat1, mat2)))
})

test_that("plot_difference returns invisibly", {
  mat1 <- matrix(c(0, 0.5, 0.3, 0), 2, 2)
  mat2 <- matrix(c(0, 0.2, 0.4, 0), 2, 2)

  result <- with_temp_png(cograph::plot_difference(mat1, mat2))

  expect_type(result, "list")
  expect_true("weights" %in% names(result))
  expect_true(is.matrix(result$weights))
})

test_that("plot_difference computes correct difference", {
  mat1 <- matrix(c(0, 0.6, 0.4, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.5, 0), 2, 2)

  result <- with_temp_png(cograph::plot_difference(mat1, mat2))

  expected_diff <- mat1 - mat2
  expect_equal(result$weights, expected_diff)
})

test_that("plot_difference works with labeled matrices", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2,
                 dimnames = list(c("A", "B"), c("A", "B")))
  mat2 <- matrix(c(0, 0.3, 0.7, 0), 2, 2,
                 dimnames = list(c("A", "B"), c("A", "B")))

  expect_no_error(with_temp_png(cograph::plot_difference(mat1, mat2)))
})

test_that("plot_difference preserves labels in output", {
  mat1 <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  mat2 <- matrix(0, 3, 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  mat1[1, 2] <- 0.5
  mat2[1, 2] <- 0.3

  result <- with_temp_png(cograph::plot_difference(mat1, mat2))

  expect_equal(rownames(result$weights), LETTERS[1:3])
  expect_equal(colnames(result$weights), LETTERS[1:3])
})

# ============================================
# Color Parameter Tests
# ============================================

test_that("plot_difference respects pos_color parameter", {
  mat1 <- matrix(c(0, 0.6, 0.3, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.6, 0), 2, 2)

  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2, pos_color = "#00FF00")
  ))
})

test_that("plot_difference respects neg_color parameter", {
  mat1 <- matrix(c(0, 0.3, 0.6, 0), 2, 2)
  mat2 <- matrix(c(0, 0.6, 0.3, 0), 2, 2)

  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2, neg_color = "#FF0000")
  ))
})

test_that("plot_difference respects both color parameters", {
  mat1 <- matrix(c(0, 0.5, 0.3, 0), 2, 2)
  mat2 <- matrix(c(0, 0.2, 0.6, 0), 2, 2)

  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2,
                 pos_color = "darkgreen",
                 neg_color = "darkred")
  ))
})

# ============================================
# Title Parameter Tests
# ============================================

test_that("plot_difference uses custom title", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2, title = "Custom Title")
  ))
})

test_that("plot_difference auto-generates title", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  # Default title should be generated
  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2)
  ))
})

# ============================================
# Labels Parameter Tests
# ============================================

test_that("plot_difference respects custom labels", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2, labels = c("Node1", "Node2"))
  ))
})

# ============================================
# Input Validation Tests
# ============================================

test_that("plot_difference errors when y is missing", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  expect_error(
    with_temp_png(cograph::plot_difference(mat1)),
    "y is required"
  )
})

test_that("plot_difference errors on dimension mismatch", {
  mat1 <- matrix(0, 2, 2)
  mat2 <- matrix(0, 3, 3)

  expect_error(
    with_temp_png(cograph::plot_difference(mat1, mat2)),
    "same dimensions"
  )
})

test_that("plot_difference errors on label mismatch", {
  mat1 <- matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  mat2 <- matrix(0, 2, 2, dimnames = list(c("X", "Y"), c("X", "Y")))
  mat1[1, 2] <- 0.5
  mat2[1, 2] <- 0.3

  expect_error(
    with_temp_png(cograph::plot_difference(mat1, mat2)),
    "same node labels"
  )
})

# ============================================
# Inits/Donut Display Tests
# ============================================

test_that("plot_difference works with inits_x and inits_y", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  inits1 <- c(0.6, 0.4)
  inits2 <- c(0.4, 0.6)

  result <- with_temp_png(
    cograph::plot_difference(mat1, mat2, inits_x = inits1, inits_y = inits2)
  )

  expect_equal(result$inits, inits1 - inits2)
})

test_that("plot_difference show_inits = FALSE hides donuts", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  inits1 <- c(0.6, 0.4)
  inits2 <- c(0.4, 0.6)

  # Should not error even with inits provided
  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2,
                 inits_x = inits1,
                 inits_y = inits2,
                 show_inits = FALSE)
  ))
})

test_that("plot_difference donut_inner_ratio is respected", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  inits1 <- c(0.6, 0.4)
  inits2 <- c(0.4, 0.6)

  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2,
                 inits_x = inits1,
                 inits_y = inits2,
                 donut_inner_ratio = 0.5)
  ))
})

test_that("plot_difference warns on inits length mismatch", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  inits1 <- c(0.6, 0.4, 0.5)  # Wrong length
  inits2 <- c(0.4, 0.6)

  expect_warning(
    with_temp_png(
      cograph::plot_difference(mat1, mat2, inits_x = inits1, inits_y = inits2)
    ),
    "length doesn't match"
  )
})

# ============================================
# List Input Tests
# ============================================

test_that("plot_difference works with list of matrices", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  net_list <- list(first = mat1, second = mat2)

  expect_no_error(with_temp_png(
    cograph::plot_difference(net_list)
  ))
})

test_that("plot_difference list with i and j parameters", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  mat3 <- matrix(c(0, 0.7, 0.7, 0), 2, 2)

  net_list <- list(A = mat1, B = mat2, C = mat3)

  # Compare specific elements
  expect_no_error(with_temp_png(
    cograph::plot_difference(net_list, i = 1, j = 3)
  ))

  expect_no_error(with_temp_png(
    cograph::plot_difference(net_list, i = "A", j = "C")
  ))
})

test_that("plot_difference errors on single-element list", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  net_list <- list(only = mat1)

  expect_error(
    with_temp_png(cograph::plot_difference(net_list)),
    "at least 2"
  )
})

# ============================================
# TNA Integration Tests
# ============================================

test_that("plot_difference works with tna objects", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create simple tna models from different subsets
  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  expect_no_error(with_temp_png(
    cograph::plot_difference(model1, model2)
  ))
})

test_that("plot_difference auto-extracts inits from tna objects", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  result <- with_temp_png(
    cograph::plot_difference(model1, model2)
  )

  # Should have extracted inits difference
  expect_true(!is.null(result$inits))
})

test_that("plot_difference works with group_tna objects", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create artificial groups
  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  # Should compare the two groups
  expect_no_error(with_temp_png(
    cograph::plot_difference(group_model)
  ))
})

test_that("plot_difference group_tna with specific i, j", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Create artificial groups
  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)

  group_model <- group_tna(engagement, group = groups)

  expect_no_error(with_temp_png(
    cograph::plot_difference(group_model, i = 1, j = 2)
  ))
})

test_that("plot_difference errors on group_tna with < 2 groups", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  # Single group
  single_group <- group_tna(
    engagement,
    group = rep("A", nrow(engagement))
  )

  expect_error(
    with_temp_png(cograph::plot_difference(single_group)),
    "at least 2 groups"
  )
})

# ============================================
# Edge Case Tests
# ============================================

test_that("plot_difference handles identical matrices", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  result <- with_temp_png(cograph::plot_difference(mat, mat))

  # All differences should be zero
  expect_true(all(result$weights == 0))
})

test_that("plot_difference handles zero matrices", {
  mat1 <- matrix(0, 3, 3)
  mat2 <- matrix(0, 3, 3)

  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2)
  ))
})

test_that("plot_difference handles negative differences", {
  mat1 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)
  mat2 <- matrix(c(0, 0.7, 0.7, 0), 2, 2)

  result <- with_temp_png(cograph::plot_difference(mat1, mat2))

  # Differences should be negative
  expect_true(all(result$weights[result$weights != 0] < 0))
})

test_that("plot_difference handles larger networks", {
  skip_on_cran()

  n <- 10
  set.seed(42)
  mat1 <- matrix(runif(n * n), n, n)
  mat2 <- matrix(runif(n * n), n, n)
  diag(mat1) <- 0
  diag(mat2) <- 0

  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2),
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

test_that("plot_difference passes additional arguments to splot", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  # Test layout passthrough
  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2, layout = "circle")
  ))

  # Test node_size passthrough
  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2, node_size = 10)
  ))
})

# ============================================
# TNA Styling Defaults Tests
# ============================================

test_that("plot_difference applies TNA styling when inputs are tna objects", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  # Should not error — TNA defaults (edge_labels, node_fill, etc.) applied

  expect_no_error(with_temp_png(
    cograph::plot_difference(model1, model2)
  ))
})

test_that("plot_difference TNA styling can be overridden by user args", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  model1 <- tna(engagement[1:100, ])
  model2 <- tna(engagement[101:200, ])

  # Override TNA defaults — should not error
  expect_no_error(with_temp_png(
    cograph::plot_difference(model1, model2,
                          edge_labels = FALSE,
                          node_fill = "gray",
                          node_size = 5)
  ))
})

test_that("plot_difference does not apply TNA styling for plain matrices", {
  mat1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  mat2 <- matrix(c(0, 0.3, 0.3, 0), 2, 2)

  # Plain matrices should work without TNA defaults
  expect_no_error(with_temp_png(
    cograph::plot_difference(mat1, mat2)
  ))
})

test_that("plot_difference group_tna applies TNA styling", {
  skip_if_no_tna()

  library(tna)
  data(engagement, package = "tna")

  n <- nrow(engagement)
  groups <- rep(c("A", "B"), length.out = n)
  group_model <- group_tna(engagement, group = groups)

  # group_tna elements are tna objects, so TNA styling should apply
  expect_no_error(with_temp_png(
    cograph::plot_difference(group_model)
  ))
})

test_that("plot_difference treats an S3 cograph_network as one network, not a list", {
  # Regression: psychnet / Nestimate netobject / as_cograph() results are S3
  # lists that inherit "cograph_network". They must compare as single networks,
  # not be misrouted into the "plain list of networks" branch.
  m1 <- matrix(runif(25), 5, 5)
  m2 <- matrix(runif(25), 5, 5)
  rownames(m1) <- colnames(m1) <- LETTERS[1:5]
  rownames(m2) <- colnames(m2) <- LETTERS[1:5]
  x <- as_cograph(m1)
  y <- as_cograph(m2)
  expect_true(is.list(x) && inherits(x, "cograph_network"))

  res <- with_temp_png(cograph::plot_difference(x, y))
  expect_true(is.list(res))
  # The difference matrix (x - y) over the shared 5 nodes is returned in $weights.
  expect_equal(dim(res$weights), c(5, 5))
  expect_equal(res$weights, to_matrix(x) - to_matrix(y), ignore_attr = TRUE)
})

test_that("plot_compare() is a deprecated alias for plot_difference()", {
  m1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  m2 <- matrix(c(0, 0.3, 0.7, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  # Emits a deprecation warning pointing at plot_difference...
  expect_warning(
    res <- with_temp_png(cograph::plot_compare(m1, m2)),
    "plot_difference"
  )
  # ...but still returns the same difference network.
  expect_equal(res$weights, m1 - m2)
})

test_that("plot_difference styles undirected diffs with the psych palette", {
  lab <- c("A", "B", "C", "D")
  m1 <- matrix(runif(16), 4, 4, dimnames = list(lab, lab)); m1 <- (m1 + t(m1)) / 2
  m2 <- matrix(runif(16), 4, 4, dimnames = list(lab, lab)); m2 <- (m2 + t(m2)) / 2
  diag(m1) <- 0; diag(m2) <- 0

  captured <- NULL
  orig <- get("splot", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("splot", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("splot",
                    function(...) { captured <<- list(...); invisible(NULL) },
                    ns = "cograph")

  cograph::plot_difference(m1, m2)
  # Okabe-Ito palette (psych styling), visible node size, sign-based edge colours.
  expect_equal(captured$node_fill[1:2], c("#E69F00", "#56B4E9"))
  expect_equal(captured$node_size, 7)
  expect_null(captured$edge_color)
  expect_false(is.null(captured$edge_positive_color))

  # User node_fill still overrides the preset.
  cograph::plot_difference(m1, m2, node_fill = "black")
  expect_equal(captured$node_fill, "black")
})

test_that("plot_difference styles directed diffs with the tna palette", {
  lab <- c("A", "B", "C", "D")
  m1 <- matrix(runif(16), 4, 4, dimnames = list(lab, lab)); diag(m1) <- 0
  m2 <- matrix(runif(16), 4, 4, dimnames = list(lab, lab)); diag(m2) <- 0

  captured <- NULL
  orig <- get("splot", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("splot", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("splot",
                    function(...) { captured <<- list(...); invisible(NULL) },
                    ns = "cograph")

  cograph::plot_difference(m1, m2)
  # Asymmetric difference -> directed -> tna palette + arrows, sign-based edges.
  expect_equal(captured$node_fill, cograph:::tna_color_palette(4))
  expect_equal(captured$node_size, 7)
  expect_null(captured$edge_color)
})

test_that("plot_difference consumes a pre-computed difference (difference = TRUE)", {
  d <- matrix(c(0, 0.4, -0.3, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  res <- with_temp_png(cograph::plot_difference(d, difference = TRUE))
  expect_equal(res$weights, d)            # x treated as the difference itself
})

test_that("plot_difference consumes a tna_comparison object", {
  skip_if_no_tna()
  data(group_regulation, package = "tna")
  n <- nrow(group_regulation)
  a <- tna::tna(group_regulation[1:(n / 2), ])
  b <- tna::tna(group_regulation[(n / 2 + 1):n, ])
  cmp <- tna::compare(a, b)
  expect_s3_class(cmp, "tna_comparison")

  res <- with_temp_png(cograph::plot_difference(cmp))
  expect_equal(res$weights, cmp$difference_matrix, ignore_attr = TRUE)
})
