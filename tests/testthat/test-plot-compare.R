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

test_that("plot_compare() is a non-deprecated alias for plot_difference()", {
  m1 <- matrix(c(0, 0.5, 0.5, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  m2 <- matrix(c(0, 0.3, 0.7, 0), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
  # tna::plot_compare() delegates to cograph::plot_compare() by name, so the
  # alias must NOT warn and must return the same difference network.
  expect_no_warning(res <- with_temp_png(cograph::plot_compare(m1, m2)))
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

test_that("plot_difference shows small difference edges (minimum defaults to 0)", {
  cap <- NULL
  orig <- get("splot", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("splot", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("splot", function(...) { cap <<- list(...); invisible(NULL) },
                    ns = "cograph")
  lab <- c("A", "B")
  m1 <- matrix(c(0, 0.505, 0.5, 0), 2, 2, dimnames = list(lab, lab))
  m2 <- matrix(c(0, 0.5, 0.5, 0), 2, 2, dimnames = list(lab, lab))
  cograph::plot_difference(m1, m2)
  expect_equal(cap$minimum, 0)                       # tiny diffs not hidden
  cograph::plot_difference(m1, m2, minimum = 0.02)
  expect_equal(cap$minimum, 0.02)                    # user value still wins
})

test_that("plot_difference warns and ignores y when difference = TRUE", {
  lab <- c("A", "B")
  d  <- matrix(c(0, 0.4, -0.3, 0), 2, 2, dimnames = list(lab, lab))
  m2 <- matrix(c(0, 0.5,  0.5,  0), 2, 2, dimnames = list(lab, lab))
  expect_warning(
    res <- with_temp_png(cograph::plot_difference(d, m2, difference = TRUE)),
    "ignored"
  )
  expect_equal(res$weights, d)                       # x used as-is, not x - y
})

test_that("splot routes a netdifference to plot_difference (directed, both triangles)", {
  lab <- c("A", "B", "C")
  # asymmetric difference: A->B = 5, B->A = -1 — an undirected rendering
  # would collapse the pair and drop one of them
  d <- matrix(0, 3, 3, dimnames = list(lab, lab))
  d["A", "B"] <- 5; d["B", "A"] <- -1; d["B", "C"] <- 2
  nd <- structure(
    list(weights = d, difference_matrix = d, directed = TRUE,
         nodes = data.frame(id = 1:3, label = lab, name = lab)),
    class = c("netdifference", "netobject", "cograph_network")
  )

  res <- with_temp_png(cograph::splot(nd))
  # plot_difference's return contract: list carrying the difference matrix
  expect_equal(res$weights, d, ignore_attr = TRUE)

  # the routing must actually reach plot_difference (falling through to
  # splot.netobject would also return $weights, so assert the call itself),
  # and user args like minimum must flow through it
  cap <- NULL
  orig <- get("plot_difference", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("plot_difference", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("plot_difference", function(x, ...) {
    cap <<- list(...); invisible(NULL)
  }, ns = "cograph")
  cograph::splot(nd, minimum = 3)
  expect_false(is.null(cap))          # plot_difference was invoked
  expect_equal(cap$minimum, 3)
})

test_that("splot does NOT route net_permutation-family netdifference to plot_difference", {
  # net_bayes carries netdifference + net_permutation; it must reach
  # splot.net_permutation (whose caller aligns per-edge CI arrays), not the
  # difference renderer
  lab <- c("A", "B", "C")
  d <- matrix(0, 3, 3, dimnames = list(lab, lab))
  d["A", "B"] <- 0.4; d["B", "C"] <- -0.2
  nb <- structure(
    list(diff = d, diff_sig = d,
         p_values = matrix(0.01, 3, 3, dimnames = list(lab, lab)),
         effect_size = matrix(1, 3, 3, dimnames = list(lab, lab)),
         alpha = 0.05,
         x = list(directed = TRUE, nodes = data.frame(label = lab))),
    class = c("net_bayes", "netdifference", "net_permutation")
  )

  cap_diff <- FALSE
  orig <- get("plot_difference", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("plot_difference", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("plot_difference", function(...) { cap_diff <<- TRUE; invisible(NULL) },
                    ns = "cograph")
  with_temp_png(cograph::splot(nb))
  expect_false(cap_diff)
})

test_that("plot_difference prefers the netdifference display matrix ($weights)", {
  lab <- c("A", "B")
  full <- matrix(c(0, 0.4, -0.3, 0), 2, 2, dimnames = list(lab, lab))
  disp <- matrix(c(0, 0.4,    0, 0), 2, 2, dimnames = list(lab, lab))
  nd <- structure(list(weights = disp, difference_matrix = full, directed = TRUE),
                  class = c("netdifference", "netobject", "cograph_network"))
  res <- with_temp_png(cograph::plot_difference(nd))
  expect_equal(res$weights, disp, ignore_attr = TRUE)
})

test_that("splot.net_permutation title survives a title_size-only call ($ partial match)", {
  # `args$title` on a list holding only title_size partially matches it
  # (0.82), silently skipping the title default — must use exact indexing
  lab <- c("A", "B", "C")
  d <- matrix(0, 3, 3, dimnames = list(lab, lab))
  d["A", "B"] <- 0.4
  perm <- structure(
    list(diff = d, diff_sig = d,
         p_values = matrix(0.01, 3, 3, dimnames = list(lab, lab)),
         effect_size = matrix(1, 3, 3, dimnames = list(lab, lab)),
         alpha = 0.05,
         x = list(directed = TRUE, nodes = data.frame(label = lab))),
    class = c("net_permutation")
  )

  cap <- NULL
  orig <- get("splot", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("splot", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("splot", function(x, ...) {
    if (inherits(x, "net_permutation")) return(orig(x, ...))
    cap <<- list(...); invisible(NULL)
  }, ns = "cograph")

  cograph::splot(perm, title_size = 0.9)
  expect_identical(cap[["title"]], "Permutation Test: Significant Differences")

  cograph::splot(perm, title = "MY TITLE", title_size = 0.9)
  expect_identical(cap[["title"]], "MY TITLE")
})

test_that("{p_diff} placeholder renders the probability of the difference", {
  lab <- build_edge_labels_from_template(
    template = "{est} (P={p_diff})",
    weights = c(0.4, -0.2),
    p_diff = c(0.998, 0.51),
    digits = 2, p_digits = 2, n = 2
  )
  expect_identical(lab, c("0.40 (P=1.00)", "-0.20 (P=0.51)"))

  # matrix form: splot indexes it at the drawn edges (survives minimum filter)
  nodes <- c("A", "B", "C")
  d <- matrix(0, 3, 3, dimnames = list(nodes, nodes))
  d["A", "B"] <- 5; d["B", "C"] <- 2
  pd <- matrix(NA_real_, 3, 3, dimnames = list(nodes, nodes))
  pd["A", "B"] <- 0.99; pd["B", "C"] <- 0.87
  nd <- structure(
    list(weights = d, difference_matrix = d, p_difference = pd,
         directed = TRUE),
    class = c("netdifference", "netobject", "cograph_network")
  )
  expect_silent(with_temp_png(
    cograph::splot(nd, minimum = 3,
                   edge_label_template = "{est} (P={p_diff})")
  ))
})

test_that("edge_betweenness netobjects get TNA-family styling (directed, not psych)", {
  lab <- c("A", "B", "C")
  w <- matrix(0, 3, 3, dimnames = list(lab, lab))
  w["A", "B"] <- 3; w["B", "C"] <- 3; w["C", "A"] <- 3   # directed cycle
  eb <- structure(
    list(weights = w, method = "edge_betweenness", directed = TRUE,
         nodes = data.frame(id = 1:3, label = lab, name = lab)),
    class = c("net_edge_betweenness", "netobject", "cograph_network")
  )
  cap <- NULL
  orig <- get("splot", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("splot", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("splot", function(x, ...) {
    if (is.list(x) && !is.matrix(x)) return(orig(x, ...))
    cap <<- list(...); invisible(NULL)
  }, ns = "cograph")
  cograph::splot(eb)
  expect_true(isTRUE(cap$tna_styling))    # not psych_styling
  expect_null(cap$psych_styling)
})

test_that("meta$splot routing works without any recognized class", {
  # The fixture must NOT carry netdifference/netobject — those classes would
  # reach plot_difference through the ordinary inherits() cascade anyway, and
  # the test could never detect a broken metadata contract. A producer-only
  # class proves the routing came from meta$splot alone.
  lab <- c("A", "B")
  d <- matrix(c(0, 0.4, -0.2, 0), 2, 2, dimnames = list(lab, lab))
  nd <- structure(
    list(weights = d, difference_matrix = d, directed = TRUE,
         meta = list(splot = list(renderer = "difference",
                                  defaults = list(minimum = 0)))),
    class = "some_producer_difference"
  )
  cap_diff <- FALSE
  orig <- get("plot_difference", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("plot_difference", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("plot_difference", function(...) { cap_diff <<- TRUE; invisible(NULL) },
                    ns = "cograph")
  cograph::splot(nd)
  expect_true(cap_diff)
})

test_that("edge_betweenness netobjects style by direction (undirected stays psych)", {
  lab <- c("A", "B", "C")
  sym <- matrix(c(0, 1, 1,
                  1, 0, 1,
                  1, 1, 0), 3, 3, dimnames = list(lab, lab))
  ebu <- structure(
    list(weights = sym, method = "edge_betweenness", directed = FALSE,
         nodes = data.frame(id = 1:3, label = lab, name = lab)),
    class = c("net_edge_betweenness", "netobject", "cograph_network")
  )
  cap <- NULL
  orig <- get("splot", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("splot", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("splot", function(x, ...) {
    if (is.list(x) && !is.matrix(x)) return(orig(x, ...))
    cap <<- list(...); invisible(NULL)
  }, ns = "cograph")
  cograph::splot(ebu)
  expect_true(isTRUE(cap$psych_styling))
  expect_null(cap$tna_styling)
})

test_that("plot_permutation() itself defaults title/layout via exact indexing", {
  # tna_permutation shape: matrices + stats live under $edges. (The flat
  # diff/diff_sig shape belongs to splot.net_permutation, not this renderer.)
  diffs <- matrix(c(0, .15, -.1, -.2, 0, .05, .1, -.05, 0), 3, 3,
                  dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  diffs_sig <- diffs
  diffs_sig[abs(diffs) < 0.1] <- 0
  perm <- list(edges = list(
    diffs_true = diffs, diffs_sig = diffs_sig,
    stats = data.frame(
      edge_name   = c("A -> B", "A -> C", "B -> A", "B -> C", "C -> A", "C -> B"),
      diff_true   = c(.15, -.1, -.2, .05, .1, -.05),
      effect_size = c(2.1, -1.5, -2.8, .4, 1.2, -.3),
      p_value     = c(.01, .04, .001, .3, .02, .5)
    )
  ))
  attr(perm, "level") <- 0.05
  attr(perm, "labels") <- c("A", "B", "C")
  class(perm) <- c("tna_permutation", "list")

  cap <- NULL
  orig <- get("splot", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("splot", orig, ns = "cograph"), add = TRUE)
  assignInNamespace("splot", function(x, ...) {
    cap <<- list(...); invisible(NULL)
  }, ns = "cograph")

  # title_size alone must not partial-match away the default title
  plot_permutation(perm, title_size = 0.9)
  expect_identical(cap[["title"]], "Permutation Test: Significant Differences")
  # layout_scale alone must not partial-match away the default layout
  plot_permutation(perm, layout_scale = 0.8)
  expect_identical(cap[["layout"]], "oval")
})

test_that("edge_label_p_diff matrix aligns by dimnames in any node order", {
  lab <- c("A", "B", "C")
  d <- matrix(0, 3, 3, dimnames = list(lab, lab))
  d["A", "B"] <- 5; d["B", "C"] <- 2
  rl <- rev(lab)
  pd <- matrix(NA_real_, 3, 3, dimnames = list(rl, rl))
  pd["A", "B"] <- 0.99; pd["B", "C"] <- 0.87

  seen <- NULL
  orig <- get("build_edge_labels_from_template", envir = asNamespace("cograph"))
  on.exit(assignInNamespace("build_edge_labels_from_template", orig,
                            ns = "cograph"), add = TRUE)
  assignInNamespace("build_edge_labels_from_template", function(...) {
    a <- list(...); seen <<- a$p_diff; orig(...)
  }, ns = "cograph")

  with_temp_png(
    cograph::splot(d, edge_label_template = "{est} (P={p_diff})",
                   edge_label_p_diff = pd)
  )
  # per-edge assignment, not just the value set: the edge weights identify
  # which edge each p_diff was attached to (A->B = 5 gets .99, B->C = 2 gets .87)
  expect_equal(length(seen), 2L)
  expect_identical(seen[[1L]], 0.99)  # first drawn edge is A->B
  expect_identical(seen[[2L]], 0.87)  # second drawn edge is B->C
})
