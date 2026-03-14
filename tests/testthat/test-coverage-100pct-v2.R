# Coverage gap tests — brings package to 100%
# Covers uncovered lines in: disparity.R, input-tna.R, motifs-data.R,
# plot-transitions.R

# ============================================
# disparity.R — lines 69-72: disparity_filter.default
# ============================================

test_that("disparity_filter.default errors on non-matrix input", {
  expect_error(
    disparity_filter(data.frame(a = 1:3)),
    "must be a matrix"
  )
  expect_error(
    disparity_filter("not a matrix"),
    "must be a matrix"
  )
})

test_that("disparity_filter.default passes matrix to core filter", {
  # Call .default directly to cover the matrix-pass-through branch (line 72)
  mat <- matrix(c(0, 0.5, 0.3, 0.4, 0, 0.2, 0.1, 0.6, 0), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  result <- disparity_filter.default(mat, level = 0.5)
  expect_true(is.matrix(result))
  expect_true(all(result %in% c(0L, 1L)))
})

# ============================================
# disparity.R — lines 232-243: plot.tna_disparity
# ============================================

test_that("plot.tna_disparity backbone type works", {
  mat <- matrix(c(0, 0.5, 0.1, 0.3, 0, 0.4, 0.1, 0.2, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  disp <- disparity_filter(as_cograph(mat), level = 0.5)

  expect_no_error(with_temp_png(
    plot(disp, type = "backbone"), width = 400, height = 400
  ))
})

test_that("plot.tna_disparity comparison type works", {
  mat <- matrix(c(0, 0.5, 0.1, 0.3, 0, 0.4, 0.1, 0.2, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  disp <- disparity_filter(as_cograph(mat), level = 0.5)

  expect_no_error(with_temp_png(
    plot(disp, type = "comparison"), width = 800, height = 400
  ))
})

# ============================================
# disparity.R — lines 264-296: splot.tna_disparity
# ============================================

test_that("splot.tna_disparity styled mode works", {
  mat <- matrix(c(0, 0.5, 0.1, 0.3, 0, 0.4, 0.1, 0.2, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  disp <- disparity_filter(as_cograph(mat), level = 0.5)

  expect_no_error(with_temp_png(
    splot.tna_disparity(disp, show = "styled"), width = 400, height = 400
  ))
})

test_that("splot.tna_disparity backbone mode works", {
  mat <- matrix(c(0, 0.5, 0.1, 0.3, 0, 0.4, 0.1, 0.2, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  disp <- disparity_filter(as_cograph(mat), level = 0.5)

  expect_no_error(with_temp_png(
    splot.tna_disparity(disp, show = "backbone"), width = 400, height = 400
  ))
})

test_that("splot.tna_disparity full mode works", {
  mat <- matrix(c(0, 0.5, 0.1, 0.3, 0, 0.4, 0.1, 0.2, 0), 3, 3, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  disp <- disparity_filter(as_cograph(mat), level = 0.5)

  expect_no_error(with_temp_png(
    splot.tna_disparity(disp, show = "full"), width = 400, height = 400
  ))
})

test_that("splot.tna_disparity styled with zero-edge network", {
  mat <- matrix(0, 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")
  disp <- structure(
    list(
      significant = mat,
      weights_orig = mat,
      weights_filtered = mat,
      level = 0.05,
      n_edges_orig = 0,
      n_edges_filtered = 0
    ),
    class = c("tna_disparity", "list")
  )

  expect_no_error(with_temp_png(
    splot.tna_disparity(disp, show = "styled"), width = 400, height = 400
  ))
})

# ============================================
# input-tna.R — lines 130-134: .extract_tna_transitions empty data
# ============================================

test_that(".extract_tna_transitions returns empty df when no valid transitions", {
  # Matrix where all values are NA → k stays 0
  data_mat <- matrix(NA_real_, nrow = 3, ncol = 4)
  labels <- c("A", "B", "C")
  result <- cograph:::.extract_tna_transitions(data_mat, labels)
  expect_equal(nrow(result), 0)
  expect_true(all(c("from", "to", "weight", "session", "time") %in% names(result)))
})

test_that("parse_tna with matrix data where all rows have < 2 valid entries", {
  # Each row has only 1 valid entry → no transitions
  data_mat <- matrix(NA_real_, nrow = 2, ncol = 3)
  data_mat[1, 1] <- 1
  data_mat[2, 2] <- 2
  model <- list(
    weights = matrix(c(0, 0.5, 0.5, 0), 2, 2),
    labels = c("A", "B"),
    inits = c(0.5, 0.5),
    data = data_mat
  )
  class(model) <- c("tna", "list")
  parsed <- parse_tna(model)
  expect_equal(nrow(parsed$edges), 0)
})

# ============================================
# input-tna.R — line 187: .simplify_tna_edges empty edges
# ============================================

test_that(".simplify_tna_edges handles empty edge data frame", {
  empty_edges <- data.frame(
    from = integer(0), to = integer(0), weight = numeric(0),
    stringsAsFactors = FALSE
  )
  result <- cograph:::.simplify_tna_edges(empty_edges, "sum", TRUE)
  expect_equal(nrow(result), 0)
  expect_true(all(c("from", "to", "weight") %in% names(result)))
})

# ============================================
# input-tna.R — lines 195-196: .simplify_tna_edges undirected
# ============================================

test_that(".simplify_tna_edges works with undirected aggregation", {
  edges <- data.frame(
    from = c(1, 2, 1, 3),
    to = c(2, 1, 3, 1),
    weight = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  result <- cograph:::.simplify_tna_edges(edges, "sum", directed = FALSE)
  # 1->2 and 2->1 should collapse to one edge with weight 2
  expect_true(nrow(result) <= 2)
  expect_true(any(result$weight == 2))
})

test_that("parse_tna with simplify=TRUE and undirected", {
  weights <- matrix(c(0, 0.5, 0.3, 0.4, 0, 0.2, 0.1, 0.6, 0), 3, 3)
  model <- list(weights = weights, labels = c("A", "B", "C"), inits = c(0.4, 0.35, 0.25))
  class(model) <- c("tna", "list")
  parsed <- parse_tna(model, directed = FALSE, simplify = TRUE)
  expect_true(nrow(parsed$edges) > 0)
  expect_false(parsed$directed)
})

# ============================================
# motifs-data.R — line 203: empty group in windowed edgelist
# ============================================

test_that(".edgelist_to_trans_array handles empty group in windowed mode", {
  el <- data.frame(
    from = c("A", "B", "A"),
    to = c("B", "C", "C"),
    actor = c("user1", "user1", "user1"),
    stringsAsFactors = FALSE
  )
  result <- cograph:::.edgelist_to_trans_array(
    el, actor_col = "actor", window = 2, window_type = "rolling"
  )
  expect_true(is.array(result$trans))
  expect_true(length(result$labels) > 0)
})

test_that(".edgelist_to_trans_array skips empty groups in windowing", {
  # Create data with two actors, one having 0 edges after split
  # We can trigger this by having a group with no rows after the split
  el <- data.frame(
    from = c("A", "B"),
    to = c("B", "C"),
    actor = c("user1", "user2"),
    stringsAsFactors = FALSE
  )
  # With window=2, each group has only 1 edge, still should work
  result <- cograph:::.edgelist_to_trans_array(
    el, actor_col = "actor", window = 2, window_type = "tumbling"
  )
  expect_true(is.array(result$trans))
})

# ============================================
# plot-transitions.R — line 1038: value_min filtering in tracked mode
# ============================================

test_that("tracked transitions with value_min filters small flows", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    T1 = c("A", "A", "A", "B", "B", "B", "C", "C"),
    T2 = c("B", "B", "B", "A", "C", "C", "A", "B"),
    stringsAsFactors = FALSE
  )

  p <- plot_transitions(df, track_individuals = TRUE,
                        show_values = TRUE, value_min = 2)
  expect_s3_class(p, "ggplot")
})

test_that("multi-step transitions with value_min filters small flows", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, 5, 1, 3, 0, 8, 2, 1, 0), 3, 3, byrow = TRUE,
                 dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  mat2 <- matrix(c(0, 4, 2, 1, 0, 7, 3, 2, 0), 3, 3, byrow = TRUE,
                 dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  p <- plot_transitions(list(mat1, mat2),
                        show_values = TRUE, value_min = 3)
  expect_s3_class(p, "ggplot")
})

# ============================================
# plot-transitions.R — lines 1572, 1586-1587, 1592, 1610:
# bundle legend with custom text and top position
# ============================================

test_that("tracked transitions with bundle_legend custom text at top", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    T1 = rep(c("A", "B", "C"), each = 10),
    T2 = rep(c("B", "C", "A"), each = 10),
    stringsAsFactors = FALSE
  )

  p <- plot_transitions(df, track_individuals = TRUE,
                        bundle_size = 5,
                        bundle_legend = "Each line = {n} cases",
                        bundle_legend_position = "top")
  expect_s3_class(p, "ggplot")
})

test_that("tracked transitions with value_min in multi-step values", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    T1 = c("A", "A", "A", "B", "B", "B", "C", "C", "C", "A"),
    T2 = c("B", "B", "A", "C", "C", "A", "A", "B", "B", "C"),
    T3 = c("C", "C", "B", "A", "A", "B", "B", "C", "A", "A"),
    stringsAsFactors = FALSE
  )

  p <- plot_transitions(df, track_individuals = TRUE,
                        show_values = TRUE, value_min = 2)
  expect_s3_class(p, "ggplot")
})
