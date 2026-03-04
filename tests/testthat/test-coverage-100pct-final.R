# Final coverage gap tests — brings package to 100%
# Covers remaining uncovered lines across plot-chord, plot-heatmap,
# and plot-transitions

# ============================================
# plot-chord.R gaps
# ============================================

test_that("plot_chord background parameter draws rect (line 147)", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tf <- tempfile(fileext = ".png"))
  result <- plot_chord(mat, background = "gray95")
  dev.off()
  unlink(tf)

  expect_s3_class(result$segments, "data.frame")
})

test_that("plot_chord ticks = TRUE draws tick marks (lines 186-187, 552-613)", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tf <- tempfile(fileext = ".png"))
  result <- plot_chord(mat, ticks = TRUE, tick_labels = TRUE)
  dev.off()
  unlink(tf)

  expect_s3_class(result$segments, "data.frame")
})

test_that("plot_chord ticks with custom interval", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tf <- tempfile(fileext = ".png"))
  result <- plot_chord(mat, ticks = TRUE, tick_interval = 0.1,
                       tick_labels = TRUE)
  dev.off()
  unlink(tf)

  expect_s3_class(result$segments, "data.frame")
})

test_that("plot_chord ticks without labels", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tf <- tempfile(fileext = ".png"))
  result <- plot_chord(mat, ticks = TRUE, tick_labels = FALSE)
  dev.off()
  unlink(tf)

  expect_s3_class(result$segments, "data.frame")
})

test_that("plot_chord chord_color_by as vector (line 445)", {
  mat <- matrix(c(0, .3, .2,
                  .4, 0, .1,
                  .3, .2, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tf <- tempfile(fileext = ".png"))
  # Pass a vector of colors (length > 1 triggers line 445)
  result <- plot_chord(mat, chord_color_by = c("red", "blue", "green",
                                                "orange", "purple", "cyan"))
  dev.off()
  unlink(tf)

  expect_true(nrow(result$chords) > 0)
})

test_that(".chord_nice_interval handles various scales (lines 623-642)", {
  nice <- cograph:::.chord_nice_interval

  # max_val <= 0 → returns 1
  expect_equal(nice(0, matrix(0, 2, 2)), 1)
  expect_equal(nice(-1, matrix(0, 2, 2)), 1)

  # Probability scale: max_val <= 0.5
  prob_mat <- matrix(c(0, 0.1, 0.2, 0), 2, 2)
  expect_equal(nice(0.3, prob_mat), 0.05)

  # Probability scale: max_val <= 1.5
  expect_equal(nice(0.8, prob_mat), 0.1)

  # Probability scale: max_val > 1.5 (edge case)
  expect_equal(nice(1.8, prob_mat), 0.2)

  # Integer / general scale — residual < 1.5 → nice = 1
  int_mat <- matrix(c(0, 10, 20, 0), 2, 2)
  expect_equal(nice(5, int_mat), 1)

  # residual < 3.5 → nice = 2
  expect_equal(nice(15, int_mat), 2)

  # residual < 7.5 → nice = 5
  expect_equal(nice(25, int_mat), 5)

  # residual >= 7.5 → nice = 10
  expect_equal(nice(50, int_mat), 10)
})

test_that(".chord_format_tick formats values correctly (lines 649-652)", {
  fmt <- cograph:::.chord_format_tick

  # abs(v) >= 1 → format "fg"
  result <- fmt(5)
  expect_type(result, "character")
  expect_true(grepl("5", result))

  # abs(v) < 1 → format "fg" with digits = 2
  result2 <- fmt(0.15)
  expect_type(result2, "character")
  expect_true(grepl("0.15", result2))
})

test_that("plot_chord ticks on directed matrix with integer weights", {
  mat <- matrix(c(0, 30, 20,
                  40, 0, 10,
                  30, 20, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tf <- tempfile(fileext = ".png"))
  result <- plot_chord(mat, ticks = TRUE, tick_labels = TRUE, directed = TRUE)
  dev.off()
  unlink(tf)

  expect_s3_class(result$segments, "data.frame")
})

# ============================================
# plot-heatmap.R gaps
# ============================================

test_that("plot_heatmap clustered with legend hidden (line 306)", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, show_legend = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap clustered with legend_position = 'none' (line 306)", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- LETTERS[1:5]

  clusters <- list(
    Group1 = c("A", "B"),
    Group2 = c("C", "D", "E")
  )

  p <- plot_heatmap(mat, cluster_list = clusters, legend_position = "none")
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap group_tna with unlabeled matrices (line 412)", {
  skip_if_not_installed("ggplot2")

  # Create a mock group_tna with matrices that have no rownames
  m1 <- matrix(runif(9), 3, 3)
  m2 <- matrix(runif(9), 3, 3)
  mock_group <- list(
    list(weights = m1),
    list(weights = m2)
  )
  names(mock_group) <- c("GroupA", "GroupB")
  class(mock_group) <- c("group_tna", "list")

  p <- plot_heatmap(mock_group)
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap with value_halo (lines 497-504)", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  p <- plot_heatmap(mat, show_values = TRUE, value_halo = "white")
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap with value_halo and fontface", {
  skip_if_not_installed("ggplot2")

  mat <- matrix(runif(9), 3, 3)
  rownames(mat) <- colnames(mat) <- c("A", "B", "C")

  p <- plot_heatmap(mat, show_values = TRUE, value_halo = "black",
                    value_fontface = "bold", value_fontfamily = "serif")
  expect_s3_class(p, "ggplot")
})

# ============================================
# plot-transitions.R gaps
# ============================================

test_that("multi-step with unnamed colors vector (line 771)", {
  skip_if_not_installed("ggplot2")

  mat1 <- matrix(c(0, .4, .6, .3, 0, .7, .5, .5, 0), 3, 3, byrow = TRUE,
                 dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  mat2 <- matrix(c(0, .5, .5, .2, 0, .8, .6, .4, 0), 3, 3, byrow = TRUE,
                 dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  # Provide colors WITHOUT names
  cols <- c("red", "blue", "green")

  p <- plot_transitions(list(mat1, mat2), from_colors = cols)
  expect_s3_class(p, "ggplot")
})

test_that("individual tracking with unnamed colors (line 1026)", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    T1 = c("A", "B", "A", "C", "B"),
    T2 = c("B", "A", "C", "A", "C"),
    stringsAsFactors = FALSE
  )

  # Colors without names
  cols <- c("red", "blue", "green")

  p <- plot_transitions(df, track_individuals = TRUE, from_colors = cols)
  expect_s3_class(p, "ggplot")
})

test_that("individual tracking with short titles (line 1031)", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    T1 = c("A", "B", "A", "C", "B"),
    T2 = c("B", "A", "C", "A", "C"),
    T3 = c("C", "C", "B", "B", "A"),
    stringsAsFactors = FALSE
  )

  # Only one title for 3 columns → triggers fallback to names(df)
  p <- plot_transitions(df, track_individuals = TRUE, from_title = "Start")
  expect_s3_class(p, "ggplot")
})

test_that("individual tracking with unknown flow_color_by (line 1248-1259)", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    T1 = c("A", "B", "A", "C", "B"),
    T2 = c("B", "A", "C", "A", "C"),
    stringsAsFactors = FALSE
  )

  # "weight" is not a recognized flow_color_by for tracked mode
  p <- plot_transitions(df, track_individuals = TRUE,
                        flow_color_by = "weight")
  expect_s3_class(p, "ggplot")
})

test_that("individual tracking value_position = 'origin' (line 1448)", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    T1 = c("A", "B", "A", "C", "B"),
    T2 = c("B", "A", "C", "A", "C"),
    stringsAsFactors = FALSE
  )

  p <- plot_transitions(df, track_individuals = TRUE,
                        show_values = TRUE, value_position = "origin")
  expect_s3_class(p, "ggplot")
})

test_that("individual tracking value_position = 'destination' (line 1451)", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(
    T1 = c("A", "B", "A", "C", "B"),
    T2 = c("B", "A", "C", "A", "C"),
    stringsAsFactors = FALSE
  )

  p <- plot_transitions(df, track_individuals = TRUE,
                        show_values = TRUE, value_position = "destination")
  expect_s3_class(p, "ggplot")
})
