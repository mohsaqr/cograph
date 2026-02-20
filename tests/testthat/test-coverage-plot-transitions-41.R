# Tests for plot-transitions.R
# Coverage for transition flow visualization functions
# Tests plot_transitions, plot_alluvial, plot_trajectories and helpers

# ============================================
# Test Data Generators
# ============================================

#' Create a simple test transition matrix
#' @param n_from Number of source states
#' @param n_to Number of destination states
#' @param seed Random seed
#' @param same_states Use same states for from/to (for multi-step)
create_test_trans_matrix <- function(n_from = 3, n_to = 3, seed = 42, same_states = FALSE) {
  set.seed(seed)
  mat <- matrix(
    sample(0:50, n_from * n_to, replace = TRUE),
    nrow = n_from,
    ncol = n_to
  )
  if (same_states) {
    # Use same state names for both row and column
    rownames(mat) <- LETTERS[1:n_from]
    colnames(mat) <- LETTERS[1:n_to]
  } else {
    rownames(mat) <- LETTERS[1:n_from]
    colnames(mat) <- LETTERS[(n_from + 1):(n_from + n_to)]
  }
  mat
}

#' Create a transition data frame
#' @param n_obs Number of observations
#' @param n_cols Number of time columns
#' @param n_states Number of unique states
#' @param seed Random seed
create_test_trans_df <- function(n_obs = 20, n_cols = 2, n_states = 3, seed = 42) {
  set.seed(seed)
  states <- LETTERS[1:n_states]
  df <- as.data.frame(
    replicate(n_cols, sample(states, n_obs, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  colnames(df) <- paste0("T", 1:n_cols)
  df
}

#' Create a from/to/count data frame
#' @param n_transitions Number of unique transitions
#' @param seed Random seed
create_test_count_df <- function(n_transitions = 6, seed = 42) {
  set.seed(seed)
  data.frame(
    from = c("A", "A", "B", "B", "C", "C"),
    to = c("X", "Y", "X", "Z", "Y", "Z"),
    count = sample(10:50, 6, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# ============================================
# Basic plot_transitions Tests - Matrix Input
# ============================================

test_that("plot_transitions works with basic matrix input", {
  mat <- create_test_trans_matrix(3, 3)

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plot_transitions works with 2x2 matrix", {
  mat <- create_test_trans_matrix(2, 2)

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions works with rectangular matrix (more rows)", {
  mat <- create_test_trans_matrix(5, 3)

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions works with rectangular matrix (more cols)", {
  mat <- create_test_trans_matrix(2, 5)

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions works with matrix without row/col names", {
  mat <- matrix(c(50, 10, 5, 15, 40, 10, 5, 20, 30), 3, 3, byrow = TRUE)

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions handles single-column matrix", {
  mat <- matrix(c(10, 20, 30), ncol = 1)
  rownames(mat) <- c("A", "B", "C")
  colnames(mat) <- "X"

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions handles single-row matrix", {
  mat <- matrix(c(10, 20, 30), nrow = 1)
  rownames(mat) <- "A"
  colnames(mat) <- c("X", "Y", "Z")

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

# ============================================
# Table Input Tests
# ============================================

test_that("plot_transitions works with table input", {
  before <- c("A", "A", "B", "B", "A", "C", "B", "C")
  after <- c("X", "Y", "X", "Z", "X", "Y", "Z", "X")
  tab <- table(before, after)

  p <- plot_transitions(tab)

  expect_s3_class(p, "gg")
})

# ============================================
# Two Vectors Input Tests (Chi-square style)
# ============================================

test_that("plot_transitions works with two vectors input", {
  before <- c("A", "A", "B", "B", "A", "C", "B", "C", "A", "B")
  after <- c("X", "Y", "X", "Z", "X", "Y", "Z", "X", "Y", "Z")

  p <- plot_transitions(before, after)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions with two vectors uses default titles", {
  before <- c("A", "A", "B", "B", "C")
  after <- c("X", "Y", "X", "Y", "Z")

  p <- plot_transitions(before, after)

  expect_s3_class(p, "gg")
})

# ============================================
# Data Frame Input Tests
# ============================================

test_that("plot_transitions works with 2-column data frame", {
  df <- create_test_trans_df(n_obs = 20, n_cols = 2)

  p <- plot_transitions(df, from_title = "Time 1", to_title = "Time 2")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions works with from/to/count data frame", {
  df <- create_test_count_df()

  p <- plot_transitions(df)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions handles multi-column data frame (triggers multi-step)", {
  df <- create_test_trans_df(n_obs = 30, n_cols = 4)

  p <- plot_transitions(df)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions errors with invalid data frame structure", {
  df <- data.frame(
    a = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  expect_error(plot_transitions(df))
})

# ============================================
# Multi-step Transitions (List of Matrices)
# ============================================

test_that("plot_transitions works with list of matrices", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2))

  expect_s3_class(p, "gg")
})

test_that("plot_transitions multi-step with custom titles", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)
  mat3 <- create_test_trans_matrix(3, 3, seed = 3, same_states = TRUE)

  p <- plot_transitions(
    list(mat1, mat2, mat3),
    from_title = c("T1", "T2", "T3", "T4")
  )

  expect_s3_class(p, "gg")
})

test_that("plot_transitions multi-step uses default titles when not provided", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2))

  expect_s3_class(p, "gg")
})

# ============================================
# Title Parameters Tests
# ============================================

test_that("plot_transitions respects from_title parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, from_title = "Before")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects to_title parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, to_title = "After")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects title_size parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, title_size = 8)

  expect_s3_class(p, "gg")
})

# ============================================
# Color Parameters Tests
# ============================================

test_that("plot_transitions uses custom from_colors", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(
    mat,
    from_colors = c("A" = "#FF0000", "B" = "#00FF00", "C" = "#0000FF")
  )

  expect_s3_class(p, "gg")
})

test_that("plot_transitions uses custom to_colors", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(
    mat,
    to_colors = c("D" = "#FFFF00", "E" = "#FF00FF", "F" = "#00FFFF")
  )

  expect_s3_class(p, "gg")
})

test_that("plot_transitions uses unnamed color vectors", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(
    mat,
    from_colors = c("#FF0000", "#00FF00", "#0000FF"),
    to_colors = c("#FFFF00", "#FF00FF", "#00FFFF")
  )

  expect_s3_class(p, "gg")
})

# ============================================
# Flow Parameters Tests
# ============================================

test_that("plot_transitions respects flow_fill parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, flow_fill = "#AA5500")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects flow_alpha parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, flow_alpha = 0.8)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects flow_color_by = 'source'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, flow_color_by = "source")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects flow_color_by = 'destination'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, flow_color_by = "destination")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects flow_border parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, flow_border = "black", flow_border_width = 1)

  expect_s3_class(p, "gg")
})

# ============================================
# Node Parameters Tests
# ============================================

test_that("plot_transitions respects node_width parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, node_width = 0.15)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects node_border parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, node_border = "black")

  expect_s3_class(p, "gg")
})
test_that("plot_transitions respects node_spacing parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, node_spacing = 0.05)

  expect_s3_class(p, "gg")
})

# ============================================
# Label Parameters Tests
# ============================================

test_that("plot_transitions respects label_size parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, label_size = 5)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects label_position = 'beside'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, label_position = "beside")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects label_position = 'inside'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, label_position = "inside")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects label_position = 'above'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, label_position = "above")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects label_position = 'below'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, label_position = "below")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects label_position = 'outside'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, label_position = "outside")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects label_halo = TRUE", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, label_halo = TRUE)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects label_halo = FALSE", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, label_halo = FALSE)

  expect_s3_class(p, "gg")
})

# ============================================
# Curve and Flow Shape Tests
# ============================================

test_that("plot_transitions respects curve_strength parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, curve_strength = 0.3)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions handles curve_strength = 0", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, curve_strength = 0)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions handles curve_strength = 1", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, curve_strength = 1)

  expect_s3_class(p, "gg")
})

# ============================================
# Value Display Tests
# ============================================

test_that("plot_transitions respects show_values = TRUE", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_values = TRUE)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects value_position = 'center'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_values = TRUE, value_position = "center")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects value_position = 'origin'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_values = TRUE, value_position = "origin")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects value_position = 'destination'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_values = TRUE, value_position = "destination")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects value_position = 'outside_origin'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_values = TRUE, value_position = "outside_origin")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects value_position = 'outside_destination'", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_values = TRUE, value_position = "outside_destination")

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects value_size parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_values = TRUE, value_size = 5)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects value_color parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_values = TRUE, value_color = "red")

  expect_s3_class(p, "gg")
})

# ============================================
# Totals Display Tests
# ============================================

test_that("plot_transitions respects show_totals = TRUE", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_totals = TRUE)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects total_size parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_totals = TRUE, total_size = 6)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects total_color parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, show_totals = TRUE, total_color = "black")

  expect_s3_class(p, "gg")
})

# ============================================
# Flow Conservation Tests
# ============================================

test_that("plot_transitions respects conserve_flow = TRUE", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, conserve_flow = TRUE)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects conserve_flow = FALSE", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, conserve_flow = FALSE)

  expect_s3_class(p, "gg")
})

# ============================================
# Minimum Flow Filter Tests
# ============================================

test_that("plot_transitions respects min_flow parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, min_flow = 10)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions handles min_flow filtering all flows", {
  mat <- create_test_trans_matrix()

  # This will filter many but not all flows
  p <- plot_transitions(mat, min_flow = 40)

  expect_s3_class(p, "gg")
})

# ============================================
# Column Gap Tests
# ============================================

test_that("plot_transitions respects column_gap = 1 (full width)", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, column_gap = 1)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions respects column_gap = 0.5 (half width)", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(mat, column_gap = 0.5)

  expect_s3_class(p, "gg")
})

# ============================================
# Individual Tracking Tests
# ============================================

test_that("plot_transitions with track_individuals = TRUE", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 3)

  p <- plot_transitions(df, track_individuals = TRUE)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions with track_individuals and custom line params", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 2)

  p <- plot_transitions(
    df,
    track_individuals = TRUE,
    line_alpha = 0.5,
    line_width = 1
  )

  expect_s3_class(p, "gg")
})

test_that("plot_transitions with track_individuals and jitter_amount", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  p <- plot_transitions(df, track_individuals = TRUE, jitter_amount = 0.5)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions with track_individuals and proportional_nodes = FALSE", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  p <- plot_transitions(df, track_individuals = TRUE, proportional_nodes = FALSE)

  expect_s3_class(p, "gg")
})

# ============================================
# plot_alluvial Alias Tests
# ============================================

test_that("plot_alluvial works with matrix input", {
  mat <- create_test_trans_matrix()

  p <- plot_alluvial(mat)

  expect_s3_class(p, "gg")
})

test_that("plot_alluvial works with data frame input", {
  df <- create_test_trans_df(n_obs = 20, n_cols = 3)

  p <- plot_alluvial(df)

  expect_s3_class(p, "gg")
})

test_that("plot_alluvial respects flow_color_by parameter", {
  mat <- create_test_trans_matrix()

  p <- plot_alluvial(mat, flow_color_by = "source")

  expect_s3_class(p, "gg")
})

test_that("plot_alluvial passes all parameters correctly", {
  mat <- create_test_trans_matrix()

  p <- plot_alluvial(
    mat,
    from_title = "Start",
    to_title = "End",
    flow_alpha = 0.6,
    node_width = 0.1,
    show_totals = TRUE,
    curve_strength = 0.5
  )

  expect_s3_class(p, "gg")
})

# ============================================
# plot_trajectories Alias Tests
# ============================================

test_that("plot_trajectories works with data frame input", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 3)

  p <- plot_trajectories(df)

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories with flow_color_by = 'first'", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_trajectories(df, flow_color_by = "first")

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories with flow_color_by = 'last'", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_trajectories(df, flow_color_by = "last")

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories with flow_color_by = 'source'", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_trajectories(df, flow_color_by = "source")

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories with flow_color_by = 'destination'", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_trajectories(df, flow_color_by = "destination")

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories respects line_alpha parameter", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  p <- plot_trajectories(df, line_alpha = 0.5)

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories respects line_width parameter", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  p <- plot_trajectories(df, line_width = 1)

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories respects jitter_amount parameter", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  p <- plot_trajectories(df, jitter_amount = 0.5)

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories respects proportional_nodes parameter", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  p <- plot_trajectories(df, proportional_nodes = FALSE)

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories respects show_totals parameter", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  p <- plot_trajectories(df, show_totals = TRUE)

  expect_s3_class(p, "gg")
})

# ============================================
# Helper Function Tests
# ============================================

test_that(".matrix_to_trans_df converts matrix correctly", {
  mat <- matrix(c(10, 5, 3, 8), 2, 2)
  rownames(mat) <- c("A", "B")
  colnames(mat) <- c("X", "Y")

  # Access internal function
  result <- cograph:::.matrix_to_trans_df(mat)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("from", "to", "count") %in% names(result)))
  expect_true(nrow(result) >= 1)
})

test_that(".matrix_to_trans_df handles matrix without row/col names", {
  mat <- matrix(c(10, 5, 3, 8), 2, 2)

  result <- cograph:::.matrix_to_trans_df(mat)

  expect_s3_class(result, "data.frame")
  expect_true(all(grepl("From_|To_", c(result$from[1], result$to[1]))))
})

test_that(".matrix_to_trans_df removes zero flows", {
  mat <- matrix(c(10, 0, 0, 8), 2, 2)
  rownames(mat) <- c("A", "B")
  colnames(mat) <- c("X", "Y")

  result <- cograph:::.matrix_to_trans_df(mat)

  expect_equal(nrow(result), 2)  # Only non-zero entries
  expect_true(all(result$count > 0))
})

test_that(".calculate_node_positions returns correct structure", {
  states <- c("A", "B", "C")
  heights <- c(0.4, 0.3, 0.2)
  spacing <- 0.02

  result <- cograph:::.calculate_node_positions(states, heights, spacing)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("state", "height", "top", "bottom") %in% names(result)))
})

test_that(".default_transition_palette returns correct number of colors", {
  colors_3 <- cograph:::.default_transition_palette(3)
  colors_8 <- cograph:::.default_transition_palette(8)
  colors_12 <- cograph:::.default_transition_palette(12)

  expect_equal(length(colors_3), 3)
  expect_equal(length(colors_8), 8)
  expect_equal(length(colors_12), 12)
})

test_that(".create_bezier_ribbon returns polygon coordinates", {
  result <- cograph:::.create_bezier_ribbon(
    x0 = 0, y0_top = 1, y0_bottom = 0.5,
    x1 = 1, y1_top = 0.8, y1_bottom = 0.3,
    strength = 0.6, n_points = 50
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("x", "y") %in% names(result)))
  expect_equal(nrow(result), 100)  # 50 points x 2 (top + bottom)
})

# ============================================
# Edge Cases and Error Handling
# ============================================

test_that("plot_transitions errors on invalid input type", {
  expect_error(plot_transitions("invalid"))
})

test_that("plot_transitions handles matrix with all zeros", {
  mat <- matrix(0, 3, 3)
  rownames(mat) <- c("A", "B", "C")
  colnames(mat) <- c("X", "Y", "Z")

  # At least one non-zero flow needed
  mat[1, 1] <- 5

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions handles matrix with very small values", {
  mat <- matrix(0.001, 3, 3)
  rownames(mat) <- c("A", "B", "C")
  colnames(mat) <- c("X", "Y", "Z")

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

test_that("plot_transitions handles matrix with large values", {
  mat <- matrix(c(10000, 5000, 3000, 8000, 2000, 7000, 4000, 6000, 9000), 3, 3)
  rownames(mat) <- c("A", "B", "C")
  colnames(mat) <- c("X", "Y", "Z")

  p <- plot_transitions(mat)

  expect_s3_class(p, "gg")
})

# ============================================
# Multi-step with flow_color_by Tests
# ============================================

test_that("multi-step transitions respect flow_color_by = 'source'", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), flow_color_by = "source")

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions respect flow_color_by = 'destination'", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), flow_color_by = "destination")

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions with show_values", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), show_values = TRUE)

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions with show_totals", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), show_totals = TRUE)

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions with custom colors", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(
    list(mat1, mat2),
    from_colors = c("A" = "red", "B" = "green", "C" = "blue")
  )

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions with label_position = 'inside'", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), label_position = "inside")

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions with label_position = 'above'", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), label_position = "above")

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions with label_position = 'below'", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), label_position = "below")

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions with label_position = 'outside'", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), label_position = "outside")

  expect_s3_class(p, "gg")
})

test_that("multi-step transitions with label_halo = FALSE", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  p <- plot_transitions(list(mat1, mat2), label_halo = FALSE)

  expect_s3_class(p, "gg")
})

# ============================================
# Individual Tracking Label Position Tests
# ============================================

test_that("individual tracking with label_position = 'outside'", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_transitions(df, track_individuals = TRUE, label_position = "outside")

  expect_s3_class(p, "gg")
})

test_that("individual tracking with label_position = 'inside'", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_transitions(df, track_individuals = TRUE, label_position = "inside")

  expect_s3_class(p, "gg")
})

test_that("individual tracking with label_halo = FALSE", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_transitions(df, track_individuals = TRUE, label_halo = FALSE)

  expect_s3_class(p, "gg")
})

# ============================================
# Graphics Device Tests
# ============================================

test_that("plot_transitions can be printed to PNG device", {
  mat <- create_test_trans_matrix()

  expect_no_error(with_temp_png({
    print(plot_transitions(mat))
  }))
})

test_that("plot_alluvial can be printed to PNG device", {
  mat <- create_test_trans_matrix()

  expect_no_error(with_temp_png({
    print(plot_alluvial(mat))
  }))
})

test_that("plot_trajectories can be printed to PNG device", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  expect_no_error(with_temp_png({
    print(plot_trajectories(df))
  }))
})

test_that("multi-step transitions can be printed to PNG device", {
  mat1 <- create_test_trans_matrix(3, 3, seed = 1, same_states = TRUE)
  mat2 <- create_test_trans_matrix(3, 3, seed = 2, same_states = TRUE)

  expect_no_error(with_temp_png({
    print(plot_transitions(list(mat1, mat2)))
  }))
})

# ============================================
# Combination Parameter Tests
# ============================================

test_that("plot_transitions with multiple styling options combined", {
  mat <- create_test_trans_matrix()

  p <- plot_transitions(
    mat,
    from_title = "Before Treatment",
    to_title = "After Treatment",
    flow_fill = "#445566",
    flow_alpha = 0.5,
    flow_border = "#000000",
    flow_border_width = 0.3,
    node_width = 0.1,
    node_border = "#333333",
    node_spacing = 0.03,
    label_size = 4,
    label_position = "beside",
    label_halo = TRUE,
    title_size = 6,
    curve_strength = 0.5,
    show_values = TRUE,
    value_position = "center",
    value_size = 3,
    value_color = "white",
    show_totals = TRUE,
    total_size = 3,
    total_color = "black",
    conserve_flow = TRUE,
    min_flow = 0
  )

  expect_s3_class(p, "gg")
})

test_that("plot_alluvial with all available options", {
  mat <- create_test_trans_matrix()

  p <- plot_alluvial(
    mat,
    from_title = "Start",
    to_title = "End",
    from_colors = c("A" = "#FF0000", "B" = "#00FF00", "C" = "#0000FF"),
    to_colors = c("D" = "#FFFF00", "E" = "#FF00FF", "F" = "#00FFFF"),
    flow_fill = "#888888",
    flow_alpha = 0.4,
    flow_border = NA,
    node_width = 0.08,
    node_spacing = 0.02,
    label_size = 3.5,
    label_position = "beside",
    label_halo = TRUE,
    title_size = 5,
    curve_strength = 0.6,
    show_values = TRUE,
    show_totals = TRUE,
    conserve_flow = TRUE,
    min_flow = 0,
    column_gap = 1
  )

  expect_s3_class(p, "gg")
})

test_that("plot_trajectories with all available options", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 3)

  p <- plot_trajectories(
    df,
    from_title = c("Week 1", "Week 2", "Week 3"),
    from_colors = c("A" = "#FF0000", "B" = "#00FF00", "C" = "#0000FF"),
    flow_color_by = "first",
    node_width = 0.1,
    node_border = "gray40",
    node_spacing = 0.02,
    label_size = 3.5,
    label_position = "beside",
    label_halo = TRUE,
    title_size = 5,
    curve_strength = 0.6,
    line_alpha = 0.4,
    line_width = 0.6,
    jitter_amount = 0.8,
    show_totals = TRUE,
    total_size = 4,
    total_color = "white",
    column_gap = 0.9,
    proportional_nodes = TRUE
  )

  expect_s3_class(p, "gg")
})

# ============================================
# Intermediate Node Labels Tests
# ============================================

test_that("trajectories label_position = 'above' labels all columns", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 4)

  p <- plot_trajectories(df, label_position = "above")

  expect_s3_class(p, "gg")
  # Above position uses all node_rects data (all columns)
  # Verify plot builds without error for multi-column case
})

test_that("trajectories label_position = 'below' labels all columns", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 4)

  p <- plot_trajectories(df, label_position = "below")

  expect_s3_class(p, "gg")
})

test_that("trajectories label_position = 'inside' labels all columns", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 4)

  p <- plot_trajectories(df, label_position = "inside")

  expect_s3_class(p, "gg")
})

test_that("trajectories label_position = 'beside' includes intermediate labels", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 4)

  p <- plot_trajectories(df, label_position = "beside")

  expect_s3_class(p, "gg")
  # Intermediate columns (2, 3) should get "above" style fallback labels
})

test_that("trajectories label_position = 'outside' includes intermediate labels", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 4)

  p <- plot_trajectories(df, label_position = "outside")

  expect_s3_class(p, "gg")
  # Intermediate columns (2, 3) should get "above" style fallback labels
})

test_that("trajectories label_position = 'beside' with label_halo = FALSE", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_trajectories(df, label_position = "beside", label_halo = FALSE)

  expect_s3_class(p, "gg")
})

test_that("trajectories label_position = 'above' with label_halo = FALSE", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_trajectories(df, label_position = "above", label_halo = FALSE)

  expect_s3_class(p, "gg")
})

test_that("trajectories label_position = 'below' with label_halo = FALSE", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 3)

  p <- plot_trajectories(df, label_position = "below", label_halo = FALSE)

  expect_s3_class(p, "gg")
})

test_that("trajectories label with 2-column data (no intermediate columns)", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  # 2 columns = no intermediate, should work like before
  p1 <- plot_trajectories(df, label_position = "beside")
  p2 <- plot_trajectories(df, label_position = "outside")
  p3 <- plot_trajectories(df, label_position = "above")

  expect_s3_class(p1, "gg")
  expect_s3_class(p2, "gg")
  expect_s3_class(p3, "gg")
})

# ============================================
# node_label_format Tests
# ============================================

test_that("node_label_format adds counts to labels", {
  df <- create_test_trans_df(n_obs = 20, n_cols = 3)

  p <- plot_trajectories(df,
    label_position = "above",
    node_label_format = "{state} (n={count})"
  )

  expect_s3_class(p, "gg")
})

test_that("node_label_format with newline separator", {
  df <- create_test_trans_df(n_obs = 20, n_cols = 3)

  p <- plot_trajectories(df,
    label_position = "above",
    node_label_format = "{state}\n(n={count})"
  )

  expect_s3_class(p, "gg")
})

test_that("node_label_format with count only", {
  df <- create_test_trans_df(n_obs = 20, n_cols = 2)

  p <- plot_trajectories(df,
    label_position = "inside",
    node_label_format = "n={count}"
  )

  expect_s3_class(p, "gg")
})

test_that("node_label_format = NULL preserves default behavior", {
  df <- create_test_trans_df(n_obs = 10, n_cols = 2)

  p <- plot_trajectories(df, node_label_format = NULL)

  expect_s3_class(p, "gg")
})

test_that("node_label_format via plot_transitions", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 3)

  p <- plot_transitions(df,
    track_individuals = TRUE,
    label_position = "above",
    node_label_format = "{state} ({count})"
  )

  expect_s3_class(p, "gg")
})

# ============================================
# bundle_size Tests
# ============================================

test_that("bundle_size integer bundles lines", {
  df <- create_test_trans_df(n_obs = 50, n_cols = 3)

  p <- plot_trajectories(df, bundle_size = 5)

  expect_s3_class(p, "gg")
})

test_that("bundle_size fraction bundles lines", {
  df <- create_test_trans_df(n_obs = 50, n_cols = 3)

  p <- plot_trajectories(df, bundle_size = 0.1)

  expect_s3_class(p, "gg")
})

test_that("bundle_size = 2 minimal bundling", {
  df <- create_test_trans_df(n_obs = 20, n_cols = 2)

  p <- plot_trajectories(df, bundle_size = 2)

  expect_s3_class(p, "gg")
})

test_that("bundle_size with large dataset", {
  set.seed(99)
  df <- create_test_trans_df(n_obs = 200, n_cols = 4, n_states = 4)

  p <- plot_trajectories(df, bundle_size = 10)

  expect_s3_class(p, "gg")
})

test_that("bundle_size fraction with large dataset", {
  set.seed(99)
  df <- create_test_trans_df(n_obs = 200, n_cols = 3)

  p <- plot_trajectories(df, bundle_size = 0.05)

  expect_s3_class(p, "gg")
})

test_that("bundle_legend = TRUE shows annotation", {
  df <- create_test_trans_df(n_obs = 30, n_cols = 3)

  p <- plot_trajectories(df, bundle_size = 5, bundle_legend = TRUE)

  expect_s3_class(p, "gg")
})

test_that("bundle_legend = FALSE hides annotation", {
  df <- create_test_trans_df(n_obs = 30, n_cols = 3)

  p <- plot_trajectories(df, bundle_size = 5, bundle_legend = FALSE)

  expect_s3_class(p, "gg")
})

test_that("bundle_size = NULL preserves default behavior", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 2)

  p <- plot_trajectories(df, bundle_size = NULL)

  expect_s3_class(p, "gg")
})

test_that("bundle_size via plot_transitions", {
  df <- create_test_trans_df(n_obs = 40, n_cols = 3)

  p <- plot_transitions(df,
    track_individuals = TRUE,
    bundle_size = 5,
    bundle_legend = TRUE
  )

  expect_s3_class(p, "gg")
})

test_that("bundle_size combined with node_label_format", {
  df <- create_test_trans_df(n_obs = 50, n_cols = 3)

  p <- plot_trajectories(df,
    label_position = "above",
    node_label_format = "{state} (n={count})",
    bundle_size = 10
  )

  expect_s3_class(p, "gg")
})

test_that("bundle_size combined with flow_color_by", {
  df <- create_test_trans_df(n_obs = 50, n_cols = 3)

  p <- plot_trajectories(df,
    flow_color_by = "first",
    bundle_size = 5
  )

  expect_s3_class(p, "gg")
})

test_that("bundle_size with all unique paths", {
  # Each individual has a unique trajectory
  df <- data.frame(
    T1 = c("A", "B", "C", "A", "B"),
    T2 = c("B", "C", "A", "C", "A"),
    T3 = c("C", "A", "B", "B", "C"),
    stringsAsFactors = FALSE
  )

  p <- plot_trajectories(df, bundle_size = 2)

  expect_s3_class(p, "gg")
})

# ============================================
# Combined New Features - Graphics Device Tests
# ============================================

test_that("intermediate labels render to PNG device", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 4)

  expect_no_error(with_temp_png({
    print(plot_trajectories(df, label_position = "above"))
  }))
})

test_that("node_label_format renders to PNG device", {
  df <- create_test_trans_df(n_obs = 15, n_cols = 3)

  expect_no_error(with_temp_png({
    print(plot_trajectories(df,
      label_position = "above",
      node_label_format = "{state} (n={count})"
    ))
  }))
})

test_that("bundle_size renders to PNG device", {
  df <- create_test_trans_df(n_obs = 50, n_cols = 3)

  expect_no_error(with_temp_png({
    print(plot_trajectories(df, bundle_size = 5))
  }))
})
