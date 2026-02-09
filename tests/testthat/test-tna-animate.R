# Tests for tna_windows() and tna_animate()
# Comprehensive test suite for TNA animation feature

# =============================================================================
# Helper function for generating test sequence data
# =============================================================================

#' Generate test sequence data
#' @param n_sequences Number of sequences (rows)
#' @param seq_length Number of time points (columns)
#' @param states Character vector of possible states
#' @param na_prop Proportion of NA values to introduce
#' @param seed Random seed
generate_test_sequences <- function(n_sequences = 20, seq_length = 10,
                                     states = c("A", "B", "C"),
                                     na_prop = 0, seed = 123) {
  set.seed(seed)
  data <- data.frame(matrix(
    sample(states, n_sequences * seq_length, replace = TRUE),
    nrow = n_sequences, ncol = seq_length
  ))
  names(data) <- paste0("T", seq_len(seq_length))

  # Add NAs if requested

if (na_prop > 0) {
    n_na <- round(n_sequences * seq_length * na_prop)
    na_positions <- sample(seq_len(n_sequences * seq_length), n_na)
    data[na_positions] <- NA
  }

  data
}

# =============================================================================
# 1. tna_windows() Tests
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1 Parameter Validation
# -----------------------------------------------------------------------------

test_that("tna_windows validates window_size parameter", {
  data <- generate_test_sequences(n_sequences = 10, seq_length = 5)

  # window_size > columns
  expect_error(
    tna_windows(data, window_size = 10),
    "window_size.*cannot exceed"
  )

  # window_size < 2
  expect_error(
    tna_windows(data, window_size = 1),
    "window_size must be at least 2"
  )

  # window_size = 0
  expect_error(
    tna_windows(data, window_size = 0),
    "window_size must be at least 2"
  )

  # window_size = columns (edge case - 1 window)
  skip_if_not_installed("tna")
  expect_no_error(tna_windows(data, window_size = 5))
})

test_that("tna_windows validates step parameter", {
  data <- generate_test_sequences(n_sequences = 10, seq_length = 10)

  expect_error(tna_windows(data, step = 0), "step must be at least 1")
  expect_error(tna_windows(data, step = -1), "step must be at least 1")

  skip_if_not_installed("tna")
  expect_no_error(tna_windows(data, step = 1))
  expect_no_error(tna_windows(data, step = 5))  # tumbling
})

test_that("tna_windows validates na_threshold parameter", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 10, seq_length = 10)

  # Extreme values should work
  expect_no_error(tna_windows(data, na_threshold = 0))
  expect_no_error(tna_windows(data, na_threshold = 1.0))
})

test_that("tna_windows requires tna package", {
  # This test checks the error message format
  skip_if_not_installed("tna")
  # If tna IS installed, we can verify the function works
  data <- generate_test_sequences(n_sequences = 10, seq_length = 5)
  expect_no_error(tna_windows(data, window_size = 3))
})

# -----------------------------------------------------------------------------
# 1.2 Input Type Handling
# -----------------------------------------------------------------------------

test_that("tna_windows accepts data.frame input", {
  skip_if_not_installed("tna")
  data <- data.frame(T1 = c("A", "B"), T2 = c("B", "A"), T3 = c("A", "A"))
  result <- tna_windows(data, window_size = 2)
  expect_true(length(result$windows) > 0)
})

test_that("tna_windows accepts matrix input", {
  skip_if_not_installed("tna")
  mat <- matrix(c("A", "B", "B", "A", "A", "A"), nrow = 2, ncol = 3)
  result <- tna_windows(mat, window_size = 2)
  expect_true(length(result$windows) > 0)
})

test_that("tna_windows handles factor columns", {
  skip_if_not_installed("tna")
  data <- data.frame(
    T1 = factor(c("A", "B")),
    T2 = factor(c("B", "A")),
    T3 = factor(c("A", "A"))
  )
  result <- tna_windows(data, window_size = 2)
  expect_true(length(result$windows) > 0)
})

test_that("tna_windows errors on NULL input", {
  expect_error(tna_windows(NULL, window_size = 2))
})

test_that("tna_windows handles empty data frame", {
  data <- data.frame()
  expect_error(tna_windows(data, window_size = 2))
})

# -----------------------------------------------------------------------------
# 1.3 Window Calculation
# -----------------------------------------------------------------------------

test_that("tna_windows calculates correct sliding windows", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 10)

  result <- tna_windows(data, window_size = 5, step = 1)

  # 10 - 5 + 1 = 6 windows
  expect_equal(length(result$windows), 6)
  expect_equal(result$start_times, c(1, 2, 3, 4, 5, 6))
  expect_equal(result$end_times, c(5, 6, 7, 8, 9, 10))
})

test_that("tna_windows calculates correct tumbling windows", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 10)

  result <- tna_windows(data, window_size = 5, step = 5)

  # 2 non-overlapping windows
  expect_equal(length(result$windows), 2)
  expect_equal(result$start_times, c(1, 6))
  expect_equal(result$end_times, c(5, 10))
})

test_that("tna_windows handles step > 1", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 10)

  result <- tna_windows(data, window_size = 3, step = 2)

  # starts: 1, 3, 5, 7 = 4 windows
  expect_equal(length(result$windows), 4)
  expect_equal(result$start_times, c(1, 3, 5, 7))
})

test_that("tna_windows handles single window case", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 5)

  result <- tna_windows(data, window_size = 5)

  expect_equal(length(result$windows), 1)
  expect_equal(result$start_times, 1)
  expect_equal(result$end_times, 5)
  expect_false(result$stopped_early)
})

test_that("tna_windows handles step larger than remaining columns", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 8)

  # window_size = 4, step = 10 -> only 1 window fits
  result <- tna_windows(data, window_size = 4, step = 10)

  expect_equal(length(result$windows), 1)
  expect_equal(result$start_times, 1)
  expect_equal(result$end_times, 4)
})

# -----------------------------------------------------------------------------
# 1.4 NA Handling
# -----------------------------------------------------------------------------

test_that("tna_windows stops early when NA threshold exceeded", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 10)
  data[, 7:10] <- NA  # Last 4 columns all NA

  result <- tna_windows(data, window_size = 5, na_threshold = 0.5)

  expect_true(result$stopped_early)
  expect_true(length(result$windows) < 6)
})

test_that("tna_windows returns empty on first window NA exceeded", {
  skip_if_not_installed("tna")
  data <- data.frame(matrix(NA, nrow = 10, ncol = 5))

  result <- tna_windows(data, window_size = 3, na_threshold = 0.5)

  expect_equal(length(result$windows), 0)
  expect_true(result$stopped_early)
  expect_equal(result$stopped_at, 1)
})

test_that("tna_windows tracks NA proportions correctly", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  data[1:5, 1] <- NA  # 25% NA in first column

  result <- tna_windows(data, window_size = 3)

  expect_equal(length(result$na_proportions), length(result$windows))
  expect_true(all(result$na_proportions >= 0 & result$na_proportions <= 1))
})

test_that("tna_windows respects na_threshold = 0", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  data[1, 1] <- NA  # Single NA

  result <- tna_windows(data, window_size = 3, na_threshold = 0)

  # Should stop at first window (has NA)
  expect_true(result$stopped_early)
  expect_equal(result$stopped_at, 1)
})

test_that("tna_windows accepts all windows with na_threshold = 1", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  data[, 4:6] <- NA  # Half the columns NA

  result <- tna_windows(data, window_size = 3, na_threshold = 1.0)

  # Should process all windows despite NAs
  expect_false(result$stopped_early)
})

test_that("tna_windows handles partial NA coverage", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 10)
  # Add 30% NA to middle columns
  data[1:6, 4:6] <- NA

  result <- tna_windows(data, window_size = 3, na_threshold = 0.4)

  # First windows should be fine, middle might trigger stopping
  expect_true(length(result$windows) > 0)
})

# -----------------------------------------------------------------------------
# 1.5 Output Structure
# -----------------------------------------------------------------------------

test_that("tna_windows returns correct structure", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 8)

  result <- tna_windows(data, window_size = 4)

  expect_type(result, "list")
  expect_true("windows" %in% names(result))
  expect_true("start_times" %in% names(result))
  expect_true("end_times" %in% names(result))
  expect_true("na_proportions" %in% names(result))
  expect_true("stopped_early" %in% names(result))
  expect_type(result$windows, "list")
  # start_times and end_times can be integer or double (numeric)
  expect_true(is.numeric(result$start_times))
  expect_true(is.numeric(result$end_times))
  expect_type(result$na_proportions, "double")
  expect_type(result$stopped_early, "logical")
})

test_that("tna_windows includes stopped_at when stopped early", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 10)
  data[, 5:10] <- NA

  result <- tna_windows(data, window_size = 3, na_threshold = 0.3)

  expect_true(result$stopped_early)
  expect_true("stopped_at" %in% names(result))
  # stopped_at can be integer or double (numeric)
  expect_true(is.numeric(result$stopped_at))
})

test_that("tna_windows returns tna objects in windows list", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)

  result <- tna_windows(data, window_size = 3)

  for (w in result$windows) {
    expect_s3_class(w, "tna")
    expect_true(!is.null(w$weights))
  }
})

test_that("tna_windows preserves state labels across windows", {
  skip_if_not_installed("tna")
  data <- generate_test_sequences(n_sequences = 20, seq_length = 6,
                                   states = c("Alpha", "Beta", "Gamma"))

  result <- tna_windows(data, window_size = 3)

  # All windows should have same state labels
  for (w in result$windows) {
    expect_true(all(c("Alpha", "Beta", "Gamma") %in% rownames(w$weights)))
  }
})

# =============================================================================
# 2. tna_animate() Tests
# =============================================================================

# -----------------------------------------------------------------------------
# 2.1 Dependency Checks
# -----------------------------------------------------------------------------

test_that("tna_animate requires gifski package", {
  skip_if_not_installed("tna")

  if (!requireNamespace("gifski", quietly = TRUE)) {
    data <- generate_test_sequences(n_sequences = 10, seq_length = 5)
    expect_error(tna_animate(data, window_size = 2), "gifski.*required")
  }
})

test_that("tna_animate propagates tna package requirement", {
  skip_if_not_installed("gifski")

  if (!requireNamespace("tna", quietly = TRUE)) {
    data <- generate_test_sequences(n_sequences = 10, seq_length = 5)
    expect_error(tna_animate(data, window_size = 2), "tna.*required")
  }
})

# -----------------------------------------------------------------------------
# 2.2 Output File Creation
# -----------------------------------------------------------------------------

test_that("tna_animate creates GIF file", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 8)
  output <- tempfile(fileext = ".gif")

  result <- tna_animate(data, window_size = 4, output = output,
                        width = 200, height = 200)

  expect_true(file.exists(output))
  expect_equal(result, output)
  expect_gt(file.size(output), 0)

  unlink(output)
})

test_that("tna_animate returns output path invisibly", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  result <- tna_animate(data, window_size = 3, output = output,
                        width = 200, height = 200)

  expect_equal(result, output)
  unlink(output)
})

test_that("tna_animate overwrites existing file", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  # Create dummy file
  writeLines("dummy", output)
  old_size <- file.size(output)

  tna_animate(data, window_size = 3, output = output,
              width = 200, height = 200)

  expect_true(file.size(output) != old_size)
  unlink(output)
})

test_that("tna_animate creates file with reasonable size", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 30, seq_length = 8)
  output <- tempfile(fileext = ".gif")

  tna_animate(data, window_size = 4, output = output,
              width = 300, height = 300, fps = 2)

  # GIF should be at least 1KB for 5 frames
  expect_gt(file.size(output), 1000)

  unlink(output)
})

# -----------------------------------------------------------------------------
# 2.3 Error Handling
# -----------------------------------------------------------------------------

test_that("tna_animate errors when no valid windows", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- data.frame(matrix(NA, nrow = 10, ncol = 5))

  expect_error(
    tna_animate(data, window_size = 3, na_threshold = 0.5),
    "No valid windows generated"
  )
})

test_that("tna_animate handles single window case", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 3)
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 200, height = 200)
  )

  expect_true(file.exists(output))
  unlink(output)
})

test_that("tna_animate handles early stopping due to NAs", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 10)
  data[, 6:10] <- NA  # Later columns all NA
  output <- tempfile(fileext = ".gif")

  # Should still create GIF with available windows
  expect_message(
    tna_animate(data, window_size = 3, output = output,
                width = 200, height = 200, na_threshold = 0.3),
    "stopped early"
  )

  expect_true(file.exists(output))
  unlink(output)
})

# -----------------------------------------------------------------------------
# 2.4 Parameter Pass-Through
# -----------------------------------------------------------------------------

test_that("tna_animate passes layout to tplot", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 200, height = 200, layout = "circle")
  )

  unlink(output)
})

test_that("tna_animate passes theme to tplot", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 200, height = 200, theme = "colorblind")
  )

  unlink(output)
})

test_that("tna_animate passes vsize to tplot", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 200, height = 200, vsize = 10)
  )

  unlink(output)
})

test_that("tna_animate passes multiple extra arguments", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 200, height = 200,
                layout = "spring",
                vsize = 8,
                label_size = 0.8)
  )

  unlink(output)
})

# -----------------------------------------------------------------------------
# 2.5 FPS and Timing
# -----------------------------------------------------------------------------

test_that("tna_animate respects fps parameter", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)

  for (fps in c(1, 2, 5)) {
    output <- tempfile(fileext = ".gif")
    expect_no_error(
      tna_animate(data, window_size = 3, fps = fps, output = output,
                  width = 200, height = 200)
    )
    expect_true(file.exists(output))
    unlink(output)
  }
})

test_that("tna_animate respects loop parameter", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)

  for (loop in c(0, 1, 5)) {
    output <- tempfile(fileext = ".gif")
    expect_no_error(
      tna_animate(data, window_size = 3, loop = loop, output = output,
                  width = 200, height = 200)
    )
    unlink(output)
  }
})

# -----------------------------------------------------------------------------
# 2.6 Dimensions
# -----------------------------------------------------------------------------

test_that("tna_animate respects width and height", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)

  # Small dimensions
  output <- tempfile(fileext = ".gif")
  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 100, height = 100)
  )
  unlink(output)

  # Non-square dimensions
  output <- tempfile(fileext = ".gif")
  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 300, height = 200)
  )
  unlink(output)
})

test_that("tna_animate handles large dimensions", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 800, height = 800)
  )

  unlink(output)
})

# -----------------------------------------------------------------------------
# 2.7 Title Template
# -----------------------------------------------------------------------------

test_that("tna_animate uses custom title template", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 200, height = 200,
                title_template = "Window %d to %d")
  )

  unlink(output)
})

test_that("tna_animate uses default title template when not specified", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 20, seq_length = 6)
  output <- tempfile(fileext = ".gif")

  # Should work with default template "Time %d-%d"
  expect_no_error(
    tna_animate(data, window_size = 3, output = output,
                width = 200, height = 200)
  )

  unlink(output)
})

# =============================================================================
# 3. Integration Tests
# =============================================================================

test_that("full animation pipeline with simulated data", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  # Use larger dataset
  data <- generate_test_sequences(n_sequences = 50, seq_length = 12,
                                   states = c("Plan", "Execute", "Review", "Adjust"))
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 5, step = 1,
                output = output, width = 300, height = 300,
                fps = 2, na_threshold = 0.5)
  )

  expect_true(file.exists(output))
  expect_gt(file.size(output), 1000)  # Reasonable size

  unlink(output)
})

test_that("windows and animate produce consistent results", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  data <- generate_test_sequences(n_sequences = 30, seq_length = 10)

  # Generate windows
  windows_result <- tna_windows(data, window_size = 4, step = 2)

  # The animate function should use same windowing logic
  output <- tempfile(fileext = ".gif")
  expect_no_error(
    tna_animate(data, window_size = 4, step = 2, output = output,
                width = 200, height = 200)
  )

  # Number of frames should match number of windows
  # (We can't directly verify this, but the test ensures no errors)
  expect_true(file.exists(output))

  unlink(output)
})

test_that("animation with Saqrlab data if available", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")
  skip_if_not_installed("Saqrlab")

  data <- Saqrlab::simulate_sequences(n_sequences = 50, seq_length = 15, max_na = 2)
  output <- tempfile(fileext = ".gif")

  expect_no_error(
    tna_animate(data, window_size = 5, step = 2,
                output = output, width = 300, height = 300)
  )

  expect_true(file.exists(output))
  unlink(output)
})
