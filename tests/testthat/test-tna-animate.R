# Tests for tna_windows() and tna_animate()

test_that("tna_windows validates input parameters", {
  # Create sample data
  set.seed(123)
  states <- c("A", "B", "C")
  data <- data.frame(matrix(
    sample(states, 50, replace = TRUE),
    nrow = 10, ncol = 5
  ))

  # window_size cannot exceed number of columns
 expect_error(
    tna_windows(data, window_size = 10),
    "window_size.*cannot exceed"
  )

  # window_size must be at least 2
  expect_error(
    tna_windows(data, window_size = 1),
    "window_size must be at least 2"
  )

  # step must be at least 1
  expect_error(
    tna_windows(data, window_size = 3, step = 0),
    "step must be at least 1"
  )
})

test_that("tna_windows generates correct number of windows", {
  skip_if_not_installed("tna")

  set.seed(123)
  states <- c("A", "B", "C")
  # 10 columns, window_size = 5, step = 1 -> 6 windows
  data <- data.frame(matrix(
    sample(states, 100, replace = TRUE),
    nrow = 10, ncol = 10
  ))

  result <- tna_windows(data, window_size = 5, step = 1)

  # Expected: windows starting at 1,2,3,4,5,6 (6 total)
  expect_equal(length(result$windows), 6)
  expect_equal(result$start_times, c(1, 2, 3, 4, 5, 6))
  expect_equal(result$end_times, c(5, 6, 7, 8, 9, 10))
  expect_false(result$stopped_early)
})

test_that("tna_windows handles tumbling windows correctly", {
  skip_if_not_installed("tna")

  set.seed(123)
  states <- c("A", "B", "C")
  # 10 columns, window_size = 5, step = 5 -> 2 windows (tumbling)
  data <- data.frame(matrix(
    sample(states, 100, replace = TRUE),
    nrow = 10, ncol = 10
  ))

  result <- tna_windows(data, window_size = 5, step = 5)

  expect_equal(length(result$windows), 2)
  expect_equal(result$start_times, c(1, 6))
  expect_equal(result$end_times, c(5, 10))
})

test_that("tna_windows stops early when NA threshold exceeded", {
  skip_if_not_installed("tna")

  set.seed(123)
  states <- c("A", "B", "C")
  data <- data.frame(matrix(
    sample(states, 100, replace = TRUE),
    nrow = 10, ncol = 10
  ))

  # Add NAs to later columns (columns 7-10 are 80% NA)
  data[, 7:10] <- NA

  result <- tna_windows(data, window_size = 5, step = 1, na_threshold = 0.5)

  # Should stop before processing windows that include too many NA columns
  expect_true(result$stopped_early)
  expect_true(length(result$windows) < 6)  # Less than full 6 windows
})

test_that("tna_windows returns empty when first window exceeds NA threshold", {
  skip_if_not_installed("tna")

  # Create data that's mostly NA
  data <- data.frame(matrix(NA, nrow = 10, ncol = 5))

  result <- tna_windows(data, window_size = 3, na_threshold = 0.5)

  expect_equal(length(result$windows), 0)
  expect_true(result$stopped_early)
  expect_equal(result$stopped_at, 1)
})

test_that("tna_windows accepts matrix input", {
  skip_if_not_installed("tna")

  set.seed(123)
  states <- c("A", "B", "C")
  mat <- matrix(
    sample(states, 50, replace = TRUE),
    nrow = 10, ncol = 5
  )

  # Should convert matrix to data.frame internally
  result <- tna_windows(mat, window_size = 3)

  expect_true(length(result$windows) > 0)
})

test_that("tna_windows tracks NA proportions correctly", {
  skip_if_not_installed("tna")

  set.seed(123)
  states <- c("A", "B", "C")
  data <- data.frame(matrix(
    sample(states, 60, replace = TRUE),
    nrow = 10, ncol = 6
  ))

  # Add some NAs to first column
  data[1:2, 1] <- NA

  result <- tna_windows(data, window_size = 3, step = 1)

  # Check that NA proportions are recorded
  expect_equal(length(result$na_proportions), length(result$windows))
  # First window should have some NAs
  expect_true(result$na_proportions[1] > 0)
})

test_that("tna_animate requires gifski package", {
  # Mock requireNamespace to return FALSE for gifski
  # This test just checks the error message format
  skip_if_not_installed("tna")

  # Create minimal data
  data <- data.frame(A = c("X", "Y"), B = c("Y", "X"))

  # If gifski is not installed, should get appropriate error
  if (!requireNamespace("gifski", quietly = TRUE)) {
    expect_error(
      tna_animate(data, window_size = 2),
      "gifski.*required"
    )
  }
})

test_that("tna_animate creates output file", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  set.seed(123)
  states <- c("A", "B", "C")
  data <- data.frame(matrix(
    sample(states, 80, replace = TRUE),
    nrow = 10, ncol = 8
  ))

  # Use temporary file for output
  output_file <- tempfile(fileext = ".gif")

  result <- tna_animate(
    data,
    window_size = 4,
    step = 1,
    fps = 2,
    output = output_file,
    width = 300,
    height = 300
  )

  expect_true(file.exists(output_file))
  expect_equal(result, output_file)

  # Clean up
  unlink(output_file)
})

test_that("tna_animate handles single window case", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  set.seed(123)
  states <- c("A", "B", "C")
  # Only 3 columns with window_size = 3 -> 1 window
  data <- data.frame(matrix(
    sample(states, 30, replace = TRUE),
    nrow = 10, ncol = 3
  ))

  output_file <- tempfile(fileext = ".gif")

  result <- tna_animate(
    data,
    window_size = 3,
    output = output_file,
    width = 200,
    height = 200
  )

  expect_true(file.exists(output_file))

  # Clean up
  unlink(output_file)
})

test_that("tna_animate errors when no valid windows", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  # All NA data
  data <- data.frame(matrix(NA, nrow = 10, ncol = 5))

  expect_error(
    tna_animate(data, window_size = 3, na_threshold = 0.5),
    "No valid windows generated"
  )
})

test_that("tna_animate passes extra arguments to tplot", {
  skip_if_not_installed("tna")
  skip_if_not_installed("gifski")

  set.seed(123)
  states <- c("A", "B", "C")
  data <- data.frame(matrix(
    sample(states, 50, replace = TRUE),
    nrow = 10, ncol = 5
  ))

  output_file <- tempfile(fileext = ".gif")

  # This should work without error, passing layout to tplot
  expect_no_error(
    tna_animate(
      data,
      window_size = 3,
      output = output_file,
      width = 200,
      height = 200,
      layout = "circle"
    )
  )

  # Clean up
  unlink(output_file)
})
