test_that("plot_chord works with basic symmetric matrix", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat)
  dev.off()

  expect_type(result, "list")
  expect_s3_class(result$segments, "data.frame")
  expect_s3_class(result$chords, "data.frame")
  expect_equal(nrow(result$segments), 3)
  expect_true(nrow(result$chords) > 0)
})

test_that("plot_chord auto-detects directed from asymmetric matrix", {
  mat <- matrix(c(0, .3, .2,
                  .4, 0, .1,
                  .3, .2, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat)
  dev.off()

  # Asymmetric → directed → more chords (each direction separate)
  expect_true(nrow(result$chords) >= 6)  # 6 off-diagonal entries
})

test_that("plot_chord respects directed = FALSE on asymmetric matrix", {
  mat <- matrix(c(0, .3, .2,
                  .4, 0, .1,
                  .3, .2, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat, directed = FALSE)
  dev.off()

  # Undirected processes i <= j only: 3 off-diagonal pairs
  expect_equal(nrow(result$chords), 3)
})

test_that("plot_chord works with 1x1 matrix", {
  mat <- matrix(0.5, 1, 1, dimnames = list("X", "X"))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat)
  dev.off()

  expect_equal(nrow(result$segments), 1)
})

test_that("plot_chord works with all-zero matrix", {
  mat <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat)
  dev.off()

  expect_equal(nrow(result$segments), 3)
  expect_equal(nrow(result$chords), 0)
})

test_that("plot_chord handles self-loops", {
  mat <- matrix(c(0.5, .3, .2,
                  .4, 0.3, .1,
                  .3, .2, 0.2), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  result_with <- plot_chord(mat, self_loop = TRUE)
  dev.off()

  png(tempfile(fileext = ".png"))
  result_without <- plot_chord(mat, self_loop = FALSE)
  dev.off()

  # With self-loops should have more chords

  expect_true(nrow(result_with$chords) > nrow(result_without$chords))
})

test_that("plot_chord respects threshold", {
  mat <- matrix(c(0, .3, .02,
                  .3, 0, .01,
                  .02, .01, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat, threshold = 0.1, directed = FALSE)
  dev.off()

  # Only the .3 edge survives the threshold
  expect_equal(nrow(result$chords), 1)
})

test_that("plot_chord chord_color_by options work", {
  mat <- matrix(c(0, .3, .2,
                  .4, 0, .1,
                  .3, .2, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, chord_color_by = "source"))
  dev.off()

  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, chord_color_by = "target"))
  dev.off()

  # Custom color vector
  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, chord_color_by = "steelblue"))
  dev.off()
})

test_that("plot_chord segment_colors parameter works", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, segment_colors = c("red", "green", "blue")))
  dev.off()

  # Single color recycled
  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, segment_colors = "gray"))
  dev.off()
})

test_that("plot_chord labels = FALSE suppresses labels", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, labels = FALSE))
  dev.off()
})

test_that("plot_chord custom labels work", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE)

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat, labels = c("X", "Y", "Z"))
  dev.off()

  expect_equal(nrow(result$segments), 3)
})

test_that("plot_chord with matrix without dimnames generates numeric labels", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE)

  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat))
  dev.off()
})

test_that("plot_chord title parameter works", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, title = "Test Chord"))
  dev.off()
})

test_that("plot_chord start_angle and clockwise work", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  r1 <- plot_chord(mat, start_angle = 0, clockwise = TRUE)
  dev.off()

  png(tempfile(fileext = ".png"))
  r2 <- plot_chord(mat, start_angle = 0, clockwise = FALSE)
  dev.off()

  # Different direction → different segment positions
  expect_false(identical(r1$segments$end, r2$segments$end))
})

test_that("plot_chord handles cograph_network input", {
  skip_if_not(exists("cograph", mode = "function"))
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  net <- cograph(mat)

  png(tempfile(fileext = ".png"))
  result <- plot_chord(net)
  dev.off()

  expect_equal(nrow(result$segments), 3)
})

test_that("plot_chord handles tna input via mock", {
  mock_tna <- list(
    weights = matrix(c(0.0, 0.4, 0.6,
                       0.3, 0.0, 0.7,
                       0.5, 0.5, 0.0), 3, 3, byrow = TRUE,
                     dimnames = list(c("A", "B", "C"), c("A", "B", "C"))),
    labels = c("A", "B", "C"),
    inits = c(0.4, 0.3, 0.3),
    data = NULL
  )
  class(mock_tna) <- c("tna", "list")

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mock_tna)
  dev.off()

  expect_equal(nrow(result$segments), 3)
  expect_true(nrow(result$chords) > 0)
})

test_that("plot_chord handles large matrix", {
  n <- 15
  mat <- matrix(runif(n * n), n, n)
  diag(mat) <- 0
  mat <- (mat + t(mat)) / 2
  dimnames(mat) <- list(LETTERS[seq_len(n)], LETTERS[seq_len(n)])

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat, threshold = 0.3)
  dev.off()

  expect_equal(nrow(result$segments), n)
})

test_that("plot_chord handles negative weights", {
  mat <- matrix(c(0, .3, -.2,
                  .3, 0, .1,
                  -.2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat, directed = FALSE)
  dev.off()

  # All edges shown (abs used internally)
  expect_equal(nrow(result$chords), 3)
})

test_that("plot_chord segment_pad and segment_width parameters work", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, segment_pad = 0.1, segment_width = 0.15))
  dev.off()
})

test_that("plot_chord chord_alpha and chord_border work", {
  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, chord_alpha = 0.8, chord_border = "gray50"))
  dev.off()
})

test_that("plot_chord label_threshold hides small labels", {
  mat <- matrix(c(0, .3, .001,
                  .3, 0, .001,
                  .001, .001, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  # Node C has very low flow → should be suppressed with high threshold
  png(tempfile(fileext = ".png"))
  expect_no_error(plot_chord(mat, label_threshold = 0.1))
  dev.off()
})

test_that("plot_chord returns correct structure", {
  mat <- matrix(c(0, .3, .2,
                  .4, 0, .1,
                  .3, .2, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat)
  dev.off()

  # Check segments structure
  expect_true(all(c("node", "start", "end", "mid", "flow") %in%
                    names(result$segments)))

  # Check chords structure
  expect_true(all(c("from", "to", "from_start", "from_end",
                     "to_start", "to_end", "weight") %in%
                    names(result$chords)))
})

test_that("plot_chord with 2x2 matrix works", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2,
                dimnames = list(c("A", "B"), c("A", "B")))

  png(tempfile(fileext = ".png"))
  result <- plot_chord(mat)
  dev.off()

  expect_equal(nrow(result$segments), 2)
  expect_true(nrow(result$chords) > 0)
})

test_that("plot_chord handles igraph input", {
  skip_if_not_installed("igraph")

  mat <- matrix(c(0, .3, .2,
                  .3, 0, .1,
                  .2, .1, 0), 3, 3, byrow = TRUE,
                dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected",
                                            weighted = TRUE)

  png(tempfile(fileext = ".png"))
  result <- plot_chord(g)
  dev.off()

  expect_equal(nrow(result$segments), 3)
})

test_that("plot_chord rejects non-square matrix", {
  mat <- matrix(1:6, 2, 3)
  expect_error(plot_chord(mat))
})

test_that("plot_chord rejects non-numeric input", {
  expect_error(plot_chord("not a matrix"))
})
