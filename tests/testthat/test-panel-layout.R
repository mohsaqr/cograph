test_that("panel_layout(c(nrow, ncol)) sets par(mfrow) and returns restorable par", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  before <- graphics::par("mfrow")
  op <- panel_layout(c(2, 3))
  after <- graphics::par("mfrow")
  graphics::par(op)
  restored <- graphics::par("mfrow")

  expect_identical(after, c(2L, 3L))
  expect_identical(restored, before)
})

test_that("panel_layout(matrix) drives graphics::layout()", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent({
    op <- panel_layout(matrix(c(1, 1, 2, 3), 2, 2))
    graphics::par(op)
  })

  # widths / heights forwarded without error
  expect_silent({
    op <- panel_layout(matrix(c(1, 2), 1, 2),
                       widths = c(2, 1), heights = 1)
    graphics::par(op)
  })
})

test_that("panel_layout() validates inputs", {
  expect_error(panel_layout("nope"),
               "c\\(nrow, ncol\\) or a numeric matrix")
  expect_error(panel_layout(c(0, 1)), "positive integer")
  expect_error(panel_layout(c(1, NA)), "positive integer")
  expect_error(panel_layout(c(1, 1), mar = c(1, 1)), "length 4")
  expect_error(panel_layout(matrix("a", 1, 1)), "must be numeric")
})

test_that("panel_layout() rejects non-integer numerics for grid spec", {
  # Reviewer flagged: as.integer(1.7) silently truncates to 1L, so prior
  # validation accepted c(1.7, 2.9) and produced a 1x2 grid without warning.
  expect_error(panel_layout(c(1.7, 2.9)), "positive integer")
  expect_error(panel_layout(c(2, 2.5)),   "positive integer")
})

test_that("panel_layout() rejects widths/heights with vector spec", {
  expect_error(panel_layout(c(2, 2), widths = c(1, 2)),
               "only valid when `spec` is a matrix")
  expect_error(panel_layout(c(2, 2), heights = c(1, 2)),
               "only valid when `spec` is a matrix")
})

test_that("panel_layout() rejects degenerate matrix specs", {
  expect_error(panel_layout(matrix(0, 2, 2)),
               "at least one positive cell")
  expect_error(panel_layout(matrix(-1, 1, 1)),
               "non-negative integers")
})

test_that("panel_layout(<matrix>) returns a snapshot that resets layout()", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(mfrow = c(1, 1))
  prior_mfrow <- graphics::par("mfrow")
  op <- panel_layout(matrix(c(1, 1, 2, 3), 2, 2))

  # The matrix path uses graphics::layout(); par("mfrow") becomes c(2, 2)
  # because the matrix has 2 rows and 2 cols, but the *snapshot* must
  # carry the pre-call mfrow so par(op) clears layout() state on restore.
  expect_identical(op$mfrow, prior_mfrow)

  graphics::par(op)
  expect_identical(graphics::par("mfrow"), prior_mfrow)
})

test_that("panel_layout() accepts custom margins", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  op <- panel_layout(c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
  expect_identical(graphics::par("mar"), c(0.5, 0.5, 1, 0.5))
  graphics::par(op)
})
