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

test_that("panel_layout() accepts custom margins", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  op <- panel_layout(c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
  expect_identical(graphics::par("mar"), c(0.5, 0.5, 1, 0.5))
  graphics::par(op)
})
