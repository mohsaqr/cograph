# plot_mcml(): layer_spacing, and making the figure follow the image height.
#
# Regression: `layer_spacing` was documented as "vertical distance between the
# bottom and top layers" but was assigned and never read -- 2 and 30 drew the
# same figure. And because the plot is drawn with asp = 1 from fixed layout
# numbers, a taller image changed nothing but the white space: 8x12 in used 59%
# of its height, 8x16 in 44%. These tests assert the limits the function hands
# to plot.window(), which is the quantity that was stuck.

mcml_layer_matrix <- function() {
  set.seed(7)
  m <- matrix(runif(81, 0, 0.4), 9, 9)
  diag(m) <- 0
  dimnames(m) <- list(letters[1:9], letters[1:9])
  m
}
mcml_layer_clusters <- list(A = c("a", "b", "c"), B = c("d", "e", "f", "g"),
                            C = c("h", "i"))

# Draw once on a device of the given size; report the limits the plot asked
# for and the share of the device's data window they fill (asp = 1 pads the
# slack dimension, and that padding is the white space).
mcml_limits <- function(width, height, ..., fn = plot_mcml) {
  seen <- new.env()
  suppressMessages(trace(graphics::plot.window, print = FALSE,
    tracer = bquote(assign("lim", list(xlim = xlim, ylim = ylim), envir = .(seen)))))
  on.exit(suppressMessages(untrace(graphics::plot.window)), add = TRUE)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = width, height = height, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  fn(mcml_layer_matrix(), mcml_layer_clusters, ...)
  usr <- graphics::par("usr")
  list(width = diff(seen$lim$xlim), height = diff(seen$lim$ylim),
       used_w = diff(seen$lim$xlim) / diff(usr[1:2]),
       used_h = diff(seen$lim$ylim) / diff(usr[3:4]))
}

test_that("by default the figure has a fixed shape, whatever the image height", {
  # The default must not move: existing figures depend on it.
  short <- mcml_limits(8, 8)
  tall <- mcml_limits(8, 16)
  expect_equal(tall$height, short$height)
  expect_equal(tall$width, short$width)
  expect_lt(tall$used_h, 0.6) # which is the complaint: half the page is empty
})

test_that("layer_spacing = \"fill\" makes the figure follow the image height", {
  heights <- c(8, 12, 16, 20)
  fills <- lapply(heights, \(h) mcml_limits(8, h, layer_spacing = "fill"))
  content_h <- vapply(fills, \(g) g$height, numeric(1))
  # taller image -> taller figure, strictly
  expect_true(all(diff(content_h) > 0))
  # and it uses the page in BOTH directions (R's own 4% axis padding aside)
  expect_true(all(vapply(fills, \(g) g$used_h, numeric(1)) > 0.9))
  expect_true(all(vapply(fills, \(g) g$used_w, numeric(1)) > 0.9))
  # the width of the layout is untouched: only the gap between layers grows
  expect_equal(vapply(fills, \(g) g$width, numeric(1)),
               rep(mcml_limits(8, 8)$width, length(heights)))
  # figure height tracks image height proportionally
  expect_equal(content_h[4] / content_h[2], 20 / 12, tolerance = 0.03)
})

test_that("\"fill\" never squeezes the layers on a wide image", {
  wide_default <- mcml_limits(10, 4)
  wide_fill <- mcml_limits(10, 4, layer_spacing = "fill")
  expect_equal(wide_fill$height, wide_default$height)
  expect_equal(wide_fill$width, wide_default$width)
})

test_that("\"fill\" accounts for the margins a title and subtitle take", {
  g <- mcml_limits(8, 14, layer_spacing = "fill", title = "Title", subtitle = "Sub")
  expect_gt(g$used_h, 0.9)
  expect_gt(g$used_w, 0.9)
  expect_lt(g$height, mcml_limits(8, 14, layer_spacing = "fill")$height)
})

test_that("a numeric layer_spacing sets the distance between the layers", {
  # Regression: 2 and 30 used to draw the identical figure.
  near <- mcml_limits(8, 12, layer_spacing = 6)
  far <- mcml_limits(8, 12, layer_spacing = 12)
  expect_gt(far$height, near$height)
  # the top layer moves by exactly the difference; nothing else changes
  expect_equal(far$height - near$height, 12 - 6)
  expect_equal(far$width, near$width)
  # it overrides inter_layer_gap
  expect_equal(mcml_limits(8, 12, layer_spacing = 6, inter_layer_gap = 3)$height,
               near$height)
})

test_that("inter_layer_gap still drives the automatic layout", {
  expect_gt(mcml_limits(8, 12, inter_layer_gap = 2)$height,
            mcml_limits(8, 12)$height)
})

test_that("layer_spacing rejects anything else, by class", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 5, height = 5, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  for (bad in list("tall", -1, 0, c(4, 5), NA_real_, Inf, TRUE)) { # labelled
    expect_error(
      plot_mcml(mcml_layer_matrix(), mcml_layer_clusters, layer_spacing = bad),
      class = "cograph_bad_layer_spacing"
    )
  }
})

test_that("a layer_spacing that overlaps the layers warns, by class, and still draws", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 5, height = 5, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  expect_warning(
    plot_mcml(mcml_layer_matrix(), mcml_layer_clusters, layer_spacing = 0.5),
    class = "cograph_layers_overlap"
  )
})

test_that("plot_mcml_donut shares the same control", {
  donut <- cograph:::plot_mcml_donut
  expect_lt(mcml_limits(8, 16, fn = donut)$used_h, 0.6)
  expect_gt(mcml_limits(8, 16, layer_spacing = "fill", fn = donut)$used_h, 0.9)
  expect_equal(mcml_limits(8, 12, layer_spacing = 12, fn = donut)$height -
                 mcml_limits(8, 12, layer_spacing = 6, fn = donut)$height, 6)
})

# ---- the helper on its own --------------------------------------------------

test_that(".mcml_top_layer_y solves the aspect equation, hand-checked", {
  top_y <- cograph:::.mcml_top_layer_y
  # NULL -> the automatic position, untouched
  expect_equal(top_y(NULL, auto_y = 3.9, content_width = 8, fixed_height = 5,
                     bottom_top = 2.1), 3.9)
  # a number -> itself
  expect_equal(top_y(7, auto_y = 3.9, content_width = 8, fixed_height = 5,
                     bottom_top = 2.1), 7)

  # "fill" on a 4 x 8 in plot region (no margins): pin ratio is exactly 2, so
  # content height must be 2 * 8 = 16, of which 5 is fixed -> y = 11.
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 4, height = 8, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  # 1e-5, not testthat's default 1.5e-8: the device reports par("pin") in
  # inches with float rounding, so "a 4 x 8 in region" has a ratio of 2 only to
  # ~1e-7 on Windows (CI: 11.0000028). The layout needs nothing finer.
  pin_tolerance <- 1e-5
  expect_equal(top_y("fill", auto_y = 3.9, content_width = 8, fixed_height = 5,
                     bottom_top = 2.1), 11, tolerance = pin_tolerance)
  # and exactly what the device's own region implies, to full precision
  pin <- graphics::par("pin")
  expect_equal(top_y("fill", auto_y = 3.9, content_width = 8, fixed_height = 5,
                     bottom_top = 2.1), 8 * pin[2] / pin[1] - 5)
  # never tighter than automatic
  expect_equal(top_y("fill", auto_y = 20, content_width = 8, fixed_height = 5,
                     bottom_top = 2.1), 20)
  # scale invariance: doubling every length doubles the answer
  expect_equal(top_y("fill", auto_y = 7.8, content_width = 16, fixed_height = 10,
                     bottom_top = 4.2),
               2 * top_y("fill", auto_y = 3.9, content_width = 8, fixed_height = 5,
                         bottom_top = 2.1))
})
