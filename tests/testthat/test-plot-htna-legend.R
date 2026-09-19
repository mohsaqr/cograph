# plot_htna() legend placement.
#
# Regression: the legend was drawn half on top of the network and half off the
# device, at every device size. Two causes. (1) plot_htna() reserved its legend
# margin with par(mar = ), which splot() overwrites with its own `margins`
# before drawing, so the band never existed. (2) the legend was pushed out with
# `inset = -0.05`, a fraction of the plot region, which moves the box by a
# sliver of its own height. "It runs without error" was true throughout, so
# these tests assert the geometry: the box legend() reports must not intersect
# the plot region and must lie inside the device.

htna_legend_matrix <- function() {
  set.seed(1)
  m <- matrix(runif(36, 0, 0.3), 6, 6)
  diag(m) <- 0
  dimnames(m) <- list(LETTERS[1:6], LETTERS[1:6])
  m
}
htna_legend_groups <- list(Human = c("A", "B", "C"), AI = c("D", "E", "F"))

# Draw once on a real device and report where the legend landed, in
# normalised device coordinates (0 = left/bottom edge, 1 = right/top edge).
legend_geometry <- function(width = 7, height = 7, ...) {
  original <- cograph:::.render_legend_base
  seen <- new.env()
  testthat::local_mocked_bindings(
    .render_legend_base = function(...) {
      out <- original(...)
      if (!isFALSE(list(...)$plot)) {
        r <- out$rect
        seen$legend_x <- graphics::grconvertX(c(r$left, r$left + r$w), "user", "ndc")
        seen$legend_y <- graphics::grconvertY(c(r$top - r$h, r$top), "user", "ndc")
        seen$plot_x <- graphics::grconvertX(graphics::par("usr")[1:2], "user", "ndc")
        seen$plot_y <- graphics::grconvertY(graphics::par("usr")[3:4], "user", "ndc")
        seen$mar <- graphics::par("mar")
        seen$cex <- list(...)$cex
      }
      invisible(out)
    },
    .package = "cograph"
  )
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = width, height = height, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  plot_htna(htna_legend_matrix(), node_list = htna_legend_groups, ...)
  as.list(seen)
}

overlaps_plot <- function(g) {
  g$legend_x[1] < g$plot_x[2] && g$legend_x[2] > g$plot_x[1] &&
    g$legend_y[1] < g$plot_y[2] && g$legend_y[2] > g$plot_y[1]
}
on_device <- function(g, tol = 1e-8) {
  all(c(g$legend_x, g$legend_y) >= -tol, c(g$legend_x, g$legend_y) <= 1 + tol)
}

test_that("the default bottom legend is clear of the plot and on the device, at any size", {
  sizes <- list(c(3, 3), c(5, 5), c(7, 7), c(8, 7), c(11, 10), c(10, 4))
  for (size in sizes) { # loop only to label each expectation with its size
    g <- legend_geometry(width = size[1], height = size[2])
    expect_false(overlaps_plot(g), label = sprintf("%gx%g in: legend overlaps plot", size[1], size[2]))
    expect_true(on_device(g), label = sprintf("%gx%g in: legend on device", size[1], size[2]))
  }
})

test_that("every side position gets a band of its own", {
  for (side in c("bottom", "top", "left", "right")) { # labelled expectations
    g <- legend_geometry(legend_position = side)
    expect_false(overlaps_plot(g), label = paste(side, "legend overlaps plot"))
    expect_true(on_device(g), label = paste(side, "legend on device"))
  }
  g <- legend_geometry(legend_position = "bottom")
  expect_lt(g$legend_y[2], g$plot_y[1] + 1e-8)
  g <- legend_geometry(legend_position = "top")
  expect_gt(g$legend_y[1], g$plot_y[2] - 1e-8)
  g <- legend_geometry(legend_position = "left")
  expect_lt(g$legend_x[2], g$plot_x[1] + 1e-8)
  g <- legend_geometry(legend_position = "right")
  expect_gt(g$legend_x[1], g$plot_x[2] - 1e-8)
})

test_that("the reserved margin is the one the network is actually drawn with", {
  # The original defect: par('mar') said 6.5 while the plot filled the page.
  g <- legend_geometry(width = 7, height = 7)
  bottom_margin_in <- g$mar[1] * graphics::par("csi")
  expect_gt(g$plot_y[1], 0.05)
  expect_equal(g$plot_y[1] * 7, bottom_margin_in, tolerance = 0.25)
})

test_that("a vertical legend and many groups still fit their band", {
  g <- legend_geometry(legend_horiz = FALSE)
  expect_false(overlaps_plot(g))
  expect_true(on_device(g))

  m <- htna_legend_matrix()
  many <- list(`A long group name` = "A", `Another long one` = "B", Third = "C",
               Fourth = "D", Fifth = "E", Sixth = "F")
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 4, height = 4, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  expect_no_error(plot_htna(m, node_list = many, legend_position = "bottom"))
})

test_that("the legend shrinks rather than overlapping when the band cannot hold it", {
  many <- list(`A long group name` = "A", `Another long one` = "B", Third = "C",
               Fourth = "D", Fifth = "E", Sixth = "F")
  original <- cograph:::.render_legend_base
  seen <- new.env()
  testthat::local_mocked_bindings(
    .render_legend_base = function(...) {
      out <- original(...)
      if (!isFALSE(list(...)$plot)) {
        r <- out$rect
        seen$x <- graphics::grconvertX(c(r$left, r$left + r$w), "user", "ndc")
        seen$cex <- list(...)$cex
      }
      invisible(out)
    },
    .package = "cograph"
  )
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 3, height = 3, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  plot_htna(htna_legend_matrix(), node_list = many, legend_size = 1.5)
  expect_lt(seen$cex, 1.5)          # six names do not fit 3 inches at this size
  expect_gte(seen$x[1], -1e-8)
  expect_lte(seen$x[2], 1 + 1e-8)
})

test_that("legend_size sets the legend text, and defaults to splot's 0.8", {
  # Regression: the size was hard-coded at cex 1.4 -- larger than the node
  # labels the legend explains -- with no argument to change it.
  expect_equal(formals(plot_htna)$legend_size, formals(splot)$legend_size)
  identity_scale <- function() list(text = 1, point = 1, line = 1, scale = 1)
  testthat::local_mocked_bindings(.get_current_visual_scale = identity_scale,
                                  .package = "cograph")
  expect_equal(legend_geometry()$cex, 0.8)
  expect_equal(legend_geometry(legend_size = 0.5)$cex, 0.5)
  # `scale` still compensates for high-resolution output
  expect_equal(legend_geometry(legend_size = 1, scale = 4)$cex, 1 / sqrt(4))
  # a larger legend gets a larger band, and stays clear of the plot
  small <- legend_geometry(legend_size = 0.5)
  large <- legend_geometry(legend_size = 1.2)
  expect_gt(large$plot_y[1], small$plot_y[1])
  expect_false(overlaps_plot(large))
  expect_true(on_device(large))
})

test_that("legend_size rejects anything but one positive number", {
  m <- htna_legend_matrix()
  for (bad in list(0, -1, NA_real_, Inf, c(1, 2), "big", NULL)) { # labelled
    expect_error(plot_htna(m, node_list = htna_legend_groups, legend_size = bad),
                 class = "cograph_bad_legend_size")
  }
})

test_that("a margin the caller passes is left alone", {
  g <- legend_geometry(mar = c(9, 1, 1, 1))
  expect_equal(g$mar, c(9, 1, 1, 1))
  g <- legend_geometry(margins = c(9, 1, 1, 1))
  expect_equal(g$mar[1], 9)
})

test_that("a title and a top legend share the top margin without colliding", {
  g <- legend_geometry(legend_position = "top", title = "A title")
  expect_false(overlaps_plot(g))
  expect_true(on_device(g))
  # the outer 1.5 lines belong to the title
  title_in <- 1.5 * graphics::par("csi")
  expect_lte(g$legend_y[2], 1 - title_in / 7 + 1e-8)
})

test_that("corner positions still draw inside the plot box", {
  g <- legend_geometry(legend_position = "topright")
  expect_true(overlaps_plot(g))
  expect_true(on_device(g))
})

test_that("plot_htna restores the caller's margins", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 5, height = 5, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  graphics::par(mar = c(3, 2, 1, 4))
  plot_htna(htna_legend_matrix(), node_list = htna_legend_groups)
  expect_equal(graphics::par("mar"), c(3, 2, 1, 4))
})

# ---- the band helpers on their own -----------------------------------------

test_that(".legend_band_lines counts the title as exactly one row", {
  # Regression: `rows + !is.null(title) + 1.5` parsed as
  # `rows + !(is.null(title) + 1.5)`, so the title (and the padding) vanished
  # and the band came out at a third of the height the legend needs.
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 5, height = 5, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  identity_scale <- list(text = 1)
  band <- function(...) cograph:::.legend_band_lines(..., visual_scale = identity_scale)
  one_row <- graphics::par("cin")[2] / graphics::par("csi")

  with_title <- band(c("a", "b"), side = "bottom", cex = 1, title = "Groups", horiz = TRUE)
  without <- band(c("a", "b"), side = "bottom", cex = 1, title = NULL, horiz = TRUE)
  expect_equal(with_title - without, one_row)
  # hand-computed: (1 entry row + 1 title row + 1.5 padding) rows, + 0.5 line
  expect_equal(with_title, 3.5 * one_row + 0.5)

  # monotone in rows, linear in cex, wider for longer names
  expect_gt(band(letters[1:6], side = "bottom", horiz = FALSE),
            band(letters[1:2], side = "bottom", horiz = FALSE))
  expect_equal(band("a", side = "bottom", cex = 2, horiz = TRUE) - 0.5,
               2 * (band("a", side = "bottom", cex = 1, horiz = TRUE) - 0.5))
  expect_gt(band("a much longer group name", side = "right"), band("a", side = "right"))
})

test_that(".render_legend_in_band keeps the box inside the band on every side", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 6, height = 6, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  for (side in c("bottom", "top", "left", "right")) { # labelled expectations
    graphics::par(mar = c(6, 6, 6, 6))
    graphics::plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
    usr <- graphics::par("usr")
    rect <- cograph:::.render_legend_in_band(
      list(legend = c("Human", "AI"), pch = 21, cex = 1, title = "Groups", bty = "n"),
      side = side
    )$rect
    fig_x <- graphics::grconvertX(c(0, 1), "nfc", "user")
    fig_y <- graphics::grconvertY(c(0, 1), "nfc", "user")
    expect_gte(rect$left, fig_x[1] - 1e-8, label = paste(side, "left edge"))
    expect_lte(rect$left + rect$w, fig_x[2] + 1e-8, label = paste(side, "right edge"))
    expect_gte(rect$top - rect$h, fig_y[1] - 1e-8, label = paste(side, "bottom edge"))
    expect_lte(rect$top, fig_y[2] + 1e-8, label = paste(side, "top edge"))
    outside <- switch(side,
      bottom = rect$top <= usr[3] + 1e-8,
      top    = rect$top - rect$h >= usr[4] - 1e-8,
      left   = rect$left + rect$w <= usr[1] + 1e-8,
      right  = rect$left >= usr[2] - 1e-8
    )
    expect_true(outside, label = paste(side, "legend outside the plot region"))
  }
})

test_that("without a legend band the figure is drawn exactly as before (htna's path)", {
  # htna::plot_htna() calls plot_htna(legend = FALSE) and draws its own legend.
  # The legend fix must not move that figure: the margin that reaches splot()
  # stays splot's own default, which is what was always really in effect.
  seen <- new.env()
  original <- cograph::tplot
  testthat::local_mocked_bindings(
    tplot = function(...) {
      seen$mar <- list(...)$mar
      original(...)
    },
    .package = "cograph"
  )
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 6, height = 6, units = "in", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)
  m <- htna_legend_matrix()
  plot_htna(m, node_list = htna_legend_groups, legend = FALSE)
  expect_equal(seen$mar, eval(formals(splot)$margins))
  plot_htna(m, node_list = htna_legend_groups, legend_position = "topright")
  expect_equal(seen$mar, eval(formals(splot)$margins))
  # a side legend changes only its own side
  plot_htna(m, node_list = htna_legend_groups, legend_position = "bottom")
  expect_equal(seen$mar[2:4], eval(formals(splot)$margins)[2:4])
  expect_gt(seen$mar[1], 1)
})
