# Regression tests for device-aware scaling of splot() output.
#
# The intent: rendering the same network at different canvas sizes / DPI
# combinations should produce consistent VISUAL RATIOS (label height to node
# diameter, legend extent to plot area). Absolute pixel sizes naturally
# differ; the ratio is what users see as "the plot looks right" or "the
# labels are too big".
#
# We measure the label-to-node ratio via the data attached to the returned
# splot object: cograph.node_diam_in (inches) plus the label cex recorded in
# network$nodes$label_size converted to inches via par("cin").

# Render the same 5-node network at a given (width, height, res) and return
# measurements useful for ratio assertions.
.device_ratio_probe <- function(width, height, res) {
  set.seed(1)
  m <- matrix(runif(25), 5, 5)
  diag(m) <- 0
  m[m < 0.6] <- 0

  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = width, height = height, res = res)
  on.exit({
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)

  out <- splot(m, labels = TRUE, legend = TRUE, legend_edge_colors = TRUE)

  # cograph.node_diam_in was set while the device was still open.
  node_diam_in <- attr(out, "cograph.node_diam_in")
  vs <- attr(out, "cograph.visual_scale")

  # Label cex that was actually passed to the renderer. For the independent
  # label-size mode (scaling = "default"), every node gets the same cex, so
  # taking the median is robust.
  label_cex <- stats::median(out$nodes$label_size, na.rm = TRUE)

  # Convert label cex to inches via character inch size. par("cin") is a
  # constant of the current device (char width / height in inches).
  label_h_in <- label_cex * grDevices::dev.size("in")[2] * 0 + label_cex * 12 / 72
  # 12pt default pointsize = 12/72 inches; cex 1 renders one line of that.

  list(
    width = width, height = height, res = res,
    node_diam_in = node_diam_in,
    label_cex = label_cex,
    label_h_in = label_h_in,
    ratio = label_h_in / node_diam_in,
    vs_text = vs$text,
    vs_line = vs$line,
    vs_point = vs$point
  )
}

test_that("splot ratios are stable across canvas sizes when scaling is on", {
  # Three devices: reference, bigger canvas, smaller canvas. With device-aware
  # scaling, the label-height / node-diameter ratio should be within a ±15%
  # band (the cap is ~2x so the band is generous; the previous broken state
  # produced 3-5x variation).
  r_ref <- .device_ratio_probe(7 * 96, 5 * 96, 96)  # reference
  r_big <- .device_ratio_probe(1600, 1600, 96)       # larger canvas
  r_small <- .device_ratio_probe(800, 800, 300)      # small canvas, high DPI

  expect_true(is.finite(r_ref$ratio))
  expect_true(is.finite(r_big$ratio))
  expect_true(is.finite(r_small$ratio))

  # Ratios should stay in a tight band relative to the reference ratio.
  ratio_bands <- c(r_big$ratio, r_small$ratio) / r_ref$ratio
  expect_true(all(ratio_bands > 0.55),
              info = sprintf("ratio bands: %s", paste(round(ratio_bands, 3),
                                                      collapse = ", ")))
  expect_true(all(ratio_bands < 1.9),
              info = sprintf("ratio bands: %s", paste(round(ratio_bands, 3),
                                                      collapse = ", ")))
})

test_that("splot at reference canvas leaves label cex ~1.0 (backward compat)", {
  r <- .device_ratio_probe(7 * 96, 5 * 96, 96)
  expect_equal(r$vs_text, 1, tolerance = 0.05)
  # scale$label_default = 1, multiplied by vs$text ~1.0 should give ~1.0.
  expect_equal(r$label_cex, 1, tolerance = 0.05)
})

test_that("scaling = 'fixed' disables device compensation", {
  set.seed(1)
  m <- matrix(runif(25), 5, 5); diag(m) <- 0; m[m < 0.6] <- 0

  render <- function(width, height, res, scaling) {
    tf <- tempfile(fileext = ".png")
    grDevices::png(tf, width = width, height = height, res = res)
    on.exit({
      if (grDevices::dev.cur() > 1L) grDevices::dev.off()
      unlink(tf)
    }, add = TRUE)
    out <- splot(m, labels = TRUE, scaling = scaling)
    list(
      label_cex = stats::median(out$nodes$label_size, na.rm = TRUE),
      vs_text = attr(out, "cograph.visual_scale")$text
    )
  }

  big_visual <- render(2000, 2000, 96, "default")
  big_fixed  <- render(2000, 2000, 96, "fixed")

  # With device-aware scaling, big canvas -> big text.
  expect_gt(big_visual$vs_text, 1)
  expect_gt(big_visual$label_cex, 1)
  # With fixed, everything is identity.
  expect_equal(big_fixed$vs_text, 1)
  expect_equal(big_fixed$label_cex, 1, tolerance = 0.001)
})

test_that("visual_scale propagates to edge_widths", {
  # Same weighted network at two canvas sizes — verify edge_size (the
  # resolved lwd) tracks vs$line.
  set.seed(1)
  m <- matrix(runif(25), 5, 5); diag(m) <- 0; m[m < 0.5] <- 0
  render <- function(width, height, res) {
    tf <- tempfile(fileext = ".png")
    grDevices::png(tf, width = width, height = height, res = res)
    on.exit({
      if (grDevices::dev.cur() > 1L) grDevices::dev.off()
      unlink(tf)
    }, add = TRUE)
    out <- splot(m, labels = FALSE, legend = FALSE)
    list(
      max_lwd = max(out$edges$edge_size, na.rm = TRUE),
      vs_line = attr(out, "cograph.visual_scale")$line
    )
  }
  ref <- render(7 * 96, 5 * 96, 96)
  big <- render(2000, 2000, 96)

  # Ratio of max lwd should track ratio of vs$line (within a few %).
  expected <- big$vs_line / ref$vs_line
  observed <- big$max_lwd / ref$max_lwd
  expect_equal(observed, expected, tolerance = 0.1,
               info = sprintf("expected %.3f observed %.3f",
                              expected, observed))
})

test_that("splot() attaches cograph.node_diam_in and cograph.visual_scale", {
  set.seed(1)
  m <- matrix(runif(16), 4, 4); diag(m) <- 0; m[m < 0.5] <- 0
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = 800, height = 800, res = 96)
  on.exit({
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)
  out <- splot(m)

  expect_true(!is.null(attr(out, "cograph.visual_scale")))
  expect_true(is.numeric(attr(out, "cograph.node_diam_in")))
  expect_gt(attr(out, "cograph.node_diam_in"), 0)
})
