# Unit tests for the device-aware visual-scale helper.
#
# compute_visual_scale() reads the active graphics device and returns
# multipliers that every plotter in the splot() family applies to cex, pt.cex,
# and lwd. The goal is that label-to-node, legend-to-plot, and edge-to-node
# *ratios* stay stable as canvas size / DPI change.
#
# See R/visual-scale.R and R/scale-constants.R for the definitions.

# Helper: open a png device at a known size and run `expr` with the device
# open. Sets tight margins (splot uses c(0.1, 0.1, 0.1, 0.1) by default) so
# par("pin") closely tracks dev.size and the scale reflects the canvas, not
# a plot region shrunk by generous default margins.
with_png <- function(width, height, res, expr, mar = c(0.1, 0.1, 0.1, 0.1)) {
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = width, height = height, res = res)
  on.exit({
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)
  # Some tiny canvases cannot accommodate default margins; tight margins here
  # match splot's rendering setup.
  tryCatch({
    graphics::par(mar = mar)
    graphics::plot.new()
  }, error = function(e) {
    # Fall back without plot.new — compute_visual_scale's dev.size() fallback
    # still produces a reasonable value.
  })
  force(expr)
}

test_that("compute_visual_scale at reference canvas returns ~1.0", {
  # 7x5" @ 96dpi is the RStudio plot-pane default; geometric mean 5.92".
  with_png(7 * 96, 5 * 96, 96, {
    vs <- compute_visual_scale()
    expect_true(is.list(vs))
    expect_named(vs, c("text", "point", "line", "box", "scale", "raw",
                       "canvas", "pin", "cra", "ref"),
                 ignore.order = TRUE)
    # 0.996 at 6.96x4.96 pin — backward-compat assertion.
    expect_equal(vs$text, 1, tolerance = 0.05)
    expect_equal(vs$line, 1, tolerance = 0.05)
    expect_equal(vs$point, 1, tolerance = 0.05)
  })
})

test_that("compute_visual_scale scales up at larger canvas", {
  with_png(2000, 2000, 96, {  # ~20 inches
    vs <- compute_visual_scale()
    expect_true(vs$text > 1)
    expect_equal(vs$text, VISUAL_SCALE_CAP[2])  # Hits the upper cap
  })
})

test_that("compute_visual_scale scales down at smaller canvas", {
  # Small pixel-default canvas at 300 dpi = ~1.3" — below the 0.35 floor
  # (raw would be ~0.22, clamped to 0.35).
  with_png(400, 400, 300, {
    vs <- compute_visual_scale()
    expect_true(vs$text < 1)
    expect_equal(vs$text, VISUAL_SCALE_CAP[1])  # Hits the lower cap
  })
})

test_that("multipliers respect the cap bounds", {
  # Enormous canvas
  with_png(6000, 6000, 300, {
    vs <- compute_visual_scale()
    expect_lte(vs$text, VISUAL_SCALE_CAP[2])
    expect_lte(vs$line, VISUAL_SCALE_CAP[2])
    expect_lte(vs$point, VISUAL_SCALE_CAP[2])
  })
  # Tiny canvas
  with_png(200, 200, 300, {
    vs <- compute_visual_scale()
    expect_gte(vs$text, VISUAL_SCALE_CAP[1])
    expect_gte(vs$line, VISUAL_SCALE_CAP[1])
    expect_gte(vs$point, VISUAL_SCALE_CAP[1])
  })
})

test_that("text, point, line, box share one uniform multiplier", {
  # Research (qgraph + ggraph) converged on: separate multipliers per
  # element unlock the ratios we are trying to preserve. compute_visual_scale
  # returns one scale applied to every element.
  with_png(900, 900, 96, {
    vs <- compute_visual_scale()
    expect_gt(vs$scale, 1)
    expect_lt(vs$scale, VISUAL_SCALE_CAP[2])
    # All four should be identical — same multiplier.
    expect_equal(vs$text, vs$scale)
    expect_equal(vs$point, vs$scale)
    expect_equal(vs$line, vs$scale)
    expect_equal(vs$box, vs$scale)
  })
})

test_that("compute_visual_scale returns a usable fallback when no device is open", {
  # Close any device that might be hanging around, then call without opening a
  # new one.
  while (grDevices::dev.cur() > 1L) grDevices::dev.off()
  vs <- compute_visual_scale()
  expect_true(is.list(vs))
  expect_true(is.finite(vs$text))
  expect_true(is.finite(vs$line))
  expect_true(is.finite(vs$point))
})

test_that(".identity_visual_scale has all multipliers = 1", {
  vs <- .identity_visual_scale()
  expect_equal(vs$text, 1)
  expect_equal(vs$point, 1)
  expect_equal(vs$line, 1)
  expect_equal(vs$box, 1)
})

test_that(".resolve_visual_scale respects scaling = 'fixed'", {
  with_png(2000, 2000, 96, {
    vs_default <- .resolve_visual_scale("default")
    vs_fixed <- .resolve_visual_scale("fixed")
    expect_true(vs_default$text > 1)
    expect_equal(vs_fixed$text, 1)
    expect_equal(vs_fixed$line, 1)
    expect_equal(vs_fixed$point, 1)
  })
})

test_that(".resolve_visual_scale respects the options escape hatch", {
  old <- getOption("cograph.visual_scaling")
  on.exit(options(cograph.visual_scaling = old), add = TRUE)
  options(cograph.visual_scaling = FALSE)
  with_png(2000, 2000, 96, {
    vs <- .resolve_visual_scale("default")
    expect_equal(vs$text, 1)
    expect_equal(vs$line, 1)
  })
})

test_that("visual-scale env stash round-trips through get/clear", {
  .clear_current_visual_scale()
  expect_false(exists("current_visual_scale", envir = .cograph_env,
                      inherits = FALSE))
  custom <- list(text = 1.5, point = 1.3, line = 1.5, box = 1.5,
                 raw = 1.5, pin = c(8, 8), cra = c(10, 10),
                 ref = VISUAL_SCALE_REFERENCE)
  .set_current_visual_scale(custom)
  got <- .get_current_visual_scale()
  expect_equal(got$text, 1.5)
  .clear_current_visual_scale()
  # After clear, .get_current_visual_scale computes a fresh one (identity if
  # no device is available).
  got_after <- .get_current_visual_scale()
  expect_true(is.list(got_after))
})

test_that(".scale_by is the identity when vs is NULL or field missing", {
  expect_equal(.scale_by(5, NULL), 5)
  expect_equal(.scale_by(5, list(), "text"), 5)
  expect_equal(.scale_by(5, list(text = NA_real_), "text"), 5)
  expect_equal(.scale_by(5, list(text = 2), "text"), 10)
  expect_equal(.scale_by(c(1, 2, 3), list(text = 0.5), "text"), c(0.5, 1, 1.5))
})

test_that("compute_visual_scale validates its inputs", {
  expect_error(compute_visual_scale(reference = -1), "reference")
  expect_error(compute_visual_scale(cap = c(2, 1)), "cap")
  expect_error(compute_visual_scale(cap = c(0)), "cap")
})
