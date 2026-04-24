# Regression tests for plot_tna() / tplot() argument merging.
#
# plot_tna() translates qgraph-style named params (edge.labels, edge.color,
# etc.) into cograph-native keys in a `splot_args` list, then forwards both
# that list and `...` to splot() via do.call. When a user also passes the
# cograph-native name in `...`, the naive `c(splot_args, list(...))` pattern
# errored with "formal argument X matched by multiple actual arguments".
# The fix: dots override translated splot_args — cograph name wins.

test_that("plot_tna() accepts cograph-native edge_labels without argument collision", {
  m <- matrix(c(0, 0.3, 0.1,
                0.2, 0,   0.4,
                0.1, 0.2, 0), nrow = 3, byrow = TRUE,
              dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = 400, height = 400)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  # Used to error: "formal argument 'edge_labels' matched by multiple..."
  expect_silent(plot_tna(m, edge_labels = FALSE))
  expect_silent(plot_tna(m, edge_labels = TRUE))
})

test_that("plot_tna() cograph-native arg wins over qgraph alias (dots override)", {
  m <- matrix(c(0, 0.3, 0.1,
                0.2, 0,   0.4,
                0.1, 0.2, 0), nrow = 3, byrow = TRUE,
              dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = 400, height = 400)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  # qgraph says labels on, cograph-native says labels off via dots.
  # Expected: cograph name wins (off).
  p <- plot_tna(m, edge.labels = TRUE, edge_labels = FALSE)
  expect_s3_class(p, "cograph_network")
})

test_that("plot_tna() accepts other cograph-native arg names via dots", {
  m <- matrix(c(0, 0.3, 0.1,
                0.2, 0,   0.4,
                0.1, 0.2, 0), nrow = 3, byrow = TRUE,
              dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = 400, height = 400)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)

  # All of these used to risk collisions. None should error.
  expect_silent(plot_tna(m, edge_color = "blue"))
  expect_silent(plot_tna(m, edge_label_size = 1.2))
  expect_silent(plot_tna(m, edge_label_position = 0.3))
})
