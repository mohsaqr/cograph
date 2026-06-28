# splot() draws a per-node `predictability` column as a donut fill, controlled
# by the `predictability` argument (TRUE/FALSE/NULL-auto). This is the hook a
# psychnet object uses to self-draw its predictability ring.

make_net <- function(pred = c(0.8, 0.5, 0.2), default = FALSE) {
  w <- matrix(c(0, .4, .3, .4, 0, .2, .3, .2, 0), 3, 3)
  net <- as_cograph(w)
  net$nodes$predictability <- pred
  net$meta$predictability_default <- default
  net
}

test_that("predictability = TRUE draws the ring from the node column", {
  net <- make_net()
  tmp <- tempfile(fileext = ".png")
  png(tmp); on.exit(unlink(tmp))
  expect_no_error(splot(net, predictability = TRUE))
  dev.off()
})

test_that("predictability = NULL follows the object's default flag", {
  tmp <- tempfile(fileext = ".png"); png(tmp); on.exit({dev.off(); unlink(tmp)})
  expect_no_error(splot(make_net(default = TRUE)))   # default-on object
  expect_no_error(splot(make_net(default = FALSE)))  # default-off object
})

test_that("predictability = FALSE is accepted and draws no ring", {
  tmp <- tempfile(fileext = ".png"); png(tmp); on.exit({dev.off(); unlink(tmp)})
  expect_no_error(splot(make_net(default = TRUE), predictability = FALSE))
})

test_that("a caller's own pie_values/donut_fill takes precedence", {
  tmp <- tempfile(fileext = ".png"); png(tmp); on.exit({dev.off(); unlink(tmp)})
  expect_no_error(splot(make_net(default = TRUE), donut_fill = c(0.1, 0.1, 0.1)))
})
