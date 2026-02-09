test_that("built-in shapes exist", {
  shapes <- list_shapes()

  expect_true("circle" %in% shapes)
  expect_true("square" %in% shapes)
  expect_true("triangle" %in% shapes)
  expect_true("diamond" %in% shapes)
  expect_true("star" %in% shapes)
  expect_true("heart" %in% shapes)
})

test_that("get_shape returns function", {
  circle_fn <- get_shape("circle")

  expect_true(is.function(circle_fn))
})

test_that("shape functions return grobs", {
  skip_if_not_installed("grid")

  circle_grob <- draw_circle(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(circle_grob, "grob")

  square_grob <- draw_square(0.5, 0.5, 0.1, "red", "black", 1)
  expect_s3_class(square_grob, "grob")

  triangle_grob <- draw_triangle(0.5, 0.5, 0.1, "green", "black", 1)
  expect_s3_class(triangle_grob, "grob")
})

test_that("custom shape can be registered", {
  custom_shape <- function(x, y, size, fill, border_color, border_width, ...) {
    grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size * 2, "npc"),
      gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
    )
  }

  register_shape("big_circle", custom_shape)

  retrieved <- get_shape("big_circle")
  expect_true(is.function(retrieved))
})
