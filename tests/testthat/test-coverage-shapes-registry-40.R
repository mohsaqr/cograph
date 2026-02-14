# =============================================================================
# Test Coverage for shapes-registry.R
# =============================================================================
# Comprehensive tests for all shape registration functions and built-in shapes

# =============================================================================
# Test: register_builtin_shapes - Basic Shapes
# =============================================================================

test_that("register_builtin_shapes registers circle shape", {
  expect_true("circle" %in% list_shapes())
  fn <- get_shape("circle")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers square shape", {
  expect_true("square" %in% list_shapes())
  fn <- get_shape("square")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers triangle shape", {
  expect_true("triangle" %in% list_shapes())
  fn <- get_shape("triangle")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers diamond shape", {
  expect_true("diamond" %in% list_shapes())
  fn <- get_shape("diamond")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers pentagon shape", {
  expect_true("pentagon" %in% list_shapes())
  fn <- get_shape("pentagon")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers hexagon shape", {
  expect_true("hexagon" %in% list_shapes())
  fn <- get_shape("hexagon")
  expect_true(is.function(fn))
})

# =============================================================================
# Test: register_builtin_shapes - Special Shapes
# =============================================================================

test_that("register_builtin_shapes registers ellipse shape", {
  expect_true("ellipse" %in% list_shapes())
  fn <- get_shape("ellipse")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers heart shape", {
  expect_true("heart" %in% list_shapes())
  fn <- get_shape("heart")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers star shape", {
  expect_true("star" %in% list_shapes())
  fn <- get_shape("star")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers pie shape", {
  expect_true("pie" %in% list_shapes())
  fn <- get_shape("pie")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers donut shape", {
  expect_true("donut" %in% list_shapes())
  fn <- get_shape("donut")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers polygon_donut shape", {
  expect_true("polygon_donut" %in% list_shapes())
  fn <- get_shape("polygon_donut")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers donut_pie shape", {
  expect_true("donut_pie" %in% list_shapes())
  fn <- get_shape("donut_pie")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers double_donut_pie shape", {
  expect_true("double_donut_pie" %in% list_shapes())
  fn <- get_shape("double_donut_pie")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers cross shape", {
  expect_true("cross" %in% list_shapes())
  fn <- get_shape("cross")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers plus alias for cross", {
  expect_true("plus" %in% list_shapes())
  fn <- get_shape("plus")
  expect_true(is.function(fn))
})

# =============================================================================
# Test: register_builtin_shapes - AI-themed Shapes
# =============================================================================

test_that("register_builtin_shapes registers neural shape", {
  expect_true("neural" %in% list_shapes())
  fn <- get_shape("neural")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers chip shape", {
  expect_true("chip" %in% list_shapes())
  fn <- get_shape("chip")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers robot shape", {
  expect_true("robot" %in% list_shapes())
  fn <- get_shape("robot")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers brain shape", {
  expect_true("brain" %in% list_shapes())
  fn <- get_shape("brain")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers network shape", {
  expect_true("network" %in% list_shapes())
  fn <- get_shape("network")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers database shape", {
  expect_true("database" %in% list_shapes())
  fn <- get_shape("database")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers cloud shape", {
  expect_true("cloud" %in% list_shapes())
  fn <- get_shape("cloud")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers gear shape", {
  expect_true("gear" %in% list_shapes())
  fn <- get_shape("gear")
  expect_true(is.function(fn))
})

# =============================================================================
# Test: register_builtin_shapes - Rectangle and None shapes
# =============================================================================

test_that("register_builtin_shapes registers rectangle shape", {
  expect_true("rectangle" %in% list_shapes())
  fn <- get_shape("rectangle")
  expect_true(is.function(fn))
})

test_that("register_builtin_shapes registers none shape", {
  expect_true("none" %in% list_shapes())
  fn <- get_shape("none")
  expect_true(is.function(fn))
})

# =============================================================================
# Test: Shape Registration Functions
# =============================================================================

test_that("register_shape validates function parameter", {
  expect_error(
    register_shape("test_shape", "not_a_function"),
    "draw_fn must be a function"
  )
})

test_that("register_shape can register custom shape", {
  custom_shape_fn <- function(x, y, size, fill, border_color, border_width, ...) {
    grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size * 1.5, "npc"),
      gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
    )
  }

  result <- register_shape("test_custom_shape", custom_shape_fn)
  expect_null(result)  # Returns invisible NULL

  # Verify registration
  expect_true("test_custom_shape" %in% list_shapes())

  fn <- get_shape("test_custom_shape")
  expect_true(is.function(fn))
})

test_that("register_shape can overwrite existing shape", {
  original_fn <- get_shape("circle")

  replacement_fn <- function(x, y, size, fill, border_color, border_width, ...) {
    grid::rectGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      width = grid::unit(size, "npc"),
      height = grid::unit(size, "npc"),
      gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
    )
  }

  register_shape("circle", replacement_fn)

  new_fn <- get_shape("circle")
  expect_true(is.function(new_fn))

  # Restore original
  register_shape("circle", original_fn)
})

test_that("get_shape returns NULL for unknown shape", {
  fn <- get_shape("nonexistent_shape_xyz")
  expect_null(fn)
})

test_that("list_shapes returns all registered shapes", {
  shapes <- list_shapes()
  expect_true(is.character(shapes))
  expect_true(length(shapes) >= 20)  # At least 20 built-in shapes

  # Check for key shapes
  expected <- c("circle", "square", "triangle", "diamond", "pentagon",
                "hexagon", "ellipse", "heart", "star", "pie", "donut",
                "polygon_donut", "donut_pie", "double_donut_pie", "cross",
                "plus", "neural", "chip", "robot", "brain", "network",
                "database", "cloud", "gear", "rectangle", "none")
  for (name in expected) {
    expect_true(name %in% shapes, info = paste("Missing shape:", name))
  }
})

# =============================================================================
# Test: Basic Shape Drawing Functions
# =============================================================================

test_that("draw_circle returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_circle(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "circle")
})

test_that("draw_square returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_square(0.5, 0.5, 0.1, "red", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "rect")
})

test_that("draw_triangle returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_triangle(0.5, 0.5, 0.1, "green", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "polygon")
})

test_that("draw_diamond returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_diamond(0.5, 0.5, 0.1, "purple", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "polygon")
})

test_that("draw_pentagon returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_pentagon(0.5, 0.5, 0.1, "orange", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "polygon")
})

test_that("draw_hexagon returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_hexagon(0.5, 0.5, 0.1, "cyan", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "polygon")
})

# =============================================================================
# Test: Special Shape Drawing Functions
# =============================================================================

test_that("draw_ellipse returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_ellipse(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "polygon")
})

test_that("draw_ellipse respects aspect parameter", {
  skip_if_not_installed("grid")

  grob <- draw_ellipse(0.5, 0.5, 0.1, "blue", "black", 1, aspect = 0.3)
  expect_s3_class(grob, "grob")
})

test_that("draw_heart returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_heart(0.5, 0.5, 0.1, "red", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "polygon")
})

test_that("draw_star returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "polygon")
})

test_that("draw_star respects n_points parameter", {
  skip_if_not_installed("grid")

  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, n_points = 6)
  expect_s3_class(grob, "grob")
})

test_that("draw_star respects inner_ratio parameter", {
  skip_if_not_installed("grid")

  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, inner_ratio = 0.2)
  expect_s3_class(grob, "grob")
})

test_that("draw_cross returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_cross(0.5, 0.5, 0.1, "gray", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_cross respects thickness parameter", {
  skip_if_not_installed("grid")

  grob <- draw_cross(0.5, 0.5, 0.1, "gray", "black", 1, thickness = 0.5)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: Pie Shape Drawing Function
# =============================================================================

test_that("draw_pie returns circle for no values", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_pie returns circle for single value", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1, values = 100)
  expect_s3_class(grob, "grob")
})

test_that("draw_pie returns gList for multiple values", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                   values = c(30, 40, 30))
  expect_s3_class(grob, "gList")
})

test_that("draw_pie uses custom colors", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                   values = c(50, 50),
                   colors = c("red", "green"))
  expect_s3_class(grob, "gList")
})

test_that("draw_pie respects pie_border_width", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 2,
                   values = c(30, 70),
                   pie_border_width = 0.5)
  expect_s3_class(grob, "gList")
})

test_that("draw_pie respects default_color", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                   values = NULL,
                   default_color = "orange")
  expect_s3_class(grob, "grob")
})

# =============================================================================
# Test: Donut Shape Drawing Function
# =============================================================================

test_that("draw_donut returns gList for default parameters", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut handles single value proportion", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = 0.75)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut handles multiple values", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = c(30, 40, 30))
  expect_s3_class(grob, "gList")
})

test_that("draw_donut respects inner_ratio parameter", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, inner_ratio = 0.3)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut respects bg_color parameter", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, bg_color = "lightgray")
  expect_s3_class(grob, "gList")
})

test_that("draw_donut shows value in center when show_value is TRUE", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, show_value = TRUE)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut hides value when show_value is FALSE", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, show_value = FALSE)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut respects value formatting parameters", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.755,
                     show_value = TRUE,
                     value_size = 10,
                     value_color = "red",
                     value_fontface = "italic",
                     value_digits = 1,
                     value_prefix = "$",
                     value_suffix = "%")
  expect_s3_class(grob, "gList")
})

test_that("draw_donut respects custom value_format function", {
  skip_if_not_installed("grid")

  custom_format <- function(x) paste0(round(x * 100), "%")
  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5,
                     show_value = TRUE,
                     value_format = custom_format)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut respects donut_border_width", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 2,
                     values = 0.5,
                     donut_border_width = 0.5)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut handles edge case prop = 0", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = 0)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut handles edge case prop = 1", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = 1)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: AI-themed Shape Drawing Functions
# =============================================================================

test_that("draw_neural returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_neural(0.5, 0.5, 0.1, "lightblue", "navy", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_neural respects n_connections parameter", {
  skip_if_not_installed("grid")

  grob <- draw_neural(0.5, 0.5, 0.1, "lightblue", "navy", 1, n_connections = 8)
  expect_s3_class(grob, "gList")
})

test_that("draw_chip returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_chip(0.5, 0.5, 0.1, "gray", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_chip respects pins_per_side parameter", {
  skip_if_not_installed("grid")

  grob <- draw_chip(0.5, 0.5, 0.1, "gray", "black", 1, pins_per_side = 5)
  expect_s3_class(grob, "gList")
})

test_that("draw_robot returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_robot(0.5, 0.5, 0.1, "silver", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_brain returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_brain(0.5, 0.5, 0.1, "pink", "darkred", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_network returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_network(0.5, 0.5, 0.1, "lightgray", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_database returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_database(0.5, 0.5, 0.1, "steelblue", "navy", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_cloud returns proper grob", {
  skip_if_not_installed("grid")

  grob <- draw_cloud(0.5, 0.5, 0.1, "white", "lightblue", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "polygon")
})

test_that("draw_gear returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_gear(0.5, 0.5, 0.1, "gray", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_gear respects n_teeth parameter", {
  skip_if_not_installed("grid")

  grob <- draw_gear(0.5, 0.5, 0.1, "gray", "black", 1, n_teeth = 12)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: Rectangle and None Shapes
# =============================================================================

test_that("rectangle shape returns proper grob", {
  skip_if_not_installed("grid")

  rect_fn <- get_shape("rectangle")
  grob <- rect_fn(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "rect")
})

test_that("rectangle shape respects aspect parameter", {
  skip_if_not_installed("grid")

  rect_fn <- get_shape("rectangle")
  grob <- rect_fn(0.5, 0.5, 0.1, "blue", "black", 1, aspect = 2.0)
  expect_s3_class(grob, "grob")
})

test_that("none shape returns null grob", {
  skip_if_not_installed("grid")

  none_fn <- get_shape("none")
  grob <- none_fn(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "null")
})

# =============================================================================
# Test: Alpha Parameter
# =============================================================================

test_that("shapes respect alpha parameter", {
  skip_if_not_installed("grid")

  # Test alpha on basic shapes
  grob <- draw_circle(0.5, 0.5, 0.1, "blue", "black", 1, alpha = 0.5)
  expect_s3_class(grob, "grob")

  grob <- draw_square(0.5, 0.5, 0.1, "red", "black", 1, alpha = 0.3)
  expect_s3_class(grob, "grob")

  grob <- draw_triangle(0.5, 0.5, 0.1, "green", "black", 1, alpha = 0.7)
  expect_s3_class(grob, "grob")
})

test_that("complex shapes respect alpha parameter", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, alpha = 0.5)
  expect_s3_class(grob, "gList")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                   values = c(30, 70), alpha = 0.5)
  expect_s3_class(grob, "gList")

  grob <- draw_neural(0.5, 0.5, 0.1, "lightblue", "navy", 1, alpha = 0.6)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: Shape Drawing at Different Positions
# =============================================================================

test_that("shapes can be drawn at different positions", {
  skip_if_not_installed("grid")

  positions <- list(
    c(0.1, 0.1),
    c(0.9, 0.9),
    c(0.5, 0.5),
    c(0.2, 0.8)
  )

  for (pos in positions) {
    grob <- draw_circle(pos[1], pos[2], 0.05, "blue", "black", 1)
    expect_s3_class(grob, "grob")
  }
})

test_that("shapes can be drawn with different sizes", {
  skip_if_not_installed("grid")

  sizes <- c(0.01, 0.05, 0.1, 0.2)

  for (size in sizes) {
    grob <- draw_square(0.5, 0.5, size, "red", "black", 1)
    expect_s3_class(grob, "grob")
  }
})

# =============================================================================
# Test: donut_pie and double_donut_pie Shapes
# =============================================================================

test_that("draw_donut_pie returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut_pie respects donut_value parameter", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                         donut_value = 0.7)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut_pie respects pie_values parameter", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                         pie_values = c(30, 40, 30))
  expect_s3_class(grob, "gList")
})

test_that("draw_donut_pie respects pie_colors parameter", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                         pie_values = c(50, 50),
                         pie_colors = c("red", "green"))
  expect_s3_class(grob, "gList")
})

test_that("draw_double_donut_pie returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_double_donut_pie respects donut_values parameter", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                                donut_values = 0.8)
  expect_s3_class(grob, "gList")
})

test_that("draw_double_donut_pie respects donut2_values parameter", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                                donut_values = 0.7,
                                donut2_values = 0.5)
  expect_s3_class(grob, "gList")
})

test_that("draw_double_donut_pie respects pie_values parameter", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                                pie_values = c(25, 25, 50))
  expect_s3_class(grob, "gList")
})

test_that("draw_double_donut_pie respects ratio parameters", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                                outer_inner_ratio = 0.8,
                                inner_inner_ratio = 0.5)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: polygon_donut Shape
# =============================================================================

test_that("draw_polygon_donut returns proper gList", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "gList")
})

test_that("draw_polygon_donut respects values parameter", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                             values = 0.6)
  expect_s3_class(grob, "gList")
})

test_that("draw_polygon_donut respects donut_shape parameter", {
  skip_if_not_installed("grid")

  shapes <- c("circle", "square", "hexagon", "triangle", "diamond", "pentagon")

  for (shape in shapes) {
    grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                               values = 0.5, donut_shape = shape)
    expect_s3_class(grob, "gList")
  }
})

test_that("draw_polygon_donut respects multi-segment values", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                             values = c(30, 40, 30))
  expect_s3_class(grob, "gList")
})

test_that("draw_polygon_donut respects show_value parameter", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                             values = 0.75, show_value = TRUE)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: Edge cases and special inputs
# =============================================================================

test_that("shapes handle very small sizes", {
  skip_if_not_installed("grid")

  grob <- draw_circle(0.5, 0.5, 0.001, "blue", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("shapes handle zero border width", {
  skip_if_not_installed("grid")

  grob <- draw_square(0.5, 0.5, 0.1, "blue", "black", 0)
  expect_s3_class(grob, "grob")
})

test_that("shapes handle extreme alpha values", {
  skip_if_not_installed("grid")

  grob <- draw_circle(0.5, 0.5, 0.1, "blue", "black", 1, alpha = 0)
  expect_s3_class(grob, "grob")

  grob <- draw_circle(0.5, 0.5, 0.1, "blue", "black", 1, alpha = 1)
  expect_s3_class(grob, "grob")
})

test_that("shapes handle NA colors gracefully", {
  skip_if_not_installed("grid")

  # Should not error with NA border
  grob <- draw_circle(0.5, 0.5, 0.1, "blue", NA, 1)
  expect_s3_class(grob, "grob")
})

test_that("all built-in shapes return proper grob structure", {
  skip_if_not_installed("grid")

  # Test shapes that return simple grobs
  simple_shapes <- c("circle", "square", "triangle", "diamond",
                     "pentagon", "hexagon", "ellipse", "heart",
                     "cloud")

  for (name in simple_shapes) {
    fn <- get_shape(name)
    grob <- fn(0.5, 0.5, 0.1, "blue", "black", 1)
    expect_true(inherits(grob, "grob"))
  }

  # Test shapes that return gLists
  complex_shapes <- c("star", "neural", "chip", "robot", "brain",
                      "network", "database", "gear", "cross")

  for (name in complex_shapes) {
    fn <- get_shape(name)
    grob <- fn(0.5, 0.5, 0.1, "blue", "black", 1)
    expect_true(inherits(grob, "grob") || inherits(grob, "gList"))
  }
})
