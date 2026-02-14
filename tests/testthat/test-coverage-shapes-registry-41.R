# =============================================================================
# Test Coverage for shapes-registry.R (Extended)
# =============================================================================
# Additional comprehensive tests for shape registration and drawing functions
# Targets functions in shapes-registry.R, shapes-basic.R, and shapes-special.R

# =============================================================================
# Test: register_builtin_shapes comprehensive coverage
# =============================================================================

test_that("register_builtin_shapes initializes all expected shapes", {
  # Force re-registration to ensure function is called
  register_builtin_shapes()

  shapes <- list_shapes()

  # Check all basic shapes
  basic_shapes <- c("circle", "square", "triangle", "diamond", "pentagon", "hexagon")
  for (shape in basic_shapes) {
    expect_true(shape %in% shapes, info = paste("Missing basic shape:", shape))
  }

  # Check all special shapes
  special_shapes <- c("ellipse", "heart", "star", "pie", "donut",
                      "polygon_donut", "donut_pie", "double_donut_pie", "cross", "plus")
  for (shape in special_shapes) {
    expect_true(shape %in% shapes, info = paste("Missing special shape:", shape))
  }

  # Check AI-themed shapes
  ai_shapes <- c("neural", "chip", "robot", "brain", "network",
                 "database", "cloud", "gear")
  for (shape in ai_shapes) {
    expect_true(shape %in% shapes, info = paste("Missing AI shape:", shape))
  }

  # Check rectangle and none
  expect_true("rectangle" %in% shapes)
  expect_true("none" %in% shapes)
})

# =============================================================================
# Test: draw_circle edge cases
# =============================================================================

test_that("draw_circle handles zero alpha", {
  skip_if_not_installed("grid")

  grob <- draw_circle(0.5, 0.5, 0.1, "blue", "black", 1, alpha = 0)
  expect_s3_class(grob, "grob")
  expect_s3_class(grob, "circle")
})

test_that("draw_circle handles negative coordinates", {
  skip_if_not_installed("grid")

  grob <- draw_circle(-0.5, -0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_circle handles very large size", {
  skip_if_not_installed("grid")

  grob <- draw_circle(0.5, 0.5, 2.0, "red", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_circle handles transparent colors", {
  skip_if_not_installed("grid")

  grob <- draw_circle(0.5, 0.5, 0.1, "transparent", "transparent", 1)
  expect_s3_class(grob, "grob")
})

# =============================================================================
# Test: draw_square edge cases
# =============================================================================

test_that("draw_square handles zero size", {
  skip_if_not_installed("grid")

  grob <- draw_square(0.5, 0.5, 0, "red", "black", 1)
  expect_s3_class(grob, "grob")
})

test_that("draw_square handles large border width", {
  skip_if_not_installed("grid")

  grob <- draw_square(0.5, 0.5, 0.1, "blue", "black", 10)
  expect_s3_class(grob, "grob")
})

# =============================================================================
# Test: draw_triangle edge cases
# =============================================================================

test_that("draw_triangle creates proper polygon with correct vertex count", {
  skip_if_not_installed("grid")

  grob <- draw_triangle(0.5, 0.5, 0.15, "green", "black", 1.5)
  expect_s3_class(grob, "polygon")

  # Should have 3 vertices
  expect_equal(length(grob$x), 3)
})

# =============================================================================
# Test: draw_diamond edge cases
# =============================================================================

test_that("draw_diamond creates proper polygon with correct vertex count", {
  skip_if_not_installed("grid")

  grob <- draw_diamond(0.5, 0.5, 0.1, "purple", "gray", 2)
  expect_s3_class(grob, "polygon")

  # Should have 4 vertices
  expect_equal(length(grob$x), 4)
})

# =============================================================================
# Test: draw_pentagon edge cases
# =============================================================================

test_that("draw_pentagon creates proper polygon with 5 vertices", {
  skip_if_not_installed("grid")

  grob <- draw_pentagon(0.5, 0.5, 0.1, "orange", "black", 1)
  expect_s3_class(grob, "polygon")

  # Should have 5 vertices
  expect_equal(length(grob$x), 5)
})

# =============================================================================
# Test: draw_hexagon edge cases
# =============================================================================

test_that("draw_hexagon creates proper polygon with 6 vertices", {
  skip_if_not_installed("grid")

  grob <- draw_hexagon(0.5, 0.5, 0.1, "cyan", "black", 1)
  expect_s3_class(grob, "polygon")

  # Should have 6 vertices
  expect_equal(length(grob$x), 6)
})

# =============================================================================
# Test: draw_ellipse additional cases
# =============================================================================

test_that("draw_ellipse handles extreme aspect ratios", {
  skip_if_not_installed("grid")

  # Very flat ellipse
  grob <- draw_ellipse(0.5, 0.5, 0.1, "blue", "black", 1, aspect = 0.1)
  expect_s3_class(grob, "polygon")

  # Tall ellipse
  grob <- draw_ellipse(0.5, 0.5, 0.1, "blue", "black", 1, aspect = 1.0)
  expect_s3_class(grob, "polygon")
})

test_that("draw_ellipse generates sufficient points for smooth curve", {
  skip_if_not_installed("grid")

  grob <- draw_ellipse(0.5, 0.5, 0.1, "blue", "black", 1)
  # Should have 50 points for smooth rendering
  expect_equal(length(grob$x), 50)
})

# =============================================================================
# Test: draw_heart additional cases
# =============================================================================

test_that("draw_heart generates proper polygon", {
  skip_if_not_installed("grid")

  grob <- draw_heart(0.5, 0.5, 0.15, "red", "darkred", 1.5)
  expect_s3_class(grob, "polygon")

  # Should have 100 points for smooth curve
  expect_equal(length(grob$x), 100)
})

# =============================================================================
# Test: draw_star additional cases
# =============================================================================

test_that("draw_star handles different point configurations", {
  skip_if_not_installed("grid")

  # 3-point star
  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, n_points = 3)
  expect_s3_class(grob, "polygon")
  expect_equal(length(grob$x), 6)  # 3 outer + 3 inner

  # 7-point star
  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, n_points = 7)
  expect_s3_class(grob, "polygon")
  expect_equal(length(grob$x), 14)  # 7 outer + 7 inner

  # 10-point star
  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, n_points = 10)
  expect_s3_class(grob, "polygon")
  expect_equal(length(grob$x), 20)
})

test_that("draw_star handles extreme inner_ratio values", {
  skip_if_not_installed("grid")

  # Very small inner ratio (thin star)
  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, inner_ratio = 0.1)
  expect_s3_class(grob, "polygon")

  # Large inner ratio (fat star)
  grob <- draw_star(0.5, 0.5, 0.1, "gold", "black", 1, inner_ratio = 0.9)
  expect_s3_class(grob, "polygon")
})

# =============================================================================
# Test: draw_pie additional cases
# =============================================================================

test_that("draw_pie handles empty values vector", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1, values = c())
  expect_s3_class(grob, "grob")
})

test_that("draw_pie handles very small proportions", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                   values = c(1, 1000))  # 0.1% vs 99.9%
  expect_s3_class(grob, "gList")
})

test_that("draw_pie handles zero border segment width", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                   values = c(50, 50),
                   pie_border_width = 0)
  expect_s3_class(grob, "gList")
})

test_that("draw_pie handles many segments", {
  skip_if_not_installed("grid")

  grob <- draw_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                   values = rep(10, 10))  # 10 equal segments
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: draw_donut additional cases
# =============================================================================

test_that("draw_donut handles extreme inner_ratio values", {
  skip_if_not_installed("grid")

  # Thin ring
  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, inner_ratio = 0.9)
  expect_s3_class(grob, "gList")

  # Thick ring
  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, inner_ratio = 0.1)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut handles value slightly over 1", {
  skip_if_not_installed("grid")

  # Should clamp to 1
  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = 1.5)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut handles negative value", {
  skip_if_not_installed("grid")

  # Should clamp to 0
  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = -0.5)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut handles all fontface options", {
  skip_if_not_installed("grid")

  fontfaces <- c("plain", "bold", "italic", "bold.italic")

  for (ff in fontfaces) {
    grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                       values = 0.5, show_value = TRUE, value_fontface = ff)
    expect_s3_class(grob, "gList")
  }
})

test_that("draw_donut handles custom value_fontfamily", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = 0.5, show_value = TRUE,
                     value_fontfamily = "serif")
  expect_s3_class(grob, "gList")
})

test_that("draw_donut handles multiple values with custom colors", {
  skip_if_not_installed("grid")

  grob <- draw_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                     values = c(25, 35, 40),
                     colors = c("red", "green", "blue"))
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: draw_polygon_donut additional cases
# =============================================================================

test_that("draw_polygon_donut handles NULL values", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = NULL)
  expect_s3_class(grob, "gList")
})

test_that("draw_polygon_donut handles empty values", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = c())
  expect_s3_class(grob, "gList")
})

test_that("draw_polygon_donut handles multi-segment with custom colors", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                             values = c(30, 40, 30),
                             colors = c("red", "green", "blue"))
  expect_s3_class(grob, "gList")
})

test_that("draw_polygon_donut handles custom value formatting", {
  skip_if_not_installed("grid")

  custom_format <- function(x) paste0(round(x * 100), "%")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                             values = 0.75,
                             show_value = TRUE,
                             value_format = custom_format)
  expect_s3_class(grob, "gList")
})

test_that("draw_polygon_donut handles all fontface types", {
  skip_if_not_installed("grid")

  fontfaces <- c("plain", "bold", "italic", "bold.italic")

  for (ff in fontfaces) {
    grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1,
                               values = 0.5, show_value = TRUE,
                               value_fontface = ff)
    expect_s3_class(grob, "gList")
  }
})

test_that("draw_polygon_donut handles edge case where prop is 0", {
  skip_if_not_installed("grid")

  grob <- draw_polygon_donut(0.5, 0.5, 0.1, "blue", "black", 1, values = 0)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: draw_donut_pie additional cases
# =============================================================================

test_that("draw_donut_pie handles NULL donut_value", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                         donut_value = NULL)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut_pie handles NULL pie_values", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                         donut_value = 0.75,
                         pie_values = NULL)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut_pie handles empty pie_values", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                         donut_value = 0.5,
                         pie_values = c())
  expect_s3_class(grob, "gList")
})

test_that("draw_donut_pie handles different border widths", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 2,
                         donut_value = 0.6,
                         pie_values = c(40, 60),
                         pie_border_width = 0.5,
                         donut_border_width = 3)
  expect_s3_class(grob, "gList")
})

test_that("draw_donut_pie handles pie_colors recycling", {
  skip_if_not_installed("grid")

  grob <- draw_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                         pie_values = c(30, 30, 40),
                         pie_colors = c("red", "green"))  # Only 2 colors for 3 values
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: draw_double_donut_pie additional cases
# =============================================================================

test_that("draw_double_donut_pie handles all NULL values", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                                donut_values = NULL,
                                donut2_values = NULL,
                                pie_values = NULL)
  expect_s3_class(grob, "gList")
})

test_that("draw_double_donut_pie handles multi-segment donuts", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                                donut_values = c(30, 40, 30),
                                donut_colors = c("red", "green", "blue"),
                                donut2_values = c(50, 50),
                                donut2_colors = c("orange", "purple"),
                                pie_values = c(25, 25, 50),
                                pie_colors = c("cyan", "magenta", "yellow"))
  expect_s3_class(grob, "gList")
})

test_that("draw_double_donut_pie handles progress-style donuts", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 1,
                                donut_values = 0.8,
                                donut_colors = "steelblue",
                                donut2_values = 0.6,
                                donut2_colors = "coral")
  expect_s3_class(grob, "gList")
})

test_that("draw_double_donut_pie handles custom border widths", {
  skip_if_not_installed("grid")

  grob <- draw_double_donut_pie(0.5, 0.5, 0.1, "blue", "black", 2,
                                donut_values = 0.5,
                                donut2_values = 0.7,
                                pie_values = c(50, 50),
                                pie_border_width = 0.5,
                                donut_border_width = 3)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: AI-themed shapes additional cases
# =============================================================================

test_that("draw_neural handles different connection counts", {
  skip_if_not_installed("grid")

  # Few connections
  grob <- draw_neural(0.5, 0.5, 0.1, "lightblue", "navy", 1, n_connections = 3)
  expect_s3_class(grob, "gList")

  # Many connections
  grob <- draw_neural(0.5, 0.5, 0.1, "lightblue", "navy", 1, n_connections = 12)
  expect_s3_class(grob, "gList")
})

test_that("draw_chip handles different pins_per_side values", {
  skip_if_not_installed("grid")

  # Few pins
  grob <- draw_chip(0.5, 0.5, 0.1, "gray", "black", 1, pins_per_side = 2)
  expect_s3_class(grob, "gList")

  # Many pins
  grob <- draw_chip(0.5, 0.5, 0.1, "gray", "black", 1, pins_per_side = 8)
  expect_s3_class(grob, "gList")
})

test_that("draw_gear handles different tooth counts", {
  skip_if_not_installed("grid")

  # Few teeth
  grob <- draw_gear(0.5, 0.5, 0.1, "gray", "black", 1, n_teeth = 4)
  expect_s3_class(grob, "gList")

  # Many teeth
  grob <- draw_gear(0.5, 0.5, 0.1, "gray", "black", 1, n_teeth = 16)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: draw_cross additional cases
# =============================================================================

test_that("draw_cross handles different thickness values", {
  skip_if_not_installed("grid")

  # Thin cross
  grob <- draw_cross(0.5, 0.5, 0.1, "gray", "black", 1, thickness = 0.1)
  expect_s3_class(grob, "gList")

  # Thick cross
  grob <- draw_cross(0.5, 0.5, 0.1, "gray", "black", 1, thickness = 0.8)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: rectangle shape edge cases
# =============================================================================

test_that("rectangle shape handles different aspect ratios", {
  skip_if_not_installed("grid")

  rect_fn <- get_shape("rectangle")

  # Wide rectangle
  grob <- rect_fn(0.5, 0.5, 0.1, "blue", "black", 1, aspect = 3.0)
  expect_s3_class(grob, "rect")

  # Square-ish (aspect = 1)
  grob <- rect_fn(0.5, 0.5, 0.1, "blue", "black", 1, aspect = 1.0)
  expect_s3_class(grob, "rect")
})

# =============================================================================
# Test: none shape edge cases
# =============================================================================

test_that("none shape returns nullGrob regardless of parameters", {
  skip_if_not_installed("grid")

  none_fn <- get_shape("none")

  # Various parameters should all return null grob
  grob1 <- none_fn(0.5, 0.5, 0.1, "red", "black", 1)
  grob2 <- none_fn(0.1, 0.9, 0.5, "blue", "white", 10)
  grob3 <- none_fn(0, 0, 0, NA, NA, 0)

  expect_s3_class(grob1, "null")
  expect_s3_class(grob2, "null")
  expect_s3_class(grob3, "null")
})

# =============================================================================
# Test: Shape function signature compatibility
# =============================================================================

test_that("all registered shapes accept standard parameters", {
  skip_if_not_installed("grid")

  shapes_to_test <- c("circle", "square", "triangle", "diamond",
                      "pentagon", "hexagon", "ellipse", "heart", "star",
                      "cross", "rectangle", "none")

  for (shape_name in shapes_to_test) {
    fn <- get_shape(shape_name)
    expect_true(is.function(fn), info = paste("Shape not a function:", shape_name))

    # All shapes should accept these standard parameters
    result <- tryCatch(
      fn(0.5, 0.5, 0.1, "blue", "black", 1, alpha = 0.8),
      error = function(e) NULL
    )
    expect_true(!is.null(result) || inherits(result, "grob") || inherits(result, "gList"),
                info = paste("Shape failed standard call:", shape_name))
  }
})

test_that("all complex shapes accept extra parameters via ...", {
  skip_if_not_installed("grid")

  complex_shapes <- c("pie", "donut", "neural", "chip", "gear")

  for (shape_name in complex_shapes) {
    fn <- get_shape(shape_name)
    expect_true(is.function(fn), info = paste("Shape not a function:", shape_name))

    # Should handle extra parameters without error
    result <- tryCatch(
      fn(0.5, 0.5, 0.1, "blue", "black", 1, alpha = 0.8, extra_param = "ignored"),
      error = function(e) NULL
    )
    expect_false(is.null(result), info = paste("Shape failed with extra params:", shape_name))
  }
})

# =============================================================================
# Test: Custom shape registration and retrieval
# =============================================================================

test_that("register_shape allows registering lambdas", {
  skip_if_not_installed("grid")

  # Register anonymous function
  register_shape("test_lambda_shape", function(x, y, size, fill, border_color, border_width, ...) {
    grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size * 0.5, "npc"),
      gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
    )
  })

  fn <- get_shape("test_lambda_shape")
  expect_true(is.function(fn))

  grob <- fn(0.5, 0.5, 0.1, "green", "darkgreen", 2)
  expect_s3_class(grob, "grob")
})

test_that("register_shape allows shape with complex return value", {
  skip_if_not_installed("grid")

  register_shape("test_complex_shape", function(x, y, size, fill, border_color, border_width, ...) {
    grobs <- list()
    grobs[[1]] <- grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size, "npc"),
      gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
    )
    grobs[[2]] <- grid::circleGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      r = grid::unit(size * 0.5, "npc"),
      gp = grid::gpar(fill = "white", col = border_color, lwd = border_width)
    )
    do.call(grid::gList, grobs)
  })

  fn <- get_shape("test_complex_shape")
  grob <- fn(0.5, 0.5, 0.1, "blue", "black", 1)
  expect_s3_class(grob, "gList")
})

# =============================================================================
# Test: Shape registry state
# =============================================================================

test_that("list_shapes returns current state of registry", {
  skip_if_not_installed("grid")

  initial_shapes <- list_shapes()

  # Register a new shape
  register_shape("test_temp_shape", function(x, y, size, fill, border_color, border_width, ...) {
    grid::nullGrob()
  })

  new_shapes <- list_shapes()

  expect_true("test_temp_shape" %in% new_shapes)
  expect_equal(length(new_shapes), length(initial_shapes) + 1)
})

test_that("get_shape returns NULL for non-existent shapes", {
  fn <- get_shape("definitely_not_a_registered_shape_12345")
  expect_null(fn)
})

# =============================================================================
# Test: Grob property verification
# =============================================================================

test_that("basic shape grobs have correct graphical parameters", {
  skip_if_not_installed("grid")

  grob <- draw_circle(0.5, 0.5, 0.1, "blue", "red", 2)

  # Check that gp exists and has expected values
  expect_true(!is.null(grob$gp))
  expect_equal(grob$gp$lwd, 2)
})

test_that("shapes handle hex color codes", {
  skip_if_not_installed("grid")

  grob <- draw_square(0.5, 0.5, 0.1, "#FF5500", "#003355", 1)
  expect_s3_class(grob, "grob")

  grob <- draw_triangle(0.5, 0.5, 0.1, "#AABBCC", "#112233", 1.5)
  expect_s3_class(grob, "polygon")
})

test_that("shapes handle RGB color specifications", {
  skip_if_not_installed("grid")

  rgb_color <- grDevices::rgb(0.5, 0.2, 0.8)
  grob <- draw_diamond(0.5, 0.5, 0.1, rgb_color, "black", 1)
  expect_s3_class(grob, "polygon")
})
