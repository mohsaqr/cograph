#' @title Shape Registry Functions
#' @description Functions for registering built-in shapes.
#' @name shapes-registry
#' @keywords internal
NULL

#' Register Built-in Shapes
#'
#' Register all built-in node shapes.
#'
#' @keywords internal
register_builtin_shapes <- function() {
  # Basic shapes
  register_shape("circle", draw_circle)
  register_shape("square", draw_square)
  register_shape("triangle", draw_triangle)
  register_shape("diamond", draw_diamond)
  register_shape("pentagon", draw_pentagon)
  register_shape("hexagon", draw_hexagon)

  # Special shapes
  register_shape("ellipse", draw_ellipse)
  register_shape("heart", draw_heart)
  register_shape("star", draw_star)
  register_shape("pie", draw_pie)
  register_shape("donut", draw_donut)
  register_shape("polygon_donut", draw_polygon_donut)
  register_shape("donut_pie", draw_donut_pie)
  register_shape("double_donut_pie", draw_double_donut_pie)
  register_shape("cross", draw_cross)
  register_shape("plus", draw_cross)  # Alias

  # AI-themed shapes
  register_shape("neural", draw_neural)
  register_shape("chip", draw_chip)
  register_shape("robot", draw_robot)
  register_shape("brain", draw_brain)
  register_shape("network", draw_network)
  register_shape("database", draw_database)
  register_shape("cloud", draw_cloud)
  register_shape("gear", draw_gear)

  # Rectangle (alias for square with different aspect)
  register_shape("rectangle", function(x, y, size, fill, border_color,
                                        border_width, alpha = 1, aspect = 1.5, ...) {
    fill_col <- adjust_alpha(fill, alpha)
    border_col <- adjust_alpha(border_color, alpha)

    grid::rectGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      width = grid::unit(size * 2 * aspect, "npc"),
      height = grid::unit(size * 2, "npc"),
      gp = grid::gpar(
        fill = fill_col,
        col = border_col,
        lwd = border_width
      )
    )
  })

  # None/invisible (for labels only)
  register_shape("none", function(x, y, size, fill, border_color,
                                   border_width, alpha = 1, ...) {
    grid::nullGrob()
  })
}
