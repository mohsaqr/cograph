#' @title Basic Node Shapes
#' @description Basic node shape drawing functions.
#' @name shapes-basic
#' @keywords internal
NULL

#' Draw Circle Node
#' @keywords internal
draw_circle <- function(x, y, size, fill, border_color, border_width,
                        alpha = 1, ...) {
  # Convert colors with alpha
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  grid::circleGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    r = grid::unit(size, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}

#' Draw Square Node
#' @keywords internal
draw_square <- function(x, y, size, fill, border_color, border_width,
                        alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  grid::rectGrob(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    width = grid::unit(size * 2, "npc"),
    height = grid::unit(size * 2, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}

#' Draw Triangle Node
#' @keywords internal
draw_triangle <- function(x, y, size, fill, border_color, border_width,
                          alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Equilateral triangle points
  angles <- c(pi/2, pi/2 + 2*pi/3, pi/2 + 4*pi/3)
  xs <- x + size * cos(angles)
  ys <- y + size * sin(angles)

  grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}

#' Draw Diamond Node
#' @keywords internal
draw_diamond <- function(x, y, size, fill, border_color, border_width,
                         alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  # Diamond (rotated square)
  angles <- c(0, pi/2, pi, 3*pi/2)
  xs <- x + size * cos(angles)
  ys <- y + size * sin(angles)

  grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}

#' Draw Pentagon Node
#' @keywords internal
draw_pentagon <- function(x, y, size, fill, border_color, border_width,
                          alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  angles <- seq(pi/2, pi/2 + 2*pi * (4/5), length.out = 5)
  xs <- x + size * cos(angles)
  ys <- y + size * sin(angles)

  grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}

#' Draw Hexagon Node
#' @keywords internal
draw_hexagon <- function(x, y, size, fill, border_color, border_width,
                         alpha = 1, ...) {
  fill_col <- adjust_alpha(fill, alpha)
  border_col <- adjust_alpha(border_color, alpha)

  angles <- seq(0, 2*pi * (5/6), length.out = 6)
  xs <- x + size * cos(angles)
  ys <- y + size * sin(angles)

  grid::polygonGrob(
    x = grid::unit(xs, "npc"),
    y = grid::unit(ys, "npc"),
    gp = grid::gpar(
      fill = fill_col,
      col = border_col,
      lwd = border_width
    )
  )
}
