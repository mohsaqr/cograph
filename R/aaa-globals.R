#' @title Global Registries for cograph
#' @description Internal registries for shapes, layouts, and themes.
#' @name globals
#' @keywords internal
NULL

# Package environment for storing registries
.cograph_env <- new.env(parent = emptyenv())

#' Initialize Global Registries
#' @keywords internal
init_registries <- function() {
  .cograph_env$shapes <- list()
  .cograph_env$layouts <- list()
  .cograph_env$themes <- list()
  .cograph_env$palettes <- list()
}

# ============================================================================
# Shape Registry
# ============================================================================

#' Register a Custom Shape
#'
#' Register a new shape that can be used for node rendering.
#'
#' @param name Character. Name of the shape.
#' @param draw_fn Function. A function that draws the shape. Should accept
#'   parameters: x, y, size, fill, border_color, border_width, ...
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' # Register a custom hexagon shape
#' register_shape("hexagon", function(x, y, size, fill, border_color, border_width, ...) {
#'   angles <- seq(0, 2 * pi, length.out = 7)
#'   grid::polygonGrob(
#'     x = x + size * cos(angles),
#'     y = y + size * sin(angles),
#'     gp = grid::gpar(fill = fill, col = border_color, lwd = border_width)
#'   )
#' })
register_shape <- function(name, draw_fn) {
  if (!is.function(draw_fn)) {
    stop("draw_fn must be a function", call. = FALSE)
  }
  .cograph_env$shapes[[name]] <- draw_fn
  invisible(NULL)
}

#' Get a Registered Shape
#'
#' @param name Character. Name of the shape.
#' @return The shape drawing function, or NULL if not found.
#' @export
#' @examples
#' get_shape("circle")
get_shape <- function(name) {

  .cograph_env$shapes[[name]]
}

#' List Available Shapes
#'
#' @return Character vector of registered shape names.
#' @export
#' @examples
#' list_shapes()
list_shapes <- function() {
  names(.cograph_env$shapes)
}

# ============================================================================
# Layout Registry
# ============================================================================

#' Register a Custom Layout
#'
#' Register a new layout algorithm that can be used for network visualization.
#'
#' @param name Character. Name of the layout.
#' @param layout_fn Function. A function that computes node positions.
#'   Should accept a CographNetwork object and return a matrix with x, y columns.
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' # Register a simple random layout
#' register_layout("random", function(network, ...) {
#'   n <- network$n_nodes
#'   cbind(x = runif(n), y = runif(n))
#' })
register_layout <- function(name, layout_fn) {
  if (!is.function(layout_fn)) {
    stop("layout_fn must be a function", call. = FALSE)
  }
  .cograph_env$layouts[[name]] <- layout_fn
  invisible(NULL)
}

#' Get a Registered Layout
#'
#' @param name Character. Name of the layout.
#' @return The layout function, or NULL if not found.
#' @export
#' @examples
#' get_layout("circle")
get_layout <- function(name) {
  .cograph_env$layouts[[name]]
}

#' List Available Layouts
#'
#' @return Character vector of registered layout names.
#' @export
#' @examples
#' list_layouts()
list_layouts <- function() {
  names(.cograph_env$layouts)
}

# ============================================================================
# Theme Registry
# ============================================================================

#' Register a Custom Theme
#'
#' Register a new theme for network visualization.
#'
#' @param name Character. Name of the theme.
#' @param theme A CographTheme object or a list of theme parameters.
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' # Register a custom theme
#' register_theme("custom", list(
#'   background = "white",
#'   node_fill = "steelblue",
#'   node_border = "navy",
#'   edge_color = "gray50"
#' ))
register_theme <- function(name, theme) {
  .cograph_env$themes[[name]] <- theme
  invisible(NULL)
}

#' Get a Registered Theme
#'
#' @param name Character. Name of the theme.
#' @return The theme object, or NULL if not found.
#' @export
#' @examples
#' get_theme("classic")
get_theme <- function(name) {

  .cograph_env$themes[[name]]
}

#' List Available Themes
#'
#' @return Character vector of registered theme names.
#' @export
#' @examples
#' list_themes()
list_themes <- function() {
  names(.cograph_env$themes)
}

# ============================================================================
# Palette Registry
# ============================================================================

#' @keywords internal
register_palette <- function(name, palette) {
  .cograph_env$palettes[[name]] <- palette
  invisible(NULL)
}

#' @keywords internal
get_palette <- function(name) {
  .cograph_env$palettes[[name]]
}

#' List Available Color Palettes
#'
#' Returns the names of all registered color palettes.
#'
#' @return Character vector of palette names.
#' @export
#' @examples
#' list_palettes()
list_palettes <- function() {
  names(.cograph_env$palettes)
}
