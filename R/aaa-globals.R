#' @title Global Registries for cograph
#' @description Internal registries for shapes, layouts, and themes.
#' @name globals
#' @keywords internal
#' @noRd
NULL

# Package environment for storing registries
.cograph_env <- new.env(parent = emptyenv())

# ============================================================================
# Edge Key Helper
# ============================================================================

#' Canonical edge keys for grouping/deduplication.
#' Undirected: sorts endpoints so A-B == B-A. Directed: preserves order.
#' @keywords internal
#' @noRd
.edge_keys <- function(from, to, directed = FALSE) {
  if (directed) paste(from, to, sep = "-")
  else paste(pmin(from, to), pmax(from, to), sep = "-")
}

# ============================================================================
# RNG State Helpers (CRAN requirement: set.seed must not alter caller's RNG)
# ============================================================================

#' Save current RNG state
#' @return List with `seed` (the .Random.seed vector or NULL) and `existed` flag.
#' @noRd
.save_rng <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    list(seed = .Random.seed, existed = TRUE)
  } else {
    list(seed = NULL, existed = FALSE)
  }
}

#' Restore previously saved RNG state
#' @param state List returned by `.save_rng()`.
#' @noRd
.restore_rng <- function(state) {
  if (state$existed) {
    assign(".Random.seed", state$seed, envir = globalenv())
  } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
}

#' Initialize Global Registries
#' @keywords internal
#' @noRd
init_registries <- function() {
  .cograph_env$shapes <- list()
  .cograph_env$layouts <- list()
  .cograph_env$themes <- list()
  .cograph_env$palettes <- list()
}

# ============================================================================
# Shape Registry
# ============================================================================

#' @title Node Shapes
#' @description
#' Node shapes are stored in a registry and selected by name through the
#' \code{node_shape} argument of \code{\link{splot}} and \code{\link{soplot}}
#' or the \code{shape} argument of \code{\link{sn_nodes}}.
#' \code{register_shape()} adds a shape given by a drawing function, and
#' \code{get_shape()} returns the drawing function of a registered shape.
#' \code{list_shapes()} returns the names of all registered shapes.
#' A shape added with \code{register_shape()} is a grid shape and is used by
#' \code{soplot()} only. \code{splot()} plots its built-in shapes and SVG
#' shapes.
#' \code{register_svg_shape()} adds a shape defined by an SVG file or an inline
#' SVG string, \code{list_svg_shapes()} returns the names of the registered SVG
#' shapes, and \code{unregister_svg_shape()} removes one. SVG shapes are used
#' by both \code{splot()} and \code{soplot()}. Registering an existing name
#' replaces that shape for the rest of the session.
#'
#' @param name Character. The name of the shape.
#' @param draw_fn A function that renders the shape. It receives the arguments
#'   \code{x}, \code{y}, \code{size}, \code{fill}, \code{border_color},
#'   \code{border_width}, \code{alpha} and \code{...} and returns a grid grob.
#' @param svg_source Character. A path to an SVG file or an inline SVG string.
#'
#' @return \code{register_shape()} and \code{register_svg_shape()} return
#'   \code{NULL} invisibly. \code{get_shape()} returns the drawing function, or
#'   \code{NULL} if no shape has that name. \code{list_shapes()} and
#'   \code{list_svg_shapes()} return a character vector of shape names.
#'   \code{unregister_svg_shape()} returns \code{TRUE} invisibly if the shape
#'   was removed and \code{FALSE} if it was not found.
#'
#' @name shapes
#' @examples
#' list_shapes()
#' splot(regulation_net, node_shape = "diamond")
NULL

#' @rdname shapes
#' @export
register_shape <- function(name, draw_fn) {
  if (!is.function(draw_fn)) {
    stop("draw_fn must be a function", call. = FALSE)
  }
  .cograph_env$shapes[[name]] <- draw_fn
  invisible(NULL)
}

#' @rdname shapes
#' @export
get_shape <- function(name) {

  .cograph_env$shapes[[name]]
}

#' @rdname shapes
#' @export
list_shapes <- function() {
  names(.cograph_env$shapes)
}

# ============================================================================
# Layout Registry
# ============================================================================

#' @title Layout Registry
#' @description
#' Layout algorithms are stored in a registry and selected by name through the
#' \code{layout} argument of \code{\link{splot}}, \code{\link{soplot}} and
#' \code{\link{sn_layout}}. \code{register_layout()} adds a layout function,
#' \code{get_layout()} returns the function of a registered layout, and
#' \code{list_layouts()} returns the names of all registered layouts.
#' Registering an existing name replaces that layout for the rest of the
#' session.
#'
#' @param name Character. The name of the layout.
#' @param layout_fn A function that computes node positions. It receives the
#'   network as the argument \code{network}, followed by any layout
#'   parameters, and returns a matrix or data frame with \code{x} and \code{y}
#'   columns.
#'
#' @return \code{register_layout()} returns \code{NULL} invisibly.
#'   \code{get_layout()} returns the layout function, or \code{NULL} if no
#'   layout has that name. \code{list_layouts()} returns a character vector of
#'   layout names.
#'
#' @seealso \code{\link{layout_circle}}, \code{\link{layout_spring}},
#'   \code{\link{layout_groups}}, \code{\link{layout_oval}}
#'
#' @name layout_registry
#' @examples
#' list_layouts()
#' splot(regulation_net, layout = "circle")
NULL

#' @rdname layout_registry
#' @export
register_layout <- function(name, layout_fn) {
  if (!is.function(layout_fn)) {
    stop("layout_fn must be a function", call. = FALSE)
  }
  .cograph_env$layouts[[name]] <- layout_fn
  invisible(NULL)
}

#' @rdname layout_registry
#' @export
get_layout <- function(name) {
  .cograph_env$layouts[[name]]
}

#' @rdname layout_registry
#' @export
list_layouts <- function() {
  names(.cograph_env$layouts)
}

# ============================================================================
# Theme Registry
# ============================================================================

#' @title Themes
#' @description
#' A theme is a \code{CographTheme} object that sets the background, node,
#' edge and label colors of a plot. Themes are stored in a registry and
#' selected by name through the \code{theme} argument of \code{\link{splot}},
#' \code{\link{soplot}} and \code{\link{sn_theme}}. The functions
#' \code{theme_cograph_*()} return the built-in themes:
#' \describe{
#'   \item{\code{theme_cograph_classic()}}{Blue nodes and gray edges
#'     (\code{"classic"}).}
#'   \item{\code{theme_cograph_colorblind()}}{Colors distinguishable under color
#'     vision deficiency (\code{"colorblind"}).}
#'   \item{\code{theme_cograph_gray()}}{Black and white for print
#'     (\code{"gray"}, also \code{"grey"}).}
#'   \item{\code{theme_cograph_dark()}}{Dark background for presentations
#'     (\code{"dark"}).}
#'   \item{\code{theme_cograph_minimal()}}{Thin borders and few colors
#'     (\code{"minimal"}).}
#'   \item{\code{theme_cograph_viridis()}}{The viridis palette
#'     (\code{"viridis"}).}
#'   \item{\code{theme_cograph_nature()}}{Earth tones (\code{"nature"}).}
#' }
#' \code{register_theme()} adds a theme under a new name, \code{get_theme()}
#' returns a registered theme, and \code{list_themes()} returns the names of
#' all registered themes.
#'
#' @param name Character. The name of the theme.
#' @param theme A \code{CographTheme} object, for example one created with
#'   \code{CographTheme$new()} or returned by a \code{theme_cograph_*()}
#'   function.
#'
#' @return The \code{theme_cograph_*()} functions return a \code{CographTheme}
#'   object. \code{register_theme()} returns \code{NULL} invisibly.
#'   \code{get_theme()} returns the theme, or \code{NULL} if no theme has that
#'   name. \code{list_themes()} returns a character vector of theme names.
#'
#' @name themes
#' @examples
#' splot(regulation_net, theme = "dark")
NULL

#' @rdname themes
#' @export
register_theme <- function(name, theme) {
  .cograph_env$themes[[name]] <- theme
  invisible(NULL)
}

#' @rdname themes
#' @export
get_theme <- function(name) {

  .cograph_env$themes[[name]]
}

#' @rdname themes
#' @export
list_themes <- function() {
  names(.cograph_env$themes)
}

# ============================================================================
# Palette Registry
# ============================================================================

#' @keywords internal
#' @noRd
register_palette <- function(name, palette) {
  .cograph_env$palettes[[name]] <- palette
  invisible(NULL)
}

#' @keywords internal
#' @noRd
get_palette <- function(name) {
  .cograph_env$palettes[[name]]
}

#' @title Color Palettes
#' @description
#' The \code{palette_*()} functions generate a vector of \code{n} colors for
#' nodes or edges. They are registered under their short names (for example
#' \code{"colorblind"}), which \code{\link{sn_palette}} accepts, and
#' \code{list_palettes()} returns the registered names.
#' \describe{
#'   \item{\code{palette_rainbow()}}{Rainbow hues.}
#'   \item{\code{palette_colorblind()}}{The colorblind-safe colors of Wong.}
#'   \item{\code{palette_pastel()}}{Soft pastel colors.}
#'   \item{\code{palette_viridis()}}{The viridis family, chosen by
#'     \code{option}.}
#'   \item{\code{palette_blues()}, \code{palette_reds()}}{Sequential blue or
#'     red shades.}
#'   \item{\code{palette_diverging()}}{Blue to red through \code{midpoint}.}
#' }
#'
#' @param n Number of colors to generate.
#' @param alpha Transparency, from 0 (transparent) to 1 (opaque).
#' @param option Viridis option, one of \code{"viridis"}, \code{"magma"},
#'   \code{"plasma"}, \code{"inferno"}, \code{"cividis"}. Any other value
#'   gives the \code{"viridis"} colors.
#' @param midpoint Color of the midpoint of the diverging palette.
#'
#' @return The \code{palette_*()} functions return a character vector of
#'   \code{n} colors. \code{list_palettes()} returns a character vector of
#'   palette names.
#'
#' @name palettes
#' @examples
#' splot(regulation_net, node_fill = palette_colorblind(n = 10))
NULL

#' @rdname palettes
#' @export
list_palettes <- function() {
  names(.cograph_env$palettes)
}

# igraph is a Suggests dependency: it is not installed with cograph, so every
# entry point that reaches it must say so plainly instead of letting R raise
# the bare "there is no package called 'igraph'" from the `::` operator.
# Raises a classed condition so callers and tests can catch it precisely.
# @noRd
.need_igraph <- function(what) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(errorCondition(
      sprintf(paste("`%s` requires the 'igraph' package, which is not",
                    "installed.\nInstall it with",
                    "install.packages(\"igraph\")."), what),
      class = "cograph_missing_suggest", call = NULL
    ))
  }
  invisible(TRUE)
}
