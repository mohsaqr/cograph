#' @title Theme Registry Functions
#' @description Functions for registering built-in themes.
#' @name themes-registry
#' @keywords internal
NULL

#' Register Built-in Themes
#'
#' Register all built-in themes.
#'
#' @keywords internal
register_builtin_themes <- function() {
  register_theme("classic", theme_cograph_classic())
  register_theme("colorblind", theme_cograph_colorblind())
  register_theme("gray", theme_cograph_gray())
  register_theme("grey", theme_cograph_gray())  # Alias
  register_theme("dark", theme_cograph_dark())
  register_theme("minimal", theme_cograph_minimal())
  register_theme("viridis", theme_cograph_viridis())
  register_theme("nature", theme_cograph_nature())
}
