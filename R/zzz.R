#' @title Package Load and Unload Functions
#' @description Functions called when the package is loaded or unloaded.
#' @name zzz
#' @keywords internal
NULL

.onLoad <- function(libname, pkgname) { # nocov start
  init_registries()
  register_builtin_shapes()
  register_builtin_layouts()
  register_builtin_themes()
  register_builtin_palettes()
} # nocov end

