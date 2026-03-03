#' @title Package Load and Unload Functions
#' @description Functions called when the package is loaded or unloaded.
#' @name zzz
#' @keywords internal
NULL

.onLoad <- function(libname, pkgname) {
  # Initialize registries
  init_registries()


  # Register built-in shapes
  register_builtin_shapes()


  # Register built-in layouts
  register_builtin_layouts()


  # Register built-in themes
  register_builtin_themes()

  # Register built-in palettes
  register_builtin_palettes()
}

