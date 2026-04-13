#' @title Package Load and Unload Functions
#' @description Functions called when the package is loaded or unloaded.
#' @name zzz
#' @keywords internal
NULL

# Null-coalescing operator (base R 4.4+; defined here for R >= 4.1 compat)
`%||%` <- function(x, y) if (is.null(x)) y else x

.onLoad <- function(libname, pkgname) { # nocov start
  init_registries()
  register_builtin_shapes()
  register_builtin_layouts()
  register_builtin_themes()
  register_builtin_palettes()

  # igraph is a Suggests — register S3 methods against its generics
  # dynamically so cograph loads even when igraph isn't installed.
  if (requireNamespace("igraph", quietly = TRUE)) {
    registerS3method(
      "modularity", "cograph_communities",
      modularity.cograph_communities,
      envir = asNamespace("igraph")
    )
  }
} # nocov end

