#' @title Global Variable Declarations
#' @description Declare global variables to avoid R CMD check NOTEs.
#' @name utils-globals
#' @keywords internal
NULL

# Declare global variables used in ggplot2 aes() calls and other NSE contexts
# This prevents "no visible binding for global variable" NOTEs
utils::globalVariables(c(
  # ggplot2 aesthetics
  "x", "y", "z", "id", "group", "color", "fill", "label",
  "xmin", "xmax", "ymin", "ymax", "value", "total",
  "motif", "triad", "count", "direction", "window",
  "line_color", "flow_color", "lw",


  # Apply function variables
  "sd"
))

#' @importFrom stats pnorm sd
#' @importFrom graphics abline legend lines
#' @keywords internal
NULL
