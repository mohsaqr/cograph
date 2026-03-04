#' @title Global Variable Declarations
#' @description Declare global variables to avoid R CMD check NOTEs.
#' @name utils-globals
#' @keywords internal
NULL

# Declare global variables used in ggplot2 aes() calls and other NSE contexts
# This prevents "no visible binding for global variable" NOTEs
utils::globalVariables(c(
  "x", "y", "id", "group", "color", "fill", "label",
  "xmin", "xmax", "ymin", "ymax", "value", "total",
  "line_color", "flow_color", "lw"
))
