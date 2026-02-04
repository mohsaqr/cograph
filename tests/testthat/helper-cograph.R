# Test helper - load the package
library(cograph)

# Make internal functions available for testing
parse_matrix <- cograph:::parse_matrix
parse_edgelist <- cograph:::parse_edgelist
draw_circle <- cograph:::draw_circle
draw_square <- cograph:::draw_square
draw_triangle <- cograph:::draw_triangle
draw_diamond <- cograph:::draw_diamond
draw_ellipse <- cograph:::draw_ellipse
draw_heart <- cograph:::draw_heart
draw_star <- cograph:::draw_star
layout_circle <- cograph:::layout_circle
layout_spring <- cograph:::layout_spring
layout_groups <- cograph:::layout_groups
recycle_to_length <- cograph:::recycle_to_length

# Additional internal functions for extended tests
if (exists("layout_oval", envir = asNamespace("cograph"), inherits = FALSE)) {
  layout_oval <- cograph:::layout_oval
}

# Converter helper functions
if (exists("map_qgraph_shape", envir = asNamespace("cograph"), inherits = FALSE)) {
  map_qgraph_shape <- cograph:::map_qgraph_shape
}
if (exists("map_qgraph_lty", envir = asNamespace("cograph"), inherits = FALSE)) {
  map_qgraph_lty <- cograph:::map_qgraph_lty
}

# Color utilities
if (exists("adjust_alpha", envir = asNamespace("cograph"), inherits = FALSE)) {
  adjust_alpha <- cograph:::adjust_alpha
}
