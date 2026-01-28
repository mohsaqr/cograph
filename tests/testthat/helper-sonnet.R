# Test helper - load the package
library(Sonnet)

# Make internal functions available for testing
parse_matrix <- Sonnet:::parse_matrix
parse_edgelist <- Sonnet:::parse_edgelist
draw_circle <- Sonnet:::draw_circle
draw_square <- Sonnet:::draw_square
draw_triangle <- Sonnet:::draw_triangle
draw_diamond <- Sonnet:::draw_diamond
draw_ellipse <- Sonnet:::draw_ellipse
draw_heart <- Sonnet:::draw_heart
draw_star <- Sonnet:::draw_star
layout_circle <- Sonnet:::layout_circle
layout_spring <- Sonnet:::layout_spring
layout_groups <- Sonnet:::layout_groups
recycle_to_length <- Sonnet:::recycle_to_length

# Additional internal functions for extended tests
if (exists("layout_oval", envir = asNamespace("Sonnet"), inherits = FALSE)) {
  layout_oval <- Sonnet:::layout_oval
}

# Converter helper functions
if (exists("map_qgraph_shape", envir = asNamespace("Sonnet"), inherits = FALSE)) {
  map_qgraph_shape <- Sonnet:::map_qgraph_shape
}
if (exists("map_qgraph_lty", envir = asNamespace("Sonnet"), inherits = FALSE)) {
  map_qgraph_lty <- Sonnet:::map_qgraph_lty
}

# Color utilities
if (exists("adjust_alpha", envir = asNamespace("Sonnet"), inherits = FALSE)) {
  adjust_alpha <- Sonnet:::adjust_alpha
}
