#' TNA-Style Network Plot (qgraph Compatible)
#'
#' A drop-in replacement for qgraph::qgraph() that uses Sonnet's splot engine.
#' Accepts qgraph parameter names for seamless migration from qgraph to Sonnet.
#'
#' @param input A weight matrix (adjacency matrix) or tna object
#' @param color Node fill colors
#' @param labels Node labels
#' @param layout Layout: "circle", "spring", "oval", or a coordinate matrix
#' @param theme Plot theme ("colorblind", "gray", etc.)
#' @param mar Plot margins (numeric vector of length 4)
#' @param cut Edge emphasis threshold
#' @param edge.labels Show edge weight labels
#' @param edge.label.position Position of edge labels along edge (0-1)
#' @param edge.label.cex Edge label size multiplier
#' @param edge.color Edge colors
#' @param vsize Node size
#' @param pie Pie/donut fill values (e.g., initial probabilities)
#' @param pieColor Pie/donut segment colors
#' @param lty Line type for edges (1=solid, 2=dashed, 3=dotted)
#' @param directed Logical, is the graph directed?
#' @param minimum Minimum edge weight to display
#' @param posCol Color for positive edges
#' @param negCol Color for negative edges
#' @param arrowAngle Arrow head angle in radians. Default pi/6 (30 degrees).
#' @param title Plot title
#' @param ... Additional arguments passed to splot()
#'
#' @return Invisibly returns the sonnet_network object from splot().
#'
#' @export
#'
#' @examples
#' # Simple usage
#' m <- matrix(runif(25), 5, 5)
#' plot_tna(m)
#'
#' # With qgraph-style parameters
#' plot_tna(m, vsize = 15, edge.label.cex = 2, layout = "circle")
#'
#' # With custom colors
#' plot_tna(m, color = rainbow(5), vsize = 10)
#'
plot_tna <- function(
    input,
    color = NULL,
    labels = NULL,
    layout = "oval",
    theme = "colorblind",
    mar = rep(5, 4),
    cut = NULL,
    edge.labels = TRUE,
    edge.label.position = 0.7,
    edge.label.cex = 0.6,
    edge.color = "#003355",
    vsize = 7,
    pie = NULL,
    pieColor = NULL,
    lty = NULL,
    directed = TRUE,
    minimum = NULL,
    posCol = NULL,
    negCol = NULL,
    arrowAngle = NULL,
    title = NULL,
    ...
) {
  # Build splot arguments
  splot_args <- list(
    x = input,
    directed = directed
  )

  # Node parameters
  if (!is.null(color)) splot_args$node_fill <- color
  if (!is.null(labels)) splot_args$labels <- labels
  splot_args$node_size <- vsize

  # Donut/pie parameters (qgraph pie = numeric vector 0-1)
  if (!is.null(pie)) splot_args$donut_fill <- pie
  if (!is.null(pieColor)) splot_args$donut_color <- pieColor

  # Edge parameters
  splot_args$edge_labels <- edge.labels
  splot_args$edge_label_position <- edge.label.position
  splot_args$edge_label_size <- edge.label.cex
  if (!is.null(edge.color)) splot_args$edge_color <- edge.color
  if (!is.null(posCol)) splot_args$edge_positive_color <- posCol
  if (!is.null(negCol)) splot_args$edge_negative_color <- negCol

  # Edge filtering
  if (!is.null(cut)) splot_args$edge_cutoff <- cut
  if (!is.null(minimum)) splot_args$threshold <- minimum

  # Layout and margins
  splot_args$layout <- layout
  splot_args$margins <- mar
  splot_args$theme <- theme

  # Title
  if (!is.null(title)) splot_args$title <- title

  # Line type mapping: qgraph lty (1=solid, 2=dashed, 3=dotted)
  if (!is.null(lty) && length(lty) > 0) {
    lty_map <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    if (is.numeric(lty)) {
      splot_args$edge_style <- lty_map[pmin(lty, 6)]
    } else {
      splot_args$edge_style <- lty
    }
  }

  # TNA defaults for edge styling
  splot_args$edge_start_style <- "dotted"
  splot_args$edge_start_length <- 0.2
  splot_args$arrow_size <- 0.61

  # Arrow angle
  if (!is.null(arrowAngle)) splot_args$arrow_angle <- arrowAngle

  # Call splot
  do.call(splot, c(splot_args, list(...)))
}

#' @rdname plot_tna
#' @export
tplot <- plot_tna
