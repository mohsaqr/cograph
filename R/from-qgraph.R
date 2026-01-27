#' Convert a qgraph object to Sonnet parameters
#'
#' Extracts the network, layout, and all relevant arguments from a qgraph
#' object and passes them to a Sonnet plotting engine.
#'
#' @param qgraph_object Return value of \code{qgraph::qgraph()}
#' @param engine Which Sonnet renderer to use: \code{"splot"}, \code{"soplot"}, or \code{"sonplot"}
#' @param plot If TRUE, immediately plot using the chosen engine
#' @param ... Override any extracted parameter
#' @return Invisibly, a named list of Sonnet parameters
#' @export
from_qgraph <- function(qgraph_object, engine = c("splot", "soplot", "sonplot"), plot = TRUE, ...) {
  engine <- match.arg(engine)

  if (!inherits(qgraph_object, "qgraph") && is.null(qgraph_object$Arguments)) {
    stop("Input does not appear to be a qgraph object (missing 'Arguments' field)")
  }

  args <- qgraph_object$Arguments
  overrides <- list(...)

  # --- Input matrix ---
  x <- args$input
  if (is.null(x)) {
    el <- qgraph_object$Edgelist
    n <- length(qgraph_object$graphAttributes$Nodes$names)
    if (is.null(n) || n == 0) n <- max(c(el$from, el$to))
    x <- matrix(0, n, n)
    for (i in seq_along(el$from)) {
      x[el$from[i], el$to[i]] <- el$weight[i]
    }
  }

  # --- Build params ---
  params <- list(x = x)

  # Layout: use computed coordinates from qgraph_object$layout
  if (!is.null(qgraph_object$layout)) {
    params$layout <- qgraph_object$layout
  }

  # --- Map qgraph Arguments to Sonnet params ---
  # Node aesthetics
  if (!is.null(args$labels))       params$labels           <- args$labels
  if (!is.null(args$groups))       params$groups           <- args$groups
  if (!is.null(args$color))        params$node_fill        <- args$color
  if (!is.null(args$vsize))        params$node_size        <- args$vsize
  if (!is.null(args$vsize2))       params$node_size2       <- args$vsize2
  if (!is.null(args$shape))        params$node_shape       <- map_qgraph_shape(args$shape)
  if (!is.null(args$border.color)) params$node_border_color <- args$border.color
  if (!is.null(args$border.width)) params$node_border_width <- args$border.width
  if (!is.null(args$label.cex))    params$label_size       <- args$label.cex
  if (!is.null(args$label.color))  params$label_color      <- args$label.color

  # Pie charts
  if (!is.null(args$pie))          params$pie_values       <- args$pie
  if (!is.null(args$pieColor))     params$pie_colors       <- args$pieColor

  # Edge aesthetics
  if (!is.null(args$edge.color))   params$edge_color       <- args$edge.color
  if (!is.null(args$edge.labels))  params$edge_labels      <- args$edge.labels
  if (!is.null(args$edge.label.cex))      params$edge_label_size     <- args$edge.label.cex
  if (!is.null(args$edge.label.position)) params$edge_label_position <- args$edge.label.position
  if (!is.null(args$edge.width))   params$edge_width       <- args$edge.width
  if (!is.null(args$esize))        params$esize            <- args$esize
  if (!is.null(args$asize))        params$arrow_size       <- args$asize
  if (!is.null(args$lty))          params$edge_style       <- args$lty
  if (!is.null(args$curve))        params$curvature        <- args$curve
  if (!is.null(args$curveShape))   params$curve_shape      <- args$curveShape
  if (!is.null(args$curveScale))   params$curve_scale      <- args$curveScale
  if (isTRUE(args$curveAll))       params$curves           <- "force"
  if (!is.null(args$directed))     params$directed         <- args$directed
  if (!is.null(args$arrows) && identical(args$arrows, FALSE)) params$show_arrows <- FALSE

  # Thresholds
  if (!is.null(args$cut))          params$cut              <- args$cut
  if (!is.null(args$minimum))      params$threshold        <- args$minimum
  if (!is.null(args$maximum))      params$maximum          <- args$maximum

  # Colors
  if (!is.null(args$posCol)) {
    pc <- args$posCol
    params$positive_color <- if (length(pc) >= 2) pc[2] else pc[1]
  }
  if (!is.null(args$negCol)) {
    nc <- args$negCol
    params$negative_color <- if (length(nc) >= 2) nc[2] else nc[1]
  }

  # Plot settings
  if (!is.null(args$title))        params$title            <- args$title
  if (!is.null(args$mar))          params$margins          <- args$mar

  # Apply overrides (user can override anything)
  for (nm in names(overrides)) {
    params[[nm]] <- overrides[[nm]]
  }

  # Plot
  if (plot) {
    plot_params <- params
    if (engine == "soplot") {
      plot_params$network <- plot_params$x
      plot_params$x <- NULL
    }
    plot_fn <- switch(engine, splot = splot, soplot = soplot, sonplot = sonplot)
    do.call(plot_fn, plot_params)
  }

  invisible(params)
}

#' Map qgraph shape names to Sonnet equivalents
#' @param shapes Character vector of qgraph shape names
#' @return Character vector of Sonnet shape names
#' @keywords internal
map_qgraph_shape <- function(shapes) {
  mapping <- c(
    "rectangle" = "square",
    "square"    = "square",
    "circle"    = "circle",
    "ellipse"   = "circle",
    "triangle"  = "triangle",
    "diamond"   = "diamond"
  )
  result <- mapping[shapes]
  unknown <- is.na(result)
  result[unknown] <- shapes[unknown]
  unname(result)
}
