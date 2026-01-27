#' Convert a qgraph object to Sonnet parameters
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

  # Layout: use the computed layout, not the string argument
  if (!is.null(qgraph_object$layout)) {
    params$layout <- qgraph_object$layout
    params$rescale <- FALSE
  }

  # Direct mappings from Arguments
  direct_map <- list(
    directed    = "directed",
    labels      = "labels",
    groups      = "groups",
    vsize       = "node_size",
    vsize2      = "node_size2",
    color       = "node_fill",
    border.color = "node_border_color",
    border.width = "node_border_width",
    label.cex   = "label_size",
    label.color  = "label_color",
    edge.width   = "edge_width",
    esize        = "esize",
    asize        = "arrow_size",
    minimum      = "threshold",
    maximum      = "maximum",
    cut          = "cut",
    curve        = "curvature",
    curveShape   = "curve_shape",
    curveScale   = "curve_scale",
    edge.labels  = "edge_labels",
    edge.label.cex = "edge_label_size",
    title        = "title"
  )

  for (qname in names(direct_map)) {
    val <- args[[qname]]
    if (!is.null(val)) {
      params[[ direct_map[[qname]] ]] <- val
    }
  }

  # Shape mapping
  if (!is.null(args$shape)) {
    params$node_shape <- map_qgraph_shape(args$shape)
  }

  # Edge color: may be n×n matrix, convert to per-edge vector
  if (!is.null(args$edge.color)) {
    ec <- args$edge.color
    if (is.matrix(ec) && !is.null(qgraph_object$Edgelist)) {
      el <- qgraph_object$Edgelist
      params$edge_color <- vapply(seq_along(el$from), function(i) {
        ec[el$from[i], el$to[i]]
      }, character(1))
    } else {
      params$edge_color <- ec
    }
  }

  # posCol / negCol: qgraph uses length-2 vectors; take the second (strong) color
  if (!is.null(args$posCol)) {
    pc <- args$posCol
    params$positive_color <- if (length(pc) >= 2) pc[2] else pc[1]
  }
  if (!is.null(args$negCol)) {
    nc <- args$negCol
    params$negative_color <- if (length(nc) >= 2) nc[2] else nc[1]
  }

  # curveAll
  if (isTRUE(args$curveAll)) {
    params$curves <- "force"
  }

  # arrows
  if (!is.null(args$arrows) && identical(args$arrows, FALSE)) {
    params$show_arrows <- FALSE
  }

  # Apply overrides
  for (nm in names(overrides)) {
    params[[nm]] <- overrides[[nm]]
  }

  # Plot
  if (plot) {
    plot_params <- params
    if (engine == "soplot") {
      plot_params$network <- plot_params$x
      plot_params$x <- NULL
      # Remove params not supported by soplot
      soplot_unsupported <- c("directed", "rescale", "node_size2", "curve_scale")
      plot_params[soplot_unsupported] <- NULL
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
  # For unknown shapes, pass through as-is
  unknown <- is.na(result)
  result[unknown] <- shapes[unknown]
  unname(result)
}
