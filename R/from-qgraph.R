#' Convert a qgraph object to Sonnet parameters
#'
#' Extracts the network, layout, and all relevant arguments from a qgraph
#' object and passes them to a Sonnet plotting engine. Reads resolved values
#' from \code{graphAttributes} rather than raw \code{Arguments}.
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

  q <- qgraph_object
  args <- q$Arguments
  ga_nodes <- q$graphAttributes$Nodes
  ga_edges <- q$graphAttributes$Edges
  ga_graph <- q$graphAttributes$Graph
  overrides <- list(...)

  # --- Input matrix ---
  x <- args$input
  if (is.null(x)) {
    el <- q$Edgelist
    n <- length(ga_nodes$names)
    if (is.null(n) || n == 0) n <- max(c(el$from, el$to))
    x <- matrix(0, n, n)
    for (i in seq_along(el$from)) {
      x[el$from[i], el$to[i]] <- el$weight[i]
    }
  }

  # --- Build params ---
  params <- list(x = x)

  # Layout: use computed coordinates
  if (!is.null(q$layout)) {
    params$layout <- q$layout
    params$rescale <- FALSE
  }

  # --- Node aesthetics from graphAttributes$Nodes ---
  if (!is.null(ga_nodes$labels))       params$labels            <- ga_nodes$labels
  else if (!is.null(ga_nodes$names))   params$labels            <- ga_nodes$names
  if (!is.null(ga_nodes$color))        params$node_fill         <- ga_nodes$color
  if (!is.null(ga_nodes$width))        params$node_size         <- ga_nodes$width
  if (!is.null(ga_nodes$shape))        params$node_shape        <- map_qgraph_shape(ga_nodes$shape)
  if (!is.null(ga_nodes$border.color)) params$node_border_color <- ga_nodes$border.color
  if (!is.null(ga_nodes$border.width)) params$node_border_width <- ga_nodes$border.width
  if (!is.null(ga_nodes$label.cex))    params$label_size        <- ga_nodes$label.cex
  if (!is.null(ga_nodes$label.color))  params$label_color       <- ga_nodes$label.color

  # --- Pie → Donut mapping ---
  if (!is.null(args$pie))              params$donut_fill        <- as.numeric(args$pie)
  if (!is.null(ga_nodes$pieColor))     params$donut_color       <- ga_nodes$pieColor

  # --- Edge aesthetics from graphAttributes$Edges ---
  if (!is.null(ga_edges$color))              params$edge_color          <- ga_edges$color
  if (!is.null(ga_edges$width))              params$edge_width          <- ga_edges$width
  if (!is.null(ga_edges$labels))             params$edge_labels         <- ga_edges$labels
  if (!is.null(ga_edges$label.cex))          params$edge_label_size     <- ga_edges$label.cex
  if (!is.null(ga_edges$lty))                params$edge_style          <- ga_edges$lty
  if (!is.null(ga_edges$curve) && length(ga_edges$curve) == 1)
    params$curvature <- ga_edges$curve
  if (!is.null(ga_edges$asize))              params$arrow_size          <- ga_edges$asize
  if (!is.null(ga_edges$edge.label.position)) params$edge_label_position <- ga_edges$edge.label.position

  # --- Graph-level from graphAttributes$Graph ---
  if (!is.null(ga_graph$cut))          params$cut               <- ga_graph$cut
  if (!is.null(ga_graph$minimum))      params$threshold         <- ga_graph$minimum
  if (!is.null(ga_graph$maximum))      params$maximum           <- ga_graph$maximum
  if (!is.null(ga_graph$groups))       params$groups            <- ga_graph$groups

  # --- Directedness from Edgelist ---
  if (!is.null(q$Edgelist$directed))   params$directed          <- any(q$Edgelist$directed)

  # --- Apply overrides (user can override anything) ---
  for (nm in names(overrides)) {
    params[[nm]] <- overrides[[nm]]
  }

  # --- Plot ---
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
