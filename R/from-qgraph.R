#' Convert a tna object to Sonnet parameters
#'
#' Extracts the transition matrix, labels, and initial state probabilities
#' from a \code{tna} object and plots with Sonnet. Initial probabilities
#' are mapped to donut fills.
#'
#' @param tna_object A \code{tna} object from \code{tna::tna()}
#' @param engine Which Sonnet renderer to use: \code{"splot"} or \code{"soplot"}
#' @param plot If TRUE, immediately plot using the chosen engine
#' @param weight_digits Number of decimal places to round edge weights to. Default 2.
#'   Edges that round to zero are removed unless \code{show_zero_edges = TRUE}.
#' @param show_zero_edges If TRUE, keep edges even if their weight rounds to zero. Default FALSE.
#' @param ... Additional parameters passed to the plotting engine
#' @return Invisibly, a named list of Sonnet parameters
#' @export
from_tna <- function(tna_object, engine = c("splot", "soplot"), plot = TRUE,
                     weight_digits = 2, show_zero_edges = FALSE, ...) {
  engine <- match.arg(engine)

  if (!inherits(tna_object, "tna")) {
    stop("Input does not appear to be a tna object", call. = FALSE)
  }

  overrides <- list(...)

  # --- Weights matrix ---
  x <- tna_object$weights

  # --- Build params ---
  params <- list(
    x          = x,
    labels     = tna_object$labels,
    directed   = TRUE,
    weight_digits     = weight_digits,
    donut_fill = as.numeric(tna_object$inits),
    donut_inner_ratio = 0.8,
    donut_empty       = FALSE
  )

  # --- Apply overrides ---
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
    plot_fn <- switch(engine, splot = splot, soplot = soplot)
    accepted <- names(formals(plot_fn))
    if (!"..." %in% accepted) {
      plot_params <- plot_params[intersect(names(plot_params), accepted)]
    }
    do.call(plot_fn, plot_params)
  }

  invisible(params)
}

#' Convert a qgraph object to Sonnet parameters
#'
#' Extracts the network, layout, and all relevant arguments from a qgraph
#' object and passes them to a Sonnet plotting engine. Reads resolved values
#' from \code{graphAttributes} rather than raw \code{Arguments}.
#'
#' @param qgraph_object Return value of \code{qgraph::qgraph()}
#' @param engine Which Sonnet renderer to use: \code{"splot"} or \code{"soplot"}
#' @param plot If TRUE, immediately plot using the chosen engine
#' @param weight_digits Number of decimal places to round edge weights to. Default 2.
#'   Edges that round to zero are removed unless \code{show_zero_edges = TRUE}.
#' @param show_zero_edges If TRUE, keep edges even if their weight rounds to zero. Default FALSE.
#' @param ... Override any extracted parameter
#' @return Invisibly, a named list of Sonnet parameters
#' @export
from_qgraph <- function(qgraph_object, engine = c("splot", "soplot"), plot = TRUE,
                         weight_digits = 2, show_zero_edges = FALSE, ...) {
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
  el <- q$Edgelist
  if (is.null(x)) {
    n <- length(ga_nodes$names)
    if (is.null(n) || n == 0) n <- max(c(el$from, el$to))
    x <- matrix(0, n, n)
    for (i in seq_along(el$from)) {
      x[el$from[i], el$to[i]] <- el$weight[i]
    }
  }
  n <- nrow(x)

  # --- Build params ---
  params <- list(x = x, weight_digits = weight_digits)

  # Layout: use computed coordinates
  if (!is.null(q$layout)) {
    params$layout <- q$layout
    params$rescale <- FALSE
  }

  # --- Node aesthetics from graphAttributes$Nodes ---
  if (!is.null(ga_nodes$labels))       params$labels            <- ga_nodes$labels
  else if (!is.null(ga_nodes$names))   params$labels            <- ga_nodes$names
  if (!is.null(ga_nodes$color))        params$node_fill         <- ga_nodes$color
  if (!is.null(ga_nodes$width))        params$node_size         <- ga_nodes$width * 1.3
  if (!is.null(ga_nodes$shape))        params$node_shape        <- map_qgraph_shape(ga_nodes$shape)
  if (!is.null(ga_nodes$border.color)) params$node_border_color <- ga_nodes$border.color
  if (!is.null(ga_nodes$border.width)) params$node_border_width <- ga_nodes$border.width
  if (!is.null(ga_nodes$label.cex))    params$label_size        <- ga_nodes$label.cex
  if (!is.null(ga_nodes$label.color))  params$label_color       <- ga_nodes$label.color

  # --- Edge colors from qgraph arguments ---
  if (!is.null(args$posCol))           params$positive_color    <- args$posCol
  if (!is.null(args$negCol))           params$negative_color    <- args$negCol
  if (!is.null(args$theme))            params$theme             <- args$theme

  # --- Pie → Donut mapping ---
  # qgraph pie values are single values per node (e.g. from tna)
  # Use graphAttributes$Nodes$pie which has the resolved values
  pie_data <- ga_nodes$pie
  if (!is.null(pie_data)) {
    n_nodes <- if (is.matrix(x)) nrow(x) else length(ga_nodes$names)
    if (is.list(pie_data)) {
      fill_vec <- vapply(pie_data, function(v) {
        if (is.null(v) || all(is.na(v))) NA_real_ else v[1]
      }, numeric(1))
    } else {
      fill_vec <- as.numeric(pie_data)
    }
    if (length(fill_vec) < n_nodes) {
      fill_vec <- c(fill_vec, rep(NA_real_, n_nodes - length(fill_vec)))
    }
    params$donut_fill <- fill_vec
    params$donut_inner_ratio <- 0.8
    params$donut_empty <- FALSE
  }
  if (!is.null(ga_nodes$pieColor) && !all(is.na(ga_nodes$pieColor)))
    params$donut_color <- ga_nodes$pieColor
  if (!is.null(args$pieColor) && is.null(params$donut_color))
    params$donut_color <- args$pieColor

  # --- Reorder per-edge vectors via matrix intermediary ---
  # qgraph's Edgelist order may differ from Sonnet's which(x!=0, arr.ind=TRUE) order.
  # Place each per-edge vector into an n×n matrix keyed by (from, to), then extract
  # in the order Sonnet will use.
  edge_vec_to_sonnet_order <- function(v) {
    if (is.null(v) || length(v) != length(el$from)) return(v)
    mat <- matrix(NA, n, n)
    for (i in seq_len(length(el$from))) {
      mat[el$from[i], el$to[i]] <- v[i]
    }
    directed <- if (!is.null(el$directed)) any(el$directed) else !isSymmetric(x)
    if (directed) {
      idx <- which(x != 0, arr.ind = TRUE)
    } else {
      idx <- which(upper.tri(x) & x != 0, arr.ind = TRUE)
    }
    mat[idx]
  }

  # --- Edge aesthetics from graphAttributes$Edges ---
  # edge_color and edge_width are intentionally not passed — qgraph bakes its
  # cut-based fading into these vectors, producing near-invisible edges. Let
  # Sonnet apply its own weight-based styling instead.
  if (!is.null(ga_edges$labels))             params$edge_labels         <- edge_vec_to_sonnet_order(ga_edges$labels)
  if (!is.null(ga_edges$label.cex))          params$edge_label_size     <- edge_vec_to_sonnet_order(ga_edges$label.cex) * 0.5
  if (!is.null(ga_edges$lty))                params$edge_style          <- map_qgraph_lty(edge_vec_to_sonnet_order(ga_edges$lty))
  if (!is.null(ga_edges$curve) && length(ga_edges$curve) == 1)
    params$curvature <- ga_edges$curve
  if (!is.null(ga_edges$asize))              params$arrow_size          <- edge_vec_to_sonnet_order(ga_edges$asize) * 0.3
  if (!is.null(ga_edges$edge.label.position)) params$edge_label_position <- edge_vec_to_sonnet_order(ga_edges$edge.label.position)

  # --- Graph-level from graphAttributes$Graph ---
  # cut is intentionally not passed — qgraph's cut causes faint edges with hanging labels
  if (!is.null(ga_graph$minimum))      params$threshold         <- ga_graph$minimum
  if (!is.null(ga_graph$maximum))      params$maximum           <- ga_graph$maximum
  if (!is.null(ga_graph$groups))       params$groups            <- ga_graph$groups

  # --- Directedness from Edgelist ---
  if (!is.null(q$Edgelist$directed))   params$directed          <- any(q$Edgelist$directed)

  # --- Apply overrides (user can override anything) ---
  # Map qgraph-style parameter names to Sonnet equivalents
  qgraph_to_sonnet <- c(minimum = "threshold", cut = "cut")
  for (nm in names(overrides)) {
    sonnet_nm <- if (nm %in% names(qgraph_to_sonnet)) qgraph_to_sonnet[[nm]] else nm
    params[[sonnet_nm]] <- overrides[[nm]]
  }
  # If user overrides layout, remove rescale=FALSE so Sonnet rescales properly
  if ("layout" %in% names(overrides)) {
    params$rescale <- NULL
  }

  # --- Plot ---
  if (plot) {
    plot_params <- params
    if (engine == "soplot") {
      plot_params$network <- plot_params$x
      plot_params$x <- NULL
    }
    plot_fn <- switch(engine, splot = splot, soplot = soplot)
    # Filter to only params accepted by the target engine
    accepted <- names(formals(plot_fn))
    if (!"..." %in% accepted) {
      plot_params <- plot_params[intersect(names(plot_params), accepted)]
    }
    # soplot expects scalar edge params; collapse per-edge vectors
    if (engine == "soplot") {
      edge_scalar_params <- c("edge_style", "arrow_size", "edge_label_size",
                              "edge_label_position")
      for (ep in edge_scalar_params) {
        v <- plot_params[[ep]]
        if (!is.null(v) && length(v) > 1) {
          uv <- unique(v)
          plot_params[[ep]] <- if (length(uv) == 1) uv else uv[1]
        }
      }
    }
    do.call(plot_fn, plot_params)
  }

  invisible(params)
}

#' Map qgraph lty codes to Sonnet edge style names
#' @param lty Numeric or character vector of R line types
#' @return Character vector of Sonnet style names
#' @keywords internal
map_qgraph_lty <- function(lty) {
  mapping <- c("1" = "solid", "2" = "dashed", "3" = "dotted",
               "4" = "dotdash", "5" = "longdash", "6" = "twodash",
               "solid" = "solid", "dashed" = "dashed", "dotted" = "dotted",
               "longdash" = "longdash", "twodash" = "twodash")
  result <- mapping[as.character(lty)]
  result[is.na(result)] <- "solid"
  unname(result)
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
