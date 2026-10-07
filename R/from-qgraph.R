#' Generate TNA-style Color Palette for Nodes
#'
#' Internal function that generates appropriate qualitative colors based on
#' the number of states, following TNA's color palette logic.
#'
#' @param n_states Number of states (nodes) in the network.
#' @return Character vector of colors.
#' @keywords internal
#' @noRd
tna_color_palette <- function(n_states) {
  color_group <- 4L -
    1L * (n_states <= 2) -
    1L * (n_states <= 8) -
    1L * (n_states <= 12)

  # Check for required packages with fallbacks
  switch(color_group,
    # 1-2 states: Accent palette (first n colors)
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      RColorBrewer::brewer.pal(n = 3, name = "Accent")[seq_len(n_states)]
    } else { # nocov start
      grDevices::hcl.colors(n_states, palette = "Set 2")
    }, # nocov end
    # 3-8 states: Full Accent palette
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      RColorBrewer::brewer.pal(n = n_states, name = "Accent")
    } else { # nocov start
      grDevices::hcl.colors(n_states, palette = "Set 2")
    }, # nocov end
    # 9-12 states: Set3 palette
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      RColorBrewer::brewer.pal(n = n_states, name = "Set3")
    } else { # nocov start
      grDevices::hcl.colors(n_states, palette = "Set 3")
    }, # nocov end
    # 13+ states: colorspace qualitative HCL
    if (requireNamespace("colorspace", quietly = TRUE)) {
      colorspace::qualitative_hcl(n = n_states, palette = "Set 3")
    } else { # nocov start
      grDevices::hcl.colors(n_states, palette = "Set 3")
    } # nocov end
  )
}

#' Psych Network Visual Style Defaults
#'
#' Returns the standard psych network visual defaults. Used by
#' \code{splot(psych_styling = TRUE)} for undirected association networks.
#'
#' @param n_nodes Number of nodes (for palette selection).
#' @return A named list of default parameters.
#' @keywords internal
#' @noRd
.psych_style_defaults <- function(n_nodes = NULL) {
  defaults <- list(
    layout                  = "spring",
    directed                = FALSE,
    show_arrows             = FALSE,
    edge_style              = 1,
    edge_label_style        = "estimate",
    edge_label_leading_zero = FALSE,
    # Baseline calibrated against large canvases (10"+): 0.6 produced edge
    # weight labels that looked outsized after device-scale multiplication.
    # 0.4 scales down to readable-small at the reference and stays
    # proportional after multiplication at posters.
    edge_label_size         = 0.4,
    edge_label_position     = 0.5,
    node_size               = 7,
    minimum                 = 0.01,
    donut_bg_color          = "white",
    donut_border_width      = 0.5,
    donut_inner_border_color = "white",
    donut_inner_border_width = 0.5
  )
  if (!is.null(n_nodes)) {
    # Skip the first color (black) in Okabe-Ito
    oi <- grDevices::palette.colors(9, palette = "Okabe-Ito")[-1]
    defaults$node_fill <- rep_len(oi, n_nodes)
  }
  defaults
}


#' TNA Visual Style Defaults
#'
#' Returns the standard TNA visual defaults as a named list. Used by
#' \code{splot(tna_styling = TRUE)}, \code{from_tna()}, and \code{plot_tna()}.
#'
#' @param n_nodes Number of nodes (for color palette). NULL skips node_fill.
#' @param directed Logical. If TRUE, includes arrow/edge-start defaults.
#' @return Named list of splot parameters.
#' @keywords internal
#' @noRd
.tna_style_defaults <- function(n_nodes = NULL, directed = TRUE) {
  defaults <- list(
    layout                 = "oval",
    edge_labels            = TRUE,
    edge_label_style       = "estimate",
    edge_label_leading_zero = FALSE,
    # See note on psych edge_label_size above — same calibration: 0.6 was
    # too large after device-scale multiplication at larger canvases.
    edge_label_size        = 0.4,
    edge_color             = COGRAPH_SCALE$tna_edge_color,
    edge_label_position    = 0.7,
    node_size              = 7,
    minimum                = 0.01
  )
  if (!is.null(n_nodes)) {
    defaults$node_fill <- tna_color_palette(n_nodes)
  }
  if (isTRUE(directed)) {
    defaults$arrow_size        <- 0.61
    defaults$edge_start_length <- 0.2
    defaults$edge_start_style  <- "dotted"
  }
  defaults
}

#' Keep edges that rounding would otherwise delete
#'
#' Zero is how this representation stores "no edge", so an edge whose weight
#' rounds to zero at `digits` vanishes from the plot entirely. When the caller
#' asked to keep such edges, each one is nudged to the smallest magnitude
#' `digits` can express, carrying its sign, so it survives the engine's
#' rounding and is still drawn. Every other weight is left exactly as it was,
#' and a matrix with nothing at risk is returned unchanged.
#' @noRd
.keep_rounded_zero_edges <- function(x, digits) {
  if (!is.matrix(x) || is.null(digits)) return(x)
  vanishing <- !is.na(x) & x != 0 & round(x, digits) == 0
  if (!any(vanishing)) return(x)
  x[vanishing] <- sign(x[vanishing]) * 10^(-digits)
  x
}

#' Convert a tna object to cograph parameters
#'
#' Extracts the transition matrix, labels, and initial state probabilities
#' from a \code{tna} object and plots the network with a cograph engine.
#' Initial probabilities are mapped to donut fills.
#'
#' @param tna_object A \code{tna} object from \code{tna::tna()}
#' @param engine Which cograph renderer to use: \code{"splot"} or \code{"soplot"}.
#'   Default: \code{"splot"}.
#' @param plot Logical. If TRUE (default), immediately plot using the chosen engine.
#' @param weight_digits Number of decimal places to round edge weights to.
#'   Default \code{NULL}, which picks the number of digits from the matrix:
#'   \code{0} when every non-zero weight is a whole number (counts, as in
#'   \code{ftna}/\code{ctna} models) and \code{2} otherwise (probabilities).
#'   Edges whose weight rounds to zero at this precision are dropped unless
#'   \code{show_zero_edges = TRUE}.
#' @param show_zero_edges Logical. A zero weight means that the edge is
#'   absent, so an edge whose weight rounds to zero at \code{weight_digits}
#'   is dropped. With \code{TRUE} each such edge is plotted at the smallest
#'   magnitude that \code{weight_digits} can express, with its original
#'   sign. Other weights are unchanged. Default: \code{FALSE}.
#' @param ... Additional parameters passed to the plotting engine (e.g., \code{layout},
#'   \code{node_fill}, \code{donut_color}).
#'
#' @details
#' ## Conversion Process
#' The transition matrix (\code{weights}) supplies the edge weights, the state
#' labels (\code{labels}) supply the node labels, and the initial state
#' probabilities (\code{inits}) supply the \code{donut_fill} values. The
#' donuts are plotted with \code{donut_inner_ratio = 0.8}.
#'
#' Directedness is read from the tna object when it is recorded there.
#' Otherwise a symmetric matrix is treated as undirected and an asymmetric
#' matrix as directed.
#'
#' ## TNA Visual Defaults
#' The following defaults are applied. Each can be overridden through
#' \code{...}.
#' \itemize{
#'   \item \code{layout = "oval"}.
#'   \item \code{node_fill}: the RColorBrewer Accent palette for up to 8
#'     states, Set3 for 9 to 12 states, and a qualitative HCL palette for more.
#'   \item \code{node_size = 7}.
#'   \item \code{edge_color = "#003355"}.
#'   \item \code{edge_labels = TRUE}, with \code{edge_label_size = 0.4} and
#'     \code{edge_label_position = 0.7}.
#'   \item \code{edge_label_style = "estimate"} and
#'     \code{edge_label_leading_zero = FALSE}, so a label shows the weight
#'     alone without a leading zero (for example \code{.42}).
#'   \item \code{minimum = 0.01}, so transitions weaker than 0.01 are not
#'     plotted.
#'   \item For directed networks, \code{arrow_size = 0.61},
#'     \code{edge_start_style = "dotted"} and \code{edge_start_length = 0.2}
#'     (the first 20 percent of each edge is dotted).
#' }
#'
#' @return Invisibly, a named list of plotting parameters (the weight matrix
#'   \code{x}, \code{labels}, \code{directed}, the donut settings and the
#'   visual defaults above, after the overrides in \code{...}). It can be
#'   passed to \code{splot()} with \code{do.call()}. An input that is not a
#'   \code{tna} object raises an error.
#'
#' @seealso
#' \code{\link{cograph}} for creating networks from scratch,
#' \code{\link{splot}} and \code{\link{soplot}} for plotting engines,
#' \code{\link{from_qgraph}} for qgraph object conversion
#'
#' @examplesIf requireNamespace("tna", quietly = TRUE)
#' from_tna(tna::tna(regulation_net))
#'
#' @export
from_tna <- function(tna_object, engine = c("splot", "soplot"), plot = TRUE,
                     weight_digits = NULL, show_zero_edges = FALSE, ...) {
  engine <- match.arg(engine)

  if (!inherits(tna_object, "tna")) {
    stop("Input does not appear to be a tna object", call. = FALSE)
  }

  overrides <- list(...)

  # --- Weights matrix ---
  x <- tna_object$weights

  # Drop ".00" tails on count-valued matrices (ftna, ctna, raw frequencies).
  if (is.null(weight_digits)) {
    nz <- x[x != 0]
    weight_digits <- if (length(nz) > 0 && all(nz == floor(nz))) 0L else 2L
  }

  if (isTRUE(show_zero_edges)) {
    x <- .keep_rounded_zero_edges(x, weight_digits)
  }

  # --- Determine directedness ---
  # Read from tna object's $directed field if present, otherwise auto-detect
  # from matrix symmetry (co-occurrence networks are symmetric/undirected)
  is_directed <- if (!is.null(tna_object$directed)) {
    tna_object$directed
  } else if (!is.null(attr(tna_object, "directed"))) {
    attr(tna_object, "directed")
  } else {
    !is_symmetric_matrix(x)
  }

  # --- Build params ---
  n_states <- nrow(x)

  params <- list(
    x          = x,
    labels     = tna_object$labels,
    directed   = is_directed,
    weight_digits     = weight_digits,
    edge_label_digits = weight_digits,
    donut_fill = as.numeric(tna_object$inits),
    donut_inner_ratio = 0.8,
    donut_empty       = FALSE
  )

  # --- TNA-specific visual defaults ---
  tna_defaults <- .tna_style_defaults(n_states, is_directed)
  params <- c(params, tna_defaults)

  # --- Apply overrides ---
  # Use single-bracket assignment with list() so a user passing
  # `from_tna(model, donut_fill = NULL)` actually stores NULL instead of
  # deleting the key (R list NULL trap; see CLAUDE.md).
  for (nm in names(overrides)) {
    params[nm] <- list(overrides[[nm]])
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

#' Convert a qgraph object to cograph parameters
#'
#' Extracts the network, the layout and the plotting arguments from a qgraph
#' object and passes them to a cograph plotting engine. Node, edge and graph
#' settings are read from the resolved \code{graphAttributes} of the object.
#' The colors \code{posCol}, \code{negCol} and the \code{theme} are read from
#' its \code{Arguments}.
#'
#' @param qgraph_object Return value of \code{qgraph::qgraph()}
#' @param engine Which cograph renderer to use: \code{"splot"} or \code{"soplot"}.
#'   Default: \code{"splot"}.
#' @param plot Logical. If TRUE (default), immediately plot using the chosen engine.
#' @param weight_digits Number of decimal places to round edge weights to. Default 2.
#'   Edges whose weight rounds to zero at this precision are dropped unless
#'   \code{show_zero_edges = TRUE}.
#' @param show_zero_edges Logical. A zero weight means that the edge is
#'   absent, so an edge whose weight rounds to zero at \code{weight_digits}
#'   is dropped. With \code{TRUE} each such edge is plotted at the smallest
#'   magnitude that \code{weight_digits} can express, with its original
#'   sign. Other weights are unchanged. Default: \code{FALSE}.
#' @param preserve_node_size Logical. If TRUE, use the node sizes extracted
#'   from the qgraph object. Default FALSE uses cograph's standard sizing.
#' @param ... Overrides for any extracted parameter, given by cograph name
#'   (for example \code{threshold}). The qgraph names \code{minimum} and
#'   \code{cut} are translated to \code{threshold} and \code{edge_cutoff}.
#'
#' @details
#' ## Parameter Mapping
#' The following qgraph parameters are extracted and mapped to their cograph
#' equivalents.
#'
#' Node properties:
#' \itemize{
#'   \item \code{labels}/\code{names} \code{->} \code{labels}
#'   \item \code{color} \code{->} \code{node_fill}
#'   \item \code{width} \code{->} \code{node_size} (scaled by 1.3x) when
#'     \code{preserve_node_size = TRUE}
#'   \item \code{shape} \code{->} \code{node_shape} (mapped to cograph equivalents)
#'   \item \code{border.color} \code{->} \code{node_border_color}
#'   \item \code{border.width} \code{->} \code{node_border_width}
#'   \item \code{label.cex} \code{->} \code{label_size}
#'   \item \code{label.color} \code{->} \code{label_color}
#' }
#'
#' Edge properties:
#' \itemize{
#'   \item \code{labels} \code{->} \code{edge_labels}
#'   \item \code{label.cex} \code{->} \code{edge_label_size} (scaled by 0.5x)
#'   \item \code{lty} \code{->} \code{edge_style} (numeric to name conversion)
#'   \item \code{curve} \code{->} \code{curvature} (only when qgraph resolved a
#'     single curvature for the whole graph)
#'   \item \code{asize} \code{->} \code{arrow_size} (scaled by 0.3x)
#'   \item \code{edge.label.position} \code{->} \code{edge_label_position}
#' }
#'
#' Graph properties:
#' \itemize{
#'   \item \code{minimum} \code{->} \code{threshold}
#'   \item \code{maximum} \code{->} \code{maximum}
#'   \item \code{groups} \code{->} \code{groups}
#'   \item \code{directed} \code{->} \code{directed}
#'   \item \code{posCol}/\code{negCol} \code{->} \code{edge_positive_color}/\code{edge_negative_color}
#'   \item \code{theme} \code{->} \code{theme}
#' }
#'
#' Pie and donut:
#' \itemize{
#'   \item \code{pie} values \code{->} \code{donut_fill} with
#'     \code{donut_inner_ratio = 0.8} and \code{donut_empty = FALSE}
#'   \item \code{pieColor} \code{->} \code{donut_color}
#' }
#'
#' ## Settings that are not extracted
#' The edge colors and widths are not extracted, because qgraph stores them
#' with its \code{cut}-based fading applied. cograph styles the edges by weight
#' instead. The \code{cut} value is not extracted either.
#'
#' ## Layout
#' The qgraph layout coordinates are kept with \code{rescale = FALSE}. When
#' \code{layout} is supplied in \code{...}, \code{rescale} is removed and
#' the new layout is rescaled.
#'
#' @return Invisibly, a named list of plotting parameters (the weight matrix
#'   \code{x}, \code{weight_digits}, the layout and the extracted node, edge
#'   and graph settings, after the overrides in \code{...}). It can be passed
#'   to \code{splot()} with \code{do.call()}. An input without an
#'   \code{Arguments} field that does not inherit from \code{"qgraph"} raises
#'   an error.
#'
#' @seealso
#' \code{\link{cograph}} for creating networks from scratch,
#' \code{\link{splot}} and \code{\link{soplot}} for plotting engines,
#' \code{\link{from_tna}} for tna object conversion
#'
#' @examplesIf requireNamespace("qgraph", quietly = TRUE)
#' q <- qgraph::qgraph(regulation_net, DoNotPlot = TRUE)
#' from_qgraph(q)
#'
#' @export
from_qgraph <- function(qgraph_object, engine = c("splot", "soplot"), plot = TRUE,
                         weight_digits = 2, show_zero_edges = FALSE,
                         preserve_node_size = FALSE, ...) {
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

  if (isTRUE(show_zero_edges)) {
    x <- .keep_rounded_zero_edges(x, weight_digits)
  }

  # --- Build params ---
  params <- list(x = x, weight_digits = weight_digits)

  # Layout: use computed coordinates
  if (!is.null(q$layout)) {
    params$layout <- q$layout
    params$rescale <- FALSE
  }

  # --- Node aesthetics from graphAttributes$Nodes ---
  if (!is.null(ga_nodes$labels))       params$labels            <- ga_nodes$labels
  else if (!is.null(ga_nodes$names))   params$labels            <- ga_nodes$names # nocov
  if (!is.null(ga_nodes$color))        params$node_fill         <- ga_nodes$color
  # qgraph's per-node `width` values are only ported over when the caller
  # explicitly opts in. Historically this override wrote the translated
  # qgraph width into `node_size` unconditionally, which made identical
  # data plotted through splot vs. from_qgraph render at visibly
  # different sizes. With preserve_node_size = FALSE (new default),
  # imported networks use cograph's standard default node size — for
  # cross-package visual consistency.
  if (isTRUE(preserve_node_size) && !is.null(ga_nodes$width)) {
    params$node_size <- ga_nodes$width * 1.3
  }
  if (!is.null(ga_nodes$shape))        params$node_shape        <- map_qgraph_shape(ga_nodes$shape)
  if (!is.null(ga_nodes$border.color)) params$node_border_color <- ga_nodes$border.color
  if (!is.null(ga_nodes$border.width)) params$node_border_width <- ga_nodes$border.width
  if (!is.null(ga_nodes$label.cex))    params$label_size        <- ga_nodes$label.cex
  if (!is.null(ga_nodes$label.color))  params$label_color       <- ga_nodes$label.color

  # --- Edge colors from qgraph arguments ---
  if (!is.null(args$posCol))           params$edge_positive_color    <- args$posCol
  if (!is.null(args$negCol))           params$edge_negative_color    <- args$negCol
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
  # qgraph's Edgelist order may differ from cograph's which(x!=0, arr.ind=TRUE) order.
  # Place each per-edge vector into an n×n matrix keyed by (from, to), then extract
  # in the order cograph will use.
  edge_vec_to_cograph_order <- function(v) {
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
  # cograph apply its own weight-based styling instead.
  if (!is.null(ga_edges$labels))             params$edge_labels         <- edge_vec_to_cograph_order(ga_edges$labels)
  if (!is.null(ga_edges$label.cex))          params$edge_label_size     <- edge_vec_to_cograph_order(ga_edges$label.cex) * 0.5
  if (!is.null(ga_edges$lty))                params$edge_style          <- map_qgraph_lty(edge_vec_to_cograph_order(ga_edges$lty))
  if (!is.null(ga_edges$curve) && length(ga_edges$curve) == 1)
    params$curvature <- ga_edges$curve
  if (!is.null(ga_edges$asize))              params$arrow_size          <- edge_vec_to_cograph_order(ga_edges$asize) * 0.3
  if (!is.null(ga_edges$edge.label.position)) params$edge_label_position <- edge_vec_to_cograph_order(ga_edges$edge.label.position)

  # --- Graph-level from graphAttributes$Graph ---
  # cut is intentionally not passed — qgraph's cut causes faint edges with hanging labels
  if (!is.null(ga_graph$minimum))      params$threshold         <- ga_graph$minimum
  if (!is.null(ga_graph$maximum))      params$maximum           <- ga_graph$maximum
  if (!is.null(ga_graph$groups))       params$groups            <- ga_graph$groups

  # --- Directedness from Edgelist ---
  if (!is.null(q$Edgelist$directed))   params$directed          <- any(q$Edgelist$directed)

  # --- Apply overrides (user can override anything) ---
  # Map qgraph-style parameter names to cograph equivalents
  qgraph_to_cograph <- c(minimum = "threshold", cut = "edge_cutoff")
  # Single-bracket assignment with list() so explicit NULL overrides actually
  # land as NULL instead of deleting the key (R list NULL trap; see CLAUDE.md).
  for (nm in names(overrides)) {
    cograph_nm <- if (nm %in% names(qgraph_to_cograph)) qgraph_to_cograph[[nm]] else nm
    params[cograph_nm] <- list(overrides[[nm]])
  }
  # If user overrides layout, remove rescale=FALSE so cograph rescales properly
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

#' Map qgraph lty codes to cograph edge style names
#' @param lty Numeric or character vector of R line types
#' @return Character vector of cograph style names
#' @keywords internal
#' @noRd
map_qgraph_lty <- function(lty) {
  mapping <- c("1" = "solid", "2" = "dashed", "3" = "dotted",
               "4" = "dotdash", "5" = "longdash", "6" = "twodash",
               "solid" = "solid", "dashed" = "dashed", "dotted" = "dotted",
               "longdash" = "longdash", "twodash" = "twodash")
  result <- mapping[as.character(lty)]
  result[is.na(result)] <- "solid"
  unname(result)
}

#' Map qgraph shape names to cograph equivalents
#' @param shapes Character vector of qgraph shape names
#' @return Character vector of cograph shape names
#' @keywords internal
#' @noRd
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

#' Translate qgraph-style parameter names to cograph equivalents
#'
#' When splot() receives a tna object, users often pass qgraph-style parameter
#' names (e.g., \code{size = 20}, \code{edge.color = "red"}) because the tna
#' package uses qgraph for plotting. This function renames those keys to their
#' cograph equivalents and applies value transforms where needed.
#'
#' @param dots Named list (typically from \code{list(...)}).
#' @return Named list with qgraph keys renamed to cograph equivalents.
#' @keywords internal
#' @noRd
.translate_qgraph_dots <- function(dots) {
  if (length(dots) == 0L || is.null(names(dots))) return(dots)

  # qgraph name -> cograph name
  name_map <- c(
    "size"                = "node_size",
    "vsize"               = "node_size",
    "color"               = "node_fill",
    "pie"                 = "donut_fill",
    "pieColor"            = "donut_color",
    "edge.labels"         = "edge_labels",
    "edge.label.position" = "edge_label_position",
    "edge.label.cex"      = "edge_label_size",
    "edge.label.color"    = "edge_label_color",
    "edge.color"          = "edge_color",
    "posCol"              = "edge_positive_color",
    "negCol"              = "edge_negative_color",
    "lty"                 = "edge_style",
    "arrowAngle"          = "arrow_angle",
    "mar"                 = "margins",
    "label.cex"           = "label_size",
    "label.color"         = "label_color",
    "border.color"        = "node_border_color",
    "border.width"        = "node_border_width",
    "asize"               = "arrow_size",
    "shape"               = "node_shape"
  )

  orig_nms <- names(dots)
  mapped <- name_map[orig_nms]
  has_mapping <- !is.na(mapped)

  # Track which qgraph names were translated (for value transforms below)
  translated_from <- character(0)

  # Rename: skip if cograph name already present (cograph wins)
  for (idx in which(has_mapping)) {
    cograph_nm <- mapped[idx]
    if (cograph_nm %in% orig_nms) next
    translated_from <- c(translated_from, orig_nms[idx])
    names(dots)[idx] <- cograph_nm
  }

  # Value transforms — only when the value came from a qgraph alias
  if ("asize" %in% translated_from) {
    dots$arrow_size <- dots$arrow_size * 0.20
  }
  if ("edge.label.cex" %in% translated_from) {
    dots$edge_label_size <- dots$edge_label_size * 1.2
  }
  if ("lty" %in% translated_from) {
    dots$edge_style <- map_qgraph_lty(dots$edge_style)
  }
  if ("shape" %in% translated_from) {
    dots$node_shape <- map_qgraph_shape(dots$node_shape)
  }

  dots
}
