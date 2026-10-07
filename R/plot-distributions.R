# =============================================================================
# Distribution and Correlation Plots
# =============================================================================


#' Plot Centrality Distribution
#'
#' Histogram or density plot of one centrality measure. The input is either
#' the data frame returned by \code{\link{centrality}} or a network, for
#' which the measure is computed first.
#'
#' @param x A data frame from \code{\link{centrality}}, or a network input
#'   (matrix, igraph, cograph_network, tna).
#' @param measure Character. Column of the centrality output to plot, for
#'   example \code{"degree_all"} (default) or \code{"strength_in"}. An
#'   unknown name is an error that lists the available columns.
#' @param type Character. \code{"histogram"} (default) or \code{"density"}.
#' @param normalize Logical. Show proportions instead of counts. Default FALSE.
#' @param bins Integer or NULL. Number of equal-width histogram bins. The
#'   default NULL uses the Freedman-Diaconis rule.
#' @param log Character. \code{"y"} or \code{"xy"} log-scales the y-axis.
#'   The x-axis is never log-scaled, and any other value gives linear axes.
#'   Default \code{""}.
#' @param col Fill color (line color for the density). Default
#'   \code{"steelblue"}.
#' @param border Bar border color for the histogram. Default \code{"white"}.
#' @param main Plot title. The default NULL builds a title such as
#'   \code{"Degree Distribution"} from the measure name.
#' @param xlab X-axis label. The default NULL uses the measure name.
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}
#'   or \code{\link[graphics]{plot}}.
#'
#' @return Invisibly, a numeric vector of the finite centrality values
#'   plotted.
#' @export
#' @examples
#' cograph::plot_centrality_distribution(regulation_net, measure = "strength_all")
plot_centrality_distribution <- function(x,
                                         measure = "degree_all",
                                         type = c("histogram", "density"),
                                         normalize = FALSE,
                                         bins = NULL,
                                         log = "",
                                         col = "steelblue",
                                         border = "white",
                                         main = NULL,
                                         xlab = NULL,
                                         ...) {

  type <- match.arg(type)

  # Accept centrality data frame or raw network
  if (is.data.frame(x) && "node" %in% names(x)) {
    df <- x
  } else {
    df <- centrality(x, measures = gsub("_all$|_in$|_out$", "", measure))
  }

  if (!measure %in% names(df)) {
    stop("Measure '", measure, "' not found. Available: ",
         paste(setdiff(names(df), "node"), collapse = ", "), call. = FALSE)
  }

  vals <- df[[measure]]
  vals <- vals[is.finite(vals)]

  if (is.null(main)) {
    pretty_name <- gsub("_", " ", gsub("_all$|_in$|_out$", "", measure))
    main <- paste0(toupper(substring(pretty_name, 1, 1)),
                   substring(pretty_name, 2), " Distribution")
  }
  if (is.null(xlab)) xlab <- gsub("_", " ", measure)

  if (type == "density") {
    d <- stats::density(vals, na.rm = TRUE)
    graphics::plot(d, main = main, xlab = xlab, col = col, lwd = 2,
                   log = if (log %in% c("y", "xy")) "y" else "", ...)
    graphics::polygon(d, col = grDevices::adjustcolor(col, 0.3), border = col)
  } else {
    deg_range <- range(vals)
    brks <- if (!is.null(bins)) {
      seq(deg_range[1], deg_range[2], length.out = bins + 1L)
    } else {
      "FD"
    }
    h <- graphics::hist(vals, breaks = brks, plot = FALSE)
    heights <- if (normalize) h$counts / sum(h$counts) else h$counts
    ylab <- if (normalize) "Proportion" else "Frequency"

    graphics::barplot(heights, names.arg = round(h$mids, 2),
                      main = main, xlab = xlab, ylab = ylab,
                      col = col, border = border, space = 0, las = 1,
                      log = if (log %in% c("y", "xy")) "y" else "", ...)
  }

  graphics::grid(nx = NA, ny = NULL,
                 col = grDevices::adjustcolor("gray50", 0.3), lty = 1)
  invisible(vals)
}


#' Plot Edge Weight Distribution
#'
#' Histogram of edge weights in a network. The number of edges and the mean
#' and standard deviation of the weights are printed in the top margin.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna.
#' @param normalize Logical. Show proportions. Default FALSE.
#' @param bins Integer or NULL. Number of equal-width bins. With the default
#'   NULL, integer weights spanning at most 30 units get one bin per integer
#'   and other weights use the Freedman-Diaconis rule.
#' @param log Character. \code{"y"} or \code{"xy"} log-scales the y-axis.
#'   Any other value gives linear axes. Default \code{""}.
#' @param directed Logical or NULL. Default NULL (auto-detect).
#' @param col Fill color. Default \code{"steelblue"}.
#' @param border Border color. Default \code{"white"}.
#' @param main Title. Default \code{"Edge Weight Distribution"}.
#' @param xlab X-axis label. Default \code{"Weight"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}.
#'
#' @return Invisibly, a numeric vector of edge weights (all 1 for an
#'   unweighted network).
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' cograph::plot_edge_weights(regulation_net)
plot_edge_weights <- function(x,
                              normalize = FALSE,
                              bins = NULL,
                              log = "",
                              directed = NULL,
                              col = "steelblue",
                              border = "white",
                              main = "Edge Weight Distribution",
                              xlab = "Weight",
                              ...) {

  g <- to_igraph(x, directed = directed)
  wts <- igraph::E(g)$weight
  if (is.null(wts)) wts <- rep(1, igraph::ecount(g))

  w_range <- range(wts)
  brks <- if (!is.null(bins)) {
    seq(w_range[1], w_range[2], length.out = bins + 1L)
  } else if (w_range[2] - w_range[1] <= 30 && all(wts == floor(wts))) {
    seq(w_range[1] - 0.5, w_range[2] + 0.5, by = 1)
  } else {
    "FD"
  }

  h <- graphics::hist(wts, breaks = brks, plot = FALSE)
  heights <- if (normalize) h$counts / sum(h$counts) else h$counts
  ylab <- if (normalize) "Proportion" else "Frequency"

  bar_names <- vapply(seq_len(length(h$breaks) - 1L), function(i) {
    lo <- h$breaks[i]; hi <- h$breaks[i + 1]
    if (abs(hi - lo - 1) < 0.01 && lo == floor(lo)) {
      as.character(ceiling(lo))
    } else {
      sprintf("%.1f", h$mids[i])
    }
  }, character(1))

  use_log <- if (log %in% c("y", "xy")) "y" else ""
  if (nzchar(use_log)) heights[heights == 0] <- NA

  graphics::barplot(heights, names.arg = bar_names,
                    main = main, xlab = xlab, ylab = ylab,
                    col = col, border = border, space = 0, las = 1,
                    log = use_log, ...)
  graphics::grid(nx = NA, ny = NULL,
                 col = grDevices::adjustcolor("gray50", 0.3), lty = 1)

  n_edges <- length(wts)
  graphics::mtext(sprintf("n = %d edges, mean = %.2f, sd = %.2f",
                          n_edges, mean(wts), stats::sd(wts)),
                  side = 3, adj = 1, cex = 0.8, col = "gray30")

  invisible(wts)
}


#' Plot Degree-Degree Correlation
#'
#' Scatter plot of each node's degree against the average degree of its
#' neighbors. A positive slope indicates assortative mixing and a negative
#' slope indicates disassortative mixing. When more than two nodes have
#' neighbors, a least-squares line is added and the Pearson correlation is
#' printed in the top margin.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna.
#' @param mode Character. Degree type and neighborhood used for directed
#'   networks: \code{"all"} (default), \code{"in"}, or \code{"out"}.
#' @param directed Logical or NULL. Default NULL (auto-detect).
#' @param col Point color. Default \code{"steelblue"}.
#' @param main Title. Default \code{"Degree-Degree Correlation"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}.
#'
#' @return Invisibly, a data frame with one row per node and columns
#'   \code{node}, \code{degree} and \code{avg_neighbor_degree}. The
#'   average neighbor degree is \code{NA} for nodes without neighbors.
#' @seealso \code{\link{centrality}}, \code{\link{degree_distribution}},
#'   \code{\link{network_summary}}
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' cograph::plot_degree_correlation(student_interactions)
plot_degree_correlation <- function(x,
                                    mode = "all",
                                    directed = NULL,
                                    col = "steelblue",
                                    main = "Degree-Degree Correlation",
                                    ...) {

  mode <- match.arg(mode, c("all", "in", "out"))
  g <- to_igraph(x, directed = directed)

  deg <- igraph::degree(g, mode = mode)
  adj_list <- igraph::as_adj_list(g, mode = mode)

  avg_nb_deg <- vapply(seq_along(adj_list), function(i) {
    nbs <- as.integer(adj_list[[i]])
    if (length(nbs) == 0) return(NA_real_)
    mean(deg[nbs])
  }, numeric(1))

  node_names <- igraph::V(g)$name
  if (is.null(node_names)) node_names <- as.character(seq_along(deg))

  # Scatter
  graphics::plot(deg, avg_nb_deg,
                 pch = 16, col = grDevices::adjustcolor(col, 0.6),
                 cex = 1.2,
                 xlab = "Node Degree",
                 ylab = "Avg. Neighbor Degree",
                 main = main, ...)

  # Trend line
  valid <- is.finite(avg_nb_deg) & is.finite(deg)
  if (sum(valid) > 2) {
    fit <- stats::lm(avg_nb_deg[valid] ~ deg[valid])
    graphics::abline(fit, col = "#E41A1C", lwd = 2, lty = 2)

    r <- stats::cor(deg[valid], avg_nb_deg[valid])
    graphics::mtext(sprintf("r = %.3f", r),
                    side = 3, adj = 1, cex = 0.9, col = "gray30")
  }

  graphics::grid(col = grDevices::adjustcolor("gray50", 0.3), lty = 1)

  result <- data.frame(node = node_names, degree = deg,
                       avg_neighbor_degree = avg_nb_deg,
                       stringsAsFactors = FALSE)
  invisible(result)
}


# Coordinates supplied as `layout` to plot_network_evolution(): a matrix or
# data frame with one row per node. Rows named after the nodes are matched by
# name, unnamed rows are taken in node order.
.evolution_layout_coords <- function(layout, node_names) {
  if (!(is.matrix(layout) || is.data.frame(layout)) || ncol(layout) < 2L) {
    stop(errorCondition(
      "layout must be a character string or a matrix or data frame of x and y coordinates.",
      class = c("cograph_bad_parameter", "cograph_error"), call = NULL))
  }
  coords <- as.matrix(layout)[, 1:2, drop = FALSE]
  storage.mode(coords) <- "double"
  if (!is.null(rownames(coords)) && all(node_names %in% rownames(coords))) {
    coords <- coords[node_names, , drop = FALSE]
  } else if (nrow(coords) != length(node_names)) {
    stop(errorCondition(
      sprintf("layout has %d rows but the network has %d nodes.",
              nrow(coords), length(node_names)),
      class = c("cograph_bad_parameter", "cograph_error"), call = NULL))
  }
  dimnames(coords) <- list(node_names, c("x", "y"))
  coords
}


#' Plot Network Evolution (Small Multiples)
#'
#' Plots a network at different time points side by side. The input is an
#' edge list data frame with a time column, a \code{cograph_network} whose
#' stored edge data contain such a column, or a list of networks. All panels
#' share one node layout. At least two periods are required.
#'
#' @param x An edge list data frame with columns \code{from}, \code{to},
#'   optionally \code{weight}, and a time column; a \code{cograph_network}
#'   with stored edge data; or a list of network objects (matrices, igraph,
#'   etc.).
#' @param time Character. Name of the time column in \code{x}. Required for
#'   data frame input and ignored if \code{x} is a list.
#' @param slices Integer or NULL. Number of equal-width bins of the numeric
#'   time column. Default NULL uses the unique time values. Bins work with
#'   \code{cumulative = TRUE} as well.
#' @param cumulative Logical. If TRUE, each panel shows all edges up to that
#'   time point (growing network). If FALSE (default), each panel shows only
#'   edges from that period.
#' @param labels Character vector of panel labels, one per period. The
#'   default NULL uses the time values, or \code{"T1"}, \code{"T2"}, ...
#'   for list input.
#' @param layout Character, or a matrix or data frame of coordinates. Any
#'   character value computes one Fruchterman-Reingold layout from the union
#'   of all edges and uses it for every panel. A matrix or data frame gives
#'   the x and y coordinates of the nodes in its first two columns, one row
#'   per node; rows named after the nodes are matched by name, otherwise they
#'   are taken in node order. Default \code{"spring"}.
#' @param ncol Integer. Number of grid columns. The default NULL uses
#'   \code{min(number of periods, 4)}.
#' @param node_size Numeric. Node size passed to \code{\link{splot}}.
#'   Default 5.
#' @param seed Integer or NULL. Random seed for the shared layout. The
#'   caller's random number state is restored on exit. NULL sets no seed.
#'   Default 42.
#' @param combined Logical. When TRUE (default), the period panels are
#'   arranged in an internal grid via \code{graphics::par(mfrow = ...)}.
#'   When FALSE, the panels are plotted into a layout the caller has already
#'   configured, for example with \code{\link{panel_layout}()}.
#' @param ... Additional arguments passed to \code{\link{splot}}.
#'
#' @return Invisibly, a list with one element per period. For data frame
#'   input each element is the edge-list data frame of that period (all
#'   earlier periods included when \code{cumulative = TRUE}). For list input
#'   it is the input list.
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' set.seed(1)
#' edges <- data.frame(
#'   from = sample(LETTERS[1:5], 30, replace = TRUE),
#'   to   = sample(LETTERS[1:5], 30, replace = TRUE),
#'   week = sample(1:4, 30, replace = TRUE))
#' cograph::plot_network_evolution(edges, time = "week")
plot_network_evolution <- function(x,
                                   time = NULL,
                                   slices = NULL,
                                   cumulative = FALSE,
                                   labels = NULL,
                                   layout = "spring",
                                   ncol = NULL,
                                   node_size = 5,
                                   seed = 42,
                                   combined = TRUE,
                                   ...) {

  # Determine mode: cograph_network, edge list data frame, or pre-built list
  if (inherits(x, "cograph_network")) {
    # Extract original edge data with extra columns
    raw <- x$data
    if (is.null(raw) || !is.data.frame(raw)) {
      stop("cograph_network has no stored edge data. Pass a data.frame with ",
           "a time column instead.", call. = FALSE)
    }
    x <- raw
  }

  if (is.data.frame(x)) {
    stopifnot(!is.null(time), time %in% names(x),
              all(c("from", "to") %in% names(x)))

    time_vals <- x[[time]]

    # Bin into slices if requested
    if (!is.null(slices)) {
      time_vals <- cut(as.numeric(time_vals), breaks = slices,
                       include.lowest = TRUE)
      x[[time]] <- time_vals
    }

    periods <- sort(unique(time_vals))
    if (is.null(labels)) labels <- as.character(periods)

    # Build one edge list per period. Periods are compared by their rank in
    # the sorted periods, which works for numbers, dates, strings and the
    # unordered factor that `slices` produces.
    period_idx <- match(time_vals, periods)
    nets <- lapply(seq_along(periods), function(i) {
      keep <- if (cumulative) period_idx <= i else period_idx == i
      x[keep %in% TRUE, , drop = FALSE]
    })
  } else if (is.list(x)) {
    nets <- x
    if (is.null(labels)) labels <- paste0("T", seq_along(nets))
  } else {
    stop("x must be an edge list data.frame with a time column, or a list ",
         "of networks.", call. = FALSE)
  }

  n_nets <- length(nets)
  stopifnot(n_nets >= 2, length(labels) == n_nets)

  if (is.null(ncol)) ncol <- min(n_nets, 4)
  n_row <- ceiling(n_nets / ncol)

  # Shared layout from the full network (union of all edges)
  if (is.data.frame(x)) {
    ecols <- intersect(names(x), c("from", "to", "weight"))
    full_net <- as_cograph(x[, ecols, drop = FALSE])
  } else {
    full_net <- nets[[n_nets]]
  }
  if (is.character(layout)) {
    if (!is.null(seed)) {
      saved_rng <- .save_rng()
      on.exit(.restore_rng(saved_rng), add = TRUE)
      set.seed(seed)
    }
    g <- to_igraph(full_net)
    shared_layout <- igraph::layout_with_fr(g)
    node_names <- igraph::V(g)$name
    if (is.null(node_names)) {
      node_names <- as.character(seq_len(igraph::vcount(g)))
    }
    rownames(shared_layout) <- node_names
  } else {
    node_names <- get_labels(as_cograph(full_net))
    shared_layout <- .evolution_layout_coords(layout, node_names)
  }

  if (combined) {
    old_par <- graphics::par(mfrow = c(n_row, ncol), mar = c(1, 1, 2, 1))
    on.exit(graphics::par(old_par), add = TRUE)
  }

  # Build adjacency matrices with ALL nodes (shared across panels)
  all_nodes <- node_names
  ecols <- c("from", "to", "weight")
  lapply(seq_len(n_nets), function(i) {
    net_i <- nets[[i]]
    if (is.data.frame(net_i)) {
      # Build full adjacency matrix with all nodes, only this slice's edges
      el <- net_i[, intersect(names(net_i), ecols), drop = FALSE]
      nn <- length(all_nodes)
      mat <- matrix(0, nn, nn, dimnames = list(all_nodes, all_nodes))
      has_w <- "weight" %in% names(el)
      vapply(seq_len(nrow(el)), function(r) {
        fi <- match(el$from[r], all_nodes)
        ti <- match(el$to[r], all_nodes)
        if (!is.na(fi) && !is.na(ti)) {
          mat[fi, ti] <<- mat[fi, ti] + if (has_w) el$weight[r] else 1
        }
        TRUE
      }, logical(1))
      net_i <- mat
    }
    splot(net_i, layout = shared_layout, node_size = node_size,
          title = labels[i], rescale = FALSE, layout_scale = 1, ...)
  })

  invisible(nets)
}
