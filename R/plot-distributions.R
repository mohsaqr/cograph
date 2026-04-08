# =============================================================================
# Distribution and Correlation Plots
# =============================================================================


#' Plot Centrality Distribution
#'
#' Histogram or density plot of any centrality measure. Accepts the output
#' of \code{\link{centrality}} directly.
#'
#' @param x A data frame from \code{\link{centrality}}, or a network input
#'   (matrix, igraph, cograph_network, tna).
#' @param measure Character. Which centrality measure to plot. Default
#'   \code{"degree_all"}. Must match a column name in the centrality output.
#' @param type Character. \code{"histogram"} (default) or \code{"density"}.
#' @param normalize Logical. Show proportions instead of counts. Default FALSE.
#' @param bins Integer or NULL. Number of bins. Default NULL (auto).
#' @param log Character. Log scaling: \code{""}, \code{"x"}, \code{"y"}, or
#'   \code{"xy"}. Default \code{""}.
#' @param col Fill color. Default \code{"steelblue"}.
#' @param border Border color. Default \code{"white"}.
#' @param main Plot title. Default auto-generated from measure name.
#' @param xlab X-axis label. Default auto-generated.
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}
#'   or \code{\link[graphics]{plot}}.
#'
#' @return Invisibly returns the centrality values plotted.
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:4]
#' cograph::plot_centrality_distribution(adj, measure = "degree_all")
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
#' Histogram of edge weights in a network.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna.
#' @param normalize Logical. Show proportions. Default FALSE.
#' @param bins Integer or NULL. Number of bins. Default NULL (auto).
#' @param log Character. Log scaling. Default \code{""}.
#' @param directed Logical or NULL. Default NULL (auto-detect).
#' @param col Fill color. Default \code{"steelblue"}.
#' @param border Border color. Default \code{"white"}.
#' @param main Title. Default \code{"Edge Weight Distribution"}.
#' @param xlab X-axis label. Default \code{"Weight"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{barplot}}.
#'
#' @return Invisibly returns the weight vector.
#' @export
#' @examples
#' adj <- matrix(c(0, 2, 3, 2, 0, 1, 3, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' cograph::plot_edge_weights(adj)
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
#' neighbors. Reveals assortative (positive slope) or disassortative
#' (negative slope) mixing patterns.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna.
#' @param mode Character. For directed networks: \code{"all"}, \code{"in"},
#'   or \code{"out"}. Default \code{"all"}.
#' @param directed Logical or NULL. Default NULL (auto-detect).
#' @param col Point color. Default \code{"steelblue"}.
#' @param main Title. Default \code{"Degree-Degree Correlation"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}.
#'
#' @return Invisibly returns a data frame with columns \code{node},
#'   \code{degree}, \code{avg_neighbor_degree}.
#' @seealso \code{\link{centrality}}, \code{\link{degree_distribution}},
#'   \code{\link{network_summary}}
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::sample_pa(100, m = 3, directed = FALSE)
#'   cograph::plot_degree_correlation(g)
#' }
#' }
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


#' Plot Network Evolution (Small Multiples)
#'
#' Displays a network at different time points side by side. Accepts an edge
#' list data frame with a time column, or a pre-built list of networks.
#' All panels share the same node layout for visual comparison.
#'
#' @param x An edge list data frame with columns \code{from}, \code{to}, and
#'   a time column, OR a list of network objects (matrices, igraph, etc.).
#' @param time Character. Name of the time/group column in \code{x}. Ignored
#'   if \code{x} is a list.
#' @param slices Integer or NULL. Number of equal-width time bins. Default
#'   NULL uses unique values of the time column.
#' @param cumulative Logical. If TRUE, each panel shows all edges up to that
#'   time point (growing network). If FALSE (default), each panel shows only
#'   edges from that period.
#' @param labels Character vector of panel labels. Default NULL (auto from
#'   time values).
#' @param layout Layout specification. Default \code{"spring"}.
#' @param ncol Integer. Grid columns. Default auto.
#' @param node_size Numeric. Default 5.
#' @param seed Integer or NULL. Default 42.
#' @param ... Additional arguments passed to \code{\link{splot}}.
#'
#' @return Invisible list of edge-list data frames (one per panel).
#' @export
#' @examples
#' \dontrun{
#' # Edge list with time column
#' edges$week <- sample(1:4, nrow(edges), replace = TRUE)
#' cograph::plot_network_evolution(edges, time = "week")
#'
#' # Cumulative: edges accumulate over time
#' cograph::plot_network_evolution(edges, time = "week", cumulative = TRUE)
#' }
plot_network_evolution <- function(x,
                                   time = NULL,
                                   slices = NULL,
                                   cumulative = FALSE,
                                   labels = NULL,
                                   layout = "spring",
                                   ncol = NULL,
                                   node_size = 5,
                                   seed = 42,
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

    # Build one edge list per period
    if (cumulative) {
      nets <- lapply(seq_along(periods), function(i) {
        x[time_vals <= periods[i], , drop = FALSE]
      })
    } else {
      nets <- lapply(periods, function(p) {
        x[time_vals == p, , drop = FALSE]
      })
    }
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
  if (is.character(layout)) {
    if (is.data.frame(x)) {
      ecols <- intersect(names(x), c("from", "to", "weight"))
      full_net <- as_cograph(x[, ecols, drop = FALSE])
    } else {
      full_net <- nets[[n_nets]]
    }
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
    shared_layout <- layout
  }

  old_par <- graphics::par(mfrow = c(n_row, ncol), mar = c(1, 1, 2, 1))
  on.exit(graphics::par(old_par), add = TRUE)

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
