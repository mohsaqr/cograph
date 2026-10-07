#' Network-Level Summary Statistics
#'
#' Computes network-level statistics and returns them as a one-row data
#' frame. The basic set covers size, density, connectivity, path lengths,
#' centralization, transitivity, reciprocity and degree assortativity.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param weighted Logical. Use edge weights for strength, shortest-path, and
#'   centrality calculations where the underlying igraph routine accepts them.
#'   Default TRUE.
#' @param mode For directed networks: "all", "in", or "out". Used by the
#'   degree, strength and closeness statistics added by \code{detailed = TRUE}.
#'   Default "all".
#' @param loops Logical. If TRUE (default), keep self-loops. Set FALSE to remove them.
#' @param simplify How to combine multiple edges between the same node pair.
#'   Options: "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
#'   multiple edges.
#' @param detailed Logical. If TRUE, add 11 summary statistics of node-level
#'   centralities to the 16 basic metrics. Default FALSE.
#' @param extended Logical. If TRUE, add 8 structural metrics (girth, radius,
#'   vertex connectivity, clique size, cut vertices, bridges, global and local
#'   efficiency). Default FALSE.
#' @param digits Integer. Round numeric results to this many decimal places.
#'   Default 3. NULL skips rounding.
#' @param ... Additional arguments (currently unused)
#'
#' @return A data frame with one row. The basic measures are always
#' computed:
#' \describe{
#'   \item{node_count}{Number of nodes in the network}
#'   \item{edge_count}{Number of edges in the network}
#'   \item{density}{Edge density (proportion of possible edges)}
#'   \item{component_count}{Number of connected components}
#'   \item{diameter}{Longest shortest path in the network}
#'   \item{mean_distance}{Average shortest path length}
#'   \item{min_cut}{Minimum number of edges whose removal disconnects the
#'     network. Edge weights are not used.}
#'   \item{centralization_degree}{Degree centralization over all ties (0-1)}
#'   \item{centralization_in_degree}{In-degree centralization (directed only)}
#'   \item{centralization_out_degree}{Out-degree centralization (directed only)}
#'   \item{centralization_betweenness}{Betweenness centralization (0-1)}
#'   \item{centralization_closeness}{Closeness centralization (0-1)}
#'   \item{centralization_eigen}{Eigenvector centralization (0-1)}
#'   \item{transitivity}{Global clustering coefficient}
#'   \item{reciprocity}{Proportion of mutual edges (directed only)}
#'   \item{assortativity_degree}{Degree assortativity coefficient}
#' }
#'
#' The extended measures are added when \code{extended = TRUE}:
#' \describe{
#'   \item{girth}{Length of shortest cycle (Inf if acyclic)}
#'   \item{radius}{Minimum eccentricity over all nodes}
#'   \item{vertex_connectivity}{Minimum nodes to remove to disconnect graph}
#'   \item{largest_clique_size}{Size of the largest complete subgraph}
#'   \item{cut_vertex_count}{Number of articulation points (cut vertices)}
#'   \item{bridge_count}{Number of bridge edges}
#'   \item{global_efficiency}{Average inverse shortest path length}
#'   \item{local_efficiency}{Average local efficiency across nodes}
#' }
#'
#' The detailed measures are added when \code{detailed = TRUE}:
#' \describe{
#'   \item{mean_degree, sd_degree, median_degree}{Degree distribution statistics}
#'   \item{mean_strength, sd_strength}{Weighted degree statistics}
#'   \item{mean_betweenness}{Average betweenness centrality}
#'   \item{mean_closeness}{Average closeness centrality}
#'   \item{mean_eigenvector}{Average eigenvector centrality}
#'   \item{mean_pagerank}{Average PageRank}
#'   \item{mean_constraint}{Average Burt's constraint}
#'   \item{mean_local_transitivity}{Average local clustering coefficient}
#' }
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' network_summary(regulation_net)
network_summary <- function(x,
                            directed = NULL,
                            weighted = TRUE,
                            mode = "all",
                            loops = TRUE,
                            simplify = "sum",
                            detailed = FALSE,
                            extended = FALSE,
                            digits = 3,
                            ...) {

  # Validate mode

  mode <- match.arg(mode, c("all", "in", "out"))

  # Convert input to igraph
  g <- to_igraph(x, directed = directed)

  # Handle loops
  if (!loops) {
    g <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
  }

  # Handle multiple edges
  if (!isFALSE(simplify) && !identical(simplify, "none")) {
    simplify <- match.arg(simplify, c("sum", "mean", "max", "min"))
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE,
                          edge.attr.comb = list(weight = simplify, "ignore"))
  }

  is_directed <- igraph::is_directed(g)

  # Get weights for weighted calculations
  weights <- if (weighted && !is.null(igraph::E(g)$weight)) {
    igraph::E(g)$weight
  } else {
    NULL
  }

  # Basic measures (always computed)
  results <- list(
    node_count = igraph::vcount(g),
    edge_count = igraph::ecount(g),
    density = igraph::edge_density(g),
    component_count = igraph::count_components(g),
    diameter = igraph::diameter(g, directed = is_directed, weights = weights),
    mean_distance = igraph::mean_distance(g, directed = is_directed, weights = weights),
    min_cut = tryCatch(
      igraph::min_cut(g, value.only = TRUE),
      error = function(e) NA_real_
    ),
    centralization_degree = igraph::centr_degree(g, mode = "all")$centralization,
    centralization_in_degree = if (is_directed) {
      igraph::centr_degree(g, mode = "in")$centralization
    } else {
      NA_real_
    },
    centralization_out_degree = if (is_directed) {
      igraph::centr_degree(g, mode = "out")$centralization
    } else {
      NA_real_
    },
    centralization_betweenness = igraph::centr_betw(g, directed = is_directed)$centralization,
    centralization_closeness = tryCatch(
      igraph::centr_clo(g, mode = "all")$centralization,
      error = function(e) NA_real_
    ),
    centralization_eigen = tryCatch(
      igraph::centr_eigen(g, directed = is_directed)$centralization,
      error = function(e) NA_real_
    ),
    transitivity = igraph::transitivity(g, type = "global"),
    reciprocity = if (is_directed) {
      igraph::reciprocity(g, mode = "ratio")
    } else {
      NA_real_
    },
    assortativity_degree = igraph::assortativity_degree(g, directed = is_directed)
  )

  # Extended structural measures (only when extended = TRUE)
  if (extended) {
    extended_results <- list(
      girth = network_girth(g),
      radius = network_radius(g, directed = is_directed),
      vertex_connectivity = network_vertex_connectivity(g),
      largest_clique_size = network_clique_size(g),
      cut_vertex_count = network_cut_vertices(g, count_only = TRUE),
      bridge_count = network_bridges(g, count_only = TRUE),
      global_efficiency = network_global_efficiency(g, directed = is_directed),
      local_efficiency = network_local_efficiency(g)
    )
    results <- c(results, extended_results)
  }

  # Detailed measures (only when detailed = TRUE)
  if (detailed) {
    deg <- igraph::degree(g, mode = mode)
    str_vals <- igraph::strength(g, mode = mode, weights = weights)
    betw <- igraph::betweenness(g, directed = is_directed, weights = weights)
    close <- igraph::closeness(g, mode = mode, weights = weights)
    eigen_vec <- tryCatch(
      igraph::eigen_centrality(g, directed = is_directed, weights = weights)$vector,
      error = function(e) rep(NA_real_, igraph::vcount(g))
    )
    pr <- igraph::page_rank(g, directed = is_directed, weights = weights)$vector
    constr <- igraph::constraint(g, weights = weights)
    local_trans <- igraph::transitivity(g, type = "local")

    detailed_results <- list(
      mean_degree = mean(deg, na.rm = TRUE),
      sd_degree = stats::sd(deg, na.rm = TRUE),
      median_degree = stats::median(deg, na.rm = TRUE),
      mean_strength = mean(str_vals, na.rm = TRUE),
      sd_strength = stats::sd(str_vals, na.rm = TRUE),
      mean_betweenness = mean(betw, na.rm = TRUE),
      mean_closeness = mean(close, na.rm = TRUE),
      mean_eigenvector = mean(eigen_vec, na.rm = TRUE),
      mean_pagerank = mean(pr, na.rm = TRUE),
      mean_constraint = mean(constr, na.rm = TRUE),
      mean_local_transitivity = mean(local_trans, na.rm = TRUE)
    )

    results <- c(results, detailed_results)
  }

  # Convert to data frame
  df <- as.data.frame(results, stringsAsFactors = FALSE)


  # Round numeric columns
  if (!is.null(digits)) {
    num_cols <- vapply(df, is.numeric, logical(1))
    df[num_cols] <- lapply(df[num_cols], round, digits = digits)
  }

  df
}


#' Degree Distribution Visualization
#'
#' Plots a histogram or a complementary cumulative distribution of node
#' degrees. When the degree range is at most 50, the default bins are
#' integer-aligned, with one bar per degree value.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param mode For directed networks: "all", "in", or "out". Default "all".
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param loops Logical. If TRUE (default), keep self-loops. Set FALSE to
#'   remove them.
#' @param simplify How to combine multiple edges between the same node pair.
#'   Options: "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
#'   multiple edges.
#' @param cumulative Logical. If TRUE, show CCDF (complementary cumulative
#'   distribution: P(degree >= k)) instead of frequency. Default FALSE.
#' @param breaks Bin specification passed to \code{\link[graphics]{hist}}. Can
#'   be a numeric vector of breakpoints, a single number giving the number of
#'   bins, or a character string naming an algorithm (e.g. "Sturges", "FD",
#'   "scott"). Overrides \code{bins} and \code{bin_width}. Default NULL
#'   (auto-detect).
#' @param bins Integer. Number of equal-width bins spanning the degree range.
#'   Overrides \code{bin_width}. Default NULL.
#' @param bin_width Numeric. Width of each bin. Default NULL, which uses a
#'   width of 1 when the degree range is at most 50 and the Freedman-Diaconis
#'   rule otherwise.
#' @param normalize Logical. If TRUE, the y-axis shows proportions (bars sum
#'   to 1) instead of counts. Default FALSE.
#' @param log Character. Axis log-scaling: "" (none, default), "x", "y", or
#'   "xy". Histogram plots apply y-axis log scaling for "y" or "xy";
#'   cumulative plots support x, y, and xy scaling, and "xy" gives a log-log
#'   CCDF.
#' @param main Character. Plot title. Default "Degree Distribution".
#' @param xlab Character. X-axis label. Default "Degree".
#' @param ylab Character. Y-axis label. Default auto-chosen based on
#'   \code{normalize} and \code{cumulative}.
#' @param col Character. Bar fill or line color. Default "steelblue".
#' @param border Character. Bar border color. Default "white".
#' @param ... Additional graphical arguments passed to
#'   \code{\link[graphics]{barplot}} (histogram) or
#'   \code{\link[graphics]{plot}} (cumulative).
#'
#' @return Invisibly returns a list with components:
#'   \describe{
#'     \item{degree}{Named numeric vector of per-node degrees.}
#'     \item{table}{Table of degree frequencies.}
#'     \item{breaks}{Breakpoints of the degree histogram.}
#'     \item{counts}{Bin counts.}
#'     \item{proportions}{Bin proportions (\code{counts / sum(counts)}).}
#'   }
#'   The same five components are returned for the histogram and the
#'   cumulative plot. The bin components always describe the histogram bins.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' cograph::degree_distribution(regulation_net)
degree_distribution <- function(x,
                                mode = "all",
                                directed = NULL,
                                loops = TRUE,
                                simplify = "sum",
                                cumulative = FALSE,
                                breaks = NULL,
                                bins = NULL,
                                bin_width = NULL,
                                normalize = FALSE,
                                log = "",
                                main = "Degree Distribution",
                                xlab = "Degree",
                                ylab = NULL,
                                col = "steelblue",
                                border = "white",
                                ...) {

  # Validate mode
  mode <- match.arg(mode, c("all", "in", "out"))
  stopifnot(is.character(log), length(log) == 1L, log %in% c("", "x", "y", "xy"))

  # Convert input to igraph
  g <- to_igraph(x, directed = directed)

  # Handle loops
  if (!loops) {
    g <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
  }

  # Handle multiple edges
  if (!isFALSE(simplify) && !identical(simplify, "none")) {
    simplify <- match.arg(simplify, c("sum", "mean", "max", "min"))
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE,
                          edge.attr.comb = list(weight = simplify, "ignore"))
  }

  # Get degree values
  deg <- igraph::degree(g, mode = mode)
  deg_range <- range(deg)

  # Compute breaks
  computed_breaks <- .compute_degree_breaks(deg, deg_range, breaks, bins,
                                            bin_width)

  # Default y-axis label
  if (is.null(ylab)) {
    ylab <- if (cumulative) {
      "P(Degree \u2265 k)"
    } else if (normalize) {
      "Proportion"
    } else {
      "Frequency"
    }
  }

  if (cumulative) {
    .plot_cumulative_degree(deg, log, main, xlab, ylab, col, ...)
  } else {
    .plot_histogram_degree(deg, computed_breaks, normalize, log, main, xlab,
                           ylab, col, border, ...)
  }

  # Return value
  deg_table <- table(deg)
  h <- graphics::hist(deg, breaks = computed_breaks, plot = FALSE)

  invisible(list(
    degree = deg,
    table = deg_table,
    breaks = h$breaks,
    counts = h$counts,
    proportions = h$counts / sum(h$counts)
  ))
}

#' Compute histogram breaks for degree data
#' @noRd
.compute_degree_breaks <- function(deg, deg_range, breaks, bins, bin_width) {
  if (!is.null(breaks)) return(breaks)

  if (!is.null(bins)) {
    return(seq(deg_range[1] - 0.5, deg_range[2] + 0.5,
               length.out = bins + 1L))
  }

  if (!is.null(bin_width)) {
    lo <- deg_range[1] - bin_width / 2
    hi <- deg_range[2] + bin_width / 2
    brks <- seq(lo, hi, by = bin_width)
    # Ensure last break covers max value
    if (brks[length(brks)] < deg_range[2]) {
      brks <- c(brks, brks[length(brks)] + bin_width)
    }
    return(brks)
  }

  # Default: integer-aligned when range <= 50, Freedman-Diaconis otherwise
  span <- deg_range[2] - deg_range[1]
  if (span <= 50) {
    seq(deg_range[1] - 0.5, deg_range[2] + 0.5, by = 1)
  } else {
    "FD"
  }
}

#' Plot cumulative degree distribution (CCDF)
#' @noRd
.plot_cumulative_degree <- function(deg, log, main, xlab, ylab, col, ...) {
  deg_tab <- table(deg)
  k_vals <- as.integer(names(deg_tab))
  n <- length(deg)
  # CCDF: P(degree >= k)
  ccdf <- vapply(k_vals, function(k) sum(deg >= k) / n, numeric(1))

  use_log <- if (log == "xy") "xy" else if (log == "x") "x" else
    if (log == "y") "y" else ""

  # Filter zeros for log scale
  keep <- if (nzchar(use_log)) ccdf > 0 else rep(TRUE, length(ccdf))

  graphics::plot(k_vals[keep], ccdf[keep],
                 type = "b",
                 pch = 16,
                 log = use_log,
                 main = main,
                 xlab = xlab,
                 ylab = ylab,
                 col = col,
                 lwd = 2,
                 ...)
  graphics::grid(col = grDevices::adjustcolor("gray50", 0.3), lty = 1)
}

#' Plot histogram for degree distribution
#' @noRd
.plot_histogram_degree <- function(deg, breaks, normalize, log, main, xlab,
                                   ylab, col, border, ...) {
  h <- graphics::hist(deg, breaks = breaks, plot = FALSE)

  heights <- if (normalize) h$counts / sum(h$counts) else h$counts
  bar_names <- .degree_bar_labels(h$breaks)

  use_log <- if (log %in% c("y", "xy")) "y" else ""
  if (nzchar(use_log)) {
    heights[heights == 0] <- NA
  }

  graphics::barplot(heights,
                    names.arg = bar_names,
                    main = main,
                    xlab = xlab,
                    ylab = ylab,
                    col = col,
                    border = border,
                    log = use_log,
                    space = 0,
                    las = 1,
                    ...)
  graphics::grid(nx = NA, ny = NULL,
                 col = grDevices::adjustcolor("gray50", 0.3), lty = 1)
}

#' Create bar labels from histogram breaks
#' @noRd
.degree_bar_labels <- function(brks) {
  vapply(seq_len(length(brks) - 1L), function(i) {
    lo <- ceiling(brks[i])
    hi <- floor(brks[i + 1L])
    if (lo > hi) {
      # Sub-integer bin width: use decimal range
      sprintf("%.1f", (brks[i] + brks[i + 1L]) / 2)
    } else if (lo == hi) {
      as.character(lo)
    } else {
      sprintf("%d-%d", lo, hi)
    }
  }, character(1))
}


# =============================================================================
# Individual Network-Level Metrics
# =============================================================================

#' Network Girth (Shortest Cycle Length)
#'
#' Computes the girth of a network, the length of its shortest cycle. Edge
#' direction, self-loops and repeated undirected edges are ignored. A pair of
#' reciprocal directed edges counts as a cycle of length 2, so a directed
#' network with any mutual tie has girth 2. An undirected forest has girth
#' Inf.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return Numeric scalar: the length of the shortest cycle, or Inf if the
#'   graph has no cycle.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' network_girth(regulation_net)
network_girth <- function(x, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }
  girth_result <- igraph::girth(g)
  girth_result$girth
}


#' Network Radius
#'
#' Computes the radius of a network, the minimum eccentricity across all
#' nodes. The eccentricity of a node is its largest shortest-path distance to
#' any other node. Edge weights are used as distances, and directed networks
#' use outgoing paths.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param directed Logical or NULL. Consider edge direction? Default NULL,
#'   which follows the directedness of the converted graph.
#' @param ... Passed to \code{\link{to_igraph}}, which accepts no arguments
#'   besides \code{directed}; anything else raises an "unused argument" error.
#'
#' @return Numeric scalar: the network radius.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' network_radius(regulation_net)
network_radius <- function(x, directed = NULL, ...) {
  if (inherits(x, "igraph")) {
    g <- x
    if (is.null(directed)) directed <- igraph::is_directed(g)
  } else {
    g <- to_igraph(x, directed = directed, ...)
    if (is.null(directed)) directed <- igraph::is_directed(g)
  }
  mode <- if (directed) "out" else "all"
  igraph::radius(g, mode = mode)
}


#' Network Vertex Connectivity
#'
#' Computes the vertex connectivity of a network, the minimum number of
#' vertices whose removal disconnects the graph or leaves a single vertex.
#' Higher values indicate a more robust structure.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return Numeric scalar: the minimum vertex cut size, or \code{NA} when
#'   igraph cannot compute it.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' network_vertex_connectivity(regulation_net)
network_vertex_connectivity <- function(x, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }
  tryCatch(
    igraph::vertex_connectivity(g),
    error = function(e) NA_integer_
  )
}


#' Largest Clique Size
#'
#' Computes the size of the largest clique (complete subgraph) in the
#' network, also called the clique number or omega of the graph.
#'
#' A clique is defined on undirected ties, so a directed network is read with
#' each pair of nodes joined when either direction is present, and loops and
#' repeated edges are dropped before counting.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return Numeric scalar: the size of the largest clique.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' network_clique_size(regulation_net)
network_clique_size <- function(x, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }
  # igraph 2.3.3's clique_num() overflows the C stack on a directed graph, and
  # cliques ignore direction anyway: count on the simple undirected skeleton.
  g <- igraph::simplify(igraph::as_undirected(g, mode = "collapse"))
  igraph::clique_num(g)
}


#' Cut Vertices (Articulation Points)
#'
#' Finds nodes whose removal would disconnect the network.
#' These are critical nodes for network connectivity.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param count_only Logical. If TRUE, return only the count. Default FALSE.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return If \code{count_only = FALSE}, a character vector of node names, or
#'   an integer vector of node indices when the graph has no names. If
#'   \code{count_only = TRUE}, an integer count.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' strong <- filter_edges(regulation_net, weight > 0.3, keep_isolates = FALSE)
#' network_cut_vertices(strong)
network_cut_vertices <- function(x, count_only = FALSE, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }
  art_points <- igraph::articulation_points(g)
  if (count_only) {
    return(length(art_points))
  }
  if (igraph::is_named(g)) {
    return(igraph::V(g)$name[art_points])
  }
  as.integer(art_points)
}


#' Bridge Edges
#'
#' Finds edges whose removal would disconnect the network.
#' These are critical edges for network connectivity.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param count_only Logical. If TRUE, return only the count. Default FALSE.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return If \code{count_only = FALSE}, a data frame with one row per bridge
#'   and columns \code{from} and \code{to} (node names, or integer indices
#'   when the graph has no names). If \code{count_only = TRUE}, an integer
#'   count.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' strong <- filter_edges(regulation_net, weight > 0.3, keep_isolates = FALSE)
#' network_bridges(strong)
network_bridges <- function(x, count_only = FALSE, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }
  bridge_ids <- igraph::bridges(g)
  if (count_only) {
    return(length(bridge_ids))
  }
  if (length(bridge_ids) == 0) {
    return(data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE))
  }
  edge_list <- igraph::ends(g, bridge_ids)
  if (igraph::is_named(g)) {
    data.frame(
      from = edge_list[, 1],
      to = edge_list[, 2],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      from = as.integer(edge_list[, 1]),
      to = as.integer(edge_list[, 2]),
      stringsAsFactors = FALSE
    )
  }
}


#' Global Efficiency
#'
#' Computes the global efficiency of a network, the average of the inverse
#' shortest path lengths between all ordered pairs of distinct nodes. Higher
#' values indicate more efficient global communication. Unreachable pairs
#' contribute 0. A graph with fewer than two nodes returns \code{NA}.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param directed Logical or NULL. Consider edge direction? Default NULL,
#'   which follows the directedness of the converted graph.
#' @param weights Numeric vector of edge weights. Default NULL uses the
#'   graph's \code{weight} attribute when present. NA ignores weights, so
#'   every edge has length 1, whatever \code{invert_weights} is.
#' @param invert_weights Logical or NULL. If TRUE, weights are converted to
#'   distances as \eqn{1/w^{\alpha}}{1/w^{alpha}}, so stronger ties give shorter paths. If
#'   FALSE, weights are used as distances. Default NULL uses TRUE for tna
#'   objects and FALSE otherwise.
#' @param alpha Numeric. Exponent for weight inversion. Default 1.
#' @param ... Passed to \code{\link{to_igraph}}, which accepts no arguments
#'   besides \code{directed}.
#'
#' @return Numeric scalar: the global efficiency. For unweighted simple graphs
#'   it lies in \eqn{[0, 1]}. Weighted graphs can exceed 1 when edge distances
#'   are below 1.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' network_global_efficiency(regulation_net)
network_global_efficiency <- function(x, directed = NULL, weights = NULL,
                                      invert_weights = NULL, alpha = 1, ...) {
  # Auto-detect invert_weights for tna objects
  is_tna_input <- inherits(x, c("tna", "group_tna", "ctna", "ftna", "atna",
                                 "group_ctna", "group_ftna", "group_atna"))
  if (is.null(invert_weights)) {
    invert_weights <- is_tna_input
  }

  if (inherits(x, "igraph")) {
    g <- x
    if (is.null(directed)) directed <- igraph::is_directed(g)
  } else {
    g <- to_igraph(x, directed = directed, ...)
    if (is.null(directed)) directed <- igraph::is_directed(g)
  }

  n <- igraph::vcount(g)
  if (n <= 1) return(NA_real_)

  # Get weights
  if (is.null(weights) && !is.null(igraph::E(g)$weight)) {
    weights <- igraph::E(g)$weight
  }

  # A single NA means "unweighted": hand it to igraph untouched
  ignore_weights <- length(weights) == 1L && is.na(weights)

  # Invert weights for path calculation (higher weight = shorter path)
  if (!is.null(weights) && !ignore_weights && invert_weights) {
    weights <- 1 / (weights ^ alpha)
    weights[!is.finite(weights)] <- .Machine$double.xmax
  }

  # Compute all-pairs shortest paths
  sp <- igraph::distances(g, mode = if (directed) "out" else "all", weights = weights)
  diag(sp) <- NA  # Exclude self-distances

  # Inverse distances (Inf becomes 0)
  inv_sp <- 1 / sp
  inv_sp[is.infinite(sp)] <- 0

  # Average (excluding diagonal)
  sum(inv_sp, na.rm = TRUE) / (n * (n - 1))
}


#' Local Efficiency
#'
#' Computes the average local efficiency across all nodes with
#' \code{igraph::average_local_efficiency()}. For each node, igraph removes
#' the node and measures the distances between its neighbors through the rest
#' of the network. The value can therefore exceed the Latora and Marchiori
#' (2001) form, which restricts those distances to the subgraph induced on the
#' neighbors. \code{centrality(x, measures = "local_efficiency")} reports the
#' induced-subgraph form. The two agree when the neighbors have no path
#' outside their induced subgraph.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param weights Numeric vector of edge weights. Default NULL uses the
#'   graph's \code{weight} attribute when present. NA ignores weights, so
#'   every edge has length 1, whatever \code{invert_weights} is.
#' @param invert_weights Logical or NULL. If TRUE, weights are converted to
#'   distances as \eqn{1/w^{\alpha}}{1/w^{alpha}}, so stronger ties give shorter paths. If
#'   FALSE, weights are used as distances. Default NULL uses TRUE for tna
#'   objects and FALSE otherwise.
#' @param alpha Numeric. Exponent for weight inversion. Default 1.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return Numeric scalar: the average local efficiency, or \code{NA} for a
#'   graph with fewer than two nodes. For unweighted simple graphs it lies in
#'   \eqn{[0, 1]}. Weighted graphs can exceed 1 when edge distances are below 1.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' network_local_efficiency(regulation_net)
network_local_efficiency <- function(x, weights = NULL, invert_weights = NULL, alpha = 1, ...) {
  # Auto-detect invert_weights for tna objects
  is_tna_input <- inherits(x, c("tna", "group_tna", "ctna", "ftna", "atna",
                                 "group_ctna", "group_ftna", "group_atna"))
  if (is.null(invert_weights)) {
    invert_weights <- is_tna_input
  }

  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }

  n <- igraph::vcount(g)
  if (n <= 1) return(NA_real_)

  # Get weights
  if (is.null(weights) && !is.null(igraph::E(g)$weight)) {
    weights <- igraph::E(g)$weight
  }

  # A single NA means "unweighted": hand it to igraph untouched
  ignore_weights <- length(weights) == 1L && is.na(weights)

  # Invert weights on the graph for path calculation
  if (!is.null(weights) && !ignore_weights && invert_weights) {
    inv_weights <- 1 / (weights ^ alpha)
    inv_weights[!is.finite(inv_weights)] <- .Machine$double.xmax
    igraph::E(g)$weight <- inv_weights
    weights <- inv_weights
  }

  # Use igraph's Latora-Marchiori (2001) implementation directly
  igraph::average_local_efficiency(g, weights = weights,
                                    directed = igraph::is_directed(g),
                                    mode = "all")
}


#' Small-World Coefficient (Sigma)
#'
#' Computes the small-world coefficient
#' \deqn{\sigma = \frac{C / C_{rand}}{L / L_{rand}}}{sigma = (C / C_{rand})/(L / L_{rand})}
#' where \eqn{C} is the global clustering coefficient, \eqn{L} is the mean
#' shortest path length, and \eqn{C_{rand}} and \eqn{L_{rand}} are their means
#' over Erdos-Renyi graphs with the same numbers of nodes and edges. A
#' directed network is collapsed to undirected first.
#'
#' Values above 1 indicate small-world structure.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param n_random Number of Erdos-Renyi comparison graphs (same \code{n} and
#'   \code{m} as the observed graph). Default 10.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return Numeric: small-world coefficient sigma. \code{NA} when the graph has
#'   fewer than 4 nodes, no edges, or an undefined/zero mean path length.
#'
#' @section Reproducibility:
#' The comparison graphs are drawn from the caller's RNG stream; this function
#' takes no \code{seed} argument and does not save or restore
#' \code{.Random.seed}. Call \code{set.seed()} beforehand for a reproducible
#' result. A larger \code{n_random} gives a more stable estimate.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' set.seed(1)
#' network_small_world(regulation_net, n_random = 5)
network_small_world <- function(x, n_random = 10, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }

  # Make undirected
  if (igraph::is_directed(g)) {
    g <- igraph::as_undirected(g, mode = "collapse")
  }

  n <- igraph::vcount(g)
  m <- igraph::ecount(g)

  if (n < 4 || m < 1) return(NA_real_)

  # Observed values
  C_obs <- igraph::transitivity(g, type = "global")
  L_obs <- igraph::mean_distance(g, directed = FALSE)

  if (is.na(C_obs) || is.na(L_obs) || is.nan(C_obs) || is.nan(L_obs)) {
    return(NA_real_)
  }
  if (L_obs == 0 || is.infinite(L_obs)) {
    return(NA_real_)
  }
  # C_obs == 0 is valid (no triangles → sigma = 0, definitively not small-world)

  # Generate random graphs and compute averages
  C_rand_vals <- numeric(n_random)
  L_rand_vals <- numeric(n_random)

  for (i in seq_len(n_random)) {
    # Erdos-Renyi random graph with same n and m
    g_rand <- igraph::sample_gnm(n, m)
    C_rand_vals[i] <- igraph::transitivity(g_rand, type = "global")
    L_rand_vals[i] <- igraph::mean_distance(g_rand, directed = FALSE)
  }

  C_rand <- mean(C_rand_vals, na.rm = TRUE)
  L_rand <- mean(L_rand_vals, na.rm = TRUE)

  if (is.na(C_rand) || C_rand == 0 || is.na(L_rand) || L_rand == 0) { # nocov start
    return(NA_real_)
  } # nocov end

  # Small-world coefficient
  sigma <- (C_obs / C_rand) / (L_obs / L_rand)
  sigma
}


#' Rich Club Coefficient
#'
#' Computes the rich club coefficient for a degree threshold \code{k}, the
#' density of ties among nodes with degree above \code{k}. It measures the
#' tendency of high-degree nodes to connect to each other. Degrees are taken
#' on the undirected simple skeleton of the network.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param k Degree threshold. Only nodes with degree greater than \code{k}
#'   are included. Default NULL uses the median degree.
#' @param normalized Logical. If TRUE, divide by the mean coefficient of
#'   random graphs with the same degree sequence. Default FALSE.
#' @param n_random Number of random graphs for normalization. Default 10.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return Numeric scalar: the rich club coefficient. A normalized value above
#'   1 indicates a rich club effect. \code{NA} when fewer than two nodes
#'   exceed \code{k}.
#'
#' @section Reproducibility:
#' When \code{normalized = TRUE} the null graphs are drawn from the caller's
#' RNG stream; this function takes no \code{seed} argument and does not save or
#' restore \code{.Random.seed}. Call \code{set.seed()} beforehand for a
#' reproducible result. \code{\link{rich_club}()} offers a \code{seed}
#' argument, confidence intervals, and the full rich club curve.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' network_rich_club(regulation_net, k = 5)
network_rich_club <- function(x, k = NULL, normalized = FALSE, n_random = 10, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }

  # Make undirected
  if (igraph::is_directed(g)) {
    g <- igraph::as_undirected(g, mode = "collapse")
  }

  # Remove loops and multiple edges
  g <- igraph::simplify(g)

  deg <- igraph::degree(g)

  # Default k to median degree
  if (is.null(k)) {
    k <- stats::median(deg)
  }

  # Nodes with degree > k
  rich_nodes <- which(deg > k)
  n_rich <- length(rich_nodes)

  if (n_rich < 2) return(NA_real_)

  # Induce subgraph on rich nodes
  subg <- igraph::induced_subgraph(g, rich_nodes)
  e_rich <- igraph::ecount(subg)

  # Maximum possible edges
  max_edges <- n_rich * (n_rich - 1) / 2

  # Rich club coefficient
  phi_k <- e_rich / max_edges

  if (!normalized) {
    return(phi_k)
  }

  # Normalized: compare to random graphs with same degree sequence
  phi_rand_vals <- numeric(n_random)

  for (i in seq_len(n_random)) {
    g_rand <- tryCatch({
      igraph::sample_degseq(deg, method = "fast.heur.simple")
    }, error = function(e) {
      # Fall back to Erdos-Renyi if degree sequence fails
      igraph::sample_gnm(igraph::vcount(g), igraph::ecount(g)) # nocov
    })

    deg_rand <- igraph::degree(g_rand)
    rich_rand <- which(deg_rand > k)
    n_rich_rand <- length(rich_rand)

    if (n_rich_rand < 2) { # nocov start
      phi_rand_vals[i] <- NA
      next
    } # nocov end

    subg_rand <- igraph::induced_subgraph(g_rand, rich_rand)
    e_rand <- igraph::ecount(subg_rand)
    max_rand <- n_rich_rand * (n_rich_rand - 1) / 2
    phi_rand_vals[i] <- e_rand / max_rand
  }

  phi_rand <- mean(phi_rand_vals, na.rm = TRUE)

  if (is.na(phi_rand) || phi_rand == 0) { # nocov start
    return(NA_real_)
  } # nocov end

  phi_k / phi_rand
}


# ---------------------------------------------------------------------------
# Graph-level spectral summaries (Batch 6 — new-API measures)
# ---------------------------------------------------------------------------

#' Estrada Index
#'
#' Computes the Estrada index, a graph-level spectral invariant
#' \deqn{EE(G) = \sum_{i=1}^{n} e^{\lambda_i}}{EE(G) = sum_{i=1}^{n} e^{lambda_i}}
#' where \eqn{\lambda_i}{lambda_i} are the eigenvalues of the binary adjacency matrix.
#' Edge weights are ignored. For an undirected network the index equals
#' \eqn{\sum_k M_k / k!}{sum_k M_k / k!}, where \eqn{M_k} is the number of closed walks of
#' length \eqn{k}, and it is the sum of the subgraph centralities of all
#' nodes. For a directed network the eigenvalues may be complex. They come in
#' conjugate pairs, so the sum of their exponentials is real and again equals
#' the trace of \eqn{e^A}{exp(A)}, the weighted count of closed walks.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#'
#' @return Numeric scalar: the Estrada index of the graph, or 0 for a graph
#'   with no nodes.
#'
#' @seealso \code{\link{centrality_subgraph}} for the per-node measure. On an
#'   undirected network its values sum to \code{estrada_index(x)}.
#' @references
#' Estrada, E. (2000). Characterization of 3D molecular structure.
#' \emph{Chemical Physics Letters}, 319(5-6), 713-718.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' estrada_index(regulation_net)
estrada_index <- function(x) {
  cg <- .cg_graph(x)
  if (cg$n == 0L) return(0)
  A <- unname(cg$b)
  ev <- eigen(A, only.values = TRUE, symmetric = isSymmetric(A))$values
  Re(sum(exp(ev)))
}


#' Trophic Incoherence Parameter
#'
#' Computes the trophic incoherence parameter \eqn{q}, which measures how
#' vertically ordered a directed network is (Johnson et al. 2014). For each edge
#' \eqn{(u, v)}, the trophic difference is \eqn{x_{uv} = s_v - s_u} where
#' \eqn{s_i} is the trophic level of node \eqn{i}. The trophic incoherence
#' parameter is the (population) standard deviation of these differences:
#' \deqn{q = \sqrt{\frac{1}{|E|} \sum_{(u,v) \in E} (x_{uv} - \bar{x})^2}}{q = sqrt(1/(|E|) sum_{(u,v) in E} (x_{uv} - bar{x})^2)}
#'
#' Values near 0 indicate a coherent network, in which every edge rises by
#' about one level. High values indicate many level-skipping or downward
#' edges. Johnson et al. (2014) reported that food webs with low \eqn{q} are
#' more stable.
#'
#' Trophic levels are computed on the binary adjacency matrix, so edge
#' weights are ignored. The levels are defined only for a directed network in
#' which every node can be reached from a basal node (a node with no incoming
#' edges).
#'
#' @param x Directed network input.
#' @param cannibalism Logical. If \code{FALSE}, self-loops are removed before
#'   computing trophic differences. Default \code{TRUE}.
#'
#' @return Numeric scalar: the trophic incoherence parameter. \code{NA} when
#'   the network has no edges or when some node cannot be reached from a basal
#'   node. An undirected network returns \code{NA} with a warning.
#'
#' @seealso \code{\link{centrality}} (the \code{trophic_level} measure) for
#'   the per-node levels used in the incoherence calculation.
#' @references
#' Johnson, S., Dominguez-Garcia, V., Donetti, L., & Munoz, M. A. (2014).
#' Trophic coherence determines food-web stability. \emph{PNAS}, 111(50),
#' 17923-17928.
#'
#' @export
#' @examples
#' strong <- filter_edges(regulation_net, weight > 0.3, keep_isolates = FALSE)
#' trophic_incoherence(strong)
trophic_incoherence <- function(x, cannibalism = TRUE) {
  cg <- .cg_graph(x, loops = isTRUE(cannibalism))
  if (!cg$directed) {
    warning("trophic_incoherence requires a directed graph; returning NA",
            call. = FALSE)
    return(NA_real_)
  }
  if (nrow(cg$edges) == 0L) return(NA_real_)

  # Trophic level s_j = 1 + (1/k_j^in) * sum_{i->j} s_i, solved as
  # (I - W^T) s = 1 with W_ji = A_ij / k_j^in. Self-loops stay in A when
  # cannibalism = TRUE, so the diagonal is read rather than dropped.
  levels <- .trophic_levels_from_adjacency(unname(cg$b))
  if (all(is.na(levels))) return(NA_real_)

  diffs <- levels[cg$edges[, 2L]] - levels[cg$edges[, 1L]]

  # NetworkX uses numpy.std with default ddof=0 (population std); R's sd()
  # uses ddof=1 (sample std) and would diverge.
  sqrt(mean((diffs - mean(diffs))^2))
}

#' Trophic levels of a binary adjacency, NA when the system is singular
#'
#' A directed graph without a basal node (every vertex has an in-edge) has a
#' singular level system; that is the one condition turned into `NA`, any
#' other solver failure is propagated.
#' @keywords internal
#' @noRd
.trophic_levels_from_adjacency <- function(A) {
  n <- nrow(A)
  in_deg <- colSums(A)
  in_deg[in_deg == 0] <- 1
  W <- t(t(A) / in_deg)
  tryCatch(
    solve(diag(n) - t(W), rep(1, n)),
    error = function(e) {
      if (grepl("singular", conditionMessage(e), fixed = TRUE)) {
        return(rep(NA_real_, n))
      }
      stop(e)
    }
  )
}


# ---------------------------------------------------------------------------
# Group centrality family (Everett & Borgatti 1999)
# ---------------------------------------------------------------------------

#' Group Centrality (Everett-Borgatti 1999)
#'
#' Computes the centrality of a set of nodes \eqn{C \subseteq V}{C subseteq V}. Distances
#' are unweighted hop counts in the direction of the edges. The measures are
#' defined as follows.
#'
#' \describe{
#'   \item{betweenness}{\eqn{GBC(C) = \sum_{s,t \in V \setminus C, s \ne t}
#'     \sigma(s, t \mid C) / \sigma(s, t)}{GBC(C) = sum_{s,t in V setminus C, s != t} sigma(s, t mid C) / sigma(s, t)}, where \eqn{\sigma(s, t)}{sigma(s, t)} is the
#'     number of shortest \eqn{s}-\eqn{t} paths and \eqn{\sigma(s, t \mid C)}{sigma(s, t mid C)}
#'     is the number of those paths passing through at least one node in
#'     \eqn{C}. Normalized by \eqn{1 / ((|V| - |C|)(|V| - |C| - 1))}.}
#'   \item{closeness}{\eqn{GCC(C) = (|V| - |C|) / \sum_{v \in V \setminus C}
#'     d(v, C)}{GCC(C) = (|V| - |C|) / sum_{v in V setminus C} d(v, C)}, where \eqn{d(v, C) = \min_{c \in C} d(v, c)}{d(v, C) = min_{c in C} d(v, c)} is the shortest
#'     distance from \eqn{v} to any group member. Unreachable nodes
#'     contribute 0 to the denominator sum. For directed graphs,
#'     \eqn{d(v, c)} follows the edges from \eqn{v} to \eqn{c}.}
#'   \item{degree}{\eqn{GDC(C) = |N(C) \setminus C| / (|V| - |C|)}{GDC(C) = |N(C) setminus C| / (|V| - |C|)}, the
#'     fraction of non-group nodes adjacent to at least one group member.
#'     For directed graphs, \code{mode} selects the neighborhood.}
#' }
#'
#' @section Group betweenness:
#' Group betweenness is computed directly from the Everett and Borgatti
#' definition, counting the shortest paths that pass through at least one
#' node in \eqn{C}. On some graphs the result differs from
#' \code{networkx.group_betweenness_centrality}, which uses the iterative
#' algorithm of Puzis, Elovici and Dolev.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param nodes Integer vector of node indices (1-based) or character vector
#'   of node names identifying the group \eqn{C}.
#' @param measure One of \code{"betweenness"} (default), \code{"closeness"},
#'   or \code{"degree"}.
#' @param mode For directed graphs with \code{measure = "degree"}: \code{"all"}
#'   (both directions, default), \code{"out"} (outgoing), or \code{"in"}
#'   (incoming). Ignored for undirected graphs and other measures.
#' @param normalized Logical, for \code{"betweenness"} only. If \code{TRUE}
#'   (default), divide by \eqn{(|V| - |C|)(|V| - |C| - 1)}.
#'
#' @return Numeric scalar: the group centrality of the set \code{nodes}.
#'   Unknown node names and out-of-range indices raise an error.
#'
#' @seealso \code{\link{centrality}} for per-node measures.
#' @references
#' Everett, M. G., & Borgatti, S. P. (1999). The centrality of groups and
#' classes. \emph{Journal of Mathematical Sociology}, 23(3), 181-201.
#'
#' Puzis, R., Elovici, Y., & Dolev, S. (2007). Fast algorithm for successive
#'   computation of group betweenness centrality. \emph{Physical Review E}, 76,
#'   056709. \doi{10.1103/PhysRevE.76.056709}.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' group_centrality(regulation_net, nodes = c("Plan", "Monitor"), measure = "betweenness")
group_centrality <- function(x, nodes,
                             measure = c("betweenness", "closeness", "degree"),
                             mode = c("all", "out", "in"),
                             normalized = TRUE) {
  measure <- match.arg(measure)
  mode <- match.arg(mode)

  cg <- .cg_graph(x)
  n <- cg$n

  # Resolve node names to integer indices
  if (is.character(nodes)) {
    if (!cg$has_names) {
      stop("group_centrality: node names not available on graph", call. = FALSE)
    }
    C <- match(nodes, cg$labels)
    if (anyNA(C)) {
      stop("group_centrality: unknown nodes: ",
           paste(nodes[is.na(C)], collapse = ", "), call. = FALSE)
    }
  } else {
    C <- as.integer(nodes)
  }
  if (any(C < 1L | C > n)) {
    stop("group_centrality: node indices out of range [1, ", n, "]",
         call. = FALSE)
  }
  C <- unique(C)

  switch(measure,
    "betweenness" = .group_betweenness(cg, C, normalized = normalized),
    "closeness"   = .group_closeness(cg, C),
    "degree"      = .group_degree(cg, C, mode = mode)
  )
}


#' Group betweenness: textbook Everett-Borgatti formula
#' @keywords internal
#' @noRd
.group_betweenness <- function(g, C, normalized = TRUE) {
  cg <- .cg_as_context(g)
  n <- cg$n
  V_minus_C <- setdiff(seq_len(n), C)
  if (length(V_minus_C) < 2L) return(0)

  # Unweighted geodesics in the graph's own direction. A geodesic passes
  # through C exactly when it is not a geodesic of the graph with C removed,
  # so the through-fraction is 1 - sigma_{G - C}(s, t) / sigma_G(s, t),
  # counting only paths that are still shortest in G (same distance).
  a <- unname(cg$b)
  diag(a) <- 0
  d <- .cg_distances(a, "out")
  sigma <- .cg_geodesic_counts(a, d)
  a_c <- a
  a_c[C, ] <- 0
  a_c[, C] <- 0
  sigma_c <- .cg_geodesic_counts(a_c, d)

  D <- d[V_minus_C, V_minus_C, drop = FALSE]
  reachable <- is.finite(D) & D > 0
  S <- sigma[V_minus_C, V_minus_C, drop = FALSE][reachable]
  S_c <- sigma_c[V_minus_C, V_minus_C, drop = FALSE][reachable]
  total <- sum(1 - S_c / S)

  if (normalized) {
    k <- length(V_minus_C)
    total <- total / (k * (k - 1L))
  }
  total
}


#' Group closeness: |V - C| / sum of min-distance-to-C over V - C
#' @keywords internal
#' @noRd
.group_closeness <- function(g, C) {
  cg <- .cg_as_context(g)
  n <- cg$n
  V_minus_C <- setdiff(seq_len(n), C)
  if (length(V_minus_C) == 0L) return(0)

  # Hop distance from each v in V - C to its closest group member.
  D <- .cg_hop_distances(cg, "out")[V_minus_C, C, drop = FALSE]
  d_vec <- apply(D, 1L, min)
  closeness_sum <- sum(d_vec[is.finite(d_vec)])
  if (closeness_sum == 0) return(0)
  length(V_minus_C) / closeness_sum
}


#' Group degree: |N(C) - C| / (N - |C|)
#' @keywords internal
#' @noRd
.group_degree <- function(g, C, mode = "all") {
  cg <- .cg_as_context(g)
  n <- cg$n
  if (!cg$directed) mode <- "all"

  nbrs <- unique(unlist(.cg_neighbors(cg$b, cg$directed, mode)[C]))
  nbrs_outside <- setdiff(nbrs, C)
  k <- n - length(C)
  if (k == 0L) return(0)
  length(nbrs_outside) / k
}


# ---------------------------------------------------------------------------
# Dispersion (Backstrom-Kleinberg 2014)
# ---------------------------------------------------------------------------

#' Dispersion (Backstrom-Kleinberg 2014)
#'
#' Computes the dispersion of a tie (Backstrom and Kleinberg 2014), a
#' per-pair measure of tie strength. Edge weights are ignored, and directed
#' networks use out-neighbors. For each pair \eqn{(u, v)} where \eqn{v} is a
#' neighbor of \eqn{u}, the computation proceeds as follows.
#'
#' \enumerate{
#'   \item Let \eqn{S_T = N(u) \cap N(v)}{S_T = N(u) cap N(v)} be their mutual friends (embeddedness).
#'   \item Count pairs \eqn{(s, t) \subset S_T}{(s, t) subset S_T} such that:
#'     \itemize{
#'       \item \eqn{s} and \eqn{t} are not directly connected, and
#'       \item \eqn{s} and \eqn{t} share no common neighbor inside \eqn{N(u)}
#'         other than \eqn{u} and \eqn{v}.
#'     }
#'   \item The raw dispersion is this count. When \code{normalized = TRUE},
#'     the result is \eqn{(\mathrm{dispersion} + b)^{\alpha} /
#'     (\mathrm{embeddedness} + c)}{(dispersion + b)^{alpha} / (embeddedness + c)} (normalization is skipped when
#'     \code{embeddedness + c == 0}).
#' }
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param u Optional source node (1-based index or node name). If \code{NULL}
#'   (default), compute for all sources.
#' @param v Optional target node. If \code{NULL}, compute for all neighbors
#'   of \code{u}.
#' @param normalized Logical. If \code{TRUE} (default), return the normalized
#'   form; otherwise the raw count.
#' @param alpha Numeric normalization exponent. Default 1.
#' @param b Numeric bias added to dispersion before exponentiation. Default 0.
#' @param c Numeric bias added to embeddedness in the denominator. Default 0.
#'
#' @return
#' \itemize{
#'   \item Scalar if both \code{u} and \code{v} are specified.
#'   \item Named numeric vector if exactly one of \code{u}, \code{v} is given,
#'     one element per neighbor of that node; the names are the neighbors'
#'     1-based node indices as character strings.
#'   \item A data frame with columns \code{from}, \code{to}, \code{dispersion}
#'     when neither \code{u} nor \code{v} is given, one row per ordered
#'     (node, neighbor) pair, with \code{from} and \code{to} given as 1-based
#'     integer node indices.
#'   \item \code{numeric(0)} for an empty graph.
#' }
#'
#' @references
#' Backstrom, L., & Kleinberg, J. (2014). Romantic partnerships and the
#' dispersion of social ties: A network analysis of relationship status on
#' Facebook. In \emph{Proceedings of CSCW} (pp. 831-841). ACM.
#' \url{https://arxiv.org/pdf/1310.6753v1.pdf}
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' dispersion(regulation_net, u = "Plan")
dispersion <- function(x, u = NULL, v = NULL,
                       normalized = TRUE,
                       alpha = 1, b = 0, c = 0) {
  cg <- .cg_graph(x)
  n <- cg$n
  if (n == 0L) return(numeric(0))

  # Resolve node labels to 1-based indices
  resolve_node <- function(node) {
    if (is.null(node)) return(NULL)
    if (is.character(node)) {
      if (!cg$has_names) {
        stop("dispersion: node names not available on graph", call. = FALSE)
      }
      idx <- match(node, cg$labels)
      if (anyNA(idx)) {
        stop("dispersion: unknown node(s): ",
             paste(node[is.na(idx)], collapse = ", "), call. = FALSE)
      }
      return(as.integer(idx))
    }
    as.integer(node)
  }
  u <- resolve_node(u)
  v <- resolve_node(v)

  # Out-neighbor lists (NetworkX reads G[u] as out-neighbors on a directed
  # graph). A self-loop lists the node itself, twice on an undirected graph,
  # which is what `igraph::neighbors()` and NetworkX both report.
  adj <- unname(cg$b) != 0
  loop_twice <- !cg$directed
  nbrs_of <- function(node) {
    j <- which(adj[node, ])
    if (loop_twice && adj[node, node]) j <- sort(c(j, node))
    j
  }

  # Single-pair inner computation
  disp_pair <- function(u_i, v_i) {
    u_nbrs <- nbrs_of(u_i)
    v_nbrs <- nbrs_of(v_i)
    ST <- intersect(v_nbrs, u_nbrs)
    set_uv <- c(u_i, v_i)
    total <- 0L
    if (length(ST) >= 2L) {
      # Every unordered pair {s, t} of mutual friends is "dispersed" when s
      # and t are not adjacent and share no common neighbor inside u's ego
      # network other than u and v.
      pairs <- utils::combn(ST, 2L)
      dispersed <- vapply(seq_len(ncol(pairs)), function(p) {
        s <- pairs[1L, p]
        t <- pairs[2L, p]
        nbrs_s <- setdiff(intersect(u_nbrs, nbrs_of(s)), set_uv)
        !(t %in% nbrs_s) && length(intersect(nbrs_s, nbrs_of(t))) == 0L
      }, logical(1))
      total <- sum(dispersed)
    }
    embeddedness <- length(ST)
    if (normalized) {
      val <- (total + b)^alpha
      if (embeddedness + c != 0) val <- val / (embeddedness + c)
      val
    } else {
      as.numeric(total)
    }
  }

  # Dispatch on u / v modes
  if (!is.null(u) && !is.null(v)) {
    return(disp_pair(u, v))
  }
  if (!is.null(u) && is.null(v)) {
    u_nbrs <- nbrs_of(u)
    out <- vapply(u_nbrs, function(v_i) disp_pair(u, v_i), numeric(1))
    names(out) <- as.character(u_nbrs)
    return(out)
  }
  if (is.null(u) && !is.null(v)) {
    v_nbrs <- nbrs_of(v)
    out <- vapply(v_nbrs, function(u_i) disp_pair(v, u_i), numeric(1))
    names(out) <- as.character(v_nbrs)
    return(out)
  }

  # Both NULL: one row per (u, v) with v a neighbor of u
  nbr_lists <- lapply(seq_len(n), nbrs_of)
  from <- rep(seq_len(n), lengths(nbr_lists))
  to <- as.integer(unlist(nbr_lists))
  if (length(from) == 0L) {
    return(data.frame(from = integer(0), to = integer(0),
                      dispersion = numeric(0)))
  }
  data.frame(
    from = from, to = to,
    dispersion = mapply(disp_pair, from, to),
    stringsAsFactors = FALSE
  )
}
