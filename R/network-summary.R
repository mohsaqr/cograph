#' Network-Level Summary Statistics
#'
#' Computes comprehensive network-level statistics for a network.
#' Returns a data frame with one row containing various metrics
#' including density, centralization scores, transitivity, and more.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param weighted Logical. Use edge weights for strength, shortest-path, and
#'   centrality calculations where the underlying igraph routine accepts them.
#'   Default TRUE.
#' @param mode For directed networks: "all", "in", or "out". Affects degree-based
#'   calculations. Default "all".
#' @param loops Logical. If TRUE (default), keep self-loops. Set FALSE to remove them.
#' @param simplify How to combine multiple edges between the same node pair.
#'   Options: "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
#'   multiple edges.
#' @param detailed Logical. If TRUE, include mean/sd centrality statistics.
#'   Default FALSE returns 18 basic metrics; TRUE returns 29 metrics.
#' @param extended Logical. If TRUE, include additional structural metrics
#'   (girth, radius, clique size, cut vertices, bridges, efficiency).
#'   Default FALSE.
#' @param digits Integer. Round numeric results to this many decimal places.
#'   Default 3.
#' @param ... Additional arguments (currently unused)
#'
#' @return A data frame with one row containing network-level statistics:
#'
#' **Basic measures (always computed):**
#' \describe{
#'   \item{node_count}{Number of nodes in the network}
#'   \item{edge_count}{Number of edges in the network}
#'   \item{density}{Edge density (proportion of possible edges)}
#'   \item{component_count}{Number of connected components}
#'   \item{diameter}{Longest shortest path in the network}
#'   \item{mean_distance}{Average shortest path length}
#'   \item{min_cut}{Minimum cut value (edge connectivity)}
#'   \item{centralization_degree}{Degree centralization (0-1)}
#'   \item{centralization_in_degree}{In-degree centralization (directed only)}
#'   \item{centralization_out_degree}{Out-degree centralization (directed only)}
#'   \item{centralization_betweenness}{Betweenness centralization (0-1)}
#'   \item{centralization_closeness}{Closeness centralization (0-1)}
#'   \item{centralization_eigen}{Eigenvector centralization (0-1)}
#'   \item{transitivity}{Global clustering coefficient}
#'   \item{reciprocity}{Proportion of mutual edges (directed only)}
#'   \item{assortativity_degree}{Degree assortativity coefficient}
#'   \item{hub_score}{Maximum hub score (HITS algorithm)}
#'   \item{authority_score}{Maximum authority score (HITS algorithm)}
#' }
#'
#' **Extended measures (when extended = TRUE):**
#' \describe{
#'   \item{girth}{Length of shortest cycle (Inf if acyclic)}
#'   \item{radius}{Minimum eccentricity (shortest max-distance from any node)}
#'   \item{vertex_connectivity}{Minimum nodes to remove to disconnect graph}
#'   \item{largest_clique_size}{Size of the largest complete subgraph}
#'   \item{cut_vertex_count}{Number of articulation points (cut vertices)}
#'   \item{bridge_count}{Number of bridge edges}
#'   \item{global_efficiency}{Average inverse shortest path length}
#'   \item{local_efficiency}{Average local efficiency across nodes}
#' }
#'
#' **Detailed measures (when detailed = TRUE):**
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
#' @examples
#' # Basic usage with adjacency matrix
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' network_summary(adj)
#'
#' # With detailed statistics
#' network_summary(adj, detailed = TRUE)
#'
#' # With extended structural metrics
#' network_summary(adj, extended = TRUE)
#'
#' # All metrics
#' network_summary(adj, detailed = TRUE, extended = TRUE)
#'
#' # From igraph object
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::sample_gnp(20, 0.3)
#'   network_summary(g)
#' }
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

  # Compute HITS scores once (hub and authority)
  hits <- tryCatch(
    igraph::hits_scores(g, weights = weights),
    error = function(e) NULL
  )

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
    assortativity_degree = igraph::assortativity_degree(g, directed = is_directed),
    hub_score = if (!is.null(hits) && length(hits$hub_score) > 0) { # nocov start
      max(hits$hub_score)
    } else NA_real_, # nocov end
    authority_score = if (!is.null(hits) && length(hits$authority_score) > 0) { # nocov start
      max(hits$authority_score)
    } else NA_real_ # nocov end
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
#' Creates a histogram or cumulative distribution plot of node degrees. By
#' default, bins are integer-aligned (one bar per degree value) so each bar
#' maps to an exact degree.
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
#' @param bins Integer. Approximate number of bins. Overrides \code{bin_width}.
#'   Default NULL.
#' @param bin_width Numeric. Width of each bin. Default NULL (auto: 1 when the
#'   degree range is \eqn{\le 50}{<= 50}, otherwise Freedman-Diaconis).
#' @param normalize Logical. If TRUE, the y-axis shows proportions (bars sum
#'   to 1) instead of counts. Default FALSE.
#' @param log Character. Axis log-scaling: "" (none, default), "x", "y", or
#'   "xy". Histogram plots apply y-axis log scaling for "y" or "xy";
#'   cumulative plots support x, y, and xy scaling, with "xy" producing a
#'   log-log CCDF (standard for power-law inspection).
#' @param main Character. Plot title. Default "Degree Distribution".
#' @param xlab Character. X-axis label. Default "Degree".
#' @param ylab Character. Y-axis label. Default auto-chosen based on
#'   \code{normalize} and \code{cumulative}.
#' @param col Character. Bar/line fill color. Default "steelblue".
#' @param border Character. Bar border color. Default "white".
#' @param ... Additional graphical arguments passed to
#'   \code{\link[graphics]{barplot}} (histogram) or
#'   \code{\link[graphics]{plot}} (cumulative).
#'
#' @return Invisibly returns a list with components:
#'   \describe{
#'     \item{degree}{Named numeric vector of per-node degrees.}
#'     \item{table}{Table of degree frequencies.}
#'     \item{breaks}{Breakpoints used for the histogram (non-cumulative only).}
#'     \item{counts}{Bin counts (non-cumulative only).}
#'     \item{proportions}{Bin proportions (non-cumulative only).}
#'   }
#'
#' @export
#' @examples
#' # Undirected network
#' adj <- matrix(c(0, 1, 1, 0, 1, 0, 1, 1,
#'                 1, 1, 0, 1, 0, 1, 1, 0), 4, 4, byrow = TRUE)
#' cograph::degree_distribution(adj)
#' cograph::degree_distribution(adj, cumulative = TRUE)
#'
#' # Directed network, in-degree
#' directed_adj <- matrix(c(0, 1, 0, 0, 0, 0, 1, 0,
#'                          1, 0, 0, 1, 0, 1, 0, 0), 4, 4, byrow = TRUE)
#' cograph::degree_distribution(directed_adj, mode = "in")
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
#' Computes the girth of a network - the length of the shortest cycle.
#' Returns Inf for acyclic graphs (trees, DAGs).
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Integer: length of shortest cycle, or Inf if no cycles exist
#'
#' @export
#' @examples
#' # Triangle has girth 3
#' triangle <- matrix(c(0,1,1, 1,0,1, 1,1,0), 3, 3)
#' network_girth(triangle)  # 3
#'
#' # Tree has no cycles (Inf)
#' tree <- matrix(c(0,1,0, 1,0,1, 0,1,0), 3, 3)
#' network_girth(tree)  # Inf
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
#' Computes the radius of a network - the minimum eccentricity across all nodes.
#' The eccentricity of a node is the maximum shortest path distance to any other node.
#' The radius is the smallest such maximum distance.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param directed Logical. Consider edge direction? Default TRUE for directed graphs.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Numeric: the network radius
#'
#' @export
#' @examples
#' # Star graph: center has eccentricity 1, leaves have 2, so radius = 1
#' star <- matrix(c(0,1,1,1, 1,0,0,0, 1,0,0,0, 1,0,0,0), 4, 4)
#' network_radius(star)  # 1
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
#' Computes the vertex connectivity of a network - the minimum number of
#' vertices that must be removed to disconnect the graph (or make it trivial).
#' Higher values indicate more robust network structure.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Integer: minimum vertex cut size
#'
#' @export
#' @examples
#' # Complete graph K4 has vertex connectivity 3
#' k4 <- matrix(1, 4, 4); diag(k4) <- 0
#' network_vertex_connectivity(k4)  # 3
#'
#' # Path graph has vertex connectivity 1
#' path <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
#' network_vertex_connectivity(path)  # 1
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
#' Finds the size of the largest clique (complete subgraph) in the network.
#' Also known as the clique number or omega of the graph.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Integer: size of the largest clique
#'
#' @export
#' @examples
#' # Triangle embedded in larger graph
#' adj <- matrix(c(0,1,1,1, 1,0,1,0, 1,1,0,0, 1,0,0,0), 4, 4)
#' network_clique_size(adj)  # 3
network_clique_size <- function(x, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }
  igraph::clique_num(g)
}


#' Cut Vertices (Articulation Points)
#'
#' Finds nodes whose removal would disconnect the network.
#' These are critical nodes for network connectivity.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param count_only Logical. If TRUE, return only the count. Default FALSE.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return If count_only = FALSE, vector of node indices (or names if graph is named).
#'   If count_only = TRUE, integer count.
#'
#' @export
#' @examples
#' # Bridge node connecting two components
#' adj <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,1,0, 0,0,1,0,1, 0,0,0,1,0), 5, 5)
#' network_cut_vertices(adj)  # Node 3 is cut vertex
#' network_cut_vertices(adj, count_only = TRUE)  # 1
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
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return If count_only = FALSE, data frame with from/to columns.
#'   If count_only = TRUE, integer count.
#'
#' @export
#' @examples
#' # Two triangles connected by single edge
#' adj <- matrix(0, 6, 6)
#' adj[1,2] <- adj[2,1] <- adj[1,3] <- adj[3,1] <- adj[2,3] <- adj[3,2] <- 1
#' adj[4,5] <- adj[5,4] <- adj[4,6] <- adj[6,4] <- adj[5,6] <- adj[6,5] <- 1
#' adj[3,4] <- adj[4,3] <- 1  # Bridge
#' network_bridges(adj)  # Edge 3-4
#' network_bridges(adj, count_only = TRUE)  # 1
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
#' Computes the global efficiency of a network - the average of the inverse
#' shortest path lengths between all pairs of nodes. Higher values indicate
#' better global communication efficiency. Handles disconnected graphs gracefully
#' (infinite distances contribute 0).
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param directed Logical. Consider edge direction? Default TRUE for directed graphs.
#' @param weights Edge weights (NULL for unweighted). Set to NA to ignore existing weights.
#' @param invert_weights Logical or NULL. Invert weights so higher weights = shorter
#'   paths? Default NULL which auto-detects: TRUE for tna objects, FALSE otherwise
#'   (matching igraph/sna). Set TRUE for strength/frequency weights (qgraph style).
#' @param alpha Numeric. Exponent for weight inversion: distance = 1/weight^alpha.
#'   Default 1.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Numeric global efficiency. For unweighted simple graphs this is in
#'   \eqn{[0, 1]}; weighted graphs can exceed 1 when edge distances are below 1.
#'
#' @export
#' @examples
#' # Complete graph has efficiency 1
#' k4 <- matrix(1, 4, 4); diag(k4) <- 0
#' network_global_efficiency(k4)  # 1
#'
#' # Star has lower efficiency
#' star <- matrix(c(0,1,1,1, 1,0,0,0, 1,0,0,0, 1,0,0,0), 4, 4)
#' network_global_efficiency(star)  # ~0.83
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

  # Invert weights for path calculation (higher weight = shorter path)
  if (!is.null(weights) && invert_weights) {
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
#' Computes the average local efficiency across all nodes. Local efficiency
#' of a node is the global efficiency of its neighborhood subgraph
#' (excluding the node itself). Measures fault tolerance and local integration.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param weights Edge weights (NULL for unweighted). Set to NA to ignore existing weights.
#' @param invert_weights Logical or NULL. Invert weights so higher weights = shorter
#'   paths? Default NULL which auto-detects: TRUE for tna objects, FALSE otherwise
#'   (matching igraph/sna). Set TRUE for strength/frequency weights (qgraph style).
#' @param alpha Numeric. Exponent for weight inversion. Default 1.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Numeric average local efficiency. For unweighted simple graphs this
#'   is in \eqn{[0, 1]}; weighted graphs can exceed 1 when edge distances are
#'   below 1.
#'
#' @export
#' @examples
#' # Complete graph: removing any node leaves complete subgraph, so local efficiency = 1
#' k5 <- matrix(1, 5, 5); diag(k5) <- 0
#' network_local_efficiency(k5)  # 1
#'
#' # Star: neighbors not connected to each other
#' star <- matrix(c(0,1,1,1,1, 1,0,0,0,0, 1,0,0,0,0, 1,0,0,0,0, 1,0,0,0,0), 5, 5)
#' network_local_efficiency(star)  # 0
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

  # Invert weights on the graph for path calculation
  if (!is.null(weights) && invert_weights) {
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
#' Computes the small-world coefficient sigma, defined as:
#' sigma = (C / C_rand) / (L / L_rand)
#' where C is clustering coefficient, L is mean path length, and _rand
#' are values from equivalent random graphs.
#'
#' Values > 1 indicate small-world properties. Typically small-world
#' networks have sigma >> 1.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param n_random Number of random graphs for comparison. Default 10.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Numeric: small-world coefficient sigma
#'
#' @export
#' @examples
#' # Watts-Strogatz small-world graph
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::sample_smallworld(1, 20, 3, 0.1)
#'   network_small_world(g)  # Should be > 1
#' }
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
#' Computes the rich club coefficient for a given degree threshold k.
#' Measures the tendency of high-degree nodes to connect to each other.
#' A normalized version compares to random graphs.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param k Degree threshold. Only nodes with degree > k are included.
#'   If NULL, uses median degree.
#' @param normalized Logical. Normalize by random graph expectation? Default FALSE.
#' @param n_random Number of random graphs for normalization. Default 10.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Numeric: rich club coefficient (> 1 indicates rich club effect when normalized)
#'
#' @export
#' @examples
#' # Scale-free networks often show rich-club effect
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::sample_pa(50, m = 2, directed = FALSE)
#'   network_rich_club(g, k = 5)
#' }
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
#' A graph-level spectral invariant derived from subgraph centrality:
#' \deqn{EE(G) = \sum_{i=1}^{n} e^{\lambda_i}}
#' where \eqn{\lambda_i} are the eigenvalues of the adjacency matrix. The
#' Estrada index equals the total number of closed walks in the graph,
#' weighted by walk length: \eqn{EE(G) = \sum_k M_k / k!} where \eqn{M_k} is
#' the number of closed walks of length \eqn{k}. It is the sum of subgraph
#' centralities across all nodes.
#'
#' Matches \code{networkx.estrada_index} at machine epsilon (max relative
#' difference ~5e-15 across random test graphs).
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#'
#' @return A single numeric value — the Estrada index of the graph.
#'
#' @seealso \code{\link{centrality_subgraph}} for the per-node equivalent
#'   (sum of \code{subgraph_centrality(x)} equals \code{estrada_index(x)}).
#' @references
#' Estrada, E. (2000). Characterization of 3D molecular structure.
#' \emph{Chemical Physics Letters}, 319(5-6), 713-718.
#'
#' @export
#' @examples
#' # Karate club
#' g <- igraph::make_graph("Zachary")
#' estrada_index(g)
estrada_index <- function(x) {
  g <- to_igraph(x)
  n <- igraph::vcount(g)
  if (n == 0) return(0)
  A <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  ev <- eigen(A, only.values = TRUE, symmetric = isSymmetric(A))$values
  sum(exp(Re(ev)))
}


#' Trophic Incoherence Parameter
#'
#' The trophic incoherence parameter \eqn{q} is a measure of how "vertically
#' ordered" a directed network is (Johnson et al. 2014). For each edge
#' \eqn{(u, v)}, the trophic difference is \eqn{x_{uv} = s_v - s_u} where
#' \eqn{s_i} is the trophic level of node \eqn{i}. The trophic incoherence
#' parameter is the (population) standard deviation of these differences:
#' \deqn{q = \sqrt{\frac{1}{|E|} \sum_{(u,v) \in E} (x_{uv} - \bar{x})^2}}
#'
#' Low values (\eqn{q \approx 0}) indicate a perfectly coherent network
#' (e.g., a pure food web where every edge goes up one level). High values
#' indicate an incoherent network with many level-skipping or downward
#' edges. Johnson et al. 2014 showed that low-\eqn{q} food webs are
#' dynamically more stable.
#'
#' Matches \code{networkx.trophic_incoherence_parameter} at machine epsilon.
#' Directed-only; requires at least one basal node (node with no incoming
#' edges) for trophic levels to be well-defined.
#'
#' @param x Directed network input.
#' @param cannibalism Logical. If \code{FALSE}, self-loops are removed before
#'   computing trophic differences. Default \code{TRUE}.
#'
#' @return A single numeric value (\code{NA_real_} for empty edge sets or
#'   undirected input).
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
#' # Small directed 3-node chain: 1 -> 2 -> 3 (perfectly coherent, q = 0)
#' adj <- matrix(c(0,1,0, 0,0,1, 0,0,0), 3, 3, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' trophic_incoherence(adj)
trophic_incoherence <- function(x, cannibalism = TRUE) {
  g <- to_igraph(x)
  if (!igraph::is_directed(g)) {
    warning("trophic_incoherence requires a directed graph; returning NA",
            call. = FALSE)
    return(NA_real_)
  }
  if (!isTRUE(cannibalism)) {
    # Remove self-loops (matching NetworkX's convention when cannibalism=FALSE)
    g <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
  }
  if (igraph::ecount(g) == 0) return(NA_real_)

  # Compute trophic levels via the existing native calculator
  levels <- calculate_trophic_level(g)
  if (all(is.na(levels))) return(NA_real_)

  el <- igraph::as_edgelist(g, names = FALSE)
  diffs <- levels[el[, 2]] - levels[el[, 1]]

  # NetworkX uses numpy.std with default ddof=0 (population std); R's sd()
  # uses ddof=1 (sample std) and would diverge.
  sqrt(mean((diffs - mean(diffs))^2))
}


# ---------------------------------------------------------------------------
# Group centrality family (Everett & Borgatti 1999)
# ---------------------------------------------------------------------------

#' Group Centrality (Everett-Borgatti 1999)
#'
#' Group centrality measures the importance of a \emph{set} of nodes
#' \eqn{C \subseteq V} rather than a single node. Three variants are
#' supported:
#'
#' \describe{
#'   \item{betweenness}{\eqn{GBC(C) = \sum_{s,t \in V \setminus C, s \ne t}
#'     \sigma(s, t \mid C) / \sigma(s, t)}, where \eqn{\sigma(s, t)} is the
#'     number of shortest \eqn{s}-\eqn{t} paths and \eqn{\sigma(s, t \mid C)}
#'     is the number of those paths passing through at least one node in
#'     \eqn{C}. Normalized by \eqn{1 / ((|V| - |C|)(|V| - |C| - 1))}.}
#'   \item{closeness}{\eqn{GCC(C) = (|V| - |C|) / \sum_{v \in V \setminus C}
#'     d(v, C)}, where \eqn{d(v, C) = \min_{c \in C} d(v, c)} is the shortest
#'     distance from \eqn{v} to any group member. Unreachable nodes
#'     contribute 0 to the denominator sum (matching NetworkX convention).
#'     For directed graphs, cograph uses \eqn{d(v, c)} in the original
#'     direction, equivalent to NetworkX's "reverse then multi-source".}
#'   \item{degree}{\eqn{GDC(C) = |N(C) \setminus C| / (|V| - |C|)}, the
#'     fraction of non-group nodes adjacent to at least one group member.
#'     \code{mode = "in"} / \code{"out"} pick the corresponding directed
#'     neighborhood.}
#' }
#'
#' @section Divergence from NetworkX on betweenness:
#' \code{networkx.group_betweenness_centrality} uses the Puzis-Yahalom-Elovici
#' iterative algorithm, which produces results that diverge from the textbook
#' Everett-Borgatti / Puzis 2008 "at least one node in C" definition on some
#' graph topologies (verified via an independent Python brute-force). cograph
#' implements the textbook formula directly; group_closeness and group_degree
#' match NetworkX exactly.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object).
#' @param nodes Integer vector of node indices (1-based) or character vector
#'   of node names identifying the group \eqn{C}.
#' @param measure One of \code{"betweenness"}, \code{"closeness"},
#'   \code{"degree"}.
#' @param mode For directed graphs with \code{measure = "degree"}: \code{"all"}
#'   (both directions), \code{"out"} (outgoing), or \code{"in"} (incoming).
#'   Ignored for undirected graphs and other measures.
#' @param normalized Logical, for \code{"betweenness"} only. If \code{TRUE}
#'   (default), divide by \eqn{(|V| - |C|)(|V| - |C| - 1)}.
#'
#' @return A single numeric scalar — the group centrality of the set
#'   \code{nodes}.
#'
#' @seealso \code{\link{centrality}} for per-node measures.
#' @references
#' Everett, M. G., & Borgatti, S. P. (1999). The centrality of groups and
#' classes. \emph{Journal of Mathematical Sociology}, 23(3), 181-201.
#'
#' Puzis, R., Yahalom, R., & Elovici, Y. (2008). Augmentative data collection
#' for betweenness centrality. In \emph{Advances in Social Networks Analysis
#' and Mining} (pp. 196-200). IEEE.
#'
#' @export
#' @examples
#' g <- igraph::make_graph("Zachary")
#' group_centrality(g, nodes = c(1, 2, 3), measure = "betweenness")
#' group_centrality(g, nodes = c(1, 2, 3), measure = "closeness")
#' group_centrality(g, nodes = c(1, 2, 3), measure = "degree")
group_centrality <- function(x, nodes,
                             measure = c("betweenness", "closeness", "degree"),
                             mode = c("all", "out", "in"),
                             normalized = TRUE) {
  measure <- match.arg(measure)
  mode <- match.arg(mode)

  g <- to_igraph(x)
  n <- igraph::vcount(g)

  # Resolve node names to integer indices
  if (is.character(nodes)) {
    vnames <- igraph::V(g)$name
    if (is.null(vnames)) {
      stop("group_centrality: node names not available on graph", call. = FALSE)
    }
    C <- match(nodes, vnames)
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
    "betweenness" = .group_betweenness(g, C, normalized = normalized),
    "closeness"   = .group_closeness(g, C),
    "degree"      = .group_degree(g, C, mode = mode)
  )
}


#' Group betweenness: textbook Everett-Borgatti formula
#' @keywords internal
#' @noRd
.group_betweenness <- function(g, C, normalized = TRUE) {
  n <- igraph::vcount(g)
  V_minus_C <- setdiff(seq_len(n), C)
  if (length(V_minus_C) < 2L) return(0)

  total <- 0
  for (s in V_minus_C) {
    for (t in V_minus_C) {
      if (s == t) next
      asp <- igraph::all_shortest_paths(g, from = s, to = t, weights = NA)
      paths <- asp$res
      if (length(paths) == 0L) next
      through <- sum(vapply(paths, function(p) {
        pv <- as.integer(p)
        if (length(pv) <= 2L) return(FALSE)
        inner <- pv[-c(1L, length(pv))]
        any(inner %in% C)
      }, logical(1)))
      total <- total + through / length(paths)
    }
  }

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
  n <- igraph::vcount(g)
  V_minus_C <- setdiff(seq_len(n), C)
  if (length(V_minus_C) == 0L) return(0)

  # distances(g, v = V-C, to = C): matrix where D[i, j] = dist from V-C[i] to C[j]
  # min per row = distance from each v in V-C to the closest group member.
  D <- igraph::distances(g, v = V_minus_C, to = C, mode = "out", weights = NA)
  d_vec <- apply(D, 1, min)
  closeness_sum <- sum(d_vec[is.finite(d_vec)])
  if (closeness_sum == 0) return(0)
  length(V_minus_C) / closeness_sum
}


#' Group degree: |N(C) - C| / (N - |C|)
#' @keywords internal
#' @noRd
.group_degree <- function(g, C, mode = "all") {
  n <- igraph::vcount(g)
  if (!igraph::is_directed(g)) mode <- "all"

  nbrs <- integer(0)
  for (c in C) {
    nbrs <- c(nbrs, as.integer(igraph::neighbors(g, c, mode = mode)))
  }
  nbrs_unique <- unique(nbrs)
  nbrs_outside <- setdiff(nbrs_unique, C)
  k <- n - length(C)
  if (k == 0L) return(0)
  length(nbrs_outside) / k
}


# ---------------------------------------------------------------------------
# Dispersion (Backstrom-Kleinberg 2014)
# ---------------------------------------------------------------------------

#' Dispersion (Backstrom-Kleinberg 2014)
#'
#' Per-pair measure of tie strength from the Facebook relationship-inference
#' paper. For each pair \eqn{(u, v)} where \eqn{v} is a neighbor of \eqn{u}:
#'
#' \enumerate{
#'   \item Let \eqn{S_T = N(u) \cap N(v)} be their mutual friends (embeddedness).
#'   \item Count pairs \eqn{(s, t) \subset S_T} such that:
#'     \itemize{
#'       \item \eqn{s} and \eqn{t} are not directly connected, AND
#'       \item \eqn{s} and \eqn{t} share no common neighbor inside \eqn{N(u)}
#'         other than \eqn{u} and \eqn{v}.
#'     }
#'   \item The raw dispersion is this count. When \code{normalized = TRUE},
#'     the result is \eqn{(\mathrm{dispersion} + b)^{\alpha} /
#'     (\mathrm{embeddedness} + c)} (normalization is skipped when
#'     \code{embeddedness + c == 0}).
#' }
#'
#' Matches \code{networkx.dispersion} bit-exact for all three call modes
#' (single pair, single source, full matrix).
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
#'   \item Named numeric vector if exactly one of \code{u}, \code{v} is given
#'     (names are the other endpoints).
#'   \item A data frame with columns \code{from}, \code{to}, \code{dispersion}
#'     when neither \code{u} nor \code{v} is given (one row per ordered edge).
#' }
#'
#' @references
#' Backstrom, L., & Kleinberg, J. (2014). Romantic partnerships and the
#' dispersion of social ties: A network analysis of relationship status on
#' Facebook. In \emph{Proceedings of CSCW} (pp. 831-841). ACM.
#' \url{https://arxiv.org/pdf/1310.6753v1.pdf}
#'
#' @export
#' @examples
#' g <- igraph::make_graph("Zachary")
#' # Node 0 (R index 1) to node 33 (R index 34)
#' dispersion(g, u = 1, v = 34)
#' # All pairs from node 1
#' head(dispersion(g, u = 1))
dispersion <- function(x, u = NULL, v = NULL,
                       normalized = TRUE,
                       alpha = 1, b = 0, c = 0) {
  g <- to_igraph(x)
  n <- igraph::vcount(g)
  if (n == 0) return(numeric(0))

  # Resolve node labels to 1-based indices
  resolve_node <- function(node) {
    if (is.null(node)) return(NULL)
    if (is.character(node)) {
      vnames <- igraph::V(g)$name
      if (is.null(vnames)) {
        stop("dispersion: node names not available on graph", call. = FALSE)
      }
      idx <- match(node, vnames)
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

  # Adjacency list (undirected treatment — dispersion is defined on the
  # undirected ego network in Backstrom-Kleinberg). For a directed graph,
  # NetworkX treats G[u] as OUT-neighbors, which we match.
  nbrs_of <- function(node) {
    as.integer(igraph::neighbors(g, node, mode = "out"))
  }

  # Single-pair inner computation
  disp_pair <- function(u_i, v_i) {
    u_nbrs <- nbrs_of(u_i)
    v_nbrs <- nbrs_of(v_i)
    ST <- intersect(v_nbrs, u_nbrs)
    set_uv <- c(u_i, v_i)
    total <- 0L
    if (length(ST) >= 2L) {
      # All unordered pairs from ST
      k <- length(ST)
      for (i in seq_len(k - 1L)) {
        for (j in seq(i + 1L, k)) {
          s <- ST[i]
          t <- ST[j]
          # nbrs_s = u's neighbors intersected with s's neighbors, minus {u, v}
          s_nbrs <- nbrs_of(s)
          nbrs_s <- setdiff(intersect(u_nbrs, s_nbrs), set_uv)
          # s and t not directly connected?
          if (!(t %in% nbrs_s)) {
            t_nbrs <- nbrs_of(t)
            # s and t don't share a common neighbor in u's ego net
            if (length(intersect(nbrs_s, t_nbrs)) == 0L) {
              total <- total + 1L
            }
          }
        }
      }
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

  # Both NULL: compute for every (u, v) where v is a neighbor of u
  rows <- list()
  for (uu in seq_len(n)) {
    u_nbrs <- nbrs_of(uu)
    for (vv in u_nbrs) {
      rows[[length(rows) + 1L]] <- data.frame(
        from = uu, to = vv,
        dispersion = disp_pair(uu, vv),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(from = integer(0), to = integer(0),
                      dispersion = numeric(0)))
  }
  do.call(rbind, rows)
}
