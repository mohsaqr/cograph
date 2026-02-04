#' Network-Level Summary Statistics
#'
#' Computes comprehensive network-level statistics for a network.
#' Returns a data frame with one row containing various metrics
#' including density, centralization scores, transitivity, and more.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param weighted Logical. Use edge weights for strength/centrality calculations.
#'   Default TRUE.
#' @param mode For directed networks: "all", "in", or "out". Affects degree-based
#'   calculations. Default "all".
#' @param loops Logical. If TRUE (default), keep self-loops. Set FALSE to remove them.
#' @param simplify How to combine multiple edges between the same node pair.
#'   Options: "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
#'   multiple edges.
#' @param detailed Logical. If TRUE, include mean/sd centrality statistics.
#'   Default FALSE returns 18 basic metrics; TRUE returns 29 metrics.
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
#' # From igraph object
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::erdos.renyi.game(20, 0.3)
#'   network_summary(g)
#' }
network_summary <- function(x,
                            directed = NULL,
                            weighted = TRUE,
                            mode = "all",
                            loops = TRUE,
                            simplify = "sum",
                            detailed = FALSE,
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
    assortativity_degree = igraph::assortativity_degree(g, directed = is_directed),
    hub_score = tryCatch(
      igraph::hub_score(g, weights = weights)$value,
      error = function(e) NA_real_
    ),
    authority_score = tryCatch(
      igraph::authority_score(g, weights = weights)$value,
      error = function(e) NA_real_
    )
  )

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
#' Creates a histogram showing the degree distribution of a network.
#' Useful for understanding the connectivity patterns and identifying
#' whether a network follows particular degree distributions (e.g.,
#' power-law, normal).
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param mode For directed networks: "all", "in", or "out". Default "all".
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param loops Logical. If TRUE (default), keep self-loops. Set FALSE to remove them.
#' @param simplify How to combine multiple edges between the same node pair.
#'   Options: "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
#'   multiple edges.
#' @param cumulative Logical. If TRUE, show cumulative distribution instead of
#'   frequency distribution. Default FALSE.
#' @param main Character. Plot title. Default "Degree Distribution".
#' @param xlab Character. X-axis label. Default "Degree".
#' @param ylab Character. Y-axis label. Default "Frequency" (or "Cumulative
#'   Frequency" if cumulative = TRUE).
#' @param col Character. Bar fill color. Default "steelblue".
#' @param ... Additional arguments passed to \code{\link[graphics]{hist}}.
#'
#' @return Invisibly returns the histogram object from \code{graphics::hist()}.
#'
#' @export
#' @examples
#' # Basic usage
#' adj <- matrix(c(0, 1, 1, 0,
#'                 1, 0, 1, 1,
#'                 1, 1, 0, 1,
#'                 0, 1, 1, 0), 4, 4, byrow = TRUE)
#' degree_distribution(adj)
#'
#' # Cumulative distribution
#' degree_distribution(adj, cumulative = TRUE)
#'
#' # For directed networks
#' directed_adj <- matrix(c(0, 1, 0, 0,
#'                          0, 0, 1, 0,
#'                          1, 0, 0, 1,
#'                          0, 1, 0, 0), 4, 4, byrow = TRUE)
#' degree_distribution(directed_adj, mode = "in", main = "In-Degree Distribution")
#'
#' # With igraph
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::erdos.renyi.game(100, 0.1)
#'   degree_distribution(g, col = "coral")
#' }
degree_distribution <- function(x,
                                mode = "all",
                                directed = NULL,
                                loops = TRUE,
                                simplify = "sum",
                                cumulative = FALSE,
                                main = "Degree Distribution",
                                xlab = "Degree",
                                ylab = "Frequency",
                                col = "steelblue",
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

  # Get degree values
  deg <- igraph::degree(g, mode = mode)

  # Adjust y-axis label for cumulative

  if (cumulative && ylab == "Frequency") {
    ylab <- "Cumulative Frequency"
  }

  # Create histogram
  if (cumulative) {
    # For cumulative, we need to compute the empirical CDF
    deg_sorted <- sort(deg)
    n <- length(deg_sorted)
    cum_freq <- seq_len(n) / n

    # Create histogram for return value, but plot CDF
    h <- graphics::hist(deg, plot = FALSE, ...)

    # Plot cumulative distribution as step function
    graphics::plot(deg_sorted, cum_freq,
                   type = "s",
                   main = main,
                   xlab = xlab,
                   ylab = ylab,
                   col = col,
                   lwd = 2,
                   ...)
  } else {
    # Standard histogram
    h <- graphics::hist(deg,
                        main = main,
                        xlab = xlab,
                        ylab = ylab,
                        col = col,
                        ...)
  }

  invisible(h)
}
