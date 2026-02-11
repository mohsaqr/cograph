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
#' @param invert_weights Logical. Invert weights so higher weights = shorter paths?
#'   Default TRUE (appropriate for strength/frequency weights). Set FALSE for
#'   distance/cost weights.
#' @param alpha Numeric. Exponent for weight inversion: distance = 1/weight^alpha.
#'   Default 1.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Numeric in \[0, 1\]: global efficiency
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
                                      invert_weights = TRUE, alpha = 1, ...) {
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
#' @param invert_weights Logical. Invert weights so higher weights = shorter paths?
#'   Default TRUE (appropriate for strength/frequency weights).
#' @param alpha Numeric. Exponent for weight inversion. Default 1.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return Numeric in \[0, 1\]: average local efficiency
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
network_local_efficiency <- function(x, weights = NULL, invert_weights = TRUE, alpha = 1, ...) {
  if (inherits(x, "igraph")) {
    g <- x
  } else {
    g <- to_igraph(x, ...)
  }

  # Make undirected for local efficiency calculation
  if (igraph::is_directed(g)) {
    g <- igraph::as.undirected(g, mode = "collapse")
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
  }

  local_eff <- numeric(n)

  for (v in seq_len(n)) {
    # Get neighbors
    neighbors <- igraph::neighbors(g, v, mode = "all")
    k <- length(neighbors)

    if (k <= 1) {
      local_eff[v] <- 0
      next
    }

    # Induce subgraph on neighbors
    subg <- igraph::induced_subgraph(g, neighbors)

    # Compute efficiency of subgraph (weights already inverted on g)
    sp <- igraph::distances(subg, mode = "all",
                            weights = if (!is.null(weights)) igraph::E(subg)$weight else NULL)
    diag(sp) <- NA
    inv_sp <- 1 / sp
    inv_sp[is.infinite(sp)] <- 0

    local_eff[v] <- sum(inv_sp, na.rm = TRUE) / (k * (k - 1))
  }

  mean(local_eff, na.rm = TRUE)
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
    g <- igraph::as.undirected(g, mode = "collapse")
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
  if (C_obs == 0 || L_obs == 0 || is.infinite(L_obs)) {
    return(NA_real_)
  }

  # Generate random graphs and compute averages
  C_rand_vals <- numeric(n_random)
  L_rand_vals <- numeric(n_random)

  for (i in seq_len(n_random)) {
    # Erdos-Renyi random graph with same n and m
    g_rand <- igraph::erdos.renyi.game(n, m, type = "gnm")
    C_rand_vals[i] <- igraph::transitivity(g_rand, type = "global")
    L_rand_vals[i] <- igraph::mean_distance(g_rand, directed = FALSE)
  }

  C_rand <- mean(C_rand_vals, na.rm = TRUE)
  L_rand <- mean(L_rand_vals, na.rm = TRUE)

  if (is.na(C_rand) || C_rand == 0 || is.na(L_rand) || L_rand == 0) {
    return(NA_real_)
  }

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
#'   g <- igraph::barabasi.game(50, m = 2)
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
    g <- igraph::as.undirected(g, mode = "collapse")
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
      igraph::erdos.renyi.game(igraph::vcount(g), igraph::ecount(g), type = "gnm")
    })

    deg_rand <- igraph::degree(g_rand)
    rich_rand <- which(deg_rand > k)
    n_rich_rand <- length(rich_rand)

    if (n_rich_rand < 2) {
      phi_rand_vals[i] <- NA
      next
    }

    subg_rand <- igraph::induced_subgraph(g_rand, rich_rand)
    e_rand <- igraph::ecount(subg_rand)
    max_rand <- n_rich_rand * (n_rich_rand - 1) / 2
    phi_rand_vals[i] <- e_rand / max_rand
  }

  phi_rand <- mean(phi_rand_vals, na.rm = TRUE)

  if (is.na(phi_rand) || phi_rand == 0) {
    return(NA_real_)
  }

  phi_k / phi_rand
}
