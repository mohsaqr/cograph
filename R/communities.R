# Community Detection Functions
# Wrapper functions for igraph community detection algorithms with full parameter exposure
# igraph is Suggests — every call site uses `igraph::` with a
# `requireNamespace("igraph", quietly = TRUE)` guard, so no @importFrom here.

# ==============================================================================
# Main Function
# ==============================================================================

#' Community Detection
#'
#' @description
#' Detects communities/clusters in networks using various algorithms.
#' Provides a unified interface to igraph's community detection functions
#' with full parameter exposure.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param method Community detection algorithm. One of:
#'   \itemize{
#'     \item \code{"louvain"} - Louvain modularity optimization (default, fast)
#'     \item \code{"leiden"} - Leiden algorithm (improved Louvain)
#'     \item \code{"fast_greedy"} - Fast greedy modularity optimization
#'     \item \code{"walktrap"} - Random walk-based detection
#'     \item \code{"infomap"} - Information theoretic approach
#'     \item \code{"label_propagation"} - Label propagation (very fast)
#'     \item \code{"edge_betweenness"} - Girvan-Newman algorithm
#'     \item \code{"leading_eigenvector"} - Leading eigenvector method
#'     \item \code{"spinglass"} - Spinglass simulation
#'     \item \code{"optimal"} - Exact modularity optimization (slow)
#'     \item \code{"fluid"} - Fluid communities algorithm
#'   }
#' @param community Optional integer or character vector. If supplied, the
#'   returned data frame is filtered to rows whose `community` column
#'   matches one of the given values. Default `NULL` (keep all communities).
#' @param weights Edge weights. If NULL, uses edge weights from the network
#'   if available, otherwise unweighted. Set to NA for explicitly unweighted.
#' @param resolution Resolution parameter for modularity-based methods
#'   (louvain, leiden). Higher values yield more communities. Default 1.
#' @param directed Logical; whether to treat the network as directed.
#'   Default NULL (auto-detect).
#' @param seed Random seed for reproducibility. Only applies to stochastic
#'   algorithms (louvain, leiden, infomap, label_propagation, spinglass).
#' @param ... Additional parameters passed to the specific algorithm.
#'   See individual functions for details.
#'
#' @return A tidy \code{cograph_communities} data frame with columns:
#'   \describe{
#'     \item{node}{Node label (character)}
#'     \item{community}{Community assignment (integer)}
#'   }
#'   Metadata stored as attributes: \code{"algorithm"}, \code{"modularity"},
#'   \code{"network"} (original input), \code{"igraph_result"}.
#'
#' @details
#' \strong{Algorithm Selection Guide:}
#'
#' \tabular{lll}{
#'   Algorithm \tab Best For \tab Time Complexity \cr
#'   louvain \tab Large networks, general use \tab O(n log n) \cr
#'   leiden \tab Large networks, better quality than louvain \tab O(n log n) \cr
#'   fast_greedy \tab Medium networks \tab O(n² log n) \cr
#'   walktrap \tab Networks with clear community structure \tab O(n² log n) \cr
#'   infomap \tab Directed networks, flow-based \tab O(E) \cr
#'   label_propagation \tab Very large networks, speed critical \tab O(E) \cr
#'   edge_betweenness \tab Small networks, hierarchical \tab O(E² n) \cr
#'   leading_eigenvector \tab Networks with dominant structure \tab O(n²) \cr
#'   spinglass \tab Small networks, allows negative weights \tab O(n³) \cr
#'   optimal \tab Tiny networks only (<50 nodes) \tab NP-hard \cr
#'   fluid \tab When k is known \tab O(E k) \cr
#' }
#'
#' @export
#' @seealso
#' \code{\link{community_louvain}}, \code{\link{community_leiden}},
#' \code{\link{community_fast_greedy}}, \code{\link{community_walktrap}},
#' \code{\link{community_infomap}}, \code{\link{community_label_propagation}},
#' \code{\link{community_edge_betweenness}}, \code{\link{community_leading_eigenvector}},
#' \code{\link{community_spinglass}}, \code{\link{community_optimal}},
#' \code{\link{community_fluid}}
#'
#' @examples
#' # Create a network with community structure
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'
#'   # Default (Louvain)
#'   comm <- cograph::communities(g)
#'   print(comm)
#'
#'   # Walktrap
#'   comm2 <- cograph::communities(g, method = "walktrap")
#'   print(comm2)
#' }
communities <- function(x,
                        method = c("louvain", "leiden", "fast_greedy",
                                   "walktrap", "infomap", "label_propagation",
                                   "edge_betweenness", "leading_eigenvector",
                                   "spinglass", "optimal", "fluid"),
                        community = NULL,
                        weights = NULL,
                        resolution = 1,
                        directed = NULL,
                        seed = NULL,
                        ...) {

  method <- match.arg(method)

  # Check for directed graph with undirected-only method
  undirected_only <- c("louvain", "leiden", "fast_greedy", "leading_eigenvector", "fluid")
  if (method %in% undirected_only) {
    g <- to_igraph(x)
    if (igraph::is_directed(g)) {
      message("Method '", method, "' requires undirected graph; falling back to 'walktrap'")
      method <- "walktrap"
    }
  }

  # Dispatch to specific function
  result <- switch(method,
    "louvain" = community_louvain(x, weights = weights, resolution = resolution,
                                  seed = seed, ...),
    "leiden" = community_leiden(x, weights = weights, resolution = resolution,
                                seed = seed, ...),
    "fast_greedy" = community_fast_greedy(x, weights = weights, ...),
    "walktrap" = community_walktrap(x, weights = weights, ...),
    "infomap" = community_infomap(x, weights = weights, seed = seed, ...),
    "label_propagation" = community_label_propagation(x, weights = weights,
                                                       seed = seed, ...),
    "edge_betweenness" = community_edge_betweenness(x, weights = weights,
                                                     directed = directed, ...),
    "leading_eigenvector" = community_leading_eigenvector(x, weights = weights, ...),
    "spinglass" = community_spinglass(x, weights = weights, seed = seed, ...),
    "optimal" = community_optimal(x, weights = weights, ...),
    "fluid" = community_fluid(x, ...)
  )

  # Filter to specific community if requested
  if (!is.null(community)) {
    result <- result[result$community %in% community, , drop = FALSE]
  }

  result
}


# ==============================================================================
# Individual Algorithm Functions
# ==============================================================================

#' Louvain Community Detection
#'
#' Multi-level modularity optimization using the Louvain algorithm.
#' Fast and widely used for large networks.
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param resolution Resolution parameter. Higher values = more communities.
#'   Default 1 (standard modularity).
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Blondel, V.D., Guillaume, J.L., Lambiotte, R., & Lefebvre, E. (2008).
#' Fast unfolding of communities in large networks.
#' \emph{Journal of Statistical Mechanics}, P10008.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'   comm <- community_louvain(g)
#'   membership(comm)
#'
#'   # Reproducible result with seed
#'   comm1 <- community_louvain(g, seed = 42)
#'   comm2 <- community_louvain(g, seed = 42)
#'   identical(membership(comm1), membership(comm2))
#' }
community_louvain <- function(x, weights = NULL, resolution = 1, seed = NULL, ...) {
  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }
  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights, abs_if_negative = TRUE)

  result <- igraph::cluster_louvain(g, weights = w, resolution = resolution)
  .wrap_communities(result, "louvain", g, network = x)
}


#' Leiden Community Detection
#'
#' Leiden algorithm - an improved version of Louvain that guarantees
#' well-connected communities. Supports CPM and modularity objectives.
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param resolution Resolution parameter. Default 1.
#' @param objective_function Optimization objective: "CPM" (Constant Potts Model)
#'   or "modularity". Default "CPM".
#' @param beta Parameter for randomness in refinement step. Default 0.01.
#' @param initial_membership Initial community assignments (optional).
#' @param n_iterations Number of iterations. Default 2. Use -1 for convergence.
#' @param vertex_weights Vertex weights for CPM objective.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Traag, V.A., Waltman, L., & van Eck, N.J. (2019).
#' From Louvain to Leiden: guaranteeing well-connected communities.
#' \emph{Scientific Reports}, 9, 5233.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'
#'   # Standard Leiden
#'   comm <- community_leiden(g)
#'
#'   # Higher resolution for more communities
#'   comm2 <- community_leiden(g, resolution = 1.5)
#'
#'   # Modularity objective
#'   comm3 <- community_leiden(g, objective_function = "modularity")
#' }
community_leiden <- function(x,
                             weights = NULL,
                             resolution = 1,
                             objective_function = c("CPM", "modularity"),
                             beta = 0.01,
                             initial_membership = NULL,
                             n_iterations = 2,
                             vertex_weights = NULL,
                             seed = NULL,
                             ...) {

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }
  objective_function <- match.arg(objective_function)
  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights, abs_if_negative = TRUE)

  result <- igraph::cluster_leiden(
    g,
    weights = w,
    resolution = resolution,
    objective_function = objective_function,
    beta = beta,
    initial_membership = initial_membership,
    n_iterations = n_iterations,
    vertex_weights = vertex_weights
  )
  .wrap_communities(result, "leiden", g, network = x)
}


#' Fast Greedy Community Detection
#'
#' Hierarchical agglomeration using greedy modularity optimization.
#' Produces a dendrogram of community merges.
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param merges Logical; return merge matrix? Default TRUE.
#' @param modularity Logical; return modularity scores? Default TRUE.
#' @param membership Logical; return membership vector? Default TRUE.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object with optional dendrogram
#'
#' @references
#' Clauset, A., Newman, M.E.J., & Moore, C. (2004).
#' Finding community structure in very large networks.
#' \emph{Physical Review E}, 70, 066111.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_fast_greedy(g)
#' membership(comm)
community_fast_greedy <- function(x,
                                  weights = NULL,
                                  merges = TRUE,
                                  modularity = TRUE,
                                  membership = TRUE,
                                  ...) {

  g <- to_igraph(x, ...)

  # Fast greedy requires undirected

  if (igraph::is_directed(g)) {
    g <- igraph::as_undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum"))
  }

  w <- .resolve_weights(g, weights, abs_if_negative = TRUE)

  result <- igraph::cluster_fast_greedy(
    g,
    weights = w,
    merges = merges,
    modularity = modularity,
    membership = membership
  )
  .wrap_communities(result, "fast_greedy", g, network = x)
}


#' Walktrap Community Detection
#'
#' Detects communities via random walks. Nodes within the same community
#' tend to have short random walk distances.
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param steps Number of random walk steps. Default 4.
#' @param merges Logical; return merge matrix? Default TRUE.
#' @param modularity Logical; return modularity scores? Default TRUE.
#' @param membership Logical; return membership vector? Default TRUE.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Pons, P., & Latapy, M. (2006).
#' Computing communities in large networks using random walks.
#' \emph{Journal of Graph Algorithms and Applications}, 10(2), 191-218.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'
#'   # Default 4 steps
#'   comm <- community_walktrap(g)
#'
#'   # More steps for larger communities
#'   comm2 <- community_walktrap(g, steps = 8)
#' }
community_walktrap <- function(x,
                               weights = NULL,
                               steps = 4,
                               merges = TRUE,
                               modularity = TRUE,
                               membership = TRUE,
                               ...) {

  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights, abs_if_negative = TRUE)

  result <- igraph::cluster_walktrap(
    g,
    weights = w,
    steps = steps,
    merges = merges,
    modularity = modularity,
    membership = membership
  )
  .wrap_communities(result, "walktrap", g, network = x)
}


#' Infomap Community Detection
#'
#' Information-theoretic community detection based on random walk dynamics.
#' Minimizes the map equation (description length of random walks).
#'
#' @param x Network input
#' @param weights Edge weights for transitions. NULL uses network weights.
#' @param v.weights Vertex weights (teleportation weights).
#' @param nb.trials Number of optimization trials. Default 10.
#' @param modularity Logical; calculate modularity? Default TRUE.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Rosvall, M., & Bergstrom, C.T. (2008).
#' Maps of random walks on complex networks reveal community structure.
#' \emph{PNAS}, 105(4), 1118-1123.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'   comm <- community_infomap(g, nb.trials = 20)
#' }
community_infomap <- function(x,
                              weights = NULL,
                              v.weights = NULL,
                              nb.trials = 10,
                              modularity = TRUE,
                              seed = NULL,
                              ...) {

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }
  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights, abs_if_negative = TRUE)

  result <- igraph::cluster_infomap(
    g,
    e.weights = w,
    v.weights = v.weights,
    nb.trials = nb.trials,
    modularity = modularity
  )
  .wrap_communities(result, "infomap", g, network = x)
}


#' Label Propagation Community Detection
#'
#' Fast semi-synchronous label propagation algorithm.
#' Each node adopts the most frequent label among its neighbors.
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param mode For directed graphs: "out" (default), "in", or "all".
#' @param initial Initial labels (integer vector or NULL for unique labels).
#' @param fixed Logical vector indicating which labels are fixed.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Raghavan, U.N., Albert, R., & Kumara, S. (2007).
#' Near linear time algorithm to detect community structures in large-scale networks.
#' \emph{Physical Review E}, 76, 036106.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'
#'   # Basic label propagation
#'   comm <- community_label_propagation(g)
#'
#'   # With some nodes fixed to specific communities
#'   initial <- rep(NA, igraph::vcount(g))
#'   initial[1] <- 1  # Node 1 in community 1
#'   initial[34] <- 2 # Node 34 in community 2
#'   fixed <- !is.na(initial)
#'   initial[is.na(initial)] <- seq_len(sum(is.na(initial)))
#'   comm2 <- community_label_propagation(g, initial = initial, fixed = fixed)
#' }
community_label_propagation <- function(x,
                                 weights = NULL,
                                 mode = c("out", "in", "all"),
                                 initial = NULL,
                                 fixed = NULL,
                                 seed = NULL,
                                 ...) {

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }
  mode <- match.arg(mode)
  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights, abs_if_negative = TRUE)

  result <- igraph::cluster_label_prop(
    g,
    weights = w,
    mode = mode,
    initial = initial,
    fixed = fixed
  )
  .wrap_communities(result, "label_propagation", g, network = x)
}


#' Edge Betweenness Community Detection
#'
#' Girvan-Newman algorithm. Iteratively removes edges with highest
#' betweenness centrality to reveal community structure.
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param directed Logical; treat graph as directed? Default TRUE.
#' @param edge.betweenness Logical; return edge betweenness values? Default TRUE.
#' @param merges Logical; return merge matrix? Default TRUE.
#' @param bridges Logical; return bridge edges? Default TRUE.
#' @param modularity Logical; return modularity scores? Default TRUE.
#' @param membership Logical; return membership vector? Default TRUE.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Girvan, M., & Newman, M.E.J. (2002).
#' Community structure in social and biological networks.
#' \emph{PNAS}, 99(12), 7821-7826.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_edge_betweenness(g)
#' membership(comm)
community_edge_betweenness <- function(x,
                                       weights = NULL,
                                       directed = TRUE,
                                       edge.betweenness = TRUE,
                                       merges = TRUE,
                                       bridges = TRUE,
                                       modularity = TRUE,
                                       membership = TRUE,
                                       ...) {

  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights, abs_if_negative = TRUE)

  if (is.null(directed)) {
    directed <- igraph::is_directed(g)
  }

  result <- igraph::cluster_edge_betweenness(
    g,
    weights = w,
    directed = directed,
    edge.betweenness = edge.betweenness,
    merges = merges,
    bridges = bridges,
    modularity = modularity,
    membership = membership
  )
  .wrap_communities(result, "edge_betweenness", g, network = x)
}


#' Leading Eigenvector Community Detection
#'
#' Detects communities using the leading eigenvector of the modularity matrix.
#' Hierarchical divisive algorithm.
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param steps Maximum number of splits. Default -1 (until modularity decreases).
#' @param start Starting community structure (membership vector).
#' @param options ARPACK options list. Default uses igraph::arpack_defaults().
#' @param callback Optional callback function called after each split.
#' @param extra Extra argument passed to callback.
#' @param env Environment for callback evaluation.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Newman, M.E.J. (2006).
#' Finding community structure using the eigenvectors of matrices.
#' \emph{Physical Review E}, 74, 036104.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_leading_eigenvector(g)
#' membership(comm)
community_leading_eigenvector <- function(x,
                                    weights = NULL,
                                    steps = -1,
                                    start = NULL,
                                    options = igraph::arpack_defaults(),
                                    callback = NULL,
                                    extra = NULL,
                                    env = parent.frame(),
                                    ...) {

  g <- to_igraph(x, ...)

  # Requires undirected
  if (igraph::is_directed(g)) {
    g <- igraph::as_undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum"))
  }

  w <- .resolve_weights(g, weights, abs_if_negative = TRUE)

  result <- igraph::cluster_leading_eigen(
    g,
    steps = steps,
    weights = w,
    start = start,
    options = options,
    callback = callback,
    extra = extra,
    env = env
  )
  .wrap_communities(result, "leading_eigenvector", g, network = x)
}


#' Spinglass Community Detection
#'
#' Statistical mechanics approach using simulated annealing.
#' Can handle negative edge weights.
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param vertex Vertex to find community for (single community mode).
#'   NULL for full partitioning.
#' @param spins Number of spins (maximum communities). Default 25.
#' @param parupdate Parallel update mode. Default FALSE.
#' @param start.temp Starting temperature. Default 1.
#' @param stop.temp Stopping temperature. Default 0.01.
#' @param cool.fact Cooling factor. Default 0.99.
#' @param update.rule Update rule: "config" (default), "random", or "simple".
#' @param gamma Gamma parameter for modularity. Default 1.
#' @param implementation "orig" (default) or "neg" (for negative weights).
#' @param gamma.minus Gamma for negative weights in "neg" implementation.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Reichardt, J., & Bornholdt, S. (2006).
#' Statistical mechanics of community detection.
#' \emph{Physical Review E}, 74, 016110.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_spinglass(g)
#' membership(comm)
community_spinglass <- function(x,
                                weights = NULL,
                                vertex = NULL,
                                spins = 25,
                                parupdate = FALSE,
                                start.temp = 1,
                                stop.temp = 0.01,
                                cool.fact = 0.99,
                                update.rule = c("config", "random", "simple"),
                                gamma = 1,
                                implementation = c("orig", "neg"),
                                gamma.minus = 1,
                                seed = NULL,
                                ...) {

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }
  update.rule <- match.arg(update.rule)
  implementation <- match.arg(implementation)

  g <- to_igraph(x, ...)

  # Requires connected graph
  if (!igraph::is_connected(g)) {
    warning("Spinglass requires connected graph. Using largest component.",
            call. = FALSE)
    comp <- igraph::components(g)
    g <- igraph::induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  }

  w <- .resolve_weights(g, weights)

  result <- igraph::cluster_spinglass(
    g,
    weights = w,
    vertex = vertex,
    spins = spins,
    parupdate = parupdate,
    start.temp = start.temp,
    stop.temp = stop.temp,
    cool.fact = cool.fact,
    update.rule = update.rule,
    gamma = gamma,
    implementation = implementation,
    gamma.minus = gamma.minus
  )
  .wrap_communities(result, "spinglass", g, network = x)
}


#' Optimal Community Detection
#'
#' Finds the optimal community structure by maximizing modularity exactly.
#' Very slow - only use for small networks (<50 nodes).
#'
#' @param x Network input
#' @param weights Edge weights. NULL uses network weights, NA for unweighted.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @note This is an NP-hard problem. Use only for tiny networks.
#'
#' @references
#' Brandes, U., Delling, D., Gaertler, M., Gorke, R., Hoefer, M.,
#' Nikoloski, Z., & Wagner, D. (2008).
#' On modularity clustering.
#' \emph{IEEE Transactions on Knowledge and Data Engineering}, 20(2), 172-188.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_ring(10)
#' comm <- community_optimal(g)
#' membership(comm)
community_optimal <- function(x, weights = NULL, ...) {

  g <- to_igraph(x, ...)

  if (igraph::vcount(g) > 50) {
    warning("Optimal modularity is very slow for >50 nodes. Consider louvain/leiden.",
            call. = FALSE)
  }

  w <- .resolve_weights(g, weights)

  result <- igraph::cluster_optimal(g, weights = w)
  .wrap_communities(result, "optimal", g, network = x)
}


#' Fluid Communities Detection
#'
#' Simulates fluid dynamics where communities compete for nodes.
#' Requires specifying the number of communities.
#'
#' @param x Network input
#' @param no.of.communities Number of communities to detect. Required.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A \code{cograph_communities} object
#'
#' @references
#' Pares, F., Gasulla, D.G., Vilalta, A., Moreno, J., Ayguade, E.,
#' Labarta, J., Cortes, U., & Suzumura, T. (2018).
#' Fluid communities: A competitive, scalable and diverse community detection algorithm.
#' \emph{Studies in Computational Intelligence}, 689, 229-240.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'
#'   # Detect exactly 2 communities
#'   comm <- community_fluid(g, no.of.communities = 2)
#' }
community_fluid <- function(x, no.of.communities, ...) {

  if (missing(no.of.communities)) {
    stop("no.of.communities is required for fluid communities", call. = FALSE)
  }

  g <- to_igraph(x, ...)

  # Requires undirected, connected
  if (igraph::is_directed(g)) {
    g <- igraph::as_undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum"))
  }
  if (!igraph::is_connected(g)) {
    warning("Fluid communities requires connected graph. Using largest component.",
            call. = FALSE)
    comp <- igraph::components(g)
    g <- igraph::induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  }

  result <- igraph::cluster_fluid_communities(g, no.of.communities = no.of.communities)
  .wrap_communities(result, "fluid", g, network = x)
}


# ==============================================================================
# Consensus Community Detection
# ==============================================================================

#' Consensus Community Detection
#'
#' Runs a stochastic community detection algorithm multiple times and finds
#' consensus communities via co-occurrence matrix thresholding. This approach
#' produces more robust and stable community assignments than single runs.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param method Community detection algorithm to use. Default "louvain".
#'   Must be a stochastic method (louvain, leiden, infomap, label_propagation,
#'   spinglass).
#' @param n_runs Number of times to run the algorithm. Default 100.
#' @param threshold Co-occurrence threshold for consensus. Default 0.5.
#'   Nodes that appear together in >= threshold proportion of runs are
#'   placed in the same community.
#' @param seed Optional seed for reproducibility. If provided, seeds for

#'   individual runs are derived from this seed.
#' @param ... Additional arguments passed to the community detection method.
#'
#' @return A \code{cograph_communities} object with consensus membership.
#'
#' @details
#' The algorithm works as follows:
#' \enumerate{
#'   \item Run the specified algorithm \code{n_runs} times (without seeds to
#'     allow variation)
#'   \item Build a co-occurrence matrix counting how often each pair of nodes
#'     appears in the same community
#'   \item Normalize to proportions (0-1)
#'   \item Threshold to create a consensus graph (edge if co-occurrence >= threshold)
#'   \item Run walktrap on the consensus graph to get final communities
#' }
#'
#' @references
#' Lancichinetti, A., & Fortunato, S. (2012).
#' Consensus clustering in complex networks.
#' \emph{Scientific Reports}, 2, 336.
#'
#' @export
#' @seealso \code{\link{communities}}, \code{\link{community_louvain}}
#'
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'
#'   # Consensus from 50 Louvain runs
#'   cc <- community_consensus(g, method = "louvain", n_runs = 50)
#'   print(cc)
#'
#'   # Stricter threshold for more robust communities
#'   cc2 <- community_consensus(g, threshold = 0.7, n_runs = 100)
#' }
community_consensus <- function(x,
                                 method = c("louvain", "leiden", "infomap",
                                            "label_propagation", "spinglass"),
                                 n_runs = 100,
                                 threshold = 0.5,
                                 seed = NULL,
                                 ...) {

  method <- match.arg(method)

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }

  g <- to_igraph(x)
  n <- igraph::vcount(g)

  # Co-occurrence matrix
  cooccur <- matrix(0, n, n)

  # Run algorithm n_runs times (no seed per run - want variation)
  comm_fn <- switch(method,
    "louvain" = function(...) igraph::cluster_louvain(g, ...),
    "leiden" = function(...) igraph::cluster_leiden(g, ...),
    "fast_greedy" = function(...) igraph::cluster_fast_greedy(g, ...),
    "walktrap" = function(...) igraph::cluster_walktrap(g, ...),
    "infomap" = function(...) igraph::cluster_infomap(g, ...),
    "label_propagation" = function(...) igraph::cluster_label_prop(g, ...),
    "edge_betweenness" = function(...) igraph::cluster_edge_betweenness(g, ...),
    "leading_eigenvector" = function(...) igraph::cluster_leading_eigen(g, ...),
    function(...) igraph::cluster_louvain(g, ...)
  )
  for (i in seq_len(n_runs)) {
    raw <- comm_fn()
    mem <- igraph::membership(raw)

    # Update co-occurrence for nodes in same community
    for (c in unique(mem)) {
      nodes <- which(mem == c)
      if (length(nodes) > 1) {
        cooccur[nodes, nodes] <- cooccur[nodes, nodes] + 1
      }
    }
    # Self-co-occurrence (each node is always with itself)
    diag(cooccur) <- diag(cooccur) + 1
  }

  # Normalize to proportions
  cooccur <- cooccur / n_runs

  # Threshold to get consensus graph
  consensus_adj <- (cooccur >= threshold) * 1
  diag(consensus_adj) <- 0

  # Final clustering on consensus graph using walktrap (deterministic)
  consensus_g <- igraph::graph_from_adjacency_matrix(
    consensus_adj,
    mode = "undirected",
    weighted = NULL
  )

  # Transfer node names if available
  if (igraph::is_named(g)) {
    igraph::V(consensus_g)$name <- igraph::V(g)$name
  }

  result <- igraph::cluster_walktrap(consensus_g)

  .wrap_communities(result, paste0("consensus_", method), g, network = x)
}

#' @rdname community_consensus
#' @export
com_consensus <- community_consensus


# ==============================================================================
# Short Aliases (com_*)
# ==============================================================================

#' @rdname community_louvain
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @export
com_lv <- community_louvain

#' @rdname community_leiden
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @export
com_ld <- community_leiden

#' @rdname community_fast_greedy
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @export
com_fg <- community_fast_greedy

#' @rdname community_walktrap
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @export
com_wt <- community_walktrap

#' @rdname community_infomap
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @export
com_im <- community_infomap

#' @rdname community_label_propagation
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' net <- as_cograph(matrix(runif(25), 5, 5))
#' com_lp(net)
#' @export
com_lp <- community_label_propagation

#' @rdname community_edge_betweenness
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' net <- as_cograph(matrix(runif(25), 5, 5))
#' com_eb(net)
#' @export
com_eb <- community_edge_betweenness

#' @rdname community_leading_eigenvector
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' net <- as_cograph(matrix(runif(25), 5, 5))
#' com_le(net)
#' @export
com_le <- community_leading_eigenvector

#' @rdname community_spinglass
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' net <- as_cograph(matrix(runif(25), 5, 5))
#' com_sg(net)
#' @export
com_sg <- community_spinglass

#' @rdname community_optimal
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' net <- as_cograph(matrix(runif(25), 5, 5))
#' com_op(net)
#' @export
com_op <- community_optimal

#' @rdname community_fluid
#' @return A \code{cograph_communities} object. See \code{\link{detect_communities}}.
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' m <- matrix(runif(25), 5, 5); diag(m) <- 0
#' net <- as_cograph(m)
#' com_fl(net, no.of.communities = 2)
#' @export
com_fl <- community_fluid


# ==============================================================================
# Helper Functions
# ==============================================================================

#' Resolve edge weights
#' @keywords internal
.resolve_weights <- function(g, weights, abs_if_negative = FALSE) {
  if (is.null(weights)) {
    # Use network weights if available
    if ("weight" %in% igraph::edge_attr_names(g)) {
      w <- igraph::E(g)$weight
      if (abs_if_negative && any(w < 0, na.rm = TRUE)) return(abs(w))
      return(w)
    }
    return(NULL)
  }
  if (length(weights) == 1 && is.na(weights)) {
    # Explicitly unweighted
    return(NULL)
  }
  if (abs_if_negative && any(weights < 0, na.rm = TRUE)) return(abs(weights))
  weights
}


#' Wrap igraph communities result
#' @keywords internal
.wrap_communities <- function(result, algorithm, g, network = NULL) {
  # Node labels
  node_labels <- if (igraph::is_named(g)) {
    igraph::V(g)$name
  } else {
    as.character(seq_len(igraph::vcount(g)))
  }

  # Extract membership (may be NULL if membership = FALSE was passed)
  mem <- result$membership
  if (is.null(mem) || length(mem) == 0L) {
    mem <- tryCatch(
      igraph::membership(result),
      error = function(e) rep(NA_integer_, length(node_labels))
    )
  }

  # Build tidy data frame
  df <- data.frame(
    node = node_labels,
    community = mem,
    stringsAsFactors = FALSE
  )

  # Attach full igraph result and metadata as attributes
  attr(df, "igraph_result") <- result
  attr(df, "algorithm") <- algorithm
  attr(df, "modularity") <- tryCatch(
    igraph::modularity(result),
    error = function(e) NA_real_
  )
  attr(df, "network") <- network

  class(df) <- c("cograph_communities", "data.frame")
  df
}


# ==============================================================================
# Methods
# ==============================================================================

#' Print Community Structure
#'
#' @param x A cograph_communities object.
#' @param ... Ignored.
#' @return Invisibly returns the original object.
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_louvain(g)
#' print(comm)
print.cograph_communities <- function(x, ...) {
  alg <- attr(x, "algorithm") %||% "unknown"
  mod <- attr(x, "modularity") %||% NA
  cat("Community structure (", alg, ")\n", sep = "")
  cat("  Nodes:", nrow(x), " | Communities:",
      length(unique(x$community)), " | Modularity:",
      round(mod, 4), "\n")
  sizes <- table(x$community)
  cat("  Sizes:", paste(sizes, collapse = ", "), "\n\n")
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}


#' Get Community Membership
#'
#' Extracts a named membership vector from a communities result.
#' Works with both \code{cograph_communities} data frames and
#' igraph communities objects.
#'
#' @param x A cograph_communities or igraph communities object.
#' @return Named integer vector of community assignments.
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_louvain(g)
#' membership(comm)
membership <- function(x) {
  if (inherits(x, "cograph_communities")) {
    m <- x$community
    names(m) <- x$node
    return(m)
  }
  igraph::membership(x)
}


#' Get Number of Communities
#'
#' @param x A cograph_communities object
#' @return Integer count of communities
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_louvain(g)
#' n_communities(comm)
n_communities <- function(x) {
  length(unique(x$community))
}


#' Get Community Sizes
#'
#' @param x A cograph_communities object
#' @return Named integer vector of community sizes
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_louvain(g)
#' community_sizes(comm)
community_sizes <- function(x) {
  as.integer(table(x$community))
}


#' Get Modularity Score
#'
#' S3 method for `igraph::modularity()` on cograph_communities objects.
#' Registered dynamically in `.onLoad()` when igraph is available, because
#' igraph is a Suggests — not an Imports — and its generic must not be
#' referenced at build time.
#'
#' @param x A cograph_communities object
#' @param graph Optional igraph object for recalculation
#' @param ... Additional arguments
#' @return Numeric modularity value
#' @keywords internal
#' @noRd
modularity.cograph_communities <- function(x, graph = NULL, ...) {
  mod <- attr(x, "modularity")
  if (!is.null(mod) && is.numeric(mod)) return(mod)
  # Fallback: use igraph result
  ig <- attr(x, "igraph_result")
  if (!is.null(ig)) {
    return(tryCatch(igraph::modularity(ig), error = function(e) NA_real_))
  }
  NA_real_
}


#' Compare Community Structures
#'
#' Compares two community structures using various similarity measures.
#'
#' @param comm1 First community structure (communities object or membership vector)
#' @param comm2 Second community structure (communities object or membership vector)
#' @param method Comparison method: "vi" (variation of information),
#'   "nmi" (normalized mutual information), "split.join",
#'   "rand" (Rand index), "adjusted.rand"
#' @return Numeric similarity/distance value
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::make_graph("Zachary")
#'   c1 <- community_louvain(g)
#'   c2 <- community_leiden(g)
#'   compare_communities(c1, c2, "nmi")
#' }
compare_communities <- function(comm1, comm2,
                                method = c("vi", "nmi", "split.join",
                                           "rand", "adjusted.rand")) {
  method <- match.arg(method)

  # Convert to 0-indexed communities objects for igraph's C code
  # igraph's C code expects membership indices in [0, n-1] range
  .to_zero_indexed <- function(comm) {
    if (inherits(comm, "cograph_communities")) {
      ig <- attr(comm, "igraph_result")
      mem <- if (!is.null(ig)) ig$membership else as.integer(as.factor(comm$community))
      structure(list(membership = mem - 1L), class = "communities")
    } else if (inherits(comm, "communities")) {
      structure(list(membership = comm$membership - 1L), class = "communities")
    } else {
      structure(list(membership = as.integer(as.factor(comm)) - 1L), class = "communities")
    }
  }

  comm1 <- .to_zero_indexed(comm1)
  comm2 <- .to_zero_indexed(comm2)

  igraph::compare(comm1, comm2, method = method)
}


#' Plot Community Structure
#'
#' Visualizes network with community coloring using splot.
#'
#' @param x A cograph_communities object
#' @param network The original network (required if not stored)
#' @param ... Additional arguments passed to splot
#' @return Invisibly returns the plot
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' g <- igraph::make_graph("Zachary")
#' comm <- community_louvain(g)
#' mat <- igraph::as_adjacency_matrix(g, sparse = FALSE)
#' plot(comm, network = mat)
plot.cograph_communities <- function(x, network = NULL, ...) {
  network <- network %||% attr(x, "network")
  if (is.null(network)) {
    stop("No network found. Pass one via: plot(comm, network = m)", call. = FALSE)
  }

  mem <- x$community
  names(mem) <- x$node
  splot(network, node_group = mem, ...)
}
