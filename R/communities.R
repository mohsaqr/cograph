# Community Detection Functions
# Wrapper functions for igraph community detection algorithms with full parameter exposure

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
#'     \item \code{"label_propagationagation"} - Label propagation (very fast)
#'     \item \code{"edge_betweenness"} - Girvan-Newman algorithm
#'     \item \code{"leading_eigenvectorvector"} - Leading eigenvector method
#'     \item \code{"spinglass"} - Spinglass simulation
#'     \item \code{"optimal"} - Exact modularity optimization (slow)
#'     \item \code{"fluid"} - Fluid communities algorithm
#'   }
#' @param weights Edge weights. If NULL, uses edge weights from the network
#'   if available, otherwise unweighted. Set to NA for explicitly unweighted.
#' @param resolution Resolution parameter for modularity-based methods
#'   (louvain, leiden). Higher values yield more communities. Default 1.
#' @param directed Logical; whether to treat the network as directed.
#'   Default NULL (auto-detect).
#' @param ... Additional parameters passed to the specific algorithm.
#'   See individual functions for details.
#'
#' @return A \code{cograph_communities} object (extends igraph's communities class)
#'   with components:
#'   \describe{
#'     \item{membership}{Integer vector of community assignments}
#'     \item{modularity}{Modularity score of the partition}
#'     \item{algorithm}{Name of the algorithm used
#'     \item{names}{Node names if available}
#'     \item{vcount}{Number of nodes}
#'   }
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
#'   comm <- communities(g)
#'   print(comm)
#'
#'   # Leiden with higher resolution
#'   comm2 <- communities(g, method = "leiden", resolution = 1.5)
#'
#'   # Compare modularity
#'   cat("Louvain modularity:", modularity(comm), "\n")
#'   cat("Leiden modularity:", modularity(comm2), "\n")
#' }
communities <- function(x,
                        method = c("louvain", "leiden", "fast_greedy",
                                   "walktrap", "infomap", "label_propagation",
                                   "edge_betweenness", "leading_eigenvector",
                                   "spinglass", "optimal", "fluid"),
                        weights = NULL,
                        resolution = 1,
                        directed = NULL,
                        ...) {

  method <- match.arg(method)

  # Dispatch to specific function
  switch(method,
    "louvain" = community_louvain(x, weights = weights, resolution = resolution, ...),
    "leiden" = community_leiden(x, weights = weights, resolution = resolution, ...),
    "fast_greedy" = community_fast_greedy(x, weights = weights, ...),
    "walktrap" = community_walktrap(x, weights = weights, ...),
    "infomap" = community_infomap(x, weights = weights, ...),
    "label_propagation" = community_label_propagation(x, weights = weights, ...),
    "edge_betweenness" = community_edge_betweenness(x, weights = weights,
                                                     directed = directed, ...),
    "leading_eigenvector" = community_leading_eigenvector(x, weights = weights, ...),
    "spinglass" = community_spinglass(x, weights = weights, ...),
    "optimal" = community_optimal(x, weights = weights, ...),
    "fluid" = community_fluid(x, ...)
  )
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
#' }
community_louvain <- function(x, weights = NULL, resolution = 1, ...) {
  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights)

  result <- igraph::cluster_louvain(g, weights = w, resolution = resolution)
  .wrap_communities(result, "louvain", g)
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
                             ...) {

  objective_function <- match.arg(objective_function)
  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights)

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
  .wrap_communities(result, "leiden", g)
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
community_fast_greedy <- function(x,
                                  weights = NULL,
                                  merges = TRUE,
                                  modularity = TRUE,
                                  membership = TRUE,
                                  ...) {

  g <- to_igraph(x, ...)

  # Fast greedy requires undirected

  if (igraph::is_directed(g)) {
    g <- igraph::as.undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum"))
  }

  w <- .resolve_weights(g, weights)

  result <- igraph::cluster_fast_greedy(
    g,
    weights = w,
    merges = merges,
    modularity = modularity,
    membership = membership
  )
  .wrap_communities(result, "fast_greedy", g)
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
  w <- .resolve_weights(g, weights)

  result <- igraph::cluster_walktrap(
    g,
    weights = w,
    steps = steps,
    merges = merges,
    modularity = modularity,
    membership = membership
  )
  .wrap_communities(result, "walktrap", g)
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
                              ...) {

  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights)

  result <- igraph::cluster_infomap(
    g,
    e.weights = w,
    v.weights = v.weights,
    nb.trials = nb.trials,
    modularity = modularity
  )
  .wrap_communities(result, "infomap", g)
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
                                 ...) {

  mode <- match.arg(mode)
  g <- to_igraph(x, ...)
  w <- .resolve_weights(g, weights)

  result <- igraph::cluster_label_prop(
    g,
    weights = w,
    mode = mode,
    initial = initial,
    fixed = fixed
  )
  .wrap_communities(result, "label_propagation", g)
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
  w <- .resolve_weights(g, weights)

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
  .wrap_communities(result, "edge_betweenness", g)
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
    g <- igraph::as.undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum"))
  }

  w <- .resolve_weights(g, weights)

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
  .wrap_communities(result, "leading_eigenvector", g)
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
                                ...) {

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
  .wrap_communities(result, "spinglass", g)
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
community_optimal <- function(x, weights = NULL, ...) {

  g <- to_igraph(x, ...)

  if (igraph::vcount(g) > 50) {
    warning("Optimal modularity is very slow for >50 nodes. Consider louvain/leiden.",
            call. = FALSE)
  }

  w <- .resolve_weights(g, weights)

  result <- igraph::cluster_optimal(g, weights = w)
  .wrap_communities(result, "optimal", g)
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
    g <- igraph::as.undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum"))
  }
  if (!igraph::is_connected(g)) {
    warning("Fluid communities requires connected graph. Using largest component.",
            call. = FALSE)
    comp <- igraph::components(g)
    g <- igraph::induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  }

  result <- igraph::cluster_fluid_communities(g, no.of.communities = no.of.communities)
  .wrap_communities(result, "fluid", g)
}


# ==============================================================================
# Short Aliases
# ==============================================================================

#' @rdname community_label_propagation
#' @export
community_label_prop <- community_label_propagation

#' @rdname community_leading_eigenvector
#' @export
community_leading_eigen <- community_leading_eigenvector

#' @rdname community_edge_betweenness
#' @export
community_edge_btwn <- community_edge_betweenness

#' @rdname community_fast_greedy
#' @export
community_fastgreedy <- community_fast_greedy

#' @rdname community_spinglass
#' @export
community_spin <- community_spinglass

#' @rdname community_walktrap
#' @export
community_walk <- community_walktrap

#' @rdname community_infomap
#' @export
community_info <- community_infomap


# ==============================================================================
# Helper Functions
# ==============================================================================

#' Resolve edge weights
#' @keywords internal
.resolve_weights <- function(g, weights) {
  if (is.null(weights)) {
    # Use network weights if available
    if ("weight" %in% igraph::edge_attr_names(g)) {
      return(igraph::E(g)$weight)
    }
    return(NULL)
  }
  if (length(weights) == 1 && is.na(weights)) {
    # Explicitly unweighted
    return(NULL)
  }
  weights
}


#' Wrap igraph communities result
#' @keywords internal
.wrap_communities <- function(result, algorithm, g) {
  # Add algorithm info
  result$algorithm <- algorithm

  # Add node names if available
  if (igraph::is_named(g)) {
    result$names <- igraph::V(g)$name
  }

  # Add class
  class(result) <- c("cograph_communities", class(result))
  result
}


# ==============================================================================
# Methods
# ==============================================================================

#' @export
print.cograph_communities <- function(x, ...) {
  cat("Community structure (", x$algorithm, ")\n", sep = "")
  cat("  Number of communities:", length(unique(x$membership)), "\n")
  cat("  Modularity:", round(igraph::modularity(x), 4), "\n")

  sizes <- table(x$membership)
  cat("  Community sizes:", paste(sizes, collapse = ", "), "\n")

  if (!is.null(x$names)) {
    cat("  Nodes:", length(x$names), "\n")
  }

  invisible(x)
}


#' Get Community Membership
#'
#' @param x A cograph_communities object
#' @return Named integer vector of community assignments
#' @export
membership.cograph_communities <- function(x) {
  m <- x$membership
  if (!is.null(x$names)) {
    names(m) <- x$names
  }
  m
}


#' Get Number of Communities
#'
#' @param x A cograph_communities object
#' @return Integer count of communities
#' @export
n_communities <- function(x) {
  length(unique(x$membership))
}


#' Get Community Sizes
#'
#' @param x A cograph_communities object
#' @return Named integer vector of community sizes
#' @export
community_sizes <- function(x) {
  as.integer(table(x$membership))
}


#' Get Modularity Score
#'
#' @param x A cograph_communities object
#' @param graph Optional igraph object for recalculation
#' @param ... Additional arguments
#' @return Numeric modularity value
#' @export
modularity.cograph_communities <- function(x, graph = NULL, ...) {
  # Try stored modularity first
  if (!is.null(x$modularity)) {
    if (is.numeric(x$modularity) && length(x$modularity) > 0) {
      return(max(x$modularity))
    }
  }

 # Fallback: calculate from membership if we have algorithm info
  tryCatch({
    NextMethod()
  }, error = function(e) {
    NA_real_
  })
}


#' Compare Community Structures
#'
#' Compares two community structures using various similarity measures.
#'
#' @param comm1 First community structure
#' @param comm2 Second community structure
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
plot.cograph_communities <- function(x, network = NULL, ...) {
  if (is.null(network)) {
    stop("network argument required for plotting", call. = FALSE)
  }

  # Get membership as groups
  membership <- x$membership

  # Call splot with community coloring
  splot(network, node_group = membership, ...)
}
