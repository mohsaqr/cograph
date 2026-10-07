# Community Detection Functions
# Wrapper functions for igraph community detection algorithms with full parameter exposure
# igraph is Suggests, so there is no @importFrom here. Call sites use
# `igraph::` directly and are reached through `to_igraph()`, which raises a
# classed `cograph_missing_suggest` via `.need_igraph()` when igraph is
# absent. A function here that reaches igraph WITHOUT going through
# `to_igraph()` must call `.need_igraph()` itself.

# ==============================================================================
# Main Function
# ==============================================================================

#' Community Detection
#'
#' @description
#' Detects communities in a network with one of the community detection
#' algorithms of igraph. Each method calls the matching
#' \code{community_*()} function.
#'
#' @param x Network input: matrix, igraph, network, CographNetwork,
#'   cograph_network, or tna object.
#' @param method Community detection algorithm. One of \code{"louvain"}
#'   (default; Louvain modularity optimization), \code{"leiden"} (Leiden
#'   algorithm), \code{"fast_greedy"} (greedy modularity optimization),
#'   \code{"walktrap"} (random walks), \code{"infomap"} (map equation),
#'   \code{"label_propagation"} (label propagation),
#'   \code{"edge_betweenness"} (Girvan-Newman), \code{"leading_eigenvector"}
#'   (leading eigenvector of the modularity matrix), \code{"spinglass"}
#'   (spinglass model), \code{"optimal"} (exact modularity maximization) or
#'   \code{"fluid"} (fluid communities).
#' @param community Optional integer or character vector. If supplied, the
#'   returned data frame is filtered to rows whose `community` column
#'   matches one of the given values. Default `NULL` (keep all communities).
#' @param weights Edge weights. \code{NULL} (default) uses the edge weights
#'   of the network when present and otherwise runs unweighted. \code{NA}
#'   runs unweighted.
#' @param resolution Resolution parameter of the louvain and leiden methods.
#'   For louvain, higher values yield more communities. Default 1.
#' @param directed Logical. Whether the edge betweenness method treats the
#'   network as directed. \code{NULL} (default) uses the direction of the
#'   network. The other methods ignore it.
#' @param seed Random seed for reproducibility. It applies to the stochastic
#'   methods (louvain, leiden, infomap, label_propagation, spinglass).
#' @param ... Additional arguments passed to the \code{community_*()}
#'   function of the chosen method, for example \code{no.of.communities} for
#'   \code{"fluid"}.
#'
#' @return A \code{cograph_communities} data frame with one row per node and
#'   the columns
#'   \describe{
#'     \item{node}{Node label (character).}
#'     \item{community}{Community number (numeric).}
#'   }
#'   The attributes \code{"algorithm"} (method name), \code{"modularity"}
#'   (modularity of the partition, \code{NA} when igraph does not compute
#'   it), \code{"network"} (the input \code{x}) and \code{"igraph_result"}
#'   (the igraph \code{communities} object) hold the metadata. When
#'   \code{community} is supplied, only the matching rows are kept.
#'
#' @details
#' The louvain, leiden, fast_greedy, leading_eigenvector and fluid methods
#' require an undirected graph. For a directed input this function prints a
#' message and runs \code{"walktrap"} instead. Called directly,
#' \code{community_louvain()} and \code{community_leiden()} raise an igraph
#' error on a directed graph, while \code{community_fast_greedy()},
#' \code{community_leading_eigenvector()} and \code{community_fluid()}
#' collapse it to an undirected graph with summed weights.
#'
#' Negative edge weights are replaced by their absolute values for all
#' methods except spinglass and optimal, which receive the weights as they
#' are.
#'
#' \tabular{ll}{
#'   Method \tab Typical use \cr
#'   louvain \tab Large undirected networks \cr
#'   leiden \tab Large undirected networks, well-connected communities \cr
#'   fast_greedy \tab Medium-sized networks, hierarchical merges \cr
#'   walktrap \tab Directed or undirected networks, hierarchical merges \cr
#'   infomap \tab Directed networks with flow structure \cr
#'   label_propagation \tab Very large networks \cr
#'   edge_betweenness \tab Small networks, hierarchical splits \cr
#'   leading_eigenvector \tab Undirected networks, hierarchical splits \cr
#'   spinglass \tab Small connected networks, negative weights \cr
#'   optimal \tab Networks of at most about 50 nodes \cr
#'   fluid \tab Connected networks with a known number of communities \cr
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
#' @section Printing and plotting:
#' Printing the result shows the algorithm, the number of nodes and
#' communities, the modularity, the community sizes and the node table. The
#' result is itself a data frame.
#' \code{plot()} on the result is documented in \code{\link{plot-results}}.
#'
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' communities(regulation_net, method = "walktrap")
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
#' Multi-level modularity optimization with the Louvain algorithm. The graph
#' must be undirected; a directed graph raises an igraph error, so a directed
#' network is converted first, for example with \code{\link{to_undirected}()}.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Negative weights are replaced by their
#'   absolute values.
#' @param resolution Resolution parameter. Higher values yield more
#'   communities. Default 1 (standard modularity).
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Blondel, V.D., Guillaume, J.L., Lambiotte, R., & Lefebvre, E. (2008).
#' Fast unfolding of communities in large networks.
#' \emph{Journal of Statistical Mechanics}, P10008.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_louvain(to_undirected(regulation_net), seed = 1)
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
#' The Leiden algorithm, a refinement of the Louvain algorithm that
#' guarantees well-connected communities. It optimizes the Constant Potts
#' Model (CPM) or modularity. The graph must be undirected; a directed graph
#' raises an igraph error.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Negative weights are replaced by their
#'   absolute values.
#' @param resolution Resolution parameter. Default 1. With the default CPM
#'   objective and resolution 1, a network with weights below 1 is typically
#'   split into single-node communities.
#' @param objective_function Optimization objective, \code{"CPM"} (default)
#'   or \code{"modularity"}.
#' @param beta Randomness parameter of the refinement step. Default 0.01.
#' @param initial_membership Initial community assignments (optional).
#' @param n_iterations Number of iterations. Default 2. A negative value
#'   iterates until the partition no longer changes.
#' @param vertex_weights Vertex weights for the CPM objective.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Traag, V.A., Waltman, L., & van Eck, N.J. (2019).
#' From Louvain to Leiden: guaranteeing well-connected communities.
#' \emph{Scientific Reports}, 9, 5233.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_leiden(to_undirected(regulation_net), objective_function = "modularity",
#'                  seed = 1)
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
#' Hierarchical agglomeration by greedy modularity optimization, which
#' produces a dendrogram of community merges. A directed graph is collapsed
#' to an undirected graph with summed edge weights.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Negative weights are replaced by their
#'   absolute values.
#' @param merges Logical. Whether igraph stores the merge matrix. Default
#'   \code{TRUE}.
#' @param modularity Logical. Whether igraph stores the modularity scores.
#'   Default \code{TRUE}.
#' @param membership Logical. Whether igraph computes the membership vector.
#'   Default \code{TRUE}.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. The igraph \code{communities} result,
#'   including the merge dendrogram when \code{merges = TRUE}, is kept in the
#'   \code{"igraph_result"} attribute.
#'
#' @references
#' Clauset, A., Newman, M.E.J., & Moore, C. (2004).
#' Finding community structure in very large networks.
#' \emph{Physical Review E}, 70, 066111.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_fast_greedy(regulation_net)
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
#' Detects communities from short random walks. Nodes in the same community
#' have short random walk distances.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Negative weights are replaced by their
#'   absolute values.
#' @param steps Length of the random walks. Default 4.
#' @param merges Logical. Whether igraph stores the merge matrix. Default
#'   \code{TRUE}.
#' @param modularity Logical. Whether igraph stores the modularity scores.
#'   Default \code{TRUE}.
#' @param membership Logical. Whether igraph computes the membership vector.
#'   Default \code{TRUE}.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Pons, P., & Latapy, M. (2006).
#' Computing communities in large networks using random walks.
#' \emph{Journal of Graph Algorithms and Applications}, 10(2), 191-218.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_walktrap(regulation_net, steps = 4)
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
#' Information-theoretic community detection based on random walks. The
#' partition minimizes the map equation, the description length of a random
#' walk on the network.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Negative weights are replaced by their
#'   absolute values.
#' @param v.weights Vertex weights (teleportation weights).
#' @param nb.trials Number of optimization trials. Default 10.
#' @param modularity Logical. Whether modularity is computed. Default
#'   \code{TRUE}.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Rosvall, M., & Bergstrom, C.T. (2008).
#' Maps of random walks on complex networks reveal community structure.
#' \emph{PNAS}, 105(4), 1118-1123.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_infomap(regulation_net, nb.trials = 10, seed = 1)
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
#' Label propagation community detection. Each node repeatedly adopts the
#' most frequent label among its neighbors.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Negative weights are replaced by their
#'   absolute values.
#' @param mode Direction of label propagation in directed graphs, one of
#'   \code{"out"} (default), \code{"in"} or \code{"all"}.
#' @param initial Initial labels, an integer vector, or \code{NULL} for a
#'   unique label per node.
#' @param fixed Logical vector marking the nodes whose labels are fixed.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Raghavan, U.N., Albert, R., & Kumara, S. (2007).
#' Near linear time algorithm to detect community structures in large-scale networks.
#' \emph{Physical Review E}, 76, 036106.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_label_propagation(regulation_net, seed = 1)
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
#' The Girvan-Newman algorithm. It repeatedly removes the edge with the
#' highest edge betweenness and keeps the partition with the highest
#' modularity. On a weighted graph igraph warns that the membership is
#' selected by modularity.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Negative weights are replaced by their
#'   absolute values.
#' @param directed Logical. Whether edge directions are used. Default
#'   \code{TRUE}. \code{NULL} uses the direction of the network.
#' @param edge.betweenness Logical. Whether igraph stores the edge
#'   betweenness values. Default \code{TRUE}.
#' @param merges Logical. Whether igraph stores the merge matrix. Default
#'   \code{TRUE}.
#' @param bridges Logical. Whether igraph stores the bridge edges. Default
#'   \code{TRUE}.
#' @param modularity Logical. Whether igraph stores the modularity scores.
#'   Default \code{TRUE}.
#' @param membership Logical. Whether igraph computes the membership vector.
#'   Default \code{TRUE}.
#' @param ... Passed to \code{\link{to_igraph}}. Its only other argument,
#'   \code{directed}, is already taken by this function, so any further
#'   argument raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Girvan, M., & Newman, M.E.J. (2002).
#' Community structure in social and biological networks.
#' \emph{PNAS}, 99(12), 7821-7826.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_edge_betweenness(igraph::make_graph("Zachary"))
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
#' Detects communities with the leading eigenvector of the modularity
#' matrix, splitting the network divisively. A directed graph is collapsed to
#' an undirected graph with summed edge weights.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Negative weights are replaced by their
#'   absolute values.
#' @param steps Maximum number of split attempts. Default -1 (no limit).
#' @param start Starting community structure (membership vector).
#' @param options ARPACK options list. Default
#'   \code{igraph::arpack_defaults()}.
#' @param callback Optional function called after each split.
#' @param extra Extra argument passed to \code{callback}.
#' @param env Environment in which \code{callback} is evaluated.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Newman, M.E.J. (2006).
#' Finding community structure using the eigenvectors of matrices.
#' \emph{Physical Review E}, 74, 036104.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_leading_eigenvector(regulation_net)
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
#' Community detection based on the spinglass model of statistical
#' mechanics, optimized by simulated annealing. Negative edge weights are
#' supported with \code{implementation = "neg"}. A disconnected graph raises
#' a warning and only its largest component is partitioned, so the result
#' has one row per node of that component.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Weights are passed unchanged.
#' @param vertex Vertex whose community is searched (single community mode).
#'   \code{NULL} (default) partitions the whole network.
#' @param spins Number of spins, the upper limit on the number of
#'   communities. Default 25.
#' @param parupdate Logical. Whether spins are updated in parallel. Default
#'   \code{FALSE}.
#' @param start.temp Starting temperature. Default 1.
#' @param stop.temp Stopping temperature. Default 0.01.
#' @param cool.fact Cooling factor. Default 0.99.
#' @param update.rule Null model of the update rule, one of \code{"config"}
#'   (default), \code{"random"} or \code{"simple"}.
#' @param gamma Weight of the null model term. Default 1.
#' @param implementation \code{"orig"} (default) or \code{"neg"}, the
#'   implementation that supports negative weights.
#' @param gamma.minus Weight of the null model term for negative edges in
#'   the \code{"neg"} implementation. Default 1.
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Reichardt, J., & Bornholdt, S. (2006).
#' Statistical mechanics of community detection.
#' \emph{Physical Review E}, 74, 016110.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_spinglass(regulation_net, seed = 1)
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
#' Finds the partition with maximum modularity by exact optimization.
#' Exact modularity maximization is NP-hard, so the computation is feasible
#' only for small networks. A network with more than 50 nodes raises a
#' warning.
#'
#' @param x Network input.
#' @param weights Edge weights. \code{NULL} uses the network weights and
#'   \code{NA} runs unweighted. Weights are passed unchanged.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Brandes, U., Delling, D., Gaertler, M., Gorke, R., Hoefer, M.,
#' Nikoloski, Z., & Wagner, D. (2008).
#' On modularity clustering.
#' \emph{IEEE Transactions on Knowledge and Data Engineering}, 20(2), 172-188.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_optimal(regulation_net)
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
#' Fluid communities algorithm, in which a fixed number of communities
#' expand and compete for nodes. A directed graph is collapsed to an
#' undirected graph. A disconnected graph raises a warning and only its
#' largest component is partitioned, so the result has one row per node of
#' that component. Edge weights are not used.
#'
#' @param x Network input.
#' @param no.of.communities Number of communities to detect. Required; a
#'   missing value raises an error.
#' @param ... Passed to \code{\link{to_igraph}}, whose only other argument
#'   is \code{directed}; anything else raises an "unused argument" error.
#'
#' @return A \code{cograph_communities} data frame with columns
#'   \code{node} and \code{community}. See \code{\link{communities}} for its
#'   attributes.
#'
#' @references
#' Pares, F., Gasulla, D.G., Vilalta, A., Moreno, J., Ayguade, E.,
#' Labarta, J., Cortes, U., & Suzumura, T. (2018).
#' Fluid communities: A competitive, scalable and diverse community detection algorithm.
#' \emph{Studies in Computational Intelligence}, 689, 229-240.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_fluid(regulation_net, no.of.communities = 2)
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
#' Runs a stochastic community detection algorithm repeatedly and derives
#' consensus communities by thresholding the co-occurrence matrix of the
#' runs.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object. The louvain and leiden methods require an undirected network.
#' @param method Community detection algorithm, one of \code{"louvain"}
#'   (default), \code{"leiden"}, \code{"infomap"},
#'   \code{"label_propagation"} or \code{"spinglass"}. The current code runs
#'   louvain for \code{"spinglass"}.
#' @param n_runs Number of runs. Default 100.
#' @param threshold Co-occurrence threshold. Default 0.5. Pairs of nodes that
#'   share a community in at least this proportion of runs are linked in the
#'   consensus graph.
#' @param seed Optional seed for reproducibility. If provided, the RNG state is
#'   initialized once before repeated runs and restored on exit.
#' @param ... Ignored. Each run calls the igraph function with its own
#'   defaults and without weights or resolution settings. For leiden this is
#'   the CPM objective with resolution 1.
#'
#' @return A \code{cograph_communities} data frame (columns \code{node} and
#'   \code{community}) holding the consensus membership. Its
#'   \code{"algorithm"} attribute is \code{"consensus_<method>"} and its
#'   \code{"modularity"} attribute is the modularity of the final walktrap
#'   partition computed on the consensus graph.
#'
#' @details
#' The algorithm is run \code{n_runs} times on the current random number
#' stream. The proportion of runs in which each pair of nodes shares a
#' community forms the co-occurrence matrix. Pairs with a proportion of at
#' least \code{threshold} are linked in an unweighted consensus graph, and
#' walktrap on that graph gives the final communities.
#'
#' @references
#' Lancichinetti, A., & Fortunato, S. (2012).
#' Consensus clustering in complex networks.
#' \emph{Scientific Reports}, 2, 336.
#'
#' @export
#' @seealso \code{\link{communities}}, \code{\link{community_louvain}}
#'
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' community_consensus(to_undirected(regulation_net), method = "louvain",
#'                     n_runs = 10, seed = 1)
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
#' @export
com_lv <- community_louvain

#' @rdname community_leiden
#' @export
com_ld <- community_leiden

#' @rdname community_fast_greedy
#' @export
com_fg <- community_fast_greedy

#' @rdname community_walktrap
#' @export
com_wt <- community_walktrap

#' @rdname community_infomap
#' @export
com_im <- community_infomap

#' @rdname community_label_propagation
#' @export
com_lp <- community_label_propagation

#' @rdname community_edge_betweenness
#' @export
com_eb <- community_edge_betweenness

#' @rdname community_leading_eigenvector
#' @export
com_le <- community_leading_eigenvector

#' @rdname community_spinglass
#' @export
com_sg <- community_spinglass

#' @rdname community_optimal
#' @export
com_op <- community_optimal

#' @rdname community_fluid
#' @export
com_fl <- community_fluid


# ==============================================================================
# Helper Functions
# ==============================================================================

#' Resolve edge weights
#' @keywords internal
#' @noRd
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
#' @noRd
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

#' @noRd
#' @export
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
#' Extracts a named membership vector from a \code{cograph_communities}
#' data frame or an igraph \code{communities} object.
#'
#' @param x A \code{cograph_communities} or igraph \code{communities}
#'   object.
#' @return A numeric vector of community numbers named by node. For an igraph
#'   object it is the igraph \code{membership} vector.
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' comm <- community_walktrap(regulation_net)
#' membership(comm)
membership <- function(x) {
  if (inherits(x, "cograph_communities")) {
    m <- x$community
    names(m) <- x$node
    return(m)
  }
  # Fallback for igraph community objects. Reached by any input that is not a
  # cograph_communities, so it must announce the missing Suggests itself.
  .need_igraph("membership()")
  igraph::membership(x)
}


#' Get Number of Communities
#'
#' @param x A \code{cograph_communities} object.
#' @return A single integer, the number of distinct communities.
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' comm <- community_walktrap(regulation_net)
#' n_communities(comm)
n_communities <- function(x) {
  length(unique(x$community))
}


#' Get Community Sizes
#'
#' @param x A \code{cograph_communities} object.
#' @return An unnamed integer vector of community sizes, ordered by
#'   community number.
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' comm <- community_walktrap(regulation_net)
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
#' Compares two partitions of the same nodes with igraph's partition
#' comparison measures.
#'
#' @param comm1,comm2 Partitions to compare. Each is a
#'   \code{cograph_communities} data frame, an igraph \code{communities}
#'   object or a membership vector.
#' @param method Comparison measure, one of \code{"vi"} (default; variation
#'   of information), \code{"nmi"} (normalized mutual information),
#'   \code{"split.join"} (split-join distance), \code{"rand"} (Rand index) or
#'   \code{"adjusted.rand"} (adjusted Rand index).
#' @return A single numeric value. \code{"vi"} and \code{"split.join"} are
#'   distances (0 for identical partitions); \code{"nmi"}, \code{"rand"} and
#'   \code{"adjusted.rand"} are similarities (1 for identical partitions).
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' walktrap <- community_walktrap(regulation_net)
#' fast_greedy <- community_fast_greedy(regulation_net)
#' compare_communities(walktrap, fast_greedy, method = "nmi")
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


#' Plot Analysis Results
#'
#' @description
#' Plot methods and plotting functions for the result objects returned by the
#' analysis functions of cograph and by the tna and Nestimate packages. Each
#' function accepts one class of result.
#' \describe{
#'   \item{\code{plot()} or \code{plot_motifs()} on a \code{cograph_motif_result}}{From
#'     \code{\link{motifs}()} or \code{\link{subgraphs}()}. Plots triad
#'     diagrams, MAN type frequencies, z-scores or MAN pattern diagrams.}
#'   \item{\code{plot()} on a \code{cograph_motif_analysis}}{From
#'     \code{\link{extract_motifs}()}. Plots the same four views as above.}
#'   \item{\code{plot()} on a \code{cograph_motifs}}{From
#'     \code{\link{motif_census}()}. Plots motif z-scores colored by
#'     significance direction, a z-score heatmap or diagrams of the selected
#'     motifs.}
#'   \item{\code{plot()} on a \code{cograph_communities}}{From
#'     \code{\link{communities}()}. Plots the network with nodes grouped by
#'     community through \code{\link{splot}()}.}
#'   \item{\code{plot()} on a \code{cograph_core_periphery}}{From
#'     \code{\link{core_periphery}()}. Plots the network with core nodes
#'     enlarged and periphery nodes reduced.}
#'   \item{\code{plot()} on a \code{cograph_rich_club}}{From
#'     \code{\link{rich_club}()}. Plots the rich club curve with the null
#'     model band, or the club members on the network.}
#'   \item{\code{plot()} on a \code{cograph_degree_fit}}{From
#'     \code{\link{fit_degree_distribution}()}. Plots a histogram of the
#'     observed degrees with the fitted distribution curves overlaid.}
#'   \item{\code{plot()} on a \code{cograph_vulnerability}}{From
#'     \code{\link{vulnerability}()}. Plots the node vulnerability scores as a
#'     bar chart.}
#'   \item{\code{plot()} and \code{splot()} on a \code{tna_disparity}}{From
#'     \code{\link{disparity_filter}()}. \code{plot()} plots the backbone or
#'     the original and backbone networks side by side. \code{splot()} plots
#'     the full network with backbone edges solid and the remaining edges
#'     dashed and faded.}
#'   \item{\code{splot()} on a \code{tna_bootstrap}}{From
#'     \code{tna::bootstrap()}. Plots the network with significant and
#'     non-significant edges styled differently.}
#'   \item{\code{plot_permutation()} or \code{splot()} on a
#'     \code{tna_permutation}}{From \code{tna::permutation_test()}. Plots the
#'     edge differences between two networks, colored by sign and styled by
#'     significance.}
#'   \item{\code{plot_group_permutation()} or \code{splot()} on a
#'     \code{group_tna_permutation}}{From \code{tna::permutation_test()} on a
#'     \code{group_tna} model. Plots one panel per pairwise comparison.}
#'   \item{\code{plot_netobject_group()} or \code{plot()} on a
#'     \code{netobject_group}}{A named list of Nestimate networks. Plots one
#'     panel per group.}
#'   \item{\code{plot_net_bootstrap_group()} or \code{plot()} on a
#'     \code{net_bootstrap_group}}{A list of Nestimate \code{net_bootstrap}
#'     results. Plots one panel per group with significance styling.}
#'   \item{\code{plot_netobject_ml()} or \code{plot()} on a
#'     \code{netobject_ml}}{A multilevel Nestimate network. Plots the
#'     between-person and within-person networks side by side.}
#'   \item{\code{plot_net_stability()} on a \code{net_stability}}{From
#'     \code{Nestimate::centrality_stability()}. Plots the mean correlation of
#'     each centrality measure with the original against the proportion of
#'     cases dropped.}
#' }
#'
#' @param x The result object. The Description lists the class each function
#'   accepts.
#' @param type Plot type. The values for each class are listed in Details.
#' @param n Maximum number of triads, patterns or z-score bars plotted. For
#'   \code{cograph_motif_analysis} with \code{type = "significance"}, the
#'   \code{n} lowest and \code{n} highest z-scores are plotted.
#' @param ncol,nrow Number of columns and rows of the panel grid. A
#'   \code{NULL} value is computed from the number of panels.
#' @param colors Colors of the significance scale in motif plots. For
#'   \code{cograph_motif_result} and \code{cograph_motif_analysis}, a vector
#'   of two colors. The first fills items that are significantly
#'   under-represented (\code{p < .05} and \code{z < 0}) and the second fills
#'   items that are significantly over-represented (\code{p < .05} and
#'   \code{z > 0}). All other items are filled neutral grey
#'   (\code{"#9E9E9E"}). When no per-type significance is available, the
#'   first color is used as a single fill. For \code{cograph_motifs}, a vector of three
#'   colors for under-represented, neutral and over-represented motifs.
#' @param node_size Relative size of the nodes in triad diagrams.
#' @param label_size Font size of the node labels in triad diagrams.
#' @param title_size Font size of the panel titles in triad diagrams.
#' @param stats_size Font size of the statistics caption of each triad panel
#'   (for example \code{n=34 z=-55.3 p<.001}).
#' @param legend_size Font size of the legend below the triad grid.
#' @param legend Logical. Whether to show the legend of node-label
#'   abbreviations below the triad grid.
#' @param motif_color,color Color of the nodes, edges and labels in triad
#'   diagrams. \code{motif_color} applies to \code{cograph_motif_result} and
#'   \code{color} to \code{cograph_motif_analysis}.
#' @param spacing Spacing multiplier for triad diagrams. Values above 1 pull
#'   the three nodes of each panel inward and values below 1 push them
#'   apart.
#' @param base_size Base font size of the ggplot2 theme used by
#'   \code{type = "types"} and \code{type = "significance"}.
#' @param res Unused. It is kept for backward compatibility.
#' @param combined Logical. When \code{TRUE} (default), a multi-panel plot is
#'   arranged in an internal grid through \code{graphics::par(mfrow = ...)}.
#'   When \code{FALSE}, the panels are plotted into a layout the caller has
#'   already set up, for example with \code{\link{panel_layout}()}. It applies
#'   to \code{type = "network"} for \code{cograph_motifs}, to
#'   \code{type = "patterns"} (and \code{type = "triads"} on census results)
#'   for the other motif results, to \code{type = "comparison"} for
#'   \code{tna_disparity}, to \code{plot_group_permutation()} when \code{i} is
#'   \code{NULL}, and to the group and multilevel panel functions.
#' @param show_nonsig Logical. Whether non-significant items are shown. For
#'   \code{cograph_motifs} these are motifs; for \code{plot_permutation()}
#'   they are edges, plotted dashed and grey. Default \code{FALSE}.
#' @param top_n Number of motifs with the largest absolute z-scores to plot.
#'   \code{NULL} (default) plots all.
#' @param k Prominence threshold whose club members are highlighted with
#'   \code{type = "network"} for \code{cograph_rich_club}. \code{NULL} uses
#'   the threshold with the highest \code{phi_norm} (or \code{phi} when the
#'   result is not normalized).
#' @param col Color of the rich club curve and club members, or of the
#'   vulnerability bars.
#' @param core_color,periphery_color Node colors of core and periphery nodes.
#' @param core_size,periphery_size Node sizes of core and periphery nodes.
#' @param which Character vector of fitted distributions to show. \code{NULL}
#'   (default) shows all fitted distributions.
#' @param log Log-scale axes for the degree fit, one of \code{""} (default),
#'   \code{"x"}, \code{"y"} or \code{"xy"}. Only \code{"y"} and \code{"xy"}
#'   set a logarithmic histogram axis. The values containing \code{"x"} only
#'   remove non-positive fitted curve values.
#' @param cols Colors of the fitted distribution curves, named or unnamed.
#'   \code{NULL} uses a built-in palette.
#' @param lwd Line width of the fitted distribution curves.
#' @param main Title of the degree-fit plot.
#' @param top Number of most vulnerable nodes to plot. \code{NULL} (default)
#'   plots all.
#' @param network The network the communities were detected on. It is
#'   required only when the result does not store the network.
#' @param show Network shown by \code{splot()} on a \code{tna_disparity}.
#'   \code{"styled"} (default) shows the full network with backbone styling,
#'   \code{"backbone"} the backbone only and \code{"full"} the full network
#'   without styling.
#' @param display Display mode of \code{splot()} on a \code{tna_bootstrap}.
#'   \code{"styled"} (default) shows all edges with significance styling,
#'   \code{"significant"} the significant edges only, \code{"full"} all
#'   edges without significance styling and \code{"ci"} all edges with
#'   confidence interval bounds in the labels and an underlay whose width
#'   reflects the interval width relative to the edge weight.
#' @param edge_style_sig Line type of significant (or backbone) edges.
#'   Default 1 (solid).
#' @param edge_style_nonsig Line type of non-significant (or non-backbone)
#'   edges. Default 2 (dashed).
#' @param alpha_nonsig Transparency of non-backbone edges. Default 0.3.
#' @param color_nonsig Accepted for compatibility. The styled bootstrap plot
#'   uses a fixed pink color for non-significant edges.
#' @param show_ci Logical. Whether confidence interval bounds are added to the
#'   edge labels. \code{display = "ci"} adds them as well.
#' @param show_stars Logical. Whether significance stars (\code{*},
#'   \code{**}, \code{***}) are added to the edge labels.
#' @param width_by Set to \code{"cr_lower"} to plot the lower bounds of the
#'   consistency range as the edge weights, with widths scaled by these
#'   bounds and the significance styling removed. \code{NULL} (default)
#'   leaves the edges unchanged.
#' @param inherit_style Logical. Whether the labels, node colors and
#'   initial-state donuts of the original TNA model are reused, with the
#'   oval layout as the default layout.
#' @param edge_positive_color,edge_negative_color Colors of significant
#'   positive (\code{x > y}) and negative (\code{x < y}) edge differences.
#' @param edge_nonsig_color,edge_nonsig_style,edge_nonsig_alpha Color, line
#'   type and transparency of non-significant edge differences.
#' @param show_effect Logical. Whether the absolute effect size is added in
#'   parentheses to the labels of significant edges.
#' @param i Index or name of a single comparison to plot. \code{NULL}
#'   (default) plots all comparisons.
#' @param common_scale Logical. Whether all panels share the same maximum
#'   edge weight. Default \code{TRUE}.
#' @param title_prefix Optional text placed before each group name in the
#'   panel titles.
#' @param layout Layout algorithm of the multilevel panels. \code{NULL}
#'   (default) uses \code{"oval"}.
#' @param titles Character vector of length 2 with the titles of the
#'   between-person and within-person panels.
#' @param ... Additional arguments passed to the underlying plotting call.
#'   Network plots pass them to \code{\link{splot}()};
#'   \code{plot_group_permutation()} passes them to \code{plot_permutation()}
#'   and \code{plot_net_bootstrap_group()} to the \code{splot()} method for
#'   \code{net_bootstrap} (for example \code{display = "significant"}). The
#'   rich club curve passes them to \code{\link[graphics]{plot}}, the degree
#'   fit to \code{\link[graphics]{hist}}, the vulnerability plot to
#'   \code{\link[graphics]{barplot}}, \code{plot_net_stability()} to
#'   \code{\link[graphics]{plot}}, and \code{cograph_motifs} with
#'   \code{type = "network"} to the per-motif igraph plot calls. The ggplot2
#'   motif views do not use them.
#'
#' @details
#' \subsection{Plot types}{
#' For \code{cograph_motif_result} and \code{cograph_motif_analysis},
#' \code{type} is one of the following values.
#' \describe{
#'   \item{\code{"triads"}}{(default) Network diagrams of node triples
#'     arranged in a grid. A census result without named nodes falls back to
#'     \code{"patterns"}. Each diagram shows a canonical representative of the
#'     MAN class, so the node labels identify the participating nodes and
#'     their positions do not encode observed source or sink roles. Panel
#'     titles read \code{"<MAN code>: <description>"}, and the caption gives
#'     the count and, when significance was tested, the z-score and
#'     p-value.}
#'   \item{\code{"types"}}{Bar chart of MAN type frequencies. For a census
#'     tested for significance the bars are colored by significance
#'     direction. Instance results and \code{cograph_motif_analysis} use a
#'     single fill, because per-type significance would require aggregating
#'     several node-triple rows of the same type.}
#'   \item{\code{"significance"}}{Z-score bars, one per MAN type for a census
#'     and one per node triple for instance results. It requires the analysis
#'     to have been run with \code{significance = TRUE}.}
#'   \item{\code{"patterns"}}{Abstract MAN pattern diagrams of each triad
#'     type. For a census tested for significance the nodes are filled by
#'     significance direction, and the panel titles add the z-score and a
#'     significance star (\code{*} p<.05, \code{**} p<.01, \code{***}
#'     p<.001). Instance results use a single fill.}
#' }
#' For \code{cograph_motifs}, \code{type} is \code{"bar"} (default; motif
#' z-scores colored by over- or under-representation), \code{"heatmap"}
#' (z-scores across motif types, labelled with the observed and expected
#' counts) or \code{"network"} (one diagram per motif that passes the
#' \code{show_nonsig} and \code{top_n} filters). The network view requires
#' a directed 3-node census and otherwise falls back to the bar chart with a
#' message. For \code{cograph_rich_club}, \code{type} is
#' \code{"curve"} (default; the coefficient across thresholds with null model
#' bands) or \code{"network"} (club members at threshold \code{k}). For
#' \code{tna_disparity}, \code{type} is \code{"backbone"} (default) or
#' \code{"comparison"} (original and backbone side by side).
#' }
#'
#' \subsection{Bootstrap and permutation input}{
#' \code{splot()} on a \code{tna_bootstrap} reads the original weights from
#' \code{weights} (or \code{weights_orig}), the significant weights from
#' \code{weights_sig} or the \code{p_values} matrix, the confidence bounds
#' from \code{ci_lower} and \code{ci_upper}, the significance level from
#' a \code{level} element and the styling from \code{model}. Results of
#' \code{tna::bootstrap()} store no \code{level} element, so their edges
#' are styled at a level of 0.05. In styled
#' mode significant edges are solid dark blue with bold starred labels and
#' are plotted on top, and non-significant edges are dashed pink with plain
#' labels.
#'
#' \code{plot_permutation()} reads the edge differences (\code{x - y}) from
#' \code{edges$diffs_true}, the significant differences from
#' \code{edges$diffs_sig} and the edge statistics from \code{edges$stats}.
#' Significant positive differences are solid green and significant negative
#' differences solid red, both with bold starred labels.
#'
#' \code{plot_net_bootstrap_group()} plots each group through the
#' \code{splot()} method for \code{net_bootstrap}, so every panel keeps the
#' solid and dashed significance styling.
#' }
#'
#' @return Each function is called for its plot. The returned value depends
#'   on the class.
#'   \describe{
#'     \item{Motif results}{A ggplot2 object, printed and returned
#'       invisibly, for \code{type = "types"}, \code{"significance"},
#'       \code{"bar"} and \code{"heatmap"}. \code{type = "triads"} and
#'       \code{"patterns"} return the input invisibly for
#'       \code{cograph_motif_result} and \code{NULL} invisibly for
#'       \code{cograph_motif_analysis}. \code{cograph_motifs} with
#'       \code{type = "network"} returns \code{NULL} invisibly. Any
#'       \code{cograph_motifs} plot returns \code{NULL} invisibly with a
#'       message when no motif passes the \code{show_nonsig} and
#'       \code{top_n} filters.}
#'     \item{Network plots}{The \code{cograph_network} built by
#'       \code{\link{splot}()}, invisibly, for \code{cograph_communities},
#'       \code{tna_disparity}, \code{tna_bootstrap} and
#'       \code{tna_permutation}. \code{plot_permutation()} returns
#'       \code{NULL} invisibly with a message when no edge remains to plot.
#'       \code{plot_group_permutation()} returns the selected panel's
#'       network when \code{i} is given and \code{NULL} invisibly
#'       otherwise.}
#'     \item{Group panels}{The input invisibly for \code{netobject_group} and
#'       \code{net_bootstrap_group}. With a single group the network of that
#'       panel is returned, and with no groups \code{NULL}.}
#'     \item{Other results}{The input invisibly for
#'       \code{cograph_core_periphery}, \code{cograph_rich_club},
#'       \code{cograph_vulnerability}, \code{netobject_ml} and
#'       \code{net_stability}, and \code{NULL} invisibly for
#'       \code{cograph_degree_fit}.}
#'   }
#'
#' @seealso \code{\link{splot}()} for the network plots of single networks.
#'
#' @examples
#' census <- motifs(regulation_net, significance = FALSE)
#' plot(census, type = "types")
#'
#' @name plot-results
NULL

#' @rdname plot-results
#' @export
plot.cograph_communities <- function(x, network = NULL, ...) {
  network <- network %||% attr(x, "network")
  if (is.null(network)) {
    stop("No network found. Pass one via: plot(comm, network = m)", call. = FALSE)
  }

  mem <- x$community
  names(mem) <- x$node
  splot(network, node_group = mem, ...)
}
