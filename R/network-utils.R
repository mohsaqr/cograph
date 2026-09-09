# =============================================================================
# Network Utility Functions
# =============================================================================

#' Convert Network to igraph Object
#'
#' Converts various network representations to an igraph object. Supports
#' matrices, edge-list data frames, igraph objects, network objects,
#' cograph_network, and tna objects.
#'
#' @param x Network input. Can be:
#'   \itemize{
#'     \item A square numeric matrix (adjacency/weight matrix)
#'     \item A data frame edge list with source and target columns
#'     \item An igraph object (returned as-is or converted if directed differs)
#'     \item A statnet network object
#'     \item A cograph_network object
#'     \item A tna object
#'   }
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#'
#' @return An igraph object.
#'
#' @seealso \code{\link{to_data_frame}}, \code{\link{as_cograph}}
#'
#' @export
#' @examples
#' # From matrix
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' g <- to_igraph(adj)
#'
#' # Force directed
#' g_dir <- to_igraph(adj, directed = TRUE)
to_igraph <- function(x, directed = NULL) {
  if (inherits(x, "igraph")) {
    # If directed override specified and different from current, convert
    if (!is.null(directed)) {
      if (directed && !igraph::is_directed(x)) {
        x <- igraph::as.directed(x, mode = "mutual")
      } else if (!directed && igraph::is_directed(x)) {
        x <- igraph::as_undirected(x, mode = "collapse")
      }
    }
    return(x)
  }

  if (inherits(x, "cograph_network")) {
    g <- network_to_igraph(x)
    # Apply directed override if specified
    if (!is.null(directed)) {
      if (directed && !igraph::is_directed(g)) {
        g <- igraph::as.directed(g, mode = "mutual")
      } else if (!directed && igraph::is_directed(g)) {
        g <- igraph::as_undirected(g, mode = "collapse")
      }
    }
    return(g)
  }

  if (is.data.frame(x)) {
    parsed <- parse_edgelist(x, directed = directed)
    node_names <- parsed$nodes$name
    edge_df <- data.frame(
      from = node_names[parsed$edges$from],
      to = node_names[parsed$edges$to],
      weight = parsed$edges$weight,
      stringsAsFactors = FALSE
    )
    vertices <- data.frame(name = node_names, stringsAsFactors = FALSE)

    return(igraph::graph_from_data_frame(
      edge_df,
      directed = parsed$directed,
      vertices = vertices
    ))
  }

  if (inherits(x, "network")) {
    if (!requireNamespace("network", quietly = TRUE)) { # nocov start
      stop("Package 'network' is required for network input. ",
           "Please install it with: install.packages('network')",
           call. = FALSE)
    } # nocov end
    # Get directedness
    is_dir <- if (!is.null(directed)) {
      directed
    } else {
      network::is.directed(x)
    }
    graph_mode <- if (is_dir) "directed" else "undirected"

    # Convert to adjacency matrix, checking for weight attribute first
    edge_attrs <- network::list.edge.attributes(x)
    if ("weight" %in% edge_attrs) {
      adj <- network::as.matrix.network(x, matrix.type = "adjacency",
                                         attrname = "weight")
    } else {
      adj <- network::as.matrix.network(x, matrix.type = "adjacency")
    }

    g <- igraph::graph_from_adjacency_matrix(adj, mode = graph_mode,
                                              weighted = TRUE)
    # Add node names
    labels <- network::network.vertex.names(x)
    if (!is.null(labels) && !all(is.na(labels))) {
      igraph::V(g)$name <- labels
    }
    return(g)
  }

  if (inherits(x, "tna")) {
    weights <- x$weights
    # Use directed override if specified, otherwise auto-detect
    graph_mode <- if (!is.null(directed)) {
      if (directed) "directed" else "undirected"
    } else {
      if (isSymmetric(weights)) "undirected" else "directed"
    }
    g <- igraph::graph_from_adjacency_matrix(
      weights, mode = graph_mode, weighted = TRUE
    )
    if (!is.null(x$labels)) igraph::V(g)$name <- x$labels
    return(g)
  }

  if (is.matrix(x)) {
    # Use directed override if specified, otherwise auto-detect
    graph_mode <- if (!is.null(directed)) {
      if (directed) "directed" else "undirected"
    } else {
      if (isSymmetric(x)) "undirected" else "directed"
    }
    g <- igraph::graph_from_adjacency_matrix(x, mode = graph_mode, weighted = TRUE)
    if (!is.null(rownames(x))) igraph::V(g)$name <- rownames(x)
    return(g)
  }

  stop("x must be a matrix, data.frame edge list, igraph, network, cograph_network, or tna object",
       call. = FALSE)
}

#' Detect Communities in a Network
#'
#' Detects communities (clusters) in a network using various community detection
#' algorithms. Returns a data frame with node-community assignments.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object.
#' @param method Community detection algorithm to use. One of:
#'   \itemize{
#'     \item \code{"louvain"}: Louvain method (default, fast and accurate)
#'     \item \code{"walktrap"}: Walktrap algorithm based on random walks
#'     \item \code{"fast_greedy"}: Fast greedy modularity optimization
#'     \item \code{"label_prop"}: Label propagation algorithm
#'     \item \code{"infomap"}: Infomap algorithm based on information flow
#'     \item \code{"leiden"}: Leiden algorithm (improved Louvain)
#'   }
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param weights Logical. Use edge weights for community detection. Default TRUE.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{node}: Node labels/names
#'     \item \code{community}: Integer community membership
#'   }
#'
#' @export
#' @examples
#' # Basic usage
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' detect_communities(adj)
#'
#' # Different algorithm
#' detect_communities(adj, method = "walktrap")
detect_communities <- function(x, method = "louvain", directed = NULL,
                               weights = TRUE) {

  # Validate method
  method <- match.arg(method, c("louvain", "walktrap", "fast_greedy",
                                 "label_prop", "infomap", "leiden"))

  # Convert to igraph

  g <- to_igraph(x, directed = directed)

  # Get weights
  edge_weights <- if (weights && !is.null(igraph::E(g)$weight)) {
    igraph::E(g)$weight
  } else {
    NULL
  }

  # louvain and leiden are undirected-only in igraph (they error on a directed
  # graph). tna models — the package's primary input — are always directed,
  # and louvain is the default method, so collapse directed edges to
  # undirected (mean) first, exactly as the fast_greedy branch below does,
  # rather than letting igraph abort.
  if (method %in% c("louvain", "leiden") && igraph::is_directed(g)) {
    message("Method '", method, "' requires an undirected graph; ",
            "collapsing directed edges (mean) for community detection.")
    g <- igraph::as_undirected(g, mode = "collapse", edge.attr.comb = "mean")
    edge_weights <- if (weights && !is.null(igraph::E(g)$weight)) {
      igraph::E(g)$weight
    } else {
      NULL
    }
  }

  # Apply community detection algorithm
  communities <- switch(method,
    "louvain" = igraph::cluster_louvain(g, weights = edge_weights),
    "walktrap" = igraph::cluster_walktrap(g, weights = edge_weights),
    "fast_greedy" = {
      # fast_greedy requires undirected graph
      g_undirected <- igraph::as_undirected(g, mode = "collapse",
                                             edge.attr.comb = "mean")
      fg_weights <- if (weights && !is.null(igraph::E(g_undirected)$weight)) {
        igraph::E(g_undirected)$weight
      } else {
        NULL
      }
      igraph::cluster_fast_greedy(g_undirected, weights = fg_weights)
    },
    "label_prop" = igraph::cluster_label_prop(g, weights = edge_weights),
    "infomap" = igraph::cluster_infomap(g, e.weights = edge_weights),
    "leiden" = {
      if (!requireNamespace("igraph", quietly = TRUE) ||
          !exists("cluster_leiden", where = asNamespace("igraph"))) { # nocov start
        stop("Leiden algorithm requires igraph >= 1.2.5", call. = FALSE)
      } # nocov end
      igraph::cluster_leiden(g, weights = edge_weights)
    }
  )

  # Return a classed cograph_communities object (inherits from data.frame)
  # with $igraph_result, $algorithm, $modularity, $network attributes so the
  # existing plot/print/modularity S3 methods actually fire.
  .wrap_communities(communities, method, g, network = x)
}

#' Color Nodes by Community
#'
#' Generate colors for nodes based on community membership. Designed for
#' direct use with \code{splot()} \code{node_fill} parameter.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object.
#' @param method Community detection algorithm. See \code{\link{detect_communities}}
#'   for available methods. Default \code{"louvain"}.
#' @param palette Color palette to use. Can be:
#'   \itemize{
#'     \item \code{NULL} (default): Uses a colorblind-friendly palette
#'     \item A character vector of colors
#'     \item A function that takes n and returns n colors
#'     \item A palette name: "rainbow", "colorblind", "pastel", "viridis"
#'   }
#' @param ... Additional arguments passed to \code{\link{detect_communities}}.
#'
#' @return A named character vector of colors (one per node), suitable for
#'   use with \code{splot()} \code{node_fill} parameter.
#'
#' @seealso \code{\link{detect_communities}}, \code{\link{splot}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Basic usage with splot
#' splot(adj, node_fill = color_communities(adj))
#'
#' # Custom palette
#' splot(adj, node_fill = color_communities(adj, palette = c("red", "blue")))
color_communities <- function(x, method = "louvain", palette = NULL, ...) {

  # Get community membership
  comm_df <- detect_communities(x, method = method, ...)

  # Get unique communities
  unique_comm <- sort(unique(comm_df$community))
  n_communities <- length(unique_comm)

  # Resolve palette
  if (is.null(palette)) {
    # Default colorblind-friendly palette
    colors <- palette_colorblind(n_communities)
  } else if (is.function(palette)) {
    colors <- palette(n_communities)
  } else if (is.character(palette) && length(palette) == 1) {
    # Palette name
    palette_func <- switch(palette,
      "rainbow" = palette_rainbow,
      "colorblind" = palette_colorblind,
      "pastel" = palette_pastel,
      "viridis" = palette_viridis,
      NULL
    )
    if (!is.null(palette_func)) {
      colors <- palette_func(n_communities)
    } else {
      # Treat as a single color - replicate
      colors <- rep(palette, n_communities)
    }
  } else {
    # Character vector of colors
    if (length(palette) < n_communities) {
      colors <- rep_len(palette, n_communities)
    } else {
      colors <- palette[seq_len(n_communities)]
    }
  }

  # Map community to color for each node
  color_map <- stats::setNames(colors, unique_comm)
  node_colors <- color_map[as.character(comm_df$community)]
  names(node_colors) <- comm_df$node

  node_colors
}

# =============================================================================
# Enhanced Filtering Functions
# =============================================================================

#' Filter Edges by Metadata
#'
#' Filter edges using dplyr-style expressions on any edge column. Returns a
#' cograph_network object by default (universal format), or optionally a
#' matrix, igraph, or statnet network object when \code{keep_format = TRUE}
#' and the input used one of those formats.
#'
#' @param x Network input: cograph_network, matrix, igraph, network, or tna object.
#' @param ... Filter expressions using any edge column (e.g., \code{weight > 0.5},
#'   \code{weight > mean(weight)}, \code{abs(weight) > 0.3}).
#' @param keep_isolates Logical. Keep nodes that end up with no edges?
#'   Default TRUE, matching \code{igraph::delete_edges()} and tidygraph:
#'   filtering edges does not remove nodes. Set FALSE to drop them, or call
#'   \code{\link{remove_isolates}()} afterwards.
#' @param .keep_isolates Deprecated. Use \code{keep_isolates}.
#' @param keep_format Logical. If TRUE, matrix, igraph, and statnet network
#'   inputs are returned in that format. Default FALSE
#'   returns cograph_network (universal format).
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#'   Only used for non-cograph_network inputs.
#'
#' @return A cograph_network object with filtered edges. If \code{keep_format = TRUE},
#'   matrix, igraph, and statnet network inputs are converted back to that type.
#'
#' @seealso \code{\link{filter_nodes}}, \code{\link{splot}}, \code{\link{subset_edges}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0, .5, 0, .3, .6,
#'                 .8, .3, 0, .4, 0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Keep only strong edges
#' filter_edges(adj, weight > 0.5)
#'
#' # Matrix in, matrix out
#' filter_edges(adj, weight > 0.5, keep_format = TRUE)
#'
#' # Pipe-friendly with cograph_network
#' as_cograph(adj) |>
#'   filter_edges(weight > 0.3) |>
#'   filter_nodes(degree >= 2) |>
#'   splot()
filter_edges <- function(x, ..., keep_isolates = TRUE, keep_format = FALSE,
                         directed = NULL, .keep_isolates = NULL) {
  keep_isolates <- .resolve_deprecated_arg(keep_isolates, .keep_isolates,
                                           ".keep_isolates", "keep_isolates")
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  edges <- get_edges(net)

  if (nrow(edges) == 0) {
    warning("Network has no edges", call. = FALSE)
    return(.finish_result(net, x, input_class, keep_format))
  }

  # Every edge column is in scope, including columns the caller added.
  eval_env <- list2env(as.list(edges), parent = parent.frame())
  dots <- substitute(list(...))[-1]
  mask <- .evaluate_filter_conditions(dots, eval_env, nrow(edges))
  filtered_edges <- edges[mask, , drop = FALSE]

  result <- .update_cograph_edges(net, filtered_edges, keep_isolates = keep_isolates)

  if (n_edges(result) == 0) {
    warning("Filter removed all edges.", call. = FALSE)
  }
  if (isTRUE(keep_isolates)) {
    # Also when every edge went: the nodes stay, so they are newly isolated.
    .warn_new_isolates(edges, filtered_edges, n_nodes(net))
  }
  if (n_nodes(result) == 0) {
    warning("Filter removed all nodes. Result may not be usable for plotting.", call. = FALSE)
  }

  .finish_result(result, x, input_class, keep_format)
}

#' Filter Nodes by Metadata or Centrality
#'
#' Filter nodes using dplyr-style expressions on any node column or centrality
#' measure. Returns a cograph_network object by default (universal format), or
#' optionally a matrix, igraph, or statnet network object when
#' \code{keep_format = TRUE} and the input used one of those formats.
#'
#' @param x Network input: cograph_network, matrix, igraph, network, or tna object.
#' @param ... Filter expressions using any node column or centrality measure.
#'   Available variables include:
#'   \describe{
#'     \item{Node columns}{All columns in the nodes dataframe: \code{id}, \code{label},
#'       \code{name}, \code{x}, \code{y}, \code{inits}, \code{color}, plus any custom}
#'     \item{Centrality measures}{\code{degree}, \code{indegree}, \code{outdegree},
#'       \code{strength}, \code{instrength}, \code{outstrength}, \code{betweenness},
#'       \code{closeness}, \code{eigenvector}, \code{pagerank}, \code{hub}, \code{authority}}
#'   }
#'   Examples: \code{degree >= 3}, \code{label \%in\% c("A", "B")},
#'   \code{pagerank > 0.1 & degree >= 2}.
#' @param .keep_edges Deprecated. Use \code{keep_edges}.
#' @param keep_edges How to handle edges. One of:
#'   \describe{
#'     \item{\code{"internal"}}{(default) Keep only edges between remaining nodes}
#'     \item{\code{"none"}}{Remove all edges}
#'   }
#' @param keep_format Logical. If TRUE, matrix, igraph, and statnet network
#'   inputs are returned in that format. Default FALSE
#'   returns cograph_network (universal format).
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#'   Only used for non-cograph_network inputs.
#'
#' @return A cograph_network object with filtered nodes. If \code{keep_format = TRUE},
#'   matrix, igraph, and statnet network inputs are converted back to that type.
#'
#' @seealso \code{\link{filter_edges}}, \code{\link{splot}}, \code{\link{subset_nodes}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0, .5, 0, .3, .6,
#'                 .8, .3, 0, .4, 0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Keep only high-degree nodes
#' filter_nodes(adj, degree >= 3)
#'
#' # Filter by label, combined with degree
#' filter_nodes(adj, degree >= 2 & label != "D")
filter_nodes <- function(x, ..., keep_edges = c("internal", "none"),
                         keep_format = FALSE, directed = NULL,
                         .keep_edges = NULL) {
  keep_edges <- .resolve_deprecated_arg(match.arg(keep_edges), .keep_edges,
                                        ".keep_edges", "keep_edges")
  keep_edges <- match.arg(keep_edges, c("internal", "none"))

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  nodes <- get_nodes(net)

  # Only the measures the expressions actually name are computed; the whole
  # twelve-measure sweep was the main cost of this verb.
  cg <- .cg_graph(net)
  dots <- substitute(list(...))[-1]
  eval_env <- .build_filter_env(
    nodes,
    .node_filter_vars(cg, .detect_needed_variables(dots)),
    parent.frame()
  )
  mask <- .evaluate_filter_conditions(dots, eval_env, nrow(nodes))
  selected_idx <- which(mask)

  if (length(selected_idx) == 0) {
    warning("No nodes match the filter criteria. Result may not be usable for plotting.",
            call. = FALSE)
    empty <- .empty_cograph_network(net$directed, meta = net$meta, data = net$data)
    return(.finish_result(empty, x, input_class, keep_format))
  }

  result <- .subset_cograph_network(net, nodes = selected_idx, keep_edges = keep_edges)
  .finish_result(result, x, input_class, keep_format)
}

#' @rdname filter_nodes
#' @export
subset_nodes <- filter_nodes

#' @rdname filter_edges
#' @return See \code{\link{filter_edges}}.
#' @export
subset_edges <- filter_edges

# =============================================================================
# Helper Functions for Filtering
# =============================================================================

#' The node vocabulary available inside filter expressions
#'
#' Three groups, resolved in this order: measures with a dedicated kernel
#' (cheap), structural context variables, and boolean predicates. Any other
#' name is looked up in [centrality()]; anything left over is a variable from
#' the caller's frame.
#'
#' @return A named list of character vectors.
#' @noRd
.node_vocabulary <- function() {
  list(
    centrality = c("degree", "indegree", "outdegree", "strength",
                   "instrength", "outstrength", "betweenness",
                   "closeness", "eigenvector", "pagerank", "hub",
                   "authority", "coreness"),
    context = c("component", "component_size", "is_largest_component",
                "neighborhood_size", "k_core", "is_articulation",
                "is_bridge_endpoint"),
    predicate = c("is_isolated", "is_source", "is_sink", "is_leaf", "is_cut",
                  "local_transitivity", "local_triangles")
  )
}

#' Detect needed variables from expressions
#'
#' @param exprs List of unevaluated filter expressions.
#' @return A list with `centrality`, `context`, `predicate` and `measure`
#'   character vectors. `measure` holds names that only [centrality()] knows.
#' @noRd
.detect_needed_variables <- function(exprs) {
  vocab <- .node_vocabulary()
  all_vars <- unique(unlist(lapply(exprs, all.vars)))

  known <- unlist(vocab, use.names = FALSE)
  leftover <- setdiff(all_vars, known)

  list(
    centrality = intersect(all_vars, vocab$centrality),
    context = intersect(all_vars, vocab$context),
    predicate = intersect(all_vars, vocab$predicate),
    measure = intersect(leftover, .cg_delegable_measures())
  )
}

#' Measure names that can be delegated to centrality()
#' @noRd
.cg_delegable_measures <- function() {
  setdiff(c(.cg_mode_measures(), .cg_no_mode_measures()),
          .node_vocabulary()$centrality)
}

#' Compute one node variable by name
#'
#' The single resolver behind `filter_nodes()`, `select_nodes()` and the `by =`
#' argument of `select_top()`. Path-based measures are undefined on negative
#' weights: they return NA with a classed warning rather than a swallowed
#' error.
#'
#' @param g A `cg_graph`, `cograph_network`, matrix or igraph object.
#' @param measure Name of the measure.
#' @return A numeric or logical vector of length `n`.
#' @noRd
.compute_single_centrality <- function(g, measure) {
  cg <- .cg_as_context(g)
  n <- cg$n
  has_negative <- any(cg$w < 0)

  switch(measure,
    "degree" = .cg_degree(cg$b, cg$directed, "all"),
    "indegree" = .cg_degree(cg$b, cg$directed, "in"),
    "outdegree" = .cg_degree(cg$b, cg$directed, "out"),
    "strength" = .cg_strength(cg$w, cg$directed, "all"),
    "instrength" = .cg_strength(cg$w, cg$directed, "in"),
    "outstrength" = .cg_strength(cg$w, cg$directed, "out"),
    "betweenness" = if (has_negative) .cg_na_negative(n, "Betweenness") else
      .cg_betweenness(cg$w, n, cg$directed),
    "closeness" = if (has_negative) .cg_na_negative(n, "Closeness") else
      .cg_closeness(.cg_distances(cg$w, "all"), n),
    "eigenvector" = .cg_eigenvector(cg$w, n),
    "pagerank" = if (has_negative) .cg_na_negative(n, "PageRank") else
      .cg_pagerank(cg$w, n),
    "hub" = .cg_hits(cg$w, n)$hub,
    "authority" = .cg_hits(cg$w, n)$authority,
    "coreness" = .cg_coreness_loops(cg$b, n, cg$directed, "all"),
    .compute_delegated_centrality(cg, measure)
  )
}

#' Compute a measure that only centrality() knows
#'
#' @param cg A `cg_graph` context.
#' @param measure Name of the measure.
#' @return A numeric vector of length `n`.
#' @noRd
.compute_delegated_centrality <- function(cg, measure) {
  if (!measure %in% .cg_delegable_measures()) {
    .stop_bad_selection(
      "Unknown centrality measure '", measure, "'. ",
      "See list_centralities() for the measures cograph computes."
    )
  }
  tbl <- centrality(cg$w, measures = measure, directed = cg$directed)
  value_col <- setdiff(names(tbl), "node")[1]
  as.numeric(tbl[[value_col]])
}

#' Compute one node predicate by name
#' @noRd
.compute_node_predicate <- function(cg, predicate) {
  n <- cg$n
  switch(predicate,
    "is_isolated" = .cg_degree(cg$b, cg$directed, "all") == 0,
    "is_source" = cg$directed &
      .cg_degree(cg$b, cg$directed, "in") == 0 &
      .cg_degree(cg$b, cg$directed, "out") > 0,
    "is_sink" = cg$directed &
      .cg_degree(cg$b, cg$directed, "out") == 0 &
      .cg_degree(cg$b, cg$directed, "in") > 0,
    "is_leaf" = .cg_degree(cg$b, cg$directed, "all") == 1,
    "is_cut" = seq_len(n) %in% .cg_articulation_points(cg$b),
    "local_transitivity" = .cg_local_transitivity(cg$b, n, cg$directed),
    "local_triangles" = .cg_triangle_counts(.cg_undirected_view(cg$b))
  )
}

#' Assemble the variables a node filter expression needs
#'
#' @param g A `cg_graph` context or anything `.cg_as_context()` accepts.
#' @param needed The list returned by `.detect_needed_variables()`.
#' @return A named list of vectors, each of length `n`.
#' @noRd
.node_filter_vars <- function(g, needed) {
  cg <- .cg_as_context(g)

  named_lapply <- function(names, fn) {
    if (length(names) == 0L) return(list())
    stats::setNames(lapply(names, fn), names)
  }

  c(
    named_lapply(c(needed$centrality, needed$measure),
                 function(m) .compute_single_centrality(cg, m)),
    .compute_lazy_context(cg, needed$context),
    named_lapply(needed$predicate, function(p) .compute_node_predicate(cg, p))
  )
}

#' Compute Centrality Variables for Filtering
#'
#' The full documented vocabulary, computed eagerly. Kept for callers that want
#' every measure at once; the verbs themselves ask for only what they need.
#'
#' @noRd
.compute_centrality_vars <- function(g) {
  .node_filter_vars(g, list(centrality = .node_vocabulary()$centrality))
}

#' Compute only needed centrality measures (lazy)
#' @noRd
.compute_lazy_centralities <- function(g, needed, has_negative = FALSE) {
  .node_filter_vars(g, list(centrality = needed))
}

#' Build Filter Environment
#' @noRd
.build_filter_env <- function(df, extra_vars = list(), parent = parent.frame()) {
  # Combine dataframe columns with extra variables
  all_vars <- c(as.list(df), extra_vars)
  list2env(all_vars, parent = parent)
}

#' Evaluate Filter Conditions
#' @noRd
.evaluate_filter_conditions <- function(dots, env, n) {
  if (length(dots) == 0) {
    return(rep(TRUE, n))
  }

  # Evaluate each condition and combine with AND
  masks <- lapply(dots, function(expr) {
    result <- eval(expr, envir = env)
    if (!is.logical(result)) {
      stop("Filter expressions must evaluate to logical vectors", call. = FALSE)
    }
    if (length(result) == 1L) {
      result <- rep(result, n)
    } else if (length(result) != n) {
      stop("Filter expressions must return length 1 or ", n,
           ", not ", length(result), call. = FALSE)
    }
    result[is.na(result)] <- FALSE
    result
  })

  Reduce(`&`, masks)
}

#' Create Empty cograph_network
#' @noRd
.empty_cograph_network <- function(directed = FALSE, meta = NULL, data = NULL) {
  .create_cograph_network(
    nodes = data.frame(id = integer(0), label = character(0)),
    edges = data.frame(from = integer(0), to = integer(0), weight = numeric(0)),
    directed = directed,
    meta = meta %||% list(source = "filtered"),
    weights = matrix(0, 0, 0),
    data = data
  )
}

#' Signal a malformed selection argument
#'
#' Selection arguments describe a set of nodes or edges. A malformed one is a
#' broken contract, not a recoverable anomaly, so it raises a classed error
#' that callers and tests can match on without reading the message text.
#'
#' @param ... Parts of the message, pasted together.
#' @return Never returns; raises a `cograph_bad_selection` error.
#' @noRd
.stop_bad_selection <- function(...) {
  stop(errorCondition(paste0(...), class = "cograph_bad_selection", call = NULL))
}

#' Build a weight matrix from an edge table
#'
#' One vectorised matrix assignment rather than a cell-by-cell loop. For an
#' undirected network the edge table holds one row per unordered pair, so the
#' transposed positions are filled as well; without that the rebuilt matrix is
#' upper-triangular and every downstream consumer reads the network as
#' directed with half the strength.
#'
#' @param labels Character vector of node labels, in node order.
#' @param edges Edge data frame with `from`, `to`, `weight` in node-index space.
#' @param directed Logical. Mirror the entries when FALSE.
#' @return A square numeric matrix with `labels` as dimnames.
#' @noRd
.network_weight_matrix <- function(labels, edges, directed) {
  n <- length(labels)
  dn <- if (n > 0L) list(labels, labels) else NULL
  w <- matrix(0, n, n, dimnames = dn)
  if (n == 0L || is.null(edges) || nrow(edges) == 0L) {
    return(w)
  }
  idx <- cbind(as.integer(edges$from), as.integer(edges$to))
  weight <- as.numeric(edges$weight)
  w[idx] <- weight
  if (!isTRUE(directed)) {
    w[idx[, c(2L, 1L), drop = FALSE]] <- weight
  }
  w
}

#' Rebuild a cograph_network from a node selection and an edge table
#'
#' The single place where a wrangling verb turns "these nodes, these edges"
#' back into a network. Remaps node indices, rebuilds the weight matrix, and
#' carries the metadata that the verbs used to drop: node groups, estimation
#' data, layout, and the original source type.
#'
#' @param net A `cograph_network`.
#' @param nodes_keep Integer indices of nodes to keep, or NULL for all.
#' @param edges Edge data frame in the *current* node-index space, or NULL to
#'   reuse the network's own edges.
#' @param keep_edges `"internal"` keeps edges whose endpoints both survive;
#'   `"none"` drops every edge.
#' @return A `cograph_network`.
#' @noRd
.rebuild_network <- function(net, nodes_keep = NULL, edges = NULL,
                             keep_edges = "internal") {
  node_df <- get_nodes(net)
  edge_df <- if (is.null(edges)) get_edges(net) else edges

  keep <- if (is.null(nodes_keep)) {
    seq_len(nrow(node_df))
  } else {
    sort(unique(as.integer(nodes_keep)))
  }

  new_nodes <- node_df[keep, , drop = FALSE]
  new_nodes$id <- seq_len(nrow(new_nodes))
  rownames(new_nodes) <- NULL

  if (identical(keep_edges, "none") || nrow(edge_df) == 0L) {
    new_edges <- edge_df[0L, , drop = FALSE]
  } else {
    inside <- edge_df$from %in% keep & edge_df$to %in% keep
    new_edges <- edge_df[inside, , drop = FALSE]
    new_edges$from <- match(new_edges$from, keep)
    new_edges$to <- match(new_edges$to, keep)
  }
  rownames(new_edges) <- NULL

  # Group assignments are keyed by label, so they follow the surviving nodes.
  groups <- net$node_groups
  if (!is.null(groups) && is.data.frame(groups) && "node" %in% names(groups)) {
    groups <- groups[groups$node %in% new_nodes$label, , drop = FALSE]
    rownames(groups) <- NULL
    if (nrow(groups) == 0L) groups <- NULL
  }

  .create_cograph_network(
    nodes = new_nodes,
    edges = new_edges,
    directed = net$directed,
    meta = net$meta,
    weights = .network_weight_matrix(as.character(new_nodes$label), new_edges,
                                     isTRUE(net$directed)),
    data = net$data,
    node_groups = groups
  )
}

#' Rebuild a network around a new weight matrix
#'
#' The counterpart of `.rebuild_network()` for verbs that work on the weight
#' matrix rather than on the edge table: the node table, layout, groups and
#' estimation data are carried across unchanged and the edge table is derived
#' from the matrix. Extra edge columns cannot survive an operation that may
#' create, merge or drop edges, so they are dropped.
#'
#' @param net The network the verb was called on.
#' @param m New square weight matrix, in the same node order.
#' @param directed Directedness of the result; NULL keeps the network's own.
#' @return A `cograph_network`.
#' @noRd
.network_from_matrix <- function(net, m, directed = NULL) {
  dir <- if (is.null(directed)) isTRUE(net$directed) else isTRUE(directed)
  nodes <- get_nodes(net)
  labels <- as.character(nodes$label)
  dimnames(m) <- if (nrow(m) > 0L) list(labels, labels) else NULL

  nz <- which(m != 0, arr.ind = TRUE)
  if (!dir && nrow(nz) > 0L) {
    # One row per unordered pair: read the upper triangle, diagonal included.
    nz <- nz[nz[, 1L] <= nz[, 2L], , drop = FALSE]
  }
  edges <- data.frame(
    from = as.integer(nz[, 1L]),
    to = as.integer(nz[, 2L]),
    weight = as.numeric(m[nz])
  )

  .create_cograph_network(
    nodes = nodes,
    edges = edges,
    directed = dir,
    meta = net$meta,
    weights = m,
    data = net$data,
    node_groups = net$node_groups
  )
}

#' Combine two aligned weight matrices, treating absence as absence
#'
#' A weight of zero means "no edge" in this representation, so it must never
#' be fed to `pmax()`/`pmin()` as if it were a comparable value: a one-way
#' edge of weight -2 would lose to the missing reverse arc and be deleted, and
#' a one-way edge of weight 2 would lose under `min`. The presence mask is
#' therefore carried separately from the weight, and two values are combined
#' only where both arcs actually exist.
#'
#' @param a,b Square numeric matrices of the same shape.
#' @param how `"max"`, `"min"`, `"mean"`, `"sum"` or `"first"`.
#' @return A matrix of the same shape.
#' @noRd
.combine_arcs <- function(a, b, how) {
  present_a <- a != 0
  present_b <- b != 0
  both <- present_a & present_b

  out <- matrix(0, nrow(a), ncol(a))
  out[present_a & !present_b] <- a[present_a & !present_b]
  out[present_b & !present_a] <- b[present_b & !present_a]

  if (any(both)) {
    out[both] <- switch(how,
      max = pmax(a[both], b[both]),
      min = pmin(a[both], b[both]),
      mean = (a[both] + b[both]) / 2,
      sum = a[both] + b[both],
      first = a[both]
    )
  }
  out
}

#' Warn when combining weights cancelled an edge to exactly zero
#'
#' Zero is the absence sentinel, so an edge whose combined weight is zero
#' disappears. That has to be said out loud rather than silently shrinking the
#' edge set.
#'
#' @noRd
.warn_cancelled_edges <- function(a, b, combined) {
  cancelled <- sum((a != 0 | b != 0) & combined == 0)
  if (cancelled > 0L) {
    warning(warningCondition(
      paste0(cancelled, " edge(s) combined to weight zero and were dropped; ",
             "zero is how this representation stores 'no edge'."),
      class = "cograph_edges_dropped"))
  }
  invisible(cancelled)
}

#' Reject non-finite edge weights
#'
#' The matrix-level verbs have no defensible answer for `NA` or `Inf`, and
#' letting one through produces a raw internal error several frames later.
#'
#' @noRd
.check_finite_weights <- function(m, fn) {
  if (any(!is.finite(m))) {
    .stop_bad_selection(
      fn, "() needs finite weights; the network has ",
      sum(!is.finite(m)), " non-finite value(s). Fix or remove them first."
    )
  }
  invisible(TRUE)
}

#' Validate a whole, non-negative count
#' @noRd
.check_count <- function(value, arg, min = 0) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    .stop_bad_selection("`", arg, "` must be a single finite number.")
  }
  if (value != as.integer(value)) {
    .stop_bad_selection("`", arg, "` must be a whole number; got ", value, ".")
  }
  if (value < min) {
    .stop_bad_selection("`", arg, "` must be at least ", min, "; got ", value, ".")
  }
  invisible(as.integer(value))
}

#' Drop edge rows whose weight is exactly zero
#'
#' The edge table and the weight matrix must agree: a zero cell is no edge, so
#' a zero-weight row cannot be kept without the two disagreeing.
#'
#' @noRd
.drop_zero_edges <- function(edges) {
  if (is.null(edges) || nrow(edges) == 0L) {
    return(edges)
  }
  zero <- edges$weight == 0 | !is.finite(edges$weight)
  if (any(zero)) {
    warning(warningCondition(
      paste0(sum(zero), " edge(s) with zero or non-finite weight were dropped; ",
             "zero is how this representation stores 'no edge'."),
      class = "cograph_edges_dropped"))
    edges <- edges[!zero, , drop = FALSE]
    rownames(edges) <- NULL
  }
  edges
}

#' Node indices that carry at least one edge
#' @noRd
.connected_nodes <- function(edges) {
  if (is.null(edges) || nrow(edges) == 0L) {
    return(integer(0))
  }
  sort(unique(as.integer(c(edges$from, edges$to))))
}

#' Subset cograph_network by Node Indices
#' @noRd
.subset_cograph_network <- function(net, nodes, keep_edges = "internal") {
  .rebuild_network(net, nodes_keep = nodes, keep_edges = keep_edges)
}

#' Update Edges in cograph_network
#'
#' Replaces the edge table. Filtering edges never removes nodes (igraph's
#' `delete_edges()` and tidygraph's `filter()` on edges behave the same way);
#' pass `keep_isolates = FALSE` for the pruning behaviour, or call
#' [remove_isolates()] afterwards.
#'
#' @noRd
.update_cograph_edges <- function(net, new_edges, keep_isolates = TRUE) {
  result <- .rebuild_network(net, edges = new_edges)

  if (!isTRUE(keep_isolates)) {
    connected <- .connected_nodes(new_edges)
    if (length(connected) == 0L) {
      return(.empty_cograph_network(net$directed, meta = net$meta, data = net$data))
    }
    result <- .rebuild_network(net, nodes_keep = connected, edges = new_edges)
  }

  result
}

#' Detect Input Class for Format Preservation
#' @noRd
.detect_input_class <- function(x) {
  if (inherits(x, "cograph_network")) {
    return("cograph_network")
  } else if (is.matrix(x)) {
    return("matrix")
  } else if (inherits(x, "igraph")) {
    return("igraph")
  } else if (inherits(x, "network")) {
    return("network")
  } else if (inherits(x, "tna") || inherits(x, "group_tna")) {
    return("tna")
  } else if (inherits(x, "qgraph")) {
    return("qgraph")
  } else {
    return("unknown")
  }
}

#' Convert cograph_network to Specified Format
#'
#' `original` is the object the verb was called on; a tna model is rebuilt from
#' it (copy, swap the weight matrix, subset labels and initial probabilities)
#' rather than silently downgraded to a cograph_network.
#'
#' @noRd
.convert_to_format <- function(net, format, original = NULL) {
  switch(format,
    matrix = to_matrix(net),
    igraph = to_igraph(net),
    network = if (n_nodes(net) == 0L) to_matrix(net) else to_network(net),
    tna = .rebuild_tna(net, original),
    {
      if (format %in% c("qgraph", "unknown") && !is.null(original)) {
        warning(warningCondition(
          paste0("Cannot rebuild a '", format, "' object; returning a ",
                 "cograph_network. Use as_cograph() upstream to make this explicit."),
          class = "cograph_no_format_roundtrip"))
      }
      net
    }
  )
}

#' Rebuild a tna model around a filtered network
#'
#' A tna object is a list with `weights`, `labels`, `inits` and `data`. Copying
#' it and swapping those fields keeps the class, the sequence data and any
#' extra fields the tna package attached.
#'
#' @param net The filtered `cograph_network`.
#' @param original The tna object the verb was called on.
#' @return A tna object, or `net` when `original` is not a tna model.
#' @noRd
.rebuild_tna <- function(net, original) {
  if (is.null(original) || !inherits(original, "tna")) {
    return(net)
  }
  labels <- get_labels(net)
  old_labels <- original$labels %||% rownames(original$weights)
  result <- original
  result$weights <- to_matrix(net)
  if (!is.null(original$labels)) {
    result$labels <- labels
  }
  if (!is.null(original$inits) && !is.null(old_labels)) {
    keep <- match(labels, old_labels)
    result$inits <- original$inits[keep]
  }
  result
}

#' Apply keep_format to a finished result
#'
#' One place where every verb decides what to hand back, so an empty result and
#' a full one take the same path.
#'
#' @noRd
.finish_result <- function(net, original, input_class, keep_format) {
  if (!isTRUE(keep_format)) {
    return(net)
  }
  .convert_to_format(net, input_class, original = original)
}

#' Resolve the deprecated dot-prefixed argument names
#'
#' `.keep_isolates` and `.keep_edges` were named with a leading dot to keep
#' them out of the way of the filter expressions in `...`. Arguments after
#' `...` are matched exactly, so the dot is unnecessary; the old names still
#' work and take precedence when supplied.
#'
#' @noRd
.resolve_deprecated_arg <- function(value, deprecated, old_name, new_name) {
  if (is.null(deprecated)) {
    return(value)
  }
  warning(warningCondition(
    paste0("`", old_name, "` is deprecated; use `", new_name, "` instead."),
    class = "cograph_deprecated_arg"))
  deprecated
}

#' Warn when a verb left nodes without edges
#'
#' Filtering edges does not remove nodes (igraph and tidygraph semantics), so
#' the caller is told when the result has isolates that the filter created.
#'
#' @noRd
.warn_new_isolates <- function(before_edges, after_edges, n_nodes) {
  had <- .connected_nodes(before_edges)
  has <- .connected_nodes(after_edges)
  created <- setdiff(had, has)
  if (length(created) > 0L) {
    warning(warningCondition(
      paste0(length(created), " node(s) have no edges left. Nodes are kept; ",
             "call remove_isolates() to drop them."),
      class = "cograph_isolates_created"))
  }
  invisible(length(created))
}

#' Export Network as Edge List Data Frame
#'
#' Converts a network to an edge list data frame with columns for source,
#' target, and weight.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{from}: Source node name/label
#'     \item \code{to}: Target node name/label
#'     \item \code{weight}: Edge weight
#'   }
#'
#' @seealso \code{\link{to_df}}, \code{\link{to_igraph}}, \code{\link{as_cograph}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Convert to edge list
#' to_data_frame(adj)
#'
#' # Use alias
#' to_df(adj)
to_data_frame <- function(x, directed = NULL) {
  net <- as_cograph(x, directed = directed)
  edges <- get_edges(net)
  labels <- get_labels(net)

  df <- data.frame(
    from = labels[edges$from],
    to = labels[edges$to],
    weight = as.numeric(edges$weight),
    stringsAsFactors = FALSE
  )
  if (nrow(edges) == 0) {
    # Keep the column skeleton so that an empty result still reports the extra
    # columns the network carries.
    df$from <- character(0)
    df$to <- character(0)
  }

  # Keep any extra edge columns the network carries (session, time, ...).
  extra_cols <- setdiff(names(edges), c("from", "to", "weight"))
  if (length(extra_cols) > 0) {
    df[extra_cols] <- edges[extra_cols]
  }

  df
}

#' @rdname to_data_frame
#' @export
to_df <- function(x, directed = NULL) {

  to_data_frame(x, directed = directed)
}


#' Convert Network to Adjacency Matrix
#'
#' Converts any supported network format to an adjacency matrix.
#'
#' @param x Network input: matrix, cograph_network, igraph, network, tna, etc.
#' @param directed Logical or NULL. If NULL (default), auto-detect from input.
#'
#' @return A square numeric adjacency matrix, preserving row/column names when
#'   available.
#'
#' @seealso \code{\link{to_igraph}}, \code{\link{to_df}}, \code{\link{as_cograph}},
#'   \code{\link{to_network}}
#'
#' @export
#' @examples
#' # From matrix
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' to_matrix(adj)
#'
#' # From cograph_network
#' net <- as_cograph(adj)
#' to_matrix(net)
#'
#' # From igraph (weighted graph)
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
#'   to_matrix(g)
#' }
to_matrix <- function(x, directed = NULL) {

  # If already a matrix, return as-is
  if (is.matrix(x)) {
    return(x)
  }

  # cograph_network with stored weight matrix: use it directly
  if (inherits(x, "cograph_network") && !is.null(x$weights) && is.matrix(x$weights)) {
    return(x$weights)
  }

  # Otherwise build it from the node and edge tables. Going through igraph
  # here used to lose trailing isolates and to fail outright on an edgeless
  # network ("No such edge attribute").
  net <- as_cograph(x, directed = directed)
  .network_weight_matrix(get_labels(net), get_edges(net), isTRUE(net$directed))
}


#' Convert Network to statnet network Object
#'
#' Converts any supported network format to a statnet network object.
#'
#' @param x Network input: matrix, cograph_network, igraph, tna, etc.
#' @param directed Logical or NULL. If NULL (default), auto-detect from input.
#'
#' @return A network object from the network package.
#'
#' @seealso \code{\link{to_igraph}}, \code{\link{to_matrix}}, \code{\link{to_df}},
#'   \code{\link{as_cograph}}
#'
#' @export
#' @examples
#' if (requireNamespace("network", quietly = TRUE)) {
#'   adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#'   rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#'   net <- to_network(adj)
#' }
to_network <- function(x, directed = NULL) {

  if (!requireNamespace("network", quietly = TRUE)) { # nocov start
    stop("Package 'network' is required for to_network(). ",
         "Install it with: install.packages('network')", call. = FALSE)
  } # nocov end

  # Get adjacency matrix

  adj <- to_matrix(x, directed = directed)

  # Determine directedness
  is_directed <- if (!is.null(directed)) {
    directed
  } else if (inherits(x, "igraph")) {
    igraph::is_directed(x)
  } else if (inherits(x, "network")) {
    network::is.directed(x)
  } else if (inherits(x, "tna")) {
    TRUE
  } else {
    # Check matrix symmetry
    !isSymmetric(adj)
  }

  # Create network object
  net <- network::network(adj,
                          directed = is_directed,
                          ignore.eval = FALSE,
                          names.eval = "weight")

  # Set vertex names if available
  if (!is.null(rownames(adj))) {
    network::set.vertex.attribute(net, "vertex.names", rownames(adj))
  }

  net
}


# =============================================================================
# select_nodes() - Lazy Centrality Selection
# =============================================================================

#' Select Nodes with Lazy Centrality Computation
#'
#' A more nuanced node selection function that improves upon \code{filter_nodes()}
#' with lazy centrality computation (only computes measures actually referenced),
#' multiple selection modes, and global context variables for structural awareness.
#'
#' @param x Network input: cograph_network, matrix, igraph, network, or tna object.
#' @param ... Filter expressions using node columns, centrality measures, or
#'   global context variables. Centrality measures are computed lazily (only
#'   those actually referenced). Available variables:
#'   \describe{
#'     \item{Node columns}{All columns in the nodes dataframe: \code{id}, \code{label},
#'       \code{name}, \code{x}, \code{y}, \code{inits}, \code{color}, plus any custom}
#'     \item{Centrality measures}{\code{degree}, \code{indegree}, \code{outdegree},
#'       \code{strength}, \code{instrength}, \code{outstrength}, \code{betweenness},
#'       \code{closeness}, \code{eigenvector}, \code{pagerank}, \code{hub},
#'       \code{authority}, \code{coreness}. Any other measure
#'       \code{\link{centrality}()} computes can be named too; see
#'       \code{\link{list_centralities}()}.}
#'     \item{Global context}{\code{component}, \code{component_size},
#'       \code{is_largest_component}, \code{neighborhood_size}, \code{k_core},
#'       \code{is_articulation}, \code{is_bridge_endpoint}}
#'     \item{Predicates}{\code{is_isolated}, \code{is_source}, \code{is_sink},
#'       \code{is_leaf}, \code{is_cut}, \code{local_transitivity},
#'       \code{local_triangles}}
#'   }
#' @param name Character vector. Select nodes by name/label.
#' @param index Integer vector. Select nodes by index (1-based).
#' @param top Integer. Select top N nodes by centrality measure.
#' @param by Character. Centrality measure for top selection. Default \code{"degree"}.
#' @param neighbors_of Character or integer. Select neighbors of these nodes
#'   (by name or index).
#' @param order Integer. Neighborhood order (1 = direct neighbors, 2 = neighbors
#'   of neighbors, etc.). Default 1.
#' @param component Selection mode for connected components:
#'   \describe{
#'     \item{\code{"largest"}}{Select nodes in the largest connected component}
#'     \item{Integer}{Select nodes in component with this ID}
#'     \item{Character}{Select component containing node with this name}
#'   }
#' @param .keep_edges Deprecated. Use \code{keep_edges}.
#' @param keep_edges How to handle edges. One of:
#'   \describe{
#'     \item{\code{"internal"}}{(default) Keep only edges between remaining nodes}
#'     \item{\code{"none"}}{Remove all edges}
#'   }
#' @param keep_format Logical. If TRUE, matrix, igraph, and statnet network
#'   inputs are returned in that format. Default FALSE returns cograph_network.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @details
#' Selection modes are combined with AND logic (like tidygraph/dplyr):
#' \itemize{
#'   \item \code{select_nodes(x, top = 10, component = "largest")} selects
#'     top 10 nodes \strong{within} the largest component
#'   \item All criteria must be satisfied for a node to be selected
#' }
#'
#' Centrality measures are computed lazily - only measures actually referenced

#' in expressions or the \code{by} parameter are computed. This makes
#' \code{select_nodes()} faster than \code{filter_nodes()} for large networks.
#'
#' For networks with negative edge weights, \code{betweenness} and \code{closeness}
#' will return NA with a warning (igraph cannot compute these with negative weights).
#'
#' @return A cograph_network object with selected nodes. If \code{keep_format = TRUE},
#'   matrix, igraph, and statnet network inputs are converted back to that type.
#'
#' @seealso \code{\link{filter_nodes}}, \code{\link{select_neighbors}},
#'   \code{\link{select_component}}, \code{\link{select_top}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0, .5, 0, .3, .6,
#'                 .8, .3, 0, .4, 0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' select_nodes(adj, degree >= 3)
#' select_nodes(adj, top = 2, by = "pagerank")
#' select_nodes(adj, neighbors_of = "A", order = 2)
#' select_nodes(adj, component = "largest")
select_nodes <- function(x, ...,
                         name = NULL,
                         index = NULL,
                         top = NULL,
                         by = "degree",
                         neighbors_of = NULL,
                         order = 1L,
                         component = NULL,
                         keep_edges = c("internal", "none"),
                         keep_format = FALSE,
                         directed = NULL,
                         .keep_edges = NULL) {
  keep_edges <- .resolve_deprecated_arg(match.arg(keep_edges), .keep_edges,
                                        ".keep_edges", "keep_edges")
  keep_edges <- match.arg(keep_edges, c("internal", "none"))

  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  n_total <- n_nodes(net)

  if (n_total == 0) {
    warning("Network has no nodes", call. = FALSE)
    return(.finish_result(net, x, input_class, keep_format))
  }

  # The native graph context, not an igraph object: it carries every node,
  # including trailing isolates that no edge mentions.
  g <- .cg_graph(net)

  # Start with all nodes selected
  selected <- rep(TRUE, n_total)
  nodes <- get_nodes(net)

  # -------------------------

  # Apply selection modes (AND logic)
  # -------------------------


  # Mode 1: By name
  if (!is.null(name)) {
    name_match <- .select_by_name(nodes, name)
    selected <- selected & name_match
  }

  # Mode 2: By index
  if (!is.null(index)) {
    index_match <- .select_by_index(n_total, index)
    selected <- selected & index_match
  }

  # Mode 3: Component selection
  if (!is.null(component)) {
    comp_match <- .select_by_component(g, nodes, component)
    selected <- selected & comp_match
  }

  # Mode 4: Neighborhood selection
  if (!is.null(neighbors_of)) {
    neighbor_match <- .select_by_neighbors(g, nodes, neighbors_of, order)
    selected <- selected & neighbor_match
  }

  # Mode 5: Top N selection (applied to currently selected nodes)
  if (!is.null(top)) {
    top_match <- .select_by_top(g, nodes, top, by, selected)
    selected <- selected & top_match
  }

  # Mode 6: Expression-based filtering
  dots <- substitute(list(...))[-1]
  if (length(dots) > 0) {
    expr_match <- .select_by_expression(g, nodes, dots, parent.frame())
    selected <- selected & expr_match
  }

  # -------------------------
  # Create result
  # -------------------------
  selected_idx <- which(selected)

  if (length(selected_idx) == 0) {
    warning("No nodes match the selection criteria.", call. = FALSE)
    empty <- .empty_cograph_network(net$directed, meta = net$meta, data = net$data)
    return(.finish_result(empty, x, input_class, keep_format))
  }

  result <- .subset_cograph_network(net, nodes = selected_idx, keep_edges = keep_edges)
  .finish_result(result, x, input_class, keep_format)
}

# =============================================================================
# Selection Mode Helpers
# =============================================================================

#' Select nodes by name/label
#' @noRd
.select_by_name <- function(nodes, names) {
  nodes$label %in% names
}

#' Select nodes by index
#' @noRd
.select_by_index <- function(n_total, indices) {
  .validate_indices(indices, n_total, "index")
  seq_len(n_total) %in% as.integer(indices)
}

#' Validate a vector of node or edge indices
#'
#' Out-of-range and fractional indices used to be dropped or truncated in
#' silence, which turns a typo into a different result rather than an error.
#'
#' @param indices Numeric vector supplied by the caller.
#' @param n_total Number of nodes (or edges) available.
#' @param arg Name of the argument, for the message.
#' @return The indices as integers, invisibly.
#' @noRd
.validate_indices <- function(indices, n_total, arg) {
  if (!is.numeric(indices)) {
    .stop_bad_selection("`", arg, "` must be numeric, not ", class(indices)[1], ".")
  }
  if (any(is.na(indices))) {
    .stop_bad_selection("`", arg, "` contains NA.")
  }
  if (any(indices != as.integer(indices))) {
    .stop_bad_selection("`", arg, "` must be whole numbers; got ",
                        paste(indices[indices != as.integer(indices)], collapse = ", "), ".")
  }
  bad <- indices[indices < 1 | indices > n_total]
  if (length(bad) > 0) {
    .stop_bad_selection("`", arg, "` out of range: ",
                        paste(bad, collapse = ", "), ". The network has ",
                        n_total, " node(s).")
  }
  invisible(as.integer(indices))
}

#' Validate a single finite number, optionally within bounds
#' @noRd
.check_scalar_number <- function(value, arg, lower = -Inf, upper = Inf) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    .stop_bad_selection("`", arg, "` must be a single finite number.")
  }
  if (value < lower || value > upper) {
    .stop_bad_selection("`", arg, "` must be between ", lower, " and ", upper,
                        "; got ", value, ".")
  }
  invisible(value)
}

#' Resolve node names or indices to node indices
#'
#' Named .resolve_node_selection, not .resolve_nodes: paths.R already defines a
#' .resolve_nodes() with a different signature, and in a flat package namespace
#' the file that collates last simply wins.
#' @noRd
.resolve_node_selection <- function(nodes, selection, arg) {
  if (is.character(selection)) {
    unknown <- setdiff(selection, nodes$label)
    if (length(unknown) > 0) {
      .stop_bad_selection("`", arg, "` names nodes that are not in the network: ",
                          paste(unknown, collapse = ", "), ".")
    }
    return(which(nodes$label %in% selection))
  }
  .validate_indices(selection, nrow(nodes), arg)
  as.integer(selection)
}

#' Select nodes by component
#' @noRd
.select_by_component <- function(g, nodes, component) {
  cg <- .cg_as_context(g)
  comp <- .cg_components_numbered(cg$b)
  membership <- comp$membership

  if (identical(component, "largest")) {
    # Find largest component
    largest_comp <- which.max(comp$csize)
    return(membership == largest_comp)
  } else if (is.numeric(component)) {
    # Select by component ID
    if (component < 1 || component > comp$no) {
      .stop_bad_selection("Component ", component, " does not exist. The network has ",
                          comp$no, " component(s).")
    }
    return(membership == component)
  } else if (is.character(component)) {
    # Select component containing node with this name
    node_idx <- which(nodes$label == component)
    if (length(node_idx) == 0) {
      .stop_bad_selection("`component` names a node that is not in the network: ",
                          component, ".")
    }
    target_comp <- membership[node_idx[1]]
    return(membership == target_comp)
  }

  .stop_bad_selection("`component` must be \"largest\", a component number, ",
                      "or a node name.")
}

#' Select neighbors of specified nodes
#' @noRd
.select_by_neighbors <- function(g, nodes, of, order) {
  node_idx <- .resolve_node_selection(nodes, of, "of")

  # Ego network (includes the focal nodes themselves), either direction
  cg <- .cg_as_context(g)
  all_neighbors <- which(.cg_ego_mask(cg$b, node_idx, order, "all"))

  seq_len(nrow(nodes)) %in% all_neighbors
}

#' Select top N nodes by centrality
#' @noRd
.select_by_top <- function(g, nodes, top, by, current_selection) {
  .validate_measure(by, "by")
  centrality_vals <- .compute_single_centrality(g, by)

  if (all(is.na(centrality_vals))) {
    warning("Could not compute '", by, "' centrality. Returning all currently selected nodes.", call. = FALSE)
    return(current_selection)
  }

  # Only consider currently selected nodes
  masked_vals <- centrality_vals
  masked_vals[!current_selection] <- -Inf

  # Get top N indices
  n_available <- sum(current_selection)
  top <- min(top, n_available)

  if (top <= 0) {
    return(rep(FALSE, length(current_selection)))
  }

  # Get indices of top values
  top_idx <- order(masked_vals, decreasing = TRUE)[seq_len(top)]

  seq_len(length(current_selection)) %in% top_idx
}

#' Select nodes by expression (lazy centrality)
#' @noRd
.select_by_expression <- function(g, nodes, dots, parent_frame) {
  eval_env <- .build_filter_env(
    nodes,
    .node_filter_vars(g, .detect_needed_variables(dots)),
    parent_frame
  )
  .evaluate_filter_conditions(dots, eval_env, nrow(nodes))
}

#' Compute only needed global context variables (lazy)
#' @noRd
.compute_lazy_context <- function(g, needed) {
  if (length(needed) == 0) return(list())

  result <- list()
  cg <- .cg_as_context(g)
  n <- cg$n

  # Component-related variables
  comp_vars <- c("component", "component_size", "is_largest_component")
  if (any(needed %in% comp_vars)) {
    comp <- .cg_components_numbered(cg$b)
    if ("component" %in% needed) {
      result$component <- comp$membership
    }
    if ("component_size" %in% needed) {
      result$component_size <- comp$csize[comp$membership]
    }
    if ("is_largest_component" %in% needed) {
      largest <- which.max(comp$csize)
      result$is_largest_component <- comp$membership == largest
    }
  }

  # Neighborhood size
  if ("neighborhood_size" %in% needed) {
    result$neighborhood_size <- .cg_degree(cg$b, cg$directed, "all")
  }

  # K-core
  if ("k_core" %in% needed) {
    result$k_core <- .cg_coreness_loops(cg$b, n, cg$directed, "all")
  }

  # Articulation points
  if ("is_articulation" %in% needed) {
    result$is_articulation <- seq_len(n) %in% .cg_articulation_points(cg$b)
  }

  # Bridge endpoints
  if ("is_bridge_endpoint" %in% needed) {
    is_bridge <- .cg_bridges(cg$b, cg$edges)
    bridge_nodes <- unique(as.vector(cg$edges[is_bridge, , drop = FALSE]))
    result$is_bridge_endpoint <- seq_len(n) %in% bridge_nodes
  }

  result
}

# =============================================================================
# Convenience Functions
# =============================================================================

#' Select Node Neighbors (Ego Network)
#'
#' Select nodes within a specified distance from focal nodes.
#'
#' @param x Network input.
#' @param of Character or integer. Focal node(s) by name or index.
#' @param order Integer. Neighborhood order (1 = direct neighbors). Default 1.
#' @param ... Additional filter expressions to apply after neighborhood selection.
#' @param keep_edges How to handle edges. Default "internal".
#' @param keep_format Logical. Keep input format? Default FALSE.
#' @param directed Logical or NULL. Auto-detect if NULL.
#'
#' @return A cograph_network with nodes in the neighborhood.
#'
#' @seealso \code{\link{select_nodes}}, \code{\link{select_component}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Direct neighbors of A
#' select_neighbors(adj, of = "A")
#'
#' # Neighbors up to 2 hops
#' select_neighbors(adj, of = "A", order = 2)
select_neighbors <- function(x, of, order = 1L, ...,
                             keep_edges = c("internal", "none"),
                             keep_format = FALSE, directed = NULL) {
  select_nodes(x, ..., neighbors_of = of, order = order,
               keep_edges = keep_edges, keep_format = keep_format,
               directed = directed)
}

#' Select Connected Component
#'
#' Select nodes belonging to a specific connected component.
#'
#' @param x Network input.
#' @param which Component selection:
#'   \describe{
#'     \item{\code{"largest"}}{(default) The largest connected component}
#'     \item{Integer}{Component by ID}
#'     \item{Character}{Component containing the named node}
#'   }
#' @param ... Additional filter expressions to apply after component selection.
#' @param keep_edges How to handle edges. Default "internal".
#' @param keep_format Logical. Keep input format? Default FALSE.
#' @param directed Logical or NULL. Auto-detect if NULL.
#'
#' @return A cograph_network with nodes in the selected component.
#'
#' @seealso \code{\link{select_nodes}}, \code{\link{select_neighbors}}
#'
#' @export
#' @examples
#' # Create disconnected network
#' adj <- matrix(0, 6, 6)
#' adj[1, 2] <- adj[2, 1] <- 1
#' adj[1, 3] <- adj[3, 1] <- 1
#' adj[4, 5] <- adj[5, 4] <- 1
#' adj[5, 6] <- adj[6, 5] <- 1
#' adj[4, 6] <- adj[6, 4] <- 1
#' rownames(adj) <- colnames(adj) <- LETTERS[1:6]
#'
#' # Largest component
#' select_component(adj, which = "largest")
#'
#' # Component containing node "A"
#' select_component(adj, which = "A")
select_component <- function(x, which = "largest", ...,
                             keep_edges = c("internal", "none"),
                             keep_format = FALSE, directed = NULL) {
  select_nodes(x, ..., component = which, keep_edges = keep_edges,
               keep_format = keep_format, directed = directed)
}

#' Select Top N Nodes by Centrality
#'
#' Select the top N nodes ranked by a centrality measure.
#'
#' @param x Network input.
#' @param n Integer. Number of top nodes to select.
#' @param by Character. Centrality measure for ranking. One of:
#'   \code{"degree"}, \code{"indegree"}, \code{"outdegree"}, \code{"strength"},
#'   \code{"instrength"}, \code{"outstrength"}, \code{"betweenness"},
#'   \code{"closeness"}, \code{"eigenvector"}, \code{"pagerank"},
#'   \code{"hub"}, \code{"authority"}, \code{"coreness"}. Default \code{"degree"}.
#' @param ... Additional filter expressions to apply.
#' @param keep_edges How to handle edges. Default "internal".
#' @param keep_format Logical. Keep input format? Default FALSE.
#' @param directed Logical or NULL. Auto-detect if NULL.
#'
#' @return A cograph_network with the top N nodes.
#'
#' @seealso \code{\link{select_nodes}}, \code{\link{select_component}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Top 2 by degree
#' select_top(adj, n = 2)
#'
#' # Top 2 by PageRank
#' select_top(adj, n = 2, by = "pagerank")
select_top <- function(x, n, by = "degree", ...,
                       keep_edges = c("internal", "none"),
                       keep_format = FALSE, directed = NULL) {
  select_nodes(x, ..., top = n, by = by, keep_edges = keep_edges,
               keep_format = keep_format, directed = directed)
}


# =============================================================================
# select_edges() - Lazy Edge Selection
# =============================================================================

#' Select Edges with Lazy Computation
#'
#' A powerful edge selection function with lazy computation (only computes
#' metrics actually referenced), multiple selection modes, and structural
#' awareness (bridges, communities, reciprocity).
#'
#' @param x Network input: cograph_network, matrix, igraph, network, or tna object.
#' @param ... Filter expressions using edge columns or computed metrics.
#'   Available variables:
#'   \describe{
#'     \item{Edge columns}{\code{from}, \code{to}, \code{weight}, plus any custom}
#'     \item{Computed metrics}{\code{abs_weight}, \code{from_degree}, \code{to_degree},
#'       \code{from_strength}, \code{to_strength}, \code{edge_betweenness},
#'       \code{weight_rank}}
#'     \item{Predicates}{\code{is_bridge}, \code{is_mutual} (alias
#'       \code{is_reciprocal}), \code{is_loop}, \code{is_multiple},
#'       \code{same_community}}
#'     \item{Endpoint labels}{\code{from_label}, \code{to_label},
#'       \code{from_community}, \code{to_community}}
#'   }
#' @param top Integer. Select top N edges by a metric.
#' @param by Character. Metric for top selection. Default \code{"weight"}.
#'   Options: \code{"weight"}, \code{"abs_weight"}, \code{"edge_betweenness"},
#'   \code{"from_degree"}, \code{"to_degree"}, \code{"from_strength"},
#'   \code{"to_strength"}, \code{"weight_rank"}.
#' @param involving Character or integer. Select edges involving these nodes
#'   (by name or index). An edge is selected if either endpoint matches.
#' @param between List of two character/integer vectors. Select edges between
#'   two node sets. Example: \code{between = list(c("A", "B"), c("C", "D"))}.
#' @param bridges_only Logical. Select only bridge edges (edges whose removal
#'   disconnects the graph). Default FALSE.
#' @param mutual_only Logical. For directed networks, select only mutual
#'   (reciprocated) edges. Default FALSE.
#' @param community Character. Community detection method for \code{same_community}
#'   variable. One of \code{"louvain"}, \code{"walktrap"}, \code{"fast_greedy"},
#'   \code{"label_prop"}, \code{"infomap"}, \code{"leiden"}. Default \code{"louvain"}.
#' @param keep_isolates Logical. Keep nodes that end up with no edges?
#'   Default TRUE, matching \code{igraph::delete_edges()} and tidygraph:
#'   filtering edges does not remove nodes. Set FALSE to drop them, or call
#'   \code{\link{remove_isolates}()} afterwards.
#' @param .keep_isolates Deprecated. Use \code{keep_isolates}.
#' @param keep_format Logical. If TRUE, matrix, igraph, and statnet network
#'   inputs are returned in that format. Default FALSE returns cograph_network.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#'
#' @details
#' Selection modes are combined with AND logic:
#' \itemize{
#'   \item \code{select_edges(x, top = 10, involving = "A")} selects
#'     top 10 edges \strong{among those involving node A}
#'   \item All criteria must be satisfied for an edge to be selected
#' }
#'
#' Edge metrics are computed lazily - only those actually referenced in
#' expressions or required by selection modes are computed.
#'
#' @return A cograph_network object with selected edges. If \code{keep_format = TRUE},
#'   matrix, igraph, and statnet network inputs are converted back to that type.
#'
#' @seealso \code{\link{filter_edges}}, \code{\link{select_nodes}},
#'   \code{\link{select_bridges}}, \code{\link{select_top_edges}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0, .5, 0, .3, .6,
#'                 .8, .3, 0, .4, 0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' select_edges(adj, weight > 0.5)
#' select_edges(adj, top = 3)
#' select_edges(adj, involving = "A")
#' select_edges(adj, between = list(c("A", "B"), c("C", "D")))
select_edges <- function(x, ...,
                         top = NULL,
                         by = "weight",
                         involving = NULL,
                         between = NULL,
                         bridges_only = FALSE,
                         mutual_only = FALSE,
                         community = "louvain",
                         keep_isolates = TRUE,
                         keep_format = FALSE,
                         directed = NULL,
                         .keep_isolates = NULL) {
  keep_isolates <- .resolve_deprecated_arg(keep_isolates, .keep_isolates,
                                           ".keep_isolates", "keep_isolates")
  input_class <- .detect_input_class(x)
  net <- as_cograph(x, directed = directed)
  edges <- get_edges(net)
  n_total <- nrow(edges)

  if (n_total == 0) {
    warning("Network has no edges", call. = FALSE)
    return(.finish_result(net, x, input_class, keep_format))
  }

  g <- .cg_graph(net)
  nodes <- get_nodes(net)

  # Start with all edges selected
  selected <- rep(TRUE, n_total)

  # -------------------------
  # Apply selection modes (AND logic)
  # -------------------------

  # Mode 1: Involving specific nodes
  if (!is.null(involving)) {
    involving_match <- .select_edges_involving(edges, nodes, involving)
    selected <- selected & involving_match
  }

  # Mode 2: Between node sets
  if (!is.null(between)) {
    between_match <- .select_edges_between(edges, nodes, between)
    selected <- selected & between_match
  }

  # Mode 3: Bridges only
  if (bridges_only) {
    bridge_match <- .select_edges_bridges(g, n_total)
    selected <- selected & bridge_match
  }

  # Mode 4: Mutual only (directed networks)
  if (mutual_only) {
    mutual_match <- .select_edges_mutual(g, edges, n_total)
    selected <- selected & mutual_match
  }

  # Mode 5: Top N selection (applied to currently selected edges)
  if (!is.null(top)) {
    top_match <- .select_edges_top(g, edges, top, by, selected)
    selected <- selected & top_match
  }

  # Mode 6: Expression-based filtering
  dots <- substitute(list(...))[-1]
  if (length(dots) > 0) {
    expr_match <- .select_edges_by_expression(g, edges, nodes, dots, community, parent.frame())
    selected <- selected & expr_match
  }

  # -------------------------
  # Create result
  # -------------------------
  if (!any(selected)) {
    warning("No edges match the selection criteria.", call. = FALSE)
    if (!isTRUE(keep_isolates)) {
      empty <- .empty_cograph_network(net$directed, meta = net$meta, data = net$data)
      return(.finish_result(empty, x, input_class, keep_format))
    }
  }

  filtered_edges <- edges[selected, , drop = FALSE]
  result <- .update_cograph_edges(net, filtered_edges, keep_isolates = keep_isolates)

  if (isTRUE(keep_isolates)) {
    .warn_new_isolates(edges, filtered_edges, n_nodes(net))
  }

  .finish_result(result, x, input_class, keep_format)
}

# =============================================================================
# Edge Selection Helpers
# =============================================================================

#' Select edges involving specific nodes
#' @noRd
.select_edges_involving <- function(edges, nodes, involving) {
  node_idx <- .resolve_node_selection(nodes, involving, "involving")

  # Edge involves node if either endpoint matches
  edges$from %in% node_idx | edges$to %in% node_idx
}

#' Select edges between two node sets
#' @noRd
.select_edges_between <- function(edges, nodes, between) {
  if (!is.list(between) || length(between) != 2) {
    .stop_bad_selection("`between` must be a list of exactly two node sets; got ",
                        if (is.list(between)) paste0("a list of ", length(between))
                        else class(between)[1], ".")
  }

  idx1 <- .resolve_node_selection(nodes, between[[1]], "between[[1]]")
  idx2 <- .resolve_node_selection(nodes, between[[2]], "between[[2]]")

  if (length(idx1) == 0 || length(idx2) == 0) {
    .stop_bad_selection("`between` node sets must both be non-empty.")
  }

  # Edge is between sets if (from in set1 AND to in set2) OR (from in set2 AND to in set1)
  (edges$from %in% idx1 & edges$to %in% idx2) |
    (edges$from %in% idx2 & edges$to %in% idx1)
}

#' Select bridge edges
#' @noRd
.select_edges_bridges <- function(g, n_edges) {
  cg <- .cg_as_context(g)
  rows <- .cg_edge_rows(g)
  if (nrow(rows) == 0) {
    return(rep(FALSE, n_edges))
  }
  seq_len(n_edges) %in% which(.cg_bridges(cg$b, rows))
}

#' Select mutual (reciprocated) edges
#' @noRd
.select_edges_mutual <- function(g, edges, n_edges) {
  cg <- .cg_as_context(g)
  if (!cg$directed) {
    # All edges are "mutual" in undirected graphs
    return(rep(TRUE, n_edges))
  }

  .cg_reciprocated(edges$from, edges$to, cg$n)
}

#' Select top N edges by metric
#' @noRd
.select_edges_top <- function(g, edges, top, by, current_selection) {
  .validate_edge_metric(by, "by")
  metric_vals <- .compute_single_edge_metric(g, edges, by)

  if (all(is.na(metric_vals))) { # nocov start
    warning("Could not compute '", by, "' metric. Returning all currently selected edges.", call. = FALSE)
    return(current_selection)
  } # nocov end

  # Only consider currently selected edges
  masked_vals <- metric_vals
  masked_vals[!current_selection] <- -Inf

  # Get top N indices
  n_available <- sum(current_selection)
  top <- min(top, n_available)

  if (top <= 0) {
    return(rep(FALSE, length(current_selection)))
  }

  # Get indices of top values
  top_idx <- order(masked_vals, decreasing = TRUE)[seq_len(top)]

  seq_len(length(current_selection)) %in% top_idx
}

#' Select edges by expression (lazy computation)
#' @noRd
.select_edges_by_expression <- function(g, edges, nodes, dots, community_method, parent_frame) {
  n <- nrow(edges)

  # Detect what variables are needed from expressions
  needed_vars <- .detect_needed_edge_variables(dots)

  # Compute only needed edge metrics (lazy)
  edge_metrics <- .compute_lazy_edge_metrics(g, edges, nodes, needed_vars, community_method)

  # Build evaluation environment with edge columns + computed metrics
  eval_env <- .build_filter_env(edges, edge_metrics, parent_frame)

  # Evaluate filter conditions
  .evaluate_filter_conditions(dots, eval_env, n)
}

#' Detect needed edge variables from expressions
#' @noRd
.detect_needed_edge_variables <- function(exprs) {
  vocab <- .edge_vocabulary()
  computed_names <- setdiff(unlist(vocab, use.names = FALSE), "weight")
  all_vars <- unique(unlist(lapply(exprs, all.vars)))
  intersect(all_vars, computed_names)
}

#' Compute single edge metric
#' @noRd
.compute_single_edge_metric <- function(g, edges, metric) {
  cg <- .cg_as_context(g)
  switch(metric,
    "weight" = edges$weight,
    "abs_weight" = abs(edges$weight),
    "edge_betweenness" = .cg_edge_betweenness_rows(cg, edges),
    "from_degree" = .cg_degree(cg$b, cg$directed, "all")[edges$from],
    "to_degree" = .cg_degree(cg$b, cg$directed, "all")[edges$to],
    "from_strength" = .cg_strength(cg$w, cg$directed, "all")[edges$from],
    "to_strength" = .cg_strength(cg$w, cg$directed, "all")[edges$to],
    "weight_rank" = rank(edges$weight, ties.method = "min"),
    .stop_bad_selection(
      "Unknown edge metric '", metric, "'. Available: ",
      paste(.edge_vocabulary()$metric, collapse = ", "), "."
    )
  )
}

#' The edge vocabulary available inside edge filter expressions
#' @noRd
.edge_vocabulary <- function() {
  list(
    metric = c("weight", "abs_weight", "edge_betweenness", "from_degree",
               "to_degree", "from_strength", "to_strength", "weight_rank"),
    predicate = c("is_bridge", "is_mutual", "is_reciprocal", "is_loop",
                  "is_multiple", "same_community"),
    label = c("from_label", "to_label", "from_community", "to_community")
  )
}

#' Validate a node measure name for `by =`
#' @noRd
.validate_measure <- function(measure, arg) {
  if (!is.character(measure) || length(measure) != 1L) {
    .stop_bad_selection("`", arg, "` must be a single measure name.")
  }
  known <- c(.node_vocabulary()$centrality, .cg_delegable_measures())
  if (!measure %in% known) {
    .stop_bad_selection(
      "Unknown centrality measure '", measure, "' for `", arg, "`. ",
      "See list_centralities() for the measures cograph computes."
    )
  }
  invisible(measure)
}

#' Validate an edge metric name for `by =`
#' @noRd
.validate_edge_metric <- function(metric, arg) {
  if (!is.character(metric) || length(metric) != 1L) {
    .stop_bad_selection("`", arg, "` must be a single metric name.")
  }
  if (!metric %in% .edge_vocabulary()$metric) {
    .stop_bad_selection(
      "Unknown edge metric '", metric, "' for `", arg, "`. Available: ",
      paste(.edge_vocabulary()$metric, collapse = ", "), "."
    )
  }
  invisible(metric)
}

#' Compute only needed edge metrics (lazy)
#' @noRd
.compute_lazy_edge_metrics <- function(g, edges, nodes, needed, community_method) {
  if (length(needed) == 0) return(list())

  result <- list()
  n <- nrow(edges)
  cg <- .cg_as_context(g)
  is_dir <- cg$directed

  # Absolute weight
  if ("abs_weight" %in% needed) {
    result$abs_weight <- abs(edges$weight)
  }

  # Endpoint degrees
  if (any(c("from_degree", "to_degree") %in% needed)) {
    deg <- .cg_degree(cg$b, is_dir, "all")
    if ("from_degree" %in% needed) result$from_degree <- deg[edges$from]
    if ("to_degree" %in% needed) result$to_degree <- deg[edges$to]
  }

  # Endpoint strengths
  if (any(c("from_strength", "to_strength") %in% needed)) {
    str <- .cg_strength(cg$w, is_dir, "all")
    if ("from_strength" %in% needed) result$from_strength <- str[edges$from]
    if ("to_strength" %in% needed) result$to_strength <- str[edges$to]
  }

  # Edge betweenness
  if ("edge_betweenness" %in% needed) {
    result$edge_betweenness <- .cg_edge_betweenness_rows(cg, edges)
  }

  # Is bridge
  if ("is_bridge" %in% needed) {
    result$is_bridge <- .cg_bridges(cg$b, cbind(edges$from, edges$to))
  }

  # Is mutual (reciprocated); `is_reciprocal` is the igraph-flavoured alias.
  if (any(c("is_mutual", "is_reciprocal") %in% needed)) {
    mutual <- if (!is_dir) rep(TRUE, n) else
      .cg_reciprocated(edges$from, edges$to, cg$n)
    if ("is_mutual" %in% needed) result$is_mutual <- mutual
    if ("is_reciprocal" %in% needed) result$is_reciprocal <- mutual
  }

  # A loop joins a node to itself.
  if ("is_loop" %in% needed) {
    result$is_loop <- edges$from == edges$to
  }

  # A parallel edge: the same unordered (undirected) or ordered (directed)
  # pair appears more than once in the edge table.
  if ("is_multiple" %in% needed) {
    key <- if (is_dir) {
      paste(edges$from, edges$to, sep = "->")
    } else {
      paste(pmin(edges$from, edges$to), pmax(edges$from, edges$to), sep = "--")
    }
    result$is_multiple <- key %in% key[duplicated(key)]
  }

  # Rank of the edge weight, smallest first, ties sharing the lower rank.
  if ("weight_rank" %in% needed) {
    result$weight_rank <- rank(edges$weight, ties.method = "min")
  }

  # Community membership of the endpoints (community detection stays on igraph)
  comm_vars <- c("same_community", "from_community", "to_community")
  if (any(comm_vars %in% needed)) {
    .cg_need_igraph("same_community")
    comm_input <- if (inherits(g, "igraph")) g else cg$w
    comm <- detect_communities(comm_input, method = community_method,
                               directed = is_dir)
    membership <- comm$community
    if ("same_community" %in% needed) {
      result$same_community <- membership[edges$from] == membership[edges$to]
    }
    if ("from_community" %in% needed) result$from_community <- membership[edges$from]
    if ("to_community" %in% needed) result$to_community <- membership[edges$to]
  }

  # Endpoint labels (convenience)
  if ("from_label" %in% needed) {
    result$from_label <- nodes$label[edges$from]
  }
  if ("to_label" %in% needed) {
    result$to_label <- nodes$label[edges$to]
  }

  result
}

# =============================================================================
# Edge Selection Convenience Functions
# =============================================================================

#' Select Bridge Edges
#'
#' Select edges whose removal would disconnect the graph.
#'
#' @param x Network input.
#' @param ... Additional filter expressions.
#' @param keep_isolates Keep nodes that end up with no edges? Default TRUE.
#' @param keep_format Keep input format? Default FALSE.
#' @param directed Auto-detect if NULL.
#'
#' @return A cograph_network with bridge edges only.
#'
#' @seealso \code{\link{select_edges}}, \code{\link{select_nodes}}
#'
#' @export
#' @examples
#' # Create network with bridge
#' adj <- matrix(0, 5, 5)
#' adj[1, 2] <- adj[2, 1] <- 1
#' adj[2, 3] <- adj[3, 2] <- 1  # Bridge
#' adj[3, 4] <- adj[4, 3] <- 1
#' adj[4, 5] <- adj[5, 4] <- 1
#' adj[3, 5] <- adj[5, 3] <- 1
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#'
#' select_bridges(adj)
select_bridges <- function(x, ..., keep_isolates = TRUE,
                           keep_format = FALSE, directed = NULL) {
  select_edges(x, ..., bridges_only = TRUE, keep_isolates = keep_isolates,
               keep_format = keep_format, directed = directed)
}

#' Select Top N Edges
#'
#' Select the top N edges ranked by weight or another metric.
#'
#' @param x Network input.
#' @param n Integer. Number of top edges to select.
#' @param by Character. Metric for ranking. One of:
#'   \code{"weight"}, \code{"abs_weight"}, \code{"edge_betweenness"}.
#'   Default \code{"weight"}.
#' @param ... Additional filter expressions.
#' @param keep_isolates Keep nodes that end up with no edges? Default TRUE.
#' @param keep_format Keep input format? Default FALSE.
#' @param directed Auto-detect if NULL.
#'
#' @return A cograph_network with the top N edges.
#'
#' @seealso \code{\link{select_edges}}, \code{\link{select_top}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Top 3 edges by weight
#' select_top_edges(adj, n = 3)
#'
#' # Top 2 by edge betweenness
#' select_top_edges(adj, n = 2, by = "edge_betweenness")
select_top_edges <- function(x, n, by = "weight", ...,
                             keep_isolates = TRUE,
                             keep_format = FALSE, directed = NULL) {
  select_edges(x, ..., top = n, by = by, keep_isolates = keep_isolates,
               keep_format = keep_format, directed = directed)
}

#' Select Edges Involving Nodes
#'
#' Select edges where at least one endpoint is in the specified node set.
#'
#' @param x Network input.
#' @param nodes Character or integer. Node names or indices.
#' @param ... Additional filter expressions.
#' @param keep_isolates Keep nodes that end up with no edges? Default TRUE.
#' @param keep_format Keep input format? Default FALSE.
#' @param directed Auto-detect if NULL.
#'
#' @return A cograph_network with edges involving the specified nodes.
#'
#' @seealso \code{\link{select_edges}}, \code{\link{select_edges_between}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Edges involving A
#' select_edges_involving(adj, nodes = "A")
#'
#' # Edges involving A or B
#' select_edges_involving(adj, nodes = c("A", "B"))
select_edges_involving <- function(x, nodes, ...,
                                   keep_isolates = TRUE,
                                   keep_format = FALSE, directed = NULL) {
  select_edges(x, ..., involving = nodes, keep_isolates = keep_isolates,
               keep_format = keep_format, directed = directed)
}

#' Select Edges Between Node Sets
#'
#' Select edges connecting two specified node sets.
#'
#' @param x Network input.
#' @param set1 Character or integer. First node set (names or indices).
#' @param set2 Character or integer. Second node set (names or indices).
#' @param ... Additional filter expressions.
#' @param keep_isolates Keep nodes that end up with no edges? Default TRUE.
#' @param keep_format Keep input format? Default FALSE.
#' @param directed Auto-detect if NULL.
#'
#' @return A cograph_network with edges between the two node sets.
#'
#' @seealso \code{\link{select_edges}}, \code{\link{select_edges_involving}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Edges between {A, B} and {C, D}
#' select_edges_between(adj, set1 = c("A", "B"), set2 = c("C", "D"))
select_edges_between <- function(x, set1, set2, ...,
                                 keep_isolates = TRUE,
                                 keep_format = FALSE, directed = NULL) {
  select_edges(x, ..., between = list(set1, set2), keep_isolates = keep_isolates,
               keep_format = keep_format, directed = directed)
}


# =============================================================================
# Internal Helper Functions
# =============================================================================

#' Convert input to adjacency matrix (internal)
#'
#' @param x Network input.
#' @param directed Logical or NULL for directedness.
#' @return Adjacency matrix.
#' @noRd
to_adjacency_matrix <- function(x, directed = NULL) {
  to_matrix(x, directed = directed)
}
