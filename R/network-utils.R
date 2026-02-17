# =============================================================================
# Network Utility Functions
# =============================================================================

#' Convert Network to igraph Object
#'
#' Converts various network representations to an igraph object. Supports
#' matrices, igraph objects, network objects, cograph_network, and tna objects.
#'
#' @param x Network input. Can be:
#'   \itemize{
#'     \item A square numeric matrix (adjacency/weight matrix)
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

  if (inherits(x, "network")) {
    if (!requireNamespace("network", quietly = TRUE)) {
      stop("Package 'network' is required for network input. ",
           "Please install it with: install.packages('network')",
           call. = FALSE)
    }
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

  stop("x must be a matrix, igraph, network, cograph_network, or tna object",
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

  # Apply community detection algorithm
  communities <- switch(method,
    "louvain" = igraph::cluster_louvain(g, weights = edge_weights),
    "walktrap" = igraph::cluster_walktrap(g, weights = edge_weights),
    "fast_greedy" = {
      # fast_greedy requires undirected graph
      g_undirected <- igraph::as_undirected(g, mode = "collapse",
                                             edge.attr.comb = "mean")
      igraph::cluster_fast_greedy(g_undirected, weights = edge_weights)
    },
    "label_prop" = igraph::cluster_label_prop(g, weights = edge_weights),
    "infomap" = igraph::cluster_infomap(g, e.weights = edge_weights),
    "leiden" = {
      if (!requireNamespace("igraph", quietly = TRUE) ||
          !exists("cluster_leiden", where = asNamespace("igraph"))) {
        stop("Leiden algorithm requires igraph >= 1.2.5", call. = FALSE)
      }
      igraph::cluster_leiden(g, weights = edge_weights)
    }
  )

  # Get node labels
  labels <- if (!is.null(igraph::V(g)$name)) {
    igraph::V(g)$name
  } else {
    as.character(seq_len(igraph::vcount(g)))
  }

  # Create result data frame
  data.frame(
    node = labels,
    community = as.integer(igraph::membership(communities)),
    stringsAsFactors = FALSE
  )
}

#' Color Nodes by Community
#'
#' Generate colors for nodes based on community membership. Designed for
#' direct use with \code{splot()} node.color parameter.
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
#'   use with \code{splot()} node.color parameter.
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
#' cograph_network object by default (universal format), or optionally the
#' same format as input when \code{keep_format = TRUE}.
#'
#' @param x Network input: cograph_network, matrix, igraph, network, or tna object.
#' @param ... Filter expressions using any edge column (e.g., \code{weight > 0.5},
#'   \code{weight > mean(weight)}, \code{abs(weight) > 0.3}).
#' @param .keep_isolates Logical. Keep nodes with no remaining edges? Default FALSE.
#' @param keep_format Logical. If TRUE, return the same format as input
#'   (matrix returns matrix, igraph returns igraph, etc.). Default FALSE
#'   returns cograph_network (universal format).
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#'   Only used for non-cograph_network inputs.
#'
#' @return A cograph_network object with filtered edges. If \code{keep_format = TRUE},
#'   returns the same type as input (matrix, igraph, network, etc.).
#'
#' @seealso \code{\link{filter_nodes}}, \code{\link{splot}}, \code{\link{subset_edges}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Keep only strong edges (returns cograph_network)
#' filter_edges(adj, weight > 0.5)
#'
#' # Keep format: matrix in, matrix out
#' filter_edges(adj, weight > 0.5, keep_format = TRUE)
#'
#' # Keep edges above mean weight
#' splot(filter_edges(adj, weight >= mean(weight)))
#'
#' # With cograph_network (pipe-friendly)
#' net <- as_cograph(adj)
#' net |>
#'   filter_edges(weight > 0.3) |>
#'   filter_nodes(degree >= 2) |>
#'   splot()
#'
#' # Keep isolated nodes
#' filter_edges(net, weight > 0.7, .keep_isolates = TRUE)
#'
#' # With igraph (keep_format = TRUE returns igraph)
#' \dontrun{
#' g <- igraph::make_ring(5)
#' filter_edges(g, weight > 0, keep_format = TRUE)  # Returns igraph
#' }
filter_edges <- function(x, ..., .keep_isolates = FALSE, keep_format = FALSE,
                         directed = NULL) {
  # Detect input format for keep_format option
  input_class <- .detect_input_class(x)

  # Warn if converting complex formats to cograph_network
  if (!keep_format && input_class %in% c("igraph", "network", "qgraph")) {
    message("Result converted to cograph_network. Use keep_format = TRUE to return ", input_class, ".")
  }

  # Convert to cograph_network if needed
  net <- as_cograph(x, directed = directed)

  # Get edges dataframe
  edges <- get_edges(net)

  if (nrow(edges) == 0) {
    warning("Network has no edges", call. = FALSE)
    if (keep_format) {
      return(.convert_to_format(net, input_class))
    }
    return(net)
  }

  # Build evaluation environment with all edge columns
  eval_env <- list2env(as.list(edges), parent = parent.frame())

  # Capture filter expressions
  dots <- substitute(list(...))[-1]

  # Evaluate filter conditions
  mask <- .evaluate_filter_conditions(dots, eval_env, nrow(edges))

  # Apply filter
  filtered_edges <- edges[mask, , drop = FALSE]

  # Update network
  result <- .update_cograph_edges(net, filtered_edges, keep_isolates = .keep_isolates)

  # Warn if result is empty
  if (n_nodes(result) == 0) {
    warning("Filter removed all nodes. Result may not be usable for plotting.", call. = FALSE)
  } else if (n_edges(result) == 0) {
    warning("Filter removed all edges.", call. = FALSE)
  }

  # Return in original format if requested
  if (keep_format) {
    return(.convert_to_format(result, input_class))
  }

  result
}

#' Filter Nodes by Metadata or Centrality
#'
#' Filter nodes using dplyr-style expressions on any node column or centrality
#' measure. Returns a cograph_network object by default (universal format), or
#' optionally the same format as input when \code{keep_format = TRUE}.
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
#' @param .keep_edges How to handle edges. One of:
#'   \describe{
#'     \item{\code{"internal"}}{(default) Keep only edges between remaining nodes}
#'     \item{\code{"none"}}{Remove all edges}
#'   }
#' @param keep_format Logical. If TRUE, return the same format as input
#'   (matrix returns matrix, igraph returns igraph, etc.). Default FALSE
#'   returns cograph_network (universal format).
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#'   Only used for non-cograph_network inputs.
#'
#' @return A cograph_network object with filtered nodes. If \code{keep_format = TRUE},
#'   returns the same type as input (matrix, igraph, network, etc.).
#'
#' @seealso \code{\link{filter_edges}}, \code{\link{splot}}, \code{\link{subset_nodes}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Keep only high-degree nodes (returns cograph_network)
#' filter_nodes(adj, degree >= 3)
#'
#' # Keep format: matrix in, matrix out
#' filter_nodes(adj, degree >= 3, keep_format = TRUE)
#'
#' # Filter by node label
#' splot(filter_nodes(adj, label %in% c("A", "C")))
#'
#' # Combine centrality and metadata filters
#' splot(filter_nodes(adj, degree >= 2 & label != "D"))
#'
#' # With cograph_network (pipe-friendly)
#' net <- as_cograph(adj)
#' net |>
#'   filter_edges(weight > 0.3) |>
#'   filter_nodes(degree >= 2) |>
#'   splot()
#'
#' # With igraph (keep_format = TRUE returns igraph)
#' \dontrun{
#' g <- igraph::make_ring(5)
#' filter_nodes(g, degree >= 2, keep_format = TRUE)  # Returns igraph
#' }
filter_nodes <- function(x, ..., .keep_edges = c("internal", "none"),
                         keep_format = FALSE, directed = NULL) {
  .keep_edges <- match.arg(.keep_edges)

  # Detect input format for keep_format option
  input_class <- .detect_input_class(x)

  # Warn if converting complex formats to cograph_network
  if (!keep_format && input_class %in% c("igraph", "network", "qgraph")) {
    message("Result converted to cograph_network. Use keep_format = TRUE to return ", input_class, ".")
  }

  # Convert to cograph_network if needed
  net <- as_cograph(x, directed = directed)

  # Get nodes dataframe
  nodes <- get_nodes(net)

  # Calculate centrality measures and add to environment
  g <- to_igraph(net)
  centrality_vars <- .compute_centrality_vars(g)

  # Build evaluation environment with:
  # 1. All node columns
  # 2. All centrality measures
  # 3. Parent frame for user variables
  eval_env <- .build_filter_env(nodes, centrality_vars, parent.frame())

  # Capture filter expressions
  dots <- substitute(list(...))[-1]

  # Evaluate filter conditions
  mask <- .evaluate_filter_conditions(dots, eval_env, nrow(nodes))

  # Apply filter
  selected_idx <- which(mask)

  if (length(selected_idx) == 0) {
    warning("No nodes match the filter criteria. Result may not be usable for plotting.", call. = FALSE)
    if (keep_format && input_class == "matrix") {
      return(matrix(0, nrow = 0, ncol = 0))
    }
    empty <- .empty_cograph_network(net$directed)
    if (keep_format) {
      return(.convert_to_format(empty, input_class))
    }
    return(empty)
  }

  # Create subgraph
  result <- .subset_cograph_network(net, nodes = selected_idx, keep_edges = .keep_edges)

  # Return in original format if requested
  if (keep_format) {
    return(.convert_to_format(result, input_class))
  }

  result
}

#' @rdname filter_nodes
#' @export
subset_nodes <- filter_nodes

#' @rdname filter_edges
#' @export
subset_edges <- filter_edges

# =============================================================================
# Helper Functions for Filtering
# =============================================================================

#' Compute Centrality Variables for Filtering
#' @noRd
.compute_centrality_vars <- function(g) {
  is_dir <- igraph::is_directed(g)
  weights <- igraph::E(g)$weight
  n <- igraph::vcount(g)

  list(
    # Degree measures
    degree = igraph::degree(g, mode = "all"),
    indegree = igraph::degree(g, mode = "in"),
    outdegree = igraph::degree(g, mode = "out"),

    # Strength measures
    strength = igraph::strength(g, mode = "all", weights = weights),
    instrength = igraph::strength(g, mode = "in", weights = weights),
    outstrength = igraph::strength(g, mode = "out", weights = weights),

    # Other centrality
    betweenness = igraph::betweenness(g, weights = weights, directed = is_dir),
    closeness = tryCatch(
      igraph::closeness(g, mode = "all", weights = weights),
      error = function(e) rep(NA_real_, n)
    ),
    eigenvector = tryCatch(
      igraph::eigen_centrality(g, weights = weights, directed = is_dir)$vector,
      error = function(e) rep(NA_real_, n)
    ),
    pagerank = igraph::page_rank(g, weights = weights, directed = is_dir)$vector,
    hub = tryCatch(
      igraph::hits_scores(g, weights = weights)$hub,
      error = function(e) rep(NA_real_, n)
    ),
    authority = tryCatch(
      igraph::hits_scores(g, weights = weights)$authority,
      error = function(e) rep(NA_real_, n)
    )
  )
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
    result[is.na(result)] <- FALSE
    result
  })

  Reduce(`&`, masks)
}

#' Create Empty cograph_network
#' @noRd
.empty_cograph_network <- function(directed = FALSE) {
  .create_cograph_network(
    nodes = data.frame(id = integer(0), label = character(0)),
    edges = data.frame(from = integer(0), to = integer(0), weight = numeric(0)),
    directed = directed,
    meta = list(source = "filtered")
  )
}

#' Subset cograph_network by Node Indices
#' @noRd
.subset_cograph_network <- function(net, nodes, keep_edges = "internal") {
  # Get current data
  node_df <- get_nodes(net)
  edge_df <- get_edges(net)

  # Filter nodes
  new_nodes <- node_df[nodes, , drop = FALSE]
  new_nodes$id <- seq_len(nrow(new_nodes))
  rownames(new_nodes) <- NULL

  # Create index mapping (old -> new)
  node_map <- stats::setNames(seq_along(nodes), nodes)

  # Filter edges
  if (keep_edges == "internal" && nrow(edge_df) > 0) {
    # Keep edges where both endpoints are in selection
    keep_edge <- edge_df$from %in% nodes & edge_df$to %in% nodes
    new_edges <- edge_df[keep_edge, , drop = FALSE]

    # Remap node indices
    if (nrow(new_edges) > 0) {
      new_edges$from <- as.integer(node_map[as.character(new_edges$from)])
      new_edges$to <- as.integer(node_map[as.character(new_edges$to)])
      rownames(new_edges) <- NULL
    }
  } else {
    new_edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  }

  # Build new weight matrix
  n <- nrow(new_nodes)
  new_weights <- matrix(0, n, n, dimnames = list(new_nodes$label, new_nodes$label))
  if (nrow(new_edges) > 0) {
    for (i in seq_len(nrow(new_edges))) {
      new_weights[new_edges$from[i], new_edges$to[i]] <- new_edges$weight[i]
    }
  }

  # Create new cograph_network using internal constructor
  .create_cograph_network(
    nodes = new_nodes,
    edges = new_edges,
    directed = net$directed,
    meta = list(source = "filtered", tna = net$meta$tna),
    weights = new_weights
  )
}

#' Update Edges in cograph_network
#' @noRd
.update_cograph_edges <- function(net, new_edges, keep_isolates = FALSE) {
  nodes <- get_nodes(net)

  # Handle case where all edges are removed and keep_isolates = FALSE
  if (!keep_isolates && nrow(new_edges) == 0) {
    return(.empty_cograph_network(net$directed))
  }

  # Remove isolates if requested - but preserve the filtered edges
  if (!keep_isolates && nrow(new_edges) > 0) {
    connected_nodes <- sort(unique(c(new_edges$from, new_edges$to)))
    if (length(connected_nodes) < nrow(nodes)) {
      # Subset nodes to only connected ones
      new_nodes <- nodes[connected_nodes, , drop = FALSE]
      new_nodes$id <- seq_len(nrow(new_nodes))
      rownames(new_nodes) <- NULL

      # Create index mapping (old -> new)
      node_map <- stats::setNames(seq_along(connected_nodes), connected_nodes)

      # Remap edge indices to new node indices
      remapped_edges <- new_edges
      remapped_edges$from <- as.integer(node_map[as.character(new_edges$from)])
      remapped_edges$to <- as.integer(node_map[as.character(new_edges$to)])
      rownames(remapped_edges) <- NULL

      # Build new weight matrix
      n <- nrow(new_nodes)
      new_weights <- matrix(0, n, n, dimnames = list(new_nodes$label, new_nodes$label))
      for (i in seq_len(nrow(remapped_edges))) {
        new_weights[remapped_edges$from[i], remapped_edges$to[i]] <- remapped_edges$weight[i]
      }

      return(.create_cograph_network(
        nodes = new_nodes,
        edges = remapped_edges,
        directed = net$directed,
        meta = list(source = "filtered", tna = net$meta$tna),
        weights = new_weights
      ))
    }
  }

  # Reset row names
  rownames(new_edges) <- NULL

  # Update weights matrix
  n <- nrow(nodes)
  new_weights <- matrix(0, n, n, dimnames = list(nodes$label, nodes$label))
  if (nrow(new_edges) > 0) {
    for (i in seq_len(nrow(new_edges))) {
      new_weights[new_edges$from[i], new_edges$to[i]] <- new_edges$weight[i]
    }
  }

  # Create updated cograph_network
  .create_cograph_network(
    nodes = nodes,
    edges = new_edges,
    directed = net$directed,
    meta = list(
      source = "filtered",
      layout = net$meta$layout,
      tna = net$meta$tna
    ),
    weights = new_weights
  )
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
#' @noRd
.convert_to_format <- function(net, format) {
switch(format,
    matrix = to_matrix(net),
    igraph = to_igraph(net),
    network = to_network(net),
    # For tna/qgraph/unknown, return cograph_network (can't reconstruct original)
    net
  )
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

  # Convert to igraph
  g <- to_igraph(x, directed = directed)

  # Get edge list (returns character names if vertices have names)
  edges <- igraph::as_edgelist(g)

  if (nrow(edges) == 0) {
    return(data.frame(
      from = character(0),
      to = character(0),
      weight = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Get weights
  weights <- if (!is.null(igraph::E(g)$weight)) {
    igraph::E(g)$weight
  } else {
    rep(1, nrow(edges))
  }

  # Build data frame - edges already contains node names/IDs
  df <- data.frame(
    from = edges[, 1],
    to = edges[, 2],
    weight = weights,
    stringsAsFactors = FALSE
  )

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
#' @return A square numeric adjacency matrix with row/column names.
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
#' # From igraph
#' \dontrun{
#' g <- igraph::make_ring(5)
#' to_matrix(g)
#' }
to_matrix <- function(x, directed = NULL) {

  # If already a matrix, return as-is
  if (is.matrix(x)) {
    return(x)

  }

  # Convert to igraph first
  g <- to_igraph(x, directed = directed)

  # Convert igraph to adjacency matrix
  adj <- igraph::as_adjacency_matrix(g, type = "both", attr = "weight", sparse = FALSE)

  # Preserve row/column names
  labels <- igraph::V(g)$name
  if (!is.null(labels)) {
    rownames(adj) <- labels
    colnames(adj) <- labels
  }

  adj
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
#' \dontrun{
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' net <- to_network(adj)
#' }
to_network <- function(x, directed = NULL) {

  if (!requireNamespace("network", quietly = TRUE)) {
    stop("Package 'network' is required for to_network(). ",
         "Install it with: install.packages('network')", call. = FALSE)
  }

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
#'       \code{authority}, \code{coreness}}
#'     \item{Global context}{\code{component}, \code{component_size},
#'       \code{is_largest_component}, \code{neighborhood_size}, \code{k_core},
#'       \code{is_articulation}, \code{is_bridge_endpoint}}
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
#' @param .keep_edges How to handle edges. One of:
#'   \describe{
#'     \item{\code{"internal"}}{(default) Keep only edges between remaining nodes}
#'     \item{\code{"none"}}{Remove all edges}
#'   }
#' @param keep_format Logical. If TRUE, return the same format as input.
#'   Default FALSE returns cograph_network.
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
#'   returns the same type as input.
#'
#' @seealso \code{\link{filter_nodes}}, \code{\link{select_neighbors}},
#'   \code{\link{select_component}}, \code{\link{select_top}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Lazy - only computes degree
#' select_nodes(adj, degree >= 3)
#'
#' # Global context - computes component info
#' select_nodes(adj, is_largest_component & degree >= 2)
#'
#' # By name
#' select_nodes(adj, name = c("A", "B", "C"))
#'
#' # Top 2 by PageRank
#' select_nodes(adj, top = 2, by = "pagerank")
#'
#' # Neighborhood of "A" up to 2 hops
#' select_nodes(adj, neighbors_of = "A", order = 2)
#'
#' # Largest connected component
#' select_nodes(adj, component = "largest")
#'
#' # Combined: top 2 in largest component
#' select_nodes(adj, component = "largest", top = 2, by = "degree")
#'
#' # Articulation points with high degree
#' # select_nodes(adj, is_articulation & degree >= 2)
select_nodes <- function(x, ...,
                         name = NULL,
                         index = NULL,
                         top = NULL,
                         by = "degree",
                         neighbors_of = NULL,
                         order = 1L,
                         component = NULL,
                         .keep_edges = c("internal", "none"),
                         keep_format = FALSE,
                         directed = NULL) {
  .keep_edges <- match.arg(.keep_edges)

  # Detect input format for keep_format option
  input_class <- .detect_input_class(x)

  # Warn if converting complex formats to cograph_network
  if (!keep_format && input_class %in% c("igraph", "network", "qgraph")) {
    message("Result converted to cograph_network. Use keep_format = TRUE to return ", input_class, ".")
  }

  # Convert to cograph_network if needed
  net <- as_cograph(x, directed = directed)
  n_total <- n_nodes(net)

  if (n_total == 0) {
    warning("Network has no nodes", call. = FALSE)
    if (keep_format) {
      return(.convert_to_format(net, input_class))
    }
    return(net)
  }

  # Get igraph representation for centrality computations
  g <- to_igraph(net)

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
    if (keep_format && input_class == "matrix") {
      return(matrix(0, nrow = 0, ncol = 0))
    }
    empty <- .empty_cograph_network(net$directed)
    if (keep_format) {
      return(.convert_to_format(empty, input_class))
    }
    return(empty)
  }

  # Create subgraph
  result <- .subset_cograph_network(net, nodes = selected_idx, keep_edges = .keep_edges)

  # Return in original format if requested
  if (keep_format) {
    return(.convert_to_format(result, input_class))
  }

  result
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
  # Validate indices
  valid_idx <- indices[indices >= 1 & indices <= n_total]
  if (length(valid_idx) < length(indices)) {
    warning("Some indices are out of range and were ignored.", call. = FALSE)
  }
  seq_len(n_total) %in% valid_idx
}

#' Select nodes by component
#' @noRd
.select_by_component <- function(g, nodes, component) {
  # Get component membership
  comp <- igraph::components(g)
  membership <- comp$membership

  if (identical(component, "largest")) {
    # Find largest component
    largest_comp <- which.max(comp$csize)
    return(membership == largest_comp)
  } else if (is.numeric(component)) {
    # Select by component ID
    if (component < 1 || component > comp$no) {
      warning("Component ID ", component, " does not exist. Network has ",
              comp$no, " components.", call. = FALSE)
      return(rep(FALSE, length(membership)))
    }
    return(membership == component)
  } else if (is.character(component)) {
    # Select component containing node with this name
    node_idx <- which(nodes$label == component)
    if (length(node_idx) == 0) {
      warning("Node '", component, "' not found.", call. = FALSE)
      return(rep(FALSE, length(membership)))
    }
    target_comp <- membership[node_idx[1]]
    return(membership == target_comp)
  }

  rep(TRUE, length(membership))
}

#' Select neighbors of specified nodes
#' @noRd
.select_by_neighbors <- function(g, nodes, of, order) {
  # Resolve node IDs
  if (is.character(of)) {
    node_idx <- which(nodes$label %in% of)
    if (length(node_idx) == 0) {
      warning("No nodes found matching: ", paste(of, collapse = ", "), call. = FALSE)
      return(rep(FALSE, nrow(nodes)))
    }
  } else {
    node_idx <- of[of >= 1 & of <= nrow(nodes)]
    if (length(node_idx) < length(of)) {
      warning("Some indices are out of range.", call. = FALSE)
    }
  }

  # Get ego network (includes the focal nodes themselves)
  ego_verts <- igraph::ego(g, order = order, nodes = node_idx, mode = "all")
  all_neighbors <- unique(unlist(lapply(ego_verts, as.integer)))

  seq_len(nrow(nodes)) %in% all_neighbors
}

#' Select top N nodes by centrality
#' @noRd
.select_by_top <- function(g, nodes, top, by, current_selection) {
  # Compute the required centrality measure
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
  n <- nrow(nodes)

  # Detect what variables are needed from expressions
  needed_vars <- .detect_needed_variables(dots)

  # Check for negative weights
  weights <- igraph::E(g)$weight
  has_negative <- !is.null(weights) && any(weights < 0, na.rm = TRUE)

  # Compute only needed centrality measures (lazy)
  centrality_vars <- .compute_lazy_centralities(g, needed_vars$centrality, has_negative)

  # Compute only needed global context variables (lazy)
  context_vars <- .compute_lazy_context(g, needed_vars$context)

  # Build evaluation environment
  eval_env <- .build_filter_env(nodes, c(centrality_vars, context_vars), parent_frame)

  # Evaluate filter conditions
  .evaluate_filter_conditions(dots, eval_env, n)
}

# =============================================================================
# Lazy Computation Helpers
# =============================================================================

#' Detect needed variables from expressions
#' @noRd
.detect_needed_variables <- function(exprs) {
  centrality_names <- c("degree", "indegree", "outdegree", "strength",
                        "instrength", "outstrength", "betweenness",
                        "closeness", "eigenvector", "pagerank", "hub",
                        "authority", "coreness")

  context_names <- c("component", "component_size", "is_largest_component",
                     "neighborhood_size", "k_core", "is_articulation",
                     "is_bridge_endpoint")

  # Get all variables referenced in expressions
  all_vars <- unique(unlist(lapply(exprs, all.vars)))

  list(
    centrality = intersect(all_vars, centrality_names),
    context = intersect(all_vars, context_names)
  )
}

#' Compute single centrality measure
#' @noRd
.compute_single_centrality <- function(g, measure) {
  n <- igraph::vcount(g)
  is_dir <- igraph::is_directed(g)
  weights <- igraph::E(g)$weight
  has_negative <- !is.null(weights) && any(weights < 0, na.rm = TRUE)

  switch(measure,
    "degree" = igraph::degree(g, mode = "all"),
    "indegree" = igraph::degree(g, mode = "in"),
    "outdegree" = igraph::degree(g, mode = "out"),
    "strength" = igraph::strength(g, mode = "all", weights = weights),
    "instrength" = igraph::strength(g, mode = "in", weights = weights),
    "outstrength" = igraph::strength(g, mode = "out", weights = weights),
    "betweenness" = {
      if (has_negative) {
        warning("Betweenness cannot be computed with negative weights. Returning NA.", call. = FALSE)
        rep(NA_real_, n)
      } else {
        igraph::betweenness(g, weights = weights, directed = is_dir)
      }
    },
    "closeness" = {
      if (has_negative) {
        warning("Closeness cannot be computed with negative weights. Returning NA.", call. = FALSE)
        rep(NA_real_, n)
      } else {
        tryCatch(
          igraph::closeness(g, mode = "all", weights = weights),
          error = function(e) rep(NA_real_, n)
        )
      }
    },
    "eigenvector" = tryCatch(
      igraph::eigen_centrality(g, weights = weights, directed = is_dir)$vector,
      error = function(e) rep(NA_real_, n)
    ),
    "pagerank" = igraph::page_rank(g, weights = weights, directed = is_dir)$vector,
    "hub" = tryCatch(
      igraph::hits_scores(g, weights = weights)$hub,
      error = function(e) rep(NA_real_, n)
    ),
    "authority" = tryCatch(
      igraph::hits_scores(g, weights = weights)$authority,
      error = function(e) rep(NA_real_, n)
    ),
    "coreness" = igraph::coreness(g, mode = "all"),
    # Default: degree
    igraph::degree(g, mode = "all")
  )
}

#' Compute only needed centrality measures (lazy)
#' @noRd
.compute_lazy_centralities <- function(g, needed, has_negative) {
  if (length(needed) == 0) return(list())

  result <- list()
  n <- igraph::vcount(g)

  for (measure in needed) {
    result[[measure]] <- .compute_single_centrality(g, measure)
  }

  result
}

#' Compute only needed global context variables (lazy)
#' @noRd
.compute_lazy_context <- function(g, needed) {
  if (length(needed) == 0) return(list())

  result <- list()
  n <- igraph::vcount(g)

  # Component-related variables
  comp_vars <- c("component", "component_size", "is_largest_component")
  if (any(needed %in% comp_vars)) {
    comp <- igraph::components(g)
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
    result$neighborhood_size <- igraph::degree(g, mode = "all")
  }

  # K-core
  if ("k_core" %in% needed) {
    result$k_core <- igraph::coreness(g, mode = "all")
  }

  # Articulation points
  if ("is_articulation" %in% needed) {
    # articulation_points returns vertex sequence, convert to logical
    art_points <- igraph::articulation_points(g)
    result$is_articulation <- seq_len(n) %in% as.integer(art_points)
  }

  # Bridge endpoints
  if ("is_bridge_endpoint" %in% needed) {
    # bridges returns edge sequence
    bridge_edges <- igraph::bridges(g)
    if (length(bridge_edges) > 0) {
      # Get endpoints - returns names if graph has names, indices otherwise
      edge_list <- igraph::ends(g, bridge_edges, names = FALSE)
      bridge_nodes <- unique(as.vector(edge_list))
      result$is_bridge_endpoint <- seq_len(n) %in% bridge_nodes
    } else {
      result$is_bridge_endpoint <- rep(FALSE, n)
    }
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
#' @param .keep_edges How to handle edges. Default "internal".
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
                             .keep_edges = c("internal", "none"),
                             keep_format = FALSE, directed = NULL) {
  select_nodes(x, ..., neighbors_of = of, order = order,
               .keep_edges = .keep_edges, keep_format = keep_format,
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
#' @param .keep_edges How to handle edges. Default "internal".
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
                             .keep_edges = c("internal", "none"),
                             keep_format = FALSE, directed = NULL) {
  select_nodes(x, ..., component = which, .keep_edges = .keep_edges,
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
#' @param .keep_edges How to handle edges. Default "internal".
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
                       .keep_edges = c("internal", "none"),
                       keep_format = FALSE, directed = NULL) {
  select_nodes(x, ..., top = n, by = by, .keep_edges = .keep_edges,
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
#'       \code{is_bridge}, \code{is_mutual}, \code{same_community}}
#'   }
#' @param top Integer. Select top N edges by a metric.
#' @param by Character. Metric for top selection. Default \code{"weight"}.
#'   Options: \code{"weight"}, \code{"abs_weight"}, \code{"edge_betweenness"}.
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
#' @param .keep_isolates Logical. Keep nodes with no remaining edges? Default FALSE.
#' @param keep_format Logical. If TRUE, return the same format as input.
#'   Default FALSE returns cograph_network.
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
#'   returns the same type as input.
#'
#' @seealso \code{\link{filter_edges}}, \code{\link{select_nodes}},
#'   \code{\link{select_bridges}}, \code{\link{select_top_edges}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Expression-based (lazy - only computes what's needed)
#' select_edges(adj, weight > 0.5)
#' select_edges(adj, abs_weight > 0.4)
#'
#' # Top N edges by weight
#' select_edges(adj, top = 3)
#' select_edges(adj, top = 3, by = "edge_betweenness")
#'
#' # Edges involving specific nodes
#' select_edges(adj, involving = "A")
#' select_edges(adj, involving = c("A", "B"))
#'
#' # Edges between two node sets
#' select_edges(adj, between = list(c("A", "B"), c("C", "D")))
#'
#' # Bridge edges only
#' select_edges(adj, bridges_only = TRUE)
#'
#' # Combined: top 3 edges involving A
#' select_edges(adj, involving = "A", top = 3)
#'
#' # Using endpoint degrees
#' select_edges(adj, from_degree >= 3 | to_degree >= 3)
#'
#' # Within-community edges
#' select_edges(adj, same_community)
select_edges <- function(x, ...,
                         top = NULL,
                         by = "weight",
                         involving = NULL,
                         between = NULL,
                         bridges_only = FALSE,
                         mutual_only = FALSE,
                         community = "louvain",
                         .keep_isolates = FALSE,
                         keep_format = FALSE,
                         directed = NULL) {

  # Detect input format for keep_format option
input_class <- .detect_input_class(x)

  # Warn if converting complex formats
  if (!keep_format && input_class %in% c("igraph", "network", "qgraph")) {
    message("Result converted to cograph_network. Use keep_format = TRUE to return ", input_class, ".")
  }

  # Convert to cograph_network if needed
  net <- as_cograph(x, directed = directed)
  edges <- get_edges(net)
  n_total <- nrow(edges)

  if (n_total == 0) {
    warning("Network has no edges", call. = FALSE)
    if (keep_format) {
      return(.convert_to_format(net, input_class))
    }
    return(net)
  }

  # Get igraph representation for metric computations
  g <- to_igraph(net)
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
    if (!.keep_isolates) {
      empty <- .empty_cograph_network(net$directed)
      if (keep_format) {
        return(.convert_to_format(empty, input_class))
      }
      return(empty)
    }
  }

  # Filter edges
  filtered_edges <- edges[selected, , drop = FALSE]

  # Update network
  result <- .update_cograph_edges(net, filtered_edges, keep_isolates = .keep_isolates)

  # Return in original format if requested
  if (keep_format) {
    return(.convert_to_format(result, input_class))
  }

  result
}

# =============================================================================
# Edge Selection Helpers
# =============================================================================

#' Select edges involving specific nodes
#' @noRd
.select_edges_involving <- function(edges, nodes, involving) {
  # Resolve node indices
  if (is.character(involving)) {
    node_idx <- which(nodes$label %in% involving)
    if (length(node_idx) == 0) {
      warning("No nodes found matching: ", paste(involving, collapse = ", "), call. = FALSE)
      return(rep(FALSE, nrow(edges)))
    }
  } else {
    node_idx <- involving[involving >= 1 & involving <= nrow(nodes)]
  }

  # Edge involves node if either endpoint matches
  edges$from %in% node_idx | edges$to %in% node_idx
}

#' Select edges between two node sets
#' @noRd
.select_edges_between <- function(edges, nodes, between) {
  if (!is.list(between) || length(between) != 2) {
    warning("'between' must be a list of two node sets", call. = FALSE)
    return(rep(TRUE, nrow(edges)))
  }

  # Resolve both node sets
  set1 <- between[[1]]
  set2 <- between[[2]]

  if (is.character(set1)) {
    idx1 <- which(nodes$label %in% set1)
  } else {
    idx1 <- set1[set1 >= 1 & set1 <= nrow(nodes)]
  }

  if (is.character(set2)) {
    idx2 <- which(nodes$label %in% set2)
  } else {
    idx2 <- set2[set2 >= 1 & set2 <= nrow(nodes)]
  }

  if (length(idx1) == 0 || length(idx2) == 0) {
    warning("One or both node sets are empty", call. = FALSE)
    return(rep(FALSE, nrow(edges)))
  }

  # Edge is between sets if (from in set1 AND to in set2) OR (from in set2 AND to in set1)
  (edges$from %in% idx1 & edges$to %in% idx2) |
    (edges$from %in% idx2 & edges$to %in% idx1)
}

#' Select bridge edges
#' @noRd
.select_edges_bridges <- function(g, n_edges) {
  bridge_edges <- igraph::bridges(g)
  if (length(bridge_edges) == 0) {
    return(rep(FALSE, n_edges))
  }
  seq_len(n_edges) %in% as.integer(bridge_edges)
}

#' Select mutual (reciprocated) edges
#' @noRd
.select_edges_mutual <- function(g, edges, n_edges) {
  if (!igraph::is_directed(g)) {
    # All edges are "mutual" in undirected graphs
    return(rep(TRUE, n_edges))
  }

  # Check for reciprocal edges
  is_mutual <- logical(n_edges)
  for (i in seq_len(n_edges)) {
    from_node <- edges$from[i]
    to_node <- edges$to[i]
    # Check if reverse edge exists
    is_mutual[i] <- any(edges$from == to_node & edges$to == from_node)
  }
  is_mutual
}

#' Select top N edges by metric
#' @noRd
.select_edges_top <- function(g, edges, top, by, current_selection) {
  # Compute the required metric
  metric_vals <- .compute_single_edge_metric(g, edges, by)

  if (all(is.na(metric_vals))) {
    warning("Could not compute '", by, "' metric. Returning all currently selected edges.", call. = FALSE)
    return(current_selection)
  }

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
  computed_names <- c("abs_weight", "from_degree", "to_degree",
                      "from_strength", "to_strength", "edge_betweenness",
                      "is_bridge", "is_mutual", "same_community",
                      "from_label", "to_label")

  # Get all variables referenced in expressions
  all_vars <- unique(unlist(lapply(exprs, all.vars)))

  intersect(all_vars, computed_names)
}

#' Compute single edge metric
#' @noRd
.compute_single_edge_metric <- function(g, edges, metric) {
  n <- nrow(edges)

  switch(metric,
    "weight" = edges$weight,
    "abs_weight" = abs(edges$weight),
    "edge_betweenness" = igraph::edge_betweenness(g, directed = igraph::is_directed(g)),
    # Default: weight
    edges$weight
  )
}

#' Compute only needed edge metrics (lazy)
#' @noRd
.compute_lazy_edge_metrics <- function(g, edges, nodes, needed, community_method) {
  if (length(needed) == 0) return(list())

  result <- list()
  n <- nrow(edges)
  is_dir <- igraph::is_directed(g)

  # Absolute weight
  if ("abs_weight" %in% needed) {
    result$abs_weight <- abs(edges$weight)
  }

  # Endpoint degrees
  if ("from_degree" %in% needed) {
    deg <- igraph::degree(g, mode = "all")
    result$from_degree <- deg[edges$from]
  }
  if ("to_degree" %in% needed) {
    deg <- if (exists("deg", inherits = FALSE)) deg else igraph::degree(g, mode = "all")
    result$to_degree <- deg[edges$to]
  }

  # Endpoint strengths
  if ("from_strength" %in% needed) {
    str <- igraph::strength(g, mode = "all")
    result$from_strength <- str[edges$from]
  }
  if ("to_strength" %in% needed) {
    str <- if (exists("str", inherits = FALSE)) str else igraph::strength(g, mode = "all")
    result$to_strength <- str[edges$to]
  }

  # Edge betweenness
  if ("edge_betweenness" %in% needed) {
    result$edge_betweenness <- igraph::edge_betweenness(g, directed = is_dir)
  }

  # Is bridge
  if ("is_bridge" %in% needed) {
    bridge_edges <- igraph::bridges(g)
    result$is_bridge <- seq_len(n) %in% as.integer(bridge_edges)
  }

  # Is mutual (reciprocated)
  if ("is_mutual" %in% needed) {
    if (!is_dir) {
      result$is_mutual <- rep(TRUE, n)
    } else {
      is_mutual <- logical(n)
      for (i in seq_len(n)) {
        is_mutual[i] <- any(edges$from == edges$to[i] & edges$to == edges$from[i])
      }
      result$is_mutual <- is_mutual
    }
  }

  # Same community
  if ("same_community" %in% needed) {
    comm <- detect_communities(g, method = community_method)
    membership <- comm$community
    from_comm <- membership[edges$from]
    to_comm <- membership[edges$to]
    result$same_community <- from_comm == to_comm
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
#' @param .keep_isolates Keep nodes with no edges? Default FALSE.
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
select_bridges <- function(x, ..., .keep_isolates = FALSE,
                           keep_format = FALSE, directed = NULL) {
  select_edges(x, ..., bridges_only = TRUE, .keep_isolates = .keep_isolates,
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
#' @param .keep_isolates Keep nodes with no edges? Default FALSE.
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
                             .keep_isolates = FALSE,
                             keep_format = FALSE, directed = NULL) {
  select_edges(x, ..., top = n, by = by, .keep_isolates = .keep_isolates,
               keep_format = keep_format, directed = directed)
}

#' Select Edges Involving Nodes
#'
#' Select edges where at least one endpoint is in the specified node set.
#'
#' @param x Network input.
#' @param nodes Character or integer. Node names or indices.
#' @param ... Additional filter expressions.
#' @param .keep_isolates Keep nodes with no edges? Default FALSE.
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
                                   .keep_isolates = FALSE,
                                   keep_format = FALSE, directed = NULL) {
  select_edges(x, ..., involving = nodes, .keep_isolates = .keep_isolates,
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
#' @param .keep_isolates Keep nodes with no edges? Default FALSE.
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
                                 .keep_isolates = FALSE,
                                 keep_format = FALSE, directed = NULL) {
  select_edges(x, ..., between = list(set1, set2), .keep_isolates = .keep_isolates,
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
