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
        x <- igraph::as.undirected(x, mode = "collapse")
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
        g <- igraph::as.undirected(g, mode = "collapse")
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
      g_undirected <- igraph::as.undirected(g, mode = "collapse",
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
    warning("No nodes match the filter criteria", call. = FALSE)
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
    source = "filtered"
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
    source = "filtered",
    layout_info = NULL,
    tna = net$tna,
    weights = new_weights
  )
}

#' Update Edges in cograph_network
#' @noRd
.update_cograph_edges <- function(net, new_edges, keep_isolates = FALSE) {
  nodes <- get_nodes(net)

  # Remove isolates if requested
  if (!keep_isolates && nrow(new_edges) > 0) {
    connected_nodes <- unique(c(new_edges$from, new_edges$to))
    if (length(connected_nodes) < nrow(nodes)) {
      return(.subset_cograph_network(
        net,
        nodes = connected_nodes,
        keep_edges = "internal"
      ))
    }
  }

  # Handle case where all edges are removed and keep_isolates = FALSE
  if (!keep_isolates && nrow(new_edges) == 0) {
    return(.empty_cograph_network(net$directed))
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
    source = "filtered",
    layout_info = net$layout_info,
    tna = net$tna,
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
