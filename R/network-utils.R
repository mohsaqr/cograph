# =============================================================================
# Network Utility Functions
# =============================================================================

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

#' Filter Edges by Weight
#'
#' Filter edges using a logical expression on edge weights. Returns a matrix
#' ready for plotting with \code{splot()}.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object.
#' @param expr A logical expression using \code{weight} to filter edges.
#'   Examples: \code{weight > 0.5}, \code{weight >= mean(weight)},
#'   \code{abs(weight) > 0.3}.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#'
#' @return An adjacency matrix with non-matching edges set to 0. Ready for
#'   direct use with \code{splot()}.
#'
#' @seealso \code{\link{filter_nodes}}, \code{\link{splot}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Keep only strong edges
#' splot(filter_edges(adj, weight > 0.5))
#'
#' # Keep edges above mean weight
#' splot(filter_edges(adj, weight >= mean(weight)))
#'
#' # Combine with color_communities
#' filtered <- filter_edges(adj, weight > 0.3)
#' splot(filtered, node_fill = color_communities(filtered))
filter_edges <- function(x, expr, directed = NULL) {

  # Convert to adjacency matrix
  adj <- to_adjacency_matrix(x, directed = directed)

  # Capture the expression
  expr_call <- substitute(expr)

  # Create environment with weight variable
  weight <- adj
  env <- list2env(list(weight = weight), parent = parent.frame())

  # Evaluate the expression
  mask <- eval(expr_call, envir = env)

  # Apply mask - set non-matching edges to 0
  result <- adj
  result[!mask] <- 0

  result
}

#' Filter Nodes by Centrality
#'
#' Filter nodes using a logical expression on centrality measures. Returns
#' a subgraph matrix ready for plotting with \code{splot()}.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object.
#' @param expr A logical expression using centrality variable names to filter nodes.
#'   Available variables:
#'   \itemize{
#'     \item \code{degree}, \code{indegree}, \code{outdegree}
#'     \item \code{strength}, \code{instrength}, \code{outstrength}
#'     \item \code{betweenness}, \code{closeness}, \code{eigenvector}
#'     \item \code{pagerank}, \code{hub}, \code{authority}
#'   }
#'   Examples: \code{degree > 3}, \code{pagerank > 0.1},
#'   \code{indegree > 2 & outdegree > 1}.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#'
#' @return A subgraph adjacency matrix containing only matching nodes.
#'   Ready for direct use with \code{splot()}.
#'
#' @seealso \code{\link{filter_edges}}, \code{\link{centrality}}, \code{\link{splot}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0, .5, .8, 0,
#'                 .5, 0, .3, .6,
#'                 .8, .3, 0, .4,
#'                  0, .6, .4, 0), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#'
#' # Keep only high-degree nodes
#' splot(filter_nodes(adj, degree >= 3))
#'
#' # Keep nodes with high PageRank
#' splot(filter_nodes(adj, pagerank > 0.2))
#'
#' # Combine with color_communities
#' hubs <- filter_nodes(adj, degree >= 3)
#' splot(hubs, node_fill = color_communities(hubs))
filter_nodes <- function(x, expr, directed = NULL) {

  # Convert to igraph for centrality calculation
  g <- to_igraph(x, directed = directed)
  is_directed <- igraph::is_directed(g)

  # Get weights
  weights <- if (!is.null(igraph::E(g)$weight)) {
    igraph::E(g)$weight
  } else {
    NULL
  }

  # Calculate all centrality measures
  n_nodes <- igraph::vcount(g)

  # Build environment with all centrality measures
  centrality_env <- list(
    # Degree measures
    degree = igraph::degree(g, mode = "all"),
    indegree = igraph::degree(g, mode = "in"),
    outdegree = igraph::degree(g, mode = "out"),

    # Strength measures
    strength = igraph::strength(g, mode = "all", weights = weights),
    instrength = igraph::strength(g, mode = "in", weights = weights),
    outstrength = igraph::strength(g, mode = "out", weights = weights),

    # Other centrality measures
    betweenness = igraph::betweenness(g, weights = weights, directed = is_directed),
    closeness = tryCatch(
      igraph::closeness(g, mode = "all", weights = weights),
      error = function(e) rep(NA_real_, n_nodes)
    ),
    eigenvector = tryCatch(
      igraph::eigen_centrality(g, weights = weights, directed = is_directed)$vector,
      error = function(e) rep(NA_real_, n_nodes)
    ),
    pagerank = igraph::page_rank(g, weights = weights, directed = is_directed)$vector,
    hub = tryCatch(
      igraph::hits_scores(g, weights = weights)$hub,
      error = function(e) rep(NA_real_, n_nodes)
    ),
    authority = tryCatch(
      igraph::hits_scores(g, weights = weights)$authority,
      error = function(e) rep(NA_real_, n_nodes)
    )
  )

  env <- list2env(centrality_env, parent = parent.frame())

  # Capture and evaluate expression
  expr_call <- substitute(expr)
  mask <- eval(expr_call, envir = env)

  # Handle NA in mask
  mask[is.na(mask)] <- FALSE

  # Get selected node indices
  selected_nodes <- which(mask)

  if (length(selected_nodes) == 0) {
    warning("No nodes match the filter criteria", call. = FALSE)
    # Return empty matrix
    result <- matrix(0, nrow = 0, ncol = 0)
    return(result)
  }

  # Extract subgraph
  adj <- to_adjacency_matrix(x, directed = directed)
  result <- adj[selected_nodes, selected_nodes, drop = FALSE]

  result
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
