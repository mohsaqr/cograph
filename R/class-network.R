#' @title CographNetwork R6 Class
#'
#' @description
#' Core class representing a network for visualization. Stores nodes, edges,
#' layout coordinates, and aesthetic mappings.
#'
#' @export
#' @examples
#' # Create network from adjacency matrix
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- CographNetwork$new(adj)
#'
#' # Access properties
#' net$n_nodes
#' net$n_edges
#' net$is_directed
CographNetwork <- R6::R6Class(
  "CographNetwork",
  public = list(
    #' @description Create a new CographNetwork object.
    #' @param input Network input (matrix, edge list, or igraph object).
    #' @param directed Logical. Force directed interpretation. NULL for auto-detect.
    #' @param node_labels Character vector of node labels.
    #' @return A new CographNetwork object.
    initialize = function(input = NULL, directed = NULL, node_labels = NULL) {
      if (!is.null(input)) {
        parsed <- parse_input(input, directed = directed)
        private$.nodes <- parsed$nodes
        private$.edges <- parsed$edges
        private$.directed <- parsed$directed
        private$.weights <- parsed$weights

        # Set node labels
        if (!is.null(node_labels)) {
          if (length(node_labels) != nrow(private$.nodes)) {
            stop("node_labels length must match number of nodes", call. = FALSE)
          }
          private$.nodes$label <- node_labels
        }
      }

      # Initialize aesthetics with defaults
      private$.node_aes <- list(
        size = 0.05,
        shape = "circle",
        fill = "#4A90D9",
        border_color = "#2C5AA0",
        border_width = 1,
        alpha = 1,
        label_size = 10,
        label_color = "black",
        label_position = "center"
      )

      private$.edge_aes <- list(
        width = 1,
        color = "gray50",
        positive_color = "#2E7D32",
        negative_color = "#C62828",
        alpha = 0.8,
        style = "solid",
        curvature = 0,
        arrow_size = 0.015,
        show_arrows = NULL  # NULL = auto (TRUE if directed)
      )

      invisible(self)
    },

    #' @description Clone the network with optional modifications.
    #' @return A new CographNetwork object.
    clone_network = function() {
      new_net <- CographNetwork$new()
      new_net$set_nodes(private$.nodes)
      new_net$set_edges(private$.edges)
      new_net$set_directed(private$.directed)
      new_net$set_weights(private$.weights)
      new_net$set_layout_coords(private$.layout)
      new_net$set_node_aes(private$.node_aes)
      new_net$set_edge_aes(private$.edge_aes)
      new_net$set_theme(private$.theme)
      if (!is.null(private$.layout_info)) {
        new_net$set_layout_info(private$.layout_info)
      }
      if (!is.null(private$.plot_params)) {
        new_net$set_plot_params(private$.plot_params)
      }
      new_net
    },

    #' @description Set nodes data frame.
    #' @param nodes Data frame with node information.
    set_nodes = function(nodes) {
      private$.nodes <- nodes
      invisible(self)
    },

    #' @description Set edges data frame.
    #' @param edges Data frame with edge information.
    set_edges = function(edges) {
      private$.edges <- edges
      invisible(self)
    },

    #' @description Set directed flag.
    #' @param directed Logical.
    set_directed = function(directed) {
      private$.directed <- directed
      invisible(self)
    },

    #' @description Set edge weights.
    #' @param weights Numeric vector of weights.
    set_weights = function(weights) {
      private$.weights <- weights
      invisible(self)
    },

    #' @description Set layout coordinates.
    #' @param coords Matrix or data frame with x, y columns.
    set_layout_coords = function(coords) {
      if (!is.null(coords)) {
        if (is.matrix(coords)) {
          coords <- as.data.frame(coords)
          if (is.null(names(coords))) {
            names(coords) <- c("x", "y")
          }
        }
        private$.layout <- coords
        # Update node positions
        if (!is.null(private$.nodes) && nrow(private$.nodes) == nrow(coords)) {
          private$.nodes$x <- coords$x
          private$.nodes$y <- coords$y
        }
      }
      invisible(self)
    },

    #' @description Set node aesthetics.
    #' @param aes List of aesthetic parameters.
    set_node_aes = function(aes) {
      private$.node_aes <- utils::modifyList(private$.node_aes, aes)
      invisible(self)
    },

    #' @description Set edge aesthetics.
    #' @param aes List of aesthetic parameters.
    set_edge_aes = function(aes) {
      private$.edge_aes <- utils::modifyList(private$.edge_aes, aes)
      invisible(self)
    },

    #' @description Set theme.
    #' @param theme CographTheme object or theme name.
    set_theme = function(theme) {
      private$.theme <- theme
      invisible(self)
    },

    #' @description Get nodes data frame.
    #' @return Data frame with node information.
    get_nodes = function() {
      private$.nodes
    },

    #' @description Get edges data frame.
    #' @return Data frame with edge information.
    get_edges = function() {
      private$.edges
    },

    #' @description Get layout coordinates.
    #' @return Data frame with x, y coordinates.
    get_layout = function() {
      private$.layout
    },

    #' @description Get node aesthetics.
    #' @return List of node aesthetic parameters.
    get_node_aes = function() {
      private$.node_aes
    },

    #' @description Get edge aesthetics.
    #' @return List of edge aesthetic parameters.
    get_edge_aes = function() {
      private$.edge_aes
    },

    #' @description Get theme.
    #' @return CographTheme object.
    get_theme = function() {
      private$.theme
    },

    #' @description Set layout info.
    #' @param info List with layout information (name, seed, etc.).
    set_layout_info = function(info) {
      private$.layout_info <- info
      invisible(self)
    },

    #' @description Get layout info.
    #' @return List with layout information.
    get_layout_info = function() {
      private$.layout_info
    },

    #' @description Set plot parameters.
    #' @param params List of all plot parameters used.
    set_plot_params = function(params) {
      private$.plot_params <- params
      invisible(self)
    },

    #' @description Get plot parameters.
    #' @return List of plot parameters.
    get_plot_params = function() {
      private$.plot_params
    },

    #' @description Print network summary.
    print = function() {
      cat("CographNetwork\n")
      cat("  Nodes:", self$n_nodes, "\n")
      cat("  Edges:", self$n_edges, "\n")
      cat("  Directed:", self$is_directed, "\n")
      cat("  Layout:", if (is.null(private$.layout)) "none" else "set", "\n")
      invisible(self)
    }
  ),

  active = list(
    #' @field n_nodes Number of nodes in the network.
    n_nodes = function() {
      if (is.null(private$.nodes)) 0L else nrow(private$.nodes)
    },

    #' @field n_edges Number of edges in the network.
    n_edges = function() {
      if (is.null(private$.edges)) 0L else nrow(private$.edges)
    },

    #' @field is_directed Whether the network is directed.
    is_directed = function() {
      private$.directed
    },

    #' @field has_weights Whether edges have weights.
    has_weights = function() {
      !is.null(private$.weights) && any(private$.weights != 1)
    },

    #' @field node_labels Vector of node labels.
    node_labels = function() {
      if (is.null(private$.nodes)) NULL else private$.nodes$label
    }
  ),

  private = list(
    .nodes = NULL,
    .edges = NULL,
    .directed = FALSE,
    .weights = NULL,
    .layout = NULL,
    .node_aes = NULL,
    .edge_aes = NULL,
    .theme = NULL,
    .layout_info = NULL,
    .plot_params = NULL
  )
)

#' @title Check if object is a CographNetwork
#' @param x Object to check.
#' @return Logical.
#' @keywords internal
is_cograph_network <- function(x) {

  inherits(x, "CographNetwork") || inherits(x, "cograph_network")
}

# =============================================================================
# Unified cograph_network Constructor
# =============================================================================

#' Create Unified cograph_network Object
#'
#' Internal constructor that creates a cograph_network object with the unified
#' format. Both cograph() and as_cograph() use this to ensure identical output.
#'
#' @param nodes Data frame with node information (id, label, x, y, ...).
#' @param edges Data frame with edge information (from, to, weight).
#' @param directed Logical. Is the network directed?
#' @param source Character. Input source type ("matrix", "tna", "igraph", etc.).
#' @param layout Data frame with x, y coordinates, or NULL.
#' @param layout_info List with layout metadata (name, seed), or NULL.
#' @param node_aes List of node aesthetic parameters.
#' @param edge_aes List of edge aesthetic parameters.
#' @param theme Theme object or NULL.
#' @param plot_params List of plot parameters.
#' @param tna List with TNA metadata (model, type, group_index, group_name), or NULL.
#' @param weights Full n×n weight matrix for TNA compatibility, or NULL.
#' @param layers Optional layer assignments.
#' @param clusters Optional cluster assignments.
#' @param groups Optional group assignments.
#' @param node_groups Optional node groupings data frame.
#' @param raw_data Optional raw data for validation.
#' @param estimator Optional estimator function for validation.
#' @return A cograph_network object (named list with class).
#' @keywords internal
.create_cograph_network <- function(
    nodes,
    edges,
    directed,
    source = "unknown",
    layout = NULL,
    layout_info = NULL,
    node_aes = NULL,
    edge_aes = NULL,
    theme = NULL,
    plot_params = NULL,
    tna = NULL,
    weights = NULL,
    layers = NULL,
    clusters = NULL,
    groups = NULL,
    node_groups = NULL,
    raw_data = NULL,
    estimator = NULL
) {
  # Extract from/to/weight vectors from edges data frame
  if (!is.null(edges) && nrow(edges) > 0) {
    from_vec <- as.integer(edges$from)
    to_vec <- as.integer(edges$to)
    weight_vec <- if (!is.null(edges$weight)) as.numeric(edges$weight) else rep(1, nrow(edges))
  } else {
    from_vec <- integer(0)
    to_vec <- integer(0)
    weight_vec <- numeric(0)
  }

  # Ensure edges data frame has standard columns
  edges_df <- if (length(from_vec) > 0) {
    data.frame(
      from = from_vec,
      to = to_vec,
      weight = weight_vec,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  }

  # Default aesthetics if not provided
  if (is.null(node_aes)) {
    node_aes <- list(
      size = 0.05,
      shape = "circle",
      fill = "#4A90D9",
      border_color = "#2C5AA0",
      border_width = 1,
      alpha = 1,
      label_size = 10,
      label_color = "black",
      label_position = "center"
    )
  }

  if (is.null(edge_aes)) {
    edge_aes <- list(
      width = 1,
      color = "gray50",
      positive_color = "#2E7D32",
      negative_color = "#C62828",
      alpha = 0.8,
      style = "solid",
      curvature = 0,
      arrow_size = 0.015,
      show_arrows = NULL
    )
  }

  if (is.null(plot_params)) {
    plot_params <- list()
  }

  # Build the unified network object

net <- list(
    # Core edge data (vectors for backwards compatibility)
    from = from_vec,
    to = to_vec,
    weight = weight_vec,

    # Edge data frame
    edges = edges_df,

    # Core node data
    nodes = nodes,
    directed = directed,
    n_nodes = nrow(nodes),
    n_edges = length(from_vec),
    labels = nodes$label,

    # Layout (computed by cograph, NULL by as_cograph)
    layout = layout,
    layout_info = layout_info,

    # Aesthetics (set by sn_* functions)
    node_aes = node_aes,
    edge_aes = edge_aes,
    theme = theme,
    plot_params = plot_params,

    # Metadata
    source = source,

    # TNA integration
    tna = tna,

    # Weight matrix (for TNA compatibility and round-trip)
    weights = weights,

    # Optional
    layers = layers,
    clusters = clusters,
    groups = groups,
    node_groups = node_groups,
    raw_data = raw_data,
    estimator = estimator
  )

  # Set S3 class
  class(net) <- c("cograph_network", "list")

  net
}

#' @title Create cograph_network S3 class wrapper (Deprecated)
#'
#' @description
#' This function is deprecated. Use \code{\link{.create_cograph_network}} instead.
#' The unified format no longer wraps R6 objects.
#'
#' @param network CographNetwork R6 object.
#' @return Object with cograph_network class.
#' @keywords internal
as_cograph_network <- function(network) {
  .Deprecated(".create_cograph_network")

  # Convert R6 to unified format for backwards compatibility
  .create_cograph_network(
    nodes = network$get_nodes(),
    edges = network$get_edges(),
    directed = network$is_directed,
    source = "r6_converted",
    layout = network$get_layout(),
    layout_info = network$get_layout_info(),
    node_aes = network$get_node_aes(),
    edge_aes = network$get_edge_aes(),
    theme = network$get_theme(),
    plot_params = network$get_plot_params()
  )
}

# =============================================================================
# Getter Functions for cograph_network
# =============================================================================

#' Get Nodes from Cograph Network
#'
#' Extracts the nodes data frame from a cograph_network object.
#'
#' @param x A cograph_network object.
#' @return A data frame with columns: id, label, name, x, y (and possibly others).
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{n_nodes}}, \code{\link{get_edges}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' get_nodes(net)
get_nodes <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Unified format: nodes stored as list element
    if (!is.null(x$nodes) && is.data.frame(x$nodes)) {
      return(x$nodes)
    }
  }
  stop("Cannot extract nodes from this object", call. = FALSE)
}

#' Get Edges from Cograph Network
#'
#' Extracts the edges data frame from a cograph_network object.
#' For the new format, builds a data frame from the from/to/weight vectors.
#'
#' @param x A cograph_network object.
#' @return A data frame with columns: from, to, weight.
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{n_edges}}, \code{\link{get_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' get_edges(net)
get_edges <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Unified format: edges stored as list element
    if (!is.null(x$edges) && is.data.frame(x$edges)) {
      return(x$edges)
    }
    # Fallback: build from vectors (backwards compatibility)
    if (!is.null(x$from)) {
      if (length(x$from) > 0) {
        return(data.frame(
          from = x$from,
          to = x$to,
          weight = x$weight,
          stringsAsFactors = FALSE
        ))
      } else {
        return(data.frame(from = integer(0), to = integer(0), weight = numeric(0)))
      }
    }
  }
  stop("Cannot extract edges from this object", call. = FALSE)
}

#' Get Labels from Cograph Network
#'
#' Extracts the node labels vector from a cograph_network object.
#'
#' @param x A cograph_network object.
#' @return A character vector of node labels.
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{get_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' get_labels(net)
get_labels <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Unified format: labels stored as list element
    if (!is.null(x$labels)) {
      return(x$labels)
    }
    # Fallback: get from nodes
    nodes <- get_nodes(x)
    if (!is.null(nodes) && "label" %in% names(nodes)) {
      return(nodes$label)
    }
  }
  stop("Cannot extract labels from this object", call. = FALSE)
}

# =============================================================================
# Setter Functions for cograph_network
# =============================================================================

#' Set Nodes in Cograph Network
#'
#' Replaces the nodes data frame in a cograph_network object.
#' Automatically updates n_nodes and labels.
#'
#' @param x A cograph_network object.
#' @param nodes_df A data frame with node information (id, label columns expected).
#' @return The modified cograph_network object.
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{get_nodes}}, \code{\link{set_edges}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' new_nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
#' net <- set_nodes(net, new_nodes)
#' get_labels(net)
set_nodes <- function(x, nodes_df) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }
  if (!is.data.frame(nodes_df)) {
    stop("nodes_df must be a data frame", call. = FALSE)
  }

  # Ensure required columns
  if (!"id" %in% names(nodes_df)) {
    nodes_df$id <- seq_len(nrow(nodes_df))
  }
  if (!"label" %in% names(nodes_df)) {
    nodes_df$label <- as.character(nodes_df$id)
  }

  # Update the network
  x$nodes <- nodes_df
  x$n_nodes <- nrow(nodes_df)
  x$labels <- nodes_df$label

  x
}

#' Set Edges in Cograph Network
#'
#' Replaces the edges in a cograph_network object.
#' Expects a data frame with from, to, and optionally weight columns.
#' Updates the from, to, weight vectors and n_edges.
#'
#' @param x A cograph_network object.
#' @param edges_df A data frame with columns: from, to, and optionally weight.
#' @return The modified cograph_network object.
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{get_edges}}, \code{\link{set_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' new_edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
#' net <- set_edges(net, new_edges)
#' get_edges(net)
set_edges <- function(x, edges_df) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }
  if (!is.data.frame(edges_df)) {
    stop("edges_df must be a data frame", call. = FALSE)
  }

  # Ensure required columns
  if (!all(c("from", "to") %in% names(edges_df))) {
    stop("edges_df must have 'from' and 'to' columns", call. = FALSE)
  }
  if (!"weight" %in% names(edges_df)) {
    edges_df$weight <- rep(1, nrow(edges_df))
  }

  # Update the network
  x$from <- as.integer(edges_df$from)
  x$to <- as.integer(edges_df$to)
  x$weight <- as.numeric(edges_df$weight)
  x$n_edges <- nrow(edges_df)

  x
}

#' Set Layout in Cograph Network
#'
#' Sets the layout coordinates in a cograph_network object.
#' Updates the x and y columns in the nodes data frame.
#'
#' @param x A cograph_network object.
#' @param layout_df A data frame with x and y columns, or a matrix with 2 columns.
#' @return The modified cograph_network object.
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{get_nodes}}, \code{\link{sn_layout}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' layout <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
#' net <- set_layout(net, layout)
#' get_nodes(net)
set_layout <- function(x, layout_df) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }

  # Convert matrix to data frame
  if (is.matrix(layout_df)) {
    layout_df <- as.data.frame(layout_df)
    if (ncol(layout_df) >= 2) {
      names(layout_df)[1:2] <- c("x", "y")
    }
  }

  if (!is.data.frame(layout_df) || !all(c("x", "y") %in% names(layout_df))) {
    stop("layout_df must have 'x' and 'y' columns", call. = FALSE)
  }

  # Update nodes with layout coordinates
  nodes <- get_nodes(x)
  if (nrow(layout_df) != nrow(nodes)) {
    stop("layout_df must have the same number of rows as nodes", call. = FALSE)
  }

  nodes$x <- layout_df$x
  nodes$y <- layout_df$y
  x$nodes <- nodes
  x$layout <- layout_df

  x
}

# =============================================================================
# New Lightweight cograph_network Format
# =============================================================================

#' Convert to Cograph Network
#'
#' Creates a lightweight cograph_network object from various network inputs.
#' The resulting object is a named list with all data accessible via \code{$}.
#'
#' @param x Network input. Can be:
#'   - A square numeric matrix (adjacency/weight matrix)
#'   - A data frame with edge list (from, to, optional weight columns)
#'   - An igraph object
#'   - A statnet network object
#'   - A qgraph object
#'   - A tna object
#'   - An existing cograph_network object (returned as-is)
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @param ... Additional arguments (currently unused).
#'
#' @return A cograph_network object: a named list with components:
#'   \describe{
#'     \item{\code{from}}{Integer vector of source node indices}
#'     \item{\code{to}}{Integer vector of target node indices}
#'     \item{\code{weight}}{Numeric vector of edge weights}
#'     \item{\code{nodes}}{Data frame with id, label, (x, y if layout applied)}
#'     \item{\code{directed}}{Logical indicating if network is directed}
#'     \item{\code{n_nodes}}{Integer count of nodes}
#'     \item{\code{n_edges}}{Integer count of edges}
#'     \item{\code{labels}}{Character vector of node labels}
#'     \item{\code{source}}{Character indicating input type}
#'     \item{\code{layout}}{Layout coordinates (NULL until computed)}
#'     \item{\code{layout_info}}{Layout algorithm info (NULL until computed)}
#'   }
#'
#' @details
#' The cograph_network format is designed to be:
#' - Simple: All data accessible via \code{net$from}, \code{net$to}, \code{net$weight}, etc.
#' - Modern: Uses named list elements instead of attributes for clean \code{$} access
#' - Compatible: Works seamlessly with splot() and other cograph functions
#'
#' Use getter functions for programmatic access:
#' \code{\link{get_nodes}}, \code{\link{get_edges}}, \code{\link{get_labels}}
#'
#' Use setter functions to modify:
#' \code{\link{set_nodes}}, \code{\link{set_edges}}, \code{\link{set_layout}}
#'
#' @seealso
#' \code{\link{get_nodes}} to extract the nodes data frame,
#' \code{\link{get_edges}} to extract edges as a data frame,
#' \code{\link{n_nodes}} and \code{\link{n_edges}} for counts,
#' \code{\link{is_directed}} to check directedness,
#' \code{\link{splot}} for plotting
#'
#' @export
#'
#' @examples
#' # From adjacency matrix
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#'
#' # Direct $ access to all data
#' net$from       # edge sources
#' net$to         # edge targets
#' net$weight     # edge weights
#' net$nodes      # nodes data frame
#' net$directed   # TRUE/FALSE
#' net$n_nodes    # 3
#' net$n_edges    # 3
#'
#' # Getter functions (recommended for programmatic use)
#' get_nodes(net)   # nodes data frame
#' get_edges(net)   # edges data frame (from, to, weight)
#' get_labels(net)  # character vector of labels
#' n_nodes(net)     # 3
#' n_edges(net)     # 3
#' is_directed(net) # FALSE (symmetric matrix)
#'
#' # Setter functions
#' net <- set_nodes(net, data.frame(id = 1:3, label = c("A", "B", "C")))
#' net <- set_edges(net, data.frame(from = c(1,2), to = c(2,3), weight = c(0.5, 0.8)))
#' net <- set_layout(net, data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1)))
#'
#' # Plot it
#' splot(net)
#'
#' # From igraph (if installed)
#' \dontrun{
#' library(igraph)
#' g <- make_ring(10)
#' net <- as_cograph(g)
#' splot(net)
#' }
as_cograph <- function(x, directed = NULL, ...) {
  # Return as-is if already a cograph_network

  if (inherits(x, "cograph_network")) {
    return(x)
  }

  # Parse the input
  parsed <- parse_input(x, directed = directed)

  # Determine source type
  source_type <- if (is.matrix(x)) {
    "matrix"
  } else if (is.data.frame(x)) {
    "edgelist"
  } else if (inherits(x, "igraph")) {
    "igraph"
  } else if (inherits(x, "network")) {
    "network"
  } else if (inherits(x, "qgraph")) {
    "qgraph"
  } else if (inherits(x, "tna")) {
    "tna"
  } else {
    "unknown"
  }

  # Get full weight matrix if available (from parse_input or original matrix)
  weights_matrix <- NULL
  if (!is.null(parsed$tna) && !is.null(parsed$tna$model)) {
    # TNA: use model's weights matrix
    weights_matrix <- parsed$tna$model$weights
  } else if (is.matrix(x) && nrow(x) == ncol(x)) {
    # Square matrix input: preserve it
    weights_matrix <- x
  }

  # Use unified constructor
  .create_cograph_network(
    nodes = parsed$nodes,
    edges = parsed$edges,
    directed = parsed$directed,
    source = source_type,
    layout = NULL,
    layout_info = NULL,
    node_aes = NULL,  # Will use defaults
    edge_aes = NULL,  # Will use defaults
    theme = NULL,
    plot_params = NULL,
    tna = parsed$tna,
    weights = weights_matrix
  )
}

#' @rdname as_cograph
#' @export
to_cograph <- function(x, directed = NULL, ...) {
  as_cograph(x, directed = directed, ...)
}

#' Set Node Groups for Auto-Plot Selection
#'
#' Assigns node groupings to a cograph_network object. The column name in the
#' resulting data frame determines which plot function \code{splot()} will
#' auto-dispatch to:
#' \itemize{
#'   \item \code{"layer"} column -> \code{plot_mlna()} (multilevel network)
#'   \item \code{"cluster"} column -> \code{plot_mtna()} (multi-cluster network)
#'   \item \code{"group"} column -> \code{plot_htna()} (heterogeneous network)
#' }
#'
#' @param x A cograph_network object.
#' @param groups Node groupings in one of these formats:
#'   \itemize{
#'     \item Character string: Community detection method ("louvain", "walktrap",
#'       "fast_greedy", "label_prop", "infomap", "leiden")
#'     \item Named list: Group name -> node vector mapping
#'       (e.g., \code{list(A = c("N1","N2"), B = c("N3","N4"))})
#'     \item Unnamed vector: Group assignment per node (same order as nodes)
#'     \item Data frame: Must have "node"/"nodes" column plus one of
#'       "layer"/"layers", "cluster"/"clusters", or "group"/"groups"
#'       (plural forms are automatically normalized to singular)
#'     \item NULL: Use \code{nodes} + one of \code{layers}/\code{clusters}/\code{groups} vectors
#'   }
#' @param type Group type determining the plot function. One of:
#'
#'   \itemize{
#'     \item \code{"group"} (default): Uses \code{plot_htna()}
#'     \item \code{"cluster"}: Uses \code{plot_mtna()}
#'     \item \code{"layer"}: Uses \code{plot_mlna()}
#'   }
#'   Ignored when using vector arguments (\code{layers}, \code{clusters}, \code{groups})
#'   since the type is inferred from which argument is provided.
#' @param nodes Character vector of node labels. Use with \code{layers}, \code{clusters},
#'   or \code{groups} to specify groupings via vectors instead of a data frame.
#' @param layers Character/factor vector of layer assignments (same length as \code{nodes}).
#'   Triggers dispatch to \code{plot_mlna()}.
#' @param clusters Character/factor vector of cluster assignments (same length as \code{nodes}).
#'   Triggers dispatch to \code{plot_mtna()}.
#'
#' @return The modified cograph_network object with \code{node_groups} set.
#'
#' @seealso \code{\link{get_groups}}, \code{\link{splot}}, \code{\link{plot_mlna}},
#'   \code{\link{plot_mtna}}, \code{\link{plot_htna}}, \code{\link{detect_communities}}
#'
#' @export
#'
#' @examples
#' # Create network
#' mat <- matrix(runif(100), 10, 10)
#' rownames(mat) <- colnames(mat) <- paste0("N", 1:10)
#' net <- as_cograph(mat)
#'
#' # Using vectors (recommended)
#' net <- set_groups(net,
#'   nodes = paste0("N", 1:10),
#'   layers = c(rep("Macro", 3), rep("Meso", 4), rep("Micro", 3))
#' )
#'
#' # Named list -> layers (for plot_mlna)
#' net <- set_groups(net, list(
#'   Macro = paste0("N", 1:3),
#'   Meso = paste0("N", 4:7),
#'   Micro = paste0("N", 8:10)
#' ), type = "layer")
#'
#' # Vector -> clusters (for plot_mtna)
#' net <- set_groups(net, c("A", "A", "A", "B", "B", "B", "C", "C", "C", "C"),
#'                   type = "cluster")
#'
#' # Community detection -> groups (for plot_htna)
#' net <- set_groups(net, "louvain", type = "group")
#'
#' # Data frame with explicit columns
#' df <- data.frame(nodes = paste0("N", 1:10),
#'                  layers = rep(c("Top", "Bottom"), each = 5))
#' net <- set_groups(net, df)
#'
#' # Check groups
#' get_groups(net)
set_groups <- function(x, groups = NULL, type = c("group", "cluster", "layer"),
                       nodes = NULL, layers = NULL, clusters = NULL) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }

  type <- match.arg(type)
  net_labels <- get_labels(x)

  # ==========================================================================
  # Handle vector arguments: nodes + layers/clusters/groups
  # ==========================================================================
  vec_args <- c(!is.null(layers), !is.null(clusters))
  if (any(vec_args)) {
    # Determine type from which vector was provided
    if (!is.null(layers)) {
      vec_type <- "layer"
      vec_values <- layers
    } else if (!is.null(clusters)) {
      vec_type <- "cluster"
      vec_values <- clusters
    }

    # If nodes not provided, assume same order as network nodes

if (is.null(nodes)) {
      if (length(vec_values) != length(net_labels)) {
        stop(vec_type, "s vector length (", length(vec_values),
             ") must match number of nodes (", length(net_labels), ")",
             call. = FALSE)
      }
      nodes <- net_labels
    }

    # Validate lengths match
    if (length(nodes) != length(vec_values)) {
      stop("nodes and ", vec_type, "s must have the same length", call. = FALSE)
    }

    df <- data.frame(
      node = nodes,
      V2 = vec_values,
      stringsAsFactors = FALSE
    )
    names(df)[2] <- vec_type

  # ==========================================================================
  # Handle groups argument (original API)
  # ==========================================================================
  } else if (!is.null(groups)) {
    if (is.character(groups) && length(groups) == 1) {
      # Community detection method name
      df <- detect_communities(x, method = groups)
      names(df)[names(df) == "community"] <- type
    } else if (is.list(groups) && !is.data.frame(groups)) {
      # Named list: list(A = c("N1","N2"), B = c("N3","N4"))
      df <- data.frame(
        node = unlist(groups, use.names = FALSE),
        V2 = rep(names(groups), lengths(groups)),
        stringsAsFactors = FALSE
      )
      names(df)[2] <- type
    } else if (is.vector(groups) && length(groups) > 1 && !is.list(groups)) {
      # Vector (same order as nodes)
      if (length(groups) != length(net_labels)) {
        stop("groups vector length (", length(groups), ") must match number of nodes (",
             length(net_labels), ")", call. = FALSE)
      }
      df <- data.frame(
        node = net_labels,
        V2 = groups,
        stringsAsFactors = FALSE
      )
      names(df)[2] <- type
    } else if (is.data.frame(groups)) {
      df <- groups

      # Normalize plural column names to singular
      col_map <- c(nodes = "node", layers = "layer", clusters = "cluster", groups = "group")
      for (old_name in names(col_map)) {
        if (old_name %in% names(df)) {
          names(df)[names(df) == old_name] <- col_map[old_name]
        }
      }

      # If df has "group"/"cluster"/"layer" column, use it; else rename 2nd col
      if (!any(c("group", "cluster", "layer") %in% names(df))) {
        if (ncol(df) >= 2) {
          names(df)[2] <- type
        } else {
          stop("Data frame must have at least 2 columns (node and group assignment)",
               call. = FALSE)
        }
      }
      # Ensure "node" column exists
      if (!"node" %in% names(df)) {
        if (ncol(df) >= 1) {
          names(df)[1] <- "node"
        }
      }
    } else {
      stop("groups must be: a community detection method name, a named list, ",
           "a vector, or a data frame", call. = FALSE)
    }
  } else {
    stop("Must provide either 'groups' or vector arguments (nodes + layers/clusters)",
         call. = FALSE)
  }

  # ==========================================================================
  # Validation
  # ==========================================================================
  # Check for duplicate nodes in assignment
  if (anyDuplicated(df$node)) {
    dups <- df$node[duplicated(df$node)]
    stop("Duplicate node assignments found: ", paste(unique(dups), collapse = ", "),
         call. = FALSE)
  }

  # Check all assigned nodes exist in the network
  missing_nodes <- setdiff(df$node, net_labels)
  if (length(missing_nodes) > 0) {
    stop("Nodes not found in network: ", paste(missing_nodes, collapse = ", "),
         call. = FALSE)
  }

  # Check all network nodes are assigned
  unassigned <- setdiff(net_labels, df$node)
  if (length(unassigned) > 0) {
    stop("Nodes missing from group assignment: ", paste(unassigned, collapse = ", "),
         call. = FALSE)
  }

  # Check we have at least 2 groups for visualization
  group_col <- intersect(c("layer", "cluster", "group"), names(df))
  if (length(group_col) > 0) {
    n_groups <- length(unique(df[[group_col[1]]]))
    if (n_groups < 2) {
      stop("At least 2 groups are required for visualization (found ", n_groups, ")",
           call. = FALSE)
    }
  }

  x$node_groups <- df
  x
}

#' Get Node Groups from Cograph Network
#'
#' Extracts the node groupings from a cograph_network object.
#'
#' @param x A cograph_network object.
#'
#' @return A data frame with node groupings, or NULL if not set. The data frame
#'   has columns:
#'   \itemize{
#'     \item \code{node}: Node labels
#'     \item One of \code{layer}, \code{cluster}, or \code{group}: Group assignment
#'   }
#'
#' @seealso \code{\link{set_groups}}, \code{\link{splot}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(runif(25), 5, 5)
#' rownames(mat) <- colnames(mat) <- LETTERS[1:5]
#' net <- as_cograph(mat)
#' net <- set_groups(net, list(G1 = c("A", "B"), G2 = c("C", "D", "E")))
#' get_groups(net)
get_groups <- function(x) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }
  x$node_groups
}

#' Get Nodes from Cograph Network (Deprecated)
#'
#' Extracts the nodes data frame from a cograph_network object.
#' \strong{Deprecated}: Use \code{\link{get_nodes}} instead.
#'
#' @param x A cograph_network object.
#' @return A data frame with columns: id, label, name, x, y (and possibly others).
#'
#' @seealso \code{\link{get_nodes}}, \code{\link{as_cograph}}, \code{\link{n_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' nodes(net)  # Deprecated, use get_nodes(net) instead
nodes <- function(x) {
  # Soft deprecation warning
  # .Deprecated("get_nodes")
  get_nodes(x)
}

#' Check if Network is Directed
#'
#' Checks whether a cograph_network is directed.
#'
#' @param x A cograph_network object.
#' @return Logical: TRUE if directed, FALSE if undirected.
#'
#' @seealso \code{\link{as_cograph}}
#'
#' @export
#'
#' @examples
#' # Symmetric matrix -> undirected
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' is_directed(net)  # FALSE
#'
#' # Asymmetric matrix -> directed
#' mat2 <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
#' net2 <- as_cograph(mat2)
#' is_directed(net2)  # TRUE
is_directed <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Unified format: directed stored as list element
    if (!is.null(x$directed)) {
      return(x$directed)
    }
  }
  stop("Cannot determine directedness for this object", call. = FALSE)
}

#' Get Number of Nodes
#'
#' Returns the number of nodes in a cograph_network.
#'
#' @param x A cograph_network object.
#' @return Integer: number of nodes.
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{n_edges}}, \code{\link{get_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' n_nodes(net)  # 3
n_nodes <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Unified format: n_nodes stored as list element
    if (!is.null(x$n_nodes)) {
      return(x$n_nodes)
    }
  }
  stop("Cannot count nodes for this object", call. = FALSE)
}

#' Get Number of Edges
#'
#' Returns the number of edges in a cograph_network.
#'
#' @param x A cograph_network object.
#' @return Integer: number of edges.
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{n_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_cograph(mat)
#' n_edges(net)  # 3
n_edges <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Unified format: n_edges stored as list element
    if (!is.null(x$n_edges)) {
      return(x$n_edges)
    }
  }
  stop("Cannot count edges for this object", call. = FALSE)
}

