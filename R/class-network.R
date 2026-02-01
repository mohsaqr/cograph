#' @title SonnetNetwork R6 Class
#'
#' @description
#' Core class representing a network for visualization. Stores nodes, edges,
#' layout coordinates, and aesthetic mappings.
#'
#' @export
#' @examples
#' # Create network from adjacency matrix
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- SonnetNetwork$new(adj)
#'
#' # Access properties
#' net$n_nodes
#' net$n_edges
#' net$is_directed
SonnetNetwork <- R6::R6Class(
  "SonnetNetwork",
  public = list(
    #' @description Create a new SonnetNetwork object.
    #' @param input Network input (matrix, edge list, or igraph object).
    #' @param directed Logical. Force directed interpretation. NULL for auto-detect.
    #' @param node_labels Character vector of node labels.
    #' @return A new SonnetNetwork object.
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
    #' @return A new SonnetNetwork object.
    clone_network = function() {
      new_net <- SonnetNetwork$new()
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
    #' @param theme SonnetTheme object or theme name.
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
    #' @return SonnetTheme object.
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
      cat("SonnetNetwork\n")
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

#' @title Check if object is a SonnetNetwork
#' @param x Object to check.
#' @return Logical.
#' @keywords internal
is_sonnet_network <- function(x) {

  inherits(x, "SonnetNetwork") || inherits(x, "sonnet_network")
}

#' @title Create sonnet_network S3 class wrapper
#' @param network SonnetNetwork R6 object.
#' @return Object with sonnet_network class.
#' @keywords internal
as_sonnet_network <- function(network) {
  obj <- structure(
    list(network = network),
    class = c("sonnet_network", "list")
  )
  # Add direct access to layout and plot params
  obj$layout <- network$get_layout()
  obj$layout_info <- network$get_layout_info()
  obj$plot_params <- network$get_plot_params()
  obj$nodes <- network$get_nodes()
  obj$edges <- network$get_edges()
  obj$node_aes <- network$get_node_aes()
  obj$edge_aes <- network$get_edge_aes()
  obj
}

# =============================================================================
# Getter Functions for sonnet_network
# =============================================================================

#' Get Nodes from Sonnet Network
#'
#' Extracts the nodes data frame from a sonnet_network object.
#'
#' @param x A sonnet_network object.
#' @return A data frame with columns: id, label, name, x, y (and possibly others).
#'
#' @seealso \code{\link{as_sonnet}}, \code{\link{n_nodes}}, \code{\link{get_edges}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' get_nodes(net)
get_nodes <- function(x) {
  if (inherits(x, "sonnet_network")) {
    # Check for new list format first (nodes stored as list element)
    if (!is.null(x$nodes) && is.data.frame(x$nodes)) {
      return(x$nodes)
    }
    # Check for old attr format
    nodes_attr <- attr(x, "nodes")
    if (!is.null(nodes_attr)) {
      return(nodes_attr)
    }
    # R6 object in old wrapper
    if (!is.null(x$network) && inherits(x$network, "SonnetNetwork")) {
      return(x$network$get_nodes())
    }
  }
  stop("Cannot extract nodes from this object", call. = FALSE)
}

#' Get Edges from Sonnet Network
#'
#' Extracts the edges data frame from a sonnet_network object.
#' For the new format, builds a data frame from the from/to/weight vectors.
#'
#' @param x A sonnet_network object.
#' @return A data frame with columns: from, to, weight.
#'
#' @seealso \code{\link{as_sonnet}}, \code{\link{n_edges}}, \code{\link{get_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' get_edges(net)
get_edges <- function(x) {
  if (inherits(x, "sonnet_network")) {
    # Check for new list format (from/to/weight as list elements)
    if (!is.null(x$n_nodes)) {
      # New format: build data frame from vectors
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
    # Check for old wrapper format with edges stored directly
    if (!is.null(x$edges) && is.data.frame(x$edges)) {
      return(x$edges)
    }
    # R6 object in old wrapper
    if (!is.null(x$network) && inherits(x$network, "SonnetNetwork")) {
      return(x$network$get_edges())
    }
  }
  stop("Cannot extract edges from this object", call. = FALSE)
}

#' Get Labels from Sonnet Network
#'
#' Extracts the node labels vector from a sonnet_network object.
#'
#' @param x A sonnet_network object.
#' @return A character vector of node labels.
#'
#' @seealso \code{\link{as_sonnet}}, \code{\link{get_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' get_labels(net)
get_labels <- function(x) {
  if (inherits(x, "sonnet_network")) {
    # Check for new list format
    if (!is.null(x$labels)) {
      return(x$labels)
    }
    # Check for old attr format
    labels_attr <- attr(x, "labels")
    if (!is.null(labels_attr)) {
      return(labels_attr)
    }
    # Try getting from nodes
    nodes <- get_nodes(x)
    if (!is.null(nodes) && "label" %in% names(nodes)) {
      return(nodes$label)
    }
  }
  stop("Cannot extract labels from this object", call. = FALSE)
}

# =============================================================================
# Setter Functions for sonnet_network
# =============================================================================

#' Set Nodes in Sonnet Network
#'
#' Replaces the nodes data frame in a sonnet_network object.
#' Automatically updates n_nodes and labels.
#'
#' @param x A sonnet_network object.
#' @param nodes_df A data frame with node information (id, label columns expected).
#' @return The modified sonnet_network object.
#'
#' @seealso \code{\link{as_sonnet}}, \code{\link{get_nodes}}, \code{\link{set_edges}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' new_nodes <- data.frame(id = 1:3, label = c("A", "B", "C"))
#' net <- set_nodes(net, new_nodes)
#' get_labels(net)
set_nodes <- function(x, nodes_df) {
  if (!inherits(x, "sonnet_network")) {
    stop("x must be a sonnet_network object", call. = FALSE)
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

#' Set Edges in Sonnet Network
#'
#' Replaces the edges in a sonnet_network object.
#' Expects a data frame with from, to, and optionally weight columns.
#' Updates the from, to, weight vectors and n_edges.
#'
#' @param x A sonnet_network object.
#' @param edges_df A data frame with columns: from, to, and optionally weight.
#' @return The modified sonnet_network object.
#'
#' @seealso \code{\link{as_sonnet}}, \code{\link{get_edges}}, \code{\link{set_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' new_edges <- data.frame(from = c(1, 2), to = c(2, 3), weight = c(0.5, 0.8))
#' net <- set_edges(net, new_edges)
#' get_edges(net)
set_edges <- function(x, edges_df) {
  if (!inherits(x, "sonnet_network")) {
    stop("x must be a sonnet_network object", call. = FALSE)
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

#' Set Layout in Sonnet Network
#'
#' Sets the layout coordinates in a sonnet_network object.
#' Updates the x and y columns in the nodes data frame.
#'
#' @param x A sonnet_network object.
#' @param layout_df A data frame with x and y columns, or a matrix with 2 columns.
#' @return The modified sonnet_network object.
#'
#' @seealso \code{\link{as_sonnet}}, \code{\link{get_nodes}}, \code{\link{sn_layout}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' layout <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
#' net <- set_layout(net, layout)
#' get_nodes(net)
set_layout <- function(x, layout_df) {
  if (!inherits(x, "sonnet_network")) {
    stop("x must be a sonnet_network object", call. = FALSE)
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
# New Lightweight sonnet_network Format
# =============================================================================

#' Convert to Sonnet Network
#'
#' Creates a lightweight sonnet_network object from various network inputs.
#' The resulting object is a named list with all data accessible via \code{$}.
#'
#' @param x Network input. Can be:
#'   - A square numeric matrix (adjacency/weight matrix)
#'   - A data frame with edge list (from, to, optional weight columns)
#'   - An igraph object
#'   - A statnet network object
#'   - A qgraph object
#'   - A tna object
#'   - An existing sonnet_network object (returned as-is)
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @param ... Additional arguments (currently unused).
#'
#' @return A sonnet_network object: a named list with components:
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
#' The sonnet_network format is designed to be:
#' - Simple: All data accessible via \code{net$from}, \code{net$to}, \code{net$weight}, etc.
#' - Modern: Uses named list elements instead of attributes for clean \code{$} access
#' - Compatible: Works seamlessly with splot() and other Sonnet functions
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
#' net <- as_sonnet(mat)
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
#' net <- as_sonnet(g)
#' splot(net)
#' }
as_sonnet <- function(x, directed = NULL, ...) {
  # Return as-is if already a sonnet_network

  if (inherits(x, "sonnet_network")) {
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

  # Extract from/to/weight from edges data frame
  edges <- parsed$edges
  if (!is.null(edges) && nrow(edges) > 0) {
    from_vec <- as.integer(edges$from)
    to_vec <- as.integer(edges$to)
    weight_vec <- if (!is.null(edges$weight)) as.numeric(edges$weight) else rep(1, nrow(edges))
  } else {
    from_vec <- integer(0)
    to_vec <- integer(0)
    weight_vec <- numeric(0)
  }

  # Get nodes data frame

  nodes_df <- parsed$nodes

  # Create the network object with all data as named list elements
  net <- list(
    # Core edge data
    from = from_vec,
    to = to_vec,
    weight = weight_vec,

    # Metadata as list elements (not attributes)
    nodes = nodes_df,
    directed = parsed$directed,
    n_nodes = nrow(nodes_df),
    n_edges = length(from_vec),
    labels = nodes_df$label,
    source = source_type,

    # Optional elements (NULL if not set)
    layout = NULL,
    layout_info = NULL,
    layers = NULL,
    clusters = NULL,
    groups = NULL
  )

  # Set S3 class
  class(net) <- c("sonnet_network", "list")

  net
}

#' Get Nodes from Sonnet Network (Deprecated)
#'
#' Extracts the nodes data frame from a sonnet_network object.
#' \strong{Deprecated}: Use \code{\link{get_nodes}} instead.
#'
#' @param x A sonnet_network object.
#' @return A data frame with columns: id, label, name, x, y (and possibly others).
#'
#' @seealso \code{\link{get_nodes}}, \code{\link{as_sonnet}}, \code{\link{n_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' nodes(net)  # Deprecated, use get_nodes(net) instead
nodes <- function(x) {
  # Soft deprecation warning
  # .Deprecated("get_nodes")
  get_nodes(x)
}

#' Check if Network is Directed
#'
#' Checks whether a sonnet_network is directed.
#'
#' @param x A sonnet_network object.
#' @return Logical: TRUE if directed, FALSE if undirected.
#'
#' @seealso \code{\link{as_sonnet}}
#'
#' @export
#'
#' @examples
#' # Symmetric matrix -> undirected
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' is_directed(net)  # FALSE
#'
#' # Asymmetric matrix -> directed
#' mat2 <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), nrow = 3)
#' net2 <- as_sonnet(mat2)
#' is_directed(net2)  # TRUE
is_directed <- function(x) {
  if (inherits(x, "sonnet_network")) {
    # Check for new list format first (directed stored as list element)
    if (!is.null(x$directed)) {
      return(x$directed)
    }
    # Check for old attr format
    dir_attr <- attr(x, "directed")
    if (!is.null(dir_attr)) {
      return(dir_attr)
    }
    # R6 object in old wrapper
    if (!is.null(x$network) && inherits(x$network, "SonnetNetwork")) {
      return(x$network$is_directed)
    }
  }
  stop("Cannot determine directedness for this object", call. = FALSE)
}

#' Get Number of Nodes
#'
#' Returns the number of nodes in a sonnet_network.
#'
#' @param x A sonnet_network object.
#' @return Integer: number of nodes.
#'
#' @seealso \code{\link{as_sonnet}}, \code{\link{n_edges}}, \code{\link{get_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' n_nodes(net)  # 3
n_nodes <- function(x) {
  if (inherits(x, "sonnet_network")) {
    # Check for new list format first (n_nodes stored as list element)
    if (!is.null(x$n_nodes)) {
      return(x$n_nodes)
    }
    # Check for old attr format
    n_attr <- attr(x, "n_nodes")
    if (!is.null(n_attr)) {
      return(n_attr)
    }
    # R6 object in old wrapper
    if (!is.null(x$network) && inherits(x$network, "SonnetNetwork")) {
      return(x$network$n_nodes)
    }
  }
  stop("Cannot count nodes for this object", call. = FALSE)
}

#' Get Number of Edges
#'
#' Returns the number of edges in a sonnet_network.
#'
#' @param x A sonnet_network object.
#' @return Integer: number of edges.
#'
#' @seealso \code{\link{as_sonnet}}, \code{\link{n_nodes}}
#'
#' @export
#'
#' @examples
#' mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' net <- as_sonnet(mat)
#' n_edges(net)  # 3
n_edges <- function(x) {
  if (inherits(x, "sonnet_network")) {
    # Check for new list format first (n_edges stored as list element)
    if (!is.null(x$n_edges)) {
      return(x$n_edges)
    }
    # Check for old attr format
    n_attr <- attr(x, "n_edges")
    if (!is.null(n_attr)) {
      return(n_attr)
    }
    # R6 object in old wrapper
    if (!is.null(x$network) && inherits(x$network, "SonnetNetwork")) {
      return(x$network$n_edges)
    }
  }
  stop("Cannot count edges for this object", call. = FALSE)
}

