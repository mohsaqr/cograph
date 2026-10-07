#' @title CographNetwork R6 Class
#'
#' @description
#' Core class representing a network for visualization. Stores nodes, edges,
#' layout coordinates, and aesthetic mappings.
#'
#' @return A \code{CographNetwork} R6 object.
#' @export
#' @examples
#' CographNetwork$new(regulation_net)
CographNetwork <- R6::R6Class(
  "CographNetwork",
  public = list(
    #' @description Create a new CographNetwork object.
    #' @param input Network input, such as a matrix, edge list, igraph,
    #'   statnet network, qgraph or tna object. `NULL` creates an empty object.
    #' @param directed Logical. Forces a directed or undirected interpretation.
    #'   `NULL` detects it from the input.
    #' @param nodes `NULL` or a data frame of node attributes. Its rows are
    #'   matched to the node labels by a `name`, `label` or `id` column, tried in
    #'   that order, and the remaining columns are added to the node table. When
    #'   no column matches and the row count equals the number of nodes, the
    #'   columns are added in row order. A `labels` column supplies the display
    #'   labels returned by `$node_labels`.
    #' @param simplify Logical or character. If FALSE (default), every transition
    #'   from tna sequence data is a separate edge. If TRUE or a string
    #'   ("sum", "mean", "max", "min"), duplicate transitions are aggregated.
    #'   Other inputs are not affected.
    #' @return A new CographNetwork object.
    initialize = function(input = NULL, directed = NULL, nodes = NULL,
                          simplify = FALSE) {
      if (!is.null(input)) {
        parsed <- parse_input(input, directed = directed, simplify = simplify)
        private$.nodes <- parsed$nodes
        private$.edges <- parsed$edges
        private$.directed <- parsed$directed
        private$.weights <- parsed$weights

        # Merge nodes metadata if provided as data frame
        if (is.data.frame(nodes)) {
          # Determine which column to use for matching
          # Priority: name > label > id (match against network's internal label)
          net_labels <- private$.nodes$label
          match_col <- NULL
          idx <- NULL

          # Try matching by 'name' column first
          if ("name" %in% names(nodes)) {
            test_idx <- match(net_labels, nodes$name)
            if (sum(!is.na(test_idx)) > 0) {
              match_col <- "name"
              idx <- test_idx
            }
          }

          # Try matching by 'label' column
          if (is.null(idx) && "label" %in% names(nodes)) {
            test_idx <- match(net_labels, nodes$label)
            if (sum(!is.na(test_idx)) > 0) {
              match_col <- "label"
              idx <- test_idx
            }
          }

          # Try matching by 'id' column
          if (is.null(idx) && "id" %in% names(nodes)) {
            test_idx <- match(net_labels, nodes$id)
            if (sum(!is.na(test_idx)) > 0) {
              match_col <- "id"
              idx <- test_idx
            }
          }

          if (!is.null(idx)) {
            # Merge all columns except the match column
            for (col in setdiff(names(nodes), match_col)) {
              private$.nodes[[col]] <- nodes[[col]][idx]
            }
          } else if (nrow(nodes) == nrow(private$.nodes)) {
            # Fallback: assume same order
            for (col in names(nodes)) {
              private$.nodes[[col]] <- nodes[[col]]
            }
          }
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

    #' @description Create a copy with the same nodes, edges, weights, layout,
    #'   aesthetics, theme, layout information and plot parameters.
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
    #' @return The object itself, invisibly.
    set_nodes = function(nodes) {
      private$.nodes <- nodes
      invisible(self)
    },

    #' @description Set edges data frame.
    #' @param edges Data frame with edge information.
    #' @return The object itself, invisibly.
    set_edges = function(edges) {
      private$.edges <- edges
      invisible(self)
    },

    #' @description Set directed flag.
    #' @param directed Logical.
    #' @return The object itself, invisibly.
    set_directed = function(directed) {
      private$.directed <- directed
      invisible(self)
    },

    #' @description Set edge weights.
    #' @param weights Numeric vector of edge weights, one per edge.
    #' @return The object itself, invisibly.
    set_weights = function(weights) {
      private$.weights <- weights
      invisible(self)
    },

    #' @description Set layout coordinates.
    #' @param coords Matrix or data frame with at least two columns and one row
    #'   per node. The first two columns are renamed `x` and `y` and are also
    #'   written to the node table. `NULL` leaves the layout unchanged.
    #' @return The object itself, invisibly.
    set_layout_coords = function(coords) {
      if (!is.null(coords)) {
        if (is.matrix(coords)) {
          coords <- as.data.frame(coords)
        }
        if (!is.data.frame(coords) || ncol(coords) < 2) {
          stop("coords must be a data frame or matrix with at least two columns",
               call. = FALSE)
        }
        names(coords)[1:2] <- c("x", "y")
        if (!is.null(private$.nodes) && nrow(private$.nodes) != nrow(coords)) {
          stop("coords must have one row per node (expected ",
               nrow(private$.nodes), ", got ", nrow(coords), ")",
               call. = FALSE)
        }
        private$.layout <- coords
        # Update node positions
        if (!is.null(private$.nodes)) {
          private$.nodes$x <- coords$x
          private$.nodes$y <- coords$y
        }
      }
      invisible(self)
    },

    #' @description Set node aesthetics. The list is merged into the current
    #'   node aesthetics.
    #' @param aes Named list of aesthetic parameters.
    #' @return The object itself, invisibly.
    set_node_aes = function(aes) {
      private$.node_aes <- utils::modifyList(private$.node_aes, aes)
      invisible(self)
    },

    #' @description Set edge aesthetics. The list is merged into the current
    #'   edge aesthetics.
    #' @param aes Named list of aesthetic parameters.
    #' @return The object itself, invisibly.
    set_edge_aes = function(aes) {
      private$.edge_aes <- utils::modifyList(private$.edge_aes, aes)
      invisible(self)
    },

    #' @description Set theme.
    #' @param theme CographTheme object or theme name.
    #' @return The object itself, invisibly.
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
    #' @return A data frame with `x` and `y` columns, or `NULL` when no layout
    #'   is set.
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
    #' @return The stored theme (a CographTheme object or theme name), or
    #'   `NULL`.
    get_theme = function() {
      private$.theme
    },

    #' @description Set layout info.
    #' @param info List with layout information (name, seed, etc.).
    #' @return The object itself, invisibly.
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
    #' @return The object itself, invisibly.
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
    #' @return The object itself, invisibly.
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

    #' @field has_weights `TRUE` when any edge weight differs from 1.
    has_weights = function() {
      !is.null(private$.weights) && any(private$.weights != 1)
    },

    #' @field node_labels Vector of node labels, taken from the `labels` column
    #'   of the node table when present and from `label` otherwise.
    node_labels = function() {
      if (is.null(private$.nodes)) {
        NULL
      } else if (!is.null(private$.nodes$labels)) {
        private$.nodes$labels
      } else {
        private$.nodes$label
      }
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
#' @noRd
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
#' @param meta List with consolidated metadata: source, layout, tna sub-fields.
#' @param weights Full n×n weight matrix when available, or NULL.
#' @param data Original estimation data (sequence matrix, edge list, etc.), or NULL.
#' @param node_groups Optional node groupings data frame.
#' @param type Optional source/type string stored in \code{meta$type}.
#' @return A cograph_network object (named list with class).
#' @keywords internal
#' @noRd
.create_cograph_network <- function(
    nodes,
    edges,
    directed,
    meta = list(),
    weights = NULL,
    data = NULL,
    node_groups = NULL,
    type = NULL
) {
  # Ensure edges data frame has standard columns, preserving extra columns
  # (e.g. session, time from temporal edge lists). An empty edge table keeps
  # its column skeleton so that names(get_edges(net)) is stable across a
  # filter that removed every row.
  if (is.null(edges) || !is.data.frame(edges)) {
    edges <- data.frame(from = integer(0), to = integer(0), weight = numeric(0))
  }
  edges_df <- data.frame(
    from = as.integer(edges$from),
    to = as.integer(edges$to),
    weight = if (!is.null(edges$weight)) as.numeric(edges$weight) else rep(1, nrow(edges)),
    stringsAsFactors = FALSE
  )
  extra_cols <- setdiff(names(edges), c("from", "to", "weight"))
  if (length(extra_cols) > 0) {
    edges_df[extra_cols] <- edges[extra_cols]
  }

  # Ensure meta has required sub-fields
  if (is.null(meta$source)) meta$source <- "unknown"
  if (is.null(meta$layout)) meta$layout <- NULL
  if (is.null(meta$tna)) meta$tna <- NULL
  if (!is.null(type)) meta$type <- type

  # Build the lean network object
  net <- list(
    # Core data
    nodes = nodes,
    edges = edges_df,
    directed = directed,

    # Full matrix (for to_matrix round-trip)
    weights = weights,

    # Original estimation data
    data = data,

    # Consolidated metadata
    meta = meta,

    # Optional groupings
    node_groups = node_groups
  )

  # Set S3 class
  class(net) <- c("cograph_network", "list")

  net
}


# =============================================================================
# Getter Functions for cograph_network
# =============================================================================

#' Access and Modify a Cograph Network
#'
#' These functions read and replace the parts of a \code{cograph_network}
#' object created by \code{\link{as_cograph}}. The getters return the node
#' table, the edge table, the node labels, the node groups, the source type,
#' the stored estimation data, the metadata list, and the counts of nodes and
#' edges. The setters return a modified copy of the network.
#'
#' @param x A \code{cograph_network} object. \code{is_directed()} also accepts
#'   a \code{\link{CographNetwork}} or an igraph object.
#' @param nodes_df A data frame of node information. A missing \code{id}
#'   column is filled with row numbers and a missing \code{label} column with
#'   the ids. The stored weight matrix is rebuilt from the new node table.
#' @param edges_df A data frame with columns \code{from} and \code{to}
#'   (integer row numbers into the node table) and an optional \code{weight}
#'   column, which defaults to 1. Extra columns are kept. Each edge may appear
#'   once, and an undirected network counts A-B and B-A as the same edge.
#' @param layout_df A data frame with \code{x} and \code{y} columns, or a
#'   matrix whose first two columns are used, with one row per node.
#' @param groups Node groupings in one of these formats:
#'   \itemize{
#'     \item A character string naming a community detection method of
#'       \code{\link{detect_communities}} (\code{"louvain"},
#'       \code{"walktrap"}, \code{"fast_greedy"}, \code{"label_prop"},
#'       \code{"infomap"}, \code{"leiden"}), which requires the igraph
#'       package.
#'     \item A named list mapping each group name to a vector of node labels,
#'       for example \code{list(A = c("N1", "N2"), B = c("N3", "N4"))}.
#'     \item An unnamed vector with one group assignment per node, in node order.
#'     \item A data frame with a \code{node} (or \code{nodes}) column and one of
#'       \code{layer}, \code{cluster} or \code{group} (plural forms are accepted
#'       and normalized to the singular).
#'     \item \code{NULL}, in which case \code{nodes} and one of \code{layers} or
#'       \code{clusters} supply the grouping.
#'   }
#' @param type Group type stored by \code{set_groups()}. One of
#'   \code{"group"} (default), \code{"cluster"} or \code{"layer"}. It is ignored
#'   when \code{layers} or \code{clusters} is given, since the type then follows
#'   from the argument used.
#' @param nodes Character vector of node labels, used with \code{layers} or
#'   \code{clusters} to give groupings as vectors. When \code{NULL}, the
#'   assignments follow the node order of the network.
#' @param layers Character or factor vector of layer assignments, the same
#'   length as \code{nodes}.
#' @param clusters Character or factor vector of cluster assignments, the same
#'   length as \code{nodes}.
#'
#' @return
#' \describe{
#'   \item{\code{get_nodes()}, \code{nodes()}}{The node table, with \code{id}
#'     and \code{label} columns plus layout coordinates or other metadata
#'     columns when present. \code{nodes()} is a deprecated alias of
#'     \code{get_nodes()}.}
#'   \item{\code{get_edges()}}{A data frame with one row per edge and columns
#'     \code{from} and \code{to} (integer row numbers into the node table),
#'     \code{weight}, and any extra edge columns. An undirected network stores
#'     one row per unordered pair. \code{\link[=as_cograph]{as.data.frame}()}
#'     returns the same table with node labels as endpoints.}
#'   \item{\code{get_labels()}}{A character vector of node labels.}
#'   \item{\code{get_groups()}}{A data frame with a \code{node} column and one
#'     of \code{layer}, \code{cluster} or \code{group}, or \code{NULL} when no
#'     groups are set.}
#'   \item{\code{get_source()}}{A character string naming the input type (for
#'     example \code{"matrix"}, \code{"tna"}, \code{"igraph"},
#'     \code{"edgelist"}), or \code{"unknown"}.}
#'   \item{\code{get_data()}}{The original estimation data (for example the
#'     sequence data of a tna model), or \code{NULL} when none is stored.}
#'   \item{\code{get_meta()}}{A list with component \code{source} (input
#'     type). Networks built from a tna model also carry \code{tna} (type,
#'     group name and group index).}
#'   \item{\code{n_nodes()}, \code{n_edges()}}{An integer count. Each
#'     undirected edge counts once.}
#'   \item{\code{is_directed()}}{A single logical value.}
#'   \item{\code{set_nodes()}, \code{set_edges()}, \code{set_layout()},
#'     \code{set_groups()}}{The modified \code{cograph_network}.
#'     \code{set_layout()} writes the coordinates into the \code{x} and
#'     \code{y} columns of the node table. \code{set_groups()} stores the
#'     grouping as \code{node_groups} for use by the group-aware plot
#'     functions. It stops with an error when a node is assigned twice, when
#'     a node is missing from the assignment or unknown to the network, and
#'     when fewer than two groups result. \code{set_edges()} raises an error
#'     of class \code{cograph_bad_selection} for endpoints outside the node
#'     table and for duplicated edges.}
#' }
#'
#' @seealso \code{\link{as_cograph}}, \code{\link{splot}},
#'   \code{\link{detect_communities}}
#'
#' @export
#'
#' @examples
#' net <- as_cograph(regulation_net)
#' get_edges(net)
get_nodes <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Unified format: nodes stored as list element
    if (!is.null(x$nodes) && is.data.frame(x$nodes)) {
      return(x$nodes)
    }
  }
  stop("Cannot extract nodes from this object", call. = FALSE)
}

#' @rdname get_nodes
#' @export
get_edges <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Edges stored as data frame
    if (!is.null(x$edges) && is.data.frame(x$edges)) {
      return(x$edges)
    }
    # Empty edges
    return(data.frame(from = integer(0), to = integer(0), weight = numeric(0)))
  }
  stop("Cannot extract edges from this object", call. = FALSE)
}

#' @rdname get_nodes
#' @export
get_labels <- function(x) {

  if (inherits(x, "cograph_network")) {
    # Compute from nodes data frame (priority: labels > label)
    nodes <- x$nodes
    if (!is.null(nodes)) {
      if ("labels" %in% names(nodes)) {
        return(nodes$labels)
      } else if ("label" %in% names(nodes)) {
        return(nodes$label)
      }
    }
  }
  stop("Cannot extract labels from this object", call. = FALSE)
}

#' @rdname get_nodes
#' @export
get_source <- function(x) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }
  x$meta$source %||% "unknown"
}

#' @rdname get_nodes
#' @export
get_data <- function(x) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }
  x$data
}

#' @rdname get_nodes
#' @export
get_meta <- function(x) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }
  x$meta
}

# =============================================================================
# Setter Functions for cograph_network
# =============================================================================

#' @rdname get_nodes
#' @export
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

  x$nodes <- nodes_df

  # The stored weight matrix is keyed by label and sized by node count, so it
  # has to follow the node table rather than go stale behind it.
  x$weights <- .network_weight_matrix(as.character(nodes_df$label),
                                      get_edges(x), isTRUE(x$directed))

  x
}

#' @rdname get_nodes
#' @export
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

  n <- nrow(get_nodes(x))
  endpoints <- c(edges_df$from, edges_df$to)
  if (length(endpoints) > 0 && (anyNA(endpoints) ||
        min(endpoints) < 1 || max(endpoints) > n)) {
    stop(errorCondition(
      paste0("edges_df refers to node indices outside 1:", n, "."),
      class = "cograph_bad_selection", call = NULL))
  }

  # For an undirected network A->B and B->A are one edge; storing both would
  # leave the edge table claiming two edges where the matrix holds one.
  keys <- .edge_key(edges_df, isTRUE(x$directed))
  if (anyDuplicated(keys) > 0L) {
    stop(errorCondition(
      paste0("edges_df names the same edge more than once: ",
             paste(unique(keys[duplicated(keys)]), collapse = ", "),
             ". Supply each edge once."),
      class = "cograph_bad_selection", call = NULL))
  }

  # Store the edge table, keeping extra columns, and rebuild the weight
  # matrix so that to_matrix() and the edge table cannot disagree.
  standard <- data.frame(
    from = as.integer(edges_df$from),
    to = as.integer(edges_df$to),
    weight = as.numeric(edges_df$weight),
    stringsAsFactors = FALSE
  )
  extra_cols <- setdiff(names(edges_df), c("from", "to", "weight"))
  if (length(extra_cols) > 0) {
    standard[extra_cols] <- edges_df[extra_cols]
  }

  x$edges <- standard
  x$weights <- .network_weight_matrix(as.character(get_nodes(x)$label),
                                      standard, isTRUE(x$directed))

  x
}

#' @rdname get_nodes
#' @export
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

  x
}

# =============================================================================
# New Lightweight cograph_network Format
# =============================================================================

#' Convert to Cograph Network
#'
#' \code{as_cograph()} creates a \code{cograph_network} object from a matrix,
#' an edge list, or a network object of another package. \code{to_cograph()}
#' is an alias. The object is a named list that every cograph function
#' accepts.
#'
#' @param x Network input. One of a square numeric weight matrix, a data frame
#'   edge list, an igraph object, a statnet network object, a qgraph object, a
#'   tna object, or an existing \code{cograph_network}, which is returned as
#'   it is. In an edge list the endpoint columns are found by name, ignoring
#'   case (\code{from}, \code{source}, \code{src}, \code{v1}, \code{node1}
#'   or \code{i}, and \code{to}, \code{target}, \code{tgt}, \code{v2},
#'   \code{node2} or \code{j}), and otherwise the first two columns are
#'   used. An optional weight column is found by the names \code{weight},
#'   \code{w}, \code{value} or \code{strength}. For \code{as.data.frame()},
#'   a \code{cograph_network}.
#' @param directed Logical. Forces a directed or undirected interpretation.
#'   \code{NULL} (default) detects it from the input.
#' @param simplify Logical or character. If \code{FALSE} (default), every
#'   transition from tna sequence data is a separate edge. If \code{TRUE}
#'   (equivalent to \code{"sum"}) or one of \code{"sum"}, \code{"mean"},
#'   \code{"max"}, \code{"min"}, duplicate transitions are aggregated with that
#'   function. Other inputs are not affected.
#' @param row.names \code{NULL} or a character vector of row names, as for
#'   \code{\link[base]{as.data.frame}}.
#' @param optional Logical, as for \code{\link[base]{as.data.frame}}. It is
#'   ignored, and the column names are always the documented ones.
#' @param what Which table \code{as.data.frame()} returns, \code{"edges"}
#'   (default) or \code{"nodes"}.
#' @param ... Passed from \code{to_cograph()} to \code{as_cograph()};
#'   otherwise unused.
#'
#' @return \code{as_cograph()} and \code{to_cograph()} return a
#'   \code{cograph_network} object, a named list with components:
#'   \describe{
#'     \item{\code{nodes}}{Data frame with \code{id}, \code{label}, and optional
#'       layout or metadata columns.}
#'     \item{\code{edges}}{Data frame with integer \code{from} and \code{to}
#'       columns (row numbers into \code{nodes}), \code{weight}, and extra
#'       columns such as \code{session} and \code{time} for tna input. Repeated
#'       rows of an edge list are kept as separate edges.}
#'     \item{\code{directed}}{Logical. Whether the network is directed.}
#'     \item{\code{weights}}{The n x n weight matrix for matrix and tna input,
#'       or \code{NULL} (for example for an edge list).}
#'     \item{\code{data}}{The original estimation data (sequence data, edge
#'       list), or \code{NULL}.}
#'     \item{\code{meta}}{Metadata list with \code{source} (input type), a
#'       \code{tna} entry for tna input (type, group name and group index)
#'       and, optionally, \code{splot} (rendering hints read by
#'       \code{\link{splot}}).}
#'     \item{\code{node_groups}}{Optional data frame of node groupings.}
#'   }
#'
#'   \code{as.data.frame()} returns a base data frame. For
#'   \code{what = "edges"} it has one row per edge with columns \code{from} and
#'   \code{to} (node labels), \code{weight}, and any extra edge columns the
#'   network carries, such as \code{session} or columns computed by
#'   \code{\link{mutate_edges}}. For \code{what = "nodes"} it has one row per
#'   node with the node table columns (\code{id}, \code{label}, layout
#'   coordinates and any custom columns). \code{\link{to_df}} returns only
#'   \code{from}, \code{to} and \code{weight}.
#'
#' @details
#' A \code{cograph_network} prints a short description of its nodes, edges and
#' source, \code{summary()} reports counts and edge weight statistics,
#' \code{plot()} plots it with \code{\link{sn_render}}, and
#' \code{as.data.frame()} returns its edge or node table.
#' The accessor and setter functions are documented in \code{\link{get_nodes}}.
#'
#' Producer packages may attach plotting hints under \code{meta$splot}. The
#' recognized fields are \code{renderer} (the cograph renderer to use),
#' \code{weight} (the edge column or matrix plotted as \code{weight}), and
#' \code{defaults} (a named list of renderer arguments). Entries in
#' \code{defaults} are defaults only, and arguments passed to \code{\link{splot}}
#' override them. \code{renderer} and \code{weight} define which view is plotted
#' and are not overridden by plot arguments.
#'
#' @seealso \code{\link{get_nodes}}, \code{\link{splot}}, \code{\link{to_df}}
#'
#' @export
#'
#' @examples
#' net <- as_cograph(regulation_net)
#' as.data.frame(net, what = "edges")
as_cograph <- function(x, directed = NULL, simplify = FALSE, ...) {
  # Return as-is if already a cograph_network

  if (inherits(x, "cograph_network")) {
    return(x)
  }

  # Parse the input
  parsed <- parse_input(x, directed = directed, simplify = simplify)

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
    "unknown" # nocov
  }

  # Get full weight matrix if available
  weights_matrix <- NULL
  if (!is.null(parsed$weights_matrix)) {
    # Use weights matrix from parse_input if provided
    weights_matrix <- parsed$weights_matrix
  } else if (is.matrix(x) && nrow(x) == ncol(x)) {
    # Square matrix input: preserve it
    weights_matrix <- x
  }

  # Create minimal TNA metadata (without model/parent)
  tna_meta <- NULL
  if (!is.null(parsed$tna)) {
    tna_meta <- list(
      type = parsed$tna$type,
      group_name = parsed$tna$group_name,
      group_index = parsed$tna$group_index
    )
  }

  # Capture raw data for $data field
  raw_data <- if (inherits(x, "tna")) {
    x$data
  } else if (is.data.frame(x)) {
    x
  } else {
    NULL
  }

  # Use lean constructor
  .create_cograph_network(
    nodes = parsed$nodes,
    edges = parsed$edges,
    directed = parsed$directed,
    meta = list(source = source_type, layout = NULL, tna = tna_meta),
    weights = weights_matrix,
    data = raw_data
  )
}

#' @rdname as_cograph
#' @export
to_cograph <- function(x, directed = NULL, ...) {
  as_cograph(x, directed = directed, ...)
}

#' @rdname get_nodes
#' @export
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
      matches <- intersect(names(col_map), names(df))
      names(df)[match(matches, names(df))] <- col_map[matches]

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

#' @rdname get_nodes
#' @export
get_groups <- function(x) {
  if (!inherits(x, "cograph_network")) {
    stop("x must be a cograph_network object", call. = FALSE)
  }
  x$node_groups
}

#' @rdname get_nodes
#' @export
nodes <- function(x) {
  # Soft deprecation warning
  # .Deprecated("get_nodes")
  get_nodes(x)
}

#' @rdname get_nodes
#' @export
is_directed <- function(x) {
  if (inherits(x, "CographNetwork")) {
    return(x$is_directed)
  }
  if (inherits(x, "cograph_network")) {
    # Unified format: directed stored as list element
    if (!is.null(x$directed)) {
      return(x$directed)
    }
  }
  if (inherits(x, "igraph")) {
    # An object can carry the igraph class without the package being installed
    # (for instance after readRDS()), so this branch needs its own guard.
    .need_igraph("is_directed()")
    return(igraph::is_directed(x))
  }
  stop("Cannot determine directedness for this object", call. = FALSE)
}

#' @rdname get_nodes
#' @export
n_nodes <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Compute from nodes data frame
    if (!is.null(x$nodes)) {
      return(nrow(x$nodes))
    }
    return(0L)
  }
  stop("Cannot count nodes for this object", call. = FALSE)
}

#' @rdname get_nodes
#' @export
n_edges <- function(x) {
  if (inherits(x, "cograph_network")) {
    # Compute from edges data frame
    if (!is.null(x$edges)) {
      return(nrow(x$edges))
    }
    return(0L)
  }
  stop("Cannot count edges for this object", call. = FALSE)
}

#' @rdname as_cograph
#' @export
as.data.frame.cograph_network <- function(x, row.names = NULL, optional = FALSE,
                                          ..., what = c("edges", "nodes")) {
  what <- match.arg(what)

  df <- if (what == "nodes") {
    nodes <- get_nodes(x)
    rownames(nodes) <- NULL
    nodes
  } else {
    # The accessor hands back the edge table whole, computed columns included.
    # to_data_frame() is the narrow conversion verb and is not used here.
    .edges_as_df(x)
  }

  if (!is.null(row.names)) {
    rownames(df) <- row.names
  }
  df
}
