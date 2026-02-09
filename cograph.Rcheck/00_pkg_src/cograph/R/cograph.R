#' @title Main Entry Point
#' @description The primary function for creating network visualizations.
#' @name cograph-main
NULL

#' Auto-convert input to cograph_network
#'
#' Internal helper that converts matrices, data frames, igraph, network,
#' qgraph, or tna objects to cograph_network objects automatically.
#' Uses the unified cograph_network format.
#'
#' @param x Input object (matrix, data.frame, igraph, network, qgraph, tna, or cograph_network).
#' @param layout Default layout to use if converting.
#' @param seed Random seed for deterministic layouts.
#' @param directed Force directed interpretation. NULL for auto-detect.
#' @param ... Additional arguments passed to cograph().
#' @return A cograph_network object (unified format).
#' @noRd
ensure_cograph_network <- function(x, layout = "spring", seed = 42, directed = NULL, ...) {

  if (inherits(x, "cograph_network")) {
    # Check if layout needs to be computed
    nodes <- get_nodes(x)
    if (!is.null(nodes) && (!"x" %in% names(nodes) || all(is.na(nodes$x)))) {
      # Need to compute layout
      x <- compute_layout_for_cograph(x, layout = layout, seed = seed, ...)
    }
    # Override directed if explicitly specified
    if (!is.null(directed)) {
      x$directed <- directed
    }
    return(x)
  }

  if (is.matrix(x) || is.data.frame(x) || inherits(x, "igraph") ||
      inherits(x, "network") || inherits(x, "qgraph") || inherits(x, "tna")) {
    return(cograph(x, layout = layout, seed = seed, directed = directed, ...))
  }

  stop("Input must be a matrix, data.frame, igraph, network, qgraph, tna, or cograph_network",
       call. = FALSE)
}

#' Compute layout for cograph_network
#'
#' Computes layout coordinates for a cograph_network object that doesn't have them.
#'
#' @param net A cograph_network object (unified format).
#' @param layout Layout algorithm name.
#' @param seed Random seed for deterministic layouts.
#' @param ... Additional arguments passed to the layout function.
#' @return The cograph_network with layout coordinates added.
#' @noRd
compute_layout_for_cograph <- function(net, layout = "spring", seed = 42, ...) {
  # Get nodes data frame
  nodes <- get_nodes(net)

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Two-letter igraph layout codes
  igraph_codes <- c("kk", "fr", "drl", "mds", "go", "tr", "st", "gr", "rd", "ni", "ci", "lgl", "sp")

  # Build edges for layout computation
  edges <- get_edges(net)

  # Get directed status
  net_directed <- is_directed(net)

  # Create temporary R6 network for layout computation
  temp_net <- CographNetwork$new()
  temp_net$set_nodes(nodes)
  temp_net$set_edges(edges)
  temp_net$set_directed(net_directed)

  # Compute layout
  if (is.function(layout)) {
    coords <- apply_igraph_layout(temp_net, layout, ...)
  } else if (is.character(layout) && (
    grepl("^(igraph_|layout_)", layout) || layout %in% igraph_codes
  )) {
    coords <- apply_igraph_layout_by_name(temp_net, layout, seed = seed, ...)
  } else if (is.matrix(layout) || is.data.frame(layout)) {
    coords <- as.data.frame(layout)
    if (ncol(coords) >= 2) {
      names(coords)[1:2] <- c("x", "y")
    }
  } else {
    layout_obj <- CographLayout$new(layout, ...)
    coords <- layout_obj$compute(temp_net, ...)
  }

  # Update nodes with layout coordinates
  nodes$x <- coords$x
  nodes$y <- coords$y

  # Update network directly
  net$nodes <- nodes
  net$layout <- coords
  net$layout_info <- list(
    name = if (is.function(layout)) "custom_function" else as.character(layout),
    seed = seed
  )

  net
}

#' Create a Network Visualization
#'
#' The main entry point for cograph. Accepts adjacency matrices, edge lists,
#' igraph, statnet network, qgraph, or tna objects and creates a visualization-ready
#' network object.
#'
#' @param input Network input. Can be:
#'   - A square numeric matrix (adjacency/weight matrix)
#'   - A data frame with edge list (from, to, optional weight columns)
#'   - An igraph object
#'   - A statnet network object
#'   - A qgraph object
#'   - A tna object
#' @param layout Layout algorithm: "circle", "spring", "groups", "grid",
#'   "random", "star", "bipartite", or "custom". Default "spring".
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @param node_labels Character vector of node labels.
#' @param seed Random seed for deterministic layouts. Default 42. Set NULL for random.
#' @param ... Additional arguments passed to the layout function.
#'
#' @return A cograph_network object that can be further customized and rendered.
#'
#' @seealso
#' \code{\link{splot}} for base R graphics rendering,
#' \code{\link{soplot}} for grid graphics rendering,
#' \code{\link{sn_nodes}} for node customization,
#' \code{\link{sn_edges}} for edge customization,
#' \code{\link{sn_layout}} for changing layouts,
#' \code{\link{sn_theme}} for visual themes,
#' \code{\link{sn_palette}} for color palettes,
#' \code{\link{from_qgraph}} and \code{\link{from_tna}} for converting external objects
#'
#' @export
#'
#' @examples
#' # From adjacency matrix
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' cograph(adj)
#'
#' # From edge list
#' edges <- data.frame(from = c(1, 1, 2), to = c(2, 3, 3))
#' cograph(edges)
#'
#' # With customization (pipe-friendly workflow)
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' cograph(adj, layout = "circle") |>
#'   sn_nodes(fill = "steelblue") |>
#'   sn_edges(color = "gray50") |>
#'   splot()
#'
#' # Weighted network with automatic styling
#' w_adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, 0.4, -0.3, 0.4, 0), nrow = 3)
#' cograph(w_adj) |>
#'   sn_edges(color = "weight", width = "weight") |>
#'   splot()
#'
#' # With igraph (if installed)
#' \dontrun{
#' library(igraph)
#' g <- make_ring(10)
#' cograph(g) |> splot()
#' }
cograph <- function(input, layout = "spring", directed = NULL,
                   node_labels = NULL, seed = 42, ...) {

  # Parse input first to get TNA metadata if applicable
  parsed <- parse_input(input, directed = directed)

  # Determine source type
  source_type <- if (is.matrix(input)) {
    "matrix"
  } else if (is.data.frame(input)) {
    "edgelist"
  } else if (inherits(input, "igraph")) {
    "igraph"
  } else if (inherits(input, "network")) {
    "network"
  } else if (inherits(input, "qgraph")) {
    "qgraph"
  } else if (inherits(input, "tna")) {
    "tna"
  } else {
    "unknown"
  }

  # Get full weight matrix if available
  weights_matrix <- NULL
  if (!is.null(parsed$tna) && !is.null(parsed$tna$model)) {
    # TNA: use model's weights matrix
    weights_matrix <- parsed$tna$model$weights
  } else if (is.matrix(input) && nrow(input) == ncol(input)) {
    # Square matrix input: preserve it
    weights_matrix <- input
  }

  # Create temporary R6 network object for layout computation
  network <- CographNetwork$new(
    input = input,
    directed = directed,
    node_labels = node_labels
  )

  # Apply default theme
  default_theme <- get_theme("classic")
  network$set_theme(default_theme)

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Two-letter igraph layout codes
  igraph_codes <- c("kk", "fr", "drl", "mds", "go", "tr", "st", "gr", "rd", "ni", "ci", "lgl", "sp")

  # Compute layout - handle igraph layouts
  if (is.function(layout)) {
    # igraph layout function passed directly
    coords <- apply_igraph_layout(network, layout, ...)
  } else if (is.character(layout) && (
    grepl("^(igraph_|layout_)", layout) || layout %in% igraph_codes
  )) {
    # igraph layout by name or two-letter code
    coords <- apply_igraph_layout_by_name(network, layout, seed = seed, ...)
  } else if (is.matrix(layout) || is.data.frame(layout)) {
    # Custom coordinates passed directly
    coords <- as.data.frame(layout)
    if (ncol(coords) >= 2) {
      names(coords)[1:2] <- c("x", "y")
    }
  } else {
    # Built-in cograph layout
    layout_obj <- CographLayout$new(layout, ...)
    coords <- layout_obj$compute(network, ...)
  }
  network$set_layout_coords(coords)

  # Get nodes with layout coordinates
  nodes_with_layout <- network$get_nodes()

  # Store layout info
  layout_info <- list(
    name = if (is.function(layout)) "custom_function" else as.character(layout),
    seed = seed
  )

  # Create unified format using constructor
  .create_cograph_network(
    nodes = nodes_with_layout,
    edges = network$get_edges(),
    directed = network$is_directed,
    source = source_type,
    layout = coords,
    layout_info = layout_info,
    node_aes = network$get_node_aes(),
    edge_aes = network$get_edge_aes(),
    theme = network$get_theme(),
    plot_params = network$get_plot_params(),
    tna = parsed$tna,
    weights = weights_matrix
  )
}

#' Apply Layout to Network
#'
#' Apply a layout algorithm to compute node positions.
#'
#' @param network A cograph_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param layout Layout algorithm name or a CographLayout object.
#' @param seed Random seed for deterministic layouts. Default 42. Set NULL for random.
#' @param ... Additional arguments passed to the layout function.
#'
#' @details
#' ## Built-in Layouts
#' \describe{
#'   \item{\strong{spring}}{Force-directed layout (Fruchterman-Reingold style).
#'     Good general-purpose layout. Default.}
#'   \item{\strong{circle}}{Nodes arranged in a circle. Good for small networks
#'     or when structure is less important.}
#'   \item{\strong{groups}}{Circular layout with grouped nodes clustered together.}
#'   \item{\strong{grid}}{Nodes in a regular grid.}
#'   \item{\strong{random}}{Random positions. Useful as starting point.}
#'   \item{\strong{star}}{Central node with others arranged around it.}
#'   \item{\strong{bipartite}}{Two-column layout for bipartite networks.}
#' }
#'
#' ## igraph Layouts
#' Two-letter codes for igraph layouts: "kk" (Kamada-Kawai), "fr" (Fruchterman-Reingold),
#' "drl", "mds", "ni" (nicely), "tr" (tree), "ci" (circle), etc.
#'
#' You can also pass igraph layout functions directly or use full names like
#' "layout_with_kk".
#'
#' @return Modified cograph_network object.
#'
#' @seealso
#' \code{\link{cograph}} for network creation,
#' \code{\link{sn_nodes}} for node customization,
#' \code{\link{sn_edges}} for edge customization,
#' \code{\link{sn_theme}} for visual themes,
#' \code{\link{splot}} and \code{\link{soplot}} for plotting
#'
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#'
#' # Built-in layouts
#' cograph(adj) |> sn_layout("circle") |> splot()
#' cograph(adj) |> sn_layout("spring") |> splot()
#'
#' # igraph layouts (if igraph installed)
#' \dontrun{
#' cograph(adj) |> sn_layout("kk") |> splot()
#' cograph(adj) |> sn_layout("fr") |> splot()
#' }
#'
#' # Custom coordinates
#' coords <- matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2, byrow = TRUE)
#' cograph(adj) |> sn_layout(coords) |> splot()
#'
#' # Direct matrix input (auto-converts)
#' adj |> sn_layout("circle")
sn_layout <- function(network, layout, seed = 42, ...) {
  # Auto-convert matrix/data.frame/igraph to cograph_network
  network <- ensure_cograph_network(network, layout = layout, seed = seed, ...)

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Two-letter igraph layout codes
  igraph_codes <- c("kk", "fr", "drl", "mds", "go", "tr", "st", "gr", "rd", "ni", "ci", "lgl", "sp")

  # Create a temporary R6 network for layout computation
  temp_net <- CographNetwork$new()
  temp_net$set_nodes(get_nodes(network))
  temp_net$set_edges(get_edges(network))
  temp_net$set_directed(is_directed(network))

  # Handle igraph layout functions
  if (is.function(layout)) {
    coords <- apply_igraph_layout(temp_net, layout, ...)
    layout_info <- list(name = "custom_function", seed = seed, coords = coords)
  } else if (is.character(layout)) {
    # Check if it's an igraph layout name or two-letter code
    if (grepl("^(igraph_|layout_)", layout) || layout %in% igraph_codes) {
      coords <- apply_igraph_layout_by_name(temp_net, layout, seed = seed, ...)
      layout_info <- list(name = layout, seed = seed, coords = coords)
    } else {
      layout_obj <- CographLayout$new(layout, ...)
      coords <- layout_obj$compute(temp_net, ...)
      layout_info <- list(name = layout, seed = seed, coords = coords)
    }
  } else if (inherits(layout, "CographLayout")) {
    coords <- layout$compute(temp_net, ...)
    layout_info <- list(name = "custom", seed = seed, coords = coords)
  } else if (is.matrix(layout) || is.data.frame(layout)) {
    coords <- as.data.frame(layout)
    if (ncol(coords) >= 2) {
      names(coords)[1:2] <- c("x", "y")
    }
    layout_info <- list(name = "custom", seed = seed, coords = coords)
  } else {
    stop("layout must be a string, CographLayout object, igraph layout function, or coordinate matrix",
         call. = FALSE)
  }

  # Update nodes with layout coordinates
  nodes_updated <- get_nodes(network)
  nodes_updated$x <- coords$x
  nodes_updated$y <- coords$y

  # Return updated network (modify list directly)
  network$nodes <- nodes_updated
  network$layout <- coords
  network$layout_info <- layout_info

  network
}

#' Apply Theme to Network
#'
#' Apply a visual theme to the network.
#'
#' @param network A cograph_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param theme Theme name (string) or CographTheme object.
#' @param ... Additional theme parameters to override.
#'
#' @details
#' ## Available Themes
#' \describe{
#'   \item{\strong{classic}}{Default theme with white background, blue nodes, gray edges.}
#'   \item{\strong{dark}}{Dark background with light nodes. Good for presentations.}
#'   \item{\strong{minimal}}{Subtle styling with thin edges and muted colors.}
#'   \item{\strong{colorblind}}{Optimized for color vision deficiency.}
#'   \item{\strong{grayscale}}{Black and white only.}
#'   \item{\strong{vibrant}}{Bold, saturated colors.}
#' }
#'
#' Use \code{list_themes()} to see all available themes.
#'
#' @return Modified cograph_network object.
#'
#' @seealso
#' \code{\link{cograph}} for network creation,
#' \code{\link{sn_palette}} for color palettes,
#' \code{\link{sn_nodes}} for node customization,
#' \code{\link{sn_edges}} for edge customization,
#' \code{\link{list_themes}} to see available themes,
#' \code{\link{splot}} and \code{\link{soplot}} for plotting
#'
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#'
#' # Apply different themes
#' cograph(adj) |> sn_theme("dark") |> splot()
#' cograph(adj) |> sn_theme("minimal") |> splot()
#'
#' # Override specific theme properties
#' cograph(adj) |> sn_theme("classic", background = "lightgray") |> splot()
#'
#' # Direct matrix input
#' adj |> sn_theme("dark")
sn_theme <- function(network, theme, ...) {
  # Auto-convert matrix/data.frame/igraph to cograph_network
  network <- ensure_cograph_network(network)

  # Get theme object
  if (is.character(theme)) {
    theme_obj <- get_theme(theme)
    if (is.null(theme_obj)) {
      stop("Unknown theme: ", theme, ". Available: ",
           paste(list_themes(), collapse = ", "), call. = FALSE)
    }
  } else if (inherits(theme, "CographTheme")) {
    theme_obj <- theme
  } else {
    stop("theme must be a string or CographTheme object", call. = FALSE)
  }

  # Apply overrides
  overrides <- list(...)
  if (length(overrides) > 0) {
    theme_obj <- theme_obj$merge(overrides)
  }

  # Update network (modify list directly)
  network$theme <- theme_obj

  network
}

#' Apply Color Palette to Network
#'
#' Apply a color palette for node and/or edge coloring.
#'
#' @param network A cograph_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param palette Palette name or function.
#' @param target What to apply the palette to: "nodes", "edges", or "both".
#' @param by Variable to map colors to (for nodes: column name or "group").
#'
#' @details
#' ## Available Palettes
#' Use \code{list_palettes()} to see all available palettes. Common options:
#' \describe{
#'   \item{\strong{viridis}}{Perceptually uniform, colorblind-friendly.}
#'   \item{\strong{colorblind}}{Optimized for color vision deficiency.}
#'   \item{\strong{pastel}}{Soft, muted colors.}
#'   \item{\strong{bright}}{Saturated, vivid colors.}
#'   \item{\strong{grayscale}}{Shades of gray.}
#' }
#'
#' You can also pass a custom palette function that takes \code{n} and returns
#' \code{n} colors.
#'
#' @return Modified cograph_network object.
#'
#' @seealso
#' \code{\link{cograph}} for network creation,
#' \code{\link{sn_theme}} for visual themes,
#' \code{\link{sn_nodes}} for node customization,
#' \code{\link{list_palettes}} to see available palettes,
#' \code{\link{splot}} and \code{\link{soplot}} for plotting
#'
#' @export
#'
#' @examples
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#'
#' # Apply palette to nodes
#' cograph(adj) |> sn_palette("viridis") |> splot()
#'
#' # Apply to edges
#' cograph(adj) |> sn_palette("colorblind", target = "edges") |> splot()
#'
#' # Apply to both
#' cograph(adj) |> sn_palette("pastel", target = "both") |> splot()
#'
#' # Custom palette function
#' my_pal <- function(n) rainbow(n, s = 0.7)
#' cograph(adj) |> sn_palette(my_pal) |> splot()
#'
#' # Direct matrix input
#' adj |> sn_palette("viridis")
sn_palette <- function(network, palette, target = "nodes", by = NULL) {
  # Auto-convert matrix/data.frame/igraph to cograph_network
  network <- ensure_cograph_network(network)

  # Get palette function
  if (is.character(palette)) {
    pal_fn <- get_palette(palette)
    if (is.null(pal_fn)) {
      stop("Unknown palette: ", palette, ". Available: ",
           paste(list_palettes(), collapse = ", "), call. = FALSE)
    }
  } else if (is.function(palette)) {
    pal_fn <- palette
  } else {
    stop("palette must be a string or function", call. = FALSE)
  }

  # Apply to nodes
  if (target %in% c("nodes", "both")) {
    n <- n_nodes(network)
    nodes_df <- get_nodes(network)

    if (!is.null(by) && by %in% names(nodes_df)) {
      # Map by variable
      colors <- scale_color_discrete(nodes_df[[by]], pal_fn)
    } else {
      # Default: all same color (first from palette)
      colors <- rep(pal_fn(1), n)
    }

    # Update node_aes
    if (is.null(network$node_aes)) {
      network$node_aes <- list()
    }
    network$node_aes$fill <- colors
  }

  # Apply to edges
  if (target %in% c("edges", "both")) {
    edges_df <- get_edges(network)
    if (!is.null(edges_df) && nrow(edges_df) > 0) {
      # Use first two colors for positive/negative
      edge_colors <- pal_fn(2)

      # Update edge_aes
      if (is.null(network$edge_aes)) {
        network$edge_aes <- list()
      }
      network$edge_aes$positive_color <- edge_colors[1]
      network$edge_aes$negative_color <- edge_colors[2]
    }
  }

  network
}
