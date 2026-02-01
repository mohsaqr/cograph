#' @title Main Entry Point
#' @description The primary function for creating network visualizations.
#' @name sonnet-main
NULL

#' Auto-convert input to sonnet_network
#'
#' Internal helper that converts matrices, data frames, igraph, network,
#' qgraph, or tna objects to sonnet_network objects automatically.
#' Works with both the old R6-based format and the new lightweight format.
#'
#' @param x Input object (matrix, data.frame, igraph, network, qgraph, tna, or sonnet_network).
#' @param layout Default layout to use if converting.
#' @param seed Random seed for deterministic layouts.
#' @param ... Additional arguments passed to sonnet().
#' @return A sonnet_network object.
#' @noRd
ensure_sonnet_network <- function(x, layout = "spring", seed = 42, ...) {

  if (inherits(x, "sonnet_network")) {
    # Check if this is a new lightweight format without layout
    # Use getter function to get nodes
    nodes <- get_nodes(x)
    if (!is.null(nodes) && (!"x" %in% names(nodes) || all(is.na(nodes$x)))) {
      # Need to compute layout for the new format
      x <- compute_layout_for_sonnet(x, layout = layout, seed = seed, ...)
    }
    return(x)
  }

  if (is.matrix(x) || is.data.frame(x) || inherits(x, "igraph") ||
      inherits(x, "network") || inherits(x, "qgraph") || inherits(x, "tna")) {
    return(sonnet(x, layout = layout, seed = seed, ...))
  }

  stop("Input must be a matrix, data.frame, igraph, network, qgraph, tna, or sonnet_network",
       call. = FALSE)
}

#' Compute layout for lightweight sonnet_network
#'
#' Computes layout coordinates for a sonnet_network object that doesn't have them.
#'
#' @param net A sonnet_network object (new lightweight format).
#' @param layout Layout algorithm name.
#' @param seed Random seed for deterministic layouts.
#' @param ... Additional arguments passed to the layout function.
#' @return The sonnet_network with layout coordinates added.
#' @noRd
compute_layout_for_sonnet <- function(net, layout = "spring", seed = 42, ...) {
  # Get nodes data frame using getter function
  nodes <- get_nodes(net)
  n <- nrow(nodes)

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Two-letter igraph layout codes
  igraph_codes <- c("kk", "fr", "drl", "mds", "go", "tr", "st", "gr", "rd", "ni", "ci", "lgl", "sp")

  # Build edges for layout computation using getter function
  edges <- get_edges(net)

  # Get directed status
  net_directed <- is_directed(net)

  # Compute layout
  if (is.function(layout)) {
    # Need to create a temporary R6 network for igraph layout
    temp_net <- SonnetNetwork$new()
    temp_net$set_nodes(nodes)
    temp_net$set_edges(edges)
    temp_net$set_directed(net_directed)
    coords <- apply_igraph_layout(temp_net, layout, ...)
  } else if (is.character(layout) && (
    grepl("^(igraph_|layout_)", layout) || layout %in% igraph_codes
  )) {
    # igraph layout by name
    temp_net <- SonnetNetwork$new()
    temp_net$set_nodes(nodes)
    temp_net$set_edges(edges)
    temp_net$set_directed(net_directed)
    coords <- apply_igraph_layout_by_name(temp_net, layout, seed = seed, ...)
  } else if (is.matrix(layout) || is.data.frame(layout)) {
    # Custom coordinates
    coords <- as.data.frame(layout)
    if (ncol(coords) >= 2) {
      names(coords)[1:2] <- c("x", "y")
    }
  } else {
    # Built-in Sonnet layout - create temporary network
    temp_net <- SonnetNetwork$new()
    temp_net$set_nodes(nodes)
    temp_net$set_edges(edges)
    temp_net$set_directed(net_directed)
    layout_obj <- SonnetLayout$new(layout, ...)
    coords <- layout_obj$compute(temp_net, ...)
  }

  # Update nodes with layout coordinates
  nodes$x <- coords$x
  nodes$y <- coords$y

  # Update using setter function or direct assignment for new format
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
#' The main entry point for Sonnet. Accepts adjacency matrices, edge lists,
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
#' @return A sonnet_network object that can be further customized and rendered.
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
#' sonnet(adj)
#'
#' # From edge list
#' edges <- data.frame(from = c(1, 1, 2), to = c(2, 3, 3))
#' sonnet(edges)
#'
#' # With customization (pipe-friendly workflow)
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
#' sonnet(adj, layout = "circle") |>
#'   sn_nodes(fill = "steelblue") |>
#'   sn_edges(color = "gray50") |>
#'   splot()
#'
#' # Weighted network with automatic styling
#' w_adj <- matrix(c(0, 0.5, -0.3, 0.5, 0, 0.4, -0.3, 0.4, 0), nrow = 3)
#' sonnet(w_adj) |>
#'   sn_edges(color = "weight", width = "weight") |>
#'   splot()
#'
#' # With igraph (if installed)
#' \dontrun{
#' library(igraph)
#' g <- make_ring(10)
#' sonnet(g) |> splot()
#' }
sonnet <- function(input, layout = "spring", directed = NULL,
                   node_labels = NULL, seed = 42, ...) {

  # Create network object
  network <- SonnetNetwork$new(
    input = input,
    directed = directed,
    node_labels = node_labels
  )

  # Apply default theme
  network$set_theme(get_theme("classic"))

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
    # Built-in Sonnet layout
    layout_obj <- SonnetLayout$new(layout, ...)
    coords <- layout_obj$compute(network, ...)
  }
  network$set_layout_coords(coords)

  # Store layout info
  network$set_layout_info(list(
    name = if (is.function(layout)) "custom_function" else as.character(layout),
    seed = seed
  ))

  # Wrap in S3 class for method dispatch
  as_sonnet_network(network)
}

#' Apply Layout to Network
#'
#' Apply a layout algorithm to compute node positions.
#'
#' @param network A sonnet_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param layout Layout algorithm name or a SonnetLayout object.
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
#' @return Modified sonnet_network object.
#'
#' @seealso
#' \code{\link{sonnet}} for network creation,
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
#' sonnet(adj) |> sn_layout("circle") |> splot()
#' sonnet(adj) |> sn_layout("spring") |> splot()
#'
#' # igraph layouts (if igraph installed)
#' \dontrun{
#' sonnet(adj) |> sn_layout("kk") |> splot()
#' sonnet(adj) |> sn_layout("fr") |> splot()
#' }
#'
#' # Custom coordinates
#' coords <- matrix(c(0, 0, 1, 0, 0.5, 1), ncol = 2, byrow = TRUE)
#' sonnet(adj) |> sn_layout(coords) |> splot()
#'
#' # Direct matrix input (auto-converts)
#' adj |> sn_layout("circle")
sn_layout <- function(network, layout, seed = 42, ...) {
  # Auto-convert matrix/data.frame/igraph to sonnet_network
  network <- ensure_sonnet_network(network, layout = layout, seed = seed, ...)

  new_net <- network$network$clone_network()

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Two-letter igraph layout codes
  igraph_codes <- c("kk", "fr", "drl", "mds", "go", "tr", "st", "gr", "rd", "ni", "ci", "lgl", "sp")

  # Handle igraph layout functions
  if (is.function(layout)) {
    # Assume it's an igraph layout function
    coords <- apply_igraph_layout(new_net, layout, ...)
    new_net$set_layout_coords(coords)
    new_net$set_layout_info(list(name = "custom_function", seed = seed, coords = coords))
    return(as_sonnet_network(new_net))
  }

  # Create layout object if string
  if (is.character(layout)) {
    # Check if it's an igraph layout name or two-letter code
    if (grepl("^(igraph_|layout_)", layout) || layout %in% igraph_codes) {
      coords <- apply_igraph_layout_by_name(new_net, layout, seed = seed, ...)
      new_net$set_layout_coords(coords)
      new_net$set_layout_info(list(name = layout, seed = seed, coords = coords))
      return(as_sonnet_network(new_net))
    }
    layout_obj <- SonnetLayout$new(layout, ...)
  } else if (inherits(layout, "SonnetLayout")) {
    layout_obj <- layout
  } else if (is.matrix(layout) || is.data.frame(layout)) {
    # Custom coordinates passed directly
    coords <- as.data.frame(layout)
    if (ncol(coords) >= 2) {
      names(coords)[1:2] <- c("x", "y")
    }
    new_net$set_layout_coords(coords)
    new_net$set_layout_info(list(name = "custom", seed = seed, coords = coords))
    return(as_sonnet_network(new_net))
  } else {
    stop("layout must be a string, SonnetLayout object, igraph layout function, or coordinate matrix",
         call. = FALSE)
  }

  # Compute and apply coordinates
  coords <- layout_obj$compute(new_net, ...)
  new_net$set_layout_coords(coords)
  new_net$set_layout_info(list(name = layout, seed = seed, coords = coords))

  as_sonnet_network(new_net)
}

#' Apply Theme to Network
#'
#' Apply a visual theme to the network.
#'
#' @param network A sonnet_network object, matrix, data.frame, or igraph object.
#'   Matrices and other inputs are auto-converted.
#' @param theme Theme name (string) or SonnetTheme object.
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
#' @return Modified sonnet_network object.
#'
#' @seealso
#' \code{\link{sonnet}} for network creation,
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
#' sonnet(adj) |> sn_theme("dark") |> splot()
#' sonnet(adj) |> sn_theme("minimal") |> splot()
#'
#' # Override specific theme properties
#' sonnet(adj) |> sn_theme("classic", background = "lightgray") |> splot()
#'
#' # Direct matrix input
#' adj |> sn_theme("dark")
sn_theme <- function(network, theme, ...) {
  # Auto-convert matrix/data.frame/igraph to sonnet_network
  network <- ensure_sonnet_network(network)

  new_net <- network$network$clone_network()

  # Get theme object
  if (is.character(theme)) {
    theme_obj <- get_theme(theme)
    if (is.null(theme_obj)) {
      stop("Unknown theme: ", theme, ". Available: ",
           paste(list_themes(), collapse = ", "), call. = FALSE)
    }
  } else if (inherits(theme, "SonnetTheme")) {
    theme_obj <- theme
  } else {
    stop("theme must be a string or SonnetTheme object", call. = FALSE)
  }

  # Apply overrides
  overrides <- list(...)
  if (length(overrides) > 0) {
    theme_obj <- theme_obj$merge(overrides)
  }

  new_net$set_theme(theme_obj)

  as_sonnet_network(new_net)
}

#' Apply Color Palette to Network
#'
#' Apply a color palette for node and/or edge coloring.
#'
#' @param network A sonnet_network object, matrix, data.frame, or igraph object.
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
#' @return Modified sonnet_network object.
#'
#' @seealso
#' \code{\link{sonnet}} for network creation,
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
#' sonnet(adj) |> sn_palette("viridis") |> splot()
#'
#' # Apply to edges
#' sonnet(adj) |> sn_palette("colorblind", target = "edges") |> splot()
#'
#' # Apply to both
#' sonnet(adj) |> sn_palette("pastel", target = "both") |> splot()
#'
#' # Custom palette function
#' my_pal <- function(n) rainbow(n, s = 0.7)
#' sonnet(adj) |> sn_palette(my_pal) |> splot()
#'
#' # Direct matrix input
#' adj |> sn_palette("viridis")
sn_palette <- function(network, palette, target = "nodes", by = NULL) {
  # Auto-convert matrix/data.frame/igraph to sonnet_network
  network <- ensure_sonnet_network(network)

  new_net <- network$network$clone_network()

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
    n <- new_net$n_nodes
    nodes_df <- new_net$get_nodes()

    if (!is.null(by) && by %in% names(nodes_df)) {
      # Map by variable
      colors <- scale_color_discrete(nodes_df[[by]], pal_fn)
    } else {
      # Default: all same color (first from palette)
      colors <- rep(pal_fn(1), n)
    }

    new_net$set_node_aes(list(fill = colors))
  }

  # Apply to edges
  if (target %in% c("edges", "both")) {
    edges_df <- new_net$get_edges()
    if (!is.null(edges_df) && nrow(edges_df) > 0) {
      # Use first two colors for positive/negative
      edge_colors <- pal_fn(2)
      new_net$set_edge_aes(list(
        positive_color = edge_colors[1],
        negative_color = edge_colors[2]
      ))
    }
  }

  as_sonnet_network(new_net)
}
