#' @title Plot Mixed Network
#' @name plot_mixed_network
NULL

#' Plot Mixed Network from Two Matrices
#'
#' Plots one network that combines the edges of a symmetric matrix, shown as
#' straight undirected edges, with the edges of an asymmetric matrix, shown as
#' curved directed edges.
#'
#' @param sym_matrix A symmetric matrix of undirected relationships. Each
#'   non-zero pair is plotted once as a straight edge without arrows.
#' @param asym_matrix An asymmetric matrix of directed relationships, with
#'   the same dimensions as \code{sym_matrix}. Its edges are plotted as curved
#'   arrows.
#' @param layout Layout algorithm or coordinate matrix. Default "oval".
#' @param sym_color Color for undirected edges. Default \code{"ivory4"}.
#' @param asym_color Color for directed edges. Either a single color, or two
#'   colors for reciprocal pairs: the first for the edge from the
#'   lower-indexed node and the second for the reverse edge. Non-reciprocal
#'   edges use the first color. Default \code{"#003355"} (dark blue, the TNA
#'   edge color).
#' @param curvature Curvature magnitude for directed edges. Default 0.3.
#' @param edge_width Edge width(s). If NULL (default), widths scale with edge
#'   weight as in TNA plots. A numeric value overrides the scaling.
#' @param node_size Node size. Default 7.
#' @param title Plot title. Default NULL.
#' @param threshold Minimum absolute edge weight to display. Values with
#'   \code{abs(value) < threshold} are set to zero, and zero-weight edges are
#'   not plotted. Default 0.
#' @param edge_labels Show edge weight labels. Default TRUE.
#' @param arrow_size Arrow head size for directed edges. Default 0.61 (TNA style).
#' @param edge_label_size Size of edge labels. Default 0.6.
#' @param edge_label_position Position of edge labels along edge (0-1). Default 0.7.
#' @param initial Optional numeric vector of initial state probabilities.
#'   A named vector is matched to the node names, with missing states set to
#'   0; an unnamed vector is used in node order. Nodes are then plotted as
#'   donuts filled in proportion to the initial probability. A warning is
#'   issued when the values do not sum to 1 (tolerance 0.01). Default NULL.
#' @param ... Additional arguments passed to \code{\link{splot}}.
#'
#' @return Invisibly, a list with three elements: \code{edges}, a data frame
#'   with one row per plotted edge and columns \code{from}, \code{to}
#'   (node indices), \code{weight}, \code{type} ("undirected" or
#'   "directed") and \code{color}; and \code{sym_matrix} and
#'   \code{asym_matrix}, the input matrices after thresholding.
#'
#' @examples
#' plot_mixed_network(symmetrize(regulation_net, keep_format = TRUE), regulation_net)
#'
#' @export
plot_mixed_network <- function(
    sym_matrix,
    asym_matrix,
    layout = "oval",
    sym_color = "ivory4",
    asym_color = COGRAPH_SCALE$tna_edge_color,
    curvature = 0.3,
    edge_width = NULL,
    node_size = 7,
    title = NULL,
    threshold = 0,
    edge_labels = TRUE,
    arrow_size = 0.61,
    edge_label_size = 0.6,
    edge_label_position = 0.7,
    initial = NULL,
    ...
) {
  # Validate inputs
  if (!is.matrix(sym_matrix) || !is.matrix(asym_matrix)) {
    stop("Both sym_matrix and asym_matrix must be matrices")
  }

  if (!all(dim(sym_matrix) == dim(asym_matrix))) {
    stop("sym_matrix and asym_matrix must have the same dimensions")
  }

  n <- nrow(sym_matrix)

  # Remove zero edges and apply threshold
  effective_threshold <- max(threshold, .Machine$double.eps)
  sym_matrix[abs(sym_matrix) < effective_threshold] <- 0
  asym_matrix[abs(asym_matrix) < effective_threshold] <- 0

  # Get node names from matrix dimnames
  node_names <- rownames(asym_matrix)
  if (is.null(node_names)) node_names <- rownames(sym_matrix)
  if (is.null(node_names)) node_names <- as.character(seq_len(n))

  # Validate and align initial state probabilities
  donut_vals <- NULL
  if (!is.null(initial)) {
    if (!is.numeric(initial))
      stop("initial must be a named numeric vector of state probabilities", call. = FALSE)
    if (!is.null(names(initial))) {
      # Align to node order; missing states get 0
      aligned <- setNames(numeric(n), node_names)
      common  <- intersect(names(initial), node_names)
      aligned[common] <- initial[common]
      initial <- aligned
    }
    if (abs(sum(initial) - 1) > 0.01)
      warning("initial probabilities do not sum to 1 (sum = ", round(sum(initial), 4), ")",
              call. = FALSE)
    donut_vals <- as.numeric(initial)
  }

  # Build edge list from both matrices
  edges_list <- list()
  edge_idx <- 0

  # Track which symmetric edges we've added (to avoid duplicates)
  sym_added <- matrix(FALSE, n, n)

  # Process symmetric matrix (undirected edges)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j && sym_matrix[i, j] != 0 && !sym_added[i, j]) {
        edge_idx <- edge_idx + 1
        edges_list[[edge_idx]] <- data.frame(
          from = i,
          to = j,
          weight = sym_matrix[i, j],
          type = "undirected",
          color = sym_color,
          stringsAsFactors = FALSE
        )
        sym_added[i, j] <- TRUE
        sym_added[j, i] <- TRUE
      }
    }
  }

  # Process asymmetric matrix (directed edges)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j && asym_matrix[i, j] != 0) {
        edge_idx <- edge_idx + 1
        # Determine if reciprocal exists
        is_recip <- asym_matrix[j, i] != 0
        # Use different colors for reciprocal pairs
        if (length(asym_color) == 2 && is_recip) {
          col <- if (i < j) asym_color[1] else asym_color[2]
        } else {
          col <- asym_color[1]
        }
        edges_list[[edge_idx]] <- data.frame(
          from = i,
          to = j,
          weight = asym_matrix[i, j],
          type = "directed",
          color = col,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(edges_list) == 0) {
    stop("No edges found in either matrix")
  }

  # Combine edges
  edges <- do.call(rbind, edges_list)
  n_edges <- nrow(edges)

  # Build aesthetic vectors
  curvature_vec <- ifelse(edges$type == "directed", curvature, 0)
  arrows_vec <- edges$type == "directed"
  color_vec <- edges$color

  # Create edge data frame for splot
  edge_df <- data.frame(
    from = edges$from,
    to = edges$to,
    weight = edges$weight
  )

  # Build splot call — only include donut args when initial probs are present
  splot_args <- c(
    list(
      edge_df,
      directed           = TRUE,
      layout             = layout,
      curvature          = curvature_vec,
      show_arrows        = arrows_vec,
      edge_color         = color_vec,
      edge_width         = edge_width,
      node_size          = node_size,
      title              = title,
      edge_labels        = edge_labels,
      edge_label_size    = edge_label_size,
      edge_label_position = edge_label_position,
      arrow_size         = arrow_size,
      edge_start_style   = "dotted",
      edge_start_length  = 0.2,
      labels             = node_names
    ),
    if (!is.null(donut_vals)) list(donut_fill = donut_vals, donut_empty = FALSE),
    list(...)
  )
  do.call(splot, splot_args)

  # Return combined network invisibly
  invisible(list(
    edges = edges,
    sym_matrix = sym_matrix,
    asym_matrix = asym_matrix
  ))
}
