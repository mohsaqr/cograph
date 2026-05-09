#' @title Plot Mixed Network
#' @description Plot a network combining symmetric (undirected) and asymmetric
#'   (directed) matrices with appropriate edge styling.
#' @name plot_mixed_network
NULL

#' Plot Mixed Network from Two Matrices
#'
#' Creates a network visualization combining edges from a symmetric matrix
#' (rendered as straight undirected edges) and an asymmetric matrix
#' (rendered as curved directed edges).
#'
#' @param sym_matrix A symmetric matrix representing undirected relationships.
#'   These edges will be drawn straight without arrows.
#' @param asym_matrix An asymmetric matrix representing directed relationships.
#'   These edges will be drawn curved with arrows. Reciprocal edges curve in
#'   opposite directions.
#' @param layout Layout algorithm or coordinate matrix. Default "oval".
#' @param sym_color Color for symmetric/undirected edges. Default
#'   \code{"ivory4"}.
#' @param asym_color Color for asymmetric/directed edges. Can be a single color
#'   or a vector of two colors for positive/negative directions. Default
#'   "#003355" (dark blue, matching TNA style).
#' @param curvature Curvature magnitude for directed edges. Default 0.3.
#' @param edge_width Edge width(s). If NULL (default), scales automatically by
#'   edge weight like TNA plots. Pass a numeric value to override.
#' @param node_size Node size. Default 7.
#' @param title Plot title. Default NULL.
#' @param threshold Minimum absolute edge weight to display. Values with
#'   \code{abs(value) < threshold} are set to zero (edge removed). Default 0.
#'   Zero-weight edges are always removed regardless of this setting.
#' @param edge_labels Show edge weight labels. Default TRUE.
#' @param arrow_size Arrow head size for directed edges. Default 0.61 (TNA style).
#' @param edge_label_size Size of edge labels. Default 0.6.
#' @param edge_label_position Position of edge labels along edge (0-1). Default 0.7.
#' @param initial Optional named numeric vector of initial state probabilities
#'   (length = number of nodes). When provided, nodes are drawn as donuts with
#'   the fill proportion equal to the initial probability. Default NULL.
#' @param ... Additional arguments passed to splot().
#'
#' @return Invisibly returns a list with the combined edge data and filtered
#'   symmetric/asymmetric matrices.
#'
#' @examples
#' # Create symmetric matrix (undirected)
#' sym <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' sym[1,2] <- sym[2,1] <- 0.5
#' sym[3,4] <- sym[4,3] <- 0.6
#'
#' # Create asymmetric matrix (directed)
#' asym <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' asym[1,3] <- 0.7
#' asym[3,1] <- 0.3
#' asym[2,4] <- 0.8
#' asym[4,2] <- 0.4
#'
#' # Plot combined network
#' plot_mixed_network(sym, asym, title = "Mixed Network")
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
