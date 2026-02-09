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
#' @param layout Layout algorithm or coordinate matrix. Default "circle".
#' @param sym_color Color for symmetric/undirected edges. Default "gray40".
#' @param asym_color Color for asymmetric/directed edges. Can be a single color
#'   or a vector of two colors for positive/negative directions. Default
#'   c("steelblue", "coral").
#' @param curvature Curvature magnitude for directed edges. Default 0.3.
#' @param edge_width Edge width. Default 2.5.
#' @param node_size Node size. Default 8.
#' @param title Plot title. Default NULL.
#' @param edge_labels Show edge weight labels. Default TRUE.
#' @param ... Additional arguments passed to splot().
#'
#' @return Invisibly returns the combined cograph_network object.
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
    layout = "circle",
    sym_color = "gray40",
    asym_color = c("steelblue", "coral"),
    curvature = 0.3,
    edge_width = 2.5,
    node_size = 8,
    title = NULL,
    edge_labels = TRUE,
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

  # Get node names
  node_names <- rownames(sym_matrix)
  if (is.null(node_names)) {
    node_names <- rownames(asym_matrix)
  }
  if (is.null(node_names)) {
    node_names <- as.character(seq_len(n))
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

  # Determine edge label style
 edge_label_style <- if (edge_labels) "weight" else "none"

  # Plot
  splot(
    edge_df,
    directed = TRUE,
    layout = layout,
    curvature = curvature_vec,
    show_arrows = arrows_vec,
    edge_color = color_vec,
    edge_width = edge_width,
    node_size = node_size,
    title = title,
    edge_label_style = edge_label_style,
    node_names = node_names,
    ...
  )

  # Return combined network invisibly
  invisible(list(
    edges = edges,
    sym_matrix = sym_matrix,
    asym_matrix = asym_matrix
  ))
}
