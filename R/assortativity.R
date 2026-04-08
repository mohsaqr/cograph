# =============================================================================
# Assortativity & Homophily Analysis
# =============================================================================


#' Degree Assortativity Coefficient
#'
#' Computes the degree assortativity coefficient, measuring the tendency of
#' nodes to connect to other nodes with similar degree. Positive values
#' indicate assortative mixing (high-degree nodes connect to high-degree nodes),
#' negative values indicate disassortative mixing.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param type Character string specifying which degree correlation to compute.
#'   One of \code{"out-in"} (default for directed), \code{"in-in"},
#'   \code{"out-out"}, \code{"in-out"}, or \code{"degree"} (for undirected).
#'   Ignored for undirected networks.
#' @param digits Integer or NULL. Round result to this many decimal places.
#'   Default NULL (no rounding).
#' @param ... Additional arguments passed to \code{\link{to_igraph}}.
#'
#' @return An object of class \code{"cograph_assortativity"} with components:
#'   \describe{
#'     \item{coefficient}{Numeric scalar: the assortativity coefficient in
#'       \eqn{[-1, 1]}.}
#'     \item{type}{Character: the degree type used.}
#'     \item{directed}{Logical: whether the network was treated as directed.}
#'     \item{n_nodes}{Integer: number of nodes.}
#'     \item{n_edges}{Integer: number of edges.}
#'     \item{network}{The original input network.}
#'   }
#'
#' @details
#' The degree assortativity coefficient is defined as the Pearson correlation
#' coefficient between the degrees of nodes at either end of each edge
#' (Newman 2002):
#'
#' \deqn{r = \frac{\sum_{jk} jk(e_{jk} - q_j q_k)}{\sigma_q^2}}
#'
#' where \eqn{e_{jk}} is the fraction of edges connecting degree-\eqn{j} to
#' degree-\eqn{k} vertices, \eqn{q_k} is the excess degree distribution, and
#' \eqn{\sigma_q^2} its variance.
#'
#' For directed networks, different degree combinations (in/out) at source and
#' target ends can be specified via the \code{type} parameter.
#'
#' @references
#' Newman, M.E.J. (2002). Assortative mixing in networks.
#' \emph{Physical Review Letters}, 89(20), 208701.
#' \doi{10.1103/PhysRevLett.89.208701}
#'
#' @seealso \code{\link{assortativity_attribute}}, \code{\link{centrality}},
#'   \code{\link{network_summary}}
#'
#' @export
#' @examples
#' # Assortative network (high-degree connect to high-degree)
#' adj <- matrix(c(
#'   0, 1, 1, 1, 0,
#'   1, 0, 1, 1, 0,
#'   1, 1, 0, 0, 1,
#'   1, 1, 0, 0, 1,
#'   0, 0, 1, 1, 0
#' ), 5, 5)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' cograph::assortativity(adj)
assortativity <- function(x,
                          directed = NULL,
                          type = NULL,
                          digits = NULL,
                          ...) {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for assortativity()", call. = FALSE)
  }

  g <- to_igraph(x, directed = directed, ...)
  is_dir <- igraph::is_directed(g)
  n <- igraph::vcount(g)
  m <- igraph::ecount(g)

  # Determine degree type
  if (is.null(type)) {
    type <- if (is_dir) "out-in" else "degree"
  }
  valid_types <- if (is_dir) {
    c("out-in", "in-in", "out-out", "in-out")
  } else {
    "degree"
  }
  if (!type %in% valid_types) {
    stop("Invalid type '", type, "'. For ",
         if (is_dir) "directed" else "undirected",
         " networks, use: ", paste(valid_types, collapse = ", "),
         call. = FALSE)
  }

  # Compute assortativity
  if (m == 0) {
    coef_val <- NA_real_
  } else if (!is_dir) {
    coef_val <- .degree_assortativity_undirected(g)
  } else {
    coef_val <- .degree_assortativity_directed(g, type)
  }

  if (!is.null(digits) && !is.na(coef_val)) {
    coef_val <- round(coef_val, digits)
  }

  result <- list(
    coefficient = coef_val,
    type = type,
    directed = is_dir,
    n_nodes = n,
    n_edges = m,
    network = x
  )
  class(result) <- "cograph_assortativity"
  result
}


#' Attribute Assortativity (Homophily)
#'
#' Computes assortativity with respect to a node attribute, measuring the
#' tendency of nodes to connect to others with similar attribute values.
#' For categorical attributes, this computes the modularity-based nominal
#' assortativity. For numeric attributes, this computes the Pearson
#' correlation between attribute values at edge endpoints.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param values Named vector of attribute values (names must match node names)
#'   or an unnamed vector in node order.
#' @param directed Logical or NULL. If NULL (default), auto-detect.
#' @param digits Integer or NULL. Round result. Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}.
#'
#' @return An object of class \code{"cograph_assortativity"} with components:
#'   \describe{
#'     \item{coefficient}{Numeric scalar: assortativity coefficient.}
#'     \item{type}{Character: \code{"nominal"} or \code{"scalar"}.}
#'     \item{directed}{Logical.}
#'     \item{n_nodes}{Integer.}
#'     \item{n_edges}{Integer.}
#'     \item{attribute_values}{The attribute values used.}
#'     \item{network}{Original input.}
#'   }
#'
#' @details
#' For categorical (nominal) attributes, the coefficient is:
#' \deqn{r = \frac{\text{tr}(\mathbf{e}) - \|\mathbf{e}^2\|}{1 - \|\mathbf{e}^2\|}}
#' where \eqn{\mathbf{e}} is the mixing matrix with \eqn{e_{ij}} = fraction of
#' edges connecting type \eqn{i} to type \eqn{j}.
#'
#' For numeric (scalar) attributes, the coefficient is the Pearson correlation
#' between attribute values at edge endpoints.
#'
#' @references
#' Newman, M.E.J. (2003). Mixing patterns in networks.
#' \emph{Physical Review E}, 67(2), 026126.
#' \doi{10.1103/PhysRevE.67.026126}
#'
#' @seealso \code{\link{assortativity}}, \code{\link{detect_communities}}
#'
#' @export
#' @examples
#' adj <- matrix(c(0,1,1,0, 1,0,0,0, 1,0,0,1, 0,0,1,0), 4, 4)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
#' groups <- c(A = "x", B = "x", C = "y", D = "y")
#' cograph::assortativity_attribute(adj, groups)
assortativity_attribute <- function(x,
                                    values,
                                    directed = NULL,
                                    digits = NULL,
                                    ...) {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for assortativity_attribute()",
         call. = FALSE)
  }

  g <- to_igraph(x, directed = directed, ...)
  is_dir <- igraph::is_directed(g)
  n <- igraph::vcount(g)
  m <- igraph::ecount(g)

  # Align attribute values with nodes
  node_names <- igraph::V(g)$name
  if (is.null(node_names)) node_names <- as.character(seq_len(n))

  if (!is.null(names(values))) {
    # Named vector: align by name
    missing <- setdiff(node_names, names(values))
    if (length(missing) > 0) {
      stop("Attribute values missing for nodes: ",
           paste(utils::head(missing, 5), collapse = ", "),
           if (length(missing) > 5) "...",
           call. = FALSE)
    }
    values <- values[node_names]
  } else {
    stopifnot(length(values) == n)
    names(values) <- node_names
  }

  # Determine if categorical or numeric
  is_numeric_attr <- is.numeric(values)
  attr_type <- if (is_numeric_attr) "scalar" else "nominal"

  if (m == 0) {
    coef_val <- NA_real_
  } else if (is_numeric_attr) {
    coef_val <- .scalar_assortativity(g, as.numeric(values))
  } else {
    coef_val <- .nominal_assortativity(g, as.character(values))
  }

  if (!is.null(digits) && !is.na(coef_val)) {
    coef_val <- round(coef_val, digits)
  }

  result <- list(
    coefficient = coef_val,
    type = attr_type,
    directed = is_dir,
    n_nodes = n,
    n_edges = m,
    attribute_values = values,
    network = x
  )
  class(result) <- "cograph_assortativity"
  result
}


#' @rdname assortativity_attribute
#' @export
homophily <- assortativity_attribute


# =============================================================================
# Internal Helpers
# =============================================================================

#' Degree assortativity for undirected networks
#' @param g igraph object
#' @return Numeric scalar
#' @keywords internal
#' @noRd
.degree_assortativity_undirected <- function(g) {
  el <- igraph::as_edgelist(g, names = FALSE)
  deg <- igraph::degree(g, mode = "all")
  x_vals <- deg[el[, 1]]
  y_vals <- deg[el[, 2]]
  # For undirected, each edge contributes in both directions
  all_x <- c(x_vals, y_vals)
  all_y <- c(y_vals, x_vals)
  sd_x <- stats::sd(all_x)
  sd_y <- stats::sd(all_y)
  if (is.na(sd_x) || is.na(sd_y) || sd_x == 0 || sd_y == 0) return(NA_real_)
  stats::cor(all_x, all_y)
}


#' Degree assortativity for directed networks
#' @param g igraph object
#' @param type One of "out-in", "in-in", "out-out", "in-out"
#' @return Numeric scalar
#' @keywords internal
#' @noRd
.degree_assortativity_directed <- function(g, type) {
  el <- igraph::as_edgelist(g, names = FALSE)
  parts <- strsplit(type, "-")[[1]]
  source_mode <- parts[1]
  target_mode <- parts[2]

  source_deg <- igraph::degree(g, mode = source_mode)
  target_deg <- igraph::degree(g, mode = target_mode)

  x_vals <- source_deg[el[, 1]]
  y_vals <- target_deg[el[, 2]]

  sd_x <- stats::sd(x_vals)
  sd_y <- stats::sd(y_vals)
  if (is.na(sd_x) || is.na(sd_y) || sd_x == 0 || sd_y == 0) return(NA_real_)
  stats::cor(x_vals, y_vals)
}


#' Scalar (numeric) attribute assortativity
#' @param g igraph object
#' @param values Numeric vector of attribute values in node order
#' @return Numeric scalar
#' @keywords internal
#' @noRd
.scalar_assortativity <- function(g, values) {
  el <- igraph::as_edgelist(g, names = FALSE)
  is_dir <- igraph::is_directed(g)

  x_vals <- values[el[, 1]]
  y_vals <- values[el[, 2]]

  if (!is_dir) {
    # Symmetrize for undirected
    all_x <- c(x_vals, y_vals)
    all_y <- c(y_vals, x_vals)
    x_vals <- all_x
    y_vals <- all_y
  }

  sd_x <- stats::sd(x_vals)
  sd_y <- stats::sd(y_vals)
  if (is.na(sd_x) || is.na(sd_y) || sd_x == 0 || sd_y == 0) return(NA_real_)
  stats::cor(x_vals, y_vals)
}


#' Nominal (categorical) attribute assortativity
#'
#' Computes Newman's nominal assortativity from the mixing matrix.
#' @param g igraph object
#' @param values Character vector of categories in node order
#' @return Numeric scalar
#' @keywords internal
#' @noRd
.nominal_assortativity <- function(g, values) {
  el <- igraph::as_edgelist(g, names = FALSE)
  is_dir <- igraph::is_directed(g)

  src_cat <- values[el[, 1]]
  tgt_cat <- values[el[, 2]]

  categories <- sort(unique(values))
  k <- length(categories)

  if (k <= 1) return(NA_real_)

  # Build mixing matrix e (fraction of edges between each pair of categories)
  cat_idx <- match(values, categories)
  src_idx <- cat_idx[el[, 1]]
  tgt_idx <- cat_idx[el[, 2]]

  e_mat <- matrix(0, k, k)
  m <- nrow(el)

  # Tabulate edges
  vapply(seq_len(m), function(i) {
    e_mat[src_idx[i], tgt_idx[i]] <<- e_mat[src_idx[i], tgt_idx[i]] + 1
    0L
  }, integer(1))

  if (!is_dir) {
    # Symmetrize: each edge counted once, so add transpose and halve
    e_mat <- (e_mat + t(e_mat)) / 2
  }

  # Normalize to fractions
  e_mat <- e_mat / sum(e_mat)

  # r = (tr(e) - ||e^2||) / (1 - ||e^2||)
  trace_e <- sum(diag(e_mat))
  e_sq <- e_mat %*% e_mat
  norm_e_sq <- sum(e_sq)

  if (abs(1 - norm_e_sq) < 1e-12) return(NA_real_)

  (trace_e - norm_e_sq) / (1 - norm_e_sq)
}


# =============================================================================
# Print Method
# =============================================================================

#' @method print cograph_assortativity
#' @export
#' @noRd
print.cograph_assortativity <- function(x, ...) {
  type_label <- switch(x$type,
    "degree" = "Degree",
    "out-in" = "Degree (out-in)",
    "in-in" = "Degree (in-in)",
    "out-out" = "Degree (out-out)",
    "in-out" = "Degree (in-out)",
    "nominal" = "Nominal Attribute",
    "scalar" = "Scalar Attribute",
    x$type
  )

  cat(sprintf("Assortativity (%s)\n", type_label))
  cat(strrep("=", 35), "\n")
  cat("  Coefficient:", round(x$coefficient, 4), "\n")

  interpretation <- if (is.na(x$coefficient)) {
    "undefined"
  } else if (x$coefficient > 0.1) {
    "assortative"
  } else if (x$coefficient < -0.1) {
    "disassortative"
  } else {
    "neutral"
  }

  cat("  Interpretation:", interpretation, "\n")
  cat("  Nodes:", x$n_nodes, "  Edges:", x$n_edges, "\n")
  cat("  Directed:", x$directed, "\n")
  invisible(x)
}
