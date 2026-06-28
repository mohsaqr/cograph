# =============================================================================
# Ego-Network Analysis
# =============================================================================


#' Ego-Network Metrics
#'
#' Extracts the ego network of each requested node (the node, its neighbours up
#' to a given order, and the ties among them) and reports a tidy table of
#' personal-network metrics: size, internal tie counts and densities, and Burt's
#' structural-hole measures. One row per ego.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param nodes Character vector of node names or integer vector of node
#'   indices selecting which egos to report. NULL (default) uses every node.
#' @param order Integer neighbourhood order defining the ego network. 1
#'   (default) is the standard ego network (ego + direct neighbours). Burt's
#'   \code{effective_size} and \code{constraint} are only defined for
#'   \code{order = 1} and are returned as \code{NA} otherwise.
#' @param mode For directed networks, which ties define the neighbourhood:
#'   \code{"all"} (default), \code{"out"}, or \code{"in"}.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}.
#'
#' @return A tidy data.frame of class \code{"cograph_ego_networks"} with one row
#'   per ego and columns:
#'   \describe{
#'     \item{node}{Ego node name.}
#'     \item{size}{Number of alters (ego-network size, excluding ego).}
#'     \item{ego_ties}{Number of edges in the ego network (ego + alters).}
#'     \item{ego_density}{Edge density of the ego network including ego.}
#'     \item{alter_ties}{Number of edges among the alters only (excluding ego).}
#'     \item{alter_density}{Edge density among the alters. Low values indicate
#'       many structural holes / brokerage opportunities.}
#'     \item{effective_size}{Burt's effective size of the ego network
#'       (\code{order = 1} only).}
#'     \item{constraint}{Burt's constraint (\code{order = 1} only).}
#'   }
#'
#' @details
#' \code{effective_size} and \code{constraint} are computed on the full network
#' (Burt's measures are defined directly from each node's order-1 ego network),
#' reusing the same implementations as \code{\link{centrality}} so results match
#' \code{centrality(x, measures = c("effective_size", "constraint"))}.
#'
#' @references
#' Burt, R.S. (1992). \emph{Structural Holes: The Social Structure of
#' Competition}. Harvard University Press.
#'
#' @seealso \code{\link{centrality}} (for \code{effective_size}, \code{constraint},
#'   \code{dispersion}), \code{\link{select_neighbors}}, \code{\link{neighborhood_overlap}}
#'
#' @export
#' @examples
#' adj <- matrix(c(
#'   0, 1, 1, 0, 0,
#'   1, 0, 1, 0, 0,
#'   1, 1, 0, 1, 1,
#'   0, 0, 1, 0, 1,
#'   0, 0, 1, 1, 0
#' ), 5, 5, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:5]
#' cograph::ego_networks(adj)
ego_networks <- function(x,
                         nodes = NULL,
                         order = 1,
                         mode = c("all", "out", "in"),
                         directed = NULL,
                         ...) {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for ego_networks()", call. = FALSE)
  }
  stopifnot(length(order) == 1L, order >= 1, order == as.integer(order))
  mode <- match.arg(mode)

  g <- to_igraph(x, directed = directed, ...)
  n <- igraph::vcount(g)
  vnames <- igraph::V(g)$name
  if (is.null(vnames)) vnames <- as.character(seq_len(n))

  # Resolve requested egos to vertex indices
  idx <- if (is.null(nodes)) {
    seq_len(n)
  } else if (is.numeric(nodes)) {
    as.integer(nodes)
  } else {
    match(as.character(nodes), vnames)
  }
  if (anyNA(idx) || any(idx < 1L) || any(idx > n)) {
    stop("Unknown node(s) requested in `nodes`.", call. = FALSE)
  }

  # Neighbourhoods, vectorised over all requested egos (no explicit loop)
  egos <- igraph::ego(g, order = order, nodes = idx, mode = mode)

  per_ego <- lapply(seq_along(idx), function(j) {
    v <- idx[j]
    members <- as.integer(egos[[j]])
    alters <- setdiff(members, v)
    size <- length(alters)
    # Drop self-loops: a self-tie is not a relationship between two distinct
    # actors, and igraph::edge_density() excludes loops from its denominator,
    # so counting them in ecount() would inflate densities above 1 (common
    # with TNA / self-transition matrices that carry a non-zero diagonal).
    sub_ego <- igraph::simplify(igraph::induced_subgraph(g, members),
                                remove.multiple = FALSE, remove.loops = TRUE)
    sub_alt <- igraph::simplify(igraph::induced_subgraph(g, alters),
                                remove.multiple = FALSE, remove.loops = TRUE)
    data.frame(
      node          = vnames[v],
      size          = size,
      ego_ties      = igraph::ecount(sub_ego),
      ego_density   = if (length(members) > 1L) igraph::edge_density(sub_ego) else NA_real_,
      alter_ties    = igraph::ecount(sub_alt),
      alter_density = if (size > 1L) igraph::edge_density(sub_alt) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, per_ego)

  # Burt structural-hole measures (order-1 only), aligned by vertex index so
  # there is no name-matching ambiguity for unnamed graphs.
  if (order == 1L) {
    es <- calculate_effective_size(g)
    con <- igraph::constraint(g, weights = NULL)
    out$effective_size <- unname(es)[idx]
    out$constraint <- unname(con)[idx]
  } else {
    out$effective_size <- NA_real_
    out$constraint <- NA_real_
  }

  rownames(out) <- NULL
  attr(out, "order") <- as.integer(order)
  attr(out, "mode") <- mode
  attr(out, "directed") <- igraph::is_directed(g)
  class(out) <- c("cograph_ego_networks", "data.frame")
  out
}


#' @export
print.cograph_ego_networks <- function(x, ...) {
  cat(sprintf("Ego Networks (order = %d, mode = %s)\n",
              attr(x, "order"), attr(x, "mode")))
  cat(strrep("=", 50), "\n")
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}
