# =============================================================================
# Edge-Level Metric Convenience Wrappers
#
# These delegate to edge_centrality() in centrality.R, following the same
# pattern as centrality_degree() delegates to centrality().
# =============================================================================


#' Neighborhood Overlap (Jaccard) for Each Edge
#'
#' Convenience wrapper around \code{\link{edge_centrality}} that returns only
#' the overlap measure sorted by overlap descending.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param top Integer or NULL. Return only the top N edges. Default NULL.
#' @param directed Logical or NULL. Default NULL (auto-detect).
#' @param digits Integer or NULL. Round numeric columns. Default NULL.
#' @param ... Additional arguments passed to \code{\link{edge_centrality}}.
#'
#' @return A data frame sorted by \code{overlap} (descending) with columns:
#'   \code{from}, \code{to}, \code{weight}, \code{overlap},
#'   \code{shared_neighbors}.
#'
#' @seealso \code{\link{edge_centrality}}, \code{\link{simmelian_strength}}
#' @export
#' @examples
#' cograph::neighborhood_overlap(regulation_net, top = 5)
neighborhood_overlap <- function(x, top = NULL, directed = NULL,
                                 digits = NULL, ...) {
  df <- edge_centrality(x, measures = c("weight", "overlap"),
                        directed = directed, digits = digits,
                        sort_by = "overlap", ...)
  if (!is.null(top)) df <- utils::head(df, top)
  df
}


#' Simmelian Strength (Triangle Count per Edge)
#'
#' Convenience wrapper around \code{\link{edge_centrality}} that returns only
#' the triangle count per edge, sorted descending.
#'
#' @inheritParams neighborhood_overlap
#'
#' @return A data frame sorted by \code{triangles} (descending) with columns:
#'   \code{from}, \code{to}, \code{weight}, \code{triangles}.
#'
#' @seealso \code{\link{edge_centrality}}, \code{\link{neighborhood_overlap}}
#' @export
#' @examples
#' cograph::simmelian_strength(regulation_net, top = 5)
simmelian_strength <- function(x, top = NULL, directed = NULL,
                               digits = NULL, ...) {
  df <- edge_centrality(x, measures = c("weight", "simmelian"),
                        directed = directed, digits = digits,
                        sort_by = "triangles", ...)
  if (!is.null(top)) df <- utils::head(df, top)
  df
}


#' Edge Reciprocity
#'
#' Convenience wrapper around \code{\link{edge_centrality}} that returns only
#' reciprocity information for directed networks.
#'
#' @inheritParams neighborhood_overlap
#'
#' @return A data frame with one row per directed edge and columns
#'   \code{from}, \code{to}, \code{weight}, \code{reciprocated} (logical),
#'   \code{reverse_weight} (NA when not reciprocated) and \code{weight_ratio}
#'   (\code{reverse_weight / weight}; NA when not reciprocated). Rows are
#'   ordered with reciprocated edges first, then by \code{|weight_ratio|}
#'   descending.
#'
#' @section Errors:
#' Raises an error when the resolved network is undirected: reciprocity is
#' only defined for directed edges.
#'
#' @seealso \code{\link{edge_centrality}}
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' cograph::edge_reciprocity(regulation_net, top = 5)
edge_reciprocity <- function(x, top = NULL, directed = NULL,
                             digits = NULL, ...) {
  # Ensure directed — reciprocity only makes sense for directed networks
  g <- to_igraph(x, directed = directed)
  if (!igraph::is_directed(g)) {
    stop("edge_reciprocity() requires a directed network. ",
         "For undirected networks, all edges are inherently reciprocal.",
         call. = FALSE)
  }
  df <- edge_centrality(x, measures = c("weight", "reciprocity"),
                        directed = directed, digits = digits, ...)
  # Sort: reciprocated first, then by weight_ratio descending
  ord <- order(!df$reciprocated, -abs(ifelse(is.na(df$weight_ratio), 0,
                                             df$weight_ratio)))
  df <- df[ord, ]
  rownames(df) <- NULL
  if (!is.null(top)) df <- utils::head(df, top)
  df
}
