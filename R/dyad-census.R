# =============================================================================
# Dyad Census
# =============================================================================


#' Dyad Census
#'
#' Classifies every dyad (unordered pair of nodes) in a directed network into
#' one of three mutually exclusive states: \strong{mutual} (M, edges in both
#' directions), \strong{asymmetric} (A, an edge in exactly one direction), or
#' \strong{null} (N, no edge between the pair). The dyad census is the
#' dyad-level companion to \code{\link{triad_census}} and underlies dyad-based
#' reciprocity.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}.
#'
#' @return A tidy data.frame of class \code{"cograph_dyad_census"} with one row
#'   per dyad type and columns:
#'   \describe{
#'     \item{type}{Character: \code{"mutual"}, \code{"asymmetric"}, or
#'       \code{"null"}.}
#'     \item{count}{Integer: number of dyads of that type.}
#'     \item{proportion}{Numeric: count divided by the total number of dyads
#'       (\eqn{n(n-1)/2}).}
#'   }
#'   The dyad-based reciprocity \eqn{2M / (2M + A)} is attached as the
#'   \code{"reciprocity"} attribute.
#'
#' @details
#' For \emph{undirected} networks every present edge is counted as a mutual
#' dyad and the asymmetric count is always zero, so the census reduces to a
#' present/absent split. The total number of dyads is \eqn{n(n-1)/2} regardless
#' of direction.
#'
#' @references
#' Wasserman, S., & Faust, K. (1994). \emph{Social Network Analysis: Methods
#' and Applications}. Cambridge University Press.
#'
#' @seealso \code{\link{triad_census}}, \code{\link{edge_reciprocity}},
#'   \code{\link{network_summary}}
#'
#' @export
#' @examples
#' # Directed network with a mix of mutual and asymmetric ties
#' adj <- matrix(c(
#'   0, 1, 1, 0,
#'   1, 0, 0, 1,
#'   0, 0, 0, 1,
#'   0, 0, 0, 0
#' ), 4, 4, byrow = TRUE)
#' rownames(adj) <- colnames(adj) <- LETTERS[1:4]
#' cograph::dyad_census(adj)
dyad_census <- function(x, directed = NULL, ...) {

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for dyad_census()", call. = FALSE)
  }

  g <- to_igraph(x, directed = directed, ...)
  is_dir <- igraph::is_directed(g)

  # igraph::dyad_census warns on undirected input; the result (all present
  # edges counted as mutual) is exactly what we want, so suppress the warning.
  dc <- if (is_dir) {
    igraph::dyad_census(g)
  } else {
    suppressWarnings(igraph::dyad_census(g))
  }

  counts <- c(
    mutual     = as.integer(dc$mut),
    asymmetric = as.integer(dc$asym),
    null       = as.integer(dc$null)
  )
  total <- sum(counts)

  recip <- if ((2 * counts[["mutual"]] + counts[["asymmetric"]]) == 0) {
    NA_real_
  } else {
    2 * counts[["mutual"]] / (2 * counts[["mutual"]] + counts[["asymmetric"]])
  }

  result <- data.frame(
    type       = names(counts),
    count      = unname(counts),
    proportion = if (total == 0) rep(NA_real_, 3L) else unname(counts) / total,
    stringsAsFactors = FALSE
  )
  attr(result, "reciprocity") <- recip
  attr(result, "directed") <- is_dir
  attr(result, "n_dyads") <- total
  class(result) <- c("cograph_dyad_census", "data.frame")
  result
}


#' @export
print.cograph_dyad_census <- function(x, ...) {
  cat("Dyad Census\n")
  cat(strrep("=", 35), "\n")
  print.data.frame(x, row.names = FALSE)
  recip <- attr(x, "reciprocity")
  cat("\n  Dyads:", attr(x, "n_dyads"),
      "  Directed:", attr(x, "directed"), "\n")
  cat("  Reciprocity (2M / (2M + A)):",
      if (is.na(recip)) "undefined" else round(recip, 4), "\n")
  invisible(x)
}
