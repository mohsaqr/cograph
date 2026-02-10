#' Calculate Network Centrality Measures
#'
#' Computes centrality measures for nodes in a network and returns a tidy
#' data frame. Accepts matrices, igraph objects, cograph_network, or tna objects.
#'
#' @param x Network input (matrix, igraph, network, cograph_network, tna object)
#' @param measures Which measures to calculate. Default "all" calculates all
#'   available measures. Can be a character vector of measure names:
#'   "degree", "strength", "betweenness", "closeness", "eigenvector",
#'   "pagerank", "authority", "hub", "eccentricity", "coreness",
#'   "constraint", "transitivity".
#' @param mode For directed networks: "all", "in", or "out". Affects degree,
#'   strength, closeness, eccentricity, and coreness.
#' @param normalized Logical. Normalize values to 0-1 range by dividing by max.
#' @param weighted Logical. Use edge weights if available. Default TRUE.
#' @param directed Logical or NULL. If NULL (default), auto-detect from matrix
#'   symmetry. Set TRUE to force directed, FALSE to force undirected.
#' @param loops Logical. If TRUE (default), keep self-loops. Set to FALSE to
#'   remove them before calculation.
#' @param simplify How to combine multiple edges between the same node pair.
#'   Options: "sum" (default), "mean", "max", "min", or FALSE/"none" to keep
#'   multiple edges.
#' @param digits Integer or NULL. Round all numeric columns to this many
#'   decimal places. Default NULL (no rounding).
#' @param sort_by Character or NULL. Column name to sort results by
#'   (descending order). Default NULL (original node order).
#' @param ... Additional arguments (currently unused)
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{node}: Node labels/names
#'     \item One column per measure, with mode suffix for directional measures
#'       (e.g., \code{degree_in}, \code{closeness_all})
#'   }
#'
#' @details
#' The following centrality measures are available:
#' \describe{
#'   \item{degree}{Count of edges (supports mode: in/out/all)}
#'   \item{strength}{Weighted degree (supports mode: in/out/all)}
#'   \item{betweenness}{Shortest path centrality}
#'   \item{closeness}{Inverse distance centrality (supports mode: in/out/all)}
#'   \item{eigenvector}{Influence-based centrality}
#'   \item{pagerank}{Random walk centrality}
#'   \item{authority}{HITS authority score}
#'   \item{hub}{HITS hub score}
#'   \item{eccentricity}{Maximum distance to other nodes (supports mode)}
#'   \item{coreness}{K-core membership (supports mode: in/out/all)}
#'   \item{constraint}{Burt's constraint (structural holes)}
#'   \item{transitivity}{Local clustering coefficient}
#' }
#'
#' @export
#' @examples
#' # Basic usage with matrix
#' adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' rownames(adj) <- colnames(adj) <- c("A", "B", "C")
#' centrality(adj)
#'
#' # Specific measures
#' centrality(adj, measures = c("degree", "betweenness"))
#'
#' # Directed network with normalization
#' centrality(adj, mode = "in", normalized = TRUE)
#'
#' # Sort by pagerank
#' centrality(adj, sort_by = "pagerank", digits = 3)
centrality <- function(x, measures = "all", mode = "all",
                       normalized = FALSE, weighted = TRUE,
                       directed = NULL, loops = TRUE, simplify = "sum",
                       digits = NULL, sort_by = NULL, ...) {

  # Validate mode

  mode <- match.arg(mode, c("all", "in", "out"))

  # Convert input to igraph (pass directed for override)
  g <- to_igraph(x, directed = directed)

 # Handle loops (remove if loops = FALSE)
  if (!loops) {
    g <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
  }

  # Handle multiple edges
  if (!isFALSE(simplify) && !identical(simplify, "none")) {
    simplify <- match.arg(simplify, c("sum", "mean", "max", "min"))
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE,
                          edge.attr.comb = list(weight = simplify, "ignore"))
  }

  # Define which measures support mode parameter
  mode_measures <- c("degree", "strength", "closeness", "eccentricity", "coreness")
  no_mode_measures <- c("betweenness", "eigenvector", "pagerank",
                        "authority", "hub", "constraint", "transitivity")
  all_measures <- c(mode_measures, no_mode_measures)

  # Resolve measures
  if (identical(measures, "all")) {
    measures <- all_measures
  } else {
    invalid <- setdiff(measures, all_measures)
    if (length(invalid) > 0) {
      stop("Unknown measures: ", paste(invalid, collapse = ", "),
           "\nAvailable: ", paste(all_measures, collapse = ", "), call. = FALSE)
    }
  }

  # Get node labels
  labels <- if (!is.null(igraph::V(g)$name)) {
    igraph::V(g)$name
  } else {
    as.character(seq_len(igraph::vcount(g)))
  }

  # Calculate each measure
  results <- list(node = labels)
  weights <- if (weighted && !is.null(igraph::E(g)$weight)) {
    igraph::E(g)$weight
  } else {
    NULL
  }

  for (m in measures) {
    # Calculate value
    value <- calculate_measure(g, m, mode, weights)

    # Normalize if requested
    if (normalized) {
      max_val <- max(value, na.rm = TRUE)
      if (!is.na(max_val) && max_val > 0) {
        value <- value / max_val
      }
    }

    # Column name with mode suffix for directional measures
    col_name <- if (m %in% mode_measures) paste0(m, "_", mode) else m
    results[[col_name]] <- value
  }

  df <- as.data.frame(results, stringsAsFactors = FALSE)

  # Round if digits specified
  if (!is.null(digits)) {
    num_cols <- sapply(df, is.numeric)
    df[num_cols] <- lapply(df[num_cols], round, digits = digits)
  }

  # Sort if sort_by specified
  if (!is.null(sort_by)) {
    if (!sort_by %in% names(df)) {
      stop("sort_by column '", sort_by, "' not found in results", call. = FALSE)
    }
    df <- df[order(df[[sort_by]], decreasing = TRUE), ]
    rownames(df) <- NULL
  }

  df
}

#' Calculate a single centrality measure
#' @noRd
calculate_measure <- function(g, measure, mode, weights) {
  directed <- igraph::is_directed(g)

  value <- switch(measure,
    # Measures that support mode
    "degree" = igraph::degree(g, mode = mode),
    "strength" = igraph::strength(g, mode = mode, weights = weights),
    "closeness" = igraph::closeness(
      g, mode = mode, weights = weights, normalized = FALSE
    ),
    "eccentricity" = igraph::eccentricity(g, mode = mode),
    "coreness" = igraph::coreness(g, mode = mode),

    # Measures without mode
    "betweenness" = igraph::betweenness(
      g, weights = weights, directed = directed
    ),
    "eigenvector" = igraph::eigen_centrality(
      g, weights = weights, directed = directed
    )$vector,
    "pagerank" = igraph::page_rank(
      g, weights = weights, directed = directed
    )$vector,
    "authority" = igraph::hits_scores(g, weights = weights)$authority,
    "hub" = igraph::hits_scores(g, weights = weights)$hub,
    "constraint" = igraph::constraint(g, weights = weights),
    "transitivity" = igraph::transitivity(g, type = "local"),

    stop("Unknown measure: ", measure, call. = FALSE)
  )

  # Remove names to ensure consistent output
  unname(value)
}

#' @rdname centrality
#' @export
centrality_degree <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "degree", mode = mode, ...)
  col <- paste0("degree_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_strength <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "strength", mode = mode, ...)
  col <- paste0("strength_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_betweenness <- function(x, ...) {
  df <- centrality(x, measures = "betweenness", ...)
  stats::setNames(df$betweenness, df$node)
}

#' @rdname centrality
#' @export
centrality_closeness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "closeness", mode = mode, ...)
  col <- paste0("closeness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_eigenvector <- function(x, ...) {
  df <- centrality(x, measures = "eigenvector", ...)
  stats::setNames(df$eigenvector, df$node)
}

#' @rdname centrality
#' @export
centrality_pagerank <- function(x, ...) {
  df <- centrality(x, measures = "pagerank", ...)
  stats::setNames(df$pagerank, df$node)
}

#' @rdname centrality
#' @export
centrality_authority <- function(x, ...) {
  df <- centrality(x, measures = "authority", ...)
  stats::setNames(df$authority, df$node)
}

#' @rdname centrality
#' @export
centrality_hub <- function(x, ...) {
  df <- centrality(x, measures = "hub", ...)
  stats::setNames(df$hub, df$node)
}

#' @rdname centrality
#' @export
centrality_eccentricity <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "eccentricity", mode = mode, ...)
  col <- paste0("eccentricity_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_coreness <- function(x, mode = "all", ...) {
  df <- centrality(x, measures = "coreness", mode = mode, ...)
  col <- paste0("coreness_", mode)
  stats::setNames(df[[col]], df$node)
}

#' @rdname centrality
#' @export
centrality_constraint <- function(x, ...) {
  df <- centrality(x, measures = "constraint", ...)
  stats::setNames(df$constraint, df$node)
}

#' @rdname centrality
#' @export
centrality_transitivity <- function(x, ...) {
  df <- centrality(x, measures = "transitivity", ...)
  stats::setNames(df$transitivity, df$node)
}
