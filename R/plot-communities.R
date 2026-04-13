#' Overlay Community Blobs on a Network Plot
#'
#' Render a network with \code{\link{splot}} and overlay smooth blob
#' shapes highlighting node communities.
#'
#' @param x A network object passed to \code{\link{splot}}: \code{tna},
#'   matrix, \code{igraph}, or \code{cograph_network}.
#' @param communities Community assignments in any format:
#'   a method name (e.g., \code{"walktrap"}, \code{"louvain"}),
#'   a numeric membership vector (e.g., \code{c(1, 1, 2, 2, 3)}),
#'   a named list of character vectors,
#'   a \code{cograph_communities} object, or
#'   a \code{tna_communities} object.
#' @param blob_colors Character vector of fill colors for blobs.
#'   Recycled if shorter than the number of communities.
#' @param blob_alpha Numeric. Fill transparency (0-1).
#' @param blob_linewidth Numeric. Border line width.
#' @param blob_line_alpha Numeric. Border line transparency (0-1).
#' @param ... Additional arguments passed to \code{\link{splot}}.
#'
#' @return The \code{splot} result (invisibly).
#'
#' @examples
#' \dontrun{
#' mat <- matrix(runif(25), 5, 5,
#'               dimnames = list(LETTERS[1:5], LETTERS[1:5]))
#' diag(mat) <- 0
#' overlay_communities(mat, list(g1 = c("A", "B"), g2 = c("C", "D", "E")))
#' }
#'
#' if (requireNamespace("tna", quietly = TRUE)) {
#'   model <- tna::tna(tna::group_regulation)
#'
#'   # With a named list
#'   overlay_communities(model, list(
#'     Regulatory = c("plan", "monitor", "adapt"),
#'     Social     = c("cohesion", "emotion", "consensus"),
#'     Task       = c("discuss", "synthesis", "coregulate")
#'   ))
#'
#'   # With a cograph_communities object (infomap supports directed graphs)
#'   comm <- cograph::communities(model$weights, method = "infomap")
#'   overlay_communities(model, comm)
#' }
#'
#' @export
overlay_communities <- function(x,
                                communities,
                                blob_colors = NULL,
                                blob_alpha = 0.25,
                                blob_linewidth = 0.7,
                                blob_line_alpha = 0.8,
                                ...) {

  # Method name string → run igraph community detection
  if (is.character(communities) && length(communities) == 1L) {
    input <- gsub("[() ]+", "_", trimws(tolower(communities)))
    input <- sub("^cluster_", "", input)
    input <- sub("_$", "", input)
    igraph_methods <- c("edge_betweenness", "fast_greedy",
                        "fluid_communities", "infomap", "label_prop",
                        "leading_eigen", "leiden", "louvain",
                        "optimal", "spinglass", "walktrap")
    # Partial match, then try collapsed (walk_trap → walktrap)
    matched <- pmatch(input, igraph_methods)
    if (is.na(matched)) {
      matched <- pmatch(gsub("_", "", input), gsub("_", "", igraph_methods))
    }
    if (is.na(matched)) {
      stop("Unknown community method '", communities, "'. Valid: ",
           paste(igraph_methods, collapse = ", "), call. = FALSE)
    }
    fn <- utils::getFromNamespace(
      paste0("cluster_", igraph_methods[matched]), "igraph"
    )
    g <- to_igraph(x)
    if (igraph::is_directed(g)) {
      g <- igraph::as_undirected(g, mode = "collapse",
                                  edge.attr.comb = "mean")
    }
    comm_obj <- fn(g)
    mem <- igraph::membership(comm_obj)
    communities <- split(names(mem), mem)
    names(communities) <- paste0("Community ", names(communities))
  }

  # Convert community objects/vectors to named list
  if (inherits(communities, "tna_communities")) {
    asgn <- communities$assignments
    method_col <- setdiff(names(asgn), "state")
    communities <- split(asgn$state, asgn[[method_col[1L]]])
    names(communities) <- paste0("Community ", names(communities))
  } else if (inherits(communities, c("cograph_communities", "communities"))) {
    mem <- membership(communities)
    comm_list <- split(names(mem), mem)
    names(comm_list) <- paste0("Community ", names(comm_list))
    communities <- comm_list
  } else if (is.numeric(communities) || is.factor(communities)) {
    # Membership vector: c(1,1,2,2,3) or factor
    node_names <- names(communities)
    if (is.null(node_names)) {
      node_names <- .extract_blob_states(x)
      if (is.null(node_names)) { # nocov
        node_names <- as.character(seq_along(communities)) # nocov
      } # nocov
    }
    communities <- split(node_names, communities)
    names(communities) <- paste0("Community ", names(communities))
  } else if (!is.list(communities)) {
    stop("'communities' must be a named list, numeric/factor vector, ",
         "cograph_communities, or tna_communities object.", call. = FALSE)
  }
  stopifnot(is.list(communities), length(communities) >= 1L)

  # Render network
  result <- splot(x, ...)

  # Node positions: splot returns 0-1, plot coords are (v - 0.5) * 1.8
  nodes <- result$nodes
  px <- setNames((nodes$x - 0.5) * 1.8, nodes$label)
  py <- setNames((nodes$y - 0.5) * 1.8, nodes$label)

  # Blob styling
  n_comm <- length(communities)
  blob_colors <- rep_len(blob_colors %||% .blob_default_colors(), n_comm)

  # Draw blobs
  for (k in seq_len(n_comm)) {
    cx <- unname(px[communities[[k]]])
    cy <- unname(py[communities[[k]]])
    blob <- .smooth_blob(cx, cy, pad = 0.25)
    border <- .darken_colors(blob_colors[k], 0.20)
    graphics::polygon(
      blob$x, blob$y,
      col = grDevices::adjustcolor(blob_colors[k], alpha.f = blob_alpha),
      border = grDevices::adjustcolor(border, alpha.f = blob_line_alpha),
      lwd = blob_linewidth
    )
  }

  invisible(result)
}


#' Plot a tna_communities object
#'
#' Plots the original tna model with nodes colored by community membership.
#' The original model is retrieved from \code{attr(x, "tna")}, which
#' \code{tna::communities()} sets automatically. Uses \code{walktrap} if
#' present in \code{x$assignments}; otherwise falls back to the first
#' available algorithm column.
#'
#' @param x A \code{tna_communities} object from \code{tna::communities()}.
#' @param ... Additional arguments forwarded to \code{\link{splot}}.
#'
#' @return Invisibly, the splot result.
#' @rdname splot
#' @export
splot.tna_communities <- function(x, ...) {
  model <- attr(x, "tna")
  algos <- setdiff(colnames(x$assignments), "state")
  if (length(algos) == 0L) {
    stop("splot.tna_communities: no algorithm columns in x$assignments",
         call. = FALSE)
  }
  algo <- if ("walktrap" %in% algos) "walktrap" else algos[[1L]]
  splot(model, node_fill = x$assignments[[algo]], ...)
}


#' Plot a cograph_communities object
#'
#' Plots the original network with nodes colored by community membership.
#' The network is retrieved from \code{attr(x, "network")}, which
#' \code{detect_communities()} / \code{.wrap_communities()} sets automatically.
#'
#' @param x A \code{cograph_communities} object from
#'   \code{detect_communities()} or one of the
#'   \code{community_<algorithm>()} helpers.
#' @param ... Additional arguments forwarded to \code{\link{splot}}.
#'
#' @return Invisibly, the splot result.
#' @rdname splot
#' @export
splot.cograph_communities <- function(x, ...) {
  network <- attr(x, "network")
  splot(network, node_fill = x$community, ...)
}
