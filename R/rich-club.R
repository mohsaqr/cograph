# =============================================================================
# Rich Club Analysis
#
# Global weighted rich club curve (Opsahl et al. 2008) and per-node local
# rich club scores. Supports degree- and strength-based prominence with
# null model normalization.
# =============================================================================


#' Rich Club Coefficient
#'
#' Computes the rich club curve across all prominence thresholds. The
#' unweighted coefficient (Colizza et al. 2006) measures the density of ties
#' among prominent nodes. The weighted coefficient (Opsahl et al. 2008)
#' measures whether prominent nodes concentrate the strongest ties of the
#' network among themselves. A directed network is converted to an undirected
#' one before the computation, with the weights of reciprocal edges summed,
#' and self-loops are removed.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param rich Character. Prominence definition: \code{"k"} (degree, default)
#'   or \code{"s"} (strength, the weighted degree).
#' @param weighted Logical. If TRUE (default), compute the weighted rich club
#'   coefficient. If FALSE, compute the unweighted version (density among rich
#'   nodes).
#' @param normalized Logical. If TRUE (default), normalize against
#'   degree-preserving random graphs and report the null distribution. The
#'   null graphs are generated with \code{igraph::sample_degseq()}, which fixes
#'   the degree sequence. For a weighted rich club the observed edge weights
#'   are also reshuffled across the null edges, following Opsahl et al.
#'   (2008).
#' @param n_random Integer. Number of random graphs for normalization. Default
#'   100.
#' @param directed Logical or NULL. Default NULL (auto-detect).
#' @param seed Integer or NULL. Random seed for the null graphs. When a seed
#'   is given, the caller's random state is restored on exit. Default NULL.
#' @param digits Integer or NULL. Round all numeric columns, including
#'   \code{threshold}. Default NULL.
#' @param ... Passed to \code{\link{to_igraph}}, which takes no further
#'   arguments, so any argument supplied here raises an error.
#'
#' @return A data frame with class \code{"cograph_rich_club"}, one row per
#'   prominence threshold at which at least two nodes are rich, and columns:
#'   \describe{
#'     \item{threshold}{The prominence cut-off. Nodes with prominence strictly
#'       greater than this value form the club. Thresholds range over the
#'       observed prominence values excluding the maximum.}
#'     \item{n_rich}{Number of club members at that threshold.}
#'     \item{phi}{Observed rich club coefficient.}
#'     \item{phi_norm, phi_rand, ci_lo, ci_hi}{Present only when
#'       \code{normalized = TRUE}. They hold the observed coefficient divided by
#'       the null mean, the null mean itself, and the 2.5\% and 97.5\%
#'       quantiles of the null distribution.}
#'   }
#'   The data frame has zero rows for graphs that are too small, complete, or
#'   regular for any threshold to yield a club. The arguments \code{rich},
#'   \code{weighted}, \code{normalized} and the original input
#'   (\code{"network"}) are stored as attributes.
#'
#' @details
#' The unweighted coefficient is
#' \eqn{\phi(k) = 2 E_{>k} / (N_{>k} (N_{>k} - 1))}{phi(k) = 2 E_{>k} / (N_{>k} (N_{>k} - 1))}, where \eqn{N_{>k}} is
#' the number of nodes with prominence above \eqn{k} and \eqn{E_{>k}} the
#' number of edges among them.
#'
#' The weighted coefficient is
#' \eqn{\phi^w(k) = W_{>k} / \sum_{l=1}^{E_{>k}} w_l^{ranked}}{phi^w(k) = W_{>k} / sum_{l=1}^{E_{>k}} w_l^{ranked}}, where
#' \eqn{W_{>k}} is the total weight of the edges among the rich nodes and
#' \eqn{w_l^{ranked}} is the \eqn{l}-th largest edge weight in the network.
#'
#' The normalized coefficient is
#' \eqn{\phi_{norm} = \phi_{obs} / \bar{\phi}_{rand}}{phi_{norm} = phi_{obs} / bar{phi}_{rand}}. A value above 1
#' indicates rich club ordering beyond what the degree sequence alone
#' explains.
#'
#' @references
#' Opsahl, T., Colizza, V., Panzarasa, P. & Ramasco, J.J. (2008).
#' Prominence and control: The weighted rich-club effect. \emph{Physical
#' Review Letters}, 101, 168702.
#'
#' Colizza, V., Flammini, A., Serrano, M.A. & Vespignani, A. (2006).
#' Detecting rich-club ordering in complex networks. \emph{Nature Physics},
#' 2, 110-115.
#'
#' @seealso \code{\link{rich_club_local}}, \code{\link{robustness}},
#'   \code{\link{centrality}}
#'
#' @section Printing and plotting:
#' Printing the result shows the prominence definition, the weighting and
#' normalization settings, the number of thresholds with \code{phi_norm > 1}
#' and the first ten rows of the table. \code{plot()} on the result is
#' documented in \code{\link{plot-results}}.
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' rich_club(regulation_net, n_random = 20, seed = 1)
rich_club <- function(x,
                      rich = c("k", "s"),
                      weighted = TRUE,
                      normalized = TRUE,
                      n_random = 100,
                      directed = NULL,
                      seed = NULL,
                      digits = NULL,
                      ...) {

  rich <- match.arg(rich)
  stopifnot(
    is.logical(weighted), length(weighted) == 1L,
    is.logical(normalized), length(normalized) == 1L,
    is.numeric(n_random), length(n_random) == 1L, n_random >= 1L
  )
  n_random <- as.integer(n_random)

  g <- to_igraph(x, directed = directed, ...)

  if (igraph::is_directed(g)) {
    g <- igraph::as_undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum", "ignore"))
  }
  g <- igraph::simplify(g)

  prominence <- if (rich == "k") igraph::degree(g) else igraph::strength(g)

  has_weights <- !is.null(igraph::E(g)$weight)
  edge_wts <- if (has_weights) igraph::E(g)$weight else rep(1, igraph::ecount(g))
  ranked_wts <- sort(edge_wts, decreasing = TRUE)

  thresholds <- sort(unique(prominence))
  thresholds <- thresholds[thresholds < max(prominence)]

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }

  # Pre-generate random graphs ONCE (not per threshold)
  rand_data <- NULL
  if (normalized) {
    deg_seq <- igraph::degree(g)
    rand_data <- lapply(seq_len(n_random), function(i) {
      g_rand <- tryCatch(
        igraph::sample_degseq(deg_seq, method = "fast.heur.simple"),
        error = function(e) NULL)
      if (is.null(g_rand)) return(NULL)

      rand_wts <- if (has_weights && weighted) sample(edge_wts) else
        rep(1, igraph::ecount(g_rand))
      if (has_weights && weighted) igraph::E(g_rand)$weight <- rand_wts

      list(g = g_rand,
           wts = rand_wts,
           ranked = sort(rand_wts, decreasing = TRUE),
           prom = if (rich == "k") igraph::degree(g_rand) else
             igraph::strength(g_rand))
    })
    rand_data <- Filter(Negate(is.null), rand_data)
  }

  curve_rows <- lapply(thresholds, function(thr) {
    rich_idx <- which(prominence > thr)
    n_rich <- length(rich_idx)
    if (n_rich < 2) return(NULL)

    phi <- .compute_phi(g, rich_idx, edge_wts, ranked_wts, weighted)

    row <- data.frame(threshold = thr, n_rich = n_rich, phi = phi,
                      stringsAsFactors = FALSE)

    if (normalized && length(rand_data) > 0) {
      phi_rand_vals <- vapply(rand_data, function(rd) {
        rich_rand <- which(rd$prom > thr)
        if (length(rich_rand) < 2) return(NA_real_)
        .compute_phi(rd$g, rich_rand, rd$wts, rd$ranked, weighted)
      }, numeric(1))

      phi_rand_vals <- phi_rand_vals[!is.na(phi_rand_vals)]
      if (length(phi_rand_vals) > 0) {
        phi_rand_mean <- mean(phi_rand_vals)
        row$phi_norm <- if (phi_rand_mean > 0) phi / phi_rand_mean else NA_real_
        row$phi_rand <- phi_rand_mean
        row$ci_lo <- stats::quantile(phi_rand_vals, 0.025, names = FALSE)
        row$ci_hi <- stats::quantile(phi_rand_vals, 0.975, names = FALSE)
      } else {
        row$phi_norm <- NA_real_
        row$phi_rand <- NA_real_
        row$ci_lo <- NA_real_
        row$ci_hi <- NA_real_
      }
    } else if (normalized) {
      row$phi_norm <- NA_real_
      row$phi_rand <- NA_real_
      row$ci_lo <- NA_real_
      row$ci_hi <- NA_real_
    }
    row
  })

  valid_rows <- Filter(Negate(is.null), curve_rows)

  if (length(valid_rows) == 0) {
    # Empty result for regular/complete/tiny graphs
    cols <- data.frame(threshold = numeric(0), n_rich = integer(0),
                       phi = numeric(0), stringsAsFactors = FALSE)
    if (normalized) {
      cols$phi_norm <- numeric(0)
      cols$phi_rand <- numeric(0)
      cols$ci_lo <- numeric(0)
      cols$ci_hi <- numeric(0)
    }
    result <- cols
  } else {
    result <- do.call(rbind, valid_rows)
    rownames(result) <- NULL
  }

  if (!is.null(digits) && nrow(result) > 0) {
    num_cols <- vapply(result, is.numeric, logical(1))
    result[num_cols] <- lapply(result[num_cols], round, digits = digits)
  }

  attr(result, "rich") <- rich
  attr(result, "weighted") <- weighted
  attr(result, "normalized") <- normalized
  attr(result, "network") <- x
  class(result) <- c("cograph_rich_club", "data.frame")
  result
}


#' Compute phi for a set of rich nodes
#' @noRd
.compute_phi <- function(g, rich_idx, edge_wts, ranked_wts, weighted) {
  subg <- igraph::induced_subgraph(g, rich_idx)
  n_rich <- length(rich_idx)
  e_rich <- igraph::ecount(subg)

  if (e_rich == 0) return(0)

  if (weighted) {
    w_rich <- sum(if (!is.null(igraph::E(subg)$weight)) {
      igraph::E(subg)$weight
    } else {
      rep(1, e_rich)
    })
    w_max <- sum(ranked_wts[seq_len(min(e_rich, length(ranked_wts)))])
    if (w_max == 0) return(0)
    w_rich / w_max
  } else {
    max_edges <- n_rich * (n_rich - 1) / 2
    if (max_edges == 0) return(0)
    e_rich / max_edges
  }
}


#' Local Rich Club Score
#'
#' Computes, for each node, whether its strongest ties go to prominent nodes.
#' The score is the mean weight of the node's ties to prominent neighbors
#' divided by the mean weight of all its ties. A score above 1 means that the
#' node's ties to prominent nodes are stronger than its average tie. A
#' directed network is converted to an undirected one before the computation,
#' with the weights of reciprocal edges summed, and self-loops are removed.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna
#'   object.
#' @param prominence Which nodes are prominent. Either a logical or 0/1
#'   vector with one element per node (TRUE or 1 marks a prominent node), or
#'   a single number used as a threshold, in which case nodes with degree or
#'   strength strictly greater than it are prominent. If NULL (default), nodes
#'   with degree or strength strictly above the median are prominent.
#' @param rich Character. \code{"k"} (degree, default) or \code{"s"}
#'   (strength). Used when \code{prominence} is NULL or a threshold.
#' @param directed Logical or NULL. Default NULL (auto-detect).
#' @param digits Integer or NULL. Round scores. Default NULL.
#' @param sort_by Character or NULL. Column to sort by in descending order,
#'   \code{"score"} (default) or \code{"node"}. Any other value, and NULL,
#'   keep the node order.
#' @param ... Passed to \code{\link{to_igraph}}, which takes no further
#'   arguments, so any argument supplied here raises an error.
#'
#' @return A plain data frame with one row per node and columns \code{node}
#'   (node label) and \code{score}. A node with no neighbors or no prominent
#'   neighbor scores 1.
#'
#' @details
#' For each node \eqn{i}, \eqn{r_i = \bar{w}_{i, rich} / \bar{w}_i}{r_i = bar{w}_{i, rich} / bar{w}_i}, where
#' both means are taken over the ties of \eqn{i} with positive weight.
#'
#' @references
#' Opsahl, T., Colizza, V., Panzarasa, P. & Ramasco, J.J. (2008).
#' Prominence and control: The weighted rich-club effect. \emph{Physical
#' Review Letters}, 101, 168702.
#'
#' @seealso \code{\link{rich_club}}, \code{\link{centrality}}
#'
#' @export
#' @examplesIf requireNamespace("igraph", quietly = TRUE)
#' cograph::rich_club_local(regulation_net)
rich_club_local <- function(x,
                            prominence = NULL,
                            rich = c("k", "s"),
                            directed = NULL,
                            digits = NULL,
                            sort_by = "score",
                            ...) {

  rich <- match.arg(rich)
  g <- to_igraph(x, directed = directed, ...)

  if (igraph::is_directed(g)) {
    g <- igraph::as_undirected(g, mode = "collapse",
                               edge.attr.comb = list(weight = "sum", "ignore"))
  }
  g <- igraph::simplify(g)

  n <- igraph::vcount(g)
  node_names <- igraph::V(g)$name
  if (is.null(node_names)) node_names <- as.character(seq_len(n))

  # Resolve prominence
  if (is.null(prominence)) {
    prom_scores <- if (rich == "k") igraph::degree(g) else igraph::strength(g)
    is_prominent <- prom_scores > stats::median(prom_scores)
  } else if (length(prominence) == 1 && is.numeric(prominence)) {
    prom_scores <- if (rich == "k") igraph::degree(g) else igraph::strength(g)
    is_prominent <- prom_scores > prominence
  } else {
    stopifnot(length(prominence) == n)
    is_prominent <- as.logical(prominence)
  }

  adj <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight",
                                                sparse = FALSE))
  if (all(adj %in% c(0, 1))) {
    adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
  }

  scores <- vapply(seq_len(n), function(i) {
    neighbors <- which(adj[i, ] > 0)
    if (length(neighbors) == 0) return(1)

    w_all <- adj[i, neighbors]
    mean_all <- mean(w_all)
    if (mean_all == 0) return(1)

    rich_neighbors <- neighbors[is_prominent[neighbors]]
    if (length(rich_neighbors) == 0) return(1)

    mean(adj[i, rich_neighbors]) / mean_all
  }, numeric(1))

  result <- data.frame(node = node_names, score = scores,
                       stringsAsFactors = FALSE)

  if (!is.null(digits)) result$score <- round(result$score, digits)

  if (!is.null(sort_by) && sort_by %in% names(result)) {
    result <- result[order(result[[sort_by]], decreasing = TRUE), ]
    rownames(result) <- NULL
  }

  result
}


# =============================================================================
# Print / Plot Methods
# =============================================================================

#' @method print cograph_rich_club
#' @export
#' @noRd
print.cograph_rich_club <- function(x, ...) {
  rich <- attr(x, "rich")
  weighted <- attr(x, "weighted")
  normalized <- attr(x, "normalized")

  cat("Rich Club Analysis\n")
  cat("==================\n")
  cat("  Prominence:", if (rich == "k") "degree" else "strength", "\n")
  cat("  Weighted:", weighted, "\n")
  cat("  Normalized:", normalized, "\n")
  cat("  Thresholds:", nrow(x), "\n")

  if (normalized && "phi_norm" %in% names(x)) {
    sig <- x[!is.na(x$phi_norm) & x$phi_norm > 1, ]
    if (nrow(sig) > 0) {
      cat("  Rich club detected at", nrow(sig), "of", nrow(x),
          "thresholds (phi_norm > 1)\n")
    } else {
      cat("  No rich club effect detected\n")
    }
  }

  cat("\n")
  print.data.frame(utils::head(x, 10), row.names = FALSE, ...)
  if (nrow(x) > 10) cat("  ... (", nrow(x), " rows total)\n")
  invisible(x)
}


#' @rdname plot-results
#' @method plot cograph_rich_club
#' @export
plot.cograph_rich_club <- function(x, type = c("curve", "network"),
                                   k = NULL, col = "#E41A1C", ...) {
  type <- match.arg(type)
  if (type == "network") {
    .plot_rich_club_network(x, k = k, col = col, ...)
  } else {
    .plot_rich_club_curve(x, col = col, ...)
  }
  invisible(x)
}

#' @noRd
.plot_rich_club_curve <- function(x, col = "#E41A1C", ...) {
  normalized <- attr(x, "normalized")
  rich <- attr(x, "rich")
  use_norm <- isTRUE(normalized) && "phi_norm" %in% names(x)

  x_label <- if (rich == "k") "Degree threshold" else "Strength threshold"

  if (use_norm) {
    keep <- !is.na(x$phi_norm)
    x_vals <- x$threshold[keep]

    # Plot observed phi and random phi together
    y_max <- max(c(x$phi[keep], x$ci_hi[keep]), na.rm = TRUE) * 1.1
    graphics::plot(x_vals, x$phi[keep],
                   type = "b", pch = 16, col = col, lwd = 2,
                   xlab = x_label, ylab = expression(phi),
                   ylim = c(0, y_max),
                   main = "Rich Club Coefficient", ...)

    # Null model CI band
    ci_col <- grDevices::adjustcolor("steelblue", 0.2)
    graphics::polygon(c(x_vals, rev(x_vals)),
                      c(x$ci_lo[keep], rev(x$ci_hi[keep])),
                      col = ci_col, border = NA)
    # Null model mean
    graphics::lines(x_vals, x$phi_rand[keep],
                    col = "steelblue", lwd = 2, lty = 2)

    # Re-draw observed on top
    graphics::lines(x_vals, x$phi[keep],
                    type = "b", pch = 16, col = col, lwd = 2)

    # Significance stars where phi > ci_hi
    sig <- keep & x$phi > x$ci_hi
    if (any(sig)) {
      graphics::points(x$threshold[sig], x$phi[sig],
                       pch = 8, col = col, cex = 1.5)
    }

    graphics::legend("topright",
                     legend = c("Observed", "Random (95% CI)"),
                     col = c(col, "steelblue"),
                     lwd = 2, lty = c(1, 2), pch = c(16, NA),
                     bty = "n", cex = 0.9)

    # Annotate phi_norm on right axis
    graphics::mtext(sprintf("Max phi_norm = %.2f",
                            max(x$phi_norm[keep], na.rm = TRUE)),
                    side = 3, adj = 1, cex = 0.8, col = "gray30")
  } else {
    keep <- !is.na(x$phi)
    graphics::plot(x$threshold[keep], x$phi[keep],
                   type = "b", pch = 16, col = col, lwd = 2,
                   xlab = x_label, ylab = expression(phi),
                   main = "Rich Club Coefficient", ...)
  }

  # n_rich annotation on top
  graphics::axis(3, at = x$threshold[keep], labels = x$n_rich[keep],
                 tick = FALSE, line = -0.8, cex.axis = 0.7, col.axis = "gray50")
  graphics::mtext("n nodes", side = 3, line = 1.2, cex = 0.7, col = "gray50")

  graphics::grid(col = grDevices::adjustcolor("gray50", 0.3), lty = 1)
}

#' @noRd
.plot_rich_club_network <- function(x, k = NULL, col = "#E41A1C", ...) {
  network <- attr(x, "network")
  rich_type <- attr(x, "rich")

  if (is.null(network)) {
    stop("Network not stored in rich_club object. Cannot plot network view.",
         call. = FALSE)
  }

  # Pick threshold
  if (is.null(k)) {
    metric <- if ("phi_norm" %in% names(x) && any(!is.na(x$phi_norm))) {
      x$phi_norm
    } else {
      x$phi
    }
    best_row <- which.max(metric)
    if (length(best_row) == 0 || nrow(x) == 0) {
      stop("No valid thresholds to plot. The network may be too small or ",
           "regular for rich club analysis.", call. = FALSE)
    }
    k <- x$threshold[best_row]
  }

  g <- to_igraph(network)
  if (igraph::is_directed(g)) {
    g_und <- igraph::as_undirected(g, mode = "collapse",
                                    edge.attr.comb = list(weight = "sum",
                                                          "ignore"))
  } else {
    g_und <- g
  }
  g_und <- igraph::simplify(g_und)

  prom <- if (rich_type == "k") igraph::degree(g_und) else
    igraph::strength(g_und)
  is_rich <- prom > k
  n_rich <- sum(is_rich)

  node_cols <- ifelse(is_rich, col, grDevices::adjustcolor("gray70", 0.8))
  base_size <- 7
  node_sizes <- ifelse(is_rich, base_size * 1.8, base_size)

  # Highlight edges between rich nodes
  el <- igraph::as_edgelist(g, names = TRUE)
  node_names <- igraph::V(g)$name
  if (is.null(node_names)) node_names <- as.character(seq_len(igraph::vcount(g)))
  rich_names <- node_names[is_rich]
  edge_in_club <- el[, 1] %in% rich_names & el[, 2] %in% rich_names
  edge_cols <- ifelse(edge_in_club, col,
                      grDevices::adjustcolor("gray80", 0.5))
  edge_widths <- ifelse(edge_in_club, 2, 0.5)

  title <- sprintf("Rich Club (k > %g, %d members)", k, n_rich)

  # Build args: user ... overrides defaults
  splot_args <- list(x = network, node_color = node_cols,
                     node_size = node_sizes, edge_color = edge_cols,
                     edge_width = edge_widths, title = title)
  user_args <- list(...)
  splot_args[names(user_args)] <- user_args
  do.call(splot, splot_args)
}
