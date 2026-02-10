#' Network Robustness Analysis
#'
#' @description
#' Performs a targeted attack or random failure analysis on a network,
#' calculating the size of the largest connected component after sequential
#' vertex or edge removal.
#'
#' In a targeted attack, vertices are sorted by degree or betweenness centrality
#' (or edges by betweenness), and successively removed from highest to lowest.
#' In a random failure analysis, vertices/edges are removed in random order.
#'
#' @param x Network input: matrix, igraph, network, cograph_network, or tna object
#' @param type Character string; either "vertex" or "edge" removals. Default: "vertex"
#' @param measure Character string; sort by "betweenness", "degree", or "random".
#'   Default: "betweenness"
#' @param n_iter Integer; number of iterations for random analysis. Default: 100
#' @param mode For directed networks: "all", "in", or "out". Default "all".
#' @param seed Random seed for reproducibility. Default NULL.
#' @param ... Additional arguments passed to \code{\link{to_igraph}}
#'
#' @return A data frame (class "cograph_robustness") with columns:
#' \describe{
#'   \item{removed_pct}{Fraction of vertices/edges removed (0 to 1)}
#'   \item{comp_size}{Size of largest component after removal}
#'   \item{comp_pct}{Ratio of component size to original maximum}
#'   \item{measure}{Attack strategy used}
#'   \item{type}{Type of analysis (vertex or edge removal)}
#' }
#'
#' @details
#' Three attack strategies are available:
#'
#' \strong{Targeted Attack - Betweenness (default):}
#' Vertices/edges are sorted by betweenness centrality and removed from
#' highest to lowest. This targets nodes that bridge different network regions.
#'
#' \strong{Targeted Attack - Degree:}
#' Vertices are sorted by degree and removed from highest to lowest.
#' This targets highly connected hub nodes. Note: for edge attacks, degree
#' is not available; use betweenness instead.
#'
#' \strong{Random Failure:}
#' Vertices/edges are removed in random order, averaged over n_iter iterations.
#' This simulates random component failures.
#'
#' Scale-free networks are typically robust to random failures but vulnerable
#' to targeted attacks, while random networks degrade more uniformly.
#'
#' @references
#' Albert, R., Jeong, H., & Barabasi, A.L. (2000). Error and attack tolerance
#' of complex networks. \emph{Nature}, 406, 378-381.
#' \doi{10.1038/35019019}
#'
#' @export
#' @examples
#' # Create a scale-free network
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::sample_pa(50, m = 2, directed = FALSE)
#'
#'   # Targeted attack by betweenness
#'   rob_btw <- robustness(g, measure = "betweenness")
#'
#'   # Targeted attack by degree
#'   rob_deg <- robustness(g, measure = "degree")
#'
#'   # Random failure
#'   rob_rnd <- robustness(g, measure = "random", n_iter = 50)
#'
#'   # View results
#'   head(rob_btw)
#' }
#'
#' @seealso \code{\link{plot_robustness}}, \code{\link{robustness_auc}}
robustness <- function(x,
                       type = c("vertex", "edge"),
                       measure = c("betweenness", "degree", "random"),
                       n_iter = 100,
                       mode = "all",
                       seed = NULL,
                       ...) {

  type <- match.arg(type)
  measure <- match.arg(measure)

  # Convert to igraph
  g <- to_igraph(x, ...)

  # Validate
  if (type == "edge" && measure == "degree") {
    stop("For edge attacks, use 'betweenness' or 'random'", call. = FALSE)
  }

  # Set seed
  if (!is.null(seed)) set.seed(seed)

  # Get original largest component size

  orig_max <- max(igraph::components(g)$csize)
  n <- switch(type, vertex = igraph::vcount(g), edge = igraph::ecount(g))

  # Removed percentage sequence
  removed_pct <- seq(0, 1, length.out = n + 1)

  # Perform analysis
  if (type == "vertex") {
    result <- robustness_vertex_attack(g, measure, mode, n_iter, orig_max, n)
  } else {
    result <- robustness_edge_attack(g, measure, n_iter, orig_max, n)
  }

  # Build output
  comp_size <- c(result, 0)
  comp_pct <- comp_size / orig_max

  out <- data.frame(
    removed_pct = removed_pct,
    comp_size = comp_size,
    comp_pct = comp_pct,
    measure = measure,
    type = paste0(
      ifelse(measure == "random", "Random ", "Targeted "),
      type,
      ifelse(measure == "random", " removal", " attack")
    ),
    stringsAsFactors = FALSE
  )

  class(out) <- c("cograph_robustness", "data.frame")
  attr(out, "n_original") <- n
  attr(out, "orig_max") <- orig_max

  out
}


#' Vertex Attack Analysis
#' @keywords internal
robustness_vertex_attack <- function(g, measure, mode, n_iter, orig_max, n) {

  if (measure == "random") {
    # Random failure - average over iterations
    all_results <- matrix(orig_max, nrow = n, ncol = n_iter)

    for (iter in seq_len(n_iter)) {
      g_temp <- g

      for (j in seq_len(n - 1)) {
        # Pick a random vertex to remove (always vertex 1 after random selection)
        n_remaining <- igraph::vcount(g_temp)
        if (n_remaining <= 1) break
        v_to_remove <- sample(seq_len(n_remaining), 1)
        g_temp <- igraph::delete_vertices(g_temp, v_to_remove)
        all_results[j + 1, iter] <- max(igraph::components(g_temp)$csize)
      }
    }

    comp_removed <- rowMeans(all_results)

  } else {
    # Targeted attack - recalculate centrality after each removal
    comp_removed <- rep(orig_max, n)
    g_temp <- g

    for (j in seq_len(n - 1)) {
      if (igraph::vcount(g_temp) <= 1) break

      # Calculate current centrality
      if (measure == "betweenness") {
        val <- igraph::betweenness(g_temp, directed = igraph::is_directed(g_temp))
      } else {
        val <- igraph::degree(g_temp, mode = mode)
      }

      # Remove highest centrality vertex
      v_to_remove <- which.max(val)
      g_temp <- igraph::delete_vertices(g_temp, v_to_remove)
      comp_removed[j + 1] <- max(igraph::components(g_temp)$csize)
    }
  }

  comp_removed
}


#' Edge Attack Analysis
#' @keywords internal
robustness_edge_attack <- function(g, measure, n_iter, orig_max, n) {

  if (measure == "random") {
    # Random edge removal
    all_results <- matrix(orig_max, nrow = n, ncol = n_iter)

    for (iter in seq_len(n_iter)) {
      g_temp <- g

      for (j in seq_len(n - 1)) {
        n_edges <- igraph::ecount(g_temp)
        if (n_edges == 0) break

        # Remove a random edge
        e_to_remove <- sample(seq_len(n_edges), 1)
        g_temp <- igraph::delete_edges(g_temp, e_to_remove)
        csize <- igraph::components(g_temp)$csize
        all_results[j + 1, iter] <- if (length(csize) > 0) max(csize) else 0
      }
    }

    comp_removed <- rowMeans(all_results)

  } else {
    # Targeted attack by edge betweenness - recalculate after each removal
    comp_removed <- rep(orig_max, n)
    g_temp <- g

    for (j in seq_len(n - 1)) {
      if (igraph::ecount(g_temp) == 0) break

      # Calculate current edge betweenness
      eb <- igraph::edge_betweenness(g_temp, directed = igraph::is_directed(g_temp))

      # Remove highest betweenness edge
      e_to_remove <- which.max(eb)
      g_temp <- igraph::delete_edges(g_temp, e_to_remove)
      csize <- igraph::components(g_temp)$csize
      comp_removed[j + 1] <- if (length(csize) > 0) max(csize) else 0
    }
  }

  comp_removed
}


#' Plot Network Robustness
#'
#' Creates a visualization of network robustness showing the fraction of
#' remaining nodes in the largest connected component during sequential
#' node/edge removal. Supports comparison of multiple attack strategies.
#'
#' @param ... One or more robustness results from \code{\link{robustness}},
#'   or named arguments to pass networks for on-the-fly computation.
#' @param x Network for computing robustness on-the-fly.
#' @param measures Character vector of attack strategies to compare.
#'   Default c("betweenness", "degree", "random").
#' @param colors Named vector of colors. Default: green=Degree, red=Betweenness,
#'   blue=Random (matching Nature paper style).
#' @param title Plot title. Default "Network Robustness: sequential removal of nodes".
#' @param xlab X-axis label. Default "Fraction of removed nodes".
#' @param ylab Y-axis label. Default "Fraction of remaining nodes".
#' @param lwd Line width. Default 1.5.
#' @param legend_pos Legend position. Default "topright".
#' @param n_iter Number of iterations for random. Default 100.
#' @param seed Random seed. Default NULL.
#' @param type Removal type. Default "vertex".
#'
#' @return Invisibly returns combined data frame of all robustness results.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::sample_pa(50, m = 2, directed = FALSE)
#'
#'   # Quick comparison of all strategies
#'   plot_robustness(x = g, n_iter = 20)
#'
#'   # Or compute separately
#'   rob1 <- robustness(g, measure = "betweenness")
#'   rob2 <- robustness(g, measure = "degree")
#'   rob3 <- robustness(g, measure = "random", n_iter = 20)
#'   plot_robustness(rob1, rob2, rob3)
#' }
plot_robustness <- function(...,
                            x = NULL,
                            measures = c("betweenness", "degree", "random"),
                            colors = NULL,
                            title = "Network Robustness: sequential removal of nodes",
                            xlab = "Fraction of removed nodes",
                            ylab = "Fraction of remaining nodes",
                            lwd = 1.5,
                            legend_pos = "topright",
                            n_iter = 100,
                            seed = NULL,
                            type = "vertex") {

  # Default colors (Nature paper style)
  if (is.null(colors)) {
    colors <- c(
      betweenness = "#E41A1C",  # Red
      degree = "#4DAF4A",       # Green
      random = "#377EB8"        # Blue
    )
  }

  # Collect data

  dots <- list(...)

  if (length(dots) > 0 && all(sapply(dots, function(d) {
    inherits(d, "cograph_robustness") || is.data.frame(d)
  }))) {
    # Combine provided results
    all_data <- do.call(rbind, dots)
  } else if (!is.null(x)) {
    # Compute on-the-fly
    if (!is.null(seed)) set.seed(seed)
    all_data <- do.call(rbind, lapply(measures, function(m) {
      robustness(x, type = type, measure = m, n_iter = n_iter)
    }))
  } else {
    stop("Provide robustness results or a network (x)", call. = FALSE)
  }

  # Get unique measures
  unique_measures <- unique(all_data$measure)

  # Set up plot
  plot(NULL,
       xlim = c(0, 1),
       ylim = c(0, 1),
       xlab = xlab,
       ylab = ylab,
       main = title,
       las = 1,
       xaxs = "i", yaxs = "i")

  # Add subtle grid
  abline(h = seq(0, 1, 0.25), col = "gray90", lty = 1)
  abline(v = seq(0, 1, 0.25), col = "gray90", lty = 1)

  # Plot lines
  for (m in unique_measures) {
    d <- all_data[all_data$measure == m, ]
    col <- if (m %in% names(colors)) colors[m] else "gray50"
    lines(d$removed_pct, d$comp_pct, col = col, lwd = lwd)
  }

  # Legend
  legend_labels <- unique_measures
  legend_labels <- gsub("betweenness", "Betweenness", legend_labels)
  legend_labels <- gsub("degree", "Degree", legend_labels)
  legend_labels <- gsub("random", "Random", legend_labels)

  legend_colors <- sapply(unique_measures, function(m) {
    if (m %in% names(colors)) colors[m] else "gray50"
  })

  legend(legend_pos,
         legend = legend_labels,
         col = legend_colors,
         lwd = lwd,
         bty = "n",
         title = "Removal based on")

  invisible(all_data)
}


#' Compare Network Robustness (ggplot2)
#'
#' Creates a ggplot2 faceted visualization comparing robustness across
#' multiple networks. Produces publication-quality figures similar to
#' those in Nature Scientific Reports.
#'
#' @param ... Named arguments: network names as names, network objects as values.
#' @param networks Named list of networks (alternative to ...).
#' @param measures Attack strategies to compare. Default c("betweenness", "degree", "random").
#' @param colors Named vector of colors for measures.
#' @param title Overall title. Default NULL.
#' @param n_iter Iterations for random. Default 100.
#' @param seed Random seed. Default NULL.
#' @param type Removal type. Default "vertex".
#' @param ncol Columns in facet. Default NULL (auto).
#' @param free_y If TRUE, allow different y-axis scales per facet. Default FALSE.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'
#'   g1 <- igraph::sample_pa(40, m = 2, directed = FALSE)
#'   g2 <- igraph::sample_gnp(40, 0.15)
#'
#'   ggplot_robustness(
#'     "Teaching network" = g1,
#'     "Collaborative network" = g2,
#'     n_iter = 20
#'   )
#' }
ggplot_robustness <- function(...,
                              networks = NULL,
                              measures = c("betweenness", "degree", "random"),
                              colors = NULL,
                              title = NULL,
                              n_iter = 100,
                              seed = NULL,
                              type = "vertex",
                              ncol = NULL,
                              free_y = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required", call. = FALSE)
  }

  # Default colors
  if (is.null(colors)) {
    colors <- c(
      Betweenness = "#E41A1C",
      Degree = "#4DAF4A",
      Random = "#377EB8"
    )
  }

  # Collect networks
  dots <- list(...)
  if (length(dots) > 0 && is.null(networks)) {
    networks <- dots
  }

  if (is.null(networks) || length(networks) == 0) {
    stop("Provide at least one network", call. = FALSE)
  }

  if (is.null(names(networks))) {
    names(networks) <- paste0("Network ", seq_along(networks))
  }

  if (!is.null(seed)) set.seed(seed)

  # Compute robustness
  all_data <- do.call(rbind, lapply(names(networks), function(net_name) {
    net <- networks[[net_name]]
    net_data <- do.call(rbind, lapply(measures, function(m) {
      rob <- robustness(net, type = type, measure = m, n_iter = n_iter)
      rob$network <- net_name
      rob
    }))
    net_data
  }))

  # Clean measure names
  all_data$Measure <- all_data$measure
  all_data$Measure <- gsub("betweenness", "Betweenness", all_data$Measure)
  all_data$Measure <- gsub("degree", "Degree", all_data$Measure)
  all_data$Measure <- gsub("random", "Random", all_data$Measure)
  all_data$Measure <- factor(all_data$Measure,
                             levels = c("Betweenness", "Degree", "Random"))

  # Preserve order
  all_data$network <- factor(all_data$network, levels = names(networks))

  # Build title for each network
  all_data$facet_title <- paste0(all_data$network, ": sequential removal of nodes")

  # Create plot
  p <- ggplot2::ggplot(all_data,
                       ggplot2::aes(x = .data$removed_pct,
                                    y = .data$comp_pct,
                                    color = .data$Measure)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = colors, name = "Removal based on") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25),
                                labels = c("0.00", "0.25", "0.50", "0.75", "1.00")) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25),
                                labels = c("0.00", "0.25", "0.50", "0.75", "1.00")) +
    ggplot2::labs(
      x = "Fraction of removed nodes",
      y = "Fraction of remaining nodes",
      title = title
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      strip.text = ggplot2::element_text(hjust = 0, face = "plain", size = 11),
      legend.position = "right",
      axis.title = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(size = 12)
    )

  # Facet
  if (length(networks) > 1) {
    if (is.null(ncol)) ncol <- min(length(networks), 2)
    scales_arg <- if (free_y) "free_y" else "fixed"
    p <- p + ggplot2::facet_wrap(~ facet_title, ncol = ncol, scales = scales_arg)
  } else {
    # Single network - use network name as title
    p <- p + ggplot2::ggtitle(paste0(names(networks)[1], ": sequential removal of nodes"))
  }

  p
}


#' Calculate Area Under Robustness Curve (AUC)
#'
#' Computes the area under the robustness curve using trapezoidal integration.
#' Higher AUC indicates a more robust network. Maximum AUC is 1.0.
#'
#' @param x A robustness result from \code{\link{robustness}}.
#'
#' @return Numeric AUC value between 0 and 1.
#'
#' @export
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- igraph::sample_pa(30, m = 2, directed = FALSE)
#'
#'   rob_btw <- robustness(g, measure = "betweenness")
#'   rob_rnd <- robustness(g, measure = "random", n_iter = 20)
#'
#'   cat("Betweenness attack AUC:", round(robustness_auc(rob_btw), 3), "\n")
#'   cat("Random failure AUC:", round(robustness_auc(rob_rnd), 3), "\n")
#' }
robustness_auc <- function(x) {
  if (!all(c("removed_pct", "comp_pct") %in% names(x))) {
    stop("Input must have 'removed_pct' and 'comp_pct' columns", call. = FALSE)
  }

  x <- x[order(x$removed_pct), ]
  n <- nrow(x)

  # Trapezoidal rule
  auc <- sum(diff(x$removed_pct) * (x$comp_pct[-n] + x$comp_pct[-1]) / 2)
  auc
}


#' Summary of Robustness Analysis
#'
#' Provides a summary comparing robustness metrics across attack strategies.
#'
#' @param ... Robustness results to summarize.
#' @param x Network for on-the-fly computation.
#' @param measures Measures to compute if x provided.
#' @param n_iter Iterations for random. Default 50.
#'
#' @return Data frame with AUC and critical points for each measure.
#'
#' @export
robustness_summary <- function(..., x = NULL, measures = NULL, n_iter = 50) {

  dots <- list(...)

  if (length(dots) > 0) {
    all_data <- dots
  } else if (!is.null(x)) {
    if (is.null(measures)) measures <- c("betweenness", "degree", "random")
    all_data <- lapply(measures, function(m) {
      robustness(x, measure = m, n_iter = n_iter)
    })
  } else {
    stop("Provide robustness results or a network (x)", call. = FALSE)
  }

  # Compute summary for each
  summaries <- lapply(all_data, function(d) {
    auc <- robustness_auc(d)

    # Find critical point (where comp_pct drops below 0.5)
    below_50 <- which(d$comp_pct < 0.5)
    critical_50 <- if (length(below_50) > 0) d$removed_pct[min(below_50)] else 1.0

    # Find point where network fragments (comp_pct < 0.1)
    below_10 <- which(d$comp_pct < 0.1)
    critical_10 <- if (length(below_10) > 0) d$removed_pct[min(below_10)] else 1.0

    data.frame(
      measure = d$measure[1],
      auc = round(auc, 4),
      critical_50 = round(critical_50, 4),
      critical_10 = round(critical_10, 4)
    )
  })

  do.call(rbind, summaries)
}
