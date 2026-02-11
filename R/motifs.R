#' Network Motif Analysis
#'
#' Analyze recurring subgraph patterns (motifs) in networks and test their
#' statistical significance against null models.
#'
#' @param x A matrix, igraph object, or cograph_network
#' @param size Motif size: 3 (triads) or 4 (tetrads). Default 3.
#' @param n_random Number of random networks for null model. Default 100.
#' @param method Null model method: "configuration" (preserves degree) or
#'   "gnm" (preserves edge count). Default "configuration".
#' @param directed Logical. Treat as directed? Default auto-detected.
#' @param seed Random seed for reproducibility
#'
#' @return A `cograph_motifs` object containing:
#'   - `counts`: Motif counts in observed network
#'   - `null_mean`: Mean counts in random networks
#'   - `null_sd`: Standard deviation in random networks
#'   - `z_scores`: Z-scores (observed - mean) / sd
#'   - `p_values`: Two-tailed p-values
#'   - `significant`: Logical vector (|z| > 2)
#'   - `size`: Motif size (3 or 4)
#'   - `directed`: Whether network is directed
#'   - `n_random`: Number of random networks used
#'
#' @examples
#' # Create a directed network
#' mat <- matrix(c(
#'   0, 1, 1, 0,
#'   0, 0, 1, 1,
#'   0, 0, 0, 1,
#'   1, 0, 0, 0
#' ), 4, 4, byrow = TRUE)
#'
#' # Analyze triadic motifs
#' m <- motif_census(mat)
#' print(m)
#' plot(m)
#'
#' @export
motif_census <- function(x, size = 3, n_random = 100,
                         method = c("configuration", "gnm"),
                         directed = NULL, seed = NULL) {


  method <- match.arg(method)


  # Convert to igraph

  if (inherits(x, "igraph")) {
    g <- x
  } else if (inherits(x, "cograph_network")) {
    g <- to_igraph(x)
  } else if (is.matrix(x)) {
    if (is.null(directed)) {
      directed <- !isSymmetric(unname(x))
    }
    mode <- if (directed) "directed" else "undirected"
    g <- igraph::graph_from_adjacency_matrix(x, mode = mode, weighted = TRUE)
  } else {
    stop("x must be a matrix, igraph object, or cograph_network")
  }

  if (is.null(directed)) {
    directed <- igraph::is_directed(g)
  }

  if (!directed && size == 3) {
    # For undirected, use triangle counting
    return(.motif_census_undirected(g, n_random, method, seed))
  }

  if (size != 3 && size != 4) {
    stop("size must be 3 or 4")
  }

  if (!is.null(seed)) set.seed(seed)

  # Count motifs in observed network
  observed <- igraph::motifs(g, size = size)
  observed[is.na(observed)] <- 0

  # Generate null distribution
  null_counts <- matrix(0, nrow = n_random, ncol = length(observed))

  for (i in seq_len(n_random)) {
    g_rand <- .generate_random_graph(g, method)
    counts <- igraph::motifs(g_rand, size = size)
    counts[is.na(counts)] <- 0
    null_counts[i, ] <- counts
  }

  # Calculate statistics
  null_mean <- colMeans(null_counts)
  null_sd <- apply(null_counts, 2, sd)

  # Z-scores (handle zero SD)
  z_scores <- ifelse(null_sd > 0,
                     (observed - null_mean) / null_sd,
                     0)

  # P-values (two-tailed)
  p_values <- 2 * pnorm(-abs(z_scores))

  # Get motif names
  motif_names <- .get_motif_names(size, directed)

  # Create result
  result <- list(
    counts = stats::setNames(observed, motif_names),
    null_mean = stats::setNames(null_mean, motif_names),
    null_sd = stats::setNames(null_sd, motif_names),
    z_scores = stats::setNames(z_scores, motif_names),
    p_values = stats::setNames(p_values, motif_names),
    significant = abs(z_scores) > 2,
    size = size,
    directed = directed,
    n_random = n_random,
    method = method
  )

  class(result) <- "cograph_motifs"
  result
}

#' @noRd
.motif_census_undirected <- function(g, n_random, method, seed) {
  if (!is.null(seed)) set.seed(seed)


  # For undirected networks, count triangles and other 3-node patterns
  n <- igraph::vcount(g)

  # Count triads: edges, wedges (2-paths), triangles
  n_edges <- igraph::ecount(g)
  n_triangles <- sum(igraph::count_triangles(g)) / 3

  # Wedges (connected triples that aren't triangles)
  # Total possible triads = n choose 3
  total_triads <- choose(n, 3)

  # Connected pairs
  degrees <- igraph::degree(g)
  n_wedges <- sum(choose(degrees, 2)) - 3 * n_triangles

  # Disconnected triads
  n_empty <- total_triads - n_triangles - n_wedges

  observed <- c(empty = n_empty, wedge = n_wedges, triangle = n_triangles)

  # Null distribution
  null_counts <- matrix(0, nrow = n_random, ncol = 3)
  colnames(null_counts) <- names(observed)

  for (i in seq_len(n_random)) {
    g_rand <- .generate_random_graph(g, method)
    deg_rand <- igraph::degree(g_rand)
    tri_rand <- sum(igraph::count_triangles(g_rand)) / 3
    wedge_rand <- sum(choose(deg_rand, 2)) - 3 * tri_rand
    empty_rand <- total_triads - tri_rand - wedge_rand
    null_counts[i, ] <- c(empty_rand, wedge_rand, tri_rand)
  }

  null_mean <- colMeans(null_counts)
  null_sd <- apply(null_counts, 2, sd)
  z_scores <- ifelse(null_sd > 0, (observed - null_mean) / null_sd, 0)
  p_values <- 2 * pnorm(-abs(z_scores))


  result <- list(
    counts = observed,
    null_mean = null_mean,
    null_sd = null_sd,
    z_scores = z_scores,
    p_values = p_values,
    significant = abs(z_scores) > 2,
    size = 3,
    directed = FALSE,
    n_random = n_random,
    method = method
  )

  class(result) <- "cograph_motifs"
  result
}

#' @noRd
.generate_random_graph <- function(g, method) {
  directed <- igraph::is_directed(g)

  if (method == "configuration") {
    if (directed) {
      in_deg <- igraph::degree(g, mode = "in")
      out_deg <- igraph::degree(g, mode = "out")
      g_rand <- igraph::sample_degseq(out_deg, in_deg, method = "configuration")
    } else {
      deg <- igraph::degree(g)
      g_rand <- igraph::sample_degseq(deg, method = "vl")
    }
  } else {
    # gnm - preserve edge count
    n <- igraph::vcount(g)
    m <- igraph::ecount(g)
    g_rand <- igraph::sample_gnm(n, m, directed = directed)
  }

  igraph::simplify(g_rand)
}

#' @noRd
.get_motif_names <- function(size, directed) {
  if (size == 3 && directed) {
    # 16 directed triads (MAN notation)
    c("003", "012", "102", "021D", "021U", "021C", "111D", "111U",
      "030T", "030C", "201", "120D", "120U", "120C", "210", "300")
  } else if (size == 3 && !directed) {
    c("empty", "edge", "triangle")
  } else if (size == 4) {
    paste0("M", seq_len(if (directed) 199 else 11))
  } else {
    paste0("motif_", seq_len(100))
  }
}

#' @method print cograph_motifs
#' @export
print.cograph_motifs <- function(x, ...) {
  cat("Network Motif Analysis\n")
  cat(sprintf("Size: %d-node motifs (%s)\n",
              x$size, if (x$directed) "directed" else "undirected"))
  cat(sprintf("Null model: %s (n=%d)\n\n", x$method, x$n_random))

  # Show significant motifs
  sig_idx <- which(x$significant & x$counts > 0)

  if (length(sig_idx) > 0) {
    cat("Significant motifs:\n")
    df <- data.frame(
      motif = names(x$counts)[sig_idx],
      count = x$counts[sig_idx],
      expected = round(x$null_mean[sig_idx], 1),
      z = round(x$z_scores[sig_idx], 2),
      p = format.pval(x$p_values[sig_idx], digits = 2)
    )
    print(df, row.names = FALSE)
  } else {
    cat("No significantly over/under-represented motifs found.\n")
  }

  # Summary
  n_over <- sum(x$z_scores > 2 & x$counts > 0, na.rm = TRUE)
  n_under <- sum(x$z_scores < -2 & x$counts > 0, na.rm = TRUE)
  cat(sprintf("\nOver-represented: %d | Under-represented: %d\n", n_over, n_under))

  invisible(x)
}

#' Plot Network Motifs
#'
#' Visualize motif frequencies and their statistical significance.
#'
#' @param x A `cograph_motifs` object from [motif_census()]
#' @param type Plot type: "bar" (default), "heatmap", or "network"
#' @param show_nonsig Show non-significant motifs? Default FALSE
#' @param top_n Show only top N motifs by |z-score|. Default NULL (all)
#' @param colors Colors for under/neutral/over-represented. Default blue/gray/red.
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object (invisibly)
#'
#' @examples
#' mat <- matrix(sample(0:1, 100, replace = TRUE, prob = c(0.7, 0.3)), 10, 10)
#' diag(mat) <- 0
#' m <- motif_census(mat, directed = TRUE, n_random = 50)
#' plot(m)
#' plot(m, type = "network")
#'
#' @method plot cograph_motifs
#' @export
plot.cograph_motifs <- function(x, type = c("bar", "heatmap", "network"),
                                 show_nonsig = FALSE, top_n = NULL,
                                 colors = c("#2166AC", "#F7F7F7", "#B2182B"),
                                 ...) {

  type <- match.arg(type)

 if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting motifs")
  }

  # Filter data
  df <- data.frame(
    motif = names(x$counts),
    count = as.numeric(x$counts),
    expected = x$null_mean,
    z = x$z_scores,
    p = x$p_values,
    significant = x$significant,
    stringsAsFactors = FALSE
  )

  # Remove zero counts unless they're significant

df <- df[df$count > 0 | df$significant, ]

  if (!show_nonsig) {
    df <- df[df$significant, ]
  }

  if (nrow(df) == 0) {
    message("No motifs to plot. Try show_nonsig = TRUE")
    return(invisible(NULL))
  }

  # Top N by absolute z-score
  if (!is.null(top_n) && nrow(df) > top_n) {
    df <- df[order(-abs(df$z)), ][seq_len(top_n), ]
  }

  # Order by z-score
  df$motif <- factor(df$motif, levels = df$motif[order(df$z)])

  if (type == "bar") {
    .plot_motifs_bar(df, colors, x$directed, x$size)
  } else if (type == "heatmap") {
    .plot_motifs_heatmap(df, colors)
  } else if (type == "network") {
    .plot_motifs_network(df, x$directed, x$size, colors)
  }
}

#' @noRd
.plot_motifs_bar <- function(df, colors, directed, size) {
  # Determine fill based on z-score
  df$direction <- ifelse(df$z > 2, "over",
                         ifelse(df$z < -2, "under", "neutral"))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = motif, y = z, fill = direction)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_hline(yintercept = c(-2, 2), linetype = "dashed",
                        color = "#666666", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "#333333", linewidth = 0.3) +
    ggplot2::scale_fill_manual(
      values = c(over = colors[3], neutral = colors[2], under = colors[1]),
      labels = c(over = "Over-represented", neutral = "Not significant",
                 under = "Under-represented"),
      name = NULL
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("%d-Node Motif Analysis", size),
      subtitle = if (directed) "Directed network" else "Undirected network",
      x = NULL,
      y = "Z-score (vs null model)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )

  print(p)
  invisible(p)
}

#' @noRd
.plot_motifs_heatmap <- function(df, colors) {
  df$label <- sprintf("%d\n(%.1f)", df$count, df$expected)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = 1, y = motif, fill = z)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_fill_gradient2(
      low = colors[1], mid = colors[2], high = colors[3],
      midpoint = 0, limits = c(-max(abs(df$z)), max(abs(df$z))),
      name = "Z-score"
    ) +
    ggplot2::labs(title = "Motif Frequencies", x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  print(p)
  invisible(p)
}

#' @noRd
.plot_motifs_network <- function(df, directed, size, colors) {
  # Plot actual motif patterns
  if (!directed || size != 3) {
    message("Network visualization only available for directed 3-node motifs")
    return(.plot_motifs_bar(df, colors, directed, size))
  }

  # Triad patterns (MAN notation)
  triad_patterns <- list(
    "003" = matrix(c(0,0,0, 0,0,0, 0,0,0), 3, 3),
    "012" = matrix(c(0,1,0, 0,0,0, 0,0,0), 3, 3),
    "102" = matrix(c(0,1,0, 1,0,0, 0,0,0), 3, 3),
    "021D" = matrix(c(0,1,1, 0,0,0, 0,0,0), 3, 3),
    "021U" = matrix(c(0,0,0, 1,0,0, 1,0,0), 3, 3),
    "021C" = matrix(c(0,1,0, 0,0,1, 0,0,0), 3, 3),
    "111D" = matrix(c(0,1,1, 1,0,0, 0,0,0), 3, 3),
    "111U" = matrix(c(0,1,0, 1,0,0, 1,0,0), 3, 3),
    "030T" = matrix(c(0,1,1, 0,0,1, 0,0,0), 3, 3),
    "030C" = matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3),
    "201" = matrix(c(0,1,1, 1,0,0, 1,0,0), 3, 3),
    "120D" = matrix(c(0,1,1, 0,0,0, 0,1,0), 3, 3),
    "120U" = matrix(c(0,0,0, 1,0,1, 1,0,0), 3, 3),
    "120C" = matrix(c(0,1,0, 0,0,1, 1,1,0), 3, 3),
    "210" = matrix(c(0,1,1, 1,0,1, 0,0,0), 3, 3),
    "300" = matrix(c(0,1,1, 1,0,1, 1,1,0), 3, 3)
  )

  # Create grid of motif plots
  motifs_to_plot <- df$motif[df$motif %in% names(triad_patterns)]

  if (length(motifs_to_plot) == 0) {
    message("No standard triads found in results")
    return(.plot_motifs_bar(df, colors, directed, size))
  }

  # Calculate grid dimensions
  n_plots <- length(motifs_to_plot)
  n_cols <- min(4, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  # Set up plot
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  graphics::par(mfrow = c(n_rows, n_cols), mar = c(1, 1, 3, 1))

  for (motif_name in motifs_to_plot) {
    mat <- triad_patterns[[motif_name]]
    z <- df$z[df$motif == motif_name]
    count <- df$count[df$motif == motif_name]

    # Color based on significance
    node_col <- if (z > 2) colors[3] else if (z < -2) colors[1] else "#999999"
    edge_col <- grDevices::adjustcolor(node_col, alpha.f = 0.7)

    # Create igraph and plot
    g <- igraph::graph_from_adjacency_matrix(mat, mode = "directed")
    igraph::V(g)$color <- node_col
    igraph::V(g)$frame.color <- node_col
    igraph::V(g)$size <- 25
    igraph::E(g)$color <- edge_col
    igraph::E(g)$width <- 2
    igraph::E(g)$arrow.size <- 0.5

    coords <- matrix(c(-1, 0, 1, 0.5, 0.5, -0.8), ncol = 2, byrow = TRUE)

    igraph::plot.igraph(g, layout = coords, vertex.label = NA,
                        main = sprintf("%s\nn=%d, z=%.1f", motif_name, count, z))
  }

  invisible(NULL)
}

#' Triad Census
#'
#' Count the 16 types of triads in a directed network using MAN notation.
#'
#' @param x A matrix, igraph object, or cograph_network
#' @param directed Logical. Default TRUE.
#'
#' @return Named vector of triad counts
#'
#' @details
#' MAN notation describes triads by:
#' - M: number of Mutual (reciprocal) edges
#' - A: number of Asymmetric edges
#' - N: number of Null (absent) edges
#'
#' The 16 triad types are:
#' 003, 012, 102, 021D, 021U, 021C, 111D, 111U,
#' 030T, 030C, 201, 120D, 120U, 120C, 210, 300
#'
#' @examples
#' mat <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)
#' diag(mat) <- 0
#' triad_census(mat)
#'
#' @export
triad_census <- function(x, directed = TRUE) {
  if (inherits(x, "igraph")) {
    g <- x
  } else if (inherits(x, "cograph_network")) {
    g <- to_igraph(x)
  } else if (is.matrix(x)) {
    g <- igraph::graph_from_adjacency_matrix(x, mode = "directed")
  } else {
    stop("x must be a matrix, igraph object, or cograph_network")
  }

  if (!igraph::is_directed(g)) {
    stop("triad_census requires a directed network")
  }

  counts <- igraph::triad_census(g)
  names(counts) <- c("003", "012", "102", "021D", "021U", "021C",
                     "111D", "111U", "030T", "030C", "201",
                     "120D", "120U", "120C", "210", "300")
  counts
}
