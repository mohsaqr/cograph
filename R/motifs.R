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

#' Extract Triads with Node Labels
#'
#' Extract all triads from a network, preserving node labels. This allows
#' users to see which specific node combinations form each motif pattern.
#'
#' @param x A matrix, igraph object, tna, or cograph_network
#' @param type Character vector of MAN codes to filter by (e.g., "030T", "030C").
#'   Default NULL returns all types.
#' @param involving Character vector of node labels. Only return triads
#'   involving at least one of these nodes. Default NULL returns all triads.
#' @param threshold Minimum edge weight for an edge to be considered present.
#'   Type is determined by edges with weight > threshold. Default 0.
#' @param min_total Minimum total weight across all 6 edges. Excludes trivial
#'   triads with low overall activity. Default 5.
#' @param directed Logical. Treat network as directed? Default auto-detected.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{A, B, C}{Node labels for the three nodes in the triad}
#'     \item{type}{MAN code (003, 012, ..., 300)}
#'     \item{weight_AB, weight_BA, weight_AC, weight_CA, weight_BC, weight_CB}{
#'       Edge weights (frequencies) for all 6 possible directed edges}
#'     \item{total_weight}{Sum of all 6 edge weights}
#'   }
#'
#' @details
#' This function complements [motif_census()] by showing the actual node
#' combinations that form each motif pattern. A typical workflow is:
#'
#' 1. Use `motif_census()` to identify over/under-represented patterns
#' 2. Use `extract_triads()` with `type` filter to see which nodes form those patterns
#' 3. Sort by `total_weight` to find the strongest triads
#'
#' **Type vs Weight distinction:**
#' - **Type** is determined by edge presence (weight > threshold)
#' - **Weights** are the actual frequency counts, useful for ranking triads by strength
#'
#' @examples
#' # Create a frequency matrix
#' mat <- matrix(c(
#'   0, 3, 2, 0,
#'   0, 0, 5, 1,
#'   0, 0, 0, 4,
#'   2, 0, 0, 0
#' ), 4, 4, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("Plan", "Execute", "Monitor", "Adapt")
#'
#' net <- as_cograph(mat)
#'
#' # Extract all triads
#' triads <- extract_triads(net)
#' head(triads)
#'
#' # Filter by motif type (feed-forward loops only)
#' ff_loops <- extract_triads(net, type = "030T")
#'
#' # Filter by node involvement
#' plan_triads <- extract_triads(net, involving = "Plan")
#'
#' # Find strongest triads
#' triads <- extract_triads(net)
#' strongest <- triads[order(triads$total_weight, decreasing = TRUE), ]
#'
#' @seealso [motif_census()], [triad_census()]
#' @export
extract_triads <- function(x, type = NULL, involving = NULL,
                           threshold = 0, min_total = 5, directed = NULL) {
  # Convert to cograph_network
  net <- as_cograph(x, directed = directed)
  mat <- to_matrix(net)
  labels <- get_labels(net)
  n <- length(labels)

  if (n < 3) {
    return(data.frame(
      A = character(0), B = character(0), C = character(0),
      type = character(0),
      weight_AB = numeric(0), weight_BA = numeric(0),
      weight_AC = numeric(0), weight_CA = numeric(0),
      weight_BC = numeric(0), weight_CB = numeric(0),
      total_weight = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Binary adjacency (edge exists if weight > threshold)
  adj <- (mat > threshold) * 1L

  # Generate all triplet indices at once
  combos <- utils::combn(n, 3)  # 3 x C(n,3) matrix
  nc <- ncol(combos)

  i <- combos[1, ]
  j <- combos[2, ]
  k <- combos[3, ]

  # VECTORIZED: Extract all 6 edge presence values for all triplets
  e_ij <- adj[cbind(i, j)]
  e_ji <- adj[cbind(j, i)]
  e_ik <- adj[cbind(i, k)]
  e_ki <- adj[cbind(k, i)]
  e_jk <- adj[cbind(j, k)]
  e_kj <- adj[cbind(k, j)]

  # VECTORIZED: Extract actual weights
  w_ij <- mat[cbind(i, j)]
  w_ji <- mat[cbind(j, i)]
  w_ik <- mat[cbind(i, k)]
  w_ki <- mat[cbind(k, i)]
  w_jk <- mat[cbind(j, k)]
  w_kj <- mat[cbind(k, j)]

  # Compute total weights for filtering

  total_w <- w_ij + w_ji + w_ik + w_ki + w_jk + w_kj

  # VECTORIZED: Classify all triads
  triad_types <- .classify_triads_vectorized(e_ij, e_ji, e_ik, e_ki, e_jk, e_kj)

  # Identify non-empty triads (have at least one edge)
  edge_sum <- e_ij + e_ji + e_ik + e_ki + e_jk + e_kj
  has_edges <- edge_sum > 0

  # Apply filters

  keep <- has_edges & (total_w >= min_total)

  if (!is.null(type)) {
    keep <- keep & (triad_types %in% type)
  }

  if (!is.null(involving)) {
    involves_node <- (labels[i] %in% involving) |
                     (labels[j] %in% involving) |
                     (labels[k] %in% involving)
    keep <- keep & involves_node
  }

  # Build result data.frame
  data.frame(
    A = labels[i[keep]],
    B = labels[j[keep]],
    C = labels[k[keep]],
    type = triad_types[keep],
    weight_AB = w_ij[keep],
    weight_BA = w_ji[keep],
    weight_AC = w_ik[keep],
    weight_CA = w_ki[keep],
    weight_BC = w_jk[keep],
    weight_CB = w_kj[keep],
    total_weight = total_w[keep],
    stringsAsFactors = FALSE
  )
}

# Vectorized triad classification using lookup table
# @noRd
.classify_triads_vectorized <- function(e_ij, e_ji, e_ik, e_ki, e_jk, e_kj) {
  # Encode 6 edges as single integer (0-63)
  # Each combination maps to a unique code
  code <- e_ij + 2L * e_ji + 4L * e_ik + 8L * e_ki + 16L * e_jk + 32L * e_kj

  # Get precomputed lookup table

lookup <- .get_triad_lookup()

  # Return types (R is 1-indexed, so add 1 to code)
  lookup[code + 1L]
}

# Build lookup table mapping edge codes to MAN types
# Called once and cached
# @noRd
.get_triad_lookup <- function() {
  # Use memoization - compute once per session
  if (exists(".triad_lookup_cache", envir = .cograph_cache)) {
    return(get(".triad_lookup_cache", envir = .cograph_cache))
  }

  lookup <- .build_triad_lookup()
  assign(".triad_lookup_cache", lookup, envir = .cograph_cache)
  lookup
}

# Build the actual lookup table
# @noRd
.build_triad_lookup <- function() {
  # Canonical triad patterns (MAN notation) - verified against igraph
  # Each pattern is one representative adjacency matrix for that triad type
  triad_patterns <- list(
    "003" = matrix(c(0L,0L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "012" = matrix(c(0L,1L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "102" = matrix(c(0L,1L,0L, 1L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "021D" = matrix(c(0L,1L,1L, 0L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "021U" = matrix(c(0L,0L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "021C" = matrix(c(0L,0L,1L, 1L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "111D" = matrix(c(0L,1L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "111U" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "030T" = matrix(c(0L,1L,1L, 0L,0L,1L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "030C" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "201" = matrix(c(0L,1L,1L, 1L,0L,0L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "120D" = matrix(c(0L,0L,1L, 1L,0L,1L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "120U" = matrix(c(0L,1L,1L, 1L,0L,1L, 0L,0L,0L), 3, 3, byrow = TRUE),
    "120C" = matrix(c(0L,1L,0L, 1L,0L,1L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "210" = matrix(c(0L,1L,1L, 1L,0L,1L, 1L,0L,0L), 3, 3, byrow = TRUE),
    "300" = matrix(c(0L,1L,1L, 1L,0L,1L, 1L,1L,0L), 3, 3, byrow = TRUE)
  )

  # All 6 permutations of 3 nodes
  perms <- list(
    c(1, 2, 3), c(1, 3, 2), c(2, 1, 3),
    c(2, 3, 1), c(3, 1, 2), c(3, 2, 1)
  )

  lookup <- character(64)

  # For each possible 6-bit edge code (0-63)
  for (code in 0:63) {
    # Decode to edge presence
    bits <- as.integer(intToBits(code)[1:6])
    adj <- matrix(0L, 3, 3)
    adj[1, 2] <- bits[1]  # e_ij
    adj[2, 1] <- bits[2]  # e_ji
    adj[1, 3] <- bits[3]  # e_ik
    adj[3, 1] <- bits[4]  # e_ki
    adj[2, 3] <- bits[5]  # e_jk
    adj[3, 2] <- bits[6]  # e_kj

    # Match against canonical patterns (checking all permutations)
    matched <- FALSE
    for (type_name in names(triad_patterns)) {
      pat <- triad_patterns[[type_name]]
      for (p in perms) {
        if (identical(adj, pat[p, p])) {
          lookup[code + 1] <- type_name
          matched <- TRUE
          break
        }
      }
      if (matched) break
    }

    # Should never happen, but fallback
    if (!matched) lookup[code + 1] <- "003"
  }

  lookup
}

# Cache environment for memoization
.cograph_cache <- new.env(parent = emptyenv())

#' Extract Raw Edge List from TNA Model
#'
#' Extract individual-level transition counts as an edge list from a tna object.
#'
#' @param x A tna object created by [tna::tna()]
#' @param by_individual Logical. If TRUE (default), returns edge list with
#'   individual IDs. If FALSE, aggregates across all individuals.
#' @param drop_zeros Logical. If TRUE (default), excludes edges with zero count.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{id}{Individual identifier (only if `by_individual = TRUE`)}
#'     \item{from}{Source state label}
#'     \item{to}{Target state label}
#'     \item{count}{Number of transitions}
#'   }
#'
#' @examples
#' \dontrun{
#' library(tna)
#' Mod <- tna(group_regulation)
#'
#' # Get edge list by individual
#' edges <- get_edge_list(Mod)
#' head(edges)
#'
#' # Aggregate across individuals
#' agg_edges <- get_edge_list(Mod, by_individual = FALSE)
#' }
#'
#' @export
get_edge_list <- function(x, by_individual = TRUE, drop_zeros = TRUE) {
  if (!inherits(x, "tna")) {
    stop("x must be a tna object")
  }

  # Extract raw transition data
d <- x$data
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  params <- attr(x, "params")

  # Use tna's internal function to get transition counts
  model <- tna:::initialize_model(d, type, scaling, params, transitions = TRUE)
  trans <- model$trans

  labels <- x$labels
  n <- dim(trans)[1]
  s <- dim(trans)[2]

  if (by_individual) {
    # Build edge list with individual IDs
    # Pre-allocate for efficiency
    edges_list <- vector("list", n)

    for (i in seq_len(n)) {
      mat <- trans[i, , ]
      idx <- if (drop_zeros) which(mat > 0, arr.ind = TRUE) else
             expand.grid(from = seq_len(s), to = seq_len(s))

      if (nrow(idx) > 0) {
        if (drop_zeros) {
          edges_list[[i]] <- data.frame(
            id = i,
            from = labels[idx[, 1]],
            to = labels[idx[, 2]],
            count = mat[idx],
            stringsAsFactors = FALSE
          )
        } else {
          edges_list[[i]] <- data.frame(
            id = i,
            from = labels[idx$from],
            to = labels[idx$to],
            count = as.vector(mat),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    edges <- do.call(rbind, edges_list)
  } else {
    # Aggregate across individuals
    agg <- apply(trans, c(2, 3), sum)

    idx <- if (drop_zeros) which(agg > 0, arr.ind = TRUE) else
           expand.grid(from = seq_len(s), to = seq_len(s))

    if (drop_zeros) {
      edges <- data.frame(
        from = labels[idx[, 1]],
        to = labels[idx[, 2]],
        count = agg[idx],
        stringsAsFactors = FALSE
      )
    } else {
      edges <- data.frame(
        from = labels[idx$from],
        to = labels[idx$to],
        count = as.vector(agg),
        stringsAsFactors = FALSE
      )
    }

    # Sort by count descending
    edges <- edges[order(edges$count, decreasing = TRUE), ]
  }

  rownames(edges) <- NULL
  edges
}

#' Extract Motifs from Network Data
#'
#' Extract and analyze triad motifs from network data with flexible filtering,
#' pattern selection, and statistical significance testing. Supports both
#' individual-level analysis (with tna objects or grouped data) and aggregate
#' analysis (with matrices or networks).
#'
#' @param x Input data. Can be:
#'   \itemize{
#'     \item A `tna` object (supports individual-level analysis)
#'     \item A matrix (aggregate analysis only, unless `data` and `id` provided)
#'     \item A `cograph_network` object
#'     \item An `igraph` object
#'   }
#' @param data Optional data.frame containing transition data with an ID column
#'   for individual-level analysis. Required columns: `from`, `to`, and the
#'   column(s) specified in `id`. If provided, `x` should be NULL or a matrix
#'   of node labels.
#' @param id Column name(s) identifying individuals/groups in `data`. Can be
#'   a single string or character vector for multiple grouping columns.
#'   Required for individual-level analysis with non-tna inputs.
#' @param level Analysis level: "individual" counts how many people have each
#'   triad, "aggregate" analyzes the summed/single network. Default depends
#'   on input: "individual" for tna or when id provided, "aggregate" otherwise.
#' @param edge_method Method for determining edge presence:
#'   \describe{
#'     \item{"any"}{Edge exists if count > 0 (simple, recommended)}
#'     \item{"expected"}{Edge exists if observed/expected >= threshold}
#'     \item{"percent"}{Edge exists if edge/total >= threshold}
#'   }
#'   Default "any".
#' @param edge_threshold Threshold value for "expected" or "percent" methods.
#'   For "percent", a proportion (e.g., 0.15 for 15%).
#'   For "expected", a ratio (e.g., 1.5 means 50% stronger than expected).
#'   Ignored when edge_method = "any". Default 1.5.
#' @param pattern Pattern filter for which triads to include:
#'   \describe{
#'     \item{"triangle"}{All 3 node pairs must be connected (any direction).
#'       Types: 030C, 030T, 120C, 120D, 120U, 210, 300. Default.}
#'     \item{"network"}{Exclude simple sequential patterns (chains/single edges).
#'       Excludes: 003, 012, 021C. Includes stars and triangles.}
#'     \item{"closed"}{Network without chain patterns. Excludes: 003, 012, 021C, 120C.
#'       Similar to network but also removes mutual+chain (120C).}
#'     \item{"all"}{Include all 16 MAN types, no filtering.}
#'   }
#' @param exclude_types Character vector of MAN types to explicitly exclude.
#'   Applied after pattern filter. E.g., c("300") to exclude cliques.
#' @param include_types Character vector of MAN types to exclusively include.
#'   If provided, only these types are returned (overrides pattern/exclude).
#' @param top Return only the top N results (by observed count or z-score).
#'   NULL returns all results. Default NULL.
#' @param by_type If TRUE, group results by MAN type in output. Default FALSE.
#' @param min_transitions Minimum total transitions for a person to be included
#'   (individual level) or minimum triad weight (aggregate). Default 5.
#' @param significance Logical. Run permutation significance test? Default FALSE.
#' @param n_perm Number of permutations for significance test. Default 100.
#' @param seed Random seed for reproducibility.
#'
#' @return A `cograph_motif_analysis` object (list) containing:
#'   \describe{
#'     \item{results}{Data frame with triad, type, observed count, and
#'       (if significance=TRUE) expected, z-score, p-value}
#'     \item{type_summary}{Summary counts by motif type}
#'     \item{params}{List of parameters used}
#'     \item{level}{Analysis level used}
#'   }
#'
#' @section MAN Notation:
#' The 16 triad types use MAN (Mutual-Asymmetric-Null) notation where:
#' \itemize{
#'   \item First digit: number of Mutual (bidirectional) pairs
#'   \item Second digit: number of Asymmetric (one-way) pairs
#'   \item Third digit: number of Null (no edge) pairs
#'   \item Letter suffix: subtype variant (C=cycle, T=transitive, D=down, U=up)
#' }
#'
#' @section Pattern Types:
#' \describe{
#'   \item{Triangle patterns (all pairs connected):}{
#'     030C (cycle), 030T (feed-forward), 120C (regulated cycle),
#'     120D (two out-stars), 120U (two in-stars), 210 (mutual+asymmetric), 300 (clique)}
#'   \item{Network patterns (has structure):}{
#'     021D (out-star), 021U (in-star), 102 (mutual pair),
#'     111D (out-star+mutual), 111U (in-star+mutual), 201 (mutual+in-star),
#'     plus all triangle patterns}
#'   \item{Sequential patterns (chains):}{
#'     012 (single edge), 021C (A->B->C chain)}
#'   \item{Empty:}{003 (no edges)}
#' }
#'
#' @examples
#' \dontrun{
#' library(tna)
#' Mod <- tna(group_regulation)
#'
#' # Basic: triangles only (default) - individual level for tna
#' m <- extract_motifs(Mod)
#' print(m)
#'
#' # Top 20 with significance testing
#' m <- extract_motifs(Mod, top = 20, significance = TRUE, n_perm = 100)
#' plot(m)
#'
#' # From a matrix (aggregate level)
#' mat <- Mod$weights
#' m <- extract_motifs(mat)
#'
#' # From data.frame with ID column (individual level)
#' # df has columns: id, from, to (and optionally weight)
#' m <- extract_motifs(data = df, id = "id")
#'
#' # Multiple grouping columns
#' m <- extract_motifs(data = df, id = c("group", "person"))
#'
#' # Only feed-forward loops
#' m <- extract_motifs(Mod, include_types = "030T")
#'
#' # Triangles but exclude cliques
#' m <- extract_motifs(Mod, pattern = "triangle", exclude_types = "300")
#' }
#'
#' @seealso [extract_triads()], [motif_census()], [plot.cograph_motif_analysis()]
#' @export
extract_motifs <- function(x = NULL,
                           data = NULL,
                           id = NULL,
                           level = NULL,
                           edge_method = c("any", "expected", "percent"),
                           edge_threshold = 1.5,
                           pattern = c("triangle", "network", "closed", "all"),
                           exclude_types = NULL,
                           include_types = NULL,
                           top = NULL,
                           by_type = FALSE,
                           min_transitions = 5,
                           significance = FALSE,
                           n_perm = 100,
                           seed = NULL) {

  edge_method <- match.arg(edge_method)
  pattern <- match.arg(pattern)

  # Define pattern-based type filters
  # Triangle types: all 3 pairs connected (at least 1 edge each direction)
  triangle_types <- c("030C", "030T", "120C", "120D", "120U", "210", "300")

  # Network types: exclude chains/sequential (012, 021C) and empty (003)
  network_exclude <- c("003", "012", "021C")

  # Closed types: exclude all chain patterns (003, 012, 021C, 120C)
  closed_exclude <- c("003", "012", "021C", "120C")

  # Determine which types to exclude based on pattern
  if (!is.null(include_types)) {
    # include_types overrides everything - we'll filter to only these
    pattern_exclude <- character(0)
  } else if (pattern == "triangle") {
    # Only allow triangle types
    all_types <- c("003", "012", "102", "021D", "021U", "021C",
                   "111D", "111U", "030T", "030C", "201",
                   "120D", "120U", "120C", "210", "300")
    pattern_exclude <- setdiff(all_types, triangle_types)
  } else if (pattern == "network") {
    pattern_exclude <- network_exclude
  } else if (pattern == "closed") {
    # Network without chain patterns
    pattern_exclude <- closed_exclude
  } else {
    pattern_exclude <- character(0)
  }

  # Combine pattern exclusions with explicit exclude_types
  final_exclude <- unique(c(pattern_exclude, exclude_types))

  if (!is.null(seed)) set.seed(seed)


  # ==========================================================================
  # INPUT HANDLING - Support multiple input types
  # ==========================================================================

  trans <- NULL
  labels <- NULL
  has_individuals <- FALSE

  # Case 1: TNA object (has individual-level data)
  if (!is.null(x) && inherits(x, "tna")) {
    d <- x$data
    type_attr <- attr(x, "type")
    scaling <- attr(x, "scaling")
    params <- attr(x, "params")
    model <- tna:::initialize_model(d, type_attr, scaling, params, transitions = TRUE)
    trans <- model$trans
    labels <- x$labels
    has_individuals <- TRUE

  # Case 2: Data.frame with id column(s)
  } else if (!is.null(data) && !is.null(id)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (!all(id %in% names(data))) {
      stop("id column(s) not found in data: ", paste(setdiff(id, names(data)), collapse = ", "))
    }
    if (!all(c("from", "to") %in% names(data))) {
      stop("data must contain 'from' and 'to' columns")
    }

    # Create composite ID if multiple columns
    if (length(id) == 1) {
      data$.id <- data[[id]]
    } else {
      data$.id <- do.call(paste, c(data[id], sep = "_"))
    }

    # Get unique individuals and states
    unique_ids <- unique(data$.id)
    all_states <- unique(c(data$from, data$to))
    labels <- sort(all_states)
    s <- length(labels)
    n_ind <- length(unique_ids)

    # Build 3D transition array
    trans <- array(0, dim = c(n_ind, s, s))
    state_idx <- setNames(seq_along(labels), labels)

    for (i in seq_along(unique_ids)) {
      ind_data <- data[data$.id == unique_ids[i], ]
      for (r in seq_len(nrow(ind_data))) {
        from_idx <- state_idx[as.character(ind_data$from[r])]
        to_idx <- state_idx[as.character(ind_data$to[r])]
        if (!is.na(from_idx) && !is.na(to_idx)) {
          wt <- if ("weight" %in% names(ind_data)) ind_data$weight[r] else 1
          trans[i, from_idx, to_idx] <- trans[i, from_idx, to_idx] + wt
        }
      }
    }
    has_individuals <- TRUE

  # Case 3: Matrix (aggregate only)
  } else if (!is.null(x) && is.matrix(x)) {
    mat <- x
    if (is.null(rownames(mat))) {
      labels <- paste0("V", seq_len(nrow(mat)))
    } else {
      labels <- rownames(mat)
    }
    s <- nrow(mat)
    # Wrap in 3D array with 1 "individual"
    trans <- array(mat, dim = c(1, s, s))
    has_individuals <- FALSE

  # Case 4: cograph_network
  } else if (!is.null(x) && inherits(x, "cograph_network")) {
    mat <- to_matrix(x)
    labels <- get_labels(x)
    s <- nrow(mat)
    trans <- array(mat, dim = c(1, s, s))
    has_individuals <- FALSE

  # Case 5: igraph

  } else if (!is.null(x) && inherits(x, "igraph")) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("igraph package required")
    }
    # Check if graph has edge weights
    if ("weight" %in% igraph::edge_attr_names(x)) {
      mat <- as.matrix(igraph::as_adjacency_matrix(x, attr = "weight", sparse = FALSE))
    } else {
      mat <- as.matrix(igraph::as_adjacency_matrix(x, sparse = FALSE))
    }
    labels <- igraph::V(x)$name
    if (is.null(labels)) labels <- paste0("V", seq_len(nrow(mat)))
    s <- nrow(mat)
    trans <- array(mat, dim = c(1, s, s))
    has_individuals <- FALSE

  } else {
    stop("Invalid input. Provide a tna object, matrix, cograph_network, igraph, ",
         "or data.frame with 'data' and 'id' arguments.")
  }

  # Determine level
  if (is.null(level)) {
    level <- if (has_individuals) "individual" else "aggregate"
  } else {
    level <- match.arg(level, c("individual", "aggregate"))
    if (level == "individual" && !has_individuals) {
      warning("Individual level requested but no individual data available. Using aggregate.")
      level <- "aggregate"
    }
  }

  n_ind <- dim(trans)[1]
  s <- dim(trans)[2]

  # Main counting function
  count_triads_internal <- function(trans_array, edge_method, edge_threshold,
                                    min_trans, exclude, include = NULL) {
    all_results <- list()

    for (ind in seq_len(dim(trans_array)[1])) {
      mat <- trans_array[ind, , ]
      if (sum(mat) < min_trans) next

      # For expected method, compute expected matrix
      if (edge_method == "expected") {
        total_mat <- sum(mat)
        row_sums <- rowSums(mat)
        col_sums <- colSums(mat)
        expected_mat <- outer(row_sums, col_sums) / total_mat
        expected_mat[expected_mat == 0] <- 0.001
      }

      for (i in 1:(s-2)) {
        for (j in (i+1):(s-1)) {
          for (k in (j+1):s) {
            obs <- c(mat[i,j], mat[j,i], mat[i,k], mat[k,i], mat[j,k], mat[k,j])
            total <- sum(obs)
            if (total == 0) next

            if (edge_method == "any") {
              edges_strong <- as.integer(obs > 0)
            } else if (edge_method == "percent") {
              threshold <- total * edge_threshold
              edges_strong <- as.integer(obs >= threshold)
            } else {
              exp_vals <- c(expected_mat[i,j], expected_mat[j,i],
                           expected_mat[i,k], expected_mat[k,i],
                           expected_mat[j,k], expected_mat[k,j])
              ratio <- obs / exp_vals
              edges_strong <- as.integer(ratio >= edge_threshold & obs > 0)
            }

            if (sum(edges_strong) == 0) next

            code <- edges_strong[1] + 2*edges_strong[2] + 4*edges_strong[3] +
                    8*edges_strong[4] + 16*edges_strong[5] + 32*edges_strong[6]
            triad_type <- .get_triad_lookup()[code + 1]

            # Apply include filter first (if specified)
            if (!is.null(include) && !(triad_type %in% include)) next
            if (triad_type %in% exclude) next

            all_results[[length(all_results) + 1]] <- data.frame(
              person = ind,
              triad = paste(labels[i], labels[j], labels[k], sep = " - "),
              type = triad_type,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }

    if (length(all_results) == 0) return(NULL)
    do.call(rbind, all_results)
  }

  # Count observed
  observed_raw <- count_triads_internal(trans, edge_method, edge_threshold,
                                        min_transitions, final_exclude,
                                        include_types)

  if (is.null(observed_raw) || nrow(observed_raw) == 0) {
    warning("No triads found with current settings")
    return(NULL)
  }

  # Aggregate by triad
  obs_freq <- stats::aggregate(person ~ triad, data = observed_raw, FUN = length)
  names(obs_freq) <- c("triad", "observed")

  # Get dominant type for each triad
  get_dom_type <- function(tr) {
    types <- observed_raw$type[observed_raw$triad == tr]
    names(sort(table(types), decreasing = TRUE))[1]
  }
  obs_freq$type <- sapply(obs_freq$triad, get_dom_type)

  # Significance testing
  if (significance) {
    null_matrix <- matrix(0, nrow = nrow(obs_freq), ncol = n_perm)
    rownames(null_matrix) <- obs_freq$triad

    for (p in seq_len(n_perm)) {
      # Permute: randomize transitions preserving marginals approximately
      trans_perm <- array(0, dim = dim(trans))

      for (ind in seq_len(n_ind)) {
        mat <- trans[ind, , ]
        total <- sum(mat)
        if (total == 0) next

        row_sums <- rowSums(mat)
        col_sums <- colSums(mat)

        if (sum(row_sums > 0) > 0 && sum(col_sums > 0) > 0) {
          row_probs <- row_sums / sum(row_sums)
          col_probs <- col_sums / sum(col_sums)

          new_mat <- matrix(0, s, s)
          for (t in seq_len(total)) {
            ri <- sample(seq_len(s), 1, prob = row_probs)
            ci <- sample(seq_len(s), 1, prob = col_probs)
            new_mat[ri, ci] <- new_mat[ri, ci] + 1
          }
          trans_perm[ind, , ] <- new_mat
        }
      }

      perm_raw <- count_triads_internal(trans_perm, edge_method, edge_threshold,
                                        min_transitions, final_exclude,
                                        include_types)

      if (!is.null(perm_raw)) {
        perm_freq <- stats::aggregate(person ~ triad, data = perm_raw, FUN = length)
        for (tr in obs_freq$triad) {
          if (tr %in% perm_freq$triad) {
            null_matrix[tr, p] <- perm_freq$person[perm_freq$triad == tr]
          }
        }
      }
    }

    null_mean <- rowMeans(null_matrix)
    null_sd <- apply(null_matrix, 1, stats::sd)
    null_sd[null_sd == 0] <- 0.1

    obs_freq$expected <- round(null_mean, 1)
    obs_freq$z <- round((obs_freq$observed - null_mean) / null_sd, 2)
    obs_freq$p <- round(2 * stats::pnorm(-abs(obs_freq$z)), 4)
    obs_freq$sig <- ifelse(obs_freq$p < 0.001, "***",
                          ifelse(obs_freq$p < 0.01, "**",
                                ifelse(obs_freq$p < 0.05, "*", "")))
  }

  # Sort by observed (or z if significance)
  if (significance) {
    obs_freq <- obs_freq[order(obs_freq$z, decreasing = TRUE), ]
  } else {
    obs_freq <- obs_freq[order(obs_freq$observed, decreasing = TRUE), ]
  }
  rownames(obs_freq) <- NULL

  # Apply by_type grouping
  if (by_type) {
    obs_freq <- obs_freq[order(obs_freq$type, -obs_freq$observed), ]
    rownames(obs_freq) <- NULL
  }

  # Apply top N filter
  if (!is.null(top) && top > 0 && nrow(obs_freq) > top) {
    obs_freq <- utils::head(obs_freq, top)
  }

  # Type summary
  type_summary <- sort(table(observed_raw$type), decreasing = TRUE)

  result <- list(
    results = obs_freq,
    type_summary = type_summary,
    params = list(
      level = level,
      edge_method = edge_method,
      edge_threshold = edge_threshold,
      pattern = pattern,
      exclude_types = exclude_types,
      include_types = include_types,
      top = top,
      by_type = by_type,
      min_transitions = min_transitions,
      significance = significance,
      n_perm = if (significance) n_perm else NA,
      n_individuals = n_ind,
      n_states = s,
      labels = labels
    )
  )

  class(result) <- "cograph_motif_analysis"
  result
}

#' @method print cograph_motif_analysis
#' @export
print.cograph_motif_analysis <- function(x, n = 20, ...) {
  cat("Motif Analysis\n")
  cat(sprintf("Pattern: %s | Edge method: %s",
              x$params$pattern, x$params$edge_method))
  if (x$params$edge_method != "any") {
    cat(sprintf(" (threshold: %s)", x$params$edge_threshold))
  }
  cat("\n")
  cat(sprintf("Individuals: %d | States: %d | Total triads: %d\n\n",
              x$params$n_individuals, x$params$n_states, nrow(x$results)))

  cat("Type distribution:\n")
  print(x$type_summary)

  show_n <- min(n, nrow(x$results))
  cat(sprintf("\nTop %d triads:\n", show_n))

  if (x$params$significance) {
    print(utils::head(x$results[, c("triad", "type", "observed", "expected", "z", "sig")], n))
  } else {
    print(utils::head(x$results[, c("triad", "type", "observed")], n))
  }

  invisible(x)
}

#' Plot Motif Analysis Results
#'
#' Create visualizations for motif analysis results including network diagrams
#' of triads, bar plots of type distributions, and significance plots.
#'
#' @param x A `cograph_motif_analysis` object from [extract_motifs()]
#' @param type Plot type: "triads" (default network diagrams), "types" (bar plot),
#'   "significance" (z-score plot), or "patterns" (abstract MAN patterns)
#' @param n Number of triads to show. Default 20.
#' @param colors Colors for visualization. Default blue/red.
#' @param res Resolution for scaling (not used with grid graphics). Default 72.
#' @param node_size Size of nodes (1-10 scale, like splot). Default 5.
#' @param label_size Font size for node labels (3-letter abbreviations). Default 7.
#' @param title_size Font size for motif type title (e.g., "120C"). Default 7.
#' @param stats_size Font size for statistics text (n, z, p). Default 5.
#' @param ncol Number of columns in the plot grid. Default 5.
#' @param legend Logical, show abbreviation legend at bottom? Default TRUE.
#' @param color Color for nodes, edges, and labels. Default "#800020" (maroon).
#' @param spacing Spacing multiplier between cells (0.5-2). Default 1.
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns NULL for triad plots, or a ggplot2 object for other types.
#'
#' @examples
#' \dontrun{
#' library(tna)
#' Mod <- tna(group_regulation)
#' m <- extract_motifs(Mod, significance = TRUE)
#'
#' # Default network diagram
#' plot(m)
#'
#' # Customize appearance
#' plot(m, node_size = 0.15, label_size = 6, title_size = 9)
#'
#' # Change layout
#' plot(m, ncol = 4, n = 12)
#'
#' # Other plot types
#' plot(m, type = "types")
#' plot(m, type = "significance")
#' }
#'
#' @method plot cograph_motif_analysis
#' @export
plot.cograph_motif_analysis <- function(x, type = c("triads", "types", "significance", "patterns"),
                                         n = 20, colors = c("#2166AC", "#B2182B"),
                                         res = 72, node_size = 5, label_size = 7,
                                         title_size = 7, stats_size = 5, ncol = 5,
                                         legend = TRUE, color = "#800020",
                                         spacing = 1, ...) {

  type <- match.arg(type)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  if (type == "types") {
    # Bar plot of type distribution
    df <- data.frame(
      type = names(x$type_summary),
      count = as.numeric(x$type_summary)
    )
    df$type <- factor(df$type, levels = df$type[order(df$count, decreasing = TRUE)])

    p <- ggplot2::ggplot(df, ggplot2::aes(x = type, y = count)) +
      ggplot2::geom_col(fill = colors[1], width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5, size = 3) +
      ggplot2::labs(
        title = "Motif Type Distribution",
        subtitle = sprintf("Pattern: %s | Edge method: %s",
                          x$params$pattern, x$params$edge_method),
        x = "MAN Type",
        y = "Count"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(face = "bold")
      )

  } else if (type == "significance" && x$params$significance) {
    # Z-score plot
    df <- utils::head(x$results, n * 2)  # Get both tails
    df <- df[order(df$z), ]
    df <- utils::head(rbind(utils::head(df, n), utils::tail(df, n)), n * 2)
    df <- df[!duplicated(df$triad), ]

    df$direction <- ifelse(df$z > 0, "over", "under")
    df$triad <- factor(df$triad, levels = df$triad[order(df$z)])

    p <- ggplot2::ggplot(df, ggplot2::aes(x = triad, y = z, fill = direction)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_hline(yintercept = c(-2, 2), linetype = "dashed",
                          color = "#666666", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = 0, color = "#333333", linewidth = 0.3) +
      ggplot2::scale_fill_manual(
        values = c(over = colors[2], under = colors[1]),
        labels = c(over = "Over-represented", under = "Under-represented"),
        name = NULL
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Motif Significance",
        subtitle = sprintf("Permutation test (n=%d) | Dashed lines: z = +/-2",
                          x$params$n_perm),
        x = NULL,
        y = "Z-score"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        legend.position = "bottom",
        plot.title = ggplot2::element_text(face = "bold")
      )

  } else if (type == "patterns") {
    # Visual motif pattern diagrams (abstract MAN types)
    .plot_motif_patterns(x, n, colors, ...)
    return(invisible(NULL))

  } else {
    # Default: network diagrams with actual node labels
    .plot_triad_networks(x, n, colors, res = res, node_size = node_size,
                        label_size = label_size, title_size = title_size,
                        stats_size = stats_size, ncol = ncol, legend = legend,
                        color = color, spacing = spacing, ...)
    return(invisible(NULL))
  }

  print(p)
  invisible(p)
}

#' Draw arrow with closed/filled head
#' @noRd
.draw_closed_arrow <- function(x0, y0, x1, y1, col = "#800020", lwd = 2.5,
                                both = FALSE, head_length = 0.12, head_width = 0.08) {
  # Draw the line
  graphics::segments(x0, y0, x1, y1, col = col, lwd = lwd)

  # Calculate direction

  dx <- x1 - x0
  dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)
  if (len == 0) return()

  # Unit vectors
  ux <- dx / len
  uy <- dy / len

  # Perpendicular
  px <- -uy
  py <- ux

  # Draw arrow head at end (x1, y1)
  tip_x <- x1
  tip_y <- y1
  base_x <- x1 - head_length * ux
  base_y <- y1 - head_length * uy

  arrow_x <- c(tip_x, base_x + head_width * px, base_x - head_width * px)
  arrow_y <- c(tip_y, base_y + head_width * py, base_y - head_width * py)
  graphics::polygon(arrow_x, arrow_y, col = col, border = col)

  # Draw arrow head at start if mutual

  if (both) {
    tip_x <- x0
    tip_y <- y0
    base_x <- x0 + head_length * ux
    base_y <- y0 + head_length * uy

    arrow_x <- c(tip_x, base_x + head_width * px, base_x - head_width * px)
    arrow_y <- c(tip_y, base_y + head_width * py, base_y - head_width * py)
    graphics::polygon(arrow_x, arrow_y, col = col, border = col)
  }
}

#' Plot individual triads as network diagrams using grid graphics
#' @noRd
.plot_triad_networks <- function(x, n = 12, colors = c("#2166AC", "#B2182B"),
                                  res = 72, node_size = 5, label_size = 7,
                                  title_size = 7, stats_size = 5, ncol = 5,
                                  legend = TRUE, color = "#800020", spacing = 1, ...) {
  df <- utils::head(x$results, n)

  if (nrow(df) == 0) {
    message("No triads to plot")
    return(invisible(NULL))
  }

  n_plots <- nrow(df)
  n_cols <- min(ncol, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  # Triad patterns (MAN notation)
  triad_patterns <- list(
    "003" = matrix(c(0L,0L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "012" = matrix(c(0L,1L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "102" = matrix(c(0L,1L,0L, 1L,0L,0L, 0L,0L,0L), 3, 3),
    "021D" = matrix(c(0L,1L,1L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "021U" = matrix(c(0L,0L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "021C" = matrix(c(0L,1L,0L, 0L,0L,1L, 0L,0L,0L), 3, 3),
    "111D" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,0L,0L), 3, 3),
    "111U" = matrix(c(0L,1L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "030T" = matrix(c(0L,1L,1L, 0L,0L,1L, 0L,0L,0L), 3, 3),
    "030C" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,0L,0L), 3, 3),
    "201" = matrix(c(0L,1L,1L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "120D" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,1L,0L), 3, 3),
    "120U" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,1L,0L), 3, 3),
    "120C" = matrix(c(0L,1L,0L, 1L,0L,1L, 1L,0L,0L), 3, 3),
    "210" = matrix(c(0L,1L,1L, 1L,0L,1L, 0L,0L,0L), 3, 3),
    "300" = matrix(c(0L,1L,1L, 1L,0L,1L, 1L,1L,0L), 3, 3)
  )

  motif_color <- color

  # Use grid graphics with clipped viewports

  grid::grid.newpage()

  # Proportional layout that fills the device
  legend_height <- grid::unit(2, "lines")

  grid::pushViewport(grid::viewport(
    layout = grid::grid.layout(
      nrow = n_rows + 1,
      ncol = n_cols,
      heights = grid::unit.c(rep(grid::unit(1, "null"), n_rows), legend_height)
    ),
    clip = "on"
  ))

  # Triangle coordinates (0-1 normalized within each cell)
  # Adjust by spacing factor (higher spacing = more compact triangles)
  spread <- 0.32 / spacing  # How far from center
  tri_x <- c(0.5, 0.5 - spread, 0.5 + spread)
  tri_y <- c(0.5 + spread * 0.7, 0.5 - spread * 0.7, 0.5 - spread * 0.7)

  for (i in seq_len(n_plots)) {
    row <- ((i - 1) %/% n_cols) + 1
    col <- ((i - 1) %% n_cols) + 1

    triad_name <- df$triad[i]
    triad_type <- df$type[i]
    count <- df$observed[i]

    nodes <- trimws(strsplit(triad_name, " - ")[[1]])
    if (length(nodes) != 3) nodes <- c("A", "B", "C")
    nodes_short <- sapply(nodes, function(nm) substr(toupper(nm), 1, 3))

    mat <- triad_patterns[[triad_type]]
    if (is.null(mat)) mat <- matrix(0L, 3, 3)

    # Push viewport for this cell with clipping
    grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col, clip = "on"))

    # Title at top, stats at bottom
    if (x$params$significance && "z" %in% names(df)) {
      p_val <- df$p[i]
      p_str <- if (p_val < 0.001) "p<.001" else sprintf("p=%.2f", p_val)
      grid::grid.text(triad_type, x = 0.5, y = 0.94,
                     gp = grid::gpar(fontsize = title_size, fontface = "bold", col = motif_color))
      grid::grid.text(sprintf("n=%d z=%.1f %s", count, df$z[i], p_str),
                     x = 0.5, y = 0.08,
                     gp = grid::gpar(fontsize = stats_size, col = "#64748b"))
    } else {
      grid::grid.text(triad_type, x = 0.5, y = 0.94,
                     gp = grid::gpar(fontsize = title_size, fontface = "bold", col = motif_color))
      grid::grid.text(sprintf("n=%d", count), x = 0.5, y = 0.08,
                     gp = grid::gpar(fontsize = stats_size, col = "#64748b"))
    }

    # Draw edges first
    drawn_mutual <- matrix(FALSE, 3, 3)
    for (from in 1:3) {
      for (to in 1:3) {
        if (from != to && mat[from, to] == 1L) {
          is_mutual <- mat[to, from] == 1L
          if (is_mutual && drawn_mutual[from, to]) next

          x0 <- tri_x[from]; y0 <- tri_y[from]
          x1 <- tri_x[to]; y1 <- tri_y[to]

          # Shorten edges so arrows don't get buried in nodes
          dx <- x1 - x0; dy <- y1 - y0
          len <- sqrt(dx^2 + dy^2)
          shrink <- node_size * 0.025 + 0.02  # Convert to npc and add margin

          x0_adj <- x0 + shrink * dx / len
          y0_adj <- y0 + shrink * dy / len
          x1_adj <- x1 - shrink * dx / len
          y1_adj <- y1 - shrink * dy / len

          # Draw line
          grid::grid.lines(x = c(x0_adj, x1_adj), y = c(y0_adj, y1_adj),
                          gp = grid::gpar(col = motif_color, lwd = 2))

          # Arrow at end
          .grid_arrow(x1_adj, y1_adj, x0_adj, y0_adj, motif_color)

          if (is_mutual) {
            # Arrow at start too
            .grid_arrow(x0_adj, y0_adj, x1_adj, y1_adj, motif_color)
            drawn_mutual[from, to] <- TRUE
            drawn_mutual[to, from] <- TRUE
          }
        }
      }
    }

    # Draw nodes - convert node_size (1-10 scale like splot) to npc
    node_r <- grid::unit(node_size * 0.025, "npc")
    for (j in 1:3) {
      grid::grid.circle(x = tri_x[j], y = tri_y[j], r = node_r,
                       gp = grid::gpar(fill = "white", col = motif_color, lwd = 2))
      grid::grid.text(nodes_short[j], x = tri_x[j], y = tri_y[j],
                     gp = grid::gpar(fontsize = label_size, fontface = "bold", col = motif_color))
    }

    grid::popViewport()
  }

  # Legend (if enabled and nodes <= 20) - 2 columns
  if (legend) {
    all_nodes <- unique(unlist(lapply(df$triad, function(tr) {
      trimws(strsplit(tr, " - ")[[1]])
    })))

    if (length(all_nodes) <= 20 && length(all_nodes) > 0) {
      grid::pushViewport(grid::viewport(layout.pos.row = n_rows + 1, layout.pos.col = 1:n_cols))
      abbrev_map <- sapply(all_nodes, function(nm) {
        paste0(substr(toupper(nm), 1, 3), "=", nm)
      })
      abbrev_map <- sort(abbrev_map)

      # Split into 2 rows
      n_items <- length(abbrev_map)
      mid <- ceiling(n_items / 2)
      row1 <- paste(abbrev_map[1:mid], collapse = "  ")
      row2 <- if (mid < n_items) paste(abbrev_map[(mid + 1):n_items], collapse = "  ") else ""

      grid::grid.text(row1, x = 0.5, y = 0.65,
                     gp = grid::gpar(fontsize = 7, col = "#64748b"))
      if (nzchar(row2)) {
        grid::grid.text(row2, x = 0.5, y = 0.35,
                       gp = grid::gpar(fontsize = 7, col = "#64748b"))
      }
      grid::popViewport()
    }
  }

  grid::popViewport()  # layout viewport
  invisible(NULL)
}

#' Draw arrow head using grid
#' @noRd
.grid_arrow <- function(tip_x, tip_y, base_x, base_y, col) {
  dx <- tip_x - base_x
  dy <- tip_y - base_y
  len <- sqrt(dx^2 + dy^2)
  if (len == 0) return()

  # Unit vectors
  ux <- dx / len
  uy <- dy / len

  # Arrow head size
  head_len <- 0.04
  head_wid <- 0.025

  # Arrow points
  ax <- tip_x - head_len * ux
  ay <- tip_y - head_len * uy

  # Perpendicular
  px <- -uy
  py <- ux

  arrow_x <- c(tip_x, ax + head_wid * px, ax - head_wid * px)
  arrow_y <- c(tip_y, ay + head_wid * py, ay - head_wid * py)

  grid::grid.polygon(x = arrow_x, y = arrow_y,
                    gp = grid::gpar(fill = col, col = col))
}

#' @noRd
.plot_motif_patterns <- function(x, n = 12, colors = c("#2166AC", "#B2182B"), ...) {
  # Get type counts
  type_counts <- x$type_summary
  type_counts <- type_counts[type_counts > 0]
  type_counts <- sort(type_counts, decreasing = TRUE)

  # Limit to top n types
  if (length(type_counts) > n) {
    type_counts <- type_counts[seq_len(n)]
  }

  # Triad patterns (MAN notation) - adjacency matrices
  triad_patterns <- list(
    "003" = matrix(c(0L,0L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "012" = matrix(c(0L,1L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "102" = matrix(c(0L,1L,0L, 1L,0L,0L, 0L,0L,0L), 3, 3),
    "021D" = matrix(c(0L,1L,1L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "021U" = matrix(c(0L,0L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "021C" = matrix(c(0L,1L,0L, 0L,0L,1L, 0L,0L,0L), 3, 3),
    "111D" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,0L,0L), 3, 3),
    "111U" = matrix(c(0L,1L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "030T" = matrix(c(0L,1L,1L, 0L,0L,1L, 0L,0L,0L), 3, 3),
    "030C" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,0L,0L), 3, 3),
    "201" = matrix(c(0L,1L,1L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "120D" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,1L,0L), 3, 3),
    "120U" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,1L,0L), 3, 3),
    "120C" = matrix(c(0L,1L,0L, 1L,0L,1L, 1L,0L,0L), 3, 3),
    "210" = matrix(c(0L,1L,1L, 1L,0L,1L, 0L,0L,0L), 3, 3),
    "300" = matrix(c(0L,1L,1L, 1L,0L,1L, 1L,1L,0L), 3, 3)
  )

  # MAN type descriptions
  type_desc <- c(
    "003" = "Empty",
    "012" = "Single edge",
    "102" = "Mutual pair",
    "021D" = "Out-star",
    "021U" = "In-star",
    "021C" = "Chain",
    "111D" = "Out-star + mutual",
    "111U" = "In-star + mutual",
    "030T" = "Feed-forward",
    "030C" = "Cycle",
    "201" = "Mutual + in-star",
    "120D" = "Two out-stars",
    "120U" = "Two in-stars",
    "120C" = "Mixed regulated",
    "210" = "Mutual + feed-forward",
    "300" = "Clique"
  )

  motifs_to_plot <- names(type_counts)
  n_plots <- length(motifs_to_plot)

  if (n_plots == 0) {
    message("No motif types to plot")
    return(invisible(NULL))
  }

  # Calculate grid dimensions
  n_cols <- min(4, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  # Set up plot
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  graphics::par(mfrow = c(n_rows, n_cols), mar = c(1, 1, 4, 1), bg = "white")

  # Node positions (triangle layout)
  coords <- matrix(c(
    0, 1,       # A (top)
    -0.866, -0.5,  # B (bottom-left)
    0.866, -0.5    # C (bottom-right)
  ), ncol = 2, byrow = TRUE)

  for (motif_name in motifs_to_plot) {
    count <- type_counts[motif_name]
    mat <- triad_patterns[[motif_name]]
    desc <- type_desc[motif_name]

    # Set up plot area
    graphics::plot(NULL, xlim = c(-1.5, 1.5), ylim = c(-1.2, 1.5),
                   asp = 1, axes = FALSE, xlab = "", ylab = "")

    # Draw edges (arrows)
    edge_col <- "#444444"
    for (i in 1:3) {
      for (j in 1:3) {
        if (i != j && mat[i, j] == 1L) {
          # Check if mutual
          is_mutual <- mat[j, i] == 1L

          x0 <- coords[i, 1]
          y0 <- coords[i, 2]
          x1 <- coords[j, 1]
          y1 <- coords[j, 2]

          # Shorten arrows to not overlap nodes
          dx <- x1 - x0
          dy <- y1 - y0
          len <- sqrt(dx^2 + dy^2)
          shrink <- 0.25 / len

          x0_adj <- x0 + dx * shrink
          y0_adj <- y0 + dy * shrink
          x1_adj <- x1 - dx * shrink
          y1_adj <- y1 - dy * shrink

          # Offset for mutual edges
          if (is_mutual && i < j) {
            offset <- 0.08
            perp_x <- -dy / len * offset
            perp_y <- dx / len * offset
            graphics::arrows(x0_adj + perp_x, y0_adj + perp_y,
                           x1_adj + perp_x, y1_adj + perp_y,
                           length = 0.12, lwd = 2.5, col = edge_col)
          } else if (is_mutual && i > j) {
            offset <- 0.08
            perp_x <- -dy / len * offset
            perp_y <- dx / len * offset
            graphics::arrows(x0_adj - perp_x, y0_adj - perp_y,
                           x1_adj - perp_x, y1_adj - perp_y,
                           length = 0.12, lwd = 2.5, col = edge_col)
          } else {
            graphics::arrows(x0_adj, y0_adj, x1_adj, y1_adj,
                           length = 0.12, lwd = 2.5, col = edge_col)
          }
        }
      }
    }

    # Draw nodes
    node_col <- colors[1]
    graphics::points(coords[, 1], coords[, 2], pch = 21, cex = 4,
                    bg = node_col, col = "white", lwd = 2)

    # Node labels
    graphics::text(coords[, 1], coords[, 2], c("A", "B", "C"),
                  col = "white", font = 2, cex = 1.1)

    # Title with count
    graphics::title(main = sprintf("%s: %s\nn = %s",
                                   motif_name, desc,
                                   format(count, big.mark = ",")),
                   cex.main = 1.1, line = 1)
  }

  invisible(NULL)
}


# =============================================================================
# TEMPORAL MOTIF ANALYSIS
# =============================================================================

#' Extract Motifs Across Time Windows
#'
#' Analyze how network motifs evolve over temporal sequences by extracting
#' and counting triad patterns within rolling time windows.
#'
#' @param x Input data. Can be:
#'   \itemize{
#'     \item A wide data.frame (rows=individuals, columns=time points)
#'     \item An edge list data.frame (with from, to, time columns)
#'     \item A long format data.frame (with id, time, state columns)
#'     \item A `tna_windows` result from [tna_windows()]
#'   }
#' @param id Column name(s) for grouping (character). For wide format, separates
#'   ID columns from time columns. For edge_list/long format, identifies individuals.
#' @param time Column name for time (edge_list/long format). Default "time".
#' @param from Column name for source node (edge_list format). Default "from".
#' @param to Column name for target node (edge_list format). Default "to".
#' @param state Column name for state (long format). Default "state".
#' @param format Input format: "auto" (detect), "wide", "edge_list", or "long".
#' @param window_size Number of time points per window. Default depends on format:
#'   1 for edge_list, 2 for wide/long.
#' @param step Window step size. 1 = sliding (default), window_size = tumbling.
#' @param pattern Pattern filter: "triangle", "network", "closed", or "all".
#'   See [extract_motifs()] for details.
#' @param edge_method Method for edge presence: "any", "expected", or "percent".
#' @param edge_threshold Threshold for expected/percent methods. Default 1.5.
#' @param min_transitions Minimum transitions per individual/window. Default 5.
#' @param exclude_types Character vector of MAN types to exclude.
#' @param include_types Character vector of MAN types to exclusively include.
#' @param na_threshold Maximum NA proportion before stopping. Default 0.5.
#' @param seed Random seed for reproducibility.
#'
#' @return A `cograph_temporal_motifs` object containing:
#'   \describe{
#'     \item{windows}{List of per-window motif results}
#'     \item{summary}{Data frame with window, start, end, type, count}
#'     \item{type_trends}{Data frame of type counts over time (wide format)}
#'     \item{params}{Parameters used}
#'   }
#'
#' @examples
#' \dontrun{
#' library(tna)
#'
#' # Wide format (default)
#' data <- group_regulation
#' m <- extract_motifs_temporal(data, window_size = 5, step = 1)
#' print(m)
#' plot(m, type = "trends")
#' plot(m, type = "heatmap")
#'
#' # With ID columns
#' data_wide <- data.frame(
#'   person = 1:100,
#'   group = rep(c("A", "B"), 50),
#'   T1 = sample(c("Plan", "Execute", "Review"), 100, replace = TRUE),
#'   T2 = sample(c("Plan", "Execute", "Review"), 100, replace = TRUE),
#'   T3 = sample(c("Plan", "Execute", "Review"), 100, replace = TRUE)
#' )
#' m <- extract_motifs_temporal(data_wide, id = c("person", "group"), window_size = 2)
#'
#' # Edge list format
#' edges <- data.frame(
#'   from = c("A", "B", "A", "C", "B", "A"),
#'   to = c("B", "C", "C", "A", "A", "B"),
#'   time = c(1, 1, 2, 2, 3, 3)
#' )
#' m <- extract_motifs_temporal(edges, from = "from", to = "to", time = "time",
#'                               window_size = 2)
#'
#' # Long format
#' long_data <- data.frame(
#'   person = rep(1:50, each = 10),
#'   timepoint = rep(1:10, 50),
#'   state = sample(c("Plan", "Execute", "Monitor"), 500, replace = TRUE)
#' )
#' m <- extract_motifs_temporal(long_data, id = "person", time = "timepoint",
#'                               state = "state", window_size = 3)
#'
#' # Pre-computed windows
#' windows <- tna_windows(data, window_size = 3)
#' m <- extract_motifs_temporal(windows)
#' }
#'
#' @seealso [extract_motifs()], [tna_windows()], [plot.cograph_temporal_motifs()]
#' @export
extract_motifs_temporal <- function(x,
                                     id = NULL,
                                     time = NULL,
                                     from = NULL,
                                     to = NULL,
                                     state = NULL,
                                     format = c("auto", "wide", "edge_list", "long"),
                                     window_size = NULL,
                                     step = 1,
                                     pattern = c("triangle", "network", "closed", "all"),
                                     edge_method = c("any", "expected", "percent"),
                                     edge_threshold = 1.5,
                                     min_transitions = 5,
                                     exclude_types = NULL,
                                     include_types = NULL,
                                     na_threshold = 0.5,
                                     seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  format <- match.arg(format)
  pattern <- match.arg(pattern)
  edge_method <- match.arg(edge_method)

  # Auto-detect format first (needed to set window_size default)
  if (format == "auto" && is.data.frame(x)) {
    if (!is.null(from) && !is.null(to)) {
      format <- "edge_list"
    } else if (!is.null(state)) {
      format <- "long"
    } else {
      format <- "wide"
    }
  }

  # Set default window_size based on format
  if (is.null(window_size)) {
    window_size <- if (format == "edge_list") 1L else 2L
  }

  # ========== FORMAT DETECTION & CONVERSION ==========

  # Case 1: Already tna_windows result
  if (inherits(x, "tna_windows") ||
      (is.list(x) && "windows" %in% names(x) && "start_times" %in% names(x))) {
    win_result <- x

  } else if (is.data.frame(x)) {

    # Case 2: Edge list format (from, to, time, weight)
    if (format == "edge_list") {
      if (is.null(from)) from <- "from"
      if (is.null(to)) to <- "to"
      if (is.null(time)) time <- "time"

      # Validate columns
      required <- c(from, to, time)
      if (!all(required %in% names(x))) {
        stop("Missing columns: ", paste(setdiff(required, names(x)), collapse = ", "))
      }

      # Convert edge list to per-window matrices
      win_result <- .edge_list_to_windows(x, from = from, to = to, time = time,
                                           id = id, window_size = window_size,
                                           step = step)

    # Case 3: Long format (id, time, state)
    } else if (format == "long") {
      if (is.null(id)) stop("id required for long format")
      if (is.null(time)) time <- "time"
      if (is.null(state)) state <- "state"

      # Validate columns
      required <- c(id, time, state)
      if (!all(required %in% names(x))) {
        stop("Missing columns: ", paste(setdiff(required, names(x)), collapse = ", "))
      }

      # Convert long to wide, then to windows
      wide_data <- .long_to_wide(x, id = id, time = time, state = state)
      win_result <- tna_windows(wide_data, window_size = window_size,
                                 step = step, na_threshold = na_threshold)

    # Case 4: Wide format (T1, T2, T3...)
    } else {
      # Handle ID columns if provided
      if (!is.null(id)) {
        if (!all(id %in% names(x))) {
          stop("id column(s) not found: ", paste(setdiff(id, names(x)), collapse = ", "))
        }
        time_cols <- setdiff(names(x), id)
        x_time <- x[, time_cols, drop = FALSE]
      } else {
        x_time <- x
      }

      win_result <- tna_windows(x_time, window_size = window_size,
                                 step = step, na_threshold = na_threshold)
    }

  } else {
    stop("x must be data.frame or tna_windows result")
  }

  n_windows <- length(win_result$windows)
  if (n_windows == 0) {
    warning("No valid windows generated")
    return(NULL)
  }

  # Extract motifs for each window
  window_results <- vector("list", n_windows)
  summary_list <- list()

  for (i in seq_len(n_windows)) {
    tna_model <- win_result$windows[[i]]

    # Get weights matrix - handle both tna objects and simple lists
    weights_mat <- if (inherits(tna_model, "tna")) {
      tna_model$weights
    } else if (is.list(tna_model) && "weights" %in% names(tna_model)) {
      tna_model$weights
    } else {
      next
    }

    # Use aggregate level since we have per-window matrices
    motifs <- tryCatch({
      extract_motifs(
        weights_mat,
        level = "aggregate",
        pattern = pattern,
        edge_method = edge_method,
        edge_threshold = edge_threshold,
        min_transitions = min_transitions,
        exclude_types = exclude_types,
        include_types = include_types,
        significance = FALSE
      )
    }, error = function(e) NULL)

    # Store motifs along with raw transition counts for edge-based counting
    if (!is.null(motifs)) {
      motifs$weights_matrix <- weights_mat

      # Compute raw transition counts from sequence data if available
      if (inherits(tna_model, "tna") && !is.null(tna_model$data)) {
        seq_data <- tna_model$data
        states <- rownames(weights_mat)
        n_states <- length(states)
        count_mat <- matrix(0L, nrow = n_states, ncol = n_states,
                            dimnames = list(states, states))

        for (row_idx in seq_len(nrow(seq_data))) {
          row <- seq_data[row_idx, ]
          for (col_idx in seq_len(length(row) - 1)) {
            from_idx <- row[col_idx]
            to_idx <- row[col_idx + 1]
            # Data contains integer indices, convert to state names
            if (!is.na(from_idx) && !is.na(to_idx) &&
                from_idx >= 1 && from_idx <= n_states &&
                to_idx >= 1 && to_idx <= n_states) {
              count_mat[from_idx, to_idx] <- count_mat[from_idx, to_idx] + 1L
            }
          }
        }
        motifs$counts_matrix <- count_mat
      }
    }
    window_results[[i]] <- motifs

    # Build summary
    if (!is.null(motifs) && nrow(motifs$results) > 0) {
      type_counts <- table(motifs$results$type)
      summary_list[[i]] <- data.frame(
        window = i,
        start = win_result$start_times[i],
        end = win_result$end_times[i],
        type = names(type_counts),
        count = as.integer(type_counts),
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine summary
  summary_df <- if (length(summary_list) > 0) {
    do.call(rbind, summary_list)
  } else {
    data.frame(window = integer(0), start = integer(0), end = integer(0),
               type = character(0), count = integer(0))
  }

  # Create type trends (pivot to wide format)
  if (nrow(summary_df) > 0) {
    all_types <- unique(summary_df$type)
    type_trends <- data.frame(
      window = seq_len(n_windows),
      start = win_result$start_times,
      end = win_result$end_times
    )
    for (tp in all_types) {
      counts <- sapply(seq_len(n_windows), function(w) {
        subset_df <- summary_df[summary_df$window == w & summary_df$type == tp, ]
        if (nrow(subset_df) > 0) sum(subset_df$count) else 0L
      })
      type_trends[[tp]] <- counts
    }
  } else {
    type_trends <- data.frame(window = seq_len(n_windows),
                              start = win_result$start_times,
                              end = win_result$end_times)
  }

  result <- list(
    windows = window_results,
    summary = summary_df,
    type_trends = type_trends,
    params = list(
      window_size = window_size,
      step = step,
      pattern = pattern,
      edge_method = edge_method,
      edge_threshold = edge_threshold,
      min_transitions = min_transitions,
      n_windows = n_windows,
      na_proportions = win_result$na_proportions
    )
  )

  class(result) <- "cograph_temporal_motifs"
  result
}


#' Convert edge list with time to windowed transition matrices
#' @noRd
.edge_list_to_windows <- function(x, from, to, time, id = NULL,
                                   window_size, step) {
  # Get unique time points and states
  times <- sort(unique(x[[time]]))
  states <- sort(unique(c(x[[from]], x[[to]])))
  n_states <- length(states)
  state_idx <- stats::setNames(seq_along(states), states)

  # Generate window start points
  n_times <- length(times)
  if (n_times < window_size) {
    return(list(windows = list(), start_times = integer(0),
                end_times = integer(0), na_proportions = numeric(0)))
  }

  starts <- seq(1, n_times - window_size + 1, by = step)
  n_windows <- length(starts)

  windows <- vector("list", n_windows)

  for (w in seq_len(n_windows)) {
    # Get time range for this window
    window_times <- times[starts[w]:(starts[w] + window_size - 1)]

    # Filter edges in this time window
    edges_w <- x[x[[time]] %in% window_times, ]

    # Build transition matrix
    mat <- matrix(0, n_states, n_states, dimnames = list(states, states))
    for (r in seq_len(nrow(edges_w))) {
      f <- as.character(edges_w[[from]][r])
      t <- as.character(edges_w[[to]][r])
      wt <- if ("weight" %in% names(edges_w)) edges_w$weight[r] else 1
      mat[f, t] <- mat[f, t] + wt
    }

    # Create minimal tna-like structure
    windows[[w]] <- list(weights = mat, labels = states)
  }

  list(
    windows = windows,
    start_times = starts,
    end_times = starts + window_size - 1,
    na_proportions = rep(0, n_windows)
  )
}


#' Convert long format (id, time, state) to wide format for tna_windows
#' @noRd
.long_to_wide <- function(x, id, time, state) {
  # Handle multiple ID columns
  if (length(id) == 1) {
    x$.id <- x[[id]]
  } else {
    x$.id <- do.call(paste, c(x[id], sep = "_"))
  }

  unique_ids <- unique(x$.id)
  unique_times <- sort(unique(x[[time]]))

  # Create wide data frame
  wide <- data.frame(row.names = seq_along(unique_ids))

  for (t in unique_times) {
    col_name <- paste0("T", t)
    wide[[col_name]] <- sapply(unique_ids, function(uid) {
      val <- x[[state]][x$.id == uid & x[[time]] == t]
      if (length(val) == 0) NA_character_ else as.character(val[1])
    })
  }

  wide
}


#' @method print cograph_temporal_motifs
#' @export
print.cograph_temporal_motifs <- function(x, ...) {
  cat("Temporal Motif Analysis\n")
  cat(sprintf("Windows: %d | Pattern: %s\n",
              x$params$n_windows, x$params$pattern))
  cat(sprintf("Window size: %d | Step: %d\n\n",
              x$params$window_size, x$params$step))

  if (nrow(x$summary) > 0) {
    # Aggregate by type
    agg <- stats::aggregate(count ~ type, data = x$summary, FUN = sum)
    agg <- agg[order(agg$count, decreasing = TRUE), ]
    cat("Total occurrences by type:\n")
    print(agg, row.names = FALSE)
  } else {
    cat("No motifs found.\n")
  }

  invisible(x)
}


#' Plot Temporal Motif Analysis Results
#'
#' Create visualizations for temporal motif analysis including trend lines
#' and heatmaps showing how motif frequencies change over time windows.
#'
#' @param x A `cograph_temporal_motifs` object from [extract_motifs_temporal()]
#' @param type Plot type: "trends" (line plot, default) or "heatmap"
#' @param top_n Show only top N types by total count. Default 10.
#' @param colors Optional color palette for types.
#' @param ... Additional arguments (unused)
#'
#' @return A ggplot2 object (invisibly)
#'
#' @examples
#' \dontrun{
#' m <- extract_motifs_temporal(data, window_size = 5)
#' plot(m, type = "trends")
#' plot(m, type = "heatmap")
#' }
#'
#' @method plot cograph_temporal_motifs
#' @export
plot.cograph_temporal_motifs <- function(x, type = c("trends", "heatmap"),
                                          top_n = 10, colors = NULL, ...) {
  type <- match.arg(type)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }

  if (type == "trends") {
    # Line plot of type counts over time
    trends <- x$type_trends
    type_cols <- setdiff(names(trends), c("window", "start", "end"))

    if (length(type_cols) == 0) {
      message("No types to plot")
      return(invisible(NULL))
    }

    # Get top N types by total count
    totals <- colSums(trends[, type_cols, drop = FALSE])
    top_types <- names(sort(totals, decreasing = TRUE))[1:min(top_n, length(totals))]

    # Reshape to long format
    df_long <- data.frame()
    for (tp in top_types) {
      df_long <- rbind(df_long, data.frame(
        window = trends$window,
        type = tp,
        count = trends[[tp]],
        stringsAsFactors = FALSE
      ))
    }

    # Create time range labels
    df_long$time_range <- paste0("T", trends$start[df_long$window], "-T",
                                  trends$end[df_long$window])

    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = window, y = count,
                                                color = type, group = type)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_x_continuous(breaks = unique(df_long$window),
                                  labels = unique(df_long$time_range)) +
      ggplot2::labs(
        title = "Motif Trends Over Time",
        subtitle = sprintf("Window size: %d | Pattern: %s",
                          x$params$window_size, x$params$pattern),
        x = "Time Window",
        y = "Count",
        color = "Type"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(face = "bold"),
        legend.position = "right"
      )

    print(p)
    invisible(p)

  } else if (type == "heatmap") {
    # Heatmap of type x window
    if (nrow(x$summary) == 0) {
      message("No data for heatmap")
      return(invisible(NULL))
    }

    df <- x$summary
    df$time_range <- paste0("T", df$start, "-T", df$end)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(window), y = type, fill = count)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = count), size = 3) +
      ggplot2::scale_fill_gradient(low = "white", high = "#800020",
                                   name = "Count") +
      ggplot2::scale_x_discrete(labels = unique(df$time_range)) +
      ggplot2::labs(
        title = "Motif Heatmap by Time Window",
        x = "Time Window",
        y = "Motif Type"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(face = "bold")
      )

    print(p)
    invisible(p)
  }
}


#' Analyze Triad Persistence Across Time Windows
#'
#' Identify which specific labeled triads or MAN types persist, emerge, or fade across
#' temporal windows in a motif analysis.
#'
#' @param x A `cograph_temporal_motifs` object from [extract_motifs_temporal()]
#' @param by Aggregation level: `"triad"` (default) tracks specific labeled triads
#'   (e.g., "Plan - Execute - Monitor"), `"type"` aggregates by MAN type (e.g., "021C").
#'   Use `"type"` to see how many triads of each structural pattern exist per window.
#' @param edge_weight Logical. If `TRUE`, weight counts by the number of edges in the triad.
#'   A full bidirectional clique (6 edges) weights 2x, while a minimal triad (3 edges) weights 1x.
#'   Formula: `count * (n_edges / 3)`. Default `FALSE` for raw counts.
#' @param min_windows Minimum number of windows a triad/type must appear in to be
#'   included in the results. Default 1 (include all).
#' @param min_persistence Minimum persistence score (0-1) to be classified as
#'   "persistent". Default 0.5 (appears in at least half the windows).
#'
#' @return A `cograph_triad_persistence` object containing:
#'   \describe{
#'     \item{triads}{Data frame with columns: triad/type label, type (MAN type),
#'       n_windows (appearances), first_window, last_window, windows (comma-separated),
#'       persistence (proportion), total_count, status}
#'     \item{counts_matrix}{Numeric matrix (rows x windows) with observed counts per cell.
#'       When `by = "type"`, counts show how many triads of that type exist per window.}
#'     \item{presence_matrix}{Binary matrix showing presence/absence}
#'     \item{window_totals}{Total number of triads observed per window (for normalization)}
#'     \item{summary}{List with counts by status}
#'     \item{params}{List of parameters used, including `by`}
#'   }
#'
#' @section Status Classification:
#' \describe{
#'   \item{persistent}{Appears in at least `min_persistence` proportion of windows}
#'   \item{transient}{Appears in only 1 window}
#'   \item{emerging}{First appears after window 1 AND last appears in final window}
#'   \item{fading}{First appears in window 1 AND disappears before final window}
#'   \item{sporadic}{Other patterns (gaps in appearance)}
#' }
#'
#' @examples
#' \dontrun{
#' library(tna)
#' data <- group_regulation
#'
#' # Extract temporal motifs
#' m <- extract_motifs_temporal(data, window_size = 5, step = 2, pattern = "all")
#'
#' # By specific triads (default) - each triad can only appear 0 or 1 time
#' pers <- triad_persistence(m)
#' print(pers)
#' plot(pers, type = "heatmap", top_n = 20)
#'
#' # By MAN type - see how many triads of each structural pattern per window
#' pers_type <- triad_persistence(m, by = "type")
#' print(pers_type)
#' plot(pers_type, type = "heatmap")               # Raw counts
#' plot(pers_type, type = "heatmap", normalize = TRUE)  # As % of window total
#'
#' # Weight by edge density (full clique = 2x, minimal triad = 1x)
#' pers_weighted <- triad_persistence(m, by = "triad", edge_weight = TRUE)
#' plot(pers_weighted, type = "heatmap", top_n = 20)
#'
#' # Access the data
#' pers_type$counts_matrix    # Counts per cell
#' pers_type$window_totals    # Total triads per window (for normalization)
#'
#' # Filter by status
#' persistent_types <- pers_type$triads[pers_type$triads$status == "persistent", ]
#' }
#'
#' @seealso [extract_motifs_temporal()], [plot.cograph_triad_persistence()]
#' @export
triad_persistence <- function(x, by = c("triad", "type"),
                              edge_weight = FALSE,
                              min_windows = 1, min_persistence = 0.5) {

  if (!inherits(x, "cograph_temporal_motifs")) {
    stop("x must be a cograph_temporal_motifs object from extract_motifs_temporal()")
  }

  by <- match.arg(by)
  n_windows <- x$params$n_windows

  # Collect all triads from all windows with their details
  all_triads_list <- list()

  for (w in seq_len(n_windows)) {
    motifs <- x$windows[[w]]
    if (is.null(motifs) || is.null(motifs$results) || nrow(motifs$results) == 0) next

    df <- motifs$results
    df$window <- w

    # Compute actual transition counts from raw counts matrix
    if (!is.null(motifs$counts_matrix)) {
      mat <- motifs$counts_matrix
      df$edge_count <- sapply(seq_len(nrow(df)), function(i) {
        nodes <- strsplit(df$triad[i], " - ")[[1]]
        if (length(nodes) == 3 && all(nodes %in% rownames(mat))) {
          # Get all edge counts between the 3 nodes (both directions)
          edges <- c(
            mat[nodes[1], nodes[2]], mat[nodes[2], nodes[1]],
            mat[nodes[1], nodes[3]], mat[nodes[3], nodes[1]],
            mat[nodes[2], nodes[3]], mat[nodes[3], nodes[2]]
          )
          # Minimum non-zero edge count = max times full triad could occur
          nonzero <- edges[edges > 0]
          base_count <- if (length(nonzero) > 0) min(nonzero) else 0L

          # Apply edge weight multiplier if requested
          if (edge_weight && base_count > 0) {
            n_edges <- sum(edges > 0)  # Number of non-zero edges (0-6)
            # Weight by edge density: 3 edges = 1x, 6 edges = 2x
            round(base_count * (n_edges / 3), 2)
          } else {
            base_count
          }
        } else {
          1L  # Fallback
        }
      })
    } else if (!is.null(motifs$weights_matrix)) {
      # Fallback to weights if no raw counts available
      mat <- motifs$weights_matrix
      df$edge_count <- sapply(seq_len(nrow(df)), function(i) {
        nodes <- strsplit(df$triad[i], " - ")[[1]]
        if (length(nodes) == 3 && all(nodes %in% rownames(mat))) {
          edges <- c(
            mat[nodes[1], nodes[2]], mat[nodes[2], nodes[1]],
            mat[nodes[1], nodes[3]], mat[nodes[3], nodes[1]],
            mat[nodes[2], nodes[3]], mat[nodes[3], nodes[2]]
          )
          base_count <- round(sum(edges) * 100)

          if (edge_weight && base_count > 0) {
            n_edges <- sum(edges > 0)
            round(base_count * (n_edges / 3), 2)
          } else {
            base_count
          }
        } else {
          1L
        }
      })
    } else {
      df$edge_count <- df$observed
    }

    all_triads_list[[length(all_triads_list) + 1]] <- df[, c("triad", "type", "edge_count", "window")]
  }

  if (length(all_triads_list) == 0) {
    warning("No triads found in any window")
    return(NULL)
  }

  all_triads <- do.call(rbind, all_triads_list)

  # Calculate window totals (for normalization)
  window_totals <- sapply(seq_len(n_windows), function(w) {
    sum(all_triads$edge_count[all_triads$window == w])
  })
  names(window_totals) <- paste0("W", seq_len(n_windows))

  # Determine grouping key based on 'by' parameter
  if (by == "type") {
    # Aggregate by MAN type - sum counts across all triads of same type per window
    unique_items <- sort(unique(all_triads$type))
    group_col <- "type"
  } else {
    # Track individual labeled triads
    unique_items <- unique(all_triads$triad)
    group_col <- "triad"
  }

  # Build counts matrix (actual counts per cell)
  counts_matrix <- matrix(0L, nrow = length(unique_items), ncol = n_windows,
                          dimnames = list(unique_items, paste0("W", seq_len(n_windows))))

  # Initialize results data frame
  results <- data.frame(
    label = unique_items,
    type = character(length(unique_items)),
    n_windows = integer(length(unique_items)),
    first_window = integer(length(unique_items)),
    last_window = integer(length(unique_items)),
    windows = character(length(unique_items)),
    persistence = numeric(length(unique_items)),
    total_count = integer(length(unique_items)),
    status = character(length(unique_items)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(unique_items)) {
    item <- unique_items[i]

    # Subset based on grouping column
    if (by == "type") {
      subset_df <- all_triads[all_triads$type == item, ]
    } else {
      subset_df <- all_triads[all_triads$triad == item, ]
    }

    # Store actual counts per window (sum of edge_count for this item)
    for (w in unique(subset_df$window)) {
      counts_matrix[i, w] <- sum(subset_df$edge_count[subset_df$window == w])
    }

    windows_present <- sort(unique(subset_df$window))

    # Type: for "triad" mode, get dominant type; for "type" mode, it's the item itself
    if (by == "type") {
      results$type[i] <- item
    } else {
      type_counts <- table(subset_df$type)
      results$type[i] <- names(type_counts)[which.max(type_counts)]
    }

    results$n_windows[i] <- length(windows_present)
    results$first_window[i] <- min(windows_present)
    results$last_window[i] <- max(windows_present)
    results$windows[i] <- paste(windows_present, collapse = ",")
    results$persistence[i] <- length(windows_present) / n_windows
    results$total_count[i] <- sum(subset_df$edge_count)

    # Classify status
    pers <- results$persistence[i]
    first <- results$first_window[i]
    last <- results$last_window[i]

    if (pers >= min_persistence) {
      results$status[i] <- "persistent"
    } else if (length(windows_present) == 1) {
      results$status[i] <- "transient"
    } else if (first > 1 && last == n_windows) {
      results$status[i] <- "emerging"
    } else if (first == 1 && last < n_windows) {
      results$status[i] <- "fading"
    } else {
      results$status[i] <- "sporadic"
    }
  }

  # Filter by min_windows
  results <- results[results$n_windows >= min_windows, ]
  counts_matrix <- counts_matrix[results$label, , drop = FALSE]

  # Derive presence matrix from counts
  presence_matrix <- (counts_matrix > 0L) * 1L

  # Sort by persistence descending, then by total_count

  results <- results[order(-results$persistence, -results$total_count), ]
  rownames(results) <- NULL

  # Summary stats
  summary_stats <- list(
    n_items = nrow(results),
    n_persistent = sum(results$status == "persistent"),
    n_transient = sum(results$status == "transient"),
    n_emerging = sum(results$status == "emerging"),
    n_fading = sum(results$status == "fading"),
    n_sporadic = sum(results$status == "sporadic")
  )

  # For backwards compatibility, also add 'triad' column as alias for 'label'
  results$triad <- results$label

  out <- list(
    triads = results,
    counts_matrix = counts_matrix,
    presence_matrix = presence_matrix,
    window_totals = window_totals,
    summary = summary_stats,
    params = list(
      by = by,
      edge_weight = edge_weight,
      n_windows = n_windows,
      min_windows = min_windows,
      min_persistence = min_persistence
    )
  )

  class(out) <- "cograph_triad_persistence"
  out
}


#' @method print cograph_triad_persistence
#' @export
print.cograph_triad_persistence <- function(x, n = 15, ...) {
  by_label <- if (x$params$by == "type") "MAN types" else "Triads"
  cat(sprintf("Triad Persistence Analysis (by %s)\n", x$params$by))
  cat(sprintf("Windows: %d | %s tracked: %d\n\n",
              x$params$n_windows, by_label, x$summary$n_items))

  cat("Status distribution:\n")
  cat(sprintf("  Persistent: %d | Transient: %d | Emerging: %d | Fading: %d | Sporadic: %d\n\n",
              x$summary$n_persistent, x$summary$n_transient,
              x$summary$n_emerging, x$summary$n_fading, x$summary$n_sporadic))

  if (nrow(x$triads) > 0) {
    cat(sprintf("Top %d triads by persistence:\n", min(n, nrow(x$triads))))
    print(utils::head(x$triads[, c("triad", "type", "persistence", "windows", "status")], n),
          row.names = FALSE)
  } else {
    cat("No triads found.\n")
  }

  invisible(x)
}


#' Plot Triad Persistence Analysis
#'
#' Create visualizations for triad persistence analysis including heatmaps,
#' timelines, and status distribution charts.
#'
#' @param x A `cograph_triad_persistence` object from [triad_persistence()]
#' @param type Plot type: "heatmap" (presence across windows, default),
#'   "timeline" (first/last appearance), or "status" (status distribution bar chart)
#' @param top_n Maximum number of triads to display. Default `NULL` shows all triads.
#'   Set to an integer to limit (e.g., `top_n = 20` for top 20).
#' @param fill For heatmap type only: "density" (default) colors cells by actual count
#'   normalized globally across all cells, "binary" shows simple present/absent coloring.
#' @param normalize Logical. If `TRUE`, normalize counts by the total number of triads
#'   in each window. This accounts for windows with more data having more observations.
#'   Default is `FALSE` for raw counts.
#' @param show_counts Logical. If `TRUE`, display count values inside heatmap cells.
#'   Default is `TRUE` when matrix is small enough (< 200 cells), `FALSE` otherwise.
#' @param ... Additional arguments (unused)
#'
#' @return A ggplot2 object (invisibly)
#'
#' @examples
#' \dontrun{
#' library(tna)
#' m <- extract_motifs_temporal(group_regulation, window_size = 5)
#'
#' # By specific triads (default) - counts are always 1
#' pers <- triad_persistence(m)
#' plot(pers, type = "heatmap", top_n = 20)
#'
#' # By MAN type - shows how many triads of each type per window
#' pers_type <- triad_persistence(m, by = "type")
#' plot(pers_type, type = "heatmap")              # Raw counts
#' plot(pers_type, type = "heatmap", normalize = TRUE)  # Normalized by window totals
#' plot(pers_type, type = "heatmap", fill = "binary")   # Binary present/absent
#'
#' plot(pers, type = "timeline")
#' plot(pers, type = "status")
#' }
#'
#' @method plot cograph_triad_persistence
#' @export
plot.cograph_triad_persistence <- function(x, type = c("heatmap", "timeline", "status"),
                                            top_n = NULL,
                                            fill = c("density", "binary"),
                                            normalize = FALSE,
                                            show_counts = NULL, ...) {
  type <- match.arg(type)
  fill <- match.arg(fill)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }

  if (nrow(x$triads) == 0) {
    message("No triads to plot")
    return(invisible(NULL))
  }

  if (type == "heatmap") {
    # Presence heatmap: triads x windows
    # Handle top_n = NULL (show all) vs integer limit
    n_show <- if (is.null(top_n)) nrow(x$triads) else min(top_n, nrow(x$triads))
    df <- x$triads[seq_len(n_show), ]

    # Get counts matrix for density fill
    mat <- x$counts_matrix[df$label, , drop = FALSE]

    n_triads <- nrow(mat)
    n_windows <- ncol(mat)

    # Apply normalization if requested (divide by window totals)
    if (normalize && !is.null(x$window_totals)) {
      for (j in seq_len(n_windows)) {
        if (x$window_totals[j] > 0) {
          mat[, j] <- mat[, j] / x$window_totals[j]
        }
      }
    }

    # Store display values (for labels) - round normalized values
    display_mat <- if (normalize) round(mat * 100, 1) else mat

    # Adaptive sizing based on density
    # Text size scales down with more triads
    y_text_size <- if (n_triads <= 10) 10 else if (n_triads <= 20) 8 else if (n_triads <= 40) 6 else 5
    x_text_size <- if (n_windows <= 10) 10 else if (n_windows <= 20) 8 else 6

    # Tile border width scales with density
    tile_border <- if (n_triads * n_windows <= 100) 0.5 else if (n_triads * n_windows <= 400) 0.3 else 0.1

    # Truncate long triad names for dense plots, ensuring uniqueness
    max_label_len <- if (n_triads <= 15) 50 else if (n_triads <= 30) 30 else 20
    triad_labels <- df$triad
    if (any(nchar(triad_labels) > max_label_len)) {
      triad_labels <- ifelse(nchar(triad_labels) > max_label_len,
                             paste0(substr(triad_labels, 1, max_label_len - 2), ".."),
                             triad_labels)
      # Ensure uniqueness by adding index if duplicated
      if (any(duplicated(triad_labels))) {
        dups <- duplicated(triad_labels) | duplicated(triad_labels, fromLast = TRUE)
        triad_labels[dups] <- paste0(triad_labels[dups], " (", seq_along(which(dups)), ")")
      }
    }

    # Calculate fill values based on fill type
    if (fill == "density") {
      # Global normalization: darkest cell = highest count across ALL cells
      global_max <- max(mat, na.rm = TRUE)
      if (global_max > 0) {
        fill_mat <- mat / global_max
      } else {
        fill_mat <- mat
      }
      legend_name <- if (normalize) "% of Window" else "Density"
    } else {
      # Binary: present (1) / absent (0)
      fill_mat <- (mat > 0) * 1
      legend_name <- NULL
    }

    # Reshape for ggplot
    plot_df <- data.frame()
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {
        plot_df <- rbind(plot_df, data.frame(
          triad = triad_labels[i],
          window = j,
          count = display_mat[i, j],
          raw_count = x$counts_matrix[df$label[i], j],
          fill_val = fill_mat[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }

    # Order triads by persistence (reversed for y-axis)
    plot_df$triad <- factor(plot_df$triad, levels = rev(triad_labels))

    # Build the plot
    if (fill == "density") {
      # Density fill: gradient from white (0) to maroon (max)
      # Set fill_val to NA for absent cells so they get na.value
      plot_df$fill_val[plot_df$raw_count == 0] <- NA

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = factor(.data$window),
                                                  y = .data$triad)) +
        ggplot2::geom_tile(ggplot2::aes(fill = .data$fill_val),
                           color = "white", linewidth = tile_border) +
        ggplot2::scale_fill_gradient(low = "#FFFFFF", high = "#800020",
                                      na.value = "#f5f5f5",
                                      name = legend_name,
                                      labels = scales::percent_format(accuracy = 1))
    } else {
      # Binary fill: simple present/absent
      plot_df$present_factor <- factor(ifelse(plot_df$raw_count > 0, "1", "0"), levels = c("0", "1"))

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = factor(.data$window),
                                                  y = .data$triad,
                                                  fill = .data$present_factor)) +
        ggplot2::geom_tile(color = if (tile_border > 0.1) "white" else NA,
                           linewidth = tile_border) +
        ggplot2::scale_fill_manual(values = c("0" = "#f5f5f5", "1" = "#800020"),
                                    labels = c("Absent", "Present"), name = NULL)
    }

    # Determine whether to show counts
    n_cells <- n_triads * n_windows
    if (is.null(show_counts)) {
      show_counts <- n_cells <= 200  # Auto: show for small matrices
    }

    # Add count labels if requested
    if (show_counts) {
      # Only show non-zero counts (use raw_count for filtering)
      label_df <- plot_df[plot_df$raw_count > 0, ]

      # Adaptive text size based on matrix density
      count_text_size <- if (n_cells <= 50) 3.5 else if (n_cells <= 100) 3 else if (n_cells <= 200) 2.5 else 2

      # Format label: percentage for normalized, integer for raw
      if (normalize) {
        label_df$label_text <- paste0(round(label_df$count, 0), "%")
      } else {
        label_df$label_text <- as.character(as.integer(label_df$count))
      }

      # Text color: white on dark cells (high density), dark on light cells
      if (fill == "density") {
        label_df$text_color <- ifelse(label_df$fill_val > 0.5, "white", "#333333")
      } else {
        label_df$text_color <- "white"  # Always white on maroon for binary
      }

      p <- p + ggplot2::geom_text(data = label_df,
                                   ggplot2::aes(x = factor(.data$window),
                                                y = .data$triad,
                                                label = .data$label_text,
                                                color = .data$text_color),
                                   size = count_text_size, fontface = "bold",
                                   show.legend = FALSE) +
        ggplot2::scale_color_identity()
    }

    # Adjust aspect ratio for very wide or tall matrices
    aspect_ratio <- n_windows / n_triads
    coord_ratio <- if (aspect_ratio > 3) 0.5 else if (aspect_ratio < 0.3) 2 else NULL

    # Build title based on parameters
    by_mode <- x$params$by
    title_label <- if (by_mode == "type") "MAN Type" else "Triad"
    title_suffix <- if (normalize) " (Normalized)" else ""
    row_label <- if (by_mode == "type") "types" else "triads"

    p <- p +
      ggplot2::labs(title = paste0(title_label, " Persistence Heatmap", title_suffix),
                    subtitle = sprintf("%d %s \u00d7 %d windows", n_triads, row_label, n_windows),
                    x = "Time Window", y = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = y_text_size),
        axis.text.x = ggplot2::element_text(size = x_text_size),
        legend.position = if (n_triads > 30) "right" else "bottom",
        plot.title = ggplot2::element_text(face = "bold"),
        panel.grid = ggplot2::element_blank()
      )

    # Add coord_fixed for better aspect ratio if needed
    if (!is.null(coord_ratio)) {
      p <- p + ggplot2::coord_fixed(ratio = coord_ratio)
    }

    print(p)
    invisible(p)

  } else if (type == "timeline") {
    # Timeline showing first/last appearance
    n_show <- if (is.null(top_n)) nrow(x$triads) else min(top_n, nrow(x$triads))
    df <- x$triads[seq_len(n_show), ]
    df$triad <- factor(df$triad, levels = rev(df$triad))

    p <- ggplot2::ggplot(df, ggplot2::aes(y = .data$triad)) +
      ggplot2::geom_segment(ggplot2::aes(x = .data$first_window,
                                          xend = .data$last_window,
                                          yend = .data$triad,
                                          color = .data$status),
                            linewidth = 3) +
      ggplot2::geom_point(ggplot2::aes(x = .data$first_window), size = 3, color = "#2E7D32") +
      ggplot2::geom_point(ggplot2::aes(x = .data$last_window), size = 3, color = "#C62828") +
      ggplot2::scale_color_manual(values = c(
        persistent = "#800020", transient = "#999999",
        emerging = "#2E7D32", fading = "#C62828", sporadic = "#E69F00"
      )) +
      ggplot2::scale_x_continuous(breaks = seq_len(x$params$n_windows)) +
      ggplot2::labs(title = "Triad Lifespan Timeline",
                    subtitle = "Green dot = first appearance, Red dot = last appearance",
                    x = "Time Window", y = NULL, color = "Status") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        legend.position = "right",
        plot.title = ggplot2::element_text(face = "bold")
      )

    print(p)
    invisible(p)

  } else if (type == "status") {
    # Bar chart of status distribution
    status_df <- data.frame(
      status = c("persistent", "transient", "emerging", "fading", "sporadic"),
      count = c(x$summary$n_persistent, x$summary$n_transient,
                x$summary$n_emerging, x$summary$n_fading, x$summary$n_sporadic)
    )
    status_df <- status_df[status_df$count > 0, ]

    if (nrow(status_df) == 0) {
      message("No status data to plot")
      return(invisible(NULL))
    }

    status_df$status <- factor(status_df$status, levels = status_df$status)

    p <- ggplot2::ggplot(status_df, ggplot2::aes(x = .data$status,
                                                  y = .data$count,
                                                  fill = .data$status)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = .data$count), vjust = -0.5, fontface = "bold") +
      ggplot2::scale_fill_manual(values = c(
        persistent = "#800020", transient = "#999999",
        emerging = "#2E7D32", fading = "#C62828", sporadic = "#E69F00"
      )) +
      ggplot2::labs(title = "Triad Status Distribution",
                    x = NULL, y = "Number of Triads") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(face = "bold")
      ) +
      ggplot2::ylim(0, max(status_df$count) * 1.15)

    print(p)
    invisible(p)
  }
}
