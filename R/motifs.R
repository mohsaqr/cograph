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
#' @seealso [extract_motifs()] for detailed triad extraction with filtering,
#'   [triad_census()] for simple MAN counts, [extract_triads()] to get
#'   individual triads with node labels, [plot.cograph_motifs()] for plotting
#' @family motifs
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
    return(.motif_census_undirected(g, n_random, method, seed))
  }

  if (size != 3 && size != 4) {
    stop("size must be 3 or 4")
  }

  if (!is.null(seed)) set.seed(seed)

  # Count motifs in observed network
  observed <- igraph::motifs(g, size = size)
  observed[is.na(observed)] <- 0

  # Generate null distribution (vectorized)
  null_list <- lapply(seq_len(n_random), function(i) {
    g_rand <- .generate_random_graph(g, method)
    counts <- igraph::motifs(g_rand, size = size)
    counts[is.na(counts)] <- 0
    counts
  })
  null_counts <- do.call(rbind, null_list)

  # Calculate statistics
  null_mean <- colMeans(null_counts)
  null_sd <- apply(null_counts, 2, sd)

  z_scores <- ifelse(null_sd > 0,
                     (observed - null_mean) / null_sd,
                     0)

  p_values <- 2 * pnorm(-abs(z_scores))

  motif_names <- .get_motif_names(size, directed)

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

  n <- igraph::vcount(g)

  n_edges <- igraph::ecount(g)
  n_triangles <- sum(igraph::count_triangles(g)) / 3

  total_triads <- choose(n, 3)

  degrees <- igraph::degree(g)
  n_wedges <- sum(choose(degrees, 2)) - 3 * n_triangles

  n_empty <- total_triads - n_triangles - n_wedges

  observed <- c(empty = n_empty, wedge = n_wedges, triangle = n_triangles)

  # Null distribution (vectorized)
  null_list <- lapply(seq_len(n_random), function(i) {
    g_rand <- .generate_random_graph(g, method)
    deg_rand <- igraph::degree(g_rand)
    tri_rand <- sum(igraph::count_triangles(g_rand)) / 3
    wedge_rand <- sum(choose(deg_rand, 2)) - 3 * tri_rand
    empty_rand <- total_triads - tri_rand - wedge_rand
    c(empty_rand, wedge_rand, tri_rand)
  })
  null_counts <- do.call(rbind, null_list)
  colnames(null_counts) <- names(observed)

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
    n <- igraph::vcount(g)
    m <- igraph::ecount(g)
    g_rand <- igraph::sample_gnm(n, m, directed = directed)
  }

  igraph::simplify(g_rand)
}

#' @method print cograph_motifs
#' @export
print.cograph_motifs <- function(x, ...) {
  cat("Network Motif Analysis\n")
  cat(sprintf("Size: %d-node motifs (%s)\n",
              x$size, if (x$directed) "directed" else "undirected"))
  cat(sprintf("Null model: %s (n=%d)\n\n", x$method, x$n_random))

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
#' @seealso [motif_census()] for the analysis that produces this object
#' @family motifs
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

  df <- data.frame(
    motif = names(x$counts),
    count = as.numeric(x$counts),
    expected = x$null_mean,
    z = x$z_scores,
    p = x$p_values,
    significant = x$significant,
    stringsAsFactors = FALSE
  )

  df <- df[df$count > 0 | df$significant, ]

  if (!show_nonsig) {
    df <- df[df$significant, ]
  }

  if (nrow(df) == 0) {
    message("No motifs to plot. Try show_nonsig = TRUE")
    return(invisible(NULL))
  }

  if (!is.null(top_n) && nrow(df) > top_n) {
    df <- df[order(-abs(df$z)), ][seq_len(top_n), ]
  }

  df$motif <- factor(df$motif, levels = df$motif[order(df$z)])

  if (type == "bar") {
    .plot_motifs_bar(df, colors, x$directed, x$size)
  } else if (type == "heatmap") {
    .plot_motifs_heatmap(df, colors)
  } else if (type == "network") {
    .plot_motifs_network(df, x$directed, x$size, colors)
  }
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
#' @seealso [motif_census()] for statistical significance testing,
#'   [extract_triads()] to get individual triads with node labels
#' @family motifs
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
#' @seealso [motif_census()], [triad_census()], [extract_motifs()]
#' @family motifs
#' @export
extract_triads <- function(x, type = NULL, involving = NULL,
                           threshold = 0, min_total = 5, directed = NULL) {
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

  adj <- (mat > threshold) * 1L

  combos <- utils::combn(n, 3)
  nc <- ncol(combos)

  i <- combos[1, ]
  j <- combos[2, ]
  k <- combos[3, ]

  # VECTORIZED: Extract all 6 edge presence values
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

  total_w <- w_ij + w_ji + w_ik + w_ki + w_jk + w_kj

  triad_types <- .classify_triads_vectorized(e_ij, e_ji, e_ik, e_ki, e_jk, e_kj)

  edge_sum <- e_ij + e_ji + e_ik + e_ki + e_jk + e_kj
  has_edges <- edge_sum > 0

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
  code <- e_ij + 2L * e_ji + 4L * e_ik + 8L * e_ki + 16L * e_jk + 32L * e_kj

  lookup <- .get_triad_lookup()

  lookup[code + 1L]
}

# Build lookup table mapping edge codes to MAN types
# Called once and cached
# @noRd
.get_triad_lookup <- function() {
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
  triad_patterns <- .get_triad_patterns_canonical()

  # All 6 permutations of 3 nodes
  perms <- list(
    c(1, 2, 3), c(1, 3, 2), c(2, 1, 3),
    c(2, 3, 1), c(3, 1, 2), c(3, 2, 1)
  )

  lookup <- character(64)

  # For each possible 6-bit edge code (0-63)
  for (code in 0:63) {
    bits <- as.integer(intToBits(code)[1:6])
    adj <- matrix(0L, 3, 3)
    adj[1, 2] <- bits[1]  # e_ij
    adj[2, 1] <- bits[2]  # e_ji
    adj[1, 3] <- bits[3]  # e_ik
    adj[3, 1] <- bits[4]  # e_ki
    adj[2, 3] <- bits[5]  # e_jk
    adj[3, 2] <- bits[6]  # e_kj

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

    if (!matched) lookup[code + 1] <- "003"
  }

  lookup
}

# Vectorized triad counting for a single matrix
# @noRd
.count_triads_matrix_vectorized <- function(mat, edge_method, edge_threshold,
                                             expected_mat = NULL,
                                             exclude = character(0),
                                             include = NULL) {
  s <- nrow(mat)
  if (s < 3) return(NULL)

  combos <- utils::combn(s, 3)
  nc <- ncol(combos)
  if (nc == 0) return(NULL)

  i <- combos[1, ]
  j <- combos[2, ]
  k <- combos[3, ]

  obs_ij <- mat[cbind(i, j)]
  obs_ji <- mat[cbind(j, i)]
  obs_ik <- mat[cbind(i, k)]
  obs_ki <- mat[cbind(k, i)]
  obs_jk <- mat[cbind(j, k)]
  obs_kj <- mat[cbind(k, j)]

  total <- obs_ij + obs_ji + obs_ik + obs_ki + obs_jk + obs_kj

  has_edges <- total > 0
  if (!any(has_edges)) return(NULL)

  if (edge_method == "any") {
    e_ij <- as.integer(obs_ij > 0)
    e_ji <- as.integer(obs_ji > 0)
    e_ik <- as.integer(obs_ik > 0)
    e_ki <- as.integer(obs_ki > 0)
    e_jk <- as.integer(obs_jk > 0)
    e_kj <- as.integer(obs_kj > 0)

  } else if (edge_method == "percent") {
    thresh <- total * edge_threshold
    e_ij <- as.integer(obs_ij >= thresh)
    e_ji <- as.integer(obs_ji >= thresh)
    e_ik <- as.integer(obs_ik >= thresh)
    e_ki <- as.integer(obs_ki >= thresh)
    e_jk <- as.integer(obs_jk >= thresh)
    e_kj <- as.integer(obs_kj >= thresh)

  } else {
    if (is.null(expected_mat)) {
      stop("expected_mat required for edge_method='expected'", call. = FALSE)
    }
    exp_ij <- expected_mat[cbind(i, j)]
    exp_ji <- expected_mat[cbind(j, i)]
    exp_ik <- expected_mat[cbind(i, k)]
    exp_ki <- expected_mat[cbind(k, i)]
    exp_jk <- expected_mat[cbind(j, k)]
    exp_kj <- expected_mat[cbind(k, j)]

    e_ij <- as.integer((obs_ij / exp_ij) >= edge_threshold & obs_ij > 0)
    e_ji <- as.integer((obs_ji / exp_ji) >= edge_threshold & obs_ji > 0)
    e_ik <- as.integer((obs_ik / exp_ik) >= edge_threshold & obs_ik > 0)
    e_ki <- as.integer((obs_ki / exp_ki) >= edge_threshold & obs_ki > 0)
    e_jk <- as.integer((obs_jk / exp_jk) >= edge_threshold & obs_jk > 0)
    e_kj <- as.integer((obs_kj / exp_kj) >= edge_threshold & obs_kj > 0)
  }

  edge_sum <- e_ij + e_ji + e_ik + e_ki + e_jk + e_kj

  keep <- has_edges & (edge_sum > 0)
  if (!any(keep)) return(NULL)

  i <- i[keep]
  j <- j[keep]
  k <- k[keep]
  e_ij <- e_ij[keep]
  e_ji <- e_ji[keep]
  e_ik <- e_ik[keep]
  e_ki <- e_ki[keep]
  e_jk <- e_jk[keep]
  e_kj <- e_kj[keep]

  triad_types <- .classify_triads_vectorized(e_ij, e_ji, e_ik, e_ki, e_jk, e_kj)

  if (!is.null(include) && length(include) > 0) {
    keep_include <- triad_types %in% include
    if (!any(keep_include)) return(NULL)
    i <- i[keep_include]
    j <- j[keep_include]
    k <- k[keep_include]
    triad_types <- triad_types[keep_include]
  }

  if (length(exclude) > 0) {
    keep_exclude <- !(triad_types %in% exclude)
    if (!any(keep_exclude)) return(NULL)
    i <- i[keep_exclude]
    j <- j[keep_exclude]
    k <- k[keep_exclude]
    triad_types <- triad_types[keep_exclude]
  }

  if (length(i) == 0) return(NULL)

  data.frame(
    i = i,
    j = j,
    k = k,
    type = triad_types,
    stringsAsFactors = FALSE
  )
}

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
#' @seealso [extract_motifs()] for motif analysis using edge lists,
#'   [extract_motifs_temporal()] for temporal motif analysis
#' @family motifs
#' @export
get_edge_list <- function(x, by_individual = TRUE, drop_zeros = TRUE) {
  if (!inherits(x, "tna")) {
    stop("x must be a tna object")
  }

  d <- x$data
  type <- attr(x, "type")
  scaling <- attr(x, "scaling")
  params <- attr(x, "params")

  init_fn <- .get_tna_initialize_model()
  model <- init_fn(d, type, scaling, params, transitions = TRUE)
  trans <- model$trans

  labels <- x$labels
  n <- dim(trans)[1]
  s <- dim(trans)[2]

  if (by_individual) {
    # Build edge list with individual IDs (vectorized)
    edges_list <- lapply(seq_len(n), function(i) {
      mat <- trans[i, , ]
      idx <- if (drop_zeros) which(mat > 0, arr.ind = TRUE) else
             expand.grid(from = seq_len(s), to = seq_len(s))

      if (nrow(idx) > 0) {
        if (drop_zeros) {
          data.frame(
            id = i,
            from = labels[idx[, 1]],
            to = labels[idx[, 2]],
            count = mat[idx],
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            id = i,
            from = labels[idx$from],
            to = labels[idx$to],
            count = as.vector(mat),
            stringsAsFactors = FALSE
          )
        }
      } else {
        NULL
      }
    })

    edges <- do.call(rbind, edges_list)
  } else {
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

    edges <- edges[order(edges$count, decreasing = TRUE), ]
  }

  rownames(edges) <- NULL
  edges
}
