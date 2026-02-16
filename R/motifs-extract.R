# Motif extraction pipeline
# Contains: extract_motifs, print.cograph_motif_analysis, plot.cograph_motif_analysis

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
#' @seealso [extract_triads()], [motif_census()], [plot.cograph_motif_analysis()],
#'   [extract_motifs_temporal()] for temporal analysis
#' @family motifs
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

  # Use shared pattern filter definitions
  pf <- .get_pattern_filters()

  # Determine which types to exclude based on pattern
  if (!is.null(include_types)) {
    pattern_exclude <- character(0)
  } else if (pattern == "triangle") {
    pattern_exclude <- setdiff(pf$all_types, pf$triangle_types)
  } else if (pattern == "network") {
    pattern_exclude <- pf$network_exclude
  } else if (pattern == "closed") {
    pattern_exclude <- pf$closed_exclude
  } else {
    pattern_exclude <- character(0)
  }

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
    init_fn <- .get_tna_initialize_model()
    model <- init_fn(d, type_attr, scaling, params, transitions = TRUE)
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

    unique_ids <- unique(data$.id)
    all_states <- unique(c(data$from, data$to))
    labels <- sort(all_states)
    s <- length(labels)
    n_ind <- length(unique_ids)

    # Build 3D transition array (vectorized)
    trans <- array(0, dim = c(n_ind, s, s))
    state_idx <- setNames(seq_along(labels), labels)

    data$from_idx <- state_idx[as.character(data$from)]
    data$to_idx <- state_idx[as.character(data$to)]
    data$ind_idx <- match(data$.id, unique_ids)
    data$wt <- if ("weight" %in% names(data)) data$weight else rep(1, nrow(data))

    valid <- !is.na(data$from_idx) & !is.na(data$to_idx)
    d_valid <- data[valid, ]

    if (nrow(d_valid) > 0) {
      agg <- stats::aggregate(wt ~ ind_idx + from_idx + to_idx,
                               data = d_valid, FUN = sum)
      trans[cbind(agg$ind_idx, agg$from_idx, agg$to_idx)] <- agg$wt
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

  # Main counting function (vectorized)
  count_triads_internal <- function(trans_array, edge_method, edge_threshold,
                                    min_trans, exclude, include = NULL) {
    all_results <- lapply(seq_len(dim(trans_array)[1]), function(ind) {
      mat <- trans_array[ind, , ]
      if (sum(mat) < min_trans) return(NULL)

      expected_mat <- NULL
      if (edge_method == "expected") {
        total_mat <- sum(mat)
        row_sums <- rowSums(mat)
        col_sums <- colSums(mat)
        expected_mat <- outer(row_sums, col_sums) / total_mat
        expected_mat[expected_mat == 0] <- 0.001
      }

      triads_df <- .count_triads_matrix_vectorized(
        mat = mat,
        edge_method = edge_method,
        edge_threshold = edge_threshold,
        expected_mat = expected_mat,
        exclude = exclude,
        include = include
      )

      if (!is.null(triads_df) && nrow(triads_df) > 0) {
        data.frame(
          person = ind,
          triad = paste(labels[triads_df$i], labels[triads_df$j],
                        labels[triads_df$k], sep = " - "),
          type = triads_df$type,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })

    all_results <- all_results[!vapply(all_results, is.null, logical(1))]
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
  obs_freq$type <- vapply(obs_freq$triad, get_dom_type, character(1))

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
        matched <- match(perm_freq$triad, obs_freq$triad)
        valid_match <- !is.na(matched)
        if (any(valid_match)) {
          null_matrix[matched[valid_match], p] <- perm_freq$person[valid_match]
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
#' @seealso [extract_motifs()] for the analysis that produces this object,
#'   [motif_census()] for statistical motif analysis
#' @family motifs
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
      .motifs_ggplot_theme(12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

  } else if (type == "significance" && x$params$significance) {
    # Z-score plot
    df <- utils::head(x$results, n * 2)
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
      .motifs_ggplot_theme(11) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        legend.position = "bottom"
      )

  } else if (type == "patterns") {
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
