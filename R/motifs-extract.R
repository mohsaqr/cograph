# Motif extraction pipeline
# Contains: extract_motifs, print.cograph_motif_analysis, plot.cograph_motif_analysis

#' Extract Motifs from Network Data
#'
#' Extract and analyze triad motifs from network data with flexible filtering,
#' pattern selection, and statistical significance testing. Supports both
#' individual-level analysis (with tna objects or grouped data) and aggregate
#' analysis (with matrices or networks). The supplied adjacency is classified
#' as directed dyads using the 16-class MAN system.
#'
#' @details Both individual and aggregate significance in this legacy extractor
#' use a directed weighted stub-matching null: positive weights retain at least
#' one integer stub, shuffled targets preserve the integerized in/out margins,
#' and generated loops/parallel edges are reduced to a simple loopless
#' projection for triad classification. This differs from aggregate
#' [motifs()], which delegates to [motif_census()] and its simple-graph rewiring
#' null. Observed self-loops are excluded before activity gating, counting, and
#' null construction.
#' The selected \code{edge_method} is reapplied to each null replicate, but
#' positive fractional weights retain at least one integer stub. This preserves
#' support while potentially changing the mass scale used by
#' \code{"percent"}/\code{"expected"} inference. Descriptive results and the
#' default \code{edge_method = "any"} are unaffected.
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
#'   For "expected", a ratio (e.g., 1.5 means 50\% stronger than expected).
#'   The default 1.5 is calibrated for this method.
#'   For "percent", a proportion (e.g., 0.15 for 15\% of triad total weight).
#'   When using "percent", set this explicitly (e.g., 0.15).
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
#' @param min_transitions At individual level: minimum total transitions for a
#'   person to be included in the analysis. At aggregate level: minimum triad
#'   weight to count as present. Default 5.
#' @param significance Logical. Run permutation significance test? Default FALSE.
#' @param n_perm Number of permutations for the significance test. When
#'   \code{significance = TRUE}, must be a whole number of at least 2.
#'   Default 100.
#' @param seed Random seed for reproducibility.
#'
#' @return A `cograph_motif_analysis` object (list) containing:
#'   \describe{
#'     \item{results}{Data frame with one row per node-triple and MAN type,
#'       the display label \code{triad}, unambiguous \code{node1}/\code{node2}/
#'       \code{node3} columns, its observed count, and (if
#'       \code{significance = TRUE}) expected
#'       count, z-score, empirical p-value, and significance marker. A node
#'       triple that has different types across individuals therefore appears
#'       in more than one row.}
#'     \item{type_summary}{Summary counts by motif type across individuals.}
#'     \item{params}{List of parameters used}
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
#' # Small aggregate example -- no significance test for speed
#' mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
#' m <- extract_motifs(mat, significance = FALSE)
#' print(m)
#'
#' @examplesIf requireNamespace("tna", quietly = TRUE)
#' \donttest{
#' Mod <- tna::tna(tna::group_regulation)
#' # Individual-level from tna -- keep n_perm tiny for example speed
#' extract_motifs(Mod, top = 10, significance = TRUE, n_perm = 10L, seed = 1)
#' # Filter to feed-forward loops only
#' extract_motifs(Mod, include_types = "030T", significance = FALSE)
#' }
#'
#' @seealso [motifs()], [subgraphs()], [extract_triads()], [motif_census()]
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
  if (significance) {
    n_perm <- .validate_motif_repetitions(n_perm, "n_perm")
  }

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

  final_exclude <- if (!is.null(include_types)) {
    character(0)
  } else {
    unique(c(pattern_exclude, exclude_types))
  }

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }

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
      stop("igraph package required") # nocov
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

  # Aggregate level means one pooled network: sum the per-individual
  # transition matrices before counting. Without this, "aggregate" ran the
  # same per-individual loop and only the metadata changed.
  if (level == "aggregate" && dim(trans)[1] > 1L) {
    pooled <- apply(trans, c(2, 3), sum)
    trans <- array(pooled, dim = c(1L, dim(trans)[2], dim(trans)[3]))
  }

  n_ind <- dim(trans)[1]
  s <- dim(trans)[2]
  trans <- .motif_strip_loops(trans)
  eligible_individuals <- if (level == "individual") {
    vapply(seq_len(n_ind), function(ind) {
      sum(.motif_unit_matrix(trans, ind)) >= min_transitions
    }, logical(1))
  } else {
    rep(TRUE, n_ind)
  }

  # Main counting function (vectorized)
  count_triads_internal <- function(trans_array, edge_method, edge_threshold,
                                    min_trans, exclude, include = NULL,
                                    eligible = NULL) {
    all_results <- lapply(seq_len(dim(trans_array)[1]), function(ind) {
      mat <- .motif_unit_matrix(trans_array, ind)
      # Documented semantics: at individual level min_transitions gates the
      # person's total activity; at aggregate level it is a per-triad weight
      # filter (applied below), not a whole-network gate.
      if (level == "individual") {
        is_eligible <- if (is.null(eligible)) {
          sum(mat) >= min_trans
        } else {
          eligible[ind]
        }
        if (!is_eligible) return(NULL)
      }

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

      if (level == "aggregate" && !is.null(triads_df)) {
        triads_df <- triads_df[triads_df$weight >= min_trans, , drop = FALSE]
      }

      if (!is.null(triads_df) && nrow(triads_df) > 0) {
        data.frame(
          person = ind,
          .triad_key = paste(triads_df$i, triads_df$j, triads_df$k,
                             sep = "\r"),
          triad = paste(labels[triads_df$i], labels[triads_df$j],
                        labels[triads_df$k], sep = " - "),
          node1 = labels[triads_df$i], node2 = labels[triads_df$j],
          node3 = labels[triads_df$k],
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
                                        include_types, eligible_individuals)

  if (is.null(observed_raw) || nrow(observed_raw) == 0) {
    warning("No triads found with current settings")
    return(NULL)
  }

  # Aggregate by (triad, MAN type), not just by node triple. The same nodes can
  # instantiate different types in different people; collapsing those rows to
  # a dominant type makes the reported type and observed count disagree.
  obs_freq <- stats::aggregate(
    person ~ .triad_key + triad + node1 + node2 + node3 + type,
    data = observed_raw, FUN = length
  )
  names(obs_freq)[7] <- "observed"
  obs_freq <- obs_freq[, c(".triad_key", "triad", "node1", "node2", "node3",
                           "observed", "type")]

  # Significance testing
  if (significance) {
    null_matrix <- matrix(0, nrow = nrow(obs_freq), ncol = n_perm)
    rownames(null_matrix) <- paste(obs_freq$.triad_key, obs_freq$type,
                                   sep = "\r")

    # Pre-compute balanced row/column stubs. Shuffling the column stubs gives
    # the directed weighted stub-matching null used by individual motifs().
    # Stub validation and construction cover only null-eligible units: a
    # malformed cell in a unit the null never touches must not abort the run.
    ind_stubs <- vector("list", n_ind)
    ind_stubs[eligible_individuals] <- lapply(
      which(eligible_individuals),
      function(ind) .motif_configuration_stubs(.motif_unit_matrix(trans, ind))
    )

    ss <- as.integer(s * s)

    lapply(seq_len(n_perm), function(p) {
      trans_perm <- array(0, dim = dim(trans))

      # Shuffle each individual's target stubs while keeping both marginals.
      vapply(seq_len(n_ind), function(ind) {
        stubs <- ind_stubs[[ind]]
        if (is.null(stubs) || stubs$total == 0L) return(0L)
        shuffled_cols <- stubs$cols[sample.int(stubs$total)]
        lin <- (shuffled_cols - 1L) * s + stubs$rows
        trans_perm[ind, , ] <<- matrix(tabulate(lin, ss), s, s)
        0L
      }, integer(1))

      perm_raw <- count_triads_internal(trans_perm, edge_method, edge_threshold,
                                        min_transitions, final_exclude,
                                        include_types, eligible_individuals)

      if (!is.null(perm_raw)) {
        perm_freq <- stats::aggregate(
          person ~ .triad_key + triad + node1 + node2 + node3 + type,
          data = perm_raw, FUN = length
        )
        observed_key <- paste(obs_freq$.triad_key, obs_freq$type, sep = "\r")
        perm_key <- paste(perm_freq$.triad_key, perm_freq$type, sep = "\r")
        matched <- match(perm_key, observed_key)
        valid_match <- !is.na(matched)
        if (any(valid_match)) {
          null_matrix[matched[valid_match], p] <<- perm_freq$person[valid_match]
        }
      }
      NULL
    })

    ns <- .motif_null_stats(obs_freq$observed, t(null_matrix))
    obs_freq$expected <- round(ns$mean, 1)
    obs_freq$z <- round(ns$z, 2)
    obs_freq$p <- ns$p
    obs_freq$sig <- get_significance_stars(obs_freq$p)
  }

  obs_freq$.triad_key <- NULL

  # Sort by observed (or z if significance). Degenerate-null rows (z = NA
  # with the smallest possible empirical p) rank as the most extreme in
  # their direction so a `top` cut below never silently drops them.
  if (significance) {
    z_rank <- .motif_z_rank(obs_freq$z, obs_freq$p,
                            effect = obs_freq$observed - obs_freq$expected)
    obs_freq <- obs_freq[order(z_rank, decreasing = TRUE), ]
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

#' @rdname extract_motifs
#' @param n Number of motif rows to print.
#' @param ... Passed to methods; currently unused.
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
#' @param type Plot type:
#'   \describe{
#'     \item{\code{"triads"}}{(default) Network diagrams of specific named triads,
#'       arranged in a grid. Each cell shows the three nodes and their edges.}
#'     \item{\code{"types"}}{Bar chart of MAN type frequencies.}
#'     \item{\code{"significance"}}{Z-score plot with one bar per node-triple
#'       and MAN type. Requires \code{significance = TRUE} in
#'       \code{extract_motifs()}.}
#'     \item{\code{"patterns"}}{Abstract MAN pattern diagrams showing edge
#'       structure of each triad type without specific node labels.}
#'   }
#' @param n Number of triads/patterns to show. Default 20.
#' @param colors Two-element color vector mapped to a three-tone significance
#'   scale (used by \code{type = "significance"} and by \code{type = "patterns"}
#'   node fills): \code{colors[1]} fills items that are significantly
#'   under-represented (\code{p < .05} and \code{z < 0}); \code{colors[2]}
#'   fills items that are significantly over-represented (\code{p < .05} and
#'   \code{z > 0}); everything else is filled neutral grey (\code{"#9E9E9E"}).
#'   When significance was not run, patterns nodes use \code{colors[1]} as a
#'   single fill. Default \code{c("#2166AC", "#B2182B")} (blue for under, red
#'   for over).
#' @param res Resolution for scaling (kept for backwards compatibility). Default 72.
#' @param node_size Size of nodes in triad diagrams (1-10 scale). Default 5.
#' @param label_size Font size for node labels (3-letter abbreviations). Default 7.
#' @param title_size Font size for motif type title (e.g., "120C"). Default 7.
#' @param stats_size Font size for statistics text (n, z, p). Default 5.
#' @param ncol Number of columns in the plot grid. Default 5.
#' @param legend Show abbreviation legend at bottom? Default TRUE.
#' @param color Color for nodes, edges, and labels in triad diagrams.
#'   Default \code{"#800020"} (maroon).
#' @param spacing Spacing multiplier between grid cells (0.5-2). Default 1.
#' @param combined Logical: when TRUE (default) and \code{type = "patterns"},
#'   arrange the per-motif panels in an internal grid via
#'   \code{graphics::par(mfrow=...)}. Set to FALSE to draw into a layout the
#'   caller has already configured (e.g. via \code{\link{panel_layout}()}).
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns NULL for triad and pattern plots, or a ggplot2
#'   object for types and significance plots.
#'
#' @examples
#' mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
#' m <- extract_motifs(mat, significance = FALSE)
#' plot(m)
#' plot(m, type = "types")
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
                                         spacing = 1, combined = TRUE, ...) {

  type <- match.arg(type)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting") # nocov
  }

  if (type == "types") {
    # Bar plot of type distribution
    df <- data.frame(
      type = names(x$type_summary),
      count = as.numeric(x$type_summary)
    )
    df$type <- factor(df$type, levels = df$type[order(df$count, decreasing = TRUE)])

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$type, y = .data$count)) +
      ggplot2::geom_col(fill = colors[1], width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = .data$count), vjust = -0.5, size = 3) +
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

  } else if (type == "significance") {
    if (!x$params$significance) {
      stop("No significance data available. ",
           "Re-run extract_motifs() with significance = TRUE.", call. = FALSE)
    }
    # Z-score plot. A node triple may legitimately have several MAN types
    # across individuals, so retain each (triple, type) row and label it
    # explicitly instead of dropping all but the first rendered triple.
    # Degenerate-null rows (z = NA) cannot be drawn as bars; drop them with
    # a message rather than letting them displace real high-z bars.
    df <- .motif_drop_na_z_rows(x$results)
    if (nrow(df) == 0) {
      stop("No motif rows with a finite z-score to plot.", call. = FALSE)
    }
    df <- df[order(df$z), ]
    if (nrow(df) > n * 2) {
      df <- rbind(utils::head(df, n), utils::tail(df, n))
    }
    df$label <- make.unique(paste0(df$triad, " [", df$type, "]"))
    df$direction <- ifelse(
      !is.na(df$p) & df$p < 0.05 & df$z > 0, "over",
      ifelse(!is.na(df$p) & df$p < 0.05 & df$z < 0, "under", "ns")
    )
    df$label <- factor(df$label, levels = df$label[order(df$z)])

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$label, y = .data$z, fill = .data$direction)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_hline(yintercept = c(-2, 2), linetype = "dashed",
                          color = "#666666", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = 0, color = "#333333", linewidth = 0.3) +
      ggplot2::scale_fill_manual(
        values = c(over = colors[2], under = colors[1], ns = "#9E9E9E"),
        labels = c(over = "Over-represented (p<.05)",
                   under = "Under-represented (p<.05)",
                   ns = "Not significant"),
        name = NULL
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Motif Significance",
        subtitle = sprintf("Permutation test (n=%d) | Dashed lines: z = +/-2 reference",
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
    .plot_motif_patterns(x, n, colors, combined = combined, ...)
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
