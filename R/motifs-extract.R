# Motif extraction pipeline
# Contains: extract_motifs, print.cograph_motif_analysis, plot.cograph_motif_analysis

#' Extract Motifs from Network Data
#'
#' Extracts the triads of a network, classifies each one into the 16 MAN
#' types of a directed network, and optionally tests each triad against a
#' permutation null model. The analysis is individual-level for tna objects
#' and for \code{data} with \code{id}, and aggregate for matrices, networks
#' and igraph objects.
#'
#' @details \code{extract_motifs()} is [subgraphs()] with
#' \code{strict = FALSE}: a row is one node triple and MAN type, and its
#' \code{observed} value is the number of units in which the triple shows that
#' type. Counting, the activity filter and the permutation null are those of
#' [subgraphs()], described in [motifs()]. Self-loops are removed before
#' counting.
#'
#' Significance needs individual-level data. For aggregate input a warning of
#' class \code{cograph_no_units} is raised and the result has no significance
#' columns.
#'
#' @param x Input data. Can be:
#'   \itemize{
#'     \item A `tna` object (supports individual-level analysis)
#'     \item A matrix (aggregate analysis only, unless `data` and `id` provided)
#'     \item A `cograph_network` object
#'     \item An `igraph` object
#'   }
#' @param data Optional data.frame of transitions for individual-level
#'   analysis, used together with \code{id}. Required columns: `from`, `to`
#'   and the column(s) named in `id`. A `weight` column, or a `count` column
#'   as returned by [get_edge_list()], gives the transition weights (1
#'   otherwise). When `x` is not a tna object, `data` is used and `x` is
#'   ignored. `data` without `id` raises an error of class
#'   \code{cograph_data_needs_id}.
#' @param id Column name(s) identifying individuals/groups in `data`. Can be
#'   a single string or a character vector; each distinct combination of the
#'   values is one individual.
#' @param level Analysis level: "individual" counts how many individuals show
#'   each triad, and "aggregate" analyzes the network summed over individuals.
#'   The default is "individual" for a tna object or when `data` and `id` are
#'   supplied, and "aggregate" otherwise. Requesting "individual" without
#'   individual data raises a warning and the aggregate level is used.
#' @param edge_method Method for determining edge presence:
#'   \describe{
#'     \item{"any"}{Edge exists if the weight is positive.}
#'     \item{"expected"}{Edge exists if observed / expected is at least the
#'       threshold, with the expectation from the row and column totals.}
#'     \item{"percent"}{Edge exists if the edge weight divided by the
#'       six-edge triad total is at least the threshold.}
#'   }
#'   Default "any".
#' @param edge_threshold Threshold for "expected" or "percent". For
#'   "expected", a ratio (1.5 means 50\% above expected). For "percent",
#'   values at or below 1 are proportions and values above 1 are percentages,
#'   so the default 1.5 means 1.5 percent; set it explicitly for "percent".
#'   Ignored when edge_method = "any". Default 1.5.
#' @param pattern Pattern filter for which triads to include:
#'   \describe{
#'     \item{"triangle"}{All 3 node pairs must be connected (any direction).
#'       Types: 030C, 030T, 120C, 120D, 120U, 210, 300. Default.}
#'     \item{"network"}{Excludes the empty triad and the sequential patterns
#'       003, 012 and 021C. Stars and triangles are kept.}
#'     \item{"closed"}{The 7 closed types, the same set as \code{"triangle"}.}
#'     \item{"all"}{All 16 MAN types.}
#'   }
#' @param exclude_types Character vector of MAN types to explicitly exclude.
#'   Applied after pattern filter. E.g., c("300") to exclude cliques.
#' @param include_types Character vector of MAN types to include. When
#'   supplied, only these types are returned, and `pattern` and
#'   `exclude_types` are ignored.
#' @param top Number of rows to return, taken after sorting (and after the
#'   \code{by_type} grouping when \code{by_type = TRUE}). NULL or 0 returns all
#'   rows. Default NULL.
#' @param by_type If TRUE, the rows are grouped by MAN type in MAN order
#'   (003 to 300), keeping the sort order within each type. Default FALSE.
#' @param min_transitions At individual level: minimum total transitions,
#'   repeats excluded, for an individual to be analysed. At aggregate level:
#'   minimum six-edge weight for a triad to be listed. Default 3, the fewest
#'   transitions that can form a closed triad. With probability or relative
#'   weights at aggregate level, use 0.
#' @param significance Logical. Run the permutation test? Needs
#'   individual-level data. Default FALSE.
#' @param n_perm Number of permutations for the significance test. When
#'   \code{significance = TRUE}, must be a whole number of at least 2.
#'   Default 100.
#' @param seed Optional random seed. The caller's random number state is
#'   restored on exit.
#'
#' @return A `cograph_motif_analysis` object, or \code{NULL} with a warning
#'   when no triad passes the filters. \code{as.data.frame()} returns its
#'   tables:
#'   \describe{
#'     \item{\code{what = "results"}}{One row per node triple and MAN type,
#'       with columns \code{triad} (display label), \code{node1},
#'       \code{node2}, \code{node3}, \code{type}, \code{observed} (units
#'       showing that type, or the six-edge weight at aggregate level) and,
#'       with \code{significance = TRUE}, \code{expected}, \code{z},
#'       \code{p} (empirical permutation p-value) and \code{sig} (logical,
#'       \code{p < 0.05}). Rows are sorted by decreasing \code{|z|}, or by
#'       decreasing \code{observed} without significance. A triple that has
#'       different types in different individuals appears in more than one
#'       row.}
#'     \item{\code{what = "types"}}{One row per MAN type with the total of
#'       \code{observed} over all rows of that type, before \code{top}.}
#'   }
#'
#' @section MAN Notation:
#' The 16 triad types use MAN (Mutual-Asymmetric-Null) notation where:
#' \itemize{
#'   \item First digit: number of mutual (bidirectional) pairs.
#'   \item Second digit: number of asymmetric (one-way) pairs.
#'   \item Third digit: number of null (no edge) pairs.
#'   \item Letter suffix: subtype (C = cycle, T = transitive, D = down,
#'     U = up). D and U describe how the type is drawn, not whether it
#'     contains an out-star or an in-star.
#' }
#'
#' @section Pattern Types:
#' The names are those used in the plot titles.
#' \describe{
#'   \item{Triangle patterns (all pairs connected):}{
#'     030C (cycle), 030T (feed-forward), 120C (cycle + mutual),
#'     120D (out-star onto mutual), 120U (in-star from mutual),
#'     210 (near-clique), 300 (clique)}
#'   \item{Network patterns:}{
#'     021D (out-star), 021U (in-star), 102 (mutual pair),
#'     111D (in-star + mutual), 111U (out-star + mutual), 201 (two mutuals),
#'     plus all triangle patterns}
#'   \item{Sequential patterns (chains):}{
#'     012 (single edge), 021C (A->B->C chain)}
#'   \item{Empty:}{003 (no edges)}
#' }
#'
#' @section Printing and plotting:
#' Printing the result shows the analysis settings, the number of units
#' analysed, the MAN type distribution and the first 20 triads.
#' \code{plot()} on the result is documented in \code{\link{plot-results}}.
#'
#' @examples
#' m <- extract_motifs(regulation_net, min_transitions = 0)
#' m
#' as.data.frame(m)
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
                           min_transitions = 3,
                           significance = FALSE,
                           n_perm = 100,
                           seed = NULL) {

  edge_method <- match.arg(edge_method)
  pattern <- match.arg(pattern)
  if (isTRUE(significance)) {
    n_perm <- .validate_motif_repetitions(n_perm, "n_perm")
  }
  stopifnot(
    "`by_type` must be TRUE or FALSE" = isTRUE(by_type) || isFALSE(by_type),
    "`significance` must be TRUE or FALSE" =
      isTRUE(significance) || isFALSE(significance),
    "`min_transitions` must be one non-negative number" =
      is.numeric(min_transitions) && length(min_transitions) == 1L &&
      !is.na(min_transitions) && min_transitions >= 0
  )

  # extract_motifs() is subgraphs(strict = FALSE): one row per node triple and
  # MAN type, counting the units that show it. This function only maps its
  # historical arguments and output shape onto that single implementation.
  input <- .extract_motifs_input(x, data, id, level)

  # At individual level `min_transitions` gates each unit's activity, as in
  # subgraphs(). At aggregate level it has always been a per-triad weight
  # filter, which is subgraphs()' `min_count` on aggregate input.
  aggregate <- input$level == "aggregate"
  res <- motifs(
    input$x,
    named_nodes = TRUE,
    actor = input$actor,
    pattern = pattern,
    include = include_types,
    exclude = exclude_types,
    significance = significance && !aggregate,
    n_perm = n_perm,
    min_count = if (aggregate) min_transitions else 1L,
    edge_method = edge_method,
    edge_threshold = edge_threshold,
    min_transitions = if (aggregate) 0 else min_transitions,
    seed = seed,
    strict = FALSE
  )
  if (significance && aggregate) {
    warning(warningCondition(
      paste0("Significance needs individual-level data (several units); ",
             "the aggregate analysis is returned without it."),
      class = "cograph_no_units", call = NULL))
    significance <- FALSE
  }
  if (is.null(res)) {
    warning("No triads found with current settings")
    return(NULL)
  }

  keep <- c("triad", "node1", "node2", "node3", "type", "observed",
            if (significance) c("expected", "z", "p", "sig"))
  results <- as.data.frame(res)[keep]
  man_order <- .triad_type_names()
  type_summary <- tapply(results$observed, factor(results$type, man_order),
                         sum)
  type_summary <- as.table(type_summary[!is.na(type_summary)])
  type_summary <- sort(type_summary, decreasing = TRUE)
  n_triads <- nrow(results)

  # Rows arrive sorted by |z| (significance) or by observed count. by_type
  # groups them in MAN order, keeping that order within each type.
  if (by_type) {
    results <- results[order(match(results$type, man_order),
                             seq_len(nrow(results))), , drop = FALSE]
  }
  if (!is.null(top) && top > 0 && nrow(results) > top) {
    results <- utils::head(results, top)
  }
  rownames(results) <- NULL

  structure(
    list(
      results = results,
      type_summary = type_summary,
      params = list(
        level = input$level,
        edge_method = edge_method,
        edge_threshold = edge_threshold,
        pattern = pattern,
        exclude_types = exclude_types,
        include_types = include_types,
        top = top,
        by_type = by_type,
        min_transitions = min_transitions,
        significance = significance,
        n_perm = if (significance) res$params$n_perm else NA,
        n_individuals = res$n_units_total %||% res$n_units,
        n_units = res$n_units,
        n_triads = n_triads,
        n_states = res$params$n_states,
        labels = res$params$labels
      )
    ),
    class = "cograph_motif_analysis"
  )
}

# Map extract_motifs() inputs to one motifs() input. Individual-level data
# (a tna, or `data` with `id`) keeps one unit per individual; aggregate
# analysis pools them into a single weighted network, as it always has.
# @noRd
.extract_motifs_input <- function(x, data, id, level) {
  if (!is.null(data) && !inherits(x, "tna")) {
    if (is.null(id)) {
      stop(errorCondition(
        "`data` needs `id`, the column(s) that identify the individuals.",
        class = "cograph_data_needs_id", call = NULL))
    }
    if (!is.data.frame(data)) stop("'data' must be a data.frame")
    if (!all(id %in% names(data))) {
      stop("id column(s) not found in data: ",
           paste(setdiff(id, names(data)), collapse = ", "))
    }
    if (!all(c("from", "to") %in% tolower(names(data)))) {
      stop("data must contain 'from' and 'to' columns")
    }
    # One integer code per distinct id combination: pasting the raw values
    # would merge ("a_b", "c") with ("a", "b_c").
    codes <- lapply(data[id], \(v) match(v, unique(v)))
    unit <- do.call(paste, c(codes, sep = "-"))
    nms <- tolower(names(data))
    edge_cols <- names(data)[nms %in% c("from", "to", "weight", "count")]
    el <- data[edge_cols]
    el$.unit <- unit
    has_units <- TRUE
  } else if (inherits(x, "tna")) {
    has_units <- TRUE
  } else if (is.matrix(x) || inherits(x, "cograph_network") ||
             inherits(x, "igraph")) {
    has_units <- FALSE
  } else {
    stop("Invalid input. Provide a tna object, matrix, cograph_network, igraph, ",
         "or data.frame with 'data' and 'id' arguments.")
  }

  if (is.null(level)) {
    level <- if (has_units) "individual" else "aggregate"
  } else {
    level <- match.arg(level, c("individual", "aggregate"))
    if (level == "individual" && !has_units) {
      warning("Individual level requested but no individual data available. Using aggregate.")
      level <- "aggregate"
    }
  }

  if (!has_units) {
    # Matrices, networks and igraph objects are one aggregate network.
    net <- as_cograph(x)
    mat <- to_matrix(net)
    dimnames(mat) <- list(get_labels(net), get_labels(net))
    return(list(x = mat, actor = NULL, level = "aggregate"))
  }
  if (!is.null(data) && !inherits(x, "tna")) {
    if (level == "individual") {
      return(list(x = el, actor = ".unit", level = level))
    }
    el$.unit <- NULL
    return(list(x = el, actor = NULL, level = level))
  }
  if (level == "individual") {
    return(list(x = x, actor = NULL, level = level))
  }
  init_fn <- .get_tna_initialize_model()
  trans <- init_fn(x$data, attr(x, "type"), attr(x, "scaling"),
                   attr(x, "params"), transitions = TRUE)$trans
  pooled <- apply(trans, c(2, 3), sum)
  dimnames(pooled) <- list(x$labels, x$labels)
  list(x = pooled, actor = NULL, level = "aggregate")
}

#' Tidy table of a motif extraction
#'
#' @param x A \code{cograph_motif_analysis} object from [extract_motifs()].
#' @param row.names,optional Ignored; present for the generic.
#' @param what \code{"results"} (default) for the triad table, or
#'   \code{"types"} for one row per MAN type with columns \code{type} and
#'   \code{count} (the number of unit-level occurrences of that type).
#' @param ... Ignored.
#' @return A data frame: the \code{results} table described in
#'   [extract_motifs()], or the per-type counts.
#' @examples
#' m <- extract_motifs(regulation_net, min_transitions = 0)
#' as.data.frame(m, what = "types")
#' @export
as.data.frame.cograph_motif_analysis <- function(x, row.names = NULL,
                                                 optional = FALSE,
                                                 what = c("results", "types"),
                                                 ...) {
  what <- match.arg(what)
  if (what == "types") {
    return(data.frame(type = names(x$type_summary),
                      count = as.numeric(x$type_summary),
                      stringsAsFactors = FALSE))
  }
  x$results
}

#' Print motif results
#'
#' Print methods for the results of [motifs()] and [subgraphs()]
#' (\code{cograph_motif_result}), [extract_motifs()]
#' (\code{cograph_motif_analysis}) and [motif_census()]
#' (\code{cograph_motifs}). Each shows the analysis settings, the number of
#' units analysed where there are several, and the leading rows of the
#' results table. \code{as.data.frame()} returns the full table.
#'
#' @param x A motif result.
#' @param n Number of triad rows to show. Default 20.
#' @param ... Passed to \code{print.data.frame()} for \code{cograph_motifs};
#'   ignored otherwise.
#' @return \code{x}, invisibly.
#' @examples
#' print(extract_motifs(regulation_net, min_transitions = 0), n = 5)
#' @name print-motifs
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
  n_triads <- x$params$n_triads %||% nrow(x$results)
  if (identical(x$params$level, "individual")) {
    cat(sprintf("Units: %d analysed of %d | ",
                x$params$n_units %||% x$params$n_individuals,
                x$params$n_individuals))
  } else {
    cat("Aggregate network | ")
  }
  cat(sprintf("States: %d | Triads: %d shown of %d\n\n",
              x$params$n_states, nrow(x$results), n_triads))

  cat("Type distribution:\n")
  print(x$type_summary)

  show_n <- min(n, nrow(x$results))
  cat(sprintf("\nTop %d triads:\n", show_n))

  cols <- c("triad", "type", "observed",
            if (isTRUE(x$params$significance)) c("expected", "z", "p", "sig"))
  print(utils::head(x$results[cols], n))

  invisible(x)
}

#' @rdname plot-results
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
