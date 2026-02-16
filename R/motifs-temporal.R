# Temporal motif analysis and triad persistence
# Contains: extract_motifs_temporal, triad_persistence, their print/plot methods,
#           .edge_list_to_windows, .long_to_wide

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
#' # Edge list format
#' edges <- data.frame(
#'   from = c("A", "B", "A", "C", "B", "A"),
#'   to = c("B", "C", "C", "A", "A", "B"),
#'   time = c(1, 1, 2, 2, 3, 3)
#' )
#' m <- extract_motifs_temporal(edges, from = "from", to = "to", time = "time",
#'                               window_size = 2)
#' }
#'
#' @seealso [extract_motifs()], [tna_windows()], [plot.cograph_temporal_motifs()],
#'   [triad_persistence()] for persistence analysis
#' @family motifs
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

  # Auto-detect format
  if (format == "auto" && is.data.frame(x)) {
    if (!is.null(from) && !is.null(to)) {
      format <- "edge_list"
    } else if (!is.null(state)) {
      format <- "long"
    } else {
      format <- "wide"
    }
  }

  if (is.null(window_size)) {
    window_size <- if (format == "edge_list") 1L else 2L
  }

  # ========== FORMAT DETECTION & CONVERSION ==========

  if (inherits(x, "tna_windows") ||
      (is.list(x) && "windows" %in% names(x) && "start_times" %in% names(x))) {
    win_result <- x

  } else if (is.data.frame(x)) {

    if (format == "edge_list") {
      if (is.null(from)) from <- "from"
      if (is.null(to)) to <- "to"
      if (is.null(time)) time <- "time"

      required <- c(from, to, time)
      if (!all(required %in% names(x))) {
        stop("Missing columns: ", paste(setdiff(required, names(x)), collapse = ", "))
      }

      win_result <- .edge_list_to_windows(x, from = from, to = to, time = time,
                                           id = id, window_size = window_size,
                                           step = step)

    } else if (format == "long") {
      if (is.null(id)) stop("id required for long format")
      if (is.null(time)) time <- "time"
      if (is.null(state)) state <- "state"

      required <- c(id, time, state)
      if (!all(required %in% names(x))) {
        stop("Missing columns: ", paste(setdiff(required, names(x)), collapse = ", "))
      }

      wide_data <- .long_to_wide(x, id = id, time = time, state = state)
      win_result <- tna_windows(wide_data, window_size = window_size,
                                 step = step, na_threshold = na_threshold)

    } else {
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
  window_data <- lapply(seq_len(n_windows), function(i) {
    tna_model <- win_result$windows[[i]]

    weights_mat <- if (inherits(tna_model, "tna")) {
      tna_model$weights
    } else if (is.list(tna_model) && "weights" %in% names(tna_model)) {
      tna_model$weights
    } else {
      return(list(motifs = NULL, summary = NULL))
    }

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

    summary_row <- NULL
    if (!is.null(motifs)) {
      motifs$weights_matrix <- weights_mat

      # Compute raw transition counts from sequence data if available
      if (inherits(tna_model, "tna") && !is.null(tna_model$data)) {
        seq_data <- tna_model$data
        states <- rownames(weights_mat)
        n_states <- length(states)

        # Vectorized transition counting
        n_cols <- ncol(seq_data)
        count_mat <- matrix(0L, nrow = n_states, ncol = n_states,
                            dimnames = list(states, states))

        if (n_cols >= 2) {
          from_vals <- as.integer(seq_data[, -n_cols, drop = FALSE])
          to_vals <- as.integer(seq_data[, -1, drop = FALSE])
          valid <- !is.na(from_vals) & !is.na(to_vals) &
                   from_vals >= 1L & from_vals <= n_states &
                   to_vals >= 1L & to_vals <= n_states
          if (any(valid)) {
            lin_idx <- (to_vals[valid] - 1L) * n_states + from_vals[valid]
            tab <- tabulate(lin_idx, nbins = n_states * n_states)
            count_mat <- matrix(tab, nrow = n_states, ncol = n_states,
                                dimnames = list(states, states))
          }
        }
        motifs$counts_matrix <- count_mat
      }

      if (nrow(motifs$results) > 0) {
        type_counts <- table(motifs$results$type)
        summary_row <- data.frame(
          window = i,
          start = win_result$start_times[i],
          end = win_result$end_times[i],
          type = names(type_counts),
          count = as.integer(type_counts),
          stringsAsFactors = FALSE
        )
      }
    }

    list(motifs = motifs, summary = summary_row)
  })

  window_results <- lapply(window_data, `[[`, "motifs")
  summary_list <- lapply(window_data, `[[`, "summary")
  summary_list <- summary_list[!vapply(summary_list, is.null, logical(1))]

  # Combine summary
  summary_df <- if (length(summary_list) > 0) {
    do.call(rbind, summary_list)
  } else {
    data.frame(window = integer(0), start = integer(0), end = integer(0),
               type = character(0), count = integer(0))
  }

  # Create type trends (pivot to wide format - vectorized)
  type_trends <- data.frame(
    window = seq_len(n_windows),
    start = win_result$start_times,
    end = win_result$end_times
  )

  if (nrow(summary_df) > 0) {
    all_types <- unique(summary_df$type)
    type_cols <- lapply(stats::setNames(all_types, all_types), function(tp) {
      vapply(seq_len(n_windows), function(w) {
        rows <- summary_df$window == w & summary_df$type == tp
        if (any(rows)) sum(summary_df$count[rows]) else 0L
      }, integer(1))
    })
    type_trends <- cbind(type_trends, as.data.frame(type_cols, check.names = FALSE))
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
  times <- sort(unique(x[[time]]))
  states <- sort(unique(c(x[[from]], x[[to]])))
  n_states <- length(states)
  state_idx <- stats::setNames(seq_along(states), states)

  n_times <- length(times)
  if (n_times < window_size) {
    return(list(windows = list(), start_times = integer(0),
                end_times = integer(0), na_proportions = numeric(0)))
  }

  starts <- seq(1, n_times - window_size + 1, by = step)
  n_windows <- length(starts)

  windows <- lapply(seq_len(n_windows), function(w) {
    window_times <- times[starts[w]:(starts[w] + window_size - 1)]
    edges_w <- x[x[[time]] %in% window_times, ]

    mat <- matrix(0, n_states, n_states, dimnames = list(states, states))
    if (nrow(edges_w) > 0) {
      f_vals <- as.character(edges_w[[from]])
      t_vals <- as.character(edges_w[[to]])
      wt_vals <- if ("weight" %in% names(edges_w)) edges_w$weight else rep(1, nrow(edges_w))
      agg <- stats::aggregate(wt_vals, by = list(from = f_vals, to = t_vals), FUN = sum)
      mat[cbind(agg$from, agg$to)] <- agg$x
    }

    list(weights = mat, labels = states)
  })

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
  if (length(id) == 1) {
    x$.id <- x[[id]]
  } else {
    x$.id <- do.call(paste, c(x[id], sep = "_"))
  }

  unique_ids <- unique(x$.id)
  unique_times <- sort(unique(x[[time]]))

  # Vectorized: build each column using vapply
  wide_cols <- lapply(stats::setNames(unique_times, paste0("T", unique_times)), function(t_val) {
    vapply(unique_ids, function(uid) {
      val <- x[[state]][x$.id == uid & x[[time]] == t_val]
      if (length(val) == 0) NA_character_ else as.character(val[1])
    }, character(1))
  })

  as.data.frame(wide_cols, stringsAsFactors = FALSE)
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
#' @seealso [extract_motifs_temporal()] for the analysis that produces this object,
#'   [triad_persistence()] for persistence analysis
#' @family motifs
#' @method plot cograph_temporal_motifs
#' @export
plot.cograph_temporal_motifs <- function(x, type = c("trends", "heatmap"),
                                          top_n = 10, colors = NULL, ...) {
  type <- match.arg(type)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for plotting")
  }

  if (type == "trends") {
    trends <- x$type_trends
    type_cols <- setdiff(names(trends), c("window", "start", "end"))

    if (length(type_cols) == 0) {
      message("No types to plot")
      return(invisible(NULL))
    }

    totals <- colSums(trends[, type_cols, drop = FALSE])
    top_types <- names(sort(totals, decreasing = TRUE))[1:min(top_n, length(totals))]

    # Vectorized reshape to long format
    df_long <- do.call(rbind, lapply(top_types, function(tp) {
      data.frame(
        window = trends$window,
        type = tp,
        count = trends[[tp]],
        stringsAsFactors = FALSE
      )
    }))

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
      .motifs_ggplot_theme(12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )

    print(p)
    invisible(p)

  } else if (type == "heatmap") {
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
      .motifs_ggplot_theme(12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    print(p)
    invisible(p)
  }
}


# =============================================================================
# TRIAD PERSISTENCE
# =============================================================================

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
#' # By specific triads (default)
#' pers <- triad_persistence(m)
#' print(pers)
#' plot(pers, type = "heatmap", top_n = 20)
#'
#' # By MAN type
#' pers_type <- triad_persistence(m, by = "type")
#' plot(pers_type, type = "heatmap")
#' }
#'
#' @seealso [extract_motifs_temporal()], [plot.cograph_triad_persistence()],
#'   [extract_motifs()] for single-network motif analysis
#' @family motifs
#' @export
triad_persistence <- function(x, by = c("triad", "type"),
                              edge_weight = FALSE,
                              min_windows = 1, min_persistence = 0.5) {

  if (!inherits(x, "cograph_temporal_motifs")) {
    stop("x must be a cograph_temporal_motifs object from extract_motifs_temporal()")
  }

  by <- match.arg(by)
  n_windows <- x$params$n_windows

  # Collect all triads from all windows
  all_triads_list <- lapply(seq_len(n_windows), function(w) {
    motifs <- x$windows[[w]]
    if (is.null(motifs) || is.null(motifs$results) || nrow(motifs$results) == 0) return(NULL)

    df <- motifs$results
    df$window <- w

    # Compute actual transition counts
    if (!is.null(motifs$counts_matrix)) {
      mat <- motifs$counts_matrix
      df$edge_count <- vapply(seq_len(nrow(df)), function(i) {
        nodes <- strsplit(df$triad[i], " - ")[[1]]
        if (length(nodes) == 3 && all(nodes %in% rownames(mat))) {
          edges <- c(
            mat[nodes[1], nodes[2]], mat[nodes[2], nodes[1]],
            mat[nodes[1], nodes[3]], mat[nodes[3], nodes[1]],
            mat[nodes[2], nodes[3]], mat[nodes[3], nodes[2]]
          )
          nonzero <- edges[edges > 0]
          base_count <- if (length(nonzero) > 0) min(nonzero) else 0L

          if (edge_weight && base_count > 0) {
            n_edges <- sum(edges > 0)
            round(base_count * (n_edges / 3), 2)
          } else {
            base_count
          }
        } else {
          1L
        }
      }, numeric(1))
    } else if (!is.null(motifs$weights_matrix)) {
      mat <- motifs$weights_matrix
      df$edge_count <- vapply(seq_len(nrow(df)), function(i) {
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
      }, numeric(1))
    } else {
      df$edge_count <- df$observed
    }

    df[, c("triad", "type", "edge_count", "window")]
  })

  all_triads_list <- all_triads_list[!vapply(all_triads_list, is.null, logical(1))]

  if (length(all_triads_list) == 0) {
    warning("No triads found in any window")
    return(NULL)
  }

  all_triads <- do.call(rbind, all_triads_list)

  # Calculate window totals
  window_totals <- vapply(seq_len(n_windows), function(w) {
    sum(all_triads$edge_count[all_triads$window == w])
  }, numeric(1))
  names(window_totals) <- paste0("W", seq_len(n_windows))

  # Determine grouping key
  if (by == "type") {
    unique_items <- sort(unique(all_triads$type))
    group_col <- "type"
  } else {
    unique_items <- unique(all_triads$triad)
    group_col <- "triad"
  }

  # Build counts matrix (vectorized using aggregate)
  counts_matrix <- matrix(0L, nrow = length(unique_items), ncol = n_windows,
                          dimnames = list(unique_items, paste0("W", seq_len(n_windows))))

  agg_counts <- stats::aggregate(
    edge_count ~ window,
    data = cbind(all_triads, item = all_triads[[group_col]]),
    FUN = sum,
    subset = NULL
  )
  # Re-aggregate properly with group column
  agg_formula <- stats::as.formula(paste("edge_count ~ item + window"))
  agg_df <- stats::aggregate(agg_formula,
                              data = data.frame(item = all_triads[[group_col]],
                                                window = all_triads$window,
                                                edge_count = all_triads$edge_count),
                              FUN = sum)
  row_idx <- match(agg_df$item, unique_items)
  counts_matrix[cbind(row_idx, agg_df$window)] <- agg_df$edge_count

  # Build results using lapply
  results_list <- lapply(seq_along(unique_items), function(i) {
    item <- unique_items[i]
    windows_present <- which(counts_matrix[i, ] > 0)

    if (by == "type") {
      type_val <- item
    } else {
      subset_df <- all_triads[all_triads$triad == item, ]
      type_counts <- table(subset_df$type)
      type_val <- names(type_counts)[which.max(type_counts)]
    }

    n_win <- length(windows_present)
    first_w <- if (n_win > 0) min(windows_present) else NA_integer_
    last_w <- if (n_win > 0) max(windows_present) else NA_integer_
    pers <- n_win / n_windows
    total_ct <- sum(counts_matrix[i, ])

    # Classify status
    status <- if (pers >= min_persistence) {
      "persistent"
    } else if (n_win == 1) {
      "transient"
    } else if (!is.na(first_w) && first_w > 1 && last_w == n_windows) {
      "emerging"
    } else if (!is.na(first_w) && first_w == 1 && last_w < n_windows) {
      "fading"
    } else {
      "sporadic"
    }

    data.frame(
      label = item,
      type = type_val,
      n_windows = n_win,
      first_window = first_w,
      last_window = last_w,
      windows = paste(windows_present, collapse = ","),
      persistence = pers,
      total_count = total_ct,
      status = status,
      stringsAsFactors = FALSE
    )
  })

  results <- do.call(rbind, results_list)

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

  # For backwards compatibility
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
#'   in each window. Default is `FALSE` for raw counts.
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
#' pers <- triad_persistence(m)
#' plot(pers, type = "heatmap", top_n = 20)
#'
#' pers_type <- triad_persistence(m, by = "type")
#' plot(pers_type, type = "heatmap")
#' plot(pers_type, type = "heatmap", normalize = TRUE)
#'
#' plot(pers, type = "timeline")
#' plot(pers, type = "status")
#' }
#'
#' @seealso [triad_persistence()] for the analysis that produces this object,
#'   [extract_motifs_temporal()] for temporal motif extraction
#' @family motifs
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
    n_show <- if (is.null(top_n)) nrow(x$triads) else min(top_n, nrow(x$triads))
    df <- x$triads[seq_len(n_show), ]

    mat <- x$counts_matrix[df$label, , drop = FALSE]

    n_triads <- nrow(mat)
    n_windows <- ncol(mat)

    # Vectorized normalization
    if (normalize && !is.null(x$window_totals)) {
      totals <- x$window_totals
      totals[totals == 0] <- 1  # Avoid division by zero
      mat <- sweep(mat, 2, totals, "/")
    }

    display_mat <- if (normalize) round(mat * 100, 1) else mat

    # Adaptive sizing
    y_text_size <- if (n_triads <= 10) 10 else if (n_triads <= 20) 8 else if (n_triads <= 40) 6 else 5
    x_text_size <- if (n_windows <= 10) 10 else if (n_windows <= 20) 8 else 6
    tile_border <- if (n_triads * n_windows <= 100) 0.5 else if (n_triads * n_windows <= 400) 0.3 else 0.1

    # Truncate long triad names
    max_label_len <- if (n_triads <= 15) 50 else if (n_triads <= 30) 30 else 20
    triad_labels <- df$triad
    if (any(nchar(triad_labels) > max_label_len)) {
      triad_labels <- ifelse(nchar(triad_labels) > max_label_len,
                             paste0(substr(triad_labels, 1, max_label_len - 2), ".."),
                             triad_labels)
      if (any(duplicated(triad_labels))) {
        dups <- duplicated(triad_labels) | duplicated(triad_labels, fromLast = TRUE)
        triad_labels[dups] <- paste0(triad_labels[dups], " (", seq_along(which(dups)), ")")
      }
    }

    # Calculate fill values
    if (fill == "density") {
      global_max <- max(mat, na.rm = TRUE)
      fill_mat <- if (global_max > 0) mat / global_max else mat
      legend_name <- if (normalize) "% of Window" else "Density"
    } else {
      fill_mat <- (mat > 0) * 1
      legend_name <- NULL
    }

    # Vectorized reshape using expand.grid
    raw_counts_mat <- x$counts_matrix[df$label, , drop = FALSE]
    grid <- expand.grid(row = seq_len(n_triads), col = seq_len(n_windows))
    plot_df <- data.frame(
      triad = triad_labels[grid$row],
      window = grid$col,
      count = display_mat[cbind(grid$row, grid$col)],
      raw_count = raw_counts_mat[cbind(grid$row, grid$col)],
      fill_val = fill_mat[cbind(grid$row, grid$col)],
      stringsAsFactors = FALSE
    )

    plot_df$triad <- factor(plot_df$triad, levels = rev(triad_labels))

    # Build the plot
    if (fill == "density") {
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
      plot_df$present_factor <- factor(ifelse(plot_df$raw_count > 0, "1", "0"), levels = c("0", "1"))

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = factor(.data$window),
                                                  y = .data$triad,
                                                  fill = .data$present_factor)) +
        ggplot2::geom_tile(color = if (tile_border > 0.1) "white" else NA,
                           linewidth = tile_border) +
        ggplot2::scale_fill_manual(values = c("0" = "#f5f5f5", "1" = "#800020"),
                                    labels = c("Absent", "Present"), name = NULL)
    }

    # Show counts
    n_cells <- n_triads * n_windows
    if (is.null(show_counts)) {
      show_counts <- n_cells <= 200
    }

    if (show_counts) {
      label_df <- plot_df[plot_df$raw_count > 0, ]

      count_text_size <- if (n_cells <= 50) 3.5 else if (n_cells <= 100) 3 else if (n_cells <= 200) 2.5 else 2

      if (normalize) {
        label_df$label_text <- paste0(round(label_df$count, 0), "%")
      } else {
        label_df$label_text <- as.character(as.integer(label_df$count))
      }

      if (fill == "density") {
        label_df$text_color <- ifelse(label_df$fill_val > 0.5, "white", "#333333")
      } else {
        label_df$text_color <- "white"
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

    # Aspect ratio
    aspect_ratio <- n_windows / n_triads
    coord_ratio <- if (aspect_ratio > 3) 0.5 else if (aspect_ratio < 0.3) 2 else NULL

    by_mode <- x$params$by
    title_label <- if (by_mode == "type") "MAN Type" else "Triad"
    title_suffix <- if (normalize) " (Normalized)" else ""
    row_label <- if (by_mode == "type") "types" else "triads"

    p <- p +
      ggplot2::labs(title = paste0(title_label, " Persistence Heatmap", title_suffix),
                    subtitle = sprintf("%d %s \u00d7 %d windows", n_triads, row_label, n_windows),
                    x = "Time Window", y = NULL) +
      .motifs_ggplot_theme(11) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = y_text_size),
        axis.text.x = ggplot2::element_text(size = x_text_size),
        legend.position = if (n_triads > 30) "right" else "bottom",
        panel.grid = ggplot2::element_blank()
      )

    if (!is.null(coord_ratio)) {
      p <- p + ggplot2::coord_fixed(ratio = coord_ratio)
    }

    print(p)
    invisible(p)

  } else if (type == "timeline") {
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
      .motifs_ggplot_theme(11) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        legend.position = "right"
      )

    print(p)
    invisible(p)

  } else if (type == "status") {
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
      .motifs_ggplot_theme(12) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0, max(status_df$count) * 1.15)

    print(p)
    invisible(p)
  }
}
