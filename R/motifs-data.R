# Shared constants and helpers for motif analysis
# Used across: motifs.R, motifs-plot.R, motifs-extract.R

# Cache environment for memoization
.cograph_cache <- new.env(parent = emptyenv())

#' Get triad patterns for visualization (column-major, no byrow)
#' Used by .plot_motifs_network, .plot_triad_networks, .plot_motif_patterns
#' @noRd
.get_triad_patterns_visual <- function() {
  list(
    "003" = matrix(c(0L,0L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "012" = matrix(c(0L,1L,0L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "102" = matrix(c(0L,1L,0L, 1L,0L,0L, 0L,0L,0L), 3, 3),
    "021D" = matrix(c(0L,0L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "021U" = matrix(c(0L,1L,1L, 0L,0L,0L, 0L,0L,0L), 3, 3),
    "021C" = matrix(c(0L,1L,0L, 0L,0L,1L, 0L,0L,0L), 3, 3),
    "111D" = matrix(c(0L,1L,1L, 1L,0L,0L, 0L,0L,0L), 3, 3),
    "111U" = matrix(c(0L,1L,0L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "030T" = matrix(c(0L,1L,1L, 0L,0L,1L, 0L,0L,0L), 3, 3),
    "030C" = matrix(c(0L,1L,0L, 0L,0L,1L, 1L,0L,0L), 3, 3),
    "201" = matrix(c(0L,1L,1L, 1L,0L,0L, 1L,0L,0L), 3, 3),
    "120D" = matrix(c(0L,1L,1L, 1L,0L,1L, 0L,0L,0L), 3, 3),
    "120U" = matrix(c(0L,1L,0L, 1L,0L,0L, 1L,1L,0L), 3, 3),
    "120C" = matrix(c(0L,1L,0L, 1L,0L,1L, 1L,0L,0L), 3, 3),
    "210" = matrix(c(0L,1L,0L, 1L,0L,1L, 1L,1L,0L), 3, 3),
    "300" = matrix(c(0L,1L,1L, 1L,0L,1L, 1L,1L,0L), 3, 3)
  )
}

#' Get triad patterns for canonical lookup (row-major, byrow = TRUE)
#' Verified against igraph - used by .build_triad_lookup
#' @noRd
.get_triad_patterns_canonical <- function() {
  list(
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
}

#' Get MAN type descriptions
#'
#' Each name describes the structure of its class (igraph's
#' `triad_census()` definitions), not the D/U letter: in 111D and 120D the
#' lone asymmetric tie(s) point *into* the mutual pair, so 111D holds an
#' in-star and 120D an out-star onto the pair. test-motifs-triad-patterns.R
#' checks every name against its pattern matrix.
#' @noRd
.get_man_descriptions <- function() {
  c(
    "003" = "Empty",
    "012" = "Single edge",
    "102" = "Mutual pair",
    "021D" = "Out-star",
    "021U" = "In-star",
    "021C" = "Chain",
    "111D" = "In-star + mutual",
    "111U" = "Out-star + mutual",
    "030T" = "Feed-forward",
    "030C" = "Cycle",
    "201" = "Two mutuals",
    "120D" = "Out-star onto mutual",
    "120U" = "In-star from mutual",
    "120C" = "Cycle + mutual",
    "210" = "Near-clique",
    "300" = "Clique"
  )
}

#' Get pattern filter definitions for motif extraction
#' @return list with triangle_types, all_types, network_exclude, closed_exclude
#' @noRd
.get_pattern_filters <- function() {
  all_types <- c("003", "012", "102", "021D", "021U", "021C",
                 "111D", "111U", "030T", "030C", "201",
                 "120D", "120U", "120C", "210", "300")
  triangle_types <- c("030C", "030T", "120C", "120D", "120U", "210", "300")
  list(
    all_types = all_types,
    triangle_types = triangle_types,
    network_exclude = c("003", "012", "021C"),
    closed_exclude = setdiff(all_types, triangle_types)
  )
}

#' Get motif names based on size and directedness
#' @noRd
.get_motif_names <- function(size, directed) {
  if (size == 3 && directed) {
    c("003", "012", "102", "021D", "021U", "021C", "111D", "111U",
      "030T", "030C", "201", "120D", "120U", "120C", "210", "300")
  } else if (size == 3 && !directed) {
    c("empty", "edge", "wedge", "triangle")
  } else if (size == 4) {
    # igraph returns every isomorphism-class slot, including disconnected
    # slots represented by NA. Directed four-node graphs have 218 slots (199
    # are connected), so names must cover the full positional result.
    paste0("M", seq_len(if (directed) 218 else 11))
  } else {
    paste0("motif_", seq_len(100))
  }
}

#' Helper to get tna internal function without using :::
#' Uses getFromNamespace which is CRAN-acceptable
#' @noRd
.get_tna_initialize_model <- function() {
  if (!requireNamespace("tna", quietly = TRUE)) {
    stop("Package 'tna' required for this operation", call. = FALSE) # nocov
  }
  get("initialize_model", envir = asNamespace("tna"))
}

#' Detect whether x$data on a cograph_network looks like tna sequence data
#'
#' Returns TRUE if `raw_data` is a data.frame or matrix whose cell values are a
#' subset of the network's node labels (the canonical tna stslist shape: rows =
#' subjects, columns = time steps). Used to route Nestimate `build_tna` and
#' similar objects through the individual-level tna path in `motifs()`.
#' @noRd
.is_tna_sequence_data <- function(raw_data, labels) {
  if (is.null(raw_data) || is.null(labels)) return(FALSE)
  if (!(is.data.frame(raw_data) || is.matrix(raw_data))) return(FALSE)
  if (is.data.frame(raw_data) &&
      all(c("from", "to") %in% tolower(names(raw_data)))) {
    return(FALSE)
  }
  if (NROW(raw_data) < 1L || NCOL(raw_data) < 2L) return(FALSE)
  vals <- unique(as.character(unlist(raw_data, use.names = FALSE)))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0L) return(FALSE)
  all(vals %in% as.character(labels))
}

# A window cuts one actor's transitions into units. Without an actor column
# there is no unit to cut, and pooling the windows into one "aggregate"
# network would mix counts from many windows with a null built from the
# first one.
# @noRd
.motif_check_window_actor <- function(window, actor_col) {
  if (!is.null(window) && is.null(actor_col)) {
    stop(errorCondition(
      paste0("`window` needs an actor or session column to split. Pass ",
             "`actor`, or add a column such as `session`, `actor` or `id`."),
      class = "cograph_window_needs_actor", call = NULL))
  }
  invisible(TRUE)
}

# The p-value that decides significance: adjusted when the result carries a
# p_adj column (motifs(p_adjust = ...)), raw otherwise. Plot colours and
# stars use it, so they always agree with the `sig` column.
# @noRd
.motif_decision_p <- function(results) {
  if ("p_adj" %in% names(results)) results$p_adj else results$p
}

# Legend wording for the decision p-value.
# @noRd
.motif_p_label <- function(x) {
  adj <- x$params$p_adjust %||% "none"
  if (identical(adj, "none")) "p" else "adjusted p"
}

#' Shared base ggplot2 theme for motif plots
#' @param base_size Base font size. Default 12.
#' @noRd
.motifs_ggplot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold")
    )
}

#' Auto-detect actor/session grouping column in a data.frame
#' Returns the original column name (preserving case), or NULL
#' @noRd
.detect_actor_column <- function(df) {
  if (!is.data.frame(df)) return(NULL)
  nms <- names(df)
  nms_lower <- tolower(nms)
  # Priority order: session_id > session > actor > user > participant > individual > id
  candidates <- c("session_id", "session", "actor", "user",
                   "participant", "individual", "id")
  for (cand in candidates) {
    idx <- match(cand, nms_lower)
    if (!is.na(idx)) return(nms[idx])
  }
  NULL
}

#' Auto-detect ordering/time column in a data.frame
#' Returns the original column name (preserving case), or NULL
#' @noRd
.detect_order_column <- function(df) {
  if (!is.data.frame(df)) return(NULL)
  nms <- names(df)
  nms_lower <- tolower(nms)
  candidates <- c("timestamp", "time", "seq", "step", "order")
  for (cand in candidates) {
    idx <- match(cand, nms_lower)
    if (!is.na(idx)) return(nms[idx])
  }
  NULL
}

#' Convert edge list data.frame to 3D transition array
#' @return list with trans (3D array), labels (character), groups (character)
#' @noRd
.edgelist_to_trans_array <- function(el,
                                      actor_col = NULL,
                                      order_col = NULL,
                                      window = NULL,
                                      window_type = "rolling") {
  # Columns are matched case-insensitively: `from` and `to` are required; the
  # weight is `weight`, or `count` (the column get_edge_list() returns), or 1.
  nms <- tolower(names(el))
  from_col <- names(el)[match("from", nms)]
  to_col <- names(el)[match("to", nms)]
  if (is.na(from_col) || is.na(to_col)) {
    stop(errorCondition(
      "An edge list needs a `from` and a `to` column.",
      class = "cograph_bad_edgelist", call = NULL))
  }
  el_from <- as.character(el[[from_col]])
  el_to <- as.character(el[[to_col]])
  weight_col <- NULL
  wt_idx <- match("weight", nms)
  if (is.na(wt_idx)) wt_idx <- match("count", nms)
  if (!is.na(wt_idx)) weight_col <- names(el)[wt_idx]

  # Get all unique states
  labels <- sort(unique(c(el_from, el_to)))
  s <- length(labels)
  state_idx <- setNames(seq_along(labels), labels)

  # Assign group IDs
  if (!is.null(actor_col)) {
    group_ids <- as.character(el[[actor_col]])
  } else {
    group_ids <- rep("__all__", nrow(el))
  }

  # Order within groups if order column provided
  if (!is.null(order_col)) {
    sort_order <- order(group_ids, el[[order_col]])
    el <- el[sort_order, ]
    el_from <- el_from[sort_order]
    el_to <- el_to[sort_order]
    group_ids <- group_ids[sort_order]
  }

  # Apply windowing if requested
  if (!is.null(window) && is.numeric(window) && window > 0) {
    groups <- split(seq_len(nrow(el)), group_ids)
    new_rows <- list()
    new_group_ids <- character(0)

    for (gname in names(groups)) {
      idx <- groups[[gname]]
      n_edges <- length(idx)
      if (n_edges == 0) next # nocov

      if (window_type == "tumbling") {
        starts <- seq(1, n_edges, by = window)
      } else {
        starts <- seq_len(max(1, n_edges - window + 1))
      }

      for (wi in seq_along(starts)) {
        st <- starts[wi]
        en <- min(st + window - 1, n_edges)
        w_idx <- idx[st:en]
        new_rows <- c(new_rows, list(w_idx))
        new_group_ids <- c(new_group_ids, paste0(gname, "_w", wi))
      }
    }

    row_indices <- unlist(new_rows)
    el <- el[row_indices, ]
    el_from <- el_from[row_indices]
    el_to <- el_to[row_indices]
    group_ids <- rep(new_group_ids, lengths(new_rows))
  }

  # Build 3D array
  unique_groups <- unique(group_ids)
  n_groups <- length(unique_groups)
  group_idx <- match(group_ids, unique_groups)

  trans <- array(0, dim = c(n_groups, s, s))

  from_idx <- state_idx[el_from]
  to_idx <- state_idx[el_to]
  wt <- if (!is.null(weight_col)) as.numeric(el[[weight_col]]) else rep(1, nrow(el))

  # Vectorized fill using aggregate
  valid <- !is.na(from_idx) & !is.na(to_idx)
  agg_df <- data.frame(g = group_idx[valid], f = from_idx[valid],
                        t = to_idx[valid], w = wt[valid])
  if (nrow(agg_df) > 0) {
    agg <- stats::aggregate(w ~ g + f + t, data = agg_df, FUN = sum)
    trans[cbind(agg$g, agg$f, agg$t)] <- agg$w
  }

  list(trans = trans, labels = labels, groups = unique_groups)
}
