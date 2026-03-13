# Unified motifs API
# Contains: motifs(), subgraphs(), print/plot methods for cograph_motif_result

#' Network Motif Analysis
#'
#' Two modes of motif analysis for networks:
#' \itemize{
#'   \item **Census** (\code{named_nodes = FALSE}, default): Counts MAN type
#'     frequencies with significance testing. Nodes are exchangeable.
#'   \item **Instances** (\code{named_nodes = TRUE}, or use \code{subgraphs()}):
#'     Lists specific node triples forming each pattern. Nodes are NOT
#'     exchangeable.
#' }
#'
#' Detects input type and analysis level automatically. For inputs with
#' individual/group data (tna objects, cograph networks from edge lists with
#' metadata), performs per-group analysis. For aggregate inputs (matrices,
#' igraph), analyzes the single network.
#'
#' @param x Input data: a tna object, cograph_network, matrix, igraph, or
#'   data.frame (edge list).
#' @param named_nodes Logical. If FALSE (default), performs census (type-level
#'   counts). If TRUE, extracts specific node triples (instance-level).
#'   \code{subgraphs()} is a convenience wrapper that sets this to TRUE.
#' @param actor Character. Column name in the edge list metadata to group by.
#'   If NULL (default), auto-detects standard column names (session_id, session,
#'   actor, user, participant). If no grouping column found, performs aggregate
#'   analysis.
#' @param window Numeric. Window size for windowed analysis. Splits each actor's
#'   transitions into windows of this size. NULL (default) means no windowing.
#' @param window_type Character. Window type: "rolling" (default) or "tumbling".
#'   Only used when \code{window} is set.
#' @param pattern Pattern filter: "triangle" (default), "network", "closed", "all".
#' @param include Character vector of MAN types to include exclusively.
#'   Overrides \code{pattern}.
#' @param exclude Character vector of MAN types to exclude. Applied after
#'   \code{pattern} filter.
#' @param significance Logical. Run permutation significance test? Default TRUE.
#' @param n_perm Number of permutations for significance. Default 1000.
#' @param min_count Minimum observed count to include a triad (instance mode
#'   only). Default 5 for instances, NULL for census.
#' @param edge_method Method for determining edge presence: "any" (default),
#'   "expected", or "percent".
#' @param edge_threshold Threshold for "expected" or "percent" methods. Default 1.5.
#' @param min_transitions Minimum total transitions for a unit to be included.
#'   Default 5.
#' @param top Return only the top N results. NULL returns all.
#' @param seed Random seed for reproducibility.
#'
#' @return A \code{cograph_motif_result} object with:
#'   \describe{
#'     \item{results}{Data frame of results. Census: type, count, (z, p, sig).
#'       Instances: triad, type, observed, (z, p, sig).}
#'     \item{type_summary}{Named counts by MAN type}
#'     \item{level}{Analysis level: "individual" or "aggregate"}
#'     \item{named_nodes}{Whether nodes are identified (TRUE) or exchangeable (FALSE)}
#'     \item{n_units}{Number of units analyzed}
#'     \item{params}{List of parameters used}
#'   }
#'
#' @examples
#' # Census from a matrix
#' mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
#' motifs(mat, significance = FALSE)
#'
#' \dontrun{
#' # Census from tna object
#' library(tna)
#' Mod <- tna(coding)
#' motifs(Mod)
#'
#' # Instances: specific node triples
#' subgraphs(Mod)
#' }
#'
#' @seealso [subgraphs()], [motif_census()], [extract_motifs()]
#' @family motifs
#' @export
motifs <- function(x,
                   named_nodes = FALSE,
                   actor = NULL,
                   window = NULL,
                   window_type = c("rolling", "tumbling"),
                   pattern = c("triangle", "network", "closed", "all"),
                   include = NULL,
                   exclude = NULL,
                   significance = TRUE,
                   n_perm = 1000L,
                   min_count = if (named_nodes) 5L else NULL,
                   edge_method = c("any", "expected", "percent"),
                   edge_threshold = 1.5,
                   min_transitions = 5,
                   top = NULL,
                   seed = NULL) {

  .user_set_pattern <- !missing(pattern)

  window_type <- match.arg(window_type)
  pattern <- match.arg(pattern)
  edge_method <- match.arg(edge_method)

  if (!is.null(seed)) {
    saved_rng <- .save_rng()
    on.exit(.restore_rng(saved_rng), add = TRUE)
    set.seed(seed)
  }

  # Pattern filtering
  pf <- .get_pattern_filters()
  if (!is.null(include)) {
    final_exclude <- character(0)
    final_include <- include
  } else {
    final_include <- NULL
    pattern_exclude <- switch(pattern,
      triangle = setdiff(pf$all_types, pf$triangle_types),
      network = pf$network_exclude,
      closed = pf$closed_exclude,
      all = character(0)
    )
    final_exclude <- unique(c(pattern_exclude, exclude))
  }

  # ================================================================
  # INPUT DISPATCH
  # ================================================================

  trans <- NULL
  labels <- NULL
  level <- "aggregate"
  n_units <- 1L

  # --- Case 1: tna object ---
  if (inherits(x, "tna")) {
    init_fn <- .get_tna_initialize_model()
    model <- init_fn(x$data, attr(x, "type"), attr(x, "scaling"),
                     attr(x, "params"), transitions = TRUE)
    trans <- model$trans
    labels <- x$labels
    level <- "individual"
    n_units <- dim(trans)[1]

  # --- Case 2: cograph_network ---
  } else if (inherits(x, "cograph_network")) {
    raw_data <- x$data

    if (is.data.frame(raw_data) &&
        all(c("from", "to") %in% tolower(names(raw_data)))) {

      actor_col <- actor
      if (is.null(actor_col)) {
        actor_col <- .detect_actor_column(raw_data)
      }

      if (!is.null(actor_col)) {
        order_col <- .detect_order_column(raw_data)

        result <- .edgelist_to_trans_array(
          raw_data,
          actor_col = actor_col,
          order_col = order_col,
          window = window,
          window_type = window_type
        )
        trans <- result$trans
        labels <- result$labels
        level <- "individual"
        n_units <- dim(trans)[1]
      } else {
        mat <- to_matrix(x)
        labels <- get_labels(x)
        trans <- array(mat, dim = c(1, nrow(mat), ncol(mat)))
      }
    } else {
      mat <- to_matrix(x)
      labels <- get_labels(x)
      trans <- array(mat, dim = c(1, nrow(mat), ncol(mat)))
    }

  # --- Case 3: data.frame edge list ---
  } else if (is.data.frame(x)) {
    actor_col <- actor
    if (is.null(actor_col)) {
      actor_col <- .detect_actor_column(x)
    }
    order_col <- .detect_order_column(x)

    result <- .edgelist_to_trans_array(
      x,
      actor_col = actor_col,
      order_col = order_col,
      window = window,
      window_type = window_type
    )
    trans <- result$trans
    labels <- result$labels
    level <- if (!is.null(actor_col)) "individual" else "aggregate"
    n_units <- dim(trans)[1]

  # --- Case 4: matrix ---
  } else if (is.matrix(x)) {
    if (is.null(rownames(x))) {
      labels <- paste0("V", seq_len(nrow(x)))
    } else {
      labels <- rownames(x)
    }
    trans <- array(x, dim = c(1, nrow(x), ncol(x)))

  # --- Case 5: igraph ---
  } else if (inherits(x, "igraph")) {
    if ("weight" %in% igraph::edge_attr_names(x)) {
      mat <- as.matrix(igraph::as_adjacency_matrix(x, attr = "weight",
                                                     sparse = FALSE))
    } else {
      mat <- as.matrix(igraph::as_adjacency_matrix(x, sparse = FALSE))
    }
    labels <- igraph::V(x)$name
    if (is.null(labels)) labels <- paste0("V", seq_len(nrow(mat)))
    trans <- array(mat, dim = c(1, nrow(mat), ncol(mat)))

  } else {
    stop("Unsupported input type. Provide a tna object, cograph_network, ",
         "matrix, igraph, or data.frame edge list.")
  }

  # ================================================================
  # TRIAD COUNTING
  # ================================================================

  s <- length(labels)

  if (!named_nodes) {
    # ---- CENSUS MODE: count MAN type frequencies per unit ----
    type_counts_per_unit <- lapply(seq_len(dim(trans)[1]), function(ind) {
      mat <- trans[ind, , ]
      if (sum(mat) < min_transitions) return(NULL)

      expected_mat <- NULL
      if (edge_method == "expected") {
        total_mat <- sum(mat)
        row_sums <- rowSums(mat)
        col_sums <- colSums(mat)
        expected_mat <- outer(row_sums, col_sums) / total_mat
        expected_mat[expected_mat == 0] <- 0.001
      }

      counted <- .count_triads_matrix_vectorized(
        mat, edge_method, edge_threshold,
        expected_mat = expected_mat,
        exclude = final_exclude,
        include = final_include
      )
      if (is.null(counted) || nrow(counted) == 0) return(NULL)
      table(counted$type)
    })

    # Aggregate: sum type counts across units
    all_types <- unique(unlist(lapply(type_counts_per_unit, names)))
    if (length(all_types) == 0) {
      message("No motifs found with the given parameters.")
      return(NULL)
    }

    type_totals <- setNames(integer(length(all_types)), all_types)
    for (tc in type_counts_per_unit) {
      if (!is.null(tc)) {
        for (nm in names(tc)) type_totals[nm] <- type_totals[nm] + tc[nm]
      }
    }

    results <- data.frame(
      type = names(type_totals),
      count = as.integer(type_totals),
      stringsAsFactors = FALSE
    )
    results <- results[order(results$count, decreasing = TRUE), ]
    rownames(results) <- NULL

    # ---- CENSUS SIGNIFICANCE ----
    if (significance) {
      if (level == "aggregate") {
        # Delegate to motif_census which uses igraph
        agg_mat <- trans[1, , ]
        rownames(agg_mat) <- colnames(agg_mat) <- labels
        mc <- motif_census(agg_mat, n_random = n_perm, seed = seed)

        results$expected <- NA_real_
        results$z <- NA_real_
        results$p <- NA_real_
        results$sig <- NA

        for (ri in seq_len(nrow(results))) {
          tp <- results$type[ri]
          if (tp %in% names(mc$z_scores)) {
            results$expected[ri] <- round(mc$null_mean[tp], 1)
            results$z[ri] <- round(mc$z_scores[tp], 2)
            results$p[ri] <- round(mc$p_values[tp], 4)
            results$sig[ri] <- abs(mc$z_scores[tp]) > 1.96
          }
        }
        results <- results[order(abs(results$z), decreasing = TRUE), ]
        rownames(results) <- NULL

      } else {
        # Individual: config model on weighted matrices
        null_matrix <- matrix(0, nrow = nrow(results), ncol = n_perm)

        n_ind_c <- dim(trans)[1]
        ind_totals_c <- integer(n_ind_c)
        rows_stubs_c <- vector("list", n_ind_c)
        cols_stubs_c <- vector("list", n_ind_c)
        for (ind in seq_len(n_ind_c)) {
          mat_c <- trans[ind, , ]
          rs_c <- as.integer(rowSums(mat_c))
          cs_c <- as.integer(colSums(mat_c))
          ind_totals_c[ind] <- sum(rs_c)
          rows_stubs_c[[ind]] <- rep(seq_len(s), times = rs_c)
          cols_stubs_c[[ind]] <- rep(seq_len(s), times = cs_c)
        }
        valid_c <- which(ind_totals_c >= min_transitions)
        ss_c <- as.integer(s * s)

        for (perm in seq_len(n_perm)) {
          perm_totals <- setNames(integer(nrow(results)), results$type)

          for (ind in valid_c) {
            rs_c <- rows_stubs_c[[ind]]
            cs_c <- cols_stubs_c[[ind]]
            cs_shuf <- sample(cs_c)
            lin_c <- (cs_shuf - 1L) * s + rs_c
            perm_mat <- matrix(tabulate(lin_c, nbins = ss_c), s, s)

            expected_mat <- NULL
            if (edge_method == "expected") {
              total_mat <- sum(perm_mat)
              row_sums <- rowSums(perm_mat)
              col_sums <- colSums(perm_mat)
              if (total_mat > 0) {
                expected_mat <- outer(row_sums, col_sums) / total_mat
                expected_mat[expected_mat == 0] <- 0.001
              }
            }

            counted <- .count_triads_matrix_vectorized(
              perm_mat, edge_method, edge_threshold,
              expected_mat = expected_mat,
              exclude = final_exclude,
              include = final_include
            )
            if (!is.null(counted) && nrow(counted) > 0) {
              tc <- table(counted$type)
              for (nm in names(tc)) {
                if (nm %in% names(perm_totals)) {
                  perm_totals[nm] <- perm_totals[nm] + tc[nm]
                }
              }
            }
          }
          null_matrix[, perm] <- perm_totals
        }

        null_mean <- rowMeans(null_matrix)
        null_sd <- apply(null_matrix, 1, stats::sd)
        null_sd[null_sd == 0] <- 1

        results$expected <- round(null_mean, 1)
        results$z <- round((results$count - null_mean) / null_sd, 2)
        results$p <- round(2 * stats::pnorm(-abs(results$z)), 4)
        results$sig <- results$p < 0.05

        results <- results[order(abs(results$z), decreasing = TRUE), ]
        rownames(results) <- NULL
      }
    }

  } else {
    # ---- INSTANCE MODE: list specific node triples ----
    all_results <- lapply(seq_len(dim(trans)[1]), function(ind) {
      mat <- trans[ind, , ]
      if (sum(mat) < min_transitions) return(NULL)

      expected_mat <- NULL
      if (edge_method == "expected") {
        total_mat <- sum(mat)
        row_sums <- rowSums(mat)
        col_sums <- colSums(mat)
        expected_mat <- outer(row_sums, col_sums) / total_mat
        expected_mat[expected_mat == 0] <- 0.001
      }

      counted <- .count_triads_matrix_vectorized(
        mat, edge_method, edge_threshold,
        expected_mat = expected_mat,
        exclude = final_exclude,
        include = final_include
      )
      if (is.null(counted) || nrow(counted) == 0) return(NULL)

      triads <- vapply(seq_len(nrow(counted)), function(r) {
        paste(labels[counted$i[r]], labels[counted$j[r]],
              labels[counted$k[r]], sep = " - ")
      }, character(1))

      data.frame(unit = ind, triad = triads, type = counted$type,
                 stringsAsFactors = FALSE)
    })

    combined <- do.call(rbind, all_results)

    if (is.null(combined) || nrow(combined) == 0) {
      message("No motifs found with the given parameters.")
      return(NULL)
    }

    # Aggregate across units
    if (level == "individual") {
      obs <- stats::aggregate(unit ~ triad, data = combined, FUN = length)
      names(obs)[2] <- "observed"
      type_map <- stats::aggregate(
        type ~ triad, data = combined,
        FUN = function(tt) names(sort(table(tt), decreasing = TRUE))[1]
      )
      results <- merge(obs, type_map, by = "triad")
      results <- results[order(results$observed, decreasing = TRUE), ]
    } else {
      results <- data.frame(
        triad = unique(combined$triad),
        type = combined$type[!duplicated(combined$triad)],
        observed = 1L,
        stringsAsFactors = FALSE
      )
    }
    rownames(results) <- NULL

    # ---- INSTANCE SIGNIFICANCE (exact configuration model) ----
    if (significance && level == "individual") {
      if (!is.null(min_count)) {
        candidates <- results[results$observed > min_count, ]
      } else {
        candidates <- results
      }

      if (nrow(candidates) > 0) {
        triad_idx <- do.call(rbind, lapply(
          strsplit(candidates$triad, " - "),
          function(nodes) match(nodes, labels)
        ))
        n_cand <- nrow(triad_idx)
        ss <- as.integer(s * s)

        # Pre-compute linear indices for 6 edge positions
        lin_ij <- (triad_idx[, 2] - 1L) * s + triad_idx[, 1]
        lin_ji <- (triad_idx[, 1] - 1L) * s + triad_idx[, 2]
        lin_ik <- (triad_idx[, 3] - 1L) * s + triad_idx[, 1]
        lin_ki <- (triad_idx[, 1] - 1L) * s + triad_idx[, 3]
        lin_jk <- (triad_idx[, 3] - 1L) * s + triad_idx[, 2]
        lin_kj <- (triad_idx[, 2] - 1L) * s + triad_idx[, 3]

        # Pre-compute per-individual stubs
        n_ind <- dim(trans)[1]
        ind_totals <- integer(n_ind)
        rows_stubs <- vector("list", n_ind)
        cols_stubs <- vector("list", n_ind)
        active_row <- matrix(FALSE, n_ind, s)
        active_col <- matrix(FALSE, n_ind, s)

        for (ind in seq_len(n_ind)) {
          mat_i <- trans[ind, , ]
          rs <- as.integer(rowSums(mat_i))
          cs <- as.integer(colSums(mat_i))
          ind_totals[ind] <- sum(rs)
          rows_stubs[[ind]] <- rep(seq_len(s), times = rs)
          cols_stubs[[ind]] <- rep(seq_len(s), times = cs)
          active_row[ind, ] <- rs > 0L
          active_col[ind, ] <- cs > 0L
        }
        valid_inds <- which(ind_totals >= max(3L, min_transitions))

        # Per-individual candidate mask
        ri <- active_row[, triad_idx[, 1], drop = FALSE]
        rj <- active_row[, triad_idx[, 2], drop = FALSE]
        rk <- active_row[, triad_idx[, 3], drop = FALSE]
        ci <- active_col[, triad_idx[, 1], drop = FALSE]
        cj <- active_col[, triad_idx[, 2], drop = FALSE]
        ck <- active_col[, triad_idx[, 3], drop = FALSE]
        ind_cand_mask <- (ri & cj) | (rj & ci) | (ri & ck) |
                         (rk & ci) | (rj & ck) | (rk & cj)

        null_matrix <- matrix(0L, n_cand, n_perm)

        for (ind in valid_inds) {
          mask <- ind_cand_mask[ind, ]
          if (!any(mask)) next # nocov — rare: individual has zero overlap with all candidates
          wm <- which(mask)
          total <- ind_totals[ind]
          rs <- rows_stubs[[ind]]
          cs <- cols_stubs[[ind]]

          perm_cols <- vapply(seq_len(n_perm),
                              function(p) sample(cs),
                              integer(total))

          all_lin <- (perm_cols - 1L) * s + rs
          dim(all_lin) <- NULL
          perm_id <- rep(seq_len(n_perm), each = total)

          presence <- matrix(FALSE, nrow = ss, ncol = n_perm)
          presence[cbind(all_lin, perm_id)] <- TRUE

          has_any <- presence[lin_ij[wm], , drop = FALSE] |
                     presence[lin_ji[wm], , drop = FALSE] |
                     presence[lin_ik[wm], , drop = FALSE] |
                     presence[lin_ki[wm], , drop = FALSE] |
                     presence[lin_jk[wm], , drop = FALSE] |
                     presence[lin_kj[wm], , drop = FALSE]

          null_matrix[wm, ] <- null_matrix[wm, ] + has_any
        }

        null_mean <- rowMeans(null_matrix)
        null_sd <- apply(null_matrix, 1, stats::sd)
        null_sd[null_sd == 0] <- 1

        candidates$expected <- round(null_mean, 1)
        candidates$z <- round((candidates$observed - null_mean) / null_sd, 2)
        candidates$p <- round(2 * stats::pnorm(-abs(candidates$z)), 4)
        candidates$sig <- candidates$p < 0.05
        candidates <- candidates[order(abs(candidates$z), decreasing = TRUE), ]
        rownames(candidates) <- NULL
      }
      results <- candidates
    }
  }

  # Min count filter (instance mode without significance)
  if (!is.null(min_count) && named_nodes && !significance) {
    results <- results[results$observed > min_count, ]
    if (nrow(results) == 0) {
      message("No motifs with count > ", min_count, ".")
      return(NULL)
    }
  }

  # Top N
  if (!is.null(top) && top < nrow(results)) {
    results <- results[seq_len(top), ]
  }

  # Type summary
  type_summary <- sort(table(results$type), decreasing = TRUE)

  # Informative message (instance mode with defaults)
  if (named_nodes && !.user_set_pattern) {
    mc_label <- if (!is.null(min_count)) min_count else 0
    message("Showing triangle patterns (count > ", mc_label, "). ",
            "For all MAN types use pattern = 'all'.")
  }

  structure(
    list(
      results = results,
      type_summary = type_summary,
      level = level,
      named_nodes = named_nodes,
      n_units = n_units,
      params = list(
        labels = labels,
        n_states = s,
        pattern = pattern,
        edge_method = edge_method,
        edge_threshold = edge_threshold,
        significance = significance,
        n_perm = n_perm,
        min_count = min_count,
        window = window,
        window_type = window_type,
        actor = actor
      )
    ),
    class = "cograph_motif_result"
  )
}


#' Extract Specific Motif Instances (Subgraphs)
#'
#' Convenience wrapper for \code{motifs(x, named_nodes = TRUE, ...)}. Returns
#' specific node triples forming each MAN pattern.
#'
#' @inheritParams motifs
#' @seealso [motifs()]
#' @family motifs
#' @export
subgraphs <- function(...) motifs(..., named_nodes = TRUE)


#' @method print cograph_motif_result
#' @export
print.cograph_motif_result <- function(x, ...) {
  mode_label <- if (x$named_nodes) "Motif Subgraphs" else "Motif Census"
  cat(mode_label, "\n")
  cat("Level:", x$level)
  if (x$level == "individual") {
    cat(" |", x$n_units, "units")
  }
  cat(" | States:", x$params$n_states)
  cat(" | Pattern:", x$params$pattern, "\n")

  if (!is.null(x$params$window)) {
    cat("Window:", x$params$window, "(", x$params$window_type, ")\n")
  }

  if (x$params$significance) {
    cat("Significance: permutation (n_perm=", x$params$n_perm, ")\n", sep = "")
  }

  if (!is.null(x$params$min_count) && x$named_nodes) {
    cat("Min count: >", x$params$min_count, "\n")
  }

  cat("\nType distribution:\n")
  print(x$type_summary)

  n_show <- min(20, nrow(x$results))
  cat("\nTop", n_show, "results:\n")
  print(x$results[seq_len(n_show), ], row.names = FALSE)

  invisible(x)
}


#' @param type Plot type: "triads" (network diagrams), "types" (bar chart),
#'   "significance" (z-score plot), "patterns" (abstract MAN diagrams).
#' @param n Number of items to plot. Default 15.
#' @param ncol Number of columns in triad grid. Default 5.
#' @param colors Colors for visualization. Default blue/red.
#' @param ... Additional arguments passed to plot helpers.
#' @rdname motifs
#' @method plot cograph_motif_result
#' @export
plot.cograph_motif_result <- function(x, type = c("triads", "types",
                                                    "significance", "patterns"),
                                       n = 15, ncol = 5,
                                       colors = c("#2166AC", "#B2182B"),
                                       ...) {
  type <- match.arg(type)

  if (type == "significance" && !x$params$significance) {
    stop("Significance data not available. Run motifs() with significance = TRUE.",
         call. = FALSE)
  }

  if (type == "triads") {
    if (x$named_nodes) {
      # Instance mode: delegate to existing helper
      .plot_triad_networks(x, n = n, ncol = ncol,
                           colors = colors, ...)
    } else {
      # Census mode: no named triads, plot patterns instead
      .plot_motif_patterns(x, n = n, colors = colors, ...)
    }
    return(invisible(x))

  } else if (type == "types") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("ggplot2 is required for this plot type", call. = FALSE) # nocov
    }
    df <- as.data.frame(x$type_summary, stringsAsFactors = FALSE)
    names(df) <- c("type", "count")
    df <- df[order(df$count, decreasing = TRUE), ]

    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = stats::reorder(.data$type, .data$count), y = .data$count)) +
      ggplot2::geom_col(fill = colors[1]) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "MAN Type", y = "Count",
                    title = "Motif Type Distribution") +
      .motifs_ggplot_theme()
    print(p)
    return(invisible(p))

  } else if (type == "significance") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("ggplot2 is required for this plot type", call. = FALSE) # nocov
    }
    sig_df <- x$results[!is.na(x$results$z), ]
    sig_df <- sig_df[order(abs(sig_df$z), decreasing = TRUE), ]
    sig_df <- utils::head(sig_df, n)

    # Create label column (use triad if available, else type)
    sig_df$label <- if ("triad" %in% names(sig_df)) sig_df$triad else sig_df$type

    p <- ggplot2::ggplot(sig_df, ggplot2::aes(
      x = stats::reorder(.data$label, abs(.data$z)),
      y = .data$z,
      fill = .data$z > 0)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = colors[2], "FALSE" = colors[1]),
        guide = "none") +
      ggplot2::geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed",
                           color = "grey50") +
      ggplot2::labs(x = NULL, y = "Z-score",
                    title = "Motif Significance") +
      .motifs_ggplot_theme()
    print(p)
    return(invisible(p))

  } else if (type == "patterns") {
    .plot_motif_patterns(x, n = n, colors = colors, ...)
    return(invisible(x))
  }

  invisible(x) # nocov — all type branches return above
}
