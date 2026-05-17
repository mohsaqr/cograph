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
#' @param pattern Which MAN triad types to include in the analysis:
#'   \describe{
#'     \item{\code{"triangle"}}{(default) Only the 7 closed triangle types:
#'       030C, 030T, 120C, 120D, 120U, 210, 300. Excludes trivial open patterns
#'       (empty triads, single edges, chains, stars, mutual pairs).}
#'     \item{\code{"network"}}{All types except trivially open ones. Excludes
#'       003 (empty), 012 (single edge), 021C (chain).}
#'     \item{\code{"closed"}}{Like \code{"network"} but also excludes 120C
#'       (mixed regulated). Excludes 003, 012, 021C, 120C.}
#'     \item{\code{"all"}}{All 16 MAN types, including empty and trivial patterns.}
#'   }
#' @param include Character vector of MAN types to include exclusively.
#'   Overrides \code{pattern}.
#' @param exclude Character vector of MAN types to exclude. Applied after
#'   \code{pattern} filter.
#' @param significance Logical. Run permutation significance test? Default TRUE.
#' @param n_perm Number of permutations for significance. Default 1000.
#' @param min_count Inclusive minimum count to keep a row — rows with
#'   \code{count >= min_count} are retained. In instance mode
#'   (\code{named_nodes = TRUE}) this filters the \code{observed} column:
#'   at individual level the number of subjects exhibiting the triad, at
#'   aggregate level the triad's weighted edge mass (sum of its 6 directed
#'   edge weights). In census mode (\code{named_nodes = FALSE}) this filters
#'   the \code{count} column — the number of times each MAN type appears.
#'   Default 5 for instances, NULL for census (no filter).
#' @param edge_method Method for determining edge presence: "any" (default),
#'   "expected", or "percent".
#' @param edge_threshold Threshold for "expected" or "percent" methods. Default 1.5.
#' @param min_transitions Minimum total transitions for a unit to be included.
#'   Default 5.
#' @param top Return only the top N results. NULL returns all.
#' @param seed Random seed for reproducibility.
#'
#' @return A \code{cograph_motif_result} object (a list) with:
#'   \describe{
#'     \item{results}{Data frame of results. Census mode
#'       (\code{named_nodes = FALSE}): one row per MAN type with columns
#'       \code{type}, \code{count}, and when \code{significance = TRUE} also
#'       \code{expected}, \code{z}, \code{p}, \code{sig}. Instance mode
#'       (\code{named_nodes = TRUE}): one row per concrete node triple with
#'       columns \code{triad}, \code{type}, \code{observed}, and when
#'       \code{significance = TRUE} also \code{expected}, \code{z}, \code{p},
#'       \code{sig}.}
#'     \item{type_summary}{Named \code{table} of MAN-type counts. In census
#'       mode the values come from the \code{count} column; in instance
#'       mode they come from \code{table(results$type)} and describe how
#'       many concrete node-triples fall under each MAN type. Sorted
#'       descending so \code{plot(., type = "patterns")} draws the most
#'       frequent types first.}
#'     \item{level}{Analysis level: \code{"individual"} when the input
#'       carried per-subject sequence data (\code{tna} with \code{$data},
#'       edge list with an actor column, Nestimate \code{netobject} built
#'       from \code{build_tna()}/similar), otherwise \code{"aggregate"}
#'       (a single transition matrix).}
#'     \item{named_nodes}{Logical mirror of the \code{named_nodes} argument.
#'       Plot helpers gate per-type significance decoration on this so the
#'       instance-mode case (multiple triples per MAN type) doesn't get
#'       silently aggregated.}
#'     \item{n_units}{Number of subjects/units. 1 at aggregate level,
#'       \code{nrow} of the input sequence data at individual level.}
#'     \item{params}{List of the call's parameters (\code{pattern},
#'       \code{edge_method}, \code{edge_threshold}, \code{significance},
#'       \code{n_perm}, \code{min_count}, \code{labels}, \code{n_states},
#'       and the window settings if any). Read by \code{print()} and the
#'       \code{plot()} dispatcher.}
#'   }
#'
#' @examples
#' # Census from a matrix (no significance test -- fastest path)
#' mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
#' motifs(mat, significance = FALSE)
#'
#' \dontrun{
#' # With a minimal significance test (set n_perm >= 500 in practice)
#' motifs(mat, n_perm = 10L, seed = 1)
#' }
#'
#' @examplesIf requireNamespace("tna", quietly = TRUE)
#' \dontrun{
#' Mod <- tna::tna(tna::group_regulation)
#' motifs(Mod, n_perm = 10L, seed = 1)
#' subgraphs(Mod, n_perm = 10L, seed = 1)
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

  # --- Case 2: cograph_network (includes Nestimate netobject) ---
  } else if (inherits(x, "cograph_network")) {
    raw_data <- x$data
    net_labels <- get_labels(x)

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

    } else if (.is_tna_sequence_data(raw_data, net_labels) &&
               requireNamespace("tna", quietly = TRUE)) {
      # Nestimate::build_tna() (and similar) stores raw sequence data in $data
      # — structurally identical to what tna::tna() consumes. Route through
      # the individual-level tna path so motifs sees per-subject transitions.
      tna_obj <- tna::tna(raw_data)
      init_fn <- .get_tna_initialize_model()
      model <- init_fn(tna_obj$data, attr(tna_obj, "type"),
                       attr(tna_obj, "scaling"), attr(tna_obj, "params"),
                       transitions = TRUE)
      trans <- model$trans
      labels <- tna_obj$labels
      level <- "individual"
      n_units <- dim(trans)[1]

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

        mc_idx <- stats::setNames(seq_len(nrow(mc)), mc$motif)
        for (ri in seq_len(nrow(results))) {
          tp <- results$type[ri]
          mc_row <- mc_idx[tp]
          if (!is.na(mc_row)) {
            results$expected[ri] <- round(mc$null_mean[mc_row], 1)
            results$z[ri] <- round(mc$z_score[mc_row], 2)
            results$p[ri] <- round(mc$p_value[mc_row], 4)
            results$sig[ri] <- abs(mc$z_score[mc_row]) > 1.96
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
                 weight = counted$weight,
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
      # Aggregate level: a single matrix contains each triad at most once, so a
      # frequency-style "observed" is structurally always 1. Use the weighted
      # edge mass of the triad (sum of its 6 directed edge weights) instead, so
      # min_count becomes a meaningful strength filter at aggregate level.
      first_idx <- !duplicated(combined$triad)
      results <- data.frame(
        triad = combined$triad[first_idx],
        type = combined$type[first_idx],
        observed = combined$weight[first_idx],
        stringsAsFactors = FALSE
      )
      results <- results[order(results$observed, decreasing = TRUE), ]
    }
    rownames(results) <- NULL

    # ---- INSTANCE SIGNIFICANCE (exact configuration model) ----
    if (significance && level == "individual") {
      if (!is.null(min_count)) {
        candidates <- results[results$observed >= min_count, ]
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

  # Min count filter (inclusive). In instance mode, applied for every path
  # EXCEPT the significance + level=="individual" branch above, which already
  # filters before computing the null distribution. In census mode
  # (named_nodes=FALSE), filters MAN types by the `count` column.
  if (!is.null(min_count)) {
    if (named_nodes && !(significance && level == "individual")) {
      results <- results[results$observed >= min_count, ]
      if (nrow(results) == 0) {
        message("No motifs with count >= ", min_count, ".")
        return(NULL)
      }
    } else if (!named_nodes && "count" %in% names(results)) {
      results <- results[results$count >= min_count, ]
      if (nrow(results) == 0) {
        message("No motif types with count >= ", min_count, ".")
        return(NULL)
      }
    }
  }

  # Top N
  if (!is.null(top) && top < nrow(results)) {
    results <- results[seq_len(top), ]
  }

  # Type summary. In census mode each MAN type collapses to one row, so
  # table(results$type) gives all 1s — use results$count directly. In
  # instance mode `results` has one row per node-triple, so table() counts
  # how many instances belong to each MAN type, which is what we want.
  # We always return a `table` so as.data.frame(type_summary) yields a
  # tidy two-column frame (consumers downstream rely on this shape).
  if (!named_nodes && "count" %in% names(results)) {
    type_summary <- as.table(stats::setNames(as.integer(results$count),
                                             as.character(results$type)))
    type_summary <- sort(type_summary, decreasing = TRUE)
  } else {
    type_summary <- sort(table(results$type), decreasing = TRUE)
  }

  # Informative message (instance mode with defaults)
  if (named_nodes && !.user_set_pattern) {
    mc_label <- if (!is.null(min_count)) min_count else 1L
    message("Showing triangle patterns (count >= ", mc_label, "). ",
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
#' one row per concrete node-triple instantiating each MAN pattern, so the
#' same MAN type can appear in many rows with its own \code{z} / \code{p}
#' per triple. For per-triple significance use
#' \code{plot(., type = "significance")} or \code{plot(., type = "triads")};
#' the per-type plots (\code{"types"}, \code{"patterns"}) deliberately drop
#' the significance decoration here, because aggregating per type requires a
#' rule (median? max-|z|?) that isn't pinned and would be misleading by
#' default.
#'
#' @param ... Arguments forwarded to \code{\link{motifs}()}. See \code{?motifs}
#'   for the full parameter list (\code{x}, \code{actor}, \code{window},
#'   \code{pattern}, \code{include}, \code{exclude}, \code{significance},
#'   \code{n_perm}, \code{min_count}, \code{edge_method}, \code{edge_threshold},
#'   \code{min_transitions}, \code{top}, \code{seed}).
#' @return A \code{cograph_motif_result} object with \code{named_nodes = TRUE}.
#'   Contains \code{$results} (data frame with columns \code{triad}, \code{type},
#'   \code{observed}, and optionally \code{z}, \code{p}, \code{sig}),
#'   \code{$type_summary}, \code{$level}, \code{$n_units}, and \code{$params}.
#'   In instance mode, \code{$type_summary} is built via
#'   \code{table(results$type)} so it counts how many node-triples fall under
#'   each MAN type.
#' @examples
#' mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
#' subgraphs(mat, significance = FALSE)
#' @seealso [motifs()]
#' @family motifs
#' @export
subgraphs <- function(...) motifs(..., named_nodes = TRUE)


#' @rdname motifs
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

  if (!is.null(x$params$min_count)) {
    cat("Min count: >=", x$params$min_count, "\n")
  }

  cat("\nType distribution:\n")
  print(x$type_summary)

  n_show <- min(20, nrow(x$results))
  cat("\nTop", n_show, "results:\n")
  print(x$results[seq_len(n_show), ], row.names = FALSE)

  invisible(x)
}


#' @param type Plot type:
#'   \describe{
#'     \item{\code{"triads"}}{Network diagrams of specific node triples
#'       (instance mode) or falls back to patterns (census mode). Each panel
#'       title reads \code{"<MAN code>: <description>"} (e.g. \code{"030T:
#'       Feed-forward"}) and, in census mode, appends the z-score and a
#'       significance star (\code{*} p<.05, \code{**} p<.01, \code{***}
#'       p<.001). Arranged in a grid.}
#'     \item{\code{"types"}}{Bar chart of MAN type frequencies. In census
#'       mode bars are colored by significance direction (see \code{colors});
#'       in instance mode bars use a single fill because per-type
#'       significance would need an aggregation rule across multiple
#'       node-triple rows of the same type.}
#'     \item{\code{"significance"}}{Z-score bars per row of
#'       \code{x$results}. In census mode each bar is one MAN type; in
#'       instance mode each bar is one concrete node-triple, labeled
#'       \code{"<triple> [<MAN code>: <description>]"}. Bars are colored
#'       with the same three-tone rule (see \code{colors}). Requires
#'       \code{significance = TRUE} in the \code{motifs()} call.}
#'     \item{\code{"patterns"}}{Abstract MAN pattern diagrams showing the
#'       edge structure of each triad type. In census mode panel nodes are
#'       filled by significance direction (red sig over / blue sig under /
#'       grey ns); in instance mode panels use a single fill, same reason
#'       as \code{"types"}.}
#'   }
#' @param n Maximum number of items to plot. Default 15.
#' @param ncol Number of columns in the triad/pattern grid. Default 5.
#' @param colors Two-element color vector mapped to a three-tone
#'   significance scale (used by \code{type = "significance"}, plus
#'   \code{type = "types"} and \code{type = "patterns"} in census mode):
#'   \code{colors[1]} fills items that are significantly under-represented
#'   (\code{p < .05} and \code{z < 0}); \code{colors[2]} fills items that
#'   are significantly over-represented (\code{p < .05} and \code{z > 0});
#'   everything else is filled neutral grey (\code{"#9E9E9E"}). Default
#'   \code{c("#2166AC", "#B2182B")} (blue for under, red for over).
#'   When significance was not run, \code{type = "types"} falls back to a
#'   single \code{colors[1]} fill and patterns nodes use \code{colors[1]}.
#' @param node_size Triad node radius (relative). Default 5.
#'   (\code{type = "triads"} only.)
#' @param label_size Triad node-label font size in points. Default 11.
#' @param title_size Per-panel title font size in points. Default 12.
#' @param stats_size Per-panel statistics caption font size in points
#'   (e.g., \code{n=34 z=-55.3 p<.001}). Default 13.
#' @param legend_size Bottom legend font size in points. Default 13.
#' @param legend Logical. Show the abbreviation legend strip below the
#'   triad grid. Default \code{TRUE}. (\code{type = "triads"} only.)
#' @param motif_color Color of triad nodes/edges/labels. Default
#'   \code{"#800020"} (deep burgundy). (\code{type = "triads"} only.)
#' @param spacing Triangle spread inside each panel; \code{> 1} pulls
#'   nodes inward, \code{< 1} pushes them apart. Default 1.
#' @param base_size Base font size for the \code{ggplot2} themes used
#'   by \code{type = "types"} and \code{type = "significance"}.
#'   Default 12.
#' @param combined Logical: when TRUE (default) and \code{type = "patterns"}
#'   (or \code{type = "triads"} on unnamed-node input that falls back to
#'   pattern plotting), arrange the per-motif panels in an internal grid via
#'   \code{graphics::par(mfrow=...)}. Set to FALSE to draw into a layout the
#'   caller has already configured (e.g. via \code{\link{panel_layout}()}).
#' @param ... Additional arguments passed to internal plot helpers.
#' @return Invisibly returns the input \code{x} for \code{"triads"} and
#'   \code{"patterns"}, or the underlying \code{ggplot} for \code{"types"} and
#'   \code{"significance"}.
#' @rdname motifs
#' @method plot cograph_motif_result
#' @export
plot.cograph_motif_result <- function(x, type = c("triads", "types",
                                                    "significance", "patterns"),
                                       n = 15, ncol = 5,
                                       colors = c("#2166AC", "#B2182B"),
                                       node_size = 5,
                                       label_size = 11,
                                       title_size = 12,
                                       stats_size = 13,
                                       legend_size = 13,
                                       legend = TRUE,
                                       motif_color = "#800020",
                                       spacing = 1,
                                       base_size = 12,
                                       combined = TRUE,
                                       ...) {
  type <- match.arg(type)

  if (type == "significance" && !x$params$significance) {
    stop("Significance data not available. Run motifs() with significance = TRUE.",
         call. = FALSE)
  }

  if (type == "triads") {
    if (x$named_nodes) {
      .plot_triad_networks(x, n = n, ncol = ncol, colors = colors,
                           node_size = node_size, label_size = label_size,
                           title_size = title_size, stats_size = stats_size,
                           legend_size = legend_size, legend = legend,
                           color = motif_color, spacing = spacing, ...)
    } else {
      .plot_motif_patterns(x, n = n, colors = colors,
                           combined = combined, ...)
    }
    return(invisible(x))

  } else if (type == "types") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("ggplot2 is required for this plot type", call. = FALSE) # nocov
    }
    df <- as.data.frame(x$type_summary, stringsAsFactors = FALSE)
    names(df) <- c("type", "count")
    df <- df[order(df$count, decreasing = TRUE), ]

    # Color bars by significance direction — only safe in census mode.
    # In instance mode (named_nodes = TRUE) `results` has one row per
    # node-triple, so the same MAN type appears in many rows with
    # potentially conflicting z/p values; there's no single type-level
    # statistic without an aggregation rule that's documented and tested.
    # Skip the coloring there and fall back to a single fill color.
    has_sig <- !isTRUE(x$named_nodes) &&
               isTRUE(x$params$significance) &&
               is.data.frame(x$results) &&
               "z" %in% names(x$results) &&
               "p" %in% names(x$results) &&
               "type" %in% names(x$results)
    if (has_sig) {
      type_z <- stats::setNames(x$results$z, x$results$type)
      type_p <- stats::setNames(x$results$p, x$results$type)
      df$direction <- ifelse(
        !is.na(type_p[df$type]) & type_p[df$type] < 0.05 &
          type_z[df$type] > 0, "over",
        ifelse(!is.na(type_p[df$type]) & type_p[df$type] < 0.05 &
                 type_z[df$type] < 0, "under", "ns")
      )
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = stats::reorder(.data$type, .data$count), y = .data$count,
        fill = .data$direction)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(
          values = c(over = colors[2], under = colors[1], ns = "#9E9E9E"),
          labels = c(over = "Over-represented (p<.05)",
                     under = "Under-represented (p<.05)",
                     ns = "Not significant"),
          name = NULL) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "MAN Type", y = "Count",
                      title = "Motif Type Distribution") +
        .motifs_ggplot_theme(base_size = base_size) +
        ggplot2::theme(legend.position = "bottom")
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = stats::reorder(.data$type, .data$count), y = .data$count)) +
        ggplot2::geom_col(fill = colors[1]) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "MAN Type", y = "Count",
                      title = "Motif Type Distribution") +
        .motifs_ggplot_theme(base_size = base_size)
    }
    print(p)
    return(invisible(p))

  } else if (type == "significance") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("ggplot2 is required for this plot type", call. = FALSE) # nocov
    }
    sig_df <- x$results[!is.na(x$results$z), ]
    sig_df <- sig_df[order(abs(sig_df$z), decreasing = TRUE), ]
    sig_df <- utils::head(sig_df, n)

    # In instance mode, label each bar with the node triple AND the MAN-type
    # description so "Context - Critique - Instruct" reads as
    # "Context - Critique - Instruct [030T: Feed-forward]" — much easier to
    # scan than the bare code.
    if ("triad" %in% names(sig_df)) {
      type_desc <- .get_man_descriptions()
      desc_vec <- type_desc[sig_df$type]
      desc_vec[is.na(desc_vec)] <- ""
      tag <- ifelse(nzchar(desc_vec),
                    sprintf("  [%s: %s]", sig_df$type, desc_vec),
                    sprintf("  [%s]", sig_df$type))
      sig_df$label <- paste0(sig_df$triad, tag)
    } else {
      type_desc <- .get_man_descriptions()
      desc_vec <- type_desc[sig_df$type]
      desc_vec[is.na(desc_vec)] <- ""
      sig_df$label <- ifelse(nzchar(desc_vec),
                             sprintf("%s: %s", sig_df$type, desc_vec),
                             sig_df$type)
    }

    # Unified 3-tone coding: red = sig over, blue = sig under, grey = ns.
    # Same rule as the types bar plot and the patterns node fills.
    sig_df$direction <- ifelse(
      !is.na(sig_df$p) & sig_df$p < 0.05 & sig_df$z > 0, "over",
      ifelse(!is.na(sig_df$p) & sig_df$p < 0.05 & sig_df$z < 0,
             "under", "ns")
    )
    p <- ggplot2::ggplot(sig_df, ggplot2::aes(
      x = stats::reorder(.data$label, abs(.data$z)),
      y = .data$z,
      fill = .data$direction)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c(over = colors[2], under = colors[1], ns = "#9E9E9E"),
        labels = c(over = "Over-represented (p<.05)",
                   under = "Under-represented (p<.05)",
                   ns = "Not significant"),
        name = NULL) +
      ggplot2::geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed",
                           color = "grey50") +
      ggplot2::labs(x = NULL, y = "Z-score",
                    title = "Motif Significance") +
      .motifs_ggplot_theme(base_size = base_size) +
      ggplot2::theme(legend.position = "bottom")
    print(p)
    return(invisible(p))

  } else if (type == "patterns") {
    .plot_motif_patterns(x, n = n, colors = colors,
                         combined = combined, ...)
    return(invisible(x))
  }

  invisible(x) # nocov — all type branches return above
}


#' Plot a motif/subgraph result
#'
#' Tab-completion-friendly wrapper around the
#' \code{plot.cograph_motif_result} S3 method. Functionally identical
#' to \code{plot(x, ...)} on a \code{cograph_motif_result} object,
#' but exposes the \code{type / n / ncol / colors} arguments to
#' editor autocompletion.
#'
#' @inheritParams plot.cograph_motif_result
#' @param x A \code{cograph_motif_result} object from \code{motifs()} or
#'   \code{subgraphs()}.
#' @return Invisibly returns the input \code{x} (or the underlying
#'   \code{ggplot} for the \code{"types"} and \code{"significance"}
#'   types, matching the S3 method).
#' @seealso \code{\link{motifs}}, \code{\link{subgraphs}}
#' @examples
#' \dontrun{
#' g <- igraph::sample_gnp(20, 0.2, directed = TRUE)
#' m <- motifs(g)
#' plot_motifs(m)
#' plot_motifs(m, type = "types")
#' }
#' @export
plot_motifs <- function(x, type = c("triads", "types",
                                     "significance", "patterns"),
                         n = 15, ncol = 5,
                         colors = c("#2166AC", "#B2182B"),
                         node_size = 5,
                         label_size = 11,
                         title_size = 12,
                         stats_size = 13,
                         legend_size = 13,
                         legend = TRUE,
                         motif_color = "#800020",
                         spacing = 1,
                         base_size = 12,
                         ...) {
  if (!inherits(x, "cograph_motif_result")) {
    stop("'x' must be a cograph_motif_result (from motifs() or subgraphs()).",
         call. = FALSE)
  }
  plot(x, type = match.arg(type), n = n, ncol = ncol, colors = colors,
       node_size = node_size, label_size = label_size,
       title_size = title_size, stats_size = stats_size,
       legend_size = legend_size, legend = legend,
       motif_color = motif_color, spacing = spacing,
       base_size = base_size, ...)
}
