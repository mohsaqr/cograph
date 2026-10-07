# Unified motifs API
# Contains: motifs(), subgraphs(), print/plot methods for cograph_motif_result

#' Network Motif Analysis
#'
#' Classifies the node triples of a network into the 16 directed MAN triad
#' types and tests their frequencies against a permutation null. The function
#' has two modes.
#' \itemize{
#'   \item In census mode (\code{named_nodes = FALSE}, the default), the result
#'     counts the triads of each MAN type. Nodes are exchangeable.
#'   \item In instance mode (\code{named_nodes = TRUE}, or \code{subgraphs()}),
#'     the result lists the node triples that form each type.
#' }
#'
#' The input type and the analysis level are detected automatically. Inputs
#' that carry per-unit data are analyzed per unit. These are tna objects,
#' networks that store tna sequence data, and edge lists (or networks built
#' from edge lists) with an actor column. Matrices, igraph objects and other
#' networks are analyzed as one aggregate network. The adjacency is always
#' classified as directed. The four-class undirected census is computed by
#' \code{motif_census(..., directed = FALSE)}.
#'
#' @details For aggregate inputs, significance is computed by [motif_census()]
#' with its degree-preserving rewiring null on the simple loop-free graph.
#' Individual weighted inputs use a directed stub-matching null. Each positive
#' edge weight is converted to at least one integer stub, and target stubs are
#' shuffled within each unit so that its integer in- and out-margins are
#' preserved. The resulting multigraph, which may contain loops or parallel
#' edges, is classified through its simple loop-free triad projection.
#' Self-loops are removed before counting and before the null is constructed.
#'
#' With \code{edge_method = "percent"}, edge presence is computed within each
#' node triple. The weight of an edge is divided by the sum of the six possible
#' directed edge weights of that triple. A threshold above 1 is read as a
#' percentage (1.5 means 1.5 percent), and a threshold at or below 1 is read
#' as a proportion.
#'
#' With an \code{edge_method} other than \code{"any"}, the significance test
#' has limits. For aggregate census input, the observed counts use the
#' threshold while the null tests the unthresholded network, and a warning is
#' raised. For individual census input, the threshold is reapplied to each
#' stub-null replicate. For individual instance input, the null classifies raw
#' stub presence and does not reapply \code{edge_method} or
#' \code{edge_threshold}. In every weighted individual null, a positive
#' fractional weight keeps at least one stub. This preserves the support but
#' can change the weight scale used by \code{"percent"} and \code{"expected"}.
#' Descriptive results with \code{significance = FALSE} or
#' \code{edge_method = "any"} are unaffected.
#'
#' @param x Input data: a tna object, cograph_network, matrix, igraph object or
#'   edge-list data frame. For \code{as.data.frame()}, a
#'   \code{cograph_motif_result} object returned by \code{motifs()} or
#'   \code{subgraphs()}.
#' @param named_nodes Logical. If FALSE (default), the MAN type census is
#'   computed. If TRUE, the individual node triples are listed.
#'   \code{subgraphs()} sets this to TRUE.
#' @param actor Character. Name of the edge-list column that identifies the
#'   units. If NULL (default), the first column named \code{session_id},
#'   \code{session}, \code{actor}, \code{user}, \code{participant},
#'   \code{individual} or \code{id} (in that order, ignoring case) is used.
#'   Without such a column the analysis is aggregate.
#' @param window Numeric. Window size for edge-list input. Each actor's
#'   transitions are split into windows of this size. NULL (default) applies
#'   no windowing.
#' @param window_type Character. \code{"rolling"} (default) or
#'   \code{"tumbling"}. Used only when \code{window} is set.
#' @param pattern Which MAN triad types to include in the analysis:
#'   \describe{
#'     \item{\code{"triangle"}}{(default) The 7 closed triangle types 030C,
#'       030T, 120C, 120D, 120U, 210 and 300.}
#'     \item{\code{"network"}}{All types except 003 (empty), 012 (single edge)
#'       and 021C (chain).}
#'     \item{\code{"closed"}}{All types except 003, 012, 021C and 120C.}
#'     \item{\code{"all"}}{All 16 MAN types.}
#'   }
#' @param include Character vector of MAN types to keep. When supplied,
#'   \code{pattern} and \code{exclude} are ignored.
#' @param exclude Character vector of MAN types to drop in addition to those
#'   removed by \code{pattern}.
#' @param significance Logical. If TRUE (default), a permutation significance
#'   test is run. In instance mode the test requires individual data, and for
#'   aggregate input it is skipped with a warning.
#' @param n_perm Number of permutations for significance. When
#'   \code{significance = TRUE}, must be a whole number of at least 2.
#'   Default 1000.
#' @param cores Number of worker processes for the permutation null. Default
#'   1 runs serially and draws all replicates from a single RNG stream.
#'   \code{cores > 1} gives each replicate its own L'Ecuyer-CMRG stream, so the
#'   result depends on \code{seed} alone and is the same for every worker
#'   count. The serial and parallel streams differ, so a serial run and a
#'   parallel run with the same seed give different p-values. Forking is used
#'   where available, and Windows uses a PSOCK cluster. Only the
#'   individual-level census null is parallelized. Values above
#'   \code{parallel::detectCores()} are capped with a
#'   \code{cograph_cores_capped} warning.
#' @param min_count Inclusive minimum count for a row to be kept. In census
#'   mode it filters the \code{count} column, the number of triads of each MAN
#'   type. In instance mode it filters the \code{observed} column. At
#'   individual level this is the number of units showing the triad, and at
#'   aggregate level it is the weighted edge mass of the triad (the sum of its
#'   6 directed edge weights). Default 5 in instance mode and NULL (no filter)
#'   in census mode.
#' @param edge_method Method for determining edge presence: \code{"any"}
#'   (default; any positive edge), \code{"expected"} (ratio of the observed
#'   weight to the weight expected from the row and column totals), or
#'   \code{"percent"} (edge weight divided by the six-edge triad total).
#' @param edge_threshold Threshold for \code{"expected"} or \code{"percent"}
#'   methods. For \code{"expected"}, 1.5 means 50 percent above expected. For
#'   \code{"percent"}, values at or below 1 are proportions and values above 1
#'   are percentages. Default 1.5.
#' @param min_transitions Minimum total edge weight for a unit to be included.
#'   Default 5. At aggregate level the network is the only unit, so a network
#'   whose weights sum to less than this value gives NULL.
#' @param top Integer or NULL. Only the first \code{top} rows of the results
#'   are kept. NULL (default) keeps all rows.
#' @param seed Random seed. Default NULL. When supplied, the caller's RNG state
#'   is restored on exit.
#'
#' @return A \code{cograph_motif_result} object, or NULL with a message when no
#'   motif passes the filters. The object is a list with the elements below.
#'   \describe{
#'     \item{results}{Data frame of results. In census mode it has one row per
#'       retained, observed MAN type and the columns \code{type} and
#'       \code{count}. In instance mode it has one row per node triple and MAN
#'       type and the columns \code{triad}, \code{node1}, \code{node2},
#'       \code{node3}, \code{type} and \code{observed}. At individual level,
#'       \code{observed} is the number of units in which the triple has that
#'       type, so one triple can occupy several rows. With
#'       \code{significance = TRUE}, the columns \code{expected}, \code{z},
#'       \code{p} and \code{sig} are added and the rows are sorted by
#'       decreasing absolute z-score. Otherwise the rows are sorted by
#'       decreasing count.}
#'     \item{type_summary}{Named \code{table} of counts per MAN type, sorted in
#'       decreasing order. In census mode it holds the \code{count} column. In
#'       instance mode it holds the number of node triples of each type.}
#'     \item{level}{\code{"individual"} when the input carried per-unit data,
#'       otherwise \code{"aggregate"}.}
#'     \item{named_nodes}{The value of the \code{named_nodes} argument.}
#'     \item{n_units}{Number of units analyzed. 1 at aggregate level.}
#'     \item{params}{List of the analysis settings: \code{labels},
#'       \code{n_states}, \code{pattern}, \code{edge_method},
#'       \code{edge_threshold}, \code{significance}, \code{n_perm},
#'       \code{min_count}, \code{window}, \code{window_type} and
#'       \code{actor}.}
#'   }
#'
#'   \code{as.data.frame()} returns one of these tables as a plain data frame.
#'   With \code{what = "results"} (default) it returns the \code{results}
#'   table described above. With \code{what = "types"} it returns one row per
#'   MAN type with columns \code{type} and \code{count}, where \code{count} is
#'   the number of triads of that type in a census or the number of node
#'   triples of that type in instance mode.
#'
#' @section Printing and plotting:
#' Printing the result shows the analysis settings, the MAN type distribution
#' and the first 20 rows of the results table. \code{as.data.frame()} returns
#' the tidy tables. \code{plot()} on the result is documented in
#' \code{\link{plot-results}}.
#'
#' @examples
#' census <- motifs(regulation_net, significance = FALSE)
#' as.data.frame(census, what = "types")
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
                   cores = 1L,
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
  if (significance) {
    n_perm <- .validate_motif_repetitions(n_perm, "n_perm")
    cores <- .motif_validate_cores(cores)
  }

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

  # Self-loops are outside the induced triad universe. Remove them before
  # activity gating, threshold calculations, and null-model construction so
  # a loop can never be shuffled into an ordinary motif edge.
  trans <- .motif_strip_loops(trans)

  if (!named_nodes) {
    # ---- CENSUS MODE: count MAN type frequencies per unit ----
    type_counts_per_unit <- lapply(seq_len(dim(trans)[1]), function(ind) {
      mat <- .motif_unit_matrix(trans, ind)
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
        # Delegate to motif_census which uses igraph. MAN census types are
        # directed classes, so the null must be directed too — a symmetric
        # matrix must not fall through to the undirected census, whose
        # empty/edge/wedge/triangle names would never match a MAN row.
        agg_mat <- trans[1, , ]
        rownames(agg_mat) <- colnames(agg_mat) <- labels
        if (edge_method != "any") {
          warning("Census significance tests the unthresholded network; ",
                  "edge_method = \"", edge_method, "\" affects observed ",
                  "counts only.", call. = FALSE)
        }
        mc <- motif_census(agg_mat, n_random = n_perm, seed = seed,
                           directed = TRUE)

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
            results$p[ri] <- mc$p_value[mc_row]
            results$sig[ri] <- mc$significant[mc_row]
          }
        }
        results <- results[order(.motif_z_rank(results$z, results$p),
                                 decreasing = TRUE), ]
        rownames(results) <- NULL

      } else {
        # Individual: config model on weighted matrices
        null_matrix <- matrix(0, nrow = nrow(results), ncol = n_perm)

        n_ind_c <- dim(trans)[1]
        # Unit eligibility is defined by the original weighted activity. Do
        # not recompute it from integerized stubs: rounding could otherwise
        # admit an observed-excluded fractional unit into the null.
        observed_activity_c <- vapply(seq_len(n_ind_c), function(ind) {
          sum(.motif_unit_matrix(trans, ind))
        }, numeric(1))
        valid_c <- which(observed_activity_c >= min_transitions)
        # Stub validation and construction cover only null-eligible units: a
        # malformed cell in a unit the null never touches must not abort the
        # whole run.
        rows_stubs_c <- vector("list", n_ind_c)
        cols_stubs_c <- vector("list", n_ind_c)
        for (ind in valid_c) {
          stubs_c <- .motif_configuration_stubs(.motif_unit_matrix(trans, ind))
          rows_stubs_c[[ind]] <- stubs_c$rows
          cols_stubs_c[[ind]] <- stubs_c$cols
        }
        ss_c <- as.integer(s * s)
        types_c <- results$type

        replicate_fun <- function(p) {
          .motif_census_replicate(
            valid_c, rows_stubs_c, cols_stubs_c, s, ss_c, types_c,
            edge_method, edge_threshold, final_exclude, final_include
          )
        }

        if (cores <= 1L) {
          # Serial path, unchanged. vapply() would express this, but the
          # replicate-then-unit RNG consumption order IS the contract here --
          # it is what makes a seed reproduce results from earlier versions --
          # so the loop states that order explicitly.
          for (perm in seq_len(n_perm)) {
            null_matrix[, perm] <- replicate_fun(perm)
          }
        } else {
          streams <- .motif_rng_streams(n_perm, seed)
          reps <- .motif_run_replicates(n_perm, cores, streams, replicate_fun,
                                        n_values = nrow(results))
          filled <- do.call(cbind, reps)
          # Assert the shape before assigning: `null_matrix[] <- m` recycles
          # silently whenever m is a whole fraction of the target.
          stopifnot(
            "parallel replicates did not fill the null matrix" =
              identical(dim(filled), dim(null_matrix))
          )
          null_matrix[] <- filled
        }

        ns <- .motif_null_stats(results$count, t(null_matrix))
        results$expected <- round(ns$mean, 1)
        results$z <- round(ns$z, 2)
        results$p <- ns$p
        results$sig <- ns$significant

        results <- results[order(.motif_z_rank(results$z, results$p),
                                 decreasing = TRUE), ]
        rownames(results) <- NULL
      }
    }

  } else {
    # ---- INSTANCE MODE: list specific node triples ----
    all_results <- lapply(seq_len(dim(trans)[1]), function(ind) {
      mat <- .motif_unit_matrix(trans, ind)
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
      triad_keys <- paste(counted$i, counted$j, counted$k, sep = "\r")

      data.frame(unit = ind, .triad_key = triad_keys,
                 triad = triads,
                 node1 = labels[counted$i], node2 = labels[counted$j],
                 node3 = labels[counted$k], type = counted$type,
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
      # One row per (triple, MAN type): the same three nodes can instantiate
      # different types in different units, and collapsing to a dominant type
      # would attribute every unit's observation to it — making the per-type
      # totals disagree with census mode on identical data.
      obs <- stats::aggregate(
        unit ~ .triad_key + triad + node1 + node2 + node3 + type,
                              data = combined,
        FUN = length
      )
      names(obs)[7] <- "observed"
      results <- obs[order(obs$observed, decreasing = TRUE),
                     c(".triad_key", "triad", "node1", "node2", "node3",
                       "observed", "type")]
    } else {
      # Aggregate level: a single matrix contains each triad at most once, so a
      # frequency-style "observed" is structurally always 1. Use the weighted
      # edge mass of the triad (sum of its 6 directed edge weights) instead, so
      # min_count becomes a meaningful strength filter at aggregate level.
      first_idx <- !duplicated(combined$.triad_key)
      results <- data.frame(
        .triad_key = combined$.triad_key[first_idx],
        triad = combined$triad[first_idx],
        node1 = combined$node1[first_idx],
        node2 = combined$node2[first_idx],
        node3 = combined$node3[first_idx],
        type = combined$type[first_idx],
        observed = combined$weight[first_idx],
        stringsAsFactors = FALSE
      )
      results <- results[order(results$observed, decreasing = TRUE), ]
    }
    rownames(results) <- NULL

    # ---- INSTANCE SIGNIFICANCE (directed weighted stub-matching model) ----
    if (significance && level != "individual") {
      warning("Instance-mode significance requires individual-level data ",
              "(an actor/session column with multiple units); skipping the ",
              "permutation test.", call. = FALSE)
      significance <- FALSE
    }
    if (significance && level == "individual") {
      if (!is.null(min_count)) {
        candidates <- results[results$observed >= min_count, ]
      } else {
        candidates <- results
      }

      if (nrow(candidates) > 0) {
        triad_idx <- do.call(rbind, strsplit(candidates$.triad_key, "\r",
                                             fixed = TRUE))
        storage.mode(triad_idx) <- "integer"
        n_cand <- nrow(triad_idx)
        ss <- as.integer(s * s)

        # Pre-compute linear indices for 6 edge positions
        lin_ij <- (triad_idx[, 2] - 1L) * s + triad_idx[, 1]
        lin_ji <- (triad_idx[, 1] - 1L) * s + triad_idx[, 2]
        lin_ik <- (triad_idx[, 3] - 1L) * s + triad_idx[, 1]
        lin_ki <- (triad_idx[, 1] - 1L) * s + triad_idx[, 3]
        lin_jk <- (triad_idx[, 3] - 1L) * s + triad_idx[, 2]
        lin_kj <- (triad_idx[, 2] - 1L) * s + triad_idx[, 3]

        # Pre-compute per-individual stubs. Unit eligibility is frozen from
        # the original weighted activity, and stub validation/construction
        # cover only null-eligible units: a malformed cell in a unit the
        # null never touches must not abort the whole run.
        n_ind <- dim(trans)[1]
        observed_activity <- vapply(seq_len(n_ind), function(ind) {
          sum(.motif_unit_matrix(trans, ind))
        }, numeric(1))
        valid_inds <- which(observed_activity >= min_transitions)

        ind_totals <- integer(n_ind)
        rows_stubs <- vector("list", n_ind)
        cols_stubs <- vector("list", n_ind)
        active_row <- matrix(FALSE, n_ind, s)
        active_col <- matrix(FALSE, n_ind, s)

        for (ind in valid_inds) {
          mat_i <- .motif_unit_matrix(trans, ind)
          stubs <- .motif_configuration_stubs(mat_i)
          ind_totals[ind] <- stubs$total
          rows_stubs[[ind]] <- stubs$rows
          cols_stubs[[ind]] <- stubs$cols
          active_row[ind, ] <- stubs$row_degrees > 0L
          active_col[ind, ] <- stubs$col_degrees > 0L
        }

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
        is_003_row <- candidates$type == "003"

        for (ind in valid_inds) {
          mask <- ind_cand_mask[ind, ]
          # Rows this unit can never place an edge into are permuted 003
          # triads — that IS the event for an 003-type row (pattern = "all"),
          # so credit those before skipping the edge computation.
          off <- which(!mask & is_003_row)
          if (length(off)) {
            null_matrix[off, ] <- null_matrix[off, ] + 1L
          }
          if (!any(mask)) next
          wm <- which(mask)
          total <- ind_totals[ind]
          rs <- rows_stubs[[ind]]
          cs <- cols_stubs[[ind]]

          perm_cols <- vapply(seq_len(n_perm),
                              function(p) cs[sample.int(total)],
                              integer(total))

          all_lin <- (perm_cols - 1L) * s + rs
          dim(all_lin) <- NULL
          perm_id <- rep(seq_len(n_perm), each = total)

          presence <- matrix(FALSE, nrow = ss, ncol = n_perm)
          presence[cbind(all_lin, perm_id)] <- TRUE

          # Classify each permuted triple and count it only when it
          # instantiates the row's own MAN type — the observed statistic is
          # "units in which this triple exhibits this type", so the null must
          # measure the same event, not "any of the six edges exists".
          b_ij <- presence[lin_ij[wm], , drop = FALSE]
          b_ji <- presence[lin_ji[wm], , drop = FALSE]
          b_ik <- presence[lin_ik[wm], , drop = FALSE]
          b_ki <- presence[lin_ki[wm], , drop = FALSE]
          b_jk <- presence[lin_jk[wm], , drop = FALSE]
          b_kj <- presence[lin_kj[wm], , drop = FALSE]
          code <- b_ij + 2L * b_ji + 4L * b_ik + 8L * b_ki +
            16L * b_jk + 32L * b_kj
          lookup <- .get_triad_lookup()
          perm_type <- matrix(lookup[code + 1L], nrow = length(wm))
          same_type <- perm_type == candidates$type[wm]

          null_matrix[wm, ] <- null_matrix[wm, ] + same_type
        }

        ns <- .motif_null_stats(candidates$observed, t(null_matrix))
        candidates$expected <- round(ns$mean, 1)
        candidates$z <- round(ns$z, 2)
        candidates$p <- ns$p
        candidates$sig <- ns$significant
        candidates <- candidates[order(.motif_z_rank(candidates$z,
                                                     candidates$p),
                                       decreasing = TRUE), ]
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

  # Internal index keys keep arbitrary node labels unambiguous during
  # aggregation and significance testing; the public result retains the
  # established human-readable `triad` column only.
  if (".triad_key" %in% names(results)) {
    results$.triad_key <- NULL
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
#' Calls \code{motifs(x, named_nodes = TRUE, ...)}. The result has one row per
#' node triple and MAN type. At individual level, \code{observed} counts the
#' units in which the triple has that type, so one triple can occupy several
#' rows. One MAN type can also appear in many rows, each with its own
#' \code{z} and \code{p}. Per-triple significance is plotted by
#' \code{plot(., type = "significance")} and \code{plot(., type = "triads")}.
#' The per-type plots (\code{"types"}, \code{"patterns"}) omit significance
#' for instance results.
#'
#' The \code{"triads"} diagram shows a canonical representative of the MAN
#' isomorphism class of each row. The labels name the participating nodes.
#' Their positions in the diagram do not encode the observed source and sink
#' roles of the nodes.
#'
#' @param ... Arguments forwarded to \code{\link{motifs}()}, which documents
#'   them (\code{x}, \code{actor}, \code{window}, \code{window_type},
#'   \code{pattern}, \code{include}, \code{exclude}, \code{significance},
#'   \code{n_perm}, \code{cores}, \code{min_count}, \code{edge_method},
#'   \code{edge_threshold}, \code{min_transitions}, \code{top}, \code{seed}).
#'   \code{named_nodes} is fixed to \code{TRUE} and must not be supplied.
#' @return A \code{cograph_motif_result} object with \code{named_nodes = TRUE},
#'   described in \code{\link{motifs}()}, or NULL with a message when no triple
#'   passes the filters. Its results table has the columns \code{triad},
#'   \code{node1}, \code{node2}, \code{node3}, \code{type} and
#'   \code{observed}, and with \code{significance = TRUE} also
#'   \code{expected}, \code{z}, \code{p} and \code{sig}. Its
#'   \code{type_summary} counts the node triples of each MAN type.
#' @examples
#' subgraphs(regulation_net, significance = FALSE, min_count = 1)
#' @seealso [motifs()]
#' @family motifs
#' @export
subgraphs <- function(...) motifs(..., named_nodes = TRUE)


#' @noRd
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


#' @rdname motifs
#' @param row.names,optional Standard \code{\link[base]{as.data.frame}}
#'   arguments. \code{row.names} replaces the default row names;
#'   \code{optional} is ignored.
#' @param ... Unused.
#' @param what Which table \code{as.data.frame()} returns, either
#'   \code{"results"} (default) or \code{"types"}. The Value section lists the
#'   columns of each.
#' @method as.data.frame cograph_motif_result
#' @export
as.data.frame.cograph_motif_result <- function(x, row.names = NULL,
                                               optional = FALSE, ...,
                                               what = c("results", "types")) {
  what <- match.arg(what)
  df <- if (what == "types") {
    tab <- x$type_summary
    data.frame(type = names(tab), count = as.integer(tab),
               stringsAsFactors = FALSE)
  } else {
    out <- x$results
    rownames(out) <- NULL
    out
  }
  if (!is.null(row.names)) {
    rownames(df) <- row.names
  }
  df
}


#' @rdname plot-results
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
    sig_df <- .motif_drop_na_z_rows(x$results)
    if (nrow(sig_df) == 0) {
      stop("No motif rows with a finite z-score to plot.", call. = FALSE)
    }
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
      sig_df$label <- make.unique(paste0(sig_df$triad, tag))
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


#' @rdname plot-results
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
