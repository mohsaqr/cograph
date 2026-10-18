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
#' networks that store sequence data (such as \code{as_cograph()} of a tna or
#' a network from \code{Nestimate::build_network()}), and edge lists (or
#' networks built from edge lists) with an actor column. Matrices, igraph
#' objects and other networks are analyzed as one aggregate network. The adjacency is always
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
#' Self-loops are removed before counting. They are also removed before the
#' null is constructed, unless \code{count_repeats = TRUE}, in which case they
#' are shuffled with the other transitions.
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
#'   edge-list data frame. An edge list has the columns \code{from} and
#'   \code{to} (any case), an optional weight column \code{weight} or
#'   \code{count} (1 per row otherwise), the optional actor column described
#'   under \code{actor}, and an optional order column (\code{timestamp},
#'   \code{time}, \code{seq}, \code{step} or \code{order}) that sorts the
#'   transitions within each actor before windowing. An edge list without
#'   \code{from} or \code{to} raises an error of class
#'   \code{cograph_bad_edgelist}. For \code{as.data.frame()}, a
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
#'   transitions are cut into windows of this many transitions, and every
#'   window is analysed as one unit (individual level). \code{window} needs
#'   an actor column; without one it raises an error of class
#'   \code{cograph_window_needs_actor}. NULL (default) applies no windowing.
#' @param window_type Character. \code{"rolling"} (default) windows start at
#'   every transition and overlap; \code{"tumbling"} windows are consecutive
#'   and do not overlap. Used only when \code{window} is set.
#' @param pattern Which MAN triad types to include in the analysis:
#'   \describe{
#'     \item{\code{"triangle"}}{(default) The 7 closed triangle types 030C,
#'       030T, 120C, 120D, 120U, 210 and 300.}
#'     \item{\code{"network"}}{All types except 003 (empty), 012 (single edge)
#'       and 021C (chain).}
#'     \item{\code{"closed"}}{The same 7 closed types as \code{"triangle"}:
#'       every pair of the three nodes is linked.}
#'     \item{\code{"all"}}{All 16 MAN types.}
#'   }
#' @param include Character vector of MAN types to keep. When supplied,
#'   \code{pattern} and \code{exclude} are ignored, and the result records
#'   the pattern as \code{"custom"}.
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
#'   6 directed edge weights). Default 2 in instance mode, since at individual
#'   level a pattern seen in a single unit cannot recur, and NULL (no filter)
#'   in census mode. At aggregate level the threshold applies to the edge
#'   weight mass, so for probability matrices lower it (for example
#'   \code{min_count = 1} or \code{NULL}).
#' @param edge_method Method for determining edge presence: \code{"any"}
#'   (default; any positive edge), \code{"expected"} (ratio of the observed
#'   weight to the weight expected from the row and column totals), or
#'   \code{"percent"} (edge weight divided by the six-edge triad total).
#' @param edge_threshold Threshold for \code{"expected"} or \code{"percent"}
#'   methods. For \code{"expected"}, 1.5 means 50 percent above expected. For
#'   \code{"percent"}, values at or below 1 are proportions and values above 1
#'   are percentages. Default 1.5.
#' @param min_transitions Minimum total edge weight for a unit to be included,
#'   counted after self-loops are removed unless \code{count_repeats = TRUE}.
#'   Default 3 in instance mode, the
#'   fewest transitions that can form a closed triad (030C and 030T have three
#'   edges), and 5 in census mode. At aggregate level the network is the only
#'   unit, so a network whose weights sum to less than this value gives NULL;
#'   for a probability matrix set \code{min_transitions = 0}.
#' @param top Integer or NULL. Only the first \code{top} rows of the results
#'   are kept. NULL (default) keeps all rows.
#' @param seed Random seed. Default NULL. When supplied, the caller's RNG state
#'   is restored on exit.
#' @param family Which rows the instance-mode significance test covers.
#'   \code{"observed"} (default) tests the triple-type rows observed in at
#'   least \code{min_count} units. \code{"all"} tests every node triple
#'   crossed with every retained MAN type, including combinations no unit
#'   shows, and \code{min_count} then filters only the rows returned. Testing
#'   only observed rows selects on the outcome: a rare configuration enters
#'   the test only when it happens to occur, which inflates the false-positive
#'   rate well above the nominal level. \code{"all"} keeps it at or below the
#'   nominal level. The full family has \code{choose(n_states, 3)} rows per
#'   retained labelled pattern with \code{strict = TRUE} (for example six
#'   for 030T, two for 030C and one for 300), or per retained MAN type with
#'   \code{strict = FALSE}, so the permutation null grows accordingly. Ignored in
#'   census mode and without individual-level significance.
#' @param p_adjust Multiplicity correction applied to the p-values of every
#'   tested row, one of \code{\link[stats]{p.adjust.methods}}. Default
#'   \code{"none"}. The adjusted values are returned in \code{p_adj} and
#'   \code{sig} is \code{p_adj < 0.05}. With \code{family = "all"} the
#'   adjustment covers the full family before \code{min_count} is applied.
#'   Plot colours and stars follow \code{p_adj}, so they match \code{sig}.
#' @param strict Logical. In instance mode, whether a row is one labelled
#'   pattern of a triple (\code{TRUE}, default) or one MAN type of a triple
#'   (\code{FALSE}). A MAN type fixes the shape of a triad but not which node
#'   plays which role: the cycles A to B to C to A and A to C to B to A are
#'   both 030C. With \code{strict = TRUE} the two are separate rows, a unit
#'   counts toward a row only when the triple shows exactly that set of
#'   arrows, and the null counts the same exact pattern. With
#'   \code{strict = FALSE} rows pool every labelled pattern of the type and
#'   \code{pattern} reports the most frequent one. Ignored in census mode.
#' @param count_repeats Logical. Whether repeated codes (self-transitions such
#'   as A to A) count as transitions. Default \code{FALSE}: repeats are
#'   removed before anything else, so only transitions between different codes
#'   decide which units are analysed and enter the permutation null. With
#'   \code{TRUE}, repeats count toward \code{min_transitions} (a unit coded
#'   A, A, A, B, C has four transitions instead of two) and are shuffled with
#'   the other transitions in the null, which treats repeated codes as
#'   distinct events whose order is random. The null then also reflects how
#'   often units repeat: units that repeat a lot have fewer ties between
#'   different codes than their shuffles, which raises the number of dense
#'   triads the null expects. Observed triads never contain a
#'   repeat in either case, because a triad is made of ties between three
#'   different nodes.
#'
#' @return A \code{cograph_motif_result} object, or NULL with a message when no
#'   motif passes the filters. The object is a list with the elements below.
#'   \describe{
#'     \item{results}{Data frame of results. In census mode it has one row per
#'       retained, observed MAN type and the columns \code{type} and
#'       \code{count}. In instance mode it has one row per node triple and
#'       labelled pattern (\code{strict = TRUE}) or per node triple and MAN
#'       type (\code{strict = FALSE}), with the columns \code{triad},
#'       \code{node1}, \code{node2}, \code{node3}, \code{type},
#'       \code{observed}, \code{pattern}, \code{pattern_n} and
#'       \code{pattern_code}. At individual level, \code{observed} is the
#'       number of units in which the triple shows that pattern (or type), so
#'       one triple can occupy several rows. \code{pattern} is the labelled
#'       edge set of the row (for example \code{"A -> B, B <-> C"}). With
#'       \code{strict = FALSE} a row pools patterns that differ in direction,
#'       so \code{pattern} is the most frequent of them and \code{pattern_n}
#'       the number of units showing it; at individual level with
#'       \code{strict = TRUE} \code{pattern_n} equals \code{observed}. \code{pattern_code} is
#'       the same edge set as a six-bit code in
#'       \code{node1}, \code{node2}, \code{node3} order (1: node1 to node2,
#'       2: node2 to node1, 4: node1 to node3, 8: node3 to node1, 16: node2 to
#'       node3, 32: node3 to node2). Modal ties go to the smallest code. Rows
#'       that no unit shows (\code{family = "all"}) have \code{observed = 0}
#'       and \code{pattern_n = 0}; their pattern is the row's own pattern
#'       when \code{strict = TRUE} and missing otherwise. At aggregate level
#'       \code{pattern_n} is 1. With \code{significance = TRUE}, the columns
#'       \code{expected} (null mean), \code{z}, \code{p}, \code{p_adj} and
#'       \code{sig} are added, unrounded, and the rows are sorted by decreasing
#'       absolute z-score. Otherwise the rows are sorted by decreasing count.
#'       Census results carry \code{expected}, \code{z}, \code{p},
#'       \code{p_adj} and \code{sig} in the same way.}
#'     \item{type_summary}{Named \code{table} of counts per MAN type, sorted in
#'       decreasing order. In census mode it holds the \code{count} column. In
#'       instance mode it holds the number of result rows of each type: node
#'       triple and labelled pattern with \code{strict = TRUE}, node triple
#'       and type with \code{strict = FALSE}.}
#'     \item{level}{\code{"individual"} when the input carried per-unit data,
#'       otherwise \code{"aggregate"}.}
#'     \item{named_nodes}{The value of the \code{named_nodes} argument.}
#'     \item{n_units}{Number of units analyzed: the units whose transition
#'       total (loop-free unless \code{count_repeats = TRUE}) reaches
#'       \code{min_transitions}. 1 at aggregate
#'       level.}
#'     \item{n_units_total}{Number of units supplied, before the
#'       \code{min_transitions} filter.}
#'     \item{params}{List of the analysis settings: \code{labels},
#'       \code{n_states}, \code{pattern} (\code{"custom"} when
#'       \code{include} is used), \code{include}, \code{edge_method},
#'       \code{edge_threshold}, \code{significance}, \code{n_perm},
#'       \code{min_count}, \code{min_transitions}, \code{count_repeats},
#'       \code{family},
#'       \code{p_adjust}, \code{strict}, \code{window}, \code{window_type} and
#'       \code{actor} (the actor column used, or NULL at aggregate level).}
#'   }
#'
#'   \code{as.data.frame()} returns one of these tables as a plain data frame.
#'   With \code{what = "results"} (default) it returns the \code{results}
#'   table described above. With \code{what = "types"} it returns one row per
#'   MAN type with columns \code{type} and \code{count}, where \code{count} is
#'   the number of triads of that type in a census or the number of result
#'   rows of that type in instance mode (see \code{type_summary}).
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
                   min_count = if (named_nodes) 2L else NULL,
                   edge_method = c("any", "expected", "percent"),
                   edge_threshold = 1.5,
                   min_transitions = if (named_nodes) 3 else 5,
                   top = NULL,
                   seed = NULL,
                   family = c("observed", "all"),
                   p_adjust = "none",
                   strict = TRUE,
                   count_repeats = FALSE) {

  .user_set_pattern <- !missing(pattern)

  window_type <- match.arg(window_type)
  pattern <- match.arg(pattern)
  edge_method <- match.arg(edge_method)
  family <- match.arg(family)
  stopifnot(
    "`strict` must be TRUE or FALSE" = isTRUE(strict) || isFALSE(strict),
    "`count_repeats` must be TRUE or FALSE" =
      isTRUE(count_repeats) || isFALSE(count_repeats),
    "`p_adjust` must be one of stats::p.adjust.methods" =
      is.character(p_adjust) && length(p_adjust) == 1L &&
      p_adjust %in% stats::p.adjust.methods
  )
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
  actor_used <- NULL

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

    if (inherits(raw_data, "tna_seq_data")) {
      # as_cograph(<tna>) keeps tna's integer-coded sequences. The per-unit
      # transition counts do not depend on the model type, so build them the
      # way the tna path does.
      init_fn <- .get_tna_initialize_model()
      model <- init_fn(raw_data, "relative", character(0),
                       list(windowed = FALSE), transitions = TRUE)
      trans <- model$trans
      labels <- attr(raw_data, "labels")
      level <- "individual"
      n_units <- dim(trans)[1]

    } else if (is.data.frame(raw_data) &&
        all(c("from", "to") %in% tolower(names(raw_data)))) {

      actor_col <- actor
      if (is.null(actor_col)) {
        actor_col <- .detect_actor_column(raw_data)
      }
      .motif_check_window_actor(window, actor_col)
      actor_used <- actor_col

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
    .motif_check_window_actor(window, actor_col)
    actor_used <- actor_col
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

  # Self-loops are outside the induced triad universe, so observed counting
  # always uses the loop-free array. By default loops are also kept out of
  # the activity gate and the null, so a loop can never be shuffled into an
  # ordinary motif edge. count_repeats = TRUE opts in to both: activity is
  # taken before stripping, and null stubs come from the unstripped array.
  # Every gate below (observed and null) reads the one `eligible` vector.
  unit_activity <- function(arr) {
    vapply(seq_len(dim(arr)[1]),
           function(ind) sum(.motif_unit_matrix(arr, ind)), numeric(1))
  }
  activity <- if (count_repeats) unit_activity(trans) else NULL
  trans_with_loops <- if (count_repeats) trans else NULL
  trans <- .motif_strip_loops(trans)
  null_trans <- trans_with_loops %||% trans
  activity <- activity %||% unit_activity(trans)
  eligible <- activity >= min_transitions

  # The activity gate drops units, so the number analysed can be far
  # smaller than the number supplied. Report both.
  n_units_total <- n_units
  n_units <- sum(eligible)

  if (!named_nodes) {
    # ---- CENSUS MODE: count MAN type frequencies per unit ----
    type_counts_per_unit <- lapply(seq_len(dim(trans)[1]), function(ind) {
      if (!eligible[ind]) return(NULL)
      mat <- .motif_unit_matrix(trans, ind)

      expected_mat <- if (edge_method == "expected") {
        .motif_expected_matrix(mat)
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

        mc_idx <- stats::setNames(seq_len(nrow(mc)), mc$motif)
        for (ri in seq_len(nrow(results))) {
          tp <- results$type[ri]
          mc_row <- mc_idx[tp]
          if (!is.na(mc_row)) {
            results$expected[ri] <- mc$null_mean[mc_row]
            results$z[ri] <- mc$z_score[mc_row]
            results$p[ri] <- mc$p_value[mc_row]
          }
        }
        results$p_adj <- stats::p.adjust(results$p, method = p_adjust)
        results$sig <- results$p_adj < 0.05
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
        valid_c <- which(eligible)
        # Stub validation and construction cover only null-eligible units: a
        # malformed cell in a unit the null never touches must not abort the
        # whole run.
        rows_stubs_c <- vector("list", n_ind_c)
        cols_stubs_c <- vector("list", n_ind_c)
        for (ind in valid_c) {
          stubs_c <- .motif_configuration_stubs(
            .motif_unit_matrix(null_trans, ind), keep_loops = count_repeats
          )
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
        results$expected <- ns$mean
        results$z <- ns$z
        results$p <- ns$p
        results$p_adj <- stats::p.adjust(ns$p, method = p_adjust)
        results$sig <- results$p_adj < 0.05

        results <- results[order(.motif_z_rank(results$z, results$p),
                                 decreasing = TRUE), ]
        rownames(results) <- NULL
      }
    }

  } else {
    # ---- INSTANCE MODE: list specific node triples ----
    all_results <- lapply(seq_len(dim(trans)[1]), function(ind) {
      if (!eligible[ind]) return(NULL)
      mat <- .motif_unit_matrix(trans, ind)

      expected_mat <- if (edge_method == "expected") {
        .motif_expected_matrix(mat)
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
                 weight = counted$weight, code = counted$code,
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
      if (strict) {
        # Strict: a row is one labelled pattern, so a unit counts only when
        # the triple shows exactly these arrows, not merely the same shape.
        results <- stats::aggregate(
          unit ~ .triad_key + triad + node1 + node2 + node3 + type + code,
          data = combined, FUN = length
        )
        results$observed <- as.integer(results$unit)
        results$pattern_n <- results$observed
        results$pattern_code <- as.integer(results$code)
      } else {
        obs <- stats::aggregate(
          unit ~ .triad_key + triad + node1 + node2 + node3 + type,
          data = combined, FUN = length
        )
        names(obs)[7] <- "observed"
        results <- merge(obs, .motif_modal_patterns(combined),
                         by = c(".triad_key", "type"), sort = FALSE)
      }
      results <- results[order(-results$observed, results$.triad_key,
                               results$pattern_code), ]
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
        pattern_n = 1L,
        pattern_code = combined$code[first_idx],
        stringsAsFactors = FALSE
      )
      results <- results[order(results$observed, decreasing = TRUE), ]
    }
    results$pattern <- .motif_pattern_label(results$pattern_code,
                                            results$node1, results$node2,
                                            results$node3)
    results <- results[c(".triad_key", "triad", "node1", "node2", "node3",
                         "type", "observed", "pattern", "pattern_n",
                         "pattern_code")]
    rownames(results) <- NULL

    # ---- INSTANCE SIGNIFICANCE (directed weighted stub-matching model) ----
    if (significance && level != "individual") {
      warning("Instance-mode significance requires individual-level data ",
              "(an actor/session column with multiple units); skipping the ",
              "permutation test.", call. = FALSE)
      significance <- FALSE
    }
    if (significance && level == "individual") {
      if (family == "all") {
        # Test every triple x retained type, including those no unit shows.
        # Selecting only observed rows conditions on the outcome and inflates
        # the false-positive rate; min_count then only filters what is shown.
        candidates <- .motif_full_family(results, labels, final_include,
                                         final_exclude, strict)
      } else if (!is.null(min_count)) {
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
        valid_inds <- which(eligible)

        ind_totals <- integer(n_ind)
        rows_stubs <- vector("list", n_ind)
        cols_stubs <- vector("list", n_ind)
        active_row <- matrix(FALSE, n_ind, s)
        active_col <- matrix(FALSE, n_ind, s)

        for (ind in valid_inds) {
          mat_i <- .motif_unit_matrix(null_trans, ind)
          stubs <- .motif_configuration_stubs(mat_i, keep_loops = count_repeats)
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
          same <- if (strict) {
            code == candidates$pattern_code[wm]
          } else {
            perm_type == candidates$type[wm]
          }

          null_matrix[wm, ] <- null_matrix[wm, ] + same
        }

        ns <- .motif_null_stats(candidates$observed, t(null_matrix))
        candidates$expected <- ns$mean
        candidates$z <- ns$z
        candidates$p <- ns$p
        # The adjustment runs over every tested row, before any display
        # filter, so the family is the one the p-values were computed for.
        candidates$p_adj <- stats::p.adjust(ns$p, method = p_adjust)
        candidates$sig <- candidates$p_adj < 0.05
        if (family == "all" && !is.null(min_count)) {
          candidates <- candidates[candidates$observed >= min_count, ]
          if (nrow(candidates) == 0) {
            message("No motifs with count >= ", min_count, ".")
            return(NULL)
          }
        }
        candidates <- candidates[order(.motif_z_rank(candidates$z,
                                                     candidates$p),
                                       decreasing = TRUE), ]
        rownames(candidates) <- NULL
      }
      if (nrow(candidates) == 0) {
        message("No motifs with count >= ", min_count %||% 1L, ".")
        return(NULL)
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
  if (named_nodes && !.user_set_pattern && length(include) == 0) {
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
      n_units_total = n_units_total,
      params = list(
        labels = labels,
        n_states = s,
        pattern = if (length(include) > 0) "custom" else pattern,
        include = include,
        edge_method = edge_method,
        edge_threshold = edge_threshold,
        significance = significance,
        n_perm = n_perm,
        min_count = min_count,
        min_transitions = min_transitions,
        count_repeats = count_repeats,
        family = family,
        p_adjust = p_adjust,
        strict = strict,
        window = window,
        window_type = window_type,
        actor = actor_used
      )
    ),
    class = "cograph_motif_result"
  )
}


#' Extract Specific Motif Instances (Subgraphs)
#'
#' Calls \code{motifs(x, named_nodes = TRUE, ...)}. By default
#' (\code{strict = TRUE}) the result has one row per node triple and labelled
#' pattern: the shape and the direction of every arrow are fixed, so a cycle
#' and its reverse are different rows. At individual level, \code{observed}
#' counts the units in which the triple shows exactly that pattern, so one
#' triple can occupy several rows. \code{strict = FALSE} pools the patterns of
#' each MAN type into one row. One MAN type can also appear in many rows,
#' each with its own
#' \code{z} and \code{p}. Per-triple significance is plotted by
#' \code{plot(., type = "significance")} and \code{plot(., type = "triads")}.
#' The per-type plots (\code{"types"}, \code{"patterns"}) omit significance
#' for instance results.
#'
#' The \code{"triads"} diagram draws the labelled pattern of each row (its
#' \code{pattern} column) with \code{node1} at the top, \code{node2} bottom
#' left and \code{node3} bottom right, so the arrows show who sends to whom.
#' With \code{strict = FALSE} it draws the most frequent pattern of the row,
#' and when the row pools other patterns the caption gives how many units
#' match the drawing, for example \code{"n=21 (13 as drawn)"}.
#'
#' @param ... Arguments forwarded to \code{\link{motifs}()}, which documents
#'   them (\code{x}, \code{actor}, \code{window}, \code{window_type},
#'   \code{pattern}, \code{include}, \code{exclude}, \code{significance},
#'   \code{n_perm}, \code{cores}, \code{min_count}, \code{edge_method},
#'   \code{edge_threshold}, \code{min_transitions}, \code{top}, \code{seed},
#'   \code{family}, \code{p_adjust}, \code{strict}, \code{count_repeats}).
#'   \code{named_nodes} is fixed to \code{TRUE} and must not be supplied.
#' @return A \code{cograph_motif_result} object with \code{named_nodes = TRUE},
#'   described in \code{\link{motifs}()}, or NULL with a message when no triple
#'   passes the filters. Its results table has the columns \code{triad},
#'   \code{node1}, \code{node2}, \code{node3}, \code{type},
#'   \code{observed}, \code{pattern}, \code{pattern_n} and
#'   \code{pattern_code}, and with \code{significance = TRUE} also
#'   \code{expected}, \code{z}, \code{p}, \code{p_adj} and \code{sig}. Its
#'   \code{type_summary} counts the node triples of each MAN type.
#' @examples
#' subgraphs(regulation_net, significance = FALSE, min_count = 1)
#' @seealso [motifs()]
#' @family motifs
#' @export
subgraphs <- function(...) motifs(..., named_nodes = TRUE)


#' @rdname print-motifs
#' @method print cograph_motif_result
#' @export
print.cograph_motif_result <- function(x, ...) {
  mode_label <- if (x$named_nodes) "Motif Subgraphs" else "Motif Census"
  cat(mode_label, "\n")
  cat("Level:", x$level)
  if (x$level == "individual") {
    n_total <- x$n_units_total %||% x$n_units
    cat(" |", x$n_units, "of", n_total, "units analysed")
    if (n_total > x$n_units) {
      cat(" (", n_total - x$n_units, " below min_transitions = ",
          x$params$min_transitions,
          if (isTRUE(x$params$count_repeats)) ", repeats counted", ")",
          sep = "")
    }
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
  shown <- x$results[seq_len(n_show), setdiff(names(x$results),
                                              "pattern_code"), drop = FALSE]
  rounded <- intersect(c("expected", "z"), names(shown))
  shown[rounded] <- lapply(shown[rounded], round, digits = 2)
  print(shown, row.names = FALSE)

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
#' @param significant Logical. When \code{TRUE}, \code{as.data.frame()} keeps
#'   only the rows whose \code{sig} flag is \code{TRUE}. It applies to
#'   \code{what = "results"} and needs a result computed with
#'   \code{significance = TRUE}; otherwise a \code{cograph_no_significance}
#'   error is raised. Default \code{FALSE}.
#' @method as.data.frame cograph_motif_result
#' @export
as.data.frame.cograph_motif_result <- function(x, row.names = NULL,
                                               optional = FALSE, ...,
                                               what = c("results", "types"),
                                               significant = FALSE) {
  what <- match.arg(what)
  stopifnot("`significant` must be TRUE or FALSE" =
              is.logical(significant) && length(significant) == 1L &&
              !is.na(significant))
  if (significant && what == "results") {
    x <- .motif_significant_rows(x)
  }
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
                                       significant = FALSE,
                                       ...) {
  type <- match.arg(type)
  stopifnot("`significant` must be TRUE or FALSE" =
              is.logical(significant) && length(significant) == 1L &&
              !is.na(significant))
  x_in <- x
  if (significant && type == "triads") {
    x <- .motif_significant_rows(x)
  }
  # The pattern view keeps its four-column grid unless `ncol` is given.
  pattern_ncol <- if (missing(ncol)) 4L else ncol

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
      .plot_motif_patterns(x, n = n, colors = colors, ncol = pattern_ncol,
                           combined = combined, ...)
    }
    return(invisible(x_in))

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
      type_p <- stats::setNames(.motif_decision_p(x$results), x$results$type)
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
    p_dec <- .motif_decision_p(sig_df)
    sig_df$direction <- ifelse(
      !is.na(p_dec) & p_dec < 0.05 & sig_df$z > 0, "over",
      ifelse(!is.na(p_dec) & p_dec < 0.05 & sig_df$z < 0,
             "under", "ns")
    )
    p_label <- .motif_p_label(x)
    p <- ggplot2::ggplot(sig_df, ggplot2::aes(
      x = stats::reorder(.data$label, abs(.data$z)),
      y = .data$z,
      fill = .data$direction)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c(over = colors[2], under = colors[1], ns = "#9E9E9E"),
        labels = c(over = sprintf("Over-represented (%s<.05)", p_label),
                   under = sprintf("Under-represented (%s<.05)", p_label),
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
    .plot_motif_patterns(x, n = n, colors = colors, ncol = pattern_ncol,
                         combined = combined, ...)
    return(invisible(x_in))
  }

  invisible(x_in) # nocov — all type branches return above
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


# Keep only the rows flagged significant. The plot method and the tidy
# accessor share it so `significant = TRUE` selects the same rows in both.
#' @noRd
.motif_significant_rows <- function(x) {
  if (!isTRUE(x$params$significance) || !"sig" %in% names(x$results)) {
    stop(errorCondition(
      "`significant = TRUE` needs a result computed with `significance = TRUE`.",
      class = "cograph_no_significance", call = NULL))
  }
  x$results <- x$results[x$results$sig %in% TRUE, , drop = FALSE]
  rownames(x$results) <- NULL
  if (!isTRUE(x$named_nodes)) {
    # Census panels are drawn from type_summary, one per MAN type.
    keep <- names(x$type_summary) %in% x$results$type
    x$type_summary <- x$type_summary[keep]
  }
  x
}


# Modal labelled edge set of every (triple, type) across units. One MAN class
# pools labelled patterns that differ in who sends to whom, including a cycle
# and its reverse, so the class alone cannot say what the units did. Ties go
# to the smallest edge code so the result is deterministic.
#' @noRd
.motif_modal_patterns <- function(combined) {
  pc <- stats::aggregate(unit ~ .triad_key + type + code, data = combined,
                         FUN = length)
  pc <- pc[order(pc$.triad_key, pc$type, -pc$unit, pc$code), ]
  pc <- pc[!duplicated(pc[c(".triad_key", "type")]), ]
  data.frame(.triad_key = pc$.triad_key, type = pc$type,
             pattern_n = as.integer(pc$unit),
             pattern_code = as.integer(pc$code),
             stringsAsFactors = FALSE)
}

# Readable labelled edge set for a six-bit triad code read in node1, node2,
# node3 order (bits: 1->2, 2->1, 1->3, 3->1, 2->3, 3->2).
#' @noRd
.motif_pattern_label <- function(code, node1, node2, node3) {
  vapply(seq_along(code), function(r) {
    if (is.na(code[r])) return(NA_character_)
    b <- bitwAnd(code[r], c(1L, 2L, 4L, 8L, 16L, 32L)) > 0L
    nodes <- c(node1[r], node2[r], node3[r])
    pairs <- list(c(1L, 2L, 1L, 2L), c(1L, 3L, 3L, 4L), c(2L, 3L, 5L, 6L))
    parts <- vapply(pairs, function(pr) {
      a <- nodes[pr[1]]
      z <- nodes[pr[2]]
      fwd <- b[pr[3]]
      back <- b[pr[4]]
      if (fwd && back) paste(a, "<->", z)
      else if (fwd) paste(a, "->", z)
      else if (back) paste(z, "->", a)
      else NA_character_
    }, character(1))
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0L) "none" else paste(parts, collapse = ", ")
  }, character(1))
}

# Every node triple crossed with every retained MAN type, joined to the
# observed rows. Unobserved combinations get observed = 0 and no pattern.
#' @noRd
.motif_full_family <- function(results, labels, include, exclude,
                               strict = FALSE) {
  types <- .triad_type_names()
  if (!is.null(include) && length(include) > 0) {
    types <- types[types %in% include]
  }
  types <- setdiff(types, exclude)
  idx <- .triad_indices(length(labels))
  # Strict rows are labelled patterns: every six-bit code whose MAN type is
  # retained. Otherwise rows are the types themselves.
  lookup <- .get_triad_lookup()
  codes <- if (strict) which(lookup %in% types) - 1L else NA_integer_
  cells <- if (strict) data.frame(type = lookup[codes + 1L], code = codes,
                                  stringsAsFactors = FALSE)
           else data.frame(type = types, code = NA_integer_,
                           stringsAsFactors = FALSE)
  n_cells <- nrow(cells)
  grid <- data.frame(
    .triad_key = rep(paste(idx$i, idx$j, idx$k, sep = "\r"), times = n_cells),
    triad = rep(paste(labels[idx$i], labels[idx$j], labels[idx$k],
                      sep = " - "), times = n_cells),
    node1 = rep(labels[idx$i], times = n_cells),
    node2 = rep(labels[idx$j], times = n_cells),
    node3 = rep(labels[idx$k], times = n_cells),
    type = rep(cells$type, each = idx$n),
    stringsAsFactors = FALSE
  )
  keys <- c(".triad_key", "type")
  if (strict) {
    grid$pattern_code <- rep(cells$code, each = idx$n)
    keys <- c(keys, "pattern_code")
  }
  obs_cols <- unique(c(keys, "observed", "pattern", "pattern_n",
                       "pattern_code"))
  full <- merge(grid, results[obs_cols], by = keys, all.x = TRUE,
                sort = FALSE)
  full$observed[is.na(full$observed)] <- 0L
  full$pattern_n[is.na(full$pattern_n)] <- 0L
  if (strict) {
    full$pattern <- .motif_pattern_label(full$pattern_code, full$node1,
                                         full$node2, full$node3)
  }
  stopifnot("full family must have one row per triple and cell" =
              nrow(full) == idx$n * n_cells && !anyDuplicated(full[keys]))
  full[names(results)]
}
