# Golden-file comparison for the centrality surface.
#
# Shared by tests/testthat/test-centrality-golden.R and the local runner
# local_testing_and_equivalence/golden/compare_golden.R. The golden file is
# produced by make_golden.R from the igraph-backed code before any measure
# was ported (docs/igraph-removal-plan.md, Phase 0).

.golden_path <- function(tag = "baseline") {
  committed <- testthat::test_path("golden", paste0("golden_", tag, ".rds"))
  if (file.exists(committed)) return(committed)
  local <- testthat::test_path("..", "..", "local_testing_and_equivalence", "golden",
                               paste0("golden_", tag, ".rds"))
  if (file.exists(local)) return(local)
  NA_character_
}

# Compare two captured results (value + conditions). Returns a data.frame of
# differences, zero rows when equivalent. `tol` applies to finite numbers;
# NA/NaN/Inf must match exactly; errors must match by class.
.golden_diff_capture <- function(ref, cur, label, tol) {
  rv <- ref$value; cv <- cur$value
  ref_err <- inherits(rv, "golden_error"); cur_err <- inherits(cv, "golden_error")
  if (ref_err && grepl("reached (elapsed|CPU) time limit", rv$error)) {
    # The reference itself timed out: there is no value to compare against.
    return(data.frame(label = label, column = "<error>", kind = "reference_timeout",
                      reference = rv$error, current = if (cur_err) cv$error else "value",
                      max_abs_diff = NA_real_, stringsAsFactors = FALSE))
  }
  if (ref_err || cur_err) {
    if (ref_err && cur_err) {
      same <- identical(rv$class[1L], cv$class[1L])
      return(if (same) NULL else data.frame(
        label = label, column = "<error>", kind = "error_class",
        reference = rv$class[1L], current = cv$class[1L], max_abs_diff = NA_real_,
        stringsAsFactors = FALSE))
    }
    return(data.frame(
      label = label, column = "<error>", kind = if (ref_err) "reference_errored" else "current_errored",
      reference = if (ref_err) rv$error else "value", current = if (cur_err) cv$error else "value",
      max_abs_diff = NA_real_, stringsAsFactors = FALSE))
  }
  if (is.data.frame(rv)) {
    cols <- union(names(rv), names(cv))
    rows <- lapply(cols, function(col) {
      if (!col %in% names(cv)) return(data.frame(label = label, column = col, kind = "column_missing",
                                                 reference = "present", current = "absent",
                                                 max_abs_diff = NA_real_, stringsAsFactors = FALSE))
      if (!col %in% names(rv)) return(data.frame(label = label, column = col, kind = "column_added",
                                                 reference = "absent", current = "present",
                                                 max_abs_diff = NA_real_, stringsAsFactors = FALSE))
      .golden_diff_vector(rv[[col]], cv[[col]], label, col, tol)
    })
    return(do.call(rbind, rows))
  }
  .golden_diff_vector(rv, cv, label, "<value>", tol)
}

.golden_diff_vector <- function(a, b, label, col, tol) {
  if (length(a) != length(b)) {
    return(data.frame(label = label, column = col, kind = "length",
                      reference = length(a), current = length(b),
                      max_abs_diff = NA_real_, stringsAsFactors = FALSE))
  }
  if (is.numeric(a) && is.numeric(b)) {
    a <- as.numeric(a); b <- as.numeric(b)
    na_a <- is.na(a); na_b <- is.na(b)
    nan_a <- is.nan(a); nan_b <- is.nan(b)
    inf_a <- is.infinite(a); inf_b <- is.infinite(b)
    special_ok <- identical(na_a, na_b) && identical(nan_a, nan_b) &&
      identical(inf_a, inf_b) && all(a[inf_a] == b[inf_b])
    fin <- !na_a & !inf_a & !na_b & !inf_b
    scale <- pmax(abs(a[fin]), abs(b[fin]), 1)
    diff <- abs(a[fin] - b[fin]) / scale
    if (special_ok && all(diff <= tol)) return(NULL)
    return(data.frame(label = label, column = col,
                      kind = if (special_ok) "numeric" else "special_values",
                      reference = if (special_ok) format(a[fin][which.max(diff)], digits = 17) else paste(sum(na_a), sum(nan_a), sum(inf_a)),
                      current = if (special_ok) format(b[fin][which.max(diff)], digits = 17) else paste(sum(na_b), sum(nan_b), sum(inf_b)),
                      max_abs_diff = if (length(diff)) max(diff) else NA_real_,
                      stringsAsFactors = FALSE))
  }
  if (identical(a, b)) return(NULL)
  data.frame(label = label, column = col, kind = "non_numeric",
             reference = paste(head(a, 3), collapse = ","), current = paste(head(b, 3), collapse = ","),
             max_abs_diff = NA_real_, stringsAsFactors = FALSE)
}

# Capture a value plus every condition; never let one call abort a run.
.golden_capture_fn <- function(time_limit = 120, keep_conditions = TRUE) {
  function(expr) {
    warnings <- character(); messages <- character()
    t0 <- proc.time()[["elapsed"]]
    setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = TRUE), add = TRUE)
    value <- withCallingHandlers(
      tryCatch(expr, error = function(e) structure(list(error = conditionMessage(e), class = class(e)),
                                                   class = "golden_error")),
      warning = function(w) { if (keep_conditions) warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") },
      message = function(mm) { if (keep_conditions) messages <<- c(messages, conditionMessage(mm)); invokeRestart("muffleMessage") })
    list(value = value, warnings = warnings, messages = messages,
         elapsed = proc.time()[["elapsed"]] - t0)
  }
}

# One record per measure: try the bulk call first (fast), and when it fails
# fall back to one call per measure so a single failing measure (singular
# `power`, an igraph leak) cannot hide the others. Column names carry the
# mode suffix for mode-aware measures; strip it so records are keyed by
# measure name.
.golden_measure_records <- function(m, measures, mode, weighted, capture) {
  mode_measures <- .cg_mode_measures()
  col_of <- function(mm) if (mm %in% mode_measures) paste0(mm, "_", mode) else mm
  # Bisection: a chunk that computes yields one record per measure; a chunk
  # that fails is split until the failing measure is isolated, so k failures
  # cost O(k log n) calls instead of n.
  records <- function(chunk) {
    res <- capture(centrality(m, measures = chunk, mode = mode, weighted = weighted))
    if (!inherits(res$value, "golden_error")) {
      df <- res$value
      out <- lapply(chunk, function(mm) list(value = df[[col_of(mm)]], warnings = res$warnings,
                                             messages = res$messages, elapsed = NA_real_))
      names(out) <- chunk
      out[["node"]] <- list(value = df$node)
      return(out)
    }
    if (length(chunk) == 1L) {
      out <- list(res); names(out) <- chunk
      return(out)
    }
    half <- ceiling(length(chunk) / 2)
    c(records(chunk[seq_len(half)]), records(chunk[-seq_len(half)]))
  }
  recs <- records(measures)
  # Keep one node record; bisection may have produced several identical ones.
  node <- recs[names(recs) == "node"]
  recs <- recs[names(recs) != "node"]
  recs <- recs[measures]
  recs[["node"]] <- if (length(node)) node[[1L]] else list(value = structure(list(error = "no measure computed", class = "golden_missing"), class = "golden_error"))
  recs
}

# Ill-posed spectral inputs. The dominant eigenvector of the adjacency (or
# of A'A for HITS) is unique only when the graph is strongly connected and
# aperiodic; igraph's ARPACK then returns a random member of the eigenspace
# or fails to converge, so the golden value is not a reference on such
# graphs. Native: components by BFS, bipartiteness by 2-colouring, primitivity
# by Wielandt's bound (A^(n^2-2n+2) > 0).
.golden_ill_posed <- function(m) {
  n <- nrow(m)
  if (n <= 1L) return(TRUE)
  b <- (m != 0) * 1
  if (max(.components_native(m)) > 1L) return(TRUE)
  directed <- !isSymmetric(unname(m))
  if (!directed) {
    colour <- rep(NA_integer_, n); colour[1L] <- 0L
    repeat {
      front <- which(!is.na(colour))
      nb <- (b[front, , drop = FALSE] > 0)
      newc <- rep(NA_integer_, n)
      for (i in seq_along(front)) newc[nb[i, ] & is.na(colour)] <- 1L - colour[front[i]]
      if (all(is.na(newc))) break
      colour[!is.na(newc)] <- newc[!is.na(newc)]
    }
    conflict <- b > 0 & outer(colour, colour, "==")
    return(!any(conflict))         # a clean 2-colouring means bipartite, which is ill-posed
  }
  # directed: primitive iff B^k has no zero for k = n^2 - 2n + 2
  k <- n * n - 2L * n + 2L
  bb <- b > 0; pw <- diag(TRUE, n)
  while (k > 0L) {
    if (k %% 2L == 1L) pw <- (pw %*% bb) > 0
    bb <- (bb %*% bb) > 0
    k <- k %/% 2L
  }
  !all(pw)
}

.golden_spectral <- c("hub", "authority", "eigenvector")
.golden_stochastic <- c("epc")
# igraph 2.3.3 computes local transitivity on a directed graph differently
# after any_multiple() has been called on the object (mutual dyads then
# count as a multi-edge in the undirected view). The old centrality() always
# called any_multiple() first, so its golden values on directed graphs with
# mutual dyads are a cache artefact, not a reference.
.golden_cache_dependent <- c("transitivity", "clusterrank")

.golden_has_mutual_dyad <- function(m) {
  b <- m != 0
  directed <- nrow(m) > 0L && !isSymmetric(unname(m))
  directed && any((b & t(b)) & !diag(TRUE, nrow(m)))
}

# HITS is well-posed only when the support graphs of A'A and AA' are each
# connected, which strong connectivity of A does not guarantee.
.golden_hits_ill_posed <- function(m) {
  b <- (m != 0) * 1
  disconnected <- function(p) { diag(p) <- 0; max(.components_native(p)) > 1L }
  disconnected(crossprod(b)) || disconnected(tcrossprod(b))
}

# igraph compares path lengths with an absolute epsilon of about 1e-10, so a
# graph whose weights all sit below ~1e-6 has most of its shortest paths
# declared tied: betweenness on such a graph is not scale-invariant in igraph
# (verified: the same graph scaled by 1e-18 changes betweenness from integers
# to fractions). Path-based golden values there are artefacts.
.golden_path_based <- c("betweenness", "closeness", "harmonic", "eccentricity", "kreach", "load",
                        "radiality", "lin", "decay", "residual_closeness", "dangalchev",
                        "generalized_closeness", "harary", "average_distance", "barycenter",
                        "wiener", "closeness_vitality", "centroid", "stress", "flow_betweenness",
                        "integration", "gilschmidt", "markov", "local_efficiency", "fragmentation",
                        "length_scaled_betweenness", "delta_betweenness", "delta_closeness",
                        "bridging", "bottleneck", "pairwisedis", "reaching_local",
                        "current_flow_betweenness", "current_flow_closeness", "information",
                        "geodesic_kpath", "improved_closeness", "ego_betweenness", "percolation",
                        "centralization_betweenness", "centralization_closeness", "edge_centrality")

.golden_sub_epsilon <- function(m) {
  v <- abs(m[m != 0])
  length(v) > 0L && max(v) < 1e-6
}

# igraph 2.3.3 alpha_centrality(weights = <vector>, loops = FALSE) simplifies
# the loops away and then fails to reassign the weight attribute ("Length of
# new attribute value must be ..."), so its golden record on a loop-carrying
# graph is an error, not a reference. With weights = NULL it computes; the
# kernel reproduces that value.
.golden_igraph_loop_bug <- c("alpha")

.golden_has_loops <- function(m) nrow(m) > 0L && any(diag(m) != 0)

# Weighted local reaching centrality averages the original weights along ONE
# shortest path per target (Mones, Vicsek & Vicsek 2012 as implemented by
# NetworkX and the old igraph-backed code). When several shortest paths tie,
# the value depends on which one the implementation picks; the kernel and
# igraph agree bit-for-bit whenever ties are absent (verified on tie-free
# random graphs), so tied graphs are not references.
.golden_tie_dependent <- c("reaching_local")

.golden_has_weight_ties <- function(m) {
  v <- m[m != 0]
  length(v) > 1L && !all(v == 1) && anyDuplicated(v) > 0L
}

# Measures whose golden value cannot serve as a reference on this network.
.golden_skip_measures <- function(m) {
  skip <- .golden_stochastic
  # the one measure family deliberately left on igraph
  if (!requireNamespace("igraph", quietly = TRUE)) skip <- c(skip, "flow_betweenness")
  if (.golden_sub_epsilon(m)) skip <- c(skip, .golden_path_based)
  if (.golden_has_loops(m)) skip <- c(skip, .golden_igraph_loop_bug)
  if (.golden_has_weight_ties(m)) skip <- c(skip, .golden_tie_dependent)
  if (nrow(m) == 0L) return(c(skip, .golden_spectral))
  if (.golden_ill_posed(m)) skip <- c(skip, .golden_spectral)
  else if (.golden_hits_ill_posed(m)) skip <- c(skip, "hub", "authority")
  if (.golden_has_mutual_dyad(m)) skip <- c(skip, .golden_cache_dependent)
  skip
}

# Recompute one golden network record with the current code.
.golden_recompute <- function(rec, m, time_limit = 30) {
  capture <- .golden_capture_fn(time_limit, keep_conditions = FALSE)
  skip <- .golden_skip_measures(m)
  cen <- lapply(rec$centrality, function(r) {
    ms <- setdiff(names(r$measures), c("node", skip))
    list(mode = r$mode, weighted = r$weighted,
         measures = .golden_measure_records(m, ms, r$mode, r$weighted, capture))
  })
  costly_names <- setdiff(names(rec$costly), skip)
  costly <- lapply(costly_names, function(cm) capture(centrality(m, measures = cm)))
  names(costly) <- costly_names
  spectral_ok <- !any(.golden_spectral %in% skip)
  adjacent <- list(
    edge_centrality = capture(edge_centrality(m)),
    centralization_degree = capture(centralization(m, measure = "degree")),
    centralization_betweenness = capture(centralization(m, measure = "betweenness")),
    centralization_closeness = capture(centralization(m, measure = "closeness")),
    centralization_eigenvector = if (spectral_ok) capture(centralization(m, measure = "eigenvector")) else NULL,
    estrada_index = capture(estrada_index(m)),
    trophic_incoherence = capture(trophic_incoherence(m)),
    basic_default = capture(if (spectral_ok) centrality(m) else centrality(m, measures = c("degree", "strength", "closeness", "betweenness", "pagerank")))
  )
  list(centrality = cen, costly = costly, adjacent = adjacent)
}

# Compare one golden record against a recomputation. Returns the diff table,
# one row per (network | design | measure) that differs.
.golden_compare_record <- function(rec, cur, tol = sqrt(.Machine$double.eps), skip = character()) {
  drop_cols <- function(cap, cols) {
    # columns carry a mode suffix for mode-aware measures (closeness_all)
    if (is.data.frame(cap$value) && length(cols)) {
      base <- sub("_(all|in|out)$", "", names(cap$value))
      cap$value <- cap$value[, !(base %in% cols), drop = FALSE]
    }
    cap
  }
  spectral_cols <- if (any(.golden_spectral %in% skip)) .golden_spectral else character()
  rec$adjacent$basic_default <- drop_cols(rec$adjacent$basic_default, spectral_cols)
  cur$adjacent$basic_default <- drop_cols(cur$adjacent$basic_default, spectral_cols)
  if (length(spectral_cols)) { rec$adjacent$centralization_eigenvector <- NULL; cur$adjacent$centralization_eigenvector <- NULL }
  for (adj in intersect(skip, c("centralization_betweenness", "centralization_closeness", "edge_centrality"))) {
    # adjacent verbs named in the skip list are dropped on both sides
    rec$adjacent[[adj]] <- NULL; cur$adjacent[[adj]] <- NULL
  }
  path_cols <- intersect(skip, c("betweenness", "closeness"))
  rec$adjacent$basic_default <- drop_cols(rec$adjacent$basic_default, path_cols)
  cur$adjacent$basic_default <- drop_cols(cur$adjacent$basic_default, path_cols)
  design_parts <- lapply(names(rec$centrality), function(nm) {
    ref_m <- rec$centrality[[nm]]$measures; cur_m <- cur$centrality[[nm]]$measures
    per <- lapply(setdiff(names(ref_m), skip), function(mm) .golden_diff_capture(ref_m[[mm]], cur_m[[mm]],
                                                                  paste(rec$name, nm, mm, sep = "|"), tol))
    per <- per[!vapply(per, is.null, logical(1))]
    if (length(per)) do.call(rbind, per) else NULL
  })
  parts <- c(
    design_parts,
    Map(function(nm) .golden_diff_capture(rec$costly[[nm]], cur$costly[[nm]],
                                          paste(rec$name, "costly", nm, sep = "|"), tol), setdiff(names(rec$costly), skip)),
    Map(function(nm) .golden_diff_capture(rec$adjacent[[nm]], cur$adjacent[[nm]],
                                          paste(rec$name, "adjacent", nm, sep = "|"), tol), names(rec$adjacent))
  )
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(NULL)
  do.call(rbind, unname(parts))
}
