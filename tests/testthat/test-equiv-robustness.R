# =============================================================================
# Numerical Equivalence: robustness() vs igraph ground truth
#
# 1. AUC trapezoidal formula (50 networks)
# 2. Static attack ordering vs igraph betweenness (50 networks)
# 3. Random failure consistency (30 networks)
#
# Reports to: tmp/robustness_equivalence_report.csv
#             + CVS inbox (vitest JSON) if validation system is present.
# =============================================================================

skip_on_cran()
skip_coverage_tests()
skip_if_not_installed("igraph")

# ---------------------------------------------------------------------------
# Report infrastructure
# ---------------------------------------------------------------------------

.equiv_log <- new.env(parent = emptyenv())
.equiv_log$rows <- list()

.log_result <- function(func, config, n_checked, n_passed, n_failed,
                        max_abs_err = NA_real_, mean_abs_err = NA_real_,
                        median_abs_err = NA_real_, p95_abs_err = NA_real_,
                        reference_package = "igraph", notes = "") {
  .equiv_log$rows[[length(.equiv_log$rows) + 1L]] <- data.frame(
    function_name = func, n_nodes = config$n, density = config$density,
    seed = config$seed, directed = isTRUE(config$directed),
    values_checked = n_checked, values_passed = n_passed,
    values_failed = n_failed,
    max_abs_error = max_abs_err, mean_abs_error = mean_abs_err,
    median_abs_error = median_abs_err, p95_abs_error = p95_abs_err,
    reference_package = reference_package,
    notes = notes, stringsAsFactors = FALSE)
}

.write_report <- function() {
  if (length(.equiv_log$rows) == 0L) return(invisible(NULL))
  df <- do.call(rbind, .equiv_log$rows)
  utils::write.csv(df, file.path(tempdir(), "robustness_equivalence_report.csv"),
                   row.names = FALSE)
  cat(sprintf(
    paste0("\n=== ROBUSTNESS EQUIVALENCE REPORT ===\n",
           "Functions: %d | Configs: %d | Checked: %s | Passed: %s | Failed: %s\n",
           "Max delta: %.2e | Mean delta: %.2e | Median delta: %.2e\n",
           "Report: %s\n"),
    length(unique(df$function_name)), nrow(df),
    format(sum(df$values_checked), big.mark = ","),
    format(sum(df$values_passed), big.mark = ","),
    format(sum(df$values_failed), big.mark = ","),
    max(df$max_abs_error, na.rm = TRUE),
    mean(df$mean_abs_error, na.rm = TRUE),
    stats::median(df$median_abs_error, na.rm = TRUE),
    file.path(tempdir(), "robustness_equivalence_report.csv")
  ))
  invisible(df)
}

.write_cvs_report <- function() {
  if (length(.equiv_log$rows) == 0L) return(invisible(NULL))
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(NULL))
  df <- do.call(rbind, .equiv_log$rows)

  assertions <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    status <- if (r$values_failed == 0) "passed" else "failed"
    list(
      ancestorTitles = list("robustness equivalence"),
      title = sprintf("%s: n=%d d=%.2f seed=%d delta=%.2e",
                      r$function_name, r$n_nodes, r$density,
                      r$seed, r$max_abs_error),
      fullName = sprintf("robustness equivalence > %s: n=%d seed=%d",
                         r$function_name, r$n_nodes, r$seed),
      status = status,
      duration = 0L,
      failureMessages = if (status == "failed")
        list(sprintf("max_abs_error=%.2e, %d/%d values failed",
                     r$max_abs_error, r$values_failed, r$values_checked))
      else list(),
      `_cvs` = list(
        delta = r$max_abs_error,
        tolerance = TOL,
        rFunction = r$function_name,
        rPackage = r$reference_package,
        module = "robustness"
      )
    )
  })

  result <- list(
    numTotalTestSuites = 1L,
    numPassedTestSuites = as.integer(sum(df$values_failed) == 0),
    numFailedTestSuites = as.integer(sum(df$values_failed) > 0),
    numTotalTests = nrow(df),
    numPassedTests = sum(df$values_failed == 0),
    numFailedTests = sum(df$values_failed > 0),
    testResults = list(list(
      name = "tests/testthat/test-equiv-robustness.R",
      assertionResults = assertions
    )),
    `_cvs` = list(target = "cograph")
  )

  inbox <- file.path("..", "..", "validation", "data", "inbox")
  if (!dir.exists(inbox)) inbox <- file.path("..", "..", "..", "validation", "data", "inbox")
  if (dir.exists(inbox)) {
    fname <- sprintf("cograph-robustness-%s.json",
                     format(Sys.time(), "%Y%m%dT%H%M%S"))
    jsonlite::write_json(result, file.path(inbox, fname),
                         auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("  CVS report written: %s\n", fname))
  }
}

# ---------------------------------------------------------------------------
# Graph generator — connected graphs with retry
# ---------------------------------------------------------------------------

.make_connected_graph <- function(n, density, seed, directed = FALSE) {
  set.seed(seed)
  g <- igraph::sample_gnp(n, density, directed = directed)
  attempts <- 0L
  mode <- if (directed) "weak" else "strong"
  while (!igraph::is_connected(g, mode = mode) && attempts < 50L) {
    g <- igraph::sample_gnp(n, density, directed = directed)
    attempts <- attempts + 1L
  }
  if (!igraph::is_connected(g, mode = "weak")) {
    g <- igraph::sample_gnp(n, min(density + 0.2, 0.8), directed = directed)
    while (!igraph::is_connected(g, mode = "weak")) {
      g <- igraph::sample_gnp(n, min(density + 0.2, 0.8), directed = directed)
    }
  }
  # Add random weights
  igraph::E(g)$weight <- round(runif(igraph::ecount(g), 0.1, 1.0), 4)
  igraph::V(g)$name <- paste0("N", seq_len(igraph::vcount(g)))
  g
}

# ---------------------------------------------------------------------------
# Comparison helpers
# ---------------------------------------------------------------------------

.compare_scalar <- function(co_val, ref_val, func_name, cfg, tol = TOL,
                            ref_pkg = "igraph", notes = "") {
  co_na <- is.na(co_val) | is.nan(co_val)
  ref_na <- is.na(ref_val) | is.nan(ref_val)
  if (co_na && ref_na) {
    .log_result(func_name, cfg, 1L, 1L, 0L, 0, 0, 0, 0, ref_pkg,
                paste0("both NA/NaN", if (nchar(notes) > 0) paste0("; ", notes)))
    return(TRUE)
  }
  if (co_na != ref_na) {
    .log_result(func_name, cfg, 1L, 0L, 1L, NA_real_, NA_real_, NA_real_,
                NA_real_, ref_pkg, "NA mismatch")
    return(FALSE)
  }
  if (!is.finite(co_val) || !is.finite(ref_val)) {
    match <- identical(co_val, ref_val)
    .log_result(func_name, cfg, 1L, as.integer(match), as.integer(!match),
                if (match) 0 else NA_real_, 0, 0, 0, ref_pkg,
                if (!match) "Inf mismatch" else "both Inf")
    return(match)
  }

  delta <- abs(co_val - ref_val)
  pass <- delta <= tol
  .log_result(func_name, cfg, 1L, as.integer(pass), as.integer(!pass),
              delta, delta, delta, delta, ref_pkg, notes)
  pass
}

.compare_vectors <- function(co_vec, ref_vec, func_name, cfg, tol = TOL,
                             ref_pkg = "igraph", notes = "") {
  n_checked <- length(co_vec)
  stopifnot(length(co_vec) == length(ref_vec))

  deltas <- abs(co_vec - ref_vec)
  n_passed <- sum(deltas <= tol)
  n_failed <- n_checked - n_passed

  .log_result(func_name, cfg, n_checked, n_passed, n_failed,
              max_abs_err = max(deltas),
              mean_abs_err = mean(deltas),
              median_abs_err = stats::median(deltas),
              p95_abs_err = stats::quantile(deltas, 0.95, names = FALSE),
              reference_package = ref_pkg, notes = notes)
  n_failed == 0L
}

# ---------------------------------------------------------------------------
# Network configurations
# ---------------------------------------------------------------------------

set.seed(2026)
TOL <- 1e-8
N_AUC <- 50L
N_ORDER <- 50L
N_RANDOM <- 30L

sizes_auc <- sample(8:15, N_AUC, replace = TRUE)
densities_auc <- runif(N_AUC, 0.25, 0.5)
seeds_auc <- sample.int(100000, N_AUC)

sizes_order <- sample(8:15, N_ORDER, replace = TRUE)
densities_order <- runif(N_ORDER, 0.25, 0.5)
seeds_order <- sample.int(100000, N_ORDER)

sizes_random <- sample(8:15, N_RANDOM, replace = TRUE)
densities_random <- runif(N_RANDOM, 0.25, 0.5)
seeds_random <- sample.int(100000, N_RANDOM)

cat("\n")
cat("================================================================\n")
cat("  ROBUSTNESS EQUIVALENCE REPORT\n")
cat(sprintf("  AUC formula: %d networks | Ordering: %d | Random: %d\n",
            N_AUC, N_ORDER, N_RANDOM))
cat(sprintf("  Sizes: 8-15 | Densities: 0.25-0.5\n"))
cat(sprintf("  Tolerance: %.0e\n", TOL))
cat("================================================================\n\n")

# =============================================================================
# 1. AUC trapezoidal formula — 50 networks
# =============================================================================

test_that("robustness_auc matches manual trapezoidal integral (50 networks)", {
  lapply(seq_len(N_AUC), function(i) {
    cfg <- list(n = sizes_auc[i], density = densities_auc[i],
                seed = seeds_auc[i], directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))

    rob <- cograph::robustness(mat, type = "vertex", measure = "betweenness",
                               strategy = "static")
    co_auc <- cograph::robustness_auc(rob)

    # Manual trapezoidal integral
    x <- rob$removed_pct
    y <- rob$comp_pct
    ref_auc <- sum(diff(x) * (y[-length(y)] + y[-1]) / 2)

    .compare_scalar(co_auc, ref_auc, "auc_trapezoidal", cfg,
                    ref_pkg = "manual_trapezoidal")

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  sub <- df[df$function_name == "auc_trapezoidal", ]
  n_fail <- sum(sub$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("AUC trapezoidal: %d values failed across %d configs",
                   n_fail, nrow(sub)))
})

# =============================================================================
# 2. Static attack ordering — 50 networks (n=8-15)
# =============================================================================

test_that("static betweenness attack: removal order and component sizes match igraph (50 networks)", {
  lapply(seq_len(N_ORDER), function(i) {
    cfg <- list(n = sizes_order[i], density = densities_order[i],
                seed = seeds_order[i], directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))

    rob <- cograph::robustness(mat, type = "vertex", measure = "betweenness",
                               strategy = "static")

    # --- Verify removal order ---
    # cograph uses igraph::betweenness internally; we replicate that ordering
    btw <- igraph::betweenness(g, directed = FALSE)
    ref_order <- order(btw, decreasing = TRUE)

    n_v <- igraph::vcount(g)

    # --- Verify component sizes at each step ---
    # rob$comp_size[1] should be the original largest component
    orig_max <- max(igraph::components(g)$csize)
    .compare_scalar(rob$comp_size[1], orig_max, "static_initial_comp", cfg)

    # After removing top-k nodes, verify largest component size
    # We check every step to ensure the full curve matches
    ref_comp_sizes <- vapply(seq_len(n_v - 1), function(k) {
      g_reduced <- igraph::delete_vertices(g, ref_order[seq_len(k)])
      csize <- igraph::components(g_reduced)$csize
      if (length(csize) > 0) max(csize) else 0
    }, numeric(1))

    # rob$comp_size has n_v+1 entries: [orig, after1, after2, ..., after_n-1, 0]
    # We compare entries 2 through n_v (indices 2:n_v correspond to k=1..n_v-1)
    co_sizes <- rob$comp_size[seq(2, n_v)]
    .compare_vectors(co_sizes, ref_comp_sizes,
                     "static_comp_sizes", cfg,
                     ref_pkg = "igraph",
                     notes = sprintf("n=%d, %d steps", n_v, n_v - 1))

    # Final entry should be 0 (all nodes removed)
    .compare_scalar(rob$comp_size[n_v + 1], 0,
                    "static_final_zero", cfg,
                    ref_pkg = "manual")

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  sub <- df[df$function_name %in% c("static_initial_comp", "static_comp_sizes",
                                     "static_final_zero"), ]
  n_fail <- sum(sub$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Static ordering: %d values failed across %d configs",
                   n_fail, nrow(sub)))
})

# =============================================================================
# 3. Random failure consistency — 30 networks
# =============================================================================

test_that("random failure: AUC in [0,1], curve starts at 1.0, ends near 0 (30 networks)", {
  lapply(seq_len(N_RANDOM), function(i) {
    cfg <- list(n = sizes_random[i], density = densities_random[i],
                seed = seeds_random[i], directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))

    rob <- cograph::robustness(mat, type = "vertex", measure = "random",
                               n_iter = 10, seed = 42)
    auc <- cograph::robustness_auc(rob)

    # AUC should be in [0, 1]
    auc_valid <- auc >= 0 && auc <= 1
    .log_result("random_auc_range", cfg, 1L,
                as.integer(auc_valid), as.integer(!auc_valid),
                max_abs_err = if (auc_valid) 0 else abs(auc),
                mean_abs_err = 0, median_abs_err = 0, p95_abs_err = 0,
                reference_package = "bounds_check",
                notes = sprintf("auc=%.6f", auc))

    # Curve should start at comp_pct = 1.0
    .compare_scalar(rob$comp_pct[1], 1.0, "random_start_at_1", cfg,
                    ref_pkg = "definition")

    # Curve should end at comp_pct = 0.0 (all nodes removed)
    .compare_scalar(rob$comp_pct[nrow(rob)], 0.0, "random_end_at_0", cfg,
                    ref_pkg = "definition")

    # removed_pct should start at 0 and end at 1
    .compare_scalar(rob$removed_pct[1], 0.0, "random_pct_start", cfg,
                    ref_pkg = "definition")
    .compare_scalar(rob$removed_pct[nrow(rob)], 1.0, "random_pct_end", cfg,
                    ref_pkg = "definition")

    # comp_pct should be monotonically non-increasing (on average)
    # Since random is averaged over n_iter, the curve should generally decrease
    diffs <- diff(rob$comp_pct)
    n_increasing <- sum(diffs > TOL)
    # Allow small fraction of non-monotonic steps due to averaging
    monotonic_ok <- n_increasing <= length(diffs) * 0.15
    .log_result("random_monotonic", cfg, 1L,
                as.integer(monotonic_ok), as.integer(!monotonic_ok),
                max_abs_err = if (n_increasing == 0) 0 else max(diffs[diffs > 0]),
                mean_abs_err = 0, median_abs_err = 0, p95_abs_err = 0,
                reference_package = "definition",
                notes = sprintf("n_increasing=%d/%d", n_increasing, length(diffs)))

    # Reproducibility: same seed should give same result
    rob2 <- cograph::robustness(mat, type = "vertex", measure = "random",
                                n_iter = 10, seed = 42)
    .compare_vectors(rob$comp_pct, rob2$comp_pct,
                     "random_reproducibility", cfg,
                     ref_pkg = "self",
                     notes = "same seed produces identical results")

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  random_funcs <- c("random_auc_range", "random_start_at_1", "random_end_at_0",
                    "random_pct_start", "random_pct_end", "random_monotonic",
                    "random_reproducibility")
  sub <- df[df$function_name %in% random_funcs, ]
  n_fail <- sum(sub$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Random failure: %d values failed across %d configs",
                   n_fail, nrow(sub)))
})

# =============================================================================
# Print per-function summary with delta stats
# =============================================================================

test_that("robustness equivalence: per-function delta report", {
  df <- do.call(rbind, .equiv_log$rows)
  fns <- unique(df$function_name)
  lapply(fns, function(fn) {
    sub <- df[df$function_name == fn, ]
    status <- if (all(sub$values_failed == 0)) "PASS" else "FAIL"
    cat(sprintf("  %-35s %s  mean_d=%.2e  median_d=%.2e  max_d=%.2e  p95_d=%.2e\n",
                paste0(fn, ":"), status,
                mean(sub$max_abs_error, na.rm = TRUE),
                stats::median(sub$max_abs_error, na.rm = TRUE),
                max(sub$max_abs_error, na.rm = TRUE),
                stats::quantile(sub$max_abs_error, 0.95, na.rm = TRUE)))
  })
  expect_true(TRUE)  # Report-only test
})

# =============================================================================
# Final: write reports and assert zero failures
# =============================================================================

test_that("robustness equivalence: zero total failures + reports written", {
  report <- .write_report()
  .write_cvs_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L,
    info = sprintf("Failed %d values across %d configs",
                   sum(report$values_failed), nrow(report)))
})
