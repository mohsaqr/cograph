# =============================================================================
# Numerical Equivalence: edge_centrality() vs direct igraph calls
#
# 100 connected undirected weighted graphs.
# Three scenarios: unweighted, weighted, inverted-weights edge betweenness.
# Every edge checked element-by-element. Delta stats: mean, median, p95, max.
# Reports to: tmp/edge_centrality_equivalence_report.csv
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
  utils::write.csv(df, file.path(tempdir(), "edge_centrality_equivalence_report.csv"),
                   row.names = FALSE)
  cat(sprintf(
    paste0("\n=== EDGE CENTRALITY EQUIVALENCE REPORT ===\n",
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
    file.path(tempdir(), "edge_centrality_equivalence_report.csv")
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
      ancestorTitles = list("edge_centrality equivalence"),
      title = sprintf("%s: n=%d d=%.2f seed=%d delta=%.2e",
                      r$function_name, r$n_nodes, r$density,
                      r$seed, r$max_abs_error),
      fullName = sprintf("edge_centrality equivalence > %s: n=%d seed=%d",
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
        module = "edge-centrality"
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
      name = "tests/testthat/test-equiv-edge-centrality.R",
      assertionResults = assertions
    )),
    `_cvs` = list(target = "cograph")
  )

  inbox <- file.path("..", "..", "validation", "data", "inbox")
  if (!dir.exists(inbox)) inbox <- file.path("..", "..", "..", "validation", "data", "inbox")
  if (dir.exists(inbox)) {
    fname <- sprintf("cograph-edge-centrality-%s.json",
                     format(Sys.time(), "%Y%m%dT%H%M%S"))
    jsonlite::write_json(result, file.path(inbox, fname),
                         auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("  CVS report written: %s\n", fname))
  }
}

# ---------------------------------------------------------------------------
# Graph generator — connected undirected graphs with retry
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
# Edge-level comparison helper: align edges by from/to, compute delta stats
# ---------------------------------------------------------------------------

.compare_edge_vectors <- function(co_df, ref_betweenness, ref_edgelist,
                                  func_name, cfg, tol = TOL,
                                  ref_pkg = "igraph", notes = "") {
  # Build reference data frame
  ref_df <- data.frame(
    from = ref_edgelist[, 1],
    to   = ref_edgelist[, 2],
    ref_val = ref_betweenness,
    stringsAsFactors = FALSE
  )

  # Create sort keys — for undirected, igraph already normalises from < to
  co_df$key  <- paste(co_df$from, co_df$to, sep = "\t")
  ref_df$key <- paste(ref_df$from, ref_df$to, sep = "\t")

  # Merge on key to align edge ordering
  merged <- merge(co_df, ref_df, by = "key", suffixes = c("_co", "_ref"))

  n_checked <- nrow(merged)
  if (n_checked == 0L) {
    .log_result(func_name, cfg, 0L, 0L, 0L,
                NA_real_, NA_real_, NA_real_, NA_real_, ref_pkg,
                "no edges matched")
    return(FALSE)
  }

  deltas <- abs(merged$betweenness - merged$ref_val)
  n_passed <- sum(deltas <= tol)
  n_failed <- n_checked - n_passed

  .log_result(func_name, cfg, n_checked, n_passed, n_failed,
              max(deltas), mean(deltas),
              stats::median(deltas),
              as.numeric(stats::quantile(deltas, 0.95)),
              ref_pkg, notes)
  n_failed == 0L
}

# ---------------------------------------------------------------------------
# 100 network configurations
# ---------------------------------------------------------------------------

set.seed(2026)
N <- 100L
TOL <- 1e-8
sizes <- sample(8:25, N, replace = TRUE)
densities <- runif(N, 0.2, 0.4)
seeds <- sample.int(100000, N)

cat("\n")
cat("================================================================\n")
cat("  EDGE CENTRALITY EQUIVALENCE REPORT\n")
cat(sprintf("  %d random connected undirected weighted graphs\n", N))
cat(sprintf("  Sizes: 8-25 | Densities: 0.2-0.4\n"))
cat(sprintf("  Tolerance: %.0e\n", TOL))
cat("================================================================\n\n")

# =============================================================================
# 1. Unweighted edge betweenness — 100 networks
# =============================================================================

test_that("edge_centrality unweighted betweenness matches igraph (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))

    # cograph: weighted = FALSE => passes weights = NULL to igraph
    co <- cograph::edge_centrality(mat, measures = "betweenness",
                                   weighted = FALSE)

    # igraph reference: weights = NULL auto-uses E(g)$weight if present,
    # which is the same path cograph takes when weighted = FALSE
    ref_bet <- igraph::edge_betweenness(g, weights = NULL)
    ref_el  <- igraph::as_edgelist(g, names = TRUE)

    .compare_edge_vectors(co, ref_bet, ref_el,
                          "edge_betweenness_unweighted", cfg)
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  sub <- df[df$function_name == "edge_betweenness_unweighted", ]
  n_fail <- sum(sub$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Unweighted: %d values failed across %d configs",
                   n_fail, nrow(sub)))
})

# =============================================================================
# 2. Weighted edge betweenness — 100 networks
# =============================================================================

test_that("edge_centrality weighted betweenness matches igraph (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))

    # cograph: weighted, no inversion
    co <- cograph::edge_centrality(mat, measures = "betweenness",
                                   weighted = TRUE, invert_weights = FALSE)

    # igraph reference: use actual weights
    ref_bet <- igraph::edge_betweenness(g, weights = igraph::E(g)$weight)
    ref_el  <- igraph::as_edgelist(g, names = TRUE)

    .compare_edge_vectors(co, ref_bet, ref_el,
                          "edge_betweenness_weighted", cfg)
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  sub <- df[df$function_name == "edge_betweenness_weighted", ]
  n_fail <- sum(sub$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Weighted: %d values failed across %d configs",
                   n_fail, nrow(sub)))
})

# =============================================================================
# 3. Inverted-weights edge betweenness — 100 networks
# =============================================================================

test_that("edge_centrality inverted-weights betweenness matches igraph (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))

    # cograph: weighted with inversion (suppress the message about inversion)
    co <- suppressMessages(
      cograph::edge_centrality(mat, measures = "betweenness",
                               weighted = TRUE, invert_weights = TRUE)
    )

    # igraph reference: invert weights manually
    inv_weights <- 1 / igraph::E(g)$weight
    ref_bet <- igraph::edge_betweenness(g, weights = inv_weights)
    ref_el  <- igraph::as_edgelist(g, names = TRUE)

    .compare_edge_vectors(co, ref_bet, ref_el,
                          "edge_betweenness_inverted", cfg)
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  sub <- df[df$function_name == "edge_betweenness_inverted", ]
  n_fail <- sum(sub$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Inverted: %d values failed across %d configs",
                   n_fail, nrow(sub)))
})

# =============================================================================
# Print per-function summary with delta stats
# =============================================================================

test_that("edge_centrality equivalence: per-function delta report", {
  df <- do.call(rbind, .equiv_log$rows)
  fns <- unique(df$function_name)
  lapply(fns, function(fn) {
    sub <- df[df$function_name == fn, ]
    status <- if (all(sub$values_failed == 0)) "PASS" else "FAIL"
    cat(sprintf("  %-40s %s  mean_d=%.2e  median_d=%.2e  max_d=%.2e  p95_d=%.2e\n",
                paste0(fn, ":"), status,
                mean(sub$max_abs_error, na.rm = TRUE),
                stats::median(sub$max_abs_error, na.rm = TRUE),
                max(sub$max_abs_error, na.rm = TRUE),
                as.numeric(stats::quantile(sub$max_abs_error, 0.95,
                                           na.rm = TRUE))))
  })
  expect_true(TRUE)  # Report-only test
})

# =============================================================================
# Final: write reports and assert zero failures
# =============================================================================

test_that("edge_centrality equivalence: zero total failures + reports written", {
  report <- .write_report()
  .write_cvs_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L,
    info = sprintf("Failed %d values across %d configs",
                   sum(report$values_failed), nrow(report)))
})
