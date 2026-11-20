# =============================================================================
# Numerical Equivalence: cograph::assortativity() vs igraph ground truth
#
# 100 connected graphs per test block (undirected degree, directed degree,
# nominal attribute, numeric attribute).
# Delta stats: mean, median, p95, max.
# Reports to: tmp/assortativity_equivalence_report.csv
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
  utils::write.csv(df, file.path(tempdir(), "assortativity_equivalence_report.csv"),
                   row.names = FALSE)
  cat(sprintf(
    paste0("\n=== ASSORTATIVITY EQUIVALENCE REPORT ===\n",
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
    file.path(tempdir(), "assortativity_equivalence_report.csv")
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
      ancestorTitles = list("assortativity equivalence"),
      title = sprintf("%s: n=%d d=%.2f seed=%d delta=%.2e",
                      r$function_name, r$n_nodes, r$density,
                      r$seed, r$max_abs_error),
      fullName = sprintf("assortativity equivalence > %s: n=%d seed=%d",
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
        module = "assortativity"
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
      name = "tests/testthat/test-equiv-assortativity.R",
      assertionResults = assertions
    )),
    `_cvs` = list(target = "cograph")
  )

  inbox <- file.path("..", "..", "validation", "data", "inbox")
  if (!dir.exists(inbox)) inbox <- file.path("..", "..", "..", "validation", "data", "inbox")
  if (dir.exists(inbox)) {
    fname <- sprintf("cograph-assortativity-%s.json",
                     format(Sys.time(), "%Y%m%dT%H%M%S"))
    jsonlite::write_json(result, file.path(inbox, fname),
                         auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("  CVS report written: %s\n", fname))
  }
}

# ---------------------------------------------------------------------------
# Graph generators -- connected graphs with retry
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
# Comparison helper: compute delta stats from two scalar values
# ---------------------------------------------------------------------------

.compare_scalar <- function(co_val, ref_val, func_name, cfg, tol = TOL,
                            ref_pkg = "igraph", notes = "") {
  # Handle NA/NaN consistency
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

# ---------------------------------------------------------------------------
# 100 network configurations
# ---------------------------------------------------------------------------

set.seed(2026)
N <- 100L
TOL <- 1e-8
sizes <- sample(c(8, 10, 12, 15, 20, 25, 30), N, replace = TRUE)
densities <- runif(N, 0.15, 0.4)
seeds <- sample.int(100000, N)

cat("\n")
cat("================================================================\n")
cat("  ASSORTATIVITY EQUIVALENCE REPORT\n")
cat(sprintf("  %d random connected graphs per metric\n", N))
cat(sprintf("  Sizes: %s | Densities: 0.15-0.4\n",
            paste(sort(unique(sizes)), collapse = ", ")))
cat(sprintf("  Tolerance: %.0e\n", TOL))
cat("================================================================\n\n")

# =============================================================================
# 1. Degree assortativity (undirected) -- 100 networks
# =============================================================================

test_that("assortativity undirected: matches igraph::assortativity_degree (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))

    co_val <- cograph::assortativity(mat, directed = FALSE)$coefficient
    ref_val <- igraph::assortativity_degree(g, directed = FALSE)

    .compare_scalar(co_val, ref_val, "assortativity_undirected", cfg)
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  undir_rows <- df[df$function_name == "assortativity_undirected", ]
  n_fail <- sum(undir_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Undirected assortativity: %d values failed across %d configs",
                   n_fail, nrow(undir_rows)))
})

# =============================================================================
# 2. Degree assortativity (directed) -- 100 networks
# =============================================================================

test_that("assortativity directed: matches igraph::assortativity_degree (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = TRUE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = TRUE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))

    co_val <- cograph::assortativity(mat, directed = TRUE)$coefficient
    ref_val <- igraph::assortativity_degree(g, directed = TRUE)

    .compare_scalar(co_val, ref_val, "assortativity_directed", cfg)
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  dir_rows <- df[df$function_name == "assortativity_directed", ]
  n_fail <- sum(dir_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Directed assortativity: %d values failed across %d configs",
                   n_fail, nrow(dir_rows)))
})

# =============================================================================
# 3. Nominal attribute assortativity -- 100 networks
# =============================================================================

test_that("assortativity_attribute nominal: matches igraph::assortativity_nominal (100 networks)", {
  # Use character categories so cograph dispatches to .nominal_assortativity
  # (mixing matrix formula). Integer categories would trigger the scalar path
  # since is.numeric(integer) is TRUE in R.
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))

    # Generate random 3-category assignments (character to ensure nominal path)
    set.seed(cfg$seed + 1000L)
    n_nodes <- igraph::vcount(g)
    cats_int <- sample(1:3, n_nodes, replace = TRUE)
    cats_chr <- c("A", "B", "C")[cats_int]
    names(cats_chr) <- igraph::V(g)$name

    co_val <- cograph::assortativity_attribute(mat, values = cats_chr,
                                               directed = FALSE)$coefficient
    ref_val <- igraph::assortativity_nominal(g, types = cats_int)

    .compare_scalar(co_val, ref_val, "assortativity_attribute_nominal", cfg)
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  nom_rows <- df[df$function_name == "assortativity_attribute_nominal", ]
  n_fail <- sum(nom_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Nominal attribute assortativity: %d values failed across %d configs",
                   n_fail, nrow(nom_rows)))
})

# =============================================================================
# 4. Numeric attribute assortativity -- 100 networks
# =============================================================================

test_that("assortativity_attribute numeric: matches igraph::assortativity (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))

    # Generate random numeric attribute values
    set.seed(cfg$seed + 2000L)
    n_nodes <- igraph::vcount(g)
    vals <- rnorm(n_nodes)
    names(vals) <- igraph::V(g)$name

    co_val <- cograph::assortativity_attribute(mat, values = vals,
                                               directed = FALSE)$coefficient
    ref_val <- suppressWarnings(igraph::assortativity(g, values = vals))

    .compare_scalar(co_val, ref_val, "assortativity_attribute_numeric", cfg)
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  num_rows <- df[df$function_name == "assortativity_attribute_numeric", ]
  n_fail <- sum(num_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Numeric attribute assortativity: %d values failed across %d configs",
                   n_fail, nrow(num_rows)))
})

# =============================================================================
# Print per-function summary with delta stats
# =============================================================================

test_that("assortativity equivalence: per-function delta report", {
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
                stats::quantile(sub$max_abs_error, 0.95, na.rm = TRUE)))
  })
  expect_true(TRUE)  # Report-only test
})

# =============================================================================
# Final: write reports and assert zero failures
# =============================================================================

test_that("assortativity equivalence: zero total failures + reports written", {
  report <- .write_report()
  .write_cvs_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L,
    info = sprintf("Failed %d values across %d configs",
                   sum(report$values_failed), nrow(report)))
})
