# =============================================================================
# Numerical Equivalence: cluster_quality() vs igraph modularity + manual formulas
#
# 100 connected undirected graphs x 2 community detection methods = 200 configs.
# Every metric checked element-by-element. Delta stats: mean, median, p95, max.
# Reports to: tmp/cluster_quality_equivalence_report.csv
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
    seed = config$seed, method = config$method,
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
  utils::write.csv(df, file.path(tempdir(), "cluster_quality_equivalence_report.csv"),
                   row.names = FALSE)
  cat(sprintf(
    paste0("\n=== CLUSTER QUALITY EQUIVALENCE REPORT ===\n",
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
    file.path(tempdir(), "cluster_quality_equivalence_report.csv")
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
      ancestorTitles = list("cluster_quality equivalence"),
      title = sprintf("%s: n=%d d=%.2f seed=%d method=%s delta=%.2e",
                      r$function_name, r$n_nodes, r$density,
                      r$seed, r$method, r$max_abs_error),
      fullName = sprintf("cluster_quality equivalence > %s: n=%d seed=%d method=%s",
                         r$function_name, r$n_nodes, r$seed, r$method),
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
        module = "cluster-quality"
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
      name = "tests/testthat/test-equiv-cluster-quality.R",
      assertionResults = assertions
    )),
    `_cvs` = list(target = "cograph")
  )

  inbox <- file.path("..", "..", "validation", "data", "inbox")
  if (!dir.exists(inbox)) inbox <- file.path("..", "..", "..", "validation", "data", "inbox")
  if (dir.exists(inbox)) {
    fname <- sprintf("cograph-cluster-quality-%s.json",
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
# Comparison helper: compare a scalar value and log the result
# ---------------------------------------------------------------------------

.compare_scalar <- function(co_val, ref_val, func_name, cfg, tol = TOL,
                            ref_pkg = "igraph", notes = "") {
  # Strip names for clean comparison
  co_val <- as.numeric(co_val)
  ref_val <- as.numeric(ref_val)

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
# Compare a vector of per-cluster values and log each
# ---------------------------------------------------------------------------

.compare_vector <- function(co_vec, ref_vec, func_name, cfg, tol = TOL,
                            ref_pkg = "manual", notes = "") {
  stopifnot(length(co_vec) == length(ref_vec))
  vapply(seq_along(co_vec), function(k) {
    .compare_scalar(co_vec[k], ref_vec[k],
                    paste0(func_name, "_c", k), cfg, tol, ref_pkg,
                    paste0("cluster ", k, if (nchar(notes) > 0) paste0("; ", notes)))
  }, logical(1))
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
methods <- c("walktrap", "louvain")

cat("\n")
cat("================================================================\n")
cat("  CLUSTER QUALITY EQUIVALENCE REPORT\n")
cat(sprintf("  %d random connected undirected graphs x %d methods = %d configs\n",
            N, length(methods), N * length(methods)))
cat(sprintf("  Sizes: %s | Densities: 0.15-0.4\n",
            paste(sort(unique(sizes)), collapse = ", ")))
cat(sprintf("  Tolerance: %.0e\n", TOL))
cat("================================================================\n\n")

# =============================================================================
# 1. Modularity vs igraph::modularity — 100 graphs x 2 methods
# =============================================================================

test_that("cluster_quality modularity matches igraph::modularity (200 configs)", {
  lapply(seq_len(N), function(i) {
    g <- .make_connected_graph(sizes[i], densities[i], seeds[i], directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))

    lapply(methods, function(meth) {
      cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                  method = meth)

      comm <- cograph::detect_communities(mat, method = meth)
      membership_vec <- setNames(comm$community, comm$node)

      cq <- cograph::cluster_quality(mat, membership_vec, directed = FALSE)

      # igraph reference: modularity with weights
      mem_int <- as.integer(membership_vec[igraph::V(g)$name])
      ref_mod <- igraph::modularity(g, mem_int, weights = igraph::E(g)$weight)

      .compare_scalar(cq$global$modularity, ref_mod,
                      "modularity", cfg, ref_pkg = "igraph")
      invisible(NULL)
    })
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  mod_rows <- df[grepl("^modularity$", df$function_name), ]
  n_fail <- sum(mod_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Modularity: %d values failed across %d configs",
                   n_fail, nrow(mod_rows)))
})

# =============================================================================
# 2. Coverage vs manual formula — 100 graphs x 2 methods
# =============================================================================

test_that("cluster_quality coverage matches manual formula (200 configs)", {
  lapply(seq_len(N), function(i) {
    g <- .make_connected_graph(sizes[i], densities[i], seeds[i], directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))
    n_v <- nrow(mat)

    lapply(methods, function(meth) {
      cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                  method = meth)

      comm <- cograph::detect_communities(mat, method = meth)
      membership_vec <- setNames(comm$community, comm$node)

      cq <- cograph::cluster_quality(mat, membership_vec, directed = FALSE)

      # Manual coverage: sum of internal edge weights / total edge weight
      # For undirected: internal = sum(A[S,S])/2, total = sum(A)/2
      m_total <- sum(mat) / 2
      unique_comms <- sort(unique(membership_vec))
      total_internal <- sum(vapply(unique_comms, function(k) {
        S <- which(membership_vec == k)
        sum(mat[S, S]) / 2
      }, numeric(1)))
      ref_coverage <- if (m_total > 0) total_internal / m_total else NA_real_

      .compare_scalar(cq$global$coverage, ref_coverage,
                      "coverage", cfg, ref_pkg = "manual")
      invisible(NULL)
    })
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  cov_rows <- df[grepl("^coverage$", df$function_name), ]
  n_fail <- sum(cov_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Coverage: %d values failed across %d configs",
                   n_fail, nrow(cov_rows)))
})

# =============================================================================
# 3. Per-cluster conductance vs manual formula — 100 graphs x 2 methods
# =============================================================================

test_that("cluster_quality conductance matches manual formula (200 configs)", {
  lapply(seq_len(N), function(i) {
    g <- .make_connected_graph(sizes[i], densities[i], seeds[i], directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))
    n_v <- nrow(mat)
    node_names <- rownames(mat)
    if (is.null(node_names)) node_names <- paste0("N", seq_len(n_v))

    lapply(methods, function(meth) {
      cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                  method = meth)

      comm <- cograph::detect_communities(mat, method = meth)
      membership_vec <- setNames(comm$community, comm$node)

      cq <- cograph::cluster_quality(mat, membership_vec, directed = FALSE)

      # Manual conductance for each cluster
      # conductance(S) = c_S / (2*m_S + c_S)
      # where m_S = internal edge weight (sum(A[S,S])/2 for undirected)
      #       c_S = boundary edge weight (sum(A[S, not_S]) for undirected)
      unique_comms <- sort(unique(membership_vec))
      ref_conductance <- vapply(unique_comms, function(k) {
        S <- which(membership_vec == k)
        not_S <- setdiff(seq_len(n_v), S)
        m_S <- sum(mat[S, S]) / 2
        c_S <- sum(mat[S, not_S])
        vol_S <- 2 * m_S + c_S
        if (vol_S > 0) c_S / vol_S else NA_real_
      }, numeric(1))

      # cograph per_cluster is ordered by cluster id
      co_conductance <- cq$per_cluster$conductance[order(cq$per_cluster$cluster)]

      .compare_vector(co_conductance, ref_conductance,
                      "conductance", cfg, ref_pkg = "manual")
      invisible(NULL)
    })
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  cond_rows <- df[grepl("^conductance_c", df$function_name), ]
  n_fail <- sum(cond_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Conductance: %d values failed across %d configs",
                   n_fail, nrow(cond_rows)))
})

# =============================================================================
# 4. Per-cluster internal_density vs manual formula — 100 graphs x 2 methods
# =============================================================================

test_that("cluster_quality internal_density matches manual formula (200 configs)", {
  lapply(seq_len(N), function(i) {
    g <- .make_connected_graph(sizes[i], densities[i], seeds[i], directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))
    n_v <- nrow(mat)

    lapply(methods, function(meth) {
      cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                  method = meth)

      comm <- cograph::detect_communities(mat, method = meth)
      membership_vec <- setNames(comm$community, comm$node)

      cq <- cograph::cluster_quality(mat, membership_vec, directed = FALSE)

      # Manual internal_density for each cluster
      # internal_density(S) = m_S / (n_S * (n_S - 1) / 2)
      unique_comms <- sort(unique(membership_vec))
      ref_int_density <- vapply(unique_comms, function(k) {
        S <- which(membership_vec == k)
        n_S <- length(S)
        m_S <- sum(mat[S, S]) / 2
        max_internal <- n_S * (n_S - 1) / 2
        if (max_internal > 0) m_S / max_internal else NA_real_
      }, numeric(1))

      co_int_density <- cq$per_cluster$internal_density[order(cq$per_cluster$cluster)]

      .compare_vector(co_int_density, ref_int_density,
                      "internal_density", cfg, ref_pkg = "manual")
      invisible(NULL)
    })
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  id_rows <- df[grepl("^internal_density_c", df$function_name), ]
  n_fail <- sum(id_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Internal density: %d values failed across %d configs",
                   n_fail, nrow(id_rows)))
})

# =============================================================================
# 5. Per-cluster cut_ratio vs manual formula — 100 graphs x 2 methods
# =============================================================================

test_that("cluster_quality cut_ratio matches manual formula (200 configs)", {
  lapply(seq_len(N), function(i) {
    g <- .make_connected_graph(sizes[i], densities[i], seeds[i], directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))
    n_v <- nrow(mat)

    lapply(methods, function(meth) {
      cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                  method = meth)

      comm <- cograph::detect_communities(mat, method = meth)
      membership_vec <- setNames(comm$community, comm$node)

      cq <- cograph::cluster_quality(mat, membership_vec, directed = FALSE)

      # Manual cut_ratio for each cluster
      # cut_ratio(S) = c_S / (n_S * (n - n_S))
      # where c_S = boundary edge weight, n = total nodes, n_S = cluster size
      unique_comms <- sort(unique(membership_vec))
      ref_cut_ratio <- vapply(unique_comms, function(k) {
        S <- which(membership_vec == k)
        not_S <- setdiff(seq_len(n_v), S)
        n_S <- length(S)
        c_S <- sum(mat[S, not_S])
        max_cut <- n_S * (n_v - n_S)
        if (max_cut > 0) c_S / max_cut else NA_real_
      }, numeric(1))

      co_cut_ratio <- cq$per_cluster$cut_ratio[order(cq$per_cluster$cluster)]

      .compare_vector(co_cut_ratio, ref_cut_ratio,
                      "cut_ratio", cfg, ref_pkg = "manual")
      invisible(NULL)
    })
    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  cr_rows <- df[grepl("^cut_ratio_c", df$function_name), ]
  n_fail <- sum(cr_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("Cut ratio: %d values failed across %d configs",
                   n_fail, nrow(cr_rows)))
})

# =============================================================================
# Print per-function summary with delta stats
# =============================================================================

test_that("cluster_quality equivalence: per-function delta report", {
  df <- do.call(rbind, .equiv_log$rows)
  # Aggregate per base metric (strip _c1, _c2 suffixes for grouping)
  df$base_metric <- sub("_c[0-9]+$", "", df$function_name)
  base_metrics <- unique(df$base_metric)
  lapply(base_metrics, function(fn) {
    sub <- df[df$base_metric == fn, ]
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

test_that("cluster_quality equivalence: zero total failures + reports written", {
  report <- .write_report()
  .write_cvs_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L,
    info = sprintf("Failed %d values across %d configs",
                   sum(report$values_failed), nrow(report)))
})
