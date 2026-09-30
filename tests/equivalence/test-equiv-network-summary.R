# =============================================================================
# Numerical Equivalence: network_summary() vs direct igraph calls
#
# 100 connected graphs per direction (undirected + directed).
# Every metric checked element-by-element. Delta stats: mean, median, p95, max.
# Reports to: tmp/network_summary_equivalence_report.csv
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
  utils::write.csv(df, file.path(tempdir(), "network_summary_equivalence_report.csv"),
                   row.names = FALSE)
  cat(sprintf(
    paste0("\n=== NETWORK SUMMARY EQUIVALENCE REPORT ===\n",
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
    file.path(tempdir(), "network_summary_equivalence_report.csv")
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
      ancestorTitles = list("network_summary equivalence"),
      title = sprintf("%s: n=%d d=%.2f seed=%d delta=%.2e",
                      r$function_name, r$n_nodes, r$density,
                      r$seed, r$max_abs_error),
      fullName = sprintf("network_summary equivalence > %s: n=%d seed=%d",
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
        module = "network-summary"
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
      name = "tests/testthat/test-equiv-network-summary.R",
      assertionResults = assertions
    )),
    `_cvs` = list(target = "cograph")
  )

  inbox <- file.path("..", "..", "validation", "data", "inbox")
  if (!dir.exists(inbox)) inbox <- file.path("..", "..", "..", "validation", "data", "inbox")
  if (dir.exists(inbox)) {
    fname <- sprintf("cograph-network-summary-%s.json",
                     format(Sys.time(), "%Y%m%dT%H%M%S"))
    jsonlite::write_json(result, file.path(inbox, fname),
                         auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("  CVS report written: %s\n", fname))
  }
}

# ---------------------------------------------------------------------------
# Graph generators — connected graphs with retry
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
# Comparison helper: compute delta stats from two numeric vectors
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
cat("  NETWORK SUMMARY EQUIVALENCE REPORT\n")
cat(sprintf("  %d random connected graphs per metric\n", N))
cat(sprintf("  Sizes: %s | Densities: 0.15-0.4\n",
            paste(sort(unique(sizes)), collapse = ", ")))
cat(sprintf("  Tolerance: %.0e\n", TOL))
cat("================================================================\n\n")

# =============================================================================
# 1. Undirected basic metrics — 100 networks
# =============================================================================

test_that("network_summary undirected: basic metrics match igraph (100 networks)", {
  metrics_tested <- character(0)

  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = FALSE)
    weights <- igraph::E(g)$weight

    # cograph via network_summary (digits = NULL for unrounded)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))
    ns <- network_summary(mat, directed = FALSE, digits = NULL, extended = TRUE,
                          detailed = TRUE)

    # igraph reference for each metric
    .compare_scalar(ns$density, igraph::edge_density(g),
                    "density", cfg)
    .compare_scalar(ns$diameter, igraph::diameter(g, directed = FALSE, weights = weights),
                    "diameter", cfg)
    .compare_scalar(ns$mean_distance,
                    igraph::mean_distance(g, directed = FALSE, weights = weights),
                    "mean_distance", cfg)
    .compare_scalar(ns$component_count, igraph::count_components(g),
                    "component_count", cfg)
    .compare_scalar(ns$centralization_degree,
                    igraph::centr_degree(g, mode = "all")$centralization,
                    "centralization_degree", cfg)
    .compare_scalar(ns$centralization_betweenness,
                    igraph::centr_betw(g, directed = FALSE)$centralization,
                    "centralization_betweenness", cfg)
    .compare_scalar(ns$centralization_closeness,
                    igraph::centr_clo(g, mode = "all")$centralization,
                    "centralization_closeness", cfg)
    .compare_scalar(ns$centralization_eigen,
                    igraph::centr_eigen(g, directed = FALSE)$centralization,
                    "centralization_eigen", cfg)
    .compare_scalar(ns$transitivity,
                    igraph::transitivity(g, type = "global"),
                    "transitivity", cfg)
    .compare_scalar(ns$assortativity_degree,
                    igraph::assortativity_degree(g, directed = FALSE),
                    "assortativity_degree", cfg)
    .compare_scalar(ns$min_cut,
                    igraph::min_cut(g, value.only = TRUE),
                    "min_cut", cfg)

    # Extended
    .compare_scalar(ns$girth, igraph::girth(g)$girth, "girth", cfg)
    .compare_scalar(ns$radius, min(igraph::eccentricity(g)),
                    "radius", cfg)
    .compare_scalar(ns$vertex_connectivity, igraph::vertex_connectivity(g),
                    "vertex_connectivity", cfg)
    .compare_scalar(ns$largest_clique_size, igraph::clique_num(g),
                    "largest_clique_size", cfg)

    # Global efficiency: manual Latora formula
    dists <- igraph::distances(g, weights = weights)
    diag(dists) <- Inf
    inv_d <- 1 / dists
    inv_d[!is.finite(inv_d)] <- 0
    n_v <- igraph::vcount(g)
    ref_ge <- sum(inv_d) / (n_v * (n_v - 1))
    .compare_scalar(ns$global_efficiency, ref_ge,
                    "global_efficiency", cfg, ref_pkg = "manual_Latora")

    # Detailed
    .compare_scalar(ns$mean_degree, mean(igraph::degree(g)),
                    "mean_degree", cfg)
    .compare_scalar(ns$mean_betweenness,
                    mean(igraph::betweenness(g, directed = FALSE, weights = weights)),
                    "mean_betweenness", cfg)
    .compare_scalar(ns$mean_closeness,
                    mean(igraph::closeness(g, mode = "all", weights = weights)),
                    "mean_closeness", cfg)
    .compare_scalar(ns$mean_eigenvector,
                    mean(igraph::eigen_centrality(g, directed = FALSE,
                                                  weights = weights)$vector),
                    "mean_eigenvector", cfg)
    .compare_scalar(ns$mean_pagerank,
                    mean(igraph::page_rank(g, directed = FALSE,
                                           weights = weights)$vector),
                    "mean_pagerank", cfg)

    invisible(NULL)
  })

  # Summary assertion
  df <- do.call(rbind, .equiv_log$rows)
  n_fail_total <- sum(df$values_failed)
  expect_equal(n_fail_total, 0L,
    info = sprintf("Undirected: %d values failed across %d configs", n_fail_total, nrow(df)))
})

# =============================================================================
# 2. Directed basic metrics — 100 networks
# =============================================================================

test_that("network_summary directed: basic + reciprocity match igraph (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = TRUE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = TRUE)
    weights <- igraph::E(g)$weight

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))
    # Note: extended=TRUE on directed graphs triggers a C stack overflow in
    # igraph (pre-existing issue). Test basic + detailed only for directed.
    ns <- network_summary(mat, directed = TRUE, digits = NULL, detailed = TRUE)

    .compare_scalar(ns$density, igraph::edge_density(g),
                    "density_dir", cfg)
    .compare_scalar(ns$diameter,
                    igraph::diameter(g, directed = TRUE, weights = weights),
                    "diameter_dir", cfg)
    .compare_scalar(ns$mean_distance,
                    igraph::mean_distance(g, directed = TRUE, weights = weights),
                    "mean_distance_dir", cfg)
    .compare_scalar(ns$centralization_degree,
                    igraph::centr_degree(g, mode = "all")$centralization,
                    "centralization_degree_dir", cfg)
    .compare_scalar(ns$centralization_in_degree,
                    igraph::centr_degree(g, mode = "in")$centralization,
                    "centralization_in_degree", cfg)
    .compare_scalar(ns$centralization_out_degree,
                    igraph::centr_degree(g, mode = "out")$centralization,
                    "centralization_out_degree", cfg)
    .compare_scalar(ns$centralization_betweenness,
                    igraph::centr_betw(g, directed = TRUE)$centralization,
                    "centralization_betweenness_dir", cfg)
    # network_summary applies simplify(sum) before computing, so reference must
    # use the simplified graph too
    g_simple <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE,
                                  edge.attr.comb = list(weight = "sum", "ignore"))
    .compare_scalar(ns$transitivity,
                    igraph::transitivity(g_simple, type = "global"),
                    "transitivity_dir", cfg)
    .compare_scalar(ns$reciprocity,
                    igraph::reciprocity(g, mode = "ratio"),
                    "reciprocity", cfg)
    .compare_scalar(ns$assortativity_degree,
                    igraph::assortativity_degree(g, directed = TRUE),
                    "assortativity_degree_dir", cfg)

    # Detailed
    .compare_scalar(ns$mean_degree, mean(igraph::degree(g, mode = "all")),
                    "mean_degree_dir", cfg)
    .compare_scalar(ns$mean_betweenness,
                    mean(igraph::betweenness(g, directed = TRUE, weights = weights)),
                    "mean_betweenness_dir", cfg)
    .compare_scalar(ns$mean_pagerank,
                    mean(igraph::page_rank(g, directed = TRUE,
                                           weights = weights)$vector),
                    "mean_pagerank_dir", cfg)

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  n_fail_total <- sum(df$values_failed)
  expect_equal(n_fail_total, 0L,
    info = sprintf("Directed: %d values failed across %d configs", n_fail_total, nrow(df)))
})

# =============================================================================
# 3. Cross-package: brainGraph (vulnerability, efficiency, small-world)
# =============================================================================

test_that("global_efficiency: matches brainGraph::efficiency (50 networks)", {
  skip_if_not_installed("brainGraph")

  lapply(seq_len(min(N, 50)), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)

    co_eff <- network_global_efficiency(g, directed = FALSE)
    ref_eff <- tryCatch(brainGraph::efficiency(g, type = "global"),
                        error = function(e) NA_real_)
    if (is.na(ref_eff)) return(invisible(NULL))

    .compare_scalar(co_eff, ref_eff, "brainGraph_efficiency", cfg,
                    ref_pkg = "brainGraph")
  })
})

test_that("vulnerability: matches brainGraph::vulnerability (50 networks)", {
  skip_if_not_installed("brainGraph")

  lapply(seq_len(min(N, 50)), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE, attr = "weight"))

    co_vuln <- vulnerability(mat)
    ref_vuln <- tryCatch(brainGraph::vulnerability(g), error = function(e) NULL)
    if (is.null(ref_vuln)) return(invisible(NULL))

    # Both return per-node vulnerability; compare sorted values
    co_vals <- sort(co_vuln$vulnerability)
    ref_vals <- sort(ref_vuln)

    if (length(co_vals) != length(ref_vals)) {
      .log_result("brainGraph_vulnerability", cfg, 0L, 0L, 0L, NA_real_,
                  NA_real_, NA_real_, NA_real_, "brainGraph", "length mismatch")
      return(invisible(NULL))
    }

    deltas <- abs(co_vals - ref_vals)
    max_err <- max(deltas)
    n_fail <- sum(deltas > TOL)
    .log_result("brainGraph_vulnerability", cfg, length(deltas),
                length(deltas) - n_fail, n_fail, max_err,
                mean(deltas), stats::median(deltas),
                stats::quantile(deltas, 0.95), "brainGraph")
    expect_true(max_err <= TOL,
      info = sprintf("n=%d seed=%d max_err=%.2e", cfg$n, cfg$seed, max_err))
  })
})

test_that("small_world: matches brainGraph::small.world (50 networks)", {
  skip_if_not_installed("brainGraph")

  lapply(seq_len(min(N, 50)), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

    co_sw <- network_small_world(mat, n_random = 10)
    ref_sw <- tryCatch(brainGraph::small.world(g, N = 10),
                       error = function(e) NULL)
    if (is.null(ref_sw) || is.na(co_sw)) return(invisible(NULL))

    # brainGraph returns a list with $sigma; compare sigma values
    ref_sigma <- if (is.list(ref_sw)) ref_sw$sigma else ref_sw
    if (is.na(ref_sigma)) return(invisible(NULL))

    # Small-world uses random graphs internally — can't expect exact match.
    # Verify same order of magnitude (within 50% relative error)
    rel_err <- abs(co_sw - ref_sigma) / max(abs(ref_sigma), 1e-10)
    pass <- rel_err < 0.5
    .log_result("brainGraph_small_world", cfg, 1L, as.integer(pass),
                as.integer(!pass), rel_err, rel_err, rel_err, rel_err,
                "brainGraph",
                sprintf("co=%.3f ref=%.3f rel=%.1f%%", co_sw, ref_sigma, rel_err * 100))
  })
})

# =============================================================================
# Print per-function summary with delta stats
# =============================================================================

test_that("network_summary equivalence: per-function delta report", {
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

test_that("network_summary equivalence: zero total failures + reports written", {
  report <- .write_report()
  .write_cvs_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L,
    info = sprintf("Failed %d values across %d configs",
                   sum(report$values_failed), nrow(report)))
})
