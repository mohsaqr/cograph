# =============================================================================
# Numerical Equivalence: detect_communities() vs direct igraph cluster_* calls
#
# 100 undirected connected graphs with random weights.
# Deterministic algorithms (walktrap, fast_greedy): exact membership + modularity.
# Stochastic algorithms (louvain, leiden, label_prop, infomap): self-consistency.
# Reports to: tmp/communities_equivalence_report.csv
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
    seed = config$seed, directed = FALSE,
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
  utils::write.csv(df, file.path(tempdir(), "communities_equivalence_report.csv"),
                   row.names = FALSE)
  cat(sprintf(
    paste0("\n=== COMMUNITY DETECTION EQUIVALENCE REPORT ===\n",
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
    file.path(tempdir(), "communities_equivalence_report.csv")
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
      ancestorTitles = list("detect_communities equivalence"),
      title = sprintf("%s: n=%d d=%.2f seed=%d delta=%.2e",
                      r$function_name, r$n_nodes, r$density,
                      r$seed, r$max_abs_error),
      fullName = sprintf("detect_communities equivalence > %s: n=%d seed=%d",
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
        module = "communities"
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
      name = "tests/testthat/test-equiv-communities.R",
      assertionResults = assertions
    )),
    `_cvs` = list(target = "cograph")
  )

  inbox <- file.path("..", "..", "validation", "data", "inbox")
  if (!dir.exists(inbox)) inbox <- file.path("..", "..", "..", "validation", "data", "inbox")
  if (dir.exists(inbox)) {
    fname <- sprintf("cograph-communities-%s.json",
                     format(Sys.time(), "%Y%m%dT%H%M%S"))
    jsonlite::write_json(result, file.path(inbox, fname),
                         auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("  CVS report written: %s\n", fname))
  }
}

# ---------------------------------------------------------------------------
# Graph generator -- connected undirected graphs with retry
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

.compare_membership <- function(co_comm, ref_membership, func_name, cfg,
                                ref_pkg = "igraph", notes = "") {
  # co_comm: cograph_communities data.frame (node, community)
  # ref_membership: named integer vector from igraph::membership()
  co_mem <- stats::setNames(co_comm$community, co_comm$node)
  ref_mem <- ref_membership[names(co_mem)]

  n <- length(co_mem)
  n_match <- sum(co_mem == ref_mem)
  n_fail <- n - n_match

  .log_result(func_name, cfg, n, n_match, n_fail,
              max_abs_err = as.numeric(n_fail > 0),
              mean_abs_err = 0, median_abs_err = 0, p95_abs_err = 0,
              ref_pkg, notes)
  n_fail == 0L
}

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
    match_ok <- identical(co_val, ref_val)
    .log_result(func_name, cfg, 1L, as.integer(match_ok), as.integer(!match_ok),
                if (match_ok) 0 else NA_real_, 0, 0, 0, ref_pkg,
                if (!match_ok) "Inf mismatch" else "both Inf")
    return(match_ok)
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
cat("  COMMUNITY DETECTION EQUIVALENCE REPORT\n")
cat(sprintf("  %d random connected undirected graphs\n", N))
cat(sprintf("  Sizes: %s | Densities: 0.15-0.4\n",
            paste(sort(unique(sizes)), collapse = ", ")))
cat(sprintf("  Tolerance (modularity): %.0e | Membership: exact\n", TOL))
cat("================================================================\n\n")

# =============================================================================
# 1. Deterministic: walktrap -- exact membership + modularity
# =============================================================================

test_that("walktrap: membership + modularity match igraph (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i])
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    weights <- igraph::E(g)$weight

    # cograph
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))
    co_comm <- cograph::detect_communities(mat, method = "walktrap")

    # igraph reference
    ref <- igraph::cluster_walktrap(g, weights = weights)
    ref_mem <- igraph::membership(ref)

    # Membership: exact match
    .compare_membership(co_comm, ref_mem, "walktrap_membership", cfg)

    # Number of communities
    co_k <- length(unique(co_comm$community))
    ref_k <- length(unique(ref_mem))
    .compare_scalar(co_k, ref_k, "walktrap_n_communities", cfg, tol = 0)

    # Modularity
    co_mod <- attr(co_comm, "modularity")
    ref_mod <- igraph::modularity(ref)
    .compare_scalar(co_mod, ref_mod, "walktrap_modularity", cfg)

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  wt_rows <- df[grepl("^walktrap_", df$function_name), ]
  n_fail <- sum(wt_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("walktrap: %d values failed across %d configs",
                   n_fail, nrow(wt_rows)))
})

# =============================================================================
# 2. Deterministic: fast_greedy -- exact membership + modularity
#    Note: detect_communities undirects the graph for fast_greedy, so
#    the reference must also use an undirected graph. Our test graphs are
#    already undirected, so igraph::as_undirected(g, mode = "collapse",
#    edge.attr.comb = "mean") is a no-op, but we apply it for parity.
# =============================================================================

test_that("fast_greedy: membership + modularity match igraph (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i])
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    weights <- igraph::E(g)$weight

    # cograph
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))
    co_comm <- cograph::detect_communities(mat, method = "fast_greedy")

    # igraph reference: undirect first (mirrors detect_communities behavior)
    g_undirected <- igraph::as_undirected(g, mode = "collapse",
                                          edge.attr.comb = "mean")
    ref <- igraph::cluster_fast_greedy(g_undirected, weights = weights)
    ref_mem <- igraph::membership(ref)

    # Membership: exact match
    .compare_membership(co_comm, ref_mem, "fast_greedy_membership", cfg)

    # Number of communities
    co_k <- length(unique(co_comm$community))
    ref_k <- length(unique(ref_mem))
    .compare_scalar(co_k, ref_k, "fast_greedy_n_communities", cfg, tol = 0)

    # Modularity
    co_mod <- attr(co_comm, "modularity")
    ref_mod <- igraph::modularity(ref)
    .compare_scalar(co_mod, ref_mod, "fast_greedy_modularity", cfg)

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  fg_rows <- df[grepl("^fast_greedy_", df$function_name), ]
  n_fail <- sum(fg_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("fast_greedy: %d values failed across %d configs",
                   n_fail, nrow(fg_rows)))
})

# =============================================================================
# 3. Stochastic: louvain -- self-consistency checks
# =============================================================================

test_that("louvain: self-consistency checks (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i])
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))
    co_comm <- cograph::detect_communities(mat, method = "louvain")
    co_mod <- attr(co_comm, "modularity")

    # Reconstruct the same igraph that detect_communities builds internally
    g_internal <- cograph::to_igraph(mat)
    w_internal <- igraph::E(g_internal)$weight

    # All nodes assigned
    n_nodes <- igraph::vcount(g_internal)
    n_assigned <- nrow(co_comm)
    .compare_scalar(n_assigned, n_nodes, "louvain_all_assigned", cfg, tol = 0)

    # At least 1 community
    co_k <- length(unique(co_comm$community))
    pass_k <- co_k >= 1L
    .log_result("louvain_n_communities", cfg, 1L, as.integer(pass_k),
                as.integer(!pass_k), 0, 0, 0, 0, "igraph",
                sprintf("k=%d", co_k))

    # Modularity > 0 for nontrivial partition
    pass_mod <- !is.na(co_mod) && is.finite(co_mod) && co_mod > 0
    .log_result("louvain_modularity_positive", cfg, 1L,
                as.integer(pass_mod), as.integer(!pass_mod),
                0, 0, 0, 0, "igraph", sprintf("mod=%.6f", co_mod))

    # Self-consistency: modularity from attr must match igraph::modularity()
    # recomputed with the same weights used during clustering
    recomputed_mod <- igraph::modularity(g_internal, co_comm$community,
                                         weights = w_internal)
    .compare_scalar(co_mod, recomputed_mod,
                    "louvain_modularity_selfcheck", cfg)

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  lv_rows <- df[grepl("^louvain_", df$function_name), ]
  n_fail <- sum(lv_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("louvain: %d values failed across %d configs",
                   n_fail, nrow(lv_rows)))
})

# =============================================================================
# 4. Stochastic: leiden -- self-consistency checks
# =============================================================================

test_that("leiden: self-consistency checks (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i])
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))
    co_comm <- cograph::detect_communities(mat, method = "leiden")
    co_mod <- attr(co_comm, "modularity")

    # Reconstruct the same igraph that detect_communities builds internally
    g_internal <- cograph::to_igraph(mat)
    w_internal <- igraph::E(g_internal)$weight

    # All nodes assigned
    n_nodes <- igraph::vcount(g_internal)
    n_assigned <- nrow(co_comm)
    .compare_scalar(n_assigned, n_nodes, "leiden_all_assigned", cfg, tol = 0)

    # At least 1 community
    co_k <- length(unique(co_comm$community))
    pass_k <- co_k >= 1L
    .log_result("leiden_n_communities", cfg, 1L, as.integer(pass_k),
                as.integer(!pass_k), 0, 0, 0, 0, "igraph",
                sprintf("k=%d", co_k))

    # Modularity: may be NA for Leiden CPM objective; only check if finite
    if (!is.na(co_mod) && is.finite(co_mod)) {
      # Self-consistency: recompute with same weights used during clustering
      recomputed_mod <- igraph::modularity(g_internal, co_comm$community,
                                           weights = w_internal)
      .compare_scalar(co_mod, recomputed_mod,
                      "leiden_modularity_selfcheck", cfg)
    } else {
      .log_result("leiden_modularity_selfcheck", cfg, 1L, 1L, 0L,
                  0, 0, 0, 0, "igraph", "modularity NA (CPM objective); skipped")
    }

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  ld_rows <- df[grepl("^leiden_", df$function_name), ]
  n_fail <- sum(ld_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("leiden: %d values failed across %d configs",
                   n_fail, nrow(ld_rows)))
})

# =============================================================================
# 5. Stochastic: label_prop -- self-consistency checks
# =============================================================================

test_that("label_prop: self-consistency checks (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i])
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))
    co_comm <- cograph::detect_communities(mat, method = "label_prop")
    co_mod <- attr(co_comm, "modularity")

    # Reconstruct the same igraph that detect_communities builds internally
    g_internal <- cograph::to_igraph(mat)
    w_internal <- igraph::E(g_internal)$weight

    # All nodes assigned
    n_nodes <- igraph::vcount(g_internal)
    n_assigned <- nrow(co_comm)
    .compare_scalar(n_assigned, n_nodes, "label_prop_all_assigned", cfg, tol = 0)

    # At least 1 community
    co_k <- length(unique(co_comm$community))
    pass_k <- co_k >= 1L
    .log_result("label_prop_n_communities", cfg, 1L, as.integer(pass_k),
                as.integer(!pass_k), 0, 0, 0, 0, "igraph",
                sprintf("k=%d", co_k))

    # Modularity >= 0 (label_prop on small dense graphs can put all nodes in
    # one community, yielding modularity = 0)
    pass_mod <- !is.na(co_mod) && is.finite(co_mod) && co_mod >= 0
    .log_result("label_prop_modularity_nonneg", cfg, 1L,
                as.integer(pass_mod), as.integer(!pass_mod),
                0, 0, 0, 0, "igraph", sprintf("mod=%.6f", co_mod))

    # Self-consistency: modularity matches recomputed with same weights
    recomputed_mod <- igraph::modularity(g_internal, co_comm$community,
                                         weights = w_internal)
    .compare_scalar(co_mod, recomputed_mod,
                    "label_prop_modularity_selfcheck", cfg)

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  lp_rows <- df[grepl("^label_prop_", df$function_name), ]
  n_fail <- sum(lp_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("label_prop: %d values failed across %d configs",
                   n_fail, nrow(lp_rows)))
})

# =============================================================================
# 6. Stochastic: infomap -- self-consistency checks
# =============================================================================

test_that("infomap: self-consistency checks (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i])
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                  attr = "weight"))
    co_comm <- cograph::detect_communities(mat, method = "infomap")
    co_mod <- attr(co_comm, "modularity")

    # Reconstruct the same igraph that detect_communities builds internally
    g_internal <- cograph::to_igraph(mat)
    w_internal <- igraph::E(g_internal)$weight

    # All nodes assigned
    n_nodes <- igraph::vcount(g_internal)
    n_assigned <- nrow(co_comm)
    .compare_scalar(n_assigned, n_nodes, "infomap_all_assigned", cfg, tol = 0)

    # At least 1 community
    co_k <- length(unique(co_comm$community))
    pass_k <- co_k >= 1L
    .log_result("infomap_n_communities", cfg, 1L, as.integer(pass_k),
                as.integer(!pass_k), 0, 0, 0, 0, "igraph",
                sprintf("k=%d", co_k))

    # Modularity: infomap modularity can be 0 for trivial partitions on small
    # dense graphs, so only check it is not NA and is finite
    pass_mod <- !is.na(co_mod) && is.finite(co_mod)
    .log_result("infomap_modularity_finite", cfg, 1L,
                as.integer(pass_mod), as.integer(!pass_mod),
                0, 0, 0, 0, "igraph", sprintf("mod=%.6f", co_mod))

    # Self-consistency: modularity matches recomputed with same weights
    recomputed_mod <- igraph::modularity(g_internal, co_comm$community,
                                         weights = w_internal)
    .compare_scalar(co_mod, recomputed_mod,
                    "infomap_modularity_selfcheck", cfg)

    invisible(NULL)
  })

  df <- do.call(rbind, .equiv_log$rows)
  im_rows <- df[grepl("^infomap_", df$function_name), ]
  n_fail <- sum(im_rows$values_failed)
  expect_equal(n_fail, 0L,
    info = sprintf("infomap: %d values failed across %d configs",
                   n_fail, nrow(im_rows)))
})

# =============================================================================
# 7. Output structure validation (all methods, single graph)
# =============================================================================

test_that("detect_communities: output structure correct for all methods", {
  g <- .make_connected_graph(15, 0.3, 42)
  mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE,
                                                attr = "weight"))
  methods <- c("louvain", "walktrap", "fast_greedy",
               "label_prop", "infomap", "leiden")

  lapply(methods, function(m) {
    comm <- cograph::detect_communities(mat, method = m)

    # Class
    expect_true(inherits(comm, "cograph_communities"),
                info = sprintf("%s: missing cograph_communities class", m))
    expect_true(inherits(comm, "data.frame"),
                info = sprintf("%s: missing data.frame class", m))

    # Columns
    expect_true("node" %in% names(comm),
                info = sprintf("%s: missing 'node' column", m))
    expect_true("community" %in% names(comm),
                info = sprintf("%s: missing 'community' column", m))

    # Dimensions
    expect_equal(nrow(comm), igraph::vcount(g),
                 info = sprintf("%s: wrong number of rows", m))

    # Attributes
    expect_false(is.null(attr(comm, "modularity")),
                 info = sprintf("%s: missing modularity attribute", m))
    expect_false(is.null(attr(comm, "algorithm")),
                 info = sprintf("%s: missing algorithm attribute", m))
    expect_equal(attr(comm, "algorithm"), m,
                 info = sprintf("%s: algorithm attribute mismatch", m))

    # Node names match
    expect_equal(comm$node, igraph::V(g)$name,
                 info = sprintf("%s: node names mismatch", m))

    invisible(NULL)
  })
})

# =============================================================================
# Print per-function summary with delta stats
# =============================================================================

test_that("detect_communities equivalence: per-function delta report", {
  df <- do.call(rbind, .equiv_log$rows)
  if (is.null(df) || nrow(df) == 0L) {
    expect_true(TRUE)
    return(invisible(NULL))
  }
  fns <- unique(df$function_name)
  lapply(fns, function(fn) {
    sub <- df[df$function_name == fn, ]
    status <- if (all(sub$values_failed == 0)) "PASS" else "FAIL"
    cat(sprintf("  %-45s %s  mean_d=%.2e  median_d=%.2e  max_d=%.2e  p95_d=%.2e\n",
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

test_that("detect_communities equivalence: zero total failures + reports written", {
  report <- .write_report()
  .write_cvs_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L,
    info = sprintf("Failed %d values across %d configs",
                   sum(report$values_failed), nrow(report)))
})
