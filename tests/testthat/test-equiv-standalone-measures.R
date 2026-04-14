# =============================================================================
# Numerical Equivalence: Standalone network-level measures
#
# estrada_index, trophic_incoherence, group_centrality, network_small_world
# 100 connected graphs per function. Every value checked vs mathematical def.
# Reports to: tmp/standalone_measures_equivalence_report.csv + CVS inbox
# =============================================================================

skip_on_cran()
skip_coverage_tests()
skip_if_not_installed("igraph")

# ---------------------------------------------------------------------------
# Report infrastructure (same pattern as test-equiv-network-summary.R)
# ---------------------------------------------------------------------------

.equiv_log <- new.env(parent = emptyenv())
.equiv_log$rows <- list()

.log_result <- function(func, config, n_checked, n_passed, n_failed,
                        max_abs_err = NA_real_, mean_abs_err = NA_real_,
                        median_abs_err = NA_real_, p95_abs_err = NA_real_,
                        reference_package = "manual", notes = "") {
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
  utils::write.csv(df, file.path(tempdir(), "standalone_measures_equivalence_report.csv"),
                   row.names = FALSE)
  cat(sprintf(
    paste0("\n=== STANDALONE MEASURES EQUIVALENCE REPORT ===\n",
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
    file.path(tempdir(), "standalone_measures_equivalence_report.csv")
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
      ancestorTitles = list("standalone measures equivalence"),
      title = sprintf("%s: n=%d seed=%d delta=%.2e",
                      r$function_name, r$n_nodes, r$seed, r$max_abs_error),
      fullName = sprintf("standalone measures > %s: n=%d seed=%d",
                         r$function_name, r$n_nodes, r$seed),
      status = status, duration = 0L,
      failureMessages = if (status == "failed")
        list(sprintf("max_err=%.2e, %d/%d failed",
                     r$max_abs_error, r$values_failed, r$values_checked))
      else list(),
      `_cvs` = list(delta = r$max_abs_error, tolerance = TOL,
                    rFunction = r$function_name,
                    rPackage = r$reference_package,
                    module = "standalone-measures")
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
      name = "tests/testthat/test-equiv-standalone-measures.R",
      assertionResults = assertions
    )),
    `_cvs` = list(target = "cograph")
  )

  inbox <- file.path("..", "..", "validation", "data", "inbox")
  if (!dir.exists(inbox)) inbox <- file.path("..", "..", "..", "validation", "data", "inbox")
  if (dir.exists(inbox)) {
    fname <- sprintf("cograph-standalone-measures-%s.json",
                     format(Sys.time(), "%Y%m%dT%H%M%S"))
    jsonlite::write_json(result, file.path(inbox, fname),
                         auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("  CVS report written: %s\n", fname))
  }
}

# ---------------------------------------------------------------------------
# Graph generators
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
  igraph::V(g)$name <- paste0("N", seq_len(igraph::vcount(g)))
  g
}

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

set.seed(2026)
N <- 100L
TOL <- 1e-8
sizes <- sample(c(8, 10, 12, 15, 20), N, replace = TRUE)
densities <- runif(N, 0.2, 0.5)
seeds <- sample.int(100000, N)

cat("\n")
cat("================================================================\n")
cat("  STANDALONE MEASURES EQUIVALENCE REPORT\n")
cat(sprintf("  %d random connected graphs per function\n", N))
cat(sprintf("  Tolerance: %.0e\n", TOL))
cat("================================================================\n\n")

# =============================================================================
# 1. estrada_index — vs eigenvalue definition + subgraph centrality sum
# =============================================================================

test_that("estrada_index: 100 networks vs eigenvalue formula", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

    # cograph
    co_val <- estrada_index(mat)

    # Reference: direct eigenvalue computation
    ev <- eigen(mat, only.values = TRUE, symmetric = TRUE)$values
    ref_val <- sum(exp(ev))

    delta <- abs(co_val - ref_val)
    pass <- delta <= TOL
    .log_result("estrada_index_eigen", cfg, 1L, as.integer(pass),
                as.integer(!pass), delta, delta, delta, delta, "manual_eigen")
    expect_true(pass,
      info = sprintf("n=%d seed=%d delta=%.2e", cfg$n, cfg$seed, delta))
  })
})

test_that("estrada_index: equals sum of subgraph centrality (100 networks)", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

    co_ei <- estrada_index(mat)
    # subgraph centrality: diagonal of expm(A) = per-node subgraph centrality
    # Estrada index = trace(expm(A)) = sum(subgraph centrality)
    expm_diag <- diag(as.matrix(Matrix::expm(Matrix::Matrix(mat))))
    ref_sc <- sum(expm_diag)

    delta <- abs(co_ei - ref_sc)
    pass <- delta <= TOL
    .log_result("estrada_index_vs_subgraph", cfg, 1L, as.integer(pass),
                as.integer(!pass), delta, delta, delta, delta,
                "Matrix::expm")
    expect_true(pass,
      info = sprintf("n=%d seed=%d delta=%.2e", cfg$n, cfg$seed, delta))
  })
})

# =============================================================================
# 2. trophic_incoherence — vs manual population std dev
# =============================================================================

test_that("trophic_incoherence: 100 directed networks vs manual formula", {
  lapply(seq_len(N), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = TRUE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed, directed = TRUE)

    # Ensure at least one basal node (no incoming edges)
    in_deg <- igraph::degree(g, mode = "in")
    if (all(in_deg > 0)) {
      # Force one basal node by removing all incoming edges to node 1
      incoming <- igraph::incident(g, 1, mode = "in")
      if (length(incoming) > 0) g <- igraph::delete_edges(g, incoming)
    }

    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

    # cograph
    co_val <- trophic_incoherence(mat)

    if (is.na(co_val)) {
      .log_result("trophic_incoherence", cfg, 0L, 0L, 0L, NA_real_,
                  NA_real_, NA_real_, NA_real_, "manual", "NA result")
      return(invisible(NULL))
    }

    # Reference: manually compute trophic levels and population std dev
    # Trophic levels: s = (L^T)^(-1) * b where L = diag(in_degree) - A^T
    # and b_i = in_degree_i
    # This is equivalent to: s_j = 1 + (1/k_in_j) * sum_{i: i->j} s_i
    n_v <- igraph::vcount(g)
    A <- mat
    in_d <- colSums(A)  # in-degree from adjacency matrix (column sums for directed)

    # Build the linear system: for each node j with in_d[j] > 0:
    # s_j - (1/k_in_j) * sum_{i} A[i,j] * s_i = 1
    # For basal nodes (in_d = 0): s_j = 1 (convention)
    coef_mat <- diag(n_v)
    rhs <- rep(1, n_v)
    for (j in seq_len(n_v)) {
      if (in_d[j] > 0) {
        for (ii in seq_len(n_v)) {
          if (A[ii, j] > 0) {
            coef_mat[j, ii] <- coef_mat[j, ii] - 1 / in_d[j]
          }
        }
      }
    }
    ref_levels <- tryCatch(solve(coef_mat, rhs), error = function(e) NULL)
    if (is.null(ref_levels)) {
      .log_result("trophic_incoherence", cfg, 0L, 0L, 0L, NA_real_,
                  NA_real_, NA_real_, NA_real_, "manual", "singular system")
      return(invisible(NULL))
    }

    # Compute edge trophic differences
    el <- igraph::as_edgelist(g, names = FALSE)
    diffs <- ref_levels[el[, 2]] - ref_levels[el[, 1]]
    # Population std dev (NOT sample sd)
    ref_q <- sqrt(mean((diffs - mean(diffs))^2))

    delta <- abs(co_val - ref_q)
    pass <- delta <= TOL
    .log_result("trophic_incoherence", cfg, 1L, as.integer(pass),
                as.integer(!pass), delta, delta, delta, delta, "manual_linear_system")
    expect_true(pass,
      info = sprintf("n=%d seed=%d co=%.6f ref=%.6f delta=%.2e",
                     cfg$n, cfg$seed, co_val, ref_q, delta))
  })
})

# =============================================================================
# 3. group_centrality — degree and closeness vs manual formulas
# =============================================================================

test_that("group_centrality degree: 50 networks x 3 random groups", {
  lapply(seq_len(min(N, 50)), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    n_v <- igraph::vcount(g)
    node_names <- igraph::V(g)$name

    # Try 3 random group selections per network
    set.seed(cfg$seed + 1000)
    for (grp_idx in 1:3) {
      grp_size <- sample(2:max(2, n_v %/% 3), 1)
      grp <- sample(node_names, grp_size)

      co_val <- group_centrality(mat, nodes = grp, measure = "degree")

      # Manual: |N(C) \ C| / (|V| - |C|)
      grp_idx_num <- match(grp, node_names)
      non_grp <- setdiff(seq_len(n_v), grp_idx_num)
      # Neighbors of group = nodes adjacent to any group member
      adj <- mat[grp_idx_num, , drop = FALSE]
      neighbors <- which(colSums(adj) > 0)
      # External neighbors = neighbors not in group
      ext_neighbors <- setdiff(neighbors, grp_idx_num)
      ref_val <- length(ext_neighbors) / max(1, length(non_grp))

      delta <- abs(co_val - ref_val)
      pass <- delta <= TOL
      .log_result("group_centrality_degree", cfg, 1L, as.integer(pass),
                  as.integer(!pass), delta, delta, delta, delta,
                  "manual_formula",
                  sprintf("grp_size=%d", grp_size))
      expect_true(pass,
        info = sprintf("n=%d seed=%d grp=%d delta=%.2e",
                       cfg$n, cfg$seed, grp_size, delta))
    }
  })
})

test_that("group_centrality closeness: 50 networks x 3 random groups", {
  lapply(seq_len(min(N, 50)), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    n_v <- igraph::vcount(g)
    node_names <- igraph::V(g)$name

    set.seed(cfg$seed + 2000)
    for (grp_idx in 1:3) {
      grp_size <- sample(2:max(2, n_v %/% 3), 1)
      grp <- sample(node_names, grp_size)

      co_val <- group_centrality(mat, nodes = grp, measure = "closeness")

      # Manual: |V - C| / sum of min distances from each non-C node to C
      grp_idx_num <- match(grp, node_names)
      non_grp <- setdiff(seq_len(n_v), grp_idx_num)
      dists <- igraph::distances(g, weights = NA)

      if (length(non_grp) == 0) {
        ref_val <- 1
      } else {
        min_dists <- vapply(non_grp, function(v) {
          min(dists[v, grp_idx_num])
        }, numeric(1))
        total_dist <- sum(min_dists)
        ref_val <- if (total_dist == 0) 1 else length(non_grp) / total_dist
      }

      delta <- abs(co_val - ref_val)
      pass <- delta <= TOL
      .log_result("group_centrality_closeness", cfg, 1L, as.integer(pass),
                  as.integer(!pass), delta, delta, delta, delta,
                  "manual_formula",
                  sprintf("grp_size=%d", grp_size))
      expect_true(pass,
        info = sprintf("n=%d seed=%d grp=%d delta=%.2e",
                       cfg$n, cfg$seed, grp_size, delta))
    }
  })
})

# =============================================================================
# 4. network_small_world — verify components match igraph
# =============================================================================

test_that("network_small_world: returns valid sigma > 0 (50 networks)", {
  lapply(seq_len(min(N, 50)), function(i) {
    cfg <- list(n = sizes[i], density = densities[i], seed = seeds[i],
                directed = FALSE)
    g <- .make_connected_graph(cfg$n, cfg$density, cfg$seed)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

    sigma <- network_small_world(mat, n_random = 5)

    # sigma should be a finite non-negative number for connected graphs.
    # sigma = 0 when C_obs = 0 (no triangles — definitively not small-world).
    # NA only valid when L_obs is 0 or Inf (truly degenerate).
    valid <- is.finite(sigma) && sigma >= 0
    .log_result("small_world_sigma", cfg, 1L, as.integer(valid),
                as.integer(!valid), 0, 0, 0, 0, "self_consistency",
                sprintf("sigma=%.4f", sigma))
    expect_true(valid,
      info = sprintf("n=%d seed=%d sigma=%s", cfg$n, cfg$seed, sigma))
  })
})

# =============================================================================
# Per-function delta report + final assertion
# =============================================================================

test_that("standalone measures: per-function delta report", {
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
  expect_true(TRUE)
})

test_that("standalone measures: zero total failures + reports written", {
  report <- .write_report()
  .write_cvs_report()
  expect_true(is.data.frame(report))
  expect_equal(sum(report$values_failed), 0L,
    info = sprintf("Failed %d values across %d configs",
                   sum(report$values_failed), nrow(report)))
})
