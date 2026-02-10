#!/usr/bin/env Rscript
#' ============================================================================
#' CENTRALITY VALIDATION: cograph vs igraph
#' ============================================================================
#'
#' This script validates that cograph centrality functions produce identical
#' results to direct igraph calls across randomized networks.
#'
#' USAGE:
#'   Rscript validation/test_centrality_igraph.R [n_tests] [seed]
#'
#' EXAMPLES:
#'   Rscript validation/test_centrality_igraph.R          # 100 tests, seed=42
#'   Rscript validation/test_centrality_igraph.R 500      # 500 tests, seed=42
#'   Rscript validation/test_centrality_igraph.R 100 123  # 100 tests, seed=123
#'
#' OUTPUT:
#'   - Console: Summary and any failures
#'   - validation/results_centrality_igraph.txt: Detailed results
#'   - validation/results_centrality_igraph.rds: R object with full data
#'
#' ============================================================================

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
n_tests <- if (length(args) >= 1) as.integer(args[1]) else 100
seed <- if (length(args) >= 2) as.integer(args[2]) else 42

# Load packages
suppressPackageStartupMessages({
  library(Saqrlab)
  library(igraph)
  devtools::load_all(".", quiet = TRUE)
})

# Configuration
tolerance <- 1e-10
output_dir <- "validation"
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("CENTRALITY VALIDATION: cograph vs igraph\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("Timestamp:", timestamp, "\n")
cat("Tests:", n_tests, "| Seed:", seed, "| Tolerance:", tolerance, "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# Helper function to compare vectors
compare_vectors <- function(cograph_val, igraph_val, measure, test_id) {
  cograph_val[is.nan(cograph_val)] <- NA
  igraph_val[is.nan(igraph_val)] <- NA

  if (length(cograph_val) != length(igraph_val)) {
    return(list(
      passed = FALSE,
      reason = sprintf("Length mismatch: cograph=%d, igraph=%d",
                       length(cograph_val), length(igraph_val)),
      max_diff = NA
    ))
  }

  both_na <- is.na(cograph_val) & is.na(igraph_val)
  both_finite <- is.finite(cograph_val) & is.finite(igraph_val)

  if (!all(both_na | both_finite)) {
    mismatch_idx <- which(!(both_na | both_finite))
    return(list(
      passed = FALSE,
      reason = sprintf("NA/Inf mismatch at positions: %s",
                       paste(head(mismatch_idx, 5), collapse = ", ")),
      max_diff = NA
    ))
  }

  if (any(both_finite)) {
    max_diff <- max(abs(cograph_val[both_finite] - igraph_val[both_finite]))
    if (max_diff > tolerance) {
      return(list(
        passed = FALSE,
        reason = sprintf("Value mismatch: max_diff=%.2e", max_diff),
        max_diff = max_diff
      ))
    }
  } else {
    max_diff <- 0
  }

  return(list(passed = TRUE, reason = "OK", max_diff = max_diff))
}

# Initialize results storage
set.seed(seed)
results <- list()
all_test_results <- list()

cat("Running", n_tests, "tests...\n\n")
pb <- txtProgressBar(min = 0, max = n_tests, style = 3)

for (i in 1:n_tests) {
  setTxtProgressBar(pb, i)

  # Generate random network parameters
  n_nodes <- sample(5:30, 1)
  directed <- sample(c(TRUE, FALSE), 1)
  edge_density <- runif(1, 1.5, 4)
  weighted <- sample(c(TRUE, FALSE), 1)

  # Generate edge list
  el <- simulate_edge_list(
    n_nodes = n_nodes,
    edge_density = edge_density,
    directed = directed,
    allow_self_loops = FALSE,
    weight_range = c(0.1, 1),
    seed = i * 1000 + seed
  )

  if (nrow(el) == 0) next

  # Create igraph object
  g <- graph_from_data_frame(el[, c("source", "target")], directed = directed)
  if (weighted) {
    E(g)$weight <- el$weight
  }

  weights <- if (weighted && !is.null(E(g)$weight)) E(g)$weight else NULL
  test_results <- list()

  # Store network info
  network_info <- list(
    test_id = i,
    n_nodes = n_nodes,
    n_edges = nrow(el),
    directed = directed,
    weighted = weighted,
    has_multiple = any_multiple(g)
  )

  # ===== TEST EACH MEASURE =====

  # 1. DEGREE
  tryCatch({
    for (mode in c("all", "in", "out")) {
      if (!directed && mode != "all") next
      cograph_val <- centrality_degree(g, mode = mode)
      igraph_val <- igraph::degree(g, mode = mode)
      names(igraph_val) <- V(g)$name
      result <- compare_vectors(cograph_val, igraph_val, paste0("degree_", mode), i)
      test_results[[paste0("degree_", mode)]] <- result
    }
  }, error = function(e) {
    test_results[["degree"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 2. STRENGTH
  tryCatch({
    for (mode in c("all", "in", "out")) {
      if (!directed && mode != "all") next
      cograph_val <- centrality_strength(g, mode = mode)
      igraph_val <- igraph::strength(g, mode = mode, weights = weights)
      names(igraph_val) <- V(g)$name
      result <- compare_vectors(cograph_val, igraph_val, paste0("strength_", mode), i)
      test_results[[paste0("strength_", mode)]] <- result
    }
  }, error = function(e) {
    test_results[["strength"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 3. BETWEENNESS
  tryCatch({
    cograph_val <- centrality_betweenness(g)
    igraph_val <- igraph::betweenness(g, weights = weights, directed = directed)
    names(igraph_val) <- V(g)$name
    result <- compare_vectors(cograph_val, igraph_val, "betweenness", i)
    test_results[["betweenness"]] <- result
  }, error = function(e) {
    test_results[["betweenness"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 4. CLOSENESS
  tryCatch({
    for (mode in c("all", "in", "out")) {
      if (!directed && mode != "all") next

      # Unnormalized
      cograph_val <- centrality_closeness(g, mode = mode, normalized = FALSE)
      igraph_val <- igraph::closeness(g, mode = mode, weights = weights, normalized = FALSE)
      names(igraph_val) <- V(g)$name
      result <- compare_vectors(cograph_val, igraph_val, paste0("closeness_", mode), i)
      test_results[[paste0("closeness_", mode)]] <- result

      # Normalized
      cograph_val_norm <- centrality_closeness(g, mode = mode, normalized = TRUE)
      igraph_val_norm <- igraph::closeness(g, mode = mode, weights = weights, normalized = TRUE)
      names(igraph_val_norm) <- V(g)$name
      result_norm <- compare_vectors(cograph_val_norm, igraph_val_norm,
                                     paste0("closeness_", mode, "_norm"), i)
      test_results[[paste0("closeness_", mode, "_norm")]] <- result_norm
    }
  }, error = function(e) {
    test_results[["closeness"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 5. EIGENVECTOR
  tryCatch({
    cograph_val <- centrality_eigenvector(g)
    igraph_val <- igraph::eigen_centrality(g, weights = weights, directed = directed)$vector
    names(igraph_val) <- V(g)$name
    result <- compare_vectors(cograph_val, igraph_val, "eigenvector", i)
    test_results[["eigenvector"]] <- result
  }, error = function(e) {
    test_results[["eigenvector"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 6. PAGERANK
  tryCatch({
    # Default damping
    cograph_val <- centrality_pagerank(g)
    igraph_val <- igraph::page_rank(g, weights = weights, directed = directed)$vector
    names(igraph_val) <- V(g)$name
    result <- compare_vectors(cograph_val, igraph_val, "pagerank", i)
    test_results[["pagerank"]] <- result

    # Custom damping
    cograph_val_d <- centrality_pagerank(g, damping = 0.5)
    igraph_val_d <- igraph::page_rank(g, weights = weights, directed = directed, damping = 0.5)$vector
    names(igraph_val_d) <- V(g)$name
    result_d <- compare_vectors(cograph_val_d, igraph_val_d, "pagerank_d05", i)
    test_results[["pagerank_damping"]] <- result_d

    # Personalized PageRank
    pers_vec <- rep(0, vcount(g))
    names(pers_vec) <- V(g)$name
    pers_vec[1] <- 1
    cograph_val_p <- centrality_pagerank(g, personalized = pers_vec)
    igraph_val_p <- igraph::page_rank(g, weights = weights, directed = directed,
                                       personalized = pers_vec)$vector
    names(igraph_val_p) <- V(g)$name
    result_p <- compare_vectors(cograph_val_p, igraph_val_p, "pagerank_pers", i)
    test_results[["pagerank_personalized"]] <- result_p
  }, error = function(e) {
    test_results[["pagerank"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 7. AUTHORITY
  tryCatch({
    cograph_val <- centrality_authority(g)
    igraph_val <- igraph::hits_scores(g, weights = weights)$authority
    names(igraph_val) <- V(g)$name
    result <- compare_vectors(cograph_val, igraph_val, "authority", i)
    test_results[["authority"]] <- result
  }, error = function(e) {
    test_results[["authority"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 8. HUB
  tryCatch({
    cograph_val <- centrality_hub(g)
    igraph_val <- igraph::hits_scores(g, weights = weights)$hub
    names(igraph_val) <- V(g)$name
    result <- compare_vectors(cograph_val, igraph_val, "hub", i)
    test_results[["hub"]] <- result
  }, error = function(e) {
    test_results[["hub"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 9. ECCENTRICITY
  tryCatch({
    for (mode in c("all", "in", "out")) {
      if (!directed && mode != "all") next
      cograph_val <- centrality_eccentricity(g, mode = mode)
      igraph_val <- igraph::eccentricity(g, mode = mode)
      names(igraph_val) <- V(g)$name
      result <- compare_vectors(cograph_val, igraph_val, paste0("eccentricity_", mode), i)
      test_results[[paste0("eccentricity_", mode)]] <- result
    }
  }, error = function(e) {
    test_results[["eccentricity"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 10. CORENESS
  tryCatch({
    for (mode in c("all", "in", "out")) {
      if (!directed && mode != "all") next
      cograph_val <- centrality_coreness(g, mode = mode)
      igraph_val <- igraph::coreness(g, mode = mode)
      names(igraph_val) <- V(g)$name
      result <- compare_vectors(cograph_val, igraph_val, paste0("coreness_", mode), i)
      test_results[[paste0("coreness_", mode)]] <- result
    }
  }, error = function(e) {
    test_results[["coreness"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 11. CONSTRAINT
  tryCatch({
    cograph_val <- centrality_constraint(g)
    igraph_val <- igraph::constraint(g, weights = weights)
    names(igraph_val) <- V(g)$name
    result <- compare_vectors(cograph_val, igraph_val, "constraint", i)
    test_results[["constraint"]] <- result
  }, error = function(e) {
    test_results[["constraint"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 12. TRANSITIVITY (using simplify=FALSE for fair comparison)
  tryCatch({
    # Local transitivity
    cograph_val <- centrality(g, measures = "transitivity", transitivity_type = "local",
                              simplify = FALSE)$transitivity
    names(cograph_val) <- V(g)$name
    igraph_val <- igraph::transitivity(g, type = "local")
    names(igraph_val) <- V(g)$name
    result <- compare_vectors(cograph_val, igraph_val, "transitivity_local", i)
    test_results[["transitivity_local"]] <- result

    # Global transitivity
    cograph_df <- centrality(g, measures = "transitivity", transitivity_type = "global",
                             simplify = FALSE)
    cograph_val_g <- cograph_df$transitivity[1]
    igraph_val_g <- igraph::transitivity(g, type = "global")

    if (abs(cograph_val_g - igraph_val_g) < tolerance ||
        (is.nan(cograph_val_g) && is.nan(igraph_val_g))) {
      test_results[["transitivity_global"]] <- list(passed = TRUE, reason = "OK", max_diff = 0)
    } else {
      test_results[["transitivity_global"]] <- list(
        passed = FALSE,
        reason = sprintf("Value mismatch: cograph=%.6f, igraph=%.6f", cograph_val_g, igraph_val_g),
        max_diff = abs(cograph_val_g - igraph_val_g)
      )
    }
  }, error = function(e) {
    test_results[["transitivity"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 13. HARMONIC
  tryCatch({
    for (mode in c("all", "in", "out")) {
      if (!directed && mode != "all") next
      cograph_val <- centrality_harmonic(g, mode = mode)
      igraph_val <- igraph::harmonic_centrality(g, mode = mode, weights = weights)
      names(igraph_val) <- V(g)$name
      result <- compare_vectors(cograph_val, igraph_val, paste0("harmonic_", mode), i)
      test_results[[paste0("harmonic_", mode)]] <- result
    }
  }, error = function(e) {
    test_results[["harmonic"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 14. CUTOFF tests
  tryCatch({
    cutoff_val <- 2
    cograph_val <- centrality(g, measures = "betweenness", cutoff = cutoff_val)$betweenness
    igraph_val <- igraph::betweenness(g, weights = weights, directed = directed, cutoff = cutoff_val)
    result <- compare_vectors(cograph_val, unname(igraph_val), "betweenness_cutoff", i)
    test_results[["betweenness_cutoff"]] <- result

    cograph_val_c <- centrality(g, measures = "closeness", cutoff = cutoff_val)$closeness_all
    igraph_val_c <- igraph::closeness(g, mode = "all", weights = weights, cutoff = cutoff_val)
    result_c <- compare_vectors(cograph_val_c, unname(igraph_val_c), "closeness_cutoff", i)
    test_results[["closeness_cutoff"]] <- result_c
  }, error = function(e) {
    test_results[["cutoff"]] <- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # Store results
  all_test_results[[i]] <- list(
    network = network_info,
    results = test_results
  )
}

close(pb)

# ===== COMPUTE SUMMARY STATISTICS =====

all_measures <- unique(unlist(lapply(all_test_results, function(x) names(x$results))))
measure_stats <- data.frame(
  measure = all_measures,
  passed = 0,
  failed = 0,
  max_diff = NA_real_,
  stringsAsFactors = FALSE
)

failures <- list()

for (r in all_test_results) {
  if (is.null(r)) next
  for (m in names(r$results)) {
    idx <- which(measure_stats$measure == m)
    if (length(idx) > 0) {
      if (r$results[[m]]$passed) {
        measure_stats$passed[idx] <- measure_stats$passed[idx] + 1
      } else {
        measure_stats$failed[idx] <- measure_stats$failed[idx] + 1
        failures[[length(failures) + 1]] <- list(
          test_id = r$network$test_id,
          measure = m,
          reason = r$results[[m]]$reason,
          network = r$network
        )
      }
      if (!is.na(r$results[[m]]$max_diff)) {
        current_max <- measure_stats$max_diff[idx]
        if (is.na(current_max) || r$results[[m]]$max_diff > current_max) {
          measure_stats$max_diff[idx] <- r$results[[m]]$max_diff
        }
      }
    }
  }
}

measure_stats$total <- measure_stats$passed + measure_stats$failed
measure_stats$pass_rate <- sprintf("%.1f%%", 100 * measure_stats$passed / measure_stats$total)
measure_stats <- measure_stats[order(measure_stats$measure), ]

# ===== OUTPUT RESULTS =====

cat("\n\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("RESULTS SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

print(measure_stats, row.names = FALSE)

total_tests <- sum(measure_stats$total)
total_passed <- sum(measure_stats$passed)
total_failed <- sum(measure_stats$failed)

cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat(sprintf("TOTAL: %d tests, %d passed, %d failed (%.2f%% pass rate)\n",
            total_tests, total_passed, total_failed,
            100 * total_passed / total_tests))
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (length(failures) > 0) {
  cat("\nFAILURES:\n")
  for (f in failures[1:min(10, length(failures))]) {
    cat(sprintf("  Test %d, %s: %s\n", f$test_id, f$measure, f$reason))
  }
  if (length(failures) > 10) {
    cat(sprintf("  ... and %d more failures\n", length(failures) - 10))
  }
} else {
  cat("\n*** ALL TESTS PASSED! ***\n")
}

# ===== SAVE RESULTS =====

# Text report
report_file <- file.path(output_dir, "results_centrality_igraph.txt")
sink(report_file)
cat("CENTRALITY VALIDATION REPORT\n")
cat("============================\n\n")
cat("Timestamp:", timestamp, "\n")
cat("Tests:", n_tests, "\n")
cat("Seed:", seed, "\n")
cat("Tolerance:", tolerance, "\n")
cat("R version:", R.version.string, "\n")
cat("igraph version:", as.character(packageVersion("igraph")), "\n")
cat("cograph version:", as.character(packageVersion("cograph")), "\n\n")

cat("SUMMARY BY MEASURE\n")
cat("------------------\n")
print(measure_stats, row.names = FALSE)

cat("\n\nOVERALL RESULTS\n")
cat("---------------\n")
cat(sprintf("Total tests: %d\n", total_tests))
cat(sprintf("Passed: %d (%.2f%%)\n", total_passed, 100 * total_passed / total_tests))
cat(sprintf("Failed: %d (%.2f%%)\n", total_failed, 100 * total_failed / total_tests))

if (length(failures) > 0) {
  cat("\n\nFAILURE DETAILS\n")
  cat("---------------\n")
  for (f in failures) {
    cat(sprintf("\nTest %d - %s:\n", f$test_id, f$measure))
    cat(sprintf("  Network: n=%d, e=%d, directed=%s, weighted=%s\n",
                f$network$n_nodes, f$network$n_edges, f$network$directed, f$network$weighted))
    cat(sprintf("  Reason: %s\n", f$reason))
  }
}
sink()

# RDS file with full data
rds_file <- file.path(output_dir, "results_centrality_igraph.rds")
saveRDS(list(
  timestamp = timestamp,
  config = list(n_tests = n_tests, seed = seed, tolerance = tolerance),
  summary = measure_stats,
  failures = failures,
  all_results = all_test_results
), rds_file)

cat("\n\nResults saved to:\n")
cat("  Text report:", report_file, "\n")
cat("  R data file:", rds_file, "\n")
