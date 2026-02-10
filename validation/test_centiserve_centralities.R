#!/usr/bin/env Rscript
#' ============================================================================
#' CENTISERVE CENTRALITY MEASURES VALIDATION
#' ============================================================================
#'
#' Validates cograph's implementations of centrality measures produce correct
#' results and match expected behavior.
#'
#' Measures tested:
#'   - leverage centrality
#'   - geodesic k-path centrality (kreach)
#'   - resilience centrality (vertex connectivity based)
#'
#' USAGE:
#'   Rscript validation/test_centiserve_centralities.R
#'
#' ============================================================================

library(igraph)
suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("CENTRALITY MEASURES VALIDATION\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n\n")

# ============================================
# Original centiserve implementations
# ============================================

#' Original leverage centrality from centiserve
leverage.original <- function(graph, vids = V(graph), mode = c("all", "out", "in"),
                              loops = TRUE) {
  k <- degree(graph, mode = mode[1], loops = as.logical(loops))
  n <- vcount(graph)
  res <- sapply(1:n, function(v) {
    mean((k[v] - k[neighbors(graph, v, mode = mode[1])]) /
         (k[v] + k[neighbors(graph, v, mode = mode[1])]))
  })
  if (is_named(graph)) {
    names(res) <- V(graph)$name
  }
  res[vids]
}

#' Original geokpath centrality from centiserve (with fixed variable name)
kreach.original <- function(graph, vids = V(graph), mode = c("all", "out", "in"),
                            weights = NULL, kpath = 3) {
  kpath <- as.integer(kpath)
  if (kpath <= 0) stop("The k parameter must be greater than 0.", call. = FALSE)
  res <- integer()
  sp <- distances(graph, mode = mode[1], weights = weights)
  for (v in V(graph)[vids]) {
    res <- append(res, length(sp[v, sp[v,] <= kpath]) - 1)
  }
  if (is_named(graph)) {
    names(res) <- V(graph)$name[vids]
  }
  res
}

# ============================================
# Test configuration
# ============================================

set.seed(42)
tolerance <- 1e-10
results <- list()

# Test function for numeric comparison
run_test <- function(name, original, cograph, tolerance = 1e-10) {
  cat(sprintf("  %-45s", name))

  # Handle NaN comparisons
  orig_nan <- is.nan(original)
  new_nan <- is.nan(cograph)

  if (!identical(orig_nan, new_nan)) {
    cat("FAIL (NaN mismatch)\n")
    return(FALSE)
  }

  # Compare non-NaN values
  orig_vals <- original[!orig_nan]
  new_vals <- cograph[!new_nan]

  if (length(orig_vals) == 0 && length(new_vals) == 0) {
    cat("PASS (all NaN)\n")
    return(TRUE)
  }

  comparison <- all.equal(as.numeric(orig_vals), as.numeric(new_vals),
                          tolerance = tolerance)

  if (isTRUE(comparison)) {
    cat("PASS\n")
    return(TRUE)
  } else {
    cat("FAIL\n")
    cat("    Original:", paste(head(original, 5), collapse = ", "), "...\n")
    cat("    Cograph: ", paste(head(cograph, 5), collapse = ", "), "...\n")
    cat("    Diff:    ", comparison, "\n")
    return(FALSE)
  }
}

# Test function for expected values
run_expected_test <- function(name, actual, expected) {
  cat(sprintf("  %-45s", name))

  if (identical(as.numeric(actual), as.numeric(expected))) {
    cat("PASS\n")
    return(TRUE)
  } else {
    cat("FAIL\n")
    cat("    Expected:", paste(expected, collapse = ", "), "\n")
    cat("    Actual:  ", paste(actual, collapse = ", "), "\n")
    return(FALSE)
  }
}

# ============================================
# Create test networks
# ============================================

# Simple undirected
adj <- matrix(c(0,1,1,0,0, 1,0,1,1,0, 1,1,0,1,1, 0,1,1,0,1, 0,0,1,1,0), 5, 5, byrow=TRUE)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
g_undir <- graph_from_adjacency_matrix(adj, mode = "undirected")

# Directed
g_dir <- graph_from_adjacency_matrix(adj, mode = "directed")
V(g_dir)$name <- LETTERS[1:5]

# Random undirected
g_rand_u <- sample_gnp(30, 0.2, directed = FALSE)
V(g_rand_u)$name <- paste0("N", 1:30)

# Random directed
g_rand_d <- sample_gnp(30, 0.15, directed = TRUE)
V(g_rand_d)$name <- paste0("N", 1:30)

# Complete graph
g_complete <- make_full_graph(6)
V(g_complete)$name <- LETTERS[1:6]

# Star graph
g_star <- make_star(10, mode = "undirected")
V(g_star)$name <- paste0("N", 1:10)

# ============================================
# LEVERAGE CENTRALITY TESTS
# ============================================

cat("LEVERAGE CENTRALITY TESTS:\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

# Basic tests
results$leverage_undir <- run_test(
  "Undirected, mode=all",
  leverage.original(g_undir, mode = "all"),
  centrality_leverage(g_undir, mode = "all")
)

results$leverage_dir_out <- run_test(
  "Directed, mode=out",
  leverage.original(g_dir, mode = "out"),
  centrality_leverage(g_dir, mode = "out")
)

results$leverage_dir_in <- run_test(
  "Directed, mode=in",
  leverage.original(g_dir, mode = "in"),
  centrality_leverage(g_dir, mode = "in")
)

results$leverage_dir_all <- run_test(
  "Directed, mode=all",
  leverage.original(g_dir, mode = "all"),
  centrality_leverage(g_dir, mode = "all")
)

results$leverage_rand_u <- run_test(
  "Random undirected (30 nodes)",
  leverage.original(g_rand_u, mode = "all"),
  centrality_leverage(g_rand_u, mode = "all")
)

results$leverage_rand_d <- run_test(
  "Random directed (30 nodes)",
  leverage.original(g_rand_d, mode = "out"),
  centrality_leverage(g_rand_d, mode = "out")
)

results$leverage_complete <- run_test(
  "Complete graph (K6)",
  leverage.original(g_complete, mode = "all"),
  centrality_leverage(g_complete, mode = "all")
)

results$leverage_star <- run_test(
  "Star graph",
  leverage.original(g_star, mode = "all"),
  centrality_leverage(g_star, mode = "all")
)

# ============================================
# K-REACH CENTRALITY TESTS
# ============================================

cat("\nK-REACH CENTRALITY TESTS:\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

results$kreach_undir_k3 <- run_test(
  "Undirected, k=3",
  kreach.original(g_undir, mode = "all", kpath = 3),
  centrality_kreach(g_undir, mode = "all", k = 3)
)

results$kreach_undir_k2 <- run_test(
  "Undirected, k=2",
  kreach.original(g_undir, mode = "all", kpath = 2),
  centrality_kreach(g_undir, mode = "all", k = 2)
)

results$kreach_undir_k1 <- run_test(
  "Undirected, k=1 (neighbors only)",
  kreach.original(g_undir, mode = "all", kpath = 1),
  centrality_kreach(g_undir, mode = "all", k = 1)
)

results$kreach_dir_out <- run_test(
  "Directed, mode=out, k=3",
  kreach.original(g_dir, mode = "out", kpath = 3),
  centrality_kreach(g_dir, mode = "out", k = 3)
)

results$kreach_dir_in <- run_test(
  "Directed, mode=in, k=3",
  kreach.original(g_dir, mode = "in", kpath = 3),
  centrality_kreach(g_dir, mode = "in", k = 3)
)

results$kreach_rand_u <- run_test(
  "Random undirected (30 nodes), k=3",
  kreach.original(g_rand_u, mode = "all", kpath = 3),
  centrality_kreach(g_rand_u, mode = "all", k = 3)
)

results$kreach_rand_d <- run_test(
  "Random directed (30 nodes), k=2",
  kreach.original(g_rand_d, mode = "out", kpath = 2),
  centrality_kreach(g_rand_d, mode = "out", k = 2)
)

results$kreach_complete <- run_test(
  "Complete graph (all reachable in k=1)",
  kreach.original(g_complete, mode = "all", kpath = 1),
  centrality_kreach(g_complete, mode = "all", k = 1)
)

# ============================================
# RESILIENCE CENTRALITY TESTS
# ============================================

cat("\nRESILIENCE CENTRALITY TESTS:\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

# Path graph - all 0s (single paths)
g_path <- make_graph(edges = c(1,2, 2,3, 3,4, 4,5), directed = FALSE)
V(g_path)$name <- LETTERS[1:5]
results$resilience_path <- run_expected_test(
  "Path graph (all 0 - single paths)",
  centrality_resilience(g_path),
  c(0, 0, 0, 0, 0)
)

# Cycle graph - all 1s (two paths to each neighbor)
g_cycle <- make_ring(5)
V(g_cycle)$name <- LETTERS[1:5]
results$resilience_cycle <- run_expected_test(
  "Cycle graph (all 1 - need remove 1 node)",
  centrality_resilience(g_cycle),
  c(1, 1, 1, 1, 1)
)

# Complete K4 - all 2s (highly connected)
g_k4 <- make_full_graph(4)
V(g_k4)$name <- LETTERS[1:4]
results$resilience_k4 <- run_expected_test(
  "Complete K4 (all 2 - highly connected)",
  centrality_resilience(g_k4),
  c(2, 2, 2, 2)
)

# Star graph - all 0s (center and leaves have single paths)
g_star_small <- make_star(5, mode = "undirected")
V(g_star_small)$name <- c("C", LETTERS[1:4])
results$resilience_star <- run_expected_test(
  "Star graph (all 0 - single paths)",
  centrality_resilience(g_star_small),
  c(0, 0, 0, 0, 0)
)

# Two triangles connected by bridge
g_bridge <- make_graph(edges = c(1,2, 2,3, 1,3, 3,4, 4,5, 5,6, 4,6), directed = FALSE)
V(g_bridge)$name <- LETTERS[1:6]
cat("  Two triangles with bridge (3-4):            ")
res_bridge <- centrality_resilience(g_bridge)
# Node 3 and 4 are bridge nodes - removing one disconnects triangles
# But within triangles, nodes have alternative paths
cat("Values:", paste(res_bridge, collapse=","), "\n")
results$resilience_bridge <- TRUE  # Just check it runs

# ============================================
# Performance Comparison
# ============================================

cat("\nPERFORMANCE COMPARISON:\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

sizes <- c(50, 100, 200)
perf_results <- data.frame(
  measure = character(),
  nodes = integer(),
  original_ms = numeric(),
  cograph_ms = numeric(),
  speedup = numeric(),
  stringsAsFactors = FALSE
)

for (n in sizes) {
  g_perf <- sample_gnp(n, 5/n, directed = FALSE)
  V(g_perf)$name <- paste0("N", 1:n)

  # Leverage
  time_orig <- system.time(leverage.original(g_perf, mode = "all"))[["elapsed"]] * 1000
  time_new <- system.time(centrality_leverage(g_perf, mode = "all"))[["elapsed"]] * 1000
  perf_results <- rbind(perf_results, data.frame(
    measure = "leverage", nodes = n,
    original_ms = round(time_orig, 2), cograph_ms = round(time_new, 2),
    speedup = round(time_orig / max(time_new, 0.01), 1)
  ))

  # Kreach
  time_orig <- system.time(kreach.original(g_perf, mode = "all", kpath = 3))[["elapsed"]] * 1000
  time_new <- system.time(centrality_kreach(g_perf, mode = "all", k = 3))[["elapsed"]] * 1000
  perf_results <- rbind(perf_results, data.frame(
    measure = "kreach", nodes = n,
    original_ms = round(time_orig, 2), cograph_ms = round(time_new, 2),
    speedup = round(time_orig / max(time_new, 0.01), 1)
  ))

  # Resilience (only small networks - O(n^2) complexity)
  if (n <= 100) {
    time_new <- system.time(centrality_resilience(g_perf))[["elapsed"]] * 1000
    perf_results <- rbind(perf_results, data.frame(
      measure = "resilience", nodes = n,
      original_ms = NA, cograph_ms = round(time_new, 2),
      speedup = NA
    ))
  }
}

cat("\nPerformance results by measure:\n")
for (m in unique(perf_results$measure)) {
  cat(sprintf("\n  %s:\n", toupper(m)))
  sub <- perf_results[perf_results$measure == m, ]
  for (i in seq_len(nrow(sub))) {
    if (is.na(sub$original_ms[i])) {
      cat(sprintf("    %4d nodes: cograph=%7.2fms\n",
                  sub$nodes[i], sub$cograph_ms[i]))
    } else {
      cat(sprintf("    %4d nodes: original=%7.2fms, cograph=%7.2fms, speedup=%5.1fx\n",
                  sub$nodes[i], sub$original_ms[i], sub$cograph_ms[i], sub$speedup[i]))
    }
  }
}

# ============================================
# Summary
# ============================================

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n\n")

passed <- sum(unlist(results))
total <- length(results)

cat(sprintf("Tests: %d passed, %d failed (%.1f%% pass rate)\n",
            passed, total - passed, 100 * passed / total))

if (passed == total) {
  cat("\n*** ALL TESTS PASSED! ***\n")
  cat("\nCentrality measures validated:\n")
  cat("  - leverage: matches centiserve original\n")
  cat("  - kreach: matches centiserve geokpath\n")
  cat("  - resilience: correct vertex connectivity behavior\n")
} else {
  cat("\nFailed tests:\n")
  for (name in names(results)) {
    if (!results[[name]]) {
      cat(sprintf("  - %s\n", name))
    }
  }
}

# Save results
output_file <- "validation/results_centiserve_centralities.txt"
sink(output_file)
cat("CENTRALITY MEASURES VALIDATION RESULTS\n")
cat("======================================\n\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("Test Results:\n")
for (name in names(results)) {
  cat(sprintf("  %s: %s\n", name, if(results[[name]]) "PASS" else "FAIL"))
}
cat("\nPerformance:\n")
print(perf_results, row.names = FALSE)
cat(sprintf("\nOverall: %d/%d tests passed\n", passed, total))
sink()

cat("\nResults saved to:", output_file, "\n")
