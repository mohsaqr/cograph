#!/usr/bin/env Rscript
#' ============================================================================
#' DIFFUSION CENTRALITY VALIDATION
#' ============================================================================
#'
#' Validates that cograph's diffusion centrality produces identical results
#' to the original centiserve implementation, but much faster.
#'
#' USAGE:
#'   Rscript validation/test_diffusion_centrality.R
#'
#' ============================================================================

library(igraph)
suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("DIFFUSION CENTRALITY VALIDATION\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n\n")

# Original function from centiserve package
diffusion.degree.original <- function(graph, vids = V(graph), mode = c("all", "out", "in"),
                                       loops = TRUE, lambda = 1) {
  d <- degree(graph, mode = mode[1], loops = as.logical(loops)) * as.numeric(lambda)
  res <- numeric()
  for (v in V(graph)[vids]) {
    n <- neighborhood(graph, 1, nodes = v, mode = mode[1])
    sm <- 0
    for (vv in n[[1]]) {
      sm <- sum(sm + d[vv])
    }
    res <- append(res, sm)
  }
  if (is_named(graph)) {
    names(res) <- V(graph)$name[vids]
  }
  res
}

# Test configuration
set.seed(42)
tolerance <- 1e-10
results <- list()

# Test function
run_test <- function(name, g, mode, lambda = 1) {
  cat(sprintf("  %-40s", name))

  orig <- suppressWarnings(diffusion.degree.original(g, mode = mode, lambda = lambda))
  new <- centrality_diffusion(g, mode = mode, lambda = lambda)

  if (all.equal(as.numeric(orig), as.numeric(new), tolerance = tolerance) == TRUE) {
    cat("PASS\n")
    return(TRUE)
  } else {
    cat("FAIL\n")
    cat("    Original:", paste(head(orig, 5), collapse = ", "), "...\n")
    cat("    Cograph: ", paste(head(new, 5), collapse = ", "), "...\n")
    return(FALSE)
  }
}

# ============================================
# Test Suite
# ============================================

cat("Basic Tests:\n")

# Simple undirected
adj <- matrix(c(0,1,1,0,0, 1,0,1,1,0, 1,1,0,1,1, 0,1,1,0,1, 0,0,1,1,0), 5, 5, byrow=TRUE)
rownames(adj) <- colnames(adj) <- LETTERS[1:5]
g_undir <- graph_from_adjacency_matrix(adj, mode = "undirected")

results$undir_all <- run_test("Undirected, mode=all", g_undir, "all")
results$undir_lambda <- run_test("Undirected, lambda=2.5", g_undir, "all", lambda = 2.5)

# Directed
g_dir <- graph_from_adjacency_matrix(adj, mode = "directed")
V(g_dir)$name <- LETTERS[1:5]

results$dir_out <- run_test("Directed, mode=out", g_dir, "out")
results$dir_in <- run_test("Directed, mode=in", g_dir, "in")
results$dir_all <- run_test("Directed, mode=all", g_dir, "all")

cat("\nRandom Network Tests:\n")

# Random undirected
g_rand_u <- sample_gnp(30, 0.2, directed = FALSE)
V(g_rand_u)$name <- paste0("N", 1:30)
results$rand_undir <- run_test("Random undirected (30 nodes)", g_rand_u, "all")

# Random directed
g_rand_d <- sample_gnp(30, 0.15, directed = TRUE)
V(g_rand_d)$name <- paste0("N", 1:30)
results$rand_dir_out <- run_test("Random directed, mode=out", g_rand_d, "out")
results$rand_dir_in <- run_test("Random directed, mode=in", g_rand_d, "in")
results$rand_dir_all <- run_test("Random directed, mode=all", g_rand_d, "all")

# Various lambda values
cat("\nLambda Parameter Tests:\n")
results$lambda_05 <- run_test("Lambda = 0.5", g_undir, "all", lambda = 0.5)
results$lambda_1 <- run_test("Lambda = 1.0 (default)", g_undir, "all", lambda = 1.0)
results$lambda_3 <- run_test("Lambda = 3.0", g_undir, "all", lambda = 3.0)

# Edge cases
cat("\nEdge Cases:\n")

# Single node
g_single <- make_empty_graph(1)
V(g_single)$name <- "A"
results$single <- run_test("Single node", g_single, "all")

# Disconnected
g_disc <- make_empty_graph(5) + edges(c(1,2, 3,4))
V(g_disc)$name <- LETTERS[1:5]
results$disconnected <- run_test("Disconnected graph", g_disc, "all")

# Complete graph
g_complete <- make_full_graph(6)
V(g_complete)$name <- LETTERS[1:6]
results$complete <- run_test("Complete graph (K6)", g_complete, "all")

# Star graph
g_star <- make_star(10, mode = "undirected")
V(g_star)$name <- paste0("N", 1:10)
results$star <- run_test("Star graph", g_star, "all")

# ============================================
# Performance Test
# ============================================

cat("\nPerformance Comparison:\n")

sizes <- c(100, 500, 1000)
perf_results <- data.frame(
  nodes = integer(),
  original_ms = numeric(),
  cograph_ms = numeric(),
  speedup = numeric()
)

for (n in sizes) {
  g_perf <- sample_gnp(n, 5/n, directed = FALSE)
  V(g_perf)$name <- paste0("N", 1:n)

  time_orig <- system.time(suppressWarnings(
    diffusion.degree.original(g_perf, mode = "all")
  ))["elapsed"] * 1000

  time_new <- system.time(
    centrality_diffusion(g_perf, mode = "all")
  )["elapsed"] * 1000

  perf_results <- rbind(perf_results, data.frame(
    nodes = n,
    original_ms = round(time_orig, 2),
    cograph_ms = round(time_new, 2),
    speedup = round(time_orig / max(time_new, 0.01), 1)
  ))

  cat(sprintf("  %4d nodes: original=%6.2fms, cograph=%6.2fms, speedup=%5.1fx\n",
              n, time_orig, time_new, time_orig / max(time_new, 0.01)))
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
  cat("\nDiffusion centrality produces identical results to original,\n")
  cat(sprintf("but is approximately %.0fx faster on large networks.\n",
              mean(perf_results$speedup)))
} else {
  cat("\nFailed tests:\n")
  for (name in names(results)) {
    if (!results[[name]]) {
      cat(sprintf("  - %s\n", name))
    }
  }
}

# Save results
output_file <- "validation/results_diffusion_centrality.txt"
sink(output_file)
cat("DIFFUSION CENTRALITY VALIDATION RESULTS\n")
cat("=======================================\n\n")
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
