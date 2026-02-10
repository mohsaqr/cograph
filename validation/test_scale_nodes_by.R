#!/usr/bin/env Rscript
#' ============================================================================
#' SCALE_NODES_BY VALIDATION
#' ============================================================================
#'
#' Tests the scale_nodes_by parameter for splot().
#'
#' USAGE:
#'   Rscript validation/test_scale_nodes_by.R
#'
#' OUTPUT:
#'   Creates test plots in validation/plots/ and a summary report.
#'
#' ============================================================================

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("SCALE_NODES_BY VALIDATION\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n\n")

# Create output directory
plot_dir <- "validation/plots"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# Create test networks
set.seed(42)

# Simple test network
adj_simple <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(adj_simple) <- colnames(adj_simple) <- c("A", "B", "C", "D", "E")

# Larger network using Saqrlab
if (requireNamespace("Saqrlab", quietly = TRUE)) {
  el <- Saqrlab::simulate_edge_list(n_nodes = 15, edge_density = 2.5, seed = 42)
  g_large <- igraph::graph_from_data_frame(el[, c("source", "target")], directed = FALSE)
  igraph::E(g_large)$weight <- el$weight
} else {
  g_large <- NULL
}

# All centrality measures to test
measures <- c("degree", "strength", "betweenness", "closeness", "eigenvector",
              "pagerank", "authority", "hub", "harmonic")

results <- list()

cat("Testing all centrality measures...\n\n")

for (measure in measures) {
  cat(sprintf("  Testing %s... ", measure))

  # Test with simple network
  tryCatch({
    filename <- file.path(plot_dir, sprintf("scale_by_%s.png", measure))
    png(filename, width = 500, height = 500)
    splot(adj_simple, scale_nodes_by = measure,
          title = sprintf("Scaled by %s", measure))
    dev.off()

    # Verify centrality values were calculated
    cent <- centrality(adj_simple, measures = measure)
    results[[measure]] <- list(
      passed = TRUE,
      values = cent[[2]],
      file = filename
    )
    cat("OK\n")
  }, error = function(e) {
    results[[measure]] <<- list(passed = FALSE, error = e$message)
    cat("FAILED:", e$message, "\n")
  })
}

# Test with custom parameters
cat("\nTesting custom parameters...\n\n")

# Custom size range
tryCatch({
  cat("  Custom size range (1, 15)... ")
  filename <- file.path(plot_dir, "scale_by_degree_custom_range.png")
  png(filename, width = 500, height = 500)
  splot(adj_simple, scale_nodes_by = "degree", node_size_range = c(1, 15),
        title = "Degree with custom range")
  dev.off()
  cat("OK\n")
  results[["custom_range"]] <- list(passed = TRUE)
}, error = function(e) {
  results[["custom_range"]] <- list(passed = FALSE, error = e$message)
  cat("FAILED:", e$message, "\n")
})

# PageRank with custom damping
tryCatch({
  cat("  PageRank with damping=0.5... ")
  filename <- file.path(plot_dir, "scale_by_pagerank_damping.png")
  png(filename, width = 500, height = 500)
  splot(adj_simple, scale_nodes_by = list("pagerank", damping = 0.5),
        title = "PageRank damping=0.5")
  dev.off()
  cat("OK\n")
  results[["pagerank_damping"]] <- list(passed = TRUE)
}, error = function(e) {
  results[["pagerank_damping"]] <- list(passed = FALSE, error = e$message)
  cat("FAILED:", e$message, "\n")
})

# Closeness with mode
tryCatch({
  cat("  Closeness with mode='in'... ")
  # Create directed network
  adj_dir <- adj_simple
  adj_dir[lower.tri(adj_dir)] <- 0
  filename <- file.path(plot_dir, "scale_by_closeness_mode.png")
  png(filename, width = 500, height = 500)
  splot(adj_dir, scale_nodes_by = list("closeness", mode = "in"), directed = TRUE,
        title = "Closeness mode=in")
  dev.off()
  cat("OK\n")
  results[["closeness_mode"]] <- list(passed = TRUE)
}, error = function(e) {
  results[["closeness_mode"]] <- list(passed = FALSE, error = e$message)
  cat("FAILED:", e$message, "\n")
})

# Test with larger network if available
if (!is.null(g_large)) {
  tryCatch({
    cat("  Large network (15 nodes)... ")
    filename <- file.path(plot_dir, "scale_by_betweenness_large.png")
    png(filename, width = 600, height = 600)
    splot(g_large, scale_nodes_by = "betweenness",
          title = "Large network - Betweenness",
          edge_duplicates = "sum")
    dev.off()
    cat("OK\n")
    results[["large_network"]] <- list(passed = TRUE)
  }, error = function(e) {
    results[["large_network"]] <- list(passed = FALSE, error = e$message)
    cat("FAILED:", e$message, "\n")
  })
}

# Summary
cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n\n")

passed <- sum(sapply(results, function(x) x$passed))
total <- length(results)

cat(sprintf("Tests: %d passed, %d failed (%.1f%% pass rate)\n",
            passed, total - passed, 100 * passed / total))
cat(sprintf("Plots saved to: %s/\n", plot_dir))

if (passed == total) {
  cat("\n*** ALL TESTS PASSED! ***\n")
} else {
  cat("\nFailed tests:\n")
  for (name in names(results)) {
    if (!results[[name]]$passed) {
      cat(sprintf("  - %s: %s\n", name, results[[name]]$error))
    }
  }
}

# Show example centrality values
cat("\n\nExample: Centrality values for simple network (A-E):\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
for (m in c("degree", "betweenness", "pagerank")) {
  if (!is.null(results[[m]]) && results[[m]]$passed) {
    vals <- round(results[[m]]$values, 3)
    cat(sprintf("%-12s: %s\n", m, paste(vals, collapse = ", ")))
  }
}
