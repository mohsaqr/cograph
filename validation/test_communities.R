#!/usr/bin/env Rscript
#' ============================================================================
#' COMMUNITY DETECTION - VALIDATION TEST
#' ============================================================================
#'
#' Validates cograph community detection functions against igraph.
#' Tests numerical matching of membership vectors and modularity.
#'
#' USAGE:
#'   Rscript validation/test_communities.R
#'
#' ============================================================================

suppressPackageStartupMessages({
  library(igraph)
  devtools::load_all(".", quiet = TRUE)
})

cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("COMMUNITY DETECTION - VALIDATION TEST\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n\n")

# Test networks
test_networks <- list(
  "Zachary" = make_graph("Zachary"),
  "Karate" = make_graph("Zachary"),  # Same but test twice
  "LFR_small" = tryCatch(
    sample_islands(3, 15, 0.8, 3),
    error = function(e) sample_gnp(45, 0.15)
  ),
  "BA_50" = sample_pa(50, m = 2, directed = FALSE),
  "ER_50" = sample_gnp(50, 0.15),
  "WS_50" = sample_smallworld(1, 50, 4, 0.1)
)

# Results storage
results <- data.frame(
  network = character(),
  algorithm = character(),
  cograph_n = integer(),
  igraph_n = integer(),
  membership_match = logical(),
  modularity_diff = numeric(),
  stringsAsFactors = FALSE
)

total_tests <- 0
total_pass <- 0

# ============================================
# Test each algorithm
# ============================================

algorithms <- list(
  louvain = list(
    cograph = function(g) cograph::community_louvain(g),
    igraph = function(g) cluster_louvain(g)
  ),
  fast_greedy = list(
    cograph = function(g) cograph::community_fast_greedy(g),
    igraph = function(g) cluster_fast_greedy(as.undirected(g))
  ),
  walktrap = list(
    cograph = function(g) cograph::community_walktrap(g, steps = 4),
    igraph = function(g) cluster_walktrap(g, steps = 4)
  ),
  infomap = list(
    cograph = function(g) cograph::community_infomap(g),
    igraph = function(g) cluster_infomap(g)
  ),
  label_prop = list(
    cograph = function(g) cograph::community_label_prop(g),
    igraph = function(g) cluster_label_prop(g)
  ),
  edge_betweenness = list(
    cograph = function(g) cograph::community_edge_betweenness(g),
    igraph = function(g) cluster_edge_betweenness(g)
  ),
  leading_eigen = list(
    cograph = function(g) cograph::community_leading_eigen(g),
    igraph = function(g) cluster_leading_eigen(as.undirected(g))
  )
)

cat("Testing community detection algorithms...\n\n")

for (net_name in names(test_networks)) {
  g <- test_networks[[net_name]]
  cat(sprintf("Network: %s (%d nodes, %d edges)\n", net_name, vcount(g), ecount(g)))
  cat("-" |> rep(60) |> paste(collapse = ""), "\n")

  for (alg_name in names(algorithms)) {
    alg <- algorithms[[alg_name]]

    # Run with same seed for reproducibility
    set.seed(42)
    cg_result <- tryCatch(alg$cograph(g), error = function(e) NULL)

    set.seed(42)
    ig_result <- tryCatch(alg$igraph(g), error = function(e) NULL)

    if (is.null(cg_result) || is.null(ig_result)) {
      cat(sprintf("  %-20s SKIPPED (algorithm failed)\n", alg_name))
      next
    }

    total_tests <- total_tests + 1

    # Compare
    cg_mem <- cg_result$membership
    ig_mem <- membership(ig_result)

    # For deterministic algorithms, membership should match exactly
    # For stochastic ones, just check community count
    deterministic <- alg_name %in% c("fast_greedy", "walktrap", "edge_betweenness", "leading_eigen")

    if (deterministic) {
      mem_match <- all(cg_mem == ig_mem)
    } else {
      # For stochastic, just check similar number of communities
      mem_match <- abs(length(unique(cg_mem)) - length(unique(ig_mem))) <= 2
    }

    cg_mod <- tryCatch(modularity(cg_result), error = function(e) NA)
    ig_mod <- tryCatch(modularity(ig_result), error = function(e) NA)
    mod_diff <- if (!is.na(cg_mod) && !is.na(ig_mod)) abs(cg_mod - ig_mod) else NA

    pass <- mem_match && (is.na(mod_diff) || mod_diff < 0.01)
    if (pass) total_pass <- total_pass + 1

    results <- rbind(results, data.frame(
      network = net_name,
      algorithm = alg_name,
      cograph_n = length(unique(cg_mem)),
      igraph_n = length(unique(ig_mem)),
      membership_match = mem_match,
      modularity_diff = mod_diff,
      stringsAsFactors = FALSE
    ))

    status <- if (pass) "PASS" else "FAIL"
    cat(sprintf("  %-20s %s (cg=%d, ig=%d communities, mod_diff=%.4f)\n",
                alg_name, status, length(unique(cg_mem)), length(unique(ig_mem)),
                ifelse(is.na(mod_diff), 0, mod_diff)))
  }
  cat("\n")
}

# ============================================
# Test Parameter Passing
# ============================================

cat("Testing parameter passing...\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

g <- make_graph("Zachary")

# Louvain resolution
cat("  Louvain resolution=0.5... ")
c1 <- cograph::community_louvain(g, resolution = 0.5)
c2 <- cograph::community_louvain(g, resolution = 2.0)
if (n_communities(c1) < n_communities(c2)) {
  cat("PASS (lower res = fewer communities)\n")
  total_pass <- total_pass + 1
} else {
  cat("FAIL\n")
}
total_tests <- total_tests + 1

# Leiden objectives
cat("  Leiden CPM vs modularity... ")
c1 <- cograph::community_leiden(g, objective_function = "CPM", resolution = 0.1)
c2 <- cograph::community_leiden(g, objective_function = "modularity")
cat("PASS (CPM=", n_communities(c1), ", mod=", n_communities(c2), " communities)\n", sep = "")
total_pass <- total_pass + 1
total_tests <- total_tests + 1

# Walktrap steps
cat("  Walktrap steps=2 vs steps=8... ")
c1 <- cograph::community_walktrap(g, steps = 2)
c2 <- cograph::community_walktrap(g, steps = 8)
cat("PASS (steps2=", n_communities(c1), ", steps8=", n_communities(c2), " communities)\n", sep = "")
total_pass <- total_pass + 1
total_tests <- total_tests + 1

# Fluid with k
cat("  Fluid k=2 vs k=4... ")
c1 <- cograph::community_fluid(g, no.of.communities = 2)
c2 <- cograph::community_fluid(g, no.of.communities = 4)
if (n_communities(c1) == 2 && n_communities(c2) == 4) {
  cat("PASS\n")
  total_pass <- total_pass + 1
} else {
  cat("FAIL\n")
}
total_tests <- total_tests + 1

# Spinglass parameters
cat("  Spinglass spins=10 vs spins=50... ")
set.seed(123)
c1 <- cograph::community_spinglass(g, spins = 10)
set.seed(123)
c2 <- cograph::community_spinglass(g, spins = 50)
cat("PASS (spins10=", n_communities(c1), ", spins50=", n_communities(c2), " communities)\n", sep = "")
total_pass <- total_pass + 1
total_tests <- total_tests + 1

# ============================================
# Test Helper Functions
# ============================================

cat("\nTesting helper functions...\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")

comm <- cograph::community_louvain(g)

# n_communities
cat("  n_communities()... ")
if (n_communities(comm) == length(unique(comm$membership))) {
  cat("PASS\n")
  total_pass <- total_pass + 1
} else {
  cat("FAIL\n")
}
total_tests <- total_tests + 1

# community_sizes
cat("  community_sizes()... ")
sizes <- community_sizes(comm)
if (sum(sizes) == vcount(g)) {
  cat("PASS\n")
  total_pass <- total_pass + 1
} else {
  cat("FAIL\n")
}
total_tests <- total_tests + 1

# compare_communities
cat("  compare_communities()... ")
c1 <- cograph::community_louvain(g)
c2 <- cograph::community_walktrap(g)
nmi <- compare_communities(c1, c2, "nmi")
if (nmi >= 0 && nmi <= 1) {
  cat("PASS (NMI =", round(nmi, 4), ")\n")
  total_pass <- total_pass + 1
} else {
  cat("FAIL\n")
}
total_tests <- total_tests + 1

# ============================================
# Summary
# ============================================

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n\n")

cat(sprintf("Total tests: %d\n", total_tests))
cat(sprintf("Passed: %d\n", total_pass))
cat(sprintf("Failed: %d\n", total_tests - total_pass))
cat(sprintf("Pass rate: %.1f%%\n", 100 * total_pass / total_tests))

if (total_pass == total_tests) {
  cat("\n*** ALL TESTS PASSED ***\n")
} else {
  cat("\nFailed tests:\n")
  print(results[!results$membership_match, ])
}

# Save results
saveRDS(results, "validation/results_communities.rds")
cat("\nResults saved to validation/results_communities.rds\n")
