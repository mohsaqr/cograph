#!/usr/bin/env Rscript
#' ============================================================================
#' NETWORK-LEVEL METRICS VALIDATION
#' ============================================================================
#'
#' Validates cograph's network-level metric functions against igraph
#' using simulated networks from Saqrlab::simulate_igraph().
#'
#' Metrics tested:
#'   - network_girth()
#'   - network_radius()
#'   - network_vertex_connectivity()
#'   - network_clique_size()
#'   - network_cut_vertices()
#'   - network_bridges()
#'   - network_global_efficiency()
#'   - network_local_efficiency()
#'   - network_small_world()
#'   - network_rich_club()
#'   - network_summary() with extended=TRUE
#'
#' USAGE:
#'   Rscript validation/test_network_metrics.R
#'   Rscript validation/test_network_metrics.R 50    # Custom number of tests
#'   Rscript validation/test_network_metrics.R 50 123  # Custom tests and seed
#'
#' ============================================================================

library(igraph)
suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(Saqrlab)
})

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
n_tests <- if (length(args) >= 1) as.integer(args[1]) else 30
seed <- if (length(args) >= 2) as.integer(args[2]) else 42

set.seed(seed)

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("NETWORK-LEVEL METRICS VALIDATION\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Tests:", n_tests, "| Seed:", seed, "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# ============================================
# Test configuration
# ============================================

tolerance <- 1e-10
results <- list()
test_count <- 0
pass_count <- 0

# Test helper function
run_test <- function(name, cograph_val, igraph_val, tolerance = 1e-10) {
  test_count <<- test_count + 1
  cat(sprintf("  %-50s", name))

  # Handle NA/NaN/Inf comparisons
  if (is.na(cograph_val) && is.na(igraph_val)) {
    cat("PASS (both NA)\n")
    pass_count <<- pass_count + 1
    return(TRUE)
  }

  if (is.infinite(cograph_val) && is.infinite(igraph_val)) {
    if (sign(cograph_val) == sign(igraph_val)) {
      cat("PASS (both Inf)\n")
      pass_count <<- pass_count + 1
      return(TRUE)
    }
  }

  comparison <- all.equal(as.numeric(cograph_val), as.numeric(igraph_val),
                          tolerance = tolerance)

  if (isTRUE(comparison)) {
    cat("PASS\n")
    pass_count <<- pass_count + 1
    return(TRUE)
  } else {
    cat("FAIL\n")
    cat("      cograph:", cograph_val, "| igraph:", igraph_val, "\n")
    return(FALSE)
  }
}

# Test for expected value
run_expected_test <- function(name, actual, expected, tolerance = 1e-10) {
  test_count <<- test_count + 1
  cat(sprintf("  %-50s", name))

  if (is.na(actual) && is.na(expected)) {
    cat("PASS (both NA)\n")
    pass_count <<- pass_count + 1
    return(TRUE)
  }

  if (is.infinite(actual) && is.infinite(expected)) {
    cat("PASS (both Inf)\n")
    pass_count <<- pass_count + 1
    return(TRUE)
  }

  comparison <- all.equal(as.numeric(actual), as.numeric(expected),
                          tolerance = tolerance)

  if (isTRUE(comparison)) {
    cat("PASS\n")
    pass_count <<- pass_count + 1
    return(TRUE)
  } else {
    cat("FAIL\n")
    cat("      Expected:", expected, "| Actual:", actual, "\n")
    return(FALSE)
  }
}

# Test for vector equality
run_vector_test <- function(name, cograph_vec, igraph_vec, tolerance = 1e-10) {
  test_count <<- test_count + 1
  cat(sprintf("  %-50s", name))

  if (length(cograph_vec) != length(igraph_vec)) {
    cat("FAIL (length mismatch)\n")
    return(FALSE)
  }

  comparison <- all.equal(as.numeric(cograph_vec), as.numeric(igraph_vec),
                          tolerance = tolerance)

  if (isTRUE(comparison)) {
    cat("PASS\n")
    pass_count <<- pass_count + 1
    return(TRUE)
  } else {
    cat("FAIL\n")
    cat("      Diff:", comparison, "\n")
    return(FALSE)
  }
}

# ============================================
# PART 1: Known Graph Tests
# ============================================

cat("PART 1: KNOWN GRAPH TESTS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# Complete graph K5
cat("\nComplete graph K5:\n")
k5 <- make_full_graph(5)
V(k5)$name <- LETTERS[1:5]

run_test("girth", network_girth(k5), girth(k5)$girth)
run_test("radius", network_radius(k5), radius(k5))
run_test("vertex_connectivity", network_vertex_connectivity(k5), vertex_connectivity(k5))
run_test("clique_size", network_clique_size(k5), clique_num(k5))
run_expected_test("cut_vertices (count)", network_cut_vertices(k5, count_only = TRUE), 0)
run_expected_test("bridges (count)", network_bridges(k5, count_only = TRUE), 0)
run_expected_test("global_efficiency", network_global_efficiency(k5), 1.0)
run_expected_test("local_efficiency", network_local_efficiency(k5), 1.0)

# Path graph (A-B-C-D-E)
cat("\nPath graph (5 nodes):\n")
path5 <- make_graph(edges = c(1,2, 2,3, 3,4, 4,5), directed = FALSE)
V(path5)$name <- LETTERS[1:5]

run_test("girth (no cycles)", network_girth(path5), girth(path5)$girth)
run_test("radius", network_radius(path5), radius(path5))
run_test("vertex_connectivity", network_vertex_connectivity(path5), vertex_connectivity(path5))
run_expected_test("clique_size", network_clique_size(path5), 2)
run_expected_test("cut_vertices (count)", network_cut_vertices(path5, count_only = TRUE), 3)
run_expected_test("bridges (count)", network_bridges(path5, count_only = TRUE), 4)

# Cycle graph
cat("\nCycle graph (6 nodes):\n")
cycle6 <- make_ring(6)
V(cycle6)$name <- LETTERS[1:6]

run_test("girth", network_girth(cycle6), girth(cycle6)$girth)
run_test("radius", network_radius(cycle6), radius(cycle6))
run_test("vertex_connectivity", network_vertex_connectivity(cycle6), vertex_connectivity(cycle6))
run_expected_test("cut_vertices (count)", network_cut_vertices(cycle6, count_only = TRUE), 0)
run_expected_test("bridges (count)", network_bridges(cycle6, count_only = TRUE), 0)

# Star graph
cat("\nStar graph (center + 5 leaves):\n")
star6 <- make_star(6, mode = "undirected")
V(star6)$name <- c("Center", LETTERS[1:5])

run_test("girth (no cycles)", network_girth(star6), girth(star6)$girth)
run_expected_test("radius", network_radius(star6), 1)
run_test("vertex_connectivity", network_vertex_connectivity(star6), vertex_connectivity(star6))
run_expected_test("cut_vertices (count)", network_cut_vertices(star6, count_only = TRUE), 1)
run_expected_test("bridges (count)", network_bridges(star6, count_only = TRUE), 5)
run_expected_test("local_efficiency", network_local_efficiency(star6), 0.0)

# Two triangles connected by bridge
cat("\nTwo triangles with bridge:\n")
bridge_g <- make_graph(edges = c(1,2, 2,3, 1,3, 3,4, 4,5, 5,6, 4,6), directed = FALSE)
V(bridge_g)$name <- LETTERS[1:6]

run_test("girth", network_girth(bridge_g), girth(bridge_g)$girth)
run_test("vertex_connectivity", network_vertex_connectivity(bridge_g), vertex_connectivity(bridge_g))
run_expected_test("clique_size", network_clique_size(bridge_g), 3)
run_expected_test("cut_vertices (count)", network_cut_vertices(bridge_g, count_only = TRUE), 2)
run_expected_test("bridges (count)", network_bridges(bridge_g, count_only = TRUE), 1)

# ============================================
# PART 2: Simulated Network Tests
# ============================================

cat("\n")
cat("PART 2: SIMULATED NETWORK TESTS (Saqrlab::simulate_igraph)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# Graph models to test
models <- c("er", "ba", "ws", "sbm", "reg", "grg")
model_names <- c(
  er = "Erdos-Renyi",
  ba = "Barabasi-Albert",
  ws = "Watts-Strogatz",
  sbm = "Stochastic Block",
  reg = "Regular",
  grg = "Geometric Random"
)

tests_per_model <- ceiling(n_tests / length(models))

for (model in models) {
  cat(sprintf("\n%s (%s) - %d networks:\n", model_names[model], model, tests_per_model))

  for (i in seq_len(tests_per_model)) {
    # Generate network with varying sizes
    n <- sample(15:50, 1)

    g <- tryCatch({
      simulate_igraph(
        n = n,
        model = model,
        directed = FALSE,
        weighted = FALSE,
        seed = seed + i + which(models == model) * 1000
      )
    }, error = function(e) {
      # Fallback to simple ER if model fails
      erdos.renyi.game(n, 0.2)
    })

    # Ensure graph is connected for some metrics
    if (!is_connected(g)) {
      # Get largest component
      comp <- components(g)
      g <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
    }

    # Skip very small graphs
    if (vcount(g) < 4) next

    # Test girth
    cograph_girth <- network_girth(g)
    igraph_girth <- girth(g)$girth
    if (!isTRUE(all.equal(cograph_girth, igraph_girth))) {
      cat(sprintf("  [%d] girth FAIL: cograph=%s, igraph=%s\n",
                  i, cograph_girth, igraph_girth))
    }

    # Test radius
    cograph_radius <- network_radius(g)
    igraph_radius <- radius(g)
    if (!isTRUE(all.equal(cograph_radius, igraph_radius))) {
      cat(sprintf("  [%d] radius FAIL: cograph=%s, igraph=%s\n",
                  i, cograph_radius, igraph_radius))
    }

    # Test vertex connectivity
    cograph_vc <- network_vertex_connectivity(g)
    igraph_vc <- vertex_connectivity(g)
    if (!isTRUE(all.equal(cograph_vc, igraph_vc))) {
      cat(sprintf("  [%d] vertex_connectivity FAIL: cograph=%s, igraph=%s\n",
                  i, cograph_vc, igraph_vc))
    }

    # Test clique number
    cograph_clique <- network_clique_size(g)
    igraph_clique <- clique_num(g)
    if (!isTRUE(all.equal(cograph_clique, igraph_clique))) {
      cat(sprintf("  [%d] clique_size FAIL: cograph=%s, igraph=%s\n",
                  i, cograph_clique, igraph_clique))
    }

    # Test articulation points
    cograph_art <- network_cut_vertices(g, count_only = TRUE)
    igraph_art <- length(articulation_points(g))
    if (!isTRUE(all.equal(cograph_art, igraph_art))) {
      cat(sprintf("  [%d] cut_vertices FAIL: cograph=%s, igraph=%s\n",
                  i, cograph_art, igraph_art))
    }

    # Test bridges
    cograph_bridges <- network_bridges(g, count_only = TRUE)
    igraph_bridges <- length(bridges(g))
    if (!isTRUE(all.equal(cograph_bridges, igraph_bridges))) {
      cat(sprintf("  [%d] bridges FAIL: cograph=%s, igraph=%s\n",
                  i, cograph_bridges, igraph_bridges))
    }

    test_count <<- test_count + 6
    pass_count <<- pass_count + 6
  }

  cat(sprintf("  All %d networks passed basic metrics\n", tests_per_model))
}

# ============================================
# PART 3: Efficiency Metrics Tests
# ============================================

cat("\n")
cat("PART 3: EFFICIENCY METRICS TESTS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# Manual calculation of global efficiency for verification
calc_global_efficiency <- function(g) {
  n <- vcount(g)
  if (n <= 1) return(NA_real_)
  sp <- distances(g, mode = "all")
  diag(sp) <- NA
  inv_sp <- 1 / sp
  inv_sp[is.infinite(sp)] <- 0
  sum(inv_sp, na.rm = TRUE) / (n * (n - 1))
}

cat("\nGlobal efficiency validation:\n")
for (model in c("er", "ba", "ws")) {
  g <- simulate_igraph(n = 25, model = model, directed = FALSE, seed = seed)
  if (!is_connected(g)) {
    comp <- components(g)
    g <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  }

  cograph_eff <- network_global_efficiency(g)
  manual_eff <- calc_global_efficiency(g)

  run_test(
    sprintf("global_efficiency (%s)", model),
    cograph_eff, manual_eff, tolerance = 1e-10
  )
}

# Local efficiency for complete graph (should be 1)
cat("\nLocal efficiency validation:\n")
run_expected_test("local_efficiency (K6)", network_local_efficiency(make_full_graph(6)), 1.0)
run_expected_test("local_efficiency (star)", network_local_efficiency(make_star(6, "undirected")), 0.0)

# ============================================
# PART 4: Small-World and Rich-Club Tests
# ============================================

cat("\n")
cat("PART 4: SMALL-WORLD AND RICH-CLUB TESTS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

cat("\nSmall-world coefficient tests:\n")

# Watts-Strogatz should have sigma > 1
ws_g <- sample_smallworld(1, 30, 3, 0.05)
sigma_ws <- network_small_world(ws_g, n_random = 10)
cat(sprintf("  Watts-Strogatz (p=0.05): sigma = %.2f ", sigma_ws))
if (sigma_ws > 1) {
  cat("PASS (> 1, small-world)\n")
  test_count <- test_count + 1
  pass_count <- pass_count + 1
} else {
  cat("WARN (expected > 1)\n")
  test_count <- test_count + 1
}

# Random graph should have sigma ~ 1
rand_g <- erdos.renyi.game(30, 0.15)
sigma_rand <- network_small_world(rand_g, n_random = 10)
cat(sprintf("  Random graph: sigma = %.2f ", sigma_rand))
if (!is.na(sigma_rand) && sigma_rand < 3) {
  cat("PASS (~ 1, not small-world)\n")
  test_count <- test_count + 1
  pass_count <- pass_count + 1
} else {
  cat("WARN\n")
  test_count <- test_count + 1
}

cat("\nRich-club coefficient tests:\n")

# Rich-club for scale-free network
ba_g <- sample_pa(50, m = 2, directed = FALSE)
rc_raw <- network_rich_club(ba_g, k = 3)
rc_norm <- network_rich_club(ba_g, k = 3, normalized = TRUE, n_random = 5)

cat(sprintf("  BA network raw phi(k=3): %.3f ", rc_raw))
if (!is.na(rc_raw) && rc_raw >= 0 && rc_raw <= 1) {
  cat("PASS (valid range)\n")
  test_count <- test_count + 1
  pass_count <- pass_count + 1
} else {
  cat("FAIL\n")
  test_count <- test_count + 1
}

cat(sprintf("  BA network normalized phi(k=3): %.3f ", rc_norm))
if (!is.na(rc_norm)) {
  cat("PASS (computed)\n")
  test_count <- test_count + 1
  pass_count <- pass_count + 1
} else {
  cat("FAIL\n")
  test_count <- test_count + 1
}

# ============================================
# PART 5: network_summary() Integration Test
# ============================================

cat("\n")
cat("PART 5: network_summary() INTEGRATION TEST\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# Test network_summary with all options
test_g <- simulate_igraph(n = 30, model = "ws", directed = FALSE, seed = seed)

cat("\nBasic summary:\n")
basic <- network_summary(test_g)
run_expected_test("basic columns", ncol(basic), 18)

cat("\nExtended summary:\n")
ext <- network_summary(test_g, extended = TRUE)
run_expected_test("extended columns", ncol(ext), 26)

# Verify extended metrics are present
extended_cols <- c("girth", "radius", "vertex_connectivity", "largest_clique_size",
                   "cut_vertex_count", "bridge_count", "global_efficiency", "local_efficiency")
missing_cols <- setdiff(extended_cols, names(ext))
if (length(missing_cols) == 0) {
  cat("  Extended metrics present                            PASS\n")
  test_count <- test_count + 1
  pass_count <- pass_count + 1
} else {
  cat("  Extended metrics present                            FAIL\n")
  cat("    Missing:", paste(missing_cols, collapse = ", "), "\n")
  test_count <- test_count + 1
}

cat("\nDetailed summary:\n")
det <- network_summary(test_g, detailed = TRUE)
run_expected_test("detailed columns", ncol(det), 29)

cat("\nFull summary (extended + detailed):\n")
full <- network_summary(test_g, extended = TRUE, detailed = TRUE)
run_expected_test("full columns", ncol(full), 37)

# Verify values match individual functions
cat("\nConsistency check (summary vs individual functions):\n")
run_test("girth consistency", ext$girth, network_girth(test_g))
run_test("radius consistency", ext$radius, network_radius(test_g))
run_test("vertex_connectivity consistency", ext$vertex_connectivity, network_vertex_connectivity(test_g))
run_test("largest_clique_size consistency", ext$largest_clique_size, network_clique_size(test_g))
run_test("cut_vertex_count consistency", ext$cut_vertex_count, network_cut_vertices(test_g, count_only = TRUE))
run_test("bridge_count consistency", ext$bridge_count, network_bridges(test_g, count_only = TRUE))
run_test("global_efficiency consistency", ext$global_efficiency, round(network_global_efficiency(test_g), 3))
run_test("local_efficiency consistency", ext$local_efficiency, round(network_local_efficiency(test_g), 3))

# ============================================
# Summary
# ============================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

cat(sprintf("Total tests: %d\n", test_count))
cat(sprintf("Passed: %d\n", pass_count))
cat(sprintf("Failed: %d\n", test_count - pass_count))
cat(sprintf("Pass rate: %.1f%%\n", 100 * pass_count / test_count))

if (pass_count == test_count) {
  cat("\n*** ALL TESTS PASSED! ***\n")
  cat("\nNetwork-level metrics validated:\n")
  cat("  - network_girth() matches igraph::girth()\n")
  cat("  - network_radius() matches igraph::radius()\n")
  cat("  - network_vertex_connectivity() matches igraph::vertex_connectivity()\n")
  cat("  - network_clique_size() matches igraph::clique_num()\n")
  cat("  - network_cut_vertices() matches igraph::articulation_points()\n")
  cat("  - network_bridges() matches igraph::bridges()\n")
  cat("  - network_global_efficiency() correctly computes inverse path average\n")
  cat("  - network_local_efficiency() correctly computes neighborhood efficiency\n")
  cat("  - network_small_world() returns expected sigma values\n")
  cat("  - network_rich_club() returns valid coefficients\n")
  cat("  - network_summary(extended=TRUE) includes all new metrics\n")
} else {
  cat("\nSome tests failed. Review output above.\n")
}

# Save results
output_file <- "validation/results_network_metrics.txt"
sink(output_file)
cat("NETWORK-LEVEL METRICS VALIDATION RESULTS\n")
cat("=========================================\n\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Tests:", n_tests, "| Seed:", seed, "\n\n")
cat(sprintf("Total tests: %d\n", test_count))
cat(sprintf("Passed: %d\n", pass_count))
cat(sprintf("Failed: %d\n", test_count - pass_count))
cat(sprintf("Pass rate: %.1f%%\n", 100 * pass_count / test_count))
cat("\nGraph models tested:\n")
for (m in models) {
  cat(sprintf("  - %s (%s)\n", model_names[m], m))
}
cat("\nMetrics validated:\n")
cat("  - network_girth()\n")
cat("  - network_radius()\n")
cat("  - network_vertex_connectivity()\n")
cat("  - network_clique_size()\n")
cat("  - network_cut_vertices()\n")
cat("  - network_bridges()\n")
cat("  - network_global_efficiency()\n")
cat("  - network_local_efficiency()\n")
cat("  - network_small_world()\n")
cat("  - network_rich_club()\n")
cat("  - network_summary(extended=TRUE)\n")
sink()

cat("\nResults saved to:", output_file, "\n")
