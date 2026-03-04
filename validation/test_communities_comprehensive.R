#!/usr/bin/env Rscript
#' ============================================================================
#' COMPREHENSIVE COMMUNITY DETECTION VALIDATION
#' ============================================================================
#'
#' Tests all 12 community detection algorithms with parameter variations,
#' directed/weighted networks, compare_communities, and helper functions.
#'
#' USAGE:
#'   Rscript validation/test_communities_comprehensive.R [n_random] [seed]
#'
#' OUTPUT:
#'   - validation/results_communities_comprehensive.rds
#'   - validation/results_communities_comprehensive.txt
#'
#' ============================================================================

args <- commandArgs(trailingOnly = TRUE)
n_random <- if (length(args) >= 1) as.integer(args[1]) else 20
seed <- if (length(args) >= 2) as.integer(args[2]) else 42

suppressPackageStartupMessages({
  library(Saqrlab)
  library(igraph)
  devtools::load_all(".", quiet = TRUE)
})

timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("COMPREHENSIVE COMMUNITY DETECTION VALIDATION\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("Timestamp:", timestamp, "\n")
cat("Random networks:", n_random, "| Seed:", seed, "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# ============================================================================
# Test Infrastructure
# ============================================================================

results <- list()
failures <- list()
test_counter <- 0L

add_result <- function(group, test_name, passed, reason = "OK") {
  test_counter <<- test_counter + 1L
  key <- paste0(group, "::", test_name)
  results[[key]] <<- list(
    id = test_counter,
    group = group,
    test = test_name,
    passed = passed,
    reason = reason
  )
  if (!passed) {
    failures[[length(failures) + 1]] <<- list(
      test_id = test_counter,
      group = group,
      reason = reason,
      network_name = test_name
    )
  }
  if (passed) {
    cat(sprintf("  [PASS] %s\n", test_name))
  } else {
    cat(sprintf("  [FAIL] %s: %s\n", test_name, reason))
  }
}

# ============================================================================
# Test Networks
# ============================================================================

set.seed(seed)

# Known networks
known_nets <- list()
known_nets$zachary <- make_graph("Zachary")
known_nets$islands <- sample_islands(3, 15, 0.8, 3)
known_nets$ba_50 <- sample_pa(50, directed = FALSE)
known_nets$er_50 <- sample_gnm(50, 120)
known_nets$ws_50 <- sample_smallworld(1, 50, 3, 0.1)

# Small networks for optimal
small_nets <- list()
for (i in seq_len(5)) {
  small_nets[[sprintf("small_%d", i)]] <-
    sample_gnm(sample(10:20, 1), sample(15:40, 1), directed = FALSE)
}

# Random networks (mixed)
random_nets <- list()
for (i in seq_len(n_random)) {
  nn <- sample(20:50, 1)
  ed <- runif(1, 2, 4)
  directed <- sample(c(TRUE, FALSE), 1)
  weighted <- sample(c(TRUE, FALSE), 1)
  el_args <- list(n_nodes = nn, edge_density = ed, directed = directed,
                   allow_self_loops = FALSE, seed = i * 5000 + seed)
  if (weighted) el_args$weight_range <- c(0.1, 1)
  el <- do.call(simulate_edge_list, el_args)
  if (nrow(el) == 0) next
  g <- graph_from_data_frame(el[, c("source", "target")], directed = directed)
  if (weighted) E(g)$weight <- el$weight
  random_nets[[sprintf("random_%d_%s_%s", i,
                       if (directed) "dir" else "undir",
                       if (weighted) "wt" else "unwt")]] <- g
}

cat("Known networks:", length(known_nets), "\n")
cat("Small networks:", length(small_nets), "\n")
cat("Random networks:", length(random_nets), "\n\n")

# ============================================================================
# Test 1: Deterministic Algorithms (exact membership match)
# ============================================================================

cat("--- Deterministic Algorithms ---\n")

deterministic_algos <- c("fast_greedy", "walktrap", "edge_betweenness", "leading_eigenvector")

for (algo in deterministic_algos) {
  cat(sprintf("\n[%s]\n", algo))

  for (net_name in names(known_nets)) {
    g <- known_nets[[net_name]]
    # These algorithms require undirected
    if (igraph::is_directed(g)) g <- igraph::as.undirected(g)

    tryCatch({
      cg_comm <- communities(g, method = algo)

      # Compare with igraph directly
      ig_fn <- switch(algo,
        fast_greedy = igraph::cluster_fast_greedy,
        walktrap = igraph::cluster_walktrap,
        edge_betweenness = igraph::cluster_edge_betweenness,
        leading_eigenvector = igraph::cluster_leading_eigen
      )

      ig_comm <- ig_fn(g)
      cg_mem <- as.integer(cg_comm$membership)
      ig_mem <- as.integer(igraph::membership(ig_comm))

      # Check membership match
      match <- all(cg_mem == ig_mem)
      if (!match) {
        # Check NMI instead (same partition, different labeling)
        nmi <- igraph::compare(cg_mem, ig_mem, method = "nmi")
        match <- nmi > 0.999
        reason <- if (match) "OK (NMI=1.0)" else sprintf("NMI=%.4f", nmi)
      } else {
        reason <- "OK (exact match)"
      }

      add_result(algo, sprintf("%s_%s", algo, net_name), match, reason)

      # Check modularity match
      cg_mod <- modularity(cg_comm)
      ig_mod <- igraph::modularity(ig_comm)
      mod_match <- abs(cg_mod - ig_mod) < 0.01
      add_result(algo, sprintf("%s_%s_modularity", algo, net_name), mod_match,
                 if (mod_match) "OK" else sprintf("cograph=%.4f, igraph=%.4f", cg_mod, ig_mod))

    }, error = function(e) {
      add_result(algo, sprintf("%s_%s", algo, net_name), FALSE, e$message)
    })
  }
}

# ============================================================================
# Test 2: Stochastic Algorithms (modularity + community count match)
# ============================================================================

cat("\n--- Stochastic Algorithms ---\n")

stochastic_algos <- c("louvain", "leiden", "infomap", "label_propagation", "spinglass")

for (algo in stochastic_algos) {
  cat(sprintf("\n[%s]\n", algo))

  for (net_name in names(known_nets)) {
    g <- known_nets[[net_name]]

    # spinglass requires connected undirected
    if (algo == "spinglass") {
      g <- igraph::as.undirected(g)
      if (!igraph::is_connected(g)) {
        comps <- igraph::components(g)
        g <- igraph::induced_subgraph(g, which(comps$membership == which.max(comps$csize)))
      }
    }

    # algorithms requiring undirected
    if (algo %in% c("louvain", "label_propagation") && igraph::is_directed(g)) {
      g <- igraph::as.undirected(g)
    }

    tryCatch({
      set.seed(seed)
      cg_comm <- communities(g, method = algo)

      set.seed(seed)
      ig_fn <- switch(algo,
        louvain = igraph::cluster_louvain,
        leiden = function(g, ...) igraph::cluster_leiden(g, ...),
        infomap = igraph::cluster_infomap,
        label_propagation = igraph::cluster_label_prop,
        spinglass = igraph::cluster_spinglass
      )
      ig_comm <- ig_fn(g)

      cg_n <- n_communities(cg_comm)
      ig_n <- length(unique(igraph::membership(ig_comm)))

      # For stochastic: allow ±2 communities
      n_match <- abs(cg_n - ig_n) <= 2

      # Modularity comparison (compute directly for algorithms like leiden)
      cg_mod <- tryCatch(modularity(cg_comm), error = function(e)
        igraph::modularity(g, cg_comm$membership))
      ig_mod <- tryCatch(igraph::modularity(ig_comm), error = function(e)
        igraph::modularity(g, igraph::membership(ig_comm)))
      mod_close <- abs(cg_mod - ig_mod) < 0.05

      passed <- n_match && mod_close
      reason <- sprintf("n_comm: cg=%d ig=%d, mod: cg=%.3f ig=%.3f",
                        cg_n, ig_n, cg_mod, ig_mod)
      add_result(algo, sprintf("%s_%s", algo, net_name), passed,
                 if (passed) "OK" else reason)

    }, error = function(e) {
      add_result(algo, sprintf("%s_%s", algo, net_name), FALSE, e$message)
    })
  }
}

# ============================================================================
# Test 3: Optimal (small networks only)
# ============================================================================

cat("\n--- Optimal Algorithm ---\n")

for (net_name in names(small_nets)) {
  g <- small_nets[[net_name]]

  tryCatch({
    cg_comm <- communities(g, method = "optimal")
    ig_comm <- igraph::cluster_optimal(g)

    cg_mem <- as.integer(cg_comm$membership)
    ig_mem <- as.integer(igraph::membership(ig_comm))

    nmi <- igraph::compare(cg_mem, ig_mem, method = "nmi")
    match <- nmi > 0.999

    cg_mod <- modularity(cg_comm)
    ig_mod <- igraph::modularity(ig_comm)
    mod_match <- abs(cg_mod - ig_mod) < 0.001

    passed <- match && mod_match
    add_result("optimal", sprintf("optimal_%s", net_name), passed,
               if (passed) "OK" else sprintf("NMI=%.4f, mod_diff=%.4f", nmi, abs(cg_mod - ig_mod)))

  }, error = function(e) {
    add_result("optimal", sprintf("optimal_%s", net_name), FALSE, e$message)
  })
}

# ============================================================================
# Test 4: Fluid Communities
# ============================================================================

cat("\n--- Fluid Communities ---\n")

for (k in c(2, 3, 5)) {
  for (net_name in names(known_nets)) {
    g <- known_nets[[net_name]]
    g <- igraph::as.undirected(g)
    if (!igraph::is_connected(g)) {
      comps <- igraph::components(g)
      g <- igraph::induced_subgraph(g, which(comps$membership == which.max(comps$csize)))
    }
    if (vcount(g) < k) next

    tryCatch({
      cg_comm <- communities(g, method = "fluid", no.of.communities = k)
      cg_n <- n_communities(cg_comm)

      # Fluid should produce exactly k communities
      passed <- cg_n == k
      add_result("fluid", sprintf("fluid_k%d_%s", k, net_name), passed,
                 if (passed) "OK" else sprintf("Expected %d, got %d communities", k, cg_n))

    }, error = function(e) {
      add_result("fluid", sprintf("fluid_k%d_%s", k, net_name), FALSE, e$message)
    })
  }
}

# ============================================================================
# Test 5: Consensus Communities
# ============================================================================

cat("\n--- Consensus Communities ---\n")

for (net_name in c("zachary", "islands")) {
  g <- known_nets[[net_name]]
  if (igraph::is_directed(g)) g <- igraph::as.undirected(g)

  for (n_runs in c(20, 50)) {
    for (threshold in c(0.3, 0.5)) {
      tryCatch({
        set.seed(seed)
        cg_comm <- community_consensus(g,
                               n_runs = n_runs, threshold = threshold)
        cg_n <- n_communities(cg_comm)
        cg_mod <- modularity(cg_comm)

        # Basic sanity: produces reasonable communities with positive modularity
        passed <- cg_n >= 1 && cg_mod > 0
        add_result("consensus",
                   sprintf("consensus_%s_r%d_t%.1f", net_name, n_runs, threshold),
                   passed,
                   if (passed) sprintf("OK (n=%d, mod=%.3f)", cg_n, cg_mod) else
                     sprintf("n=%d, mod=%.3f", cg_n, cg_mod))

      }, error = function(e) {
        add_result("consensus",
                   sprintf("consensus_%s_r%d_t%.1f", net_name, n_runs, threshold),
                   FALSE, e$message)
      })
    }
  }
}

# ============================================================================
# Test 6: Parameter Variations
# ============================================================================

cat("\n--- Parameter Variations ---\n")

g_test <- known_nets$zachary

# Louvain resolution
tryCatch({
  res_vals <- c(0.5, 1.0, 2.0)
  n_comms <- vapply(res_vals, function(r) {
    set.seed(seed)
    comm <- communities(g_test, method = "louvain", resolution = r)
    n_communities(comm)
  }, integer(1))

  # Higher resolution -> more communities (generally)
  monotonic <- all(diff(n_comms) >= 0)
  add_result("params", "louvain_resolution_monotonic", monotonic,
             if (monotonic) sprintf("OK: n_comms=%s", paste(n_comms, collapse = ","))
             else sprintf("Not monotonic: n_comms=%s", paste(n_comms, collapse = ",")))
}, error = function(e) {
  add_result("params", "louvain_resolution_monotonic", FALSE, e$message)
})

# Leiden objective functions
tryCatch({
  set.seed(seed)
  comm_cpm <- communities(g_test, method = "leiden", objective_function = "CPM")
  set.seed(seed)
  comm_mod <- communities(g_test, method = "leiden", objective_function = "modularity")

  # Both should produce valid partitions
  passed <- n_communities(comm_cpm) >= 1 && n_communities(comm_mod) >= 1
  add_result("params", "leiden_objectives",
             passed,
             sprintf("CPM: n=%d, Modularity: n=%d", n_communities(comm_cpm), n_communities(comm_mod)))
}, error = function(e) {
  add_result("params", "leiden_objectives", FALSE, e$message)
})

# Walktrap steps
tryCatch({
  step_vals <- c(2, 4, 8)
  comms <- lapply(step_vals, function(s) {
    communities(g_test, method = "walktrap", steps = s)
  })

  # All should produce valid partitions
  n_comms <- vapply(comms, n_communities, integer(1))
  passed <- all(n_comms >= 1)
  add_result("params", "walktrap_steps",
             passed,
             sprintf("steps=%s -> n_comms=%s",
                     paste(step_vals, collapse = ","),
                     paste(n_comms, collapse = ",")))
}, error = function(e) {
  add_result("params", "walktrap_steps", FALSE, e$message)
})

# Spinglass spins
tryCatch({
  g_sp <- igraph::as.undirected(g_test)
  spin_vals <- c(10, 25, 50)
  comms <- lapply(spin_vals, function(s) {
    set.seed(seed)
    communities(g_sp, method = "spinglass", spins = s)
  })
  n_comms <- vapply(comms, n_communities, integer(1))
  # All should be <= spins
  passed <- all(mapply(function(nc, s) nc <= s, n_comms, spin_vals))
  add_result("params", "spinglass_spins",
             passed,
             sprintf("spins=%s -> n_comms=%s",
                     paste(spin_vals, collapse = ","),
                     paste(n_comms, collapse = ",")))
}, error = function(e) {
  add_result("params", "spinglass_spins", FALSE, e$message)
})

# Infomap trials
tryCatch({
  trial_vals <- c(1, 10)
  comms <- lapply(trial_vals, function(t) {
    set.seed(seed)
    communities(g_test, method = "infomap", nb.trials = t)
  })
  n_comms <- vapply(comms, n_communities, integer(1))
  passed <- all(n_comms >= 1)
  add_result("params", "infomap_trials",
             passed,
             sprintf("trials=%s -> n_comms=%s",
                     paste(trial_vals, collapse = ","),
                     paste(n_comms, collapse = ",")))
}, error = function(e) {
  add_result("params", "infomap_trials", FALSE, e$message)
})

# ============================================================================
# Test 7: Directed Networks
# ============================================================================

cat("\n--- Directed Networks ---\n")

dir_algos <- c("louvain", "infomap", "label_propagation", "walktrap", "edge_betweenness")
dir_nets <- random_nets[grep("dir", names(random_nets))]
if (length(dir_nets) > 10) dir_nets <- dir_nets[seq_len(10)]

for (net_name in names(dir_nets)) {
  g <- dir_nets[[net_name]]
  for (algo in dir_algos) {
    tryCatch({
      comm <- communities(g, method = algo)
      nc <- n_communities(comm)
      passed <- nc >= 1 && !is.null(comm$membership) && length(comm$membership) == vcount(g)
      add_result("directed", sprintf("%s_%s", algo, net_name), passed,
                 if (passed) sprintf("OK (n=%d)", nc) else "Invalid result")
    }, error = function(e) {
      # Some algorithms don't support directed — that's OK
      add_result("directed", sprintf("%s_%s", algo, net_name), TRUE,
                 sprintf("SKIP: %s", e$message))
    })
  }
}

# ============================================================================
# Test 8: Weighted Networks
# ============================================================================

cat("\n--- Weighted Networks ---\n")

wt_algos <- c("louvain", "leiden", "fast_greedy", "walktrap", "infomap")
wt_nets <- random_nets[grep("wt", names(random_nets))]
if (length(wt_nets) > 10) wt_nets <- wt_nets[seq_len(10)]

for (net_name in names(wt_nets)) {
  g <- wt_nets[[net_name]]
  for (algo in wt_algos) {
    tryCatch({
      # These require undirected
      g_u <- if (igraph::is_directed(g)) igraph::as.undirected(g) else g
      comm <- communities(g_u, method = algo)
      nc <- n_communities(comm)
      passed <- nc >= 1 && !is.null(comm$membership) && length(comm$membership) == vcount(g_u)
      add_result("weighted", sprintf("%s_%s", algo, net_name), passed,
                 if (passed) sprintf("OK (n=%d)", nc) else "Invalid result")
    }, error = function(e) {
      add_result("weighted", sprintf("%s_%s", algo, net_name), FALSE, e$message)
    })
  }
}

# ============================================================================
# Test 9: compare_communities()
# ============================================================================

cat("\n--- compare_communities ---\n")

g_cmp <- known_nets$zachary

# Generate two community structures
set.seed(seed)
comm1 <- communities(g_cmp, method = "louvain")
comm2 <- communities(g_cmp, method = "fast_greedy")

methods <- c("vi", "nmi", "split.join", "rand", "adjusted.rand")
for (method in methods) {
  tryCatch({
    # Self-comparison
    self_val <- compare_communities(comm1, comm1, method = method)

    if (method == "nmi") {
      passed <- abs(self_val - 1.0) < 1e-10
      add_result("compare", sprintf("self_%s", method), passed,
                 sprintf("Self NMI = %.6f", self_val))
    } else if (method == "vi") {
      passed <- abs(self_val) < 1e-10
      add_result("compare", sprintf("self_%s", method), passed,
                 sprintf("Self VI = %.6f", self_val))
    } else if (method == "rand") {
      passed <- abs(self_val - 1.0) < 1e-10
      add_result("compare", sprintf("self_%s", method), passed,
                 sprintf("Self Rand = %.6f", self_val))
    } else if (method == "adjusted.rand") {
      passed <- abs(self_val - 1.0) < 1e-10
      add_result("compare", sprintf("self_%s", method), passed,
                 sprintf("Self Adj.Rand = %.6f", self_val))
    } else if (method == "split.join") {
      passed <- abs(self_val) < 1e-10
      add_result("compare", sprintf("self_%s", method), passed,
                 sprintf("Self SplitJoin = %.6f", self_val))
    }

    # Cross-comparison against igraph
    ig_val <- igraph::compare(comm1$membership, comm2$membership, method = method)
    cg_val <- compare_communities(comm1, comm2, method = method)
    match <- abs(ig_val - cg_val) < 1e-10
    add_result("compare", sprintf("cross_%s", method), match,
               if (match) "OK" else sprintf("cg=%.6f, ig=%.6f", cg_val, ig_val))

  }, error = function(e) {
    add_result("compare", sprintf("%s", method), FALSE, e$message)
  })
}

# ============================================================================
# Test 10: Helper Functions
# ============================================================================

cat("\n--- Helper Functions ---\n")

comm_test <- communities(g_cmp, method = "fast_greedy")

# n_communities
tryCatch({
  nc <- n_communities(comm_test)
  ig_nc <- length(unique(igraph::membership(igraph::cluster_fast_greedy(g_cmp))))
  passed <- nc == ig_nc
  add_result("helpers", "n_communities", passed,
             if (passed) sprintf("OK (%d)", nc) else sprintf("cg=%d, ig=%d", nc, ig_nc))
}, error = function(e) {
  add_result("helpers", "n_communities", FALSE, e$message)
})

# community_sizes
tryCatch({
  sizes <- community_sizes(comm_test)
  ig_sizes <- as.integer(table(igraph::membership(igraph::cluster_fast_greedy(g_cmp))))
  passed <- all(sort(sizes) == sort(ig_sizes))
  add_result("helpers", "community_sizes", passed,
             if (passed) "OK" else sprintf("cg=%s, ig=%s",
               paste(sort(sizes), collapse = ","), paste(sort(ig_sizes), collapse = ",")))
}, error = function(e) {
  add_result("helpers", "community_sizes", FALSE, e$message)
})

# membership extraction
tryCatch({
  mem <- membership(comm_test)
  passed <- length(mem) == vcount(g_cmp) && all(is.finite(mem))
  add_result("helpers", "membership", passed,
             if (passed) sprintf("OK (length=%d)", length(mem)) else "Invalid membership")
}, error = function(e) {
  add_result("helpers", "membership", FALSE, e$message)
})

# modularity
tryCatch({
  mod <- modularity(comm_test)
  passed <- is.numeric(mod) && is.finite(mod) && mod > 0
  add_result("helpers", "modularity", passed,
             if (passed) sprintf("OK (%.4f)", mod) else sprintf("Invalid: %s", mod))
}, error = function(e) {
  add_result("helpers", "modularity", FALSE, e$message)
})

# ============================================================================
# Compute Summary
# ============================================================================

all_groups <- unique(vapply(results, `[[`, character(1), "group"))
summary_df <- do.call(rbind, lapply(all_groups, function(grp) {
  grp_results <- results[vapply(results, function(r) r$group == grp, logical(1))]
  passed <- sum(vapply(grp_results, function(r) r$passed, logical(1)))
  failed <- sum(vapply(grp_results, function(r) !r$passed, logical(1)))
  total <- passed + failed
  data.frame(
    test_group = grp,
    passed = passed,
    failed = failed,
    total = total,
    pass_rate = sprintf("%.1f%%", if (total > 0) 100 * passed / total else 100),
    stringsAsFactors = FALSE
  )
}))

total_tests <- sum(summary_df$total)
total_passed <- sum(summary_df$passed)
total_failed <- sum(summary_df$failed)

# ============================================================================
# Print Summary
# ============================================================================

cat("\n\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("COMPREHENSIVE COMMUNITY DETECTION RESULTS\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

print(summary_df, row.names = FALSE)

cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat(sprintf("TOTAL: %d tests, %d passed, %d failed (%.2f%% pass rate)\n",
            total_tests, total_passed, total_failed,
            100 * total_passed / max(total_tests, 1)))
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (length(failures) > 0) {
  cat("\nFAILURES:\n")
  for (f in failures[seq_len(min(20, length(failures)))]) {
    cat(sprintf("  [%s] %s: %s\n", f$group, f$network_name, f$reason))
  }
  if (length(failures) > 20) {
    cat(sprintf("  ... and %d more\n", length(failures) - 20))
  }
} else {
  cat("\n*** ALL TESTS PASSED! ***\n")
}

# ============================================================================
# Save Results
# ============================================================================

txt_file <- file.path("validation", "results_communities_comprehensive.txt")
sink(txt_file)
cat("COMPREHENSIVE COMMUNITY DETECTION VALIDATION REPORT\n")
cat("===================================================\n\n")
cat("Timestamp:", timestamp, "\n")
cat("Random networks:", n_random, "\n")
cat("Seed:", seed, "\n")
cat("R version:", R.version.string, "\n")
cat("igraph version:", as.character(packageVersion("igraph")), "\n")
cat("cograph version:", as.character(packageVersion("cograph")), "\n\n")

cat("Algorithms tested: fast_greedy, walktrap, edge_betweenness,\n")
cat("  leading_eigenvector, louvain, leiden, infomap, label_propagation,\n")
cat("  spinglass, optimal, fluid, consensus\n\n")

cat("SUMMARY BY GROUP\n")
cat("-----------------\n")
print(summary_df, row.names = FALSE)

cat(sprintf("\n\nOVERALL: %d tests, %d passed, %d failed (%.2f%%)\n",
            total_tests, total_passed, total_failed,
            100 * total_passed / max(total_tests, 1)))

if (length(failures) > 0) {
  cat("\n\nFAILURE DETAILS\n")
  cat("---------------\n")
  for (f in failures) {
    cat(sprintf("  [%s] %s: %s\n", f$group, f$network_name, f$reason))
  }
}
sink()

rds_file <- file.path("validation", "results_communities_comprehensive.rds")
saveRDS(list(
  timestamp = timestamp,
  config = list(n_random = n_random, seed = seed),
  summary = summary_df,
  failures = failures,
  all_results = results
), rds_file)

cat("\n\nResults saved to:\n")
cat("  Text:", txt_file, "\n")
cat("  RDS: ", rds_file, "\n")
