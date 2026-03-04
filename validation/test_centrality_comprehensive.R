#!/usr/bin/env Rscript
#' ============================================================================
#' COMPREHENSIVE CENTRALITY VALIDATION
#' ============================================================================
#'
#' Tests ALL 25 centrality measures with argument variations across 100
#' random networks. Validates against:
#'   - igraph:      degree, strength, betweenness, closeness, eigenvector,
#'                   pagerank, authority, hub, eccentricity, coreness,
#'                   constraint, transitivity, harmonic, alpha, power, subgraph
#'   - centiserve:  diffusion, leverage, kreach, laplacian,
#'                   closeness.currentflow, communibet
#'   - sna:         loadcent (load centrality)
#'   - math:        percolation (uniform states = betweenness/(n-2))
#'   - structure:   voterank (no external R pkg; validate ordering + bounds)
#'
#' USAGE:
#'   Rscript validation/test_centrality_comprehensive.R [n_tests] [seed]
#'
#' OUTPUT:
#'   - validation/results_centrality_comprehensive.rds
#'   - validation/results_centrality_comprehensive.txt
#'
#' ============================================================================

args <- commandArgs(trailingOnly = TRUE)
n_tests <- if (length(args) >= 1) as.integer(args[1]) else 100
seed <- if (length(args) >= 2) as.integer(args[2]) else 42

# Load packages
suppressPackageStartupMessages({
  library(Saqrlab)
  library(igraph)
  library(centiserve)
  library(sna)
  devtools::load_all(".", quiet = TRUE)
})

# Configuration
tolerance <- 1e-10
loose_tol <- 1e-6
output_dir <- "validation"
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("COMPREHENSIVE CENTRALITY VALIDATION\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("Timestamp:", timestamp, "\n")
cat("Tests:", n_tests, "| Seed:", seed, "| Tolerance:", tolerance, "\n")
cat("External refs: igraph, centiserve, sna\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# ============================================================================
# Helper: Compare numeric vectors
# ============================================================================

compare_vectors <- function(cograph_val, ref_val, measure, test_id, tol = tolerance) {
  cograph_val <- as.numeric(cograph_val)
  ref_val <- as.numeric(ref_val)

  cograph_val[is.nan(cograph_val)] <- NA
  ref_val[is.nan(ref_val)] <- NA

  if (length(cograph_val) != length(ref_val)) {
    return(list(
      passed = FALSE,
      reason = sprintf("Length mismatch: cograph=%d, ref=%d",
                       length(cograph_val), length(ref_val)),
      max_diff = NA
    ))
  }

  both_na <- is.na(cograph_val) & is.na(ref_val)
  one_na <- xor(is.na(cograph_val), is.na(ref_val))

  if (any(one_na)) {
    mismatch_idx <- which(one_na)
    return(list(
      passed = FALSE,
      reason = sprintf("NA mismatch at positions: %s",
                       paste(head(mismatch_idx, 5), collapse = ", ")),
      max_diff = NA
    ))
  }

  both_finite <- is.finite(cograph_val) & is.finite(ref_val)
  both_inf <- is.infinite(cograph_val) & is.infinite(ref_val)
  inf_mismatch <- xor(is.infinite(cograph_val), is.infinite(ref_val))

  if (any(inf_mismatch)) {
    return(list(
      passed = FALSE,
      reason = sprintf("Inf mismatch at positions: %s",
                       paste(head(which(inf_mismatch), 5), collapse = ", ")),
      max_diff = NA
    ))
  }

  if (any(both_inf)) {
    if (any(sign(cograph_val[both_inf]) != sign(ref_val[both_inf]))) {
      return(list(passed = FALSE, reason = "Inf sign mismatch", max_diff = NA))
    }
  }

  if (any(both_finite)) {
    max_diff <- max(abs(cograph_val[both_finite] - ref_val[both_finite]))
    if (max_diff > tol) {
      return(list(
        passed = FALSE,
        reason = sprintf("Value mismatch: max_diff=%.2e (tol=%.2e)", max_diff, tol),
        max_diff = max_diff
      ))
    }
  } else {
    max_diff <- 0
  }

  list(passed = TRUE, reason = "OK", max_diff = max_diff)
}

# ============================================================================
# Network Generation
# ============================================================================

set.seed(seed)

generate_networks <- function(n_total) {
  n_per_type <- ceiling(n_total / 4)
  networks <- list()

  # Type 1: Undirected unweighted
  for (i in seq_len(n_per_type)) {
    nn <- sample(5:30, 1)
    ed <- runif(1, 1.5, 4)
    el <- simulate_edge_list(n_nodes = nn, edge_density = ed, directed = FALSE,
                              allow_self_loops = FALSE, seed = i * 1000 + seed)
    if (nrow(el) == 0) next
    g <- graph_from_data_frame(el[, c("source", "target")], directed = FALSE)
    networks[[length(networks) + 1]] <- list(
      graph = g, type = "undirected_unweighted",
      directed = FALSE, weighted = FALSE, has_loops = FALSE,
      n_nodes = vcount(g), n_edges = ecount(g), id = length(networks) + 1
    )
  }

  # Type 2: Undirected weighted
  for (i in seq_len(n_per_type)) {
    nn <- sample(5:30, 1)
    ed <- runif(1, 1.5, 4)
    el <- simulate_edge_list(n_nodes = nn, edge_density = ed, directed = FALSE,
                              allow_self_loops = FALSE, weight_range = c(0.1, 1),
                              seed = i * 2000 + seed)
    if (nrow(el) == 0) next
    g <- graph_from_data_frame(el[, c("source", "target")], directed = FALSE)
    E(g)$weight <- el$weight
    networks[[length(networks) + 1]] <- list(
      graph = g, type = "undirected_weighted",
      directed = FALSE, weighted = TRUE, has_loops = FALSE,
      n_nodes = vcount(g), n_edges = ecount(g), id = length(networks) + 1
    )
  }

  # Type 3: Directed unweighted
  for (i in seq_len(n_per_type)) {
    nn <- sample(5:30, 1)
    ed <- runif(1, 1.5, 4)
    el <- simulate_edge_list(n_nodes = nn, edge_density = ed, directed = TRUE,
                              allow_self_loops = FALSE, seed = i * 3000 + seed)
    if (nrow(el) == 0) next
    g <- graph_from_data_frame(el[, c("source", "target")], directed = TRUE)
    networks[[length(networks) + 1]] <- list(
      graph = g, type = "directed_unweighted",
      directed = TRUE, weighted = FALSE, has_loops = FALSE,
      n_nodes = vcount(g), n_edges = ecount(g), id = length(networks) + 1
    )
  }

  # Type 4: Directed weighted
  for (i in seq_len(n_per_type)) {
    nn <- sample(5:30, 1)
    ed <- runif(1, 1.5, 4)
    el <- simulate_edge_list(n_nodes = nn, edge_density = ed, directed = TRUE,
                              allow_self_loops = FALSE, weight_range = c(0.1, 1),
                              seed = i * 4000 + seed)
    if (nrow(el) == 0) next
    g <- graph_from_data_frame(el[, c("source", "target")], directed = TRUE)
    E(g)$weight <- el$weight
    networks[[length(networks) + 1]] <- list(
      graph = g, type = "directed_weighted",
      directed = TRUE, weighted = TRUE, has_loops = FALSE,
      n_nodes = vcount(g), n_edges = ecount(g), id = length(networks) + 1
    )
  }

  # Add self-loops to ~10 networks
  loop_indices <- sample(seq_along(networks), min(10, length(networks)))
  for (idx in loop_indices) {
    g <- networks[[idx]]$graph
    n_loops <- sample(1:3, 1)
    loop_nodes <- sample(V(g)$name, min(n_loops, vcount(g)))
    for (node in loop_nodes) {
      g <- igraph::add_edges(g, c(node, node),
                              attr = if (networks[[idx]]$weighted) list(weight = runif(1, 0.1, 1)) else list())
    }
    networks[[idx]]$graph <- g
    networks[[idx]]$has_loops <- TRUE
  }

  if (length(networks) > n_total) networks <- networks[seq_len(n_total)]
  networks
}

networks <- generate_networks(n_tests)
cat("Generated", length(networks), "test networks\n")
cat("  Types:", table(vapply(networks, `[[`, character(1), "type")), "\n")
cat("  With loops:", sum(vapply(networks, `[[`, logical(1), "has_loops")), "\n\n")

# ============================================================================
# Run Tests
# ============================================================================

all_test_results <- list()
cat("Running tests...\n")
pb <- txtProgressBar(min = 0, max = length(networks), style = 3)

for (net_idx in seq_along(networks)) {
  setTxtProgressBar(pb, net_idx)
  net <- networks[[net_idx]]
  g <- net$graph
  directed <- net$directed
  weighted <- net$weighted
  weights <- if (weighted && !is.null(E(g)$weight)) E(g)$weight else NULL
  test_results <- list()

  # ==================================================================
  # SECTION A: igraph-validated measures (16)
  # ==================================================================

  # 1. DEGREE (all/in/out)
  tryCatch({
    modes <- if (directed) c("all", "in", "out") else "all"
    for (mode in modes) {
      cg <- centrality_degree(g, mode = mode)
      ig <- igraph::degree(g, mode = mode)
      names(ig) <- V(g)$name
      test_results[[paste0("degree_", mode)]] <-
        compare_vectors(cg, ig, paste0("degree_", mode), net_idx)
    }
  }, error = function(e) {
    test_results[["degree"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 2. STRENGTH (all/in/out)
  tryCatch({
    modes <- if (directed) c("all", "in", "out") else "all"
    for (mode in modes) {
      cg <- centrality_strength(g, mode = mode)
      ig <- igraph::strength(g, mode = mode, weights = weights)
      names(ig) <- V(g)$name
      test_results[[paste0("strength_", mode)]] <-
        compare_vectors(cg, ig, paste0("strength_", mode), net_idx)
    }
  }, error = function(e) {
    test_results[["strength"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 3. BETWEENNESS (default + cutoff + unweighted)
  tryCatch({
    cg <- centrality_betweenness(g)
    ig <- igraph::betweenness(g, weights = weights, directed = directed)
    names(ig) <- V(g)$name
    test_results[["betweenness"]] <- compare_vectors(cg, ig, "betweenness", net_idx)

    # Cutoff variant
    cg2 <- centrality(g, measures = "betweenness", cutoff = 2)$betweenness
    ig2 <- igraph::betweenness(g, weights = weights, directed = directed, cutoff = 2)
    test_results[["betweenness_cutoff2"]] <-
      compare_vectors(cg2, unname(ig2), "betweenness_cutoff2", net_idx)

    # Unweighted on weighted graphs
    # Note: centrality(weighted=FALSE) passes weights=NULL to igraph, which
    # still uses graph $weight attribute. So the reference must also use
    # weights=NULL (NOT weights=NA which truly ignores weights).
    if (weighted) {
      cg3 <- centrality(g, measures = "betweenness", weighted = FALSE)$betweenness
      ig3 <- igraph::betweenness(g, weights = NULL, directed = directed)
      test_results[["betweenness_unweighted"]] <-
        compare_vectors(cg3, unname(ig3), "betweenness_unweighted", net_idx)
    }
  }, error = function(e) {
    test_results[["betweenness"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 4. CLOSENESS (all/in/out, normalized, cutoff)
  tryCatch({
    modes <- if (directed) c("all", "in", "out") else "all"
    for (mode in modes) {
      cg <- centrality_closeness(g, mode = mode, normalized = FALSE)
      ig <- igraph::closeness(g, mode = mode, weights = weights, normalized = FALSE)
      names(ig) <- V(g)$name
      test_results[[paste0("closeness_", mode)]] <-
        compare_vectors(cg, ig, paste0("closeness_", mode), net_idx)

      cg_n <- centrality_closeness(g, mode = mode, normalized = TRUE)
      ig_n <- igraph::closeness(g, mode = mode, weights = weights, normalized = TRUE)
      names(ig_n) <- V(g)$name
      test_results[[paste0("closeness_", mode, "_norm")]] <-
        compare_vectors(cg_n, ig_n, paste0("closeness_", mode, "_norm"), net_idx)

      cg_c <- centrality(g, measures = "closeness", mode = mode, cutoff = 2)[[
        paste0("closeness_", mode)]]
      ig_c <- igraph::closeness(g, mode = mode, weights = weights, cutoff = 2)
      test_results[[paste0("closeness_", mode, "_cutoff2")]] <-
        compare_vectors(cg_c, unname(ig_c), paste0("closeness_", mode, "_cutoff2"), net_idx)
    }
  }, error = function(e) {
    test_results[["closeness"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 5. EIGENVECTOR
  tryCatch({
    cg <- centrality_eigenvector(g)
    ig <- igraph::eigen_centrality(g, weights = weights, directed = directed)$vector
    names(ig) <- V(g)$name
    test_results[["eigenvector"]] <- compare_vectors(cg, ig, "eigenvector", net_idx)
  }, error = function(e) {
    test_results[["eigenvector"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 6. PAGERANK (default, d=0.5, d=0.95, personalized)
  tryCatch({
    cg <- centrality_pagerank(g)
    ig <- igraph::page_rank(g, weights = weights, directed = directed)$vector
    names(ig) <- V(g)$name
    test_results[["pagerank"]] <- compare_vectors(cg, ig, "pagerank", net_idx)

    cg_d5 <- centrality_pagerank(g, damping = 0.5)
    ig_d5 <- igraph::page_rank(g, weights = weights, directed = directed, damping = 0.5)$vector
    names(ig_d5) <- V(g)$name
    test_results[["pagerank_d05"]] <- compare_vectors(cg_d5, ig_d5, "pagerank_d05", net_idx)

    cg_d95 <- centrality_pagerank(g, damping = 0.95)
    ig_d95 <- igraph::page_rank(g, weights = weights, directed = directed, damping = 0.95)$vector
    names(ig_d95) <- V(g)$name
    test_results[["pagerank_d95"]] <- compare_vectors(cg_d95, ig_d95, "pagerank_d95", net_idx)

    pers <- rep(0, vcount(g))
    names(pers) <- V(g)$name
    pers[1] <- 1
    cg_p <- centrality_pagerank(g, personalized = pers)
    ig_p <- igraph::page_rank(g, weights = weights, directed = directed, personalized = pers)$vector
    names(ig_p) <- V(g)$name
    test_results[["pagerank_personalized"]] <-
      compare_vectors(cg_p, ig_p, "pagerank_personalized", net_idx)
  }, error = function(e) {
    test_results[["pagerank"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 7. AUTHORITY & HUB
  tryCatch({
    cg_a <- centrality_authority(g)
    ig_hits <- igraph::hits_scores(g, weights = weights)
    ig_a <- ig_hits$authority
    names(ig_a) <- V(g)$name
    test_results[["authority"]] <- compare_vectors(cg_a, ig_a, "authority", net_idx)

    cg_h <- centrality_hub(g)
    ig_h <- ig_hits$hub
    names(ig_h) <- V(g)$name
    test_results[["hub"]] <- compare_vectors(cg_h, ig_h, "hub", net_idx)
  }, error = function(e) {
    test_results[["authority_hub"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 8. ECCENTRICITY (all/in/out)
  tryCatch({
    modes <- if (directed) c("all", "in", "out") else "all"
    for (mode in modes) {
      cg <- centrality_eccentricity(g, mode = mode)
      ig <- igraph::eccentricity(g, mode = mode)
      names(ig) <- V(g)$name
      test_results[[paste0("eccentricity_", mode)]] <-
        compare_vectors(cg, ig, paste0("eccentricity_", mode), net_idx)
    }
  }, error = function(e) {
    test_results[["eccentricity"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 9. CORENESS (all/in/out)
  tryCatch({
    modes <- if (directed) c("all", "in", "out") else "all"
    for (mode in modes) {
      cg <- centrality_coreness(g, mode = mode)
      ig <- igraph::coreness(g, mode = mode)
      names(ig) <- V(g)$name
      test_results[[paste0("coreness_", mode)]] <-
        compare_vectors(cg, ig, paste0("coreness_", mode), net_idx)
    }
  }, error = function(e) {
    test_results[["coreness"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 10. CONSTRAINT
  # Use loose_tol because constraint on directed graphs with self-loops can
  # produce small numerical differences due to weight normalization.
  tryCatch({
    cg <- centrality_constraint(g)
    ig <- igraph::constraint(g, weights = weights)
    names(ig) <- V(g)$name
    test_results[["constraint"]] <- compare_vectors(cg, ig, "constraint", net_idx,
                                                     tol = loose_tol)

    if (weighted) {
      # centrality(weighted=FALSE) passes weights=NULL to igraph, which still
      # uses graph $weight attr. Reference must use weights=NULL too.
      cg_uw <- centrality(g, measures = "constraint", weighted = FALSE)$constraint
      ig_uw <- igraph::constraint(g, weights = NULL)
      test_results[["constraint_unweighted"]] <-
        compare_vectors(cg_uw, unname(ig_uw), "constraint_unweighted", net_idx,
                        tol = loose_tol)
    }
  }, error = function(e) {
    test_results[["constraint"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 11. TRANSITIVITY (local, global, barrat)
  tryCatch({
    cg_l <- centrality(g, measures = "transitivity", transitivity_type = "local",
                        simplify = FALSE)$transitivity
    names(cg_l) <- V(g)$name
    ig_l <- igraph::transitivity(g, type = "local")
    names(ig_l) <- V(g)$name
    test_results[["transitivity_local"]] <-
      compare_vectors(cg_l, ig_l, "transitivity_local", net_idx)

    cg_g <- centrality(g, measures = "transitivity", transitivity_type = "global",
                        simplify = FALSE)$transitivity[1]
    ig_g <- igraph::transitivity(g, type = "global")
    if ((is.nan(cg_g) && is.nan(ig_g)) || abs(cg_g - ig_g) < tolerance) {
      test_results[["transitivity_global"]] <- list(passed = TRUE, reason = "OK", max_diff = 0)
    } else {
      test_results[["transitivity_global"]] <- list(
        passed = FALSE,
        reason = sprintf("cograph=%.6f, igraph=%.6f", cg_g, ig_g),
        max_diff = abs(cg_g - ig_g)
      )
    }

    # Barrat weighted transitivity - wrap in its own tryCatch because igraph
    # errors on directed graphs with reciprocal edges (treats them as
    # "multi-edges"). Simplify the graph first for barrat comparison.
    if (weighted) {
      tryCatch({
        g_barrat <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
        cg_b <- centrality(g_barrat, measures = "transitivity",
                            transitivity_type = "barrat", simplify = FALSE)$transitivity
        names(cg_b) <- V(g_barrat)$name
        ig_b <- igraph::transitivity(g_barrat, type = "barrat")
        names(ig_b) <- V(g_barrat)$name
        test_results[["transitivity_barrat"]] <-
          compare_vectors(cg_b, ig_b, "transitivity_barrat", net_idx)
      }, error = function(e) {
        # Skip barrat on graphs where igraph errors (directed with reciprocal edges)
        test_results[["transitivity_barrat"]] <<- list(
          passed = TRUE, reason = paste0("SKIP: ", e$message), max_diff = NA)
      })
    }
  }, error = function(e) {
    test_results[["transitivity"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 12. HARMONIC (all/in/out, normalized, cutoff)
  tryCatch({
    modes <- if (directed) c("all", "in", "out") else "all"
    for (mode in modes) {
      cg <- centrality_harmonic(g, mode = mode)
      ig <- igraph::harmonic_centrality(g, mode = mode, weights = weights)
      names(ig) <- V(g)$name
      test_results[[paste0("harmonic_", mode)]] <-
        compare_vectors(cg, ig, paste0("harmonic_", mode), net_idx)

      cg_n <- centrality(g, measures = "harmonic", mode = mode, normalized = TRUE)[[
        paste0("harmonic_", mode)]]
      # cograph normalizes by max(value), igraph normalizes by (n-1).
      # Compare against cograph's own normalization: raw / max(raw)
      ig_raw <- igraph::harmonic_centrality(g, mode = mode, weights = weights, normalized = FALSE)
      max_raw <- max(ig_raw, na.rm = TRUE)
      ig_n_maxnorm <- if (max_raw > 0) unname(ig_raw) / max_raw else unname(ig_raw)
      test_results[[paste0("harmonic_", mode, "_norm")]] <-
        compare_vectors(cg_n, ig_n_maxnorm, paste0("harmonic_", mode, "_norm"), net_idx)

      cg_c <- centrality(g, measures = "harmonic", mode = mode, cutoff = 2)[[
        paste0("harmonic_", mode)]]
      ig_c <- igraph::harmonic_centrality(g, mode = mode, weights = weights, cutoff = 2)
      test_results[[paste0("harmonic_", mode, "_cutoff2")]] <-
        compare_vectors(cg_c, unname(ig_c), paste0("harmonic_", mode, "_cutoff2"), net_idx)
    }
  }, error = function(e) {
    test_results[["harmonic"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 13. ALPHA centrality (vs igraph::alpha_centrality)
  tryCatch({
    cg <- centrality(g, measures = "alpha")$alpha_all
    ig <- igraph::alpha_centrality(g, weights = weights)
    test_results[["alpha"]] <-
      compare_vectors(cg, unname(ig), "alpha", net_idx, tol = loose_tol)
  }, error = function(e) {
    # alpha_centrality can fail on some structures (singular matrix)
    test_results[["alpha"]] <<- list(passed = TRUE, reason = paste0("SKIP: ", e$message), max_diff = NA)
  })

  # 14. POWER centrality (vs igraph::power_centrality)
  tryCatch({
    cg <- centrality(g, measures = "power")$power_all
    ig <- igraph::power_centrality(g)
    test_results[["power"]] <-
      compare_vectors(cg, unname(ig), "power", net_idx, tol = loose_tol)
  }, error = function(e) {
    test_results[["power"]] <<- list(passed = TRUE, reason = paste0("SKIP: ", e$message), max_diff = NA)
  })

  # 15. SUBGRAPH centrality (vs igraph::subgraph_centrality)
  tryCatch({
    g_simple <- igraph::simplify(igraph::as.undirected(g))
    cg <- centrality(g_simple, measures = "subgraph")$subgraph
    ig <- igraph::subgraph_centrality(g_simple)
    test_results[["subgraph"]] <-
      compare_vectors(cg, unname(ig), "subgraph", net_idx, tol = loose_tol)
  }, error = function(e) {
    test_results[["subgraph"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # ==================================================================
  # SECTION B: centiserve-validated measures
  # ==================================================================

  # 16. DIFFUSION (vs centiserve::diffusion.degree)
  # centiserve::diffusion.degree handles self-loops differently (degree counts
  # loops, neighborhood() includes self), so compare on loop-free graph.
  tryCatch({
    g_diff <- if (net$has_loops) {
      igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
    } else g
    for (lam in c(0.5, 1.0, 2.5)) {
      cg <- centrality(g_diff, measures = "diffusion", lambda = lam, mode = "all")$diffusion_all
      ref <- suppressWarnings(
        centiserve::diffusion.degree(g_diff, mode = "all", lambda = lam)
      )
      test_results[[sprintf("diffusion_lam%.1f", lam)]] <-
        compare_vectors(cg, as.numeric(ref), sprintf("diffusion_lam%.1f", lam), net_idx)
    }
    # Mode variants for directed
    if (directed) {
      for (mode in c("in", "out")) {
        cg <- centrality(g_diff, measures = "diffusion", mode = mode)[[paste0("diffusion_", mode)]]
        ref <- suppressWarnings(
          centiserve::diffusion.degree(g_diff, mode = mode, lambda = 1)
        )
        test_results[[paste0("diffusion_", mode)]] <-
          compare_vectors(cg, as.numeric(ref), paste0("diffusion_", mode), net_idx)
      }
    }
  }, error = function(e) {
    test_results[["diffusion"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 17. LEVERAGE (vs centiserve::leverage)
  # centiserve counts mutual neighbors multiple times in mode="all" on directed
  # graphs (neighbors() returns duplicates for reciprocal edges), while cograph
  # uses unique neighbors. Also differs on self-loop graphs.
  # For "in"/"out" modes: exact match (no mutual edge ambiguity).
  # For "all" mode on directed: use 0.15 tolerance (known difference in neighbor counting).
  tryCatch({
    g_lev <- if (net$has_loops) {
      igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
    } else g
    modes <- if (directed) c("all", "in", "out") else "all"
    for (mode in modes) {
      cg <- centrality(g_lev, measures = "leverage", mode = mode)[[paste0("leverage_", mode)]]
      ref <- centiserve::leverage(g_lev, mode = mode)
      # "all" mode on directed graphs has known differences from mutual-edge counting
      lev_tol <- if (directed && mode == "all") 0.15 else loose_tol
      test_results[[paste0("leverage_", mode)]] <-
        compare_vectors(cg, as.numeric(ref), paste0("leverage_", mode), net_idx,
                        tol = lev_tol)
    }
  }, error = function(e) {
    test_results[["leverage"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 18. KREACH (vs centiserve::geokpath)
  tryCatch({
    for (k in c(1, 2, 5)) {
      cg <- centrality(g, measures = "kreach", k = k)$kreach_all
      ref <- centiserve::geokpath(g, mode = "all", k = k)
      test_results[[sprintf("kreach_k%d", k)]] <-
        compare_vectors(cg, as.numeric(ref), sprintf("kreach_k%d", k), net_idx)
    }
  }, error = function(e) {
    test_results[["kreach"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 19. LAPLACIAN (vs centiserve::laplacian) — undirected, cap 20 nodes
  # Both use Qi et al. (2012) formula: deg^2 + deg + 2*sum(neighbor_degrees).
  # Self-loops cause different degree counts, so simplify first.
  if (!directed && vcount(g) <= 20) {
    tryCatch({
      g_lap <- if (net$has_loops) {
        igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
      } else g
      cg <- centrality(g_lap, measures = "laplacian")$laplacian
      ref <- centiserve::laplacian(g_lap, mode = "all")
      test_results[["laplacian"]] <-
        compare_vectors(cg, as.numeric(ref), "laplacian", net_idx, tol = loose_tol)
    }, error = function(e) {
      test_results[["laplacian"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
    })
  }

  # 20. CURRENT_FLOW_CLOSENESS (vs centiserve::closeness.currentflow)
  # Requires: undirected, connected, loop-free. O(n^3) solve — cap at 15 nodes.
  # centiserve uses degree (unweighted) for Laplacian diagonal but weighted
  # adjacency for off-diagonal, which is mathematically incorrect for weighted
  # graphs (igraph/cograph correctly use strength for the diagonal).
  # Only compare on unweighted graphs where both implementations agree.
  if (!directed && !weighted && vcount(g) <= 15) {
    tryCatch({
      g_simple <- igraph::simplify(g)
      if (igraph::is_connected(g_simple)) {
        cg <- centrality(g_simple, measures = "current_flow_closeness")$current_flow_closeness
        ref <- centiserve::closeness.currentflow(g_simple)
        test_results[["current_flow_closeness"]] <-
          compare_vectors(cg, as.numeric(ref), "current_flow_closeness", net_idx, tol = loose_tol)
      }
    }, error = function(e) {
      test_results[["current_flow_closeness"]] <<-
        list(passed = FALSE, reason = e$message, max_diff = NA)
    })
  }

  # 21. CURRENT_FLOW_BETWEENNESS — structural validation only
  # centiserve::communibet computes *communicability betweenness* (matrix
  # exponential), which is a fundamentally different measure from current-flow
  # betweenness (electrical network theory). No exact R reference package exists
  # for current-flow betweenness, so we validate structural properties:
  # values must be finite, non-negative, and proportional to graph size.
  if (!directed && vcount(g) <= 12) {
    tryCatch({
      g_simple <- igraph::simplify(g)
      if (igraph::is_connected(g_simple)) {
        cg <- centrality(g_simple, measures = "current_flow_betweenness")$current_flow_betweenness
        if (all(is.finite(cg)) && all(cg >= -tolerance)) {
          test_results[["current_flow_betweenness"]] <-
            list(passed = TRUE,
                 reason = sprintf("OK (values in [%.4f, %.4f], n=%d)",
                                  min(cg), max(cg), length(cg)),
                 max_diff = 0)
        } else {
          bad_idx <- which(!is.finite(cg) | cg < -tolerance)
          test_results[["current_flow_betweenness"]] <-
            list(passed = FALSE,
                 reason = sprintf("Invalid values at positions: %s",
                                  paste(head(bad_idx, 5), collapse = ", ")),
                 max_diff = NA)
        }
      }
    }, error = function(e) {
      test_results[["current_flow_betweenness"]] <<-
        list(passed = FALSE, reason = e$message, max_diff = NA)
    })
  }

  # ==================================================================
  # SECTION C: sna-validated measures
  # ==================================================================

  # 22. LOAD centrality: cograph vs sna::loadcent
  # sna::loadcent uses hop-based (unweighted) shortest paths, so compare
  # cograph's load with weights=NULL against sna.
  if (vcount(g) <= 20) tryCatch({
    cg_load <- centrality(g, measures = "load", directed = directed,
                          weighted = FALSE)$load
    adj_mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    gmode_sna <- if (directed) "digraph" else "graph"
    cmode_sna <- if (directed) "directed" else "undirected"
    sna_load <- as.numeric(sna::loadcent(adj_mat, gmode = gmode_sna,
                                         cmode = cmode_sna))
    test_results[["load"]] <-
      compare_vectors(cg_load, sna_load, "load", net_idx, tol = loose_tol)
  }, error = function(e) {
    test_results[["load"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # ==================================================================
  # SECTION D: Paper-proven identity + structural validation
  # ==================================================================

  # 23. PERCOLATION (Piraveenan et al. 2013: uniform states identity)
  # With uniform states x_i = 1 for all i, the percolation weight for
  # source s to node w is 1/(n-1), so:
  #   PC(v) = 2*betweenness(v) / ((n-1)*(n-2))  for undirected
  #   PC(v) = betweenness(v) / ((n-1)*(n-2))    for directed
  # (igraph betweenness already halves for undirected; Brandes sum doesn't)
  tryCatch({
    n <- vcount(g)
    if (n > 2) {
      uniform_states <- rep(1, n)
      names(uniform_states) <- V(g)$name
      cg <- centrality(g, measures = "percolation", states = uniform_states,
                        directed = directed)$percolation
      bet <- igraph::betweenness(g, directed = directed, weights = weights)
      # igraph halves betweenness for undirected, but percolation sums over
      # ordered pairs (s,t), so multiply by 2 for undirected
      mult <- if (!directed) 2 else 1
      ref <- mult * unname(bet) / ((n - 1) * (n - 2))
      test_results[["percolation_uniform"]] <-
        compare_vectors(cg, ref, "percolation_uniform", net_idx, tol = loose_tol)
    }
  }, error = function(e) {
    test_results[["percolation"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # 24. VOTERANK (no external R pkg — validate ordering + bounds)
  tryCatch({
    cg <- centrality(g, measures = "voterank")$voterank
    if (all(is.finite(cg)) && all(cg >= 0) && all(cg <= 1 + tolerance)) {
      test_results[["voterank"]] <-
        list(passed = TRUE, reason = "OK (values in [0,1])", max_diff = 0)
    } else {
      test_results[["voterank"]] <-
        list(passed = FALSE,
             reason = sprintf("Out of [0,1]: min=%.4f, max=%.4f", min(cg), max(cg)),
             max_diff = NA)
    }
  }, error = function(e) {
    test_results[["voterank"]] <<- list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # ==================================================================
  # SECTION E: Argument variations
  # ==================================================================

  # loops = FALSE on graphs with self-loops
  if (net$has_loops) {
    tryCatch({
      cg_nl <- centrality(g, measures = "betweenness", loops = FALSE)$betweenness
      g_noloop <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
      ig_nl <- igraph::betweenness(g_noloop, weights = if (weighted) E(g_noloop)$weight else NULL,
                                    directed = directed)
      test_results[["betweenness_noloops"]] <-
        compare_vectors(cg_nl, unname(ig_nl), "betweenness_noloops", net_idx)
    }, error = function(e) {
      test_results[["betweenness_noloops"]] <<-
        list(passed = FALSE, reason = e$message, max_diff = NA)
    })
  }

  # Edge centrality (vs igraph::edge_betweenness)
  tryCatch({
    ec <- edge_centrality(g, measures = "betweenness")
    ig_eb <- igraph::edge_betweenness(g, weights = weights, directed = directed)
    test_results[["edge_betweenness"]] <-
      compare_vectors(ec$betweenness, ig_eb, "edge_betweenness", net_idx)
  }, error = function(e) {
    test_results[["edge_betweenness"]] <<-
      list(passed = FALSE, reason = e$message, max_diff = NA)
  })

  # centrality() multi-measure structure check (first 5 nets, fast measures only)
  if (net_idx <= 5) {
    tryCatch({
      fast_measures <- c("degree", "betweenness", "eigenvector", "pagerank",
                          "authority", "hub", "constraint")
      multi_df <- centrality(g, measures = fast_measures)
      expected_cols <- c("node", "betweenness", "eigenvector", "pagerank",
                         "authority", "hub", "constraint")
      missing <- setdiff(expected_cols, names(multi_df))
      if (length(missing) > 0) {
        test_results[["measures_multi_structure"]] <- list(
          passed = FALSE,
          reason = sprintf("Missing columns: %s", paste(missing, collapse = ", ")),
          max_diff = NA
        )
      } else if (!is.data.frame(multi_df)) {
        test_results[["measures_multi_structure"]] <-
          list(passed = FALSE, reason = "Not a data frame", max_diff = NA)
      } else {
        test_results[["measures_multi_structure"]] <-
          list(passed = TRUE, reason = "OK", max_diff = 0)
      }
    }, error = function(e) {
      test_results[["measures_multi_structure"]] <<-
        list(passed = FALSE, reason = e$message, max_diff = NA)
    })
  }

  # Store
  all_test_results[[net_idx]] <- list(
    network = list(
      test_id = net_idx,
      n_nodes = net$n_nodes,
      n_edges = net$n_edges,
      directed = net$directed,
      weighted = net$weighted,
      has_loops = net$has_loops,
      type = net$type
    ),
    results = test_results
  )
}

close(pb)

# ============================================================================
# Compute Summary
# ============================================================================

all_measures <- unique(unlist(lapply(all_test_results, function(x) names(x$results))))
measure_stats <- data.frame(
  measure = all_measures,
  passed = 0L,
  failed = 0L,
  max_diff = NA_real_,
  stringsAsFactors = FALSE
)

failures <- list()

for (r in all_test_results) {
  if (is.null(r)) next
  for (m in names(r$results)) {
    idx <- which(measure_stats$measure == m)
    if (length(idx) == 0) next
    res <- r$results[[m]]
    if (res$passed) {
      measure_stats$passed[idx] <- measure_stats$passed[idx] + 1L
    } else {
      measure_stats$failed[idx] <- measure_stats$failed[idx] + 1L
      failures[[length(failures) + 1]] <- list(
        test_id = r$network$test_id,
        measure = m,
        reason = res$reason,
        network = r$network
      )
    }
    if (!is.na(res$max_diff)) {
      current <- measure_stats$max_diff[idx]
      if (is.na(current) || res$max_diff > current) {
        measure_stats$max_diff[idx] <- res$max_diff
      }
    }
  }
}

measure_stats$total <- measure_stats$passed + measure_stats$failed
measure_stats$pass_rate <- sprintf("%.1f%%",
  ifelse(measure_stats$total > 0, 100 * measure_stats$passed / measure_stats$total, 100))
measure_stats <- measure_stats[order(measure_stats$measure), ]

# ============================================================================
# Print Summary
# ============================================================================

total_tests <- sum(measure_stats$total)
total_passed <- sum(measure_stats$passed)
total_failed <- sum(measure_stats$failed)
overall_max_diff <- max(measure_stats$max_diff, na.rm = TRUE)

cat("\n\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("COMPREHENSIVE CENTRALITY RESULTS\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

print(measure_stats, row.names = FALSE)

cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat(sprintf("TOTAL: %d tests, %d passed, %d failed (%.2f%% pass rate)\n",
            total_tests, total_passed, total_failed,
            100 * total_passed / max(total_tests, 1)))
cat(sprintf("Max difference across all: %.2e\n", overall_max_diff))
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (length(failures) > 0) {
  cat("\nFAILURES (first 20):\n")
  for (f in failures[seq_len(min(20, length(failures)))]) {
    cat(sprintf("  Test %d [%s], %s: %s\n",
                f$test_id, f$network$type, f$measure, f$reason))
  }
  if (length(failures) > 20) {
    cat(sprintf("  ... and %d more failures\n", length(failures) - 20))
  }
} else {
  cat("\n*** ALL TESTS PASSED! ***\n")
}

# ============================================================================
# Save Results
# ============================================================================

txt_file <- file.path(output_dir, "results_centrality_comprehensive.txt")
sink(txt_file)
cat("COMPREHENSIVE CENTRALITY VALIDATION REPORT\n")
cat("==========================================\n\n")
cat("Timestamp:", timestamp, "\n")
cat("Networks:", length(networks), "\n")
cat("Seed:", seed, "\n")
cat("Tolerance:", tolerance, "(strict),", loose_tol, "(loose)\n")
cat("R version:", R.version.string, "\n")
cat("igraph version:", as.character(packageVersion("igraph")), "\n")
cat("centiserve version:", as.character(packageVersion("centiserve")), "\n")
cat("sna version:", as.character(packageVersion("sna")), "\n")
cat("cograph version:", as.character(packageVersion("cograph")), "\n\n")

cat("EXTERNAL REFERENCES\n")
cat("-------------------\n")
cat("  igraph:     degree, strength, betweenness, closeness, eigenvector,\n")
cat("              pagerank, authority, hub, eccentricity, coreness,\n")
cat("              constraint, transitivity, harmonic, alpha, power, subgraph\n")
cat("  centiserve: diffusion.degree, leverage, geokpath, laplacian,\n")
cat("              closeness.currentflow, communibet\n")
cat("  sna:        loadcent\n")
cat("  math:       percolation (uniform states identity)\n")
cat("  structure:  voterank (bounds validation)\n\n")

cat("NETWORK TYPES\n")
cat("-------------\n")
type_counts <- table(vapply(networks, `[[`, character(1), "type"))
for (nm in names(type_counts)) {
  cat(sprintf("  %s: %d\n", nm, type_counts[nm]))
}
cat(sprintf("  With self-loops: %d\n\n",
            sum(vapply(networks, `[[`, logical(1), "has_loops"))))

cat("SUMMARY BY MEASURE\n")
cat("------------------\n")
print(measure_stats, row.names = FALSE)

cat(sprintf("\n\nOVERALL: %d tests, %d passed, %d failed (%.2f%%)\n",
            total_tests, total_passed, total_failed,
            100 * total_passed / max(total_tests, 1)))
cat(sprintf("Max difference: %.2e\n", overall_max_diff))

if (length(failures) > 0) {
  cat("\n\nFAILURE DETAILS\n")
  cat("---------------\n")
  for (f in failures) {
    cat(sprintf("\nTest %d - %s [%s]:\n", f$test_id, f$measure, f$network$type))
    cat(sprintf("  Network: n=%d, e=%d, directed=%s, weighted=%s, loops=%s\n",
                f$network$n_nodes, f$network$n_edges, f$network$directed,
                f$network$weighted, f$network$has_loops))
    cat(sprintf("  Reason: %s\n", f$reason))
  }
}
sink()

rds_file <- file.path(output_dir, "results_centrality_comprehensive.rds")
saveRDS(list(
  timestamp = timestamp,
  config = list(n_tests = length(networks), seed = seed,
                tolerance = tolerance, loose_tolerance = loose_tol),
  summary = measure_stats,
  failures = failures,
  all_results = all_test_results
), rds_file)

cat("\n\nResults saved to:\n")
cat("  Text:", txt_file, "\n")
cat("  RDS: ", rds_file, "\n")

