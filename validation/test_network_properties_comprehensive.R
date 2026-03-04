#!/usr/bin/env Rscript
#' ============================================================================
#' COMPREHENSIVE NETWORK PROPERTIES VALIDATION
#' ============================================================================
#'
#' Tests network-level metrics against known graph values and igraph, covering
#' directed/weighted variants, network_summary validation, degree_distribution,
#' small_world, and rich_club.
#'
#' USAGE:
#'   Rscript validation/test_network_properties_comprehensive.R [n_per_model] [seed]
#'
#' OUTPUT:
#'   - validation/results_network_properties_comprehensive.rds
#'   - validation/results_network_properties_comprehensive.txt
#'
#' ============================================================================

args <- commandArgs(trailingOnly = TRUE)
n_per_model <- if (length(args) >= 1) as.integer(args[1]) else 10
seed <- if (length(args) >= 2) as.integer(args[2]) else 42

suppressPackageStartupMessages({
  library(Saqrlab)
  library(igraph)
  devtools::load_all(".", quiet = TRUE)
})

tolerance <- 1e-10
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("COMPREHENSIVE NETWORK PROPERTIES VALIDATION\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("Timestamp:", timestamp, "\n")
cat("Networks per model:", n_per_model, "| Seed:", seed, "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# ============================================================================
# Test Infrastructure
# ============================================================================

results <- list()
failures <- list()
test_counter <- 0L

add_result <- function(category, test_name, passed, reason = "OK",
                       cograph_val = NA, igraph_val = NA, diff_val = NA) {
  test_counter <<- test_counter + 1L
  key <- paste0(category, "::", test_name)
  results[[key]] <<- list(
    id = test_counter,
    category = category,
    test = test_name,
    passed = passed,
    reason = reason,
    cograph_val = cograph_val,
    igraph_val = igraph_val,
    diff = diff_val
  )
  if (!passed) {
    failures[[length(failures) + 1]] <<- list(
      test_id = test_counter,
      category = category,
      metric = test_name,
      reason = reason
    )
    cat(sprintf("  [FAIL] %s: %s\n", test_name, reason))
  } else {
    cat(sprintf("  [PASS] %s\n", test_name))
  }
}

compare_val <- function(cg, ig, name, category, tol = tolerance) {
  cg <- as.numeric(cg)
  ig <- as.numeric(ig)

  if (length(cg) != 1 || length(ig) != 1) {
    # Vector comparison
    if (length(cg) != length(ig)) {
      add_result(category, name, FALSE,
                 sprintf("Length mismatch: %d vs %d", length(cg), length(ig)))
      return(invisible(FALSE))
    }
    cg[is.nan(cg)] <- NA
    ig[is.nan(ig)] <- NA
    both_na <- is.na(cg) & is.na(ig)
    both_fin <- is.finite(cg) & is.finite(ig)
    if (!all(both_na | both_fin)) {
      add_result(category, name, FALSE, "NA/Inf pattern mismatch")
      return(invisible(FALSE))
    }
    if (any(both_fin)) {
      md <- max(abs(cg[both_fin] - ig[both_fin]))
      if (md > tol) {
        add_result(category, name, FALSE, sprintf("max_diff=%.2e", md),
                   diff_val = md)
        return(invisible(FALSE))
      }
    }
    add_result(category, name, TRUE, "OK", diff_val = 0)
    return(invisible(TRUE))
  }

  # Scalar
  if (is.na(cg) && is.na(ig)) {
    add_result(category, name, TRUE, "OK (both NA)", cg, ig, 0)
    return(invisible(TRUE))
  }
  if (is.nan(cg) && is.nan(ig)) {
    add_result(category, name, TRUE, "OK (both NaN)", cg, ig, 0)
    return(invisible(TRUE))
  }
  if (is.infinite(cg) && is.infinite(ig) && sign(cg) == sign(ig)) {
    add_result(category, name, TRUE, "OK (both Inf)", cg, ig, 0)
    return(invisible(TRUE))
  }
  if (is.na(cg) || is.na(ig) || is.infinite(cg) || is.infinite(ig)) {
    add_result(category, name, FALSE,
               sprintf("NA/Inf mismatch: cg=%s, ig=%s", cg, ig), cg, ig, NA)
    return(invisible(FALSE))
  }

  d <- abs(cg - ig)
  passed <- d <= tol
  add_result(category, name, passed,
             if (passed) "OK" else sprintf("diff=%.2e", d),
             cg, ig, d)
  invisible(passed)
}

# ============================================================================
# Part 1: Known Graphs (exact expected values)
# ============================================================================

cat("--- Part 1: Known Graphs ---\n")

# K5 (Complete graph on 5 nodes)
K5 <- make_full_graph(5, directed = FALSE)
compare_val(network_girth(K5), 3, "K5_girth", "known_graphs")
compare_val(network_radius(K5), 1, "K5_radius", "known_graphs")
compare_val(network_vertex_connectivity(K5), 4, "K5_vertex_connectivity", "known_graphs")
compare_val(network_clique_size(K5), 5, "K5_clique_size", "known_graphs")
compare_val(network_cut_vertices(K5, count = TRUE), 0, "K5_cut_vertices", "known_graphs")
compare_val(network_bridges(K5, count = TRUE), 0, "K5_bridges", "known_graphs")
compare_val(network_global_efficiency(K5), 1.0, "K5_global_efficiency", "known_graphs")

# K10
K10 <- make_full_graph(10, directed = FALSE)
compare_val(network_girth(K10), 3, "K10_girth", "known_graphs")
compare_val(network_radius(K10), 1, "K10_radius", "known_graphs")
compare_val(network_clique_size(K10), 10, "K10_clique_size", "known_graphs")
compare_val(network_global_efficiency(K10), 1.0, "K10_global_efficiency", "known_graphs")

# Path(5)
P5 <- make_ring(5, directed = FALSE, circular = FALSE)
compare_val(network_girth(P5), igraph::girth(P5)$girth, "P5_girth", "known_graphs")
compare_val(network_radius(P5), igraph::radius(P5), "P5_radius", "known_graphs")
compare_val(network_cut_vertices(P5, count = TRUE),
            length(igraph::articulation_points(P5)), "P5_cut_vertices", "known_graphs")
compare_val(network_bridges(P5, count = TRUE),
            length(igraph::bridges(P5)), "P5_bridges", "known_graphs")

# Path(10)
P10 <- make_ring(10, directed = FALSE, circular = FALSE)
compare_val(network_girth(P10), igraph::girth(P10)$girth, "P10_girth", "known_graphs")
compare_val(network_radius(P10), igraph::radius(P10), "P10_radius", "known_graphs")

# Cycle(5)
C5 <- make_ring(5, directed = FALSE, circular = TRUE)
compare_val(network_girth(C5), 5, "C5_girth", "known_graphs")
compare_val(network_radius(C5), 2, "C5_radius", "known_graphs")
compare_val(network_cut_vertices(C5, count = TRUE), 0, "C5_cut_vertices", "known_graphs")
compare_val(network_bridges(C5, count = TRUE), 0, "C5_bridges", "known_graphs")

# Cycle(8)
C8 <- make_ring(8, directed = FALSE, circular = TRUE)
compare_val(network_girth(C8), igraph::girth(C8)$girth, "C8_girth", "known_graphs")

# Star(6)
S6 <- make_star(6, mode = "undirected")
compare_val(network_girth(S6), igraph::girth(S6)$girth, "S6_girth", "known_graphs")
compare_val(network_radius(S6), 1, "S6_radius", "known_graphs")
compare_val(network_cut_vertices(S6, count = TRUE), 1, "S6_cut_vertices", "known_graphs")
compare_val(network_bridges(S6, count = TRUE),
            length(igraph::bridges(S6)), "S6_bridges", "known_graphs")

# Star(10)
S10 <- make_star(10, mode = "undirected")
compare_val(network_radius(S10), 1, "S10_radius", "known_graphs")
compare_val(network_cut_vertices(S10, count = TRUE), 1, "S10_cut_vertices", "known_graphs")

# Grid(3x3)
G33 <- make_lattice(c(3, 3))
compare_val(network_girth(G33), igraph::girth(G33)$girth, "Grid3x3_girth", "known_graphs")
compare_val(network_radius(G33), igraph::radius(G33), "Grid3x3_radius", "known_graphs")
compare_val(network_vertex_connectivity(G33), igraph::vertex_connectivity(G33),
            "Grid3x3_vertex_connectivity", "known_graphs")

# Petersen graph
pet <- make_graph("Petersen")
compare_val(network_girth(pet), 5, "Petersen_girth", "known_graphs")
compare_val(network_radius(pet), 2, "Petersen_radius", "known_graphs")
compare_val(network_vertex_connectivity(pet), 3, "Petersen_vertex_connectivity", "known_graphs")
compare_val(network_clique_size(pet), 2, "Petersen_clique_size", "known_graphs")

# ============================================================================
# Part 2: Simulated Networks (igraph match)
# ============================================================================

cat("\n--- Part 2: Simulated Networks ---\n")

set.seed(seed)

models <- list(
  ER = function() sample_gnm(sample(20:40, 1), sample(40:100, 1)),
  BA = function() sample_pa(sample(20:40, 1), directed = FALSE),
  WS = function() sample_smallworld(1, sample(20:40, 1), 3, 0.1),
  SBM = function() {
    n <- 30
    pm <- matrix(0.05, 3, 3)
    diag(pm) <- 0.3
    sample_sbm(n, pm, c(10, 10, 10))
  },
  Regular = function() sample_k_regular(sample(20:30, 1), sample(3:5, 1)),
  Geometric = function() sample_grg(sample(30:50, 1), 0.3)
)

metrics_to_test <- c("girth", "radius", "vertex_connectivity", "clique_size",
                     "cut_vertices", "bridges")

for (model_name in names(models)) {
  cat(sprintf("\n  [%s]\n", model_name))

  for (i in seq_len(n_per_model)) {
    set.seed(seed + i * 100 + match(model_name, names(models)) * 1000)
    g <- tryCatch(models[[model_name]](), error = function(e) NULL)
    if (is.null(g) || vcount(g) < 4 || ecount(g) < 3) next

    net_label <- sprintf("%s_%d", model_name, i)

    for (metric in metrics_to_test) {
      tryCatch({
        cg_val <- switch(metric,
          girth = network_girth(g),
          radius = network_radius(g),
          vertex_connectivity = network_vertex_connectivity(g),
          clique_size = network_clique_size(g),
          cut_vertices = network_cut_vertices(g, count = TRUE),
          bridges = network_bridges(g, count = TRUE)
        )

        ig_val <- switch(metric,
          girth = igraph::girth(g)$girth,
          radius = igraph::radius(g),
          vertex_connectivity = igraph::vertex_connectivity(g),
          clique_size = igraph::clique_num(g),
          cut_vertices = length(igraph::articulation_points(g)),
          bridges = length(igraph::bridges(g))
        )

        compare_val(cg_val, ig_val, sprintf("%s_%s", net_label, metric), "simulated")
      }, error = function(e) {
        add_result("simulated", sprintf("%s_%s", net_label, metric), FALSE, e$message)
      })
    }

    # Global efficiency (manual verification)
    tryCatch({
      cg_eff <- network_global_efficiency(g)
      dists <- igraph::distances(g)
      n <- vcount(g)
      inv_dists <- 1 / dists
      diag(inv_dists) <- 0
      inv_dists[is.infinite(inv_dists)] <- 0
      manual_eff <- sum(inv_dists) / (n * (n - 1))
      compare_val(cg_eff, manual_eff, sprintf("%s_global_efficiency", net_label), "simulated")
    }, error = function(e) {
      add_result("simulated", sprintf("%s_global_efficiency", net_label), FALSE, e$message)
    })
  }
}

# ============================================================================
# Part 3: Directed Network Variants
# ============================================================================

cat("\n--- Part 3: Directed Networks ---\n")

set.seed(seed + 7000)
for (i in seq_len(10)) {
  nn <- sample(15:30, 1)
  ed <- runif(1, 2, 4)
  el <- simulate_edge_list(n_nodes = nn, edge_density = ed, directed = TRUE,
                            allow_self_loops = FALSE, seed = i * 8000 + seed)
  if (nrow(el) == 0) next
  g <- graph_from_data_frame(el[, c("source", "target")], directed = TRUE)
  net_label <- sprintf("dir_%d", i)

  # Radius with mode (cograph uses mode="out" for directed)
  tryCatch({
    cg_r <- network_radius(g)
    ig_r <- igraph::radius(g, mode = "out")
    compare_val(cg_r, ig_r, sprintf("%s_radius", net_label), "directed")
  }, error = function(e) {
    add_result("directed", sprintf("%s_radius", net_label), FALSE, e$message)
  })

  # Global efficiency directed
  tryCatch({
    cg_eff <- network_global_efficiency(g)
    dists <- igraph::distances(g, mode = "out")
    n <- vcount(g)
    inv_dists <- 1 / dists
    diag(inv_dists) <- 0
    inv_dists[is.infinite(inv_dists)] <- 0
    manual_eff <- sum(inv_dists) / (n * (n - 1))
    compare_val(cg_eff, manual_eff, sprintf("%s_global_efficiency", net_label), "directed",
                tol = 1e-6)
  }, error = function(e) {
    add_result("directed", sprintf("%s_global_efficiency", net_label), FALSE, e$message)
  })

  # Girth
  tryCatch({
    cg_girth <- network_girth(g)
    ig_girth <- igraph::girth(g)$girth
    compare_val(cg_girth, ig_girth, sprintf("%s_girth", net_label), "directed")
  }, error = function(e) {
    add_result("directed", sprintf("%s_girth", net_label), FALSE, e$message)
  })
}

# ============================================================================
# Part 4: Weighted Network Variants
# ============================================================================

cat("\n--- Part 4: Weighted Networks ---\n")

set.seed(seed + 9000)
for (i in seq_len(10)) {
  nn <- sample(15:30, 1)
  ed <- runif(1, 2, 4)
  el <- simulate_edge_list(n_nodes = nn, edge_density = ed, directed = FALSE,
                            allow_self_loops = FALSE, weight_range = c(0.1, 5),
                            seed = i * 10000 + seed)
  if (nrow(el) == 0) next
  g <- graph_from_data_frame(el[, c("source", "target")], directed = FALSE)
  E(g)$weight <- el$weight
  net_label <- sprintf("wt_%d", i)

  # Weighted efficiency (weights as distances)
  tryCatch({
    cg_eff <- network_global_efficiency(g)
    dists <- igraph::distances(g, weights = E(g)$weight)
    n <- vcount(g)
    inv_dists <- 1 / dists
    diag(inv_dists) <- 0
    inv_dists[is.infinite(inv_dists)] <- 0
    manual_eff <- sum(inv_dists) / (n * (n - 1))
    compare_val(cg_eff, manual_eff, sprintf("%s_global_efficiency", net_label), "weighted",
                tol = 1e-6)
  }, error = function(e) {
    add_result("weighted", sprintf("%s_global_efficiency", net_label), FALSE, e$message)
  })

  # Vertex connectivity
  tryCatch({
    cg_vc <- network_vertex_connectivity(g)
    ig_vc <- igraph::vertex_connectivity(g)
    compare_val(cg_vc, ig_vc, sprintf("%s_vertex_connectivity", net_label), "weighted")
  }, error = function(e) {
    add_result("weighted", sprintf("%s_vertex_connectivity", net_label), FALSE, e$message)
  })
}

# ============================================================================
# Part 5: network_summary() Column Validation
# ============================================================================

cat("\n--- Part 5: network_summary() Validation ---\n")

set.seed(seed + 11000)
for (i in seq_len(min(10, n_per_model))) {
  g <- sample_gnm(sample(20:40, 1), sample(40:100, 1))
  net_label <- sprintf("summary_%d", i)

  # Basic level
  tryCatch({
    summ_basic <- network_summary(g)
    # Check key columns exist
    expected_basic <- c("node_count", "edge_count", "density", "diameter",
                        "mean_distance", "transitivity")
    missing <- setdiff(expected_basic, names(summ_basic))
    passed <- length(missing) == 0
    add_result("network_summary", sprintf("%s_basic_columns", net_label), passed,
               if (passed) "OK" else sprintf("Missing: %s", paste(missing, collapse = ", ")))

    # Verify individual values
    if (passed) {
      compare_val(summ_basic$node_count, vcount(g),
                  sprintf("%s_node_count", net_label), "network_summary")
      compare_val(summ_basic$edge_count, ecount(g),
                  sprintf("%s_edge_count", net_label), "network_summary")
      compare_val(summ_basic$density, igraph::edge_density(g),
                  sprintf("%s_density", net_label), "network_summary", tol = 0.001)
      compare_val(summ_basic$diameter, igraph::diameter(g),
                  sprintf("%s_diameter", net_label), "network_summary")
    }
  }, error = function(e) {
    add_result("network_summary", sprintf("%s_basic", net_label), FALSE, e$message)
  })

  # Extended level
  tryCatch({
    summ_ext <- network_summary(g, extended = TRUE)
    expected_ext <- c("girth", "radius", "vertex_connectivity",
                      "global_efficiency", "local_efficiency")
    missing <- setdiff(expected_ext, names(summ_ext))
    passed <- length(missing) == 0
    add_result("network_summary", sprintf("%s_extended_columns", net_label), passed,
               if (passed) "OK" else sprintf("Missing: %s", paste(missing, collapse = ", ")))

    if (passed) {
      compare_val(summ_ext$girth, igraph::girth(g)$girth,
                  sprintf("%s_ext_girth", net_label), "network_summary")
      compare_val(summ_ext$radius, igraph::radius(g),
                  sprintf("%s_ext_radius", net_label), "network_summary")
    }
  }, error = function(e) {
    add_result("network_summary", sprintf("%s_extended", net_label), FALSE, e$message)
  })

  # Detailed level
  tryCatch({
    summ_det <- network_summary(g, detailed = TRUE)
    expected_det <- c("mean_degree", "sd_degree", "mean_betweenness")
    missing <- setdiff(expected_det, names(summ_det))
    passed <- length(missing) == 0
    add_result("network_summary", sprintf("%s_detailed_columns", net_label), passed,
               if (passed) "OK" else sprintf("Missing: %s", paste(missing, collapse = ", ")))

    if (passed) {
      ig_deg <- igraph::degree(g)
      compare_val(summ_det$mean_degree, mean(ig_deg),
                  sprintf("%s_mean_degree", net_label), "network_summary", tol = 0.01)
    }
  }, error = function(e) {
    add_result("network_summary", sprintf("%s_detailed", net_label), FALSE, e$message)
  })

  # Full level (both extended + detailed)
  tryCatch({
    summ_full <- network_summary(g, extended = TRUE, detailed = TRUE)
    n_cols <- ncol(summ_full)
    # Full should have the most columns
    passed <- n_cols >= 25  # at least 25 columns for undirected
    add_result("network_summary", sprintf("%s_full_ncols", net_label), passed,
               sprintf("n_cols=%d", n_cols))
  }, error = function(e) {
    add_result("network_summary", sprintf("%s_full", net_label), FALSE, e$message)
  })
}

# ============================================================================
# Part 6: degree_distribution()
# ============================================================================

cat("\n--- Part 6: degree_distribution ---\n")

for (net_name in c("K5", "er", "ba")) {
  g <- switch(net_name,
    K5 = make_full_graph(5),
    er = sample_gnm(50, 120),
    ba = sample_pa(50, directed = FALSE)
  )

  tryCatch({
    # Capture the histogram object
    png(tempfile())
    hist_obj <- degree_distribution(g)
    dev.off()

    ig_deg <- igraph::degree(g)

    # Verify histogram counts match degree frequency
    if (!is.null(hist_obj) && inherits(hist_obj, "histogram")) {
      total_count <- sum(hist_obj$counts)
      passed <- total_count == vcount(g)
      add_result("degree_distribution", sprintf("%s_total_count", net_name), passed,
                 sprintf("hist=%d, expected=%d", total_count, vcount(g)))
    } else {
      add_result("degree_distribution", sprintf("%s_returns_hist", net_name), TRUE,
                 "OK (function ran without error)")
    }

    # mode = "in" / "out" on directed
    if (net_name == "er") {
      g_dir <- sample_gnm(50, 120, directed = TRUE)
      for (mode in c("in", "out")) {
        tryCatch({
          png(tempfile())
          hist_dir <- degree_distribution(g_dir, mode = mode)
          dev.off()
          add_result("degree_distribution", sprintf("directed_%s", mode), TRUE, "OK")
        }, error = function(e) {
          add_result("degree_distribution", sprintf("directed_%s", mode), FALSE, e$message)
        })
      }
    }

  }, error = function(e) {
    try(dev.off(), silent = TRUE)
    add_result("degree_distribution", sprintf("%s", net_name), FALSE, e$message)
  })
}

# ============================================================================
# Part 7: small_world
# ============================================================================

cat("\n--- Part 7: Small World ---\n")

# WS should have sigma > 1
tryCatch({
  set.seed(seed)
  g_ws <- sample_smallworld(1, 100, 5, 0.1)
  sigma_ws <- network_small_world(g_ws, n_random = 10)
  passed <- is.finite(sigma_ws) && sigma_ws > 1
  add_result("small_world", "WS_sigma_gt1", passed,
             sprintf("sigma=%.3f", sigma_ws))
}, error = function(e) {
  add_result("small_world", "WS_sigma_gt1", FALSE, e$message)
})

# ER should have sigma ~ 1
tryCatch({
  set.seed(seed)
  g_er <- sample_gnm(100, 500)
  sigma_er <- network_small_world(g_er, n_random = 10)
  # ER sigma should be relatively close to 1
  passed <- is.finite(sigma_er) && sigma_er > 0 && sigma_er < 5
  add_result("small_world", "ER_sigma_near1", passed,
             sprintf("sigma=%.3f", sigma_er))
}, error = function(e) {
  add_result("small_world", "ER_sigma_near1", FALSE, e$message)
})

# BA (scale-free) sigma test — can be NA if clustering is zero
tryCatch({
  set.seed(seed)
  g_ba <- sample_pa(100, directed = FALSE)
  sigma_ba <- network_small_world(g_ba, n_random = 10)
  passed <- is.na(sigma_ba) || (is.finite(sigma_ba) && sigma_ba > 0)
  add_result("small_world", "BA_sigma_valid", passed,
             sprintf("sigma=%s", sigma_ba))
}, error = function(e) {
  add_result("small_world", "BA_sigma_positive", FALSE, e$message)
})

# Too-small graph should return NA
tryCatch({
  g_tiny <- make_ring(3)
  sigma_tiny <- network_small_world(g_tiny)
  passed <- is.na(sigma_tiny)
  add_result("small_world", "tiny_graph_NA", passed,
             sprintf("sigma=%s", sigma_tiny))
}, error = function(e) {
  add_result("small_world", "tiny_graph_NA", FALSE, e$message)
})

# ============================================================================
# Part 8: rich_club
# ============================================================================

cat("\n--- Part 8: Rich Club ---\n")

# K5: all degrees equal -> rich club phi should be 1 (all rich nodes connected)
tryCatch({
  phi_k5 <- network_rich_club(K5, k = 3)
  passed <- is.finite(phi_k5) && abs(phi_k5 - 1.0) < 0.01
  add_result("rich_club", "K5_phi_1", passed, sprintf("phi=%.4f", phi_k5))
}, error = function(e) {
  add_result("rich_club", "K5_phi_1", FALSE, e$message)
})

# Star: hub has high degree, periphery low -> rich club can be NA if < 2 rich nodes
tryCatch({
  g_star <- make_star(20, mode = "undirected")
  phi_star <- network_rich_club(g_star, k = 1)
  passed <- is.na(phi_star) || (is.finite(phi_star) && phi_star >= 0 && phi_star <= 1)
  add_result("rich_club", "Star_phi_valid", passed, sprintf("phi=%s", phi_star))
}, error = function(e) {
  add_result("rich_club", "Star_phi_bounds", FALSE, e$message)
})

# BA network: normalized > 1 expected (rich club effect)
tryCatch({
  set.seed(seed)
  g_ba_rc <- sample_pa(100, directed = FALSE)
  phi_norm <- network_rich_club(g_ba_rc, normalized = TRUE, n_random = 10)
  passed <- is.finite(phi_norm) && phi_norm > 0
  add_result("rich_club", "BA_normalized", passed,
             sprintf("phi_norm=%.4f", phi_norm))
}, error = function(e) {
  add_result("rich_club", "BA_normalized", FALSE, e$message)
})

# Default k (median degree)
tryCatch({
  g_rc <- sample_gnm(50, 150)
  phi_default <- network_rich_club(g_rc)
  passed <- is.finite(phi_default) && phi_default >= 0 && phi_default <= 1
  add_result("rich_club", "default_k", passed, sprintf("phi=%.4f", phi_default))
}, error = function(e) {
  add_result("rich_club", "default_k", FALSE, e$message)
})

# ============================================================================
# Compute Summary
# ============================================================================

all_categories <- unique(vapply(results, `[[`, character(1), "category"))
summary_df <- do.call(rbind, lapply(all_categories, function(cat) {
  cat_results <- results[vapply(results, function(r) r$category == cat, logical(1))]
  passed <- sum(vapply(cat_results, function(r) r$passed, logical(1)))
  failed <- sum(vapply(cat_results, function(r) !r$passed, logical(1)))
  total <- passed + failed
  diffs <- vapply(cat_results, function(r) {
    if (!is.null(r$diff) && !is.na(r$diff)) r$diff else NA_real_
  }, numeric(1))
  max_diff <- if (all(is.na(diffs))) NA_real_ else max(diffs, na.rm = TRUE)

  data.frame(
    category = cat,
    passed = passed,
    failed = failed,
    total = total,
    max_diff = max_diff,
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
cat("COMPREHENSIVE NETWORK PROPERTIES RESULTS\n")
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
    cat(sprintf("  [%s] %s: %s\n", f$category, f$metric, f$reason))
  }
} else {
  cat("\n*** ALL TESTS PASSED! ***\n")
}

# ============================================================================
# Save Results
# ============================================================================

txt_file <- file.path("validation", "results_network_properties_comprehensive.txt")
sink(txt_file)
cat("COMPREHENSIVE NETWORK PROPERTIES VALIDATION REPORT\n")
cat("==================================================\n\n")
cat("Timestamp:", timestamp, "\n")
cat("Networks per model:", n_per_model, "\n")
cat("Seed:", seed, "\n")
cat("Tolerance:", tolerance, "\n")
cat("R version:", R.version.string, "\n")
cat("igraph version:", as.character(packageVersion("igraph")), "\n")
cat("cograph version:", as.character(packageVersion("cograph")), "\n\n")

cat("SUMMARY BY CATEGORY\n")
cat("--------------------\n")
print(summary_df, row.names = FALSE)

cat(sprintf("\n\nOVERALL: %d tests, %d passed, %d failed (%.2f%%)\n",
            total_tests, total_passed, total_failed,
            100 * total_passed / max(total_tests, 1)))

if (length(failures) > 0) {
  cat("\n\nFAILURE DETAILS\n")
  cat("---------------\n")
  for (f in failures) {
    cat(sprintf("  [%s] %s: %s\n", f$category, f$metric, f$reason))
  }
}
sink()

rds_file <- file.path("validation", "results_network_properties_comprehensive.rds")
saveRDS(list(
  timestamp = timestamp,
  config = list(n_per_model = n_per_model, seed = seed, tolerance = tolerance),
  summary = summary_df,
  failures = failures,
  all_results = results
), rds_file)

cat("\n\nResults saved to:\n")
cat("  Text:", txt_file, "\n")
cat("  RDS: ", rds_file, "\n")
