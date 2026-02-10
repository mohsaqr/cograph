#!/usr/bin/env Rscript
#' ============================================================================
#' NETWORK-LEVEL METRICS - NUMERICAL MATCHING
#' ============================================================================
#'
#' Shows exact numerical comparisons between cograph and igraph functions
#' using simulated networks from Saqrlab::simulate_igraph().
#'
#' USAGE:
#'   Rscript validation/test_network_metrics_numerical.R
#'   Rscript validation/test_network_metrics_numerical.R 20    # networks per model
#'   Rscript validation/test_network_metrics_numerical.R 20 123  # with seed
#'
#' ============================================================================

suppressPackageStartupMessages({
  library(igraph)
  devtools::load_all(".", quiet = TRUE)
  library(Saqrlab)
})

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
n_per_model <- if (length(args) >= 1) as.integer(args[1]) else 10
seed <- if (length(args) >= 2) as.integer(args[2]) else 42

set.seed(seed)

cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("NETWORK-LEVEL METRICS - NUMERICAL MATCHING\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Networks per model:", n_per_model, "| Seed:", seed, "\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n\n")

# ============================================
# Results storage
# ============================================

results <- data.frame(
  model = character(),
  network = integer(),
  n_nodes = integer(),
  n_edges = integer(),
  metric = character(),
  cograph = numeric(),
  igraph = numeric(),
  diff = numeric(),
  match = logical(),
  stringsAsFactors = FALSE
)

total_tests <- 0
total_pass <- 0

# ============================================
# Helper function
# ============================================

add_result <- function(model, net_id, n, m, metric, cograph_val, igraph_val) {
  # Handle Inf
  if (is.infinite(cograph_val) && is.infinite(igraph_val)) {
    diff <- 0
    match <- TRUE
  } else if (is.na(cograph_val) && is.na(igraph_val)) {
    diff <- NA
    match <- TRUE
  } else {
    diff <- abs(cograph_val - igraph_val)
    match <- diff < 1e-10
  }

  total_tests <<- total_tests + 1
  if (match) total_pass <<- total_pass + 1

  results <<- rbind(results, data.frame(
    model = model,
    network = net_id,
    n_nodes = n,
    n_edges = m,
    metric = metric,
    cograph = cograph_val,
    igraph = igraph_val,
    diff = diff,
    match = match,
    stringsAsFactors = FALSE
  ))

  match
}

# ============================================
# PART 1: Known Graphs
# ============================================

cat("PART 1: KNOWN GRAPHS\n")
cat("-" |> rep(80) |> paste(collapse = ""), "\n\n")

known_graphs <- list(
  "Complete_K5" = make_full_graph(5),
  "Complete_K6" = make_full_graph(6),
  "Path_5" = make_graph(c(1,2, 2,3, 3,4, 4,5), directed = FALSE),
  "Path_10" = make_lattice(10, directed = FALSE),
  "Cycle_5" = make_ring(5),
  "Cycle_8" = make_ring(8),
  "Star_6" = make_star(6, "undirected"),
  "Star_10" = make_star(10, "undirected"),
  "Grid_3x3" = make_lattice(c(3, 3)),
  "Petersen" = make_graph("Petersen")
)

cat(sprintf("%-15s %6s %6s %25s %12s %12s %8s\n",
            "Graph", "Nodes", "Edges", "Metric", "cograph", "igraph", "Match"))
cat("-" |> rep(80) |> paste(collapse = ""), "\n")

for (name in names(known_graphs)) {
  g <- known_graphs[[name]]
  n <- vcount(g)
  m <- ecount(g)

  metrics <- list(
    girth = c(network_girth(g), girth(g)$girth),
    radius = c(network_radius(g), radius(g)),
    vertex_conn = c(network_vertex_connectivity(g), vertex_connectivity(g)),
    clique_size = c(network_clique_size(g), clique_num(g)),
    cut_vertices = c(network_cut_vertices(g, count_only = TRUE), length(articulation_points(g))),
    bridges = c(network_bridges(g, count_only = TRUE), length(bridges(g)))
  )

  for (metric_name in names(metrics)) {
    vals <- metrics[[metric_name]]
    cg <- vals[1]
    ig <- vals[2]
    match <- add_result(name, 0, n, m, metric_name, cg, ig)

    cg_str <- ifelse(is.infinite(cg), "Inf", sprintf("%.6f", cg))
    ig_str <- ifelse(is.infinite(ig), "Inf", sprintf("%.6f", ig))

    cat(sprintf("%-15s %6d %6d %25s %12s %12s %8s\n",
                name, n, m, metric_name, cg_str, ig_str,
                ifelse(match, "YES", "NO")))
  }
}

# ============================================
# PART 2: Simulated Networks
# ============================================

cat("\n")
cat("PART 2: SIMULATED NETWORKS (Saqrlab::simulate_igraph)\n")
cat("-" |> rep(80) |> paste(collapse = ""), "\n\n")

models <- c("er", "ba", "ws", "sbm", "reg", "grg")
model_names <- c(
  er = "Erdos-Renyi",
  ba = "Barabasi-Albert",
  ws = "Watts-Strogatz",
  sbm = "Stochastic Block",
  reg = "Regular",
  grg = "Geometric Random"
)

for (model in models) {
  cat(sprintf("\n%s (%s):\n", model_names[model], toupper(model)))
  cat(sprintf("%-4s %6s %6s %20s %15s %15s %8s\n",
              "#", "Nodes", "Edges", "Metric", "cograph", "igraph", "Match"))
  cat("-" |> rep(80) |> paste(collapse = ""), "\n")

  for (i in seq_len(n_per_model)) {
    n <- sample(20:60, 1)

    g <- tryCatch({
      simulate_igraph(
        n = n,
        model = model,
        directed = FALSE,
        weighted = FALSE,
        seed = seed + i + which(models == model) * 1000
      )
    }, error = function(e) {
      sample_gnp(n, 0.15)
    })

    # Get largest connected component
    if (!is_connected(g)) {
      comp <- components(g)
      g <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
    }

    if (vcount(g) < 4) next

    n_v <- vcount(g)
    m_e <- ecount(g)

    # Test all metrics
    metrics <- list(
      girth = c(network_girth(g), girth(g)$girth),
      radius = c(network_radius(g), radius(g)),
      vertex_conn = c(network_vertex_connectivity(g), vertex_connectivity(g)),
      clique_size = c(network_clique_size(g), clique_num(g)),
      cut_vertices = c(network_cut_vertices(g, count_only = TRUE), length(articulation_points(g))),
      bridges = c(network_bridges(g, count_only = TRUE), length(bridges(g)))
    )

    for (metric_name in names(metrics)) {
      vals <- metrics[[metric_name]]
      cg <- vals[1]
      ig <- vals[2]
      match <- add_result(model, i, n_v, m_e, metric_name, cg, ig)

      cg_str <- ifelse(is.infinite(cg), "Inf", sprintf("%.6f", cg))
      ig_str <- ifelse(is.infinite(ig), "Inf", sprintf("%.6f", ig))

      cat(sprintf("%-4d %6d %6d %20s %15s %15s %8s\n",
                  i, n_v, m_e, metric_name, cg_str, ig_str,
                  ifelse(match, "YES", "NO")))
    }
  }
}

# ============================================
# PART 3: Efficiency Metrics
# ============================================

cat("\n")
cat("PART 3: EFFICIENCY METRICS (with manual verification)\n")
cat("-" |> rep(80) |> paste(collapse = ""), "\n\n")

# Manual global efficiency calculation
calc_global_efficiency <- function(g) {
  n <- vcount(g)
  if (n <= 1) return(NA_real_)
  sp <- distances(g, mode = "all")
  diag(sp) <- NA
  inv_sp <- 1 / sp
  inv_sp[is.infinite(sp)] <- 0
  sum(inv_sp, na.rm = TRUE) / (n * (n - 1))
}

cat(sprintf("%-20s %6s %6s %15s %15s %15s %8s\n",
            "Model", "Nodes", "Edges", "cograph", "manual_calc", "difference", "Match"))
cat("-" |> rep(80) |> paste(collapse = ""), "\n")

for (model in c("er", "ba", "ws", "sbm")) {
  g <- simulate_igraph(n = 30, model = model, directed = FALSE, seed = seed)
  if (!is_connected(g)) {
    comp <- components(g)
    g <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  }

  cg <- network_global_efficiency(g)
  manual <- calc_global_efficiency(g)
  diff <- abs(cg - manual)
  match <- diff < 1e-10

  total_tests <<- total_tests + 1
  if (match) total_pass <<- total_pass + 1

  cat(sprintf("%-20s %6d %6d %15.10f %15.10f %15.2e %8s\n",
              model_names[model], vcount(g), ecount(g), cg, manual, diff,
              ifelse(match, "YES", "NO")))
}

# ============================================
# Summary
# ============================================

cat("\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("=" |> rep(80) |> paste(collapse = ""), "\n\n")

cat(sprintf("Total comparisons: %d\n", total_tests))
cat(sprintf("Exact matches: %d\n", total_pass))
cat(sprintf("Mismatches: %d\n", total_tests - total_pass))
cat(sprintf("Match rate: %.2f%%\n", 100 * total_pass / total_tests))

# Check for any failures
failures <- results[!results$match, ]
if (nrow(failures) > 0) {
  cat("\nFAILURES:\n")
  print(failures)
} else {
  cat("\n*** ALL NUMERICAL VALUES MATCH EXACTLY ***\n")
}

# Save results
output_file <- "validation/results_network_metrics_numerical.txt"
sink(output_file)
cat("NETWORK-LEVEL METRICS - NUMERICAL MATCHING RESULTS\n")
cat("===================================================\n\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Networks per model:", n_per_model, "| Seed:", seed, "\n\n")
cat(sprintf("Total comparisons: %d\n", total_tests))
cat(sprintf("Exact matches: %d\n", total_pass))
cat(sprintf("Match rate: %.2f%%\n\n", 100 * total_pass / total_tests))
cat("Detailed Results:\n")
cat("-----------------\n")
print(results, row.names = FALSE)
sink()

# Also save as RDS for further analysis
saveRDS(results, "validation/results_network_metrics_numerical.rds")

cat("\nResults saved to:\n")
cat("  -", output_file, "\n")
cat("  - validation/results_network_metrics_numerical.rds\n")
