# ═══════════════════════════════════════════════════════════════════════════════
# Centrality Validation: Mathematical Equivalence Tests
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script validates cograph centrality measures against:
# 1. igraph implementations (for igraph-backed measures)
# 2. Manual mathematical calculations (for custom implementations)
#
# Run with: Rscript validation/test_centrality_equivalence.R
# ═══════════════════════════════════════════════════════════════════════════════

library(igraph)
devtools::load_all(".")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("              CENTRALITY VALIDATION: Mathematical Equivalence\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Test network
mat <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(mat) <- colnames(mat) <- LETTERS[1:5]
g <- igraph::graph_from_adjacency_matrix(mat, mode = "undirected")
n <- 5

passed <- 0
failed <- 0
tests <- list()

check <- function(name, cograph_val, expected_val, tol = 1e-6) {
  match <- all(abs(cograph_val - expected_val) < tol)
  tests[[name]] <<- list(passed = match, cograph = cograph_val, expected = expected_val)
  if (match) {
    cat("  ✓", name, "\n")
    passed <<- passed + 1
  } else {
    cat("  ✗", name, "\n")
    cat("    cograph: ", paste(round(cograph_val, 6), collapse = ", "), "\n")
    cat("    expected:", paste(round(expected_val, 6), collapse = ", "), "\n")
    failed <<- failed + 1
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1: igraph-backed measures
# ═══════════════════════════════════════════════════════════════════════════════
cat("─── Part 1: igraph-backed measures ───\n\n")

check("degree", centrality_degree(mat), igraph::degree(g))
check("betweenness", centrality_betweenness(mat), igraph::betweenness(g))
check("closeness", centrality_closeness(mat), igraph::closeness(g))
check("eigenvector", centrality_eigenvector(mat), igraph::eigen_centrality(g)$vector)
check("pagerank", centrality_pagerank(mat), igraph::page_rank(g)$vector)
check("harmonic", centrality_harmonic(mat), igraph::harmonic_centrality(g))
check("alpha (Katz)", centrality_alpha(mat), igraph::alpha_centrality(g, exo = 1))
check("subgraph", centrality_subgraph(mat), igraph::subgraph_centrality(g, diag = FALSE))
check("power (Bonacich)", centrality_power(mat), igraph::power_centrality(g, exponent = 1))
check("edge_betweenness", edge_centrality(mat)$betweenness, igraph::edge_betweenness(g))

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1.5: centiserve and sna package comparison
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n─── Part 1.5: centiserve/sna package comparison ───\n\n")

if (requireNamespace("centiserve", quietly = TRUE)) {
  check("laplacian (vs centiserve)",
        centrality_laplacian(mat),
        centiserve::laplacian(g))

  check("current_flow_closeness (vs centiserve)",
        centrality_current_flow_closeness(mat),
        centiserve::closeness.currentflow(g))

  check("diffusion (vs centiserve)",
        centrality_diffusion(mat),
        centiserve::diffusion.degree(g))

  check("leverage (vs centiserve)",
        centrality_leverage(mat),
        centiserve::leverage(g))

  check("kreach (vs centiserve::geokpath)",
        centrality_kreach(mat, k = 3),
        centiserve::geokpath(g, k = 3))
} else {
  cat("  (centiserve not installed, skipping)\n")
}

if (requireNamespace("sna", quietly = TRUE)) {
  sna_load <- sna::loadcent(mat, gmode = "graph")
  names(sna_load) <- LETTERS[1:5]
  check("load (vs sna::loadcent)", centrality_load(mat), sna_load)
} else {
  cat("  (sna not installed, skipping)\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1.6: NetworkX comparison (via reticulate)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n─── Part 1.6: NetworkX comparison ───\n\n")

if (requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_module_available("networkx")) {
  nx <- reticulate::import("networkx")

  # Create NetworkX graph
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  # Current-flow betweenness
  nx_cfb <- nx$current_flow_betweenness_centrality(G)
  nx_cfb_vec <- sapply(LETTERS[1:5], function(x) nx_cfb[[x]])
  check("current_flow_betweenness (vs NetworkX)",
        centrality_current_flow_betweenness(mat), nx_cfb_vec, tol = 1e-5)

  # Percolation
  states <- reticulate::py_dict(LETTERS[1:5], rep(1.0, 5))
  nx_perc <- nx$percolation_centrality(G, states = states)
  nx_perc_vec <- sapply(LETTERS[1:5], function(x) nx_perc[[x]])
  check("percolation (vs NetworkX)",
        centrality_percolation(mat), nx_perc_vec)

  # Laplacian (qi method)
  nx_lap <- nx$laplacian_centrality(G, normalized = FALSE)
  nx_lap_vec <- sapply(LETTERS[1:5], function(x) nx_lap[[x]])
  check("laplacian (vs NetworkX)",
        centrality_laplacian(mat), nx_lap_vec)

  # VoteRank (ordering)
  nx_vr <- unlist(nx$voterank(G))
  cg_vr <- centrality_voterank(mat)
  cg_order <- names(sort(cg_vr, decreasing = TRUE))[1:length(nx_vr)]
  if (all(cg_order == nx_vr)) {
    cat("  ✓ voterank ordering (vs NetworkX)\n")
    passed <- passed + 1
  } else {
    cat("  ✗ voterank ordering\n")
    failed <- failed + 1
  }
} else {
  cat("  (reticulate/NetworkX not available, skipping)\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 2: Property-based tests (fallback validation)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n─── Part 2: Property-based tests ───\n\n")

# Percolation with uniform states should equal normalized betweenness
perc <- centrality_percolation(mat)
betw <- centrality_betweenness(mat)
perc_zero_where_betw_zero <- all(perc[betw == 0] == 0)
correlation <- cor(perc, betw)
if (perc_zero_where_betw_zero && abs(correlation - 1) < 1e-6) {
  cat("  ✓ percolation equals betweenness (uniform states)\n")
  passed <- passed + 1
} else {
  cat("  ✗ percolation (r =", round(correlation, 6), ")\n")
  failed <- failed + 1
}

# VoteRank: most connected node should rank highest
vr <- centrality_voterank(mat)
if (vr["C"] == max(vr) && all(vr >= 0 & vr <= 1)) {
  cat("  ✓ voterank (most connected node ranks highest)\n")
  passed <- passed + 1
} else {
  cat("  ✗ voterank\n")
  failed <- failed + 1
}

# ═══════════════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
if (failed == 0) {
  cat(sprintf("  ✓ ALL %d TESTS PASSED\n", passed))
} else {
  cat(sprintf("  Results: %d passed, %d failed\n", passed, failed))
}
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Return exit code
quit(status = failed)
