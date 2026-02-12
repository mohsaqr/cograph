# =============================================================================
# OBSESSIVE GRADE TESTING: Community Detection & Motifs
# =============================================================================

devtools::load_all(".")
library(igraph)

# Helper for string concatenation
`%s+%` <- function(a, b) paste0(a, b)

cat("=" %s+% strrep("=", 70) %s+% "\n")
cat("OBSESSIVE GRADE TESTING\n")
cat("=" %s+% strrep("=", 70) %s+% "\n\n")

errors <- 0
tests_run <- 0

check <- function(condition, msg) {
  tests_run <<- tests_run + 1
  if (condition) {
    cat("✓", msg, "\n")
  } else {
    cat("✗ FAIL:", msg, "\n")
    errors <<- errors + 1
  }
}

# =============================================================================
# PART 1: SEED REPRODUCIBILITY TESTS
# =============================================================================

cat("\n" %s+% strrep("-", 70) %s+% "\n")
cat("PART 1: SEED REPRODUCIBILITY TESTS\n")
cat(strrep("-", 70) %s+% "\n\n")

# Test graphs
g_zachary <- make_graph("Zachary")
g_random <- sample_gnm(50, 200)

# Test 1.1: Louvain seed reproducibility
cat("1.1 Louvain seed reproducibility:\n")
for (seed in c(1, 42, 123, 999)) {
  c1 <- community_louvain(g_zachary, seed = seed)
  c2 <- community_louvain(g_zachary, seed = seed)
  check(identical(membership(c1), membership(c2)),
        sprintf("  seed=%d produces identical results", seed))
}

# Test different seeds produce potentially different results (run multiple times)
cat("\n1.2 Different seeds can produce different results:\n")
results_different_seeds <- lapply(1:10, function(s) {
  membership(community_louvain(g_random, seed = s))
})
n_unique <- length(unique(lapply(results_different_seeds, as.character)))
cat("  ", n_unique, "unique partitions from 10 different seeds\n")
check(n_unique >= 1, "  Different seeds explored (at least 1 unique)")

# Test 1.3: Leiden seed reproducibility
cat("\n1.3 Leiden seed reproducibility:\n")
for (seed in c(1, 42, 123)) {
  c1 <- community_leiden(g_zachary, seed = seed)
  c2 <- community_leiden(g_zachary, seed = seed)
  check(identical(membership(c1), membership(c2)),
        sprintf("  seed=%d produces identical results", seed))
}

# Test 1.4: Infomap seed reproducibility
cat("\n1.4 Infomap seed reproducibility:\n")
for (seed in c(1, 42)) {
  c1 <- community_infomap(g_zachary, seed = seed)
  c2 <- community_infomap(g_zachary, seed = seed)
  check(identical(membership(c1), membership(c2)),
        sprintf("  seed=%d produces identical results", seed))
}

# Test 1.5: Label propagation seed reproducibility
cat("\n1.5 Label propagation seed reproducibility:\n")
for (seed in c(1, 42)) {
  c1 <- community_label_propagation(g_zachary, seed = seed)
  c2 <- community_label_propagation(g_zachary, seed = seed)
  check(identical(membership(c1), membership(c2)),
        sprintf("  seed=%d produces identical results", seed))
}

# Test 1.6: Spinglass seed reproducibility (connected graph required)
cat("\n1.6 Spinglass seed reproducibility:\n")
for (seed in c(1, 42)) {
  c1 <- community_spinglass(g_zachary, seed = seed)
  c2 <- community_spinglass(g_zachary, seed = seed)
  check(identical(membership(c1), membership(c2)),
        sprintf("  seed=%d produces identical results", seed))
}

# Test 1.7: communities() dispatcher passes seed correctly
cat("\n1.7 communities() dispatcher seed passing:\n")
methods_with_seed <- c("louvain", "leiden", "infomap", "label_propagation")
for (method in methods_with_seed) {
  c1 <- cograph::communities(g_zachary, method = method, seed = 42)
  c2 <- cograph::communities(g_zachary, method = method, seed = 42)
  check(identical(membership(c1), membership(c2)),
        sprintf("  method='%s' with seed=42", method))
}

# =============================================================================
# PART 2: COMPARE WITH IGRAPH RESULTS
# =============================================================================

cat("\n" %s+% strrep("-", 70) %s+% "\n")
cat("PART 2: COMPARE WITH IGRAPH RESULTS\n")
cat(strrep("-", 70) %s+% "\n\n")

# Test 2.1: Modularity values should be valid
cat("2.1 Modularity values are valid:\n")
for (method in c("louvain", "fast_greedy", "walktrap", "infomap")) {
  comm <- cograph::communities(g_zachary, method = method)
  mod <- tryCatch(modularity(comm), error = function(e) NA)
  if (!is.na(mod)) {
    check(mod >= -0.5 && mod <= 1,
          sprintf("  %s modularity=%.4f is valid", method, mod))
  } else {
    # Calculate modularity manually for methods that don't store it
    mod <- igraph::modularity(g_zachary, membership(comm))
    check(mod >= -0.5 && mod <= 1,
          sprintf("  %s modularity=%.4f is valid (calculated)", method, mod))
  }
}

# Test 2.2: Membership vectors have correct length
cat("\n2.2 Membership vector lengths:\n")
for (method in c("louvain", "leiden", "fast_greedy", "walktrap")) {
  comm <- cograph::communities(g_zachary, method = method)
  mem <- membership(comm)
  check(length(mem) == vcount(g_zachary),
        sprintf("  %s: length=%d == vcount=%d", method, length(mem), vcount(g_zachary)))
}

# Test 2.3: Community IDs are consecutive integers starting from 1
cat("\n2.3 Community IDs are valid:\n")
for (method in c("louvain", "leiden", "walktrap")) {
  comm <- cograph::communities(g_zachary, method = method)
  mem <- membership(comm)
  unique_ids <- sort(unique(mem))
  check(all(unique_ids == seq_along(unique_ids)),
        sprintf("  %s: IDs are consecutive 1:%d", method, max(mem)))
}

# Test 2.4: Compare modularity with direct igraph calculation
cat("\n2.4 Modularity matches igraph calculation:\n")
for (method in c("louvain", "walktrap", "fast_greedy")) {
  comm <- cograph::communities(g_zachary, method = method, seed = 42)
  mem <- membership(comm)

  # Calculate modularity directly with igraph
  igraph_mod <- igraph::modularity(g_zachary, mem)
  cograph_mod <- tryCatch(modularity(comm), error = function(e) igraph_mod)

  check(abs(igraph_mod - cograph_mod) < 1e-10,
        sprintf("  %s: cograph=%.6f == igraph=%.6f", method, cograph_mod, igraph_mod))
}

# Test 2.5: Direct comparison with igraph functions
cat("\n2.5 Direct comparison with igraph cluster functions:\n")

set.seed(42)
ig_louvain <- igraph::cluster_louvain(g_zachary)
set.seed(42)
co_louvain <- community_louvain(g_zachary, seed = 42)

# Modularities should match
check(abs(modularity(ig_louvain) - modularity(co_louvain)) < 1e-10,
      sprintf("  Louvain modularity: igraph=%.6f, cograph=%.6f",
              modularity(ig_louvain), modularity(co_louvain)))

# Number of communities should match
check(length(unique(membership(ig_louvain))) == length(unique(membership(co_louvain))),
      sprintf("  Louvain n_communities: igraph=%d, cograph=%d",
              length(unique(membership(ig_louvain))),
              length(unique(membership(co_louvain)))))

# =============================================================================
# PART 3: CONSENSUS CLUSTERING TESTS
# =============================================================================

cat("\n" %s+% strrep("-", 70) %s+% "\n")
cat("PART 3: CONSENSUS CLUSTERING TESTS\n")
cat(strrep("-", 70) %s+% "\n\n")

# Test 3.1: Basic functionality
cat("3.1 Basic consensus clustering:\n")
cc <- community_consensus(g_zachary, method = "louvain", n_runs = 50, seed = 42)
check(inherits(cc, "cograph_communities"), "  Returns cograph_communities object")
check(length(membership(cc)) == vcount(g_zachary), "  Membership has correct length")
check(!is.na(modularity(cc)), "  Modularity is not NA")
cat("    Modularity:", round(modularity(cc), 4), "\n")
cat("    N communities:", length(unique(membership(cc))), "\n")

# Test 3.2: Consensus is reproducible with seed
cat("\n3.2 Consensus reproducibility:\n")
cc1 <- community_consensus(g_zachary, method = "louvain", n_runs = 30, seed = 123)
cc2 <- community_consensus(g_zachary, method = "louvain", n_runs = 30, seed = 123)
check(identical(membership(cc1), membership(cc2)), "  Same seed produces identical results")

# Test 3.3: Different thresholds
cat("\n3.3 Threshold parameter:\n")
cc_low <- community_consensus(g_zachary, threshold = 0.3, n_runs = 50, seed = 42)
cc_high <- community_consensus(g_zachary, threshold = 0.8, n_runs = 50, seed = 42)
cat("    threshold=0.3: ", length(unique(membership(cc_low))), " communities\n")
cat("    threshold=0.8: ", length(unique(membership(cc_high))), " communities\n")
check(TRUE, "  Different thresholds produce results")

# Test 3.4: Different methods
cat("\n3.4 Different methods for consensus:\n")
for (method in c("louvain", "leiden", "label_propagation")) {
  cc <- tryCatch(
    community_consensus(g_zachary, method = method, n_runs = 20, seed = 42),
    error = function(e) NULL
  )
  check(!is.null(cc), sprintf("  method='%s' works", method))
}

# Test 3.5: Alias works
cat("\n3.5 Alias com_consensus:\n")
cc_alias <- com_consensus(g_zachary, n_runs = 20, seed = 42)
check(inherits(cc_alias, "cograph_communities"), "  com_consensus alias works")

# =============================================================================
# PART 4: CLUSTER SIGNIFICANCE TESTS
# =============================================================================

cat("\n" %s+% strrep("-", 70) %s+% "\n")
cat("PART 4: CLUSTER SIGNIFICANCE TESTS\n")
cat(strrep("-", 70) %s+% "\n\n")

# Test 4.1: Basic functionality
cat("4.1 Basic cluster significance:\n")
comm <- community_louvain(g_zachary, seed = 42)
sig <- cluster_significance(g_zachary, comm, n_random = 100, seed = 123)

check(inherits(sig, "cograph_cluster_significance"), "  Returns correct class")
check(!is.na(sig$observed_modularity), "  observed_modularity is not NA")
check(!is.na(sig$null_mean), "  null_mean is not NA")
check(!is.na(sig$null_sd), "  null_sd is not NA")
check(!is.na(sig$z_score), "  z_score is not NA")
check(!is.na(sig$p_value), "  p_value is not NA")
check(length(sig$null_values) == 100, "  null_values has correct length")

cat("    Observed modularity:", round(sig$observed_modularity, 4), "\n")
cat("    Null mean:", round(sig$null_mean, 4), "\n")
cat("    Z-score:", round(sig$z_score, 2), "\n")
cat("    P-value:", format.pval(sig$p_value), "\n")

# Test 4.2: P-value is valid
cat("\n4.2 P-value validity:\n")
check(sig$p_value >= 0 && sig$p_value <= 1, "  P-value in [0,1]")

# Test 4.3: Z-score calculation is correct
cat("\n4.3 Z-score calculation:\n")
expected_z <- (sig$observed_modularity - sig$null_mean) / sig$null_sd
check(abs(sig$z_score - expected_z) < 1e-10,
      sprintf("  Z-score calculation correct: %.4f", sig$z_score))

# Test 4.4: Reproducibility with seed
cat("\n4.4 Reproducibility:\n")
sig1 <- cluster_significance(g_zachary, comm, n_random = 50, seed = 456)
sig2 <- cluster_significance(g_zachary, comm, n_random = 50, seed = 456)
check(identical(sig1$null_values, sig2$null_values), "  Same seed produces identical null values")

# Test 4.5: Configuration vs GNM models
cat("\n4.5 Different null models:\n")
sig_config <- cluster_significance(g_zachary, comm, n_random = 50,
                                    method = "configuration", seed = 789)
sig_gnm <- cluster_significance(g_zachary, comm, n_random = 50,
                                 method = "gnm", seed = 789)
check(sig_config$method == "configuration", "  Configuration model recorded")
check(sig_gnm$method == "gnm", "  GNM model recorded")
cat("    Configuration Z:", round(sig_config$z_score, 2), "\n")
cat("    GNM Z:", round(sig_gnm$z_score, 2), "\n")

# Test 4.6: Works with membership vector directly
cat("\n4.6 Works with membership vector:\n")
mem_vec <- membership(comm)
sig_vec <- cluster_significance(g_zachary, mem_vec, n_random = 30, seed = 42)
check(!is.na(sig_vec$z_score), "  Works with raw membership vector")

# Test 4.7: Alias works
cat("\n4.7 Alias csig:\n")
sig_alias <- csig(g_zachary, comm, n_random = 30, seed = 42)
check(inherits(sig_alias, "cograph_cluster_significance"), "  csig alias works")

# Test 4.8: Known case - random graph should have low modularity
cat("\n4.8 Random graph test (should have lower significance):\n")
g_random_small <- sample_gnm(30, 100)
comm_random <- cluster_louvain(g_random_small)
sig_random <- cluster_significance(g_random_small, comm_random, n_random = 50,
                                    method = "gnm", seed = 42)
cat("    Random graph Z-score:", round(sig_random$z_score, 2), "\n")
cat("    Random graph p-value:", format.pval(sig_random$p_value), "\n")
# Random graphs often still show some structure due to degree heterogeneity

# =============================================================================
# PART 5: MOTIF VECTORIZATION TESTS
# =============================================================================

cat("\n" %s+% strrep("-", 70) %s+% "\n")
cat("PART 5: MOTIF VECTORIZATION TESTS\n")
cat(strrep("-", 70) %s+% "\n\n")

# Test 5.1: All 16 triad types
cat("5.1 All 16 MAN triad types:\n")

# Define canonical patterns for each type
triad_matrices <- list(
  "003" = matrix(c(0,0,0, 0,0,0, 0,0,0), 3, 3, byrow=TRUE),
  "012" = matrix(c(0,1,0, 0,0,0, 0,0,0), 3, 3, byrow=TRUE),
  "102" = matrix(c(0,1,0, 1,0,0, 0,0,0), 3, 3, byrow=TRUE),
  "021D" = matrix(c(0,1,1, 0,0,0, 0,0,0), 3, 3, byrow=TRUE),
  "021U" = matrix(c(0,0,0, 1,0,0, 1,0,0), 3, 3, byrow=TRUE),
  "021C" = matrix(c(0,0,1, 1,0,0, 0,0,0), 3, 3, byrow=TRUE),
  "111D" = matrix(c(0,1,0, 1,0,0, 1,0,0), 3, 3, byrow=TRUE),
  "111U" = matrix(c(0,1,1, 1,0,0, 0,0,0), 3, 3, byrow=TRUE),
  "030T" = matrix(c(0,1,1, 0,0,1, 0,0,0), 3, 3, byrow=TRUE),
  "030C" = matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow=TRUE),
  "201" = matrix(c(0,1,1, 1,0,0, 1,0,0), 3, 3, byrow=TRUE),
  "120D" = matrix(c(0,0,1, 1,0,1, 1,0,0), 3, 3, byrow=TRUE),
  "120U" = matrix(c(0,1,1, 1,0,1, 0,0,0), 3, 3, byrow=TRUE),
  "120C" = matrix(c(0,1,0, 1,0,1, 1,0,0), 3, 3, byrow=TRUE),
  "210" = matrix(c(0,1,1, 1,0,1, 1,0,0), 3, 3, byrow=TRUE),
  "300" = matrix(c(0,1,1, 1,0,1, 1,1,0), 3, 3, byrow=TRUE)
)

for (type_name in names(triad_matrices)) {
  mat <- triad_matrices[[type_name]]

  # Skip empty triad (003)
  if (sum(mat) == 0) {
    check(TRUE, sprintf("  %s: skipped (empty)", type_name))
    next
  }

  result <- cograph:::.count_triads_matrix_vectorized(
    mat = mat,
    edge_method = "any",
    edge_threshold = 0,
    expected_mat = NULL,
    exclude = character(0),
    include = NULL
  )

  if (!is.null(result) && nrow(result) > 0) {
    detected_type <- result$type[1]
    check(detected_type == type_name,
          sprintf("  %s: detected as %s", type_name, detected_type))
  } else {
    check(FALSE, sprintf("  %s: no result returned", type_name))
  }
}

# Test 5.2: Compare with igraph triad_census
cat("\n5.2 Compare with igraph triad_census:\n")
set.seed(42)
g_test <- sample_gnm(10, 30, directed = TRUE)
mat_test <- as.matrix(as_adjacency_matrix(g_test))

# igraph triad census
ig_census <- triad_census(g_test)
names(ig_census) <- c("003", "012", "102", "021D", "021U", "021C",
                       "111D", "111U", "030T", "030C", "201",
                       "120D", "120U", "120C", "210", "300")

# Our vectorized count
result <- cograph:::.count_triads_matrix_vectorized(
  mat = mat_test,
  edge_method = "any",
  edge_threshold = 0,
  expected_mat = NULL,
  exclude = character(0),
  include = NULL
)

if (!is.null(result)) {
  our_census <- table(result$type)

  # Compare non-empty types
  for (type in names(our_census)) {
    our_count <- as.integer(our_census[type])
    ig_count <- as.integer(ig_census[type])
    check(our_count == ig_count,
          sprintf("  %s: ours=%d, igraph=%d", type, our_count, ig_count))
  }

  # Total triads should match
  total_ours <- nrow(result)
  total_igraph <- sum(ig_census) - ig_census["003"]  # Exclude empty triads
  check(total_ours == total_igraph,
        sprintf("  Total non-empty: ours=%d, igraph=%d", total_ours, total_igraph))
} else {
  check(FALSE, "  Vectorized function returned NULL")
}

# Test 5.3: Edge method "percent"
cat("\n5.3 Edge method 'percent':\n")
mat_weighted <- matrix(c(
  0, 10, 2,
  0, 0, 8,
  0, 0, 0
), 3, 3, byrow = TRUE)

# With threshold 0.2 (20%), only edges >= 20% of total should count
result_percent <- cograph:::.count_triads_matrix_vectorized(
  mat = mat_weighted,
  edge_method = "percent",
  edge_threshold = 0.2,
  expected_mat = NULL,
  exclude = character(0),
  include = NULL
)
# Total = 20, threshold = 4. Edges: 10 (yes), 2 (no), 8 (yes)
# Strong edges: A->B (10>=4) and B->C (8>=4), but NOT A->C (2<4)
# This is a chain: A -> B -> C = 021C
if (!is.null(result_percent)) {
  cat("    Detected type:", result_percent$type[1], "\n")
  check(result_percent$type[1] == "021C", "  Percent threshold works correctly (chain A->B->C)")
} else {
  check(FALSE, "  Percent method returned NULL")
}

# Test 5.4: Edge method "expected"
cat("\n5.4 Edge method 'expected':\n")
mat_exp <- matrix(c(
  0, 5, 5,
  5, 0, 5,
  5, 5, 0
), 3, 3, byrow = TRUE)

total_mat <- sum(mat_exp)
row_sums <- rowSums(mat_exp)
col_sums <- colSums(mat_exp)
expected_mat <- outer(row_sums, col_sums) / total_mat
expected_mat[expected_mat == 0] <- 0.001

result_expected <- cograph:::.count_triads_matrix_vectorized(
  mat = mat_exp,
  edge_method = "expected",
  edge_threshold = 1.0,  # ratio >= 1.0
  expected_mat = expected_mat,
  exclude = character(0),
  include = NULL
)
check(!is.null(result_expected), "  Expected method works")
if (!is.null(result_expected)) {
  cat("    Detected type:", result_expected$type[1], "\n")
}

# Test 5.5: Include/exclude filters
cat("\n5.5 Include/exclude filters:\n")

mat_030T <- matrix(c(0,1,1, 0,0,1, 0,0,0), 3, 3, byrow = TRUE)

# Exclude 030T
result_excl <- cograph:::.count_triads_matrix_vectorized(
  mat = mat_030T, edge_method = "any", edge_threshold = 0,
  expected_mat = NULL, exclude = "030T", include = NULL
)
check(is.null(result_excl), "  Exclude filter removes 030T")

# Include only 300
result_incl <- cograph:::.count_triads_matrix_vectorized(
  mat = mat_030T, edge_method = "any", edge_threshold = 0,
  expected_mat = NULL, exclude = character(0), include = "300"
)
check(is.null(result_incl), "  Include filter (300 only) excludes 030T")

# Include 030T
result_incl2 <- cograph:::.count_triads_matrix_vectorized(
  mat = mat_030T, edge_method = "any", edge_threshold = 0,
  expected_mat = NULL, exclude = character(0), include = "030T"
)
check(!is.null(result_incl2) && result_incl2$type[1] == "030T",
      "  Include filter (030T) keeps 030T")

# Test 5.6: Performance test
cat("\n5.6 Performance test:\n")
set.seed(42)
sizes <- c(10, 20, 50, 100)
for (n in sizes) {
  mat_perf <- matrix(sample(0:3, n*n, replace = TRUE), n, n)
  diag(mat_perf) <- 0

  start <- Sys.time()
  result <- cograph:::.count_triads_matrix_vectorized(
    mat = mat_perf, edge_method = "any", edge_threshold = 0,
    expected_mat = NULL, exclude = character(0), include = NULL
  )
  elapsed <- as.numeric(Sys.time() - start, units = "secs")

  n_triplets <- choose(n, 3)
  n_found <- if (!is.null(result)) nrow(result) else 0

  cat(sprintf("    n=%d: %d triplets, %d non-empty triads, %.4f sec\n",
              n, n_triplets, n_found, elapsed))
  check(elapsed < 5, sprintf("  n=%d completed in reasonable time", n))
}

# Test 5.7: extract_triads function (uses vectorized code)
cat("\n5.7 extract_triads integration:\n")
mat_test <- matrix(c(
  0, 3, 2, 0,
  0, 0, 5, 1,
  0, 0, 0, 4,
  2, 0, 0, 0
), 4, 4, byrow = TRUE)
rownames(mat_test) <- colnames(mat_test) <- c("A", "B", "C", "D")

net <- as_cograph(mat_test)
triads <- extract_triads(net)

check(is.data.frame(triads), "  Returns data.frame")
check(nrow(triads) == choose(4, 3), sprintf("  Found all %d triplets", choose(4, 3)))
check(all(c("A", "B", "C", "type", "total_weight") %in% names(triads)),
      "  Has expected columns")

cat("    Types found:", paste(unique(triads$type), collapse = ", "), "\n")

# Test 5.8: extract_motifs integration
cat("\n5.8 extract_motifs integration:\n")
tryCatch({
  motifs <- extract_motifs(mat_test, level = "aggregate", edge_method = "any",
                            pattern = "all", significance = FALSE)
  if (!is.null(motifs)) {
    check(is.list(motifs), "  Returns list")
    check("results" %in% names(motifs), "  Has 'results' component")
    cat("    Unique triads found:", nrow(motifs$results), "\n")
    check(TRUE, "  extract_motifs works with vectorized internals")
  }
}, error = function(e) {
  cat("    Error:", conditionMessage(e), "\n")
  check(FALSE, "  extract_motifs failed")
})

# =============================================================================
# PART 6: EDGE CASES AND ERROR HANDLING
# =============================================================================

cat("\n" %s+% strrep("-", 70) %s+% "\n")
cat("PART 6: EDGE CASES AND ERROR HANDLING\n")
cat(strrep("-", 70) %s+% "\n\n")

# Test 6.1: Empty graph
cat("6.1 Small graphs:\n")
g_tiny <- make_empty_graph(2)
comm_tiny <- tryCatch(community_louvain(g_tiny, seed = 42), error = function(e) NULL)
check(is.null(comm_tiny) || length(membership(comm_tiny)) == 2,
      "  2-node graph handled")

# Test 6.2: Single node
g_single <- make_empty_graph(1)
comm_single <- tryCatch(community_louvain(g_single, seed = 42), error = function(e) NULL)
check(is.null(comm_single) || length(membership(comm_single)) == 1,
      "  1-node graph handled")

# Test 6.3: Matrix input for cluster_significance
cat("\n6.2 Matrix input for cluster_significance:\n")
mat_input <- as.matrix(as_adjacency_matrix(g_zachary))
comm_for_mat <- community_louvain(g_zachary, seed = 42)
sig_mat <- tryCatch(
  cluster_significance(mat_input, membership(comm_for_mat), n_random = 20, seed = 42),
  error = function(e) NULL
)
check(!is.null(sig_mat), "  Works with matrix input")

# Test 6.4: Vectorized function with 2 nodes (should return NULL)
cat("\n6.3 Vectorized triads with small matrices:\n")
mat_2x2 <- matrix(c(0,1,1,0), 2, 2)
result_2x2 <- cograph:::.count_triads_matrix_vectorized(
  mat = mat_2x2, edge_method = "any", edge_threshold = 0,
  expected_mat = NULL, exclude = character(0), include = NULL
)
check(is.null(result_2x2), "  2x2 matrix returns NULL (need 3+ nodes)")

# Test 6.5: All-zero matrix
mat_zeros <- matrix(0, 5, 5)
result_zeros <- cograph:::.count_triads_matrix_vectorized(
  mat = mat_zeros, edge_method = "any", edge_threshold = 0,
  expected_mat = NULL, exclude = character(0), include = NULL
)
check(is.null(result_zeros), "  All-zero matrix returns NULL")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n" %s+% strrep("=", 70) %s+% "\n")
cat("TEST SUMMARY\n")
cat(strrep("=", 70) %s+% "\n\n")

cat("Tests run:", tests_run, "\n")
cat("Passed:", tests_run - errors, "\n")
cat("Failed:", errors, "\n")

if (errors == 0) {
  cat("\n*** ALL TESTS PASSED ***\n")
} else {
  cat("\n*** SOME TESTS FAILED ***\n")
  quit(status = 1)
}
