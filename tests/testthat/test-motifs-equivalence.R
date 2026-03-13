# Equivalence tests: cograph motif functions vs igraph
# Verifies numerical equivalence of triad census, motif census, triad
# classification, and triangle counts against igraph as ground truth.

# =============================================================================
# Test matrices
# =============================================================================

# Dense directed — 4 nodes with cycle
mat4 <- matrix(c(
  0, 1, 1, 0,
  0, 0, 1, 1,
  0, 0, 0, 1,
  1, 0, 0, 0
), 4, 4, byrow = TRUE)
rownames(mat4) <- colnames(mat4) <- c("A", "B", "C", "D")

# Sparse directed — 5 nodes
mat5 <- matrix(c(
  0, 1, 0, 0, 0,
  0, 0, 1, 0, 0,
  1, 0, 0, 1, 0,
  0, 0, 0, 0, 1,
  0, 1, 0, 0, 0
), 5, 5, byrow = TRUE)
rownames(mat5) <- colnames(mat5) <- c("P", "Q", "R", "S", "T")

# Random directed — 8 nodes (reproducible)
set.seed(123)
mat8 <- matrix(sample(0:1, 64, replace = TRUE, prob = c(0.6, 0.4)), 8, 8)
diag(mat8) <- 0
rownames(mat8) <- colnames(mat8) <- paste0("N", 1:8)

# Weighted TNA-like — 6 nodes (transition probabilities)
set.seed(456)
mat6w <- matrix(runif(36, 0, 0.5), 6, 6)
diag(mat6w) <- 0
mat6w <- mat6w / rowSums(mat6w)
rownames(mat6w) <- colnames(mat6w) <- c("plan", "exec", "monitor", "adapt", "eval", "reflect")

# Undirected — symmetric 5 nodes
mat5u <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 0, 1,
  0, 1, 0, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(mat5u) <- colnames(mat5u) <- c("a", "b", "c", "d", "e")

# Complete directed — 4 nodes (all edges present)
mat4_complete <- matrix(1, 4, 4)
diag(mat4_complete) <- 0
rownames(mat4_complete) <- colnames(mat4_complete) <- LETTERS[1:4]

# =============================================================================
# 1. triad_census() vs igraph::triad_census()
# =============================================================================

test_that("triad_census matches igraph exactly — mat4", {
  g <- igraph::graph_from_adjacency_matrix(mat4, mode = "directed")
  igraph_tc <- igraph::triad_census(g)
  names(igraph_tc) <- c("003", "012", "102", "021D", "021U", "021C",
                         "111D", "111U", "030T", "030C", "201",
                         "120D", "120U", "120C", "210", "300")
  cograph_tc <- triad_census(mat4)
  expect_identical(as.integer(cograph_tc), as.integer(igraph_tc))
})

test_that("triad_census matches igraph exactly — mat5", {
  g <- igraph::graph_from_adjacency_matrix(mat5, mode = "directed")
  igraph_tc <- igraph::triad_census(g)
  names(igraph_tc) <- c("003", "012", "102", "021D", "021U", "021C",
                         "111D", "111U", "030T", "030C", "201",
                         "120D", "120U", "120C", "210", "300")
  cograph_tc <- triad_census(mat5)
  expect_identical(as.integer(cograph_tc), as.integer(igraph_tc))
})

test_that("triad_census matches igraph exactly — mat8 (random dense)", {
  g <- igraph::graph_from_adjacency_matrix(mat8, mode = "directed")
  igraph_tc <- igraph::triad_census(g)
  names(igraph_tc) <- c("003", "012", "102", "021D", "021U", "021C",
                         "111D", "111U", "030T", "030C", "201",
                         "120D", "120U", "120C", "210", "300")
  cograph_tc <- triad_census(mat8)
  expect_identical(as.integer(cograph_tc), as.integer(igraph_tc))
})

test_that("triad_census matches igraph — complete graph", {
  g <- igraph::graph_from_adjacency_matrix(mat4_complete, mode = "directed")
  igraph_tc <- igraph::triad_census(g)
  names(igraph_tc) <- c("003", "012", "102", "021D", "021U", "021C",
                         "111D", "111U", "030T", "030C", "201",
                         "120D", "120U", "120C", "210", "300")
  cograph_tc <- triad_census(mat4_complete)
  expect_identical(as.integer(cograph_tc), as.integer(igraph_tc))
  # All triads in complete directed graph must be type 300 (clique)
  expect_equal(cograph_tc[["300"]], choose(4, 3))
  expect_equal(sum(cograph_tc[names(cograph_tc) != "300"]), 0)
})

test_that("triad_census matches igraph — weighted TNA matrix", {
  # Binarize (any edge > 0 → 1) for directed triad census
  bin <- (mat6w > 0) * 1
  g <- igraph::graph_from_adjacency_matrix(bin, mode = "directed")
  igraph_tc <- igraph::triad_census(g)
  names(igraph_tc) <- c("003", "012", "102", "021D", "021U", "021C",
                         "111D", "111U", "030T", "030C", "201",
                         "120D", "120U", "120C", "210", "300")
  cograph_tc <- triad_census(bin)
  expect_identical(as.integer(cograph_tc), as.integer(igraph_tc))
})

test_that("triad_census total equals choose(n, 3)", {
  # Sum of all triad types must equal C(n, 3)
  expect_equal(sum(triad_census(mat4)), choose(4, 3))
  expect_equal(sum(triad_census(mat5)), choose(5, 3))
  expect_equal(sum(triad_census(mat8)), choose(8, 3))
})

# =============================================================================
# 2. motif_census()$counts vs igraph::motifs()
# =============================================================================

test_that("motif_census observed counts match igraph::motifs — mat4", {
  g <- igraph::graph_from_adjacency_matrix(mat4, mode = "directed", weighted = TRUE)
  igraph_m <- igraph::motifs(g, size = 3)
  igraph_m[is.na(igraph_m)] <- 0

  mc <- motif_census(mat4, size = 3, n_random = 10, seed = 1)
  cograph_m <- as.integer(mc$counts)

  expect_identical(cograph_m, as.integer(igraph_m))
})

test_that("motif_census observed counts match igraph::motifs — mat5", {
  g <- igraph::graph_from_adjacency_matrix(mat5, mode = "directed", weighted = TRUE)
  igraph_m <- igraph::motifs(g, size = 3)
  igraph_m[is.na(igraph_m)] <- 0

  mc <- motif_census(mat5, size = 3, n_random = 10, seed = 1)
  cograph_m <- as.integer(mc$counts)

  expect_identical(cograph_m, as.integer(igraph_m))
})

test_that("motif_census observed counts match igraph::motifs — mat8", {
  g <- igraph::graph_from_adjacency_matrix(mat8, mode = "directed", weighted = TRUE)
  igraph_m <- igraph::motifs(g, size = 3)
  igraph_m[is.na(igraph_m)] <- 0

  mc <- motif_census(mat8, size = 3, n_random = 10, seed = 1)
  cograph_m <- as.integer(mc$counts)

  expect_identical(cograph_m, as.integer(igraph_m))
})

test_that("motif_census observed counts match igraph::motifs — weighted matrix", {
  g <- igraph::graph_from_adjacency_matrix(mat6w, mode = "directed", weighted = TRUE)
  igraph_m <- igraph::motifs(g, size = 3)
  igraph_m[is.na(igraph_m)] <- 0

  mc <- motif_census(mat6w, size = 3, n_random = 10, seed = 1)
  cograph_m <- as.integer(mc$counts)

  expect_identical(cograph_m, as.integer(igraph_m))
})

# =============================================================================
# 3. Undirected: triangle count vs igraph
# =============================================================================

test_that("motif_census undirected triangle count matches igraph — mat5u", {
  g <- igraph::graph_from_adjacency_matrix(mat5u, mode = "undirected")
  igraph_triangles <- sum(igraph::count_triangles(g)) / 3

  mc <- motif_census(mat5u, directed = FALSE, n_random = 10, seed = 1)
  cograph_triangles <- mc$counts[["triangle"]]

  expect_equal(cograph_triangles, igraph_triangles)
})

test_that("motif_census undirected wedge + triangle + empty = choose(n, 3)", {
  mc <- motif_census(mat5u, directed = FALSE, n_random = 10, seed = 1)
  total <- sum(mc$counts)
  expect_equal(total, choose(5, 3))
})

test_that("motif_census undirected triangle count — complete graph", {
  full_u <- matrix(1, 5, 5)
  diag(full_u) <- 0
  mc <- motif_census(full_u, directed = FALSE, n_random = 10, seed = 1)
  expect_equal(mc$counts[["triangle"]], choose(5, 3))
  expect_equal(mc$counts[["empty"]], 0)
  expect_equal(mc$counts[["wedge"]], 0)
})

test_that("motif_census undirected triangle count — star graph (no triangles)", {
  star <- matrix(0, 5, 5)
  star[1, ] <- 1; star[, 1] <- 1; diag(star) <- 0
  mc <- motif_census(star, directed = FALSE, n_random = 10, seed = 1)
  expect_equal(mc$counts[["triangle"]], 0)
  # Wedges: each pair of leaves forms a wedge through center = C(4,2) = 6
  expect_equal(mc$counts[["wedge"]], choose(4, 2))
})

# =============================================================================
# 4. extract_triads() classification consistency with triad_census()
# =============================================================================

test_that("extract_triads type counts match triad_census — mat4", {
  tc <- triad_census(mat4)
  triads <- extract_triads(mat4, min_total = 0, threshold = 0)

  # Count triads per type from extract_triads
  et_counts <- table(triads$type)

  # Every non-empty type in extract_triads should match triad_census
  # (extract_triads excludes type "003" since those have no edges)
  for (type_name in names(et_counts)) {
    expect_equal(as.integer(et_counts[type_name]),
                 as.integer(tc[type_name]),
                 info = paste("Type", type_name))
  }

  # Total triads from extract_triads + 003 count = choose(n, 3)
  n003 <- tc[["003"]]
  expect_equal(nrow(triads) + n003, choose(4, 3))
})

test_that("extract_triads type counts match triad_census — mat5", {
  tc <- triad_census(mat5)
  triads <- extract_triads(mat5, min_total = 0, threshold = 0)

  et_counts <- table(triads$type)
  for (type_name in names(et_counts)) {
    expect_equal(as.integer(et_counts[type_name]),
                 as.integer(tc[type_name]),
                 info = paste("Type", type_name))
  }
  n003 <- tc[["003"]]
  expect_equal(nrow(triads) + n003, choose(5, 3))
})

test_that("extract_triads type counts match triad_census — mat8", {
  tc <- triad_census(mat8)
  triads <- extract_triads(mat8, min_total = 0, threshold = 0)

  et_counts <- table(triads$type)
  for (type_name in names(et_counts)) {
    expect_equal(as.integer(et_counts[type_name]),
                 as.integer(tc[type_name]),
                 info = paste("Type", type_name))
  }
  n003 <- tc[["003"]]
  expect_equal(nrow(triads) + n003, choose(8, 3))
})

test_that("extract_triads type counts match triad_census — complete graph", {
  tc <- triad_census(mat4_complete)
  triads <- extract_triads(mat4_complete, min_total = 0, threshold = 0)

  # All triads should be type 300
  expect_true(all(triads$type == "300"))
  expect_equal(nrow(triads), choose(4, 3))
})

# =============================================================================
# 5. extract_triads() weight extraction accuracy
# =============================================================================

test_that("extract_triads weights match matrix values", {
  triads <- extract_triads(mat4, min_total = 0, threshold = 0)

  # Check each row: weights should match mat4[A,B], mat4[B,A], etc.
  for (r in seq_len(nrow(triads))) {
    a <- triads$A[r]; b <- triads$B[r]; c <- triads$C[r]
    expect_equal(triads$weight_AB[r], mat4[a, b], info = paste(a, "->", b))
    expect_equal(triads$weight_BA[r], mat4[b, a], info = paste(b, "->", a))
    expect_equal(triads$weight_AC[r], mat4[a, c], info = paste(a, "->", c))
    expect_equal(triads$weight_CA[r], mat4[c, a], info = paste(c, "->", a))
    expect_equal(triads$weight_BC[r], mat4[b, c], info = paste(b, "->", c))
    expect_equal(triads$weight_CB[r], mat4[c, b], info = paste(c, "->", b))
  }
})

test_that("extract_triads total_weight is sum of 6 edges", {
  triads <- extract_triads(mat6w, min_total = 0, threshold = 0)

  for (r in seq_len(nrow(triads))) {
    expected_total <- triads$weight_AB[r] + triads$weight_BA[r] +
                      triads$weight_AC[r] + triads$weight_CA[r] +
                      triads$weight_BC[r] + triads$weight_CB[r]
    expect_equal(triads$total_weight[r], expected_total,
                 tolerance = 1e-10, info = paste("Row", r))
  }
})

# =============================================================================
# 6. Vectorized classification vs brute-force classification
# =============================================================================

test_that("vectorized triad classification matches brute-force — all 64 codes", {
  # For each possible 6-bit edge code, classify via vectorized method
  # and verify against igraph's triad_census on the same 3-node graph

  man_names <- c("003", "012", "102", "021D", "021U", "021C",
                 "111D", "111U", "030T", "030C", "201",
                 "120D", "120U", "120C", "210", "300")

  for (code in 0:63) {
    bits <- as.integer(intToBits(code)[1:6])
    adj <- matrix(0L, 3, 3)
    adj[1, 2] <- bits[1]
    adj[2, 1] <- bits[2]
    adj[1, 3] <- bits[3]
    adj[3, 1] <- bits[4]
    adj[2, 3] <- bits[5]
    adj[3, 2] <- bits[6]

    # cograph vectorized classification
    cograph_type <- .classify_triads_vectorized(
      bits[1], bits[2], bits[3], bits[4], bits[5], bits[6]
    )

    # igraph ground truth
    g <- igraph::graph_from_adjacency_matrix(adj, mode = "directed")
    ig_tc <- igraph::triad_census(g)
    ig_type <- man_names[which(ig_tc == 1)]

    # If no edges, igraph returns 003
    if (length(ig_type) == 0) ig_type <- "003"

    expect_equal(cograph_type, ig_type,
                 info = sprintf("code=%d, bits=%s", code,
                                paste(bits, collapse = "")))
  }
})

# =============================================================================
# 7. motif_census reproducibility with seed
# =============================================================================

test_that("motif_census is reproducible with same seed", {
  mc1 <- motif_census(mat8, n_random = 50, seed = 99)
  mc2 <- motif_census(mat8, n_random = 50, seed = 99)

  expect_identical(mc1$counts, mc2$counts)
  expect_identical(mc1$null_mean, mc2$null_mean)
  expect_identical(mc1$null_sd, mc2$null_sd)
  expect_identical(mc1$z_scores, mc2$z_scores)
  expect_identical(mc1$p_values, mc2$p_values)
})

test_that("motif_census different seeds give different null distributions", {
  mc1 <- motif_census(mat8, n_random = 50, seed = 1)
  mc2 <- motif_census(mat8, n_random = 50, seed = 2)

  # Observed counts are the same (deterministic)
  expect_identical(mc1$counts, mc2$counts)
  # But null distributions differ
  expect_false(identical(mc1$null_mean, mc2$null_mean))
})

test_that("motif_census does not alter global RNG state", {
  set.seed(42)
  before <- runif(1)
  set.seed(42)
  motif_census(mat4, n_random = 10, seed = 999)
  after <- runif(1)
  expect_equal(before, after)
})

# =============================================================================
# 8. igraph object input equivalence
# =============================================================================

test_that("triad_census(igraph) matches triad_census(matrix)", {
  g <- igraph::graph_from_adjacency_matrix(mat8, mode = "directed")
  tc_mat <- triad_census(mat8)
  tc_ig <- triad_census(g)
  expect_identical(as.integer(tc_mat), as.integer(tc_ig))
})

test_that("motif_census(igraph) counts match motif_census(matrix)", {
  g <- igraph::graph_from_adjacency_matrix(mat4, mode = "directed", weighted = TRUE)
  mc_mat <- motif_census(mat4, n_random = 10, seed = 1)
  mc_ig <- motif_census(g, n_random = 10, seed = 1)
  expect_identical(as.integer(mc_mat$counts), as.integer(mc_ig$counts))
})

# =============================================================================
# 9. Real TNA data equivalence (group_regulation)
# =============================================================================

test_that("triad_census on group_regulation matches igraph", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(tna::group_regulation)
  w <- Mod$weights

  g <- igraph::graph_from_adjacency_matrix(w, mode = "directed", weighted = TRUE)
  igraph_tc <- igraph::triad_census(g)
  names(igraph_tc) <- c("003", "012", "102", "021D", "021U", "021C",
                         "111D", "111U", "030T", "030C", "201",
                         "120D", "120U", "120C", "210", "300")
  cograph_tc <- triad_census(w)
  expect_identical(as.integer(cograph_tc), as.integer(igraph_tc))
})

test_that("motif_census on group_regulation counts match igraph", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(tna::group_regulation)
  w <- Mod$weights

  g <- igraph::graph_from_adjacency_matrix(w, mode = "directed", weighted = TRUE)
  igraph_m <- igraph::motifs(g, size = 3)
  igraph_m[is.na(igraph_m)] <- 0

  mc <- motif_census(w, size = 3, n_random = 10, seed = 1)
  expect_identical(as.integer(mc$counts), as.integer(igraph_m))
})

test_that("extract_triads on group_regulation consistent with triad_census", {
  skip_if_not_installed("tna")
  Mod <- tna::tna(tna::group_regulation)
  w <- Mod$weights

  tc <- triad_census(w)
  # Use threshold=0 but note: weighted matrix — all edges are > 0
  triads <- extract_triads(w, min_total = 0, threshold = 0)

  et_counts <- table(triads$type)
  for (type_name in names(et_counts)) {
    expect_equal(as.integer(et_counts[type_name]),
                 as.integer(tc[type_name]),
                 info = paste("Type", type_name))
  }
  n003 <- tc[["003"]]
  n <- nrow(w)
  expect_equal(nrow(triads) + n003, choose(n, 3))
})

# =============================================================================
# 10. Edge cases
# =============================================================================

test_that("triad_census on empty graph is all 003", {
  empty <- matrix(0, 4, 4)
  rownames(empty) <- colnames(empty) <- LETTERS[1:4]
  g <- igraph::graph_from_adjacency_matrix(empty, mode = "directed")
  igraph_tc <- igraph::triad_census(g)
  cograph_tc <- triad_census(empty)
  expect_identical(as.integer(cograph_tc), as.integer(igraph_tc))
  expect_equal(cograph_tc[["003"]], choose(4, 3))
})

test_that("triad_census on single-edge graph", {
  one_edge <- matrix(0, 4, 4)
  one_edge[1, 2] <- 1
  rownames(one_edge) <- colnames(one_edge) <- LETTERS[1:4]
  g <- igraph::graph_from_adjacency_matrix(one_edge, mode = "directed")
  igraph_tc <- igraph::triad_census(g)
  cograph_tc <- triad_census(one_edge)
  expect_identical(as.integer(cograph_tc), as.integer(igraph_tc))
})

test_that("extract_triads returns empty data.frame for 2-node graph", {
  tiny <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(tiny) <- colnames(tiny) <- c("X", "Y")
  result <- extract_triads(tiny)
  expect_equal(nrow(result), 0)
  expect_true(all(c("A", "B", "C", "type", "total_weight") %in% names(result)))
})
