# Tests for build_mcml()

# ==============================================================================
# Test Data
# ==============================================================================

# Simple edge list
edges_simple <- data.frame(
  from   = c("A", "A", "B", "C", "C", "D", "A", "D"),
  to     = c("B", "C", "A", "D", "D", "A", "D", "C"),
  stringsAsFactors = FALSE
)

# Weighted edge list
edges_weighted <- data.frame(
  from   = c("A", "A", "B", "C", "C", "D"),
  to     = c("B", "C", "A", "D", "D", "A"),
  weight = c(1,   2,   1,   3,   1,   2),
  stringsAsFactors = FALSE
)

# Edge list with group column
edges_grouped <- data.frame(
  from   = c("A", "A", "B", "C", "C", "D"),
  to     = c("B", "C", "A", "D", "D", "A"),
  weight = c(1,   2,   1,   3,   1,   2),
  group  = c("G1", "G1", "G1", "G2", "G2", "G2"),
  stringsAsFactors = FALSE
)

# Clusters
clusters_list <- list(G1 = c("A", "B"), G2 = c("C", "D"))

# Sequence data
seqs <- data.frame(
  T1 = c("A", "C", "B"),
  T2 = c("B", "D", "A"),
  T3 = c("C", "C", "D"),
  T4 = c("D", "A", "C"),
  stringsAsFactors = FALSE
)

# ==============================================================================
# Test: Edge list + list clusters
# ==============================================================================

test_that("build_mcml works with edge list + list clusters", {
  result <- build_mcml(edges_simple, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(dim(result$between$weights), c(2, 2))
  expect_equal(names(result$clusters), c("G1", "G2"))
  expect_equal(result$meta$source, "transitions")

  # Verify raw counts from edges_simple (no node self-loops in this data):
  # A->B: G1->G1, A->C: G1->G2, B->A: G1->G1,
  # C->D: G2->G2, C->D: G2->G2, D->A: G2->G1,
  # A->D: G1->G2, D->C: G2->G2
  # G1->G1 = 2, G1->G2 = 2, G2->G2 = 3, G2->G1 = 1
  expect_equal(result$between$weights["G1", "G1"], 2)
  expect_equal(result$between$weights["G1", "G2"], 2)
  expect_equal(result$between$weights["G2", "G2"], 3)
  expect_equal(result$between$weights["G2", "G1"], 1)
})

# ==============================================================================
# Test: Edge list + column name clusters
# ==============================================================================

test_that("build_mcml works with column name clusters", {
  # edges_grouped has from, to, weight, group columns
  # group column has G1 for rows where from=A or B, G2 for from=C or D
  # Build lookup from both from+group and to+group
  result <- build_mcml(edges_grouped, "group", type = "raw")

  expect_s3_class(result, "cluster_summary")
  # Should have 2 clusters
  expect_equal(length(result$clusters), 2)
})

# ==============================================================================
# Test: Weighted edge list
# ==============================================================================

test_that("build_mcml uses weight column from edge list", {
  result <- build_mcml(edges_weighted, clusters_list, type = "raw")

  # A->C has weight 2 (G1->G2), no other G1->G2 transitions
  # (A->B is within G1)
  expect_equal(result$between$weights["G1", "G2"], 2)

  # D->A has weight 2 (G2->G1)
  expect_equal(result$between$weights["G2", "G1"], 2)
})

# ==============================================================================
# Test: Sequence data.frame + clusters
# ==============================================================================

test_that("build_mcml works with sequence data", {
  result <- build_mcml(seqs, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$source, "transitions")

  # Manual count of transitions:
  # Row 1: A->B (G1->G1), B->C (G1->G2), C->D (G2->G2)
  # Row 2: C->D (G2->G2), D->C (G2->G2), C->A (G2->G1)
  # Row 3: B->A (G1->G1), A->D (G1->G2), D->C (G2->G2)
  # G1->G1 = 2, G1->G2 = 2, G2->G2 = 4, G2->G1 = 1
  expect_equal(result$between$weights["G1", "G1"], 2)
  expect_equal(result$between$weights["G1", "G2"], 2)
  expect_equal(result$between$weights["G2", "G2"], 4)
  expect_equal(result$between$weights["G2", "G1"], 1)
})

# ==============================================================================
# Test: tna object with $data
# ==============================================================================

test_that("build_mcml uses sequence path for tna with $data", {
  # Create a mock tna object with $data
  mock_tna <- structure(
    list(
      weights = matrix(0.5, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4])),
      data = seqs,
      labels = LETTERS[1:4],
      inits = rep(0.25, 4)
    ),
    class = "tna"
  )

  result <- build_mcml(mock_tna, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$source, "transitions")
  # Same as sequence test above
  expect_equal(result$between$weights["G1", "G2"], 2)
})

# ==============================================================================
# Test: tna object without $data (fallback)
# ==============================================================================

test_that("build_mcml falls back to cluster_summary for tna without $data", {
  mat <- matrix(c(0, 0.5, 0.3, 0.2,
                  0.4, 0, 0.1, 0.5,
                  0.2, 0.3, 0, 0.5,
                  0.1, 0.6, 0.3, 0), 4, 4, byrow = TRUE)
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]

  mock_tna <- structure(
    list(
      weights = mat,
      data = NULL,
      labels = LETTERS[1:4],
      inits = rep(0.25, 4)
    ),
    class = "tna"
  )

  result <- build_mcml(mock_tna, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  # This uses cluster_summary, so meta$source should NOT be "transitions"
  expect_null(result$meta$source)
})

# ==============================================================================
# Test: Matrix input (fallback to cluster_summary)
# ==============================================================================

test_that("build_mcml delegates square matrix to cluster_summary", {
  mat <- matrix(runif(16), 4, 4)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]

  result <- build_mcml(mat, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  # Uses cluster_summary path, no transitions source
  expect_null(result$meta$source)
})

# ==============================================================================
# Test: Non-square matrix treated as sequence data
# ==============================================================================

test_that("build_mcml treats non-square matrix as sequence data", {
  seq_mat <- matrix(c("A", "B", "C", "D",
                       "C", "D", "A", "B"), nrow = 2, byrow = TRUE)

  result <- build_mcml(seq_mat, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$source, "transitions")
})

# ==============================================================================
# Test: NAs in sequences are skipped
# ==============================================================================

test_that("build_mcml skips NA transitions in sequences", {
  seqs_na <- data.frame(
    T1 = c("A", "C", NA),
    T2 = c("B", NA, "A"),
    T3 = c("C", "D", "B"),
    stringsAsFactors = FALSE
  )

  result <- build_mcml(seqs_na, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  # Row 1: A->B (G1->G1), B->C (G1->G2)
  # Row 2: C->NA (skip), NA->D (skip)
  # Row 3: NA->A (skip), A->B (G1->G1)
  # G1->G1 = 2, G1->G2 = 1, G2->G1 = 0
  expect_equal(result$between$weights["G1", "G1"], 2)
  expect_equal(result$between$weights["G1", "G2"], 1)
  expect_equal(result$between$weights["G2", "G1"], 0)
})

# ==============================================================================
# Test: Single-node clusters produce 1x1 zero within matrix
# ==============================================================================

test_that("single-node clusters get 1x1 zero within matrix", {
  edges_3node <- data.frame(
    from = c("A", "B", "C"),
    to   = c("B", "C", "A"),
    stringsAsFactors = FALSE
  )
  cls <- list(G1 = c("A", "B"), G2 = "C")

  result <- build_mcml(edges_3node, cls, type = "raw")

  expect_equal(dim(result$within$G2$weights), c(1, 1))
  expect_equal(result$within$G2$weights[1, 1], 0)
})

# ==============================================================================
# Test: Unmapped nodes cause error
# ==============================================================================

test_that("build_mcml errors on unmapped nodes", {
  edges_extra <- data.frame(
    from = c("A", "B", "X"),
    to   = c("B", "C", "A"),
    stringsAsFactors = FALSE
  )
  # clusters_list only has A, B, C, D — X is unmapped
  expect_error(
    build_mcml(edges_extra, clusters_list),
    "Unmapped nodes"
  )
})

# ==============================================================================
# Test: compute_within = FALSE
# ==============================================================================

test_that("build_mcml with compute_within = FALSE returns NULL within", {
  result <- build_mcml(edges_simple, clusters_list, type = "raw",
                        compute_within = FALSE)

  expect_null(result$within)
  expect_equal(dim(result$between$weights), c(2, 2))
})

# ==============================================================================
# Test: cluster_summary passthrough
# ==============================================================================

test_that("build_mcml returns cluster_summary as-is", {
  mat <- matrix(runif(16), 4, 4)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]

  cs <- cluster_summary(mat, clusters_list, type = "raw")
  result <- build_mcml(cs)

  expect_identical(result, cs)
})

# ==============================================================================
# Test: TNA type produces row-normalized between matrix
# ==============================================================================

test_that("build_mcml type='tna' row-normalizes between matrix", {
  result <- build_mcml(edges_simple, clusters_list, type = "tna")

  rs <- rowSums(result$between$weights)
  # Rows with non-zero entries should sum to 1
  nonzero_rows <- rs[rs > 0]
  expect_true(all(abs(nonzero_rows - 1) < 1e-10))
})

# ==============================================================================
# Test: Within-cluster transitions are correct
# ==============================================================================

test_that("build_mcml correctly computes within-cluster matrices", {
  result <- build_mcml(edges_simple, clusters_list, type = "raw")

  # Within G1: A->B (1), B->A (1)
  expect_equal(result$within$G1$weights["A", "B"], 1)
  expect_equal(result$within$G1$weights["B", "A"], 1)

  # Within G2: C->D (2), D->C (1)
  expect_equal(result$within$G2$weights["C", "D"], 2)
  expect_equal(result$within$G2$weights["D", "C"], 1)
})

# ==============================================================================
# Test: Raw counts match manual calculation (comprehensive)
# ==============================================================================

test_that("build_mcml raw counts match manual edge-by-edge calculation", {
  # Enumerate all transitions from edges_simple:
  # A->B: G1->G1 (diagonal)
  # A->C: G1->G2
  # B->A: G1->G1 (diagonal)
  # C->D: G2->G2 (diagonal)
  # C->D: G2->G2 (diagonal)
  # D->A: G2->G1
  # A->D: G1->G2
  # D->C: G2->G2 (diagonal)

  result <- build_mcml(edges_simple, clusters_list, type = "raw")

  # Between matrix includes diagonal (within-cluster loops)
  expect_equal(result$between$weights["G1", "G1"], 2)
  expect_equal(result$between$weights["G1", "G2"], 2)
  expect_equal(result$between$weights["G2", "G2"], 3)
  expect_equal(result$between$weights["G2", "G1"], 1)

  # Total transitions = 8
  expect_equal(sum(result$between$weights), 8)

  # Within G1 detail: A->B=1, B->A=1
  expect_equal(sum(result$within$G1$weights), 2)

  # Within G2 detail: C->D=2, D->C=1
  expect_equal(sum(result$within$G2$weights), 3)
})

# ==============================================================================
# Test: Aggregation methods work
# ==============================================================================

test_that("build_mcml respects aggregation method", {
  # Multiple G1->G2 transitions with weights
  edges_multi <- data.frame(
    from   = c("A", "A", "B"),
    to     = c("C", "D", "C"),
    weight = c(2,   4,   6),
    stringsAsFactors = FALSE
  )

  result_sum <- build_mcml(edges_multi, clusters_list, method = "sum",
                            type = "raw")
  result_mean <- build_mcml(edges_multi, clusters_list, method = "mean",
                             type = "raw")
  result_max <- build_mcml(edges_multi, clusters_list, method = "max",
                            type = "raw")

  # G1->G2: weights 2, 4, 6
  expect_equal(result_sum$between$weights["G1", "G2"], 12)
  expect_equal(result_mean$between$weights["G1", "G2"], 4)
  expect_equal(result_max$between$weights["G1", "G2"], 6)
})

# ==============================================================================
# Test: Compatibility with plot_mcml / as_tna
# ==============================================================================

test_that("build_mcml output works with as_tna", {
  skip_if_not_installed("tna")

  result <- build_mcml(edges_simple, clusters_list, type = "tna")
  tna_models <- as_tna(result)

  expect_s3_class(tna_models, "cluster_tna")
  expect_s3_class(tna_models$between, "tna")
})

# ==============================================================================
# Test: Node-level self-loops count as cluster self-loops
# ==============================================================================

test_that("node-level self-loops (A->A) count on diagonal", {
  edges_self <- data.frame(
    from = c("A", "A", "A", "C"),
    to   = c("A", "B", "C", "D"),
    stringsAsFactors = FALSE
  )

  result <- build_mcml(edges_self, clusters_list, type = "raw")

  # A->A is G1->G1 (node self-loop = cluster self-loop)
  # A->B is G1->G1
  # A->C is G1->G2
  # C->D is G2->G2
  expect_equal(result$between$weights["G1", "G1"], 2)  # A->A + A->B
  expect_equal(result$between$weights["G1", "G2"], 1)  # A->C
  expect_equal(result$between$weights["G2", "G2"], 1)  # C->D
})

test_that("build_mcml output works with print methods", {
  result <- build_mcml(edges_simple, clusters_list, type = "tna")

  # Should print via mcml_network method
  expect_output(print(result), "MCML Network")
  # Should also work as cluster_summary
  expect_s3_class(result, "cluster_summary")
})

# ==============================================================================
# Test: mcml_network class and edges
# ==============================================================================

test_that("build_mcml returns mcml_network class with edges", {
  result <- build_mcml(edges_simple, clusters_list, type = "raw")

  expect_s3_class(result, "mcml_network")
  expect_s3_class(result, "cluster_summary")

  # Edges data.frame

  expect_true(is.data.frame(result$edges))
  expect_equal(nrow(result$edges), nrow(edges_simple))
  expect_true(all(c("from", "to", "weight", "cluster_from",
                     "cluster_to", "type") %in% names(result$edges)))

  # Edge types
  expect_true(all(result$edges$type %in% c("within", "between")))

  # A->B is within G1
  ab <- result$edges[result$edges$from == "A" & result$edges$to == "B", ]
  expect_equal(ab$cluster_from, "G1")
  expect_equal(ab$cluster_to, "G1")
  expect_equal(ab$type, "within")

  # A->C is between G1->G2
  ac <- result$edges[result$edges$from == "A" & result$edges$to == "C", ]
  expect_equal(ac$cluster_from, "G1")
  expect_equal(ac$cluster_to, "G2")
  expect_equal(ac$type, "between")
})

test_that("matrix fallback does NOT produce mcml_network class", {
  mat <- matrix(runif(16), 4, 4)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- LETTERS[1:4]

  result <- build_mcml(mat, clusters_list, type = "raw")

  # Falls back to cluster_summary, not mcml_network
  expect_false(inherits(result, "mcml_network"))
  expect_s3_class(result, "cluster_summary")
})

# ==============================================================================
# .detect_mcml_input - all input types
# ==============================================================================

test_that(".detect_mcml_input identifies edge list with standard columns", {
  df <- data.frame(from = "A", to = "B")
  expect_equal(cograph:::.detect_mcml_input(df), "edgelist")
})

test_that(".detect_mcml_input identifies edge list with alternative columns", {
  df1 <- data.frame(source = "A", target = "B")
  expect_equal(cograph:::.detect_mcml_input(df1), "edgelist")

  df2 <- data.frame(src = "A", tgt = "B")
  expect_equal(cograph:::.detect_mcml_input(df2), "edgelist")

  df3 <- data.frame(v1 = "A", v2 = "B")
  expect_equal(cograph:::.detect_mcml_input(df3), "edgelist")

  df4 <- data.frame(node1 = "A", node2 = "B")
  expect_equal(cograph:::.detect_mcml_input(df4), "edgelist")

  df5 <- data.frame(i = "A", j = "B")
  expect_equal(cograph:::.detect_mcml_input(df5), "edgelist")
})

test_that(".detect_mcml_input identifies sequence data.frame", {
  df <- data.frame(T1 = "A", T2 = "B", T3 = "C")
  expect_equal(cograph:::.detect_mcml_input(df), "sequence")
})

test_that(".detect_mcml_input identifies square numeric matrix", {
  m <- matrix(0, 3, 3)
  expect_equal(cograph:::.detect_mcml_input(m), "matrix")
})

test_that(".detect_mcml_input identifies non-square matrix as sequence", {
  m <- matrix("A", 2, 5)
  expect_equal(cograph:::.detect_mcml_input(m), "sequence")
})

test_that(".detect_mcml_input identifies tna with data", {
  obj <- structure(list(data = data.frame(a = 1)), class = "tna")
  expect_equal(cograph:::.detect_mcml_input(obj), "tna_data")
})

test_that(".detect_mcml_input identifies tna without data", {
  obj <- structure(list(data = NULL), class = "tna")
  expect_equal(cograph:::.detect_mcml_input(obj), "tna_matrix")
})

test_that(".detect_mcml_input identifies cograph_network with data", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$data <- data.frame(T1 = "A")
  expect_equal(cograph:::.detect_mcml_input(net), "cograph_data")
})

test_that(".detect_mcml_input identifies cograph_network without data", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  expect_equal(cograph:::.detect_mcml_input(net), "cograph_matrix")
})

test_that(".detect_mcml_input returns unknown for unsupported types", {
  expect_equal(cograph:::.detect_mcml_input(42), "unknown")
  expect_equal(cograph:::.detect_mcml_input(TRUE), "unknown")
  expect_equal(cograph:::.detect_mcml_input(NULL), "unknown")
})

# ==============================================================================
# .auto_detect_clusters
# ==============================================================================

test_that(".auto_detect_clusters finds cluster column in nodes", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$nodes$cluster <- c("X", "X", "Y", "Y")

  result <- cograph:::.auto_detect_clusters(net)
  expect_equal(result, c("X", "X", "Y", "Y"))
})

test_that(".auto_detect_clusters finds clusters column in nodes", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$nodes$clusters <- c("X", "X", "Y", "Y")

  result <- cograph:::.auto_detect_clusters(net)
  expect_equal(result, c("X", "X", "Y", "Y"))
})

test_that(".auto_detect_clusters finds group in nodes", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- c("X", "X", "Y", "Y")

  result <- cograph:::.auto_detect_clusters(net)
  expect_equal(result, c("X", "X", "Y", "Y"))
})

test_that(".auto_detect_clusters falls back to node_groups", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- NULL
  net$node_groups <- data.frame(
    name = LETTERS[1:4],
    cluster = c("X", "X", "Y", "Y")
  )

  result <- cograph:::.auto_detect_clusters(net)
  expect_equal(result, c("X", "X", "Y", "Y"))
})

test_that(".auto_detect_clusters finds layer in node_groups", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- NULL
  net$node_groups <- data.frame(
    name = LETTERS[1:4],
    layer = c(1, 1, 2, 2)
  )

  result <- cograph:::.auto_detect_clusters(net)
  expect_equal(result, c(1, 1, 2, 2))
})

test_that(".auto_detect_clusters errors when nothing found", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- NULL
  net$node_groups <- NULL

  expect_error(
    cograph:::.auto_detect_clusters(net),
    "No clusters found"
  )
})

# ==============================================================================
# .build_cluster_lookup
# ==============================================================================

test_that(".build_cluster_lookup handles named list", {
  cls <- list(X = c("A", "B"), Y = c("C", "D"))
  lookup <- cograph:::.build_cluster_lookup(cls, LETTERS[1:4])
  expect_equal(lookup[["A"]], "X")
  expect_equal(lookup[["B"]], "X")
  expect_equal(lookup[["C"]], "Y")
  expect_equal(lookup[["D"]], "Y")
})

test_that(".build_cluster_lookup handles character vector", {
  vec <- c("X", "X", "Y", "Y")
  lookup <- cograph:::.build_cluster_lookup(vec, LETTERS[1:4])
  expect_equal(lookup[["A"]], "X")
  expect_equal(lookup[["D"]], "Y")
})

test_that(".build_cluster_lookup handles numeric vector", {
  vec <- c(1, 1, 2, 2)
  lookup <- cograph:::.build_cluster_lookup(vec, LETTERS[1:4])
  expect_equal(lookup[["A"]], "1")
  expect_equal(lookup[["C"]], "2")
})

test_that(".build_cluster_lookup handles factor", {
  vec <- factor(c("X", "X", "Y", "Y"))
  lookup <- cograph:::.build_cluster_lookup(vec, LETTERS[1:4])
  expect_equal(lookup[["A"]], "X")
  expect_equal(lookup[["D"]], "Y")
})

test_that(".build_cluster_lookup errors on unmapped nodes", {
  cls <- list(X = c("A", "B"))
  expect_error(
    cograph:::.build_cluster_lookup(cls, LETTERS[1:4]),
    "Unmapped nodes"
  )
})

test_that(".build_cluster_lookup errors on wrong-length char vector", {
  expect_error(
    cograph:::.build_cluster_lookup(c("X", "Y"), LETTERS[1:4]),
    "must equal"
  )
})

test_that(".build_cluster_lookup errors on wrong-length numeric", {
  expect_error(
    cograph:::.build_cluster_lookup(c(1, 2), LETTERS[1:4]),
    "must equal"
  )
})

test_that(".build_cluster_lookup errors on unsupported type", {
  expect_error(
    cograph:::.build_cluster_lookup(TRUE, LETTERS[1:4]),
    "must be a named list"
  )
})

# ==============================================================================
# .process_weights additional branches
# ==============================================================================

test_that(".process_weights raw returns unchanged", {
  raw <- matrix(c(0, 3, 1, 0), 2, 2)
  result <- cograph:::.process_weights(raw, "raw", TRUE)
  expect_equal(result, raw)
})

test_that(".process_weights frequency returns unchanged", {
  raw <- matrix(c(0, 3, 1, 0), 2, 2)
  result <- cograph:::.process_weights(raw, "frequency", TRUE)
  expect_equal(result, raw)
})

test_that(".process_weights cooccurrence symmetrizes", {
  raw <- matrix(c(0, 3, 1, 0), 2, 2)
  result <- cograph:::.process_weights(raw, "cooccurrence", TRUE)
  expect_equal(result, t(result))
  expect_equal(result[1, 2], 2)
  expect_equal(result[2, 1], 2)
})

test_that(".process_weights tna row-normalizes", {
  raw <- matrix(c(0, 2, 1, 3, 0, 1, 2, 3, 0), 3, 3)
  result <- cograph:::.process_weights(raw, "tna", TRUE)
  row_sums <- rowSums(result)
  expect_true(all(abs(row_sums - 1) < 1e-10 | row_sums == 0))
})

test_that(".process_weights handles zero-sum rows", {
  raw <- matrix(c(0, 0, 0,
                  0, 0, 0,
                  1, 2, 0), 3, 3, byrow = TRUE)
  result <- cograph:::.process_weights(raw, "tna", TRUE)
  # Rows 1 and 2 have zero sums, divided by 1 (stays 0)
  expect_equal(sum(result[1, ]), 0)
  expect_equal(sum(result[2, ]), 0)
  # Row 3 sum = 3: 1/3, 2/3, 0
  expect_equal(result[3, 1], 1 / 3, tolerance = 1e-10)
  expect_equal(result[3, 2], 2 / 3, tolerance = 1e-10)
})

# ==============================================================================
# build_mcml with cograph_network inputs
# ==============================================================================

test_that("build_mcml with cograph_network + edgelist data", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$data <- edges_simple

  result <- build_mcml(net, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$source, "transitions")
})

test_that("build_mcml with cograph_network + sequence data", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$data <- seqs

  result <- build_mcml(net, clusters_list, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$source, "transitions")
})

test_that("build_mcml with cograph_network auto-detects clusters from nodes", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$data <- seqs
  net$nodes$cluster <- c("G1", "G1", "G2", "G2")

  result <- build_mcml(net, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 2)
})

test_that("build_mcml with cograph_network auto-detects from node_groups", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$data <- seqs
  net$nodes$clusters <- NULL
  net$nodes$cluster <- NULL
  net$nodes$groups <- NULL
  net$nodes$group <- NULL
  net$node_groups <- data.frame(
    name = LETTERS[1:4],
    group = c("G1", "G1", "G2", "G2")
  )

  result <- build_mcml(net, type = "raw")

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 2)
})

test_that("build_mcml with cograph_network without data falls back", {
  mat4 <- matrix(0.5, 4, 4)
  diag(mat4) <- 0
  rownames(mat4) <- colnames(mat4) <- LETTERS[1:4]
  net <- as_cograph(mat4)
  net$nodes$cluster <- c("G1", "G1", "G2", "G2")

  result <- build_mcml(net, type = "raw")

  expect_s3_class(result, "cluster_summary")
})

# ==============================================================================
# build_mcml type variants
# ==============================================================================

test_that("build_mcml type=cooccurrence symmetrizes between matrix", {
  result <- build_mcml(edges_simple, clusters_list, type = "cooccurrence")
  expect_equal(result$between$weights, t(result$between$weights))
})

test_that("build_mcml type=frequency preserves raw counts", {
  result <- build_mcml(edges_simple, clusters_list, type = "frequency")
  expect_s3_class(result, "cluster_summary")
  # Should be same as raw
  result_raw <- build_mcml(edges_simple, clusters_list, type = "raw")
  expect_equal(result$between$weights, result_raw$between$weights)
})

test_that("build_mcml type=semi_markov row-normalizes", {
  result <- build_mcml(edges_simple, clusters_list, type = "semi_markov")
  rs <- rowSums(result$between$weights)
  nonzero <- rs[rs > 0]
  expect_true(all(abs(nonzero - 1) < 1e-10))
})

# ==============================================================================
# build_mcml errors on unsupported input
# ==============================================================================

test_that("build_mcml errors on unsupported input type", {
  expect_error(
    build_mcml(42, clusters_list),
    "Cannot build MCML"
  )
})

# ==============================================================================
# Sequence data with recoded between/within data
# ==============================================================================

test_that("build_mcml attaches recoded sequence data to between tna", {
  result <- build_mcml(seqs, clusters_list, type = "raw")

  # Between tna should have recoded data
  expect_true(!is.null(result$between$data))
  # All values should be cluster names
  all_vals <- unlist(result$between$data, use.names = FALSE)
  all_vals <- all_vals[!is.na(all_vals)]
  expect_true(all(all_vals %in% c("G1", "G2")))
})

test_that("build_mcml within tna has filtered sequence data", {
  result <- build_mcml(seqs, clusters_list, type = "raw")

  # Within G1 should have sequence data with only A/B (others NA)
  within_g1_data <- result$within$G1$data
  expect_true(!is.null(within_g1_data))
  all_vals <- unlist(within_g1_data, use.names = FALSE)
  non_na <- all_vals[!is.na(all_vals)]
  expect_true(all(non_na %in% c("A", "B")))
})

# ==============================================================================
# Coverage: density method in .build_from_transitions (lines 781-784)
# ==============================================================================

test_that("build_mcml with method=density computes n_possible", {
  result <- build_mcml(edges_simple, clusters_list, method = "density",
                        type = "raw")

  expect_s3_class(result, "cluster_summary")
  # G1->G2: 2 transitions, n_possible = 2*2 = 4, density = 2/4 = 0.5
  expect_equal(result$between$weights["G1", "G2"], 0.5)
})

# ==============================================================================
# Coverage: zero total between_inits (line 804)
# ==============================================================================

test_that("build_mcml handles zero-weight transitions for inits", {
  # All zero-weight edges
  edges_zero <- data.frame(
    from = c("A", "C"),
    to   = c("B", "D"),
    weight = c(0, 0),
    stringsAsFactors = FALSE
  )

  result <- build_mcml(edges_zero, clusters_list, type = "raw")

  # Inits should be uniform 1/k
  expect_equal(result$between$inits[["G1"]], 0.5)
  expect_equal(result$between$inits[["G2"]], 0.5)
})

# ==============================================================================
# Coverage: from_col/to_col defaults (lines 975, 979)
# ==============================================================================

test_that("build_mcml edgelist defaults to columns 1,2 for non-standard names", {
  # Data.frame with non-standard column names but still has from/to data
  df_weird <- data.frame(
    origin = c("A", "B", "C"),
    destination = c("C", "D", "A"),
    stringsAsFactors = FALSE
  )
  # Rename to not match any standard names
  names(df_weird) <- c("x_col", "y_col")

  # This will be detected as "sequence" not "edgelist" by .detect_mcml_input
  # because column names don't match from/to patterns
  # So test the edgelist function directly
  result <- cograph:::.build_mcml_edgelist(
    df_weird, clusters_list, "sum", "raw", TRUE, TRUE
  )
  expect_s3_class(result, "cluster_summary")
})

# ==============================================================================
# Coverage: NULL clusters error for edgelist (line 1023)
# ==============================================================================

test_that("build_mcml_edgelist errors with NULL clusters", {
  expect_error(
    cograph:::.build_mcml_edgelist(
      edges_simple, NULL, "sum", "raw", TRUE, TRUE
    ),
    "clusters argument is required"
  )
})

# ==============================================================================
# Coverage: vector clusters in edgelist (line 1030)
# ==============================================================================

test_that("build_mcml_edgelist normalizes vector clusters", {
  vec <- c("G1", "G1", "G2", "G2")

  result <- cograph:::.build_mcml_edgelist(
    edges_simple, vec, "sum", "raw", TRUE, TRUE
  )

  expect_s3_class(result, "cluster_summary")
  expect_equal(result$meta$n_clusters, 2)
})

# ==============================================================================
# Coverage: sequence < 2 cols error (line 1053)
# ==============================================================================

test_that("build_mcml_sequence errors with single column", {
  one_col <- data.frame(T1 = c("A", "B", "C"))

  expect_error(
    cograph:::.build_mcml_sequence(
      one_col, clusters_list, "sum", "raw", TRUE, TRUE
    ),
    "at least 2 columns"
  )
})

# ==============================================================================
# Coverage: NULL clusters error for sequence (line 1074)
# ==============================================================================

test_that("build_mcml_sequence errors with NULL clusters", {
  expect_error(
    cograph:::.build_mcml_sequence(
      seqs, NULL, "sum", "raw", TRUE, TRUE
    ),
    "clusters argument is required"
  )
})
