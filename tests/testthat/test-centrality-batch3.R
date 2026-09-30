# ===========================================================================
# Tests for Batch 3 classical centrality measures
# Reference validation against centiserve / sna / igraph / NetworkX.
# ===========================================================================

# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

skip_coverage_tests()

# ---------------------------------------------------------------------------
# Test graphs
# ---------------------------------------------------------------------------

k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(k3) <- colnames(k3) <- c("A", "B", "C")

path4 <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 0, 1,
  0, 0, 1, 0
), 4, 4)
rownames(path4) <- colnames(path4) <- c("A", "B", "C", "D")

# Directed 3-cycle (for directed-only measures)
d3 <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
rownames(d3) <- colnames(d3) <- c("A","B","C")

# ===========================================================================
# Katz centrality (Katz 1953)
# ===========================================================================

test_that("katz returns a numeric vector of correct length", {
  v <- centrality_katz(k3)
  expect_type(v, "double")
  expect_length(v, 3)
  expect_named(v, c("A", "B", "C"))
  # Symmetric graph: all equal
  expect_equal(v[[1]], v[[2]])
  expect_equal(v[[2]], v[[3]])
})


# NetworkX cross-language reference test (skip if reticulate / nx unavailable)
has_nx <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_module_available("networkx")
}


# ===========================================================================
# Hubbell centrality (Hubbell 1965)
# ===========================================================================

test_that("hubbell returns NA with warning when not solvable", {
  # K3 spectral radius = 2; default weightfactor 0.5 gives 0.5*2 = 1 (boundary
  # - numerical instability -> NA with warning)
  expect_warning(res <- centrality_hubbell(k3), "not solvable")
  expect_true(all(is.na(res)))
})

test_that("hubbell works with appropriate weightfactor", {
  v <- centrality_hubbell(k3, hubbell_weight = 0.3)
  expect_length(v, 3)
  expect_true(all(is.finite(v)))
  expect_true(all(v > 0))
})

test_that("hubbell refuses a signed network whose spectral radius exceeds 1", {
  # Regression: the divergence guard tested max(Re(lambda)), not the spectral
  # radius. This signed network scales to eigenvalues 0 +/- 1.5i and +/- 0.5,
  # so the Neumann series diverges (modulus 1.5) while every real part is 0.5.
  # It previously returned confident finite scores.
  W <- matrix(c(0, 3, 0, 0,
                -3, 0, 0, 0,
                0, 0, 0, 1,
                0, 0, 1, 0), 4, 4, byrow = TRUE)
  rownames(W) <- colnames(W) <- LETTERS[1:4]
  ev <- eigen(W * 0.5, only.values = TRUE)$values
  expect_gt(max(Mod(ev)), 1)
  expect_lt(max(Re(ev)), 1)
  expect_warning(res <- centrality_hubbell(W), "not solvable")
  expect_true(all(is.na(res)))
})

test_that("the hubbell guard is unchanged for non-negative networks", {
  # For a non-negative matrix the Perron root is real, positive and equal to
  # the spectral radius, so max(Re) and max(Mod) agree and the fix is a no-op.
  set.seed(4242)
  for (i in 1:40) {
    n <- sample(3:8, 1)
    m <- matrix(stats::rbinom(n * n, 1, 0.4) * stats::runif(n * n, 0.1, 3), n, n)
    m[lower.tri(m)] <- t(m)[lower.tri(m)]
    diag(m) <- 0
    ev <- eigen(m * 0.5, only.values = TRUE)$values
    expect_equal(max(Re(ev)), max(Mod(ev)), tolerance = 1e-8, info = paste("i =", i))
  }
})


# ===========================================================================
# Information centrality (Stephenson-Zelen 1989)
# ===========================================================================

test_that("information centrality is symmetric on K3", {
  v <- centrality_information(k3)
  expect_length(v, 3)
  expect_equal(v[[1]], v[[2]])
  expect_equal(v[[2]], v[[3]])
})


# ===========================================================================
# Pairwise Disconnectivity (Potapov et al. 2008)
# ===========================================================================

test_that("pairwisedis warns and returns NA on undirected input", {
  expect_warning(v <- centrality_pairwisedis(k3), "directed")
  expect_true(all(is.na(v)))
})

test_that("pairwisedis works on directed 3-cycle", {
  v <- centrality_pairwisedis(d3)
  expect_length(v, 3)
  # All nodes equivalent by symmetry
  expect_equal(v[[1]], v[[2]])
  # For a 3-cycle: 6 reachable ordered pairs before removal; 1 remaining
  # after any removal -> PD = (6 - 1) / 6 = 5/6
  expect_equal(unname(v[[1]]), 5/6, tolerance = 1e-10)
})


# ===========================================================================
# Local + Global Reaching Centrality (Mones, Vicsek & Vicsek 2012)
# ===========================================================================

test_that("reaching_local returns proportion of reachable nodes", {
  # Undirected K3: every node reaches both others in 1 step
  # normalized harmonic = (1 + 1)/2 = 1
  v <- centrality_reaching_local(k3)
  expect_equal(unname(v), rep(1, 3))

  # Path A-B-C-D: harmonic mean of inverse distances / (N-1)
  # Node A: 1/1 + 1/2 + 1/3 = 11/6, / 3 = 11/18 ≈ 0.6111
  v2 <- centrality_reaching_local(path4)
  expect_equal(unname(v2[["A"]]), 11/18, tolerance = 1e-10)
})

test_that("reaching_global scalar within [0, 1]", {
  r <- reaching_global(path4)
  expect_length(r, 1)
  expect_true(r >= 0 && r <= 1)
})

test_that("reaching_local on undirected matches normalized harmonic", {
  skip_if_not_installed("igraph")
  set.seed(5001)
  for (i in 1:8) {
    n <- sample(6:20, 1)
    g <- igraph::sample_gnp(n, 0.4, directed = FALSE)
    if (igraph::ecount(g) < 2) next
    cog <- centrality(g, measures = "reaching_local")$reaching_local_all
    hm  <- igraph::harmonic_centrality(g, normalized = TRUE)
    expect_equal(cog, unname(hm), tolerance = 1e-12,
                 info = sprintf("graph %d, n=%d", i, n))
  }
})


# ===========================================================================
# Batch 4 — Directed Prestige Family (Wasserman-Faust / sna)
# ===========================================================================

test_that("prestige_domain warns and returns NA on undirected input", {
  expect_warning(v <- centrality_prestige_domain(k3), "directed")
  expect_true(all(is.na(v)))
})

test_that("prestige_domain on directed 3-cycle", {
  # Every node can reach every other node -> domain = 2 for each
  v <- centrality_prestige_domain(d3)
  expect_equal(unname(v), c(2, 2, 2))
})


test_that("prestige_domain_proximity warns and returns NA on undirected", {
  expect_warning(v <- centrality_prestige_domain_proximity(k3), "directed")
  expect_true(all(is.na(v)))
})


# ===========================================================================
# Batch 5 — Gould-Fernandez brokerage (5 roles)
# ===========================================================================

# Small deterministic test graph: 4 nodes, 2 groups
brokerage_g <- matrix(c(
  0, 1, 1, 0,
  0, 0, 1, 1,
  0, 0, 0, 1,
  1, 0, 0, 0
), 4, 4, byrow = TRUE)
rownames(brokerage_g) <- colnames(brokerage_g) <- LETTERS[1:4]
brokerage_cl <- c(1, 1, 2, 2)

test_that("brokerage measures warn + NA when membership is missing", {
  # Matches the convention used by participation, within_module_z, gateway
  expect_warning(
    v <- centrality_brokerage_coordinator(brokerage_g),
    "membership"
  )
  expect_true(all(is.na(v)))
})

test_that("brokerage measures warn + NA on undirected input", {
  k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  expect_warning(
    v <- centrality_brokerage_coordinator(k3, membership = c(1, 1, 2)),
    "directed"
  )
  expect_true(all(is.na(v)))
})

test_that("brokerage measures return correct length and type", {
  for (fn in list(centrality_brokerage_coordinator,
                  centrality_brokerage_itinerant,
                  centrality_brokerage_representative,
                  centrality_brokerage_gatekeeper,
                  centrality_brokerage_liaison)) {
    v <- fn(brokerage_g, membership = brokerage_cl)
    expect_length(v, 4)
    expect_named(v, LETTERS[1:4])
    expect_type(v, "integer")
  }
})


# ===========================================================================
# Batch 6 — new-API measures (graph-level / set-level / pair-level)
# ===========================================================================

test_that("estrada_index returns a positive scalar", {
  g <- igraph::make_graph("Zachary")
  ei <- estrada_index(g)
  expect_length(ei, 1)
  expect_true(is.numeric(ei))
  expect_true(ei > 0)
})

test_that("estrada_index equals sum of subgraph_centrality", {
  # Mathematical identity: EE(G) = sum_i exp(lambda_i) = trace(exp(A))
  # subgraph_centrality_i = (exp(A))_ii, so sum_i SC_i = trace(exp(A))
  g <- igraph::make_graph("Zachary")
  ei <- estrada_index(g)
  sc_sum <- sum(centrality(g, measures = "subgraph")$subgraph)
  expect_equal(ei, sc_sum, tolerance = 1e-10)
})


test_that("trophic_incoherence: q = 0 for a perfect chain", {
  # 1 -> 2 -> 3 -> 4: trophic levels = (1, 2, 3, 4), all diffs = 1
  adj <- matrix(0, 4, 4)
  adj[1, 2] <- adj[2, 3] <- adj[3, 4] <- 1
  q <- trophic_incoherence(adj)
  expect_equal(q, 0, tolerance = 1e-12)
})

test_that("trophic_incoherence warns + NA on undirected input", {
  k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  expect_warning(q <- trophic_incoherence(k3), "directed")
  expect_true(is.na(q))
})

# ===========================================================================
# group_centrality family (Everett-Borgatti 1999)
# ===========================================================================


test_that("group_centrality: textbook betweenness on directed 4-cycle", {
  # Known case: 0->1->2->3->0, C = {1}
  # NX gives GBC({1}) = 3.0 normalized=FALSE (matches textbook any)
  g <- igraph::make_graph(c(1,2, 2,3, 3,4, 4,1), n = 4, directed = TRUE)
  v <- group_centrality(g, nodes = 2, measure = "betweenness", normalized = FALSE)
  expect_equal(v, 3, tolerance = 1e-12)

  # C = {1, 2}: path 0->1->2->3 has both, counted ONCE in any-formula
  v2 <- group_centrality(g, nodes = c(2, 3), measure = "betweenness", normalized = FALSE)
  expect_equal(v2, 1, tolerance = 1e-12)
})

test_that("group_centrality: betweenness on a 6-node directed graph (textbook)", {
  # Hand-verified case — matches NX output because on this graph the
  # Puzis iterative algorithm happens to agree with the textbook formula.
  el <- matrix(c(1,6, 2,1, 3,1, 4,1, 5,1, 2,6, 6,2, 1,3, 3,6,
                 2,4, 3,4, 5,4, 1,5, 4,5),
               ncol = 2, byrow = TRUE)
  g <- igraph::make_graph(as.vector(t(el)), n = 6, directed = TRUE)
  v <- group_centrality(g, nodes = c(1, 2), measure = "betweenness",
                        normalized = FALSE)
  expect_equal(v, 7.5, tolerance = 1e-12)
})

test_that("group_centrality: node-name lookup", {
  adj <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  v <- group_centrality(adj, nodes = c("A", "B"), measure = "degree")
  expect_type(v, "double")
  expect_true(is.finite(v))
})

test_that("group_centrality: unknown node name errors", {
  adj <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  expect_error(
    group_centrality(adj, nodes = c("A", "Z"), measure = "degree"),
    "unknown nodes"
  )
})

# ===========================================================================
# dispersion (Backstrom-Kleinberg 2014)
# ===========================================================================

test_that("dispersion returns scalar for single pair", {
  g <- igraph::make_graph("Zachary")
  v <- dispersion(g, u = 1, v = 2)
  expect_length(v, 1)
  expect_true(is.numeric(v))
})

test_that("dispersion returns named vector for single source", {
  g <- igraph::make_graph("Zachary")
  v <- dispersion(g, u = 1)
  expect_true(is.numeric(v))
  expect_true(length(v) == igraph::degree(g, v = 1))
  expect_false(is.null(names(v)))
})

test_that("dispersion returns data frame for full graph", {
  g <- igraph::make_graph("Zachary")
  df <- dispersion(g)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("from", "to", "dispersion"))
  expect_equal(nrow(df), 2 * igraph::ecount(g))  # undirected: each edge counted in both directions
})


test_that("dispersion accepts node names", {
  adj <- matrix(c(0,1,1,1, 1,0,1,0, 1,1,0,1, 1,0,1,0), 4, 4)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
  v <- dispersion(adj, u = "A", v = "B")
  expect_length(v, 1)
  expect_true(is.numeric(v))
})

test_that("dispersion: unknown node name errors", {
  adj <- matrix(c(0,1,1,0, 1,0,1,1, 1,1,0,1, 0,1,1,0), 4, 4)
  rownames(adj) <- colnames(adj) <- LETTERS[1:4]
  expect_error(dispersion(adj, u = "Z"), "unknown node")
})


test_that("brokerage on small deterministic graph gives exact roles", {
  # Adjacency (4 nodes, 2 groups):
  #   A(1) -> B(1), A(1) -> C(2), B(1) -> C(2), B(1) -> D(2),
  #   C(2) -> D(2), D(2) -> A(1)
  # Enumerate open 2-paths through each broker by hand:
  #   v = A(1): in = {D}, out = {B, C}
  #     D -> A -> B: (2,1,1) b_OI, open (no D->B)  [count]
  #     D -> A -> C: (2,1,2) w_O, BUT D->C? no. open [count]
  #   v = B(1): in = {A}, out = {C, D}
  #     A -> B -> C: (1,1,2) b_IO, but A->C IS edge -> CLOSED, skip
  #     A -> B -> D: (1,1,2) b_IO, A->D? no, open [count]
  #   v = C(2): in = {A, B}, out = {D}
  #     A -> C -> D: (1,2,2) b_OI, A->D? no, open [count]
  #     B -> C -> D: (1,2,2) b_OI, B->D IS edge -> CLOSED, skip
  #   v = D(2): in = {B, C}, out = {A}
  #     B -> D -> A: (1,2,1) w_O, B->A? no, open [count]
  #     C -> D -> A: (2,2,1) b_IO, C->A? no, open [count]
  #
  # Expected raw counts per node:
  #   A: w_O=1, b_OI=1, others=0
  #   B: b_IO=1, others=0
  #   C: b_OI=1, others=0
  #   D: w_O=1, b_IO=1, others=0
  expect_equal(unname(centrality_brokerage_coordinator(brokerage_g,
                                                       membership = brokerage_cl)),
               c(0L, 0L, 0L, 0L))
  expect_equal(unname(centrality_brokerage_itinerant(brokerage_g,
                                                     membership = brokerage_cl)),
               c(1L, 0L, 0L, 1L))
  expect_equal(unname(centrality_brokerage_representative(brokerage_g,
                                                          membership = brokerage_cl)),
               c(0L, 1L, 0L, 1L))
  expect_equal(unname(centrality_brokerage_gatekeeper(brokerage_g,
                                                      membership = brokerage_cl)),
               c(1L, 0L, 1L, 0L))
  expect_equal(unname(centrality_brokerage_liaison(brokerage_g,
                                                   membership = brokerage_cl)),
               c(0L, 0L, 0L, 0L))
})
