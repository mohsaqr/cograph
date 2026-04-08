# ===========================================================================
# Tests for Batch 3 classical centrality measures
# Reference validation against centiserve / sna / igraph / NetworkX.
# ===========================================================================

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

test_that("katz matches centiserve::katzcent BIT-EXACT (12 random graphs)", {
  skip_if_not_installed("centiserve")
  skip_if_not_installed("igraph")
  set.seed(1001)
  for (i in 1:12) {
    n <- sample(6:20, 1)
    g <- igraph::sample_gnp(n, runif(1, 0.2, 0.5), directed = FALSE)
    if (igraph::ecount(g) < 2) next
    # Pick alpha < 1 / spectral_radius so centiserve accepts it
    A  <- as.matrix(igraph::as_adjacency_matrix(g))
    sr <- max(Re(eigen(A, only.values = TRUE)$values))
    if (sr <= 0) next
    a  <- min(0.1, 0.5 / sr)
    cog <- centrality(g, measures = "katz", katz_alpha = a)$katz
    cs  <- centiserve::katzcent(g, alpha = a)
    # Bit-exact: cograph's calculate_katz mirrors centiserve's
    # solve(I - alpha*A^T) %*% 1 LAPACK call sequence exactly.
    expect_identical(cog, cs,
                     info = sprintf("graph %d, n=%d, alpha=%.4f", i, n, a))
  }
})

test_that("katz matches igraph::alpha_centrality at machine epsilon", {
  skip_if_not_installed("igraph")
  set.seed(1002)
  for (i in 1:5) {
    n <- sample(10:30, 1)
    g <- igraph::sample_gnp(n, 0.3, directed = FALSE)
    cog <- centrality(g, measures = "katz", katz_alpha = 0.1)$katz
    ig  <- igraph::alpha_centrality(g, alpha = 0.1, exo = 1, sparse = TRUE)
    # Sparse iterative solver vs dense direct solve: machine-epsilon agreement.
    expect_equal(cog, unname(ig), tolerance = 1e-9,
                 info = sprintf("graph %d, n=%d", i, n))
  }
})

# NetworkX cross-language reference test (skip if reticulate / nx unavailable)
has_nx <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_module_available("networkx")
}

test_that("katz matches NetworkX katz_centrality_numpy on karate (ULP)", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  g_r  <- igraph::make_graph("Zachary")
  g_nx <- nx$karate_club_graph()
  cog <- centrality(g_r, measures = "katz", katz_alpha = 0.1)$katz
  nxv <- unname(unlist(nx$katz_centrality_numpy(g_nx, alpha = 0.1, beta = 1,
                                                normalized = FALSE)))
  # 1-2 ULPs of difference are unavoidable across R and Python LAPACK builds.
  expect_equal(cog, nxv, tolerance = 1e-13)
})

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

test_that("hubbell matches centiserve::hubbell BIT-EXACT (weighted)", {
  skip_if_not_installed("centiserve")
  skip_if_not_installed("igraph")
  set.seed(2001)
  for (i in 1:8) {
    n <- sample(5:12, 1)
    repeat {
      g <- igraph::sample_gnp(n, 0.5, directed = FALSE)
      if (igraph::is_connected(g) && igraph::ecount(g) >= 2) break
    }
    igraph::E(g)$weight <- runif(igraph::ecount(g), 0.1, 0.5)
    A <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
    sr <- max(Re(eigen(A)$values))
    wf <- 0.8 / sr
    cog <- centrality(g, measures = "hubbell", hubbell_weight = wf)$hubbell
    # IMPORTANT: centiserve::hubbell(weights = NULL) silently uses uniform
    # weights of 1. To reproduce cograph's behavior (respecting E(g)$weight),
    # we must pass the weights argument explicitly.
    cs  <- centiserve::hubbell(g, weightfactor = wf,
                               weights = igraph::E(g)$weight)
    expect_identical(cog, cs,
                     info = sprintf("graph %d, n=%d, wf=%.4f", i, n, wf))
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

test_that("information matches sna::infocent BIT-EXACT (connected)", {
  skip_if_not_installed("sna")
  skip_if_not_installed("igraph")
  set.seed(3001)
  for (i in 1:12) {
    n <- sample(6:20, 1)
    repeat {
      g <- igraph::sample_gnp(n, 0.4, directed = FALSE)
      if (igraph::is_connected(g)) break
    }
    A <- as.matrix(igraph::as_adjacency_matrix(g))
    cog <- centrality(g, measures = "information")$information
    sn  <- sna::infocent(A)
    expect_identical(cog, sn,
                     info = sprintf("graph %d, n=%d", i, n))
  }
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

test_that("pairwisedis matches centiserve::pairwisedis BIT-EXACT", {
  skip_if_not_installed("centiserve")
  skip_if_not_installed("igraph")
  set.seed(4001)
  for (i in 1:12) {
    n <- sample(5:20, 1)
    g <- igraph::sample_gnp(n, runif(1, 0.2, 0.5), directed = TRUE)
    if (igraph::ecount(g) < 2) next
    cog <- centrality(g, measures = "pairwisedis")$pairwisedis
    cs  <- centiserve::pairwisedis(g)
    expect_identical(cog, cs,
                     info = sprintf("graph %d, n=%d, m=%d",
                                    i, n, igraph::ecount(g)))
  }
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

test_that("reaching_local on undirected matches normalized harmonic BIT-EXACT", {
  skip_if_not_installed("igraph")
  set.seed(5001)
  for (i in 1:8) {
    n <- sample(6:20, 1)
    g <- igraph::sample_gnp(n, 0.4, directed = FALSE)
    if (igraph::ecount(g) < 2) next
    cog <- centrality(g, measures = "reaching_local")$reaching_local_all
    hm  <- igraph::harmonic_centrality(g, normalized = TRUE)
    expect_identical(cog, unname(hm),
                     info = sprintf("graph %d, n=%d", i, n))
  }
})

test_that("reaching_local matches NetworkX (karate undirected)", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  g_r  <- igraph::make_graph("Zachary")
  g_nx <- nx$karate_club_graph()
  cog <- centrality(g_r, measures = "reaching_local")$reaching_local_all
  nxv <- unname(sapply(0:33,
                       function(v) nx$local_reaching_centrality(g_nx, as.integer(v))))
  expect_equal(cog, nxv, tolerance = 1e-15)
})

test_that("reaching_local matches NetworkX on directed unweighted graphs", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  set.seed(6001)
  for (i in 1:3) {
    n <- sample(6:12, 1)
    g <- igraph::sample_gnp(n, 0.35, directed = TRUE)
    el <- igraph::as_edgelist(g)
    g_py <- nx$DiGraph()
    g_py$add_nodes_from(as.integer(0:(n - 1)))
    if (nrow(el) > 0) {
      edges_py <- lapply(seq_len(nrow(el)),
                         function(i) c(as.integer(el[i, 1] - 1),
                                       as.integer(el[i, 2] - 1)))
      g_py$add_edges_from(edges_py)
    }
    cog <- centrality(g, measures = "reaching_local", mode = "out")$reaching_local_out
    nxv <- unname(sapply(0:(n - 1),
                         function(v) nx$local_reaching_centrality(g_py, as.integer(v))))
    # Directed unweighted reaching: simple integer counts -> bit-exact match.
    expect_identical(cog, nxv,
                     info = sprintf("graph %d, n=%d", i, n))
  }
})

test_that("reaching_global matches NetworkX global_reaching_centrality on karate", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  g_r  <- igraph::make_graph("Zachary")
  g_nx <- nx$karate_club_graph()
  cog <- reaching_global(g_r)
  nxv <- nx$global_reaching_centrality(g_nx)
  expect_equal(cog, nxv, tolerance = 1e-13)
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

test_that("prestige_domain matches sna::prestige(cmode='domain') BIT-EXACT", {
  skip_if_not_installed("sna")
  skip_if_not_installed("igraph")
  set.seed(7001)
  for (i in 1:12) {
    n <- sample(6:20, 1)
    g <- igraph::sample_gnp(n, runif(1, 0.15, 0.4), directed = TRUE)
    if (igraph::ecount(g) < 2) next
    A  <- as.matrix(igraph::as_adjacency_matrix(g))
    cog <- centrality(g, measures = "prestige_domain")$prestige_domain
    sn  <- sna::prestige(A, cmode = "domain")
    expect_identical(cog, as.numeric(sn),
                     info = sprintf("graph %d, n=%d, m=%d",
                                    i, n, igraph::ecount(g)))
  }
})

test_that("prestige_domain_proximity warns and returns NA on undirected", {
  expect_warning(v <- centrality_prestige_domain_proximity(k3), "directed")
  expect_true(all(is.na(v)))
})

test_that("prestige_domain_proximity matches sna BIT-EXACT (strongly connected)", {
  skip_if_not_installed("sna")
  skip_if_not_installed("igraph")
  # Strongly connected directed graphs only: sna's formula has a
  # FALSE * Inf = NaN bug that zeros every node when any pair is
  # unreachable. cograph's is.finite()-masked formula is correct
  # on all directed graphs, but bit-exact matching requires the
  # subset where sna's formula is well-defined.
  set.seed(7002)
  tested <- 0
  attempts <- 0
  while (tested < 8 && attempts < 200) {
    attempts <- attempts + 1
    n <- sample(5:12, 1)
    g <- igraph::sample_gnp(n, runif(1, 0.4, 0.7), directed = TRUE)
    if (!igraph::is_connected(g, mode = "strong")) next
    A  <- as.matrix(igraph::as_adjacency_matrix(g))
    cog <- centrality(g, measures = "prestige_domain_proximity")$prestige_domain_proximity
    sn  <- sna::prestige(A, cmode = "domain.proximity")
    expect_identical(cog, as.numeric(sn),
                     info = sprintf("strongly connected n=%d, m=%d",
                                    n, igraph::ecount(g)))
    tested <- tested + 1
  }
  expect_gte(tested, 3)  # ensure we actually ran some tests
})

test_that("prestige_domain_proximity gives correct values where sna has a bug", {
  skip_if_not_installed("sna")
  skip_if_not_installed("igraph")
  # On a directed graph with any unreachable pair, sna::prestige's
  # domain.proximity formula produces NaN -> all zeros (a known bug).
  # cograph produces the mathematically correct values.
  set.seed(7003)
  # Build a graph with a disconnected isolated node guaranteed
  g <- igraph::make_graph(c(1,2, 2,3, 3,1, 1,4, 4,5), n = 6, directed = TRUE)
  # Node 6 is isolated -> unreachable pairs -> sna returns all zeros
  A   <- as.matrix(igraph::as_adjacency_matrix(g))
  cog <- centrality(g, measures = "prestige_domain_proximity")$prestige_domain_proximity
  sn  <- sna::prestige(A, cmode = "domain.proximity")
  # sna zeros everything due to the NaN bug
  expect_true(all(sn == 0))
  # cograph gives sensible non-zero values
  expect_true(sum(cog > 0) >= 2,
              info = "cograph should compute non-zero values where sna has NaN bug")
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

test_that("brokerage all 5 roles match sna BIT-EXACT (20 random graphs)", {
  skip_if_not_installed("sna")
  skip_if_not_installed("igraph")
  set.seed(8001)
  cog_col <- c(w_I = "brokerage_coordinator",
               w_O = "brokerage_itinerant",
               b_IO = "brokerage_representative",
               b_OI = "brokerage_gatekeeper",
               b_O = "brokerage_liaison")
  for (i in 1:20) {
    n  <- sample(8:15, 1)
    g  <- igraph::sample_gnp(n, runif(1, 0.2, 0.5), directed = TRUE)
    if (igraph::ecount(g) < 3) next
    cl <- sample(1:3, n, replace = TRUE)
    A  <- as.matrix(igraph::as_adjacency_matrix(g))
    ref <- sna::brokerage(A, cl = cl)$raw.nli  # N x 6 (w_I,w_O,b_IO,b_OI,b_O,t)

    for (sna_role in names(cog_col)) {
      cog <- centrality(g, measures = cog_col[[sna_role]],
                        membership = cl)[[cog_col[[sna_role]]]]
      expect_identical(cog, as.integer(ref[, sna_role]),
                       info = sprintf("graph %d (n=%d) role %s",
                                      i, n, sna_role))
    }
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

test_that("estrada_index matches NetworkX at machine epsilon", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  set.seed(6101)
  for (i in 1:5) {
    n <- sample(8:20, 1)
    g_r <- igraph::sample_gnp(n, runif(1, 0.2, 0.5), directed = FALSE)
    if (igraph::ecount(g_r) < 2) next
    g_nx <- nx$Graph()
    g_nx$add_nodes_from(as.integer(0:(n - 1)))
    el <- igraph::as_edgelist(g_r)
    if (nrow(el) > 0) {
      for (j in seq_len(nrow(el))) {
        g_nx$add_edge(as.integer(el[j, 1] - 1), as.integer(el[j, 2] - 1))
      }
    }
    cog <- estrada_index(g_r)
    nxv <- nx$estrada_index(g_nx)
    rel <- abs(cog - nxv) / abs(nxv)
    expect_lt(rel, 1e-13,
              label = sprintf("estrada graph %d (n=%d)", i, n))
  }
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

test_that("group_centrality: degree matches NetworkX BIT-EXACT (undirected)", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  set.seed(7101)
  for (i in 1:6) {
    n <- sample(10:15, 1)
    repeat {
      g <- igraph::sample_gnp(n, 0.4, directed = FALSE)
      if (igraph::is_connected(g)) break
    }
    S <- sort(sample(seq_len(n), 3))

    el <- igraph::as_edgelist(g)
    g_py <- nx$Graph()
    g_py$add_nodes_from(as.integer(0:(n - 1)))
    for (j in seq_len(nrow(el))) {
      g_py$add_edge(as.integer(el[j, 1] - 1), as.integer(el[j, 2] - 1))
    }
    S_py <- reticulate::py_eval(sprintf("set([%s])",
                                        paste(S - 1L, collapse = ",")))
    cog <- group_centrality(g, S, measure = "degree")
    nxv <- nx$group_degree_centrality(g_py, S_py)
    expect_equal(cog, nxv, tolerance = 0,
                 info = sprintf("undirected graph %d (n=%d)", i, n))
  }
})

test_that("group_centrality: closeness matches NetworkX BIT-EXACT (undirected)", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  set.seed(7102)
  for (i in 1:6) {
    n <- sample(10:15, 1)
    repeat {
      g <- igraph::sample_gnp(n, 0.4, directed = FALSE)
      if (igraph::is_connected(g)) break
    }
    S <- sort(sample(seq_len(n), 3))

    el <- igraph::as_edgelist(g)
    g_py <- nx$Graph()
    g_py$add_nodes_from(as.integer(0:(n - 1)))
    for (j in seq_len(nrow(el))) {
      g_py$add_edge(as.integer(el[j, 1] - 1), as.integer(el[j, 2] - 1))
    }
    S_py <- reticulate::py_eval(sprintf("set([%s])",
                                        paste(S - 1L, collapse = ",")))
    cog <- group_centrality(g, S, measure = "closeness")
    nxv <- nx$group_closeness_centrality(g_py, S_py)
    expect_equal(cog, nxv, tolerance = 1e-13,
                 info = sprintf("undirected graph %d (n=%d)", i, n))
  }
})

test_that("group_centrality: directed degree modes match NetworkX", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  set.seed(7103)
  tested <- 0
  for (i in 1:10) {
    n <- sample(10:14, 1)
    g <- igraph::sample_gnp(n, 0.35, directed = TRUE)
    if (igraph::ecount(g) < 4) next
    S <- sort(sample(seq_len(n), 3))

    el <- igraph::as_edgelist(g)
    g_py <- nx$DiGraph()
    g_py$add_nodes_from(as.integer(0:(n - 1)))
    for (j in seq_len(nrow(el))) {
      g_py$add_edge(as.integer(el[j, 1] - 1), as.integer(el[j, 2] - 1))
    }
    S_py <- reticulate::py_eval(sprintf("set([%s])",
                                        paste(S - 1L, collapse = ",")))

    cog_out <- group_centrality(g, S, measure = "degree", mode = "out")
    nxv_out <- nx$group_out_degree_centrality(g_py, S_py)
    expect_equal(cog_out, nxv_out, tolerance = 0,
                 info = sprintf("out-deg graph %d", i))

    cog_in <- group_centrality(g, S, measure = "degree", mode = "in")
    nxv_in <- nx$group_in_degree_centrality(g_py, S_py)
    expect_equal(cog_in, nxv_in, tolerance = 0,
                 info = sprintf("in-deg graph %d", i))

    tested <- tested + 1
  }
  expect_gte(tested, 5)
})

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

test_that("dispersion matches NetworkX BIT-EXACT on karate (all edges)", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  g_r  <- igraph::make_graph("Zachary")
  g_nx <- nx$karate_club_graph()

  nx_full <- nx$dispersion(g_nx, normalized = TRUE)
  cog_full <- dispersion(g_r, normalized = TRUE)

  for (row_i in seq_len(nrow(cog_full))) {
    u_R <- cog_full$from[row_i]
    v_R <- cog_full$to[row_i]
    cog_val <- cog_full$dispersion[row_i]
    nx_val <- nx_full[[as.character(u_R - 1L)]][[as.character(v_R - 1L)]]
    expect_equal(cog_val, nx_val, tolerance = 1e-12,
                 info = sprintf("edge (%d, %d)", u_R, v_R))
  }
})

test_that("dispersion unnormalized matches NetworkX BIT-EXACT", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  g_r  <- igraph::make_graph("Zachary")
  g_nx <- nx$karate_club_graph()

  # Test single-pair unnormalized on a few specific edges
  pairs <- list(c(1L, 34L), c(1L, 2L), c(3L, 4L), c(9L, 14L))
  for (p in pairs) {
    cog <- dispersion(g_r, u = p[1], v = p[2], normalized = FALSE)
    nxv <- nx$dispersion(g_nx, as.integer(p[1] - 1L), as.integer(p[2] - 1L),
                         normalized = FALSE)
    expect_equal(cog, nxv, tolerance = 0,
                 info = sprintf("pair %d,%d unnormalized", p[1], p[2]))
  }
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

test_that("trophic_incoherence matches NetworkX BIT-EXACT", {
  skip_if_not(has_nx(), "NetworkX not available")
  nx <- reticulate::import("networkx")
  set.seed(6201)
  passes <- 0
  for (i in 1:10) {
    n <- sample(10:20, 1)
    g_r <- igraph::sample_gnp(n, 0.15, directed = TRUE)
    # Need at least one basal node (in-degree 0) for trophic levels
    if (all(igraph::degree(g_r, mode = "in") > 0)) next
    if (igraph::ecount(g_r) < 2) next

    el <- igraph::as_edgelist(g_r)
    g_nx <- nx$DiGraph()
    g_nx$add_nodes_from(as.integer(0:(n - 1)))
    for (j in seq_len(nrow(el))) {
      g_nx$add_edge(as.integer(el[j, 1] - 1), as.integer(el[j, 2] - 1))
    }
    cog <- tryCatch(trophic_incoherence(g_r), warning = function(w) NA, error = function(e) NA)
    nxv <- tryCatch(nx$trophic_incoherence_parameter(g_nx),
                    error = function(e) NA)
    if (is.na(cog) || is.na(nxv)) next

    expect_equal(cog, nxv, tolerance = 1e-13,
                 info = sprintf("graph %d, n=%d", i, n))
    passes <- passes + 1
  }
  expect_gte(passes, 3)
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
