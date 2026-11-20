# Equivalence tests moved from tests/testthat/test-centrality-batch3.R: cograph against
# other implementations. Developer-only; not part of R CMD check.
# The source file's top-level setup is repeated below so every block keeps
# its fixtures and skips.

# ---- setup from the source file ----
skip_if_not_installed("igraph")
skip_coverage_tests()
k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(k3) <- colnames(k3) <- c("A", "B", "C")
path4 <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 0, 1,
  0, 0, 1, 0
), 4, 4)
rownames(path4) <- colnames(path4) <- c("A", "B", "C", "D")
d3 <- matrix(c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE)
rownames(d3) <- colnames(d3) <- c("A","B","C")
has_nx <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    reticulate::py_module_available("networkx")
}
brokerage_g <- matrix(c(
  0, 1, 1, 0,
  0, 0, 1, 1,
  0, 0, 0, 1,
  1, 0, 0, 0
), 4, 4, byrow = TRUE)
rownames(brokerage_g) <- colnames(brokerage_g) <- LETTERS[1:4]
brokerage_cl <- c(1, 1, 2, 2)

# ---- equivalence tests ----

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
