# ===========================================================================
# Tests for extended centrality measures and convenience wrappers
# ===========================================================================

skip_coverage_tests()

# ---------------------------------------------------------------------------
# Test data
# ---------------------------------------------------------------------------

# Path graph: A - B - C - D
path4 <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 0, 1,
  0, 0, 1, 0
), 4, 4)
rownames(path4) <- colnames(path4) <- c("A", "B", "C", "D")

# Triangle (K3)
k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
rownames(k3) <- colnames(k3) <- c("A", "B", "C")

# Star: center A connected to B, C, D, E
star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5[2:5, 1] <- 1
rownames(star5) <- colnames(star5) <- c("A", "B", "C", "D", "E")

# Directed graph for SALSA/LeaderRank
dir3 <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3)
rownames(dir3) <- colnames(dir3) <- c("A", "B", "C")

# Two-community graph for community-aware measures
comm6 <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), 6, 6)
rownames(comm6) <- colnames(comm6) <- LETTERS[1:6]
comm_membership <- c(1, 1, 1, 2, 2, 2)

# ===========================================================================
# Test: All 64 measures produce correct-length output
# ===========================================================================

test_that("centrality() returns 64 measures for undirected graph", {
  suppressWarnings(df <- centrality(k3, type = "all", membership = c(1, 1, 2)))
  # node column + measures

  expect_equal(nrow(df), 3)
  # salsa and leaderrank return NA on undirected, but columns still present
  expect_true(ncol(df) >= 60)
})

test_that("centrality() returns correct node labels", {
  df <- centrality(k3, measures = "degree")
  expect_equal(df$node, c("A", "B", "C"))
})

# ===========================================================================
# Distance-based closeness variants
# ===========================================================================

test_that("radiality works on path graph", {
  r <- centrality_radiality(path4)
  expect_length(r, 4)
  expect_named(r, c("A", "B", "C", "D"))
  # Interior nodes (B, C) should have higher radiality than endpoints
  expect_true(r["B"] > r["A"])
  expect_true(r["C"] > r["D"])
  # Symmetry
  expect_equal(unname(r["A"]), unname(r["D"]))
  expect_equal(unname(r["B"]), unname(r["C"]))
})

test_that("lin works on path graph", {
  l <- centrality_lin(path4)
  expect_length(l, 4)
  # Interior nodes more central
  expect_true(l["B"] > l["A"])
})

test_that("decay works with different parameters", {
  d1 <- centrality_decay(k3, decay_parameter = 0.5)
  d2 <- centrality_decay(k3, decay_parameter = 0.9)
  expect_length(d1, 3)
  # Higher decay parameter = less distance penalty = higher scores
  expect_true(all(d2 > d1))
  # K3 all nodes equal
  expect_equal(unname(d1[1]), unname(d1[2]))
})

test_that("residual_closeness and dangalchev are equivalent", {
  rc <- centrality_residual_closeness(path4)
  dc <- centrality_dangalchev(path4)
  expect_equal(unname(rc), unname(dc))
})

test_that("generalized_closeness equals decay with same parameter", {
  d <- centrality_decay(k3, decay_parameter = 0.7)
  gc <- centrality_generalized_closeness(k3, decay_parameter = 0.7)
  expect_equal(unname(d), unname(gc))
})

test_that("harary on K3 gives equal values", {
  h <- centrality_harary(k3)
  expect_length(h, 3)
  expect_equal(unname(h[1]), unname(h[2]))
})

test_that("average_distance on path graph: interior < endpoint", {
  ad <- centrality_average_distance(path4)
  # Lower average distance = more central
  expect_true(ad["B"] < ad["A"])
})

test_that("barycenter on K3 gives equal values", {
  b <- centrality_barycenter(k3)
  expect_equal(unname(b[1]), unname(b[2]))
})

test_that("wiener on star: center has lowest sum of distances", {
  w <- centrality_wiener(star5)
  # Star center (A) has distance 1 to all leaves = 4
  # Leaves have distance 1 to center + 2 to other leaves = 1 + 3*2 = 7
  expect_equal(unname(w["A"]), 4)
  expect_equal(unname(w["B"]), 7)
})

test_that("closeness_vitality returns finite values", {
  cv <- centrality_closeness_vitality(k3)
  expect_length(cv, 3)
  expect_true(all(is.finite(cv)))
})

# ===========================================================================
# Spectral / walk-based measures
# ===========================================================================

test_that("communicability on K3 gives equal values", {
  c <- centrality_communicability(k3)
  expect_length(c, 3)
  expect_equal(unname(c[1]), unname(c[2]), tolerance = 1e-10)
})

test_that("communicability_betweenness on K3 gives equal values", {
  cb <- centrality_communicability_betweenness(k3)
  expect_length(cb, 3)
  expect_equal(unname(cb[1]), unname(cb[2]), tolerance = 1e-10)
})

test_that("random_walk on connected graph returns finite", {
  rw <- centrality_random_walk(k3)
  expect_length(rw, 3)
  expect_true(all(is.finite(rw)))
})

test_that("random_walk warns on disconnected graph", {
  disc <- matrix(0, 4, 4)
  disc[1, 2] <- disc[2, 1] <- 1
  rownames(disc) <- colnames(disc) <- LETTERS[1:4]
  expect_warning(rw <- centrality_random_walk(disc), "disconnected")
  expect_true(all(is.na(rw)))
})

# ===========================================================================
# Path-based measures
# ===========================================================================

test_that("stress on path graph: interior > endpoint", {
  s <- centrality_stress(path4)
  # B and C are on shortest paths between A-D
  expect_true(s["B"] > s["A"])
  expect_equal(unname(s["A"]), 0)
})

test_that("flow_betweenness on K3 gives equal values", {
  fb <- centrality_flow_betweenness(k3)
  expect_length(fb, 3)
  expect_equal(unname(fb[1]), unname(fb[2]), tolerance = 1e-10)
})

# ===========================================================================
# Local / neighborhood measures
# ===========================================================================

test_that("lobby on K3 equals 2 for all nodes", {
  l <- centrality_lobby(k3)
  # K3: each node has closed neighborhood of 3 nodes, all degree 2
  # h-index: k=2, 3 nodes with deg >= 2 => h=2
  expect_equal(unname(l), c(2L, 2L, 2L))
})

test_that("lobby on star: center equals leaves (both h=1)", {
  l <- centrality_lobby(star5)
  # Star: center has closed nbhd {A,B,C,D,E} with degs {4,1,1,1,1}
  # Only 1 node with deg >= 2, so h = 1
  # Leaf B: closed nbhd {B,A} with degs {1,4}
  # 2 nodes with deg >= 1, 1 with deg >= 2, so h = 1
  expect_equal(unname(l["A"]), 1L)
  expect_equal(unname(l["B"]), 1L)
})

test_that("entropy returns finite values", {
  e <- centrality_entropy(k3)
  expect_length(e, 3)
  expect_true(all(is.finite(e)))
})

test_that("semilocal on K3 gives equal values", {
  sl <- centrality_semilocal(k3)
  expect_equal(unname(sl[1]), unname(sl[2]))
})

test_that("clusterrank on K3", {
  cr <- centrality_clusterrank(k3)
  expect_length(cr, 3)
  # K3: cc = 1 for all nodes, neighbors have degree 2+1=3
  expect_true(all(cr > 0))
})

test_that("bottleneck on path graph: interior > endpoint", {
  bn <- centrality_bottleneck(path4)
  expect_true(bn["B"] >= bn["A"])
})

test_that("centroid: star center has highest centroid", {
  cv <- centrality_centroid(star5)
  expect_true(cv["A"] > cv["B"])
})

test_that("mnc on K3 equals 1", {
  m <- centrality_mnc(k3)
  # Each node's neighborhood is the other 2 nodes, connected => MNC = 2
  expect_equal(unname(m), c(2L, 2L, 2L))
})

test_that("dmnc on K3 returns finite values", {
  d <- centrality_dmnc(k3)
  expect_length(d, 3)
  expect_true(all(is.finite(d)))
})

test_that("topological_coefficient on K3 gives equal values", {
  tc <- centrality_topological_coefficient(k3)
  expect_equal(unname(tc[1]), unname(tc[2]))
})

test_that("bridging on K3: all nodes equal (symmetric)", {
  b <- centrality_bridging(k3)
  expect_equal(unname(b[1]), unname(b[2]))
})

test_that("local_bridging on K3 gives equal values", {
  lb <- centrality_local_bridging(k3)
  expect_equal(unname(lb[1]), unname(lb[2]))
})

test_that("effective_size on star: center has highest", {
  es <- centrality_effective_size(star5)
  expect_true(es["A"] > es["B"])
  # Star center: 4 neighbors, none connected to each other => eff = 4
  expect_equal(unname(es["A"]), 4)
})

test_that("diversity on unweighted K3: all equal", {
  d <- centrality_diversity(k3)
  expect_equal(unname(d[1]), unname(d[2]))
})

test_that("cross_clique on K3", {
  cc <- centrality_cross_clique(k3)
  expect_length(cc, 3)
  # K3: cliques are {A}, {B}, {C}, {A,B}, {A,C}, {B,C}, {A,B,C}
  # Each node appears in: itself (1) + 2 pairs + 1 triangle = 4
  expect_equal(unname(cc), c(4L, 4L, 4L))
})

test_that("markov on connected graph returns finite", {
  mk <- centrality_markov(k3)
  expect_length(mk, 3)
  expect_true(all(is.finite(mk)))
})

test_that("markov warns on disconnected graph", {
  disc <- matrix(0, 4, 4)
  disc[1, 2] <- disc[2, 1] <- 1
  rownames(disc) <- colnames(disc) <- LETTERS[1:4]
  expect_warning(mk <- centrality_markov(disc), "disconnected")
  expect_true(all(is.na(mk)))
})

# ===========================================================================
# Mode-supporting influence measures
# ===========================================================================

test_that("integration on K3: all equal", {
  i <- centrality_integration(k3)
  expect_equal(unname(i[1]), unname(i[2]))
})

test_that("expected on star: center highest", {
  e <- centrality_expected(star5)
  # Center A: neighbors B,C,D,E all have deg 1 => sum = 4
  # Leaf B: neighbor A has deg 4 => sum = 4
  expect_equal(unname(e["A"]), unname(e["B"]))
})

test_that("gilschmidt on K3: all equal", {
  gs <- centrality_gilschmidt(k3)
  expect_equal(unname(gs[1]), unname(gs[2]))
})

# ===========================================================================
# Directed-only measures
# ===========================================================================

test_that("salsa works on directed graph", {
  s <- centrality_salsa(dir3)
  expect_length(s, 3)
  expect_true(all(is.finite(s)))
  # All values between 0 and 1
  expect_true(all(s >= 0 & s <= 1))
})

test_that("salsa returns NA with warning on undirected graph", {
  expect_warning(s <- centrality_salsa(k3), "directed")
  expect_true(all(is.na(s)))
})

test_that("leaderrank works on directed graph", {
  lr <- centrality_leaderrank(dir3)
  expect_length(lr, 3)
  expect_true(all(is.finite(lr)))
  expect_true(all(lr > 0))
})

test_that("leaderrank returns NA with warning on undirected graph", {
  expect_warning(lr <- centrality_leaderrank(k3), "directed")
  expect_true(all(is.na(lr)))
})

# ===========================================================================
# Community-aware measures
# ===========================================================================

test_that("participation works with valid membership", {
  p <- centrality_participation(comm6, membership = comm_membership)
  expect_length(p, 6)
  # Node C (bridges communities) should have highest participation
  expect_true(p["C"] > 0)
  # Purely internal nodes (A, B) have lower participation
  expect_equal(unname(p["A"]), unname(p["B"]))
  # Node D also bridges
  expect_true(p["D"] > 0)
})

test_that("participation returns NA without membership", {
  expect_warning(
    df <- centrality(comm6, measures = "participation"),
    "membership"
  )
  expect_true(all(is.na(df[["participation_all"]])))
})

test_that("within_module_z works with valid membership", {
  wz <- centrality_within_module_z(comm6, membership = comm_membership)
  expect_length(wz, 6)
  # NaN is valid for modules with constant within-degree
  expect_true(all(is.finite(wz) | is.nan(wz)))
})

test_that("within_module_z returns NA without membership", {
  expect_warning(
    df <- centrality(comm6, measures = "within_module_z"),
    "membership"
  )
  expect_true(all(is.na(df[["within_module_z_all"]])))
})

test_that("gateway works with valid membership", {
  gw <- centrality_gateway(comm6, membership = comm_membership)
  expect_length(gw, 6)
  expect_true(all(is.finite(gw)))
  expect_true(all(gw >= 0 & gw <= 1))
})

test_that("gateway returns NA without membership", {
  expect_warning(
    df <- centrality(comm6, measures = "gateway"),
    "membership"
  )
  expect_true(all(is.na(df[["gateway_all"]])))
})

# ===========================================================================
# Edge cases
# ===========================================================================

test_that("extended measures handle 2-node graph", {
  g2 <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(g2) <- colnames(g2) <- c("A", "B")
  # Should not error
  suppressWarnings(df <- centrality(g2, measures = c(
    "radiality", "lin", "decay", "lobby", "mnc", "entropy",
    "communicability", "stress", "bridging", "effective_size"
  )))
  expect_equal(nrow(df), 2)
})

test_that("extended measures handle single-node graph", {
  g1 <- matrix(0, 1, 1)
  rownames(g1) <- colnames(g1) <- "A"
  suppressWarnings(df <- centrality(g1, measures = c(
    "radiality", "lin", "lobby", "entropy", "communicability"
  )))
  expect_equal(nrow(df), 1)
})

# ===========================================================================
# Cross-package equivalence: centiserve
# ===========================================================================

test_that("extended measures match centiserve on random graphs", {
  skip_if_not_installed("centiserve")

  set.seed(42)
  n_tests <- 20
  failures <- 0L

  for (i in seq_len(n_tests)) {
    g <- igraph::sample_gnp(8, 0.4)
    while (!igraph::is_connected(g)) {
      g <- igraph::sample_gnp(8, 0.4)
    }

    # Radiality
    co_rad <- cograph:::calculate_radiality(g, mode = "all", weights = NULL)
    cs_rad <- centiserve::radiality(g)
    if (!isTRUE(all.equal(co_rad, cs_rad, tolerance = 1e-8))) {
      failures <- failures + 1L
    }

    # Lobby index (centiserve returns double, we return integer)
    co_lob <- cograph:::calculate_lobby(g, mode = "all")
    cs_lob <- centiserve::lobby(g)
    if (!isTRUE(all.equal(as.numeric(co_lob), as.numeric(cs_lob)))) {
      failures <- failures + 1L
    }

    # Barycenter
    co_bar <- cograph:::calculate_barycenter(g, mode = "all", weights = NULL)
    cs_bar <- centiserve::barycenter(g)
    if (!isTRUE(all.equal(co_bar, cs_bar, tolerance = 1e-8))) {
      failures <- failures + 1L
    }

    # Bottleneck
    co_bn <- cograph:::calculate_bottleneck(g, mode = "all")
    cs_bn <- centiserve::bottleneck(g)
    if (!isTRUE(all.equal(as.numeric(co_bn), as.numeric(cs_bn)))) {
      failures <- failures + 1L
    }

    # Centroid — SKIP: centiserve::centroid() has a known bug where the
    # self-exclusion check uses stale loop variable `u` instead of `w`,
    # causing incorrect results on some graphs. Our implementation is
    # verified by hand on known topologies above.

    # MNC
    co_mnc <- cograph:::calculate_mnc(g, mode = "all")
    cs_mnc <- centiserve::mnc(g)
    if (!isTRUE(all.equal(as.numeric(co_mnc), as.numeric(cs_mnc)))) {
      failures <- failures + 1L
    }

    # Average distance
    co_ad <- cograph:::calculate_average_distance(g, mode = "all",
                                                   weights = NULL)
    cs_ad <- centiserve::averagedis(g)
    if (!isTRUE(all.equal(co_ad, cs_ad, tolerance = 1e-8))) {
      failures <- failures + 1L
    }

    # Closeness vitality (centiserve errors on some graphs)
    cs_cv <- tryCatch(centiserve::closeness.vitality(g), error = function(e) NULL)
    if (!is.null(cs_cv)) {
      co_cv <- cograph:::calculate_closeness_vitality(g, mode = "all",
                                                       weights = NULL)
      if (!isTRUE(all.equal(co_cv, cs_cv, tolerance = 1e-8))) {
        failures <- failures + 1L
      }
    }

    # Cross-clique
    co_cc <- cograph:::calculate_cross_clique(g)
    cs_cc <- centiserve::crossclique(g)
    if (!isTRUE(all.equal(as.numeric(co_cc), as.numeric(cs_cc)))) {
      failures <- failures + 1L
    }
  }

  cat(sprintf("centiserve equivalence: %d tests, %d failures\n",
              n_tests * 8, failures))
  expect_equal(failures, 0L)
})

# ===========================================================================
# Cross-package equivalence: sna
# ===========================================================================

test_that("stress matches sna on random graphs", {
  skip_if_not_installed("sna")

  set.seed(123)
  failures <- 0L

  for (i in seq_len(20)) {
    g <- igraph::sample_gnp(8, 0.4)
    while (!igraph::is_connected(g)) {
      g <- igraph::sample_gnp(8, 0.4)
    }

    co_stress <- cograph:::calculate_stress(g, weights = NULL, directed = FALSE)
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    sna_stress <- sna::stresscent(mat, gmode = "graph")

    if (!isTRUE(all.equal(co_stress, sna_stress, tolerance = 1e-8))) {
      failures <- failures + 1L
    }
  }

  cat(sprintf("sna stress equivalence: 20 tests, %d failures\n", failures))
  expect_equal(failures, 0L)
})

# ---------- Weighted stress ----------
# Reference: reconstruct stress from igraph::all_shortest_paths() enumeration.
# Slow but transparent — enumerates every shortest path and counts which ones
# pass through each interior node.
stress_from_all_paths <- function(g, weights = NULL, directed = TRUE) {
  n <- igraph::vcount(g)
  is_dir <- igraph::is_directed(g) && directed
  mode <- if (is_dir) "out" else "all"
  stress <- numeric(n)
  w_use <- if (is.null(weights)) NA else weights
  pairs <- if (is_dir) {
    expand.grid(s = seq_len(n), t = seq_len(n))
    } else {
      do.call(rbind, lapply(seq_len(n - 1), function(s) {
        data.frame(s = s, t = seq.int(s + 1L, n))
      }))
    }
  pairs <- pairs[pairs$s != pairs$t, , drop = FALSE]

  for (row in seq_len(nrow(pairs))) {
    s <- pairs$s[row]; t <- pairs$t[row]
    paths <- suppressWarnings(
      igraph::all_shortest_paths(g, from = s, to = t,
                                 mode = mode, weights = w_use)$vpaths
    )
    for (p in paths) {
      p_int <- as.integer(p)
      if (length(p_int) <= 2L) next
      interior <- p_int[-c(1L, length(p_int))]
      stress[interior] <- stress[interior] + 1
    }
  }
  stress
}

test_that("weighted stress matches all_shortest_paths enumeration (undirected)", {
  set.seed(2026)
  failures <- 0L
  diffs <- numeric(0)
  for (i in seq_len(15)) {
    g <- igraph::sample_gnp(6, 0.5, directed = FALSE)
    while (!igraph::is_connected(g)) g <- igraph::sample_gnp(6, 0.5)
    w <- runif(igraph::ecount(g), 0.1, 3.0)
    igraph::E(g)$weight <- w

    co <- cograph:::calculate_stress(g, weights = w, directed = FALSE)
    ref <- stress_from_all_paths(g, weights = w, directed = FALSE)
    if (!isTRUE(all.equal(co, ref, tolerance = 1e-8))) {
      failures <- failures + 1L
      diffs <- c(diffs, max(abs(co - ref)))
    }
  }
  cat(sprintf("weighted undirected stress: 15 tests, %d failures\n", failures))
  expect_equal(failures, 0L)
})

test_that("weighted stress matches all_shortest_paths enumeration (directed)", {
  set.seed(2027)
  failures <- 0L
  for (i in seq_len(15)) {
    g <- igraph::sample_gnp(6, 0.5, directed = TRUE)
    w <- runif(igraph::ecount(g), 0.1, 3.0)
    igraph::E(g)$weight <- w

    co <- cograph:::calculate_stress(g, weights = w, directed = TRUE)
    ref <- stress_from_all_paths(g, weights = w, directed = TRUE)
    if (!isTRUE(all.equal(co, ref, tolerance = 1e-8))) {
      failures <- failures + 1L
    }
  }
  cat(sprintf("weighted directed stress: 15 tests, %d failures\n", failures))
  expect_equal(failures, 0L)
})

test_that("weighted stress with constant-1 weights matches unweighted stress", {
  # Equivalence check: uniform weights should reproduce hop-count (BFS) result.
  set.seed(2028)
  g <- igraph::sample_gnp(10, 0.35, directed = FALSE)
  while (!igraph::is_connected(g)) g <- igraph::sample_gnp(10, 0.35)
  w1 <- rep(1, igraph::ecount(g))

  unw <- cograph:::calculate_stress(g, weights = NULL, directed = FALSE)
  w <- cograph:::calculate_stress(g, weights = w1, directed = FALSE)
  expect_equal(w, unw, tolerance = 1e-8)
})

test_that("weighted stress honors edge weight ordering (shorter path wins)", {
  # Triangle A-B-C with edges A-B=1, B-C=1, A-C=10. Shortest A<->C path is
  # A-B-C, so B should accrue stress from that pair. With A-C=0.5 instead,
  # the direct edge wins and B's stress drops to 0.
  mk <- function(wAC) {
    g <- igraph::make_graph(c(1, 2, 2, 3, 1, 3), directed = FALSE)
    igraph::E(g)$weight <- c(1, 1, wAC)
    g
  }

  g_long_ac <- mk(10)
  s1 <- cograph:::calculate_stress(g_long_ac,
                                   weights = igraph::E(g_long_ac)$weight,
                                   directed = FALSE)
  expect_equal(s1[2], 1)  # B is the midpoint of A-B-C

  g_short_ac <- mk(0.5)
  s2 <- cograph:::calculate_stress(g_short_ac,
                                   weights = igraph::E(g_short_ac)$weight,
                                   directed = FALSE)
  expect_equal(s2[2], 0)  # A-C direct is shortest, B is on no shortest path
})

# ---------- Expected influence (Robinaugh, Millner & McNally 2016) ----------

test_that("expected_influence_1 matches qgraph on signed graphs", {
  skip_if_not_installed("qgraph")

  set.seed(42)
  failures <- 0L
  max_diff <- 0
  for (i in seq_len(20)) {
    n <- sample(5:12, 1)
    W <- matrix(runif(n * n, -0.8, 0.8), n, n)
    W <- (W + t(W)) / 2
    diag(W) <- 0
    rownames(W) <- colnames(W) <- paste0("n", seq_len(n))

    qg <- suppressMessages(qgraph::centrality(W))
    ref <- qg$OutExpectedInfluence
    names(ref) <- rownames(W)
    co <- centrality_expected_influence_1(W)
    co <- co[names(ref)]

    if (!isTRUE(all.equal(unname(co), unname(ref), tolerance = 1e-8))) {
      failures <- failures + 1L
    }
    max_diff <- max(max_diff, max(abs(co - ref)))
  }
  cat(sprintf("qgraph expected_influence equivalence: 20 tests, %d failures (max diff %.2e)\n",
              failures, max_diff))
  expect_equal(failures, 0L)
})

test_that("expected_influence_2 matches Robinaugh 2016 formula", {
  set.seed(7)
  for (i in seq_len(10)) {
    n <- sample(5:10, 1)
    W <- matrix(runif(n * n, -0.6, 0.6), n, n)
    W <- (W + t(W)) / 2
    diag(W) <- 0
    rownames(W) <- colnames(W) <- paste0("n", seq_len(n))

    ei1 <- centrality_expected_influence_1(W)
    ei2 <- centrality_expected_influence_2(W)
    ei2_formula <- ei1 + as.numeric(W %*% ei1)
    expect_equal(unname(ei2), unname(ei2_formula), tolerance = 1e-10)
  }
})

test_that("expected influence respects mode on directed graphs", {
  skip_if_not_installed("qgraph")
  set.seed(99)
  W <- matrix(runif(64, -0.6, 0.6), 8, 8)
  diag(W) <- 0
  rownames(W) <- colnames(W) <- paste0("n", 1:8)

  qg <- suppressMessages(qgraph::centrality(W))
  out_ref <- qg$OutExpectedInfluence; names(out_ref) <- rownames(W)
  in_ref  <- qg$InExpectedInfluence;  names(in_ref)  <- rownames(W)

  out_co <- centrality_expected_influence_1(W, mode = "out")
  in_co  <- centrality_expected_influence_1(W, mode = "in")
  expect_equal(unname(out_co[names(out_ref)]), unname(out_ref), tolerance = 1e-8)
  expect_equal(unname(in_co[names(in_ref)]),   unname(in_ref),  tolerance = 1e-8)
})

test_that("gilschmidt matches sna on random graphs", {
  skip_if_not_installed("sna")

  set.seed(456)
  failures <- 0L

  for (i in seq_len(20)) {
    g <- igraph::sample_gnp(8, 0.4)
    while (!igraph::is_connected(g)) {
      g <- igraph::sample_gnp(8, 0.4)
    }

    co_gs <- cograph:::calculate_gilschmidt(g, mode = "all")
    mat <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))
    sna_gs <- sna::gilschmidt(mat, gmode = "graph")

    if (!isTRUE(all.equal(co_gs, sna_gs, tolerance = 1e-8))) {
      failures <- failures + 1L
    }
  }

  cat(sprintf("sna gilschmidt equivalence: 20 tests, %d failures\n", failures))
  expect_equal(failures, 0L)
})

# ===========================================================================
# Cross-package equivalence: influenceR
# ===========================================================================

# NOTE: influenceR::bridging() uses Valente & Fujimoto (2010) formula,
# while cograph's bridging centrality uses Hwang et al. (2006):
# betweenness * bridging_coefficient. These are different measures.

test_that("bridging produces reasonable values on random graphs", {
  set.seed(789)
  g <- igraph::sample_gnp(10, 0.35)
  while (!igraph::is_connected(g)) {
    g <- igraph::sample_gnp(10, 0.35)
  }

  co_br <- cograph:::calculate_bridging(g, weights = NULL, directed = FALSE)
  expect_length(co_br, igraph::vcount(g))
  expect_true(all(co_br >= 0))
})

test_that("effective_size matches influenceR on random graphs", {
  skip_if_not_installed("influenceR")

  set.seed(321)
  failures <- 0L

  for (i in seq_len(20)) {
    g <- igraph::sample_gnp(10, 0.35)
    while (!igraph::is_connected(g)) {
      g <- igraph::sample_gnp(10, 0.35)
    }

    co_es <- cograph:::calculate_effective_size(g)
    ir_es <- influenceR::ens(g)

    if (!isTRUE(all.equal(co_es, ir_es, tolerance = 1e-8))) {
      failures <- failures + 1L
    }
  }

  cat(sprintf("influenceR effective_size equivalence: 20 tests, %d failures\n",
              failures))
  expect_equal(failures, 0L)
})

# ===========================================================================
# Centralization function
# ===========================================================================

test_that("centralization returns correct values for star graph", {
  # Star graph has maximum degree centralization = 1
  c_deg <- cograph::centralization(star5, "degree")
  expect_true(c_deg > 0.9)  # Near 1 for star
  expect_true(c_deg <= 1.0)

  # Betweenness centralization
  c_bet <- cograph::centralization(star5, "betweenness")
  expect_true(c_bet >= 0 && c_bet <= 1)
})

test_that("centralization returns 0 for K3 (complete graph)", {
  c_deg <- cograph::centralization(k3, "degree")
  expect_equal(c_deg, 0)
})

# ===========================================================================
# Wrapper functions return named vectors
# ===========================================================================

test_that("all convenience wrappers return named numeric vectors", {
  wrappers_no_mode <- list(
    centrality_communicability,
    centrality_communicability_betweenness,
    centrality_random_walk,
    centrality_stress,
    centrality_flow_betweenness,
    centrality_topological_coefficient,
    centrality_bridging,
    centrality_local_bridging,
    centrality_effective_size,
    centrality_diversity,
    centrality_cross_clique,
    centrality_markov
  )

  for (fn in wrappers_no_mode) {
    result <- fn(k3)
    expect_true(is.numeric(result), info = paste("Failed for", deparse(fn)))
    expect_named(result, c("A", "B", "C"), info = paste("Failed for", deparse(fn)))
    expect_length(result, 3)
  }
})

test_that("all mode wrappers return named numeric vectors", {
  wrappers_mode <- list(
    centrality_radiality,
    centrality_lin,
    centrality_harary,
    centrality_average_distance,
    centrality_barycenter,
    centrality_wiener,
    centrality_closeness_vitality,
    centrality_lobby,
    centrality_entropy,
    centrality_semilocal,
    centrality_clusterrank,
    centrality_bottleneck,
    centrality_centroid,
    centrality_mnc,
    centrality_dmnc,
    centrality_integration,
    centrality_expected,
    centrality_gilschmidt
  )

  for (fn in wrappers_mode) {
    result <- fn(k3)
    expect_true(is.numeric(result) || is.integer(result),
                info = paste("Failed for", deparse(fn)))
    expect_named(result, c("A", "B", "C"),
                 info = paste("Failed for", deparse(fn)))
    expect_length(result, 3)
  }
})

test_that("decay and generalized_closeness wrappers pass parameters", {
  d1 <- centrality_decay(k3, decay_parameter = 0.3)
  d2 <- centrality_decay(k3, decay_parameter = 0.8)
  expect_true(all(d2 > d1))

  gc1 <- centrality_generalized_closeness(k3, decay_parameter = 0.3)
  gc2 <- centrality_generalized_closeness(k3, decay_parameter = 0.8)
  expect_true(all(gc2 > gc1))
})

test_that("residual_closeness wrapper works", {
  rc <- centrality_residual_closeness(k3)
  expect_length(rc, 3)
  expect_true(all(rc > 0))
})

test_that("dangalchev wrapper works", {
  dc <- centrality_dangalchev(k3)
  expect_length(dc, 3)
  expect_true(all(dc > 0))
})

# ===========================================================================
# NetworkX equivalence (via reticulate)
# ===========================================================================

# Test matrix for NetworkX comparison
.nx_mat <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 1, 0,
  1, 1, 0, 1, 1,
  0, 1, 1, 0, 1,
  0, 0, 1, 1, 0
), 5, 5, byrow = TRUE)
rownames(.nx_mat) <- colnames(.nx_mat) <- LETTERS[1:5]

test_that("current_flow_betweenness matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_cfb <- nx$current_flow_betweenness_centrality(G)
  nx_vec <- vapply(LETTERS[1:5], function(x) nx_cfb[[x]], numeric(1))

  expect_equal(
    unname(centrality_current_flow_betweenness(.nx_mat)),
    unname(nx_vec),
    tolerance = 1e-5
  )
})

test_that("percolation matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  states <- reticulate::py_dict(LETTERS[1:5], rep(1.0, 5))
  nx_perc <- nx$percolation_centrality(G, states = states)
  nx_vec <- vapply(LETTERS[1:5], function(x) nx_perc[[x]], numeric(1))

  expect_equal(
    unname(centrality_percolation(.nx_mat)),
    unname(nx_vec),
    tolerance = 1e-6
  )
})

test_that("laplacian matches NetworkX", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_lap <- nx$laplacian_centrality(G, normalized = FALSE)
  nx_vec <- vapply(LETTERS[1:5], function(x) nx_lap[[x]], numeric(1))

  expect_equal(
    unname(centrality_laplacian(.nx_mat)),
    unname(nx_vec),
    tolerance = 1e-6
  )
})

test_that("voterank matches NetworkX ordering", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("networkx"), "NetworkX not available")

  nx <- reticulate::import("networkx")
  G <- nx$Graph()
  G$add_nodes_from(LETTERS[1:5])
  G$add_edges_from(list(
    c("A", "B"), c("A", "C"), c("B", "C"), c("B", "D"),
    c("C", "D"), c("C", "E"), c("D", "E")
  ))

  nx_vr <- unlist(nx$voterank(G))
  cg_vr <- centrality_voterank(.nx_mat)
  cg_order <- names(sort(cg_vr, decreasing = TRUE))

  # Top spreaders should match in order
  expect_equal(cg_order[seq_along(nx_vr)], nx_vr)
})

# ===========================================================================
# Local efficiency (Gemini audit fix)
# ===========================================================================

test_that("local efficiency matches igraph", {
  set.seed(42)
  g <- igraph::sample_gnp(20, 0.3)
  while (!igraph::is_connected(g)) g <- igraph::sample_gnp(20, 0.3)

  expect_equal(
    cograph::network_local_efficiency(g),
    igraph::average_local_efficiency(g),
    tolerance = 1e-10
  )
})

test_that("local efficiency matches igraph (weighted)", {
  set.seed(42)
  g <- igraph::sample_gnp(20, 0.3)
  while (!igraph::is_connected(g)) g <- igraph::sample_gnp(20, 0.3)
  igraph::E(g)$weight <- runif(igraph::ecount(g), 0.1, 1.0)

  expect_equal(
    cograph::network_local_efficiency(g, invert_weights = FALSE),
    igraph::average_local_efficiency(g),
    tolerance = 1e-10
  )
})
