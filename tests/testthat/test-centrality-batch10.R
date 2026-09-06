# ===========================================================================
# Batch 10 — the node measures other centrality packages exposed and
# `centrality()` did not: local efficiency, s-core, distance-weighted
# fragmentation, the k-path census and the edge percolated component.
#
# Each measure is held to its defining paper through a hand-computed value
# on a graph small enough to check by eye, to a brute-force reading of the
# definition, and to the package whose gap it closes.
# ===========================================================================

# A six-node graph: a triangle A-B-C, C-D, and a triangle D-E-F.
adj6 <- matrix(0, 6, 6)
adj6[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj6 <- adj6 + t(adj6)
rownames(adj6) <- colnames(adj6) <- LETTERS[1:6]

star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5 <- star5 + t(star5)
rownames(star5) <- colnames(star5) <- LETTERS[1:5]

# ===========================================================================
# Local efficiency
# ===========================================================================

test_that("local efficiency is the efficiency of the neighbour subgraph", {
  eff <- centrality_local_efficiency(adj6)
  # A's neighbours are B and C, which are adjacent: one pair at distance 1,
  # so the mean of the two ordered inverse distances is 1.
  expect_equal(unname(eff["A"]), 1)
  # C's neighbours are A, B, D. A-B is an edge; D is isolated inside the
  # subgraph. Two ordered pairs of six contribute 1 each.
  expect_equal(unname(eff["C"]), 2 / 6)
  # E's neighbours are D and F, adjacent, so 1 again.
  expect_equal(unname(eff["E"]), 1)
})

test_that("a hub whose neighbours are unconnected has zero local efficiency", {
  eff <- centrality_local_efficiency(star5)
  expect_equal(unname(eff["A"]), 0)          # four neighbours, no edges
  expect_true(all(eff == 0))                 # leaves have one neighbour
})

test_that("local efficiency matches brainGraph on a real network", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("brainGraph")
  skip_on_cran()
  g <- igraph::make_graph("Zachary")
  expect_equal(
    centrality(g, measures = "local_efficiency")$local_efficiency_all,
    unname(brainGraph::efficiency(g, type = "local", use.parallel = FALSE))
  )
})

test_that("local efficiency is not igraph's local_efficiency", {
  skip_if_not_installed("igraph")
  # The four-cycle separates the two conventions: after deleting a node its
  # two neighbours are two steps apart through the fourth node, which igraph
  # counts and the induced subgraph does not.
  c4 <- igraph::make_ring(4)
  expect_equal(centrality(c4, measures = "local_efficiency")$local_efficiency_all,
               rep(0, 4))
  expect_equal(igraph::local_efficiency(c4, weights = NA), rep(0.5, 4))
})

# ===========================================================================
# s-core
# ===========================================================================

test_that("the s-core reduces to the k-core when weights are absent", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  expect_equal(centrality(g, measures = "s_core")$s_core,
               unname(as.numeric(igraph::coreness(g))))
})

test_that("the s-core index is the largest surviving strength threshold", {
  # A weighted triangle A-B-C with heavy edges plus a light pendant D.
  w <- matrix(0, 4, 4)
  w[1, 2] <- w[1, 3] <- w[2, 3] <- 5
  w[3, 4] <- 1
  w <- w + t(w)
  rownames(w) <- colnames(w) <- LETTERS[1:4]
  s <- centrality_s_core(w)
  # D has strength 1 and leaves first; the triangle then has strength 10 each.
  expect_equal(unname(s), c(10, 10, 10, 1))
})

test_that("isolates score zero and a single node is handled", {
  m <- matrix(0, 3, 3)
  m[1, 2] <- m[2, 1] <- 1
  rownames(m) <- colnames(m) <- LETTERS[1:3]
  expect_equal(unname(centrality_s_core(m)), c(1, 1, 0))
})

# ===========================================================================
# Distance-weighted fragmentation
# ===========================================================================

test_that("fragmentation follows Borgatti's definition on the six-node graph", {
  frag <- centrality_fragmentation(adj6)
  # Deleting C cuts the graph into {A, B} and {D, E, F}. The surviving
  # inverse distances are 2 (A-B both ways) and 6 (the D-E-F triangle),
  # over the (n-1)(n-2) = 20 ordered pairs.
  expect_equal(unname(frag["C"]), 1 - 8 / 20)
  # Deleting A leaves B-C-D-E-F connected, so the score is lower.
  expect_lt(unname(frag["A"]), unname(frag["C"]))
  expect_true(all(frag >= 0 & frag <= 1))
})

test_that("fragmentation matches keyplayer::fragment", {
  skip_if_not_installed("keyplayer")
  skip_if_not_installed("sna")
  skip_on_cran()
  expect_equal(
    unname(centrality_fragmentation(adj6)),
    as.numeric(keyplayer::fragment(adj6, binary = TRUE, large = FALSE))
  )
})

test_that("fragmentation needs three nodes", {
  m <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(m) <- colnames(m) <- c("A", "B")
  expect_true(all(is.nan(centrality_fragmentation(m))))
})

# ===========================================================================
# k-path census
# ===========================================================================

test_that("paths of length one reproduce degree", {
  expect_equal(unname(centrality_kpath(adj6, kpath_len = 1)),
               unname(centrality_degree(adj6)))
  expect_equal(unname(centrality_kpath(star5, kpath_len = 1)),
               c(4, 1, 1, 1, 1))
})

test_that("the k-path census counts each undirected path once", {
  # A path A-B-C: one path of length 2 (A-B-C) and two of length 1.
  p3 <- matrix(0, 3, 3)
  p3[1, 2] <- p3[2, 3] <- 1
  p3 <- p3 + t(p3)
  rownames(p3) <- colnames(p3) <- LETTERS[1:3]
  expect_equal(unname(centrality_kpath(p3, kpath_len = 2)), c(2, 3, 2))
})

test_that("the k-path census matches sna::kpath.census", {
  skip_if_not_installed("sna")
  skip_on_cran()
  ref <- sna::kpath.census(adj6, maxlen = 3, mode = "graph",
                           tabulate.by.vertex = TRUE)$path.count
  expect_equal(unname(centrality_kpath(adj6, kpath_len = 3)),
               unname(colSums(ref)[-1]))
})

test_that("direction is followed when mode is out", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
  out <- centrality(g, measures = "kpath", mode = "out", kpath_len = 2)$kpath_out
  # Directed paths: 1->2, 2->3, 1->2->3.
  expect_equal(out, c(2, 3, 2))
})

# ===========================================================================
# Edge percolated component
# ===========================================================================

test_that("EPC is reproducible with a seed and varies without one", {
  a <- centrality_epc(adj6, epc_runs = 100, epc_seed = 42)
  b <- centrality_epc(adj6, epc_runs = 100, epc_seed = 42)
  expect_equal(a, b)
  set.seed(1)
  c1 <- centrality_epc(adj6, epc_runs = 100)
  c2 <- centrality_epc(adj6, epc_runs = 100)
  expect_false(isTRUE(all.equal(c1, c2)))
})

test_that("EPC leaves the caller's random stream alone", {
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  invisible(centrality_epc(adj6, epc_runs = 20, epc_seed = 7))
  expect_equal(stats::runif(1), before)
})

test_that("EPC recovers the exact percolation mean on a triangle", {
  # Each of the three edges survives with probability 1/2, so over the eight
  # equally likely configurations a node sits in a component of size 3 in the
  # four with two or three edges, size 2 in the two single-edge cases that
  # touch it, and size 1 in the remaining two: (4*3 + 2*2 + 2*1) / 8 = 2.25,
  # which is 0.75 of the three nodes.
  tri <- matrix(1, 3, 3)
  diag(tri) <- 0
  rownames(tri) <- colnames(tri) <- LETTERS[1:3]
  est <- centrality_epc(tri, epc_runs = 20000, epc_seed = 3)
  expect_equal(unname(est), rep(0.75, 3), tolerance = 0.01)
})

test_that("EPC does not move when the number of runs changes", {
  a <- centrality_epc(adj6, epc_runs = 2000, epc_seed = 5)
  b <- centrality_epc(adj6, epc_runs = 8000, epc_seed = 5)
  expect_equal(unname(a), unname(b), tolerance = 0.05)
  expect_true(all(a > 0 & a <= 1))
})

test_that("EPC is centiserve's number divided by the run count", {
  skip_if_not_installed("centiserve")
  skip_if_not_installed("igraph")
  skip_on_cran()
  # Different random draws, so the two agree in scale rather than exactly.
  # centiserve is fixed at 1000 runs, whose standard error on a component
  # share is about 0.011; over 34 nodes the largest gap runs to a few of
  # those, so the bound is set at 0.06 rather than at the per-node error.
  g <- igraph::make_graph("Zachary")
  set.seed(4)
  ref <- as.numeric(centiserve::epc(g)) / 1000
  mine <- centrality(g, measures = "epc", epc_runs = 20000, epc_seed = 4)$epc
  expect_lt(max(abs(mine - ref)), 0.06)
  expect_gt(stats::cor(mine, ref, method = "kendall"), 0.9)
})

test_that("a graph with no edges gives every node the same score", {
  m <- matrix(0, 4, 4)
  rownames(m) <- colnames(m) <- LETTERS[1:4]
  expect_equal(unname(centrality_epc(m, epc_runs = 10, epc_seed = 1)),
               rep(1 / 4, 4))
})

# ===========================================================================
# Wiring into centrality()
# ===========================================================================

test_that("the new measures are listed and the costly ones held back", {
  tab <- list_centralities()
  new <- c("local_efficiency", "s_core", "fragmentation", "kpath", "epc")
  expect_true(all(new %in% tab$measure))
  costly <- list_centralities(costly = TRUE)$measure
  expect_true(all(c("fragmentation", "epc") %in% costly))
  all_df <- suppressWarnings(centrality(adj6, type = "all"))
  expect_true(all(c("local_efficiency_all", "s_core", "kpath_all") %in%
                    names(all_df)))
  expect_false(any(c("fragmentation_all", "epc") %in% names(all_df)))
})

test_that("empty and single-node graphs return the right shape", {
  skip_if_not_installed("igraph")
  g0 <- igraph::make_empty_graph(0)
  for (m in c("local_efficiency", "s_core", "fragmentation", "kpath", "epc")) {
    expect_equal(nrow(centrality(g0, measures = m)), 0L, info = m)
  }
  g1 <- igraph::make_empty_graph(1)
  expect_equal(centrality(g1, measures = "s_core")$s_core, 0)
  expect_equal(centrality(g1, measures = "local_efficiency")[[2]], 0)
})

test_that("mode-aware columns carry the suffix and no-mode ones do not", {
  df <- suppressWarnings(centrality(
    adj6, measures = c("local_efficiency", "s_core", "kpath")))
  expect_named(df, c("node", "local_efficiency_all", "s_core", "kpath_all"))
})

# ===========================================================================
# Gaps that turned out not to be gaps
#
# Three functions the cross-coverage document listed as missing are measures
# cograph already computed under another name. These pin that claim.
# ===========================================================================

test_that("centiserve::closeness.latora is cograph's harmonic centrality", {
  skip_if_not_installed("centiserve")
  skip_if_not_installed("igraph")
  skip_on_cran()
  g <- igraph::make_graph("Zachary")
  expect_equal(centrality(g, measures = "harmonic")$harmonic_all,
               unname(centiserve::closeness.latora(g)))
})

test_that("brainGraph nodal efficiency is harmonic centrality over n - 1", {
  skip_if_not_installed("brainGraph")
  skip_if_not_installed("igraph")
  skip_on_cran()
  g <- igraph::make_graph("Zachary")
  expect_equal(
    centrality(g, measures = "harmonic")$harmonic_all / (igraph::vcount(g) - 1),
    unname(brainGraph::efficiency(g, type = "nodal", use.parallel = FALSE))
  )
})

test_that("centiserve::communibet is cograph's communicability betweenness", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  # centiserve::communibet transcribed with Matrix::expm, which uses scaling
  # and squaring rather than the eigendecomposition cograph uses, so the two
  # routes to exp(A) are independent.
  g <- igraph::make_graph("Zachary")
  n <- igraph::vcount(g)
  adj <- as.matrix(igraph::as_adjacency_matrix(g, names = FALSE))
  ex <- function(m) as.matrix(Matrix::expm(Matrix::Matrix(m)))
  exp_adj <- ex(adj)
  ref <- vapply(seq_len(n), function(v) {
    reduced <- adj
    reduced[v, ] <- 0
    reduced[, v] <- 0
    b <- (exp_adj - ex(reduced)) / exp_adj
    b[v, ] <- 0
    b[, v] <- 0
    diag(b) <- 0
    sum(b)
  }, numeric(1)) / ((n - 1)^2 - (n - 1))
  expect_equal(
    centrality(g, measures = "communicability_betweenness")$communicability_betweenness,
    ref, tolerance = 1e-8)
})
