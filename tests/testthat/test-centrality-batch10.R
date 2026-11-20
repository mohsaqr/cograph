# ===========================================================================
# Batch 10 — the node measures other centrality packages exposed and
# `centrality()` did not: local efficiency, s-core, distance-weighted
# fragmentation, the k-path census and the edge percolated component.
#
# Unit tests: invariants, conventions and contracts. The comparisons with
# hand-computed values, brute-force readings of the definitions and igraph
# are declared in tests/equivalence/test-centrality-zoo-a-*.R.
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
# Distance-weighted fragmentation
# ===========================================================================

test_that("fragmentation: a cut vertex scores higher, all within [0, 1]", {
  frag <- centrality_fragmentation(adj6)
  # Deleting A leaves B-C-D-E-F connected, so the score is lower.
  expect_lt(unname(frag["A"]), unname(frag["C"]))
  expect_true(all(frag >= 0 & frag <= 1))
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
  skip_on_cran()
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
  skip_on_cran()
  a <- centrality_epc(adj6, epc_runs = 2000, epc_seed = 5)
  b <- centrality_epc(adj6, epc_runs = 8000, epc_seed = 5)
  expect_equal(unname(a), unname(b), tolerance = 0.05)
  expect_true(all(a > 0 & a <= 1))
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
  skip_on_cran()
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
