# Batch 69 --- four measures built on 2026-11-26 on the owner's decisions
# (`eigentrust` was removed the same day: it is `pagerank` with damping 1 - a
# and the pre-trust as `personalized`; see test-centrality-zoo-b-batch69.R):
#   rcfb      Agryzkov, Tortosa and Vicent (2019), Applied Mathematics and
#             Computation 347:600-615, eqs. (14)-(20), Algorithm 2; limit
#             epsilon -> 0 by default
#   m_centrality  Ibnoulouafi, El Haziti and Cherifi (2018), J. Stat. Mech.
#             073407, eqs. (6)-(11); equal weights where eq. (11) is 0/0
#   wksd      Namtirtha, Dutta and Dutta (2018, 2020) as restated by Shetty
#             and Bhattacharjee (2025); `wksd_alpha`, `wksd_mu` required
#   mapping_entropy_betweenness  Gialampoukidis et al. (2016), EISIC;
#             zero-betweenness neighbors skipped
# Comparisons with outside references are in
# tests/equivalence/test-centrality-zoo-b-batch69.R. Every expected value
# below is derived by hand in the comment above it.

b69_from_edges <- function(n, el, directed = FALSE) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  el <- matrix(el, ncol = 2L, byrow = !is.matrix(el))
  a[el] <- 1
  if (!directed) a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b69_path <- function(n) b69_from_edges(n, cbind(seq_len(n - 1L), 2:n))
b69_cycle <- function(n) b69_from_edges(n, cbind(seq_len(n), c(2:n, 1L)))
b69_star <- function(leaves) b69_from_edges(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))
# A triangle 1-2-3 with the pendant 3-4.
b69_paw <- function() b69_from_edges(4L, c(1, 2, 1, 3, 2, 3, 3, 4))
# Arcs 1->2, 2->3, 3->1, 3->2.
b69_digraph <- function() b69_from_edges(3L, c(1, 2, 2, 3, 3, 1, 3, 2), directed = TRUE)
b69_kite <- function() {
  b69_from_edges(10L, c(1, 2, 1, 3, 1, 4, 1, 6, 2, 4, 2, 5, 2, 7, 3, 4, 3, 6,
                        4, 5, 4, 6, 4, 7, 5, 7, 6, 7, 6, 8, 7, 8, 8, 9, 9, 10))
}

b69_measures <- c("rcfb", "m_centrality", "wksd",
                  "mapping_entropy_betweenness")
b69_all <- function(x, ...) {
  centrality(x, measures = b69_measures, wksd_alpha = 0.2, wksd_mu = 0.9, ...)
}

# --- rcfb -------------------------------------------------------------------

test_that("rcfb follows Algorithm 2 on a path of three in the limit", {
  # Source 1: 1 enters at node 1 and 1/3 leaves at every node, so edge 1-2
  # carries 2/3 (credited to 1) and 2-3 carries 1/3 (to 2). Source 2: 1/3
  # each way, both credited to 2. Source 3 mirrors source 1. Totals
  # (2/3, 4/3, 2/3), divided by n = 3.
  expect_equal(unname(centrality_rcfb(b69_path(3L))), c(2, 4, 2) / 9)
})

test_that("rcfb with epsilon follows (epsilon I + L)^-1 on one edge", {
  # One edge of weight w: (epsilon I + L)^-1 has M11 - M21 = 1 / (epsilon +
  # 2 w), the current w / (epsilon + 2 w) goes to the source, each node is
  # source once, n = 2. The bound of eq. (14) is 1 / (1 / w) = w.
  edge <- b69_path(2L)
  expect_equal(unname(centrality_rcfb(edge, rcfb_epsilon = 0.5)),
               rep(1 / (2 * 2.5), 2))
  expect_equal(unname(centrality_rcfb(edge)), rep(1 / 4, 2))
  heavy <- edge * 2
  expect_equal(unname(centrality_rcfb(heavy, rcfb_epsilon = 0.1)),
               rep(2 / 4.1 / 2, 2))
  expect_equal(unname(centrality_rcfb(heavy, rcfb_epsilon = 0.1, weighted = FALSE)),
               rep(1 / 2.1 / 2, 2))
  expect_error(centrality_rcfb(edge, rcfb_epsilon = 1),
               class = "cograph_bad_parameter")
  expect_error(centrality_rcfb(edge, rcfb_epsilon = -1),
               class = "cograph_bad_parameter")
  expect_error(centrality_rcfb(edge, rcfb_epsilon = c(0, 1)),
               class = "cograph_bad_parameter")
})

test_that("rcfb tends to the limit as epsilon falls", {
  kite <- b69_kite()
  limit <- centrality_rcfb(kite)
  gaps <- vapply(c(1e-2, 1e-4, 1e-6), \(e) max(abs(centrality_rcfb(kite, rcfb_epsilon = e) - limit)),
                 numeric(1))
  expect_true(all(diff(gaps) < 0))
  expect_lt(gaps[3], 1e-5)
})

test_that("the rcfb kernel reproduces the audit's kite values at epsilon = 1", {
  # candidate_review54/rcfb_check.R prints these six-decimal values for
  # M = (I + L)^-1 with the printed loop of Algorithm 2. epsilon = 1 is above
  # the bound 1/18 of eq. (14), so only the kernel can be asked for it.
  audit <- c(0.092971, 0.092971, 0.072229, 0.135031, 0.072229, 0.132188,
             0.132188, 0.122780, 0.086682, 0.038668)
  got <- cograph:::.cg_rcfb(unname(b69_kite()), 1)
  expect_true(all(abs(got - audit) < 5e-7))
  expect_error(centrality_rcfb(b69_kite(), rcfb_epsilon = 1),
               class = "cograph_bad_parameter")
})

test_that("rcfb keeps current inside a component and scores an isolate 0", {
  # An edge and an isolate, n = 3: source 1 sends 1/2 (half of its unit
  # leaves at node 2) along the edge, credited to 1; the isolate gets none.
  net <- b69_from_edges(3L, c(1, 2))
  expect_equal(unname(centrality_rcfb(net)), c(1, 1, 0) / 6)
  expect_equal(unname(centrality_rcfb(matrix(0, 1, 1))), 0)
  expect_equal(unname(centrality_rcfb(matrix(0, 3, 3))), c(0, 0, 0))
})

test_that("rcfb reads conductances on a cycle and needs a symmetric input", {
  # On a tree the currents are forced, so weights do not change the limit.
  tree <- b69_star(3L)
  heavy_tree <- tree
  heavy_tree[1, 2] <- heavy_tree[2, 1] <- 2
  heavy_tree[1, 3] <- heavy_tree[3, 1] <- 3
  expect_equal(centrality_rcfb(heavy_tree), centrality_rcfb(tree))
  # On a cycle they do.
  ring <- b69_cycle(4L)
  heavy <- ring
  heavy[1, 2] <- heavy[2, 1] <- 5
  expect_false(isTRUE(all.equal(centrality_rcfb(heavy), centrality_rcfb(ring))))
  expect_warning(score <- centrality_rcfb(b69_digraph()),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
  # Reciprocal arcs of equal weight are an undirected tie.
  both <- b69_from_edges(3L, c(1, 2, 2, 1, 2, 3, 3, 2), directed = TRUE)
  expect_equal(unname(centrality_rcfb(both)), c(2, 4, 2) / 9)
})

# --- m_centrality -----------------------------------------------------------

test_that("m_centrality follows equations (6)-(11) on a star", {
  # Star with three leaves: coreness 1 everywhere, so E1 = 1 and mu =
  # (1 - 1) / (2 - 1 - E2) = 0. DeltaD(centre) = 3 x (2 + 2 + 2) / 3 = 6,
  # DeltaD(leaf) = 1 x 2 / 3. M = DeltaD.
  expect_equal(unname(centrality_m_centrality(b69_star(3L))),
               c(6, 2 / 3, 2 / 3, 2 / 3))
})

test_that("m_centrality mixes coreness and degree variation on the paw", {
  # Degrees (2, 2, 3, 1), coreness (2, 2, 2, 1).
  # DeltaD1 = 2 x (0 + 1) / 5 = 2/5 (= DeltaD2), DeltaD3 = 3 x (1 + 1 + 2) / 5
  # = 12/5, DeltaD4 = 1 x 2 / 3.
  ks <- c(2, 2, 2, 1)
  dd <- c(2 / 5, 2 / 5, 12 / 5, 2 / 3)
  e <- function(x) { r <- x / sum(x); -sum(r * log(r)) / log(4) }
  mu <- (1 - e(ks)) / (2 - e(ks) - e(dd))
  expect_equal(unname(centrality_m_centrality(b69_paw())), mu * ks + (1 - mu) * dd)
})

test_that("m_centrality uses equal weights where equation (11) is 0/0", {
  # Cycle of five: DeltaD = 0 everywhere, coreness 2: M = 2 / 2 = 1.
  expect_equal(unname(centrality_m_centrality(b69_cycle(5L))), rep(1, 5))
  # A triangle beside an edge: every neighbor shares the node's degree, so
  # DeltaD = 0; coreness (2, 2, 2, 1, 1), M = coreness / 2.
  pair <- b69_from_edges(5L, c(1, 2, 1, 3, 2, 3, 4, 5))
  expect_equal(unname(centrality_m_centrality(pair)), c(1, 1, 1, 0.5, 0.5))
  # No edges and one node: 0.
  expect_equal(unname(centrality_m_centrality(matrix(0, 3, 3))), c(0, 0, 0))
  expect_equal(unname(centrality_m_centrality(matrix(0, 1, 1))), 0)
})

test_that("m_centrality scores an isolate 0 and ignores direction", {
  # Star plus an isolate: the isolate has DeltaD = 0 and coreness 0; with
  # five nodes the coreness shares are (1, 1, 1, 1, 0) / 4, E1 = ln 4 / ln 5.
  net <- b69_from_edges(5L, c(1, 2, 1, 3, 1, 4))
  expect_equal(centrality_m_centrality(net)[["5"]], 0)
  directed <- b69_from_edges(3L, c(1, 2, 2, 3), directed = TRUE)
  expect_equal(centrality_m_centrality(directed),
               centrality_m_centrality(b69_path(3L)))
})

# --- wksd -------------------------------------------------------------------

test_that("wksd follows the restated formula on a five-node graph", {
  # Edges 1-2, 2-3, 3-4, 2-4, 4-5: degrees (1, 3, 2, 3, 1), k-shell
  # (1, 2, 2, 2, 1). alpha = 0.2, mu = 0.9: a = 0.2 k + 0.9 ks =
  # (1.1, 2.4, 2.2, 2.4, 1.1); Wksd = a x (A a):
  # node 1 = 1.1 x 2.4 = 2.64, node 2 = 2.4 x (1.1 + 2.2 + 2.4) = 13.68,
  # node 3 = 2.2 x 4.8 = 10.56.
  toy <- b69_from_edges(5L, c(1, 2, 2, 3, 3, 4, 2, 4, 4, 5))
  expect_equal(unname(centrality_wksd(toy, wksd_alpha = 0.2, wksd_mu = 0.9)),
               c(2.64, 13.68, 10.56, 13.68, 2.64))
  # alpha = mu = 1: a = (2, 5, 4, 5, 2): node 2 = 5 x 11 = 55.
  expect_equal(unname(centrality_wksd(toy, wksd_alpha = 1, wksd_mu = 1)),
               c(10, 55, 40, 55, 10))
  # mu = 0: alpha^2 k_i sum of the neighbors' degrees.
  expect_equal(unname(centrality_wksd(toy, wksd_alpha = 0.5, wksd_mu = 0)),
               0.25 * c(1 * 3, 3 * 6, 2 * 6, 3 * 6, 1 * 3))
})

test_that("wksd needs both weights and checks their range", {
  toy <- b69_path(4L)
  expect_warning(score <- centrality_wksd(toy, wksd_alpha = 0.2),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
  expect_warning(centrality_wksd(toy, wksd_mu = 0.2),
                 class = "cograph_undefined_measure")
  for (v in list(1.5, -0.1, "a", c(0.1, 0.2))) {
    expect_error(centrality_wksd(toy, wksd_alpha = v, wksd_mu = 0.5),
                 class = "cograph_bad_parameter")
    expect_error(centrality_wksd(toy, wksd_alpha = 0.5, wksd_mu = v),
                 class = "cograph_bad_parameter")
  }
})

test_that("wksd scores an isolate 0", {
  net <- b69_from_edges(4L, c(1, 2, 2, 3))
  expect_equal(unname(centrality_wksd(net, wksd_alpha = 0.3, wksd_mu = 0.7)),
               # a = 0.3 k + 0.7 ks = (1, 1.3, 1, 0)
               c(1.3, 1.3 * 2, 1.3, 0))
})

# --- mapping_entropy_betweenness ---------------------------------------------

test_that("mapping_entropy_betweenness follows the formula on a path of five", {
  # Betweenness (0, 3, 4, 3, 0) over (N - 1)(N - 2) / 2 = 6 pairs:
  # BC = (0, 1/2, 2/3, 1/2, 0). Node 2: its neighbor 1 has BC 0 and is
  # skipped, -1/2 log(2/3). Node 3: -(2/3) x 2 log(1/2).
  expect_equal(unname(centrality_mapping_entropy_betweenness(b69_path(5L))),
               c(0, 0.5 * log(1.5), (4 / 3) * log(2), 0.5 * log(1.5), 0))
})

test_that("mapping_entropy_betweenness gives 0 where every log or BC is 0", {
  # Star: the centre has BC 1 (log 1 = 0), the leaves BC 0.
  expect_equal(unname(centrality_mapping_entropy_betweenness(b69_star(4L))),
               rep(0, 5))
  expect_equal(unname(centrality_mapping_entropy_betweenness(b69_path(2L))), c(0, 0))
  expect_equal(unname(centrality_mapping_entropy_betweenness(matrix(0, 1, 1))), 0)
  # Never negative, on the kite.
  expect_true(all(centrality_mapping_entropy_betweenness(b69_kite()) >= 0))
})

# --- shared properties -------------------------------------------------------

test_that("the batch 69 measures follow a relabelling of the nodes", {
  kite <- b69_kite()
  perm <- c(7, 3, 10, 1, 5, 9, 2, 8, 4, 6)
  out <- b69_all(kite)
  permuted <- b69_all(kite[perm, perm])
  for (m in b69_measures) {
    expect_equal(permuted[[m]], out[[m]][perm], info = m)
  }
})

test_that("the batch 69 measures handle a one-node network and no nodes", {
  one <- matrix(0, 1, 1, dimnames = list("a", "a"))
  out <- b69_all(one)
  expect_equal(out$rcfb, 0)
  expect_equal(out$m_centrality, 0)
  expect_equal(out$wksd, 0)
  expect_equal(out$mapping_entropy_betweenness, 0)
})

test_that("the structural measures ignore direction and weights", {
  kite <- b69_kite()
  weighted <- kite * (1 + (seq_len(100) %% 4))
  weighted <- pmax(weighted, t(weighted))
  out <- b69_all(kite)
  heavy <- b69_all(weighted)
  for (m in c("m_centrality", "wksd", "mapping_entropy_betweenness")) {
    expect_equal(heavy[[m]], out[[m]], info = m)
  }
  expect_false(isTRUE(all.equal(heavy$rcfb, out$rcfb)))
})
