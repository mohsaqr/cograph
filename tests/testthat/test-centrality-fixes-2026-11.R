# Regression tests for the six centrality corrections of cograph 2.7.19.
# Every expected value is computed by hand from the definition (shown in the
# comments), not taken from another implementation.

.labelled_matrix <- function(m) {
  dimnames(m) <- list(paste0("N", seq_len(nrow(m))), paste0("N", seq_len(nrow(m))))
  m
}

.undirected_from <- function(n, from, to) {
  m <- matrix(0, n, n)
  m[cbind(from, to)] <- 1
  .labelled_matrix(m + t(m))
}

.directed_from <- function(n, from, to) {
  m <- matrix(0, n, n)
  m[cbind(from, to)] <- 1
  .labelled_matrix(m)
}

# ---------------------------------------------------------------------------
# 1. Centroid: minimum over u != v (Wuchty and Stadler 2003)
# ---------------------------------------------------------------------------

test_that("centroid takes the minimum over the other nodes only", {
  # Star, hub N1 and four leaves. Hub against a leaf u: the hub and the three
  # other leaves are closer to the hub (4), only u is closer to u (1): f = 3.
  # Leaf against the hub: -3; leaf against another leaf: 0. So the hub scores
  # 3 (it was 0 while f(v, v) = 0 entered the minimum) and every leaf -3.
  star <- .undirected_from(5, rep(1, 4), 2:5)
  expect_equal(centrality_centroid(star),
               c(N1 = 3, N2 = -3, N3 = -3, N4 = -3, N5 = -3))
})

test_that("centroid can exceed 0 and is 0 on a one-node graph", {
  path <- .undirected_from(5, 1:4, 2:5)
  # Path N1-...-N5: the middle node N3 is closer to N1, N2, N3 than N2 is
  # (3 against 2), so f(N3, N2) = 1, and likewise against N4: score 1.
  expect_equal(unname(centrality_centroid(path)[3]), 1)
  expect_true(all(centrality_centroid(path)[-3] < 1))
  expect_equal(unname(centrality_centroid(.labelled_matrix(matrix(0, 1, 1)))), 0)
})

# ---------------------------------------------------------------------------
# 2. Flow betweenness: max flow lost when the node is removed (Freeman,
#    Borgatti and White 1991)
# ---------------------------------------------------------------------------

test_that("flow betweenness sums the maximum flow lost when the node is removed", {
  skip_if_not_installed("igraph")
  # Edges 1-2, 1-3, 1-4, 2-5, 3-5, 4-2 (unit capacities). Node N4 is needed
  # only for the pair (1, 2): maximum flow 3 (1-2, 1-4-2, 1-3-5-2), 2 without
  # N4; every other pair keeps its flow of 2. So N4 scores 1, although some
  # maximum flows from N1 to N5 run through it (1-4-2-5). Worked by hand for
  # every node: N1 6, N2 6, N3 4, N4 1, N5 4.
  m <- .undirected_from(5, c(1, 1, 1, 2, 3, 4), c(2, 3, 4, 5, 5, 2))
  expect_equal(centrality_flow_betweenness(m),
               c(N1 = 6, N2 = 6, N3 = 4, N4 = 1, N5 = 4))
})

test_that("flow betweenness on a directed cycle counts ordered pairs", {
  skip_if_not_installed("igraph")
  # 1 -> 2 -> 3 -> 1. For N1: the pair (2, 3) keeps its flow 1 without N1;
  # the pair (3, 2) runs only through N1 (3 -> 1 -> 2) and loses 1.
  cycle <- .directed_from(3, 1:3, c(2, 3, 1))
  expect_equal(centrality_flow_betweenness(cycle), c(N1 = 1, N2 = 1, N3 = 1))
})

test_that("flow betweenness equals sna::flowbet()", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("sna")
  set.seed(20261122)
  m <- .labelled_matrix(matrix(stats::rbinom(100, 1, 0.35), 10, 10))
  diag(m) <- 0
  u <- .labelled_matrix(((m + t(m)) > 0) * 1)
  expect_equal(unname(centrality_flow_betweenness(u)),
               sna::flowbet(u, gmode = "graph"))
  expect_equal(unname(centrality_flow_betweenness(m)),
               sna::flowbet(m, gmode = "digraph"))
})

# ---------------------------------------------------------------------------
# 3. Second-order centrality (Kermarrec et al. 2011)
# ---------------------------------------------------------------------------

test_that("second-order centrality is the SD of return times on the balanced walk", {
  # K4 needs no loops. A walk reaches a given other node in 3 steps on
  # average and returns in n = 4: sum_i m_ij = 4 + 3 * 3 = 13, and
  # sqrt(2 * 13 - 4 * 5) = sqrt(6).
  k4 <- .labelled_matrix(matrix(1, 4, 4) - diag(4))
  expect_equal(centrality(k4, measures = "second_order")$second_order,
               rep(sqrt(6), 4))
  # Star with hub N1 and three leaves; each leaf gets a loop of weight 2. Hub:
  # m_leaf,hub = 3, return 4: sqrt(2 * 13 - 20) = sqrt(6). Leaf j: return 4,
  # from the hub 9, from another leaf 12: sum 37, sqrt(74 - 20) = sqrt(54).
  star <- .undirected_from(4, rep(1, 3), 2:4)
  expect_equal(centrality(star, measures = "second_order")$second_order,
               c(sqrt(6), rep(sqrt(54), 3)))
})

test_that("second-order centrality is undefined on directed input", {
  cycle <- .directed_from(3, 1:3, c(2, 3, 1))
  expect_warning(res <- centrality(cycle, measures = "second_order"),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(res$second_order)))
})

# ---------------------------------------------------------------------------
# 4. Centralization: igraph's theoretical maxima
# ---------------------------------------------------------------------------

test_that("degree centralization of a reciprocated star is 1", {
  # n = 6: in + out degree hub 10, leaves 2; summed gap 5 * 8 = 40 over
  # 2 (n - 1)(n - 2) = 40. (Over (n - 1)^2 = 25 it was 1.6.)
  star <- .directed_from(6, c(rep(1, 5), 2:6), c(2:6, rep(1, 5)))
  expect_equal(centralization(star, "degree", directed = TRUE), 1)
  # One direction alone: out-degree hub 5, leaves 1: 5 * 4 / (n - 1)^2.
  expect_equal(centralization(star, "degree", directed = TRUE, mode = "out"),
               20 / 25)
  # Out-star: out-degree hub 5, leaves 0: 25 / 25.
  out_star <- .directed_from(6, rep(1, 5), 2:6)
  expect_equal(centralization(out_star, "degree", mode = "out"), 1)
})

test_that("directed closeness centralization in one direction divides by (n - 1)^2 / n", {
  # Reciprocated star, n = 5, mode out: hub closeness 4 / 4 = 1, a leaf 4 / 7
  # (1 to the hub, 2 to three leaves). Gap 4 * 3 / 7 over 16 / 5 = 15 / 28.
  star <- .directed_from(5, c(rep(1, 4), 2:5), c(2:5, rep(1, 4)))
  expect_equal(centralization(star, "closeness", directed = TRUE, mode = "out"),
               15 / 28)
})

test_that("centralization equals igraph's centr_*() for every mode", {
  skip_if_not_installed("igraph")
  set.seed(20261123)
  m <- .labelled_matrix(matrix(stats::rbinom(144, 1, 0.3), 12, 12))
  diag(m) <- 0
  m[cbind(1:12, c(2:12, 1))] <- 1 # a directed cycle: strongly connected
  g <- igraph::graph_from_adjacency_matrix(m, mode = "directed")
  modes <- c("all", "in", "out")
  expect_equal(
    vapply(modes, \(md) centralization(m, "degree", mode = md), numeric(1L)),
    vapply(modes, \(md) igraph::centr_degree(g, mode = md, loops = FALSE)$centralization,
           numeric(1L)))
  expect_equal(
    vapply(modes, \(md) centralization(m, "closeness", mode = md), numeric(1L)),
    vapply(modes, \(md) igraph::centr_clo(g, mode = md)$centralization, numeric(1L)))
})

test_that("degree centralization never exceeds 1 on unweighted digraphs", {
  set.seed(20261124)
  values <- vapply(seq_len(30), function(i) {
    m <- .labelled_matrix(matrix(stats::rbinom(64, 1, stats::runif(1)), 8, 8))
    diag(m) <- 0
    vapply(c("all", "in", "out"), \(md) centralization(m, "degree", directed = TRUE, mode = md),
           numeric(1L))
  }, numeric(3L))
  expect_true(all(values <= 1 + 1e-12))
  expect_true(all(values >= 0))
})

# ---------------------------------------------------------------------------
# 5. MNC on directed networks: largest strongly connected component
# ---------------------------------------------------------------------------

test_that("MNC takes the largest strongly connected component on a digraph", {
  # 1 -> 2, 1 -> 3, 2 -> 3. N1's out-neighbours {N2, N3} carry only 2 -> 3:
  # two strong components of size 1 (one weak component of size 2).
  m <- .directed_from(3, c(1, 1, 2), c(2, 3, 3))
  expect_equal(centrality_mnc(m, mode = "out"), c(N1 = 1L, N2 = 1L, N3 = 0L))
  # With 3 -> 2 added, N2 and N3 form one strong component.
  m[3, 2] <- 1
  expect_equal(centrality_mnc(m, mode = "out"), c(N1 = 2L, N2 = 1L, N3 = 1L))
})

# ---------------------------------------------------------------------------
# 6. Load on a one-node graph (sna::loadcent())
# ---------------------------------------------------------------------------

test_that("load of the single node of a one-node graph is 1", {
  expect_equal(centrality_load(.labelled_matrix(matrix(0, 1, 1))), c(N1 = 1))
})
