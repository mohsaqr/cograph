# Batch 59 --- three measures over all simple cycles or paths:
#   all_cycle_betweenness  Zhou, Liang, Zhao and Zhang (2018), Scientific
#                          Reports 8:11749, eq. (10)
#   transportation         Piraveenan and Saripada (2023), IEEE Access
#                          11:142214-142234, eqs. (8)-(10)
#   path_transfer          Tutzauer (2007), as restated by Oggier,
#                          Phetsouvanh and Datta (2019), PeerJ CS 5:e220
# Published values are compared in
# tests/equivalence/test-centrality-zoo-b-batch59.R.

b59_from_edges <- function(n, el, directed = FALSE) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  if (!directed) a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b59_complete <- function(n) {
  a <- 1 - diag(n)
  dimnames(a) <- list(seq_len(n), seq_len(n))
  a
}

b59_ring <- function(n) b59_from_edges(n, cbind(seq_len(n), c(2:n, 1L)))

b59_path <- function(n) b59_from_edges(n, cbind(seq_len(n - 1L), 2:n))

# --- all cycle betweenness -------------------------------------------------

test_that("all cycle betweenness counts each cycle once", {
  # A ring of n has one cycle of n edges.
  expect_equal(unname(centrality_all_cycle_betweenness(b59_ring(6L))),
               rep(0.5^4, 6))
  # K4: each node is on 3 triangles and all 3 four-cycles.
  expect_equal(unname(centrality_all_cycle_betweenness(b59_complete(4L))),
               rep(3 * 0.5 + 3 * 0.25, 4))
  expect_equal(unname(centrality_all_cycle_betweenness(b59_complete(4L),
                                                       cycle_alpha = 0.1)),
               rep(3 * 0.1 + 3 * 0.01, 4))
})

test_that("nodes on no cycle score zero", {
  expect_equal(unname(centrality_all_cycle_betweenness(b59_path(5L))),
               rep(0, 5))
  # Triangle with a pendant.
  paw <- b59_from_edges(4L, rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4)))
  expect_equal(unname(centrality_all_cycle_betweenness(paw)),
               c(0.5, 0.5, 0.5, 0))
  expect_error(centrality_all_cycle_betweenness(paw, cycle_alpha = 1),
               class = "cograph_bad_parameter")
  expect_error(centrality_all_cycle_betweenness(paw, cycle_alpha = 0),
               class = "cograph_bad_parameter")
})

# --- transportation --------------------------------------------------------

test_that("transportation centrality follows the paper's closed forms", {
  # Piraveenan and Saripada's Fig. 1: 1-2, 1-3, 2-3, 3-4.
  paw <- b59_from_edges(4L, rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4)))
  lapply(c(0, 0.1, 2), \(beta) {
    e <- exp(-beta)
    expect_equal(unname(centrality_transportation(paw,
                                                  transport_beta = beta)),
                 c(2 * e / (3 * e + 3), 2 * e / (3 * e + 3),
                   (3 * e + 2) / (3 * e + 3), 0))
  })
})

test_that("transportation tends to betweenness as beta grows", {
  paw <- b59_from_edges(4L, rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4)))
  # Ordered pairs: twice the undirected betweenness (0, 0, 2, 0) over 6.
  expect_equal(unname(centrality_transportation(paw, transport_beta = 60)),
               c(0, 0, 2 / 3, 0), tolerance = 1e-12)
  # On a path there is one path per pair, so beta does not matter.
  path <- b59_path(5L)
  expect_equal(centrality_transportation(path, transport_beta = 0),
               centrality_transportation(path, transport_beta = 3))
  expect_equal(unname(centrality_transportation(path)),
               2 * c(0, 3, 4, 3, 0) / 12)
})

test_that("transportation uses direction and needs three nodes", {
  # 1 -> 2 -> 3: only the pair (1, 3) has a path, through node 2.
  line <- b59_from_edges(3L, cbind(1:2, 2:3), directed = TRUE)
  expect_equal(unname(centrality_transportation(line)), c(0, 1 / 2, 0))
  pair <- b59_from_edges(2L, cbind(1L, 2L))
  expect_warning(out <- centrality_transportation(pair),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  expect_error(centrality_transportation(line, transport_beta = -1),
               class = "cograph_bad_parameter")
})

# --- path transfer ---------------------------------------------------------

test_that("path-transfer centrality reproduces the worked example", {
  # Oggier et al., Fig. 1A: from v1 the flow stops at v1, v2 with 1/4 each,
  # at v3, v4 with 1/8 each and at v5 with 1/4: 3/4 * 2 + 2/8 * 3 = 2.25.
  flow <- b59_from_edges(5L, rbind(c(1, 2), c(1, 3), c(1, 4), c(3, 5),
                                   c(4, 5)), directed = TRUE)
  expect_equal(unname(centrality_path_transfer(flow)), c(2.25, 0, 1, 1, 0))
})

test_that("path-transfer on an undirected pair and triangle, by hand", {
  # Pair: stop at once (1/2) or move and stop (1/2): one bit.
  pair <- b59_from_edges(2L, cbind(1L, 2L))
  expect_equal(unname(centrality_path_transfer(pair)), c(1, 1))
  # Triangle: stop at the origin with 1/3; each neighbour with
  # 1/3 * 1/2 directly plus 1/3 * 1/2 through the other: 1/3 each.
  triangle <- b59_complete(3L)
  expect_equal(unname(centrality_path_transfer(triangle)), rep(log2(3), 3))
  kite_like <- b59_ring(5L)
  expect_true(all(centrality_path_transfer(kite_like) <= log2(5)))
})

# --- shared ----------------------------------------------------------------

test_that("the enumeration limit gives NA with a classed warning", {
  k7 <- b59_complete(7L)
  lapply(c("all_cycle_betweenness", "transportation", "path_transfer"),
         \(measure) {
    expect_warning(out <- centrality(k7, measures = measure,
                                     enumeration_limit = 50),
                   class = "cograph_undefined_measure")
    expect_true(all(is.na(out[[measure]])))
    expect_false(anyNA(centrality(k7, measures = measure,
                                  enumeration_limit = Inf)[[measure]]))
  })
  expect_error(centrality(k7, measures = "path_transfer",
                          enumeration_limit = 0),
               class = "cograph_bad_parameter")
})

test_that("the three measures are equivariant and registered as costly", {
  set.seed(59)
  a <- b59_from_edges(7L, rbind(c(1, 2), c(2, 3), c(3, 1), c(3, 4), c(4, 5),
                                c(5, 6), c(6, 4), c(6, 7), c(2, 5)))
  measures <- c("all_cycle_betweenness", "transportation", "path_transfer")
  base <- centrality(a, measures = measures)
  perm <- sample.int(7L)
  moved <- centrality(a[perm, perm], measures = measures)
  lapply(measures, \(m) expect_equal(moved[[m]], base[[m]][perm]))
  expect_equal(centrality(a * 3, measures = measures)[, -1], base[, -1])
  listed <- list_centralities()
  rows <- listed[listed$measure %in% measures, ]
  expect_identical(nrow(rows), 3L)
  expect_true(all(rows$costly))
})
