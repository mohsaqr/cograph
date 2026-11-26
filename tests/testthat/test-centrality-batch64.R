# Batch 64 --- eight measures read from their papers on 2026-11-26:
#   wd, ewd   Yang et al. (2018), Physics Letters A 382(34):2361-2371,
#             eqs. (1)-(2)
#   mlc       Ma and Ma (2017), Physica A 465:312-330, section 3
#   spreading_probability  Bao, Ma, Xiang and Zhang (2017), Physica A
#             468:391-397, eq. (5)
#   profitleader  Yu, Shao, Yang and Sun (2019), World Wide Web
#             22(2):533-553, eqs. (1)-(4) with the + 1 of the authors'
#             released code (changed in batch 67)
#   hks       Zareie and Sheikhahmadi (2018), Expert Systems with
#             Applications 93:200-211, eqs. (10)-(11)
#   niccm     Hu, Fan and Mei (2015), Physica A 429:169-176, eqs. (1)-(4)
#   correlation_centrality  Fan, Liu and Hu (2013), Physica Scripta
#             88(6):065201, eq. (4)
# Published values are compared in
# tests/equivalence/test-centrality-zoo-b-batch64.R.

b64_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b64_path <- function(n) b64_from_edges(n, cbind(seq_len(n - 1L), 2:n))

b64_complete <- function(n) {
  a <- matrix(1, n, n, dimnames = list(seq_len(n), seq_len(n)))
  diag(a) <- 0
  a
}

b64_kite <- function() {
  b64_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

b64_measures <- c("wd", "ewd", "mlc", "spreading_probability",
                  "profitleader", "hks", "niccm", "correlation_centrality")

# --- wd, ewd ---------------------------------------------------------------

test_that("wd and ewd follow the removal rounds on a path of four", {
  # Round 1 removes the two ends, round 2 the two middle nodes.
  path <- b64_path(4L)
  expect_equal(unname(centrality_wd(path)), c(1, 1.5, 1.5, 1))
  expect_equal(unname(centrality_ewd(path)), c(1.5, 2.5, 2.5, 1.5))
})

test_that("ewd is the neighbor sum of wd, the node itself included", {
  kite <- b64_kite()
  expect_equal(unname(centrality_ewd(kite)),
               as.numeric(kite %*% centrality_wd(kite)))
  expect_equal(unname(centrality_wd(kite)),
               as.numeric(kite %*% centrality_iksd(kite)) /
                 max(centrality_iksd(kite)))
})

# --- mlc -------------------------------------------------------------------

test_that("mlc is hand-computed on small graphs and can be negative", {
  expect_equal(unname(centrality_mlc(b64_path(4L))), c(1, 2, 2, 1))
  expect_equal(unname(centrality_mlc(b64_path(2L))), c(-1, -1))
  expect_equal(unname(centrality_mlc(b64_path(3L))), c(0, 0, 0))
})

test_that("mlc is semilocal less twice the neighbors' degree sum", {
  kite <- b64_kite()
  expect_equal(unname(centrality_mlc(kite)),
               unname(centrality_semilocal(kite)) -
                 2 * as.numeric(kite %*% rowSums(kite)))
})

# --- spreading_probability -------------------------------------------------

test_that("spreading_probability is hand-computed on a path of four", {
  # Mean degree 3 / 2, so one step costs 2 / 3.
  expect_equal(unname(centrality_spreading_probability(b64_path(4L))),
               c(38 / 27, 16 / 9, 16 / 9, 38 / 27))
  # Two shortest paths lead to the opposite corner of a square.
  square <- b64_from_edges(4L, rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 1)))
  expect_equal(unname(centrality_spreading_probability(square)),
               rep(2 / 2 + 2 / 4, 4))
})

test_that("spreading_probability at radius one is degree over mean degree", {
  kite <- b64_kite()
  expect_equal(unname(centrality_spreading_probability(kite, sp_radius = 1)),
               unname(rowSums(kite) / mean(rowSums(kite))))
  near <- centrality_spreading_probability(kite, sp_radius = 2)
  far <- centrality_spreading_probability(kite, sp_radius = Inf)
  expect_true(all(far >= near))
  expect_error(centrality_spreading_probability(kite, sp_radius = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_spreading_probability(kite, sp_radius = 1.5),
               class = "cograph_bad_parameter")
})

# --- profitleader ----------------------------------------------------------

test_that("profitleader is hand-computed on a path and a triangle", {
  # The available resource is d_u + 1 plus the degrees of u's neighbors
  # outside N[v] (the authors' code). Path 1-2-3-4: node 1 sends
  # (1 + 1) * |{1,2}| / |{1,2,3}| = 4/3; node 2 sends (2 + 1 + d_3) * 2/3 =
  # 10/3 to node 1 and (2 + 1 + d_1) * |{2,3}| / |{1,2,3,4}| = 2 to node 3.
  expect_equal(unname(centrality_profitleader(b64_path(4L))),
               c(4 / 3, 16 / 3, 16 / 3, 4 / 3))
  # Triangle: each of two neighbors gets (2 + 1) * 3 / 3.
  expect_equal(unname(centrality_profitleader(b64_complete(3L))), rep(6, 3))
})

test_that("profitleader is the article's eq. (2) plus the summed sharing probabilities", {
  # The +1 adds SP(u, v) = |N[u] n N[v]| / |N[u] u N[v]| once per neighbor.
  # Every node of the kite, written out with explicit neighbor sets.
  kite <- b64_kite()
  neighbors <- \(u) which(kite[u, ] > 0)
  degree <- rowSums(kite)
  by_sets <- vapply(seq_len(nrow(kite)), \(u) {
    terms <- vapply(neighbors(u), \(v) {
      closed_u <- c(u, neighbors(u))
      closed_v <- c(v, neighbors(v))
      share <- length(intersect(closed_u, closed_v)) /
        length(union(closed_u, closed_v))
      outside <- setdiff(neighbors(u), closed_v)
      c(article = (degree[[u]] + sum(degree[outside])) * share, share = share)
    }, numeric(2))
    sum(terms["article", ]) + sum(terms["share", ])
  }, numeric(1))
  expect_equal(unname(centrality_profitleader(kite)), by_sets)
})

# --- hks -------------------------------------------------------------------

test_that("hks is hand-computed on a path of four", {
  # b = (1, 2, 2, 1), f = (1, 2, 2, 1), k (b + f) = (2, 8, 8, 2).
  expect_equal(unname(centrality_hks(b64_path(4L))), c(10, 18, 18, 10))
})

test_that("hks scores each component on its own", {
  two <- b64_from_edges(7L, rbind(c(1, 2), c(2, 3), c(3, 4),
                                  c(5, 6), c(6, 7), c(5, 7)))
  expect_equal(unname(centrality_hks(two))[1:4], c(10, 18, 18, 10))
})

# --- niccm -----------------------------------------------------------------

test_that("niccm is hand-computed on a triangle", {
  # Efficiency 2 / 3 each; a deletion costs each of the two others 1 / 3.
  expect_equal(unname(centrality_niccm(b64_complete(3L))), rep(4 / 9, 3))
})

test_that("niccm depends on the number of layers and rejects a bad one", {
  kite <- b64_kite()
  two <- centrality_niccm(kite)
  expect_equal(two, centrality_niccm(kite, niccm_layers = 2))
  expect_false(isTRUE(all.equal(two, centrality_niccm(kite, niccm_layers = 1))))
  expect_true(all(is.finite(centrality_niccm(kite, niccm_layers = Inf))))
  expect_error(centrality_niccm(kite, niccm_layers = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_niccm(kite, niccm_layers = 2.5),
               class = "cograph_bad_parameter")
})

# --- correlation_centrality ------------------------------------------------

test_that("correlation_centrality is hand-computed", {
  expect_equal(unname(centrality_correlation_centrality(b64_complete(3L))),
               rep(4 / 9, 3))
  # Path of four: efficiencies 11/24 at the ends and 5/8 in the middle.
  expect_equal(
    unname(centrality_correlation_centrality(b64_path(4L)))[1:2],
    c((5 / 8 + 5 / 8 / 8 + 11 / 24 / 27) / 4,
      (11 / 24 + 5 / 8 + 11 / 24 / 8) / 4))
})

test_that("correlation_centrality with exponent zero drops the distance", {
  path <- b64_path(4L)
  efficiency <- c(11 / 24, 5 / 8, 5 / 8, 11 / 24)
  expect_equal(unname(centrality_correlation_centrality(path, coc_alpha = 0)),
               (sum(efficiency) - efficiency) / 4)
  expect_error(centrality_correlation_centrality(path, coc_alpha = -1),
               class = "cograph_bad_parameter")
  expect_error(centrality_correlation_centrality(path, coc_alpha = Inf),
               class = "cograph_bad_parameter")
})

# --- shared ----------------------------------------------------------------

test_that("the eight measures are equivariant, skeleton-based and registered", {
  set.seed(64)
  kite <- b64_kite()
  base <- centrality(kite, measures = b64_measures)
  perm <- sample.int(10L)
  moved <- centrality(kite[perm, perm], measures = b64_measures)
  arcs <- kite
  arcs[lower.tri(arcs)] <- 0
  one_way <- centrality(arcs, measures = b64_measures)
  heavy <- centrality(kite * 4, measures = b64_measures)
  lapply(b64_measures, \(m) {
    expect_equal(moved[[m]], base[[m]][perm])
    expect_equal(one_way[[m]], base[[m]])
    expect_equal(heavy[[m]], base[[m]])
  })
  listed <- list_centralities()
  rows <- listed[listed$measure %in% b64_measures, ]
  expect_identical(nrow(rows), 8L)
  expect_identical(rows$measure[rows$costly], "niccm")
  expect_false(any(rows$uses_weights))
})

test_that("isolated nodes score zero and an empty network is all zero", {
  lonely <- b64_from_edges(5L, rbind(c(1, 2), c(2, 3), c(3, 1)))
  scores <- centrality(lonely, measures = b64_measures)
  expect_equal(unlist(scores[4, -1], use.names = FALSE), rep(0, 8))
  expect_equal(unlist(scores[5, -1], use.names = FALSE), rep(0, 8))
  empty <- matrix(0, 3L, 3L, dimnames = list(1:3, 1:3))
  expect_equal(unlist(centrality(empty, measures = b64_measures)[1, -1],
                      use.names = FALSE), rep(0, 8))
})

test_that("a one-node network scores zero on all eight", {
  single <- matrix(0, 1L, 1L, dimnames = list("a", "a"))
  expect_equal(unlist(centrality(single, measures = b64_measures)[1, -1],
                      use.names = FALSE), rep(0, 8))
})

test_that("niccm is left out of the default set", {
  expect_false("niccm" %in% names(centrality(b64_kite())))
})
