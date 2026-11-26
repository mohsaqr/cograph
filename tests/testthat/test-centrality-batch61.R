# Batch 61 --- long-range interaction centrality with the maximal path
# (Aleskerov, Meshcheryakova and Shvydun; working paper arXiv:1610.05892,
# Definitions 1-2, eqs. (1), (2), (5), aggregation p. 27). The paper's
# Table 15 is compared in tests/equivalence/test-centrality-zoo-b-lric.R.

lric_from_arcs <- function(n, arcs) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[arcs[, 1:2, drop = FALSE]] <- arcs[, 3]
  a
}

# Numerical Example 2 of the working paper, Fig. 4: 10 nodes, 27 arcs.
lric_example <- function() {
  lric_from_arcs(10L, matrix(c(
    6, 1, 125, 6, 2, 30, 2, 1, 200, 4, 1, 375, 3, 1, 300, 4, 6, 490,
    7, 6, 510, 1, 8, 250, 1, 9, 490, 1, 10, 490, 1, 7, 300, 2, 7, 510,
    7, 8, 510, 8, 2, 485, 5, 2, 485, 4, 8, 240, 7, 9, 510, 7, 4, 250,
    9, 4, 250, 5, 4, 250, 10, 4, 250, 4, 3, 200, 5, 3, 300, 9, 3, 200,
    10, 3, 300, 3, 7, 190, 7, 10, 510), ncol = 3L, byrow = TRUE))
}

test_that("direct influence follows Definitions 1-2 on the paper's example", {
  direct <- cograph:::.cg_lric_direct(lric_example())
  # Page 18: c_31 = 300 / 500. Node 1 receives 1000 in all, quota 500; the
  # smallest critical group with node 3 pivotal is {3, 2}.
  expect_equal(direct[3, 1], 0.6)
  # Node 7 receives 300 + 510 + 190 = 1000: node 2 alone meets the quota.
  expect_equal(direct[2, 7], 1)
  # Node 5 has no in-neighbour: nobody influences it.
  expect_equal(direct[, 5], rep(0, 10))
  expect_true(all(direct >= 0 & direct <= 1))
})

test_that("a single source has full influence; a chain multiplies", {
  # 1 -> 2 -> 3 with any weights: each arc is the only one entering its
  # target, so c = 1 and c*_13 = 1. u = (w12, w23, 0) / total.
  chain <- lric_from_arcs(3L, rbind(c(1, 2, 2), c(2, 3, 6)))
  expect_equal(unname(centrality_lric_max(chain)), c(6 / 8, 0, 0))
  # Limited to one tie, node 1 reaches only node 2 (u_2 = 6/8 still).
  expect_equal(unname(centrality_lric_max(chain, lric_path_length = 1)),
               c(6 / 8, 0, 0))
  longer <- lric_from_arcs(4L, rbind(c(1, 2, 1), c(2, 3, 1), c(3, 4, 1)))
  expect_equal(unname(centrality_lric_max(longer)), c(2 / 3, 1 / 3, 0, 0))
  expect_equal(unname(centrality_lric_max(longer, lric_path_length = 1)),
               c(1 / 3, 1 / 3, 0, 0))
})

test_that("on unit weights the direct influence is 1 / ceiling(d / 2)", {
  # Quota 0.5 of d unit weights: a pivotal member needs ceiling(d / 2)
  # members in all.
  k6 <- 1 - diag(6L)
  expect_equal(cograph:::.cg_lric_direct(k6)[1, 2], 1 / 3)
  star_in <- lric_from_arcs(5L, cbind(2:5, 1L, 1))
  expect_equal(cograph:::.cg_lric_direct(star_in)[2:5, 1], rep(1 / 2, 4))
})

test_that("the quota decides who is pivotal", {
  # Node 3 receives 6 from node 1 and 4 from node 2.
  a <- lric_from_arcs(3L, rbind(c(1, 3, 6), c(2, 3, 4)))
  # Quota 0.5 (need 5): node 1 alone suffices (c = 1); node 2 is pivotal
  # only together with... {2, 1} has node 1 already at 6 >= 5, so node 2
  # is pivotal in no group.
  half <- cograph:::.cg_lric_direct(a, quota = 0.5)
  expect_equal(half[c(1, 2), 3], c(1, 0))
  # Quota 0.7 (need 7): both are needed, each pivotal in {1, 2}.
  high <- cograph:::.cg_lric_direct(a, quota = 0.7)
  expect_equal(high[c(1, 2), 3], c(6 / 10, 4 / 10))
  # A group that meets the quota exactly is critical: 5 + 5 at quota 0.5.
  even <- lric_from_arcs(3L, rbind(c(1, 3, 5), c(2, 3, 5)))
  expect_equal(cograph:::.cg_lric_direct(even)[c(1, 2), 3], c(1, 1))
})

test_that("lric_max scales with nothing: weights times a constant change no score", {
  a <- lric_example()
  expect_equal(centrality_lric_max(a * 0.01), centrality_lric_max(a))
})

test_that("lric_max is permutation equivariant", {
  set.seed(61)
  a <- lric_example()
  perm <- sample.int(10L)
  expect_equal(unname(centrality_lric_max(a[perm, perm])),
               unname(centrality_lric_max(a))[perm])
})

test_that("degenerate inputs and bad parameters", {
  empty <- matrix(0, 3L, 3L, dimnames = list(1:3, 1:3))
  expect_identical(unname(centrality_lric_max(empty)), c(0, 0, 0))
  a <- lric_example()
  expect_error(centrality_lric_max(a, lric_quota = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_lric_max(a, lric_quota = 1.2),
               class = "cograph_bad_parameter")
  expect_error(centrality_lric_max(a, lric_path_length = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_lric_max(a, lric_path_length = 2.5),
               class = "cograph_bad_parameter")
})

test_that("the subset-sum limit gives NA with a classed warning", {
  # Twelve in-neighbours with distinct powers of two: 2^11 distinct sums.
  hub <- lric_from_arcs(13L, cbind(2:13, 1L, 2^(0:11)))
  expect_warning(out <- centrality_lric_max(hub, enumeration_limit = 100),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  expect_false(anyNA(centrality_lric_max(hub)))
  row <- list_centralities()
  row <- row[row$measure == "lric_max", ]
  expect_true(row$costly)
  expect_true(row$uses_weights)
})
