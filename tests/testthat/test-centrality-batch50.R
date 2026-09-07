# Batch 50 --- degree and importance of lines (Liu, Xiong, Shi, Shi and
# Wang 2016, Physica A 452:209-219).
#
# The closed forms asserted here were derived by hand from equations
# (1)-(3), pages 210-211; three of the fixtures are printed in the paper
# and are named where they are used.

dil_star <- function(leaves) {
  a <- matrix(0, leaves + 1L, leaves + 1L)
  a[1L, -1L] <- 1
  a[-1L, 1L] <- 1
  a
}

dil_ring <- function(n) {
  a <- matrix(0, n, n)
  a[cbind(seq_len(n), c(seq_len(n - 1L) + 1L, 1L))] <- 1
  a[cbind(c(seq_len(n - 1L) + 1L, 1L), seq_len(n))] <- 1
  a
}

dil_path <- function(n) {
  a <- matrix(0, n, n)
  a[cbind(seq_len(n - 1L), seq_len(n - 1L) + 1L)] <- 1
  a[cbind(seq_len(n - 1L) + 1L, seq_len(n - 1L))] <- 1
  a
}

dil_complete <- function(n) {
  a <- matrix(1, n, n)
  diag(a) <- 0
  a
}

dil_from_edges <- function(n, el) {
  a <- matrix(0, n, n)
  a[el] <- 1
  a[el[, 2:1]] <- 1
  a
}

# A triangle whose two degree-three corners each carry a pendant. This is
# the smallest graph on which lambda actually matters: p = 1 on the line
# between the two corners and both k - p - 1 factors survive.
dil_lambda_trap <- function() {
  dil_from_edges(5L, rbind(c(1, 2), c(1, 3), c(2, 3), c(1, 4), c(2, 5)))
}

dil_kite <- function() {
  el <- rbind(c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7),
              c(3, 4), c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7),
              c(6, 8), c(7, 8), c(8, 9), c(9, 10))
  dil_from_edges(10L, el)
}

# A triangle with one pendant: an added component that carries an edge, a
# triangle and a degree-one node.
dil_paw <- function() {
  dil_from_edges(4L, rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4)))
}

dil_petersen <- function() {
  dil_from_edges(10L, rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 1),
                            c(6, 8), c(8, 10), c(10, 7), c(7, 9), c(9, 6),
                            c(1, 6), c(2, 7), c(3, 8), c(4, 9), c(5, 10)))
}

# ===========================================================================
# The published fixtures
# ===========================================================================

test_that("the paper's Fig. 1 edge importances are reproduced", {
  # Page 210. Fig. 1(a) is a tree: p = 0, U = 3 * 3 = 9, lambda = 1 and the
  # paper prints I_e45 = 9. Fig. 1(b) adds the line e35, so v3 becomes a
  # common neighbour of v4 and v5: p = 1, U = 2 * 2 = 4, lambda = 1.5 and
  # the paper prints I_e45 = 8/3.
  fig1a <- dil_from_edges(8L, rbind(c(1, 4), c(2, 4), c(3, 4), c(4, 5),
                                    c(5, 6), c(5, 7), c(5, 8)))
  fig1b <- dil_from_edges(7L, rbind(c(1, 4), c(2, 4), c(3, 4), c(3, 5),
                                    c(4, 5), c(5, 6), c(5, 7)))
  expect_equal(attr(cograph:::.cg_dil_terms(fig1a), "importance")[4, 5], 9)
  expect_equal(attr(cograph:::.cg_dil_terms(fig1b), "importance")[4, 5], 8 / 3)
})

test_that("lambda is p/2 + 1 and not 2p + 1", {
  # The stacked fraction extracts from the PDF's text layer as 2p + 1. On
  # the trap graph p = 1, U = (3-1-1)(3-1-1) = 1 and the two corners split
  # I evenly, so each scores 3 + (1/1.5)/2 = 10/3. The wrong reading gives
  # I = 1/3 and a score of 19/6.
  hubs <- centrality_dil(dil_lambda_trap())[1:2]
  expect_equal(unname(hubs), c(10 / 3, 10 / 3))
  expect_false(isTRUE(all.equal(unname(hubs[1]), 19 / 6)))
})

test_that("the paper's Fig. 2 node scores are reproduced", {
  # Page 211 prints L_v2 = 26/9 and L_v5 = 52/15 on this 27-node tree, and
  # the four edge importances I_e12 = 8, I_e227 = 0, I_e45 = 2, I_e56 = 4
  # that pin the degrees the reading depends on.
  el <- rbind(c(1, 2), c(1, 3), c(1, 17), c(1, 18), c(1, 19), c(1, 20),
              c(1, 21), c(1, 22), c(1, 23), c(2, 27), c(3, 4), c(3, 24),
              c(3, 26), c(4, 5), c(4, 25), c(5, 6), c(6, 7), c(6, 8),
              c(6, 9), c(6, 10), c(8, 11), c(8, 12), c(8, 13), c(9, 14),
              c(10, 15), c(10, 16))
  fig2 <- dil_from_edges(27L, el)
  terms <- cograph:::.cg_dil_terms(fig2)
  importance <- attr(terms, "importance")
  expect_equal(importance[1, 2], 8)
  expect_equal(importance[2, 27], 0)
  expect_equal(importance[4, 5], 2)
  expect_equal(importance[5, 6], 4)
  scores <- centrality_dil(fig2)
  expect_equal(unname(scores[2]), 26 / 9)
  expect_equal(unname(scores[5]), 52 / 15)
  # The point the paper makes with the fixture: the bridge node outranks
  # the hub's neighbour even though the two have equal degree.
  expect_gt(scores[5], scores[2])
})

test_that("all 21 printed values of the ARPA table are reproduced", {
  # Table 3 and Fig. 6, page 217. The edge list is read off Fig. 6 and is
  # corroborated by Table 3's own degree column, checked here first. The
  # prose on page 216 says 23 lines where the table and the figure both say
  # 26; the table and the figure are followed.
  el <- rbind(c(1, 2), c(1, 15), c(2, 3), c(2, 15), c(2, 16), c(3, 4),
              c(3, 17), c(3, 18), c(4, 5), c(5, 6), c(6, 7), c(6, 21),
              c(7, 8), c(8, 9), c(9, 10), c(10, 11), c(11, 12), c(12, 13),
              c(12, 19), c(13, 14), c(14, 15), c(14, 16), c(14, 17),
              c(18, 19), c(19, 20), c(20, 21))
  arpa <- dil_from_edges(21L, el)
  expect_equal(nrow(el), 26L)
  terms <- cograph:::.cg_dil_terms(arpa)
  printed_degree <- c(2, 4, 4, 2, 2, 3, 2, 2, 2, 2, 2, 3, 2, 4, 3, 2, 2, 2,
                      3, 2, 2)
  expect_equal(terms$degree, printed_degree)
  # Table 3's DIL column, in the descending order the paper prints it.
  nodes <- c(3, 14, 2, 12, 19, 6, 15, 16, 17, 13, 18, 4, 5, 7, 11, 20, 21,
             8, 9, 10, 1)
  printed <- c(15.25, 14.35, 11.55, 7.6667, 7.6667, 7, 5.9333, 3.5, 3.5,
               3.4167, 3.4167, 3.25, 3.1667, 3.1667, 3.1667, 3.1667, 3.1667,
               3, 3, 3, 2)
  computed <- as.numeric(centrality_dil(arpa))[nodes]
  # The paper prints four decimals, so the comparison is against its own
  # rounding rather than against a widened tolerance.
  expect_equal(floor(computed * 10000 + 0.5) / 10000, printed)
  # The exact rationals behind the four repeating values.
  expect_equal(computed[4], 23 / 3)
  expect_equal(computed[7], 89 / 15)
  expect_equal(computed[10], 41 / 12)
  expect_equal(computed[13], 19 / 6)
  # And the printed row order is the descending order of the scores.
  expect_false(is.unsorted(rev(computed)))
})

# ===========================================================================
# Hand-derived analytic families
# ===========================================================================

test_that("a complete graph scores its degree at every node", {
  # Every line of K_n sits on p = n - 2 triangles, so
  # U = (n - 1 - (n - 2) - 1)^2 = 0 and no line carries any importance.
  for (n in 2:9) {
    expect_equal(unname(centrality_dil(dil_complete(n))), rep(n - 1, n))
  }
})

test_that("a star scores its degree at every node", {
  # Every line of a star touches a leaf, whose k - p - 1 factor is zero.
  for (leaves in 1:8) {
    expect_equal(unname(centrality_dil(dil_star(leaves))),
                 c(leaves, rep(1, leaves)))
  }
})

test_that("a triangle-free k-regular graph scores k + k(k-1)^2/2", {
  # p = 0, so I = (k-1)^2 on every line and the two equal-degree endpoints
  # split it evenly.
  for (n in 4:12) {
    expect_equal(unname(centrality_dil(dil_ring(n))), rep(3, n))
  }
  expect_equal(unname(centrality_dil(dil_petersen())), rep(9, 10))
})

test_that("C3 follows the complete-graph form, not the ring form", {
  # C3 is K3: it is not triangle-free, so the 2-regular ring value 3 does
  # not apply and every node scores its degree.
  expect_equal(unname(centrality_dil(dil_ring(3))), rep(2, 3))
})

test_that("a path scores 1, 2.5, 3, ..., 3, 2.5, 1", {
  # A line to an end has U = 0; a line between two degree-two nodes has
  # I = 1, split evenly.
  expect_equal(unname(centrality_dil(dil_path(2))), c(1, 1))
  expect_equal(unname(centrality_dil(dil_path(3))), c(1, 2, 1))
  expect_equal(unname(centrality_dil(dil_path(4))), c(1, 2.5, 2.5, 1))
  expect_equal(unname(centrality_dil(dil_path(5))), c(1, 2.5, 3, 2.5, 1))
  expect_equal(unname(centrality_dil(dil_path(7))),
               c(1, 2.5, 3, 3, 3, 2.5, 1))
})

test_that("a complete bipartite graph matches its closed form", {
  # K_{a,b} is triangle-free, so I = (a-1)(b-1) on every line and a node of
  # the a-side scores b + b (a-1)(b-1)^2 / (a + b - 2).
  for (sides in list(c(2, 3), c(3, 3), c(3, 4), c(4, 5))) {
    sa <- sides[1L]
    sb <- sides[2L]
    bip <- matrix(0, sa + sb, sa + sb)
    bip[seq_len(sa), sa + seq_len(sb)] <- 1
    bip[sa + seq_len(sb), seq_len(sa)] <- 1
    imp <- (sa - 1) * (sb - 1)
    den <- sa + sb - 2
    expect_equal(
      unname(centrality_dil(bip)),
      c(rep(sb + sb * imp * (sb - 1) / den, sa),
        rep(sa + sa * imp * (sa - 1) / den, sb))
    )
  }
})

# ===========================================================================
# Invariants of the published equations
# ===========================================================================

test_that("U is never negative, so no score falls below its degree", {
  set.seed(50)
  for (trial in seq_len(20)) {
    n <- sample(3:14, 1L)
    a <- matrix(0, n, n)
    upper <- which(upper.tri(a))
    a[sample(upper, sample(seq_along(upper), 1L))] <- 1
    a <- pmax(a, t(a))
    terms <- cograph:::.cg_dil_terms(a)
    expect_true(all(attr(terms, "importance") >= 0))
    expect_true(all(terms$dil >= terms$degree))
  }
})

test_that("a line's importance is conserved when it is split", {
  # The two endpoint shares sum to one, so the network's total excess over
  # degree is exactly the total importance of its lines.
  set.seed(51)
  for (trial in seq_len(20)) {
    n <- sample(3:14, 1L)
    a <- matrix(0, n, n)
    upper <- which(upper.tri(a))
    a[sample(upper, sample(seq_along(upper), 1L))] <- 1
    a <- pmax(a, t(a))
    terms <- cograph:::.cg_dil_terms(a)
    importance <- attr(terms, "importance")
    expect_equal(sum(terms$dil - terms$degree), sum(importance) / 2)
    expect_equal(importance, t(importance))
  }
})

test_that("raw scores are component-local", {
  kite <- dil_kite()
  paw <- dil_paw()
  alone <- centrality_dil(kite)
  paw_alone <- centrality_dil(paw)
  padded <- matrix(0, 15L, 15L)
  padded[1:10, 1:10] <- kite
  padded[11:14, 11:14] <- paw
  joint <- centrality_dil(padded)
  expect_equal(unname(joint[1:10]), unname(alone))
  expect_equal(unname(joint[11:14]), unname(paw_alone))
  expect_equal(unname(joint[15]), 0)
})

# ===========================================================================
# Degenerate input
# ===========================================================================

test_that("an isolated K2 is the one 0/0 and it resolves to the degree", {
  # k_i + k_j - 2 vanishes only when both endpoints have degree one, and
  # there U = (1-0-1)(1-0-1) = 0 as well, so the importance being split is
  # exactly zero and every admissible split gives zero. Both nodes score 1.
  k2 <- dil_from_edges(2L, rbind(c(1, 2)))
  scores <- centrality_dil(k2)
  expect_equal(unname(scores), c(1, 1))
  expect_true(all(is.finite(scores)))
  expect_false(anyNA(scores))
  # The kernel must not evaluate the 0/0: the share matrix is a clean zero.
  terms <- cograph:::.cg_dil_terms(k2)
  expect_equal(attr(terms, "contributions"), matrix(0, 2, 2))
  expect_equal(attr(terms, "importance"), matrix(c(0, 0, 0, 0), 2, 2))
  # And it stays finite alongside other components.
  padded <- matrix(0, 5L, 5L)
  padded[1:2, 1:2] <- k2
  padded[3:4, 3:4] <- k2
  expect_equal(unname(centrality_dil(padded)), c(1, 1, 1, 1, 0))
})

test_that("isolates, singletons and the empty graph score without NA", {
  expect_equal(unname(centrality_dil(matrix(0, 1L, 1L))), 0)
  expect_equal(unname(centrality_dil(matrix(0, 6L, 6L))), rep(0, 6))
  expect_length(centrality_dil(matrix(0, 0L, 0L)), 0L)
  ring <- dil_ring(5)
  padded <- matrix(0, 6L, 6L)
  padded[1:5, 1:5] <- ring
  expect_equal(unname(centrality_dil(padded)), c(rep(3, 5), 0))
})

test_that("a disconnected graph needs no special rule", {
  two <- matrix(0, 8L, 8L)
  two[1:4, 1:4] <- dil_ring(4)
  two[5:8, 5:8] <- dil_ring(4)
  expect_equal(unname(centrality_dil(two)), rep(3, 8))
})

# ===========================================================================
# The public surface
# ===========================================================================

test_that("labels are preserved and the wrapper matches centrality()", {
  ring <- dil_ring(5)
  dimnames(ring) <- list(letters[1:5], letters[1:5])
  scores <- centrality_dil(ring)
  expect_named(scores, letters[1:5])
  df <- centrality(ring, measures = "dil")
  expect_equal(unname(df$dil), unname(scores))
  expect_equal(df$node, letters[1:5])
})

test_that("the measure is invariant under relabelling", {
  set.seed(52)
  kite <- dil_kite()
  base <- unname(centrality_dil(kite))
  for (trial in seq_len(10)) {
    perm <- sample.int(10L)
    expect_equal(unname(centrality_dil(kite[perm, perm])), base[perm])
  }
})

test_that("direction, weights, loops and mode are projected away", {
  # The source states its domain on page 210: "an undirected and unweighted
  # network". Everything outside it collapses onto the simple skeleton.
  directed <- matrix(0, 5L, 5L)
  directed[cbind(1:5, c(2:5, 1))] <- 1
  expect_equal(unname(centrality_dil(directed, directed = TRUE)), rep(3, 5))
  weighted <- dil_ring(5) * 7
  diag(weighted) <- 3
  expect_equal(
    unname(centrality_dil(weighted, loops = TRUE, mode = "in", cutoff = 1,
                          invert_weights = TRUE)),
    rep(3, 5)
  )
  expect_true("dil" %in% cograph:::.cg_no_mode_measures())
  expect_false("dil" %in% cograph:::.cg_weighted_measures())
})

test_that("normalized = TRUE max-scales the finished vector", {
  path <- dil_path(5)
  raw <- unname(centrality_dil(path))
  expect_equal(unname(centrality_dil(path, normalized = TRUE)),
               raw / max(raw))
})

test_that("the catalogue row reports the measure correctly", {
  expect_true("dil" %in% cograph:::.cg_no_mode_measures())
  # Two-hop local at O(n <k>^2), which the source claims in its Table 4,
  # page 218, so the measure is not held back from type = "all".
  expect_false("dil" %in% cograph:::.cg_costly_measures())
  row <- list_centralities()
  row <- row[row$measure == "dil", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$orientation, "higher")
  expect_false(row$mode_aware)
  expect_false(row$needs_membership)
  expect_false(row$uses_weights)
  expect_false(row$costly)
  everything <- suppressWarnings(centrality(dil_ring(5), type = "all"))
  expect_true("dil" %in% names(everything))
})

test_that("an unknown measure name is still refused", {
  # `dil` takes no parameter of its own, so the only validation it adds is
  # its name. Note the pre-existing asymmetry, left alone here: the
  # `include =` path raises a classed `cograph_unknown_measure` while the
  # `measures =` path raises a plain error.
  expect_error(centrality(dil_ring(5), measures = "dill"),
               "Unknown measures: dill")
  expect_error(centrality(dil_ring(5), type = "all", include = "dill"),
               class = "cograph_unknown_measure")
})
