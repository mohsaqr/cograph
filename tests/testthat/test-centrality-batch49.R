# Batch 49 --- immediate effects centrality (Friedkin 1991, AJS 96(6)).
#
# The closed forms asserted here were derived by hand from equations (11)
# and (20); three of them also appear in the paper's Table 1, pages
# 1492-1494, and those rows are named where they are used.

iec_star <- function(leaves) {
  a <- matrix(0, leaves + 1L, leaves + 1L)
  a[1L, -1L] <- 1
  a[-1L, 1L] <- 1
  a
}

iec_ring <- function(n) {
  a <- matrix(0, n, n)
  a[cbind(seq_len(n), c(seq_len(n - 1L) + 1L, 1L))] <- 1
  a[cbind(c(seq_len(n - 1L) + 1L, 1L), seq_len(n))] <- 1
  a
}

iec_directed_cycle <- function(n) {
  a <- matrix(0, n, n)
  a[cbind(seq_len(n), c(seq_len(n - 1L) + 1L, 1L))] <- 1
  a
}

iec_kite <- function() {
  a <- matrix(0, 10, 10)
  el <- rbind(c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7),
              c(3, 4), c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7),
              c(6, 8), c(7, 8), c(8, 9), c(9, 10))
  a[el] <- 1
  a[el[, 2:1]] <- 1
  a
}

iec_complete <- function(n) {
  a <- matrix(1, n, n)
  diag(a) <- 0
  a
}

test_that("a complete graph scores 1/n at every node", {
  # W = J/n, so W^Inf = W, Z = I, Z_dg = I and equation (11) gives
  # M = (I - I + E) diag(n) = n E. Every off-diagonal column entry is n,
  # the column sum over i != j is n(n-1) and the score is 1/n.
  # Friedkin's Table 1 prints .200 for network 21, the five-node case.
  for (n in 3:9) {
    expect_equal(unname(centrality_iec(iec_complete(n))), rep(1 / n, n))
  }
  expect_equal(unname(centrality_iec(iec_complete(5))), rep(0.2, 5))
})

test_that("a star matches its hand derivation", {
  # With a_ii = 1 a leaf stays put or steps to the centre with probability
  # 1/2 each, so its mean first passage to the centre is 2 and the centre's
  # column sums to 2m: IEC(centre) = m / (2m) = 1/2 for every m.
  # For a leaf target, x = E[steps from the centre] and y = E[steps from
  # another leaf] satisfy x = 1 + x/(m+1) + (m-1)y/(m+1) and y = 1 + y/2 +
  # x/2. The second gives y = x + 2; substituting, every x term cancels and
  # x = 3m - 1, y = 3m + 1, so the column sums to (3m-2)(m+1) and
  # IEC(leaf) = m / ((3m-2)(m+1)).
  # Table 1 network 1 prints .500 and .080, which is m = 4.
  for (m in 2:9) {
    expect_equal(unname(centrality_iec(iec_star(m))),
                 c(0.5, rep(m / ((3 * m - 2) * (m + 1)), m)))
  }
  expect_equal(unname(centrality_iec(iec_star(4))), c(0.5, rep(0.08, 4)))
})

test_that("a ring matches its hand derivation", {
  # W is circulant with 1/3 on the diagonal and on both neighbours: the
  # cycle walk made lazy with probability 1/3. Laziness multiplies every
  # mean first passage time by 3/2, so from m(d) = d(q-d) the column sums
  # to (3/2) q(q^2-1)/6 = q(q^2-1)/4 and IEC = 4/(q(q+1)).
  # Table 1 network 8, the pentagon, prints .133 = 4/30.
  for (q in 3:10) {
    expect_equal(unname(centrality_iec(iec_ring(q))),
                 rep(4 / (q * (q + 1)), q))
  }
  expect_equal(unname(centrality_iec(iec_ring(5))), rep(4 / 30, 5))
  # C_3 is K_3, so the ring and complete forms must agree there.
  expect_equal(unname(centrality_iec(iec_ring(3))),
               unname(centrality_iec(iec_complete(3))))
})

test_that("a directed cycle matches its hand derivation", {
  # Every row sum is 2, so the walker stays or advances one arc with
  # probability 1/2. Reaching a node d arcs ahead needs d advances, each a
  # Geometric(1/2) wait, so m_ij = 2d, the column sums to q(q-1) and the
  # score is 1/q --- what the complete graph gives, on a quarter of the arcs.
  for (q in 3:9) {
    expect_equal(unname(centrality_iec(iec_directed_cycle(q),
                                       directed = TRUE)),
                 rep(1 / q, q))
  }
})

test_that("the five-node path reproduces Friedkin's Table 1 network 3", {
  # The paper prints .050 .105 .167 for the end, the next node and the
  # middle. The exact values are 1/20, 2/19 and 1/6.
  p5 <- matrix(0, 5, 5)
  p5[cbind(1:4, 2:5)] <- 1
  p5[cbind(2:5, 1:4)] <- 1
  scores <- unname(centrality_iec(p5))
  expect_equal(scores, c(1 / 20, 2 / 19, 1 / 6, 2 / 19, 1 / 20))
  expect_equal(round(scores, 3), c(0.050, 0.105, 0.167, 0.105, 0.050))
})

test_that("the influence chain carries the source's unit self-loop", {
  # Page 1494: "the diagonal entries of its adjacency matrix were set to
  # one". The kernel's stationary vector is then Friedkin's TEC, which on
  # undirected input has the closed form (d_i + 1) / sum_k (d_k + 1).
  kite <- iec_kite()
  terms <- .cg_iec_terms(kite)
  degree <- rowSums(kite)
  expect_equal(terms$stationary, (degree + 1) / sum(degree + 1))
  # The five-node star's TEC is 5/13 and 2/13, printed .385 and .154.
  star_terms <- .cg_iec_terms(iec_star(4))
  expect_equal(star_terms$stationary, c(5 / 13, rep(2 / 13, 4)))
  # A loop already in the input is absorbed by that diagonal, not added to.
  looped <- kite
  diag(looped) <- 1
  expect_equal(unname(centrality_iec(looped)), unname(centrality_iec(kite)))
})

test_that("the mean first passage matrix satisfies its defining identities", {
  kite <- iec_kite()
  terms <- .cg_iec_terms(kite)
  mfpt <- attr(terms, "mfpt")
  w <- attr(terms, "influence")
  n <- nrow(kite)
  # m_ii is the mean recurrence time 1 / c_i (Kemeny and Snell).
  expect_equal(diag(mfpt), 1 / terms$stationary)
  expect_equal(terms$recurrence, 1 / terms$stationary)
  # First-step analysis: m_ij = 1 + sum_{k != j} w_ik m_kj. Production never
  # uses this characterisation, so it is an independent identity.
  residual <- vapply(seq_len(n), function(j) {
    column <- mfpt[, j]
    max(abs(column - 1 - (w %*% column - w[, j] * column[j])))
  }, numeric(1))
  expect_lt(max(residual), 1e-9)
  # Kemeny's constant: sum_j c_j m_ij does not depend on i.
  totals <- as.numeric(mfpt %*% terms$stationary)
  expect_lt(max(totals) - min(totals), 1e-9)
  # Equation (20) is the reciprocal of the mean of the column, diagonal out.
  expect_equal(terms$iec, (n - 1) / (colSums(mfpt) - diag(mfpt)))
})

test_that("a reducible influence chain is NA with a classed warning", {
  two <- matrix(0, 4, 4)
  two[1, 2] <- two[2, 1] <- two[3, 4] <- two[4, 3] <- 1
  expect_warning(scores <- centrality_iec(two),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(scores)))
  expect_length(scores, 4L)
  # An isolate makes the whole graph undefined; it does not score zero.
  padded <- matrix(0, 11, 11)
  padded[1:10, 1:10] <- iec_kite()
  expect_warning(padded_scores <- centrality_iec(padded),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(padded_scores)))
  expect_false(any(padded_scores %in% 0))
  # A directed graph that is connected but not strongly connected is
  # refused for the same reason.
  weak <- matrix(0, 3, 3)
  weak[1, 2] <- weak[2, 3] <- 1
  expect_warning(weak_scores <- centrality_iec(weak, directed = TRUE),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(weak_scores)))
  # An edgeless graph is reducible as soon as it has two nodes.
  expect_warning(edgeless <- centrality_iec(matrix(0, 6, 6)),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(edgeless)))
})

test_that("degenerate sizes are handled explicitly", {
  # Equation (20) divides by n - 1, which is zero on a singleton.
  expect_warning(single <- centrality_iec(matrix(0, 1, 1)),
                 class = "cograph_undefined_measure")
  expect_true(is.na(single))
  expect_length(single, 1L)
  # An empty graph has no nodes and therefore no scores, and no warning.
  expect_silent(empty <- centrality_iec(matrix(0, 0, 0)))
  expect_length(empty, 0L)
  expect_type(empty, "double")
  expect_identical(attr(.cg_iec_terms(matrix(0, 0, 0)), "status"), "empty")
  expect_identical(attr(.cg_iec_terms(matrix(0, 1, 1)), "status"), "singleton")
  expect_identical(attr(.cg_iec_terms(matrix(0, 4, 4)), "status"), "reducible")
  expect_identical(attr(.cg_iec_terms(iec_kite()), "status"), "ok")
})

test_that("labels are preserved and the score is permutation invariant", {
  kite <- iec_kite()
  labels <- paste0("n", seq_len(10))
  dimnames(kite) <- list(labels, labels)
  scores <- centrality_iec(kite)
  expect_identical(names(scores), labels)
  perm <- c(7, 2, 9, 1, 4, 10, 3, 6, 8, 5)
  permuted <- centrality_iec(kite[perm, perm])
  expect_equal(unname(permuted), unname(scores)[perm])
  expect_identical(names(permuted), labels[perm])
})

test_that("direction is kept and everything else is projected away", {
  kite <- iec_kite()
  base <- unname(centrality_iec(kite))
  weighted <- kite * 13
  diag(weighted) <- 5
  expect_equal(unname(centrality_iec(weighted, loops = TRUE, mode = "in",
                                     cutoff = 1, invert_weights = TRUE)),
               base)
  # Parallel edges collapse: a_ij = 1 wherever a line exists.
  expect_equal(unname(centrality_iec(kite * 2)), base)
  # Direction, by contrast, changes the answer: a directed triangle and the
  # undirected triangle are different chains.
  arc_scores <- unname(centrality_iec(iec_directed_cycle(4), directed = TRUE))
  edge_scores <- unname(centrality_iec(iec_ring(4)))
  expect_false(isTRUE(all.equal(arc_scores, edge_scores)))
})

test_that("normalized = TRUE max-scales the raw vector", {
  kite <- iec_kite()
  raw <- unname(centrality_iec(kite))
  expect_equal(unname(centrality_iec(kite, normalized = TRUE)),
               raw / max(raw))
  expect_equal(max(centrality_iec(kite, normalized = TRUE)), 1)
})

test_that("centrality() agrees with the wrapper and reports the metadata", {
  kite <- iec_kite()
  df <- centrality(kite, measures = "iec")
  expect_true("iec" %in% names(df))
  expect_equal(df$iec, unname(centrality_iec(kite)))
  # No mode suffix: the measure has no in/out/all variant.
  expect_false(any(grepl("^iec_", names(df))))
  expect_true("iec" %in% .cg_no_mode_measures())
  expect_true("iec" %in% .cg_costly_measures())
  expect_false("iec" %in% .cg_weighted_measures())
  row <- list_centralities()
  row <- row[row$measure == "iec", ]
  expect_equal(nrow(row), 1L)
  expect_false(row$mode_aware)
  expect_false(row$uses_weights)
  expect_false(row$needs_membership)
  expect_true(row$costly)
  expect_identical(row$orientation, "higher")
  # Costly measures are held back from type = "all"; naming one in
  # `measures =` always computes it, as the equality above shows.
  expect_true("iec" %in% list_centralities(costly = TRUE)$measure)
})

test_that("iec is not markov, and not a rescaling of it", {
  # Two differences: markov normalises A without the unit diagonal, and it
  # divides the column sum by n rather than n - 1. The second is a constant
  # factor; the first is not, and it can reorder nodes.
  star <- iec_star(4)
  expect_equal(unname(centrality_iec(star)), c(0.5, rep(0.08, 4)))
  expect_false(isTRUE(all.equal(unname(centrality_iec(star)),
                                unname(centrality_markov(star)))))
  kite <- iec_kite()
  ratio <- unname(centrality_iec(kite)) / unname(centrality_markov(kite))
  # A pure rescaling would make every ratio identical.
  expect_gt(max(ratio) - min(ratio), 1e-6)
})
