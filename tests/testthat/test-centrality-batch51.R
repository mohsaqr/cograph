# Batch 51 --- trust-PageRank (Sheng, Zhu, Wang, Wang and Hou 2020,
# Algorithms 13(11):280, doi:10.3390/a13110280).
#
# The closed forms asserted here were derived by hand from equations (2),
# (4), (5), (6) and (7), journal pages 5 to 7. The published fixtures are
# Table 3 on page 6 and Table 5 on page 10, and they are named where they
# are used.

# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

tpr_from_edges <- function(n, el) {
  a <- matrix(0, n, n)
  a[el] <- 1
  a[el[, 2:1]] <- 1
  a
}

tpr_ring <- function(n) {
  tpr_from_edges(n, cbind(seq_len(n), c(seq_len(n - 1L) + 1L, 1L)))
}

tpr_path <- function(n) {
  tpr_from_edges(n, cbind(seq_len(n - 1L), seq_len(n - 1L) + 1L))
}

tpr_star <- function(leaves) {
  tpr_from_edges(leaves + 1L, cbind(rep(1L, leaves), seq_len(leaves) + 1L))
}

tpr_complete <- function(n) {
  a <- matrix(1, n, n)
  diag(a) <- 0
  a
}

# The five-node network of the paper's Figure 3, page 6. Its seven lines are
# recovered from the drawing and, independently, from Table 3's dash pattern.
tpr_fig3 <- function() {
  tpr_from_edges(5L, rbind(c(1, 2), c(1, 3), c(1, 5), c(2, 5), c(3, 4),
                           c(3, 5), c(4, 5)))
}

# The Krackhardt kite, labelled as the paper's Figure 5(a) draws it.
tpr_kite <- function() {
  tpr_from_edges(10L, rbind(c(6, 10), c(6, 5), c(6, 7), c(10, 5), c(10, 7),
                            c(10, 9), c(5, 7), c(5, 4), c(5, 3), c(7, 9),
                            c(7, 4), c(7, 8), c(9, 4), c(9, 8), c(4, 8),
                            c(4, 3), c(3, 2), c(2, 1)))
}

# A triangle with one pendant: the smallest graph carrying both a triangle
# and a degree-one node.
tpr_paw <- function() {
  tpr_from_edges(4L, rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4)))
}

tpr_petersen <- function() {
  tpr_from_edges(10L, rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 1),
                            c(6, 8), c(8, 10), c(10, 7), c(7, 9), c(9, 6),
                            c(1, 6), c(2, 7), c(3, 8), c(4, 9), c(5, 10)))
}

# The octahedron K_{2,2,2}: vertex-transitive and full of triangles, so a
# uniform score is forced by symmetry rather than by completeness.
tpr_octahedron <- function() {
  tpr_from_edges(6L, rbind(c(1, 3), c(1, 4), c(1, 5), c(1, 6), c(2, 3),
                           c(2, 4), c(2, 5), c(2, 6), c(3, 5), c(3, 6),
                           c(4, 5), c(4, 6)))
}

quiet <- function(expr) suppressWarnings(expr)

# ===========================================================================
# The published fixtures
# ===========================================================================

test_that("the paper's Table 3 similarities are reproduced to 2 dp", {
  # Page 6. The paper prints s(1,2) = s(3,4) = 0.60, s(1,3) = 0.52,
  # s(1,5) = s(3,5) = 0.47 and s(2,5) = s(4,5) = 0.51. Both sides are
  # compared under the paper's own rounding rather than under a widened
  # tolerance.
  similarity <- attr(cograph:::.cg_tpr_terms(tpr_fig3()), "similarity")
  lines <- rbind(c(1, 2), c(1, 3), c(1, 5), c(2, 5), c(3, 4), c(3, 5),
                 c(4, 5))
  computed <- similarity[lines]
  printed <- c(0.60, 0.52, 0.47, 0.51, 0.60, 0.47, 0.51)
  expect_equal(floor(computed * 100 + 0.5) / 100, printed)
  # The largest unrounded deviation, so a silent change of convention that
  # still rounded correctly would still be caught.
  expect_lt(max(abs(computed - printed)), 0.005)
  expect_gt(max(abs(computed - printed)), 0.004)
  # Symmetry is an invariant of equation (4), not an accident of the fixture.
  expect_equal(similarity, t(similarity))
})

test_that("Table 3's S_v column is the sum of its own rounded cells", {
  # Node 5's similarities sum to 1.966262, which rounds to 1.97, but the
  # paper prints 1.96 = 0.47 + 0.51 + 0.47 + 0.51. The convention is named
  # here rather than absorbed into a tolerance.
  terms <- cograph:::.cg_tpr_terms(tpr_fig3())
  rounded <- floor(attr(terms, "similarity") * 100 + 0.5) / 100
  printed <- c(1.59, 1.11, 1.59, 1.11, 1.96)
  expect_equal(rowSums(rounded), printed)
  expect_equal(round(terms$similarity_sum[5], 6), 1.966262)
  expect_false(isTRUE(all.equal(floor(terms$similarity_sum[5] * 100 + 0.5) /
                                  100, printed[5])))
})

test_that("Figure 3's printed edge annotation decodes symbol by symbol", {
  # Page 6 annotates the two arcs between nodes 1 and 2 with
  #   into node 1:  (1-k) x 0.6/1.11 + k x 3/7
  #   into node 2:  (1-k) x 0.6/1.59 + k x 2/9
  # Every symbol is a quantity of equations (2) and (5), which is what fixes
  # the (1-k)-on-similarity orientation of equation (6).
  terms <- cograph:::.cg_tpr_terms(tpr_fig3())
  similarity <- attr(terms, "similarity")
  expect_equal(floor(similarity[1, 2] * 100 + 0.5) / 100, 0.60)
  expect_equal(floor(rowSums(floor(similarity * 100 + 0.5) / 100) * 100 +
                       0.5) / 100, c(1.59, 1.11, 1.59, 1.11, 1.96))
  expect_equal(terms$degree, c(3, 2, 3, 2, 4))
  expect_equal(terms$neighbor_degree_sum, c(9, 7, 9, 7, 10))
  # And the trust matrix is the annotation evaluated at the paper's k.
  trust <- attr(terms, "trust")
  expect_equal(trust[1, 2],
               0.15 * similarity[1, 2] / terms$similarity_sum[2] +
                 0.85 * 3 / 7)
  expect_equal(trust[2, 1],
               0.15 * similarity[1, 2] / terms$similarity_sum[1] +
                 0.85 * 2 / 9)
})

test_that("Table 5's karate top ten is reproduced in order", {
  # Page 10, alpha = k = 0.85. All ten printed positions.
  zachary <- igraph::make_graph("Zachary")
  karate <- as.matrix(igraph::as_adjacency_matrix(zachary))
  score <- as.numeric(centrality_trust_pagerank(karate))
  expect_equal(order(-score, seq_len(34))[1:10],
               c(1, 34, 33, 3, 2, 32, 4, 14, 9, 24))
  # Positions 8 and 9 are a genuine near tie, so the ordering there is a
  # real claim about a gap of about 3e-05 rather than a formality.
  expect_gt(score[14] - score[9], 2.9e-05)
  expect_lt(score[14] - score[9], 3.0e-05)
  # The trust matrix is column-stochastic, so the scores are a distribution.
  expect_equal(sum(score), 1)
})

test_that("Table 5's kite top ten is reproduced up to its forced ties", {
  # Page 10. The kite's automorphism group swaps 4 with 5, 9 with 10 and 6
  # with 8, so those three pairs carry equal scores and the paper's order
  # within them is a tie-break. Ties are broken here by ascending label, a
  # deterministic secondary key, and the tie widths are asserted.
  score <- as.numeric(centrality_trust_pagerank(tpr_kite()))
  run <- cumsum(c(TRUE, abs(diff(sort(score, decreasing = TRUE))) > 1e-12))
  ordered <- order(-score, seq_len(10))
  ordered <- ordered[order(run, seq_len(10)[ordered])]
  expect_equal(ordered, c(7, 4, 5, 9, 10, 3, 6, 8, 2, 1))
  expect_lt(abs(score[4] - score[5]), 1e-15)
  expect_lt(abs(score[9] - score[10]), 1e-15)
  expect_lt(abs(score[6] - score[8]), 1e-15)
  expect_equal(sum(score), 1)
})

# ===========================================================================
# Hand-derived analytic families
# ===========================================================================

test_that("a complete graph scores 1 / n at every node", {
  # In K_n every node has degree n - 1 and every neighbourhood has total
  # degree (n-1)^2, so Rd = 1/(n-1); symmetry makes every similarity equal,
  # so Rs = 1/(n-1) too and T is the uniform column-stochastic matrix at
  # every k. The unique fixed point of equation (7) is then uniform.
  for (n in 3:10) {
    expect_equal(unname(centrality_trust_pagerank(tpr_complete(n))),
                 rep(1 / n, n), info = n)
  }
})

test_that("a vertex-transitive graph with triangles is uniform too", {
  # The octahedron is not complete, so this separates "uniform because
  # symmetric" from "uniform because everything is adjacent".
  expect_equal(unname(centrality_trust_pagerank(tpr_octahedron())),
               rep(1 / 6, 6))
})

test_that("the degree-only setting matches an explicit linear solve", {
  # At k = 1 the trust-value is T(i, j) = d_i / sum_{l in N_j} d_l, which
  # needs no similarity at all, so equation (7) can be solved in closed form
  # and compared against production's iteration.
  paw <- tpr_paw()
  d <- rowSums(paw)
  trust <- (matrix(d, 4, 4) / matrix(as.numeric(paw %*% d), 4, 4,
                                     byrow = TRUE)) * paw
  expected <- solve(diag(4) - 0.85 * trust, rep(0.15 / 4, 4))
  expect_equal(unname(centrality_trust_pagerank(paw, tpr_k = 1)), expected)
  expect_equal(colSums(trust), c(1, 1, 1, 1))
})

test_that("the similarity-only setting still uses the similarity", {
  # At k = 0 the degree ratio drops out entirely, so the score is driven by
  # the similarity recursion alone and must differ from the k = 1 answer.
  paw <- tpr_paw()
  only_similarity <- centrality_trust_pagerank(paw, tpr_k = 0)
  only_degree <- centrality_trust_pagerank(paw, tpr_k = 1)
  expect_false(isTRUE(all.equal(unname(only_similarity),
                                unname(only_degree))))
  expect_equal(sum(only_similarity), 1)
})

test_that("the trust matrix is column-stochastic at every k", {
  paw <- tpr_paw()
  for (k in c(0, 0.25, 0.5, 0.85, 1)) {
    trust <- attr(cograph:::.cg_tpr_terms(paw, mix = k), "trust")
    expect_equal(colSums(trust), c(1, 1, 1, 1), info = k)
  }
})

# ===========================================================================
# The degenerate class: a component with lines but no triangle
# ===========================================================================

test_that("every triangle-free family has no value at all", {
  # The similarity recursion is homogeneous with no triangle, its least
  # nonnegative fixed point is zero, and equation (2) is then 0/0. This is
  # most of the package's standard analytic test set, which is why it is
  # asserted family by family rather than mentioned in prose.
  families <- list(K2 = tpr_path(2), P3 = tpr_path(3), P4 = tpr_path(4),
                   P7 = tpr_path(7), C4 = tpr_ring(4), C5 = tpr_ring(5),
                   C8 = tpr_ring(8), S1 = tpr_star(1), S5 = tpr_star(5),
                   Petersen = tpr_petersen())
  for (name in names(families)) {
    scores <- quiet(centrality_trust_pagerank(families[[name]]))
    expect_true(all(is.na(scores)), info = name)
  }
  # Complete bipartite graphs too, including the ones on which the source's
  # own 0.1 initialisation would have been a fixed point.
  for (sides in list(c(2, 2), c(2, 3), c(3, 3), c(3, 4))) {
    bip <- matrix(0, sum(sides), sum(sides))
    bip[seq_len(sides[1L]), sides[1L] + seq_len(sides[2L])] <- 1
    bip <- pmax(bip, t(bip))
    expect_true(all(is.na(quiet(centrality_trust_pagerank(bip)))),
                info = paste(sides, collapse = ","))
  }
})

test_that("C3 is K3, so the ring family has a defined boundary", {
  expect_equal(unname(centrality_trust_pagerank(tpr_ring(3))), rep(1 / 3, 3))
  expect_true(all(is.na(quiet(centrality_trust_pagerank(tpr_ring(4))))))
})

test_that("the undefined case warns with a classed condition", {
  expect_warning(centrality_trust_pagerank(tpr_ring(6)),
                 class = "cograph_undefined_measure")
  expect_warning(centrality_trust_pagerank(tpr_ring(6)),
                 "divides zero by zero")
})

test_that("the domain does not move with tpr_k", {
  # At k = 1 the similarity ratio carries weight zero, but it is still part
  # of the trust-value the source defines, and cograph does not switch a
  # measure's domain on the knife-edge value k = 1.
  for (k in c(0, 0.5, 1)) {
    expect_true(all(is.na(quiet(centrality_trust_pagerank(tpr_ring(5),
                                                          tpr_k = k)))),
                info = k)
  }
})

test_that("an undefined component takes only itself down", {
  # An undefined trust column makes equation (7) undefined for everything
  # that solves against it, and for nothing else.
  mixed <- matrix(0, 7L, 7L)
  mixed[1:3, 1:3] <- tpr_ring(3)
  mixed[4:7, 4:7] <- tpr_path(4)
  scores <- quiet(centrality_trust_pagerank(mixed))
  expect_false(anyNA(scores[1:3]))
  expect_true(all(is.na(scores[4:7])))
  # The defined component keeps the shape it has on its own; only the
  # teleport term, which depends on n, differs.
  expect_equal(unname(scores[1] / scores[2]), 1)
})

test_that("an isolate is defined and keeps the bare teleport share", {
  # An isolate is never a denominator in equation (2), and equation (7)
  # gives it (1 - alpha)/n through an empty sum. Because its column emits
  # nothing, the scores no longer sum to one.
  padded <- matrix(0, 4L, 4L)
  padded[1:3, 1:3] <- tpr_ring(3)
  scores <- centrality_trust_pagerank(padded)
  expect_false(anyNA(scores))
  expect_equal(unname(scores[4]), 0.15 / 4)
  expect_lt(sum(scores), 1)
  expect_equal(unname(scores[1:3]), rep(unname(scores[1]), 3))
})

test_that("singletons, edgeless graphs and the empty graph behave", {
  expect_equal(unname(centrality_trust_pagerank(matrix(0, 1L, 1L))), 0.15)
  expect_equal(unname(centrality_trust_pagerank(matrix(0, 6L, 6L))),
               rep(0.15 / 6, 6L))
  expect_length(centrality_trust_pagerank(matrix(0, 0L, 0L)), 0L)
})

test_that("a disconnected graph of defined components needs no rule", {
  two <- matrix(0, 6L, 6L)
  two[1:3, 1:3] <- tpr_ring(3)
  two[4:6, 4:6] <- tpr_ring(3)
  expect_equal(unname(centrality_trust_pagerank(two)), rep(1 / 6, 6))
})

# ===========================================================================
# Parameters
# ===========================================================================

test_that("the source's claim that C does not matter is false", {
  # Page 5 says "the value of C does not affect the results, since only the
  # ratio of similarity is calculated". That holds for a homogeneous
  # recursion; the diagonal makes this one affine, so C enters the resolvent
  # as well as the scale.
  kite <- tpr_kite()
  base <- centrality_trust_pagerank(kite)
  half <- centrality_trust_pagerank(kite, tpr_decay = 0.5)
  expect_false(isTRUE(all.equal(unname(base), unname(half))))
  expect_gt(max(abs(unname(base) - unname(half))), 1e-6)
  # It is still a probability vector at every admissible C.
  expect_equal(sum(half), 1)
})

test_that("alpha interpolates between uniform and the trust limit", {
  kite <- tpr_kite()
  nearly_uniform <- centrality_trust_pagerank(kite, tpr_alpha = 1e-8)
  expect_lt(max(abs(unname(nearly_uniform) - 0.1)), 1e-7)
  expect_equal(sum(nearly_uniform), 1)
})

test_that("tightening the tolerance does not move the answer", {
  kite <- tpr_kite()
  expect_equal(unname(centrality_trust_pagerank(kite, tpr_tol = 1e-15,
                                                tpr_max_iter = 20000L)),
               unname(centrality_trust_pagerank(kite)),
               tolerance = 1e-11)
})

test_that("a stalled recursion warns instead of returning quietly", {
  expect_warning(centrality_trust_pagerank(tpr_kite(), tpr_max_iter = 1),
                 class = "cograph_no_converge")
  expect_warning(centrality_trust_pagerank(tpr_kite(), tpr_max_iter = 1),
                 "did not settle")
})

test_that("every parameter guard names the contract it enforces", {
  paw <- tpr_paw()
  expect_error(centrality_trust_pagerank(paw, tpr_alpha = 0), "tpr_alpha")
  expect_error(centrality_trust_pagerank(paw, tpr_alpha = 1), "tpr_alpha")
  expect_error(centrality_trust_pagerank(paw, tpr_alpha = c(0.5, 0.6)),
               "tpr_alpha")
  expect_error(centrality_trust_pagerank(paw, tpr_alpha = NA_real_),
               "tpr_alpha")
  expect_error(centrality_trust_pagerank(paw, tpr_k = -0.1), "tpr_k")
  expect_error(centrality_trust_pagerank(paw, tpr_k = 1.1), "tpr_k")
  expect_error(centrality_trust_pagerank(paw, tpr_decay = 0), "tpr_decay")
  expect_error(centrality_trust_pagerank(paw, tpr_decay = 1.5), "tpr_decay")
  expect_error(centrality_trust_pagerank(paw, tpr_tol = 0), "tpr_tol")
  expect_error(centrality_trust_pagerank(paw, tpr_tol = -1), "tpr_tol")
  expect_error(centrality_trust_pagerank(paw, tpr_max_iter = 0),
               "tpr_max_iter")
  expect_error(centrality_trust_pagerank(paw, tpr_max_iter = 2.5),
               "tpr_max_iter")
})

# ===========================================================================
# The public surface
# ===========================================================================

test_that("labels are preserved and the wrapper matches centrality()", {
  paw <- tpr_paw()
  dimnames(paw) <- list(letters[1:4], letters[1:4])
  scores <- centrality_trust_pagerank(paw)
  expect_named(scores, letters[1:4])
  df <- centrality(paw, measures = "trust_pagerank")
  expect_equal(unname(df$trust_pagerank), unname(scores))
  expect_equal(df$node, letters[1:4])
})

test_that("the measure is invariant under relabelling", {
  set.seed(51)
  kite <- tpr_kite()
  base <- unname(centrality_trust_pagerank(kite))
  for (trial in seq_len(10)) {
    perm <- sample.int(10L)
    expect_equal(unname(centrality_trust_pagerank(kite[perm, perm])),
                 base[perm])
  }
})

test_that("direction, weights, loops and mode are projected away", {
  # The source works in an undirected network with a(i, j) = 1 (page 3), so
  # everything outside that domain collapses onto the simple skeleton.
  directed <- matrix(0, 3L, 3L)
  directed[cbind(1:3, c(2, 3, 1))] <- 1
  expect_equal(unname(centrality_trust_pagerank(directed, directed = TRUE)),
               rep(1 / 3, 3))
  weighted <- tpr_paw() * 7
  diag(weighted) <- 3
  expect_equal(
    unname(centrality_trust_pagerank(weighted, loops = TRUE, mode = "in",
                                     cutoff = 1, invert_weights = TRUE)),
    unname(centrality_trust_pagerank(tpr_paw()))
  )
  expect_true("trust_pagerank" %in% cograph:::.cg_no_mode_measures())
  expect_false("trust_pagerank" %in% cograph:::.cg_weighted_measures())
})

test_that("normalized = TRUE max-scales the finished vector", {
  paw <- tpr_paw()
  raw <- unname(centrality_trust_pagerank(paw))
  expect_equal(unname(centrality_trust_pagerank(paw, normalized = TRUE)),
               raw / max(raw))
  # And it leaves an all-NA vector alone rather than dividing by nothing.
  expect_true(all(is.na(quiet(centrality_trust_pagerank(tpr_ring(5),
                                                        normalized = TRUE)))))
})

test_that("the catalogue row reports the measure correctly", {
  expect_true("trust_pagerank" %in% cograph:::.cg_no_mode_measures())
  # Two fixed-point recursions over dense n x n matrices, so it is held back
  # from type = "all" unless asked for.
  expect_true("trust_pagerank" %in% cograph:::.cg_costly_measures())
  row <- list_centralities()
  row <- row[row$measure == "trust_pagerank", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$orientation, "higher")
  expect_false(row$mode_aware)
  expect_false(row$needs_membership)
  expect_false(row$uses_weights)
  expect_true(row$costly)
  everything <- quiet(centrality(tpr_paw(), type = "all",
                                 include = "trust_pagerank"))
  expect_true("trust_pagerank" %in% names(everything))
})

test_that("an unknown measure name is still refused", {
  expect_error(centrality(tpr_paw(), measures = "trust_pagerankk"),
               "Unknown measures: trust_pagerankk")
  expect_error(centrality(tpr_paw(), type = "all", include = "trust_pr"),
               class = "cograph_unknown_measure")
})
