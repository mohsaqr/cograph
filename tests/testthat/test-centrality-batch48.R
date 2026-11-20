# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

lhc_kite <- function() {
  a <- matrix(0, 10, 10)
  el <- rbind(c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7),
              c(3, 4), c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7),
              c(6, 8), c(7, 8), c(8, 9), c(9, 10))
  for (k in seq_len(nrow(el))) {
    a[el[k, 1], el[k, 2]] <- 1
    a[el[k, 2], el[k, 1]] <- 1
  }
  a
}

lhc_ring <- function(n) {
  a <- matrix(0, n, n)
  for (k in seq_len(n)) {
    a[k, k %% n + 1L] <- 1
    a[k %% n + 1L, k] <- 1
  }
  a
}

test_that("the share is normalised by TNTS, not by the triangle count", {
  # The source states that the number of distinct triangles is (1/3) TNTS,
  # so TNTS = 3 * Delta and TP sums to exactly one. The Centrality Zoo's
  # entry 2.221 writes the denominator as Delta instead; cograph follows
  # the paper, and the two readings differ substantially.
  kite <- lhc_kite()
  terms <- .cg_lhc_terms(kite)
  expect_equal(attr(terms, "tnts"), 33)
  expect_equal(attr(terms, "triangles"), 11)
  expect_equal(attr(terms, "tnts"), 3 * attr(terms, "triangles"))
  expect_equal(sum(terms$tp), 1)
  expect_equal(unname(centrality_lhc(kite))[1], 3305 / 33)

  # The Zoo's literal reading, computed here only to show it is a different
  # number. cograph does not implement it.
  zoo <- {
    a <- kite
    d <- .cg_distances(a, "all")
    inside <- is.finite(d) & d >= 1 & d <= 2
    disc <- matrix(0, 10, 10)
    disc[inside] <- 1 / d[inside]^2
    tp <- .cg_triangle_counts(a) / 11
    as.numeric(a %*% (disc %*% (rowSums(a) * (1 + tp))))
  }
  expect_equal(zoo[1], 1380 / 11)
  expect_false(isTRUE(all.equal(unname(centrality_lhc(kite)), zoo)))
})

test_that("triangle counts agree with an independent implementation", {
  set.seed(4801)
  for (trial in seq_len(10)) {
    g <- igraph::sample_gnp(11, 0.35)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    expect_equal(.cg_triangle_counts(a),
                 as.numeric(igraph::count_triangles(g)))
  }
  expect_length(.cg_triangle_counts(matrix(0, 0, 0)), 0L)
})

test_that("a triangle-free graph drops TP rather than dividing by zero", {
  # TNTS = 0 makes TP a 0/0 at every node. Every numerator is zero too, so
  # cograph writes TP as zero and the index reduces to the plain
  # degree-over-squared-distance sum. Nothing is NA and nothing is NaN.
  # (The comparison with that sum moved to
  # tests/equivalence/test-centrality-zoo-b-lhc.R.)
  adj <- function(g) as.matrix(igraph::as_adjacency_matrix(g))
  graphs <- list(adj(igraph::make_tree(11, 3, mode = "undirected")),
                 adj(igraph::make_star(7, mode = "undirected")),
                 lhc_ring(8),
                 adj(igraph::make_full_bipartite_graph(3, 4)),
                 adj(igraph::make_ring(9)))
  for (a in graphs) {
    terms <- .cg_lhc_terms(a)
    expect_equal(attr(terms, "tnts"), 0)
    expect_equal(terms$tp, rep(0, nrow(terms)))
    expect_true(all(is.finite(terms$lhc)))
  }
})

test_that("the radius widens monotonically and then saturates", {
  set.seed(4803)
  a <- as.matrix(igraph::as_adjacency_matrix(igraph::sample_gnp(14, 0.25)))
  scores <- lapply(1:6, function(r) unname(centrality_lhc(a, lhc_radius = r)))
  for (r in 2:6) expect_true(all(scores[[r]] >= scores[[r - 1L]] - 1e-12))
  # Beyond the diameter nothing more can enter Phi.
  far <- unname(centrality_lhc(a, lhc_radius = 40))
  expect_equal(unname(centrality_lhc(a, lhc_radius = 200)), far)
  # A ring's ball is the whole graph once the radius reaches the radius of
  # the graph, and the score stops moving there.
  ring <- lhc_ring(9)
  expect_equal(unname(centrality_lhc(ring, lhc_radius = 4)),
               unname(centrality_lhc(ring, lhc_radius = 9)))
})

test_that("scores are not component-local, and exactly why", {
  kite <- lhc_kite()
  base <- unname(centrality_lhc(kite))

  # An isolate changes no degree, no triangle and no finite distance, so it
  # is inert -- and scores zero itself, because tau is empty.
  padded <- matrix(0, 11, 11)
  padded[1:10, 1:10] <- kite
  expect_equal(unname(centrality_lhc(padded))[1:10], base)
  expect_equal(unname(centrality_lhc(padded))[11], 0)

  # So is a triangle-free component, for the same reason.
  with_edge <- matrix(0, 12, 12)
  with_edge[1:10, 1:10] <- kite
  with_edge[11, 12] <- with_edge[12, 11] <- 1
  expect_equal(unname(centrality_lhc(with_edge))[1:10], base)

  # A component carrying a triangle is NOT inert: TNTS is a global sum, so
  # every TP and therefore every score is rescaled.
  with_triangle <- matrix(0, 13, 13)
  with_triangle[1:10, 1:10] <- kite
  with_triangle[11:13, 11:13] <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  moved <- unname(centrality_lhc(with_triangle))[1:10]
  expect_true(all(abs(moved - base) > 1e-8))
  expect_equal(attr(.cg_lhc_terms(with_triangle), "tnts"), 36)
})

test_that("degenerate graphs return derived zeros or nothing at all", {
  expect_length(centrality_lhc(igraph::make_empty_graph(0)), 0L)
  expect_equal(unname(centrality_lhc(igraph::make_empty_graph(1))), 0)
  expect_equal(unname(centrality_lhc(igraph::make_empty_graph(6))), rep(0, 6))
  expect_length(.cg_lhc_terms(matrix(0, 0, 0))$lhc, 0L)
  expect_equal(attr(.cg_lhc_terms(matrix(0, 0, 0)), "tnts"), 0)
})

test_that("direction, weights and loops are absorbed by the skeleton", {
  # k_u is a count, d(uv) a hop count and NTS(u) combinatorial, so the
  # source's simple undirected skeleton must swallow all three.
  kite <- lhc_kite()
  base <- centrality_lhc(kite)
  projected <- kite * 13
  diag(projected) <- 5
  expect_equal(centrality_lhc(projected, loops = TRUE, mode = "in",
                              cutoff = 1, invert_weights = TRUE), base)
  # A directed orientation of the same edges reads as the same skeleton.
  upper <- kite
  upper[lower.tri(upper)] <- 0
  expect_equal(unname(centrality_lhc(upper, directed = TRUE)), unname(base))
})

test_that("lhc_radius is enforced, not extended", {
  ring <- lhc_ring(6)
  for (bad in list(0, -1, 0.5, 2.5)) {
    expect_error(centrality_lhc(ring, lhc_radius = bad),
                 class = "cograph_bad_parameter")
  }
  for (bad in list(NA_real_, Inf, c(2, 2), "2")) {
    expect_error(centrality_lhc(ring, lhc_radius = bad),
                 class = "cograph_bad_parameter")
  }
  # Integer storage is accepted and gives the same answer as the double.
  expect_equal(centrality_lhc(ring, lhc_radius = 2L),
               centrality_lhc(ring, lhc_radius = 2))
})

test_that("labels, permutation and normalization behave", {
  a <- lhc_kite()
  dimnames(a) <- list(letters[1:10], letters[1:10])
  score <- centrality_lhc(a)
  expect_named(score, letters[1:10])
  expect_equal(centrality(a, measures = "lhc")$lhc, unname(score))
  set.seed(4804)
  perm <- sample.int(10)
  expect_equal(unname(centrality_lhc(a[perm, perm])), unname(score)[perm])
  expect_equal(unname(centrality_lhc(a, normalized = TRUE)),
               unname(score) / max(unname(score)))
})

test_that("lhc is an unweighted no-mode measure of ordinary cost", {
  meta <- list_centralities()
  expect_true("lhc" %in% meta$measure)
  row <- meta[meta$measure == "lhc", ]
  expect_false(row$uses_weights)
  expect_false(row$costly)
  expect_false(row$mode_aware)
  expect_false(row$needs_membership)
  expect_identical(row$orientation, "higher")
  expect_true("lhc" %in% .cg_no_mode_measures())
  expect_false("lhc" %in% .cg_mode_measures())
  expect_false("lhc" %in% .cg_costly_measures())
  expect_false("lhc" %in% .cg_weighted_measures())
  expect_identical(names(formals(centrality_lhc)), c("x", "..."))
  expect_true("lhc_radius" %in% names(formals(centrality)))
  expect_equal(eval(formals(centrality)$lhc_radius), 2)
})
