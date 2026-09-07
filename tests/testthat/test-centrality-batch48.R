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

lhc_star <- function(leaves) {
  a <- matrix(0, leaves + 1L, leaves + 1L)
  a[1L, -1L] <- 1
  a[-1L, 1L] <- 1
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

test_that("the path 1-2-3 matches its hand derivation", {
  # Triangle-free, so TP is zero. Degrees 1, 2, 1 and d(1,3) = 2, so
  # C(end) = 2/1 + 1/4 and C(middle) = 1/1 + 1/1 = 2. Equation (2) then
  # gives Lhc(end) = C(middle) = 2 and Lhc(middle) = 2 C(end) = 4.5.
  p3 <- matrix(0, 3, 3)
  p3[1, 2] <- p3[2, 1] <- p3[2, 3] <- p3[3, 2] <- 1
  expect_equal(unname(centrality_lhc(p3)), c(2, 4.5, 2))
  # At radius 1 the second-order term disappears: C(end) = 2, C(mid) = 2.
  expect_equal(unname(centrality_lhc(p3, lhc_radius = 1)), c(2, 4, 2))
})

test_that("a star matches its hand derivation", {
  # A star is triangle-free. The centre has degree m and every leaf 1;
  # centre-leaf distance is 1 and leaf-leaf distance 2, so
  #   C(centre) = m * 1,      C(leaf) = m + (m - 1) / 4   (radius >= 2)
  #   Lhc(centre) = m * C(leaf),  Lhc(leaf) = C(centre) = m.
  for (m in 2:9) {
    star <- lhc_star(m)
    expect_equal(unname(centrality_lhc(star)),
                 c(m * (m + (m - 1) / 4), rep(m, m)))
    # At radius 1 the leaf-leaf term vanishes and C(leaf) = m.
    expect_equal(unname(centrality_lhc(star, lhc_radius = 1)),
                 c(m * m, rep(m, m)))
  }
})

test_that("a complete graph matches its hand derivation", {
  # Every degree is n - 1, every NTS is (n-1)(n-2)/2, so TNTS is
  # n(n-1)(n-2)/2 and TP is exactly 1/n. Every distance is 1, so
  #   C(v) = (n-1) * (n-1) * (1 + 1/n)  and  Lhc(v) = (n-1) C(v).
  closed <- function(n) (n - 1)^3 * (n + 1) / n
  for (n in 3:8) {
    for (radius in 1:3) {
      expect_equal(unname(centrality_lhc(igraph::make_full_graph(n),
                                         lhc_radius = radius)),
                   rep(closed(n), n))
    }
  }
  expect_equal(unname(centrality_lhc(igraph::make_full_graph(5))),
               rep(76.8, 5))
})

test_that("a ring matches its hand derivation while the ball does not wrap", {
  # C_n is triangle-free for n >= 4 and every degree is 2, with exactly two
  # nodes at each distance 1..radius as long as n > 2 * radius:
  #   C(v) = sum_{d = 1..radius} 4 / d^2,   Lhc(v) = 2 C(v).
  for (radius in 1:3) {
    closed <- 2 * sum(4 / seq_len(radius)^2)
    for (n in seq(max(4, 2 * radius + 1), max(4, 2 * radius + 1) + 4)) {
      expect_equal(unname(centrality_lhc(lhc_ring(n), lhc_radius = radius)),
                   rep(closed, n))
    }
  }
  # The n >= 4 precondition is load-bearing, not decorative: C_3 is K_3,
  # its TP term does not vanish, and it follows the complete-graph form
  # (32/3) rather than the triangle-free ring form (8).
  expect_equal(unname(centrality_lhc(lhc_ring(3), lhc_radius = 1)),
               rep(32 / 3, 3))
})

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
  plain <- function(a, radius) {
    n <- nrow(a)
    d <- .cg_distances(a, "all")
    inside <- is.finite(d) & d >= 1 & d <= radius
    disc <- matrix(0, n, n)
    disc[inside] <- 1 / d[inside]^2
    as.numeric(a %*% (disc %*% rowSums(a)))
  }
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
    for (radius in 1:3) {
      expect_equal(unname(centrality_lhc(a, lhc_radius = radius)),
                   plain(a, radius))
    }
  }
})

test_that("the kernel agrees with the definition written out node by node", {
  # Equations (1) and (2) accumulated with explicit loops over Phi(v) and
  # tau(v), rather than as the matrix products production uses. The point
  # of the test is that it is written out, so the loops are the check.
  definitional <- function(a, radius) {
    a <- .cg_undirected_view(a)
    n <- nrow(a)
    k <- rowSums(a)
    nts <- .cg_triangle_counts(a)
    tnts <- sum(nts)
    tp <- if (tnts > 0) nts / tnts else rep(0, n)
    d <- .cg_distances(a, "all")
    influence <- numeric(n)
    for (v in seq_len(n)) {
      total <- 0
      for (u in seq_len(n)) {
        if (is.finite(d[v, u]) && d[v, u] >= 1 && d[v, u] <= radius) {
          total <- total + k[u] * (1 + tp[u]) / d[v, u]^2
        }
      }
      influence[v] <- total
    }
    out <- numeric(n)
    for (v in seq_len(n)) {
      for (w in seq_len(n)) if (a[v, w] > 0) out[v] <- out[v] + influence[w]
    }
    out
  }
  set.seed(4802)
  adj <- function(g) as.matrix(igraph::as_adjacency_matrix(g))
  graphs <- list(lhc_kite(),
                 adj(igraph::sample_gnp(12, 0.3)),
                 adj(igraph::sample_gnp(9, 0.5)),
                 adj(igraph::make_tree(13, 2, mode = "undirected")),
                 adj(igraph::sample_gnp(10, 0.25)))
  for (a in graphs) {
    for (radius in c(1, 2, 3, 7)) {
      expect_equal(.cg_lhc_terms(a, radius)$lhc, definitional(a, radius))
    }
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
