# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("hcc is 2 everywhere on a regular graph and ehcc is 2 (1 + k)", {
  # Every node of an r-regular graph has the same extended degree for any
  # delta, so one round removes the whole graph: pos and pos_max are both
  # one, the extended-degree share is one, and HCC is 1 + 1 = 2.
  for (n in 3:8) {
    ring <- igraph::make_ring(n)
    expect_equal(unname(centrality_hcc(ring)), rep(2, n))
    expect_equal(unname(centrality_ehcc(ring)), rep(6, n))
    full <- igraph::make_full_graph(n)
    expect_equal(unname(centrality_hcc(full)), rep(2, n))
    expect_equal(unname(centrality_ehcc(full)), rep(2 * n, n))
  }
  # The single edge is one-regular.
  expect_equal(unname(centrality_hcc(igraph::make_full_graph(2))), c(2, 2))
  expect_equal(unname(centrality_ehcc(igraph::make_full_graph(2))), c(4, 4))
})

test_that("hcc has a closed form on stars", {
  # A q-star has one centre of degree q and q leaves of degree one. The
  # centre's extended degree is delta q + (1 - delta) q = q; a leaf's is
  # delta + (1 - delta) q, which is smaller whenever q > 1 *and* delta > 0,
  # so round one removes every leaf and round two the now isolated centre.
  # The maximum extended degree is the centre's q and pos_max is two. At
  # delta = 0 the two coincide and the star peels in one round instead; it
  # is covered by the uniform case in the hcc_delta test below.
  for (q in 2:7) {
    g <- igraph::make_star(q + 1, mode = "undirected")
    for (delta in c(0.25, 0.5, 0.75, 1)) {
      leaf <- (delta + (1 - delta) * q) / q + 1 / 2
      score <- centrality_hcc(g, hcc_delta = delta)
      expect_equal(unname(score), c(2, rep(leaf, q)))
      expect_equal(unname(centrality_ehcc(g, hcc_delta = delta)),
                   c(2 + q * leaf, rep(leaf + 2, q)))
    }
  }
  # delta = 0.5 on the five-star: leaves score 3/5 + 1/2 = 1.1.
  expect_equal(unname(centrality_hcc(igraph::make_star(6, "undirected"))),
               c(2, rep(1.1, 5)))
})

test_that("hcc follows a hand-computed peel on paths", {
  # Path of five. Degrees 1, 2, 2, 2, 1; extended degrees at delta = 0.5
  # are 1.5, 2.5, 3, 2.5, 1.5. Round one removes the two ends, leaving a
  # three-path whose ends have extended degree 1.5 and whose centre has 2,
  # so round two removes them and round three the centre alone. pos is
  # 1, 2, 3, 2, 1 and pos_max is three; k^ex_max is 3.
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  terms <- .cg_hcc_terms(as.matrix(igraph::as_adjacency_matrix(g)))
  expect_equal(terms$kex, c(1.5, 2.5, 3, 2.5, 1.5))
  expect_equal(terms$pos, c(1, 2, 3, 2, 1))
  expect_equal(attr(terms, "pos_max"), 3L)
  expect_equal(attr(terms, "kex_max"), 3)
  score <- c(1.5 / 3 + 1 / 3, 2.5 / 3 + 2 / 3, 1 + 1,
             2.5 / 3 + 2 / 3, 1.5 / 3 + 1 / 3)
  expect_equal(unname(centrality_hcc(g)), score)
  expect_equal(unname(centrality_ehcc(g)),
               c(score[1] + score[2], sum(score[1:3]), sum(score[2:4]),
                 sum(score[3:5]), score[4] + score[5]))
  expect_equal(unname(centrality_hcc(g, normalized = TRUE)), score / max(score))
  # The three-path is the two-star, so its centre scores 2 and its ends
  # (0.5 + 0.5 * 2) / 2 + 1 / 2 = 1.25.
  path3 <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  expect_equal(unname(centrality_hcc(path3)), c(1.25, 2, 1.25))
  # The four-path peels in two rounds: the ends, then the middle pair,
  # whose extended degree drops from 2 to 1 once the ends are gone.
  path4 <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = FALSE)
  four <- .cg_hcc_terms(as.matrix(igraph::as_adjacency_matrix(path4)))
  expect_equal(four$pos, c(1, 2, 2, 1))
})

test_that("hcc reproduces every value the source prints for its figure 1", {
  # Liu and Zheng (2023), Scientific Reports 13:3197, page 4. The figure 1
  # graph has 10 nodes and 14 edges; tables 1, 2 and 3 print the classical
  # and extended degrees, the six-round decomposition trace and the HCC
  # and EHCC scores.
  nodes <- letters[1:10]
  pairs <- c("c", "d", "a", "b", "a", "e", "d", "f", "d", "e", "d", "g",
             "e", "f", "f", "g", "f", "i", "b", "e", "e", "g", "g", "i",
             "g", "h", "i", "j")
  g <- igraph::make_graph(pairs, directed = FALSE)
  expect_equal(igraph::vcount(g), 10L)
  expect_equal(igraph::ecount(g), 14L)
  terms <- .cg_hcc_terms(as.matrix(igraph::as_adjacency_matrix(g)))
  order_in_graph <- match(nodes, igraph::V(g)$name)
  # Table 1, printed.
  expect_equal(terms$degree[order_in_graph], c(2, 2, 1, 4, 5, 4, 5, 1, 3, 1))
  expect_equal(terms$kex[order_in_graph],
               c(4.5, 4.5, 2.5, 9.5, 11, 10.5, 11, 3, 6.5, 2))
  # Table 2, printed: the position indexes of the six iterations.
  expect_equal(terms$pos[order_in_graph], c(4, 4, 2, 6, 6, 6, 6, 3, 5, 1))
  expect_equal(attr(terms, "pos_max"), 6L)
  expect_equal(attr(terms, "kex_max"), 11)
  # Table 3, printed: ten HCC values at the printed two decimals.
  score <- centrality_hcc(g)[nodes]
  expect_equal(unname(round(score, 2)),
               c(1.08, 1.08, 0.56, 1.86, 2, 1.95, 2, 0.77, 1.42, 0.35))
  # The paper's own worked lines HCC(a) = 4.5/11 + 4/6 and
  # EHCC(a) = HCC(a) + HCC(b) + HCC(e).
  expect_equal(unname(score[["a"]]), 4.5 / 11 + 4 / 6)
  escore <- centrality_ehcc(g)[nodes]
  expect_equal(unname(escore[["a"]]),
               unname(score[["a"]] + score[["b"]] + score[["e"]]))
  expect_equal(unname(round(escore, 2)),
               c(4.15, 4.15, 2.42, 8.38, 9.97, 9.24, 10.02, 2.77, 5.73, 1.77))
})

test_that("one printed EHCC value disagrees with the source's own rounding", {
  # Table 3 prints EHCC(g) = 10.01. The exact value is 661/66, which is
  # 10.015151... and rounds to 10.02, a gap of 0.00515 outside the
  # half-unit. 10.01 is the truncation, but six other printed cells --
  # HCC(a), HCC(b), HCC(j), EHCC(d), EHCC(e) and EHCC(i) -- require
  # rounding and fail under truncation, so no single convention
  # reproduces all twenty printed entries. The disagreement is recorded,
  # not tuned away; it does not change the paper's ranking.
  nodes <- letters[1:10]
  pairs <- c("c", "d", "a", "b", "a", "e", "d", "f", "d", "e", "d", "g",
             "e", "f", "f", "g", "f", "i", "b", "e", "e", "g", "g", "i",
             "g", "h", "i", "j")
  g <- igraph::make_graph(pairs, directed = FALSE)
  escore <- centrality_ehcc(g)[nodes]
  expect_equal(unname(escore[["g"]]), 661 / 66)
  expect_equal(round(unname(escore[["g"]]), 2), 10.02)
  expect_false(isTRUE(all.equal(round(unname(escore[["g"]]), 2), 10.01)))
  expect_equal(trunc(unname(escore[["g"]]) * 100) / 100, 10.01)
  printed <- c(1.08, 1.08, 0.56, 1.86, 2, 1.95, 2, 0.77, 1.42, 0.35,
               4.15, 4.15, 2.42, 8.38, 9.97, 9.24, 10.01, 2.77, 5.73, 1.77)
  both <- c(centrality_hcc(g)[nodes], escore)
  expect_equal(sum(abs(trunc(both * 100) / 100 - printed) > 1e-9), 6L)
  expect_equal(sum(abs(round(both, 2) - printed) > 1e-9), 1L)
  expect_equal(unname(which.max(escore)), 7L)
})

test_that("hcc_delta spans the source's domain and is refused outside it", {
  g <- igraph::make_star(6, mode = "undirected")
  # delta = 1 makes the extended degree the classical degree, which the
  # source states on page 3.
  expect_equal(.cg_hcc_terms(as.matrix(igraph::as_adjacency_matrix(g)), 1)$kex,
               c(5, 1, 1, 1, 1, 1))
  # delta = 0 drops the node's own degree: every node of a star then has
  # extended degree 5, so one round removes everything.
  expect_equal(.cg_hcc_terms(as.matrix(igraph::as_adjacency_matrix(g)), 0)$kex,
               rep(5, 6))
  expect_equal(unname(centrality_hcc(g, hcc_delta = 0)), rep(2, 6))
  expect_equal(unname(centrality_ehcc(g, hcc_delta = 0)), c(12, rep(4, 5)))
  # The default is the source's 0.5.
  expect_equal(centrality_hcc(g), centrality_hcc(g, hcc_delta = 0.5))
  for (bad in list(-0.1, 1.5, 2, NA_real_, Inf, c(0.5, 0.5), "0.5")) {
    expect_error(centrality_hcc(g, hcc_delta = bad),
                 class = "cograph_bad_parameter")
  }
})

test_that("hcc scores isolates, singletons, edgeless and empty graphs", {
  # An isolate has extended degree zero, the global minimum for every
  # delta in [0, 1], so it always leaves in round one. On an edgeless
  # graph every extended degree is zero, k^ex_max is zero, and the 0/0 of
  # equation (4) is written as zero, leaving pos / pos_max = 1 / 1.
  for (n in 1:4) {
    g <- igraph::make_empty_graph(n, directed = FALSE)
    expect_equal(unname(centrality_hcc(g)), rep(1, n))
    expect_equal(unname(centrality_ehcc(g)), rep(1, n))
    expect_false(any(is.nan(centrality_hcc(g))))
  }
  expect_length(centrality_hcc(igraph::make_empty_graph(0)), 0L)
  expect_length(centrality_ehcc(igraph::make_empty_graph(0)), 0L)
  # An isolate beside a triangle leaves in round one; the triangle then
  # peels in round two, so pos_max is two rather than one.
  g <- igraph::make_graph(c(1, 2, 2, 3, 1, 3), n = 4, directed = FALSE)
  terms <- .cg_hcc_terms(as.matrix(igraph::as_adjacency_matrix(g)))
  expect_equal(terms$kex, c(3, 3, 3, 0))
  expect_equal(terms$pos, c(2, 2, 2, 1))
  expect_equal(unname(centrality_hcc(g)), c(2, 2, 2, 0.5))
  # An isolate scores exactly its own HCC, having no neighbours to add.
  expect_equal(unname(centrality_ehcc(g)), c(6, 6, 6, 0.5))
})

test_that("hcc is not component-local: an extra component moves every score", {
  # k^ex_max and pos_max are single global constants, so a disconnected
  # addition rescales the two terms independently rather than applying one
  # common factor. Adding one isolate to the five-path pushes every
  # position index up by one and the number of rounds from three to four.
  path <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  alone <- unname(centrality_hcc(path))
  padded <- igraph::disjoint_union(path,
                                   igraph::make_empty_graph(1,
                                                            directed = FALSE))
  terms <- .cg_hcc_terms(as.matrix(igraph::as_adjacency_matrix(padded)))
  expect_equal(terms$pos, c(2, 3, 4, 3, 2, 1))
  expect_equal(attr(terms, "pos_max"), 4L)
  after <- unname(centrality_hcc(padded))
  expect_equal(after, c(1.5 / 3 + 2 / 4, 2.5 / 3 + 3 / 4, 1 + 1,
                        2.5 / 3 + 3 / 4, 1.5 / 3 + 2 / 4, 0 + 1 / 4))
  expect_false(isTRUE(all.equal(after[1:5], alone)))
  # Not a common rescaling either: the ratios differ across nodes.
  ratio <- after[1:5] / alone
  expect_false(isTRUE(all.equal(ratio[1], ratio[3])))
})

test_that("hcc terms obey the peel and extended-degree invariants", {
  # The extended degrees sum to delta * 2m + (1 - delta) * sum of squared
  # degrees; every position index from 1 to pos_max is attained because a
  # round removes at least one node; and the extended degree is zero
  # exactly at an isolate.
  set.seed(4620)
  for (seed in 1:5) {
    g <- igraph::sample_gnp(12, 0.3)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    for (delta in c(0, 0.5, 1)) {
      terms <- .cg_hcc_terms(a, delta)
      expect_equal(sum(terms$kex),
                   delta * sum(terms$degree) +
                     (1 - delta) * sum(terms$degree^2))
      expect_equal(sort(unique(terms$pos)),
                   as.numeric(seq_len(attr(terms, "pos_max"))))
      expect_equal(terms$kex == 0, terms$degree == 0)
      expect_true(all(terms$pos[terms$degree == 0] == 1))
      expect_true(all(terms$hcc >= 0 & terms$hcc <= 2))
      expect_equal(terms$ehcc, terms$hcc + as.numeric(a %*% terms$hcc))
    }
  }
})

test_that("hcc input projections and public contracts are explicit", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7)
  igraph::V(g)$name <- c("b", "c", "a")
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 2)
  score <- centrality_hcc(g)
  # The skeleton is the three-node path b - c - a, which is the two-star:
  # the centre scores 2 and the ends 1.25.
  expect_named(score, c("b", "c", "a"))
  expect_equal(unname(score), c(1.25, 2, 1.25))
  expect_equal(centrality(g, measures = "hcc")$hcc, unname(score))
  expect_equal(centrality(g, measures = "ehcc")$ehcc,
               unname(centrality_ehcc(g)))
  expect_equal(centrality_hcc(g, simplify = FALSE, mode = "in", loops = TRUE,
                              invert_weights = TRUE, cutoff = 1), score)
  expect_equal(centrality_hcc(g, weighted = FALSE), score)
  expect_equal(centrality_hcc(a[perm, perm]), score[perm])
  expect_equal(centrality_ehcc(a[perm, perm]), centrality_ehcc(g)[perm])
})

test_that("hcc and ehcc are permutation invariant on random graphs", {
  set.seed(4621)
  for (trial in seq_len(6)) {
    g <- igraph::sample_gnp(10, 0.35)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    perm <- sample.int(10)
    expect_equal(unname(centrality_hcc(a[perm, perm])),
                 unname(centrality_hcc(a))[perm])
    expect_equal(unname(centrality_ehcc(a[perm, perm])),
                 unname(centrality_ehcc(a))[perm])
  }
})

test_that("hcc and ehcc are registered as topology-only measures", {
  meta <- list_centralities()
  for (name in c("hcc", "ehcc")) {
    expect_true(name %in% meta$measure)
    expect_false(meta$uses_weights[meta$measure == name])
    expect_false(meta$mode_aware[meta$measure == name])
    expect_false(meta$costly[meta$measure == name])
    expect_false(meta$needs_membership[meta$measure == name])
    expect_identical(meta$orientation[meta$measure == name], "higher")
    expect_true(name %in% .cg_no_mode_measures())
    expect_false(name %in% .cg_mode_measures())
  }
  expect_identical(names(formals(centrality_hcc)), c("x", "..."))
  expect_identical(names(formals(centrality_ehcc)), c("x", "..."))
  expect_true("hcc_delta" %in% names(formals(centrality)))
  expect_equal(eval(formals(centrality)$hcc_delta), 0.5)
})
