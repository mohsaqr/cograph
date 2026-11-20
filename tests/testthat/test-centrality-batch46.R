# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

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
  score <- unname(centrality_hcc(g))
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

test_that("hcc terms reproduce tables 1 and 2 of the source's figure 1", {
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
  # Table 3 and the worked line HCC(a) = 4.5/11 + 4/6 are compared in
  # tests/equivalence (zoo-b-hybrid-characteristic). The worked line
  # EHCC(a) = HCC(a) + HCC(b) + HCC(e) is the definition, checked here.
  score <- centrality_hcc(g)[nodes]
  escore <- centrality_ehcc(g)[nodes]
  expect_equal(unname(escore[["a"]]),
               unname(score[["a"]] + score[["b"]] + score[["e"]]))
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
