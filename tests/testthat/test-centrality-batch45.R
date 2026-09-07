test_that("ked has a closed form on stars", {
  # A q-star has n = q + 1. The centre's q neighbours are all leaves of
  # degree one, so its neighbour-degree distribution is uniform, H = 1 and
  # E = 2; its cluster degree is q. A leaf's only neighbour is the centre,
  # so its cluster degree is q too, and a single neighbour means H = 0 and
  # E = 1 by the cograph convention. Both therefore share the same D, and
  # the centre scores exactly 2q times a leaf.
  for (q in 2:7) {
    g <- igraph::make_star(q + 1, mode = "undirected")
    d <- exp(q / (q + 1))
    expect_equal(unname(centrality_ked(g)), c(2 * q * d, rep(d, q)))
  }
  # The two-node star is the single edge: both nodes are leaves, so both
  # take the H = 0 convention and score exp(1 / 2), not 2 exp(1 / 2).
  expect_equal(unname(centrality_ked(igraph::make_star(2, "undirected"))),
               rep(exp(0.5), 2))
})

test_that("ked is constant on regular graphs and depends on the order", {
  # On an r-regular graph every neighbour has degree r, so the distribution
  # is uniform, H = 1 and E = 2, and the cluster degree is r^2.
  for (n in 3:8) {
    r <- n - 1
    expect_equal(unname(centrality_ked(igraph::make_full_graph(n))),
                 rep(2 * r * exp(r^2 / n), n))
  }
  for (n in 3:8) {
    # A ring is two-regular: 2 * 2 * exp(4 / n).
    expect_equal(unname(centrality_ked(igraph::make_ring(n))),
                 rep(4 * exp(4 / n), n))
  }
  expect_equal(unname(centrality_ked(igraph::make_ring(4))),
               rep(4 * exp(1), 4))
})

test_that("ked follows hand-computed entropies on a path", {
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  # Degrees 1, 2, 2, 2, 1; cluster degrees 2, 3, 4, 3, 2; N = 5. The ends
  # have one neighbour, so H = 0 and E = 1. Node 3's two neighbours both
  # have degree two, so its distribution is uniform and E = 2. Nodes 2 and
  # 4 see degrees 1 and 2 against a cluster degree of 3.
  h2 <- -(1 / 3) * log(1 / 3) - (2 / 3) * log(2 / 3)
  e2 <- 1 + h2 / log(2)
  score <- c(1 * 1 * exp(2 / 5), 2 * e2 * exp(3 / 5), 2 * 2 * exp(4 / 5),
             2 * e2 * exp(3 / 5), 1 * 1 * exp(2 / 5))
  expect_equal(unname(centrality_ked(g)), score)
  expect_equal(unname(centrality_ked(g, normalized = TRUE)),
               score / max(score))
  # The middle node wins even though every interior node has degree two.
  expect_equal(which.max(score), 3L)
})

test_that("ked reproduces the two scores printed for the source's fig. 1", {
  # Chen, Xiao, Zeng and Zhang (2014), page 4: "the f_i for the red node is
  # 25.9187 in fig. 1(a) and 19.2212 in fig. 1(b)". Panel (a) is a red node
  # joined to five blue nodes, each carrying three grey leaves; panel (b)
  # joins the same red node to four blue leaves and one blue hub carrying
  # fifteen grey leaves. Both have 21 nodes, and the red node has degree
  # five and cluster degree twenty in both.
  spokes <- c(rbind(rep(1, 5), 2:6))
  panel_a <- igraph::make_graph(c(spokes, rbind(rep(2:6, each = 3), 7:21)),
                                directed = FALSE)
  panel_b <- igraph::make_graph(c(spokes, rbind(rep(6, 15), 7:21)),
                                directed = FALSE)
  expect_equal(igraph::vcount(panel_a), 21L)
  expect_equal(igraph::vcount(panel_b), 21L)
  terms_a <- .cg_ked_terms(as.matrix(igraph::as_adjacency_matrix(panel_a)))
  terms_b <- .cg_ked_terms(as.matrix(igraph::as_adjacency_matrix(panel_b)))
  # The caption's claim: same degree, same cluster degree, different
  # neighbour-degree distribution.
  expect_equal(terms_a$degree[1], 5)
  expect_equal(terms_b$degree[1], 5)
  expect_equal(terms_a$cluster_degree[1], 20)
  expect_equal(terms_b$cluster_degree[1], 20)
  expect_equal(terms_a$diversity[1], 1)
  expect_lt(terms_b$diversity[1], 1)
  expect_equal(round(centrality_ked(panel_a)[[1]], 4), 25.9187)
  expect_equal(round(centrality_ked(panel_b)[[1]], 4), 19.2212)
})

test_that("ked is not the Centrality Zoo's formula", {
  # Zoo section 2.215 drops the 1 + from E_i and divides the cluster degree
  # by max_l K_l instead of by N. On the source's own panel (a), where
  # H = 1, dropping the 1 + halves the score exactly.
  spokes <- c(rbind(rep(1, 5), 2:6))
  panel_a <- igraph::make_graph(c(spokes, rbind(rep(2:6, each = 3), 7:21)),
                                directed = FALSE)
  terms <- .cg_ked_terms(as.matrix(igraph::as_adjacency_matrix(panel_a)))
  zoo <- terms$degree[1] * terms$diversity[1] *
    exp(terms$cluster_degree[1] / max(terms$cluster_degree))
  expect_equal(round(zoo, 4), 13.5914)
  expect_false(isTRUE(all.equal(zoo, 25.9187, tolerance = 1e-4)))
  # cograph exposes no alpha or beta: the source's equation (6) is a bare
  # product and the exponents appear only in the Zoo.
  expect_identical(names(formals(centrality_ked)), c("x", "..."))
})

test_that("ked changes with the whole graph's order, ranking included", {
  ring <- igraph::make_ring(5)
  alone <- unname(centrality_ked(ring))
  expect_equal(alone, rep(4 * exp(4 / 5), 5))
  # N is the whole graph's order, so an isolate changes every score.
  with_isolate <- igraph::disjoint_union(
    ring, igraph::make_empty_graph(1, directed = FALSE)
  )
  expect_equal(unname(centrality_ked(with_isolate)),
               c(rep(4 * exp(4 / 6), 5), 0))
  expect_false(isTRUE(all.equal(unname(centrality_ked(with_isolate))[1:5],
                                alone)))
  # Unlike a plain rescaling, the change can reorder nodes: exp(K / N)
  # shrinks a large cluster degree more than a small one. On this
  # seven-node graph nodes 3 and 7 swap when one isolate is added.
  g <- igraph::make_graph(c(1, 4, 3, 4, 2, 5, 3, 5, 4, 5, 5, 6,
                            1, 7, 2, 7, 3, 7, 6, 7), directed = FALSE)
  before <- centrality_ked(g)
  padded <- igraph::disjoint_union(g,
                                   igraph::make_empty_graph(1,
                                                            directed = FALSE))
  after <- centrality_ked(padded)
  expect_gt(before[[3]], before[[7]])
  expect_lt(after[[3]], after[[7]])
})

test_that("ked scores isolates, singletons and empty graphs explicitly", {
  # A degree-zero node has both an empty entropy and an empty normaliser;
  # cograph writes H = 0, and the score is zero whatever finite E is used
  # because the degree multiplies the product.
  for (n in 0:4) {
    g <- igraph::make_empty_graph(n, directed = FALSE)
    expect_equal(unname(centrality_ked(g)), numeric(n))
  }
  singleton <- centrality_ked(igraph::make_empty_graph(1, directed = FALSE))
  expect_equal(unname(singleton), 0)
  expect_false(is.nan(singleton))
  expect_length(centrality_ked(igraph::make_empty_graph(0)), 0L)
  # An isolate beside an edge: N is three, so the edge's nodes score
  # exp(1 / 3) rather than the exp(1 / 2) they score alone.
  g <- igraph::make_graph(c(1, 2), n = 3, directed = FALSE)
  expect_equal(unname(centrality_ked(g)), c(exp(1 / 3), exp(1 / 3), 0))
  expect_true(all(is.finite(centrality_ked(g))))
})

test_that("ked terms obey the entropy and cluster-degree invariants", {
  # The cluster degrees sum to the sum of squared degrees, each degree
  # counted once per neighbour; and H lies in [0, 1] with H = 1 exactly
  # when a node's neighbour degrees are all equal.
  for (seed in 1:5) {
    set.seed(seed)
    g <- igraph::sample_gnp(12, 0.3)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    terms <- .cg_ked_terms(a)
    expect_equal(sum(terms$cluster_degree), sum(terms$degree^2))
    expect_true(all(terms$diversity >= 0))
    expect_true(all(terms$diversity <= 1 + 1e-12))
    expect_equal(terms$ked == 0, terms$degree == 0)
    plural <- terms$degree > 1
    uniform <- vapply(seq_len(12), function(i) {
      nbrs <- which(a[i, ] > 0)
      length(unique(terms$degree[nbrs])) == 1L
    }, logical(1))
    expect_equal(abs(terms$diversity[plural] - 1) < 1e-12, uniform[plural])
    expect_equal(terms$ked, terms$degree * terms$e_factor * terms$d_factor)
  }
})

test_that("ked's normalised entropy does not depend on the logarithm base", {
  # Equation (2) divides the entropy by log(k_i) in the same base, so the
  # base cancels. Recomputing H in base ten must give the same numbers.
  set.seed(4520)
  for (trial in seq_len(4)) {
    g <- igraph::sample_gnp(10, 0.4)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    terms <- .cg_ked_terms(a)
    plural <- which(terms$degree > 1)
    base_ten <- vapply(plural, function(i) {
      p <- terms$degree[a[i, ] > 0] / terms$cluster_degree[i]
      sum(-p * log10(p)) / log10(terms$degree[i])
    }, numeric(1))
    expect_equal(terms$diversity[plural], base_ten, tolerance = 1e-12)
  }
})

test_that("ked raises an error instead of returning an infinite score", {
  # K_i / N is at most (n - 1)^2 / n, so exp() can only overflow on a dense
  # graph of about 710 vertices or more. A complete graph on 750 vertices
  # has an exponent of 749^2 / 750, well past the double-precision range.
  n <- 750L
  dense <- matrix(1, n, n)
  diag(dense) <- 0
  expect_error(.cg_ked_terms(dense), "overflow")
  # One vertex fewer than the threshold still returns finite scores.
  small <- matrix(1, 600L, 600L)
  diag(small) <- 0
  expect_true(all(is.finite(.cg_ked_terms(small)$ked)))
})

test_that("ked input projections and public contracts are explicit", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7)
  igraph::V(g)$name <- c("b", "c", "a")
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 2)
  score <- centrality_ked(g)
  # The skeleton is the three-node path b - c - a: degrees 1, 2, 1, cluster
  # degrees 2, 2, 2 and N = 3, so the ends score exp(2 / 3) and the middle
  # 2 * 2 * exp(2 / 3), its neighbours' degrees being equal.
  expect_named(score, c("b", "c", "a"))
  expect_equal(unname(score), c(exp(2 / 3), 4 * exp(2 / 3), exp(2 / 3)))
  expect_equal(centrality(g, measures = "ked")$ked, unname(score))
  expect_equal(centrality_ked(g, simplify = FALSE, mode = "in", loops = TRUE,
                              invert_weights = TRUE, cutoff = 1), score)
  expect_equal(centrality_ked(g, weighted = FALSE), score)
  expect_equal(centrality_ked(a[perm, perm]), score[perm])
})

test_that("ked is permutation invariant on random graphs", {
  set.seed(4521)
  for (trial in seq_len(6)) {
    g <- igraph::sample_gnp(10, 0.35)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    perm <- sample.int(10)
    expect_equal(unname(centrality_ked(a[perm, perm])),
                 unname(centrality_ked(a))[perm])
  }
})

test_that("ked is registered as a parameter-free topology-only measure", {
  meta <- list_centralities()
  expect_true("ked" %in% meta$measure)
  expect_false(meta$uses_weights[meta$measure == "ked"])
  expect_false(meta$mode_aware[meta$measure == "ked"])
  expect_false(meta$costly[meta$measure == "ked"])
  expect_false(meta$needs_membership[meta$measure == "ked"])
  expect_identical(meta$orientation[meta$measure == "ked"], "higher")
  expect_true("ked" %in% .cg_no_mode_measures())
  expect_false("ked" %in% .cg_mode_measures())
})
