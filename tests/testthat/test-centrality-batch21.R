# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("global structure models agree with analytical graphs", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_global_structure(star)),
               exp(1 / 5) * c(4, rep(2.5, 4)))
  self <- exp(c(4, 1, 1, 1, 1) / 5)
  expected <- c(4 * self[1] * self[2],
                rep(self[2] * (self[1] + 1.5 * self[2]), 4))
  expect_equal(unname(centrality_hybrid_global_structure(star)), expected)
  ring <- igraph::make_ring(4)
  expect_equal(unname(centrality_global_structure(ring)), rep(5 * exp(0.5), 4))
  expect_equal(unname(centrality_hybrid_global_structure(ring)),
               rep(2.25 * exp(2), 4))
  clique <- igraph::make_full_graph(4)
  score <- centrality_global_structure(clique)
  expect_equal(unname(score), rep(9 * exp(0.75), 4))
  expect_equal(unname(centrality_hybrid_global_structure(clique)),
               rep(3 * exp(4.5), 4))
  path <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  expect_equal(unname(centrality_global_structure(path)[1]), 25 / 12 * exp(0.2))
  expect_equal(centrality_global_structure(path, cutoff = 1),
               centrality_global_structure(path))
})

test_that("constructed graphs reproduce the published focal calculations", {
  # Constructed from the supplied core/degree/distance information;
  # these are not claimed to be visually transcribed original figures.
  g <- igraph::make_graph(c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4,
                            4, 7, 4, 8, 7, 8, 5, 7, 5, 8, 5, 6,
                            7, 9, 4, 10, 10, 11, 10, 12, 10, 13),
                          directed = FALSE)
  core <- c(3, 3, 3, 3, 2, 1, 2, 2, 1, 1, 1, 1, 1)
  expect_equal(unname(igraph::coreness(g)), core)
  expect_equal(unname(igraph::distances(g, v = 4)[1, -4]),
               c(1, 1, 1, 2, 3, 1, 1, 2, 1, 2, 2, 2))
  expect_equal(unname(round(centrality_global_structure(g)[4], 3)), 21.833)
  # Original labels 0..6 are shifted to R vertex IDs 1..7.
  h <- igraph::make_graph(c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4,
                            2, 5, 3, 6, 4, 6, 4, 7), directed = FALSE)
  expect_equal(unname(igraph::coreness(h)), c(3, 3, 3, 3, 1, 2, 1))
  expect_equal(unname(igraph::degree(h)), c(3, 4, 4, 5, 1, 2, 1))
  focal <- unname(centrality_hybrid_global_structure(h)[4])
  # The source calculation rounds intermediate exponentials to six decimals.
  expect_lt(abs(focal - 152.877133), 1e-4)
})

test_that("global structure conventions include global size and isolates", {
  functions <- list(centrality_global_structure,
                    centrality_improved_global_structure,
                    centrality_hybrid_global_structure)
  for (fun in functions) {
    for (n in 0:3) {
      g <- igraph::make_empty_graph(n)
      expect_equal(unname(fun(g)), numeric(n))
      expect_equal(unname(fun(g, normalized = TRUE)), numeric(n))
    }
    g <- igraph::make_ring(4)
    with_isolate <- igraph::add_vertices(g, 1)
    expect_equal(unname(fun(with_isolate)[5]), 0)
    changed <- unname(fun(with_isolate)[1:4])
    expect_false(isTRUE(all.equal(changed, unname(fun(g)))))
  }
  edge <- igraph::make_graph(c(1, 2), n = 3, directed = FALSE)
  score <- centrality_global_structure(edge)
  expect_equal(unname(score), c(exp(1 / 3), exp(1 / 3), 0))
  expect_equal(unname(centrality_hybrid_global_structure(edge)),
               c(exp(2 / 3), exp(2 / 3), 0))
})

test_that("global structure projections, labels and metadata are consistent", {
  a <- as.matrix(igraph::as_adjacency_matrix(igraph::make_ring(5)))
  labels <- c("C", "A", "E", "D", "B")
  dimnames(a) <- list(labels, labels)
  arcs <- a
  arcs[lower.tri(arcs)] <- 0
  arcs[arcs > 0] <- seq_len(sum(arcs > 0))
  diag(arcs) <- 7
  perm <- c(5, 2, 1, 4, 3)
  functions <- list(centrality_global_structure,
                    centrality_improved_global_structure,
                    centrality_hybrid_global_structure)
  for (fun in functions) {
    expected <- fun(a)
    expect_identical(names(expected), labels)
    projected <- fun(arcs, directed = TRUE, loops = TRUE, mode = "in",
                     invert_weights = TRUE)
    expect_equal(projected, expected)
    expect_equal(fun(a[perm, perm]), expected[perm])
    expect_equal(fun(a, normalized = TRUE), expected / max(expected))
    g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3), directed = FALSE)
    expect_equal(fun(g, simplify = FALSE), fun(igraph::simplify(g)))
  }
  meta <- list_centralities()
  keys <- c("global_structure", "hybrid_global_structure",
            "improved_global_structure")
  expect_false(any(meta$uses_weights[meta$measure %in% keys]))
  expect_false(any(meta$mode_aware[meta$measure %in% keys]))
})

test_that("IGSM retains the mean-degree ceiling and its sparse extensions", {
  star <- igraph::make_star(5, mode = "undirected")
  expected <- c(4 * exp(0.8), rep(5.5 * exp(0.2), 4))
  expect_equal(unname(centrality_improved_global_structure(star)), expected)
  ring <- igraph::make_ring(4)
  expect_equal(unname(centrality_improved_global_structure(ring)),
               rep(5 * exp(0.5), 4))
  # Mean degree=0.4, ceil(log2(0.4))=-1: distance two multiplies by two.
  path <- igraph::make_graph(c(1, 2, 2, 3), n = 10, directed = FALSE)
  expected <- c(4 * exp(0.1), 2 * exp(0.2), 4 * exp(0.1), rep(0, 7))
  expect_equal(unname(centrality_improved_global_structure(path)), expected)
  # Mean degree=0.8 has exponent zero, including all reachable degrees.
  path <- igraph::delete_vertices(path, 6:10)
  expected <- c(3 * exp(0.2), 2 * exp(0.4), 3 * exp(0.2), 0, 0)
  expect_equal(unname(centrality_improved_global_structure(path)), expected)
})
