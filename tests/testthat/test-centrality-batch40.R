test_that("dkgm masses combine degree, shell and removal stage on stars", {
  # Leaves leave in stage one and the centre in stage two, so q(1) = 2 and
  # the improved shell denominator is three.
  for (q in c(2:6, 20)) {
    g <- igraph::make_star(q + 1, mode = "undirected")
    leaf <- 7 / 3
    centre <- q + 5 / 3
    expect_equal(unname(centrality_dkgm(g, dkgm_radius = 1)),
                 c(q * centre * leaf, rep(leaf * centre, q)))
    expect_equal(unname(centrality_dkgm(g)),
                 c(q * centre * leaf,
                   rep(leaf * centre + (q - 1) * leaf^2 / 4, q)))
  }
})

test_that("dkgm reproduces the published nine-node example", {
  edges <- c(1, 2, 2, 3, 2, 7, 3, 4, 3, 7, 4, 5, 4, 6, 4, 7,
             5, 6, 5, 7, 5, 8, 6, 7, 6, 9)
  g <- igraph::make_graph(edges, directed = FALSE)
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  peel <- .cg_shell_stage(a)
  # Li and Huang (2021) Tables 1-4.
  expect_equal(peel$shell, c(1L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L))
  expect_equal(peel$stage, c(1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L))
  expect_identical(peel$max_stage, 2L)
  expect_equal(round(peel$shell + peel$stage / (peel$max_stage + 1), 4),
               c(1.3333, 2.3333, 2.6667, rep(3.3333, 4), 1.3333, 1.3333))
  expect_equal(round(.cg_dk_index(a), 4),
               c(2.3333, 5.3333, 5.6667, rep(7.3333, 3), 8.3333,
                 2.3333, 2.3333))
  # Table 5, R = 2.
  expect_equal(round(unname(centrality_dkgm(g)), 2),
               c(20.61, 116.44, 143.08, 228.56, 210.22, 210.22, 289.58,
                 30.53, 30.53))
})

test_that("dkgm follows shell stages and distance shells on a path", {
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  # Ends leave in stage one, their neighbours in stage two and the centre in
  # stage three, so the denominator is four and DK is 9/4, 7/2, 15/4, 7/2, 9/4.
  expect_equal(.cg_dk_index(as.matrix(igraph::as_adjacency_matrix(g))),
               c(9 / 4, 7 / 2, 15 / 4, 7 / 2, 9 / 4))
  radius_two <- c(639 / 64, 385 / 16, 30.46875, 385 / 16, 639 / 64)
  expect_equal(unname(centrality_dkgm(g)), radius_two)
  expect_equal(unname(centrality_dkgm(g, dkgm_radius = 1)),
               c(63 / 8, 21, 26.25, 21, 63 / 8))
  expect_equal(unname(centrality_dkgm(g, normalized = TRUE)),
               radius_two / max(radius_two))
  expect_equal(centrality_dkgm(g, dkgm_radius = 1.9),
               centrality_dkgm(g, dkgm_radius = 1))
  expect_equal(centrality_dkgm(g, dkgm_radius = NULL),
               centrality_dkgm(g, dkgm_radius = Inf))
  expect_gt(centrality_dkgm(g, dkgm_radius = Inf)[1], radius_two[1])
  # Mean finite positive hop distance is two, so the paper's R = <d>/2 is one.
  expect_equal(centrality_dkgm(g, dkgm_radius = "auto"),
               centrality_dkgm(g, dkgm_radius = 1))
})

test_that("dkgm agrees with regular graphs and empty sums", {
  for (n in c(2, 3, 5, 8)) {
    # Every node has shell n-1, stage one and distance one to the rest.
    expect_equal(unname(centrality_dkgm(igraph::make_full_graph(n))),
                 rep((n - 1) * (2 * n - 1.5)^2, n))
  }
  for (n in 0:3) {
    expect_equal(unname(centrality_dkgm(
      igraph::make_empty_graph(n, directed = FALSE)
    )), numeric(n))
  }
  g <- igraph::make_ring(7)
  expect_equal(unname(centrality_dkgm(g, dkgm_radius = 0)), numeric(7))
  expect_equal(unname(centrality_dkgm(g, dkgm_radius = .5)), numeric(7))
  expect_equal(unname(centrality_dkgm(g, dkgm_radius = 1)), rep(2 * 4.5^2, 7))
  expect_equal(unname(centrality_dkgm(g)), rep(2 * 4.5^2 * 1.25, 7))
})

test_that("dkgm shares one global stage denominator across components", {
  ring <- igraph::make_ring(5)
  alone <- unname(centrality_dkgm(ring))
  expect_equal(alone, rep(50.625, 5))
  # An isolate leaves in the first stage of the one-shell, so q stays one.
  with_isolate <- igraph::disjoint_union(
    ring, igraph::make_empty_graph(1, directed = FALSE)
  )
  expect_equal(unname(centrality_dkgm(with_isolate)), c(alone, 0))
  # A five-node path needs three stages, which lengthens the denominator for
  # every node in the graph, so the ring's raw scores change.
  with_path <- igraph::disjoint_union(
    ring, igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
  )
  expect_equal(unname(centrality_dkgm(with_path))[1:5], rep(45.15625, 5))
  expect_false(isTRUE(all.equal(unname(centrality_dkgm(with_path))[1:5],
                                alone)))
})

test_that("dkgm peeling follows the at-most-k reading", {
  # Strict equality would leave the centre of a three-node path at degree
  # zero and never remove it; the paper's stage loop ends at degree > k.
  a <- matrix(0, 3, 3)
  a[cbind(c(1, 2, 2, 3), c(2, 1, 3, 2))] <- 1
  peel <- .cg_shell_stage(a)
  expect_equal(peel$shell, c(1L, 1L, 1L))
  expect_equal(peel$stage, c(1L, 2L, 1L))
  expect_identical(peel$max_stage, 2L)
  # An isolate falls in the one-shell, not the zero-shell coreness reports.
  isolated <- .cg_shell_stage(matrix(0, 2, 2))
  expect_equal(isolated$shell, c(1L, 1L))
  expect_equal(.cg_dk_index(matrix(0, 2, 2)), c(1.5, 1.5))
  expect_equal(unname(centrality(matrix(0, 2, 2),
                                 measures = "coreness")$coreness_all),
               c(0, 0))
})

test_that("dkgm input projections and public contracts are explicit", {
  g <- igraph::make_graph(c(1, 2, 2, 1, 2, 3, 2, 3, 3, 3), directed = TRUE)
  igraph::E(g)$weight <- c(2, 8, 3, 9, 7)
  igraph::V(g)$name <- c("b", "c", "a")
  a <- as.matrix(igraph::as_adjacency_matrix(g, attr = "weight"))
  perm <- c(3, 1, 2)
  score <- centrality_dkgm(g)
  expect_named(score, c("b", "c", "a"))
  expect_equal(centrality(g, measures = "dkgm")$dkgm, unname(score))
  expect_equal(centrality_dkgm(g, simplify = FALSE, mode = "in", loops = TRUE,
                               invert_weights = TRUE, cutoff = 1,
                               gravity_mass = "degree"), score)
  expect_equal(centrality_dkgm(g, weighted = FALSE), score)
  expect_equal(centrality_dkgm(a[perm, perm]), score[perm])
  for (radius in list(-1, NA_real_, NaN, "3", TRUE, 1:2, numeric())) {
    expect_error(centrality_dkgm(g, dkgm_radius = radius), "dkgm_radius")
  }
  expect_error(centrality_dkgm(igraph::make_empty_graph(0), dkgm_radius = -1),
               "dkgm_radius")
  meta <- list_centralities()
  expect_false(meta$uses_weights[meta$measure == "dkgm"])
  expect_false(meta$mode_aware[meta$measure == "dkgm"])
  expect_false(meta$costly[meta$measure == "dkgm"])
  expect_true("dkgm" %in% .cg_no_mode_measures())
})
