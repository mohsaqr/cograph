test_that("finite diffusion follows outgoing walks with q and T", {
  a <- matrix(0, 3, 3, dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  a[1, 2] <- 0.5
  a[2, 3] <- 0.25
  got <- centrality_diffusion_centrality(a, diffusion_q = 0.5,
                                         diffusion_steps = 2)
  expect_equal(unname(got), c(0.28125, 0.125, 0))
  expect_identical(names(got), c("C", "A", "B"))
  incoming <- centrality_diffusion_centrality(t(a), diffusion_q = 0.5,
                                              diffusion_steps = 2)
  expect_equal(unname(incoming), c(0, 0.25, 0.15625))
  expect_equal(unname(centrality_diffusion_centrality(a, diffusion_steps = 1)),
               rowSums(unname(a)))
  expect_equal(unname(centrality_diffusion_centrality(a, diffusion_steps = 0)),
               numeric(3))
  expect_equal(unname(centrality_diffusion_centrality(a, diffusion_q = 0)),
               numeric(3))
  # Walks may return to their source, including on a self-loop.
  expect_equal(unname(centrality_diffusion_centrality(matrix(0.5, 1, 1))),
               0.875)
  expect_equal(unname(centrality_diffusion_centrality(matrix(0.5, 1, 1),
                                                      loops = FALSE)), 0)
  expect_equal(unname(centrality_diffusion_centrality(matrix(0.5, 1, 1),
                                                      weighted = FALSE)), 3)
})

test_that("finite diffusion honours weights and parallel-edge conventions", {
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3), directed = TRUE)
  igraph::E(g)$weight <- c(0.2, 0.4, 0.5)
  value <- function(...) {
    unname(centrality_diffusion_centrality(g, diffusion_steps = 1, ...))
  }
  expect_equal(value(), c(0.6, 0.5, 0))
  expect_equal(value(simplify = FALSE), c(0.6, 0.5, 0))
  expect_equal(value(simplify = "mean"), c(0.3, 0.5, 0))
  expect_equal(value(weighted = FALSE), c(1, 1, 0))
  expect_equal(value(weighted = FALSE, simplify = FALSE), c(2, 1, 0))
  expect_equal(value(invert_weights = TRUE, mode = "in", lambda = 99,
                     diffusion_method = "power_series"), value())
  tab <- list_centralities()
  expect_true(tab$uses_weights[tab$measure == "diffusion_centrality"])
  expect_false(tab$mode_aware[tab$measure == "diffusion_centrality"])
  expect_false(tab$costly[tab$measure == "diffusion_centrality"])
  expect_equal(unname(centrality_diffusion_centrality(g, normalized = TRUE)),
               c(1, 0.5 / 0.9, 0))
})

test_that("finite diffusion validates parameters, weights and precision", {
  g <- igraph::make_ring(3)
  for (bad in list(NULL, NA_real_, NaN, Inf, -1, 1.1, "1", c(0, 1))) {
    expect_error(centrality_diffusion_centrality(g, diffusion_q = bad),
                 "diffusion_q")
  }
  for (bad in list(NULL, NA_real_, Inf, -1, 1.1, "1", c(0, 1), 2^31)) {
    expect_error(centrality_diffusion_centrality(g, diffusion_steps = bad),
                 "diffusion_steps")
  }
  for (bad in c(-1, NA_real_, Inf, NaN)) {
    igraph::E(g)$weight <- c(bad, 1, 1)
    expect_error(centrality_diffusion_centrality(g), "nonnegative edge weights")
  }
  huge <- matrix(1e200, 1, 1)
  expect_error(centrality_diffusion_centrality(huge), "double precision")
  expect_error(centrality_diffusion_centrality(huge, normalized = TRUE),
               "double precision")
  for (n in 0:3) {
    empty <- igraph::make_empty_graph(n)
    expect_equal(unname(centrality_diffusion_centrality(empty)), numeric(n))
  }
})

test_that("finite diffusion differs from degree and includes the TNA sum", {
  g <- igraph::make_ring(4)
  expect_equal(unname(centrality_diffusion_centrality(g)), rep(14, 4))
  expect_equal(unname(centrality_diffusion(g)), rep(6, 4))
  tna <- centrality_diffusion(g, diffusion_method = "power_series")
  expect_equal(unname(centrality_diffusion_centrality(g, diffusion_steps = 4)),
               unname(tna))
})

test_that("dynamical importance measures actual spectral loss", {
  # A clique's exact deletion loss is 1/(n-1); the perturbation gives 1/n.
  clique <- igraph::make_full_graph(4)
  expect_equal(unname(centrality_dynamical_importance(clique)), rep(1 / 3, 4))
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_dynamical_importance(star)),
               c(1, rep(1 - sqrt(3) / 2, 4)))
  cycle <- igraph::make_ring(4, directed = TRUE)
  expect_equal(unname(centrality_dynamical_importance(cycle)), rep(1, 4))
  # An equally strong second component keeps the global radius unchanged.
  twin <- igraph::disjoint_union(igraph::make_full_graph(3),
                                 igraph::make_full_graph(3))
  expect_equal(unname(centrality_dynamical_importance(twin)), numeric(6))
  # Two reciprocal dyads linked in one direction: repeated eigenvalue 1.
  # Each deletion leaves at least one dyad, so every exact loss is zero.
  linked <- igraph::make_graph(c(1, 3, 3, 1, 2, 4, 4, 2, 2, 3,
                                 1, 5, 2, 5, 4, 5, 6, 3), directed = TRUE)
  expect_equal(unname(centrality_dynamical_importance(linked)), numeric(6))
  isolate <- igraph::disjoint_union(igraph::make_full_graph(3),
                                    igraph::make_empty_graph(1, FALSE))
  expect_equal(unname(centrality_dynamical_importance(isolate)),
               c(0.5, 0.5, 0.5, 0))
  empty <- igraph::make_empty_graph(0)
  expect_equal(unname(centrality_dynamical_importance(empty)), numeric(0))
  for (n in 1:4) {
    dag <- igraph::make_tree(n, children = 1, mode = "out")
    expect_true(all(is.nan(centrality_dynamical_importance(dag))))
    expect_no_warning(centrality_dynamical_importance(dag, normalized = TRUE))
  }
})

test_that("dynamical importance preserves weights and removes loops", {
  a <- matrix(c(0, 0.5, 0, 2, 0, 0.25, 0, 1, 0), 3, 3,
              dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  # Two weighted reciprocal dyads give rho^2 = 1 + 0.25.
  expected <- c(1 - sqrt(0.25 / 1.25), 1, 1 - sqrt(1 / 1.25))
  got <- centrality_dynamical_importance(a)
  expect_equal(unname(got), expected)
  expect_identical(names(got), rownames(a))
  expect_equal(centrality_dynamical_importance(t(a)), got)
  expect_equal(centrality_dynamical_importance(a * 7), got)
  diag(a) <- 10
  expect_equal(centrality_dynamical_importance(a), got)
  expect_equal(centrality_dynamical_importance(a, loops = FALSE), got)
  expect_equal(centrality_dynamical_importance(a, mode = "in",
                                               invert_weights = TRUE), got)
  a[1, 2] <- -1
  expect_error(centrality_dynamical_importance(a), "nonnegative edge weights")
  meta <- list_centralities()
  expect_true(meta$costly[meta$measure == "dynamical_importance"])
  expect_true(meta$uses_weights[meta$measure == "dynamical_importance"])
  expect_false(meta$mode_aware[meta$measure == "dynamical_importance"])
})
