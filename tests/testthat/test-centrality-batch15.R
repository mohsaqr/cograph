test_that("DS uses both spreading and recovery, excluding the initial seed", {
  edge <- igraph::make_full_graph(2)
  # On a regular graph d, each term is beta*d*(beta*d+1-mu)^r.
  recovery_score <- function(mu) {
    result <- centrality_dynamics_sensitive(
      edge, ds_beta = 0.5, ds_mu = mu, ds_steps = 3
    )
    unname(result)
  }
  expect_equal(recovery_score(1), rep(0.875, 2))
  expect_equal(recovery_score(0.5), rep(1.5, 2))
  expect_equal(recovery_score(0), rep(2.375, 2))
  expect_equal(unname(centrality_dynamics_sensitive(edge, ds_steps = 0)),
               numeric(2))
  expect_equal(unname(centrality_dynamics_sensitive(edge, ds_beta = 0)),
               numeric(2))
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_dynamics_sensitive(star, ds_steps = 1)),
               c(0.4, rep(0.1, 4)))
  two_steps <- centrality_dynamics_sensitive(
    star, ds_beta = 0.5, ds_mu = 0.5, ds_steps = 2
  )
  expect_equal(unname(two_steps), c(4, rep(1.75, 4)))
})

test_that("DS validates the full parameter family and reports overflow", {
  g <- igraph::make_ring(3)
  for (bad in list(NULL, NA_real_, Inf, -1, 1.1, "0.1", c(0, 1))) {
    expect_error(centrality_dynamics_sensitive(g, ds_beta = bad), "ds_beta")
    expect_error(centrality_dynamics_sensitive(g, ds_mu = bad), "ds_mu")
  }
  for (bad in list(NULL, NA_real_, Inf, -1, 1.1, "5", c(1, 2), 2^31)) {
    expect_error(centrality_dynamics_sensitive(g, ds_steps = bad), "ds_steps")
  }
  expect_error(
    centrality_dynamics_sensitive(g, ds_beta = 1, ds_mu = 0, ds_steps = 1000),
    "double precision"
  )
})

test_that("Malatya has a definitional reciprocal relationship to bridging", {
  star <- igraph::make_star(5, mode = "undirected")
  expect_equal(unname(centrality_malatya(star)), c(16, rep(0.25, 4)))
  clique <- igraph::make_full_graph(5)
  expect_equal(unname(centrality_malatya(clique)), rep(4, 5))
  g <- igraph::make_graph("Zachary")
  expect_equal(unname(centrality_malatya(g)),
               1 / unname(centrality_bridging_coefficient(g)))
})

test_that("batch15 measures preserve labels and their projection conventions", {
  a <- matrix(c(3, 0, 0, 2, 4, 0, 0, 5, 6), 3, 3,
              dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  b <- (a != 0 | t(a) != 0) * 1
  diag(b) <- 0
  measures <- c("dynamics_sensitive", "malatya")
  actual <- centrality(a, measures = measures, directed = TRUE)
  expect_equal(actual, centrality(b, measures = measures))
  expect_identical(actual$node, rownames(a))
  normalized <- centrality(a, measures = measures, normalized = TRUE)
  for (m in measures) {
    expect_equal(normalized[[m]], actual[[m]] / max(actual[[m]]))
    for (n in 0:3) {
      expect_equal(centrality(igraph::make_empty_graph(n), measures = m)[[m]],
                   numeric(n))
    }
  }
  multi <- igraph::make_graph(c(1, 2, 1, 2, 2, 1, 2, 3, 3, 3))
  expect_equal(centrality(multi, measures = measures)[-1], actual[-1])
  meta <- list_centralities()
  expect_false(any(meta$mode_aware[meta$measure %in% measures]))
  expect_false(any(meta$uses_weights[meta$measure %in% measures]))
  expect_false(any(meta$costly[meta$measure %in% measures]))
})
