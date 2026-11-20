# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("map equation keeps the paper and Infomap conventions distinct", {
  # Table I, Figure 2 and the closed-form savings are equivalence tests
  # (tests/equivalence/test-centrality-zoo-b-map-equation.R).
  edges <- c(1, 2, 1, 3, 1, 4, 2, 4, 3, 4, 4, 5,
             5, 6, 5, 7, 5, 8, 6, 7)
  g <- igraph::make_graph(edges, n = 8, directed = FALSE)
  groups <- rep(1:2, each = 4)
  paper <- centrality_map_equation(g, membership = groups)
  author <- centrality_map_equation(g, membership = groups,
                                    map_convention = "infomap")
  expect_true(all(paper > author))
  expect_false(isTRUE(all.equal(paper / max(paper), author / max(author))))
  expect_equal(round(unname(author[5]), 2), .21)
  expect_equal(centrality(g, measures = "map_equation", membership = groups)$
                 map_equation, unname(paper))
})

test_that("map equation handles sinks and recorded teleportation", {
  g <- igraph::make_graph(c(1, 2, 2, 3), n = 3)
  # The damping-0 and singleton-module closed forms are equivalence tests.
  # The paper counts an exit symbol for node2, but the terminal node has none.
  score <- centrality_map_equation(g, membership = 1:3)
  expect_equal(unname(score[c(1, 3)]), c(0, 0))
  expect_gt(unname(score[2]), 0)
  for (n in 0:3) {
    empty <- igraph::make_empty_graph(n)
    expect_equal(unname(centrality_map_equation(empty)), numeric(n))
    # Recorded flow on two or more isolates is an equivalence test.
    if (n <= 1) {
      expect_equal(unname(centrality_map_equation(empty, map_flow = "recorded")),
                   numeric(n))
    }
  }
})

test_that("map equation respects weights, labels and fixed leaf partitions", {
  a <- matrix(c(0, 2, 0, 1, 0, 4, 2, 1, 0), 3, 3, byrow = TRUE)
  dimnames(a) <- list(c("a", "b", "c"), c("a", "b", "c"))
  membership <- c(a = "branch/one", b = "branch/one", c = "other/one")
  for (flow in c("recorded", "unrecorded")) {
    for (convention in c("paper", "infomap")) {
      got <- centrality_map_equation(a, membership = membership,
                                     map_flow = flow,
                                     map_convention = convention)
      expect_equal(centrality_map_equation(a * 100, membership = membership,
                                           map_flow = flow,
                                           map_convention = convention), got)
      expect_equal(centrality_map_equation(a, membership = membership[3:1],
                                           map_flow = flow,
                                           map_convention = convention), got)
      expect_equal(centrality_map_equation(a, membership = membership,
                                           map_flow = flow,
                                           map_convention = convention,
                                           normalized = TRUE), got / max(got))
      diag(a) <- 10
      expect_equal(centrality_map_equation(a, membership = membership,
                                           map_flow = flow,
                                           map_convention = convention,
                                           mode = "in", invert_weights = TRUE,
                                           cutoff = 1, loops = TRUE), got)
      diag(a) <- 0
    }
  }
  expect_named(centrality_map_equation(a), colnames(a))
  meta <- list_centralities()
  expect_true(meta$uses_weights[meta$measure == "map_equation"])
  expect_false(meta$mode_aware[meta$measure == "map_equation"])
})

test_that("map equation rejects ambiguous parameters and invalid partitions", {
  g <- igraph::make_ring(3)
  for (d in list(-1, 1, Inf, NaN, NA_real_, numeric(), c(.1, .2), "0.5")) {
    expect_error(centrality_map_equation(g, damping = d), "damping")
  }
  for (m in list(c(1, 2), c(1, NA, 2), matrix(1:3), list(1, 2, 3))) {
    expect_error(centrality_map_equation(g, membership = m), "membership")
  }
  expect_error(centrality_map_equation(g, membership = c(a = 1, b = 1, c = 2)),
               "names")
  expect_error(centrality_map_equation(g, map_flow = "other"), "map_flow")
  expect_error(centrality_map_equation(g, map_convention = "other"),
               "map_convention")
  igraph::E(g)$weight <- c(-1, 1, 1)
  expect_error(centrality_map_equation(g), "nonnegative")
})

test_that("map equation refuses a weight range beyond double precision", {
  # The 1e-300 saving itself is an equivalence test.
  a <- matrix(0, 3, 3)
  a[1, 2] <- 1e100
  a[1, 3] <- 1e-300
  expect_error(centrality_map_equation(a), "weight range")
})
