# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

test_that("map equation keeps the paper and Infomap conventions distinct", {
  edges <- c(1, 2, 1, 3, 1, 4, 2, 4, 3, 4, 4, 5,
             5, 6, 5, 7, 5, 8, 6, 7)
  g <- igraph::make_graph(edges, n = 8, directed = FALSE)
  partitions <- list(rep(1, 8), c(rep(1, 3), rep(2, 5)), rep(1:2, each = 4))
  printed <- cbind(c(.20, .14, .14, .26, .26, .14, .14, .07),
                   c(.16, .12, .12, .24, .24, .13, .13, .07),
                   c(.184, .130, .130, .228, .212, .127, .127, .068))
  p <- c(.15, .10, .10, .20, .20, .10, .10, .05)
  # TableI is the author visit-only convention, despite eq2/11 including exits.
  for (j in 1:3) {
    got <- centrality_map_equation(g, membership = partitions[[j]],
                                   map_convention = "infomap")
    expect_equal(round(unname(got), if (j == 3) 3 else 2), printed[, j])
  }
  expect_equal(unname(centrality_map_equation(g)), -(1 - p) * log2(1 - p))
  groups <- partitions[[3]]
  paper <- centrality_map_equation(g, membership = groups)
  author <- centrality_map_equation(g, membership = groups,
                                    map_convention = "infomap")
  # The bridge carries flow1/20, so codebook rates are .60 and .50.
  s <- c(rep(.60, 4), rep(.50, 4))
  expect_equal(unname(paper), -(s - p) * log2((s - p) / s))
  expect_true(all(paper > author))
  expect_false(isTRUE(all.equal(paper / max(paper), author / max(author))))
  # Figure2 rounds the complete silenced-code difference at node5 to .22.
  expect_equal(round(unname(paper[5]), 2), .22)
  expect_equal(round(unname(author[5]), 2), .21)
  expect_equal(centrality(g, measures = "map_equation", membership = groups)$
                 map_equation, unname(paper))
})

test_that("map equation handles sinks and recorded teleportation", {
  g <- igraph::make_graph(c(1, 2, 2, 3), n = 3)
  # At damping0, recorded node visits are uniform, including isolated sources.
  expect_equal(unname(centrality_map_equation(g, map_flow = "recorded",
                                              damping = 0)),
               rep(-2 / 3 * log2(2 / 3), 3))
  # Unrecorded link teleportation samples the two links equally at damping0.
  expect_equal(unname(centrality_map_equation(g, damping = 0)), c(0, .5, .5))
  # With each node in its own codebook, visit-only centrality is identically0.
  expect_equal(unname(centrality_map_equation(g, membership = 1:3,
                                              map_convention = "infomap")),
               rep(0, 3))
  # The paper counts an exit symbol for node2, but the terminal node has none.
  score <- centrality_map_equation(g, membership = 1:3)
  expect_equal(unname(score[c(1, 3)]), c(0, 0))
  expect_gt(unname(score[2]), 0)
  for (n in 0:3) {
    empty <- igraph::make_empty_graph(n)
    expect_equal(unname(centrality_map_equation(empty)), numeric(n))
    recorded <- centrality_map_equation(empty, map_flow = "recorded")
    expected <- if (n > 1) {
      rep(-(1 - 1 / n) * log2(1 - 1 / n), n)
    } else {
      numeric(n)
    }
    expect_equal(unname(recorded), expected)
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

test_that("map equation preserves extremely small codelength savings", {
  a <- matrix(0, 3, 3)
  a[1, 2] <- 1
  a[1, 3] <- 1e-300
  # One module, with visits approximately(0,1,epsilon). Direct subtraction
  # of focal mass from module mass loses the dominant node's remaining mass.
  expected <- c(0, 1e-300 * log2(1e300), 1e-300 / log(2))
  for (convention in c("paper", "infomap")) {
    got <- unname(centrality_map_equation(a, map_convention = convention))
    expect_equal(got[2:3] / expected[2:3], c(1, 1), tolerance = 1e-12)
    expect_equal(unname(centrality_map_equation(
      a, map_convention = convention, normalized = TRUE
    )), expected / max(expected), tolerance = 1e-12)
  }
  a[1, 2] <- 1e100
  expect_error(centrality_map_equation(a), "weight range")
})
