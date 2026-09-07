# Alias the public wrapper to keep the test expressions readable.
rwd <- centrality_random_walk_decay

test_that("random walk decay follows first arrivals and terminal sinks", {
  path <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
  expect_equal(unname(rwd(path)), c(1, 1.5, 1.75))
  expect_equal(unname(rwd(path, rwd_node_weights = c(2, 0, 1))), c(2, 1, 1.5))
  reversed <- igraph::reverse_edges(path)
  expect_equal(unname(rwd(reversed)), c(1.75, 1.5, 1))
  ring <- igraph::make_ring(4, directed = TRUE)
  for (decay in c(0, 0.1, 0.5, 0.9, 1 - .Machine$double.eps / 2)) {
    expect_equal(
      unname(rwd(ring, decay)),
      rep(sum(decay^(0:3)), 4)
    )
  }
  undirected <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
  expect_equal(
    unname(rwd(undirected)),
    c(10 / 7, 2, 10 / 7)
  )
  for (n in 0:3) {
    expect_equal(
      unname(rwd(igraph::make_empty_graph(n))),
      rep(1, n)
    )
  }
  isolated <- igraph::add_vertices(path, 1)
  expect_equal(unname(rwd(isolated)), c(1, 1.5, 1.75, 1))
  expect_equal(unname(rwd(path, rwd_node_weights = c(0, 0, 0))), rep(0, 3))
})

test_that("random walk decay reproduces the published Example 4 table", {
  edges <- c(
    1, 2, 2, 3, 3, 4, 4, 1, 5, 6, 6, 7, 7, 8, 8, 5,
    8, 4, 3, 7, 2, 6
  )
  graph <- igraph::make_graph(edges, directed = TRUE)
  modified <- igraph::add_edges(graph, c(1, 4))
  # Source labels u1,u2,u3,u4,v1,v2,v3,v4; rounded to two decimals.
  expect_equal(
    round(unname(rwd(graph, 0.8)), 2),
    c(3.72, 3.75, 2.48, 3.68, 2.76, 4.13, 4.67, 4.43)
  )
  expect_equal(
    round(unname(rwd(modified, 0.8)), 2),
    c(3.72, 3.02, 2.17, 3.94, 2.60, 3.77, 4.36, 4.21)
  )
  # Lack of self-impact concerns raw scores, not maximum-normalized ones.
  expect_equal(
    rwd(graph, 0.8)[1],
    rwd(modified, 0.8)[1]
  )
})

test_that("random walk decay retains loops, multiplicity and node weights", {
  # Published Figure 1: u loops, two u->v arcs, v->w, w->v, w->t.
  g <- igraph::make_graph(c(1, 1, 1, 2, 1, 2, 2, 3, 3, 2, 3, 4),
    directed = TRUE
  )
  igraph::V(g)$name <- c("u", "v", "w", "t")
  mass <- c(u = 1, v = 0, w = 1, t = 0)
  # Equation 6 and the first-arrival series give these values. The
  # inconsistent printed Example 3 values are retained in the local audit.
  expected <- c(u = 1, v = 13 / 20, w = 6 / 5, t = 12 / 35)
  score <- rwd(g,
    rwd_node_weights = mass,
    simplify = FALSE
  )
  expect_equal(score, expected)
  igraph::E(g)$weight <- rep(1, igraph::ecount(g))
  expect_equal(rwd(g, rwd_node_weights = mass), expected)
  expect_equal(
    rwd(g,
      rwd_node_weights = mass[c(4, 2, 1, 3)],
      mode = "in", invert_weights = TRUE
    ),
    expected
  )
  expect_equal(
    rwd(g, rwd_node_weights = 3 * mass),
    3 * expected
  )
  removed <- rwd(g, rwd_node_weights = mass, loops = FALSE)
  expect_equal(removed, c(u = 1, v = 3 / 4, w = 5 / 4, t = 5 / 14))
  no_out <- igraph::delete_edges(g, igraph::incident(g, "v", mode = "out"))
  expect_equal(
    rwd(no_out, rwd_node_weights = mass)["v"],
    expected["v"]
  )
})

test_that("random walk decay reproduces the published Example 5 table", {
  a <- matrix(0, 10, 10)
  a[1:3, 1:3] <- 1
  a[4:6, 4:6] <- 1
  for (edge in list(
    c(7, 8), c(8, 9), c(9, 10), c(10, 7),
    c(1, 4), c(4, 7), c(7, 1)
  )) {
    a[edge[1], edge[2]] <- a[edge[2], edge[1]] <- 1
  }
  diag(a) <- 0
  b <- a
  b[2, 1] <- b[5, 4] <- 0
  b[2, 4] <- b[5, 1] <- 1
  # Source Figure 3: u1,u2,u3,v1,v2,v3,w1,w2,w3,w4.
  expect_equal(
    round(unname(rwd(a, 0.8, directed = TRUE)), 2),
    c(4.15, 2.56, 2.56, 4.15, 2.56, 2.56, 4.40, 2.74, 2.62, 2.74)
  )
  expect_equal(
    round(unname(rwd(b, 0.8, directed = TRUE)), 2),
    c(4.46, 2.85, 2.70, 4.46, 2.85, 2.70, 4.40, 2.74, 2.62, 2.74)
  )
})

test_that("random walk decay handles weighted transitions and normalization", {
  a <- matrix(c(0, 1, 3, 0, 0, 0, 0, 0, 0), 3, 3,
    byrow = TRUE,
    dimnames = list(c("C", "A", "B"), c("C", "A", "B"))
  )
  expected <- c(C = 1, A = 9 / 8, B = 11 / 8)
  expect_equal(rwd(a), expected)
  expect_equal(
    centrality(a, measures = "random_walk_decay")$random_walk_decay,
    unname(expected)
  )
  for (scale in c(1e-300, 1e300)) {
    expect_equal(rwd(a * scale, directed = TRUE), expected)
  }
  expect_equal(
    unname(rwd(a, weighted = FALSE)),
    c(1, 1.25, 1.25)
  )
  expect_equal(
    rwd(a, normalized = TRUE),
    expected / max(expected)
  )
  perm <- c(3, 1, 2)
  mass <- c(C = 2, A = 0, B = 1)
  result <- rwd(a, rwd_node_weights = mass)
  expect_equal(
    rwd(a[perm, perm], rwd_node_weights = mass),
    result[perm]
  )
  star <- igraph::make_star(4, mode = "in")
  expect_error(
    rwd(star, rwd_node_weights = rep(1e308, 4)),
    "raw scores overflow"
  )
  normalized <- rwd(star, rwd_node_weights = rep(1e308, 4), normalized = TRUE)
  expect_equal(unname(normalized), c(1, 0.4, 0.4, 0.4))
  meta <- subset(list_centralities(), measure == "random_walk_decay")
  expect_true(meta$uses_weights)
  expect_true(meta$costly)
  expect_false(meta$mode_aware)
})

test_that("random walk decay validates parameters and starting weights", {
  g <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
  for (bad in list(-1, 1, Inf, NaN, NA_real_, "0.5", TRUE, c(0.1, 0.2))) {
    expect_error(
      rwd(igraph::make_empty_graph(0), bad),
      "rwd_decay"
    )
  }
  for (bad in list(c(1, 2), c(-1, 0, 1), c(1, NA, 2), c(1, Inf, 2), "1")) {
    expect_error(
      rwd(g, rwd_node_weights = bad),
      "rwd_node_weights"
    )
  }
  expect_error(
    rwd(g, rwd_node_weights = c(
      a = 1, b = 2,
      c = 3
    )),
    "names must match"
  )
  duplicate <- setNames(1:3, c("1", "1", "3"))
  expect_error(rwd(g, rwd_node_weights = duplicate), "names must match")
  for (bad in c(-1, NA_real_, Inf)) {
    igraph::E(g)$weight <- c(1, bad)
    expect_error(rwd(g), "finite nonnegative")
  }
  branch <- igraph::make_graph(c(1, 2, 1, 3), directed = TRUE)
  igraph::E(branch)$weight <- c(1e300, 1e-300)
  expect_error(rwd(branch), "transition range")
  branch <- igraph::make_graph(c(1, 2, 1, 3, 1, 4), directed = TRUE)
  smallest <- .Machine$double.xmin * .Machine$double.eps
  igraph::E(branch)$weight <- c(1, 1, smallest)
  expect_error(rwd(branch), "transition range")
  loop <- igraph::make_graph(c(1, 1, 1, 2), directed = TRUE)
  igraph::E(loop)$weight <- c(1e-100, 1)
  expect_error(rwd(loop, 1e-300), "discounted transition range")
})

test_that("forward mass recovers underflowed first-hit contributions", {
  path <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
  for (decay in c(1e-200, 1e-300)) {
    result <- rwd(path, decay, rwd_node_weights = c(1e308, 0, 0))
    expected <- c(1e308, 1e308 * decay, (1e308 * decay) * decay)
    # Compare ratios so that an incorrectly returned tiny zero cannot
    # pass a tolerance scaled by the much larger first node's score.
    expect_equal(unname(result) / expected, rep(1, 3), tolerance = 1e-12)
  }
})
