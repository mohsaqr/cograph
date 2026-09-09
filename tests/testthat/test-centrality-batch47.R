# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

rsp_kite <- function() {
  a <- matrix(0, 10, 10)
  el <- rbind(c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7),
              c(3, 4), c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7),
              c(6, 8), c(7, 8), c(8, 9), c(9, 10))
  for (k in seq_len(nrow(el))) {
    a[el[k, 1], el[k, 2]] <- 1
    a[el[k, 2], el[k, 1]] <- 1
  }
  a
}

test_that("a single edge scores exactly one at every rsp_beta", {
  # Hand derivation. With u = exp(-beta), Z = [[1, u], [u, 1]] / (1 - u^2),
  # so equation (15) gives p - q^2 / p = (1 - u^2) / (1 - u^2) = 1 at both
  # nodes, whatever beta is.
  edge <- matrix(c(0, 1, 1, 0), 2, 2)
  for (beta in c(0.001, 0.01, 0.5, 1, 5, 20)) {
    expect_equal(unname(centrality_rsp_betweenness(edge, rsp_beta = beta)),
                 c(1, 1))
  }
  expect_equal(unname(centrality_rsp_betweenness(igraph::make_full_graph(2))),
               c(1, 1))
})

test_that("a directed cycle scores n (n - 1) / 2 independently of rsp_beta", {
  # On C_n the fundamental matrix is z_st = u^d(s,t) / (1 - u^n), and
  # equation (14) collapses to the indicator that i lies on the directed
  # path s -> t with i != t. The score therefore counts ordered pairs and
  # the beta dependence cancels exactly.
  for (n in 3:8) {
    cyc <- matrix(0, n, n)
    for (k in seq_len(n)) cyc[k, k %% n + 1L] <- 1
    for (beta in c(0.01, 1, 4)) {
      got <- centrality_rsp_betweenness(cyc, rsp_beta = beta, directed = TRUE)
      expect_equal(unname(got), rep(n * (n - 1) / 2, n))
    }
  }
})

test_that("a complete graph matches its hand-derived closed form", {
  # Sherman-Morrison on ((1 + a) I - a J) with a = exp(-beta) / (n - 1)
  # gives z_ii = p and z_ij = q; summing equation (14) by symmetry over the
  # n - 1 targets gives the expression below.
  closed <- function(n, beta) {
    u <- exp(-beta)
    v <- 1 - u
    p <- ((n - 1) * v + u) / (v * (n - 1 + u))
    q <- u / (v * (n - 1 + u))
    (n - 1) * (p - q^2 / p + (n - 2) * (q - q^2 / p))
  }
  for (n in 3:7) {
    for (beta in c(0.01, 0.5, 2)) {
      got <- centrality_rsp_betweenness(igraph::make_full_graph(n),
                                        rsp_beta = beta)
      expect_equal(unname(got), rep(closed(n, beta), n))
    }
  }
})

test_that("the closed form agrees with the definitional double sum", {
  # Equations (8) and (14) accumulated literally over ordered pairs, with
  # the paper's own zero rule below equation (9) for an unreachable pair.
  # Production evaluates equation (15) instead, so this is the check that
  # the collapse is faithful.
  double_sum <- function(a, beta) {
    n <- nrow(a)
    diag(a) <- 0
    out <- rowSums(a)
    pref <- matrix(0, n, n)
    live <- out > 0
    if (any(live)) pref[live, ] <- a[live, , drop = FALSE] / out[live]
    cm <- matrix(0, n, n)
    arc <- a > 0
    cm[arc] <- 1 / a[arc]
    z <- solve(diag(1, n) - pref * exp(-beta * cm))
    reach <- .cg_reach_closure(a)
    total <- numeric(n)
    # Definitional double sum: the point of the test is that it is written
    # out pair by pair rather than collapsed, so the loops are the check.
    for (s in seq_len(n)) {
      for (t in seq_len(n)) {
        if (!reach[s, t]) next
        total <- total + (z[s, ] / z[s, t] - z[t, ] / z[t, t]) * z[, t]
      }
    }
    total
  }
  set.seed(4701)
  adj <- function(g) as.matrix(igraph::as_adjacency_matrix(g))
  graphs <- list(rsp_kite(),
                 adj(igraph::make_ring(7)),
                 adj(igraph::make_star(6, mode = "undirected")),
                 adj(igraph::make_tree(10, 3)),
                 adj(igraph::sample_gnp(9, 0.4, directed = TRUE)))
  for (a in graphs) {
    for (beta in c(0.01, 1)) {
      expect_equal(.cg_rsp_terms(a, beta)$score, double_sum(a, beta))
    }
  }
})

test_that("strongly connected input reproduces equation (15) verbatim", {
  # Equation (15) as printed, with no reachability mask anywhere. It is
  # only defined when every entry of Z is positive, which is exactly the
  # strongly connected case.
  printed <- function(a, beta) {
    n <- nrow(a)
    pref <- a / rowSums(a)
    cm <- 1 / a
    cm[!is.finite(cm)] <- 0
    z <- solve(diag(1, n) - pref * exp(-beta * cm))
    recip <- 1 / z
    diag(z %*% t(recip - n * diag(diag(recip), n)) %*% z)
  }
  kite <- rsp_kite()
  for (beta in c(0.01, 0.3, 1, 2)) {
    expect_equal(unname(centrality_rsp_betweenness(kite, rsp_beta = beta)),
                 printed(kite, beta))
  }
  ring <- as.matrix(igraph::as_adjacency_matrix(igraph::make_ring(8)))
  expect_equal(unname(centrality_rsp_betweenness(ring, rsp_beta = 1)),
               printed(ring, 1))
})

test_that("scores are component-local and an isolate scores a derived zero", {
  # A node's score depends only on the pairs it can stand between, so two
  # disjoint triangles score what one triangle scores.
  tri <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  two <- matrix(0, 6, 6)
  two[1:3, 1:3] <- tri
  two[4:6, 4:6] <- tri
  one <- centrality_rsp_betweenness(tri, rsp_beta = 1)
  expect_equal(unname(centrality_rsp_betweenness(two, rsp_beta = 1)),
               rep(unname(one), 2))

  # Adding an isolate leaves every other score untouched, and the isolate
  # itself scores exactly zero because its P^ref row is zero, giving
  # z_ii = 1 and 1 - 1 = 0.
  kite <- rsp_kite()
  padded <- rbind(cbind(kite, 0), 0)
  expect_equal(unname(centrality_rsp_betweenness(padded, rsp_beta = 1))[1:10],
               unname(centrality_rsp_betweenness(kite, rsp_beta = 1)))
  expect_equal(unname(centrality_rsp_betweenness(padded, rsp_beta = 1))[11], 0)
})

test_that("degenerate graphs return derived zeros or nothing at all", {
  expect_length(centrality_rsp_betweenness(igraph::make_empty_graph(0)), 0L)
  expect_equal(unname(centrality_rsp_betweenness(igraph::make_empty_graph(1))),
               0)
  expect_equal(unname(centrality_rsp_betweenness(igraph::make_empty_graph(5))),
               rep(0, 5))
  expect_length(.cg_rsp_terms(matrix(0, 0, 0))$score, 0L)
})

test_that("the measure is direction-sensitive and a sink scores zero", {
  # 1 -> 2 -> 3. Node 3 emits no walk and is never an intermediate, so it
  # scores zero; reversing the arcs moves the zero to node 1.
  p3 <- matrix(0, 3, 3)
  p3[1, 2] <- 1
  p3[2, 3] <- 1
  forward <- centrality_rsp_betweenness(p3, rsp_beta = 1, directed = TRUE)
  backward <- centrality_rsp_betweenness(t(p3), rsp_beta = 1, directed = TRUE)
  expect_equal(unname(forward), c(2, 2, 0))
  expect_equal(unname(backward), c(0, 2, 2))
  expect_false(isTRUE(all.equal(unname(forward), unname(backward))))
})

test_that("the beta -> 0+ limit is proportional to degree (source page 9)", {
  set.seed(4702)
  g <- igraph::sample_gnp(12, 0.4)
  while (!igraph::is_connected(g)) g <- igraph::sample_gnp(12, 0.4)
  a <- as.matrix(igraph::as_adjacency_matrix(g))
  degree <- rowSums(a)
  spread <- vapply(c(1e-2, 1e-4, 1e-6), function(beta) {
    ratio <- unname(centrality_rsp_betweenness(a, rsp_beta = beta)) / degree
    max(abs(ratio - mean(ratio))) / abs(mean(ratio))
  }, numeric(1))
  expect_true(all(diff(spread) < 0))
  expect_lt(spread[3], 1e-4)
  # Each two decades of beta divides the spread by a hundred: the approach
  # is first order in beta.
  expect_lt(abs(log10(spread[3] / spread[2]) + 2), 0.05)
})

test_that("rsp_cost only matters on weighted input", {
  # C = 1 / w and C = w are both unit cost per arc on a binary graph.
  kite <- rsp_kite()
  expect_equal(centrality_rsp_betweenness(kite, rsp_cost = "weight"),
               centrality_rsp_betweenness(kite, rsp_cost = "inverse"))
  # On a genuinely weighted graph they must differ.
  w <- matrix(0, 4, 4)
  w[1, 2] <- w[2, 1] <- 2
  w[2, 3] <- w[3, 2] <- 1
  w[3, 4] <- w[4, 3] <- 4
  inverse <- centrality_rsp_betweenness(w, rsp_beta = 1, rsp_cost = "inverse")
  weight <- centrality_rsp_betweenness(w, rsp_beta = 1, rsp_cost = "weight")
  expect_false(isTRUE(all.equal(unname(inverse), unname(weight))))
  # Both are still symmetric under relabelling the symmetric path.
  expect_equal(unname(inverse), rev(rev(unname(inverse))))
})

test_that("rsp_beta and rsp_cost domains are enforced, not extended", {
  ring <- as.matrix(igraph::as_adjacency_matrix(igraph::make_ring(6)))
  for (bad in list(0, -1, -0.01)) {
    expect_error(centrality_rsp_betweenness(ring, rsp_beta = bad),
                 class = "cograph_bad_parameter")
  }
  for (bad in list(NA_real_, Inf, c(0.5, 0.5), "0.5")) {
    expect_error(centrality_rsp_betweenness(ring, rsp_beta = bad),
                 class = "cograph_bad_parameter")
  }
  expect_error(centrality_rsp_betweenness(ring, rsp_cost = "sqrt"))

  # Algorithm 1 takes a non-negative cost matrix; a negative weight makes
  # exp(-beta C) exceed one and the Neumann series diverge.
  neg <- ring
  neg[1, 2] <- neg[2, 1] <- -1
  expect_error(.cg_rsp_terms(neg, 0.01), class = "cograph_bad_input")
  inf_w <- ring
  inf_w[1, 2] <- Inf
  expect_error(.cg_rsp_terms(inf_w, 0.01), class = "cograph_bad_input")
})

test_that("labels, permutation and input projection behave", {
  a <- rsp_kite()
  dimnames(a) <- list(letters[1:10], letters[1:10])
  score <- centrality_rsp_betweenness(a, rsp_beta = 1)
  expect_named(score, letters[1:10])
  expect_equal(centrality(a, measures = "rsp_betweenness",
                          rsp_beta = 1)$rsp_betweenness, unname(score))
  set.seed(4703)
  perm <- sample.int(10)
  expect_equal(unname(centrality_rsp_betweenness(a[perm, perm], rsp_beta = 1)),
               unname(score)[perm])
  # Loops, mode, cutoff and weight inversion are outside the domain and are
  # absorbed; weights are not, so they are left alone here.
  projected <- unname(a)
  diag(projected) <- 5
  ignored <- centrality_rsp_betweenness(projected, rsp_beta = 1, loops = TRUE,
                                        mode = "in", cutoff = 1,
                                        invert_weights = TRUE)
  expect_equal(unname(ignored), unname(score))
  expect_equal(unname(centrality_rsp_betweenness(a, rsp_beta = 1,
                                                 normalized = TRUE)),
               unname(score) / max(unname(score)))
})

test_that("rsp_betweenness is a costly weighted no-mode measure", {
  meta <- list_centralities()
  expect_true("rsp_betweenness" %in% meta$measure)
  row <- meta[meta$measure == "rsp_betweenness", ]
  expect_true(row$uses_weights)
  expect_true(row$costly)
  expect_false(row$mode_aware)
  expect_false(row$needs_membership)
  expect_identical(row$orientation, "higher")
  expect_true("rsp_betweenness" %in% .cg_no_mode_measures())
  expect_false("rsp_betweenness" %in% .cg_mode_measures())
  expect_true("rsp_betweenness" %in% .cg_costly_measures())
  expect_identical(names(formals(centrality_rsp_betweenness)), c("x", "..."))
  expect_true("rsp_beta" %in% names(formals(centrality)))
  expect_true("rsp_cost" %in% names(formals(centrality)))
  expect_equal(eval(formals(centrality)$rsp_beta), 0.01)
  expect_identical(eval(formals(centrality)$rsp_cost),
                   c("inverse", "weight"))
  # Costly measures are held back from the "all" tier and computed when
  # named directly.
  # The "all" tier is setdiff(every measure, the costly ones), so being on
  # the costly list is exactly what holds it back; naming it always computes
  # it. Asserted against the tier construction rather than by running the
  # whole tier, which raises unrelated warnings on an undirected graph.
  everything <- c(.cg_mode_measures(), .cg_no_mode_measures())
  expect_false("rsp_betweenness" %in%
                 setdiff(everything, .cg_costly_measures()))
  expect_true("rsp_betweenness" %in%
                names(centrality(rsp_kite(), measures = "rsp_betweenness")))
})

test_that("the reachability closure matches an independent traversal", {
  set.seed(4704)
  for (trial in seq_len(8)) {
    g <- igraph::sample_gnp(9, 0.25, directed = TRUE)
    a <- as.matrix(igraph::as_adjacency_matrix(g))
    expected <- is.finite(igraph::distances(g, mode = "out", weights = NA))
    expect_equal(unname(.cg_reach_closure(a)), unname(expected))
  }
})
