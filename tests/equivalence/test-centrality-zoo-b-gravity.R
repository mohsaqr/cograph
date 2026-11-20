# Gravity-model measures of the centrality batches 38 to 40, through their wrappers: centrality_mcgm(),
# centrality_mixed_gravity(), centrality_extended_mixed_gravity() and centrality_dkgm() (each a call of
# centrality(measures = "<measure>")). Every one sums mass(i) mass(j) / d(i, j)^2 over the partners j within a
# hop radius; they differ in the masses.
#
# - MCGM (Li and Huang 2022, equations 17-18): mass K + alpha S + X, with degree K, core number S and leading
#   eigenvector X each divided by its maximum, alpha = max(median K, median X) / median S; radius 2.
# - Mixed gravity (Wang, Li and Xia 2018, as reproduced by Li and Huang 2022, equations 5-8): core number of
#   the focal node times the degrees of its partners, radius 3; the extended form sums the neighbours' scores.
# - DKGM (Li and Huang 2021, equations 1-3): mass DK = degree + improved k-shell index; radius 2.
#
# Compared with the tables printed by Li and Huang (MCGM 2022, Table 2; DKGM 2021, Table 5), with MCGM and mixed
# gravity written out from their definitions in base R (hop distances by breadth-first layers, core numbers by
# peeling, the eigenvector by eigen()), and with closed forms on stars, cliques, rings and paths.

independent <- "independent: closed forms and hand derivations from the definition"

# The nine-node, thirteen-edge graph of Li and Huang's Figure 1 (the same graph in the 2021 and 2022 papers).
li_huang_graph <- zoob_graph(9L, c(1, 2, 2, 3, 2, 7, 3, 4, 3, 7, 4, 5, 4, 6, 4, 7, 5, 6, 5, 7, 5, 8, 6, 7, 6, 9))

# Hop distances by breadth-first layers (matrix powers of the 0/1 adjacency); Inf where unreachable.
hop_distances <- function(graph) {
  n <- nrow(graph)
  step <- (graph > 0) * 1
  start <- list(walk = diag(n), dist = ifelse(diag(n) == 1, 0, Inf))
  Reduce(\(state, k) {
    walk <- (state$walk %*% step > 0) * 1
    fresh <- walk > 0 & is.infinite(state$dist)
    state$dist[fresh] <- k
    list(walk = walk, dist = state$dist)
  }, seq_len(max(n - 1L, 1L)), start)$dist
}

# Core numbers by peeling: at threshold k remove every node of remaining degree at most k (Seidman 1983).
core_numbers <- function(graph) {
  adjacency <- (graph > 0) * 1
  peel <- function(alive, core, k) {
    if (!any(alive)) return(core)
    low <- which(alive)[rowSums(adjacency[alive, alive, drop = FALSE]) <= k]
    if (!length(low)) return(peel(alive, core, k + 1))
    core[low] <- k
    alive[low] <- FALSE
    peel(alive, core, k)
  }
  peel(rep(TRUE, nrow(adjacency)), numeric(nrow(adjacency)), 0)
}

# focal(i) * sum over reachable j with 0 < d(i, j) <= radius of partner(j) / d(i, j)^2.
gravity_sum <- function(graph, focal, partner, radius) {
  d <- hop_distances(graph)
  inverse_square <- ifelse(is.finite(d) & d > 0 & d <= radius, 1 / d^2, 0)
  focal * as.numeric(inverse_square %*% partner)
}

mixed_gravity_by_definition <- function(graph, radius = 3) {
  gravity_sum(graph, core_numbers(graph), rowSums(graph > 0), radius)
}

mcgm_by_definition <- function(graph, radius = 2) {
  degree <- rowSums(graph > 0)
  core <- core_numbers(graph)
  leading <- abs(eigen((graph > 0) * 1, symmetric = TRUE)$vectors[, 1L])
  features <- list(degree / max(degree), core / max(core), leading / max(leading))
  alpha <- max(stats::median(features[[1L]]), stats::median(features[[3L]])) / stats::median(features[[2L]])
  mass <- features[[1L]] + alpha * features[[2L]] + features[[3L]]
  gravity_sum(graph, mass, mass, radius)
}

test_that("MCGM equals every score of Li and Huang's (2022) Table 2", {
  # Table 2 prints four decimals: half a unit is 5e-5 (absolute).
  expect_reference(centrality_mcgm(li_huang_graph),
                   c(1.9679, 13.1293, 16.9320, 29.0955, 26.0652, 26.0652, 35.9099, 3.4704, 3.4704),
                   reference = "published: Li and Huang 2022, Table 2", covers = c("centrality()", "centrality_mcgm()"),
                   quantity = "mcgm, Li and Huang Figure 1 graph", tolerance = 5e-5)
})

test_that("MCGM equals its definition written out on connected graphs", {
  cases <- list("Li and Huang Figure 1 graph" = li_huang_graph, kite = zoob_kite(), karate = zoob_karate())
  # Sums of a few dozen products of eigenvector entries: rounding of the eigen-solvers, scaled by the score.
  lapply(names(cases), \(name) expect_reference(centrality_mcgm(cases[[name]]), mcgm_by_definition(cases[[name]]),
                                                reference = independent, covers = c("centrality()", "centrality_mcgm()"),
                                                quantity = sprintf("mcgm, %s, definition", name),
                                                tolerance = 1e-10, measure = "scaled"))
})

test_that("MCGM equals its closed forms on cliques, rings and stars", {
  # Every feature is 1 on a regular graph and alpha = 1, so every mass is 3: a clique sums (n - 1) * 9; a ring
  # within radius 2 has two partners at distance 1 and two at distance 2, 2 * 9 + 2 * 9 / 4 = 22.5, and within
  # radius 1.9 only the first two, 18.
  lapply(c(3L, 5L, 8L), \(n) expect_reference(centrality_mcgm(zoob_complete(n)), rep(9 * (n - 1), n),
                                              reference = independent, covers = c("centrality()", "centrality_mcgm()"),
                                              quantity = sprintf("mcgm, clique on %d nodes", n), tolerance = 1e-10))
  lapply(c(5L, 7L, 10L), \(n) {
    expect_reference(centrality_mcgm(zoob_ring(n)), rep(22.5, n), reference = independent,
                     covers = c("centrality()", "centrality_mcgm()"), quantity = sprintf("mcgm, ring on %d nodes", n),
                     tolerance = 1e-10)
    expect_reference(centrality_mcgm(zoob_ring(n), mcgm_radius = 1.9), rep(18, n), reference = independent,
                     covers = c("centrality()", "centrality_mcgm()"),
                     quantity = sprintf("mcgm, ring on %d nodes, radius 1.9", n), tolerance = 1e-10)
  })
  # The 5-leaf star: centre mass 2 + alpha, leaf mass 1/5 + 2 alpha with alpha = 1 / sqrt(5) (the leaf's
  # eigenvector share); the centre sees five leaves at distance 1, a leaf the centre and four leaves at 2.
  alpha <- 1 / sqrt(5)
  centre <- 2 + alpha
  leaf <- 0.2 + 2 * alpha
  expect_reference(centrality_mcgm(zoob_star(5L)), c(5 * centre * leaf, rep(centre * leaf + leaf^2, 5)),
                   reference = independent, covers = c("centrality()", "centrality_mcgm()"),
                   quantity = "mcgm, 5-leaf star", tolerance = 1e-10)
})

test_that("mixed gravity and its extension equal their definitions written out", {
  cases <- list("Li and Huang Figure 1 graph" = li_huang_graph, kite = zoob_kite(), karate = zoob_karate(),
                "path of 5" = zoob_path(5L),
                "ring of 7, clique of 5 and an isolate" = zoob_union(zoob_ring(7L), zoob_complete(5L), matrix(0, 1, 1)))
  lapply(names(cases), \(name) {
    graph <- cases[[name]]
    lapply(list(3, 1, Inf), \(radius) {
      expect_reference(centrality_mixed_gravity(graph, gravity_radius = radius),
                       mixed_gravity_by_definition(graph, radius), reference = independent,
                       covers = c("centrality()", "centrality_mixed_gravity()"),
                       quantity = sprintf("mixed_gravity, %s, radius %g", name, radius),
                       tolerance = 1e-10, measure = "scaled")
      expect_reference(centrality_extended_mixed_gravity(graph, gravity_radius = radius),
                       as.numeric((graph > 0) %*% mixed_gravity_by_definition(graph, radius)), reference = independent,
                       covers = c("centrality()", "centrality_extended_mixed_gravity()"),
                       quantity = sprintf("extended_mixed_gravity, %s, radius %g", name, radius),
                       tolerance = 1e-10, measure = "scaled")
    })
  })
})

test_that("mixed gravity and its extension equal their closed forms on stars and cliques", {
  # q-leaf star: every core number is 1; the centre sees q leaves of degree 1, a leaf sees the centre (degree q)
  # and q - 1 leaves at distance 2, q + (q - 1) / 4. The extension sums the neighbours: q * leaf at the centre,
  # q at a leaf. Radius 1 drops the distance-2 term. A clique of n has core n - 1 and n - 1 partners of degree
  # n - 1 at distance 1, (n - 1)^3, and its extension (n - 1)^4.
  lapply(c(2L, 5L, 20L), \(q) {
    leaf <- q + (q - 1) / 4
    star <- zoob_star(q)
    expect_reference(centrality_mixed_gravity(star), c(q, rep(leaf, q)), reference = independent,
                     covers = c("centrality()", "centrality_mixed_gravity()"),
                     quantity = sprintf("mixed_gravity, %d-leaf star", q), tolerance = 1e-10)
    expect_reference(centrality_extended_mixed_gravity(star), c(q * leaf, rep(q, q)), reference = independent,
                     covers = c("centrality()", "centrality_extended_mixed_gravity()"),
                     quantity = sprintf("extended_mixed_gravity, %d-leaf star", q), tolerance = 1e-10)
    expect_reference(centrality_mixed_gravity(star, gravity_radius = 1), rep(q, q + 1), reference = independent,
                     covers = c("centrality()", "centrality_mixed_gravity()"),
                     quantity = sprintf("mixed_gravity, %d-leaf star, radius 1", q), tolerance = 1e-10)
    expect_reference(centrality_extended_mixed_gravity(star, gravity_radius = 1), c(q^2, rep(q, q)),
                     reference = independent, covers = c("centrality()", "centrality_extended_mixed_gravity()"),
                     quantity = sprintf("extended_mixed_gravity, %d-leaf star, radius 1", q), tolerance = 1e-10)
  })
  lapply(c(3L, 5L, 8L), \(n) {
    expect_reference(centrality_mixed_gravity(zoob_complete(n)), rep((n - 1)^3, n), reference = independent,
                     covers = c("centrality()", "centrality_mixed_gravity()"),
                     quantity = sprintf("mixed_gravity, clique on %d nodes", n), tolerance = 1e-10)
    expect_reference(centrality_extended_mixed_gravity(zoob_complete(n)), rep((n - 1)^4, n), reference = independent,
                     covers = c("centrality()", "centrality_extended_mixed_gravity()"),
                     quantity = sprintf("extended_mixed_gravity, clique on %d nodes", n), tolerance = 1e-10)
  })
})

test_that("DKGM equals every score of Li and Huang's (2021) Table 5", {
  # Table 5 (radius 2) prints two decimals: half a unit is 0.005 (absolute).
  expect_reference(centrality_dkgm(li_huang_graph),
                   c(20.61, 116.44, 143.08, 228.56, 210.22, 210.22, 289.58, 30.53, 30.53),
                   reference = "published: Li and Huang 2021, Table 5", covers = c("centrality()", "centrality_dkgm()"),
                   quantity = "dkgm, Li and Huang Figure 1 graph, radius 2", tolerance = 0.005)
})

test_that("DKGM equals its closed forms on stars, cliques, rings and a path", {
  # q-leaf star: the leaves leave the one-shell in stage 1 and the centre in stage 2, so the stage denominator
  # is 3, DK(leaf) = 1 + 1 + 1/3 = 7/3 and DK(centre) = q + 1 + 2/3. Radius 1: the centre sees q leaves, a leaf
  # the centre; radius 2 adds the q - 1 other leaves at distance 2.
  lapply(c(2L, 5L, 20L), \(q) {
    leaf <- 7 / 3
    centre <- q + 5 / 3
    expect_reference(centrality_dkgm(zoob_star(q), dkgm_radius = 1), c(q * centre * leaf, rep(leaf * centre, q)),
                     reference = independent, covers = c("centrality()", "centrality_dkgm()"),
                     quantity = sprintf("dkgm, %d-leaf star, radius 1", q), tolerance = 1e-10)
    expect_reference(centrality_dkgm(zoob_star(q)),
                     c(q * centre * leaf, rep(leaf * centre + (q - 1) * leaf^2 / 4, q)), reference = independent,
                     covers = c("centrality()", "centrality_dkgm()"), quantity = sprintf("dkgm, %d-leaf star", q),
                     tolerance = 1e-10)
  })
  # A clique: shell n - 1, stage 1 of 1, so DK = (n - 1) + (n - 1) + 1/2 and n - 1 partners at distance 1.
  lapply(c(2L, 5L, 8L), \(n) expect_reference(centrality_dkgm(zoob_complete(n)), rep((n - 1) * (2 * n - 1.5)^2, n),
                                              reference = independent, covers = c("centrality()", "centrality_dkgm()"),
                                              quantity = sprintf("dkgm, clique on %d nodes", n), tolerance = 1e-10))
  # A ring of 7: DK = 2 + 2 + 1/2 = 4.5 everywhere; two partners at distance 1 and two at distance 2.
  expect_reference(centrality_dkgm(zoob_ring(7L), dkgm_radius = 1), rep(2 * 4.5^2, 7), reference = independent,
                   covers = c("centrality()", "centrality_dkgm()"), quantity = "dkgm, ring of 7, radius 1",
                   tolerance = 1e-10)
  expect_reference(centrality_dkgm(zoob_ring(7L)), rep(2 * 4.5^2 * 1.25, 7), reference = independent,
                   covers = c("centrality()", "centrality_dkgm()"), quantity = "dkgm, ring of 7, radius 2",
                   tolerance = 1e-10)
  # Path of 5: ends leave in stage 1, their neighbours in stage 2, the centre in stage 3 (denominator 4), so
  # DK = 9/4, 7/2, 15/4, 7/2, 9/4; equation 3 is then the gravity sum of DK with itself.
  path <- zoob_path(5L)
  dk <- c(9 / 4, 7 / 2, 15 / 4, 7 / 2, 9 / 4)
  lapply(1:2, \(radius) expect_reference(centrality_dkgm(path, dkgm_radius = radius), gravity_sum(path, dk, dk, radius),
                                         reference = independent, covers = c("centrality()", "centrality_dkgm()"),
                                         quantity = sprintf("dkgm, path of 5, radius %d", radius), tolerance = 1e-10))
})
