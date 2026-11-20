# NINL, the neighbour-layer centrality of Zhu and Wang (2021), through centrality_ninl() (a wrapper of
# centrality(measures = "ninl")). It is compared with every value printed in the paper's Table 1 and with the
# definition written out in base R: b_i sums the degrees of the nodes within r = ceiling(mean shortest-path
# length) hops of i (i included), and NINL of order p is A^p b (Zhu and Wang 2021, equations 1 and 2).

zhu_wang_graph <- zoob_graph(13L, c(1, 3, 2, 4, 3, 4, 3, 5, 3, 6, 4, 5, 4, 7, 4, 8, 4, 9, 5, 6, 5, 8, 6, 8, 8, 9,
                                    9, 10, 9, 11, 9, 12, 10, 12, 11, 12, 12, 13))

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

# Equations 1 and 2 of Zhu and Wang (2021): radius from the mean distance over distinct reachable pairs (the
# global radius is infinite when some pair is unreachable), b = degrees summed within the radius, then A^p b.
ninl_by_definition <- function(graph, order) {
  d <- hop_distances(graph)
  off <- d[row(d) != col(d)]
  radius <- if (all(is.finite(off))) ceiling(mean(off)) else Inf
  degree <- rowSums(graph > 0)
  b <- as.numeric((d <= radius & is.finite(d)) %*% degree)
  Reduce(\(scores, k) as.numeric((graph > 0) %*% scores), seq_len(order), b)
}

test_that("NINL equals every score of Zhu and Wang's Table 1", {
  # The transcription of Figure 1 is checked against the sum of ordered-pair distances the paper prints (358).
  expect_equal(sum(hop_distances(zhu_wang_graph)), 358)
  # Table 1 prints integers, so half a unit is 0.5 (absolute).
  printed <- list(
    c(29, 37, 37, 38, 37, 37, 37, 38, 38, 37, 37, 37, 24),
    c(37, 38, 141, 224, 150, 112, 38, 150, 187, 75, 75, 136, 37),
    c(141, 224, 523, 704, 627, 441, 224, 673, 660, 323, 323, 374, 136),
    c(523, 704, 1913, 2931, 2341, 1823, 704, 2432, 2397, 1034, 1034, 1442, 374))
  lapply(0:3, \(p) expect_reference(centrality_ninl(zhu_wang_graph, ninl_order = p), printed[[p + 1L]],
                                    reference = "published: Zhu and Wang 2021, Table 1",
                                    covers = c("centrality()", "centrality_ninl()"),
                                    quantity = sprintf("ninl, Zhu and Wang graph, order %d", p),
                                    tolerance = 0.5))
})

test_that("NINL equals its definition written out on the kite, karate and a graph with an isolate", {
  # Integer arithmetic: exact agreement is expected.
  disconnected <- zoob_union(zoob_path(7L), matrix(0, 1, 1))
  cases <- list(list(zoob_kite(), "kite", 2L), list(zoob_karate(), "karate", 1L),
                list(zoob_karate(), "karate", 3L), list(zhu_wang_graph, "Zhu and Wang graph", 3L),
                list(disconnected, "path of 7 plus an isolate", 0L))
  lapply(cases, \(case) expect_reference(centrality_ninl(case[[1L]], ninl_order = case[[3L]]),
                                         ninl_by_definition(case[[1L]], case[[3L]]),
                                         reference = "independent: closed forms and hand derivations from the definition",
                                         covers = c("centrality()", "centrality_ninl()"),
                                         quantity = sprintf("ninl, %s, order %d", case[[2L]], case[[3L]]),
                                         tolerance = 1e-10))
})

test_that("NINL on a star keeps the parity of the walk count, at any order", {
  # On the 5-leaf star every node is within the radius 2 of all others, so b = 10 everywhere; A^p b alternates
  # between (1, ..., 1) for even p and (1, 1/5, ..., 1/5) for odd p once divided by its maximum. The orders
  # near 2^53 are reached by repeated squaring in floating point, hence the relative 1e-12.
  star <- zoob_star(5L)
  orders <- c(0, 1, 4, 5, 2049, 2^53 - 2, 2^53 - 1)
  lapply(orders, \(p) expect_reference(centrality_ninl(star, ninl_order = p, normalized = TRUE),
                                       if (p %% 2 == 0) rep(1, 6) else c(1, rep(0.2, 5)),
                                       reference = "independent: closed forms and hand derivations from the definition",
                                       covers = c("centrality()", "centrality_ninl()"),
                                       quantity = sprintf("ninl, 5-leaf star, normalised, order %.0f", p),
                                       tolerance = 1e-12))
  expect_reference(centrality_ninl(star, ninl_order = 0), rep(10, 6),
                   reference = "independent: closed forms and hand derivations from the definition",
                   covers = c("centrality()", "centrality_ninl()"), quantity = "ninl, 5-leaf star, raw, order 0",
                   tolerance = 1e-10)
  expect_reference(centrality_ninl(star, ninl_order = 1), c(50, rep(10, 5)),
                   reference = "independent: closed forms and hand derivations from the definition",
                   covers = c("centrality()", "centrality_ninl()"), quantity = "ninl, 5-leaf star, raw, order 1",
                   tolerance = 1e-10)
  # Radius 0 keeps only the node itself: b is the degree.
  expect_reference(centrality_ninl(star, ninl_order = 0, ninl_radius = 0), c(5, rep(1, 5)),
                   reference = "independent: closed forms and hand derivations from the definition",
                   covers = c("centrality()", "centrality_ninl()"), quantity = "ninl, 5-leaf star, radius 0, order 0",
                   tolerance = 1e-10)
})
