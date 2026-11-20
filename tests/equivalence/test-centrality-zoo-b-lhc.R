# The LHC index, centrality_lhc() (Wang, Yang, Liu and Ma 2021, PLoS ONE 16(5):e0251208), against computations
# written out from the source: equation (1) C(v) = sum over u within distance d of v of k_u (1 + TP(u)) / d(v, u)^2,
# with TP(u) = NTS(u) / TNTS (NTS the triangles at u, TNTS their sum over all nodes, page 3), and equation (2)
# Lhc(v) = sum over the neighbours w of v of C(w) (page 4). Closed forms on paths, stars, complete graphs and rings
# are worked out by hand; on general graphs the definition is evaluated in base R with hop distances computed here.
# No other package implements this measure.

independent <- "independent: closed forms and hand derivations from the definition"
# Sums of a few dozen rationals: agreement to rounding.
exact <- 1e-10

# Hop distances between all nodes, by breadth-first growth of the reached set.
lhc_hops <- function(a) {
  n <- nrow(a)
  adjacent <- unname(a) > 0
  start <- list(reach = diag(n) > 0, d = ifelse(diag(n) > 0, 0, Inf))
  Reduce(\(state, step) {
    reach <- (state$reach %*% adjacent) > 0 | state$reach
    state$d[reach & !state$reach] <- step
    list(reach = reach, d = state$d)
  }, seq_len(max(n - 1L, 1L)), start)$d
}

# Equations (1) and (2) on the simple undirected skeleton. NTS(u) is diag(A^3) / 2; when the graph has no triangle
# TNTS is 0 and every NTS is 0, and TP is taken as 0 (max(TNTS, 1) leaves 0 / 1).
lhc_definition <- function(a, radius = 2) {
  a <- unname((a + t(a)) > 0) * 1
  diag(a) <- 0
  degree <- rowSums(a)
  nts <- diag(a %*% a %*% a) / 2
  tp <- nts / max(sum(nts), 1)
  d <- lhc_hops(a)
  discount <- ifelse(d >= 1 & d <= radius, 1 / d^2, 0)
  as.numeric(a %*% (discount %*% (degree * (1 + tp))))
}

test_that("lhc of the path 1-2-3 matches its hand derivation", {
  # Triangle-free, degrees 1, 2, 1, d(1, 3) = 2: C(end) = 2 / 1 + 1 / 2^2, C(middle) = 1 + 1, and equation (2)
  # gives Lhc(end) = C(middle), Lhc(middle) = 2 C(end). At radius 1 the 1 / 4 term drops out.
  expect_reference(centrality_lhc(zoob_path(3)), c(2, 2 * (2 + 1 / 4), 2), reference = independent,
                   covers = c("centrality()", "centrality_lhc()"), quantity = "lhc, path of 3, radius 2",
                   tolerance = exact)
  expect_reference(centrality_lhc(zoob_path(3), lhc_radius = 1), c(2, 2 * 2, 2), reference = independent,
                   covers = c("centrality()", "centrality_lhc()"), quantity = "lhc, path of 3, radius 1",
                   tolerance = exact)
})

test_that("lhc of a star matches its closed form", {
  # Triangle-free. C(centre) = m, C(leaf) = m + (m - 1) / 4 at radius >= 2 (m at radius 1);
  # Lhc(centre) = m C(leaf), Lhc(leaf) = C(centre) = m.
  lapply(c(2L, 5L, 9L), \(m) {
    expect_reference(centrality_lhc(zoob_star(m)), c(m * (m + (m - 1) / 4), rep(m, m)), reference = independent,
                     covers = c("centrality()", "centrality_lhc()"), quantity = sprintf("lhc, star of %d, radius 2", m),
                     tolerance = exact)
    expect_reference(centrality_lhc(zoob_star(m), lhc_radius = 1), c(m * m, rep(m, m)), reference = independent,
                     covers = c("centrality()", "centrality_lhc()"), quantity = sprintf("lhc, star of %d, radius 1", m),
                     tolerance = exact)
  })
})

test_that("lhc of a complete graph matches its closed form", {
  # Every NTS is (n - 1)(n - 2) / 2, so TP = 1 / n; every distance is 1, so C(v) = (n - 1)^2 (1 + 1 / n) and
  # Lhc(v) = (n - 1) C(v) = (n - 1)^3 (n + 1) / n at any radius. K_3 is also the 3-ring.
  cases <- expand.grid(n = c(3L, 5L, 8L), radius = c(1L, 3L))
  Map(\(n, radius) {
    expect_reference(centrality_lhc(zoob_complete(n), lhc_radius = radius), rep((n - 1)^3 * (n + 1) / n, n),
                     reference = independent, covers = c("centrality()", "centrality_lhc()"),
                     quantity = sprintf("lhc, complete graph n = %d, radius %d", n, radius), tolerance = exact)
  }, cases$n, cases$radius)
})

test_that("lhc of a ring matches its closed form while the ball does not wrap", {
  # C_n (n >= 4) is triangle-free and 2-regular with two nodes at each distance 1..radius while n > 2 radius:
  # C(v) = sum_{d = 1..radius} 2 * 2 / d^2 and Lhc(v) = 2 C(v).
  cases <- data.frame(n = c(4L, 6L, 9L), radius = c(1L, 2L, 3L))
  Map(\(n, radius) {
    expect_reference(centrality_lhc(zoob_ring(n), lhc_radius = radius), rep(2 * sum(4 / seq_len(radius)^2), n),
                     reference = independent, covers = c("centrality()", "centrality_lhc()"),
                     quantity = sprintf("lhc, ring n = %d, radius %d", n, radius), tolerance = exact)
  }, cases$n, cases$radius)
})

test_that("lhc equals equations (1) and (2) evaluated on general graphs", {
  tree <- zoob_graph(13L, cbind(seq_len(12L) + 1L, (seq_len(12L) + 1L) %/% 2L))
  bipartite <- zoob_graph(7L, cbind(rep(1:3, each = 4L), rep(4:7, 3L)))
  graphs <- list(kite = zoob_kite(), `random 12 nodes` = ref_graph(12, 0.3, seed = 21L),
                 `random 9 nodes` = ref_graph(9, 0.5, seed = 22L), `binary tree` = tree,
                 `ring n = 8` = zoob_ring(8), `K(3, 4)` = bipartite,
                 `kite plus a triangle` = zoob_union(zoob_kite(), zoob_complete(3)))
  cases <- expand.grid(graph = names(graphs), radius = c(1L, 2L, 3L, 7L), stringsAsFactors = FALSE)
  Map(\(graph, radius) {
    expect_reference(centrality_lhc(graphs[[graph]], lhc_radius = radius),
                     lhc_definition(graphs[[graph]], radius), reference = independent,
                     covers = c("centrality()", "centrality_lhc()"),
                     quantity = sprintf("lhc, %s, radius %d", graph, radius), tolerance = exact, measure = "scaled")
  }, cases$graph, cases$radius)
})
