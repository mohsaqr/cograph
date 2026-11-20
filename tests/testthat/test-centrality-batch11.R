# ===========================================================================
# Batch 11 — measures that reweight, rescope or re-tune a family cograph
# already computed: length-scaled and distance-decayed betweenness, ego
# betweenness, geodesic power closeness, and the gravity family.
#
# Each measure is held to the member it generalises, so a wrong exponent or
# a dropped factor shows up as a failure rather than as a plausible number.
# The comparisons with the definitions (hand sums, brute force) are declared
# in tests/equivalence/test-centrality-zoo-a-betweenness.R.
# ===========================================================================

adj6 <- matrix(0, 6, 6)
adj6[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj6 <- adj6 + t(adj6)
rownames(adj6) <- colnames(adj6) <- LETTERS[1:6]

# A path A-B-C-D-E: the middle nodes are the only brokers.
p5 <- matrix(0, 5, 5)
p5[cbind(1:4, 2:5)] <- 1
p5 <- p5 + t(p5)
rownames(p5) <- colnames(p5) <- LETTERS[1:5]

star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5 <- star5 + t(star5)
rownames(star5) <- colnames(star5) <- LETTERS[1:5]

# ===========================================================================
# Length-scaled betweenness
# ===========================================================================

test_that("length-scaled betweenness halves the star centre's betweenness", {
  # The star's centre separates all 6 leaf pairs, each at distance 2, so the
  # length-scaled score is half the ordinary one.
  expect_equal(unname(centrality_length_scaled_betweenness(star5)["A"]),
               unname(centrality_betweenness(star5)["A"]) / 2)
})

# ===========================================================================
# Distance-decayed betweenness
# ===========================================================================

test_that("delta = 0 recovers ordinary betweenness", {
  flat <- centrality_delta_betweenness(adj6, betweenness_delta = 0)
  expect_equal(unname(flat), unname(centrality_betweenness(adj6)))
  expect_equal(unname(centrality_delta_betweenness(p5, betweenness_delta = 0)),
               unname(centrality_betweenness(p5)))
})

test_that("raising delta concentrates the score on nearby pairs", {
  # On the path, C brokers the most distant pairs, B the nearest ones, so
  # decay costs C more than B.
  low <- centrality_delta_betweenness(p5, betweenness_delta = 0)
  high <- centrality_delta_betweenness(p5, betweenness_delta = 3)
  expect_lt(unname(high["C"]) / unname(low["C"]),
            unname(high["B"]) / unname(low["B"]))
})

# ===========================================================================
# Ego betweenness
# ===========================================================================

test_that("ego betweenness is not a function of effective size", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  # Burt's effective size and ego betweenness both measure local brokerage,
  # but they are not in bijection: this graph holds two nodes with the same
  # effective size and different ego betweenness.
  set.seed(11)
  g <- igraph::sample_gnp(24, 0.3)
  df <- centrality(g, measures = c("ego_betweenness", "effective_size"))
  dup <- duplicated(round(df$effective_size, 8)) |
    duplicated(round(df$effective_size, 8), fromLast = TRUE)
  expect_true(any(dup))
  expect_gt(length(unique(round(df$ego_betweenness[dup], 8))), 1L)
})

# ===========================================================================
# Geodesic power closeness
# ===========================================================================

test_that("delta-closeness spans the closeness family", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  n <- igraph::vcount(g)
  expect_equal(centrality(g, measures = "delta_closeness",
                          closeness_delta = 1)$delta_closeness_all,
               centrality(g, measures = "harmonic")$harmonic_all / (n - 1))
  expect_equal(centrality(g, measures = "delta_closeness",
                          closeness_delta = 2)$delta_closeness_all,
               centrality(g, measures = "harary")$harary_all / (n - 1))
})

# ===========================================================================
# Gravity family
# ===========================================================================

test_that("the mass and radius arguments select the published variants", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  gm <- centrality(g, measures = "gravity", gravity_mass = "degree",
                   gravity_radius = NULL)$gravity_all
  # The local model truncates; its radius is half the mean distance.
  auto <- centrality(g, measures = "gravity", gravity_mass = "degree",
                     gravity_radius = "auto")$gravity_all
  expect_true(all(auto <= gm))
  expect_false(isTRUE(all.equal(auto, gm)))
})

test_that("the legacy mass is not the default any more", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  old <- centrality(g, measures = "gravity", gravity_mass = "legacy",
                    gravity_radius = NULL)$gravity_all
  expect_false(isTRUE(all.equal(centrality(g, measures = "gravity")$gravity_all,
                                old)))
})

# ===========================================================================
# Wiring
# ===========================================================================

test_that("the new measures are listed and reach type = all", {
  skip_on_cran()
  tab <- list_centralities()
  new <- c("length_scaled_betweenness", "delta_betweenness",
           "ego_betweenness", "delta_closeness")
  expect_true(all(new %in% tab$measure))
  expect_false(any(tab$costly[tab$measure %in% new]))
  memb <- c(1, 1, 1, 2, 2, 2)
  df <- suppressWarnings(centrality(adj6, type = "all", membership = memb))
  expect_true(all(c("length_scaled_betweenness", "delta_betweenness",
                    "ego_betweenness", "delta_closeness_all") %in% names(df)))
})

test_that("empty and single-node graphs return the right shape", {
  skip_if_not_installed("igraph")
  g0 <- igraph::make_empty_graph(0)
  for (m in c("length_scaled_betweenness", "delta_betweenness",
              "ego_betweenness", "delta_closeness", "gravity")) {
    expect_equal(nrow(centrality(g0, measures = m)), 0L, info = m)
  }
  g1 <- igraph::make_empty_graph(1)
  expect_equal(centrality(g1, measures = "gravity")$gravity_all, 0)
  expect_equal(centrality(g1, measures = "ego_betweenness")$ego_betweenness, 0)
})
