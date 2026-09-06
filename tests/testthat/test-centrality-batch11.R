# ===========================================================================
# Batch 11 — measures that reweight, rescope or re-tune a family cograph
# already computed: length-scaled and distance-decayed betweenness, ego
# betweenness, geodesic power closeness, and the gravity family.
#
# Each measure is held to the member it generalises, so a wrong exponent or
# a dropped factor shows up as a failure rather than as a plausible number.
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

test_that("length-scaled betweenness weights each pair by 1 / distance", {
  # The star's centre separates all 6 leaf pairs, each at distance 2, so the
  # score is 6 * (1/2) = 3 where ordinary betweenness is 6.
  ls <- centrality_length_scaled_betweenness(star5)
  expect_equal(unname(ls["A"]), 3)
  expect_equal(unname(centrality_betweenness(star5)["A"]), 6)
  expect_true(all(ls[c("B", "C", "D", "E")] == 0))
})

test_that("length-scaled betweenness on a path matches the hand sum", {
  # C separates A-D (d=3), A-E (d=4), B-D (d=2), B-E (d=3), A-B? no.
  # Pairs through C: A-D 1/3, A-E 1/4, B-D 1/2, B-E 1/3.
  expect_equal(unname(centrality_length_scaled_betweenness(p5)["C"]),
               1 / 3 + 1 / 4 + 1 / 2 + 1 / 3)
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

test_that("adjacent pairs contribute nothing rather than dividing by zero", {
  # Every pair in a triangle is adjacent, so no node brokers anything and
  # the (d - 1)^-delta weight is never evaluated at d = 1.
  tri <- matrix(1, 3, 3)
  diag(tri) <- 0
  rownames(tri) <- colnames(tri) <- LETTERS[1:3]
  v <- centrality_delta_betweenness(tri, betweenness_delta = 2)
  expect_true(all(v == 0))
  expect_false(anyNA(v))
})

# ===========================================================================
# Ego betweenness
# ===========================================================================

test_that("ego betweenness is betweenness inside the ego network", {
  # The star's centre is the only path between each of its 6 leaf pairs.
  expect_equal(unname(centrality_ego_betweenness(star5)["A"]), 6)
  # A node with fewer than two neighbours brokers nothing.
  leaves <- centrality_ego_betweenness(star5)[c("B", "C", "D", "E")]
  expect_true(all(leaves == 0))
})

test_that("ego betweenness matches the Everett-Borgatti matrix shortcut", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  set.seed(4)
  g <- igraph::sample_gnp(16, 0.3)
  ref <- vapply(seq_len(igraph::vcount(g)), function(v) {
    nb <- as.integer(igraph::neighbors(g, v, mode = "all"))
    if (length(nb) < 2) return(0)
    ids <- sort(unique(c(v, nb)))
    sub <- igraph::induced_subgraph(g, ids)
    a <- as.matrix(igraph::as_adjacency_matrix(sub))
    m <- (a %*% a) * (1 - a)
    ego <- which(ids == v)
    m <- m[-ego, -ego, drop = FALSE]
    keep <- upper.tri(m) & m > 0
    sum(1 / m[keep])
  }, numeric(1))
  expect_equal(centrality(g, measures = "ego_betweenness")$ego_betweenness, ref)
})

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

test_that("delta = 0 counts the reachable set", {
  # Every node of the connected six-node graph reaches the other five.
  expect_true(all(centrality_delta_closeness(adj6, closeness_delta = 0) == 1))
})

test_that("a large delta approaches degree over n - 1", {
  v <- centrality_delta_closeness(adj6, closeness_delta = 40)
  expect_equal(unname(v), unname(centrality_degree(adj6)) / 5)
})

test_that("unreachable nodes contribute nothing but stay in the denominator", {
  # Two disjoint edges: each node reaches exactly one other at distance 1.
  m <- matrix(0, 4, 4)
  m[1, 2] <- m[2, 1] <- m[3, 4] <- m[4, 3] <- 1
  rownames(m) <- colnames(m) <- LETTERS[1:4]
  expect_equal(unname(centrality_delta_closeness(m)), rep(1 / 3, 4))
})

# ===========================================================================
# Gravity family
# ===========================================================================

test_that("the gravity default is Ma et al. 2016", {
  # k-shell mass at both ends, squared distance, truncated at 3.
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  ks <- igraph::coreness(g)
  d <- igraph::distances(g, weights = NA)
  ref <- vapply(seq_len(igraph::vcount(g)), function(i) {
    j <- setdiff(seq_len(igraph::vcount(g)), i)
    keep <- is.finite(d[i, j]) & d[i, j] > 0 & d[i, j] <= 3
    sum(ks[i] * ks[j][keep] / d[i, j][keep]^2)
  }, numeric(1))
  expect_equal(centrality(g, measures = "gravity")$gravity_all, ref)
})

test_that("the mass and radius arguments select the published variants", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  deg <- igraph::degree(g)
  d <- igraph::distances(g, weights = NA)
  gm <- vapply(seq_len(igraph::vcount(g)), function(i) {
    j <- setdiff(seq_len(igraph::vcount(g)), i)
    keep <- is.finite(d[i, j]) & d[i, j] > 0
    sum(deg[i] * deg[j][keep] / d[i, j][keep]^2)
  }, numeric(1))
  expect_equal(centrality(g, measures = "gravity", gravity_mass = "degree",
                          gravity_radius = NULL)$gravity_all, gm)
  # The local model truncates; its radius is half the mean distance.
  auto <- centrality(g, measures = "gravity", gravity_mass = "degree",
                     gravity_radius = "auto")$gravity_all
  expect_true(all(auto <= gm))
  expect_false(isTRUE(all.equal(auto, gm)))
})

test_that("the legacy mass reproduces the pre-2.4.8 values", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  deg <- igraph::degree(g)
  ks <- igraph::coreness(g)
  d <- igraph::distances(g, weights = NA)
  old <- vapply(seq_len(igraph::vcount(g)), function(i) {
    j <- setdiff(seq_len(igraph::vcount(g)), i)
    keep <- is.finite(d[i, j]) & d[i, j] > 0
    sum((deg[j] * ks[j])[keep] / d[i, j][keep]^2)
  }, numeric(1))
  expect_equal(centrality(g, measures = "gravity", gravity_mass = "legacy",
                          gravity_radius = NULL)$gravity_all, old)
  # And it is not what the default returns any more.
  expect_false(isTRUE(all.equal(centrality(g, measures = "gravity")$gravity_all,
                                old)))
})

# ===========================================================================
# Bounded-distance betweenness needs no measure of its own
# ===========================================================================

test_that("cutoff computes bounded-distance (k-)betweenness", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  # Written from the definition: pairs no further apart than k, credited by
  # the share of their geodesics that pass through the node.
  set.seed(5)
  g <- igraph::sample_gnp(11, 0.3)
  n <- igraph::vcount(g)
  d <- igraph::distances(g)
  adj <- as.matrix(igraph::as_adjacency_matrix(g)) != 0
  sigma <- matrix(0, n, n)
  diag(sigma) <- 1
  for (src in seq_len(n)) {
    for (v in order(d[src, ])) {
      if (v == src || !is.finite(d[src, v])) next
      pr <- which(adj[, v] & d[src, ] == d[src, v] - 1)
      sigma[src, v] <- sum(sigma[src, pr])
    }
  }
  for (k in c(2, 3)) {
    ref <- vapply(seq_len(n), function(i) {
      tot <- 0
      for (j in seq_len(n)) for (l in seq_len(n)) {
        if (j == l || i == j || i == l) next
        if (!is.finite(d[j, l]) || d[j, l] > k) next
        if (isTRUE(all.equal(d[j, i] + d[i, l], d[j, l])) && sigma[j, l] > 0) {
          tot <- tot + sigma[j, i] * sigma[i, l] / sigma[j, l]
        }
      }
      tot / 2
    }, numeric(1))
    expect_equal(centrality(g, measures = "betweenness",
                            cutoff = k)$betweenness, ref, info = k)
  }
})

# ===========================================================================
# Wiring
# ===========================================================================

test_that("the new measures are listed and reach type = all", {
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
