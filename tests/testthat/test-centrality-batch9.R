# ===========================================================================
# Tests for Batch 9 — remaining Centrality Zoo measures
#
# Published fixtures (edge lists reconstructed from the papers' figures by
# the research agents and checked against every printed column), hand
# values, closed forms and brute-force identities. Cross-checks against the
# Python references live in local_testing_and_equivalence/batch9/.
# ===========================================================================

# ---------------------------------------------------------------------------
# Fixtures from the papers
# ---------------------------------------------------------------------------

zhao_el <- matrix(c(
  15, 15, 15, 17, 18, 19, 20, 1, 2, 3, 3, 3, 4, 4, 5, 13, 12, 12, 12, 10, 10,
  10, 11, 14, 7, 7, 1, 2, 2, 2, 2, 20, 17, 16, 20, 16, 16, 16, 16, 2, 21, 4,
  5, 6, 5, 6, 6, 12, 10, 11, 14, 11, 7, 14, 7, 9, 9, 8, 16, 16, 10, 7, 3, 10
), ncol = 2)
zhao_memb <- c(
  1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 1
)
zhao_cbc <- c(
  0.428571, 1.52381, 0.714286, 0.571429, 0.571429, 0.571429, 1.666667,
  0.380952, 0.761905, 1.952381, 1.142857, 1.52381, 0.380952, 1.142857,
  0.857143, 1.714286, 0.571429, 0.285714, 0.285714, 0.952381, 0.142857
)
tulu_el <- matrix(c(
  3, 3, 3, 4, 4, 5, 15, 15, 15, 16, 16, 16, 16, 1, 2, 11, 11, 11, 12, 12, 12,
  14, 14, 10, 9, 8, 8, 8, 26, 26, 26, 26, 25, 28, 28, 28, 22, 22, 22, 23, 24,
  2, 2, 2, 2, 2, 1, 7, 9, 28, 27, 24, 4, 5, 6, 5, 6, 6, 16, 17, 20, 17, 18,
  19, 20, 2, 21, 12, 10, 7, 13, 10, 14, 10, 9, 7, 7, 29, 26, 25, 30, 31, 32,
  25, 32, 22, 27, 23, 27, 23, 24, 24, 27, 6, 10, 7, 16, 22, 16, 8, 26, 8, 25,
  25
), ncol = 2)
tulu_memb <- c(
  4, 4, 1, 1, 1, 1, 2, 3, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5, 4, 6, 6, 6, 3,
  3, 6, 6, 3, 3, 3, 3
)
tulu_cbm <- c(
  0.0192, 0.1505, 0, 0, 0, 0.0312, 0.0659, 0.0659, 0.0265, 0.0347, 0, 0, 0,
  0, 0, 0.0581, 0, 0, 0, 0, 0, 0.0347, 0, 0.0312, 0.0467, 0.0375, 0.0312,
  0.0312, 0, 0, 0, 0
)
tulu_cbc <- c(
  0.2813, 1.0938, 0.375, 0.375, 0.375, 0.4688, 0.9688, 1.0313, 0.6563,
  0.9688, 0.6563, 0.875, 0.2188, 0.6563, 0.5625, 1.125, 0.375, 0.1875,
  0.1875, 0.375, 0.0938, 0.7188, 0.4688, 0.6875, 0.9688, 1.3125, 0.6875,
  0.6875, 0.2188, 0.2188, 0.2188, 0.4375
)
kite_el <- matrix(c(
  1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 8, 9, 2, 7, 6, 5, 3, 7, 4,
  7, 4, 7, 5, 8, 7, 6, 8, 7, 9, 10
), ncol = 2)
kite_fld <- c(
  0.3609, 0.3609, 0.3015, 0.4554, 0.4554, 0.3015, 0.4442, 0.076, 0.0375,
  -0.1163
)
duron_el <- matrix(c(
  1, 2, 3, 3, 3, 3, 4, 5, 6, 6, 6, 8, 8, 9, 9, 10, 10, 10, 12, 3, 3, 4, 5, 6,
  7, 5, 6, 7, 8, 14, 9, 10, 10, 12, 11, 12, 13, 15
), ncol = 2)
duron_chm <- c(
  13.0, 13.0, -6.667, 10.5, 0.667, -7.2, 6.5, -3.0, 0.0, -6.8, 13.0, 1.667,
  13.0, 13.0, 13.0
)
rw2_el <- matrix(c(
  2, 2, 2, 3, 3, 5, 5, 6, 3, 5, 6, 4, 6, 4, 6, 4
), ncol = 2)
rw2_w <- c(
  1.0, 3.0, 1.0, 2.0, 3.0, 7.0, 8.0, 4.0
)
rw2_counts <- c(
  2.0, 2.0, 3.0, 3.0, 2.0
)
rw2_degree <- c(
  5, 6, 13, 18, 16
)

adj_of <- function(el, n) {
  m <- matrix(0, n, n)
  m[el] <- 1
  m <- pmax(m, t(m))
  rownames(m) <- colnames(m) <- paste0("v", seq_len(n))
  m
}
wadj_of <- function(el, w, n) {
  m <- matrix(0, n, n)
  m[el] <- w
  m <- pmax(m, t(m))
  rownames(m) <- colnames(m) <- paste0("v", seq_len(n))
  m
}

star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5 <- star5 + t(star5)
rownames(star5) <- colnames(star5) <- LETTERS[1:5]

path5 <- matrix(0, 5, 5)
path5[cbind(1:4, 2:5)] <- 1
path5 <- path5 + t(path5)
rownames(path5) <- colnames(path5) <- LETTERS[1:5]

bridge6 <- matrix(0, 6, 6)
bridge6[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
bridge6 <- bridge6 + t(bridge6)
rownames(bridge6) <- colnames(bridge6) <- LETTERS[1:6]
bridge_membership <- c(1, 1, 1, 2, 2, 2)

ols_slope <- function(x, y) stats::coef(stats::lm(y ~ x))[[2]]

# ===========================================================================
# Community-aware measures
# ===========================================================================

test_that("community_based reproduces Zhao et al. (2015) Table 1", {
  v <- centrality_community_based(adj_of(zhao_el, 21), membership = zhao_memb)
  expect_equal(unname(v), zhao_cbc, tolerance = 1e-5)
})

test_that("community_based, community_mediator reproduce Tulu (2018) Table 1", {
  a <- adj_of(tulu_el, 32)
  expect_equal(unname(centrality_community_based(a, membership = tulu_memb)),
               tulu_cbc, tolerance = 2e-4)
  expect_equal(unname(centrality_community_mediator(a, membership = tulu_memb)),
               tulu_cbm, tolerance = 5e-4)
})

test_that("community measures: hand values on the two-triangle bridge", {
  cb <- centrality_community_based(bridge6, membership = bridge_membership)
  # A: two links into a community of size 3 -> 6 / 6
  expect_equal(unname(cb), c(1, 1, 1.5, 1.5, 1, 1))
  cm <- centrality_community_mediator(bridge6, membership = bridge_membership)
  # A, B, E, F: all links inside -> 0; C: p = (2/3, 1/3), H = 0.9183,
  # times 3 / 14
  h <- -(2 / 3 * log2(2 / 3) + 1 / 3 * log2(1 / 3))
  expect_equal(unname(cm), c(0, 0, h * 3 / 14, h * 3 / 14, 0, 0))
  cc <- centrality_comm_centrality(bridge6, membership = bridge_membership)
  # mu = mean(0, 0, 1/3) = 1/9; R = max intra = 2; C: in 2/2*2 = 2,
  # out (1/1 * 2)^2 = 4 -> (1 + 1/9) 2 + (1 - 1/9) 4
  mu <- 1 / 9
  expect_equal(cc[["C"]], (1 + mu) * 2 + (1 - mu) * 4)
  expect_equal(cc[["A"]], (1 + mu) * 2)
  cc1 <- centrality_comm_centrality(bridge6, membership = bridge_membership,
                                    comm_r = 1)
  expect_equal(cc1[["C"]], (1 + mu) * 1 + (1 - mu) * 1)
})

test_that("community measures: membership contract", {
  for (m in c("community_based", "comm_centrality", "community_mediator")) {
    expect_warning(w <- centrality(bridge6, measures = m), "membership")
    expect_true(all(is.na(w[[paste0(m, "_all")]])))
    expect_error(centrality(bridge6, measures = m, membership = 1:2),
                 class = "cograph_bad_membership")
  }
})

# ===========================================================================
# Dimension family
# ===========================================================================

test_that("local_dimension_fixed: Silva-Costa path values and isolates", {
  # Centre of a 9-node path at radius r: r * 2 / (2r + 1)
  p9 <- matrix(0, 9, 9)
  p9[cbind(1:8, 2:9)] <- 1
  p9 <- p9 + t(p9)
  for (r in 1:4) {
    v <- centrality(p9, measures = "local_dimension_fixed", ld_radius = r)
    expect_equal(v$local_dimension_fixed_all[5], r * 2 / (2 * r + 1),
                 info = sprintf("r = %d", r))
  }
  # The hub's eccentricity (1) is below the radius -> 0; a leaf sees the
  # three other leaves at distance 2: 2 * 3 / (1 + 1 + 3)
  expect_equal(unname(centrality_local_dimension_fixed(star5)),
               c(0, rep(1.2, 4)))
  iso <- rbind(cbind(star5, 0), 0)
  expect_equal(centrality_local_dimension_fixed(iso, ld_radius = 1)[[6]], 0)
})

test_that("fuzzy_local_dimension reproduces Wen & Jiang (2019) Table 1", {
  v <- centrality_fuzzy_local_dimension(adj_of(kite_el, 10))
  # The paper truncates (toward zero) to four decimals
  expect_equal(trunc(unname(v) * 1e4) / 1e4, kite_fld, tolerance = 1.5e-4)
  # Node 7 fuzzy balls quoted in the paper: 0.4582, 0.7551, 0.8198, 0.8353
  d <- cograph:::.cg_distances(adj_of(kite_el, 10), "all")
  d7 <- d[7, ]
  balls <- vapply(1:4, function(r) {
    inside <- d7[d7 <= r]
    sum(exp(-inside^2 / r^2)) / length(inside)
  }, numeric(1))
  expect_equal(round(balls, 4), c(0.4582, 0.7551, 0.8198, 0.8353))
})

test_that("fuzzy_local_dimension: karate top ten matches the paper's order", {
  skip_if_not_installed("igraph")
  v <- centrality(igraph::make_graph("Zachary"),
                  measures = "fuzzy_local_dimension")$fuzzy_local_dimension_all
  expect_equal(order(-v)[1:10], c(1, 34, 33, 3, 2, 32, 24, 28, 31, 30))
})

test_that("local_volume_dimension: hand slope on a path and NaN cases", {
  # Endpoint A of path5, degrees (1,2,2,2,1): volumes 3, 5, 7, 8 at l = 1..4
  v <- centrality_local_volume_dimension(path5)
  expect_equal(v[["A"]], ols_slope(log(1:4), log(c(3, 5, 7, 8))))
  # Middle node (own degree 2 counts): volumes 6, 8 at l = 1, 2
  expect_equal(v[["C"]], ols_slope(log(1:2), log(c(6, 8))))
  # Star: every node has eccentricity <= 2; the hub has a single radius
  s <- centrality_local_volume_dimension(star5)
  expect_true(is.nan(s[["A"]]))
  expect_true(is.nan(centrality_fuzzy_local_dimension(star5)[["A"]]))
})

# ===========================================================================
# VoteRank family
# ===========================================================================

test_that("wvoterank, enrenew, voterank_plus: hub first, complete rankings", {
  verbs <- list(centrality_wvoterank, centrality_enrenew,
                centrality_voterank_plus)
  for (f in verbs) {
    v <- f(star5)
    expect_equal(names(which.max(v)), "A")
    expect_equal(sort(unname(v)), 1:5 / 5)
  }
  # EnRenew hub entropy on the star: four equal neighbour degrees -> ln 4
  b <- cograph:::.cg_undirected_view(star5)
  deg <- rowSums(b)
  p <- deg / sum(deg[2:5])
  expect_equal(-sum(p[2:5] * log(p[2:5])), log(4))
})

test_that("wvoterank: weights change the election order", {
  w <- bridge6
  w["A", "B"] <- w["B", "A"] <- 10
  unweighted <- centrality_wvoterank(bridge6)
  weighted <- centrality_wvoterank(w)
  expect_equal(names(which.max(unweighted)), "C")
  expect_true(names(which.max(weighted)) %in% c("A", "B"))
})

# ===========================================================================
# Node contraction and two-way random walk
# ===========================================================================

test_that("node_contraction: path closed forms and improved form by hand", {
  # Wang et al. (2011) / arXiv:2509.11659: on a path of n nodes,
  # IMC(end) = 2 / (n + 1), IMC(inner) = 2 (2n - 1) / (n (n + 1))
  v <- centrality_node_contraction(path5)
  expect_equal(unname(v), c(2 / 6, 0.6, 0.6, 0.6, 2 / 6))
  # The line graph of path5 is path4: edge scores 2/5, 0.7, 0.7, 2/5
  e <- c(0.4, 0.7, 0.7, 0.4)
  iimc <- centrality_node_contraction_improved(path5)
  expect_equal(unname(iimc),
               5 / 6 * unname(v) + 1 / 6 * c(e[1], e[1] + e[2], e[2] + e[3],
                                             e[3] + e[4], e[4]))
})

test_that("two_way_rw reproduces the Curado et al. (2022) toy example", {
  w <- wadj_of(rw2_el - 1, rw2_w, 5)   # the toy's nodes are labelled 1..5
  expect_equal(unname(rowSums(w)), rw2_degree)
  expect_equal(unname(centrality_two_way_rw(w)), rw2_counts)
})

# ===========================================================================
# Local measures
# ===========================================================================

test_that("heatmap reproduces Duron (2020) Table 1 and its ranking", {
  v <- centrality_heatmap(adj_of(duron_el, 15))
  expect_equal(unname(v), duron_chm, tolerance = 5e-4)
  expect_equal(order(v), c(6, 10, 3, 8, 9, 5, 12, 7, 4, 1, 2, 11, 13, 14, 15))
  iso <- rbind(cbind(star5, 0), 0)
  expect_true(is.nan(centrality_heatmap(iso)[[6]]))
})

test_that("flow_coefficient: 1 - clustering undirected; directed hand case", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph("Zachary")
  fc <- centrality(g, measures = "flow_coefficient")$flow_coefficient
  cl <- igraph::transitivity(g, type = "local", isolates = "zero")
  deg <- igraph::degree(g)
  expect_equal(fc[deg > 1], 1 - cl[deg > 1])
  # j -> v -> k with no direct j -> k: one of the two ordered pairs
  chain <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  rownames(chain) <- colnames(chain) <- c("j", "v", "k")
  expect_equal(unname(centrality_flow_coefficient(chain)), c(0, 0.5, 0))
})

test_that("local_entropy, weighted_h_index, redundancy: hand values", {
  le <- centrality_local_entropy(star5)
  expect_equal(unname(le), c(0, rep(-4 * log(4), 4)))
  wh <- centrality_weighted_h_index(bridge6)
  # C: weights 3*2 twice, 3*2 twice, 3*3 three times -> sorted 9,9,9,6,6,6,6;
  # six values are at least 6, only three are at least 7 -> h = 6
  expect_equal(wh[["C"]], 6)
  expect_equal(unname(centrality_weighted_h_index(star5)), rep(4, 5))
  rd <- centrality_redundancy(bridge6)
  expect_equal(unname(rd), c(1, 1, 2 / 3, 2 / 3, 1, 1))
  es <- centrality(bridge6, measures = c("degree", "effective_size"))
  expect_equal(unname(rd), es$degree_all - es$effective_size)
})

# ===========================================================================
# Coreness variants and geodesic k-path
# ===========================================================================

test_that("weighted_kshell: unit weights give k-core; weights matter", {
  skip_if_not_installed("igraph")
  set.seed(9)
  for (i in 1:6) {
    g <- igraph::sample_gnp(sample(6:14, 1), 0.3)
    if (i %% 3 == 0) g <- igraph::add_vertices(g, 1)
    expect_equal(centrality(g, measures = "weighted_kshell")$weighted_kshell,
                 igraph::coreness(g), info = sprintf("graph %d", i))
  }
  # Garas Figure 1 spirit: a heavy pendant link lifts the pendant node
  w <- bridge6
  w["A", "B"] <- w["B", "A"] <- 6
  expect_true(all(centrality_weighted_kshell(w)[c("A", "B")] >=
                    centrality_weighted_kshell(bridge6)[c("A", "B")]))
})

test_that("renewed_coreness: only the bridge survives on two triangles", {
  # Triangle links lead nowhere new (D = 0); the bridge has D = 2
  expect_equal(unname(centrality_renewed_coreness(bridge6)),
               c(0, 0, 1, 1, 0, 0))
  k4 <- matrix(1, 4, 4) - diag(4)
  expect_equal(unname(centrality_renewed_coreness(k4)), rep(0, 4))
})

test_that("geodesic_kpath: hand counts", {
  # A reaches B, C, D within 3; B reaches A, C, D, E
  expect_equal(unname(centrality_geodesic_kpath(path5, kpath_k = 3)),
               c(3, 4, 4, 4, 3))
  expect_equal(unname(centrality_geodesic_kpath(star5, kpath_k = 1)),
               c(4, 1, 1, 1, 1))
  expect_equal(unname(centrality_geodesic_kpath(star5, kpath_k = 2)),
               c(4, 4, 4, 4, 4))
  # 4-cycle: two geodesics to the opposite node
  c4 <- matrix(0, 4, 4)
  c4[cbind(1:4, c(2, 3, 4, 1))] <- 1
  c4 <- c4 + t(c4)
  expect_equal(unname(centrality_geodesic_kpath(c4, kpath_k = 2)), rep(4, 4))
})

# ===========================================================================
# Verb integration and invariants
# ===========================================================================

batch9 <- c("community_based", "comm_centrality", "community_mediator",
            "local_dimension_fixed", "fuzzy_local_dimension",
            "local_volume_dimension", "wvoterank", "enrenew", "voterank_plus",
            "node_contraction", "node_contraction_improved", "two_way_rw",
            "heatmap", "flow_coefficient", "local_entropy", "weighted_h_index",
            "redundancy", "weighted_kshell", "renewed_coreness",
            "geodesic_kpath")

test_that("centrality(): batch 9 measures run and appear under type = all", {
  df <- centrality(bridge6, measures = batch9, membership = bridge_membership)
  expect_equal(nrow(df), 6)
  expect_equal(ncol(df), length(batch9) + 1)
  # type = "all" holds back the costly measures; the rest must all appear,
  # and include = "costly" restores them.
  all_df <- suppressWarnings(centrality(bridge6, type = "all",
                                        membership = bridge_membership))
  costly <- list_centralities(costly = TRUE)$measure
  expect_true(all(setdiff(names(df)[-1], costly) %in% names(all_df)))
  everything <- suppressWarnings(centrality(bridge6, type = "all",
                                            include = "costly",
                                            membership = bridge_membership))
  expect_true(all(names(df)[-1] %in% names(everything)))
})

test_that("batch 9 measures are invariant to node relabelling", {
  skip_if_not_installed("igraph")
  set.seed(41)
  repeat {
    g <- igraph::sample_gnp(14, 0.35)
    if (igraph::is_connected(g)) break
  }
  igraph::V(g)$name <- paste0("n", seq_len(14))
  memb <- sample(1:3, 14, replace = TRUE)
  ref <- centrality(g, measures = batch9, membership = memb)
  perm <- sample(14)
  gp <- igraph::permute(g, perm)
  memb_p <- integer(14)
  memb_p[perm] <- memb
  got <- centrality(gp, measures = batch9, membership = memb_p)
  got <- got[match(ref$node, got$node), ]
  rownames(got) <- NULL
  # Election orders follow labels through their tie rule, so only their
  # score multisets are compared; two_way_rw picks the first maximum in
  # label order and its counts can change with a relabelling (documented),
  # so it is excluded.
  greedy <- c("wvoterank", "enrenew", "voterank_plus")
  keep <- setdiff(names(ref), c(greedy, "two_way_rw"))
  expect_equal(got[keep], ref[keep])
  for (m in greedy) expect_equal(sort(got[[m]]), sort(ref[[m]]), info = m)
})
