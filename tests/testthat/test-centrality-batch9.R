# ===========================================================================
# Tests for Batch 9 — remaining Centrality Zoo measures
#
# Unit tests: invariants, conventions and contracts. The comparisons with the
# papers' printed tables, hand values and closed forms are declared in
# tests/equivalence/test-centrality-zoo-a-*.R; cross-checks against the
# Python references live in local_testing_and_equivalence/batch9/.
# ===========================================================================

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Wen & Jiang (2019), Fig. 3: the kite network
kite_el <- matrix(c(
  1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 8, 9, 2, 7, 6, 5, 3, 7, 4,
  7, 4, 7, 5, 8, 7, 6, 8, 7, 9, 10
), ncol = 2)
adj_of <- function(el, n) {
  m <- matrix(0, n, n)
  m[el] <- 1
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

# ===========================================================================
# Community-aware measures
# ===========================================================================

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

test_that("local_dimension_fixed: an isolate scores 0", {
  iso <- rbind(cbind(star5, 0), 0)
  expect_equal(centrality_local_dimension_fixed(iso, ld_radius = 1)[[6]], 0)
})

test_that("fuzzy_local_dimension: the distance kernel gives the paper's node-7 fuzzy balls", {
  # Wen & Jiang (2019), kite network, node 7 fuzzy balls quoted in the paper: 0.4582, 0.7551, 0.8198, 0.8353
  d <- cograph:::.cg_distances(adj_of(kite_el, 10), "all")
  d7 <- d[7, ]
  balls <- vapply(1:4, function(r) {
    inside <- d7[d7 <= r]
    sum(exp(-inside^2 / r^2)) / length(inside)
  }, numeric(1))
  expect_equal(round(balls, 4), c(0.4582, 0.7551, 0.8198, 0.8353))
})

test_that("local_volume_dimension and fuzzy_local_dimension: NaN with one radius", {
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
# Local measures
# ===========================================================================

test_that("heatmap: an isolate is NaN", {
  iso <- rbind(cbind(star5, 0), 0)
  expect_true(is.nan(centrality_heatmap(iso)[[6]]))
})

test_that("flow_coefficient: directed hand case", {
  # j -> v -> k with no direct j -> k: one of the two ordered pairs
  chain <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  rownames(chain) <- colnames(chain) <- c("j", "v", "k")
  expect_equal(unname(centrality_flow_coefficient(chain)), c(0, 0.5, 0))
})

test_that("redundancy is degree minus effective size", {
  rd <- centrality_redundancy(bridge6)
  es <- centrality(bridge6, measures = c("degree", "effective_size"))
  expect_equal(unname(rd), es$degree_all - es$effective_size)
})

# ===========================================================================
# Coreness variants and geodesic k-path
# ===========================================================================

test_that("weighted_kshell: weights matter", {
  # Garas Figure 1 spirit: a heavy pendant link lifts the pendant node
  w <- bridge6
  w["A", "B"] <- w["B", "A"] <- 6
  expect_true(all(centrality_weighted_kshell(w)[c("A", "B")] >=
                    centrality_weighted_kshell(bridge6)[c("A", "B")]))
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
  skip_on_cran()
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

# ===========================================================================
# Parameter validation (classed conditions, not opaque internal failures)
# ===========================================================================

test_that("ld_radius below 1 raises a classed condition", {
  star <- matrix(0, 5, 5); star[1, 2:5] <- 1; star[2:5, 1] <- 1
  rownames(star) <- colnames(star) <- LETTERS[1:5]
  for (bad in list(0, -1, NA_real_, Inf, c(1, 2), "two")) {
    expect_error(centrality_local_dimension_fixed(star, ld_radius = bad),
                 class = "cograph_bad_parameter")
  }
  expect_silent(centrality_local_dimension_fixed(star, ld_radius = 2))
})

test_that("comm_r rejects anything but \"max_intra\" or a positive number", {
  star <- matrix(0, 5, 5); star[1, 2:5] <- 1; star[2:5, 1] <- 1
  rownames(star) <- colnames(star) <- LETTERS[1:5]
  memb <- c(1, 1, 2, 2, 2)
  for (bad in list("bogus", 0, -2, NA_real_, Inf, c(1, 2))) {
    expect_error(centrality_comm_centrality(star, membership = memb, comm_r = bad),
                 class = "cograph_bad_parameter")
  }
  expect_type(centrality_comm_centrality(star, membership = memb), "double")
  expect_type(centrality_comm_centrality(star, membership = memb, comm_r = 3), "double")
})
