# ===========================================================================
# Tests for Batch 8 — Centrality Zoo "on the way" batch
#
# Unit tests: invariants, conventions and contracts. The comparisons with
# brute-force definitions, hand-computed values and the worked examples of
# the source papers are declared in tests/equivalence/test-centrality-zoo-a-*.R;
# cross-checks against the Python references live in
# local_testing_and_equivalence/batch8/.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test graphs and helpers
# ---------------------------------------------------------------------------

# Inputs or oracles in this file are built with igraph; without it the
# file is skipped as a whole (the igraph-free proof is the golden and port tests).
skip_if_not_installed("igraph")

star5 <- matrix(0, 5, 5)
star5[1, 2:5] <- 1
star5 <- star5 + t(star5)
rownames(star5) <- colnames(star5) <- LETTERS[1:5]

path5 <- matrix(0, 5, 5)
path5[cbind(1:4, 2:5)] <- 1
path5 <- path5 + t(path5)
rownames(path5) <- colnames(path5) <- LETTERS[1:5]

# Two triangles (A,B,C) and (D,E,F) joined by the bridge C -- D
bridge6 <- matrix(0, 6, 6)
bridge6[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
bridge6 <- bridge6 + t(bridge6)
rownames(bridge6) <- colnames(bridge6) <- LETTERS[1:6]
bridge_membership <- c(1, 1, 1, 2, 2, 2)

# ===========================================================================
# Shapley value games
# ===========================================================================

test_that("shapley closed forms equal exact coalition enumeration", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  set.seed(808)
  # Sequential sweep over random graphs; each is an independent expectation.
  for (i in 1:24) {
    directed <- i %% 2 == 0
    g <- igraph::sample_gnp(sample(4:7, 1), 0.45, directed = directed)
    if (i %% 6 == 0) g <- igraph::add_vertices(g, 1)   # isolate
    if (i %% 8 == 0) g <- igraph::add_edges(g, c(1, 1)) # self-loop
    b <- cograph:::.cg_path_matrix(g, NULL)
    d <- cograph:::.cg_distances(b, "out")
    for (game in 1:3) {
      closed <- cograph:::.cg_shapley(b, game, k = 2, cutoff = 2, d = d)
      exact <- cograph:::.cg_shapley_exact(b, game, k = 2, cutoff = 2, d = d)
      expect_equal(closed, exact, tolerance = 1e-10,
                   info = sprintf("graph %d game %d", i, game))
      expect_equal(sum(closed), nrow(b), info = "efficiency")
    }
  }
})

test_that("shapley: efficiency and game reductions on the star", {
  g1 <- centrality_shapley_game1(star5)
  expect_equal(sum(g1), 5)
  # k = 1 reduces game 2 to game 1; cutoff 1 reduces game 3 to game 1
  expect_equal(centrality_shapley_game2(star5, shapley_k = 1), g1)
  expect_equal(centrality_shapley_game3(star5, shapley_cutoff = 1), g1)
})

# ===========================================================================
# Access and hide information
# ===========================================================================

test_that("access/hide: the totals agree", {
  a <- centrality_access_information(star5)
  h <- centrality_hide_information(star5)
  # Both sum the same search-information matrix
  expect_equal(sum(a), sum(h))
})

test_that("access/hide stay finite on disconnected graphs", {
  skip_if_not_installed("igraph")
  g <- igraph::add_vertices(igraph::make_graph("Zachary"), 1)
  df <- centrality(g, measures = c("access_information", "hide_information"))
  expect_false(anyNA(df$access_information))
  expect_true(all(is.finite(df$access_information)))
  expect_equal(df$access_information[35], 0)
  # The isolate does not change anyone else's value
  ref <- centrality(igraph::make_graph("Zachary"),
                    measures = "access_information")
  expect_equal(df$access_information[1:34], ref$access_information)
})

# ===========================================================================
# Rumor centrality
# ===========================================================================

test_that("rumor: an isolate scores log 1 and leaves the tree alone", {
  # The tree of Shah & Zaman (2011), Fig. 5 (edges 1-2, 1-3, 2-4, 2-5)
  adj <- matrix(0, 5, 5)
  adj[cbind(c(1, 1, 2, 2), c(2, 3, 4, 5))] <- 1
  adj <- adj + t(adj)
  iso <- rbind(cbind(adj, 0), 0)
  expect_equal(centrality_rumor(iso)[[6]], 0)
  expect_equal(centrality_rumor(iso)[1:5], centrality_rumor(adj))
})

# ===========================================================================
# Community hub-bridge
# ===========================================================================

test_that("community_hub_bridge: membership contract", {
  expect_warning(w <- centrality(bridge6, measures = "community_hub_bridge"),
                 "membership")
  expect_true(all(is.na(w$community_hub_bridge_all)))
  expect_error(centrality(bridge6, measures = "community_hub_bridge",
                          membership = 1:2),
               class = "cograph_bad_membership")
})

test_that("entropy_variation: an isolate scores 0", {
  iso <- rbind(cbind(star5, 0), 0)
  rownames(iso) <- colnames(iso) <- LETTERS[1:6]
  expect_equal(centrality_entropy_variation(iso)[["F"]], 0)
})

# ===========================================================================
# s-shell index
# ===========================================================================

test_that("s_shell: reference-implementation toy values and star", {
  # Six-node graph of the Liu et al. reference implementation (edges
  # 1-2, 2-3, 1-3, 3-4, 4-5, 5-6), a = 0.5
  toy <- matrix(0, 6, 6)
  toy[cbind(c(1, 2, 1, 3, 4, 5), c(2, 3, 3, 4, 5, 6))] <- 1
  toy <- toy + t(toy)
  aw <- cograph:::.cg_asymmetric_weights(toy, 0.5)
  # w_12 = w_21 = 1 (no out-reaching links), w_13 = w_23 = 1 + sqrt(2),
  # w_31 = w_32 = 1, w_34 = 1 + sqrt(3), w_43 = 3, w_45 = w_54 = 1 + sqrt(2),
  # w_56 = 1, w_65 = 2
  expect_equal(aw$s, c(2 + sqrt(2), 2 + sqrt(2), 3 + sqrt(3),
                       4 + sqrt(2), 2 + sqrt(2), 2))
  expect_equal(unname(centrality_s_shell(toy)), c(3, 3, 3, 3, 2, 1))
  expect_equal(unname(centrality_s_shell(star5)), rep(1, 5))
})

test_that("ncvoterank: theta = 1 without two-hop weakening is VoteRank", {
  g6 <- igraph::graph_from_adjacency_matrix(bridge6, mode = "undirected")
  b <- cograph:::.cg_path_matrix(g6, NULL)
  ks <- igraph::coreness(g6)
  expect_equal(cograph:::.cg_ncvoterank(b, ks, theta = 1, two_hop = FALSE),
               cograph:::.cg_voterank(b, FALSE))
  v <- centrality_ncvoterank(bridge6)
  expect_equal(sort(unname(v)), 1:6 / 6)   # a complete ranking
  # The first elected node has the highest coreness-weighted votes: C or D
  expect_true(names(which.max(v)) %in% c("C", "D"))
})

# ===========================================================================
# Verb integration and invariants
# ===========================================================================

batch8_done <- c("shapley_game1", "shapley_game2", "shapley_game3",
                 "access_information", "hide_information", "rumor",
                 "community_hub_bridge", "entropy_variation_degree",
                 "entropy_variation_betweenness", "s_shell",
                 "degree_discount", "single_discount", "ncvoterank")

test_that("centrality(): batch 8 columns and type = 'all'", {
  skip_on_cran()
  df <- centrality(bridge6, measures = batch8_done,
                   membership = bridge_membership)
  expect_named(df, c("node", "shapley_game1", "shapley_game2", "shapley_game3",
                     "access_information", "hide_information", "rumor",
                     "community_hub_bridge_all", "entropy_variation_degree_all",
                     "entropy_variation_betweenness", "s_shell",
                     "degree_discount", "single_discount", "ncvoterank"))
  expect_false(anyNA(df[-1]))
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

test_that("batch 8 measures are invariant to node relabelling", {
  skip_if_not_installed("igraph")
  set.seed(41)
  g <- igraph::sample_gnp(14, 0.35)
  igraph::V(g)$name <- paste0("n", seq_len(14))
  memb <- sample(1:3, 14, replace = TRUE)
  ref <- centrality(g, measures = batch8_done, membership = memb)
  perm <- sample(14)
  gp <- igraph::permute(g, perm)
  memb_p <- integer(14)
  memb_p[perm] <- memb
  got <- centrality(gp, measures = batch8_done, membership = memb_p)
  got <- got[match(ref$node, got$node), ]
  rownames(got) <- NULL
  # The greedy selection orders break ties by label, so only their score
  # multisets are compared; rumor centrality's BFS tree also follows label
  # order and its values change with it (documented), so it is excluded.
  greedy <- c("degree_discount", "single_discount", "ncvoterank")
  keep <- setdiff(names(ref), c("rumor", greedy))
  expect_equal(got[keep], ref[keep])
  for (m in greedy) expect_equal(sort(got[[m]]), sort(ref[[m]]), info = m)
})

test_that("a negative s_shell_a raises a classed condition", {
  star <- matrix(0, 5, 5); star[1, 2:5] <- 1; star[2:5, 1] <- 1
  rownames(star) <- colnames(star) <- LETTERS[1:5]
  for (bad in list(-1, NA_real_, Inf, c(0.5, 1), "half")) {
    expect_error(centrality_s_shell(star, s_shell_a = bad),
                 class = "cograph_bad_parameter")
  }
  expect_type(centrality_s_shell(star, s_shell_a = 0.5), "integer")
  expect_type(centrality_s_shell(star, s_shell_a = 0), "integer")
})
