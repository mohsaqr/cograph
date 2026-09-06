# ===========================================================================
# Tests for Batch 8 — Centrality Zoo "on the way" batch
#
# Every kernel is checked against an exact brute-force definition written
# independently here (coalition enumeration, spreading-order enumeration,
# all-shortest-paths enumeration) plus hand-computed values and the worked
# examples of the source papers. Cross-checks against the Python
# references live in local_testing_and_equivalence/batch8/.
# ===========================================================================

# ---------------------------------------------------------------------------
# Test graphs and helpers
# ---------------------------------------------------------------------------

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

hop_matrix <- function(g, mode = "out") {
  cograph:::.cg_distances(cograph:::.cg_path_matrix(g, NULL), mode)
}

# Number of SI spreading orders starting at `v`: permutations of the
# nodes that start at v and add, at every step, a node adjacent to an
# already-infected one. Brute force, n <= 6.
spreading_orders <- function(adj, v) {
  n <- nrow(adj)
  grow <- function(infected) {
    if (length(infected) == n) return(1)
    frontier <- which(colSums(adj[infected, , drop = FALSE]) > 0)
    frontier <- setdiff(frontier, infected)
    sum(vapply(frontier, function(w) grow(c(infected, w)), numeric(1L)))
  }
  grow(v)
}

# Search information S(i -> j) by enumerating every shortest path.
search_info_enum <- function(g, i, j) {
  if (i == j) return(0)
  paths <- igraph::all_shortest_paths(g, from = i, to = j)$res
  if (length(paths) == 0) return(Inf)
  k <- igraph::degree(g)
  probs <- vapply(paths, function(p) {
    p <- as.integer(p)
    inner <- p[-c(1, length(p))]
    (1 / k[p[1]]) * prod(1 / (k[inner] - 1))
  }, numeric(1L))
  -log2(sum(probs))
}

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

test_that("shapley: star hand values and game reductions", {
  g1 <- centrality_shapley_game1(star5)
  # The hub gets one fifth for itself plus one half from each leaf; each
  # leaf gets one half for itself plus one fifth from the hub.
  expect_equal(g1[["A"]], 1 / 5 + 4 / 2)
  expect_equal(unname(g1[-1]), rep(1 / 2 + 1 / 5, 4))
  expect_equal(sum(g1), 5)
  # k = 1 reduces game 2 to game 1; cutoff 1 reduces game 3 to game 1
  expect_equal(centrality_shapley_game2(star5, shapley_k = 1), g1)
  expect_equal(centrality_shapley_game3(star5, shapley_cutoff = 1), g1)
  # Paper's worked value: nu2({v1, v2}, k = 2) on its Fig. 2a is checked in
  # the exact oracle; here the default k = 2 on the star:
  g2 <- centrality_shapley_game2(star5)
  expect_equal(g2[["A"]], min(1, 2 / 5) + 4 * max(0, (1 - 2 + 1) / (1 * 2)))
})

# ===========================================================================
# Access and hide information
# ===========================================================================

test_that("search information matches all-shortest-paths enumeration", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  set.seed(51)
  # Sequential sweep over random connected graphs.
  for (i in 1:10) {
    repeat {
      g <- igraph::sample_gnp(sample(7:11, 1), 0.4)
      if (igraph::is_connected(g)) break
    }
    n <- igraph::vcount(g)
    b <- cograph:::.cg_path_matrix(g, NULL)
    s_mat <- cograph:::.cg_search_information(b, hop_matrix(g),
                                              directed = FALSE)
    ref <- outer(seq_len(n), seq_len(n),
                 Vectorize(function(a, z) search_info_enum(g, a, z)))
    expect_equal(s_mat, ref, info = sprintf("graph %d", i))
  }
})

test_that("access/hide: star values from the papers", {
  # Rosvall et al. (2005): hub -> leaf costs log2(k_hub) = 2 bits on K_{1,4}
  g_star <- igraph::graph_from_adjacency_matrix(star5, mode = "undirected")
  b <- cograph:::.cg_path_matrix(g_star, NULL)
  s_mat <- cograph:::.cg_search_information(b, hop_matrix(g_star), FALSE)
  expect_equal(s_mat[1, 2], 2)
  expect_equal(s_mat[2, 1], 0)              # leaf -> hub: one link, k = 1
  expect_equal(s_mat[2, 3], log2(3))        # leaf -> leaf via hub, k_hub - 1
  a <- centrality_access_information(star5)
  h <- centrality_hide_information(star5)
  expect_equal(a[["A"]], 4 * 2 / 5)
  expect_equal(unname(a[-1]), rep(3 * log2(3) / 5, 4))
  expect_equal(h[["A"]], 0)
  expect_equal(unname(h[-1]), rep((2 + 3 * log2(3)) / 5, 4))
  # Hubs have low access and low hide; the totals agree
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

test_that("rumor: exp(log R) counts spreading orders on trees", {
  skip_if_not_installed("igraph")
  set.seed(77)
  # Sequential sweep over random trees, n <= 6.
  for (i in 1:8) {
    tr <- igraph::sample_tree(6)
    adj <- as.matrix(igraph::as_adjacency_matrix(tr))
    got <- exp(centrality(tr, measures = "rumor")$rumor)
    brute <- vapply(1:6, function(v) spreading_orders(adj, v), numeric(1L))
    expect_equal(got, brute, info = sprintf("tree %d", i))
  }
  # Path of 5: endpoints have one order, the middle node 4!/(2*2) = 6
  expect_equal(exp(centrality_rumor(path5)),
               c(A = 1, B = 4, C = 6, D = 4, E = 1))
})

test_that("rumor: Shah & Zaman Fig. 5 value and isolates", {
  # Fig. 5 of the 2011 paper (edges 1-2, 1-3, 2-4, 2-5): "R(1, G) = 5!/(5*3)
  # = 8", with the eight permitted permutations listed; the other nodes
  # follow from eq. 10: R(2) = 12, R(3) = 2, R(4) = R(5) = 3.
  adj <- matrix(0, 5, 5)
  adj[cbind(c(1, 1, 2, 2), c(2, 3, 4, 5))] <- 1
  adj <- adj + t(adj)
  expect_equal(unname(exp(centrality_rumor(adj))), c(8, 12, 2, 3, 3))
  iso <- rbind(cbind(adj, 0), 0)
  expect_equal(centrality_rumor(iso)[[6]], 0)
  expect_equal(centrality_rumor(iso)[1:5], centrality_rumor(adj))
})

# ===========================================================================
# Community hub-bridge
# ===========================================================================

test_that("community_hub_bridge: hand-computed toy and membership contract", {
  v <- centrality_community_hub_bridge(bridge6, membership = bridge_membership)
  # A, B: 3 * 2 intra + 0 ; C: 3 * 2 + 1 other community * 1 inter link
  expect_equal(unname(v), c(6, 6, 7, 7, 6, 6))
  expect_warning(w <- centrality(bridge6, measures = "community_hub_bridge"),
                 "membership")
  expect_true(all(is.na(w$community_hub_bridge_all)))
  expect_error(centrality(bridge6, measures = "community_hub_bridge",
                          membership = 1:2),
               class = "cograph_bad_membership")
})

test_that("community_hub_bridge: mode restricts to out- or in-links", {
  chain <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  rownames(chain) <- colnames(chain) <- LETTERS[1:3]
  out <- centrality_community_hub_bridge(chain, membership = c(1, 1, 2),
                                         mode = "out")
  inn <- centrality_community_hub_bridge(chain, membership = c(1, 1, 2),
                                         mode = "in")
  expect_equal(unname(out), c(2 * 1, 1 * 1, 0))
  expect_equal(unname(inn), c(0, 2 * 1, 1 * 1))
})

# ===========================================================================
# Entropy variation
# ===========================================================================

entropy_nats <- function(f) {
  p <- f[f > 0] / sum(f)
  -sum(p * log(p))
}

test_that("entropy_variation: star and path hand values", {
  # Degree: star f = (4,1,1,1,1); removing the hub leaves no edges (I = 0),
  # removing a leaf leaves f = (3,1,1,1)
  v <- centrality_entropy_variation(star5)
  i_star <- entropy_nats(c(4, 1, 1, 1, 1))
  expect_equal(v[["A"]], i_star)
  expect_equal(unname(v[-1]), rep(i_star - entropy_nats(c(3, 1, 1, 1)), 4))
  # Betweenness on path5: f = (0,3,4,3,0); removing C leaves two edges with
  # zero betweenness (I = 0); removing A leaves a path of 4, f = (0,2,2,0)
  w <- centrality_entropy_variation(path5, of = "betweenness")
  i_path <- entropy_nats(c(3, 4, 3))
  expect_equal(w[["C"]], i_path)
  expect_equal(w[["A"]], i_path - log(2))
  expect_equal(w[["E"]], w[["A"]])
})

test_that("entropy_variation: directed modes and isolates", {
  chain <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0), 3, 3, byrow = TRUE)
  rownames(chain) <- colnames(chain) <- LETTERS[1:3]
  out <- centrality_entropy_variation(chain, mode = "out")
  inn <- centrality_entropy_variation(chain, mode = "in")
  # out-degree (1,1,0): removing C leaves (1,0) -> I = 0; removing A leaves
  # (1,0) -> 0; removing B leaves (0,0) -> 0. So every EnV = I(G) = log 2.
  expect_equal(unname(out), rep(log(2), 3))
  expect_equal(unname(inn), rep(log(2), 3))
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

test_that("s_shell: a = 0 gives the dense rank of the k-core", {
  skip_if_not_installed("igraph")
  set.seed(21)
  # Sequential sweep over random graphs, some with an isolate.
  for (i in 1:12) {
    g <- igraph::sample_gnp(sample(6:16, 1), 0.3)
    if (i %% 4 == 0) g <- igraph::add_vertices(g, 1)
    got <- centrality(g, measures = "s_shell", s_shell_a = 0)$s_shell
    expect_equal(got, as.integer(factor(igraph::coreness(g))),
                 info = sprintf("graph %d", i))
  }
})

# ===========================================================================
# DegreeDiscountIC, SingleDiscount, NCVoteRank
# ===========================================================================

test_that("degree_discount / single_discount: hand-traced selection orders", {
  # Star: hub first, then the leaves (all tied at dd = -1 / d - t = 0) in
  # node order. Scores 1, 4/5, 3/5, 2/5, 1/5.
  expect_equal(unname(centrality_degree_discount(star5)), c(5:1) / 5)
  expect_equal(unname(centrality_single_discount(star5)), c(5:1) / 5)
  # Two triangles + bridge, traced by hand (p = 0.01):
  #  1. C (degree 3, lower index than D). A, B -> -0.01; D -> 0.98.
  #  2. E (still 2, beats D's 0.98). D -> 3 - 4 - 1*2*0.01 = -1.02; F -> -0.01.
  #  3. A (first of A, B, F at -0.01). B -> 2 - 4 = -2.
  #  4. F (-0.01 beats B -2, D -1.02). D -> 3 - 6 = -3.
  #  5. B (-2 beats D -3).  6. D.
  v <- centrality_degree_discount(bridge6)
  expect_equal(names(sort(v, decreasing = TRUE)),
               c("C", "E", "A", "F", "B", "D"))
  expect_equal(unname(sort(v, decreasing = TRUE)), 6:1 / 6)
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
  df <- centrality(bridge6, measures = batch8_done,
                   membership = bridge_membership)
  expect_named(df, c("node", "shapley_game1", "shapley_game2", "shapley_game3",
                     "access_information", "hide_information", "rumor",
                     "community_hub_bridge_all", "entropy_variation_degree_all",
                     "entropy_variation_betweenness", "s_shell",
                     "degree_discount", "single_discount", "ncvoterank"))
  expect_false(anyNA(df[-1]))
  all_df <- suppressWarnings(centrality(bridge6, type = "all",
                                        membership = bridge_membership))
  expect_true(all(names(df)[-1] %in% names(all_df)))
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
