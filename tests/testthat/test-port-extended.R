# Hand-computed checks for the igraph-free port of R/centrality-extended.R.
#
# Every expected value below is derived by hand from the definition on a
# graph from tests/testthat/networks/degenerate.rds, so a kernel and its
# wiring are checked against arithmetic, not against another implementation.

fixtures <- readRDS(testthat::test_path("networks", "degenerate.rds"))
nets <- fixtures$matrices

# Measures implemented in R/centrality-extended.R (flow_betweenness stays on
# igraph by design and is checked separately).
ported_measures <- c(
  "stress", "lobby", "radiality", "lin", "decay", "residual_closeness",
  "dangalchev", "generalized_closeness", "harary", "average_distance",
  "barycenter", "closeness_vitality", "wiener", "communicability",
  "communicability_betweenness", "random_walk", "entropy", "semilocal",
  "clusterrank", "bottleneck", "centroid", "mnc", "dmnc",
  "topological_coefficient", "bridging", "local_bridging", "effective_size",
  "diversity", "cross_clique", "markov", "integration", "expected",
  "gilschmidt", "salsa", "leaderrank", "lac", "participation",
  "within_module_z", "gateway", "onion", "second_order",
  "collective_influence", "local_hindex", "infection",
  "expected_influence_1", "expected_influence_2", "nonbacktracking",
  "trophic_level", "hindex_strength", "spanning_tree", "katz", "hubbell",
  "information", "pairwisedis", "reaching_local", "prestige_domain",
  "prestige_domain_proximity", "brokerage_coordinator",
  "brokerage_itinerant", "brokerage_representative", "brokerage_gatekeeper",
  "brokerage_liaison"
)

score <- function(m, measure, ...) {
  res <- suppressWarnings(suppressMessages(centrality(m, measures = measure, ...)))
  col <- grep(paste0("^", measure), names(res), value = TRUE)
  as.numeric(res[[col]])
}

test_that("radiality on path_10: (diam + 1 - d) summed over all nodes, over n - 1", {
  # Path 1-2-...-10, diameter 9. Node 1 sees distances 0..9, so the sum of
  # (10 - d) is 10 + 9 + ... + 1 = 55; node 5 sees 4,3,2,1,0,1,2,3,4,5 giving
  # 6 + 7 + 8 + 9 + 10 + 9 + 8 + 7 + 6 + 5 = 75. Both are divided by 9.
  rad <- score(nets$path_10, "radiality")
  expect_equal(rad[1], 55 / 9)
  expect_equal(rad[5], 75 / 9)
  expect_equal(rad[10], 55 / 9)
})

test_that("cross_clique on complete_6 counts every subset containing the node", {
  # A node of K6 lies in one clique per subset of the other five nodes: 2^5.
  expect_equal(score(nets$complete_6, "cross_clique"), rep(32, 6))
})

test_that("bottleneck on star_10 credits only the hub", {
  # From each leaf, the hub sits on all 9 non-trivial shortest paths
  # (> 10 / 4), every other leaf on exactly one. From the hub nobody
  # qualifies. So the hub scores 9, the leaves 0.
  expect_equal(score(nets$star_10, "bottleneck"), c(9, rep(0, 9)))
})

test_that("effective_size lists a directed self-loop once, as as_adj_list() does", {
  # duplicate_edges: arcs 1->1, 1->2, 2->3, 3->1. Neighbour lists under
  # mode "all" (loop once): 1: {1,2,3}, 2: {1,3}, 3: {1,2}.
  # Node 1: k = 3, shared 3 + 2 + 2 = 7, size 3 - 7/3 = 2/3.
  # Node 2: k = 2, shared 2 + 1 = 3, size 2 - 3/2 = 1/2. Node 3 likewise.
  expect_equal(score(nets$duplicate_edges, "effective_size"), c(2 / 3, 1 / 2, 1 / 2))
})

test_that("lac under mode in counts the loop once and keeps it in the subgraph", {
  # duplicate_edges in-neighbours: 1: {1,3}; 2: {1}; 3: {2}.
  # Node 1: subgraph {1,3} has arcs 1->1 and 3->1, in-degrees 2 and 0: 2/2 = 1.
  # Node 2: subgraph {1} keeps the loop, in-degree 1: 1/1 = 1.
  # Node 3: subgraph {2} has no arc: 0.
  expect_equal(score(nets$duplicate_edges, "lac", mode = "in"), c(1, 1, 0))
})

test_that("reaching_local looks edge weights up in path order", {
  # one_way_only: a weighted chain 1->2->3->4. Under mode "out" the paths
  # from 1 average w12, (w12 + w23) / 2 and (w12 + w23 + w34) / 3; each
  # node's score is that sum over n - 1 = 3. Under mode "in" the traversed
  # arcs run the other way, so the a->b lookup finds nothing and every
  # score is 0.
  m <- nets$one_way_only
  w <- as.numeric(t(m))
  w <- w[w > 0]
  w12 <- w[1]; w23 <- w[2]; w34 <- w[3]
  expect_equal(
    score(m, "reaching_local", mode = "out"),
    c(w12 + (w12 + w23) / 2 + (w12 + w23 + w34) / 3,
      w23 + (w23 + w34) / 2,
      w34,
      0) / 3)
  expect_equal(score(m, "reaching_local", mode = "in"), rep(0, 4))
})

test_that("weighted stress equals unweighted stress under constant weights", {
  # 4-cycle 1-2, 1-3, 2-4, 3-4: the pair (1, 4) has two shortest paths, one
  # through 2 and one through 3; the pair (2, 3) likewise through 1 and 4.
  # Every node therefore lies on exactly one shortest path.
  diamond <- matrix(0, 4, 4)
  diamond[cbind(c(1, 1, 2, 3), c(2, 3, 4, 4))] <- 1
  diamond <- diamond + t(diamond)
  expect_equal(score(diamond, "stress"), c(1, 1, 1, 1))
  expect_equal(score(diamond * 2.5, "stress", weighted = TRUE), c(1, 1, 1, 1))
})

test_that("centralization matches Freeman's formula on a star and a path", {
  # Star: every degree score is 1 except the hub's 9; sum of gaps 9 * 8 = 72
  # over (n - 1)(n - 2) = 72.
  expect_equal(centralization(nets$star_10, "degree"), 1)
  # Path 1-...-10: degrees 1,2,...,2,1; max gap sum 2 over 72.
  expect_equal(centralization(nets$path_10, "degree"), 2 / 72)
  # Betweenness on the star: hub carries all 36 pairs, leaves none;
  # 36 * 9 over (n - 1)^2 (n - 2) / 2 = 324.
  expect_equal(centralization(nets$star_10, "betweenness"), 1)
})

test_that("every ported measure runs without reaching igraph", {
  skip_if_not_installed("igraph")
  old_opts <- options(cograph.forbid_igraph = TRUE)
  on.exit(options(old_opts), add = TRUE)
  m <- nets$strongly_connected_8
  leaked <- Filter(function(mm) {
    r <- tryCatch(suppressWarnings(centrality(m, measures = mm)),
                  error = function(e) e)
    inherits(r, "cograph_igraph_leak")
  }, ported_measures)
  expect_identical(leaked, character(0))
})

test_that("flow_betweenness is the one measure that still needs igraph", {
  skip_if_not_installed("igraph")
  old_opts <- options(cograph.forbid_igraph = TRUE)
  on.exit(options(old_opts), add = TRUE)
  expect_error(centrality(nets$star_10, measures = "flow_betweenness"),
               class = "cograph_igraph_leak")
})

test_that("calculate_* accept an igraph object as well as a context", {
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_adjacency_matrix(nets$star_10, mode = "undirected")
  expect_equal(cograph:::calculate_bottleneck(g, mode = "all"), c(9L, rep(0L, 9)))
})
