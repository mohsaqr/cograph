# The voting heuristics that no installed package implements: WVoteRank (Sun et al. 2019), NCVoteRank (Kumar
# and Panda 2020), VoteRank++ (Liu et al. 2021) and EnRenew (Guo et al. 2020). Each election is written again from
# its paper in base R (helper-voting.R): the scores, the winner, the suppression of the winner's neighbourhood,
# round after round, so the comparison is of the whole order of election. The definitions, and where a paper is
# silent, follow the source audits of the owner's centrality batches (local_testing_and_equivalence/batch8,
# batch9: SPEC.md of each measure); WVoteRank's Figure 1 is also compared as published.
#
# Graphs: Zachary's karate club, a connected random graph, a weighted connected random graph (six-decimal
# weights, so that two weighted scores rarely tie), and the ten-node toy of Sun et al. (2019), Figure 1.

defined <- "independent: the election written from the paper in base R (helper-voting.R)"
# Sums and square roots of at most n terms per round: rounding only.
exact <- 1e-10

karate <- ref_karate()
connected <- ref_connected_graph(14, density = 0.25, seed = 71L)
weighted <- ref_connected_graph(14, density = 0.25, seed = 72L, weighted = TRUE, digits = 6L)
# Sun et al. (2019), Figure 1: 10 nodes, 12 edges, w(2, 7) = 4 and w(8, 9) = 2, every other weight 1.
toy <- local({
  ends <- rbind(c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(1, 6), c(7, 2), c(7, 3), c(7, 4), c(7, 5), c(6, 8), c(8, 9),
                c(8, 10))
  graph <- matrix(0, 10, 10, dimnames = list(paste0("v", 1:10), paste0("v", 1:10)))
  graph[ends] <- c(1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 2, 1)
  graph[ends[, 2:1]] <- graph[ends]
  graph
})

test_that("WVoteRank elects the nodes in the order of its definition", {
  covers <- c('centrality(measures = "wvoterank")', "centrality_wvoterank()")
  expect_reference(centrality_wvoterank(toy), vr_wvoterank(toy), reference = defined, covers = covers,
                   quantity = "wvoterank, Figure 1 toy of Sun et al. (2019)", tolerance = exact)
  expect_reference(centrality_wvoterank(weighted), vr_wvoterank(weighted), reference = defined, covers = covers,
                   quantity = "wvoterank, weighted connected random graph", tolerance = exact)
  expect_reference(centrality_wvoterank(karate), vr_wvoterank(karate), reference = defined, covers = covers,
                   quantity = "wvoterank, karate", tolerance = exact)
  expect_reference(centrality(weighted, measures = "wvoterank"), ref_nodes(wvoterank = vr_wvoterank(weighted)),
                   reference = defined, covers = covers, quantity = "centrality(measures = 'wvoterank'), weighted",
                   tolerance = exact, key = "node")
})

test_that("WVoteRank elects v7, v1, v8 first on the toy of Sun et al. (2019), Figure 1", {
  # The paper: v7 wins round 1 (s = 5.29), v1 round 2 (s = 4.33), v8 round 3; of 10 nodes they score 1, 0.9, 0.8.
  expect_reference(centrality_wvoterank(toy)[c("v7", "v1", "v8")], c(v7 = 1, v1 = 0.9, v8 = 0.8),
                   reference = "published: Sun, Chen, He and Ch'ng (2019), Figure 1", covers = "centrality_wvoterank()",
                   quantity = "first three elected, Figure 1 toy", tolerance = exact)
})

test_that("NCVoteRank elects the nodes in the order of its definition", {
  covers <- c('centrality(measures = "ncvoterank")', "centrality_ncvoterank()")
  expect_reference(centrality_ncvoterank(karate), vr_ncvoterank(karate), reference = defined, covers = covers,
                   quantity = "ncvoterank, theta 0.5, karate", tolerance = exact)
  expect_reference(centrality_ncvoterank(connected), vr_ncvoterank(connected), reference = defined, covers = covers,
                   quantity = "ncvoterank, theta 0.5, connected random graph", tolerance = exact)
  expect_reference(centrality_ncvoterank(connected, ncvote_theta = 0.2), vr_ncvoterank(connected, theta = 0.2),
                   reference = defined, covers = covers, quantity = "ncvoterank, theta 0.2, connected random graph",
                   tolerance = exact)
})

test_that("VoteRank++ elects the nodes in the order of its definition", {
  covers <- c('centrality(measures = "voterank_plus")', "centrality_voterank_plus()")
  expect_reference(centrality_voterank_plus(karate), vr_voterank_plus(karate), reference = defined, covers = covers,
                   quantity = "voterank_plus, lambda 0.1, karate", tolerance = exact)
  expect_reference(centrality_voterank_plus(connected), vr_voterank_plus(connected), reference = defined,
                   covers = covers, quantity = "voterank_plus, lambda 0.1, connected random graph", tolerance = exact)
  expect_reference(centrality_voterank_plus(connected, voterank_lambda = 0.4), vr_voterank_plus(connected, lambda = 0.4),
                   reference = defined, covers = covers, quantity = "voterank_plus, lambda 0.4, connected random graph",
                   tolerance = exact)
})

test_that("EnRenew elects the nodes in the order of its definition", {
  covers <- c('centrality(measures = "enrenew")', "centrality_enrenew()")
  expect_reference(centrality_enrenew(karate), vr_enrenew(karate), reference = defined, covers = covers,
                   quantity = "enrenew, depth 2, karate", tolerance = exact)
  expect_reference(centrality_enrenew(connected), vr_enrenew(connected), reference = defined, covers = covers,
                   quantity = "enrenew, depth 2, connected random graph", tolerance = exact)
  expect_reference(centrality_enrenew(connected, enrenew_depth = 1), vr_enrenew(connected, depth = 1L),
                   reference = defined, covers = covers, quantity = "enrenew, depth 1, connected random graph",
                   tolerance = exact)
})
