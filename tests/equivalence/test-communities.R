# Community detection: communities(), detect_communities(), the community_*() functions and their com_*()
# aliases, n_communities(), community_sizes() and compare_communities().
#
# cograph hands every algorithm to igraph's cluster_*() and compare(), so igraph is not independent evidence
# here. The deterministic algorithms are compared with a second implementation: NetworkX's
# greedy_modularity_communities() (Clauset, Newman and Moore 2004, the algorithm of igraph's fast greedy) and
# girvan_newman() (the edge-betweenness division, cut at maximum modularity as igraph does), and the base-R
# programs of helper-d2.R written from the papers: Newman's (2006) leading eigenvector, Pons and Latapy's
# (2005) walktrap, and the exhaustive search over every partition for the optimum of modularity. The modularity
# each result reports is compared with NetworkX's modularity() of the same partition.
#
# Partitions are compared label-free: each node is labelled by the first node, in the graph's node order, of
# its community (ref_canonical()), so two programs that number the communities differently still agree when their
# communities are the same.
#
# Not here: louvain, leiden, infomap, label propagation, spinglass, fluid and consensus draw random numbers
# inside igraph and offer no argument that supplies the draws to a second program.
#
# Graphs: Zachary's karate club (unweighted, the classical benchmark) and connected random graphs with
# six-decimal weights, on which two merges or divisions almost never tie.

karate <- ref_karate()
weighted <- ref_connected_graph(16, density = 0.3, seed = 21L, weighted = TRUE, digits = 6L)
weighted_large <- ref_connected_graph(20, density = 0.2, seed = 8L, weighted = TRUE, digits = 6L)
small_weighted <- ref_connected_graph(9, density = 0.35, seed = 5L, weighted = TRUE, digits = 6L)

# Partition labels are words: equal or not. Modularity is one sum of at most n^2 products: rounding only.
exact <- 1e-10

test_that("fast greedy finds NetworkX's greedy_modularity_communities() partition", {
  nx_karate <- ref_networkx(karate, c(
    membership = "{n: i for i, c in enumerate(nx.community.greedy_modularity_communities(G)) for n in c}"))
  nx_weighted <- ref_networkx(weighted, c(
    membership = "{n: i for i, c in enumerate(nx.community.greedy_modularity_communities(G, weight='weight')) for n in c}"))
  expect_reference(ref_canonical(membership(community_fast_greedy(karate))), ref_canonical(nx_karate$membership, rownames(karate)),
                   reference = "networkx", covers = "community_fast_greedy()",
                   quantity = "fast greedy partition, karate club", tolerance = 0)
  expect_reference(ref_canonical(membership(com_fg(weighted))), ref_canonical(nx_weighted$membership, rownames(weighted)),
                   reference = "networkx", covers = c("com_fg()", "community_fast_greedy()"),
                   quantity = "fast greedy partition, weighted", tolerance = 0)
  expect_reference(ref_canonical(membership(communities(weighted, method = "fast_greedy"))),
                   ref_canonical(nx_weighted$membership, rownames(weighted)), reference = "networkx", covers = "communities()",
                   quantity = "communities(method = 'fast_greedy') partition, weighted", tolerance = 0)
  expect_reference(ref_canonical(membership(detect_communities(weighted, method = "fast_greedy"))),
                   ref_canonical(nx_weighted$membership, rownames(weighted)), reference = "networkx", covers = "detect_communities()",
                   quantity = "detect_communities(method = 'fast_greedy') partition, weighted", tolerance = 0)
})

test_that("the number and sizes of communities equal NetworkX's partition", {
  nx <- ref_networkx(weighted, c(
    count = "len(nx.community.greedy_modularity_communities(G, weight='weight'))",
    sizes = "sorted(len(c) for c in nx.community.greedy_modularity_communities(G, weight='weight'))"))
  found <- community_fast_greedy(weighted)
  expect_reference(n_communities(found), nx$count, reference = "networkx", covers = "n_communities()",
                   quantity = "number of fast greedy communities, weighted", tolerance = 0)
  expect_reference(sort(community_sizes(found)), nx$sizes, reference = "networkx", covers = "community_sizes()",
                   quantity = "fast greedy community sizes, smallest first, weighted", tolerance = 0)
})

test_that("edge betweenness finds NetworkX's girvan_newman() level of highest modularity", {
  # igraph keeps the division of highest modularity along the edge removals; the same choice is made here over
  # NetworkX's hierarchy. Unweighted: igraph reads weights as lengths, NetworkX's default as none.
  nx <- ref_networkx(karate, c(membership = paste0(
    "{n: i for i, c in enumerate(max(nx.community.girvan_newman(G), ",
    "key=lambda p: nx.community.modularity(G, p))) for n in c}")))
  expect_reference(ref_canonical(membership(community_edge_betweenness(karate))), ref_canonical(nx$membership, rownames(karate)),
                   reference = "networkx", covers = "community_edge_betweenness()",
                   quantity = "edge betweenness partition, karate club", tolerance = 0)
  expect_reference(ref_canonical(membership(com_eb(karate))), ref_canonical(nx$membership, rownames(karate)),
                   reference = "networkx", covers = c("com_eb()", "community_edge_betweenness()"),
                   quantity = "com_eb() partition, karate club", tolerance = 0)
})

test_that("leading eigenvector equals Newman's (2006) repeated division, written from the paper", {
  reference <- "independent: Newman (2006) leading-eigenvector division, base R eigen()"
  expect_reference(ref_canonical(membership(community_leading_eigenvector(karate))),
                   ref_canonical(ref_leading_eigenvector(karate)), reference = reference,
                   covers = "community_leading_eigenvector()", quantity = "leading eigenvector partition, karate club",
                   tolerance = 0)
  expect_reference(ref_canonical(membership(com_le(weighted))), ref_canonical(ref_leading_eigenvector(weighted)),
                   reference = reference, covers = c("com_le()", "community_leading_eigenvector()"),
                   quantity = "leading eigenvector partition, weighted", tolerance = 0)
  expect_reference(ref_canonical(membership(communities(weighted, method = "leading_eigenvector"))),
                   ref_canonical(ref_leading_eigenvector(weighted)), reference = reference, covers = "communities()",
                   quantity = "communities(method = 'leading_eigenvector') partition, weighted", tolerance = 0)
})

test_that("walktrap equals Pons and Latapy's (2005) agglomeration, written from the paper", {
  reference <- "independent: Pons and Latapy (2005) walktrap, t = 4, base R"
  on_weighted <- ref_walktrap(weighted)
  on_large <- ref_walktrap(weighted_large)
  expect_reference(ref_canonical(membership(community_walktrap(weighted))), ref_canonical(on_weighted$membership),
                   reference = reference, covers = "community_walktrap()", quantity = "walktrap partition, weighted",
                   tolerance = 0)
  expect_reference(ref_canonical(membership(com_wt(weighted_large))), ref_canonical(on_large$membership),
                   reference = reference, covers = c("com_wt()", "community_walktrap()"),
                   quantity = "walktrap partition, weighted, 20 nodes", tolerance = 0)
  expect_reference(ref_canonical(membership(detect_communities(weighted, method = "walktrap"))),
                   ref_canonical(on_weighted$membership), reference = reference, covers = "detect_communities()",
                   quantity = "detect_communities(method = 'walktrap') partition, weighted", tolerance = 0)
  expect_reference(attr(community_walktrap(weighted), "modularity"), on_weighted$modularity, reference = reference,
                   covers = "community_walktrap()", quantity = "walktrap modularity, weighted", tolerance = exact)
})

test_that("the optimal partition equals the exhaustive search over all partitions", {
  # 9 nodes: the 21147 partitions (Bell number B(9)) are enumerated and the modularity of each computed.
  reference <- "independent: modularity of every partition of 9 nodes (Newman 2006), base R"
  best <- ref_optimal_modularity(small_weighted)
  expect_reference(ref_canonical(membership(community_optimal(small_weighted))), ref_canonical(best$membership),
                   reference = reference, covers = "community_optimal()", quantity = "optimal partition, weighted",
                   tolerance = 0)
  expect_reference(attr(com_op(small_weighted), "modularity"), best$modularity, reference = reference,
                   covers = c("com_op()", "community_optimal()"), quantity = "maximum modularity, weighted",
                   tolerance = exact)
})

test_that("the modularity each deterministic method reports equals NetworkX's modularity() of its partition", {
  found <- list(fast_greedy = community_fast_greedy(weighted), walktrap = community_walktrap(weighted),
                leading_eigenvector = community_leading_eigenvector(weighted),
                edge_betweenness = community_edge_betweenness(karate))
  graphs <- list(fast_greedy = weighted, walktrap = weighted, leading_eigenvector = weighted,
                 edge_betweenness = karate)
  # The edge-betweenness partition of the unweighted karate club: NetworkX's unweighted modularity.
  weight_argument <- c(fast_greedy = "'weight'", walktrap = "'weight'", leading_eigenvector = "'weight'",
                       edge_betweenness = "None")
  nx <- vapply(names(found), \(method) ref_networkx(graphs[[method]], c(q = sprintf(
    "nx.community.modularity(G, %s, weight=%s)", py_partition(membership(found[[method]])),
    weight_argument[[method]])))$q, numeric(1))
  reported <- vapply(found, \(result) attr(result, "modularity"), numeric(1))
  expect_reference(reported, nx, reference = "networkx",
                   covers = c("community_fast_greedy()", "community_walktrap()", "community_leading_eigenvector()",
                              "community_edge_betweenness()"),
                   quantity = "reported modularity", tolerance = exact)
})

test_that("compare_communities() equals the definitions of VI, NMI, split-join, Rand and adjusted Rand", {
  # igraph::compare() does the computation; the reference is each index written from its definition on the
  # contingency table (helper-d2.R).
  first <- community_fast_greedy(karate)
  second <- community_leading_eigenvector(karate)
  methods <- c("vi", "nmi", "split.join", "rand", "adjusted.rand")
  scores <- vapply(methods, \(method) compare_communities(first, second, method = method), numeric(1))
  expect_reference(scores, ref_partition_scores(membership(first), membership(second)[names(membership(first))]),
                   reference = "independent: partition indices from the contingency table, base R",
                   covers = "compare_communities()", quantity = "fast greedy against leading eigenvector, karate club",
                   tolerance = exact)
  plain_first <- c(1, 1, 2, 2, 3, 3, 3, 1, 2)
  plain_second <- c(2, 2, 2, 1, 1, 3, 3, 3, 1)
  plain <- vapply(methods, \(method) compare_communities(plain_first, plain_second, method = method), numeric(1))
  expect_reference(plain, ref_partition_scores(plain_first, plain_second),
                   reference = "independent: partition indices from the contingency table, base R",
                   covers = "compare_communities()", quantity = "two membership vectors of 9 nodes", tolerance = exact)
})

# n_communities() on the result of every deterministic method, on several graphs: unweighted (karate club, a
# random graph) and six-decimal weighted, connected and of two components. The count of the reference partition
# is computed by the reference (NetworkX, or the base-R programs of helper-d2.R), never by cograph.
#
# Two inputs were tested for ties by jitter: the same graph with every weight multiplied by 1 + 1e-9 u,
# u ~ U(0, 1) symmetric, seeds 1 to 10, both programs given the identical jittered weights.
# - Unweighted fast greedy on ref_connected_graph(20, 0.15, seed = 33): igraph 4 communities (Q = 0.2575),
#   NetworkX 3 (Q = 0.2627). Jittered, igraph and NetworkX return the same partition on 10 of 10 seeds, and the
#   partition moves with the seed (3 communities, Q = 0.3109 on 5 seeds; 3, Q = 0.2627 on 1; 4, Q = 0.2728 on 3;
#   4, Q = 0.3067 on 1). The algorithm leaves the order of equal merges undefined: not comparable, and the jittered
#   counts are compared.
# - Walktrap on the karate club: igraph 5 communities (Q = 0.3532), helper-d2 4 (Q = 0.4198). Jittered, both sides
#   keep their partition on 10 of 10 seeds (5, Q = 0.3532 against 4, Q = 0.4198): not ties. Explained (2026-11-22):
#   igraph runs the authors' program, whose lazy delta-sigma estimates do not always merge the pair of smallest
#   delta sigma; written as ref_walktrap_program(), that rule gives igraph's partition (the last test of this file).
# The stochastic methods (louvain, leiden, infomap, label propagation, spinglass, fluid, consensus) draw inside
# igraph, and no second program can be given the draws.

two_components <- function(first, second) {
  n <- nrow(first) + nrow(second)
  graph <- matrix(0, n, n)
  graph[seq_len(nrow(first)), seq_len(nrow(first))] <- first
  graph[nrow(first) + seq_len(nrow(second)), nrow(first) + seq_len(nrow(second))] <- second
  dimnames(graph) <- list(paste0("N", seq_len(n)), paste0("N", seq_len(n)))
  graph
}
plain <- ref_connected_graph(20, density = 0.15, seed = 33L)
weighted_24 <- ref_connected_graph(24, density = 0.2, seed = 55L, weighted = TRUE, digits = 6L)
split_weighted <- two_components(ref_connected_graph(9, 0.35, 41L, weighted = TRUE, digits = 6L),
                                 ref_connected_graph(8, 0.4, 42L, weighted = TRUE, digits = 6L))
split_plain <- two_components(ref_connected_graph(10, 0.3, 43L), ref_connected_graph(7, 0.4, 44L))
small_split <- two_components(ref_connected_graph(5, 0.5, 45L, weighted = TRUE, digits = 6L),
                              ref_connected_graph(4, 0.5, 46L, weighted = TRUE, digits = 6L))
count_of <- \(graphs, method) vapply(graphs, \(graph) n_communities(method(graph)), integer(1))

test_that("n_communities() of fast greedy equals the count of NetworkX's greedy_modularity_communities()", {
  graphs <- list(karate = karate, weighted_16 = weighted, weighted_20 = weighted_large, weighted_24 = weighted_24,
                 two_components_weighted = split_weighted, two_components_unweighted = split_plain)
  nx <- vapply(graphs, \(graph) as.integer(ref_networkx(graph, c(count = sprintf(
    "len(nx.community.greedy_modularity_communities(G, weight=%s))",
    if (all(graph %in% c(0, 1))) "None" else "'weight'")))$count), integer(1))
  expect_reference(count_of(graphs, community_fast_greedy), nx, reference = "networkx", covers = "n_communities()",
                   quantity = "number of fast greedy communities", tolerance = 0)
})

test_that("n_communities() of edge betweenness equals the count of NetworkX's girvan_newman() level of highest modularity", {
  # The candidates are the components of the whole graph (igraph's first level, which girvan_newman() does not
  # yield) and every level girvan_newman() yields. On weighted graphs igraph reads the weights as lengths for
  # betweenness and as weights for modularity; the NetworkX side is told the same.
  level <- \(weight) sprintf(paste0(
    "len(max([list(nx.connected_components(G))] + list(nx.community.girvan_newman(G%s)), ",
    "key=lambda p: nx.community.modularity(G, p, weight=%s)))"),
    if (is.null(weight)) "" else paste0(", most_valuable_edge=lambda H: max(nx.edge_betweenness_centrality(",
                                        "H, weight='weight').items(), key=lambda kv: kv[1])[0]"),
    if (is.null(weight)) "None" else "'weight'")
  unweighted <- list(karate = karate, random_20 = plain, two_components = split_plain)
  weighted_graphs <- list(weighted_16 = weighted, two_components = split_weighted)
  nx_unweighted <- vapply(unweighted, \(graph) as.integer(ref_networkx(graph, c(count = level(NULL)))$count), integer(1))
  nx_weighted <- vapply(weighted_graphs, \(graph) as.integer(ref_networkx(graph, c(count = level("weight")))$count),
                        integer(1))
  expect_reference(count_of(unweighted, community_edge_betweenness), nx_unweighted, reference = "networkx",
                   covers = "n_communities()", quantity = "number of edge betweenness communities, unweighted",
                   tolerance = 0)
  expect_reference(count_of(weighted_graphs, community_edge_betweenness), nx_weighted, reference = "networkx",
                   covers = "n_communities()", quantity = "number of edge betweenness communities, weighted",
                   tolerance = 0)
})

test_that("n_communities() of leading eigenvector, walktrap and optimal equals the papers' algorithms", {
  eigen_graphs <- list(karate = karate, random_20 = plain, weighted_16 = weighted, weighted_20 = weighted_large,
                       two_components_weighted = split_weighted, two_components_unweighted = split_plain)
  expect_reference(count_of(eigen_graphs, community_leading_eigenvector),
                   vapply(eigen_graphs, \(graph) length(unique(ref_leading_eigenvector(graph))), integer(1)),
                   reference = "independent: Newman (2006) leading-eigenvector division, base R eigen()",
                   covers = "n_communities()", quantity = "number of leading eigenvector communities", tolerance = 0)
  # ref_walktrap() needs a connected graph.
  walk_graphs <- list(weighted_16 = weighted, weighted_20 = weighted_large, weighted_24 = weighted_24)
  expect_reference(count_of(walk_graphs, community_walktrap),
                   vapply(walk_graphs, \(graph) length(unique(ref_walktrap(graph)$membership)), integer(1)),
                   reference = "independent: Pons and Latapy (2005) walktrap, t = 4, base R",
                   covers = "n_communities()", quantity = "number of walktrap communities", tolerance = 0)
  optimal_graphs <- list(weighted_9 = small_weighted, two_components_9 = small_split)
  expect_reference(count_of(optimal_graphs, community_optimal),
                   vapply(optimal_graphs, \(graph) length(unique(ref_optimal_modularity(graph)$membership)), integer(1)),
                   reference = "independent: modularity of every partition of 9 nodes (Newman 2006), base R",
                   covers = "n_communities()", quantity = "number of optimal communities", tolerance = 0)
})

jittered <- \(graph, seed) {
  noise <- ref_with_seed(seed, matrix(1e-9 * stats::runif(length(graph)), nrow(graph)))
  noise[lower.tri(noise)] <- t(noise)[lower.tri(noise)]
  graph * (1 + noise)
}

test_that("n_communities() of unweighted fast greedy depends on the order of tied merges; jittered it equals NetworkX's", {
  nx_count <- \(graph, weight) as.integer(ref_networkx(graph, c(count = sprintf(
    "len(nx.community.greedy_modularity_communities(G, weight=%s))", weight)))$count)
  expect_not_comparable(n_communities(community_fast_greedy(plain)), nx_count(plain, "None"), reference = "networkx",
                        covers = "n_communities()", quantity = "number of fast greedy communities, unweighted random graph",
                        reason = paste(
                          "with equal weights many merges tie and the algorithm leaves their order undefined: igraph",
                          "stops at 4 communities (Q = 0.2575), NetworkX at 3 (Q = 0.2627); with weights 1 + 1e-9 u",
                          "(seeds 1 to 10) both return the same partition on 10 of 10 seeds, which moves with the seed",
                          "(3 or 4 communities, Q 0.2627 to 0.3109)"))
  graphs <- stats::setNames(lapply(1:10, \(seed) jittered(plain, seed)), sprintf("jitter seed %d", 1:10))
  expect_reference(count_of(graphs, community_fast_greedy), vapply(graphs, nx_count, integer(1), weight = "'weight'"),
                   reference = "networkx", covers = "n_communities()",
                   quantity = "number of fast greedy communities, random graph with weights 1 + 1e-9 u", tolerance = 0)
})

test_that("walktrap on the unweighted karate club follows the merge rule of Pons and Latapy's program", {
  # The exact-minimum agglomeration (ref_walktrap(), the paper's rule) and the program's lazy rule
  # (ref_walktrap_program(), the authors' C++ code that igraph runs) part at the 21st merge of the karate club with
  # weights 1 + 1e-3 u (seed 1): the program's stored estimate for {1, 20} with {3, 14} is above the exact delta sigma (1.80e-5), so it merges
  # {5, 6, 7, 11} (2.03e-5) instead. Probe 2026-11-22: the program rule gave igraph's whole merge sequence on the
  # karate club with weights 1 + 1e-9 u and 1 + 1e-3 u (seeds 1 to 10, 20 of 20) and on five random weighted graphs
  # of 16 to 40 nodes (5 of 5); the exact rule on 0 of 20 and 4 of 5. Not ties: the 1e-3 jitter separates them.
  program <- "independent: Pons and Latapy's walktrap program, its lazy delta-sigma merge rule, base R"
  on_karate <- ref_walktrap_program(karate)
  found <- community_walktrap(karate)
  expect_reference(ref_canonical(membership(found)), ref_canonical(on_karate$membership), reference = program,
                   covers = c("community_walktrap()", "com_wt()"), quantity = "walktrap partition, karate club",
                   tolerance = 0)
  expect_reference(n_communities(found), length(unique(on_karate$membership)), reference = program,
                   covers = "n_communities()", quantity = "number of walktrap communities, karate club", tolerance = 0)
  expect_reference(attr(found, "modularity"), on_karate$modularity, reference = program,
                   covers = "community_walktrap()", quantity = "walktrap modularity, karate club", tolerance = exact)
  # Weights 1 + 1e-3 u: no two merges tie, the two rules still part.
  graphs <- stats::setNames(lapply(1:5, \(seed) {
    noise <- ref_with_seed(seed, matrix(1e-3 * stats::runif(length(karate)), nrow(karate)))
    noise[lower.tri(noise)] <- t(noise)[lower.tri(noise)]
    karate * (1 + noise)
  }), sprintf("seed %d", 1:5))
  lapply(names(graphs), \(name) {
    expect_reference(ref_canonical(membership(com_wt(graphs[[name]]))),
                     ref_canonical(ref_walktrap_program(graphs[[name]])$membership), reference = program,
                     covers = c("com_wt()", "community_walktrap()"),
                     quantity = sprintf("walktrap partition, karate club with weights 1 + 1e-3 u, %s", name), tolerance = 0)
  })
  expect_known_difference(n_communities(found), length(unique(ref_walktrap(karate)$membership)),
                          reference = "independent: Pons and Latapy (2005) walktrap, t = 4, base R",
                          covers = "n_communities()", quantity = "number of walktrap communities, karate club, exact rule",
                          tolerance = 0, agrees_with = "the merge rule of the authors' program (ref_walktrap_program())",
                          reason = paste(
                            "igraph runs the authors' program, which after a merge stores an estimate of delta sigma for a",
                            "community adjacent to only one of the two merged and makes it exact only when it is the",
                            "smallest; an estimate above the exact value keeps a pair from merging. On the karate club with",
                            "weights 1 + 1e-3 u (seed 1) the program merges {5, 6, 7, 11} (2.03e-5) at step 21 where the exact minimum is {1, 20} with",
                            "{3, 14} (1.80e-5); unweighted, 5 communities (Q = 0.3532) against 4 (Q = 0.4198)"))
})
