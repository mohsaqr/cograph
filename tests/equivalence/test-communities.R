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
