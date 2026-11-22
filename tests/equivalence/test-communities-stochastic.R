# The stochastic community algorithms: community_louvain(), community_leiden(), community_infomap(),
# community_label_propagation(), community_spinglass(), community_fluid(), community_consensus() and their com_*()
# aliases. cograph hands each to igraph, whose random draws no second program can share, so igraph is not
# evidence and no partition is compared draw for draw. What is checked holds for ANY draw, over five seeds each:
#
# - the modularity a result reports equals NetworkX's modularity() of the partition it returns (Newman 2006);
# - on a ring of four 5-cliques joined by single edges, the partition of highest modularity (Q = 0.659, against
#   0.455 for pairs of cliques) is the four cliques: Louvain, Leiden (modularity) and Infomap return it;
# - Leiden's guarantee (Traag et al. 2019): every community it returns induces a connected subgraph;
# - Leiden with the CPM objective at resolution 1 on a graph whose weights are all below 1 keeps every node
#   alone (joining i and j changes the quality by w_ij - 1 < 0);
# - fluid communities returns exactly the number of communities asked for (Pares et al. 2018);
# - consensus clustering of the ring: every run finds the four cliques, so the consensus graph is the four
#   disjoint cliques, its walktrap partition is the cliques and the modularity reported (computed on the
#   consensus graph, as documented) is 1 - 4 (1/4)^2 = 0.75.
#
# A probe of 40 seeds (10 for consensus) before this file was written: the ring was recovered 40 of 40 times by
# Louvain, Leiden and Infomap (label propagation 37, spinglass 38, fluid 26: not used), every Leiden community was
# connected 40 of 40 times on both graphs, fluid gave 3 communities 40 of 40 times, consensus recovered the ring
# 10 of 10 times for each method, and the reported modularity equalled the partition's within 3e-16 on 20 of 20
# seeds for every algorithm except spinglass on a weighted graph (cograph 2.7.18 reported igraph's Potts-model
# value there; corrected in 2.7.19).

karate <- ref_karate()
weighted <- ref_connected_graph(16, density = 0.3, seed = 21L, weighted = TRUE, digits = 6L)
cliques <- rep(1:4, each = 5)
ring <- local({
  graph <- outer(cliques, cliques, "==") * 1
  diag(graph) <- 0
  bridges <- cbind(c(5, 10, 15, 20), c(6, 11, 16, 1))
  graph[bridges] <- 1
  graph[bridges[, 2:1]] <- 1
  dimnames(graph) <- list(paste0("c", 1:20), paste0("c", 1:20))
  graph
})
# The weighted graph scaled so that every weight is below 1 (its ring ties weigh 1).
below_one <- weighted * 0.9
ring_truth <- ref_canonical(stats::setNames(cliques, rownames(ring)))
seeds <- 1:5
exact <- 1e-10

#' NetworkX's modularity of a returned partition
nx_modularity <- function(graph, result, weight = "'weight'") {
  ref_networkx(graph, c(q = sprintf("nx.community.modularity(G, %s, weight=%s)", py_partition(membership(result)),
                                    weight)))$q
}

#' Whether each community of a partition induces a connected subgraph (reachability inside the community)
connected_parts <- function(graph, groups) {
  binary <- vr_neighbours(graph)
  vapply(split(names(groups), groups), \(members) {
    hops <- vr_hops(binary[members, members, drop = FALSE])
    all(is.finite(hops))
  }, logical(1))
}

test_that("the modularity Louvain, Infomap and label propagation report is that of their partition", {
  algorithms <- list(louvain = list(community_louvain, com_lv, "community_louvain()", "com_lv()"),
                     infomap = list(community_infomap, com_im, "community_infomap()", "com_im()"),
                     label_propagation = list(community_label_propagation, com_lp, "community_label_propagation()",
                                              "com_lp()"))
  lapply(names(algorithms), \(name) {
    run <- algorithms[[name]]
    lapply(seeds, \(seed) {
      on_karate <- run[[1L]](karate, seed = seed)
      on_weighted <- run[[2L]](weighted, seed = seed)
      expect_reference(attr(on_karate, "modularity"), nx_modularity(karate, on_karate, "None"), reference = "networkx",
                       covers = run[[3L]], quantity = sprintf("reported modularity, %s, karate, seed %d", name, seed),
                       tolerance = exact)
      expect_reference(attr(on_weighted, "modularity"), nx_modularity(weighted, on_weighted), reference = "networkx",
                       covers = c(run[[4L]], run[[3L]]),
                       quantity = sprintf("reported modularity, %s, weighted, seed %d", name, seed), tolerance = exact)
    })
  })
})

test_that("the modularity spinglass reports is that of its partition", {
  lapply(seeds, \(seed) {
    found <- community_spinglass(karate, seed = seed)
    expect_reference(attr(found, "modularity"), nx_modularity(karate, found, "None"), reference = "networkx",
                     covers = "community_spinglass()", quantity = sprintf("reported modularity, spinglass, karate, seed %d", seed),
                     tolerance = exact)
  })
  alias <- com_sg(karate, seed = 6)
  expect_reference(attr(alias, "modularity"), nx_modularity(karate, alias, "None"), reference = "networkx",
                   covers = c("com_sg()", "community_spinglass()"), quantity = "reported modularity, com_sg(), karate, seed 6",
                   tolerance = exact)
  lapply(1:2, \(seed) {
    found <- com_sg(weighted, seed = seed)
    expect_reference(attr(found, "modularity"), nx_modularity(weighted, found), reference = "networkx",
                     covers = c("com_sg()", "community_spinglass()"),
                     quantity = sprintf("reported modularity, spinglass, weighted, seed %d", seed), tolerance = exact)
  })
})

test_that("Louvain, Leiden and Infomap return the four cliques of the ring", {
  algorithms <- list(louvain = \(seed) community_louvain(ring, seed = seed),
                     com_lv = \(seed) com_lv(ring, seed = seed),
                     leiden = \(seed) community_leiden(ring, objective_function = "modularity", seed = seed),
                     com_ld = \(seed) com_ld(ring, objective_function = "modularity", seed = seed),
                     infomap = \(seed) community_infomap(ring, seed = seed),
                     com_im = \(seed) com_im(ring, seed = seed))
  covers <- list(louvain = "community_louvain()", com_lv = c("com_lv()", "community_louvain()"),
                 leiden = "community_leiden()", com_ld = c("com_ld()", "community_leiden()"),
                 infomap = "community_infomap()", com_im = c("com_im()", "community_infomap()"))
  lapply(names(algorithms), \(name) lapply(seeds, \(seed) {
    expect_reference(ref_canonical(membership(algorithms[[name]](seed)), rownames(ring)), ring_truth,
                     reference = "independent: the four cliques, the partition of highest modularity of the ring",
                     covers = covers[[name]], quantity = sprintf("partition of the ring of cliques, %s, seed %d", name, seed),
                     tolerance = 0)
  }))
})

test_that("every Leiden community induces a connected subgraph, and CPM below weight 1 keeps nodes alone", {
  lapply(seeds, \(seed) {
    on_karate <- membership(community_leiden(karate, objective_function = "modularity", seed = seed))
    on_weighted <- membership(com_ld(weighted, objective_function = "modularity", seed = seed))
    expect_reference(connected_parts(karate, on_karate), rep(TRUE, length(unique(on_karate))),
                     reference = "independent: Leiden's guarantee of connected communities (Traag et al. 2019)",
                     covers = "community_leiden()", quantity = sprintf("each community connected, karate, seed %d", seed),
                     tolerance = 0)
    expect_reference(connected_parts(weighted, on_weighted), rep(TRUE, length(unique(on_weighted))),
                     reference = "independent: Leiden's guarantee of connected communities (Traag et al. 2019)",
                     covers = c("com_ld()", "community_leiden()"),
                     quantity = sprintf("each community connected, weighted, seed %d", seed), tolerance = 0)
    expect_reference(ref_canonical(membership(community_leiden(below_one, seed = seed)), rownames(below_one)),
                     stats::setNames(rownames(below_one), rownames(below_one)),
                     reference = "independent: CPM quality at resolution 1 with every weight below 1",
                     covers = "community_leiden()", quantity = sprintf("CPM partition, weighted, seed %d", seed),
                     tolerance = 0)
  })
})

test_that("fluid communities returns the number of communities asked for", {
  lapply(seeds, \(seed) {
    found <- ref_with_seed(seed, community_fluid(karate, no.of.communities = 3))
    alias <- ref_with_seed(seed + 5L, com_fl(karate, no.of.communities = 3))
    expect_reference(n_communities(found), 3, reference = "independent: fluid communities keeps k communities (Pares et al. 2018)",
                     covers = "community_fluid()", quantity = sprintf("number of communities, k = 3, karate, seed %d", seed),
                     tolerance = 0)
    expect_reference(n_communities(alias), 3, reference = "independent: fluid communities keeps k communities (Pares et al. 2018)",
                     covers = c("com_fl()", "community_fluid()"),
                     quantity = sprintf("com_fl() number of communities, k = 3, karate, seed %d", seed + 5L), tolerance = 0)
    expect_reference(sort(names(membership(found))), sort(rownames(karate)),
                     reference = "independent: a partition places every node once", covers = "community_fluid()",
                     quantity = sprintf("nodes of the partition, karate, seed %d", seed), tolerance = 0)
  })
})

test_that("consensus clustering of the ring returns the cliques and the consensus graph's modularity", {
  methods <- c("louvain", "label_propagation", "infomap")
  lapply(methods, \(method) lapply(1:3, \(seed) {
    found <- community_consensus(ring, method = method, n_runs = 20, seed = seed)
    expect_reference(ref_canonical(membership(found), rownames(ring)), ring_truth,
                     reference = "independent: the four cliques, the partition of highest modularity of the ring",
                     covers = "community_consensus()",
                     quantity = sprintf("consensus partition of the ring, %s, seed %d", method, seed), tolerance = 0)
    expect_reference(attr(found, "modularity"), 1 - 4 * (1 / 4)^2,
                     reference = "independent: modularity of four disjoint equal cliques, 1 - 4 (1/4)^2",
                     covers = "community_consensus()",
                     quantity = sprintf("consensus modularity of the ring, %s, seed %d", method, seed), tolerance = exact)
  }))
  alias <- com_consensus(ring, n_runs = 20, seed = 9)
  expect_reference(ref_canonical(membership(alias), rownames(ring)), ring_truth,
                   reference = "independent: the four cliques, the partition of highest modularity of the ring",
                   covers = c("com_consensus()", "community_consensus()"), quantity = "com_consensus() partition of the ring",
                   tolerance = 0)
})
