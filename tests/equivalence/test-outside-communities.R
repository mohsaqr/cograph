# Community detection set beside outside software and a published optimum, in addition to the base-R programs
# of test-communities.R and the invariants of test-communities-stochastic.R:
#
# - the modularity each method reports equals NetworkX's modularity() of the partition it returns (the exact
#   optimum, leading eigenvector, walktrap and the com_*() aliases; Leiden and fluid report none, NA);
# - the optimum of the karate club equals the published maximum modularity, 0.4198 (Brandes et al. 2008; the
#   same value in Aloise et al. 2010);
# - on graphs of clear communities (a ring of cliques, simulab block models with strong blocks) Leiden, Louvain and
#   consensus clustering return NetworkX's Louvain partition, and recover the blocks simulab drew;
# - compare_communities() equals aricode's NMI, ARI and Rand index and mclust's adjusted Rand index, and the
#   variation of information from aricode's entropies.

karate <- ref_karate()
weighted <- ref_connected_graph(16, density = 0.3, seed = 21L, weighted = TRUE, digits = 6L)
small_weighted <- ref_connected_graph(9, density = 0.35, seed = 5L, weighted = TRUE, digits = 6L)
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
# Three blocks of 10 with ties inside at 0.7 and between at 0.01, two draws
planted <- lapply(c(31L, 32L), \(seed) sim_blocks(c(10, 10, 10), seed = seed, within_probability = 0.7,
                                                  between_probability = 0.01))
seeds <- 1:5
# Modularity is one sum of at most n^2 products: rounding only.
exact <- 1e-10

nx_q <- \(graph, result, weight = "'weight'") ref_networkx(graph, c(q = sprintf(
  "nx.community.modularity(G, %s, weight=%s)", py_partition(membership(result)), weight)))$q
nx_louvain <- \(graph, seed) {
  found <- ref_networkx(graph, c(m = sprintf(
    "{n: i for i, c in enumerate(nx.community.louvain_communities(G, seed=%d)) for n in c}", seed)))$m
  ref_canonical(found, rownames(graph))
}

test_that("the modularity the exact optimum reports is NetworkX's modularity of its partition", {
  # Leiden and fluid communities report no modularity (the attribute is NA): their partitions are compared below.
  optimum <- community_optimal(karate)
  alias <- com_op(small_weighted)
  expect_reference(attr(optimum, "modularity"), nx_q(karate, optimum, "None"), reference = "networkx",
                   covers = "community_optimal()", quantity = "reported modularity, optimal, karate", tolerance = exact)
  expect_reference(attr(alias, "modularity"), nx_q(small_weighted, alias), reference = "networkx",
                   covers = c("com_op()", "community_optimal()"), quantity = "reported modularity, com_op(), weighted",
                   tolerance = exact)
})

test_that("the modularity leading eigenvector and walktrap report through their aliases is NetworkX's", {
  found <- list(com_le_karate = com_le(karate), com_le_weighted = com_le(weighted), com_wt_karate = com_wt(karate),
                com_wt_weighted = com_wt(weighted), walktrap_karate = community_walktrap(karate),
                leading_eigenvector_karate = community_leading_eigenvector(karate))
  graphs <- list(com_le_karate = karate, com_le_weighted = weighted, com_wt_karate = karate, com_wt_weighted = weighted,
                 walktrap_karate = karate, leading_eigenvector_karate = karate)
  covers <- list(com_le_karate = c("com_le()", "community_leading_eigenvector()"),
                 com_le_weighted = c("com_le()", "community_leading_eigenvector()"),
                 com_wt_karate = c("com_wt()", "community_walktrap()"), com_wt_weighted = c("com_wt()", "community_walktrap()"),
                 walktrap_karate = "community_walktrap()", leading_eigenvector_karate = "community_leading_eigenvector()")
  lapply(names(found), \(name) {
    weight <- if (identical(graphs[[name]], karate)) "None" else "'weight'"
    expect_reference(attr(found[[name]], "modularity"), nx_q(graphs[[name]], found[[name]], weight), reference = "networkx",
                     covers = covers[[name]], quantity = sprintf("reported modularity, %s", name), tolerance = exact)
  })
})

test_that("the optimal partition of the karate club has the published maximum modularity", {
  # Published to four decimals: the tolerance is half a unit of the fourth.
  expect_reference(round(attr(community_optimal(karate), "modularity"), 4), 0.4198,
                   reference = "published: Brandes et al. 2008, maximum modularity of Zachary's karate club",
                   covers = "community_optimal()", quantity = "maximum modularity, karate club", tolerance = 5e-5)
  expect_reference(round(attr(com_op(karate), "modularity"), 4), 0.4198,
                   reference = "published: Brandes et al. 2008, maximum modularity of Zachary's karate club",
                   covers = c("com_op()", "community_optimal()"), quantity = "com_op() maximum modularity, karate club",
                   tolerance = 5e-5)
  expect_reference(n_communities(community_optimal(karate)), 4,
                   reference = "published: Brandes et al. 2008, maximum modularity of Zachary's karate club",
                   covers = c("community_optimal()", "n_communities()"),
                   quantity = "number of communities of the optimum, karate club", tolerance = 0)
})

test_that("on graphs of clear communities Leiden, Louvain and consensus return NetworkX's Louvain partition", {
  lapply(seeds, \(seed) {
    expect_reference(ref_canonical(membership(community_leiden(ring, objective_function = "modularity", seed = seed)),
                                   rownames(ring)), nx_louvain(ring, seed), reference = "networkx",
                     covers = "community_leiden()", quantity = sprintf("leiden partition of the ring of cliques, seed %d", seed),
                     tolerance = 0)
    expect_reference(ref_canonical(membership(community_consensus(ring, method = "louvain", n_runs = 20, seed = seed)),
                                   rownames(ring)), nx_louvain(ring, seed), reference = "networkx",
                     covers = c("community_consensus()", "com_consensus()"),
                     quantity = sprintf("consensus partition of the ring of cliques, seed %d", seed), tolerance = 0)
    lapply(seq_along(planted), \(draw) {
      graph <- planted[[draw]]$graph
      expect_reference(ref_canonical(membership(com_ld(graph, objective_function = "modularity", seed = seed)), rownames(graph)),
                       nx_louvain(graph, seed), reference = "networkx", covers = c("com_ld()", "community_leiden()"),
                       quantity = sprintf("leiden partition of simulab block model %d, seed %d", draw, seed), tolerance = 0)
      expect_reference(ref_canonical(membership(com_consensus(graph, n_runs = 20, seed = seed)), rownames(graph)),
                       nx_louvain(graph, seed), reference = "networkx", covers = c("com_consensus()", "community_consensus()"),
                       quantity = sprintf("consensus partition of simulab block model %d, seed %d", draw, seed),
                       tolerance = 0)
    })
  })
})

test_that("community detection recovers the blocks simulab drew", {
  # Blocks of 10 with ties at 0.7 inside and 0.01 between: the blocks are the partition of highest modularity.
  methods <- list(leiden = \(graph, seed) community_leiden(graph, objective_function = "modularity", seed = seed),
                  louvain = \(graph, seed) community_louvain(graph, seed = seed),
                  consensus = \(graph, seed) community_consensus(graph, n_runs = 20, seed = seed),
                  walktrap = \(graph, seed) community_walktrap(graph),
                  leading_eigenvector = \(graph, seed) community_leading_eigenvector(graph),
                  fast_greedy = \(graph, seed) community_fast_greedy(graph))
  covers <- c(leiden = "community_leiden()", louvain = "community_louvain()", consensus = "community_consensus()",
              walktrap = "community_walktrap()", leading_eigenvector = "community_leading_eigenvector()",
              fast_greedy = "community_fast_greedy()")
  lapply(names(methods), \(method) lapply(seq_along(planted), \(draw) lapply(1:3, \(seed) {
    graph <- planted[[draw]]$graph
    expect_reference(ref_canonical(membership(methods[[method]](graph, seed)), rownames(graph)),
                     ref_canonical(planted[[draw]]$blocks, rownames(graph)),
                     reference = "independent: recovery of the blocks of simulab simulate_network(model = 'block')",
                     covers = covers[[method]],
                     quantity = sprintf("blocks recovered, %s, block model %d, seed %d", method, draw, seed), tolerance = 0)
  })))
})

test_that("compare_communities() equals aricode's NMI, ARI and Rand index, and mclust's adjusted Rand index", {
  skip_without_reference("aricode")
  first <- community_fast_greedy(karate)
  second <- community_leading_eigenvector(karate)
  found <- membership(community_louvain(planted[[1L]]$graph, seed = 2L))
  pairs <- list(
    karate = list(first, second, membership(first), membership(second)[names(membership(first))]),
    plain = list(c(1, 1, 2, 2, 3, 3, 3, 1, 2), c(2, 2, 2, 1, 1, 3, 3, 3, 1),
                 c(1, 1, 2, 2, 3, 3, 3, 1, 2), c(2, 2, 2, 1, 1, 3, 3, 3, 1)),
    random_30 = ref_with_seed(9L, {
      a <- sample(1:4, 30, replace = TRUE)
      b <- sample(1:5, 30, replace = TRUE)
      list(a, b, a, b)
    }),
    # aricode reads labels as integers: the block labels as numbers
    louvain_against_blocks = local({
      blocks <- as.integer(factor(planted[[1L]]$blocks[names(found)]))
      list(found, blocks, unname(found), blocks)
    }))
  lapply(names(pairs), \(name) {
    p <- pairs[[name]]
    entropies <- aricode::entropy(p[[3L]], p[[4L]])
    expect_reference(c(nmi = compare_communities(p[[1L]], p[[2L]], method = "nmi"),
                       adjusted.rand = compare_communities(p[[1L]], p[[2L]], method = "adjusted.rand"),
                       rand = compare_communities(p[[1L]], p[[2L]], method = "rand"),
                       vi = compare_communities(p[[1L]], p[[2L]], method = "vi")),
                     c(nmi = aricode::NMI(p[[3L]], p[[4L]], variant = "sum"), adjusted.rand = aricode::ARI(p[[3L]], p[[4L]]),
                       rand = aricode::RI(p[[3L]], p[[4L]]), vi = 2 * entropies$UV - entropies$U - entropies$V),
                     reference = "aricode", covers = "compare_communities()",
                     quantity = sprintf("NMI, adjusted Rand, Rand and VI, %s", name), tolerance = exact)
  })
  skip_without_reference("mclust")
  lapply(names(pairs), \(name) {
    p <- pairs[[name]]
    expect_reference(compare_communities(p[[1L]], p[[2L]], method = "adjusted.rand"),
                     mclust::adjustedRandIndex(p[[3L]], p[[4L]]), reference = "mclust", covers = "compare_communities()",
                     quantity = sprintf("adjusted Rand index, %s", name), tolerance = exact)
  })
})

test_that("fluid communities of two cliques joined by an edge equal NetworkX's asyn_fluidc() partition", {
  # A probe of 40 seeds before this test was written: cograph (igraph) and NetworkX each returned the two cliques
  # 40 of 40 times with k = 2.
  barbell <- outer(rep(1:2, each = 6), rep(1:2, each = 6), "==") * 1
  diag(barbell) <- 0
  barbell[6, 7] <- barbell[7, 6] <- 1
  dimnames(barbell) <- list(paste0("v", 1:12), paste0("v", 1:12))
  nx_fluid <- \(seed) ref_canonical(ref_networkx(barbell, c(m = sprintf(
    "{n: i for i, c in enumerate(nx.community.asyn_fluidc(G, 2, seed=%d)) for n in c}", seed)))$m, rownames(barbell))
  lapply(seeds, \(seed) {
    expect_reference(ref_canonical(membership(ref_with_seed(seed, community_fluid(barbell, no.of.communities = 2))),
                                   rownames(barbell)), nx_fluid(seed), reference = "networkx", covers = "community_fluid()",
                     quantity = sprintf("fluid partition of two joined cliques, seed %d", seed), tolerance = 0)
    expect_reference(ref_canonical(membership(ref_with_seed(seed + 5L, com_fl(barbell, no.of.communities = 2))),
                                   rownames(barbell)), nx_fluid(seed + 5L), reference = "networkx",
                     covers = c("com_fl()", "community_fluid()"),
                     quantity = sprintf("com_fl() partition of two joined cliques, seed %d", seed + 5L), tolerance = 0)
  })
})
