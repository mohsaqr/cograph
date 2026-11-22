# centrality() and its wrappers for the measures built on random walks and spreading: pagerank (and personalised
# pagerank), leaderrank, weighted and adaptive leaderrank, salsa, random walk, markov, second-order, trophic level,
# diffusion and voterank. cograph computes them natively (power iteration, eigen(), solve()), not through igraph.
#
# Conventions, read from the sources before the run:
# - pagerank reads a weight as a transition strength and sends the mass of a node without out-ties to the reset
#   vector, as igraph (PRPACK) and NetworkX do.
# - The walk measures random_walk, markov and second_order walk on the 0/1 adjacency (each step to a uniformly
#   chosen neighbour); weights are not read. leaderrank and its variants and trophic level also read the 0/1
#   adjacency.
# - Passage times here are computed one target at a time from the hitting-time equations (helper-a1.R), a route
#   that shares nothing with cograph's fundamental-matrix formula.

undirected <- ref_connected_graph(12, density = 0.25, seed = 41L)
directed <- ref_connected_graph(12, density = 0.2, seed = 43L, directed = TRUE)
directed_weighted <- ref_connected_graph(12, density = 0.2, seed = 44L, directed = TRUE, weighted = TRUE, digits = 6L)
undirected_weighted <- ref_connected_graph(12, density = 0.25, seed = 42L, weighted = TRUE, digits = 6L)
# A food web for trophic levels: the upper triangle of a random directed graph is acyclic, and every node is a
# basal node (no in-tie) or is reached from one.
food_web <- ref_graph(10, density = 0.35, seed = 45L, directed = TRUE) * upper.tri(diag(10))
# A directed graph with nodes that have no out-tie (dangling), for pagerank's handling of them.
dangling <- ref_graph(12, density = 0.2, seed = 46L, directed = TRUE, weighted = TRUE, digits = 6L)

exact <- 1e-10
# NetworkX's pagerank() iterates until the summed change of one sweep is below n * tol; with tol = 1e-14 that
# leaves under 1e-12 to the fixed point at damping 0.85. igraph's PRPACK solves the system directly.
power_iteration <- 1e-10

test_that("pagerank equals NetworkX's pagerank()", {
  call <- "nx.pagerank(G, alpha=0.85, weight='weight', tol=1e-14, max_iter=100000)"
  on_directed <- ref_networkx(directed_weighted, c(pagerank = call))
  on_dangling <- ref_networkx(dangling, c(pagerank = call), directed = TRUE)
  on_undirected <- ref_networkx(undirected_weighted, c(pagerank = call))
  expect_reference(centrality_pagerank(directed_weighted), on_directed$pagerank, reference = "networkx",
                   covers = c("centrality()", "centrality_pagerank()"), quantity = "pagerank, directed weighted",
                   tolerance = power_iteration)
  expect_reference(centrality_pagerank(dangling, directed = TRUE), on_dangling$pagerank, reference = "networkx",
                   covers = c("centrality()", "centrality_pagerank()"),
                   quantity = "pagerank, directed weighted with dangling nodes", tolerance = power_iteration)
  expect_reference(centrality_pagerank(undirected_weighted), on_undirected$pagerank, reference = "networkx",
                   covers = c("centrality()", "centrality_pagerank()"), quantity = "pagerank, undirected weighted",
                   tolerance = power_iteration)
})

test_that("pagerank equals igraph's page_rank()", {
  skip_without_reference("igraph")
  expect_reference(centrality_pagerank(directed_weighted), igraph::page_rank(ref_igraph(directed_weighted))$vector,
                   reference = "igraph", covers = c("centrality()", "centrality_pagerank()"),
                   quantity = "pagerank, directed weighted", tolerance = power_iteration)
  expect_reference(centrality_pagerank(dangling, directed = TRUE),
                   igraph::page_rank(ref_igraph(dangling, directed = TRUE))$vector,
                   reference = "igraph", covers = c("centrality()", "centrality_pagerank()"),
                   quantity = "pagerank, directed weighted with dangling nodes", tolerance = power_iteration)
  expect_reference(centrality_pagerank(undirected, damping = 0.6), igraph::page_rank(ref_igraph(undirected), damping = 0.6)$vector,
                   reference = "igraph", covers = c("centrality()", "centrality_pagerank()"),
                   quantity = "pagerank, damping 0.6, undirected", tolerance = power_iteration)
})

test_that("personalised pagerank equals NetworkX's pagerank(personalization) and igraph's page_rank(personalized)", {
  reset <- stats::setNames(c(4, 3, 2, 1, rep(0, 8)), rownames(directed_weighted))
  nx <- ref_networkx(directed_weighted, c(
    pagerank = "nx.pagerank(G, personalization={'N1': 4, 'N2': 3, 'N3': 2, 'N4': 1}, tol=1e-14, max_iter=100000)"))
  expect_reference(centrality_pagerank(directed_weighted, personalized = reset), nx$pagerank, reference = "networkx",
                   covers = c("centrality()", "centrality_pagerank()"),
                   quantity = "pagerank, personalised to N1-N4, directed weighted", tolerance = power_iteration)
  skip_without_reference("igraph")
  expect_reference(centrality_pagerank(directed_weighted, personalized = reset),
                   igraph::page_rank(ref_igraph(directed_weighted), personalized = unname(reset))$vector,
                   reference = "igraph", covers = c("centrality()", "centrality_pagerank()"),
                   quantity = "pagerank, personalised to N1-N4, directed weighted", tolerance = power_iteration)
})

test_that("leaderrank equals the stationary walk of Lu et al. (2011) with a ground node", {
  # Lu, Zhang, Yeung and Zhou (2011): a ground node g is tied both ways to every node; the walk starts with one
  # unit on each node and none on g, so it keeps total mass n, and g's share is spread evenly at the end:
  # score_i = n pi_i + pi_g. The stationary pi is taken from eigen(), not iterated.
  leaderrank <- \(graph) {
    n <- nrow(graph)
    extended <- rbind(cbind((graph != 0) * 1, 1), c(rep(1, n), 0))
    stationary <- ref_stationary(extended / rowSums(extended))
    ref_named(n * stationary[seq_len(n)] + stationary[[n + 1L]], graph)
  }
  # cograph iterates until the mean relative change of one sweep is below 2e-5 and stops there; the distance left
  # to the fixed point is of the order of that change times |lambda_2| / (1 - |lambda_2|) of the chain. 1e-4
  # relative allows a few times the stopping threshold.
  expect_reference(centrality_leaderrank(directed), leaderrank(directed),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "Lu et al. (2011) ground-node chain, stationary vector by base R eigen()",
                   covers = c("centrality()", "centrality_leaderrank()"), quantity = "leaderrank, directed",
                   tolerance = 1e-4, measure = "relative")
})

test_that("weighted leaderrank equals the stationary walk of Li et al. (2014)", {
  # Li, Zhou, Lu and Chen (2014): the ground node sends weight k_in(i)^alpha to node i and receives weight 1 from
  # every node; the walk keeps total mass n + 1 and the ground's share is not redistributed:
  # score_i = (n + 1) pi_i on the full (n + 1)-state chain.
  weighted_leaderrank <- \(graph, alpha) {
    n <- nrow(graph)
    adjacency <- (graph != 0) * 1
    extended <- rbind(cbind(adjacency, 1), c(colSums(adjacency)^alpha, 0))
    stationary <- ref_stationary(extended / rowSums(extended))
    ref_named((n + 1) * stationary[seq_len(n)], graph)
  }
  expect_reference(centrality_weighted_leaderrank(directed, wlr_alpha = 1), weighted_leaderrank(directed, 1),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "Li et al. (2014) ground-node chain, stationary vector by base R eigen()",
                   covers = c("centrality()", "centrality_weighted_leaderrank()"),
                   quantity = "weighted leaderrank, alpha 1, directed", tolerance = exact)
  expect_reference(centrality_weighted_leaderrank(directed_weighted, wlr_alpha = 0.5), weighted_leaderrank(directed_weighted, 0.5),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "Li et al. (2014) ground-node chain, stationary vector by base R eigen()",
                   covers = c("centrality()", "centrality_weighted_leaderrank()"),
                   quantity = "weighted leaderrank, alpha 0.5, directed weighted (0/1 adjacency)", tolerance = exact)
})

test_that("adaptive leaderrank equals the stationary walk of Xu and Wang (2017)", {
  # Xu and Wang (2017), eq. 3: every move into node j is weighted by j's H-index (Lu et al. 2016: the h-index of
  # its neighbours' degrees, on the undirected skeleton for alr_h_mode = "all"); the ground node has H-index 1.
  # The walk starts with mass n on the nodes and the ground keeps its share: score_i = n pi_i.
  adaptive_leaderrank <- \(graph) {
    n <- nrow(graph)
    adjacency <- (graph != 0) * 1
    skeleton <- ((adjacency + t(adjacency)) != 0) * 1
    degree <- rowSums(skeleton)
    hindex <- vapply(seq_len(n), \(i) ref_hindex(degree[skeleton[i, ] != 0]), numeric(1))
    extended <- rbind(cbind(sweep(adjacency, 2L, hindex, `*`), 1), c(hindex, 0))
    stationary <- ref_stationary(extended / rowSums(extended))
    ref_named(n * stationary[seq_len(n)], graph)
  }
  expect_reference(centrality_adaptive_leaderrank(directed), adaptive_leaderrank(directed),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "Xu and Wang (2017) H-index-weighted ground-node chain, stationary vector by base R eigen()",
                   covers = c("centrality()", "centrality_adaptive_leaderrank()"),
                   quantity = "adaptive leaderrank, directed", tolerance = exact)
})

test_that("salsa authority equals the in-degree share of Lempel and Moran (2001)", {
  # Lempel and Moran (2001), Proposition 2: on a graph whose authority side is connected, the stationary authority
  # weight of a node is proportional to its in-degree. cograph rescales to maximum 1.
  expect_reference(centrality_salsa(directed), ref_unit_max(colSums((directed != 0) * 1)),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "Lempel and Moran (2001) Proposition 2, authority weight proportional to in-degree",
                   covers = c("centrality()", "centrality_salsa()"), quantity = "salsa, directed", tolerance = exact)
})

test_that("random walk centrality equals the inverse sum of symmetrised passage times", {
  # cograph's definition (as tidygraph's centrality_random_walk()): d(i, j) = (m_ij + m_ji) / 2 with m the mean
  # first-passage times of the simple random walk; the score is 1 / sum_j d(i, j).
  random_walk <- \(graph) {
    passage <- ref_mfpt(graph)
    1 / rowSums((passage + t(passage)) / 2)
  }
  expect_reference(centrality_random_walk(undirected), random_walk(undirected),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "hitting-time equations (Kemeny and Snell 1960) solved per target in base R",
                   covers = c("centrality()", "centrality_random_walk()"), quantity = "random walk, undirected",
                   tolerance = exact, measure = "relative")
  expect_reference(centrality_random_walk(directed), random_walk(directed),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "hitting-time equations (Kemeny and Snell 1960) solved per target in base R",
                   covers = c("centrality()", "centrality_random_walk()"), quantity = "random walk, directed",
                   tolerance = exact, measure = "relative")
})

test_that("random walk centrality differs from netrankr's inverse sum of random-walk distances", {
  skip_without_reference(c("netrankr", "igraph"))
  distances <- netrankr::indirect_relations(ref_igraph(undirected), type = "dist_rwalk")
  # Established after the first run from netrankr 2.0.0's dist_rwalk_fct(): its invsum is 1 / sum_i m_ij, the
  # passage times INTO the node (Noh and Rieger 2004); it equals 1 / colSums(ref_mfpt()) on this graph.
  expect_known_difference(centrality_random_walk(undirected),
                          ref_named(netrankr::aggregate_positions(distances, type = "invsum"), undirected),
                          reference = "netrankr", covers = c("centrality()", "centrality_random_walk()"),
                          quantity = "random walk, undirected", tolerance = exact, measure = "relative",
                          reason = paste("cograph sums the symmetrised distance (m_ij + m_ji) / 2 over j; netrankr's",
                                         "dist_rwalk with invsum (tidygraph's centrality_random_walk()) sums only the",
                                         "passage times into the node, 1 / sum_i m_ij (Noh and Rieger 2004)."))
})

test_that("markov centrality equals centiserve's markovcent() and n over the passage times into the node", {
  # White and Smyth (2003) as centiserve implements it: n / sum_i m_ij, the inverse mean passage time into j.
  expect_reference(centrality_markov(directed), nrow(directed) / colSums(ref_mfpt(directed)),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "hitting-time equations (Kemeny and Snell 1960) solved per target in base R",
                   covers = c("centrality()", "centrality_markov()"), quantity = "markov, directed",
                   tolerance = exact, measure = "relative")
  skip_without_reference(c("centiserve", "igraph"))
  expect_reference(centrality_markov(undirected), ref_named(centiserve::markovcent(ref_igraph(undirected)), undirected),
                   reference = "centiserve", covers = c("centrality()", "centrality_markov()"),
                   quantity = "markov, undirected", tolerance = exact, measure = "relative")
})

test_that("second-order centrality against NetworkX's second_order_centrality()", {
  # Kermarrec, Le Merrer, Sericola and Tredan (2011): the standard deviation of the return times to a node of a
  # random walk made uniform by self-loops (weight d_max - d_i), sqrt(2 sum_i m_ij - n (n + 1)).
  nx <- ref_networkx(undirected, c(second_order = "nx.second_order_centrality(G, weight=None)"))
  expect_reference(centrality(undirected, measures = "second_order"), ref_nodes(second_order = nx$second_order),
                   reference = "networkx", covers = "centrality()", quantity = "second_order, undirected",
                   tolerance = exact, measure = "relative", key = "node")
})

test_that("trophic level equals NetworkX's trophic_levels() and Levine's (1980) system", {
  # Levine (1980): s_j = 1 + (1 / k_in(j)) sum_{i -> j} s_i, with s = 1 for a node without in-ties.
  nx <- ref_networkx(food_web, c(trophic = "nx.trophic_levels(G, weight=None)"), directed = TRUE)
  expect_reference(centrality(food_web, measures = "trophic_level", directed = TRUE), ref_nodes(trophic_level = nx$trophic),
                   reference = "networkx", covers = "centrality()", quantity = "trophic_level, food web",
                   tolerance = exact, key = "node")
  adjacency <- (food_web != 0) * 1
  feeding <- t(adjacency) / pmax(colSums(adjacency), 1)
  levine <- ref_named(solve(diag(1, nrow(adjacency)) - feeding, rep(1, nrow(adjacency))), food_web)
  expect_reference(centrality(food_web, measures = "trophic_level", directed = TRUE), ref_nodes(trophic_level = levine),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "Levine (1980) trophic-level system solved with base R solve()",
                   covers = "centrality()", quantity = "trophic_level, food web", tolerance = exact, key = "node")
})

test_that("diffusion equals centiserve's diffusion.degree() and the degree sum over the closed neighbourhood", {
  # Diffusion degree (Kandhway and Kuri 2014, lambda = 1): the node's degree plus the degrees of its neighbours.
  degree <- rowSums((undirected != 0) * 1)
  expect_reference(centrality_diffusion(undirected), degree + as.numeric(((undirected != 0) * 1) %*% degree),
                   reference = "independent: the walk definitions written in base R (each named in its note)", note = "degree plus neighbours' degrees, base R matrix product",
                   covers = c("centrality()", "centrality_diffusion()"), quantity = "diffusion, lambda 1, undirected",
                   tolerance = exact)
  skip_without_reference(c("centiserve", "igraph"))
  expect_reference(centrality_diffusion(undirected), ref_named(centiserve::diffusion.degree(ref_igraph(undirected)), undirected),
                   reference = "centiserve", covers = c("centrality()", "centrality_diffusion()"),
                   quantity = "diffusion, lambda 1, undirected", tolerance = exact)
})

test_that("voterank's order of election equals NetworkX's voterank()", {
  # Zhang et al. (2016). NetworkX returns the elected nodes in order and stops when no node has a vote left;
  # cograph elects every node and scores the r-th elected (n + 1 - r) / n. The nodes NetworkX elects are compared
  # with the score their place in NetworkX's order gives.
  elected <- unlist(ref_networkx(undirected, c(voterank = "nx.voterank(G)"))$voterank)
  n <- nrow(undirected)
  expected <- stats::setNames((n + 1 - seq_along(elected)) / n, elected)
  expect_reference(centrality_voterank(undirected)[elected], expected, reference = "networkx",
                   covers = c("centrality()", "centrality_voterank()"),
                   quantity = "voterank, order of election, undirected", tolerance = exact)
})
