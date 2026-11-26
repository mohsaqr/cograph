# Three measures of cograph's centrality batch 57, and the INF centrality that centrality_beta_measure() equals on
# an undirected network, against published values and NetworkX.
#   centrality_beta_measure(): Huang, Chen, Wang and Ren (2020), Entropy 22(4):450, equation (15) at the paper's R = 1:
#     INF(i) = sum over the neighbours j of 1 / k_j. Table 2, p. 9, prints the logistic transform of that sum for the
#     Krackhardt kite (the text does not mention the transform).
#   centrality_gin(): Zhao, Wang and Deng (2020), Chaos, Solitons and Fractals 133:109637, read through its
#     reproduction in Yu et al. (2022), Entropy 24(2):275, equation (8): exp(k_i / n) sum_{j != i} k_j / d_ij.
#   centrality_icentr(): Mussone, Viseh and Notari (2022), Physica A 589:126595, equations (9)-(13).
#   centrality_gli(): Yang, Hu and Huang (2020), Chinese Physics B 29(8):088903, equations (4)-(5), r = 3.
# No other package implements these measures.

independent <- "independent: closed forms and hand derivations from the definition"
huang_table_2 <- "published: Huang, Chen, Wang and Ren 2020, Table 2, p. 9"
mussone_figure_1 <- "published: Mussone, Viseh and Notari 2022, Figure 1 and its worked values"
wang_2023 <- "published: Wang, Alias, Sun, Wang, Fan and Hu 2023, Heliyon 9:e16112, Table 3 (GIN column)"
yang_table_2 <- "published: Yang, Hu and Huang 2020, Table 2"
exact <- 1e-10
# Four printed decimals: half a unit in the last printed place.
printed_4dp <- 5e-5
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()

test_that("beta_measure reproduces Table 2 of the INF paper for the kite, after the logistic transform", {
  printed <- c(0.7211, 0.7211, 0.6495, 0.8273, 0.6495, 0.7830, 0.7830, 0.7109, 0.7914, 0.6225)
  expect_reference(stats::plogis(centrality_beta_measure(zoob_kite())), stats::setNames(printed, 1:10),
                   reference = huang_table_2, covers = c("centrality()", "centrality_beta_measure()"),
                   quantity = "plogis(beta_measure), Krackhardt kite", tolerance = printed_4dp)
})

test_that("beta_measure on an undirected graph is the sum of reciprocal degrees over NetworkX's neighbours", {
  call <- c(inf = "{i: sum(1 / G.degree(j) for j in G[i]) for i in G}")
  Map(\(graph, name) {
    expect_reference(centrality_beta_measure(graph), ref_networkx(graph, call)$inf, reference = "networkx",
                     covers = c("centrality()", "centrality_beta_measure()"),
                     quantity = sprintf("beta_measure as INF, %s", name),
                     tolerance = exact)
  }, list(undirected, sparse, karate), c("undirected", "sparse", "karate"))
})

test_that("gin ranks the karate club as a third-party table of GIN prints it", {
  # Table 3 of Wang et al. (2023) lists the top-10 nodes of the karate club by GIN, computed by its authors, not by
  # Zhao, Wang and Deng. It checks the shape of the formula, not its values.
  expect_reference(as.numeric(names(sort(centrality_gin(karate), decreasing = TRUE))[1:10]),
                   c(1, 34, 3, 33, 2, 32, 9, 14, 4, 20), reference = wang_2023,
                   covers = c("centrality()", "centrality_gin()"), quantity = "gin, karate, ten highest nodes in order",
                   tolerance = 0)
})

test_that("gin equals its equation evaluated over NetworkX's shortest path lengths", {
  call <- c(gin = paste0(
    "{i: (np.exp(G.degree(i) / len(G)) * sum(G.degree(j) / d for j, d in",
    " nx.single_source_shortest_path_length(G, i).items() if j != i)) if G.degree(i) > 0 else 0.0 for i in G}"))
  Map(\(graph, name) {
    expect_reference(centrality_gin(graph), ref_networkx(graph, call)$gin, reference = "networkx",
                     covers = c("centrality()", "centrality_gin()"), quantity = sprintf("gin, %s", name),
                     tolerance = 1e-9, measure = "scaled")
  }, list(undirected, sparse, karate), c("undirected", "sparse", "karate"))
})

test_that("icentr reproduces the values of the paper's Figure 1", {
  # The paper prints Icentr = 4.5 for node 1 and 3.75 for nodes 2 and 4, and states the symmetries that give node 3
  # the value of node 1 and node 5 the value of nodes 2 and 4.
  figure_1 <- zoob_graph(5L, c(1, 2, 1, 3, 1, 5, 2, 3, 3, 4, 4, 5))
  expect_reference(centrality_icentr(figure_1)[c("1", "2", "4")], c(`1` = 4.5, `2` = 3.75, `4` = 3.75),
                   reference = mussone_figure_1, covers = c("centrality()", "centrality_icentr()"),
                   quantity = "icentr, Figure 1, nodes 1, 2 and 4", tolerance = exact)
})

test_that("icentr equals the level sum over NetworkX's breadth-first levels", {
  call <- c(icentr = paste0(
    "{i: (lambda lev: sum(2.0 ** -((lev[u] + 1 if lev[u] == lev[v] else max(lev[u], lev[v])) - 1)",
    " for u, v in G.edges() if u in lev))(nx.single_source_shortest_path_length(G, i)) for i in G}"))
  Map(\(graph, name) {
    expect_reference(centrality_icentr(graph), ref_networkx(graph, call)$icentr, reference = "networkx",
                     covers = c("centrality()", "centrality_icentr()"), quantity = sprintf("icentr, %s", name),
                     tolerance = exact)
  }, list(undirected, sparse, karate), c("undirected", "sparse", "karate"))
})

test_that("gli puts the ten nodes of the paper's Table 2 at the top of the karate club", {
  # The paper lists 1, 3, 34, 33, 14, 2, 9, 32, 4, 20. cograph gives the same ten with nodes 2 and 9 exchanged, the
  # two scores 0.1 per cent apart; the comparison is of the set (sorted node numbers).
  top <- as.numeric(names(sort(centrality_gli(karate), decreasing = TRUE))[1:10])
  expect_reference(sort(top), sort(c(1, 3, 34, 33, 14, 2, 9, 32, 4, 20)), reference = yang_table_2,
                   covers = c("centrality()", "centrality_gli()"),
                   quantity = "gli, karate, the ten highest nodes as a set",
                   tolerance = 0)
})

test_that("gli on a star follows the hand computation", {
  # Three leaves: masses 6 (hub: shell 1, round 2, degree 3) and 3 (leaf), total 15.
  expect_reference(centrality_gli(zoob_star(3L)), c(exp(6 / 15) * 9, rep(exp(3 / 15) * 9, 3)), reference = independent,
                   covers = c("centrality()", "centrality_gli()"), quantity = "gli, star of 3", tolerance = exact)
})
