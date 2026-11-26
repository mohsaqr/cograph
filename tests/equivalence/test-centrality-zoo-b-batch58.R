# Five measures of cograph's centrality batch 58, against published values and NetworkX.
#   centrality_niem(): Zhou, Zhang, Li, Hui and Wu (2012), Acta Physica Sinica 61(5):050201, equations (4)-(6):
#     C_i = I_i sum_{j in N(i)} D_j I_j / <k>^2 with I_k = (1/n) sum_{i != k} 1/d_ki. Tables 1 and 2 of the paper.
#   centrality_iksd(): the improved k-shell decomposition of Liu, Jiang, Wang and Yu (2015), Knowledge-Based Systems
#     84:56-66, section 2.2.5, p. 57 (the paper prints no per-node value).
#   centrality_density_centrality(): Ibnoulouafi and El Haziti (2018), as printed by Ullah et al. (2021), Scientific
#     Reports 11:6173, equation (4) (the 2018 paper was not read).
#   centrality_wksdn(): Maji (2020), Journal of Computational Science 39:101055, as reproduced in a later paper (the
#     2020 paper was not read; no published value exists to compare with).
#   centrality_mu_pci(): Papadimitriou, Katsaros and Manolopoulos (2010), p. 6 and Fig. 4.
# No other package implements these measures.

zhou_table_1 <- "published: Zhou, Zhang, Li, Hui and Wu 2012, Table 1"
zhou_table_2 <- "published: Zhou, Zhang, Li, Hui and Wu 2012, Table 2"
papadimitriou_fig_4 <- "published: Papadimitriou, Katsaros and Manolopoulos 2010, Fig. 4 and the text below it"
exact <- 1e-10
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()

test_that("niem reproduces Table 1 of the paper (8 nodes)", {
  # Three printed decimals. Seven cells agree to half a unit of the last place; node 8 computes to 0.151497 and is
  # printed .152, so the comparison is made at one unit of the last printed place.
  figure_1 <- zoob_graph(8L, c(1, 3, 2, 3, 3, 4, 4, 5, 5, 6, 5, 7, 6, 7, 6, 8))
  printed <- c(0.157, 0.157, 0.254, 0.461, 0.535, 0.417, 0.404, 0.152)
  expect_reference(centrality_niem(figure_1), stats::setNames(printed, 1:8), reference = zhou_table_1,
                   covers = c("centrality()", "centrality_niem()"), quantity = "niem, Fig. 1 network", tolerance = 1e-3,
                   note = "one unit of the last printed digit; node 8 computes to 0.151497 against the printed .152")
})

test_that("niem reproduces Table 2 of the paper (ARPA network, 21 nodes)", {
  # The paper's text says 23 links; its figure has 26, and the 26 reproduce the table.
  arpa <- zoob_graph(21L, c(1, 2, 1, 15, 2, 3, 2, 15, 2, 16, 3, 4, 3, 17, 3, 18, 4, 5, 5, 6, 6, 7, 6, 21, 7, 8, 8, 9,
                            9, 10, 10, 11, 11, 12, 12, 13, 12, 19, 13, 14, 14, 15, 14, 16, 14, 17, 18, 19, 19, 20,
                            20, 21))
  printed <- c(0.1528, 0.2987, 0.2984, 0.1562, 0.1090, 0.1261, 0.0935, 0.0634, 0.0624, 0.0680, 0.1062, 0.1815,
               0.1839, 0.2369, 0.2522, 0.1978, 0.2214, 0.1970, 0.1845, 0.1115, 0.1023)
  expect_reference(centrality_niem(arpa), stats::setNames(printed, 1:21), reference = zhou_table_2,
                   covers = c("centrality()", "centrality_niem()"), quantity = "niem, ARPA network", tolerance = 5e-5)
})

test_that("iksd equals the lowest-degree removal run on a NetworkX graph", {
  call <- c(iksd = paste0(
    "(lambda H, out: [([out.__setitem__(v, k) for v in batch], H.remove_nodes_from(batch))",
    " for k in range(1, len(G) + 1)",
    " for batch in [[v for v, d in H.degree() if d == min(dict(H.degree()).values())] if len(H) else []]] and out)",
    "(nx.Graph(G), {})"))
  Map(\(graph, name) {
    expect_reference(centrality_iksd(graph), ref_networkx(graph, call)$iksd, reference = "networkx",
                     covers = c("centrality()", "centrality_iksd()"), quantity = sprintf("iksd, %s", name),
                     tolerance = 0)
  }, list(undirected, sparse, karate), c("undirected", "sparse", "karate"))
})

test_that("density centrality equals its formula over NetworkX's shortest path lengths", {
  call_3 <- c(density = paste0(
    "{i: G.degree(i) / np.pi * sum(1 / d ** 2 for d in",
    " nx.single_source_shortest_path_length(G, i, cutoff=3).values() if d >= 1) for i in G}"))
  call_all <- c(density = paste0("{i: G.degree(i) / np.pi * sum(1 / d ** 2 for d in",
                                 " nx.single_source_shortest_path_length(G, i).values() if d >= 1) for i in G}"))
  Map(\(graph, name) {
    expect_reference(centrality_density_centrality(graph), ref_networkx(graph, call_3)$density,
                     reference = "networkx", covers = c("centrality()", "centrality_density_centrality()"),
                     quantity = sprintf("density_centrality, radius 3, %s", name), tolerance = exact, measure = "scaled")
    expect_reference(centrality_density_centrality(graph, density_radius = Inf),
                     ref_networkx(graph, call_all)$density, reference = "networkx", covers = c("centrality()", "centrality_density_centrality()"),
                     quantity = sprintf("density_centrality, no radius, %s", name), tolerance = exact,
                     measure = "scaled")
  }, list(undirected, sparse, karate), c("undirected", "sparse", "karate"))
})

test_that("wksdn equals its equations over NetworkX's core numbers", {
  call <- c(wksdn = paste0(
    "(lambda ks, lam: {i: sum((ks[i] + ks[j]) + lam * (G.degree(i) + G.degree(j)) for j in G[i]) for i in G})",
    "(nx.core_number(G), sum(nx.core_number(G).values()) / sum(d for _, d in G.degree()))"))
  Map(\(graph, name) {
    expect_reference(centrality_wksdn(graph), ref_networkx(graph, call)$wksdn, reference = "networkx",
                     covers = c("centrality()", "centrality_wksdn()"), quantity = sprintf("wksdn, %s", name),
                     tolerance = exact, measure = "scaled")
  }, list(undirected, sparse, karate), c("undirected", "sparse", "karate"))
})

test_that("mu_pci reproduces the four values the paper prints for its Fig. 4", {
  figure_4 <- zoob_graph(9L, c(9, 3, 3, 7, 3, 6, 3, 4, 4, 8, 4, 6, 4, 5, 6, 5, 6, 7, 7, 1, 7, 2))
  expect_reference(centrality_mu_pci(figure_4)[c("7", "4", "6", "3")], c(`7` = 2, `4` = 2, `6` = 3, `3` = 3),
                   reference = papadimitriou_fig_4, covers = c("centrality()", "centrality_mu_pci()"),
                   quantity = "mu_pci, mu 1, Fig. 4, nodes 7, 4, 6, 3", tolerance = 0)
})

test_that("mu_pci equals the count over NetworkX's mu-hop balls", {
  count <- paste0(
    "{i: (lambda degs: max([k for k in range(0, len(G) + 1) if sum(1 for d in degs if d >= k) >= %d * k]))",
    "([G.degree(j) for j, d in nx.single_source_shortest_path_length(G, i, cutoff=%d).items() if d >= 1]) for i in G}")
  Map(\(graph, name) {
    lapply(1:3, \(mu) {
      expect_reference(centrality_mu_pci(graph, pci_mu = mu), ref_networkx(graph, c(pci = sprintf(count, mu, mu)))$pci,
                       reference = "networkx", covers = c("centrality()", "centrality_mu_pci()"),
                       quantity = sprintf("mu_pci, mu %d, %s", mu, name), tolerance = 0)
    })
  }, list(undirected, sparse, karate), c("undirected", "sparse", "karate"))
})
