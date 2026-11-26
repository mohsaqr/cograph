# Eight measures of cograph's centrality batch 64, against published values and NetworkX.
#   centrality_wd(), centrality_ewd(): Yang, Li, Xu, Liu, Wang, Zhang, Zhang and Yao (2018), Physics Letters A
#     382(34):2361-2371, equations (1)-(2). The paper gives its 15-node example as a figure; the edge list used here
#     is the one graph found consistent with every column of the paper's Table 5.
#   centrality_mlc(): Ma and Ma (2017), Physica A 465:312-330, section 3 (the paper prints no value of the measure).
#   centrality_spreading_probability(): Bao, Ma, Xiang and Zhang (2017), Physica A 468:391-397, equation (5) (no
#     value on a small network is printed).
#   centrality_profitleader(): Yu, Shao, Yang and Sun (2019), World Wide Web 22(2):533-553, equations (1)-(4) with
#     the + 1 of the authors' released code, and the ordered top ten of Table 5 for four of its six networks.
#   centrality_hks(): Zareie and Sheikhahmadi (2018), Expert Systems with Applications 93:200-211, equations
#     (10)-(11); Table 3 prints the monotonicity of the ranking on the karate club network.
#   centrality_niccm(): Hu, Fan and Mei (2015), Physica A 429:169-176, equations (1)-(4), Tables 2 and 3.
#   centrality_correlation_centrality(): Fan, Liu and Hu (2013), Physica Scripta 88(6):065201, equation (4), Table 1.
# No other package implements these measures.

yang_text <- "published: Yang et al. 2018, worked values in the text below equation (2)"
zareie_table_3 <- "published: Zareie and Sheikhahmadi 2018, Table 3"
hu_table_2 <- "published: Hu, Fan and Mei 2015, Table 2"
hu_table_3 <- "published: Hu, Fan and Mei 2015, Table 3"
fan_table_1 <- "published: Fan, Liu and Hu 2013, Table 1"
yu_table_5 <- "published: Yu, Shao, Yang and Sun 2019, Table 5"
exact <- 1e-10
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()
graphs <- list(undirected = undirected, sparse = sparse, karate = karate)
arpa <- zoob_graph(21L, c(1, 2, 1, 15, 2, 3, 2, 15, 2, 16, 3, 4, 3, 17, 3, 18, 4, 5, 5, 6, 6, 7, 6, 21, 7, 8, 8, 9,
                          9, 10, 10, 11, 11, 12, 12, 13, 12, 19, 13, 14, 14, 15, 14, 16, 14, 17, 18, 19, 19, 20,
                          20, 21))

# The removal rounds of the remaining-minimum-degree decomposition, as a Python expression over a NetworkX graph.
rounds <- paste0(
  "(lambda H, out: [([out.__setitem__(v, k) for v in batch], H.remove_nodes_from(batch))",
  " for k in range(1, len(G) + 1)",
  " for batch in [[v for v, d in H.degree() if d == min(dict(H.degree()).values())] if len(H) else []]] and out)",
  "(nx.Graph(G), {})")
weighted_degree <- sprintf("(lambda it: {v: sum(it[u] for u in G[v]) / max(it.values()) for v in G})(%s)", rounds)

compare_networkx <- function(measure, call, tolerance = exact) {
  wrapper <- match.fun(paste0("centrality_", measure))
  Map(\(graph, name) {
    expect_reference(wrapper(graph), ref_networkx(graph, c(value = call))$value, reference = "networkx",
                     covers = c("centrality()", sprintf("centrality_%s()", measure)),
                     quantity = sprintf("%s, %s", measure, name), tolerance = tolerance)
  }, graphs, names(graphs))
}

test_that("wd and ewd reproduce the worked values of the paper's 15-node example", {
  figure_1a <- zoob_graph(15L, c(1, 6, 1, 12, 2, 7, 2, 8, 2, 15, 3, 9, 3, 12, 3, 13, 3, 15, 4, 14, 5, 10, 5, 14,
                                 6, 15, 7, 13, 8, 12, 10, 15, 11, 13, 13, 15))
  expect_reference(centrality_wd(figure_1a)[["15"]], 4.143, reference = yang_text,
                   covers = c("centrality()", "centrality_wd()"), quantity = "wd, node 15 of Fig. 1(a)",
                   tolerance = 5e-4, note = "the edge list is reconstructed from Table 5, the figure is an image")
  expect_reference(centrality_ewd(figure_1a)[["15"]], 11.429, reference = yang_text,
                   covers = c("centrality()", "centrality_ewd()"), quantity = "ewd, node 15 of Fig. 1(a)",
                   tolerance = 5e-4, note = "the edge list is reconstructed from Table 5, the figure is an image")
})

test_that("wd and ewd equal equations (1)-(2) written over NetworkX", {
  compare_networkx("wd", weighted_degree)
  compare_networkx("ewd", sprintf("(lambda wd: {v: sum(wd[u] for u in G[v]) for v in G})(%s)", weighted_degree))
})

test_that("mlc equals its definition written over NetworkX's two-step balls", {
  compare_networkx("mlc", paste0(
    "(lambda n2: {v: sum(n2[w] for u in G[v] for w in G[u]) - 2 * sum(G.degree(u) for u in G[v]) for v in G})",
    "({w: len(nx.single_source_shortest_path_length(G, w, cutoff=2)) - 1 for w in G})"))
})

test_that("spreading_probability equals equation (5) over NetworkX's enumerated shortest paths", {
  compare_networkx("spreading_probability", paste0(
    "(lambda kb: {i: sum(len(list(nx.all_shortest_paths(G, i, j))) * kb ** (-d)",
    " for j, d in nx.single_source_shortest_path_length(G, i, cutoff=3).items() if d >= 1) for i in G})",
    "(2 * G.number_of_edges() / G.number_of_nodes())"))
})

test_that("profitleader reproduces the ordered top ten of Table 5 (karate, dolphins, adjnoun, lesmis)", {
  networks <- list(
    karate = list(graph = karate, top = c(1, 34, 33, 3, 2, 32, 9, 4, 14, 24), labels = NULL),
    dolphins = list(graph = zoob_dolphins(), top = c(15, 46, 38, 34, 52, 58, 21, 14, 30, 18),
                    labels = "Grin Topless SN4 Scabs Trigger Web Kringel Gallatin Patchback Jet"),
    adjnoun = list(graph = zoob_adjnoun(), top = c(18, 3, 52, 44, 105, 51, 25, 55, 26, 28),
                   labels = "little old good other same dear room pretty thing way"),
    lesmis = list(graph = zoob_lesmis(), top = c(12, 49, 56, 59, 26, 24, 65, 28, 63, 42),
                  labels = "Valjean Gavroche Marius Enjolras Thenardier Fantine Bossuet Javert Courfeyrac Eponine"))
  Map(\(net, name) {
    score <- centrality_profitleader(net$graph)
    expect_reference(as.numeric(names(score)[order(-score, seq_along(score))][1:10]), net$top,
                     reference = yu_table_5, covers = c("centrality()", "centrality_profitleader()"),
                     quantity = sprintf("ProfitLeader top ten in order, %s", name), tolerance = 0,
                     note = sprintf(paste("node ids of the netzschleuder copy in candidate_review57, printed as %s;",
                                          "unweighted. Without the + 1 of the authors' code two adjacent pairs",
                                          "swap (karate 4 and 14, lesmis Bossuet and Javert)"),
                                    net$labels %||% "the node numbers"))
  }, networks, names(networks))
})

test_that("profitleader equals equations (1)-(4) with the code's + 1 written with Python sets", {
  compare_networkx("profitleader", paste0(
    "{u: sum((G.degree(u) + 1 + sum(G.degree(s) for s in set(G[u]) - set(G[v]) - {v}))",
    " * len((set(G[u]) | {u}) & (set(G[v]) | {v})) / len((set(G[u]) | {u}) | (set(G[v]) | {v}))",
    " for v in G[u]) for u in G}"))
})

test_that("hks equals Algorithms 1-2 and equations (10)-(11) written over NetworkX", {
  peel <- paste0(
    "(lambda peel: peel(peel, nx.Graph(G), 1, 1, {}))",
    "(lambda self, H, shell, rnd, out: out if len(H) == 0 else",
    " (lambda low: (out.update({v: rnd for v in low}), H.remove_nodes_from(low),",
    " self(self, H, shell, rnd + 1, out))[2] if low else self(self, H, shell + 1, rnd, out))",
    "([v for v, d in H.degree() if d <= shell]))")
  compare_networkx("hks", sprintf(paste0(
    "(lambda b: (lambda s: (lambda S: {i: sum(S[j] for j in G[i]) for i in G})",
    "({i: sum(s[j] for j in G[i]) for i in G}))",
    "({j: G.degree(j) * (b[j] + max(b[u] - d for u, d in nx.single_source_shortest_path_length(G, j).items()))",
    " for j in G}))(%s)"), peel))
})

test_that("hks gives the monotonicity the paper prints for the karate club", {
  score <- centrality_hks(zoob_karate())
  ties <- as.numeric(table(score))
  monotonicity <- (1 - sum(ties * (ties - 1)) / (34 * 33))^2
  expect_reference(monotonicity, 0.9542, reference = zareie_table_3, covers = c("centrality()", "centrality_hks()"),
                   quantity = "monotonicity of hks, karate club", tolerance = 5e-5,
                   note = "a tie count: it does not tell the product of equation (10) from a sum")
})

test_that("niccm reproduces Table 2 of the paper (10 nodes)", {
  figure_1 <- zoob_graph(10L, c(7, 1, 7, 2, 7, 3, 7, 4, 7, 5, 7, 6, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 4, 8, 4, 9, 4, 10))
  printed <- c(0.3187, 0.4442, 0.4089, 1.5492, 0.4791, 0.3373, 0.9807, 0.2141, 0.2141, 0.2141)
  expect_reference(centrality_niccm(figure_1), stats::setNames(printed, 1:10), reference = hu_table_2,
                   covers = c("centrality()", "centrality_niccm()"), quantity = "niccm, Fig. 1 network",
                   tolerance = 1e-4, note = "one unit of the last printed digit; the edge list is reconstructed")
})

test_that("niccm reproduces Table 3 of the paper (ARPA network, 21 nodes)", {
  printed <- c(0.1365, 0.3049, 0.4134, 0.1953, 0.1783, 0.2550, 0.1657, 0.1526, 0.1430, 0.1641, 0.1716, 0.2824,
               0.1904, 0.3791, 0.1891, 0.1451, 0.1679, 0.1819, 0.2619, 0.1711, 0.1555)
  expect_reference(centrality_niccm(arpa), stats::setNames(printed, 1:21), reference = hu_table_3,
                   covers = c("centrality()", "centrality_niccm()"), quantity = "niccm, ARPA network",
                   tolerance = 1e-4, note = "one unit of the last printed digit")
})

test_that("niccm equals equations (1)-(4) written over NetworkX with every node deleted in turn", {
  compare_networkx("niccm", paste0(
    "(lambda n: (lambda eff, D: (lambda I: (lambda g: {j: I[j] * sum(g[i].get(j, 0) for i in G) for j in G})",
    "({i: (lambda Ii: {j: (I[j] - Ii[j]) / sum(I[k] - Ii[k] for k in G if k != i and D[i].get(k) == D[i][j])",
    " / D[i][j] * I[i] for j in G if 1 <= D[i].get(j, 0) <= 2})(eff(G.subgraph(set(G) - {i}))) for i in G}))",
    "(eff(G)))",
    "(lambda H: {v: sum(1 / d for d in nx.single_source_shortest_path_length(H, v).values() if d > 0) / n",
    " for v in H}, dict(nx.all_pairs_shortest_path_length(G))))(len(G))"))
})

test_that("correlation_centrality reproduces Table 1 of the paper (ARPA network)", {
  printed <- c(0.0506, 0.0902, 0.0923, 0.0544, 0.0487, 0.0596, 0.0428, 0.0384, 0.0378, 0.0400, 0.0469, 0.0698,
               0.0573, 0.0876, 0.0720, 0.0585, 0.0608, 0.0597, 0.0710, 0.0491, 0.0472)
  expect_reference(centrality_correlation_centrality(arpa), stats::setNames(printed, 1:21), reference = fan_table_1,
                   covers = c("centrality()", "centrality_correlation_centrality()"),
                   quantity = "correlation_centrality, ARPA network", tolerance = 5e-5,
                   note = "the paper gives the network as a figure; the edge list is the one of the NIEM tests")
})

test_that("correlation_centrality equals equation (4) over NetworkX's enumerated shortest paths", {
  compare_networkx("correlation_centrality", paste0(
    "(lambda n, D: (lambda ec: {i: sum(ec[j] * len(list(nx.all_shortest_paths(G, i, j))) / d ** 3",
    " for j, d in D[i].items() if d >= 1) / n for i in G})",
    "({j: sum(1 / d for d in D[j].values() if d >= 1) / n for j in G}))",
    "(len(G), dict(nx.all_pairs_shortest_path_length(G)))"))
})
