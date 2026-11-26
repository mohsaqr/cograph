# Eight measures of cograph's centrality batch 67, and the Weighted TOPSIS mapping, against published values and
# NetworkX.
#   centrality_mapping_entropy(): Nie, Guo, Zhao and Lu (2016), Physica A 453:290-297, equation (3) with
#     DC = d / (N - 1); no value is printed.
#   centrality_dcc(): Yang, Wang, Chen, Hu and Ruan (2020), IEEE Access 8:58742-58751, equations (7)-(12), the
#     worked example on Fig. 1(b) and Tables 2-3 on the karate club network. Fig. 1(b) is a figure; the edge list
#     was read from it.
#   centrality_edge_clustering(): Wang, Li, Wang and Pan (2012), IEEE/ACM TCBB 9(4):1070-1080, equations (7)-(8);
#     the one printed value is an edge value (tests/testthat/test-centrality-batch67.R).
#   centrality_ahp(): Bian, Hu and Deng (2017), Physica A 479:422-436, section 4 and the worked example on the kite
#     (Fig. 2, Table 3). The kite is numbered as the paper numbers it.
#   centrality_semi_ia(): Luan, Bao and Zhang (2021), Journal of Systems Science and Complexity 34(6):2168-2181,
#     equations (8)-(9) with X'_j in the sum, and Table 2.
#   centrality_synthesize(): Liu, Yu and Lu (2013), ICSSC 2013, 268-271, equations (6)-(9); no value is printed.
#   centrality_inward_accessibility(): Travencolo and Costa (2008), Physics Letters A 373(1):89-95, equations
#     (4)-(5); no value is printed.
#   centrality_improved_entropy(): Ni, Yang and Kong (2020), Physica A 545:123659, equations (2)-(10); no value is
#     printed.
#   centrality_topsis() as the Zoo's Weighted TOPSIS: Hu, Du, Mo, Wei and Deng (2016), Physica A 444:73-85,
#     Table 2 on the 6-node network of Fig. 2, with the weights its worked example applies.
# No other package implements these measures.

yang_table_2 <- "published: Yang, Wang, Chen, Hu and Ruan 2020, Table 2"
yang_table_3 <- "published: Yang, Wang, Chen, Hu and Ruan 2020, Table 3"
yang_example <- "published: Yang, Wang, Chen, Hu and Ruan 2020, worked example of section III"
bian_example <- "published: Bian, Hu and Deng 2017, worked example of section 4.2"
bian_table_3 <- "published: Bian, Hu and Deng 2017, Table 3"
luan_table_2 <- "published: Luan, Bao and Zhang 2021, Table 2"
hu_table_2 <- "published: Hu, Du, Mo, Wei and Deng 2016, Table 2"
exact <- 1e-10
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()
# A 4-clique and a triangle joined by a long path, with one pendant.
long <- zoob_graph(14L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12,
                          11, 13, 12, 13, 13, 14))
graphs <- list(undirected = undirected, sparse = sparse, karate = karate, long = long)
connected <- list(undirected = undirected, karate = karate, long = long)
weighted <- list(weighted = ref_connected_graph(12, density = 0.3, seed = 47L, weighted = TRUE, digits = 6L),
                 unweighted = undirected, karate = karate)

compare_networkx <- function(measure, call, tolerance = exact, on = graphs, ...) {
  wrapper <- match.fun(paste0("centrality_", measure))
  Map(\(graph, name) {
    expect_reference(wrapper(graph, ...), ref_networkx(graph, c(value = call))$value, reference = "networkx",
                     covers = c("centrality()", sprintf("centrality_%s()", measure)),
                     quantity = sprintf("%s, %s", measure, name), tolerance = tolerance)
  }, on, names(on))
}

# Shared Python pieces.
distances <- "dict(nx.all_pairs_shortest_path_length(G))"
closeness <- "{i: 1 / sum(nx.single_source_shortest_path_length(G, i).values()) for i in G}"

test_that("mapping_entropy equals equation (3) with DC = d / (N - 1) written over NetworkX", {
  compare_networkx("mapping_entropy", paste0(
    "{i: -(G.degree(i) / (len(G) - 1)) * sum(np.log(G.degree(j) / (len(G) - 1)) for j in G[i]) for i in G}"))
})

test_that("dcc reproduces the karate results of Tables 2 and 3 and not the entropy weights of the example", {
  score <- centrality_dcc(karate)
  expect_reference(as.numeric(names(score)[order(-score, seq_along(score))][1:5]), c(1, 34, 3, 33, 32),
                   reference = yang_table_3, covers = c("centrality()", "centrality_dcc()"),
                   quantity = "DCC top five, karate club", tolerance = 0, note = "Zachary's 34 nodes and 78 edges")
  rounded <- round(score, 12)
  expect_reference(100 * mean(rounded %in% rounded[duplicated(rounded)]), 32.35, reference = yang_table_2,
                   covers = c("centrality()", "centrality_dcc()"),
                   quantity = "DCC share of tied nodes (TRF), karate club, percent", tolerance = 0.005,
                   note = "11 of the 34 scores are tied; the table prints two decimals")
  figure_1b <- zoob_graph(28L, c(1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 1, 7, 2, 3, 3, 4, 5, 15, 6, 20, 6, 21, 6, 22, 7, 17,
                                 7, 18, 7, 19, 8, 9, 8, 10, 8, 11, 8, 12, 8, 13, 8, 14, 9, 10, 10, 11, 11, 16, 12, 16,
                                 13, 26, 13, 27, 13, 28, 26, 27, 27, 28, 14, 23, 14, 24, 14, 25, 23, 24, 24, 25))
  expect_known_difference(centrality_dcc(figure_1b)[["1"]], 16.2677, reference = yang_example,
                          covers = c("centrality()", "centrality_dcc()"), quantity = "DCC of node 1, Fig. 1(b)",
                          tolerance = 5e-5, reason = paste(
                            "the printed I_D(1) = 23 and I_C(1) = 2.3338 follow from the network drawn in Fig. 1(b),",
                            "but its row norms (61.51, 11.47) and so E_1 = 2.4011 and alpha = 0.6742 do not (62.45,",
                            "11.26, 2.3759, 0.6488 here); no single edge change of the figure gives its norms"),
                          note = "edge list read from Fig. 1(b), 28 nodes and 35 edges")
})

test_that("dcc equals equations (7)-(12) written over NetworkX's clustering and two-step balls", {
  compare_networkx("dcc", on = connected, paste0(
    "(lambda C, n: (lambda ID, IC: (lambda E: (lambda a: {i: a * ID[i] + (1 - a) * IC[i] for i in G})",
    "((1 - E[0]) / (2 - E[0] - E[1])))([-sum(v / np.linalg.norm(list(X.values()))",
    " * np.log(v / np.linalg.norm(list(X.values()))) for v in X.values() if v > 0) / np.log(n) for X in (ID, IC)]))",
    "({i: G.degree(i) + sum(G.degree(j) for j in G[i]) for i in G},",
    " {i: np.exp(-C[i]) * sum(C[j] for j in nx.single_source_shortest_path_length(G, i, cutoff=2) if j != i)",
    " for i in G}))(nx.clustering(G), len(G))"))
})

test_that("edge_clustering equals equations (7)-(8) written with Python sets", {
  compare_networkx("edge_clustering", paste0(
    "{u: sum(len(set(G[u]) & set(G[v])) / (min(G.degree(u), G.degree(v)) - 1) for v in G[u]",
    " if min(G.degree(u), G.degree(v)) > 1) for u in G}"))
})

test_that("ahp reproduces the shares and the ranking of the worked example on the kite", {
  # Fig. 2, numbered as the paper: 1 Beverly, 2 Andre, 3 Carol, 4 Fernando, 5 Garth, 6 Ed, 7 Diane, 8 Heather,
  # 9 Ike, 10 Jane.
  kite <- zoob_graph(10L, c(2, 1, 2, 3, 2, 7, 2, 4, 1, 7, 1, 6, 1, 5, 3, 7, 3, 4, 7, 6, 7, 4, 7, 5, 6, 5, 4, 5,
                            4, 8, 5, 8, 8, 9, 9, 10))
  note <- "edge list read from Fig. 2; printed to four decimals"
  share <- \(w) centrality_ahp(kite, ahp_weights = w)
  expect_reference(share(c(1, 0, 0)), stats::setNames(c(0.1111, 0.1111, 0.0833, 0.1389, 0.1389, 0.0833, 0.1667,
                                                         0.0833, 0.0556, 0.0278), 1:10),
                   reference = bian_example, covers = c("centrality()", "centrality_ahp()"),
                   quantity = "AHP priority vector s(1) of the degree, ahp_weights = c(1, 0, 0)", tolerance = 5e-5,
                   note = note)
  expect_reference(share(c(0, 0, 1)), stats::setNames(c(0.1002, 0.1002, 0.0946, 0.1217, 0.1217, 0.0946, 0.1136,
                                                         0.1136, 0.0811, 0.0587), 1:10),
                   reference = bian_example, covers = c("centrality()", "centrality_ahp()"),
                   quantity = "AHP priority vector s(3) of the closeness, ahp_weights = c(0, 0, 1)",
                   tolerance = 5e-5, note = note)
  printed_b2 <- paste("the printed B(2) is not the kite's betweenness: it gives the automorphic nodes 4 and 5",
                      "different values")
  expect_known_difference(share(c(0, 1, 0)), stats::setNames(c(0.0155, 0.0155, 0, 0.1487, 0.1208, 0, 0.0186, 0.2416,
                                                                0.1394, 0), 1:10),
                          reference = bian_example, covers = c("centrality()", "centrality_ahp()"),
                          quantity = "AHP priority vector s(2) of the betweenness, ahp_weights = c(0, 1, 0)",
                          tolerance = 5e-5, reason = printed_b2, note = note)
  # The printed weights are in the column order of matrix A, whose closeness and betweenness headings are exchanged;
  # paired with the measure each was computed from they are DC 0.3354, BC 0.3249, CC 0.3397.
  score <- share(c(0.3354, 0.3249, 0.3397))
  expect_reference(as.numeric(names(score)[order(-score, seq_along(score))]), c(8, 4, 5, 7, 9, 1, 2, 3, 6, 10),
                   reference = bian_table_3, covers = c("centrality()", "centrality_ahp()"),
                   quantity = "AHP ranking of the kite, the paper's weights", tolerance = 0,
                   note = "Fernando (4) and Garth (5) are tied on the kite; listed by number, as in the table")
  expect_known_difference(score, stats::setNames(c(0.0762, 0.0762, 0.0599, 0.1363, 0.1272, 0.0599, 0.1005, 0.1451,
                                                   0.0915, 0.0291), 1:10),
                          reference = bian_example, covers = c("centrality()", "centrality_ahp()"),
                          quantity = "AHP scores v of the kite, the paper's weights", tolerance = 5e-5,
                          reason = paste(printed_b2, "and the final scores are built on it"), note = note)
})

test_that("ahp equals the shares of section 4 written over NetworkX", {
  compare_networkx("ahp", on = connected, paste0(
    "(lambda X: (lambda S: {i: sum(S[c][i] for c in range(3)) / 3 for i in G})",
    "([{i: x[i] / sum(x.values()) * sum(1 for v in x.values() if v > 0) / len(G) for i in G} for x in X]))",
    "([dict(G.degree()), nx.betweenness_centrality(G, normalized=False), ", closeness, "])"))
})

test_that("semi_ia reproduces the monotonicity of Table 2 on the adjective-noun network", {
  score <- centrality_semi_ia(zoob_adjnoun())
  ties <- table(signif(score, 12))
  n <- length(score)
  expect_reference((1 - sum(ties * (ties - 1)) / (n * (n - 1)))^2, 0.9997, reference = luan_table_2,
                   covers = c("centrality()", "centrality_semi_ia()"),
                   quantity = "monotonicity M of the semi-IA ranking, Word (adjnoun) network", tolerance = 5e-5,
                   note = paste("netzschleuder copy of the network (N 112, M 425, as Table 1); the value does not",
                                "separate the two readings of equation (8)"))
})

test_that("semi_ia equals the leading eigenpair of W over NetworkX's path counts", {
  compare_networkx("semi_ia", on = connected, tolerance = 1e-9, paste0(
    "(lambda D: (lambda w, v: dict(zip(G, w[-1] * np.abs(v[:, -1]))))(*np.linalg.eigh(np.array(",
    "[[len(list(nx.all_shortest_paths(G, i, j))) ** 0.2 / D[i][j] if j in D[i] and 1 <= D[i][j] <= 3 else 0.0",
    " for j in G] for i in G]))))(", distances, ")"))
})

test_that("synthesize equals equations (6)-(9) written over NetworkX", {
  compare_networkx("synthesize", paste0(
    "(lambda n, B, D: {v: (G.degree(v) / (n - 1) + 2 * B[v] / (n - 1) ** 2)",
    " / (sum(D[i].get(v, 1000) for i in G if i != v) / (n - 1) ** 2) for v in G})",
    "(len(G), nx.betweenness_centrality(G, normalized=False), ", distances, ")"))
})

# Self-avoiding walks of h steps from every node, enumerated by a recursive Python function; `stay` keeps a stopped
# walk at its last node, otherwise it is dropped.
walks <- function(h, stay) paste0(
  "(lambda walk: (lambda P: {i: np.exp(-sum(q * np.log(q) for j in G if j != i",
  " for q in [P[j].get(i, 0) / (len(G) - 1)] if q > 0)) / (len(G) - 1) for i in G})",
  "({s: walk(walk, [s], 1.0, ", h, ", {}) for s in G}))",
  "(lambda self, path, p, h, out: (out.__setitem__(path[-1], out.get(path[-1], 0) + p) or out) if h == 0 else",
  " (lambda nxt: ([self(self, path + [v], p / len(nxt), h - 1, out) for v in nxt] and out) if nxt else ",
  if (stay) "(out.__setitem__(path[-1], out.get(path[-1], 0) + p) or out)" else "out",
  ")([v for v in G[path[-1]] if v not in path]))")

test_that("inward_accessibility equals equations (4)-(5) over self-avoiding walks enumerated in Python", {
  for (h in 1:3) {
    compare_networkx("inward_accessibility", walks(h, FALSE), ia_steps = h)
    compare_networkx("inward_accessibility", walks(h, TRUE), on = list(sparse = sparse, karate = karate),
                     ia_steps = h, ia_stuck = "stay")
  }
})

test_that("improved_entropy equals equations (2)-(10) written over NetworkX with weights", {
  compare_networkx("improved_entropy", on = weighted, paste0(
    "(lambda H: (lambda DI: {i: 0.5 * DI[i] + 0.5 * (lambda K: sum(DI[i] * np.mean([DI[j] for j in",
    " set(G[i]) & set(G[k])]) for k in K) / len(K) if K else 0.0)([k for k in G if k != i and k not in G[i]",
    " and set(G[i]) & set(G[k])]) for i in G})({i: 0.5 * H([G[i][j]['weight'] for j in G[i]])",
    " + 0.5 * H([G.degree(j) ** 2 for j in G[i]]) for i in G}))",
    "(lambda x: -sum(v / sum(x) * np.log10(v / sum(x)) for v in x))"))
})

test_that("topsis with the weights of Hu et al. reproduces their Table 2 (Weighted TOPSIS, Fig. 2)", {
  figure_2 <- zoob_graph(6L, c(1, 2, 1, 4, 1, 5, 1, 6, 3, 4, 3, 5, 5, 6))
  # The example prints W = (0.5824, 0.0705, 0.3471) in the order degree, betweenness, closeness and multiplies it
  # into the attributes in the order degree, closeness, betweenness, which is the order of topsis_weights.
  score <- centrality_topsis(figure_2, topsis_weights = c(0.5824, 0.0705, 0.3471))
  note <- "edge list read from Fig. 2 (seven edges; the caption says six, Table 1's degrees need seven)"
  expect_reference(score, stats::setNames(c(1, 0, 0.2216, 0.2558, 0.4971, 0.2028), 1:6), reference = hu_table_2,
                   covers = c("centrality()", "centrality_topsis()"), quantity = "w-TOPSIS closeness C, Fig. 2",
                   tolerance = 5e-5, note = paste(note, "; printed to four decimals"))
  expect_reference(rank(-score), stats::setNames(c(1, 6, 4, 3, 2, 5), 1:6), reference = hu_table_2,
                   covers = c("centrality()", "centrality_topsis()"), quantity = "w-TOPSIS rank, Fig. 2",
                   tolerance = 0, note = note)
})
