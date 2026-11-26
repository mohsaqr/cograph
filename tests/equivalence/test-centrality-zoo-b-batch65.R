# Fifteen measures of cograph's centrality batch 65, against published values and NetworkX.
#   centrality_dsc(), centrality_dsr(), centrality_edsr(): Zareie, Sheikhahmadi and Jalili (2019), Future Generation
#     Computer Systems 94:120-129, equations (21)-(26); Table 2 prints the monotonicity of the two rankings.
#   centrality_ksgc(): Yang and Xiao (2021), Knowledge-Based Systems 227:107198, equations (7)-(10) (no value on a
#     small network is printed).
#   centrality_lsc(): Gao, Ma, Chen, Wang and Xing (2014), Physica A 403:130-147, equations (5)-(6), Table 1. The
#     paper gives its 28-node example as a figure; the edge list is the one graph found consistent with Table 1.
#   centrality_classified_neighbors(): Li, Wang, Sun and Xia (2018), Applied Mathematics and Computation 320:512-523,
#     equation (1), Table 1 (which lists the class of every neighbor, and with it every edge).
#   centrality_hybrid_degree(): Ma and Ma (2017), Physica A 465:312-330, section 3 (no value is printed).
#   centrality_topsis(): Du, Gao, Hu, Mahadevan and Deng (2014), Physica A 399:57-69, equations (6)-(12), Tables 2-4
#     (a decision matrix that is not a network; no score of a network node is printed).
#   centrality_all_around(): Hou, Yao and Liao (2012), Physica A 391(15):4012-4017, equation (1) (no value printed).
#   centrality_egm(): Li and Xiao (2021), Information Sciences 578:725-749, equations (16)-(22) (no value printed).
#   centrality_gls(): Sheng et al. (2020), Physica A 541:123262, worked example of section 3.2, Tables 3 and 4. The
#     12-node example is a figure; the edge list is the one graph found consistent with Table 3.
#   centrality_ks_if(): Wang, Zhao, Xi and Du (2016), Physica A 461:171-181, equations (1)-(2) (the paper's
#     illustration is not realisable, so no published value is used).
#   centrality_nl(): Shao, Liu, Zhao and Liu (2019), Peer-to-Peer Networking and Applications 12:1528-1537,
#     equation (5) (the paper's tables were not available).
#   centrality_entropy_influence(): Peng, Yang, Cao, Yu and Xie (2017), Information Sciences 379:146-159, equations
#     (2)-(13), Tables 1, 4 and 5.
#   centrality_multi_evidence(): Mo and Deng (2019), Physica A 529:121538, Table 1.
# No other package implements these measures.

zareie_table_2 <- "published: Zareie, Sheikhahmadi and Jalili 2019, Table 2"
gao_table_1 <- "published: Gao et al. 2014, Table 1"
li_table_1 <- "published: Li, Wang, Sun and Xia 2018, Table 1"
du_table_4 <- "published: Du et al. 2014, Table 4"
sheng_example <- "published: Sheng et al. 2020, worked example of section 3.2"
sheng_table_3 <- "published: Sheng et al. 2020, Table 3"
sheng_table_4 <- "published: Sheng et al. 2020, Table 4"
peng_table_4 <- "published: Peng et al. 2017, Table 4"
peng_table_5 <- "published: Peng et al. 2017, Table 5"
mo_table_1 <- "published: Mo and Deng 2019, Table 1"
exact <- 1e-10
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()
# A 4-clique and a triangle joined by a long path, with one pendant: three shells and a mean distance above 4.
long <- zoob_graph(14L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12,
                          11, 13, 12, 13, 13, 14))
graphs <- list(undirected = undirected, sparse = sparse, karate = karate, long = long)
connected <- list(undirected = undirected, karate = karate, long = long)
arpa <- zoob_graph(21L, c(1, 2, 1, 15, 2, 3, 2, 15, 2, 16, 3, 4, 3, 17, 3, 18, 4, 5, 5, 6, 6, 7, 6, 21, 7, 8, 8, 9,
                          9, 10, 10, 11, 11, 12, 12, 13, 12, 19, 13, 14, 14, 15, 14, 16, 14, 17, 18, 19, 19, 20,
                          20, 21))

# Pieces of Python shared by the expressions below.
rounds <- paste0(
  "(lambda H, out: [([out.__setitem__(v, k) for v in batch], H.remove_nodes_from(batch))",
  " for k in range(1, len(G) + 1)",
  " for batch in [[v for v, d in H.degree() if d == min(dict(H.degree()).values())] if len(H) else []]] and out)",
  "(nx.Graph(G), {})")
two_step <- "{w: len(nx.single_source_shortest_path_length(G, w, cutoff=2)) - 1 for w in G}"
distances <- "dict(nx.all_pairs_shortest_path_length(G))"
dsc <- paste0(
  "(lambda it, H: {v: (lambda nb: 0.0 if not nb else (lambda S, prof: (S / len(nb)) * H([it[u] / S for u in nb])",
  " * max(H([sum(p[k] for p in prof) / len(nb) for k in range(len(prof[0]))]) - sum(H(p) for p in prof) / len(nb),",
  " 0))(sum(it[u] for u in nb), [[sum(1 for w in G[u] if it[w] == k) / len(G[u])",
  " for k in range(1, max(it.values()) + 1)] for u in nb]))(list(G[v])) for v in G})",
  "(", rounds, ", lambda p: -sum(x * np.log(x) for x in p if x > 0))")
neighbor_sum <- function(inner) sprintf("(lambda s: {v: sum(s[u] for u in G[v]) for v in G})(%s)", inner)
topsis <- function(third) paste0(
  "(lambda nodes, X: (lambda V: (lambda sp, sm: {v: sm[i] / (sm[i] + sp[i]) for i, v in enumerate(nodes)})",
  "(np.sqrt(((V.max(0) - V) ** 2).sum(1)), np.sqrt(((V - V.min(0)) ** 2).sum(1))))",
  "(X / np.sqrt((X ** 2).sum(0)) / 3))",
  "(list(G), (lambda t: np.array([[G.degree(v), 1 / sum(nx.single_source_shortest_path_length(G, v).values()),",
  " t[v]] for v in G]))(", third, "))")

compare_networkx <- function(measure, call, tolerance = exact, on = graphs, ...) {
  wrapper <- match.fun(paste0("centrality_", measure))
  Map(\(graph, name) {
    expect_reference(wrapper(graph, ...), ref_networkx(graph, c(value = call))$value, reference = "networkx",
                     covers = c("centrality()", sprintf("centrality_%s()", measure)),
                     quantity = sprintf("%s, %s", measure, name), tolerance = tolerance)
  }, on, names(on))
}

monotonicity <- function(score) {
  ties <- table(score)
  (1 - sum(ties * (ties - 1)) / (length(score) * (length(score) - 1)))^2
}

test_that("dsr and edsr reproduce the monotonicity the paper prints for the karate club", {
  expect_reference(monotonicity(centrality_dsr(karate)), 0.9403, reference = zareie_table_2,
                   covers = c("centrality()", "centrality_dsr()", "centrality_dsc()"),
                   quantity = "monotonicity of dsr, karate club", tolerance = 5e-5,
                   note = "a tie count: it rules out the entropy-only reading of dsc (0.9542), not the choice of shell")
  expect_reference(monotonicity(centrality_edsr(karate)), 0.9542, reference = zareie_table_2,
                   covers = c("centrality()", "centrality_edsr()"),
                   quantity = "monotonicity of edsr, karate club", tolerance = 5e-5, note = "a tie count")
})

test_that("dsc, dsr and edsr equal equations (21)-(26) written over NetworkX", {
  compare_networkx("dsc", dsc)
  compare_networkx("dsr", neighbor_sum(dsc))
  compare_networkx("edsr", neighbor_sum(neighbor_sum(dsc)))
})

test_that("ksgc equals equations (7)-(10) written over NetworkX's core numbers and distances", {
  compare_networkx("ksgc", paste0(
    "(lambda D, ks: (lambda r, lo, hi: {i: sum((np.exp((ks[i] - ks[j]) / (hi - lo)) if hi > lo else 1.0)",
    " * G.degree(i) * G.degree(j) / d ** 2 for j, d in D[i].items() if 1 <= d <= r) for i in G})",
    "(0.5 * np.mean([d for i in D for d in D[i].values() if d > 0]), min(ks.values()), max(ks.values())))",
    "(", distances, ", nx.core_number(G))"))
})

test_that("lsc reproduces Table 1 of the paper (28 nodes, alpha = 0.7)", {
  figure_1b <- zoob_graph(28L, c(1, 2, 1, 3, 1, 4, 2, 3, 3, 4, 1, 6, 1, 7, 6, 15, 6, 16, 6, 17, 7, 18, 7, 19, 7, 20,
                                 8, 9, 8, 10, 8, 11, 9, 10, 10, 11, 8, 13, 8, 14, 13, 21, 13, 22, 13, 23, 21, 22,
                                 22, 23, 14, 24, 14, 25, 14, 26, 24, 25, 25, 26, 1, 5, 5, 27, 27, 28, 28, 12, 12, 8))
  printed <- c(`8` = 37.64, `1` = 35.04, `13` = 22.12, `14` = 22.12, `10` = 21.58, `7` = 18.42, `6` = 18.42,
               `3` = 19.38, `9` = 16.74, `11` = 16.74)
  expect_reference(centrality_lsc(figure_1b, lsc_alpha = 0.7)[names(printed)], printed, reference = gao_table_1,
                   covers = c("centrality()", "centrality_lsc()"), quantity = "lsc, Fig. 1(b) network",
                   tolerance = 5e-3, note = "the edge list is reconstructed from the other columns of Table 1")
})

test_that("lsc equals equations (5)-(6) written over NetworkX's two-step balls and clustering", {
  compare_networkx("lsc", paste0(
    "(lambda B, c: {v: sum(0.2 * len(B[u]) + 0.8 * sum(c[w] for w in B[u]) for u in G[v]) for v in G})",
    "({u: set(nx.single_source_shortest_path_length(G, u, cutoff=2)) - {u} for u in G}, nx.clustering(G))"))
})

test_that("classified_neighbors reproduces the classes of Table 1 of the paper (14 nodes)", {
  figure_1 <- zoob_graph(14L, c(1, 4, 2, 4, 3, 4, 4, 7, 5, 14, 6, 7, 6, 8, 6, 9, 7, 8, 7, 9, 8, 9, 8, 10, 8, 12, 8, 13,
                                8, 14, 9, 14, 10, 12, 10, 11, 11, 12, 12, 13), labels = letters[1:14])
  # Number of neighbors of a to n in each class, counted from the cells of Table 1.
  printed <- list(upper = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 2),
                  equal_upper = c(1, 1, 1, 0, 0, 3, 3, 3, 3, 1, 2, 1, 1, 0),
                  equal_lower = c(0, 0, 0, 3, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0),
                  lower = c(0, 0, 0, 0, 0, 0, 1, 4, 1, 0, 0, 0, 0, 1))
  Map(\(count, class, position) {
    selector <- replace(numeric(4), position, 1)
    expect_reference(centrality_classified_neighbors(figure_1, cn_weights = selector),
                     stats::setNames(count, letters[1:14]), reference = li_table_1,
                     covers = c("centrality()", "centrality_classified_neighbors()"),
                     quantity = sprintf("number of %s neighbors, Fig. 1 network", class), tolerance = 0)
  }, printed, names(printed), seq_along(printed))
})

test_that("classified_neighbors equals equation (1) over NetworkX's core numbers and onion layers", {
  compare_networkx("classified_neighbors", paste0(
    "(lambda ks, ol: {i: sum(0.4 if ks[j] > ks[i] else 0.1 if ks[j] < ks[i] else 0.35 if ol[j] >= ol[i] else 0.25",
    " for j in G[i]) for i in G})(nx.core_number(G), nx.onion_layers(G))"))
})

test_that("hybrid_degree equals the formula of section 3 written over NetworkX's two-step balls", {
  compare_networkx("hybrid_degree", hc_p = 0.05, call = paste0(
    "(lambda n2: {v: (0.1 - 0.05) * 1000 * G.degree(v) + 0.05 * (sum(n2[w] for u in G[v] for w in G[u])",
    " - 2 * sum(G.degree(u) for u in G[v])) for v in G})(", two_step, ")"))
})

test_that("topsis reproduces the closeness coefficients of the paper's numerical illustration", {
  decision <- matrix(c(0.950, 0.953, 0.950, 1.000, 0.900, 0.902, 0.974, 0.975, 0.946, 0.984, 0.982, 0.903,
                       1.000, 0.974, 0.925), ncol = 3L, byrow = TRUE)
  printed <- c(`2008` = 0.5622, `2009` = 0.3378, `2010` = 0.7747, `2011` = 0.6330, `2012` = 0.7730)
  computed <- stats::setNames(cograph:::.cg_topsis(decision, rep(1 / 3, 3))$value, names(printed))
  expect_reference(computed, printed, reference = du_table_4, covers = "centrality_topsis()",
                   quantity = "TOPSIS closeness coefficient, five alternatives on three criteria", tolerance = 1.5e-3,
                   note = "the paper divides distances rounded to four decimals; full precision differs by 0.0014")
  expect_reference(rank(-computed), c(`2008` = 4, `2009` = 5, `2010` = 1, `2011` = 3, `2012` = 2),
                   reference = du_table_4, covers = "centrality_topsis()",
                   quantity = "TOPSIS rank, five alternatives on three criteria", tolerance = 0)
})

test_that("topsis equals equations (6)-(12) written with NumPy over NetworkX's three attributes", {
  compare_networkx("topsis", topsis("nx.betweenness_centrality(G, normalized=False)"), on = connected)
  compare_networkx("topsis", topsis("nx.eigenvector_centrality_numpy(G)"), tolerance = 1e-8, on = connected,
                   topsis_attribute = "eigenvector")
})

test_that("all_around equals equation (1) over NetworkX's degree, betweenness and core numbers", {
  compare_networkx("all_around", paste0(
    "(lambda bc, ks, k: (lambda mb, mk, md: {v: np.sqrt((k[v] / md) ** 2 + (bc[v] / mb if mb > 0 else 0) ** 2",
    " + (ks[v] / mk) ** 2) for v in G})(max(bc.values()), max(ks.values()), max(k.values())))",
    "(nx.betweenness_centrality(G, normalized=False), nx.core_number(G), dict(G.degree()))"))
})

test_that("egm equals equations (16)-(22) written over NetworkX's distances", {
  compare_networkx("egm", on = connected, call = paste0(
    "(lambda D, k: (lambda V, R: {i: sum(V[i] * V[j] / d ** 2 for j, d in D[i].items()",
    " if d >= 1 and d <= R[i] * (1 + 1e-9)) for i in G})",
    "({i: -k[i] * sum((k[j] / s) * np.log(k[j] / s) for s in [k[i] + sum(k[u] for u in G[i])]",
    " for j in [i] + list(G[i])) for i in G},",
    " {i: (lambda e: e / (1 + np.sqrt(np.mean([k[j] for j, d in D[i].items() if d == e]) / k[i])))",
    "(max(D[i].values())) for i in G}))(", distances, ", dict(G.degree()))"))
})

test_that("gls reproduces the worked example and Table 3 of the paper (12 nodes)", {
  figure_1a <- zoob_graph(12L, c(7, 8, 7, 9, 7, 11, 7, 12, 7, 6, 4, 2, 4, 3, 4, 1, 4, 5, 5, 6, 5, 10, 6, 10, 10, 1))
  score <- centrality_gls(figure_1a)
  expect_reference(score[["5"]], 9.58, reference = sheng_example, covers = c("centrality()", "centrality_gls()"),
                   quantity = "gls, the node of the worked example", tolerance = 5e-3,
                   note = "the edge list is reconstructed from Table 3, the figure is an image")
  printed_order <- c(7, 4, 6, 5, 10, 1, 8, 9, 11, 12, 2, 3)
  expect_reference(as.numeric(names(score)[order(-score, as.numeric(names(score)))]), printed_order,
                   reference = sheng_table_3, covers = c("centrality()", "centrality_gls()"),
                   quantity = "gls rank order, Fig. 1(a) network", tolerance = 0,
                   note = "tied leaves are listed by label, as in the table")
})

test_that("gls reproduces the top ten of the karate club in Table 4 of the paper", {
  score <- centrality_gls(karate)
  expect_reference(as.numeric(names(score)[order(-score)][1:10]), c(34, 1, 33, 3, 2, 32, 4, 9, 14, 24),
                   reference = sheng_table_4, covers = c("centrality()", "centrality_gls()"),
                   quantity = "gls top ten, karate club", tolerance = 0,
                   note = "the literal equations (4)-(7) give a different order")
})

test_that("gls equals its formula written over NetworkX's common neighbors", {
  compare_networkx("gls", paste0(
    "{i: G.degree(i) ** 2 / (len(G) - 1) * sum(1.1 ** len(list(nx.common_neighbors(G, i, j)))",
    " for j in G if j != i) for i in G}"))
})

test_that("ks_if equals equations (1)-(2) with the iterations read from NetworkX's onion layers", {
  compare_networkx("ks_if", paste0(
    "(lambda ks, ol: (lambda lo, hi: (lambda dl: {i: dl[i] * G.degree(i) + sum(dl[j] * G.degree(j) for j in G[i])",
    " for i in G})({v: ks[v] * (1 + (ol[v] - lo[ks[v]] + 1) / (hi[ks[v]] - lo[ks[v]] + 1)) for v in G}))",
    "({k: min(ol[v] for v in G if ks[v] == k) for k in set(ks.values())},",
    " {k: max(ol[v] for v in G if ks[v] == k) for k in set(ks.values())}))",
    "(nx.core_number(G), nx.onion_layers(G))"))
})

test_that("nl equals equation (5) written over NetworkX's two-step balls and common neighbors", {
  compare_networkx("nl", paste0(
    "(lambda n2, t, k: {v: sum(n2[u] + (k[u] - t(u, v) - 1) * (k[v] - t(u, v) - 1) / (t(u, v) / 2 + 1)",
    " * ((k[v] - 1) / (k[v] + k[u] - 2) if k[v] + k[u] > 2 else 0) for u in G[v]) for v in G})",
    "(", two_step, ", lambda u, v: len(list(nx.common_neighbors(G, u, v))), dict(G.degree()))"))
})

test_that("entropy_influence reproduces Tables 4 and 5 of the paper (11 nodes)", {
  people <- LETTERS[1:11]
  from <- c("A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D", "D", "D", "E", "E", "E", "F", "F",
            "F", "G", "G", "G", "G", "G", "H", "H", "H", "H", "H", "I", "I", "I", "J", "J", "J", "J", "K", "K")
  to <- c("B", "C", "D", "A", "D", "F", "G", "A", "D", "E", "A", "B", "C", "E", "G", "H", "C", "D", "H", "B", "G",
          "I", "B", "D", "F", "H", "J", "D", "E", "G", "J", "K", "F", "G", "J", "G", "H", "I", "K", "E", "J")
  count <- c(3, 6, 9, 6, 15, 4, 8, 16, 7, 11, 4, 6, 7, 3, 4, 17, 4, 8, 8, 2, 1, 7, 7, 8, 8, 21, 5, 3, 11, 13, 7, 3,
             6, 7, 12, 9, 22, 11, 6, 4, 2)
  messages <- matrix(0, 11L, 11L, dimnames = list(people, people))
  messages[cbind(from, to)] <- count
  direct <- c(0.4544, 0.5711, 0.4632, 0.7265, 0.4657, 0.3998, 0.6616, 0.5723, 0.2862, 0.5715, 0)
  expect_reference(centrality_entropy_influence(messages, ei_omega = 1), stats::setNames(direct, people),
                   reference = peng_table_4, covers = c("centrality()", "centrality_entropy_influence()"),
                   quantity = "direct influence, 11-node example", tolerance = 1e-4,
                   note = "one unit of the last printed digit")
  total <- c(A = 0.3881, B = 0.4830, C = 0.3961, D = 0.6151, E = 0.3979, F = 0.3327, J = 0.4757, K = 0)
  score <- centrality_entropy_influence(messages)
  expect_reference(score[names(total)], total, reference = peng_table_4,
                   covers = c("centrality()", "centrality_entropy_influence()"),
                   quantity = "total influence, 8 of the 11 nodes of the example", tolerance = 1.5e-4,
                   note = paste("the printed indirect influence of G, H and I does not follow the paper's equations",
                                "(a common friend and a two-step friend are left out); their totals are not compared"))
  expect_reference(match(people, names(score)[order(-score)]),
                   match(people, c("D", "G", "H", "B", "J", "E", "C", "A", "F", "I", "K")), reference = peng_table_5,
                   covers = c("centrality()", "centrality_entropy_influence()"),
                   quantity = "rank by total influence, 11-node example", tolerance = 0)
})

test_that("entropy_influence equals equations (5)-(13) on unweighted graphs written over NetworkX", {
  compare_networkx("entropy_influence", paste0(
    "(lambda DI, cn: {i: 0.6 * DI[i] + 0.4 * (lambda T: DI[i] * np.mean([np.mean([DI[j] for j in cn(i, k)])",
    " for k in T]) if T else 0.0)([k for k in G if k != i and k not in G[i] and len(cn(i, k)) > 0]) for i in G})",
    "({v: np.log10(G.degree(v)) if G.degree(v) > 0 else 0.0 for v in G},",
    " lambda i, k: list(nx.common_neighbors(G, i, k)))"))
})

test_that("multi_evidence reproduces Table 1 of the paper (ARPA network)", {
  printed <- c(-0.3799, 0.1577, 0.4321, -0.0722, -0.1447, 0.0454, -0.2582, -0.3428, -0.3413, -0.2736, -0.1579,
               0.2046, -0.0879, 0.2012, -0.1460, -0.3194, -0.2337, -0.1222, 0.1347, -0.2214, -0.2805)
  expect_reference(centrality_multi_evidence(arpa), stats::setNames(printed, 1:21), reference = mo_table_1,
                   covers = c("centrality()", "centrality_multi_evidence()"),
                   quantity = "multi_evidence, ARPA network", tolerance = 2e-4,
                   note = paste("two units of the last printed digit: the paper fuses inputs rounded to four",
                                "decimals; the edge list is the one of the NIEM tests"))
})

test_that("multi_evidence equals Dempster's rule over four indices computed with NetworkX", {
  compare_networkx("multi_evidence", paste0(
    "(lambda n, D, bc: (lambda ec: (lambda coc: (lambda bpa, comb: (lambda M: {v: M[i, 0] - M[i, 1]",
    " for i, v in enumerate(G)})(comb(comb(comb(bpa([G.degree(v) / (n - 1) for v in G]), bpa([bc[v] for v in G])),",
    " bpa([ec[v] for v in G])), bpa([coc[v] for v in G]))))",
    "(lambda x: (lambda x, s: np.column_stack([(x - x.min()) / s, (x.max() - x) / s,",
    " 0 * x + 1 - (x.max() - x.min()) / s]))(np.array(x), max(x) - min(x) + 1),",
    " lambda a, b: np.column_stack([a[:, 0] * b[:, 0] + a[:, 0] * b[:, 2] + a[:, 2] * b[:, 0],",
    " a[:, 1] * b[:, 1] + a[:, 1] * b[:, 2] + a[:, 2] * b[:, 1], a[:, 2] * b[:, 2]])",
    " / (1 - a[:, 0] * b[:, 1] - a[:, 1] * b[:, 0])[:, None]))",
    "({i: sum(ec[j] * len(list(nx.all_shortest_paths(G, i, j))) / d ** 3 for j, d in D[i].items() if d >= 1) / n",
    " for i in G}))({j: sum(1 / d for d in D[j].values() if d >= 1) / n for j in G}))",
    "(len(G), ", distances, ", nx.betweenness_centrality(G, normalized=True))"))
})
