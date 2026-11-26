# Thirteen measures of cograph's centrality batch 66, against published values and NetworkX.
#   centrality_ksh(), centrality_eskh(): Namtirtha, Dutta and Dutta (2018), Physica A 499:310-324, equations (1)-(4),
#     Table 3 and the worked examples of sections 4.4-4.5; Maji, Namtirtha, Dutta and Curado Malta (2020), Expert
#     Systems with Applications 144:113092, Table 1, print the same network again. The 13-node network is a figure in
#     both papers; the edge list was read from the figures, which agree.
#   centrality_ikh(), centrality_eikh(): Maji et al. (2020), equations (16)-(19), Table 1 and the worked examples of
#     sections 5.1-5.2.
#   centrality_mcde(): Sheikhahmadi and Nematbakhsh (2017), Journal of Information Science 43(3):412-423, equation
#     (8), Table 2. The 23-node network is a figure; the edge list was read from it.
#   centrality_mcdwe(), centrality_mcsde(), centrality_mcsdwe(): the same paper, equations (11)-(14); no value of
#     these three is printed.
#   centrality_lgc(): Zhang, Shuai and Lu (2022), Information Sciences 618:98-117, equations (10)-(12), Tables 1-2
#     and the worked example of section 3.2. The ARPA network is a figure; the edge list was read from it.
#   centrality_wgravity(): Liu, Wang and Deng (2020), Knowledge-Based Systems 193:105464, equations (7)-(9); no
#     value is printed.
#   centrality_asp_centrality(): Lv, Zhao, Xiong and Chen (2019), Physica A 523:488-497, equations (9)-(11) and the
#     worked example of section 3. The 9-node network is a figure; the edge list was read from it.
#   centrality_wdc(), centrality_ewdc(): Liu, Wei, Du, Xiao and Deng (2016), Chaos, Solitons & Fractals 86:1-7,
#     equations (4)-(9); no value on an available network is printed.
# No other package implements these measures.

namtirtha_table_3 <- "published: Namtirtha, Dutta and Dutta 2018, Table 3"
namtirtha_example <- "published: Namtirtha, Dutta and Dutta 2018, worked examples of sections 4.4 and 4.5"
maji_table_1 <- "published: Maji, Namtirtha, Dutta and Curado Malta 2020, Table 1"
maji_example <- "published: Maji, Namtirtha, Dutta and Curado Malta 2020, worked examples of sections 5.1 and 5.2"
sheikh_table_2 <- "published: Sheikhahmadi and Nematbakhsh 2017, Table 2"
zhang_table_1 <- "published: Zhang, Shuai and Lu 2022, Table 1"
zhang_table_2 <- "published: Zhang, Shuai and Lu 2022, Table 2"
zhang_example <- "published: Zhang, Shuai and Lu 2022, worked example of section 3.2"
lv_example <- "published: Lv, Zhao, Xiong and Chen 2019, worked example of section 3"
exact <- 1e-10
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()
# A 4-clique and a triangle joined by a long path, with one pendant: three shells and a mean distance above 4.
long <- zoob_graph(14L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12,
                          11, 13, 12, 13, 13, 14))
graphs <- list(undirected = undirected, sparse = sparse, karate = karate, long = long)
connected <- list(undirected = undirected, karate = karate, long = long)
fig1_letters <- zoob_graph(13L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 1, 5, 1, 6, 5, 6, 5, 7, 6, 7, 7, 8, 8, 9, 8, 10,
                                  9, 10, 9, 11, 10, 12, 10, 13), labels = letters[1:13])
truncated <- \(x) floor(x * 100 + 1e-9) / 100

# Pieces of Python shared by the expressions below.
distances <- "dict(nx.all_pairs_shortest_path_length(G))"
two_step <- "{w: len(nx.single_source_shortest_path_length(G, w, cutoff=2)) - 1 for w in G}"
neighbor_sum <- function(inner) sprintf("(lambda s: {v: sum(s[u] for u in G[v]) for v in G})(%s)", inner)
ksh <- paste0(
  "(lambda D, ks: {m: sum((np.sqrt(ks[m] + ks[n]) + 0.4 * G.degree(n)) / d ** 2 for n, d in D[m].items()",
  " if 1 <= d <= 3) for m in G})(", distances, ", nx.core_number(G))")
ikh <- paste0(
  "(lambda D, ks: {m: sum((np.sqrt(ks[m] * ks[n]) + 2 * (3 - d + 1) / (3 * 4) * G.degree(n)) / d ** 2",
  " for n, d in D[m].items() if 1 <= d <= 3) for m in G})(", distances, ", nx.core_number(G))")
entropy <- function(weight) paste0(
  "{v: -sum(x * np.log2(x)", weight, " for s, x in [(s, sum(1 for u in G[v] if ks[u] == s) / G.degree(v))",
  " for s in set(ks.values()) if G.degree(v) > 0] if x > 0) for v in G}")
plain <- ""
by_shell <- " / (max(ks.values()) - s + 1)"
mixed <- function(weight) paste0(
  "(lambda ks: (lambda H: {v: ks[v] + G.degree(v) + H[v] for v in G})(", entropy(weight), "))(nx.core_number(G))")
semi_mixed <- function(weight) paste0(
  "(lambda ks, n2: (lambda cols: {v: sum((c[v] - min(c.values())) / (max(c.values()) - min(c.values()))",
  " if max(c.values()) > min(c.values()) else 0 for c in cols) for v in G})",
  "([ks, {v: sum(n2[w] for u in G[v] for w in G[u]) for v in G}, ", entropy(weight), "]))",
  "(nx.core_number(G), ", two_step, ")")
half_mean <- "0.5 * np.mean([d for i in D for d in D[i].values() if d > 0])"
assortativity <- "(lambda r: 0.0 if r != r else abs(r))(nx.degree_assortativity_coefficient(G))"
wdc <- "{i: (sum(G.degree(j) for j in G[i]) - G.degree(i)) * G.degree(i) ** a for i in G}"

compare_networkx <- function(measure, call, tolerance = exact, on = graphs, ...) {
  wrapper <- match.fun(paste0("centrality_", measure))
  Map(\(graph, name) {
    expect_reference(wrapper(graph, ...), ref_networkx(graph, c(value = call))$value, reference = "networkx",
                     covers = c("centrality()", sprintf("centrality_%s()", measure)),
                     quantity = sprintf("%s, %s", measure, name), tolerance = tolerance)
  }, on, names(on))
}

test_that("ksh reproduces Table 3 of Namtirtha et al. and Table 1 of Maji et al. (13 nodes)", {
  score <- centrality_ksh(fig1_letters)
  printed_3 <- c(a = 19.06, b = 13.84, c = 13.84, d = 13.84, e = 14.76, f = 14.76, g = 14.21, h = 13.66, i = 11.50,
                 j = 12.70, k = 5.22, l = 5.77, m = 5.77)
  expect_reference(truncated(score), printed_3, reference = namtirtha_table_3,
                   covers = c("centrality()", "centrality_ksh()"), quantity = "ksh, Fig. 1 network, mu 0.4, r 3",
                   tolerance = 1e-9, note = paste("the paper truncates to two decimals, so the scores are truncated",
                                                  "before the comparison; edge list read from Fig. 1"))
  expect_reference(truncated(score[["e"]]), 14.76, reference = namtirtha_example,
                   covers = c("centrality()", "centrality_ksh()"), quantity = "ksh of node e, worked example",
                   tolerance = 1e-9, note = "truncated to two decimals, as printed")
  printed_1 <- c(a = 19.06, b = 13.85, c = 13.85, d = 13.85, e = 14.77, f = 14.77, g = 14.22, h = 13.67, i = 11.51,
                 j = 12.71, k = 5.23, l = 5.78, m = 5.78)
  expect_reference(score, printed_1, reference = maji_table_1, covers = c("centrality()", "centrality_ksh()"),
                   quantity = "ksh, Fig. 1 network (v1-v13), mu 0.4, r 3", tolerance = 0.005,
                   note = "half a unit of the second decimal; v1-v13 are a-m; edge list read from Fig. 1")
})

test_that("eskh reproduces Table 1 of Maji et al. and 11 of the 14 values of Namtirtha et al.", {
  score <- centrality_eskh(fig1_letters)
  printed_1 <- c(a = 71.08, b = 46.76, c = 46.76, d = 46.76, e = 48.05, f = 48.05, g = 43.21, h = 38.43, i = 31.60,
                 j = 36.74, k = 11.51, l = 12.71, m = 12.71)
  expect_reference(score, printed_1, reference = maji_table_1, covers = c("centrality()", "centrality_eskh()"),
                   quantity = "ksh+, Fig. 1 network, mu 0.4, r 3", tolerance = 0.005,
                   note = "half a unit of the second decimal; edge list read from Fig. 1")
  printed_3 <- c(a = 71.08, b = 46.75, c = 46.75, d = 46.75, g = 43.20, h = 38.43, i = 31.60, j = 36.73, k = 11.50,
                 l = 12.70, m = 12.70)
  expect_reference(truncated(score[names(printed_3)]), printed_3, reference = namtirtha_table_3,
                   covers = c("centrality()", "centrality_eskh()"), quantity = "ksh+, Fig. 1 network, 11 nodes",
                   tolerance = 1e-9, note = "truncated to two decimals, as printed")
  expect_known_difference(truncated(score[c("e", "f")]), c(e = 48.03, f = 48.03), reference = namtirtha_table_3,
                          covers = c("centrality()", "centrality_eskh()"), quantity = "ksh+ of nodes e and f",
                          tolerance = 1e-9, reason = paste(
                            "the paper adds the already truncated ksh values 14.21 + 14.76 + 19.06 = 48.03; the",
                            "exact sum is 48.045, which Maji et al. print as 48.05"))
  expect_known_difference(truncated(score[["e"]]), 48.03, reference = namtirtha_example,
                          covers = c("centrality()", "centrality_eskh()"), quantity = "ksh+ of node e, worked example",
                          tolerance = 1e-9, reason = "the same sum of truncated values as in Table 3")
})

test_that("ikh and eikh reproduce Table 1 and the worked examples of Maji et al. (13 nodes)", {
  ikh_score <- centrality_ikh(fig1_letters)
  eikh_score <- centrality_eikh(fig1_letters)
  printed_ikh <- c(a = 22.59, b = 16.55, c = 16.55, d = 16.55, e = 15.86, f = 15.86, g = 14.62, h = 14.18, i = 11.59,
                   j = 12.57, k = 4.68, l = 5.30, m = 5.30)
  printed_eikh <- c(a = 81.38, b = 55.69, c = 55.69, d = 55.69, e = 53.07, f = 53.07, g = 45.90, h = 38.79,
                    i = 31.42, j = 36.36, k = 11.59, l = 12.57, m = 12.57)
  note <- "half a unit of the second decimal; edge list read from Fig. 1 (v1-v13 are a-m)"
  expect_reference(ikh_score, printed_ikh, reference = maji_table_1, covers = c("centrality()", "centrality_ikh()"),
                   quantity = "kshi, Fig. 1 network, r 3", tolerance = 0.005, note = note)
  expect_reference(eikh_score, printed_eikh, reference = maji_table_1,
                   covers = c("centrality()", "centrality_eikh()"), quantity = "kshi+, Fig. 1 network, r 3",
                   tolerance = 0.005, note = note)
  expect_reference(c(ikh = ikh_score[["e"]], eikh = eikh_score[["e"]]), c(ikh = 15.86, eikh = 53.07),
                   reference = maji_example, covers = c("centrality()", "centrality_ikh()", "centrality_eikh()"),
                   quantity = "kshi and kshi+ of node v5, worked examples", tolerance = 0.005, note = note)
})

test_that("ksh, eskh, ikh and eikh equal equations (1)-(4) and (16)-(19) written over NetworkX", {
  compare_networkx("ksh", ksh)
  compare_networkx("eskh", neighbor_sum(ksh))
  compare_networkx("ikh", ikh)
  compare_networkx("eikh", neighbor_sum(ikh))
})

test_that("mcde reproduces Table 2 of the paper (23 nodes)", {
  figure_1 <- zoob_graph(23L, c(1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 1, 9, 1, 10, 2, 3, 2, 4, 2, 11, 2, 13, 2, 14, 2, 15,
                                3, 4, 3, 7, 4, 7, 4, 16, 4, 17, 4, 18, 5, 6, 5, 19, 5, 20, 7, 8, 7, 21, 11, 12, 16, 22,
                                16, 23))
  score <- centrality_mcde(figure_1)
  printed <- c(`1` = 11.6, `2` = 11, `3` = 7.8, `5` = 7.5, `6` = 5, `7` = 7, `8` = 2, `9` = 2, `10` = 2, `11` = 4,
               `12` = 2, `13` = 2, `14` = 2, `15` = 2, `16` = 4.9, `17` = 2, `18` = 2, `19` = 2, `20` = 2, `21` = 2,
               `22` = 2, `23` = 2)
  note <- "half a unit of the printed decimal; edge list read from Fig. 1"
  expect_reference(score[names(printed)], printed, reference = sheikh_table_2,
                   covers = c("centrality()", "centrality_mcde()"), quantity = "MCDE, Fig. 1 network, 22 nodes",
                   tolerance = 0.05, note = note)
  expect_known_difference(score[["4"]], 11.5, reference = sheikh_table_2,
                          covers = c("centrality()", "centrality_mcde()"), quantity = "MCDE of node 4",
                          tolerance = 0.05, reason = paste(
                            "the exact value is 11.4488 (entropy 1.4488, shares 3/7, 1/7, 3/7); rounding the entropy",
                            "to two decimals and then to one gives the printed 1.5, direct rounding 1.4"))
  expect_reference(as.numeric(names(score)[order(-score, as.numeric(names(score)))]),
                   c(1, 4, 2, 3, 5, 7, 6, 16, 11, 8:10, 12:15, 17:23), reference = sheikh_table_2,
                   covers = c("centrality()", "centrality_mcde()"), quantity = "MCDE rank order, Fig. 1 network",
                   tolerance = 0, note = "tied leaves are listed by label, as in the table")
})

test_that("mcde and mcdwe equal equations (8)-(12) written over NetworkX's core numbers", {
  compare_networkx("mcde", mixed(plain))
  compare_networkx("mcdwe", mixed(by_shell))
})

test_that("mcsde and mcsdwe equal equations (13)-(14), min-max scaled, written over NetworkX", {
  compare_networkx("mcsde", semi_mixed(plain))
  compare_networkx("mcsdwe", semi_mixed(by_shell))
})

test_that("lgc reproduces Tables 1 and 2 and the worked example of the paper (ARPA network)", {
  arpa <- zoob_graph(21L, c(1, 2, 1, 15, 2, 3, 2, 15, 2, 16, 3, 4, 3, 17, 3, 18, 4, 5, 5, 6, 6, 7, 6, 21, 7, 8, 8, 9,
                            9, 10, 10, 11, 11, 12, 12, 13, 12, 19, 13, 14, 14, 15, 14, 16, 14, 17, 18, 19, 19, 20,
                            20, 21))
  note <- "edge list read from Fig. 1"
  expect_reference(centrality_laplacian(arpa),
                   stats::setNames(c(20, 42, 40, 18, 16, 24, 16, 14, 14, 14, 16, 26, 20, 38, 32, 22, 22, 20, 26, 16,
                                     16), 1:21),
                   reference = zhang_table_1, covers = c("centrality()", "centrality_laplacian()"),
                   quantity = "Laplacian centrality LC, the mass of LGC, ARPA network", tolerance = 0, note = note)
  score <- centrality_lgc(arpa)
  expect_reference(score[["2"]], 4788, reference = zhang_example, covers = c("centrality()", "centrality_lgc()"),
                   quantity = "LGC of node 2", tolerance = 0, note = note)
  printed <- c(0.247, 1, 0.839, 0.14, 0.064, 0.173, 0.049, 0.006, 0, 0.006, 0.056, 0.278, 0.202, 0.741, 0.639,
               0.311, 0.301, 0.211, 0.278, 0.064, 0.056)
  expect_reference((score - min(score)) / (max(score) - min(score)), stats::setNames(printed, 1:21),
                   reference = zhang_table_2, covers = c("centrality()", "centrality_lgc()"),
                   quantity = "LGC min-max rescaled, ARPA network", tolerance = 5e-4,
                   note = paste("the table rescales to [0, 1]; half a unit of the third decimal;", note))
})

test_that("lgc equals equations (11)-(12) over NetworkX's Laplacian centrality and distances", {
  compare_networkx("lgc", paste0(
    "(lambda D, lc: (lambda r: {i: sum(lc[i] * lc[j] / d ** 2 for j, d in D[i].items() if 1 <= d <= r)",
    " for i in G})(", half_mean, "))(", distances,
    # LC is a whole number on an unweighted graph; NetworkX computes it from eigenvalues, so it is rounded.
    ", {i: round(v) for i, v in nx.laplacian_centrality(G, normalized=False).items()})"))
})

test_that("wgravity equals equation (9) over NumPy's eigenvector and NetworkX's distances", {
  compare_networkx("wgravity", on = connected, tolerance = 1e-8, call = paste0(
    "(lambda D, e: (lambda r: {i: e[i] * sum(G.degree(i) * G.degree(j) / d ** 2 for j, d in D[i].items()",
    " if 1 <= d <= r) for i in G})(", half_mean, "))(", distances,
    ", (lambda w, v: dict(zip(G, np.abs(v[:, np.argmax(w)]))))(*np.linalg.eigh(nx.to_numpy_array(G))))"))
})

test_that("asp_centrality reproduces the worked example of the paper (9 nodes)", {
  figure_1 <- zoob_graph(9L, c(1, 2, 1, 8, 2, 3, 2, 8, 3, 8, 3, 4, 3, 9, 4, 9, 8, 9, 8, 7, 8, 6, 9, 6, 9, 5, 5, 6))
  printed <- c(0.1270, 0.1587, 0.2063, 0.1270, 0.1111, 0.1746, 0.1111, 0.3810, 0.2698)
  expect_reference(centrality_asp_centrality(figure_1), stats::setNames(printed, 1:9), reference = lv_example,
                   covers = c("centrality()", "centrality_asp_centrality()"), quantity = "AC, Fig. 1 network",
                   tolerance = 5e-5, note = paste("half a unit of the fourth decimal; edge list read from Fig. 1;",
                                                  "the removed node stays as an isolate, as in the example"))
})

test_that("asp_centrality equals equations (9)-(11) written over NetworkX's distances", {
  compare_networkx("asp_centrality", paste0(
    "(lambda Dm: (lambda tot: (lambda W: {k: (tot((lambda H: (H.remove_edges_from(list(G.edges(k))), H)[1])",
    "(G.copy())) - W) / W for k in G})(tot(G)))(lambda H: sum(L.get(j, Dm) for i in H",
    " for L in [nx.single_source_shortest_path_length(H, i)] for j in H if j != i)))",
    "(max(d for i, row in ", distances, ".items() for d in row.values()))"))
})

test_that("wdc and ewdc equal equations (4)-(9) over NetworkX's degree assortativity", {
  on <- c(graphs, list(ring = zoob_ring(8L)))
  compare_networkx("wdc", on = on, paste0("(lambda a: ", wdc, ")(", assortativity, ")"))
  compare_networkx("ewdc", on = on, paste0(
    "(lambda a: (lambda w: {i: (sum(w[j] for j in G[i]) - w[i]) * w[i] ** a for i in G})(", wdc, "))(",
    assortativity, ")"))
})
