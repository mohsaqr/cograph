# Nine measures of cograph's centrality batch 68 against published values and NetworkX.
#   centrality_physarum_spreader(): Wang, Zhang, Zhang, Mahadevan and Deng (2015), PLoS ONE 10(12):e0145028,
#     equations (7)-(12), the printed Q(0) and final scores of the four-node example (Fig. 1 / Fig. 3).
#   centrality_articlerank(): Li and Willett (2009), Aslib Proceedings 61(6):605-618, equation (2), Table 5.
#   centrality_entropy_gravity(): Yan, Cui and Ni (2020), Chinese Physics B 29(4):048902, equations (2)-(6) read by
#     the paper's prose; no value is printed.
#   centrality_influence_capability(): Wang, Du, Fan and Xing (2017), Neurocomputing 260:466-477, equations (1)-(7),
#     Tables 1-3.
#   centrality_ehc(): Zareie and Sheikhahmadi (2019), Physica A 514:141-155, equations (19)-(24), the worked example
#     on Fig. 1 and Table 6.
#   centrality_erm(): Zareie, Sheikhahmadi and Fatemi (2017), Chaos, Solitons and Fractals 104:485-494, equations
#     (2)-(7), Tables 2-3.
#   centrality_shell_clustering() and centrality_ecrm(): Zareie, Sheikhahmadi, Jalili and Fasaei (2020),
#     Knowledge-Based Systems 194:105580, equations (2)-(6), Table 2.
#   centrality_weight_neighborhood(): Wang, Hou, Li and Ding (2017), Physica A 475:88-105, equations (9)-(10),
#     Tables 1 and 3.
# Monotonicity M and the share of distinct ranks DM are computed from the scores rounded to 12 significant digits.
# The karate club's automorphisms cap M at 0.9542 and DM at 0.7941 for any exact structural measure (dolphins 0.9979
# and 0.9677, adjective-noun 0.9997); a printed value at the cap is weak evidence, one above it cannot be reached.
# No other package implements these measures.

wang15 <- "published: Wang, Zhang, Zhang, Mahadevan and Deng 2015, PhysarumSpreader example (pp. 7-8)"
li_table_5 <- "published: Li and Willett 2009, Table 5 and the text on P004 and P005"
wang17_t1 <- "published: Wang, Du, Fan and Xing 2017, Table 1"
wang17_t2 <- "published: Wang, Du, Fan and Xing 2017, Table 2"
wang17_t3 <- "published: Wang, Du, Fan and Xing 2017, Table 3"
zs_example <- "published: Zareie and Sheikhahmadi 2019, worked example of Section 3"
zs_table_6 <- "published: Zareie and Sheikhahmadi 2019, Table 6"
zsf_tables <- "published: Zareie, Sheikhahmadi and Fatemi 2017, Tables 2 and 3"
zsjf_table_2 <- "published: Zareie, Sheikhahmadi, Jalili and Fasaei 2020, Table 2"
whld_table_1 <- "published: Wang, Hou, Li and Ding 2017, Table 1"
whld_table_3 <- "published: Wang, Hou, Li and Ding 2017, Table 3"
exact <- 1e-10
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
sparse <- ref_graph(16, density = 0.25, seed = 46L)
karate <- ref_karate()
dolphins <- zoob_dolphins()
adjnoun <- zoob_adjnoun()
# A 4-clique and a triangle joined by a long path, with one pendant.
long <- zoob_graph(14L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12,
                          11, 13, 12, 13, 13, 14))
graphs <- list(undirected = undirected, sparse = sparse, karate = karate, long = long)
connected <- list(undirected = undirected, karate = karate, long = long)
directed <- ref_graph(10, density = 0.3, seed = 48L, directed = TRUE, weighted = TRUE, digits = 6L)
weighted <- ref_connected_graph(12, density = 0.3, seed = 47L, weighted = TRUE, digits = 6L)
dag <- ref_graph(14, density = 0.3, seed = 49L, directed = TRUE)
dag[lower.tri(dag)] <- 0
monotonicity <- \(s) {
  ties <- table(signif(s, 12))
  (1 - sum(ties * (ties - 1)) / (length(s) * (length(s) - 1)))^2
}
distinct <- \(s) length(unique(signif(s, 12))) / length(s)
dense_rank <- \(s) match(-signif(s, 10), sort(unique(-signif(s, 10))))
groups_rank <- \(groups, n) {
  rank <- numeric(n)
  rank[unlist(groups)] <- rep(seq_along(groups), lengths(groups))
  stats::setNames(rank, seq_len(n))
}

compare_networkx <- function(measure, call, tolerance = exact, on = graphs, ...) {
  wrapper <- match.fun(paste0("centrality_", measure))
  Map(\(graph, name) {
    expect_reference(wrapper(graph, ...), ref_networkx(graph, c(value = call))$value, reference = "networkx",
                     covers = c("centrality()", sprintf("centrality_%s()", measure)),
                     quantity = sprintf("%s, %s", measure, name), tolerance = tolerance)
  }, on, names(on))
}

# --- physarum_spreader -----------------------------------------------------------------------------------------------

test_that("physarum_spreader reproduces the example after 52 updates, with nodes 1 and 2 exchanged", {
  net <- zoob_graph(4L, c(1, 2, 2, 3, 3, 1, 4, 1, 4, 2), directed = TRUE)
  net[1, 2] <- net[4, 1] <- net[4, 2] <- 2
  note <- "network of Fig. 1 / Fig. 3: 1->2 (2), 2->3 (1), 3->1 (1), 4->1 (2), 4->2 (2); printed to four decimals"
  score <- centrality_physarum_spreader(net, physarum_steps = 52)
  expect_reference(score[c("3", "4")], c(`3` = 1.0002, `4` = 0.9801), reference = wang15,
                   covers = c("centrality()", "centrality_physarum_spreader()"),
                   quantity = "PhysarumSpreader S of nodes 3 and 4 after 52 updates", tolerance = 5e-5,
                   note = paste(note, "; the paper gives no stopping rule, and only t = 52 gives these values"))
  expect_known_difference(score[c("1", "2")], c(`1` = 1.0100, `2` = 1.0097), reference = wang15,
                          covers = c("centrality()", "centrality_physarum_spreader()"),
                          quantity = "PhysarumSpreader S of nodes 1 and 2 after 52 updates", tolerance = 5e-5,
                          reason = paste("the computed values are 1.0097 for node 1 and 1.0100 for node 2, the",
                                         "printed pair exchanged; the source audit found no update order",
                                         "consistent with the printed Q(0) that gives node 1 the larger score after",
                                         "the fifth update"),
                          note = note)
  expect_reference(sort(unname(score)), c(0.9801, 1.0002, 1.0097, 1.0100), reference = wang15,
                   covers = c("centrality()", "centrality_physarum_spreader()"),
                   quantity = "PhysarumSpreader S after 52 updates, the four values without their labels",
                   tolerance = 5e-5, note = note)
  # One update from I(0) = 1 with the printed Q(0), which prints 1/3 as .33: I(1) = Q(0)' I(0), S = I + I_g / 4.
  printed_q0 <- rbind(c(0, .5, 0, 0, .5), c(0, 0, .5, 0, .5), c(.5, 0, 0, 0, .5), c(.33, .33, 0, 0, .33),
                      c(.25, .25, .25, .25, 0))
  arrivals <- as.numeric(crossprod(printed_q0, c(1, 1, 1, 1, 0)))
  expect_reference(centrality_physarum_spreader(net, physarum_steps = 1),
                   stats::setNames(arrivals[1:4] + arrivals[5] / 4, 1:4), reference = wang15,
                   covers = c("centrality()", "centrality_physarum_spreader()"),
                   quantity = "PhysarumSpreader S after one update, from the printed flux matrix Q(0)",
                   tolerance = 5e-3, note = paste(note, "; Q(0) printed to two decimals (.33 for 1/3)"))
})

# Equations (7)-(12) over NumPy: the ground node is the last row and column; D adapts only between nodes.
physarum <- function(steps) paste0(
  "(lambda A, n: (lambda W, M: (lambda S: dict(zip(G, S[1][:n] + S[1][n] / n)))(__import__('functools').reduce(",
  "lambda s, _: (lambda Q: (s[0] + M * (Q - s[0]) / 2, Q.T @ s[1]))((W * s[0]) / (W * s[0]).sum(axis=1,",
  " keepdims=True)), range(", steps, "), ((W > 0) * 1.0, np.r_[np.ones(n), 0.0]))))(np.block([[A,",
  " np.where((A > 0).sum(1) > 0, A.sum(1) / np.maximum((A > 0).sum(1), 1), 1.0)[:, None]], [np.ones((1, n)),",
  " np.zeros((1, 1))]]),",
  " np.block([[(A > 0) * 1.0, np.zeros((n, 1))], [np.zeros((1, n + 1))]])))",
  "(nx.to_numpy_array(G, nodelist=list(G), weight='weight'), len(G))")

test_that("physarum_spreader equals equations (7)-(12) iterated in NumPy", {
  on <- list(directed = directed, weighted = weighted, karate = karate)
  for (steps in c(1, 5, 52)) compare_networkx("physarum_spreader", physarum(steps), on = on, physarum_steps = steps)
})

# --- articlerank -----------------------------------------------------------------------------------------------------

table_5 <- data.frame(
  cites = rep(c("P004", "P005"), each = 23),
  ar = c(1.95486, 1.73167, 4.23415, 1.12020, 0.82136, 0.73390, 0.71420, 0.49924, 0.24623, 0.26453, 0.35321, 0.31781,
         0.25426, 0.21919, 0.20241, 0.20241, 0.22026, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15,
         3.16803, 1.88691, 2.76597, 1.29026, 0.73394, 0.58421, 0.53752, 1.22665, 0.51089, 0.28445, 0.38102, 0.26563,
         0.27218, 0.31103, 0.19512, 0.31500, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15),
  nr = c(35, 26, 49, 40, 19, 50, 26, 33, 70, 31, 21, 41, 62, 23, 45, 63, 4, 51, 64, 41, 48, 68, 23,
         48, 16, 49, 191, 99, 49, 33, 43, 55, 72, 36, 67, 59, 65, 33, 41, 26, 60, 58, 66, 65, 12, 72),
  c_x1000 = c(27.68929, 28.11154, 50.04908, 14.81746, 15.04320, 8.57360, 11.59417, 7.27755, 2.33169, 3.97197,
              6.24053, 4.14902, 2.60516, 3.74048, 2.51133, 2.05287, 5.56220, 1.73210, 1.50602, 1.95822, 1.79426,
              1.44788, 2.55973, 37.89504, 36.56802, 32.69472, 5.69398, 5.45276, 6.90557, 7.83561, 15.60618, 5.63898,
              2.64360, 5.32154, 2.58901, 2.87716, 3.09175, 2.84430, 4.11228, 2.43506, 1.56904, 1.60256, 1.47638,
              1.49105, 3.15126, 1.39405),
  row.names = c("P009", "P010", "P016", "P022", "P037", "P038", "P039", "P048", "P082", "P093", "P095", "P100",
                "P104", "P105", "P111", "P118", "P139", "P160", "P184", "P319", "P328", "P336", "P337", "P007",
                "P008", "P017", "P024", "P032", "P051", "P053", "P064", "P075", "P080", "P094", "P097", "P101",
                "P115", "P122", "P135", "P170", "P173", "P182", "P198", "P252", "P291", "P341"))

test_that("articlerank reproduces the terms of Table 5 whose citing paper is not cited", {
  # A citing paper that nobody cites has AR = 1 - d = 0.15 (printed). Network: the cited paper T, the citing paper C
  # (NR from Table 5) and two isolated papers whose NR brings the mean to the printed NRbar = 35.6. Then
  # AR(T) = 0.15 + 0.85 x 35.6 x 0.15 / (35.6 + NR(C)), and the Table 5 term C = (AR(T) - 0.15) / (0.85 x 35.6).
  uncited <- table_5[abs(table_5$ar - 0.15) < 1e-12, ]
  term <- vapply(seq_len(nrow(uncited)), \(i) {
    pair <- zoob_graph(4L, c(2, 1), directed = TRUE, labels = c("T", "C", "F1", "F2"))
    filler <- (3 * 35.6 - uncited$nr[i]) / 2
    score <- centrality_articlerank(pair, ar_references = c(T = 35.6, C = uncited$nr[i], F1 = filler, F2 = filler))
    1000 * (score[["T"]] - 0.15) / (0.85 * 35.6)
  }, numeric(1))
  expect_reference(stats::setNames(term, rownames(uncited)), stats::setNames(uncited$c_x1000, rownames(uncited)),
                   reference = li_table_5, covers = c("centrality()", "centrality_articlerank()"),
                   quantity = "1000 AR(P) / (NRbar + NR(P)) of the 13 citing papers with AR = 0.15", tolerance = 5e-6,
                   note = "one citation per network, NRbar fixed at the printed 35.6 by two isolated papers")
})

test_that("articlerank reproduces the printed AR(P004) - AR(P005), which needs the NRbar multiplier", {
  # A citation network in which the 23 papers that cite only P004 and the 23 that cite only P005 have their printed
  # AR and NR: a paper printed with AR = a > 0.15 is cited by m papers that nobody cites, each with
  # NR = 0.85 x 35.6 x 0.15 m / (a - 0.15) - 35.6, m the smallest count that keeps this nonnegative; one isolated
  # paper brings the mean NR to 35.6. The 11 papers that cite both cancel from the difference and are left out.
  m <- ifelse(abs(table_5$ar - 0.15) < 1e-12, 0, ceiling((table_5$ar - 0.15) / (0.85 * 0.15)))
  sub_nr <- 0.85 * 35.6 * 0.15 * m / (table_5$ar - 0.15) - 35.6
  first_sub <- 2 + nrow(table_5) + cumsum(c(0, m))[seq_along(m)]
  arcs <- rbind(cbind(2 + seq_len(nrow(table_5)), ifelse(table_5$cites == "P004", 1, 2)),
                do.call(rbind, lapply(which(m > 0), \(i) cbind(first_sub[i] + seq_len(m[i]), 2 + i))))
  n <- 2 + nrow(table_5) + sum(m) + 1
  references <- c(35.6, 35.6, table_5$nr, rep(sub_nr[m > 0], m[m > 0]))
  references <- c(references, n * 35.6 - sum(references))
  citations <- zoob_graph(n, arcs, directed = TRUE)
  score <- centrality_articlerank(citations, ar_references = references)
  note <- paste("citation network built from Table 5 (", n, "papers); AR printed to five decimals and NRbar to three",
                "significant figures, which moves the difference by up to 3e-5")
  expect_reference(unname(score[as.character(2 + seq_len(nrow(table_5)))]), table_5$ar, reference = li_table_5,
                   covers = c("centrality()", "centrality_articlerank()"),
                   quantity = "AR of the 46 citing papers of Table 5 in the built network", tolerance = 1e-10,
                   note = note)
  expect_reference(score[["1"]] - score[["2"]], 10.30939 - 9.81223, reference = li_table_5,
                   covers = c("centrality()", "centrality_articlerank()"), quantity = "AR(P004) - AR(P005)",
                   tolerance = 4e-5, note = paste(note, "; without the NRbar multiplier the difference is 0.014"))
})

# Equation (2) iterated 5000 times from 1 - d, as the paper iterates it, over NumPy.
articlerank <- function(references = "A.sum(1)", damping = 0.85) paste0(
  "(lambda A: (lambda NR: (lambda Mt: dict(zip(G, __import__('functools').reduce(lambda x, _: ", 1 - damping,
  " + Mt @ x, range(5000), np.full(len(G), ", 1 - damping, ")))))(", damping, " * NR.mean() * A.T",
  " / (NR.mean() + NR)[None, :]))(", references, "))(nx.to_numpy_array(G, nodelist=list(G), weight=None))")

test_that("articlerank equals equation (2) iterated in NumPy", {
  path <- zoob_path(6L)
  compare_networkx("articlerank", articlerank(), on = list(dag = dag, path = path), tolerance = 1e-12)
  compare_networkx("articlerank", articlerank("np.arange(1, len(G) + 1) * 2.5"), on = list(dag = dag),
                   tolerance = 1e-12, ar_references = seq_len(nrow(dag)) * 2.5)
  compare_networkx("articlerank", articlerank(damping = 0.3), on = list(karate = karate), tolerance = 1e-12,
                   damping = 0.3)
})

# --- entropy_gravity -------------------------------------------------------------------------------------------------

# Node H-index from sorted neighbor degrees, BFS closeness, NetworkX betweenness, PageRank from NumPy's linear solve;
# min-max with values within 1e-9 of 0 set to 0, the guard of eq. (3), entropy over ln N, weights of eq. (4).
entropy_gravity <- function(radius = 3, damping = 0.85) paste0(
  "(lambda n, D, BC: (lambda X: (lambda B: (lambda S: (lambda C: {i: C[a] * sum(C[b] / D[i][j] ** 2",
  " for b, j in enumerate(G) if 1 <= D[i].get(j, 0) <= ", radius, ") for a, i in enumerate(G)})(B @ ((1 - S)",
  " / (4 - S.sum()))))(np.array([-sum(p * np.log(p) for p in q if p > 0) / np.log(n) for q in [(c + ((c > 0).sum()",
  " == 1)) / (c + ((c > 0).sum() == 1)).sum() for c in B.T]])))(np.column_stack([np.where((x - x.min()) / (x.max()",
  " - x.min()) <= 1e-9, 0, (x - x.min()) / (x.max() - x.min())) for x in X.T])))(np.column_stack([[sum(1 for k, v",
  " in enumerate(sorted((G.degree(j) for j in G[i]), reverse=True), 1) if v >= k) for i in G], [1 / sum(D[i].",
  "values()) for i in G], [BC[i] for i in G], np.linalg.solve(np.eye(n) - ", damping, " * (lambda A: A / A.sum(1,",
  " keepdims=True))(nx.to_numpy_array(G, nodelist=list(G), weight=None)).T, np.full(n, ", 1 - damping, " / n))])))",
  "(len(G), dict(nx.all_pairs_shortest_path_length(G)), nx.betweenness_centrality(G, normalized=False))")

test_that("entropy_gravity equals the prose reading of equations (2)-(6) written over NetworkX", {
  compare_networkx("entropy_gravity", entropy_gravity(), on = connected, tolerance = 1e-9)
  compare_networkx("entropy_gravity", entropy_gravity(radius = 1, damping = 0.5), on = connected, tolerance = 1e-9,
                   entropy_gravity_radius = 1, damping = 0.5)
})

# --- influence_capability --------------------------------------------------------------------------------------------

ic_fig1 <- zoob_graph(17L, c(1, 6, 5, 6, 6, 4, 4, 3, 4, 2, 6, 7, 7, 8, 7, 10, 8, 9, 8, 10, 10, 11, 10, 12, 10, 13, 10,
                             14, 11, 13, 11, 12, 12, 13, 13, 15, 12, 15, 15, 16, 16, 17))
ic_fig3 <- zoob_graph(13L, c(9, 10, 8, 10, 8, 7, 7, 10, 7, 4, 5, 4, 5, 6, 4, 3, 3, 2, 3, 1, 10, 11, 10, 12, 10, 13, 11,
                             13, 11, 12, 12, 13))

test_that("influence_capability reproduces the rankings of Tables 1-3 and not the printed entropy weights", {
  covers <- c("centrality()", "centrality_influence_capability()")
  printed_t2 <- list(10, c(12, 13), 11, 7, 15, 8, 6, 4, 14, 16, 9, c(1, 5), c(2, 3), 17)
  expect_reference(dense_rank(centrality_influence_capability(ic_fig1)), unname(groups_rank(printed_t2, 17)),
                   reference = wang17_t2, covers = covers, quantity = "IC rank of each node of Fig. 1 (14 groups)",
                   tolerance = 0, note = "edge list read from Fig. 1 (17 nodes, 21 edges)")
  # Table 1 numbers the nodes of Fig. 3 differently: the figure's 4, 5, 6 are the table's 6, 4, 5.
  table_label <- c(1, 2, 3, 6, 4, 5, 7, 8, 9, 10, 11, 12, 13)
  printed_t1 <- list(10, c(11, 12, 13), 7, 8, 6, 3, 9, 4, c(1, 2), 5)
  expected_t1 <- unname(groups_rank(printed_t1, 13))[table_label]
  expect_reference(dense_rank(centrality_influence_capability(ic_fig3)), expected_t1, reference = wang17_t1,
                   covers = covers, quantity = "IC rank of each node of Fig. 3 (10 groups)", tolerance = 0,
                   note = "edge list read from Fig. 3; the table's labels 6, 4, 5 are the figure's 4, 5, 6")
  expect_reference(monotonicity(centrality_influence_capability(dolphins)), 0.9958, reference = wang17_t3,
                   covers = covers, quantity = "monotonicity M of IC, dolphins", tolerance = 5e-5,
                   note = "62 nodes, 159 edges; below the dolphins cap of 0.9979")
  expect_reference(monotonicity(centrality_influence_capability(karate)), 0.9542, reference = wang17_t3,
                   covers = covers, quantity = "monotonicity M of IC, karate club", tolerance = 5e-5,
                   note = "at the karate cap of 0.9542, so weak evidence")
  weight <- \(graph) cograph:::.cg_ic_parts(unname(graph))$weight
  expect_known_difference(c(weight(ic_fig1), weight(ic_fig3), weight(karate), weight(dolphins)),
                          c(0.2852, 0.7148, 0.2764, 0.7236, 0.4394, 0.5606, 0.3883, 0.6117), reference = wang17_t3,
                          covers = covers, tolerance = 5e-5,
                          quantity = "entropy weights w1, w2 of eq. (7) on Fig. 1, Fig. 3, karate and dolphins",
                          reason = paste("equations (5)-(7) give 0.3171, 0.3187, 0.4484 and 0.3913 for w1; no",
                                         "normalisation or iteration convention tried reproduces all of the printed",
                                         "weights, and no printed ranking depends on them"),
                          note = "printed in Tables 1-3 (Fig. 1 in the header of Table 2)")
})

# The global iteration of the k-shell peel by a recursive Python function; eqs. (1)-(7).
influence_capability <- paste0(
  "(lambda it: (lambda icp, icn: (lambda H: (lambda w: {i: w[0] * icp[i] / sum(icp.values()) + w[1] * icn[i]",
  " / sum(icn.values()) for i in G})([(1 - h) / (2 - sum(H)) for h in H]))([-sum(v / sum(x.values()) * np.log(v",
  " / sum(x.values())) for v in x.values() if v > 0) / np.log(len(G)) for x in (icp, icn)]))({i: G.degree(i) - sum(1",
  " for j in G[i] if it[j] < it[i]) + sum(2 / np.pi * np.arctan(it[j] ** (1 / 3)) for j in G[i] if it[j] < it[i])",
  " for i in G}, {i: sum(G.degree(k) for j in G[i] for k in G[j]) for i in G}))((lambda peel: peel(peel, set(G), 1,",
  " 0, {}))(lambda self, alive, k, c, it: it if not alive else (lambda hit: self(self, alive - hit, k, c + 1, {**it,",
  " **{v: c + 1 for v in hit}}) if hit else self(self, alive, k + 1, c, it))({v for v in alive if sum(1 for u in",
  " G[v] if u in alive) <= k})))")

test_that("influence_capability equals equations (1)-(7) written over NetworkX", {
  compare_networkx("influence_capability", influence_capability)
})

# --- ehc -------------------------------------------------------------------------------------------------------------

test_that("ehc reproduces the worked example of Fig. 1 and the monotonicity of Table 6", {
  fig1 <- zoob_graph(17L, c(1, 4, 1, 5, 1, 6, 1, 7, 1, 9, 9, 8, 9, 16, 9, 14, 9, 10, 9, 3, 8, 2, 8, 16, 8, 14, 16, 2,
                            16, 14, 2, 14, 2, 15, 2, 17, 14, 15, 14, 3, 14, 13, 3, 10, 3, 13, 3, 11, 3, 12))
  covers <- c("centrality()", "centrality_ehc()")
  expect_reference(centrality_ehc(fig1, ehc_p = 0.5, ehc_r = 10)[c("1", "3")], c(`1` = 21.616, `3` = 42.106),
                   reference = zs_example, covers = covers, quantity = "EHC of nodes 1 and 3, p = 0.5, r = 10",
                   tolerance = 5e-4,
                   note = "edge list read from Fig. 1 (17 nodes, 25 edges); printed to three decimals")
  expect_known_difference(monotonicity(centrality_ehc(karate)), 0.9612, reference = zs_table_6, covers = covers,
                          quantity = "monotonicity M of EHC, karate club", tolerance = 5e-5,
                          reason = paste("0.9612 is above 0.9542, the largest M any exact structural measure",
                                         "reaches on the karate club (automorphism orbits {5,11}, {6,7},",
                                         "{15,16,19,21,23}, {18,22}); the implementation gives 0.9542"))
  expect_reference(monotonicity(centrality_ehc(dolphins)), 0.9979, reference = zs_table_6, covers = covers,
                   quantity = "monotonicity M of EHC, dolphins", tolerance = 5e-5,
                   note = "at the dolphins cap of 0.9979, so weak evidence")
})

ehc <- function(p = 0.8, r = 100) paste0(
  "(lambda h: {i: sum(sum(", p, " ** (1 + k * ", p, " / ", r, ") * sum(1 for l in G[j] if G.degree(l) >= k)",
  " for k in range(1, h + 1)) for j in G[i]) for i in G})(max(d for _, d in G.degree()))")

test_that("ehc equals equations (19)-(24) written over NetworkX", {
  compare_networkx("ehc", ehc())
  compare_networkx("ehc", ehc(0.5, 10), on = list(karate = karate, sparse = sparse), ehc_p = 0.5, ehc_r = 10)
})

# --- erm -------------------------------------------------------------------------------------------------------------

test_that("erm reproduces the distinct ranks and monotonicity of Tables 2 and 3, and not the karate values", {
  schematic <- zoob_graph(21L, c(5, 18, 5, 17, 5, 10, 5, 21, 5, 12, 5, 19, 5, 20, 5, 2, 10, 17, 19, 20, 16, 20, 2, 3,
                                 2, 11, 2, 15, 2, 4, 4, 15, 12, 13, 12, 6, 13, 7, 13, 8, 6, 7, 1, 7, 1, 6, 1, 16, 1, 8,
                                 1, 9, 8, 14))
  covers <- c("centrality()", "centrality_erm()")
  at_cap <- "the largest value an exact structural measure reaches on this network, so weak evidence"
  expect_reference(c(distinct(centrality_erm(schematic)), monotonicity(centrality_erm(schematic))), c(0.8095, 0.9623),
                   reference = zsf_tables, covers = covers, quantity = "DM and M of ERM, schematic network of Fig. 1",
                   tolerance = 5e-5, note = paste("edge list read from Fig. 1 (21 nodes, 27 edges);", at_cap))
  expect_reference(c(distinct(centrality_erm(dolphins)), monotonicity(centrality_erm(dolphins))), c(0.9677, 0.9979),
                   reference = zsf_tables, covers = covers, quantity = "DM and M of ERM, dolphins", tolerance = 5e-5,
                   note = at_cap)
  expect_reference(monotonicity(centrality_erm(adjnoun)), 0.9997, reference = zsjf_table_2, covers = covers,
                   quantity = "monotonicity M of ERM, Copperfield (adjective-noun network)", tolerance = 5e-5,
                   note = paste("112 nodes, 425 edges, as Table 1 of the source;", at_cap))
  expect_known_difference(c(distinct(centrality_erm(karate)), monotonicity(centrality_erm(karate))), c(0.8529, 0.9612),
                          reference = zsf_tables, covers = covers, quantity = "DM and M of ERM, karate club",
                          tolerance = 5e-5,
                          reason = paste("both are above the largest values an exact structural measure reaches on the",
                                         "karate club (0.7941 and 0.9542, which the implementation gives)"))
})

erm <- paste0(
  "(lambda d1: (lambda d2: (lambda EC: {i: sum(EC[k] for j in G[i] for k in G[j]) for i in G})({i: -sum(G.degree(j)",
  " / d1[i] * np.log(G.degree(j) / d1[i]) for j in G[i]) - d2[i] / max(d2.values()) * sum(d1[j] / d2[i] * np.log(",
  "d1[j] / d2[i]) for j in G[i]) for i in G}))({i: sum(d1[j] for j in G[i]) for i in G}))({i: sum(G.degree(j) for j",
  " in G[i]) for i in G})")

test_that("erm equals equations (2)-(7) written over NetworkX", {
  compare_networkx("erm", erm)
})

# --- shell_clustering and ecrm ---------------------------------------------------------------------------------------

test_that("ecrm reproduces the monotonicity of Table 2", {
  covers <- c("centrality()", "centrality_ecrm()")
  expect_reference(c(monotonicity(centrality_ecrm(dolphins)), monotonicity(centrality_ecrm(adjnoun))),
                   c(0.9979, 0.9997), reference = zsjf_table_2, covers = covers,
                   quantity = "monotonicity M of ECRM, dolphins and Copperfield (adjective-noun network)",
                   tolerance = 5e-5,
                   note = "each is the largest value an exact structural measure reaches on its network: weak evidence")
})

# The lowest-degree peel by a recursive Python function, shell vectors, numpy.corrcoef (0 for a constant vector).
scc <- paste0(
  "(lambda it: (lambda f, top: (lambda sv: {i: sum((2 - (lambda x, y: 0.0 if len(set(x)) == 1 or len(set(y)) == 1",
  " else np.corrcoef(x, y)[0, 1])(sv[i], sv[j])) + (2 * G.degree(j) / top + 1) for j in G[i]) for i in G})({i: [sum(1",
  " for j in G[i] if it[j] == k) for k in range(1, f + 1)] for i in G}))(max(it.values()), max(d for _, d in",
  " G.degree())))((lambda peel: peel(peel, set(G), 0, {}))(lambda self, alive, c, it: it if not alive else (lambda",
  " deg: (lambda low: self(self, alive - {v for v in alive if deg[v] == low}, c + 1, {**it, **{v: c + 1 for v in",
  " alive if deg[v] == low}}))(min(deg.values())))({v: sum(1 for u in G[v] if u in alive) for v in alive})))")

test_that("shell_clustering and ecrm equal equations (2)-(6) written over NetworkX", {
  compare_networkx("shell_clustering", scc)
  compare_networkx("ecrm", paste0("(lambda S: {i: sum(S[k] for j in G[i] for k in G[j]) for i in G})(", scc, ")"))
})

# --- weight_neighborhood ---------------------------------------------------------------------------------------------

test_that("weight_neighborhood reproduces Table 1 on Fig. 2 and the monotonicity of Table 3", {
  # The copy read cuts off the top of Fig. 2; the edges 5-16, 5-17, 17-18, 18-19 and 18-20 were inferred from the
  # degree, coreness, Cnc, Cnc+ and betweenness Table 1 prints for nodes 5 and 17 (source audit).
  fig2 <- zoob_graph(22L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 1, 7, 2, 7, 7, 21, 7, 22, 2, 8, 3, 8, 8, 9, 8, 10, 10,
                            11, 5, 6, 4, 6, 4, 5, 5, 12, 5, 13, 5, 14, 5, 15, 5, 16, 5, 17, 17, 18, 18, 19, 18, 20))
  covers <- c("centrality()", "centrality_weight_neighborhood()")
  ten <- c("4", "2", "5", "3", "1", "8", "7", "6", "17", "12")
  note <- paste("22 nodes, 27 edges: 22 read from Fig. 2 and five (5-16, 5-17, 17-18, 18-19, 18-20) inferred from",
                "Table 1; printed to four decimals")
  expect_reference(centrality_weight_neighborhood(fig2)[ten],
                   stats::setNames(c(53.3524, 39.4269, 31.5186, 29.3754, 29.3754, 18.2350, 17.3066, 15.7708, 13.2951,
                                     5.9513), ten),
                   reference = whld_table_1, covers = covers, quantity = "C(k) of the ten nodes of Table 1",
                   tolerance = 5e-5, note = note)
  expect_reference(centrality_weight_neighborhood(fig2, wnc_benchmark = "coreness")[ten],
                   stats::setNames(c(25.8223, 24.2751, 18.0917, 18.4728, 18.4728, 11.2837, 10.9742, 6.7966, 3.9398,
                                     2.2378), ten),
                   reference = whld_table_1, covers = covers, quantity = "C(ks) of the ten nodes of Table 1",
                   tolerance = 5e-5, note = note)
  expect_reference(c(monotonicity(centrality_weight_neighborhood(dolphins)),
                     monotonicity(centrality_weight_neighborhood(dolphins, wnc_benchmark = "coreness"))),
                   c(0.9905, 0.9905), reference = whld_table_3, covers = covers,
                   quantity = "monotonicity M of C(k) and C(ks), dolphins", tolerance = 5e-5,
                   note = "below the dolphins cap of 0.9979")
})

weight_neighborhood <- function(phi = "dict(G.degree())", alpha = 1) paste0(
  "(lambda phi: (lambda W: {i: phi[i] + sum((G.degree(i) * G.degree(j)) ** ", alpha, " / W * phi[j] for j in G[i])",
  " for i in G})(np.mean([(G.degree(u) * G.degree(v)) ** ", alpha, " for u, v in G.edges()])))(", phi, ")")

test_that("weight_neighborhood equals equations (9)-(10) written over NetworkX's core numbers", {
  compare_networkx("weight_neighborhood", weight_neighborhood())
  compare_networkx("weight_neighborhood", weight_neighborhood("nx.core_number(G)", 0.5), wnc_benchmark = "coreness",
                   wnc_alpha = 0.5)
})
