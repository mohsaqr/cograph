# TOPSIS with relative entropy, centrality_topsis_re() (Liu, Jiang, Wang and Yu 2015, Knowledge-Based Systems
# 84:56-66). Four indicators: DC_i = d_i / (N - 1) (equation 2), CC_i = (N - 1) / sum_j l_ij (3), Freeman's
# betweenness (4) and the improved k-shell index (section 2.2.5); t_ij = v_ij / sqrt(sum_i v_ij^2) (6);
# r_ij = w_j t_ij with W = (0.0625, 0.1875, 0.1875, 0.5625) (7); relative-entropy distances to the column maxima and
# minima (12a, 12b) and C_i = S_i^- / (S_i^+ + S_i^-) (13). The paper prints no per-node value. The indicators are
# taken from NetworkX here and the equations evaluated term by term in Python, with the convention cograph states:
# a zero indicator below a positive maximum makes the distance to the ideal infinite, hence C_i = 0.

independent <- "independent: closed forms and hand derivations from the definition"
exact <- 1e-9

topsis_re_call <- c(score = paste0(
  "(lambda cols: (lambda r: (lambda dist: {v: (0.0 if dist(i, [max(c) for c in r]) == float('inf') else",
  " dist(i, [min(c) for c in r]) / (dist(i, [max(c) for c in r]) + dist(i, [min(c) for c in r])))",
  " for i, v in enumerate(G)})",
  "(lambda i, ideal: (float('inf') if any(ideal[j] > 0 and r[j][i] == 0 for j in range(4)) else",
  " max(sum((ideal[j] * np.log(ideal[j] / r[j][i]) if ideal[j] > 0 else 0.0)",
  " + (1 - ideal[j]) * np.log((1 - ideal[j]) / (1 - r[j][i])) for j in range(4)), 0.0) ** 0.5)))",
  "([[w * x / (sum(y * y for y in c) ** 0.5) for x in c]",
  " for c, w in zip(cols, (0.0625, 0.1875, 0.1875, 0.5625))]))",
  "([[G.degree(v) / (len(G) - 1) for v in G],",
  " [(len(G) - 1) / sum(nx.single_source_shortest_path_length(G, v).values()) for v in G],",
  " [nx.betweenness_centrality(G, normalized=False)[v] for v in G],",
  " (lambda H, lab: [([lab.__setitem__(v, k) for v in b], H.remove_nodes_from(b)) for k in range(1, len(G) + 1)",
  " for b in [[v for v, d in H.degree() if d == min(dict(H.degree()).values())] if len(H) else []]]",
  " and [float(lab[v]) for v in G])(nx.Graph(G), {})])"))

test_that("topsis_re equals the paper's equations over NetworkX's indicators", {
  graphs <- list(kite = zoob_kite(), `karate club` = ref_karate(),
                 `random 12 nodes` = ref_connected_graph(12, density = 0.3, seed = 41L))
  Map(\(graph, name) {
    expect_reference(centrality_topsis_re(graph), ref_networkx(graph, topsis_re_call)$score, reference = "networkx",
                     covers = c("centrality()", "centrality_topsis_re()"), quantity = sprintf("topsis_re, %s", name),
                     tolerance = exact)
  }, graphs, names(graphs))
})

test_that("topsis_re on a star: the hub is the ideal, the leaves have zero betweenness", {
  expect_reference(centrality_topsis_re(zoob_star(4L)), c(1, 0, 0, 0, 0), reference = independent,
                   covers = c("centrality()", "centrality_topsis_re()"), quantity = "topsis_re, star of 4",
                   tolerance = 1e-10)
})
