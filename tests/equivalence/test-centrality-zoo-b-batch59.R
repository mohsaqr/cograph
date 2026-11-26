# Three measures of cograph's centrality batch 59, defined over all simple cycles or all simple paths, against the
# examples their sources print and against NetworkX's enumerations.
#   centrality_all_cycle_betweenness(): Zhou, Liang, Zhao and Zhang (2018), Scientific Reports 8:11749, equation (10):
#     C_A(i) = sum_k alpha^(k - 2) s_k(i), s_k(i) the number of simple cycles of k edges through i. Example 3, p. 7.
#   centrality_transportation(): Piraveenan and Saripada (2023), IEEE Access 11:142214-142234, equations (8)-(10),
#     read in the early-access version. Fig. 1 and Table 2.
#   centrality_path_transfer(): Tutzauer (2007), Social Networks 29(2):249-265, not read; the definition and the
#     example are those of Oggier, Phetsouvanh and Datta (2019), PeerJ Computer Science 5:e220, equation (1), Fig. 1A.
# No other package implements these measures.

independent <- "independent: closed forms and hand derivations from the definition"
zhou_example_3 <- "published: Zhou, Liang, Zhao and Zhang 2018, Example 3 and Table 2, p. 7"
piraveenan_table_2 <- "published: Piraveenan and Saripada 2023, Fig. 1 and Table 2"
oggier_figure_1 <- "published: Oggier, Phetsouvanh and Datta 2019, Fig. 1A"
exact <- 1e-10
small <- ref_connected_graph(8, density = 0.35, seed = 59L)
small_directed <- ref_connected_graph(7, density = 0.3, seed = 60L, directed = TRUE)

test_that("all cycle betweenness reproduces Example 3 of the paper", {
  # Edges 0-1, 1-2, 2-0, 2-3, 3-4, 4-0, alpha = 0.5; the paper prints 0.875, 0.625, 0.875, 0.375, 0.375.
  example <- zoob_graph(5L, c(1, 2, 2, 3, 3, 1, 3, 4, 4, 5, 5, 1), labels = as.character(0:4))
  expect_reference(centrality_all_cycle_betweenness(example),
                   c(`0` = 0.875, `1` = 0.625, `2` = 0.875, `3` = 0.375, `4` = 0.375), reference = zhou_example_3,
                   covers = c("centrality()", "centrality_all_cycle_betweenness()"),
                   quantity = "all_cycle_betweenness, alpha 0.5, Example 3", tolerance = exact)
})

test_that("all cycle betweenness equals the sum over NetworkX's simple cycles", {
  call <- paste0("(lambda out: [out.__setitem__(v, out[v] + %s ** (len(c) - 2)) for c in nx.simple_cycles(G)",
                 " if len(c) >= 3 for v in c] and out)({v: 0.0 for v in G})")
  lapply(c(0.5, 0.1), \(alpha) {
    expect_reference(centrality_all_cycle_betweenness(small, cycle_alpha = alpha),
                     ref_networkx(small, c(acc = sprintf(call, format(alpha))))$acc, reference = "networkx",
                     covers = c("centrality()", "centrality_all_cycle_betweenness()"),
                     quantity = sprintf("all_cycle_betweenness, alpha %s, 8 nodes", format(alpha)),
                     tolerance = 1e-9, measure = "scaled")
  })
})

test_that("transportation centrality reproduces Table 2 of the paper", {
  # Fig. 1: 1-2, 1-3, 2-3, 3-4. Table 2 prints the closed forms and, at beta = 2.0, 0.08, 0.08, 0.706, 0.
  figure_1 <- zoob_graph(4L, c(1, 2, 1, 3, 2, 3, 3, 4))
  expect_reference(centrality_transportation(figure_1, transport_beta = 2)[c("1", "2")], c(`1` = 0.08, `2` = 0.08),
                   reference = piraveenan_table_2, covers = c("centrality()", "centrality_transportation()"),
                   quantity = "transportation, beta 2, Fig. 1, nodes 1 and 2 (two printed decimals)",
                   tolerance = 5e-3)
  expect_reference(centrality_transportation(figure_1, transport_beta = 2)[c("3", "4")], c(`3` = 0.706, `4` = 0),
                   reference = piraveenan_table_2, covers = c("centrality()", "centrality_transportation()"),
                   quantity = "transportation, beta 2, Fig. 1, nodes 3 and 4 (three printed decimals)",
                   tolerance = 5e-4)
  # The printed closed forms, 2 e^-b / (3 e^-b + 3) and (3 e^-b + 2) / (3 e^-b + 3), at three values of beta.
  lapply(c(0, 0.1, 2), \(beta) {
    e <- exp(-beta)
    expect_reference(centrality_transportation(figure_1, transport_beta = beta),
                     c(`1` = 2 * e / (3 * e + 3), `2` = 2 * e / (3 * e + 3), `3` = (3 * e + 2) / (3 * e + 3), `4` = 0),
                     reference = piraveenan_table_2, covers = c("centrality()", "centrality_transportation()"),
                     quantity = sprintf("transportation, Table 2 closed forms at beta %s", format(beta)),
                     tolerance = exact)
  })
})

test_that("transportation centrality equals equations (8)-(10) over NetworkX's simple paths", {
  call <- paste0(
    "{v: sum((lambda ps: sum(np.exp(-%s * (len(p) - 1)) for p in ps if v in p[1:-1])",
    " / sum(np.exp(-%s * (len(p) - 1)) for p in ps) if ps else 0.0)(list(nx.all_simple_paths(G, s, t)))",
    " for s in G for t in G if s != t and v != s and v != t) / ((len(G) - 1) * (len(G) - 2)) for v in G}")
  Map(\(graph, name) {
    lapply(c(0.1, 2), \(beta) {
      expect_reference(centrality_transportation(graph, transport_beta = beta),
                       ref_networkx(graph, c(tc = sprintf(call, format(beta), format(beta))))$tc,
                       reference = "networkx", covers = c("centrality()", "centrality_transportation()"),
                       quantity = sprintf("transportation, beta %s, %s", format(beta), name), tolerance = exact)
    })
  }, list(small, small_directed), c("8 nodes", "7 nodes directed"))
})

test_that("path-transfer centrality reproduces the entropy printed for Fig. 1A", {
  # v1 -> v2, v1 -> v3, v1 -> v4, v3 -> v5, v4 -> v5; the paper prints C_H(v1) = 2.25.
  figure_1a <- zoob_graph(5L, c(1, 2, 1, 3, 1, 4, 3, 5, 4, 5), directed = TRUE)
  expect_reference(centrality_path_transfer(figure_1a)["1"], c(`1` = 2.25), reference = oggier_figure_1,
                   covers = c("centrality()", "centrality_path_transfer()"),
                   quantity = "path_transfer, Fig. 1A, node v1", tolerance = exact)
})

test_that("path-transfer centrality follows hand derivations", {
  # Triangle: the flow stops at each of the three nodes with probability 1/3.
  expect_reference(centrality_path_transfer(zoob_complete(3L)), rep(log2(3), 3), reference = independent,
                   covers = c("centrality()", "centrality_path_transfer()"), quantity = "path_transfer, triangle",
                   tolerance = exact)
  # Star of three leaves. From the hub: stop 1/4, each leaf 1/4: 2 bits. From a leaf: stop 1/2, hub 1/2 * 1/3,
  # each other leaf 1/2 * 1/3.
  leaf <- -(0.5 * log2(0.5) + 3 * (1 / 6) * log2(1 / 6))
  expect_reference(centrality_path_transfer(zoob_star(3L)), c(2, rep(leaf, 3)), reference = independent,
                   covers = c("centrality()", "centrality_path_transfer()"), quantity = "path_transfer, star of 3",
                   tolerance = exact)
})
