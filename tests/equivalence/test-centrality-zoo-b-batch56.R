# Three measures of cograph's centrality batch 56.
#   centrality_dst(): dynamical spanning tree centrality, DST(i) = 1 - t(G - v_i) / t(G), equation (50) of Lu, Chen,
#     Ren, Zhang, Zhang and Zhou (2016), Physics Reports 650:1-63, there attributed to Chen, Hu, Yip, Hu and Zhong
#     (2003), whose paper was not read. Compared with spanning-tree counts from NetworkX and closed forms.
#   centrality_kdec(): Zhang, Wang, Sheng, Dai, Hu and Chen (2019), Information 10(10):311, pp. 4-6, against the value the paper prints
#     for node 7 of its Fig. 1 and against the procedure written out over NetworkX's core numbers.
#   centrality_counting_betweenness(): Bloechl, Theis, Vega-Redondo and Fisher (2011), Physical Review E 83:046127,
#     equations (1) and (6)-(8), against the values printed for the paper's Fig. 1 and the equations summed edge by
#     edge.
# No other package cograph's tests can load implements these measures.

independent <- "independent: closed forms and hand derivations from the definition"
zhang_2019 <- "published: Zhang, Wang, Sheng, Dai, Hu and Chen 2019, worked example on Fig. 1, p. 6"
bloechl_2011 <- "published: Bloechl, Theis, Vega-Redondo and Fisher 2011, Fig. 1"
exact <- 1e-10
# KDEC(v7) = 28.7212 is printed to four decimals.
printed_4dp <- 5e-5
undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
karate <- ref_karate()

test_that("dst equals one minus the ratio of NetworkX's spanning-tree counts", {
  call <- c(dst = paste0(
    "(lambda t: {v: 1 - (lambda H: t(H) if nx.is_connected(H) else 0)(nx.restricted_view(G, [v], [])) / t(G)",
    " for v in G})(lambda H: round(nx.number_of_spanning_trees(H)))"))
  Map(\(graph, name) {
    expect_reference(centrality_dst(graph), ref_networkx(graph, call)$dst, reference = "networkx",
                     covers = c("centrality()", "centrality_dst()"), quantity = sprintf("dst, %s", name),
                     tolerance = 1e-9)
  }, list(undirected, zoob_kite()), c("undirected", "kite"))
})

test_that("dst follows Cayley's formula, the cycle count and the star", {
  lapply(c(4L, 7L), \(n) {
    expect_reference(centrality_dst(zoob_complete(n)), rep(1 - (n - 1)^(n - 3) / n^(n - 2), n), reference = independent,
                     covers = c("centrality()", "centrality_dst()"), quantity = sprintf("dst, complete graph n = %d", n),
                     tolerance = exact)
  })
  expect_reference(centrality_dst(zoob_ring(9L)), rep(1 - 1 / 9, 9), reference = independent,
                   covers = c("centrality()", "centrality_dst()"), quantity = "dst, ring n = 9", tolerance = exact)
  expect_reference(centrality_dst(zoob_star(5L)), c(1, rep(0, 5)), reference = independent,
                   covers = c("centrality()", "centrality_dst()"), quantity = "dst, star of 5", tolerance = exact)
})

# Fig. 1 of Zhang, Wang, Sheng, Dai, Hu and Chen (2019): 14 nodes, 18 edges.
kdec_figure_1 <- function() {
  zoob_graph(14L, c(1, 2, 2, 3, 3, 5, 3, 4, 5, 7, 5, 4, 5, 6, 7, 9, 9, 12, 9, 13, 7, 8, 7, 4, 7, 6, 7, 10, 8, 10,
                    10, 11, 14, 4, 4, 6))
}

test_that("kdec reproduces the value printed for node 7 of the paper's Fig. 1", {
  expect_reference(centrality_kdec(kdec_figure_1())["7"], c(`7` = 28.7212), reference = zhang_2019,
                   covers = c("centrality()", "centrality_kdec()"), quantity = "kdec, Fig. 1, node 7",
                   tolerance = printed_4dp)
})

test_that("kdec equals the paper's procedure evaluated over NetworkX's core numbers", {
  call <- c(kdec = paste0(
    "(lambda W, e, n: {i: sum(W[i] * W[j] / (n * e(j) ** 2) + sum(W[i] * W[k] / (n * (e(j) + e(k)) ** 2)",
    " for k in G[j] if k != i and k not in G[i]) for j in G[i]) for i in G})",
    "({v: nx.core_number(G)[v] * G.degree(v) for v in G}, lambda j: 1 - np.log10(1 / G.degree(j)), len(G))"))
  Map(\(graph, name) {
    expect_reference(centrality_kdec(graph), ref_networkx(graph, call)$kdec, reference = "networkx",
                     covers = c("centrality()", "centrality_kdec()"), quantity = sprintf("kdec, %s", name),
                     tolerance = 1e-9, measure = "scaled")
  }, list(undirected, karate, kdec_figure_1()), c("undirected", "karate", "Fig. 1"))
})

# Equations (6)-(8) summed edge by edge: N^st_ij = m_ij F_si with F = (I - M_-t)^-1.
cbet_definition <- function(a) {
  a <- unname(a)
  n <- nrow(a)
  m <- a / rowSums(a)
  per_target <- vapply(seq_len(n), \(target) {
    keep <- setdiff(seq_len(n), target)
    f <- solve(diag(n - 1L) - m[keep, keep, drop = FALSE])
    rowSums(vapply(seq_along(keep), \(s) {
      edge <- m[keep, keep, drop = FALSE] * f[s, ]
      visits <- (rowSums(edge) + colSums(edge)) / 2
      visits[s] <- visits[s] + 1
      replace(replace(numeric(n), keep, visits), target, 1)
    }, numeric(n)))
  }, numeric(n))
  rowSums(per_target) / (n * (n - 1))
}

# Fig. 1 of Bloechl et al. (2011), Newman's example: two K5 (nodes 1-5 and 6-10) joined by 5-6, and node 11 tied to
# 5 and 6. The paper labels a = 1, b = 5, c = 11.
bloechl_figure_1 <- function() {
  k5 <- t(utils::combn(5L, 2L))
  zoob_graph(11L, rbind(k5, k5 + 5L, c(5, 6), c(5, 11), c(6, 11)))
}

test_that("counting betweenness of the paper's Fig. 1, to the precision the print supports", {
  # The paper prints C_c = 1.93, 2.80 and 1.03 for a, b and c. Equations (6)-(8) give 1.9394, 2.7915 and 1.0339: c
  # agrees to the printed digits, a and b differ by one unit in the last printed digit. The companion measure of
  # the same figure (random walk centrality, 0.048, 0.094, 0.044) is reproduced to every printed digit on this
  # graph, so the graph is the paper's. The comparison is made at one unit of the last printed place.
  expect_reference(centrality_counting_betweenness(bloechl_figure_1())[c("1", "5", "11")],
                   c(`1` = 1.93, `5` = 2.80, `11` = 1.03), reference = bloechl_2011,
                   covers = c("centrality()", "centrality_counting_betweenness()"),
                   quantity = "counting_betweenness, Fig. 1, nodes a, b, c", tolerance = 0.01,
                   note = "one unit of the last printed digit; a and b computed 1.9394 and 2.7915")
})

test_that("counting betweenness equals equations (6)-(8) summed edge by edge", {
  directed <- ref_connected_graph(9, density = 0.35, seed = 56L, directed = TRUE, weighted = TRUE, digits = 3L)
  ring <- zoob_ring(9L, directed = TRUE)
  dimnames(ring) <- dimnames(directed)
  # A directed cycle laid over the random arcs makes the graph strongly connected; two loops are added.
  strong <- directed + ring
  diag(strong)[c(2L, 6L)] <- c(0.8, 1.9)
  graphs <- list(kite = zoob_kite(), `Fig. 1` = bloechl_figure_1(), `weighted directed with loops` = strong)
  Map(\(graph, name) {
    expect_reference(centrality_counting_betweenness(graph), ref_named(cbet_definition(graph), graph),
                     reference = independent, covers = c("centrality()", "centrality_counting_betweenness()"),
                     quantity = sprintf("counting_betweenness, edge form, %s", name), tolerance = 1e-9)
  }, graphs, names(graphs))
})
