# centrality_lnc() (local neighbor contribution, Dai et al. 2019, IEEE Access 7, 131719-131731) and
# centrality_ked() (the KED method, Chen, Xiao, Zeng and Zhang 2014, EPL 104, 68006). Both are compared with the
# values their papers print for their figure graphs, and with the definitions written out below in base R.
#
# Definitions, with k_i the degree, K_i = sum of the neighbours' degrees and n the number of nodes:
#   LNC (Dai 2019 eqs. 1-6): LNC_i = k_i^3 (1 - 1/k_i)^(k_i - 1) K_i / (n - 1); 0^0 = 1, so a leaf's own
#     contribution is 1; an isolate scores 0.
#   KED (Chen 2014 eq. 6): KED_i = k_i (1 + H_i) exp(K_i / n), H_i = -sum_j p_j log p_j / log k_i over the
#     neighbours j, p_j = k_j / K_i; H_i = 0 for a node with one neighbour (0/0); an isolate scores 0.

lnc_definition <- function(graph) {
  degree <- rowSums(graph)
  cluster <- as.numeric(graph %*% degree)
  own <- degree^3 * (1 - 1 / degree)^(degree - 1)
  stats::setNames(ifelse(degree > 0, own * cluster / (nrow(graph) - 1), 0), rownames(graph))
}

ked_definition <- function(graph) {
  degree <- rowSums(graph)
  cluster <- as.numeric(graph %*% degree)
  diversity <- vapply(seq_len(nrow(graph)), \(i) {
    shares <- degree[graph[i, ] > 0] / cluster[i]
    if (degree[i] > 1) -sum(shares * log(shares)) / log(degree[i]) else 0
  }, numeric(1))
  stats::setNames(degree * (1 + diversity) * exp(cluster / nrow(graph)), rownames(graph))
}

# Dai et al. 2019, Figure 1: eleven nodes, v1 to v11.
dai_figure1 <- function() {
  zoob_graph(11L, c(2, 1, 1, 3, 1, 4, 1, 5, 5, 6, 5, 7, 5, 8, 6, 8, 7, 8, 8, 9, 9, 10, 9, 11, 10, 11))
}

# Chen et al. 2014, figure 1: a red node (1) joined to five blue nodes (2-6); in (a) each blue node carries three
# grey leaves, in (b) four blue nodes are leaves and node 6 carries fifteen grey leaves. 21 nodes each.
chen_panel <- function(panel) {
  spokes <- c(rbind(1, 2:6))
  leaves <- if (panel == "a") c(rbind(rep(2:6, each = 3), 7:21)) else c(rbind(6, 7:21))
  zoob_graph(21L, c(spokes, leaves))
}

independent <- "independent: closed forms and hand derivations from the definition"
exact <- 1e-10

test_that("LNC equals Dai et al. (2019) Table 1 and its ranking", {
  score <- centrality_lnc(dai_figure1())
  # Table 1 prints one decimal: half a unit in the last place is 0.05, absolute.
  expect_reference(score, c(18.9, 0.4, 0.4, 0.4, 32.4, 3.2, 3.2, 29.7, 9.6, 2, 2),
                   reference = "published: Dai et al. 2019, Table 1", covers = c("centrality()", "centrality_lnc()"),
                   quantity = "lnc, figure 1", tolerance = 0.05)
  # The text ranks v5, v8, v1, v9 first to fourth: ranks are whole numbers, so 0.5 separates them.
  expect_reference(rank(-score)[c("5", "8", "1", "9")], c(`5` = 1, `8` = 2, `1` = 3, `9` = 4),
                   reference = "published: Dai et al. 2019, Table 1", covers = c("centrality()", "centrality_lnc()"),
                   quantity = "lnc, figure 1, rank of the top four", tolerance = 0.5)
})

test_that("LNC equals its definition written out", {
  graphs <- list(`figure 1 of Dai 2019` = dai_figure1(), kite = zoob_kite(), karate = zoob_karate(),
                 `kite with an isolate` = zoob_union(zoob_kite(), zoob_graph(1L, matrix(numeric(), 0, 2))))
  Map(\(graph, name) {
    expect_reference(centrality_lnc(graph), lnc_definition(graph), reference = independent,
                     covers = c("centrality()", "centrality_lnc()"), quantity = paste0("lnc, definition, ", name),
                     tolerance = exact, measure = "relative")
  }, graphs, names(graphs))
  # Closed forms: every leaf of a q-star scores 1 and the centre q^3 (1 - 1/q)^(q - 1); a ring of n scores
  # 16 / (n - 1); the path of five scores 1 * 2 / 4, 4 * 3 / 4, 4 * 4 / 4, 4 * 3 / 4, 1 * 2 / 4.
  expect_reference(centrality_lnc(zoob_star(5L)), c(5^3 * (4 / 5)^4, rep(1, 5)), reference = independent,
                   covers = c("centrality()", "centrality_lnc()"), quantity = "lnc, star of 5 leaves", tolerance = exact)
  # A complete graph of n: every K_i is k^2 with k = n - 1, so the score is k^4 (1 - 1/k)^(k - 1).
  expect_reference(centrality_lnc(zoob_complete(6L)), rep(5^4 * (4 / 5)^4, 6), reference = independent,
                   covers = c("centrality()", "centrality_lnc()"), quantity = "lnc, complete graph of 6",
                   tolerance = exact, measure = "relative")
  expect_reference(centrality_lnc(zoob_ring(7L)), rep(16 / 6, 7), reference = independent,
                   covers = c("centrality()", "centrality_lnc()"), quantity = "lnc, ring of 7", tolerance = exact)
  expect_reference(centrality_lnc(zoob_path(5L)), c(2 / 4, 12 / 4, 16 / 4, 12 / 4, 2 / 4), reference = independent,
                   covers = c("centrality()", "centrality_lnc()"), quantity = "lnc, path of 5", tolerance = exact)
})

test_that("KED equals the two scores Chen et al. (2014) print for figure 1", {
  # Page 4: "the f_i for the red node is 25.9187 in fig. 1(a) and 19.2212 in fig. 1(b)". Four decimals: half a
  # unit in the last place is 5e-5, absolute.
  scores <- c(a = centrality_ked(chen_panel("a"))[["1"]], b = centrality_ked(chen_panel("b"))[["1"]])
  expect_reference(scores, c(a = 25.9187, b = 19.2212), reference = "published: Chen et al. 2014, p. 4",
                   covers = c("centrality()", "centrality_ked()"), quantity = "ked, red node of figure 1, panel",
                   tolerance = 5e-5)
})

test_that("KED equals its definition written out", {
  graphs <- list(`figure 1(a) of Chen 2014` = chen_panel("a"), `figure 1(b) of Chen 2014` = chen_panel("b"),
                 kite = zoob_kite(), karate = zoob_karate(),
                 `kite with an isolate` = zoob_union(zoob_kite(), zoob_graph(1L, matrix(numeric(), 0, 2))))
  Map(\(graph, name) {
    expect_reference(centrality_ked(graph), ked_definition(graph), reference = independent,
                     covers = c("centrality()", "centrality_ked()"), quantity = paste0("ked, definition, ", name),
                     tolerance = exact, measure = "relative")
  }, graphs, names(graphs))
  # Closed forms: a q-star's leaves score exp(q / (q + 1)) and its centre 2q times that (H = 1 at the centre);
  # an r-regular graph on n nodes scores 2 r exp(r^2 / n); on the path of five the ends have H = 0, the middle
  # H = 1 and nodes 2 and 4 see the shares 1/3 and 2/3.
  expect_reference(centrality_ked(zoob_star(4L)), c(8 * exp(4 / 5), rep(exp(4 / 5), 4)), reference = independent,
                   covers = c("centrality()", "centrality_ked()"), quantity = "ked, star of 4 leaves", tolerance = exact)
  expect_reference(centrality_ked(zoob_complete(5L)), rep(8 * exp(16 / 5), 5), reference = independent,
                   covers = c("centrality()", "centrality_ked()"), quantity = "ked, complete graph of 5",
                   tolerance = exact, measure = "relative")
  expect_reference(centrality_ked(zoob_ring(6L)), rep(4 * exp(4 / 6), 6), reference = independent,
                   covers = c("centrality()", "centrality_ked()"), quantity = "ked, ring of 6", tolerance = exact)
  e2 <- 1 + (-(1 / 3) * log(1 / 3) - (2 / 3) * log(2 / 3)) / log(2)
  expect_reference(centrality_ked(zoob_path(5L)),
                   c(exp(2 / 5), 2 * e2 * exp(3 / 5), 4 * exp(4 / 5), 2 * e2 * exp(3 / 5), exp(2 / 5)),
                   reference = independent, covers = c("centrality()", "centrality_ked()"), quantity = "ked, path of 5",
                   tolerance = exact)
})
