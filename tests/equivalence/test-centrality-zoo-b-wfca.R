# Weighted formal concept analysis centrality, centrality_wfca() (Sun, Wang, Sheng, Hu, Wang and Shao 2017, IEEE Access
# 5:3777-3789), Algorithm 1 and formula (3), p. 3782: W_i = sum over the formal concepts (T, P) of the adjacency
# context with i in P of |T| / |P|. Compared with the two weights the paper works out for its 15-node example and
# with the concepts derived from the definition (every subset of objects, its common attributes, closed back).
# No other package implements this measure.

independent <- "independent: closed forms and hand derivations from the definition"
sun_2017 <- "published: Sun, Wang, Sheng, Hu, Wang and Shao 2017, worked sums W_1 and W_7, p. 3782"
exact <- 1e-10
# The paper prints 2.666 and 9.333 (digits cut after the third decimal).
printed_3dp <- 1e-3

# Every formal concept of a 0/1 context, from the definition.
wfca_definition <- function(a) {
  a <- unname(a)
  n <- nrow(a)
  has <- a != 0
  intents <- unique(t(vapply(seq_len(2^n) - 1L, \(code) {
    objects <- as.logical(bitwAnd(code, 2^(seq_len(n) - 1L)))
    if (any(objects)) colSums(!has[objects, , drop = FALSE]) == 0 else rep(TRUE, n)
  }, logical(n))))
  shares <- vapply(seq_len(nrow(intents)), \(k) {
    intent <- intents[k, ]
    if (!any(intent)) return(numeric(n))
    intent * sum(rowSums(!has[, intent, drop = FALSE]) == 0) / sum(intent)
  }, numeric(n))
  rowSums(shares)
}

test_that("wfca reproduces the two weights the paper works out for its 15-node example", {
  # The context printed in the paper's Table 2, as an undirected graph of 22 edges (the printed table lacks the
  # cell 14-8 that its cell 8-14 implies; both readings give the same W_1 and W_7).
  example <- zoob_graph(15L, c(1, 3, 1, 6, 2, 3, 3, 4, 3, 5, 3, 6, 3, 7, 7, 8, 7, 9, 7, 10, 7, 12, 8, 9, 8, 14, 8, 15,
                               9, 10, 9, 11, 10, 11, 11, 15, 12, 13, 12, 14, 13, 14, 14, 15))
  expect_reference(centrality_wfca(example)[c("1", "7")], c(`1` = 2.666, `7` = 9.333), reference = sun_2017,
                   covers = c("centrality()", "centrality_wfca()"),
                   quantity = "wfca, 15-node example, nodes 1 and 7 (8/3 and 28/3)", tolerance = printed_3dp)
})

test_that("wfca equals the sum over the concepts derived from the definition", {
  directed <- ref_connected_graph(9, density = 0.3, seed = 61L, directed = TRUE)
  graphs <- list(kite = zoob_kite(), `random 10 nodes` = ref_connected_graph(10, density = 0.35, seed = 60L),
                 `directed 9 nodes` = directed)
  Map(\(graph, name) {
    expect_reference(centrality_wfca(graph), ref_named(wfca_definition(graph), graph), reference = independent,
                     covers = c("centrality()", "centrality_wfca()"),
                     quantity = sprintf("wfca, all concepts, %s", name), tolerance = exact, measure = "scaled")
  }, graphs, names(graphs))
})

test_that("wfca follows hand derivations on a star and a triangle", {
  expect_reference(centrality_wfca(zoob_star(3L)), c(3, rep(1 / 3, 3)), reference = independent,
                   covers = c("centrality()", "centrality_wfca()"), quantity = "wfca, star of 3", tolerance = exact)
  expect_reference(centrality_wfca(zoob_complete(3L)), rep(3, 3), reference = independent,
                   covers = c("centrality()", "centrality_wfca()"), quantity = "wfca, triangle", tolerance = exact)
})
