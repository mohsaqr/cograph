# X-nonbacktracking centrality, centrality_x_nonbacktracking() (Torres, Chan, Tong and Eliassi-Rad 2021, SIAM
# Journal on Mathematics of Data Science 3(2):656-675), Proposition 3.7 / equation (3.11), pp. 660-661:
# alpha_c = u' X v with u = P v, v the principal right eigenvector of the nonbacktracking matrix B' of the graph
# without c, scaled so that u' v = 1, and X = D F E from the block partition (3.1) of the nonbacktracking matrix B
# of the whole graph; P reverses every directed edge. cograph evaluates the node form of the same equation,
# (sum_i a_ci v^i)^2 - sum_i a_ci (v^i)^2 with v^i the nonbacktracking centralities (2.2). The reference here stays
# on the directed edges: it builds B, cuts out D, E and F, forms X and evaluates the quadratic form.
#
# The paper prints no per-node value, and the authors' program (github.com/leotrs/inbox) fails on its exact branch
# and defaults to the approximation of section 4, a different quantity. No other package implements the measure.

independent <- "independent: closed forms and hand derivations from the definition"
# An eigenvector of a non-symmetric matrix, entering quadratically.
spectral <- 1e-8
exact <- 1e-10

# B of equation (2.1) for the directed edges given as two integer vectors.
xnb_matrix <- function(from, to) (outer(from, to, "==") & !outer(to, from, "==")) * 1

# Equation (3.11) on the directed edges, for a node whose removal leaves a simple largest eigenvalue.
xnb_edge_space <- function(a, focal) {
  a <- unname(a)
  arcs <- which(a != 0, arr.ind = TRUE)
  incident <- arcs[, 1L] == focal | arcs[, 2L] == focal
  arcs <- rbind(arcs[!incident, , drop = FALSE], arcs[incident, , drop = FALSE])
  rest <- seq_len(sum(!incident))
  b <- xnb_matrix(arcs[, 1L], arcs[, 2L])
  x <- b[rest, -rest, drop = FALSE] %*% b[-rest, -rest, drop = FALSE] %*% b[-rest, rest, drop = FALSE]
  spectrum <- eigen(b[rest, rest, drop = FALSE])
  v <- Re(spectrum$vectors[, which.max(Re(spectrum$values))])
  u <- v[match(paste(arcs[rest, 2L], arcs[rest, 1L]), paste(arcs[rest, 1L], arcs[rest, 2L]))]
  sum(u * (x %*% v)) / sum(u * v)
}

test_that("x_nonbacktracking equals the edge-space quadratic form of Proposition 3.7", {
  graphs <- list(kite = zoob_kite(), `karate club` = zoob_karate(), `complete graph n = 6` = zoob_complete(6L),
                 `random 12 nodes` = ref_connected_graph(12, density = 0.45, seed = 54L))
  Map(\(graph, name) {
    actual <- centrality_x_nonbacktracking(graph)
    expected <- ref_named(vapply(seq_len(nrow(graph)), \(focal) xnb_edge_space(graph, focal), numeric(1)), graph)
    expect_reference(actual, expected, reference = independent,
                     covers = c("centrality()", "centrality_x_nonbacktracking()"),
                     quantity = sprintf("x_nonbacktracking, edge-space quadratic form, %s", name),
                     tolerance = spectral, measure = "scaled")
  }, graphs, names(graphs))
})

test_that("x_nonbacktracking follows closed forms", {
  # K_n: the rest is K_(n-1) with a constant eigenvector; the node form gives (n - 2)^2.
  lapply(c(5L, 7L), \(n) {
    expect_reference(centrality_x_nonbacktracking(zoob_complete(n)), rep((n - 2)^2, n), reference = independent,
                     covers = c("centrality()", "centrality_x_nonbacktracking()"),
                     quantity = sprintf("x_nonbacktracking, complete graph n = %d", n), tolerance = exact)
  })
  # A node of degree one has X = 0 (the paper, below equation 3.3): the pendant of K5 scores 0, and the nodes of
  # K5 other than its anchor score as in K5.
  pendant <- zoob_graph(6L, c(1, 2, 1, 3, 1, 4, 1, 5, 2, 3, 2, 4, 2, 5, 3, 4, 3, 5, 4, 5, 1, 6))
  expect_reference(centrality_x_nonbacktracking(pendant)[2:6], c(`2` = 9, `3` = 9, `4` = 9, `5` = 9, `6` = 0),
                   reference = independent, covers = c("centrality()", "centrality_x_nonbacktracking()"),
                   quantity = "x_nonbacktracking, K5 with a pendant", tolerance = exact)
})
