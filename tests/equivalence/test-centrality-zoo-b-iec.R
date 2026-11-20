# Immediate effects centrality, centrality_iec() (Friedkin 1991, American Journal of Sociology 96(6):1478-1504),
# against the paper's Table 1 (pp. 1492-1494: IEC of every node of all 21 connected five-node graphs, printed to
# three decimals) and against computations written out from the definition: W = A / rowSums(A) with a_ii = 1
# (p. 1494), the mean first passage matrix M = (I - Z + E Z_dg) D of equation (11) (Kemeny and Snell 1960), with
# Z = (I - W + W^inf)^-1 and D = diag(1 / c), c the stationary vector, and equation (20),
# IEC_j = (n - 1) / sum_{i != j} m_ij. No other package implements this measure.
#
# Table 1's graphs are transcribed from the page images into
# equivalence/cograph/local/local_testing_and_equivalence/batch49/source_graph_edges.csv; the node numbers are
# that transcription's.

independent <- "independent: closed forms and hand derivations from the definition"
friedkin <- "published: Friedkin 1991, Table 1, pp. 1492-1494"
# Exact rationals, or an eigenvector and a 10 x 10 solve: agreement to rounding.
exact <- 1e-10
# Table 1 prints three decimals: half a unit in the last printed place, absolute.
printed_3dp <- 5e-4

# Table 1: the edges of each network and its printed IEC column, node 1 to 5.
friedkin_table_1 <- list(
  `1` = list(edges = c(1, 2, 2, 3, 2, 4, 2, 5), iec = c(0.080, 0.500, 0.080, 0.080, 0.080)),
  `2` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4), iec = c(0.133, 0.250, 0.069, 0.069, 0.056)),
  `3` = list(edges = c(1, 2, 1, 5, 2, 3, 3, 4), iec = c(0.105, 0.167, 0.105, 0.050, 0.050)),
  `4` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 3), iec = c(0.400, 0.133, 0.133, 0.067, 0.067)),
  `5` = list(edges = c(1, 2, 1, 5, 2, 4, 2, 5, 3, 5), iec = c(0.143, 0.222, 0.059, 0.059, 0.222)),
  `6` = list(edges = c(1, 2, 1, 3, 2, 3, 2, 5, 4, 5), iec = c(0.105, 0.222, 0.105, 0.045, 0.105)),
  `7` = list(edges = c(1, 2, 1, 5, 2, 3, 3, 5, 4, 5), iec = c(0.142, 0.125, 0.142, 0.060, 0.235)),
  `8` = list(edges = c(1, 4, 1, 5, 2, 3, 2, 5, 3, 4), iec = c(0.133, 0.133, 0.133, 0.133, 0.133)),
  `9` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 5, 3, 4), iec = c(0.333, 0.109, 0.109, 0.109, 0.109)),
  `10` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4, 3, 5, 4, 5), iec = c(0.129, 0.214, 0.129, 0.129, 0.214)),
  `11` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 5, 3, 5, 4, 5), iec = c(0.125, 0.200, 0.125, 0.057, 0.333)),
  `12` = list(edges = c(1, 2, 1, 5, 2, 4, 2, 5, 3, 4, 3, 5), iec = c(0.118, 0.210, 0.120, 0.120, 0.210)),
  `13` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 5, 3, 4, 3, 5), iec = c(0.111, 0.199, 0.200, 0.051, 0.199)),
  `14` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 4, 3, 4, 3, 5), iec = c(0.286, 0.108, 0.183, 0.183, 0.108)),
  `15` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4, 2, 5, 3, 5, 4, 5), iec = c(0.114, 0.286, 0.114, 0.114, 0.286)),
  `16` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4, 3, 4, 3, 5, 4, 5), iec = c(0.111, 0.187, 0.185, 0.185, 0.187)),
  `17` = list(edges = c(1, 2, 1, 3, 1, 5, 2, 3, 2, 5, 3, 5, 4, 5), iec = c(0.178, 0.178, 0.178, 0.050, 0.286)),
  `18` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 4, 2, 5, 3, 4, 3, 5), iec = c(0.250, 0.167, 0.167, 0.167, 0.167)),
  `19` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4, 2, 5, 3, 4, 3, 5, 4, 5), iec = c(0.100, 0.250, 0.164, 0.164, 0.250)),
  `20` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 3, 2, 4, 3, 4, 3, 5, 4, 5),
              iec = c(0.222, 0.150, 0.222, 0.222, 0.150)),
  `21` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 3, 2, 4, 2, 5, 3, 4, 3, 5, 4, 5),
              iec = c(0.200, 0.200, 0.200, 0.200, 0.200))
)

# Equations (11) and (20) for a strongly connected graph (the chain W is then irreducible and, with a_ii = 1,
# aperiodic).
iec_definition <- function(a) {
  n <- nrow(a)
  a <- unname(a)
  diag(a) <- 1
  w <- a / rowSums(a)
  leading <- eigen(t(w))
  stationary <- Re(leading$vectors[, 1L])
  stationary <- stationary / sum(stationary)
  z <- solve(diag(n) - w + matrix(stationary, n, n, byrow = TRUE))
  m <- (diag(n) - z + matrix(1, n, n) %*% diag(diag(z))) %*% diag(1 / stationary)
  (n - 1) / (colSums(m) - diag(m))
}

test_that("iec reproduces Friedkin's Table 1 for the 21 five-node graphs", {
  # Network 16, nodes 2 and 5, are recorded separately below.
  Map(\(network, entry) {
    keep <- if (network == "16") c(1L, 3L, 4L) else seq_len(5L)
    expect_reference(centrality_iec(zoob_graph(5L, entry$edges))[keep], entry$iec[keep], reference = friedkin,
                     covers = c("centrality()", "centrality_iec()"),
                     quantity = sprintf("iec, Table 1 network %s", network), tolerance = printed_3dp)
  }, names(friedkin_table_1), friedkin_table_1)
})

test_that("iec of Table 1 network 16 at the two half-unit ties", {
  # The exact IEC of nodes 2 and 5 is 3/16 = 0.1875, half a unit from both .187 and .188; the table prints .187
  # (the tie rounded down). A comparison at exactly the half-unit would be decided by the last bit of floating
  # point, so these two cells are compared with equations (11) and (20) written out instead of with the print.
  graph <- zoob_graph(5L, friedkin_table_1[["16"]]$edges)
  expect_reference(centrality_iec(graph)[c(2L, 5L)], iec_definition(graph)[c(2L, 5L)], reference = independent,
                   covers = c("centrality()", "centrality_iec()"),
                   quantity = "iec, Table 1 network 16, nodes 2 and 5 (printed .187, a rounding tie)",
                   tolerance = exact)
})

test_that("iec of complete graphs, stars, rings and directed cycles matches the closed forms", {
  # Complete graph: W = J / n, every m_ij = n, IEC = 1 / n.
  lapply(c(3L, 6L, 9L), \(n) {
    expect_reference(centrality_iec(zoob_complete(n)), rep(1 / n, n), reference = independent,
                     covers = c("centrality()", "centrality_iec()"), quantity = sprintf("iec, complete graph n = %d", n),
                     tolerance = exact)
  })
  # Star with m leaves: a leaf's mean first passage to the centre is 2, so IEC(centre) = m / (2 m) = 1 / 2; for a
  # leaf target x = 3m - 1 from the centre and y = 3m + 1 from another leaf, so IEC(leaf) = m / ((3m - 2)(m + 1)).
  lapply(c(2L, 5L, 9L), \(m) {
    expect_reference(centrality_iec(zoob_star(m)), c(0.5, rep(m / ((3 * m - 2) * (m + 1)), m)),
                     reference = independent, covers = c("centrality()", "centrality_iec()"),
                     quantity = sprintf("iec, star of %d", m), tolerance = exact)
  })
  # Ring C_q: the cycle walk made lazy with probability 1/3; m(d) = (3/2) d (q - d), column sum q (q^2 - 1) / 4,
  # IEC = 4 / (q (q + 1)).
  lapply(c(3L, 6L, 10L), \(q) {
    expect_reference(centrality_iec(zoob_ring(q)), rep(4 / (q * (q + 1)), q), reference = independent,
                     covers = c("centrality()", "centrality_iec()"), quantity = sprintf("iec, ring n = %d", q),
                     tolerance = exact)
  })
  # Directed cycle: stay or advance with probability 1/2 each, m_ij = 2 d, column sum q (q - 1), IEC = 1 / q.
  lapply(c(3L, 5L, 9L), \(q) {
    expect_reference(centrality_iec(zoob_ring(q, directed = TRUE), directed = TRUE), rep(1 / q, q),
                     reference = independent, covers = c("centrality()", "centrality_iec()"),
                     quantity = sprintf("iec, directed cycle n = %d", q), tolerance = exact)
  })
})

test_that("iec equals equations (11) and (20) on strongly connected graphs", {
  directed <- ref_connected_graph(8, density = 0.25, seed = 3L, directed = TRUE)
  expect_reference(centrality_iec(zoob_kite()), iec_definition(zoob_kite()), reference = independent,
                   covers = c("centrality()", "centrality_iec()"), quantity = "iec, Kemeny-Snell, kite",
                   tolerance = exact)
  expect_reference(centrality_iec(zoob_karate()), iec_definition(zoob_karate()), reference = independent,
                   covers = c("centrality()", "centrality_iec()"), quantity = "iec, Kemeny-Snell, karate club",
                   tolerance = exact)
  expect_reference(centrality_iec(directed, directed = TRUE), iec_definition(directed), reference = independent,
                   covers = c("centrality()", "centrality_iec()"),
                   quantity = "iec, Kemeny-Snell, directed strongly connected", tolerance = exact)
})
