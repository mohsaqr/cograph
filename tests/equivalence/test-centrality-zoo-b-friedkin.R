# Total and mediative effects centrality, centrality_tec() and centrality_mec() (Friedkin 1991, American Journal of
# Sociology 96(6):1478-1504), against the paper's Table 1 (pp. 1492-1494: TEC and MEC of every node of all 21
# connected five-node graphs, printed to three decimals) and against computations written out from the definition.
# W = A / rowSums(A) with a_ii = 1 (p. 1494). TEC, equation (14), p. 1487, is the mean of column j of the total
# effects matrix; with the uniform total effects of p. 1484 every row of that matrix is the stationary vector c of
# W, so TEC_j = c_j. MEC, equations (22)-(23), p. 1490: tbar_(k)j = sum_{i != j,k} t_(k)ij / ((n - 2) t_(k)jj) with
# T_(k) = (I - W_(k))^-1, W_(k) the matrix W without row and column k (equation 13, p. 1486), and
# MEC_j = sum_{k != j} tbar_(k)j / (n - 1). No other package implements these measures.
#
# The graphs are read from the diagrams of Table 1, whose points are "labeled counterclockwise with first points at
# 12:00" (note to Table 1). Network 6 is the roof triangle 1-2-5 with the side 2-3 and the base 3-4. The IEC file
# (test-centrality-zoo-b-iec.R) holds it with points 3 and 5 exchanged, which TEC and IEC cannot tell apart (both
# print the same value at points 1, 3 and 5) and MEC can (.600 at point 3, .450 at points 1 and 5).

independent <- "independent: closed forms and hand derivations from the definition"
friedkin <- "published: Friedkin 1991, Table 1, pp. 1492-1494"
# Rationals and 4 x 4 solves: agreement to rounding.
exact <- 1e-10
# Table 1 prints three decimals: half a unit in the last printed place, absolute.
printed_3dp <- 5e-4

friedkin_table_1 <- list(
  `1` = list(edges = c(1, 2, 2, 3, 2, 4, 2, 5),
             tec = c(.154, .385, .154, .154, .154), mec = c(.375, 1.000, .375, .375, .375)),
  `2` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4),
             tec = c(.231, .308, .154, .154, .154), mec = c(.667, .875, .347, .347, .264)),
  `3` = list(edges = c(1, 2, 1, 5, 2, 3, 3, 4),
             tec = c(.231, .231, .231, .154, .154), mec = c(.625, .750, .625, .250, .250)),
  `4` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 3),
             tec = c(.333, .200, .200, .133, .133), mec = c(.917, .500, .500, .292, .292)),
  `5` = list(edges = c(1, 2, 1, 5, 2, 4, 2, 5, 3, 5),
             tec = c(.200, .267, .133, .133, .267), mec = c(.533, .742, .242, .242, .742)),
  `6` = list(edges = c(1, 2, 1, 5, 2, 5, 2, 3, 3, 4),
             tec = c(.200, .267, .200, .133, .200), mec = c(.450, .792, .600, .208, .450)),
  `7` = list(edges = c(1, 2, 1, 5, 2, 3, 3, 5, 4, 5),
             tec = c(.200, .200, .200, .133, .267), mec = c(.526, .472, .526, .226, .750)),
  `8` = list(edges = c(1, 4, 1, 5, 2, 3, 2, 5, 3, 4),
             tec = c(.200, .200, .200, .200, .200), mec = c(.500, .500, .500, .500, .500)),
  `9` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 5, 3, 4),
             tec = c(.294, .176, .176, .176, .176), mec = c(.833, .417, .417, .417, .417)),
  `10` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4, 3, 5, 4, 5),
              tec = c(.176, .235, .176, .176, .235), mec = c(.417, .625, .417, .417, .625)),
  `11` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 5, 3, 5, 4, 5),
              tec = c(.176, .235, .176, .118, .294), mec = c(.431, .611, .431, .209, .817)),
  `12` = list(edges = c(1, 2, 1, 5, 2, 4, 2, 5, 3, 4, 3, 5),
              tec = c(.176, .235, .176, .176, .235), mec = c(.386, .629, .428, .428, .629)),
  `13` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 5, 3, 4, 3, 5),
              tec = c(.176, .235, .235, .118, .235), mec = c(.383, .624, .683, .186, .624)),
  `14` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 4, 3, 4, 3, 5),
              tec = c(.263, .158, .211, .211, .158), mec = c(.714, .345, .548, .548, .345)),
  `15` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4, 2, 5, 3, 5, 4, 5),
              tec = c(.158, .263, .158, .158, .263), mec = c(.361, .708, .361, .361, .708)),
  `16` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4, 3, 4, 3, 5, 4, 5),
              tec = c(.158, .211, .211, .211, .211), mec = c(.357, .548, .523, .523, .548)),
  `17` = list(edges = c(1, 2, 1, 3, 1, 5, 2, 3, 2, 5, 3, 5, 4, 5),
              tec = c(.210, .210, .210, .105, .263), mec = c(.528, .528, .528, .167, .750)),
  `18` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 4, 2, 5, 3, 4, 3, 5),
              tec = c(.238, .190, .190, .190, .190), mec = c(.619, .470, .470, .470, .470)),
  `19` = list(edges = c(1, 2, 1, 5, 2, 3, 2, 4, 2, 5, 3, 4, 3, 5, 4, 5),
              tec = c(.143, .238, .190, .190, .238), mec = c(.302, .635, .464, .464, .635)),
  `20` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 3, 2, 4, 3, 4, 3, 5, 4, 5),
              tec = c(.217, .174, .217, .217, .174), mec = c(.560, .411, .560, .560, .411)),
  `21` = list(edges = c(1, 2, 1, 3, 1, 4, 1, 5, 2, 3, 2, 4, 2, 5, 3, 4, 3, 5, 4, 5),
              tec = c(.200, .200, .200, .200, .200), mec = c(.500, .500, .500, .500, .500))
)

# W of p. 1494 for a 0/1 matrix.
friedkin_influence <- function(a) {
  a <- unname(a)
  diag(a) <- 1
  a / rowSums(a)
}

# The stationary vector as the leading left eigenvector of W.
tec_definition <- function(a) {
  stationary <- Re(eigen(t(friedkin_influence(a)))$vectors[, 1L])
  stationary / sum(stationary)
}

# Equations (22)-(23) through hitting probabilities instead of the fundamental matrices T_(k): the probability of
# reaching j before k from i, t_(k)ij / t_(k)jj, solves h = W h off {j, k} with h_j = 1 and h_k = 0.
mec_definition <- function(a) {
  w <- friedkin_influence(a)
  n <- nrow(w)
  share <- function(j, k) {
    free <- setdiff(seq_len(n), c(j, k))
    sum(solve(diag(length(free)) - w[free, free, drop = FALSE], w[free, j])) / (n - 2)
  }
  vapply(seq_len(n), \(j) sum(vapply(setdiff(seq_len(n), j), \(k) share(j, k), numeric(1))) / (n - 1), numeric(1))
}

test_that("mec reproduces Friedkin's Table 1 for the 21 five-node graphs", {
  Map(\(network, entry) {
    expect_reference(centrality_mec(zoob_graph(5L, entry$edges)), entry$mec, reference = friedkin,
                     covers = c("centrality()", "centrality_mec()"),
                     quantity = sprintf("mec, Table 1 network %s", network), tolerance = printed_3dp)
  }, names(friedkin_table_1), friedkin_table_1)
})

test_that("tec reproduces Friedkin's Table 1 for the 21 five-node graphs", {
  # Network 17, points 1 to 3, are recorded separately below.
  Map(\(network, entry) {
    keep <- if (network == "17") 4:5 else seq_len(5L)
    expect_reference(centrality_tec(zoob_graph(5L, entry$edges))[keep], entry$tec[keep], reference = friedkin,
                     covers = c("centrality()", "centrality_tec()"),
                     quantity = sprintf("tec, Table 1 network %s", network), tolerance = printed_3dp)
  }, names(friedkin_table_1), friedkin_table_1)
})

test_that("tec of Table 1 network 17 at the three cells the table prints as .210", {
  # Points 1 to 3 have degree 3 in a graph of 7 lines, so TEC = (3 + 1) / (14 + 5) = 4/19 = 0.2105..., which rounds
  # to .211; the table prints .210 (the digits cut, not rounded). Every other TEC cell of the table is the rounded
  # value. The three cells are compared with the exact fraction.
  graph <- zoob_graph(5L, friedkin_table_1[["17"]]$edges)
  expect_reference(centrality_tec(graph)[1:3], c(`1` = 4 / 19, `2` = 4 / 19, `3` = 4 / 19), reference = independent,
                   covers = c("centrality()", "centrality_tec()"),
                   quantity = "tec, Table 1 network 17, points 1 to 3, exact 4/19", tolerance = exact)
})

test_that("mec and tec follow closed forms on stars, cycles and complete graphs", {
  # Star of four leaves: the hub transmits everything; a leaf scores (0 + 3 / 2) / 4 = 3/8.
  expect_reference(centrality_mec(zoob_star(4L)), c(1, rep(3 / 8, 4)), reference = independent,
                   covers = c("centrality()", "centrality_mec()"), quantity = "mec, star of 4", tolerance = exact)
  # Directed cycle of four: the three absorbers give 1/2, 0 and 1.
  expect_reference(centrality_mec(zoob_ring(4L, directed = TRUE)), rep(1 / 2, 4), reference = independent,
                   covers = c("centrality()", "centrality_mec()"), quantity = "mec, directed cycle of 4",
                   tolerance = exact)
  # Vertex-transitive graphs: by symmetry between j and k the two hitting probabilities are equal and sum to one.
  expect_reference(centrality_mec(zoob_complete(6L)), rep(1 / 2, 6), reference = independent,
                   covers = c("centrality()", "centrality_mec()"), quantity = "mec, complete graph n = 6",
                   tolerance = exact)
  expect_reference(centrality_mec(zoob_ring(7L)), rep(1 / 2, 7), reference = independent,
                   covers = c("centrality()", "centrality_mec()"), quantity = "mec, ring n = 7", tolerance = exact)
  # Undirected: detailed balance gives c_i proportional to k_i + 1.
  kite <- zoob_kite()
  expect_reference(centrality_tec(kite), (rowSums(kite) + 1) / sum(rowSums(kite) + 1), reference = independent,
                   covers = c("centrality()", "centrality_tec()"), quantity = "tec, kite, (k + 1) / sum(k + 1)",
                   tolerance = exact)
})

test_that("mec and tec equal their definitions evaluated on general graphs", {
  directed <- ref_connected_graph(9, density = 0.35, seed = 53L, directed = TRUE)
  ring <- zoob_ring(9L, directed = TRUE)
  dimnames(ring) <- dimnames(directed)
  # A directed cycle laid over the random arcs makes the graph strongly connected.
  strong <- (directed + ring > 0) * 1
  graphs <- list(kite = zoob_kite(), `karate club` = zoob_karate(), `strongly connected directed 9 nodes` = strong)
  Map(\(graph, name) {
    expect_reference(centrality_mec(graph), ref_named(mec_definition(graph), graph), reference = independent,
                     covers = c("centrality()", "centrality_mec()"),
                     quantity = sprintf("mec, hitting probabilities, %s", name), tolerance = exact)
    expect_reference(centrality_tec(graph), ref_named(tec_definition(graph), graph), reference = independent,
                     covers = c("centrality()", "centrality_tec()"),
                     quantity = sprintf("tec, leading left eigenvector, %s", name), tolerance = exact)
  }, graphs, names(graphs))
})
