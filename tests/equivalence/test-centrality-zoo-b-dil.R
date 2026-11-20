# Degree and importance of lines, centrality_dil() (Liu, Xiong, Shi, Shi and Wang 2016, Physica A 452:209-219),
# against the values the paper prints (Fig. 2 on p. 211, Table 3 on p. 217) and against computations written out
# from equations (1)-(3), pp. 210-211: a line e_ij carries U = (k_i - p - 1)(k_j - p - 1), p the number of
# triangles on the line, and importance I = U / lambda with lambda = p / 2 + 1; node i receives the share
# (k_i - 1) / (k_i + k_j - 2) of each of its lines' importance, and L_i = k_i + sum_j I_ij (k_i - 1) / (k_i + k_j - 2).
# No other package implements this measure.

independent <- "independent: closed forms and hand derivations from the definition"
figure_2 <- "published: Liu, Xiong, Shi, Shi and Wang 2016, Fig. 2, p. 211"
table_3 <- "published: Liu, Xiong, Shi, Shi and Wang 2016, Table 3, p. 217"
# Rational arithmetic: agreement to rounding.
exact <- 1e-10
# Table 3 prints four decimals: half a unit in the last printed place, absolute.
printed_4dp <- 5e-5

# Fig. 2, p. 211: the 27-node tree.
dil_figure_2 <- function() {
  zoob_graph(27L, c(1, 2, 1, 3, 1, 17, 1, 18, 1, 19, 1, 20, 1, 21, 1, 22, 1, 23, 2, 27, 3, 4, 3, 24, 3, 26, 4, 5,
                    4, 25, 5, 6, 6, 7, 6, 8, 6, 9, 6, 10, 8, 11, 8, 12, 8, 13, 9, 14, 10, 15, 10, 16))
}

# Fig. 6, p. 217: the ARPA network, 21 nodes and 26 lines (Table 3's degree column agrees with it).
dil_arpa <- function() {
  zoob_graph(21L, c(1, 2, 1, 15, 2, 3, 2, 15, 2, 16, 3, 4, 3, 17, 3, 18, 4, 5, 5, 6, 6, 7, 6, 21, 7, 8, 8, 9, 9, 10,
                    10, 11, 11, 12, 12, 13, 12, 19, 13, 14, 14, 15, 14, 16, 14, 17, 18, 19, 19, 20, 20, 21))
}

# Equations (1)-(3) on a simple undirected graph. The share's denominator k_i + k_j - 2 is zero only on an isolated
# edge, where U is zero too; the share is then taken as zero.
dil_definition <- function(a) {
  a <- unname(a)
  n <- nrow(a)
  degree <- rowSums(a)
  triangles <- a %*% a
  own <- matrix(degree, n, n)
  other <- matrix(degree, n, n, byrow = TRUE)
  importance <- a * (own - triangles - 1) * (other - triangles - 1) / (triangles / 2 + 1)
  denominator <- own + other - 2
  share <- ifelse(a > 0 & denominator > 0, (own - 1) / pmax(denominator, 1), 0)
  degree + rowSums(importance * share)
}

test_that("dil reproduces the two node scores printed for Fig. 2", {
  # The paper prints L_v2 = 26/9 and L_v5 = 52/15 as exact fractions, so the comparison is to rounding.
  expect_reference(centrality_dil(dil_figure_2())[c("2", "5")], c(`2` = 26 / 9, `5` = 52 / 15), reference = figure_2,
                   covers = c("centrality()", "centrality_dil()"), quantity = "dil, Fig. 2 tree", tolerance = exact)
})

test_that("dil reproduces Table 3's DIL column for the ARPA network", {
  printed <- c(`3` = 15.25, `14` = 14.35, `2` = 11.55, `12` = 7.6667, `19` = 7.6667, `6` = 7, `15` = 5.9333,
               `16` = 3.5, `17` = 3.5, `13` = 3.4167, `18` = 3.4167, `4` = 3.25, `5` = 3.1667, `7` = 3.1667,
               `11` = 3.1667, `20` = 3.1667, `21` = 3.1667, `8` = 3, `9` = 3, `10` = 3, `1` = 2)
  expect_reference(centrality_dil(dil_arpa()), printed, reference = table_3,
                   covers = c("centrality()", "centrality_dil()"), quantity = "dil, ARPA network",
                   tolerance = printed_4dp)
})

test_that("dil follows the closed forms of complete graphs, stars, rings, paths and bipartite graphs", {
  # K_n: every line has p = n - 2, so U = 0 and L = k = n - 1.
  lapply(c(2L, 5L, 9L), \(n) {
    expect_reference(centrality_dil(zoob_complete(n)), rep(n - 1, n), reference = independent,
                     covers = c("centrality()", "centrality_dil()"), quantity = sprintf("dil, complete graph n = %d", n),
                     tolerance = exact)
  })
  # Star: every line touches a leaf, whose factor k - p - 1 is 0, so L = k.
  lapply(c(1L, 4L, 8L), \(leaves) {
    expect_reference(centrality_dil(zoob_star(leaves)), c(leaves, rep(1, leaves)), reference = independent,
                     covers = c("centrality()", "centrality_dil()"), quantity = sprintf("dil, star of %d", leaves),
                     tolerance = exact)
  })
  # Triangle-free k-regular: I = (k - 1)^2 on every line, split evenly, L = k + k (k - 1)^2 / 2: 3 on a ring
  # (k = 2), 9 on the Petersen graph (k = 3). C_3 = K_3 is not triangle-free and scores its degree, 2.
  lapply(c(4L, 7L, 12L), \(n) {
    expect_reference(centrality_dil(zoob_ring(n)), rep(2 + 2 * 1^2 / 2, n), reference = independent,
                     covers = c("centrality()", "centrality_dil()"), quantity = sprintf("dil, ring n = %d", n),
                     tolerance = exact)
  })
  petersen <- zoob_graph(10L, c(1, 2, 2, 3, 3, 4, 4, 5, 5, 1, 6, 8, 8, 10, 10, 7, 7, 9, 9, 6, 1, 6, 2, 7, 3, 8, 4, 9,
                                5, 10))
  expect_reference(centrality_dil(petersen), rep(3 + 3 * 2^2 / 2, 10), reference = independent,
                   covers = c("centrality()", "centrality_dil()"), quantity = "dil, Petersen graph", tolerance = exact)
  expect_reference(centrality_dil(zoob_ring(3)), rep(2, 3), reference = independent,
                   covers = c("centrality()", "centrality_dil()"), quantity = "dil, ring n = 3", tolerance = exact)
  # Path of 5: lines to an end carry U = 0; the two inner lines carry I = 1 each, split evenly.
  expect_reference(centrality_dil(zoob_path(5)), c(1, 2 + 1 / 2, 2 + 1 / 2 + 1 / 2, 2 + 1 / 2, 1),
                   reference = independent, covers = c("centrality()", "centrality_dil()"), quantity = "dil, path of 5",
                   tolerance = exact)
  # K(a, b): triangle-free, I = (a - 1)(b - 1) on every line; an a-side node scores b + b I (b - 1) / (a + b - 2).
  lapply(list(c(2L, 3L), c(3L, 4L), c(4L, 5L)), \(sides) {
    a <- sides[[1L]]
    b <- sides[[2L]]
    bipartite <- zoob_graph(a + b, cbind(rep(seq_len(a), each = b), a + rep(seq_len(b), a)))
    importance <- (a - 1) * (b - 1)
    expect_reference(centrality_dil(bipartite),
                     c(rep(b + b * importance * (b - 1) / (a + b - 2), a),
                       rep(a + a * importance * (a - 1) / (a + b - 2), b)),
                     reference = independent, covers = c("centrality()", "centrality_dil()"),
                     quantity = sprintf("dil, complete bipartite K(%d, %d)", a, b), tolerance = exact)
  })
})

test_that("dil reads lambda as p / 2 + 1 on the triangle with two pendants", {
  # Line 1-2 has p = 1 and U = (3 - 1 - 1)(3 - 1 - 1) = 1, so I = 1 / 1.5, split evenly between the two corners,
  # which score 3 + (1 / 1.5) / 2. (Reading lambda as 2p + 1 would give 19 / 6.)
  trap <- zoob_graph(5L, c(1, 2, 1, 3, 2, 3, 1, 4, 2, 5))
  expect_reference(centrality_dil(trap)[c("1", "2")], c(`1` = 3 + (1 / 1.5) / 2, `2` = 3 + (1 / 1.5) / 2),
                   reference = independent, covers = c("centrality()", "centrality_dil()"),
                   quantity = "dil, triangle with two pendants, corners", tolerance = exact)
})

test_that("dil equals equations (1)-(3) evaluated on general graphs", {
  graphs <- list(kite = zoob_kite(), ARPA = dil_arpa(), `karate club` = zoob_karate(),
                 `random 12 nodes` = ref_graph(12, 0.35, seed = 5L))
  Map(\(graph, name) {
    expect_reference(centrality_dil(graph), dil_definition(graph), reference = independent,
                     covers = c("centrality()", "centrality_dil()"), quantity = sprintf("dil, definition, %s", name),
                     tolerance = exact, measure = "scaled")
  }, graphs, names(graphs))
})
