# Batch 57 --- four measures:
#   inf     Huang, Chen, Wang and Ren (2020), Entropy 22(4):450, eq. (15)
#   gin     Zhao, Wang and Deng (2020), Chaos, Solitons and Fractals
#           133:109637, as reproduced in Entropy 24(2):275, eq. (8)
#   icentr  Mussone, Viseh and Notari (2022), Physica A 589:126595,
#           eqs. (9)-(13)
#   gli     Yang, Hu and Huang (2020), Chinese Physics B 29(8):088903,
#           eqs. (4)-(5)
# Published values are compared in
# tests/equivalence/test-centrality-zoo-b-batch57.R.

b57_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b57_kite <- function() {
  b57_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

b57_star <- function(leaves) {
  b57_from_edges(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))
}

b57_path <- function(n) b57_from_edges(n, cbind(seq_len(n - 1L), 2:n))

# --- gin -------------------------------------------------------------------

test_that("gin matches hand computations on the kite and a path", {
  # Kite pendant (node 10): neighbour of degree 2 at distance 1, degree 3
  # at 2, two of degree 5 at 3, and the remaining 20 degrees at 4.
  expect_equal(unname(centrality_gin(b57_kite())[10]),
               exp(1 / 10) * (2 + 3 / 2 + 10 / 3 + 20 / 4))
  # Path 1-2-3: ends exp(1/3) (2 + 1/2), middle exp(2/3) (1 + 1).
  expect_equal(unname(centrality_gin(b57_path(3L))),
               c(exp(1 / 3) * 2.5, exp(2 / 3) * 2, exp(1 / 3) * 2.5))
})

test_that("gin: alpha = 0 drops the own-degree factor; isolates score 0", {
  kite <- b57_kite()
  hops <- cograph:::.cg_hop_matrix(kite > 0, 10L)
  diag(hops) <- Inf
  expect_equal(unname(centrality_gin(kite, gin_alpha = 0)),
               as.numeric((1 / hops) %*% rowSums(kite)))
  padded <- rbind(cbind(kite, 0), 0)
  dimnames(padded) <- list(1:11, 1:11)
  expect_identical(unname(centrality_gin(padded)[11]), 0)
  expect_error(centrality_gin(kite, gin_alpha = -1),
               class = "cograph_bad_parameter")
})

# --- icentr ----------------------------------------------------------------

test_that("icentr reproduces the paper's Figure 1 and simple closed forms", {
  fig1 <- b57_from_edges(5L, rbind(c(1, 2), c(1, 3), c(1, 5), c(2, 3),
                                   c(3, 4), c(4, 5)))
  expect_equal(unname(centrality_icentr(fig1)),
               c(4.5, 3.75, 4.5, 3.75, 3.75))
  # Star: from the hub all edges are at level 1; from a leaf one edge is at
  # level 1 and the others at level 2.
  expect_equal(unname(centrality_icentr(b57_star(4L))),
               c(4, rep(1 + 3 / 2, 4)))
  # Triangle: two edges at the node, one inside level 1 (level 2).
  triangle <- b57_from_edges(3L, rbind(c(1, 2), c(2, 3), c(1, 3)))
  expect_equal(unname(centrality_icentr(triangle)), rep(2.5, 3))
})

test_that("icentr stays inside the node's component", {
  kite <- b57_kite()
  joined <- rbind(cbind(kite, matrix(0, 10L, 3L)),
                  cbind(matrix(0, 3L, 10L), 1 - diag(3L)))
  dimnames(joined) <- list(1:13, 1:13)
  out <- centrality_icentr(joined)
  expect_equal(unname(out[1:10]), unname(centrality_icentr(kite)))
  expect_equal(unname(out[11:13]), rep(2.5, 3))
})

# --- gli -------------------------------------------------------------------

test_that("the improved k-shell reproduces Table 1 of the paper", {
  # Figure 1 of Yang, Hu and Huang (2020). The edge from node 4 into the
  # core is drawn ambiguously (4-5 or 4-8); both readings give Table 1.
  base <- rbind(c(1, 2), c(2, 3), c(3, 4), c(3, 5), c(5, 6), c(5, 7),
                c(5, 8), c(6, 7), c(6, 8), c(7, 8), c(5, 9), c(9, 10),
                c(9, 11), c(10, 11))
  printed <- data.frame(shell = c(1, 1, 2, 2, 3, 3, 3, 3, 2, 2, 2),
                        round = c(1, 2, 3, 3, 5, 5, 5, 5, 4, 3, 3),
                        iks = c(2, 3, 5, 5, 8, 8, 8, 8, 6, 5, 5))
  lapply(list(c(4, 5), c(4, 8)), \(extra) {
    terms <- cograph:::.cg_gli_terms(b57_from_edges(11L, rbind(base, extra)))
    expect_equal(terms[, c("shell", "round", "iks")], printed)
  })
})

test_that("gli on a star, by hand", {
  # Three leaves: one round removes the leaves (round 1), the hub goes in
  # round 2. Mass: hub 1 + 2 + 3 = 6, leaf 1 + 1 + 1 = 3, total 15.
  expect_equal(unname(centrality_gli(b57_star(3L))),
               c(exp(6 / 15) * 9, rep(exp(3 / 15) * (6 + 6 / 2), 3)))
})

test_that("gli_radius limits the ball; isolates score 0", {
  path <- b57_path(5L)
  near <- centrality_gli(path, gli_radius = 1)
  wide <- centrality_gli(path, gli_radius = 4)
  expect_true(all(wide > near))
  expect_equal(centrality_gli(path, gli_radius = 4),
               centrality_gli(path, gli_radius = 9))
  padded <- rbind(cbind(path, 0), 0)
  dimnames(padded) <- list(1:6, 1:6)
  terms <- cograph:::.cg_gli_terms(padded)
  expect_identical(terms$gli[6], 0)
  expect_identical(terms$iks[6], 0)
  expect_equal(terms$round[1:5], cograph:::.cg_gli_terms(path)$round)
  expect_error(centrality_gli(path, gli_radius = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_gli(path, gli_radius = 1.5),
               class = "cograph_bad_parameter")
})

# --- shared ----------------------------------------------------------------

test_that("the three measures are equivariant, skeleton-based and registered", {
  set.seed(57)
  kite <- b57_kite()
  measures <- c("gin", "icentr", "gli")
  base <- centrality(kite, measures = measures)
  perm <- sample.int(10L)
  moved <- centrality(kite[perm, perm], measures = measures)
  arcs <- kite
  arcs[lower.tri(arcs)] <- 0
  one_way <- centrality(arcs, measures = measures)
  heavy <- centrality(kite * 4, measures = measures)
  lapply(measures, \(m) {
    expect_equal(moved[[m]], base[[m]][perm])
    expect_equal(one_way[[m]], base[[m]])
    expect_equal(heavy[[m]], base[[m]])
  })
  listed <- list_centralities()
  rows <- listed[listed$measure %in% measures, ]
  expect_identical(nrow(rows), 3L)
  expect_false(any(rows$costly))
  expect_false(any(rows$uses_weights))
  empty <- matrix(0, 3L, 3L, dimnames = list(1:3, 1:3))
  expect_equal(unlist(centrality(empty, measures = measures)[1, -1],
                      use.names = FALSE), rep(0, 3))
})
