# Batch 68 --- nine measures built on 2026-11-26 from papers read in full:
#   physarum_spreader  Wang, Zhang, Zhang, Mahadevan and Deng (2015), PLoS ONE
#             10(12):e0145028, eqs. (7)-(12), `physarum_steps` required
#   articlerank  Li and Willett (2009), Aslib Proceedings 61(6):605-618,
#             eq. (2) with the mean reference count as a multiplier
#   entropy_gravity  Yan, Cui and Ni (2020), Chinese Physics B 29(4):048902,
#             eqs. (2)-(6) read by the paper's prose, r = 3
#   influence_capability  Wang, Du, Fan and Xing (2017), Neurocomputing
#             260:466-477, eqs. (1)-(7)
#   ehc       Zareie and Sheikhahmadi (2019), Physica A 514:141-155,
#             eqs. (19)-(24), p = 0.8, r = 100
#   erm       Zareie, Sheikhahmadi and Fatemi (2017), Chaos, Solitons and
#             Fractals 104:485-494, eqs. (2)-(7), natural log
#   shell_clustering, ecrm  Zareie, Sheikhahmadi, Jalili and Fasaei (2020),
#             Knowledge-Based Systems 194:105580, eqs. (2)-(6)
#   weight_neighborhood  Wang, Hou, Li and Ding (2017), Physica A
#             475:88-105, eqs. (9)-(10)
# Published values of the public functions are compared in
# tests/equivalence/test-centrality-zoo-b-batch68.R; printed intermediate
# values (internal steps) are checked here.

b68_from_edges <- function(n, el, directed = FALSE) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  el <- matrix(el, ncol = 2L, byrow = !is.matrix(el))
  a[el] <- 1
  if (!directed) a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b68_path <- function(n) b68_from_edges(n, cbind(seq_len(n - 1L), 2:n))
b68_cycle <- function(n) b68_from_edges(n, cbind(seq_len(n), c(2:n, 1L)))

# A triangle 1-2-3 with the pendant 1-4.
b68_paw <- function() b68_from_edges(4L, c(1, 2, 1, 3, 2, 3, 1, 4))

b68_kite <- function() {
  b68_from_edges(10L, c(1, 2, 1, 3, 1, 4, 1, 6, 2, 4, 2, 5, 2, 7, 3, 4, 3, 6,
                        4, 5, 4, 6, 4, 7, 5, 7, 6, 7, 6, 8, 7, 8, 8, 9, 9, 10))
}

# A 4-clique and a triangle joined by a path, with a pendant.
b68_long <- function() {
  b68_from_edges(14L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 4, 5, 5, 6, 6, 7,
                        7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 11, 13, 12, 13,
                        13, 14))
}

# Fig. 1 / Fig. 3 of Wang et al. (2015): 1->2 (2), 2->3, 3->1, 4->1 (2), 4->2 (2).
b68_physarum_net <- function() {
  a <- b68_from_edges(4L, c(1, 2, 2, 3, 3, 1, 4, 1, 4, 2), directed = TRUE)
  a[1, 2] <- a[4, 1] <- a[4, 2] <- 2
  a
}

b68_measures <- c("physarum_spreader", "articlerank", "entropy_gravity",
                  "influence_capability", "ehc", "erm", "shell_clustering",
                  "ecrm", "weight_neighborhood")
b68_structural <- setdiff(b68_measures, c("physarum_spreader", "articlerank"))

b68_all <- function(x, measures = b68_measures, ...) {
  centrality(x, measures = measures, physarum_steps = 5, ...)
}

b68_entropy <- function(p) -sum(ifelse(p > 0, p * log(p), 0))

# --- physarum_spreader -----------------------------------------------------

test_that("physarum_spreader follows equations (7)-(12) on the paper's network", {
  # w_ig: node 1 has one out-arc of weight 2, so 2; nodes 2 and 3 have 1;
  # node 4 two arcs of weight 2, so 2. Q(0) rows: node 1 (to 2, g) 1/2 1/2;
  # node 2 (to 3, g) 1/2 1/2; node 3 (to 1, g) 1/2 1/2; node 4 (to 1, 2, g)
  # 1/3 each; ground 1/4 each (the printed Q(0)). I(0) = (1, 1, 1, 1, 0):
  # I(1) = (1/2 + 1/3, 1/2 + 1/3, 1/2, 0, 3/2 + 1/3), S = I + I_g / 4.
  net <- b68_physarum_net()
  ground <- 11 / 6
  expect_equal(unname(centrality_physarum_spreader(net, physarum_steps = 1)),
               c(5 / 6, 5 / 6, 1 / 2, 0) + ground / 4)
  # D(1) = (Q(0) + 1) / 2 on the arcs between nodes: 3/4 on 1->2, 2->3, 3->1
  # and 2/3 on node 4's arcs (the printed D(1)). Q(1): nodes 1-3 send 3/7 on
  # their arc and 4/7 to g; node 4 sends 2/7, 2/7 and 3/7 to g.
  # I(2) = (3/7 x 1/2 + 11/24, 3/7 x 5/6 + 11/24, 3/7 x 5/6 + 11/24, 11/24)
  # and I_g(2) = 4/7 (5/6 + 5/6 + 1/2) = 26/21.
  i2 <- c(3 / 14, 5 / 14, 5 / 14, 0) + 11 / 24
  expect_equal(unname(centrality_physarum_spreader(net, physarum_steps = 2)),
               i2 + 26 / 21 / 4)
})

test_that("physarum_spreader conserves the resource and flattens towards 1", {
  # Every flux row sums to one, so I_1 + ... + I_N + I_g = N and the scores
  # add up to N after any number of updates.
  kite <- b68_kite()
  for (t in c(1, 7, 52)) {
    expect_equal(sum(centrality_physarum_spreader(kite, physarum_steps = t)),
                 10, info = t)
  }
  late <- centrality_physarum_spreader(kite, physarum_steps = 5000)
  early <- centrality_physarum_spreader(kite, physarum_steps = 10)
  expect_lt(diff(range(late)), diff(range(early)) / 100)
})

test_that("physarum_spreader reads direction and weights", {
  # In the paper's network every node's out-arcs weigh the same, and w_ig is
  # their mean, so its weights do not change the shares; node 4's do here.
  net <- b68_physarum_net()
  net[4, 1] <- 5
  binary <- (net > 0) * 1
  expect_false(isTRUE(all.equal(
    centrality_physarum_spreader(net, physarum_steps = 3),
    centrality_physarum_spreader(binary, physarum_steps = 3))))
  expect_equal(centrality_physarum_spreader(net, physarum_steps = 3,
                                            weighted = FALSE),
               centrality_physarum_spreader(binary, physarum_steps = 3))
  expect_false(isTRUE(all.equal(
    centrality_physarum_spreader(binary, physarum_steps = 3),
    centrality_physarum_spreader(t(binary), physarum_steps = 3))))
})

test_that("physarum_spreader needs physarum_steps", {
  expect_warning(score <- centrality_physarum_spreader(b68_path(4L)),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
  for (bad in list(0, 1.5, -2, c(1, 2), "3")) {
    expect_error(centrality_physarum_spreader(b68_path(4L), physarum_steps = bad),
                 class = "cograph_bad_parameter")
  }
  negative <- b68_path(3L)
  negative[1, 2] <- negative[2, 1] <- -1
  expect_error(centrality_physarum_spreader(negative, physarum_steps = 2),
               class = "cograph_negative_weights")
})

# --- articlerank -----------------------------------------------------------

test_that("articlerank follows equation (2) on a small citation network", {
  # B, C and D cite A, D cites B; NR = out-degree = (0, 1, 1, 2), NRbar = 1.
  # AR(C) = AR(D) = 0.15; AR(B) = 0.15 + 0.85 x 1 x 0.15 / (1 + 2) = 0.1925;
  # AR(A) = 0.15 + 0.85 (0.1925 / 2 + 0.15 / 2 + 0.15 / 3) = 0.3380625.
  citations <- b68_from_edges(4L, c(2, 1, 3, 1, 4, 1, 4, 2), directed = TRUE)
  expect_equal(unname(centrality_articlerank(citations)),
               c(0.3380625, 0.1925, 0.15, 0.15))
  # With NR = (12, 30, 8, 45), NRbar = 23.75: AR(B) = 0.15 + 0.85 x 23.75 x
  # 0.15 / 68.75; AR(A) = 0.15 + 0.85 x 23.75 (AR(B) / 53.75 + 0.15 / 31.75
  # + 0.15 / 68.75).
  ar_b <- 0.15 + 0.85 * 23.75 * 0.15 / 68.75
  expect_equal(unname(centrality_articlerank(
    citations, ar_references = c(12, 30, 8, 45))),
    c(0.15 + 0.85 * 23.75 * (ar_b / 53.75 + 0.15 / 31.75 + 0.15 / 68.75),
      ar_b, 0.15, 0.15))
})

test_that("articlerank keeps the NRbar multiplier on a cyclic network", {
  # Undirected path 1-2-3-4: NR = (1, 2, 2, 1), NRbar = 1.5, radius 0.65.
  # By symmetry AR = (a, b, b, a) with a = 0.15 + 1.275 b / 3.5 and
  # b = 0.15 + 1.275 (a / 2.5 + b / 3.5).
  c1 <- 1.275 / 3.5
  c2 <- 1.275 / 2.5
  b <- 0.15 * (1 + c2) / (1 - c1 - c2 * c1)
  a <- 0.15 + c1 * b
  expect_equal(unname(centrality_articlerank(b68_path(4L))), c(a, b, b, a))
  expect_equal(a, 0.333386, tolerance = 1e-6)
})

test_that("articlerank warns and gives NA where the iteration diverges", {
  # Complete network of four: every NR is 3, the map is 0.85 x 3 / 6 times
  # the adjacency matrix, radius 0.85 x 3 x 3 / 6 = 1.275.
  complete <- 1 - diag(4)
  dimnames(complete) <- list(1:4, 1:4)
  expect_warning(score <- centrality_articlerank(complete),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
  # A smaller damping brings the radius to 0.75 and the score back.
  expect_equal(unname(centrality_articlerank(complete, damping = 0.5)),
               rep(0.5 / (1 - 0.75), 4))
})

test_that("articlerank is not taken over by another package's qr() for arrays", {
  # brainGraph registers an S3 qr() method for arrays, which a matrix also is;
  # the verify run loads it and qr.solve() then failed.
  skip_if_not_installed("brainGraph")
  loadNamespace("brainGraph")
  citations <- b68_from_edges(4L, c(2, 1, 3, 1, 4, 1, 4, 2), directed = TRUE)
  expect_equal(unname(centrality_articlerank(citations)),
               c(0.3380625, 0.1925, 0.15, 0.15))
})

test_that("articlerank matches named references and rejects bad ones", {
  citations <- b68_from_edges(4L, c(2, 1, 3, 1, 4, 1, 4, 2), directed = TRUE)
  expect_equal(centrality_articlerank(citations,
                                      ar_references = c(`4` = 45, `1` = 12,
                                                        `3` = 8, `2` = 30)),
               centrality_articlerank(citations,
                                      ar_references = c(12, 30, 8, 45)))
  for (bad in list(c(1, 2, 3), c(1, -1, 2, 3), c(1, NA, 2, 3),
                   c(a = 1, b = 2, c = 3, d = 4), c(0, 0, 0, 0))) {
    expect_error(centrality_articlerank(citations, ar_references = bad),
                 class = "cograph_bad_parameter")
  }
})

# --- entropy_gravity -------------------------------------------------------

test_that("entropy_gravity follows the prose reading on the paw", {
  # Node H-index (2, 2, 2, 1) -> b = (1, 1, 1, 0); closeness (1/3, 1/4, 1/4,
  # 1/5) -> (1, 3/8, 3/8, 0); betweenness (2, 0, 0, 0) -> (1, 0, 0, 0), a
  # single nonzero value, so its shares are (2, 1, 1, 1) / 5. PageRank from
  # its linear system. Entropies over ln 4, weights (1 - S) / (4 - sum S),
  # mass C = b w, and every pair is within r = 3.
  paw <- b68_paw()
  degree <- rowSums(paw)
  pagerank <- solve(diag(4) - 0.85 * t(paw / degree), rep(0.15 / 4, 4))
  scaled <- cbind(c(1, 1, 1, 0), c(1, 3 / 8, 3 / 8, 0), c(1, 0, 0, 0),
                  (pagerank - min(pagerank)) / diff(range(pagerank)))
  shares <- list(scaled[, 1] / 3, scaled[, 2] / sum(scaled[, 2]),
                 c(2, 1, 1, 1) / 5, scaled[, 4] / sum(scaled[, 4]))
  entropy <- vapply(shares, b68_entropy, numeric(1)) / log(4)
  mass <- as.numeric(scaled %*% ((1 - entropy) / (4 - sum(entropy))))
  distance <- matrix(c(0, 1, 1, 1, 1, 0, 1, 2, 1, 1, 0, 2, 1, 2, 2, 0), 4)
  pull <- ifelse(distance > 0, 1 / distance^2, 0)
  expect_equal(unname(centrality_entropy_gravity(paw)),
               mass * as.numeric(pull %*% mass), tolerance = 1e-12)
  # At radius 1 only the neighbors count.
  expect_equal(unname(centrality_entropy_gravity(paw, entropy_gravity_radius = 1)),
               mass * as.numeric((distance == 1) %*% mass), tolerance = 1e-12)
})

test_that("entropy_gravity takes the degree share as PageRank at damping 1", {
  # On a connected undirected network the walk's stationary distribution is
  # the degree share; the paw's PageRank column then scales to
  # (3 - 1, 2 - 1, 2 - 1, 0) / 2 = (1, 1/2, 1/2, 0). The path of five is
  # bipartite, where power iteration at damping 1 does not settle.
  paw <- b68_paw()
  scaled <- cbind(c(1, 1, 1, 0), c(1, 3 / 8, 3 / 8, 0), c(1, 0, 0, 0),
                  c(1, 1 / 2, 1 / 2, 0))
  shares <- list(scaled[, 1] / 3, scaled[, 2] / sum(scaled[, 2]),
                 c(2, 1, 1, 1) / 5, scaled[, 4] / 2)
  entropy <- vapply(shares, b68_entropy, numeric(1)) / log(4)
  mass <- as.numeric(scaled %*% ((1 - entropy) / (4 - sum(entropy))))
  distance <- matrix(c(0, 1, 1, 1, 1, 0, 1, 2, 1, 1, 0, 2, 1, 2, 2, 0), 4)
  expect_equal(unname(centrality_entropy_gravity(paw, damping = 1)),
               mass * as.numeric(ifelse(distance > 0, 1 / distance^2, 0) %*% mass))
  expect_true(all(is.finite(centrality_entropy_gravity(b68_path(5L), damping = 1))))
})

test_that("entropy_gravity warns and gives NA off its domain", {
  # A cycle: every attribute is the same at every node.
  expect_warning(score <- centrality_entropy_gravity(b68_cycle(6L)),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
  disconnected <- b68_from_edges(5L, c(1, 2, 2, 3, 4, 5))
  expect_warning(score <- centrality_entropy_gravity(disconnected),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
  expect_error(centrality_entropy_gravity(b68_kite(), entropy_gravity_radius = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_entropy_gravity(b68_kite(), entropy_gravity_radius = 1.5),
               class = "cograph_bad_parameter")
})

test_that("entropy_gravity reads damping and the radius", {
  kite <- b68_kite()
  expect_false(isTRUE(all.equal(centrality_entropy_gravity(kite),
                                centrality_entropy_gravity(kite, damping = 0.5))))
  # The kite's diameter is 4, so radius 4 and Inf agree. Node 10 is lowest
  # on every attribute (mass 0) and is in every pair at distance 4, so
  # radius 3 agrees as well; radius 2 drops pairs of positive mass.
  expect_equal(centrality_entropy_gravity(kite, entropy_gravity_radius = Inf),
               centrality_entropy_gravity(kite, entropy_gravity_radius = 4))
  expect_equal(centrality_entropy_gravity(kite),
               centrality_entropy_gravity(kite, entropy_gravity_radius = 4))
  expect_false(isTRUE(all.equal(
    centrality_entropy_gravity(kite),
    centrality_entropy_gravity(kite, entropy_gravity_radius = 2))))
})

# --- influence_capability --------------------------------------------------

b68_ic_fig1 <- function() {
  b68_from_edges(17L, c(1, 6, 5, 6, 6, 4, 4, 3, 4, 2, 6, 7, 7, 8, 7, 10, 8, 9,
                        8, 10, 10, 11, 10, 12, 10, 13, 10, 14, 11, 13, 11, 12,
                        12, 13, 13, 15, 12, 15, 15, 16, 16, 17))
}

test_that("influence_capability reproduces the worked example of Fig. 1", {
  # Printed: Iter(9) = Iter(17) = Iter(5) = Iter(1) = 1, Iter(16) = Iter(4)
  # = 2; D(Iter 1) = 0.5, D(Iter 2) = 0.5729; ICP(8) = 2.5, ICP(15) =
  # 2.5729, ICP = 1 at every leaf; ICN(1, 2, 9, 14, 17) = 8, 6, 10, 18, 4.
  fig1 <- unname(b68_ic_fig1())
  iteration <- cograph:::.cg_ic_iteration(fig1)
  expect_equal(iteration[c(9, 17, 5, 1, 16, 4)], c(1, 1, 1, 1, 2, 2))
  expect_equal(2 / pi * atan(c(1, 2)^(1 / 3)), c(0.5, 0.5729), tolerance = 5e-5)
  parts <- cograph:::.cg_ic_parts(fig1)
  expect_equal(parts$icp[c(8, 15)], c(2.5, 2.5729), tolerance = 5e-5)
  expect_equal(unique(parts$icp[rowSums(fig1) == 1]), 1)
  expect_equal(parts$icn[c(1, 2, 9, 14, 17)], c(8, 6, 10, 18, 4))
})

test_that("influence_capability follows equations (1)-(7) on a path of three", {
  # Iteration 1 removes the leaves, iteration 2 the middle node, whose
  # neighbors were both removed earlier: ICP = (1, 2 D(1), 1) = (1, 1, 1),
  # so H_ICP = 1 and w_ICP = 0. ICN = A A d = (2, 4, 2), so IC = ICN / 8.
  expect_equal(unname(centrality_influence_capability(b68_path(3L))),
               c(0.25, 0.5, 0.25))
  # Both attributes constant on a cycle: weights 1/2, IC = 1/n everywhere.
  expect_equal(unname(centrality_influence_capability(b68_cycle(5L))),
               rep(0.2, 5))
})

test_that("influence_capability weights the normalised attributes", {
  # IC = w1 ICP / sum ICP + w2 ICN / sum ICN with w = (1 - H) / (2 - sum H).
  kite <- b68_kite()
  parts <- cograph:::.cg_ic_parts(unname(kite))
  entropy <- vapply(list(parts$icp, parts$icn),
                    \(x) b68_entropy(x / sum(x)) / log(10), numeric(1))
  expect_equal(parts$weight, (1 - entropy) / (2 - sum(entropy)))
  expect_equal(sum(parts$weight), 1)
  expect_equal(unname(centrality_influence_capability(kite)),
               parts$weight[1] * parts$icp / sum(parts$icp) +
                 parts$weight[2] * parts$icn / sum(parts$icn))
})

test_that("influence_capability warns and gives NA without edges", {
  expect_warning(score <- centrality_influence_capability(
    matrix(0, 3, 3, dimnames = list(1:3, 1:3))),
    class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
})

# --- ehc -------------------------------------------------------------------

b68_ehc_fig1 <- function() {
  b68_from_edges(17L, c(1, 4, 1, 5, 1, 6, 1, 7, 1, 9, 9, 8, 9, 16, 9, 14, 9, 10,
                        9, 3, 8, 2, 8, 16, 8, 14, 16, 2, 16, 14, 2, 14, 2, 15, 2,
                        17, 14, 15, 14, 3, 14, 13, 3, 10, 3, 13, 3, 11, 3, 12))
}

test_that("ehc reproduces the printed CMC values of the worked example", {
  # Fig. 1 with p = 0.5 and r = 10: h = 7, S(v1) = (5, 1, 1, 1, 1, 1, 0),
  # S(v3) = (6, 4, 2, 2, 2, 2, 1) and the printed CMC of nodes 1, 3, 4-7, 9-14.
  fig1 <- unname(b68_ehc_fig1())
  degree <- rowSums(fig1)
  expect_equal(max(degree), 7)
  cumulative <- fig1 %*% outer(degree, 1:7, ">=")
  expect_equal(cumulative[1, ], c(5, 1, 1, 1, 1, 1, 0))
  expect_equal(cumulative[3, ], c(6, 4, 2, 2, 2, 2, 1))
  cmc <- cograph:::.cg_ehc_cmc(fig1, 0.5, 10)
  printed <- c(`1` = 4.594, `3` = 8.581, `4` = 2.256, `5` = 2.256, `6` = 2.256,
               `7` = 2.256, `9` = 12.592, `10` = 5.324, `11` = 2.662,
               `12` = 2.662, `13` = 5.716)
  expect_equal(cmc[as.integer(names(printed))], unname(printed), tolerance = 5e-4)
  # Node 14: exactly 13.14949, which the paper prints as 13.150.
  expect_equal(cmc[14], 13.150, tolerance = 6e-4)
})

test_that("ehc follows equations (23)-(24) on a star", {
  # Star with center 1 and three leaves, h = 3. The center's neighbors have
  # degree 1, so c = (3, 0, 0); each leaf's neighbor has degree 3, so
  # c = (1, 1, 1). EHC(center) = 3 CMC(leaf), EHC(leaf) = CMC(center).
  star <- b68_from_edges(4L, c(1, 2, 1, 3, 1, 4))
  coef <- \(p, r) p^(1 + (1:3) * p / r)
  for (s in list(c(0.8, 100), c(0.5, 10), c(1, 3))) {
    co <- coef(s[1], s[2])
    expect_equal(unname(centrality_ehc(star, ehc_p = s[1], ehc_r = s[2])),
                 c(3 * sum(co), rep(3 * co[1], 3)), info = toString(s))
  }
})

test_that("ehc at p = 1 is the two-step sum of degrees", {
  kite <- b68_kite()
  expect_equal(unname(centrality_ehc(kite, ehc_p = 1)),
               as.numeric(kite %*% kite %*% rowSums(kite)))
  for (bad in list(c(ehc_p = 0), c(ehc_p = 1.2), c(ehc_r = 0), c(ehc_r = Inf))) {
    expect_error(do.call(centrality_ehc, c(list(kite), as.list(bad))),
                 class = "cograph_bad_parameter")
  }
})

# --- erm -------------------------------------------------------------------

test_that("erm follows equations (2)-(7) on the paw", {
  # Degrees (3, 2, 2, 1); d1 = (5, 5, 5, 3); d2 = (13, 10, 10, 5).
  # E1: node 1 shares (2, 2, 1) / 5, nodes 2-3 (3, 2) / 5, node 4 one share.
  # E2: node 1 (5, 5, 3) / 13, nodes 2-3 (5, 5) / 10, node 4 one share.
  # lambda = d2 / 13. ERM = A A EC.
  e1 <- c(b68_entropy(c(2, 2, 1) / 5), b68_entropy(c(3, 2) / 5),
          b68_entropy(c(3, 2) / 5), 0)
  e2 <- c(b68_entropy(c(5, 5, 3) / 13), log(2), log(2), 0)
  ec <- e1 + c(13, 10, 10, 5) / 13 * e2
  paw <- b68_paw()
  expect_equal(unname(centrality_erm(paw)), as.numeric(paw %*% paw %*% ec))
  # Node 1: three walks back to itself, one each to 2 and 3.
  expect_equal(centrality_erm(paw)[["1"]], 3 * ec[1] + ec[2] + ec[3])
})

test_that("erm gives a leaf EC = 0", {
  # Star: every leaf has one neighbor (both entropies 0); the center sees
  # four equal shares in each, lambda = 1: EC(center) = 2 log 4. The center's
  # two-step walks return to it four times; a leaf's reach the leaves only.
  star <- b68_from_edges(5L, c(1, 2, 1, 3, 1, 4, 1, 5))
  expect_equal(unname(centrality_erm(star)), c(4 * 2 * log(4), 0, 0, 0, 0))
  # A path of three: E1(center) = E2(center) = log 2 (two equal shares
  # each), lambda = 1, so EC(center) = 2 log 2 and the leaves have EC 0. The
  # center's two walks return to it: 4 log 2; a leaf's end at the leaves.
  expect_equal(unname(centrality_erm(b68_path(3L))), c(0, 2 * 2 * log(2), 0))
  # Path of four: the leaf 1 reaches node 3 (EC > 0) in two steps.
  expect_gt(centrality_erm(b68_path(4L))[["1"]], 0)
})

# --- shell_clustering and ecrm ---------------------------------------------

b68_scc_fig1 <- function() {
  b68_from_edges(17L, c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 1, 5, 1, 6, 5, 6, 5,
                        7, 6, 7, 7, 8, 7, 9, 8, 9, 8, 16, 9, 10, 9, 11, 11, 15,
                        16, 17, 12, 16, 12, 13, 10, 12, 10, 14))
}

test_that("shell_clustering reproduces the hierarchies and shell vectors of Fig. 1", {
  # Printed: IT of nodes 1-17, f = 7, SV8, SV9, SV7 and C(8, 9) about 0.75.
  fig1 <- unname(b68_scc_fig1())
  expect_equal(cograph:::.cg_iksd(fig1),
               c(7, 7, 7, 7, 6, 6, 5, 4, 4, 3, 2, 3, 1, 1, 1, 3, 1))
  shell <- cograph:::.cg_shell_vectors(fig1)
  expect_equal(ncol(shell), 7L)
  expect_equal(shell[8, ], c(0, 0, 1, 1, 1, 0, 0))
  expect_equal(shell[9, ], c(0, 1, 1, 1, 1, 0, 0))
  expect_equal(shell[7, ], c(0, 0, 0, 2, 0, 2, 0))
  expect_equal(cograph:::.cg_shell_correlation(shell)[8, 9],
               stats::cor(shell[8, ], shell[9, ]))
  expect_equal(cograph:::.cg_shell_correlation(shell)[8, 9], 0.75)
})

test_that("shell_clustering and ecrm follow equations (4)-(6) on a path of four", {
  # Round 1 removes the leaves 1 and 4, round 2 nodes 2 and 3 (f = 2).
  # SV = (0, 1), (1, 1), (1, 1), (0, 1): the middle vectors are constant,
  # so every C is 0. max d = 2: SCC_1 = (2 - 0) + (2 x 2 / 2 + 1) = 5,
  # SCC_2 = [2 + (2 x 1 / 2 + 1)] + [2 + (2 x 2 / 2 + 1)] = 9.
  # CRM = A SCC = (9, 14, 14, 9); ECRM = A CRM = (14, 23, 23, 14).
  path <- b68_path(4L)
  expect_equal(unname(centrality_shell_clustering(path)), c(5, 9, 9, 5))
  expect_equal(unname(centrality_ecrm(path)), c(14, 23, 23, 14))
})

test_that("shell_clustering uses the correlation on the paw", {
  # Round 1 removes node 4, round 2 the triangle (f = 2). SV = (1, 2),
  # (0, 2), (0, 2), (0, 1): every pair rises together, so every C is 1.
  # max d = 3: SCC_1 = [1 + (4/3 + 1)] x 2 + [1 + (2/3 + 1)] = 28/3,
  # SCC_2 = [1 + (2 + 1)] + [1 + (4/3 + 1)] = 22/3, SCC_4 = 1 + 3 = 4.
  expect_equal(unname(centrality_shell_clustering(b68_paw())),
               c(28 / 3, 22 / 3, 22 / 3, 4))
})

test_that("a constant shell vector counts as zero correlation", {
  # On a cycle every node leaves in round 1 (f = 1): C = 0/0 -> 0, so every
  # node scores 2 x [(2 - 0) + (2 + 1)] = 10.
  expect_equal(unname(centrality_shell_clustering(b68_cycle(5L))), rep(10, 5))
  expect_equal(unname(centrality_ecrm(b68_cycle(5L))), rep(40, 5))
})

# --- weight_neighborhood ---------------------------------------------------

test_that("weight_neighborhood follows equations (9)-(10) on the paw", {
  # Edges 1-2 and 1-3 weigh 6, 2-3 weighs 4, 1-4 weighs 3: <w> = 19/4.
  # C(k): node 1 = 3 + (6 x 2 + 6 x 2 + 3 x 1) / <w>, node 2 = 2 + (6 x 3 +
  # 4 x 2) / <w>, node 4 = 1 + 3 x 3 / <w>. Coreness (2, 2, 2, 1).
  paw <- b68_paw()
  w <- 19 / 4
  expect_equal(unname(centrality_weight_neighborhood(paw)),
               c(3 + 27 / w, 2 + 26 / w, 2 + 26 / w, 1 + 9 / w))
  expect_equal(unname(centrality_weight_neighborhood(paw,
                                                     wnc_benchmark = "coreness")),
               c(2 + 27 / w, 2 + 20 / w, 2 + 20 / w, 1 + 6 / w))
  # alpha = 1/2: w = sqrt(6), sqrt(6), 2, sqrt(3).
  root <- c(sqrt(6), sqrt(6), 2, sqrt(3))
  m <- mean(root)
  expect_equal(unname(centrality_weight_neighborhood(paw, wnc_alpha = 0.5)),
               c(3 + (2 * sqrt(6) * 2 + sqrt(3)) / m,
                 2 + (3 * sqrt(6) + 2 * 2) / m, 2 + (3 * sqrt(6) + 2 * 2) / m,
                 1 + 3 * sqrt(3) / m))
})

test_that("weight_neighborhood rejects bad parameters", {
  expect_error(centrality_weight_neighborhood(b68_paw(), wnc_benchmark = "kshell"),
               class = "cograph_bad_parameter")
  expect_error(centrality_weight_neighborhood(b68_paw(), wnc_alpha = Inf),
               class = "cograph_bad_parameter")
  expect_error(centrality_weight_neighborhood(b68_paw(), wnc_alpha = c(1, 2)),
               class = "cograph_bad_parameter")
})

# --- shared properties -----------------------------------------------------

test_that("the batch 68 measures follow a relabeling of the nodes", {
  set.seed(68)
  for (graph in list(b68_kite(), b68_long())) {
    shuffle <- sample(nrow(graph))
    # A DAG version keeps articlerank defined.
    dag <- graph
    dag[lower.tri(dag)] <- 0
    original <- b68_all(graph, setdiff(b68_measures, "articlerank"))
    shuffled <- b68_all(graph[shuffle, shuffle],
                        setdiff(b68_measures, "articlerank"))
    for (measure in setdiff(b68_measures, "articlerank")) {
      expect_equal(shuffled[[measure]], original[[measure]][shuffle],
                   info = measure)
    }
    expect_equal(centrality_articlerank(dag[shuffle, shuffle]),
                 centrality_articlerank(dag)[shuffle])
  }
})

test_that("the structural measures ignore weights, direction and loops", {
  long <- b68_long()
  set.seed(681)
  noisy <- long * matrix(stats::runif(196, 1, 5), 14, 14)
  noisy[lower.tri(noisy)] <- 0
  diag(noisy) <- 2
  expect_equal(b68_all(noisy, b68_structural), b68_all(long, b68_structural))
})

test_that("articlerank reads direction but not weights", {
  citations <- b68_from_edges(4L, c(2, 1, 3, 1, 4, 1, 4, 2), directed = TRUE)
  weighted <- citations * c(1, 3, 5, 7)
  expect_equal(centrality_articlerank(weighted), centrality_articlerank(citations))
  expect_false(isTRUE(all.equal(centrality_articlerank(t(citations)),
                                centrality_articlerank(citations))))
})

test_that("isolated nodes score zero where the sums are empty", {
  # A triangle with a pendant, and the isolate 5.
  graph <- b68_from_edges(5L, c(1, 2, 2, 3, 1, 3, 3, 4))
  scores <- b68_all(graph, setdiff(b68_measures, "entropy_gravity"))
  for (measure in c("influence_capability", "ehc", "erm", "shell_clustering",
                    "ecrm", "weight_neighborhood")) {
    expect_equal(scores[[measure]][5L], 0, info = measure)
    expect_true(all(is.finite(scores[[measure]])), info = measure)
  }
  # Nobody cites the isolate: 1 - d. It trades resource with the ground node
  # only, so its PhysarumSpreader score is not 0.
  expect_equal(scores$articlerank[5L], 0.15)
  expect_true(scores$physarum_spreader[5L] > 0)
  expect_equal(sum(scores$physarum_spreader), 5)
})

test_that("a one-node network scores or warns and gives NA", {
  single <- matrix(0, 1, 1, dimnames = list("a", "a"))
  defined <- c("physarum_spreader", "articlerank", "ehc", "erm",
               "shell_clustering", "ecrm", "weight_neighborhood")
  scores <- b68_all(single, defined)
  expect_equal(unlist(scores[defined], use.names = FALSE),
               c(1, 0.15, 0, 0, 0, 0, 0))
  for (measure in c("entropy_gravity", "influence_capability")) {
    expect_warning(value <- b68_all(single, measure)[[measure]],
                   class = "cograph_undefined_measure")
    expect_true(is.na(value), info = measure)
  }
})

test_that("normalized scores have a maximum of one", {
  long <- b68_long()
  dag <- long
  dag[lower.tri(dag)] <- 0
  scores <- b68_all(long, setdiff(b68_measures, "articlerank"),
                    normalized = TRUE)
  for (measure in setdiff(b68_measures, "articlerank")) {
    expect_equal(max(scores[[measure]]), 1, info = measure)
  }
  expect_equal(max(centrality(dag, measures = "articlerank",
                              normalized = TRUE)$articlerank), 1)
})

test_that("the batch 68 measures are in the default set", {
  table <- list_centralities()
  expect_true(all(b68_measures %in% table$measure))
  expect_false(any(b68_measures %in% cograph:::.cg_costly_measures()))
  expect_equal(table$uses_weights[match(b68_measures, table$measure)],
               b68_measures == "physarum_spreader")
})
