# Batch 55 --- four measures:
#   con     common out-neighbor score, Bonato, Eikmeier, Gleich and Malik (2019)
#   emi     entropy and mutual information-based centrality, Li, Cai, Li and
#           Du (2020), Entropy 22(1):52, eqs. (22), (24), (25)
#   nwrank  Wang, Duenas-Osorio and Padgett (2015), Sci. Rep. 5:15141,
#           eqs. (5)-(7)
#   biased_random_walk  Takes and Kosters (2011), BNAIC
# Comparisons with outside references and published values are in
# tests/equivalence/test-centrality-zoo-b-batch55.R.

b55_from_edges <- function(n, el, directed = FALSE) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  if (!directed) a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b55_kite <- function() {
  b55_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

b55_star <- function(leaves) {
  b55_from_edges(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))
}

b55_quiet <- function(expr) {
  withCallingHandlers(expr, cograph_undefined_measure = \(w) {
    invokeRestart("muffleWarning")
  })
}

# --- con -------------------------------------------------------------------

test_that("con counts shared out-neighbours with the minimum multiplicity", {
  # 1 -> 3 twice, 1 -> 4 once; 2 -> 3 once, 2 -> 4 three times.
  # CON(1,2) = min(2,1) + min(1,3) = 2; nodes 3 and 4 send nothing.
  a <- matrix(0, 4L, 4L, dimnames = list(1:4, 1:4))
  a[1, 3] <- 2
  a[1, 4] <- 1
  a[2, 3] <- 1
  a[2, 4] <- 3
  expect_equal(unname(centrality_con(a)), c(2, 2, 0, 0))
  # Unweighted, each arc counts once: one shared target each way... two.
  expect_equal(unname(centrality_con(a, weighted = FALSE)), c(2, 2, 0, 0))
  a[2, 4] <- 0
  expect_equal(unname(centrality_con(a)), c(1, 1, 0, 0))
})

test_that("con on an undirected graph is the neighbours' degrees minus one", {
  kite <- b55_kite()
  degree <- rowSums(kite)
  expect_equal(unname(centrality_con(kite)),
               unname(as.numeric(kite %*% (degree - 1))))
  # A star: each leaf shares the hub with the other leaves; the hub shares
  # no target with anyone.
  expect_equal(unname(centrality_con(b55_star(5L))), c(0, rep(4, 5)))
})

test_that("con ignores loops and is permutation equivariant", {
  set.seed(551)
  kite <- b55_kite()
  looped <- kite
  diag(looped) <- 1
  expect_equal(centrality_con(looped), centrality_con(kite))
  perm <- sample.int(10L)
  expect_equal(unname(centrality_con(kite[perm, perm])),
               unname(centrality_con(kite))[perm])
})

# --- emi -------------------------------------------------------------------

test_that("emi reproduces the mutual information of the paper's Fig. 3", {
  # Fig. 3 with neighbour sets a:{b,c,d}, b:{a,c}, c:{a,b}, d:{a}. The paper
  # prints MI = -0.03876, -0.05062, -0.05062, 0.
  fig3 <- b55_from_edges(4L, rbind(c(1, 2), c(2, 3), c(3, 1), c(1, 4)))
  terms <- cograph:::.cg_emi_terms(fig3)
  # Five printed decimals: half a unit in the last place, absolute.
  expect_true(all(abs(terms$mutual - c(-0.03876, -0.05062, -0.05062, 0)) <
                    5e-6))
  # Node a by hand: shares 2/5, 2/5, 1/5 give S = 0.45815; b: 3/5, 2/5.
  hand_s <- c(-(2 * 0.4 * log10(0.4) + 0.2 * log10(0.2)),
              -(0.6 * log10(0.6) + 0.4 * log10(0.4)))
  expect_equal(terms$entropy[1:2], hand_s)
  expect_equal(unname(centrality_emi(fig3)), terms$entropy + terms$mutual)
})

test_that("emi: no triangles means no mutual information; isolates score 0", {
  ring <- b55_from_edges(6L, cbind(1:6, c(2:6, 1L)))
  terms <- cograph:::.cg_emi_terms(ring)
  expect_equal(terms$mutual, rep(0, 6))
  # Two neighbours of equal degree: S = log10(2).
  expect_equal(unname(centrality_emi(ring)), rep(log10(2), 6))
  lonely <- rbind(cbind(ring, 0), 0)
  dimnames(lonely) <- list(1:7, 1:7)
  expect_identical(unname(centrality_emi(lonely)[7]), 0)
  expect_true(all(cograph:::.cg_emi_terms(b55_kite())$mutual <= 0))
})

# --- nwrank ----------------------------------------------------------------

test_that("nwrank is the principal eigenvector of W H built from eq. (7)", {
  kite <- b55_kite()
  n <- nrow(kite)
  links <- which(kite != 0 & upper.tri(kite), arr.ind = TRUE)
  m <- nrow(links)
  degree <- rowSums(kite)
  between <- cograph:::.cg_edge_betweenness(kite, n, FALSE, edges = links)
  w <- matrix(0, n, m)
  w[cbind(links[, 1], seq_len(m))] <- 1
  w[cbind(links[, 2], seq_len(m))] <- 1
  share <- function(alpha) {
    h <- matrix(0, m, n)
    lapply(seq_len(n), \(j) {
      at <- which(links[, 1] == j | links[, 2] == j)
      far <- ifelse(links[at, 1] == j, links[at, 2], links[at, 1])
      h[at, j] <<- alpha * degree[far] / sum(degree[far]) +
        (1 - alpha) * between[at] / sum(between[at])
    })
    vec <- Re(eigen(w %*% h)$vectors[, 1L])
    vec / sum(vec)
  }
  lapply(c(0, 0.5, 1), \(alpha) {
    expect_equal(unname(centrality_nwrank(kite, nwrank_alpha = alpha)),
                 share(alpha), tolerance = 1e-10)
  })
  expect_equal(sum(centrality_nwrank(kite)), 1)
})

test_that("nwrank on a regular vertex-transitive graph is uniform", {
  ring <- b55_from_edges(7L, cbind(1:7, c(2:7, 1L)))
  expect_equal(unname(centrality_nwrank(ring)), rep(1 / 7, 7))
})

test_that("nwrank: isolates score 0, several components give NA", {
  kite <- b55_kite()
  with_isolate <- rbind(cbind(kite, 0), 0)
  dimnames(with_isolate) <- list(1:11, 1:11)
  out <- centrality_nwrank(with_isolate)
  expect_identical(unname(out[11]), 0)
  expect_equal(unname(out[1:10]), unname(centrality_nwrank(kite)))
  two <- b55_from_edges(4L, rbind(c(1, 2), c(3, 4)))
  expect_warning(out <- centrality_nwrank(two),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  empty <- matrix(0, 3L, 3L, dimnames = list(1:3, 1:3))
  expect_warning(out <- centrality_nwrank(empty),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
})

test_that("nwrank_alpha outside [0, 1] raises cograph_bad_parameter", {
  expect_error(centrality_nwrank(b55_kite(), nwrank_alpha = 1.2),
               class = "cograph_bad_parameter")
  expect_error(centrality_nwrank(b55_kite(), nwrank_alpha = NA_real_),
               class = "cograph_bad_parameter")
})

# --- biased random walk ----------------------------------------------------

test_that("the biased walk's stationary vector is fixed by the chain", {
  kite <- b55_kite()
  n <- nrow(kite)
  nb <- lapply(seq_len(n), \(v) which(kite[v, ] > 0))
  degree <- lengths(nb)
  f_nd <- vapply(seq_len(n), \(v) {
    1 - sum(vapply(nb[[v]], \(w) {
      if (degree[w] == 1L) return(0)
      length(intersect(nb[[w]], nb[[v]])) / ((degree[w] - 1) * degree[v])
    }, numeric(1)))
  }, numeric(1))
  weight <- 0.5 * (1 - 1 / degree) + 0.5 * f_nd
  chain <- t(vapply(seq_len(n), \(v) {
    row <- numeric(n)
    row[nb[[v]]] <- weight[nb[[v]]] / sum(weight[nb[[v]]])
    0.85 * row + 0.15 / n
  }, numeric(n)))
  got <- unname(centrality_biased_random_walk(kite))
  expect_equal(as.numeric(got %*% chain), got, tolerance = 1e-12)
  expect_equal(sum(got), 1)
  # f_nd of the kite's node 4 (six neighbours): 1 - 11/12.
  expect_equal(f_nd[4], 1 / 12)
})

test_that("the biased walk is uniform on a ring and at jump = 1", {
  ring <- b55_from_edges(8L, cbind(1:8, c(2:8, 1L)))
  expect_equal(unname(centrality_biased_random_walk(ring)), rep(1 / 8, 8))
  expect_equal(unname(centrality_biased_random_walk(b55_kite(),
                                                    brw_jump = 1)),
               rep(1 / 10, 10))
})

test_that("the biased walk handles zero-weight rows and isolates", {
  # Triangle at alpha = 0: every f_nd is 0, so every move weight is 0 and
  # the walker picks a neighbour uniformly.
  triangle <- b55_from_edges(3L, rbind(c(1, 2), c(2, 3), c(1, 3)))
  expect_equal(unname(centrality_biased_random_walk(triangle,
                                                    brw_alpha = 0)),
               rep(1 / 3, 3))
  with_isolate <- rbind(cbind(triangle, 0), 0)
  dimnames(with_isolate) <- list(1:4, 1:4)
  out <- centrality_biased_random_walk(with_isolate)
  expect_equal(sum(out), 1)
  expect_true(all(out > 0))
  expect_lt(out[4], out[1])
})

test_that("brw parameters outside their domain raise cograph_bad_parameter", {
  kite <- b55_kite()
  expect_error(centrality_biased_random_walk(kite, brw_alpha = -0.1),
               class = "cograph_bad_parameter")
  expect_error(centrality_biased_random_walk(kite, brw_jump = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_biased_random_walk(kite, brw_jump = 1.5),
               class = "cograph_bad_parameter")
})

# --- shared ----------------------------------------------------------------

test_that("emi, nwrank and the biased walk ignore direction and weights", {
  kite <- b55_kite()
  arcs <- kite
  arcs[lower.tri(arcs)] <- 0
  lapply(c("emi", "nwrank", "biased_random_walk"), \(measure) {
    base <- centrality(kite, measures = measure)[[measure]]
    expect_equal(centrality(arcs, measures = measure)[[measure]], base)
    expect_equal(centrality(kite * 2.5, measures = measure)[[measure]], base)
  })
})

test_that("the four measures are registered", {
  listed <- list_centralities()
  rows <- listed[listed$measure %in% c("con", "emi", "nwrank",
                                       "biased_random_walk"), ]
  expect_identical(nrow(rows), 4L)
  expect_false(any(rows$costly))
  expect_identical(rows$uses_weights[rows$measure == "con"], TRUE)
  out <- centrality(b55_kite(), measures = c("con", "emi", "nwrank",
                                             "biased_random_walk"))
  expect_identical(names(out), c("node", "con", "emi", "nwrank",
                                 "biased_random_walk"))
  expect_false(anyNA(out))
})
