# Batch 58 --- five measures:
#   niem    Zhou, Zhang, Li, Hui and Wu (2012), Acta Physica Sinica
#           61(5):050201, eqs. (4)-(6)
#   iksd    Liu, Jiang, Wang and Yu (2015), Knowledge-Based Systems
#           84:56-66, section 2.2.5
#   density_centrality  Ibnoulouafi and El Haziti (2018), as given by
#           Ullah et al. (2021), Scientific Reports 11:6173, eq. (4)
#   wksdn   Maji (2020), Journal of Computational Science 39:101055
#   mu_pci  Papadimitriou, Katsaros and Manolopoulos (2010)
# Published values are compared in
# tests/equivalence/test-centrality-zoo-b-batch58.R.

b58_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b58_kite <- function() {
  b58_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

b58_star <- function(leaves) {
  b58_from_edges(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))
}

b58_path <- function(n) b58_from_edges(n, cbind(seq_len(n - 1L), 2:n))

b58_complete <- function(n) {
  a <- 1 - diag(n)
  dimnames(a) <- list(seq_len(n), seq_len(n))
  a
}

# --- niem ------------------------------------------------------------------

test_that("niem on a path of three, by hand", {
  # n = 3, <k> = 4/3. Efficiencies: ends (1 + 1/2)/3 = 1/2, middle 2/3.
  # End: 1/2 * (2 * 2/3) / (16/9) = 3/8. Middle: 2/3 * (1/2 + 1/2) / (16/9).
  expect_equal(unname(centrality_niem(b58_path(3L))), c(3 / 8, 3 / 8, 3 / 8))
})

test_that("niem on a complete graph is (n - 1)^2 / n^2", {
  # Efficiency (n-1)/n, degree n-1 = <k>: I * (n-1) * (n-1) I / (n-1)^2.
  lapply(c(4L, 7L), \(n) {
    expect_equal(unname(centrality_niem(b58_complete(n))),
                 rep(((n - 1) / n)^2, n))
  })
})

test_that("niem: unreachable pairs add nothing; no edges gives zeros", {
  two <- b58_from_edges(5L, rbind(c(1, 2), c(3, 4)))
  # n = 5, <k> = 4/5; every linked node has I = 1/5 and one neighbour.
  expect_equal(unname(centrality_niem(two)),
               c(rep((1 / 5) * (1 / 5) / (16 / 25), 4), 0))
  empty <- matrix(0, 3L, 3L, dimnames = list(1:3, 1:3))
  expect_identical(unname(centrality_niem(empty)), c(0, 0, 0))
})

# --- iksd ------------------------------------------------------------------

test_that("iksd removes only the lowest degree in each iteration", {
  # Path of five: ends (degree 1) first, then nodes 2 and 4 (now degree
  # 1), then the middle (degree 0).
  expect_equal(unname(centrality_iksd(b58_path(5L))), c(1, 2, 3, 2, 1))
  # Triangle with a pendant on node 3: pendant (1), then all three corners
  # share degree 2.
  paw <- b58_from_edges(4L, rbind(c(1, 2), c(2, 3), c(1, 3), c(3, 4)))
  expect_equal(unname(centrality_iksd(paw)), c(2, 2, 2, 1))
})

test_that("iksd differs from the onion decomposition and counts isolates", {
  # A path of four (3-4-2-5) beside a path of three (1-7-6). After the five
  # ends leave, node 7 has degree 0 and nodes 2 and 4 have degree 1: iksd
  # takes node 7 alone, the onion decomposition takes all three together.
  two_paths <- b58_from_edges(7L, rbind(c(2, 4), c(3, 4), c(2, 5), c(1, 7),
                                        c(6, 7)))
  expect_equal(unname(centrality_iksd(two_paths)), c(1, 3, 1, 3, 1, 1, 2))
  expect_equal(centrality(two_paths, measures = "onion")$onion,
               c(1, 2, 1, 2, 1, 1, 2))
  # The kite's tail: the pendant 10 goes first, then node 9, then node 8.
  expect_equal(unname(centrality_iksd(b58_kite())[c(10, 9, 8)]), c(1, 2, 3))
  padded <- rbind(cbind(b58_path(5L), 0), 0)
  dimnames(padded) <- list(1:6, 1:6)
  expect_equal(unname(centrality_iksd(padded)), c(2, 3, 4, 3, 2, 1))
})

# --- density centrality ----------------------------------------------------

test_that("density centrality on a star and a path, by hand", {
  # Star of four leaves. Hub: 4 nodes at distance 1 -> 4 * 4 / pi. Leaf:
  # the hub at 1 and three leaves at 2 -> (1 + 3/4) / pi.
  expect_equal(unname(centrality_density_centrality(b58_star(4L))),
               c(16 / pi, rep(1.75 / pi, 4)))
  path <- b58_path(5L)
  expect_equal(unname(centrality_density_centrality(path,
                                                    density_radius = 1)),
               unname(rowSums(path)^2 / pi))
  # End node, radius 2 against unlimited.
  expect_equal(unname(centrality_density_centrality(path,
                                                    density_radius = 2)[1]),
               (1 + 1 / 4) / pi)
  expect_equal(unname(centrality_density_centrality(path,
                                                    density_radius = Inf)[1]),
               (1 + 1 / 4 + 1 / 9 + 1 / 16) / pi)
})

test_that("density_radius outside its domain raises cograph_bad_parameter", {
  path <- b58_path(4L)
  expect_error(centrality_density_centrality(path, density_radius = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_density_centrality(path, density_radius = 2.5),
               class = "cograph_bad_parameter")
  expect_error(centrality_density_centrality(path, density_radius = NA),
               class = "cograph_bad_parameter")
})

# --- wksdn -----------------------------------------------------------------

test_that("wksdn on a star and a complete graph, by hand", {
  # Star of four leaves: every k-shell index is 1, lambda = 5 / 8.
  # Hub: 4 ties of (1 + 1) + 5/8 (4 + 1). Leaf: one such tie.
  tie <- 2 + (5 / 8) * 5
  expect_equal(unname(centrality_wksdn(b58_star(4L))), c(4 * tie, rep(tie, 4)))
  # K5: ks = k = 4, lambda = 1: each of 4 ties weighs 8 + 8.
  expect_equal(unname(centrality_wksdn(b58_complete(5L))), rep(64, 5))
})

test_that("wksdn equals its closed form on the kite", {
  kite <- b58_kite()
  k <- rowSums(kite)
  ks <- unname(centrality_coreness(kite))
  lambda <- sum(ks) / sum(k)
  expect_equal(unname(centrality_wksdn(kite)),
               unname(k * ks + as.numeric(kite %*% ks) +
                        lambda * (k^2 + as.numeric(kite %*% k))))
})

# --- mu-pci ----------------------------------------------------------------

test_that("mu_pci reproduces the four values printed for the paper's Fig. 4", {
  fig4 <- b58_from_edges(9L, rbind(c(9, 3), c(3, 7), c(3, 6), c(3, 4),
                                   c(4, 8), c(4, 6), c(4, 5), c(6, 5),
                                   c(6, 7), c(7, 1), c(7, 2)))
  pci <- centrality_mu_pci(fig4)
  expect_equal(unname(pci[c(7, 4, 6, 3)]), c(2, 2, 3, 3))
})

test_that("mu_pci at mu = 1 is lobby; larger mu widens the ball", {
  kite <- b58_kite()
  expect_equal(unname(centrality_mu_pci(kite)),
               unname(centrality_lobby(kite)))
  # Star of six leaves at mu = 2, from a leaf: six nodes within two steps
  # (hub of degree 6, five leaves of degree 1); k = 1 needs 2 of degree
  # >= 1 (yes), k = 2 needs 4 of degree >= 2 (no).
  expect_equal(unname(centrality_mu_pci(b58_star(6L), pci_mu = 2)),
               c(1, rep(1, 6)))
  expect_error(centrality_mu_pci(kite, pci_mu = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_mu_pci(kite, pci_mu = 1.5),
               class = "cograph_bad_parameter")
})

# --- shared ----------------------------------------------------------------

test_that("the five measures are equivariant, skeleton-based and registered", {
  set.seed(58)
  kite <- b58_kite()
  measures <- c("niem", "iksd", "density_centrality", "wksdn", "mu_pci")
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
  expect_identical(nrow(rows), 5L)
  expect_false(any(rows$costly))
  expect_false(any(rows$uses_weights))
})
