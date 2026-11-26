# Batch 66 --- thirteen measures built on 2026-11-26 from papers read in full:
#   ksh, eskh  Namtirtha, Dutta and Dutta (2018), Physica A 499:310-324,
#             eqs. (1)-(4)
#   ikh, eikh  Maji, Namtirtha, Dutta and Curado Malta (2020), Expert
#             Systems with Applications 144:113092, eqs. (16)-(19)
#   mcde, mcdwe, mcsde, mcsdwe  Sheikhahmadi and Nematbakhsh (2017),
#             Journal of Information Science 43(3):412-423, eqs. (8)-(14)
#   lgc       Zhang, Shuai and Lu (2022), Information Sciences 618:98-117,
#             eqs. (10)-(12)
#   wgravity  Liu, Wang and Deng (2020), Knowledge-Based Systems 193:105464,
#             eqs. (7)-(9)
#   asp_centrality  Lv, Zhao, Xiong and Chen (2019), Physica A 523:488-497,
#             eqs. (9)-(11)
#   wdc, ewdc  Liu, Wei, Du, Xiao and Deng (2016), Chaos, Solitons &
#             Fractals 86:1-7, eqs. (4)-(9)
# Published values are compared in
# tests/equivalence/test-centrality-zoo-b-batch66.R.

b66_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b66_path <- function(n) b66_from_edges(n, cbind(seq_len(n - 1L), 2:n))

b66_star <- function(leaves) {
  b66_from_edges(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))
}

b66_complete <- function(n) {
  a <- matrix(1, n, n, dimnames = list(seq_len(n), seq_len(n)))
  diag(a) <- 0
  a
}

# A triangle 1-2-3 with the tail 3-4-5-6: shells 2, 2, 2, 1, 1, 1.
b66_tailed_triangle <- function() {
  b66_from_edges(6L, rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5),
                           c(5, 6)))
}

b66_kite <- function() {
  b66_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

# A 4-clique and a triangle joined by a path, with a pendant: three shells
# and a mean distance above 4, so the gravity radii admit several steps.
b66_long <- function() {
  b66_from_edges(14L, matrix(c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 4, 5, 5, 6,
                               6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 11, 13,
                               12, 13, 13, 14), ncol = 2L, byrow = TRUE))
}

b66_measures <- c("ksh", "eskh", "ikh", "eikh", "mcde", "mcdwe", "mcsde",
                  "mcsdwe", "lgc", "wgravity", "asp_centrality", "wdc", "ewdc")

b66_all <- function(x) centrality(x, measures = b66_measures)

# --- ksh, eskh, ikh, eikh --------------------------------------------------

test_that("ksh and eskh follow equations (1)-(4) on a path of four", {
  # One shell, so every root is sqrt(1 + 1). Node 1 reaches 2 (degree 2) at
  # distance 1, 3 (degree 2) at 2 and 4 (degree 1) at 3; node 2 reaches 1
  # and 3 at distance 1 and 4 at 2.
  end <- (sqrt(2) + 0.8) * (1 + 1 / 4) + (sqrt(2) + 0.4) / 9
  middle <- (sqrt(2) + 0.4) + (sqrt(2) + 0.8) + (sqrt(2) + 0.4) / 4
  expect_equal(unname(centrality_ksh(b66_path(4L))),
               c(end, middle, middle, end))
  expect_equal(unname(centrality_eskh(b66_path(4L))),
               c(middle, end + middle, end + middle, middle))
})

test_that("ikh and eikh weight the levels by 1/2, 1/3 and 1/6 at radius 3", {
  # Level weights 2 (3 - l + 1) / 12; the shell term is sqrt(1 * 1) = 1.
  end <- (1 + 2 / 2) + (1 + 2 / 3) / 4 + (1 + 1 / 6) / 9
  middle <- (1 + 1 / 2) + (1 + 2 / 2) + (1 + 1 / 3) / 4
  expect_equal(unname(centrality_ikh(b66_path(4L))),
               c(end, middle, middle, end))
  expect_equal(unname(centrality_eikh(b66_path(4L))),
               c(middle, end + middle, end + middle, middle))
})

test_that("ksh adds the shells under the root and ikh multiplies them", {
  # Node 4 (shell 1) has the neighbors 3 (shell 2, degree 3) and 5 (shell 1,
  # degree 2). At radius 1 the level weight of ikh is 1.
  tail <- b66_tailed_triangle()
  expect_equal(centrality_ksh(tail, ksh_radius = 1)[["4"]],
               sqrt(3) + 0.4 * 3 + sqrt(2) + 0.4 * 2)
  expect_equal(centrality_ikh(tail, ksh_radius = 1)[["4"]],
               sqrt(2) + 3 + sqrt(1) + 2)
})

test_that("ksh with mu = 0 is the shell term alone", {
  # A complete network of four: every pair at distance 1 with shells 3.
  expect_equal(unname(centrality_ksh(b66_complete(4L), ksh_mu = 0)),
               rep(3 * sqrt(6), 4))
})

test_that("an infinite radius sums ksh over the whole component", {
  path <- b66_path(6L)
  expect_equal(centrality_ksh(path, ksh_radius = Inf),
               centrality_ksh(path, ksh_radius = 5))
})

test_that("the k-shell hybrid measures reject bad parameters", {
  path <- b66_path(4L)
  expect_error(centrality_ksh(path, ksh_mu = 1.5),
               class = "cograph_bad_parameter")
  expect_error(centrality_eskh(path, ksh_mu = NA_real_),
               class = "cograph_bad_parameter")
  expect_error(centrality_ksh(path, ksh_radius = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_ikh(path, ksh_radius = 1.5),
               class = "cograph_bad_parameter")
  expect_error(centrality_ikh(path, ksh_radius = Inf),
               class = "cograph_bad_parameter")
  expect_error(centrality_eikh(path, ksh_radius = Inf),
               class = "cograph_bad_parameter")
})

# --- mcde, mcdwe, mcsde, mcsdwe --------------------------------------------

test_that("mcde and mcdwe add core, degree and shell entropy", {
  # Node 3 has two neighbors in shell 2 and one in shell 1; node 4 one in
  # each. K = 2, so shell 2 has weight 1 and shell 1 weight 1/2.
  tail <- b66_tailed_triangle()
  entropy_3 <- log2(3) - 2 / 3
  weighted_3 <- (1 / 3) * log2(3) / 2 + (2 / 3) * log2(3 / 2)
  expect_equal(unname(centrality_mcde(tail)),
               c(4, 4, 2 + 3 + entropy_3, 1 + 2 + 1, 3, 2))
  expect_equal(unname(centrality_mcdwe(tail)),
               c(4, 4, 2 + 3 + weighted_3, 1 + 2 + 0.75, 3, 2))
})

test_that("mcsde and mcsdwe add the min-max scaled terms", {
  # Semi-local centrality 18, 18, 21, 18, 10, 7 (hand count of the sums of
  # two-step neighborhood sizes), scaled by (x - 7) / 14. Shells scale to
  # 1, 1, 1, 0, 0, 0; the entropies (0, 0, e3, 1, 0, 0) and the weighted
  # entropies (0, 0, w3, 3/4, 0, 0) to their maximum.
  tail <- b66_tailed_triangle()
  expect_equal(unname(centrality_semilocal(tail)), c(18, 18, 21, 18, 10, 7))
  local <- (c(18, 18, 21, 18, 10, 7) - 7) / 14
  entropy_3 <- log2(3) - 2 / 3
  weighted_3 <- (1 / 3) * log2(3) / 2 + (2 / 3) * log2(3 / 2)
  expect_equal(unname(centrality_mcsde(tail)),
               c(1, 1, 1, 0, 0, 0) + local + c(0, 0, entropy_3, 1, 0, 0))
  expect_equal(unname(centrality_mcsdwe(tail)),
               c(1, 1, 1, 0, 0, 0) + local +
                 c(0, 0, weighted_3 / 0.75, 1, 0, 0))
})

test_that("a constant term contributes nothing to mcsde", {
  # A path is one shell and every entropy is 0, so only the semi-local term
  # varies; on a complete network all three are constant.
  path <- b66_path(5L)
  local <- centrality_semilocal(path)
  expect_equal(centrality_mcsde(path),
               (local - min(local)) / (max(local) - min(local)))
  expect_equal(unname(centrality_mcsdwe(b66_complete(4L))), numeric(4))
})

# --- lgc, wgravity ---------------------------------------------------------

test_that("lgc is a one-step gravity model on a path of six", {
  # Mean distance 35/15, radius 7/6. Laplacian centralities
  # 6, 12, 14, 14, 12, 6 (k^2 + k + 2 x the neighbors' degrees).
  path <- b66_path(6L)
  expect_equal(unname(centrality_laplacian(path)), c(6, 12, 14, 14, 12, 6))
  expect_equal(unname(centrality_lgc(path)),
               c(72, 72 + 168, 168 + 196, 168 + 196, 72 + 168, 72))
})

test_that("lgc and wgravity are zero when the mean distance is below two", {
  for (graph in list(b66_kite(), b66_complete(4L))) {
    expect_equal(unname(centrality_lgc(graph)), numeric(nrow(graph)))
    expect_equal(unname(centrality_wgravity(graph)), numeric(nrow(graph)))
  }
})

test_that("wgravity weights the degree gravity by the unit eigenvector", {
  # The leading eigenvector of a path of six is sin(i pi / 7), of squared
  # length 7/2; the degree gravity at radius 1 is 2, 6, 8, 8, 6, 2.
  eigenvector <- sin(seq_len(6) * pi / 7) / sqrt(3.5)
  expect_equal(unname(centrality_wgravity(b66_path(6L))),
               eigenvector * c(2, 6, 8, 8, 6, 2))
})

test_that("wgravity is zero outside the component of largest spectral radius", {
  # A path of six and a separate edge: mean distance 72/32, radius 1.125.
  graph <- b66_from_edges(8L, rbind(cbind(1:5, 2:6), c(7, 8)))
  eigenvector <- sin(seq_len(6) * pi / 7) / sqrt(3.5)
  expect_equal(unname(centrality_wgravity(graph)),
               c(eigenvector * c(2, 6, 8, 8, 6, 2), 0, 0))
})

test_that("wgravity warns and gives NA when the eigenvector is not unique", {
  # Two separate paths of six share the largest spectral radius.
  twin <- b66_from_edges(12L, rbind(cbind(1:5, 2:6), cbind(7:11, 8:12)))
  expect_warning(score <- centrality_wgravity(twin),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
})

# --- asp_centrality --------------------------------------------------------

test_that("asp_centrality is the relative growth of the total distance", {
  # Path 1-2-3: ordered distance total 8, diameter 2. Cutting node 2 leaves
  # every pair at 2 (total 12); cutting node 1 leaves 2-3 at 1 (total 10).
  expect_equal(unname(centrality_asp_centrality(b66_path(3L))),
               c(0.25, 0.5, 0.25))
  # Star of three leaves: total 18. Cutting the center: 24; a leaf: 20.
  expect_equal(unname(centrality_asp_centrality(b66_star(3L))),
               c(1 / 3, 1 / 9, 1 / 9, 1 / 9))
})

test_that("asp_centrality charges unreachable input pairs the diameter", {
  # A triangle and an isolate: D = 1, so every pair is 1 before and after.
  graph <- b66_from_edges(4L, rbind(c(1, 2), c(1, 3), c(2, 3)))
  expect_equal(unname(centrality_asp_centrality(graph)), numeric(4))
  expect_equal(unname(centrality_asp_centrality(b66_complete(5L))),
               numeric(5))
})

test_that("asp_centrality mirrors closeness vitality without cut vertices", {
  # A six-cycle with the chord 1-4 is 2-connected: 7 pairs at distance 1,
  # 6 at 2 and 2 at 3, so W = 2 (7 + 12 + 6) = 50 and D = 3. Cutting node 1
  # leaves the path 2-...-6 (pair total 20) and 5 pairs at 3: AC = 0.4.
  # Cutting node 2 leaves pairs totalling 16 and 5 pairs at 3: AC = 0.24.
  graph <- b66_from_edges(6L, rbind(cbind(1:6, c(2:6, 1)), c(1, 4)))
  expect_equal(unname(centrality_asp_centrality(graph)),
               c(0.4, 0.24, 0.24, 0.4, 0.24, 0.24))
  # AC(k) = (2 (N - 1) D - CV(k)) / W, CV over ordered pairs.
  expect_equal(centrality_asp_centrality(graph),
               (2 * 5 * 3 - centrality_closeness_vitality(graph)) / 50)
})

# --- wdc, ewdc -------------------------------------------------------------

test_that("wdc and ewdc follow equations (4)-(7) on a star", {
  # A star is perfectly disassortative, r = -1, so alpha = 1.
  # Wdc: center (3 - 3) * 3 = 0, leaf (3 - 1) * 1 = 2.
  # EWdc: center (6 - 0) * 0 = 0, leaf (0 - 2) * 2 = -4.
  expect_equal(unname(centrality_wdc(b66_star(3L))), c(0, 2, 2, 2))
  expect_equal(unname(centrality_ewdc(b66_star(3L))), c(0, -4, -4, -4))
  # With alpha = 0, 0^0 = 1: the center keeps the sum of its leaves.
  expect_equal(unname(centrality_ewdc(b66_star(3L), wdc_alpha = 0)),
               c(6, -2, -2, -2))
})

test_that("wdc uses the absolute assortativity of a path", {
  # The degrees at the two ends of the edges of a path of four correlate at
  # r = -1/2, so alpha = 1/2.
  expect_equal(unname(centrality_wdc(b66_path(4L))),
               c(1, sqrt(2), sqrt(2), 1))
  expect_equal(unname(centrality_ewdc(b66_path(4L))),
               c(sqrt(2) - 1, 2^0.25, 2^0.25, sqrt(2) - 1))
})

test_that("wdc takes alpha = 0 when the assortativity is undefined", {
  # A five-cycle is regular: r is 0/0, alpha is 0.
  cycle <- b66_from_edges(5L, cbind(1:5, c(2:5, 1)))
  expect_equal(unname(centrality_wdc(cycle)), rep(2, 5))
  expect_equal(unname(centrality_ewdc(cycle)), rep(2, 5))
})

test_that("wdc with alpha = 0 is con", {
  kite <- b66_kite()
  expect_equal(centrality_wdc(kite, wdc_alpha = 0), centrality_con(kite))
})

test_that("wdc and ewdc reject a bad alpha", {
  expect_error(centrality_wdc(b66_path(4L), wdc_alpha = -1),
               class = "cograph_bad_parameter")
  expect_error(centrality_ewdc(b66_path(4L), wdc_alpha = c(1, 2)),
               class = "cograph_bad_parameter")
  expect_error(centrality_wdc(b66_path(4L), wdc_alpha = Inf),
               class = "cograph_bad_parameter")
})

# --- shared properties -----------------------------------------------------

test_that("the batch 66 measures follow a relabeling of the nodes", {
  set.seed(66)
  for (graph in list(b66_kite(), b66_long())) {
    shuffle <- sample(nrow(graph))
    original <- b66_all(graph)
    shuffled <- b66_all(graph[shuffle, shuffle])
    for (measure in b66_measures) {
      expect_equal(shuffled[[measure]], original[[measure]][shuffle],
                   info = measure)
    }
  }
})

test_that("the batch 66 measures ignore weights, direction and loops", {
  long <- b66_long()
  set.seed(661)
  noisy <- long * matrix(stats::runif(196, 1, 5), 14, 14)
  noisy[lower.tri(noisy)] <- 0
  diag(noisy) <- 2
  expect_equal(b66_all(noisy), b66_all(long))
})

test_that("isolated nodes score zero", {
  with_isolate <- b66_from_edges(6L, rbind(cbind(1:4, 2:5)))
  scores <- b66_all(with_isolate)
  for (measure in b66_measures) {
    expect_equal(scores[[measure]][6L], 0, info = measure)
    expect_true(all(is.finite(scores[[measure]])), info = measure)
  }
})

test_that("a one-node network scores zero on every measure", {
  single <- matrix(0, 1, 1, dimnames = list("a", "a"))
  scores <- b66_all(single)
  for (measure in b66_measures) expect_equal(scores[[measure]], 0,
                                             info = measure)
})

test_that("normalized scores have a maximum of one", {
  scores <- centrality(b66_long(), measures = b66_measures, normalized = TRUE)
  for (measure in b66_measures) {
    expect_equal(max(scores[[measure]]), 1, info = measure)
  }
})

test_that("the batch 66 measures are in the default set", {
  expect_true(all(b66_measures %in% list_centralities()$measure))
  expect_false(any(b66_measures %in% cograph:::.cg_costly_measures()))
  expect_false(any(list_centralities()$uses_weights[
    list_centralities()$measure %in% b66_measures]))
})
