# Batch 65 --- fifteen measures built on 2026-11-26 from papers read in full:
#   dsc, dsr, edsr  Zareie, Sheikhahmadi and Jalili (2019), Future
#             Generation Computer Systems 94:120-129, eqs. (21)-(26)
#   ksgc      Yang and Xiao (2021), Knowledge-Based Systems 227:107198,
#             eqs. (7)-(10)
#   lsc       Gao, Ma, Chen, Wang and Xing (2014), Physica A 403:130-147,
#             eqs. (5)-(6)
#   classified_neighbors  Li, Wang, Sun and Xia (2018), Applied Mathematics
#             and Computation 320:512-523, eq. (1)
#   hybrid_degree  Ma and Ma (2017), Physica A 465:312-330, section 3
#   topsis    Du, Gao, Hu, Mahadevan and Deng (2014), Physica A 399:57-69,
#             eqs. (6)-(12)
#   all_around  Hou, Yao and Liao (2012), Physica A 391(15):4012-4017,
#             eq. (1)
#   egm       Li and Xiao (2021), Information Sciences 578:725-749,
#             eqs. (16)-(22)
#   gls       Sheng et al. (2020), Physica A 541:123262
#   ks_if     Wang, Zhao, Xi and Du (2016), Physica A 461:171-181,
#             eqs. (1)-(2)
#   nl        Shao, Liu, Zhao and Liu (2019), Peer-to-Peer Networking and
#             Applications 12:1528-1537, eq. (5)
#   entropy_influence  Peng, Yang, Cao, Yu and Xie (2017), Information
#             Sciences 379:146-159, eqs. (2)-(13)
#   multi_evidence  Mo and Deng (2019), Physica A 529:121538
# Published values are compared in
# tests/equivalence/test-centrality-zoo-b-batch65.R.

b65_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b65_path <- function(n) b65_from_edges(n, cbind(seq_len(n - 1L), 2:n))

b65_star <- function(leaves) {
  b65_from_edges(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))
}

b65_complete <- function(n) {
  a <- matrix(1, n, n, dimnames = list(seq_len(n), seq_len(n)))
  diag(a) <- 0
  a
}

# A triangle 1-2-3 with the tail 3-4-5-6.
b65_tailed_triangle <- function() {
  b65_from_edges(6L, rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5),
                           c(5, 6)))
}

b65_kite <- function() {
  b65_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

b65_measures <- c("dsc", "dsr", "edsr", "ksgc", "lsc", "classified_neighbors",
                  "hybrid_degree", "topsis", "all_around", "egm", "gls",
                  "ks_if", "nl", "entropy_influence", "multi_evidence")

b65_all <- function(x) centrality(x, measures = b65_measures, hc_p = 0.05)

# --- dsc, dsr, edsr --------------------------------------------------------

test_that("dsc is the three-factor product on a path of four", {
  # Rounds 1, 2, 2, 1. For node 2 the neighbors have rounds 1 and 2: mean
  # 1.5, entropy of (1/3, 2/3); their shell distributions are (0, 1) and
  # (1/2, 1/2), with mixture (1/4, 3/4).
  entropy <- \(p) -sum(p * log(p))
  divergence <- entropy(c(0.25, 0.75)) - log(2) / 2
  expected <- 1.5 * entropy(c(1, 2) / 3) * divergence
  expect_equal(unname(centrality_dsc(b65_path(4L))),
               c(0, expected, expected, 0))
})

test_that("dsr and edsr are neighbor sums of dsc", {
  kite <- b65_kite()
  dsc <- centrality_dsc(kite)
  expect_equal(unname(centrality_dsr(kite)), as.numeric(kite %*% dsc))
  expect_equal(unname(centrality_edsr(kite)),
               as.numeric(kite %*% (kite %*% dsc)))
})

test_that("dsc is zero where the neighbors cannot differ", {
  expect_equal(unname(centrality_dsc(b65_star(4L))), numeric(5))
  expect_equal(unname(centrality_dsc(b65_complete(5L))), numeric(5))
})

# --- ksgc ------------------------------------------------------------------

test_that("ksgc is the one-step gravity model on a single-shell path", {
  # Mean distance 35/15, so the radius 7/6 admits neighbors only, and one
  # shell makes every coefficient 1.
  expect_equal(unname(centrality_ksgc(b65_path(6L))), c(2, 6, 8, 8, 6, 2))
})

test_that("ksgc weights a tie by the difference in k-shell", {
  # Shells 2, 2, 2, 1, 1, 1; degrees 2, 2, 3, 2, 2, 1; mean distance 31/15.
  expect_equal(unname(centrality_ksgc(b65_tailed_triangle())),
               c(10, 10, 12 + 6 * exp(1), 4 + 6 / exp(1), 6, 2))
})

test_that("ksgc is zero when the mean distance is below two", {
  expect_equal(unname(centrality_ksgc(b65_kite())), numeric(10))
  expect_equal(unname(centrality_ksgc(b65_complete(4L))), numeric(4))
})

# --- lsc -------------------------------------------------------------------

test_that("lsc counts two-step neighborhoods on a path", {
  # No triangles; two-step sizes 2, 3, 3, 2.
  expect_equal(unname(centrality_lsc(b65_path(4L))), c(0.6, 1, 1, 0.6))
  expect_equal(unname(centrality_lsc(b65_path(4L), lsc_alpha = 1)),
               c(3, 5, 5, 3))
})

test_that("lsc adds clustering on a complete network", {
  # Each of the 3 neighbors sees 3 others, all with clustering 1.
  expect_equal(unname(centrality_lsc(b65_complete(4L), lsc_alpha = 0.3)),
               rep(9, 4))
})

test_that("lsc rejects an alpha outside the unit interval", {
  expect_error(centrality_lsc(b65_path(4L), lsc_alpha = 1.5),
               class = "cograph_bad_parameter")
  expect_error(centrality_lsc(b65_path(4L), lsc_alpha = NA_real_),
               class = "cograph_bad_parameter")
})

# --- classified_neighbors --------------------------------------------------

test_that("classified neighbors follow the removal batches on a path", {
  # One shell; the ends leave first. An end sees a later neighbor (0.35);
  # a middle node sees an earlier one (0.25) and one of its own batch (0.35).
  expect_equal(unname(centrality_classified_neighbors(b65_path(4L))),
               c(0.35, 0.6, 0.6, 0.35))
})

test_that("classified neighbors separate higher and lower shells", {
  # Node 3 (shell 2) has two same-batch neighbors and one lower; node 4
  # (shell 1) has one higher and one earlier neighbor of its own shell.
  score <- centrality_classified_neighbors(b65_tailed_triangle(),
                                           cn_weights = c(1000, 100, 10, 1))
  expect_equal(unname(score), c(200, 200, 201, 1010, 110, 100))
})

test_that("the four classes add up to the degree", {
  kite <- b65_kite()
  expect_equal(
    unname(centrality_classified_neighbors(kite, cn_weights = rep(1, 4))),
    rowSums(kite), ignore_attr = TRUE)
})

test_that("classified neighbors reject malformed weights", {
  expect_error(centrality_classified_neighbors(b65_path(4L),
                                               cn_weights = c(1, 2, 3)),
               class = "cograph_bad_parameter")
  expect_error(centrality_classified_neighbors(b65_path(4L),
                                               cn_weights = c(1, -1, 0, 0)),
               class = "cograph_bad_parameter")
})

# --- hybrid_degree ---------------------------------------------------------

test_that("hybrid degree blends degree and modified local centrality", {
  path <- b65_path(4L)
  expect_equal(unname(centrality_hybrid_degree(path, hc_p = 0.1)),
               0.1 * unname(centrality_mlc(path)))
  expect_equal(unname(centrality_hybrid_degree(path, hc_p = 0)),
               c(100, 200, 200, 100))
  expect_equal(
    unname(centrality_hybrid_degree(path, hc_p = 0.05, hc_alpha = 10,
                                    hc_beta = 0.2)),
    0.15 * 10 * c(1, 2, 2, 1) + 0.05 * c(1, 2, 2, 1))
})

test_that("hybrid degree has no value without a spreading probability", {
  expect_warning(value <- centrality_hybrid_degree(b65_path(4L)),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(value)))
})

test_that("hybrid degree rejects parameters outside their domain", {
  expect_error(centrality_hybrid_degree(b65_path(4L), hc_p = 2),
               class = "cograph_bad_parameter")
  expect_error(centrality_hybrid_degree(b65_path(4L), hc_p = 0.1,
                                        hc_alpha = -1),
               class = "cograph_bad_parameter")
  expect_error(centrality_hybrid_degree(b65_path(4L), hc_p = 0.1,
                                        hc_beta = Inf),
               class = "cograph_bad_parameter")
})

# --- topsis ----------------------------------------------------------------

test_that("topsis puts the center of a star at the ideal", {
  for (attribute in c("betweenness", "eigenvector")) {
    expect_equal(
      unname(centrality_topsis(b65_star(4L), topsis_attribute = attribute)),
      c(1, 0, 0, 0, 0))
  }
})

test_that("topsis depends on the proportions of the weights only", {
  kite <- b65_kite()
  expect_equal(centrality_topsis(kite, topsis_weights = c(2, 2, 2)),
               centrality_topsis(kite))
  expect_false(isTRUE(all.equal(
    centrality_topsis(kite, topsis_weights = c(1, 0, 0)),
    centrality_topsis(kite))))
})

test_that("topsis with all weight on degree is the rescaled degree", {
  kite <- b65_kite()
  degree <- rowSums(kite)
  expect_equal(unname(centrality_topsis(kite, topsis_weights = c(1, 0, 0))),
               unname((degree - min(degree)) / (max(degree) - min(degree))))
})

test_that("topsis has no value where an attribute or the ideal is undefined", {
  disconnected <- b65_from_edges(5L, rbind(c(1, 2), c(2, 3), c(4, 5)))
  expect_warning(value <- centrality_topsis(disconnected),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(value)))
  expect_warning(value <- centrality_topsis(b65_complete(4L)),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(value)))
})

test_that("topsis reads an attribute equal up to rounding as constant", {
  # On a regular network the eigenvector is constant only up to rounding;
  # the score must be NA, not a ratio of rounding errors.
  ring <- b65_from_edges(5L, cbind(1:5, c(2:5, 1L)))
  for (network in list(ring, b65_complete(3L), b65_complete(4L))) {
    expect_warning(
      value <- centrality_topsis(network, topsis_attribute = "eigenvector"),
      class = "cograph_undefined_measure")
    expect_true(all(is.na(value)))
  }
})

test_that("topsis rejects an unknown attribute and malformed weights", {
  expect_error(centrality_topsis(b65_kite(), topsis_attribute = "pagerank"),
               class = "cograph_bad_parameter")
  expect_error(centrality_topsis(b65_kite(), topsis_weights = c(1, 1)),
               class = "cograph_bad_parameter")
  expect_error(centrality_topsis(b65_kite(), topsis_weights = c(0, 0, 0)),
               class = "cograph_bad_parameter")
})

# --- all_around ------------------------------------------------------------

test_that("the all-around score is the length of three rescaled indices", {
  # Degree 1, 2, 2, 1; betweenness 0, 2, 2, 0; one shell.
  expect_equal(unname(centrality_all_around(b65_path(4L))),
               c(sqrt(1.25), sqrt(3), sqrt(3), sqrt(1.25)))
  # No betweenness on a complete network: two indices at their maximum.
  expect_equal(unname(centrality_all_around(b65_complete(4L))),
               rep(sqrt(2), 4))
})

# --- egm -------------------------------------------------------------------

test_that("egm follows the radius and the entropy on a path of four", {
  # Ends: radius 3 / 2, value entropy(1/3, 2/3). Middle: radius
  # 2 / (1 + sqrt(1/2)), value 2 * entropy(0.2, 0.4, 0.4). Both radii admit
  # neighbors only.
  entropy <- \(p) -sum(p * log(p))
  end <- entropy(c(1, 2) / 3)
  middle <- 2 * entropy(c(1, 2, 2) / 5)
  expect_equal(unname(centrality_egm(b65_path(4L))),
               c(end * middle, middle * (end + middle),
                 middle * (end + middle), end * middle))
})

test_that("egm is zero for a node whose radius is below one", {
  # Node 8 of the kite: degree 3, eccentricity 2, far nodes of mean degree
  # 3.5, so the radius is 2 / (1 + sqrt(3.5 / 3)) < 1.
  egm <- centrality_egm(b65_kite())
  expect_equal(egm[["8"]], 0)
  expect_true(all(egm[names(egm) != "8"] > 0))
})

test_that("egm has no value on a disconnected network", {
  disconnected <- b65_from_edges(5L, rbind(c(1, 2), c(2, 3), c(4, 5)))
  expect_warning(value <- centrality_egm(disconnected),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(value)))
})

# --- gls -------------------------------------------------------------------

test_that("gls counts common neighbors with every other node", {
  # An end shares one neighbor with the node two steps away:
  # 1 * (1 + 1.1 + 1) * 1 / 3. A middle node: 2 * (1 + 1 + 1.1) * 2 / 3.
  expect_equal(unname(centrality_gls(b65_path(4L))),
               c(3.1 / 3, 12.4 / 3, 12.4 / 3, 3.1 / 3))
  # Complete network of 4: two common neighbors with each of 3 others.
  expect_equal(unname(centrality_gls(b65_complete(4L))),
               rep(3 * 3 * 1.1^2 * 3 / 3, 4))
})

# --- ks_if -----------------------------------------------------------------

test_that("ks_if uses the iteration of removal within the shell", {
  # Path of four: one stage of two iterations; delta 1.5 at the ends and 2
  # in the middle.
  expect_equal(unname(centrality_ks_if(b65_path(4L))), c(5.5, 9.5, 9.5, 5.5))
  # One stage, one iteration: delta = 2 * shell for every node.
  expect_equal(unname(centrality_ks_if(b65_complete(4L))), rep(6 * 3 * 4, 4))
})

test_that("ks_if counts iterations from one in each shell", {
  # Tail 6, 5, 4 leaves in three iterations of stage 1 (delta 4/3, 5/3, 2);
  # the triangle leaves in one iteration of stage 2 (delta 4).
  delta <- c(4, 4, 4, 2, 5 / 3, 4 / 3)
  tailed <- b65_tailed_triangle()
  weighted <- delta * rowSums(tailed)
  expect_equal(unname(centrality_ks_if(tailed)),
               unname(weighted + as.numeric(tailed %*% weighted)))
})

# --- nl --------------------------------------------------------------------

test_that("nl adds two-step counts and tie contributions on a path", {
  # Two-step counts 2, 3, 3, 2. The middle tie has importance 1 and gives
  # each end of it one half; the end ties have importance 0.
  expect_equal(unname(centrality_nl(b65_path(4L))), c(3, 5.5, 5.5, 3))
})

test_that("nl is the two-step sum plus dil less the degree", {
  kite <- b65_kite()
  within_two <- (kite + kite %*% kite) > 0
  diag(within_two) <- FALSE
  expect_equal(unname(centrality_nl(kite)),
               unname(as.numeric(kite %*% rowSums(within_two)) +
                        centrality_dil(kite) - rowSums(kite)))
})

# --- entropy_influence ------------------------------------------------------

test_that("the entropy influence is the log degree plus the bridge term", {
  # Unweighted path of five: direct influence log10 of the degree. Each
  # inner node reaches its two-step nodes through an inner node, so its
  # indirect influence is log10(2)^2; an end has direct influence 0.
  l2 <- log10(2)
  inner <- 0.6 * l2 + 0.4 * l2^2
  expect_equal(unname(centrality_entropy_influence(b65_path(5L))),
               c(0, inner, inner, inner, 0))
})

test_that("the entropy influence keeps mutual ties only", {
  one_way <- matrix(0, 3, 3, dimnames = list(1:3, 1:3))
  one_way[1, 2] <- one_way[2, 3] <- one_way[3, 1] <- 1
  expect_equal(unname(centrality_entropy_influence(one_way)), numeric(3))
})

test_that("the entropy influence reads weights as interaction counts", {
  # Node 1 sends 1 and 3 to its two friends: entropy of (1/4, 3/4).
  counts <- matrix(c(0, 1, 3, 1, 0, 1, 3, 1, 0), 3, 3, byrow = TRUE,
                   dimnames = list(1:3, 1:3))
  spread <- -(0.25 * log10(0.25) + 0.75 * log10(0.75))
  value <- centrality_entropy_influence(counts, ei_omega = 1)
  expect_equal(value[["1"]], 0.4 * log10(2) + 0.6 * spread)
  expect_equal(value[["2"]], log10(2))
})

test_that("the entropy influence rejects mixing weights outside [0, 1]", {
  expect_error(centrality_entropy_influence(b65_path(4L), ei_alpha = -0.1),
               class = "cograph_bad_parameter")
  expect_error(centrality_entropy_influence(b65_path(4L), ei_omega = 2),
               class = "cograph_bad_parameter")
})

# --- multi_evidence --------------------------------------------------------

test_that("multi-evidence is zero when no index separates the nodes", {
  expect_equal(unname(centrality_multi_evidence(b65_complete(5L))),
               numeric(5))
})

test_that("multi-evidence fuses four opposed assignments on a star", {
  # The center is the maximum and each leaf the minimum of all four
  # indices, so every assignment is (s / (s + 1), 0, 1 / (s + 1)) for the
  # center, with s the span of the index, and mirrored for a leaf.
  star <- b65_star(4L)
  n <- 5
  efficiency <- c(4, 1 + 3 / 2) / n
  # A leaf reaches the center in one step and three leaves in two.
  correlation <- c(4 * efficiency[2],
                   efficiency[1] + 3 * efficiency[2] / 8) / n
  spans <- c(1 - 1 / 4, 1, diff(rev(efficiency)), diff(rev(correlation)))
  undecided <- prod(1 / (spans + 1))
  expect_equal(unname(centrality_multi_evidence(star)),
               c(1 - undecided, rep(-(1 - undecided), 4)))
})

test_that("multi-evidence rejects an omega outside (0, 1]", {
  expect_error(centrality_multi_evidence(b65_kite(), evidence_omega = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_multi_evidence(b65_kite(), evidence_omega = 1.5),
               class = "cograph_bad_parameter")
})

# --- shared properties -----------------------------------------------------

test_that("the batch 65 measures follow a relabeling of the nodes", {
  kite <- b65_kite()
  set.seed(65)
  shuffle <- sample(nrow(kite))
  original <- b65_all(kite)
  shuffled <- b65_all(kite[shuffle, shuffle])
  for (measure in b65_measures) {
    expect_equal(shuffled[[measure]], original[[measure]][shuffle],
                 info = measure)
  }
})

test_that("the batch 65 measures ignore weights, direction and loops", {
  kite <- b65_kite()
  set.seed(651)
  noisy <- kite * matrix(stats::runif(100, 1, 5), 10, 10)
  noisy[lower.tri(noisy)] <- 0
  diag(noisy) <- 2
  structural <- setdiff(b65_measures, "entropy_influence")
  expect_equal(centrality(noisy, measures = structural, hc_p = 0.05),
               centrality(kite, measures = structural, hc_p = 0.05))
})

test_that("isolated nodes score zero where the measure is defined", {
  with_isolate <- b65_from_edges(5L, rbind(c(1, 2), c(2, 3), c(3, 4)))
  defined <- setdiff(b65_measures, c("topsis", "egm", "multi_evidence"))
  scores <- centrality(with_isolate, measures = defined, hc_p = 0.05)
  for (measure in defined) {
    expect_equal(scores[[measure]][5L], 0, info = measure)
    expect_true(all(is.finite(scores[[measure]])), info = measure)
  }
  # The isolate is the minimum of all four indices of multi-evidence.
  evidence <- centrality_multi_evidence(with_isolate)
  expect_true(all(is.finite(evidence)))
  expect_equal(which.min(evidence), c(`5` = 5L))
})

test_that("a one-node network gives one value per measure", {
  single <- matrix(0, 1, 1, dimnames = list("a", "a"))
  defined <- setdiff(b65_measures, c("topsis", "egm"))
  scores <- centrality(single, measures = defined, hc_p = 0.05)
  for (measure in defined) expect_equal(scores[[measure]], 0, info = measure)
  expect_warning(expect_true(is.na(centrality_topsis(single))),
                 class = "cograph_undefined_measure")
  expect_warning(expect_true(is.na(centrality_egm(single))),
                 class = "cograph_undefined_measure")
})

test_that("normalized scores have a maximum of one", {
  kite <- b65_kite()
  positive <- setdiff(b65_measures, c("ksgc", "multi_evidence"))
  scores <- centrality(kite, measures = positive, hc_p = 0.05,
                       normalized = TRUE)
  for (measure in positive) {
    expect_equal(max(scores[[measure]]), 1, info = measure)
  }
})

test_that("the batch 65 measures are in the default set", {
  expect_true(all(b65_measures %in% list_centralities()$measure))
  expect_false(any(b65_measures %in% cograph:::.cg_costly_measures()))
})
