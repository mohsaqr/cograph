# Batch 67 --- eight measures built on 2026-11-26 from papers read in full:
#   mapping_entropy  Nie, Guo, Zhao and Lu (2016), Physica A 453:290-297,
#             eq. (3), with DC = d / (N - 1)
#   dcc       Yang, Wang, Chen, Hu and Ruan (2020), IEEE Access
#             8:58742-58751, eqs. (7)-(12)
#   edge_clustering  Wang, Li, Wang and Pan (2012), IEEE/ACM TCBB
#             9(4):1070-1080, eqs. (7)-(8)
#   ahp       Bian, Hu and Deng (2017), Physica A 479:422-436, section 4
#   semi_ia   Luan, Bao and Zhang (2021), Journal of Systems Science and
#             Complexity 34(6):2168-2181, eqs. (8)-(9) with X'_j
#   synthesize  Liu, Yu and Lu (2013), ICSSC 2013, 268-271, eqs. (6)-(9)
#   inward_accessibility  Travencolo and Costa (2008), Physics Letters A
#             373(1):89-95, eqs. (4)-(5)
#   improved_entropy  Ni, Yang and Kong (2020), Physica A 545:123659,
#             eqs. (2)-(10)
# Published values are compared in
# tests/equivalence/test-centrality-zoo-b-batch67.R.

b67_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1, drop = FALSE]] <- 1
  a
}

b67_path <- function(n) b67_from_edges(n, cbind(seq_len(n - 1L), 2:n))

b67_star <- function(leaves) {
  b67_from_edges(leaves + 1L, cbind(1L, seq_len(leaves) + 1L))
}

b67_complete <- function(n) {
  a <- matrix(1, n, n, dimnames = list(seq_len(n), seq_len(n)))
  diag(a) <- 0
  a
}

# A triangle 1-2-3 with the pendant 1-4.
b67_paw <- function() {
  b67_from_edges(4L, rbind(c(1, 2), c(1, 3), c(2, 3), c(1, 4)))
}

b67_kite <- function() {
  b67_from_edges(10L, rbind(
    c(1, 2), c(1, 3), c(1, 4), c(1, 6), c(2, 4), c(2, 5), c(2, 7), c(3, 4),
    c(3, 6), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(6, 7), c(6, 8), c(7, 8),
    c(8, 9), c(9, 10)))
}

# A 4-clique and a triangle joined by a path, with a pendant.
b67_long <- function() {
  b67_from_edges(14L, matrix(c(1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4, 4, 5, 5, 6,
                               6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 11, 13,
                               12, 13, 13, 14), ncol = 2L, byrow = TRUE))
}

b67_measures <- c("mapping_entropy", "dcc", "edge_clustering", "ahp",
                  "semi_ia", "synthesize", "inward_accessibility",
                  "improved_entropy")
b67_structural <- setdiff(b67_measures, "improved_entropy")

b67_all <- function(x, measures = b67_measures) {
  centrality(x, measures = measures, ia_steps = 2)
}

# --- mapping_entropy -------------------------------------------------------

test_that("mapping_entropy follows equation (3) with DC = d / (N - 1)", {
  # Paw, N = 4: DC = 1, 2/3, 2/3, 1/3.
  # ME_1 = -(log 2/3 + log 2/3 + log 1/3); ME_2 = -(2/3)(log 1 + log 2/3);
  # ME_4 = -(1/3) log 1 = 0.
  expect_equal(unname(centrality_mapping_entropy(b67_paw())),
               c(2 * log(1.5) + log(3), 2 / 3 * log(1.5), 2 / 3 * log(1.5), 0))
  # Path of four: DC = 1/3, 2/3, 2/3, 1/3.
  expect_equal(unname(centrality_mapping_entropy(b67_path(4L))),
               c(log(1.5) / 3, 2 / 3 * (log(3) + log(1.5)),
                 2 / 3 * (log(3) + log(1.5)), log(1.5) / 3))
})

test_that("mapping_entropy is not local_entropy (eq. 2, raw degree)", {
  # local_entropy of node 1 of the paw: -(2 log 2 + 2 log 2 + 1 log 1).
  paw <- b67_paw()
  expect_equal(centrality_local_entropy(paw)[["1"]], -4 * log(2))
  expect_false(isTRUE(all.equal(centrality_mapping_entropy(paw),
                                centrality_local_entropy(paw))))
})

# --- dcc -------------------------------------------------------------------

test_that("dcc follows equations (7)-(12) on the paw", {
  # Clustering 1/3, 1, 1, 0. I_D = d + neighbors' degrees = 8, 7, 7, 4.
  # Every other node is within two steps of every node, so the clustering
  # sum is 7/3 minus the node's own: I_C = 2 e^(-1/3), (4/3) e^(-1),
  # (4/3) e^(-1), 7/3 (exactly-two-steps would give 2 for node 4).
  i_d <- c(8, 7, 7, 4)
  i_c <- c(2 * exp(-1 / 3), 4 / 3 * exp(-1), 4 / 3 * exp(-1), 7 / 3)
  entropy <- \(x) {
    r <- x / sqrt(sum(x^2))
    -sum(r * log(r)) / log(4)
  }
  alpha <- (1 - entropy(i_d)) / (2 - entropy(i_d) - entropy(i_c))
  expect_equal(unname(centrality_dcc(b67_paw())),
               alpha * i_d + (1 - alpha) * i_c)
})

test_that("dcc warns and gives NA on a network without a triangle", {
  for (graph in list(b67_path(5L), b67_star(4L), b67_from_edges(4L, rbind(
    c(1, 2), c(2, 3), c(3, 4), c(4, 1))))) {
    expect_warning(score <- centrality_dcc(graph),
                   class = "cograph_undefined_measure")
    expect_true(all(is.na(score)))
  }
})

# --- edge_clustering -------------------------------------------------------

test_that("edge_clustering reproduces ECC(1, 4) = 2/3 on Fig. 1", {
  # Fig. 1: node 1 has the edges 1-2 (no triangle), 1-4, 1-5, 1-6 (two
  # triangles each, min(4 - 1, 4 - 1) = 3), so NC(1) = 3 x 2/3 = 2 and
  # ECC(1, 4) = 2/3. Node 4: 4-1 and 4-3 at 2/3, 4-5 and 4-6 at 3/3.
  # Node 2 has degree 2 and no triangle.
  figure_1 <- b67_from_edges(6L, rbind(
    c(1, 2), c(1, 4), c(1, 5), c(1, 6), c(2, 3), c(3, 4), c(3, 5), c(3, 6),
    c(4, 5), c(4, 6), c(5, 6)))
  expect_equal(unname(centrality_edge_clustering(figure_1)),
               c(2, 0, 2, 10 / 3, 10 / 3, 10 / 3))
})

test_that("an edge with a degree-one end contributes 0 to edge_clustering", {
  # Paw: 1-2 and 1-3 have one triangle and min(3 - 1, 2 - 1) = 1; 2-3 has
  # one and min(1, 1) = 1; the pendant edge 1-4 has a zero denominator.
  expect_equal(unname(centrality_edge_clustering(b67_paw())), c(2, 2, 2, 0))
  expect_equal(unname(centrality_edge_clustering(b67_star(3L))), numeric(4))
  # Complete network of four: two triangles per edge, min(2, 2) = 2.
  expect_equal(unname(centrality_edge_clustering(b67_complete(4L))),
               rep(3, 4))
})

# --- ahp -------------------------------------------------------------------

test_that("ahp is the weighted sum of the discounted shares", {
  # Path 1-2-3. Degree shares 1/4, 1/2, 1/4 (no zero, N+/N = 1);
  # betweenness 0, 1, 0, so (1/3) x (0, 1, 0); closeness 1/3, 1/2, 1/3 of
  # total 7/6, so 2/7, 3/7, 2/7.
  path <- b67_path(3L)
  expect_equal(unname(centrality_ahp(path)),
               c(1 / 4 + 2 / 7, 1 / 2 + 1 / 3 + 3 / 7, 1 / 4 + 2 / 7) / 3)
  expect_equal(unname(centrality_ahp(path, ahp_weights = c(0, 1, 0))),
               c(0, 1 / 3, 0))
  expect_equal(unname(centrality_ahp(path, ahp_weights = c(2, 0, 0))),
               c(1 / 2, 1, 1 / 2))
})

test_that("ahp warns and gives NA where closeness has no value", {
  graph <- b67_from_edges(4L, rbind(c(1, 2), c(3, 4)))
  expect_warning(score <- centrality_ahp(graph),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
  expect_warning(single <- centrality_ahp(matrix(0, 1, 1)),
                 class = "cograph_undefined_measure")
  expect_true(is.na(single))
})

test_that("ahp rejects bad weights", {
  for (bad in list(c(1, 1), c(-1, 1, 1), c(0, 0, 0), c(NA, 1, 1))) {
    expect_error(centrality_ahp(b67_path(3L), ahp_weights = bad),
                 class = "cograph_bad_parameter")
  }
})

# --- semi_ia ---------------------------------------------------------------

test_that("semi_ia tends to lambda_1 v_1 on a path of three", {
  # W = [0 1 1/2; 1 0 1; 1/2 1 0] (every path count is 1). With v = (1, b, 1)
  # the eigen equations give b = 2 / lambda and lambda^2 - lambda/2 - 2 = 0.
  lambda <- (0.5 + sqrt(8.25)) / 2
  v <- c(1, 2 / lambda, 1)
  expect_equal(unname(centrality_semi_ia(b67_path(3L))),
               lambda * v / sqrt(sum(v^2)), tolerance = 1e-10)
})

test_that("semi_ia weights the path count by gamma on a four-cycle", {
  # Opposite nodes are two steps apart by two paths: W has 1 for neighbors
  # and 2^gamma / 2 for the opposite node, so every row sums to
  # lambda = 2 + 2^gamma / 2 and the unit eigenvector is 1/2 everywhere.
  cycle <- b67_from_edges(4L, cbind(1:4, c(2:4, 1)))
  for (gamma in c(0, 0.2, 1)) {
    expect_equal(unname(centrality_semi_ia(cycle, semi_ia_gamma = gamma)),
                 rep((2 + 2^gamma / 2) / 2, 4), tolerance = 1e-10)
  }
})

test_that("semi_ia warns when the update has no steady state", {
  # At radius 1 the weight matrix of a star is its adjacency matrix, whose
  # eigenvalues +-sqrt(3) make the normalised vector alternate.
  expect_warning(centrality_semi_ia(b67_star(3L), semi_ia_radius = 1,
                                    semi_ia_max_iter = 50),
                 class = "cograph_no_converge")
})

test_that("semi_ia warns and gives NA off a connected network", {
  graph <- b67_from_edges(5L, rbind(c(1, 2), c(2, 3), c(4, 5)))
  expect_warning(score <- centrality_semi_ia(graph),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
})

test_that("semi_ia rejects bad parameters", {
  path <- b67_path(4L)
  expect_error(centrality_semi_ia(path, semi_ia_gamma = 1.5),
               class = "cograph_bad_parameter")
  expect_error(centrality_semi_ia(path, semi_ia_radius = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_semi_ia(path, semi_ia_max_iter = 0.5),
               class = "cograph_bad_parameter")
})

test_that("semi_ia takes radius 3 below 1000 nodes", {
  long <- b67_long()
  expect_equal(centrality_semi_ia(long),
               centrality_semi_ia(long, semi_ia_radius = 3))
})

# --- synthesize ------------------------------------------------------------

test_that("synthesize follows equations (6)-(9) on a path of three", {
  # n - 1 = 2. Degree 1/2, 1, 1/2; betweenness through node 2 over ordered
  # pairs 2, so 2/4; farness (1 + 2)/4, 2/4, 3/4.
  expect_equal(unname(centrality_synthesize(b67_path(3L))),
               c((1 / 2) / (3 / 4), (1 + 1 / 2) / (1 / 2), (1 / 2) / (3 / 4)))
})

test_that("synthesize charges an unreachable pair the distance 1000", {
  # An edge 1-2 and an isolate 3: farness of node 1 is (1 + 1000) / 4.
  graph <- b67_from_edges(3L, rbind(c(1, 2)))
  expect_equal(unname(centrality_synthesize(graph)),
               c(2 / 1001, 2 / 1001, 0))
  expect_warning(single <- centrality_synthesize(matrix(0, 1, 1)),
                 class = "cograph_undefined_measure")
  expect_true(is.na(single))
})

# --- inward_accessibility --------------------------------------------------

test_that("the walk enumeration reproduces the 18 entropies of Fig. 2", {
  # Diversity entropy of node 1, eq. (1), from the walk probabilities that
  # inward accessibility is built on. Panel graphs transcribed from Fig. 2.
  panels <- list(
    a = list(c(1, 2, 2, 3, 3, 4), "stay", c(0, 0, 0)),
    b = list(c(1, 2, 2, 3, 3, 4, 3, 5, 3, 6), "stay", c(0, 0, log(3))),
    c = list(c(1, 2, 2, 3, 3, 4, 1, 5, 5, 6, 6, 7, 1, 8, 8, 9, 9, 10), "stay",
             rep(log(3), 3)),
    d = list(c(1, 2, 1, 3, 1, 4, 2, 5, 3, 6, 4, 7, 5, 8, 5, 9, 5, 10, 6, 8,
               6, 9, 6, 10, 7, 8, 7, 9, 7, 10), "stay", rep(log(3), 3)),
    e = list(c(1, 2, 1, 3, 3, 4, 1, 5, 5, 6, 6, 7), "stay", rep(log(3), 3)),
    f = list(c(1, 2, 1, 3, 3, 4, 1, 5, 5, 6, 6, 7), "vanish",
             log(3) * c(1, 2 / 3, 1 / 3)))
  printed <- list(a = c(0, 0, 0), b = c(0, 0, 1.1), c = rep(1.1, 3),
                  d = rep(1.1, 3), e = rep(1.1, 3), f = c(1.1, 0.73, 0.37))
  for (name in names(panels)) {
    edges <- matrix(panels[[name]][[1]], ncol = 2L, byrow = TRUE)
    graph <- unname(b67_from_edges(max(edges), edges))
    entropy <- vapply(1:3, \(h) {
      p <- cograph:::.cg_saw_probabilities(graph, h, panels[[name]][[2]])[1, -1]
      -sum(ifelse(p > 0, p * log(p), 0))
    }, numeric(1))
    expect_equal(entropy, panels[[name]][[3]], info = name)
    expect_equal(round(entropy, 2), printed[[name]], tolerance = 0.006,
                 info = name)
  }
})

test_that("inward_accessibility follows equations (4)-(5) on a star", {
  # Star with center 1 and leaves 2-4, N - 1 = 3.
  # h = 1: every leaf reaches the center with probability 1 (q = 1/3 each,
  # entropy log 3, IA 1); a leaf is reached by the center with 1/3
  # (q = 1/9, IA = 9^(1/9) / 3).
  star <- b67_star(3L)
  expect_equal(unname(centrality_inward_accessibility(star, ia_steps = 1)),
               c(1, rep(9^(1 / 9) / 3, 3)))
  # h = 2, vanish: a leaf reaches each other leaf with 1/2 (q = 1/6); the
  # center's walks stop at a leaf and vanish; nothing is at the center.
  expect_equal(unname(centrality_inward_accessibility(star, ia_steps = 2)),
               c(1 / 3, rep(6^(1 / 3) / 3, 3)))
  # h = 2, stay: the center's walks stay at the leaf they reached (1/3).
  expect_equal(unname(centrality_inward_accessibility(star, ia_steps = 2,
                                                      ia_stuck = "stay")),
               c(1 / 3, rep(exp(log(6) / 3 + log(9) / 9) / 3, 3)))
})

test_that("an unreached node scores the floor 1 / (N - 1)", {
  graph <- b67_from_edges(5L, rbind(c(1, 2), c(2, 3), c(3, 4)))
  expect_equal(centrality_inward_accessibility(graph, ia_steps = 1)[["5"]],
               1 / 4)
})

test_that("inward_accessibility needs ia_steps", {
  expect_warning(score <- centrality_inward_accessibility(b67_path(4L)),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
  expect_warning(single <- centrality_inward_accessibility(matrix(0, 1, 1),
                                                           ia_steps = 2),
                 class = "cograph_undefined_measure")
  expect_true(is.na(single))
})

test_that("inward_accessibility warns past the enumeration limit", {
  expect_warning(score <- centrality_inward_accessibility(
    b67_kite(), ia_steps = 3, enumeration_limit = 10),
    class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
})

test_that("inward_accessibility rejects bad parameters", {
  path <- b67_path(4L)
  expect_error(centrality_inward_accessibility(path, ia_steps = 0),
               class = "cograph_bad_parameter")
  expect_error(centrality_inward_accessibility(path, ia_steps = 1.5),
               class = "cograph_bad_parameter")
  expect_error(centrality_inward_accessibility(path, ia_steps = 2,
                                               ia_stuck = "keep"),
               class = "cograph_bad_parameter")
})

# --- improved_entropy ------------------------------------------------------

test_that("improved_entropy follows equations (2)-(10) on a path of four", {
  # Degrees 1, 2, 2, 1. A degree-one node has DI = 0. Node 2: weight shares
  # 1/2, 1/2 (log10 2) and confidence 1/5, 4/5 at beta = 2. Node 2's only
  # two-step node is 4, through 3: II = DI_2 DI_3 = DI_2^2. Node 1's is 3,
  # through 2: II = DI_1 DI_2 = 0.
  confidence <- -(0.2 * log10(0.2) + 0.8 * log10(0.8))
  di <- 0.5 * log10(2) + 0.5 * confidence
  expect_equal(unname(centrality_improved_entropy(b67_path(4L))),
               c(0, 0.5 * di + 0.5 * di^2, 0.5 * di + 0.5 * di^2, 0))
})

test_that("improved_entropy reads the weights and ignores beta on a star", {
  # Center of a star with tie weights 1, 2, 3: weight shares 1/6, 2/6, 3/6;
  # every leaf has degree 1, so the confidence is 1/3 each whatever beta.
  # The center has no node two steps away (II = 0); every leaf has DI = 0.
  star <- b67_star(3L)
  star[1, 2:4] <- star[2:4, 1] <- 1:3
  shares <- (1:3) / 6
  di <- 0.5 * -sum(shares * log10(shares)) + 0.5 * log10(3)
  for (beta in c(-1, 0, 2)) {
    expect_equal(unname(centrality_improved_entropy(star, ie_beta = beta)),
                 c(0.5 * di, 0, 0, 0))
  }
  expect_equal(unname(centrality_improved_entropy(star, weighted = FALSE)),
               c(0.5 * log10(3), 0, 0, 0))
})

test_that("improved_entropy warns and gives NA on a directed network", {
  arcs <- matrix(0, 3, 3, dimnames = list(1:3, 1:3))
  arcs[1, 2] <- arcs[2, 3] <- 1
  expect_warning(score <- centrality_improved_entropy(arcs),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(score)))
})

test_that("improved_entropy rejects a bad beta and negative weights", {
  expect_error(centrality_improved_entropy(b67_path(4L), ie_beta = Inf),
               class = "cograph_bad_parameter")
  expect_error(centrality_improved_entropy(b67_path(4L), ie_beta = c(1, 2)),
               class = "cograph_bad_parameter")
  negative <- b67_path(3L)
  negative[1, 2] <- negative[2, 1] <- -1
  expect_error(centrality_improved_entropy(negative),
               class = "cograph_negative_weights")
})

test_that("improved_entropy is invariant to the scale of the weights", {
  kite <- b67_kite()
  set.seed(671)
  weighted <- kite * matrix(stats::runif(100, 1, 5), 10, 10)
  weighted[lower.tri(weighted)] <- t(weighted)[lower.tri(weighted)]
  expect_equal(centrality_improved_entropy(weighted * 7),
               centrality_improved_entropy(weighted))
})

# --- shared properties -----------------------------------------------------

test_that("the batch 67 measures follow a relabeling of the nodes", {
  set.seed(67)
  for (graph in list(b67_kite(), b67_long())) {
    shuffle <- sample(nrow(graph))
    original <- b67_all(graph)
    shuffled <- b67_all(graph[shuffle, shuffle])
    for (measure in b67_measures) {
      expect_equal(shuffled[[measure]], original[[measure]][shuffle],
                   info = measure)
    }
  }
})

test_that("the structural measures ignore weights, direction and loops", {
  long <- b67_long()
  set.seed(672)
  noisy <- long * matrix(stats::runif(196, 1, 5), 14, 14)
  noisy[lower.tri(noisy)] <- 0
  diag(noisy) <- 2
  expect_equal(b67_all(noisy, b67_structural), b67_all(long, b67_structural))
})

test_that("isolated nodes score zero, or the floor of inward accessibility", {
  # A path, a triangle (so that dcc is defined) and the isolate 9.
  graph <- b67_from_edges(9L, rbind(cbind(1:4, 2:5), c(6, 7), c(7, 8),
                                    c(6, 8)))
  scores <- b67_all(graph, setdiff(b67_measures, c("ahp", "semi_ia")))
  for (measure in c("mapping_entropy", "dcc", "edge_clustering",
                    "synthesize", "improved_entropy")) {
    expect_equal(scores[[measure]][9L], 0, info = measure)
    expect_true(all(is.finite(scores[[measure]])), info = measure)
  }
  expect_equal(scores$inward_accessibility[9L], 1 / 8)
})

test_that("a one-node network scores 0 or warns and gives NA", {
  single <- matrix(0, 1, 1, dimnames = list("a", "a"))
  zero <- c("mapping_entropy", "edge_clustering", "improved_entropy")
  scores <- b67_all(single, zero)
  for (measure in zero) expect_equal(scores[[measure]], 0, info = measure)
  for (measure in setdiff(b67_measures, zero)) {
    expect_warning(value <- b67_all(single, measure)[[measure]],
                   class = "cograph_undefined_measure")
    expect_true(is.na(value), info = measure)
  }
})

test_that("normalized scores have a maximum of one", {
  scores <- centrality(b67_long(), measures = b67_measures, ia_steps = 2,
                       normalized = TRUE)
  for (measure in b67_measures) {
    expect_equal(max(scores[[measure]]), 1, info = measure)
  }
})

test_that("the batch 67 measures are in the default set", {
  table <- list_centralities()
  expect_true(all(b67_measures %in% table$measure))
  expect_false(any(b67_measures %in% cograph:::.cg_costly_measures()))
  expect_equal(table$uses_weights[match(b67_measures, table$measure)],
               b67_measures == "improved_entropy")
  expect_true("entropy_influence" %in% table$measure)
})
