# Regression tests for cograph 2.7.20: measures brought back to their papers (owner rule of 2026-11-22: the
# literature wins over a package convention). Each value is computed by hand in the comments, on a small graph
# where cograph 2.7.19 gave a different number (stated as "was").

adjacency <- function(edges, nodes, directed = FALSE) {
  m <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  m[edges] <- 1
  if (!directed) m[edges[, c(2, 1), drop = FALSE]] <- 1
  m
}

path3 <- adjacency(rbind(c("A", "B"), c("B", "C")), c("A", "B", "C"))

test_that("lobby index counts the neighbours only (Korn, Schubert and Telcs 2009, Definition 1)", {
  # A's neighbours B, C, D have degrees 3, 3, 1: h-index 2 (was 3 with A's own degree 3 added)
  g <- adjacency(rbind(c("A", "B"), c("A", "C"), c("A", "D"), c("B", "C"), c("B", "E"), c("C", "F")),
                 c("A", "B", "C", "D", "E", "F"))
  expect_equal(unname(centrality_lobby(g)["A"]), 2)
  # strength h-index on unit weights is the lobby index
  expect_equal(unname(centrality(g, measures = "hindex_strength")$hindex_strength_all[1]), 2)
})

test_that("a reciprocated neighbour counts once under mode 'all' (semi-local, Chen et al. 2012)", {
  # A <-> B, B -> C: every node has N = 2, Q(A) = 2, Q(B) = 4, Q(C) = 2, so C_L = 4, 4, 4 (was 12, 10, 6)
  g <- adjacency(rbind(c("A", "B"), c("B", "A"), c("B", "C")), c("A", "B", "C"), directed = TRUE)
  expect_equal(unname(centrality_semilocal(g)), c(4, 4, 4))
  expect_equal(unname(centrality_lobby(g)), c(1, 1, 1))
})

test_that("distance measures sum over the other nodes", {
  # path A - B - C, A's distances 1 and 2, diameter 2
  expect_equal(unname(centrality_residual_closeness(path3)["A"]), 0.5 + 0.25)        # was 1.75
  expect_equal(unname(centrality_dangalchev(path3)["A"]), 0.75)
  expect_equal(unname(centrality_average_distance(path3)["A"]), 3 / 2)               # was 3 / 4
  expect_equal(unname(centrality_radiality(path3)), c(1.5, 2, 1.5))                  # was 3, 3.5, 3
  expect_equal(unname(centrality_integration(path3)), c(1.5, 2, 1.5))                # was 3, 3.5, 3
})

test_that("integration reads the paths into the node; Gil-Schmidt divides by the nodes reached", {
  chain <- adjacency(rbind(c("A", "B"), c("B", "C")), c("A", "B", "C"), directed = TRUE)
  # A -> B -> C, diameter 2: C is reached from B (1) and A (2): (2 + 1) / 2; B from A: 2 / 2
  expect_equal(unname(centrality_integration(chain, mode = "out")), c(0, 1, 1.5))
  # Gil and Schmidt (1996): A reaches B, C: (1 + 1/2) / 2; B reaches C only: 1 (was 1/2); C reaches none: 0
  expect_equal(unname(centrality_gilschmidt(chain, mode = "out")), c(0.75, 1, 0))
})

test_that("ClusterRank is 10^-c times the neighbours' k + 1 (Chen et al. 2013, Eq. 1-4)", {
  # triangle A B C and a pendant D on A: c_A = 1/3, neighbours' degrees 2, 2, 1
  g <- adjacency(rbind(c("A", "B"), c("A", "C"), c("B", "C"), c("A", "D")), c("A", "B", "C", "D"))
  out <- centrality_clusterrank(g)
  expect_equal(unname(out["A"]), 10^(-1 / 3) * 8)     # was 8 / 3
  expect_equal(unname(out["B"]), 10^-1 * (4 + 3))     # was 7
  expect_true(is.nan(out[["D"]]))
  # directed: A -> B, A -> C, B <-> C. A's followers B, C; the arcs among them 2, so c_A = 2 / (2 * 1) = 1;
  # their out-degrees 1 and 1: 10^-1 * 4 (was 8 on the undirected skeleton)
  d <- adjacency(rbind(c("A", "B"), c("A", "C"), c("B", "C"), c("C", "B")), c("A", "B", "C"), directed = TRUE)
  expect_equal(unname(centrality_clusterrank(d)["A"]), 0.4)
})

test_that("effective size on a directed network is Burt's (as NetworkX)", {
  # A <-> B, A -> C, B -> C. Mutual weights A-B 2, A-C 1, B-C 1. e(A) = (1 - 1/3 * 1/2) + (1 - 2/3 * 1) = 7/6
  # (was 2, Borgatti's form with B counted twice)
  g <- adjacency(rbind(c("A", "B"), c("B", "A"), c("A", "C"), c("B", "C")), c("A", "B", "C"), directed = TRUE)
  expect_equal(unname(centrality_effective_size(g)["A"]), 7 / 6)
  # undirected: Borgatti's k - 2t/k, unchanged
  expect_equal(unname(centrality_effective_size(path3)), c(1, 2, 1))
})

test_that("VoteRank on a directed network votes for in-neighbours (Zhang et al. 2016)", {
  # NetworkX's docstring example DiGraph([(0, 1), (2, 1), (2, 3), (3, 4)]) elects [2, 3]: here C, then D
  g <- adjacency(rbind(c("A", "B"), c("C", "B"), c("C", "D"), c("D", "E")), c("A", "B", "C", "D", "E"),
                 directed = TRUE)
  expect_equal(unname(centrality_voterank(g)), c(0.6, 0.4, 1, 0.8, 0.2))   # was B first
})

test_that("Laplacian centrality reads the weights (Qi et al. 2012)", {
  w <- matrix(c(0, 2, 0, 2, 0, 1, 0, 1, 0), 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  # strengths 2, 3, 1; energy 24; removing A, B, C leaves 4, 0, 16
  expect_equal(unname(centrality_laplacian(w)), c(20, 24, 8))
  expect_equal(unname(centrality_laplacian(w, weighted = FALSE)), c(1 + 1 + 2 * 2, 4 + 2 + 2 * 2, 1 + 1 + 2 * 2))
})

test_that("bottleneck compares with a quarter of the nodes reached (Przulj et al. 2004)", {
  # A -> B -> C and three isolated nodes. From A (|T| = 3) B and C lie on more than 3/4 paths; from B (|T| = 2)
  # C on more than 1/2. With n / 4 = 1.5 only B counted from A (was C 0).
  g <- adjacency(rbind(c("A", "B"), c("B", "C")), c("A", "B", "C", "D", "E", "F"), directed = TRUE)
  expect_equal(unname(centrality_bottleneck(g, mode = "out")[c("B", "C")]), c(1, 2))
})

test_that("eigenvector centralization divides by n - 2 on an undirected network (Freeman 1979)", {
  star <- adjacency(rbind(c("A", "B"), c("A", "C"), c("A", "D")), c("A", "B", "C", "D"))
  expect_equal(centralization(star, "eigenvector"), (3 - sqrt(3)) / 2, tolerance = 1e-8)   # was (3 - sqrt(3)) / 3
})

test_that("network_summary degree centralization is Freeman's, without loops", {
  skip_if_not_installed("igraph")
  star <- adjacency(rbind(c("A", "B"), c("A", "C"), c("A", "D")), c("A", "B", "C", "D"))
  expect_equal(network_summary(star, digits = NULL)$centralization_degree, 1)   # was 0.5
})

test_that("network_local_efficiency measures the induced subgraph of the neighbours (Latora and Marchiori 2001)", {
  skip_if_not_installed("igraph")
  cycle4 <- adjacency(rbind(c("A", "B"), c("B", "C"), c("C", "D"), c("D", "A")), c("A", "B", "C", "D"))
  expect_equal(network_local_efficiency(cycle4), 0)   # was 0.5, igraph's form through the fourth node
})

test_that("VoteRank counts votes exactly, so a tie elects the first node", {
  # snajs fixture graph 0027 (8 nodes, directed). Replayed in exact fractions (Python Fraction, the paper's
  # directed rule) the election order is E, A, G, H, B, D, C, F: C and F tie in round 7 and C, listed first, is
  # elected. Summed in floating point the two totals rounded apart and F was elected (2.7.19 and earlier, and
  # networkx.voterank()).
  rows <- list(c(0, 1, 1, 1, 0, 1, 1, 1), c(1, 0, 1, 1, 1, 1, 1, 0), c(1, 0, 0, 1, 1, 0, 1, 1),
               c(1, 0, 1, 0, 1, 1, 1, 1), c(1, 1, 1, 1, 0, 1, 1, 1), c(0, 1, 1, 0, 1, 0, 1, 1),
               c(1, 1, 1, 1, 1, 1, 0, 1), c(1, 1, 1, 1, 1, 1, 0, 0))
  g <- matrix(unlist(rows), 8, byrow = TRUE, dimnames = list(LETTERS[1:8], LETTERS[1:8]))
  expect_equal(unname(centrality_voterank(g)), c(7, 4, 2, 3, 8, 1, 6, 5) / 8)
})
