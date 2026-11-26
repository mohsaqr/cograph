# Batch 52 --- improved neighbors' k-core (Lin, Guo, Dong, Tang and Liu
# 2014, Physics Letters A 378(45):3279-3284), equation (1), page 3280:
# S(i) = sum_{j in J(i)} k_s(j)^alpha. Comparisons with NetworkX and igraph
# are in tests/equivalence/test-centrality-zoo-b-ink.R.

ink_from_edges <- function(n, el) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  a[el[, 2:1]] <- 1
  a
}

# K4 on nodes 1-4 (the whole 3-core), a pendant on node 2, a path 3-6-7 and
# the isolate 8.
ink_fixture <- function() {
  ink_from_edges(8L, rbind(c(1, 2), c(1, 3), c(1, 4), c(2, 3), c(2, 4),
                           c(3, 4), c(2, 5), c(3, 6), c(6, 7)))
}

# Equation (1) written without a matrix product, on core numbers found by
# deleting nodes of degree below k until none is left, for each k in turn.
ink_definition <- function(a, alpha) {
  n <- nrow(a)
  core_at <- function(k) {
    alive <- rep(TRUE, n)
    repeat {
      drop <- alive & as.numeric(a %*% alive) < k
      if (!any(drop)) break
      alive[drop] <- FALSE
    }
    alive
  }
  member <- vapply(seq_len(n), core_at, logical(n))
  core <- rowSums(matrix(member, n, n))
  vapply(seq_len(n), \(i) sum(core[a[i, ] != 0]^alpha), numeric(1))
}

test_that("ink is the hand-computed neighbour sum of core numbers", {
  a <- ink_fixture()
  # Core numbers: 3 3 3 3 1 1 1 0.
  expect_equal(unname(centrality_ink(a)), c(9, 10, 10, 9, 3, 4, 1, 0))
  # alpha = 2: K4 neighbours contribute 9 each.
  expect_equal(unname(centrality_ink(a, ink_alpha = 2)),
               c(27, 28, 28, 27, 9, 10, 1, 0))
})

test_that("the node's own core number is not counted", {
  # Lin et al.'s Fig. 1 prints S(2) = 10 for a node of the K4 3-core; a
  # closed neighbourhood would make every such score at least 12.
  k4 <- ink_from_edges(4L, t(utils::combn(4L, 2L)))
  expect_equal(unname(centrality_ink(k4)), rep(9, 4))
})

test_that("alpha = 0 gives the degree and isolates score zero", {
  a <- ink_fixture()
  expect_equal(unname(centrality_ink(a, ink_alpha = 0)), unname(rowSums(a)))
  expect_identical(unname(centrality_ink(a)[8]), 0)
})

test_that("ink matches the definition on random graphs for several alpha", {
  set.seed(52)
  graphs <- lapply(c(6L, 11L, 17L), \(n) {
    upper <- matrix(stats::runif(n * n) < 0.3, n, n)
    upper[lower.tri(upper, diag = TRUE)] <- FALSE
    a <- (upper | t(upper)) * 1
    dimnames(a) <- list(seq_len(n), seq_len(n))
    a
  })
  lapply(graphs, \(a) {
    lapply(c(0, 0.7, 1, 2.4, 5), \(alpha) {
      expect_equal(unname(centrality_ink(a, ink_alpha = alpha)),
                   ink_definition(a, alpha), tolerance = 1e-12)
    })
  })
})

test_that("ink is permutation equivariant", {
  set.seed(521)
  a <- ink_fixture()
  perm <- sample.int(nrow(a))
  expect_equal(unname(centrality_ink(a[perm, perm], ink_alpha = 2.4)),
               unname(centrality_ink(a, ink_alpha = 2.4))[perm])
})

test_that("ink never decreases in alpha and is unchanged by a new component", {
  a <- ink_fixture()
  # Every neighbour's core number is at least one, so each term grows.
  expect_true(all(centrality_ink(a, ink_alpha = 2) >=
                    centrality_ink(a, ink_alpha = 1)))
  joined <- rbind(cbind(a, matrix(0, 8L, 3L)),
                  cbind(matrix(0, 3L, 8L), 1 - diag(3L)))
  dimnames(joined) <- list(seq_len(11L), seq_len(11L))
  expect_equal(unname(centrality_ink(joined))[seq_len(8L)],
               unname(centrality_ink(a)))
})

test_that("direction, weights and loops are ignored", {
  a <- ink_fixture()
  arcs <- a
  arcs[lower.tri(arcs)] <- 0
  expect_equal(centrality_ink(arcs), centrality_ink(a))
  expect_equal(centrality_ink(a * 4.5), centrality_ink(a))
  looped <- a
  diag(looped) <- 1
  expect_equal(centrality_ink(looped), centrality_ink(a))
})

test_that("normalized = TRUE divides by the maximum", {
  a <- ink_fixture()
  raw <- centrality_ink(a)
  expect_equal(centrality_ink(a, normalized = TRUE), raw / max(raw))
})

test_that("ink_alpha outside its domain raises cograph_bad_parameter", {
  a <- ink_fixture()
  expect_error(centrality_ink(a, ink_alpha = -1),
               class = "cograph_bad_parameter")
  expect_error(centrality_ink(a, ink_alpha = c(1, 2)),
               class = "cograph_bad_parameter")
  expect_error(centrality_ink(a, ink_alpha = NA_real_),
               class = "cograph_bad_parameter")
  expect_error(centrality_ink(a, ink_alpha = "1"),
               class = "cograph_bad_parameter")
  expect_error(centrality_ink(a, ink_alpha = Inf),
               class = "cograph_bad_parameter")
  # 3^2000 is not a finite double.
  expect_error(centrality_ink(a, ink_alpha = 2000),
               class = "cograph_bad_parameter")
})

test_that("ink is registered and reachable through centrality()", {
  a <- ink_fixture()
  listed <- list_centralities()
  expect_true("ink" %in% listed$measure)
  out <- centrality(a, measures = "ink")
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("node", "ink"))
  expect_equal(out$ink, unname(centrality_ink(a)))
})

test_that("ink handles the empty-edge and single-node networks", {
  lone <- matrix(0, 1L, 1L, dimnames = list("a", "a"))
  expect_identical(unname(centrality_ink(lone)), 0)
  empty <- matrix(0, 3L, 3L, dimnames = list(letters[1:3], letters[1:3]))
  expect_identical(unname(centrality_ink(empty)), c(0, 0, 0))
})
