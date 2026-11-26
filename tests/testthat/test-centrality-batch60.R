# Batch 60 --- weighted formal concept analysis centrality (Sun, Wang,
# Sheng, Hu, Wang and Shao 2017, IEEE Access 5:3777-3789), Algorithm 1 and
# formula (3), page 3782. The paper's printed weights are compared in
# tests/equivalence/test-centrality-zoo-b-wfca.R.

wfca_from_edges <- function(n, el, directed = FALSE) {
  a <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))
  a[el] <- 1
  if (!directed) a[el[, 2:1, drop = FALSE]] <- 1
  a
}

# Every formal concept from the definition: the common attributes of every
# subset of objects, closed back to its extent.
wfca_definition <- function(a) {
  n <- nrow(a)
  has <- a != 0
  subsets <- lapply(seq_len(2^n) - 1L,
                    \(code) as.logical(bitwAnd(code, 2^(seq_len(n) - 1L))))
  intents <- unique(t(vapply(subsets, \(objects) {
    if (any(objects)) colSums(!has[objects, , drop = FALSE]) == 0 else {
      rep(TRUE, n)
    }
  }, logical(n))))
  score <- numeric(n)
  lapply(seq_len(nrow(intents)), \(k) {
    intent <- intents[k, ]
    if (!any(intent)) return(NULL)
    extent <- sum(rowSums(!has[, intent, drop = FALSE]) == 0)
    score[intent] <<- score[intent] + extent / sum(intent)
  })
  score
}

test_that("wfca on a star, by hand", {
  # Three leaves. Concepts with a non-empty intent: ({leaves}, {hub}) gives
  # the hub 3/1; ({hub}, {leaves}) gives each leaf 1/3.
  star <- wfca_from_edges(4L, cbind(1L, 2:4))
  expect_equal(unname(centrality_wfca(star)), c(3, rep(1 / 3, 3)))
})

test_that("wfca on a triangle, by hand", {
  # Rows {2,3}, {1,3}, {1,2}. Intents: each pair (one object, 1/2 to each
  # of two nodes) and each single node (two objects, 2/1): every node gets
  # 2 + 1/2 + 1/2.
  triangle <- wfca_from_edges(3L, rbind(c(1, 2), c(2, 3), c(1, 3)))
  expect_equal(unname(centrality_wfca(triangle)), rep(3, 3))
})

test_that("wfca uses direction", {
  # 1 -> 3 and 2 -> 3: node 3 is the common target of two objects.
  arcs <- wfca_from_edges(3L, rbind(c(1, 3), c(2, 3)), directed = TRUE)
  expect_equal(unname(centrality_wfca(arcs)), c(0, 0, 2))
})

test_that("wfca matches the definition on random graphs", {
  set.seed(60)
  lapply(c(5L, 7L, 9L), \(n) {
    a <- (matrix(stats::runif(n * n) < 0.35, n, n)) * 1
    diag(a) <- 0
    dimnames(a) <- list(seq_len(n), seq_len(n))
    expect_equal(unname(centrality_wfca(a)), wfca_definition(a),
                 tolerance = 1e-12)
    sym <- (a + t(a) > 0) * 1
    expect_equal(unname(centrality_wfca(sym)), wfca_definition(sym),
                 tolerance = 1e-12)
  })
})

test_that("wfca: isolates score 0; weights are ignored; equivariance", {
  set.seed(601)
  star <- wfca_from_edges(5L, cbind(1L, 2:4))
  expect_identical(unname(centrality_wfca(star)[5]), 0)
  expect_equal(centrality_wfca(star * 2.5), centrality_wfca(star))
  ring <- wfca_from_edges(6L, cbind(1:6, c(2:6, 1L)))
  ring[1, 4] <- ring[4, 1] <- 1
  perm <- sample.int(6L)
  expect_equal(unname(centrality_wfca(ring[perm, perm])),
               unname(centrality_wfca(ring))[perm])
})

test_that("wfca stops at the enumeration limit and is registered as costly", {
  set.seed(602)
  a <- (matrix(stats::runif(144) < 0.5, 12L, 12L)) * 1
  diag(a) <- 0
  dimnames(a) <- list(seq_len(12L), seq_len(12L))
  expect_warning(out <- centrality_wfca(a, enumeration_limit = 5),
                 class = "cograph_undefined_measure")
  expect_true(all(is.na(out)))
  expect_false(anyNA(centrality_wfca(a)))
  row <- list_centralities()
  row <- row[row$measure == "wfca", ]
  expect_identical(nrow(row), 1L)
  expect_true(row$costly)
})
