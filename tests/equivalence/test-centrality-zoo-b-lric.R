# Long-range interaction centrality with the maximal path, centrality_lric_max() (Aleskerov, Meshcheryakova and
# Shvydun 2017, "Power in network structures"; read in its working-paper version, arXiv:1610.05892). Definitions 1-2
# and equation (1), pp. 16-18: the direct influence c_ij is w_ij over the smallest critical group of j in which i is
# pivotal. Equations (2) and (5), pp. 20-21: the influence along a path is the product of its direct influences and
# the total influence c*_ij that of the best simple path. Page 27: LRIC(i) = sum_j c*_ij u_j, u_j the share of all
# weight that leaves j. Compared with Table 15 of the working paper (Numerical Example 2, Fig. 4) and with the
# definitions applied literally. The authors' program (slric) limits paths to three arcs by default and was not run.

independent <- "independent: closed forms and hand derivations from the definition"
aleskerov_table_15 <- "published: Aleskerov, Meshcheryakova and Shvydun, arXiv:1610.05892, Table 15 (MaxPath column)"
exact <- 1e-10
# Three printed decimals: half a unit in the last printed place.
printed_3dp <- 5e-4

lric_arcs <- function(n, arcs) {
  graph <- matrix(0, n, n, dimnames = list(as.character(seq_len(n)), as.character(seq_len(n))))
  graph[arcs[, 1:2, drop = FALSE]] <- arcs[, 3L]
  graph
}

# Fig. 4: 10 nodes and 27 weighted arcs (from, to, weight).
lric_example <- function() {
  lric_arcs(10L, matrix(c(6, 1, 125, 6, 2, 30, 2, 1, 200, 4, 1, 375, 3, 1, 300, 4, 6, 490, 7, 6, 510, 1, 8, 250,
                          1, 9, 490, 1, 10, 490, 1, 7, 300, 2, 7, 510, 7, 8, 510, 8, 2, 485, 5, 2, 485, 4, 8, 240,
                          7, 9, 510, 7, 4, 250, 9, 4, 250, 5, 4, 250, 10, 4, 250, 4, 3, 200, 5, 3, 300, 9, 3, 200,
                          10, 3, 300, 3, 7, 190, 7, 10, 510), ncol = 3L, byrow = TRUE))
}

# Definitions 1-2 and equation (1) for every subset of the in-neighbours, then the best walk of at most n - 1 arcs
# by a max-product recursion written with explicit candidate matrices.
lric_definition <- function(a, quota = 0.5) {
  a <- unname(a)
  n <- nrow(a)
  direct <- vapply(seq_len(n), \(j) {
    sources <- which(a[, j] > 0)
    need <- quota * sum(a[sources, j])
    vapply(seq_len(n), \(i) {
      if (!(i %in% sources)) return(0)
      others <- setdiff(sources, i)
      groups <- lapply(seq_len(2^length(others)) - 1L,
                       \(code) others[as.logical(bitwAnd(code, 2^(seq_along(others) - 1L)))])
      rest <- vapply(groups, \(group) sum(a[group, j]), numeric(1))
      ok <- rest + a[i, j] >= need - 1e-9 & rest < need - 1e-9
      if (any(ok)) a[i, j] / (a[i, j] + min(rest[ok])) else 0
    }, numeric(1))
  }, numeric(n))
  best <- Reduce(\(current, step) {
    candidate <- outer(seq_len(n), seq_len(n), Vectorize(\(i, j) max(current[i, ] * direct[, j])))
    updated <- pmax(current, candidate)
    diag(updated) <- 0
    updated
  }, seq_len(n - 2L), direct)
  as.numeric(best %*% (rowSums(a) / sum(a)))
}

test_that("lric_max reproduces the MaxPath column of Table 15", {
  printed <- c(0, 0.610, 0.102, 0.149, 0.655, 0.078, 0.426, 0.598, 0.158, 0.169)
  expect_reference(centrality_lric_max(lric_example()), stats::setNames(printed, 1:10),
                   reference = aleskerov_table_15, covers = c("centrality()", "centrality_lric_max()"),
                   quantity = "lric_max, quota 0.5, all paths, Numerical Example 2", tolerance = printed_3dp)
})

test_that("lric_max equals Definitions 1-2 and equations (1), (2), (5) applied literally", {
  weighted <- ref_connected_graph(8, density = 0.4, seed = 61L, directed = TRUE, weighted = TRUE, digits = 1L)
  undirected <- ref_connected_graph(8, density = 0.4, seed = 62L)
  graphs <- list(`Numerical Example 2` = lric_example(), `weighted directed 8 nodes` = weighted,
                 `unweighted 8 nodes` = undirected)
  Map(\(graph, name) {
    expect_reference(centrality_lric_max(graph), ref_named(lric_definition(graph), graph), reference = independent,
                     covers = c("centrality()", "centrality_lric_max()"),
                     quantity = sprintf("lric_max, literal definition, %s", name), tolerance = 1e-9)
  }, graphs, names(graphs))
  expect_reference(centrality_lric_max(weighted, lric_quota = 0.7), ref_named(lric_definition(weighted, 0.7), weighted),
                   reference = independent, covers = c("centrality()", "centrality_lric_max()"),
                   quantity = "lric_max, quota 0.7, weighted directed 8 nodes", tolerance = 1e-9)
})

test_that("lric_max on a chain is the share of weight leaving the nodes downstream", {
  # 1 -> 2 -> 3 -> 4, unit weights: every arc is the only one into its target, so every influence is 1.
  chain <- lric_arcs(4L, rbind(c(1, 2, 1), c(2, 3, 1), c(3, 4, 1)))
  expect_reference(centrality_lric_max(chain), c(`1` = 2 / 3, `2` = 1 / 3, `3` = 0, `4` = 0), reference = independent,
                   covers = c("centrality()", "centrality_lric_max()"), quantity = "lric_max, chain of 4",
                   tolerance = exact)
})
