# centrality_relative_entropy(): the relative-entropy integrated evaluation (Chen, Wang and Luo 2016, Journal of
# Systems Engineering and Electronics 27(6), 1219-1226). Compared with the paper's Table 2 for its Kite graph,
# and with the integration written out below in base R from the paper's equations.
#
# Definitions (Chen 2016): an index C becomes a distribution over the n nodes, u_i = C_i / sum C (eq. 8, a
# positive index) or u_i = (1 - C_i / sum C) / (n - 1) (eq. 9, a negative index: constraint and the largest
# remaining component by the paper's declaration). The score is the normalised geometric mean of the m
# distributions (eq. 11): w_i = prod_j u_ji^(1/m) / sum_l prod_j u_jl^(1/m). The indexes written out here:
# degree; closeness (eq. 3) = 1 / sum of hop distances, over the reachable partners (cograph's extension for a
# disconnected graph; 0 for an isolate); constraint (eq. 6) = sum over every other node j of
# (p_ij + sum_q p_iq p_qj)^2 with p_ij = a_ij / k_i (0 for an isolate).

re_distances <- function(graph) {
  n <- nrow(graph)
  reach <- Reduce(\(walks, step) walks %*% graph, seq_len(n - 1L), diag(n), accumulate = TRUE)
  first <- vapply(seq_len(n * n), \(cell) {
    hit <- which(vapply(reach, \(m) m[cell] > 0, logical(1)))
    if (length(hit)) hit[[1L]] - 1 else Inf
  }, numeric(1))
  matrix(first, n, n)
}

re_index <- function(graph, index) {
  degree <- rowSums(graph)
  switch(index,
         degree = degree,
         closeness = {
           distance <- re_distances(graph)
           total <- rowSums(ifelse(is.finite(distance), distance, 0))
           ifelse(total > 0, 1 / total, 0)
         },
         constraint = {
           share <- graph / ifelse(degree > 0, degree, 1)
           both <- share + share %*% share
           diag(both) <- 0
           rowSums(both^2)
         })
}

re_definition <- function(graph, indexes, negative = intersect(indexes, "constraint")) {
  n <- nrow(graph)
  distributions <- vapply(indexes, \(index) {
    values <- re_index(graph, index)
    if (index %in% negative) (1 - values / sum(values)) / (n - 1) else values / sum(values)
  }, numeric(n))
  mean_log <- rowMeans(log(matrix(distributions, nrow = n)))
  stats::setNames(exp(mean_log) / sum(exp(mean_log)), rownames(graph))
}

# The Kite of Chen et al. 2016, numbered as the paper's figure.
chen_kite <- function() {
  zoob_graph(10L, c(9, 10, 9, 8, 9, 4, 9, 7, 10, 6, 10, 5, 10, 7, 8, 7, 8, 4, 6, 7, 6, 5, 7, 4, 7, 5, 4, 5, 4, 3, 5, 3,
                    3, 2, 2, 1))
}

independent <- "independent: closed forms and hand derivations from the definition"
exact <- 1e-10

test_that("relative entropy and Chen, Wang and Luo (2016) Table 2", {
  kite <- chen_kite()
  # Table 2 prints six decimals: half a unit in the last place is 5e-7, absolute.
  # The two component indexes are integers, which Table 1 prints exactly, so that column is exact.
  expect_reference(centrality_relative_entropy(kite, re_indexes = c("n_components", "largest_component")),
                   c(0.092119, 0.131109, 0.131936, rep(0.092119, 7)),
                   reference = "published: Chen, Wang and Luo 2016, Table 2",
                   covers = c("centrality()", "centrality_relative_entropy()"),
                   quantity = "relative_entropy, Kite, two component indexes", tolerance = 5e-7)
  sets <- list(`four indexes` = c("degree", "closeness", "betweenness", "constraint"),
               `all six indexes` = c("degree", "closeness", "betweenness", "constraint", "n_components",
                                     "largest_component"))
  printed <- list(c(0, 0.127939, 0.177613, 0.180540, 0.180540, 0, 0.151213, 0, 0.091078, 0.091078),
                  c(0, 0.145025, 0.180857, 0.162205, 0.162205, 0, 0.144126, 0, 0.102791, 0.102791))
  Map(\(indexes, values, name) {
    expect_known_difference(centrality_relative_entropy(kite, re_indexes = indexes), values,
                            reference = "published: Chen, Wang and Luo 2016, Table 2",
                            covers = c("centrality()", "centrality_relative_entropy()"),
                            quantity = paste0("relative_entropy, Kite, ", name), tolerance = 5e-7,
                            reason = paste("The paper integrated its own rounded Table 1 and Table 3 columns (indexes",
                                           "printed to 2-5 decimals), so its Table 2 departs from the exact graph by",
                                           "up to about 4e-5; recomputed from the printed columns it agrees to 5e-7",
                                           "(a unit test, as it needs cograph's internal index maps)."))
  }, sets, printed, names(sets))
})

test_that("relative entropy equals the integration written out from the definition", {
  graphs <- list(`Kite of Chen 2016` = chen_kite(), kite = zoob_kite(), karate = zoob_karate(),
                 `kite with an isolate` = zoob_union(zoob_kite(), zoob_graph(1L, matrix(numeric(), 0, 2))))
  indexes <- c("degree", "closeness", "constraint")
  Map(\(graph, name) {
    expect_reference(centrality_relative_entropy(graph, re_indexes = indexes), re_definition(graph, indexes),
                     reference = independent, covers = c("centrality()", "centrality_relative_entropy()"),
                     quantity = paste0("relative_entropy, degree closeness constraint, ", name), tolerance = exact)
  }, graphs, names(graphs))
  # Constraint declared positive inverts the reading (re_negative = character()).
  path <- zoob_path(5L)
  expect_reference(centrality_relative_entropy(path, re_indexes = "constraint", re_negative = character()),
                   re_definition(path, "constraint", negative = character()), reference = independent,
                   covers = c("centrality()", "centrality_relative_entropy()"),
                   quantity = "relative_entropy, constraint as a positive index, path of 5", tolerance = exact)
})

test_that("relative entropy equals its closed forms", {
  # Path of five: degree 1, 2, 2, 2, 1 gives eq. (8) directly; constraint 5/4, 9/16, 5/8, 9/16, 5/4 (sum 17/4)
  # as a negative index gives (1 - C / (17/4)) / 4.
  path <- zoob_path(5L)
  expect_reference(centrality_relative_entropy(path, re_indexes = "degree"), c(1, 2, 2, 2, 1) / 8,
                   reference = independent, covers = c("centrality()", "centrality_relative_entropy()"),
                   quantity = "relative_entropy, degree, path of 5", tolerance = exact)
  expect_reference(centrality_relative_entropy(path, re_indexes = "constraint"),
                   c(3 / 17, 59 / 272, 29 / 136, 59 / 272, 3 / 17), reference = independent,
                   covers = c("centrality()", "centrality_relative_entropy()"),
                   quantity = "relative_entropy, constraint, path of 5", tolerance = exact)
  # A star: leaves carry no shortest path, so betweenness zeroes them and the centre takes everything; with
  # degree and closeness only, the centre's share over a leaf's is sqrt(2q - 1).
  expect_reference(centrality_relative_entropy(zoob_star(5L)), c(1, rep(0, 5)), reference = independent,
                   covers = c("centrality()", "centrality_relative_entropy()"),
                   quantity = "relative_entropy, star of 5 leaves", tolerance = exact)
  leaf <- 1 / (5 + sqrt(9))
  expect_reference(centrality_relative_entropy(zoob_star(5L), re_indexes = c("degree", "closeness")),
                   c(sqrt(9) * leaf, rep(leaf, 5)), reference = independent,
                   covers = c("centrality()", "centrality_relative_entropy()"),
                   quantity = "relative_entropy, degree and closeness, star of 5 leaves", tolerance = exact)
  # A vertex-transitive graph: every index is constant, so the score is uniform.
  expect_reference(centrality_relative_entropy(zoob_ring(7L)), rep(1 / 7, 7), reference = independent,
                   covers = c("centrality()", "centrality_relative_entropy()"),
                   quantity = "relative_entropy, ring of 7", tolerance = exact)
  # Two three-node paths: reachable-partner closeness is 1/3 at an end and 1/2 at a centre.
  end <- 1 / (4 + 2 * sqrt(3))
  expect_reference(centrality_relative_entropy(zoob_union(zoob_path(3L), zoob_path(3L)),
                                               re_indexes = c("degree", "closeness")),
                   c(end, sqrt(3) * end, end, end, sqrt(3) * end, end), reference = independent,
                   covers = c("centrality()", "centrality_relative_entropy()"),
                   quantity = "relative_entropy, degree and closeness, two paths of 3", tolerance = exact)
})
