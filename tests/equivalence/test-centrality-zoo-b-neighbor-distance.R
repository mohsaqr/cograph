# centrality_neighbor_distance(): the neighbor distance centrality of Liu, Tang, Zhou and Do (2016, Physica A
# 452, 289-298; read as the preprint arXiv:1511.00441v1). The paper prints no table of node scores, so the
# measure is compared with its equation (1) written out below in base R, and with closed forms.
#
# Definition (eq. 1): C_i = theta_i + sum_{k = 1..n} a^k sum over the non-backtracking walks of length k that
# start at i of theta at the walk's end, once per walk; theta is the degree (or the k-shell index), a = 0.2 and
# n = 2 by default. A walk is non-backtracking when no step returns along the edge it just used.
#
# The walks are counted with the state "last edge used": S_1 holds the walks of length one leaving i (row i of
# A); S_(k+1)[c, d] = sum_p S_k[p, c] A[c, d] [d != p] = colSums(S_k)[c] A[c, d] - S_k[d, c] A[c, d]. The walks
# of length k end at node d colSums(S_k)[d] times.

nd_definition <- function(graph, order = 2L, decay = 0.2, theta = rowSums(graph)) {
  n <- nrow(graph)
  score <- vapply(seq_len(n), \(i) {
    start <- matrix(0, n, n)
    start[i, ] <- graph[i, ]
    extend <- \(state) diag(colSums(state), n) %*% graph - t(state) * graph
    states <- Reduce(\(walks, step) c(walks, list(extend(walks[[length(walks)]]))),
                     seq_len(max(order - 1L, 0L)), list(start))
    ends <- vapply(states[seq_len(order)], \(state) sum(colSums(state) * theta), numeric(1))
    theta[[i]] + sum(decay^seq_len(order) * ends)
  }, numeric(1))
  stats::setNames(score, rownames(graph))
}

# The k-shell index by peeling (Seidman 1983): remove every node of degree at most k, repeat, then raise k.
nd_kshell <- function(graph) {
  peel <- function(alive, shell, k) {
    if (!any(alive)) return(shell)
    degree <- rowSums(graph[, alive, drop = FALSE])
    low <- alive & degree <= k
    if (any(low)) peel(alive & !low, replace(shell, low, k), k) else peel(alive, shell, k + 1)
  }
  peel(rep(TRUE, nrow(graph)), numeric(nrow(graph)), 0)
}

independent <- "independent: closed forms and hand derivations from the definition"
exact <- 1e-10

test_that("neighbor distance equals equation (1) written out", {
  graphs <- list(`triangle with a pendant` = zoob_graph(4L, c(1, 2, 1, 3, 2, 3, 1, 4)), `ring of 4` = zoob_ring(4L),
                 kite = zoob_kite(), karate = zoob_karate())
  Map(\(graph, name) {
    expect_reference(centrality_neighbor_distance(graph), nd_definition(graph), reference = independent,
                     covers = c("centrality()", "centrality_neighbor_distance()"),
                     quantity = paste0("neighbor_distance, degree, order 2, ", name), tolerance = exact,
                     measure = "relative")
  }, graphs, names(graphs))
  kite <- zoob_kite()
  expect_reference(centrality_neighbor_distance(kite, nd_order = 4, nd_decay = 0.5),
                   nd_definition(kite, order = 4L, decay = 0.5), reference = independent,
                   covers = c("centrality()", "centrality_neighbor_distance()"),
                   quantity = "neighbor_distance, degree, order 4, decay 0.5, kite", tolerance = exact, measure = "relative")
  expect_reference(centrality_neighbor_distance(kite, nd_mass = "coreness", nd_order = 3),
                   nd_definition(kite, order = 3L, theta = nd_kshell(kite)), reference = independent,
                   covers = c("centrality()", "centrality_neighbor_distance()"),
                   quantity = "neighbor_distance, coreness, order 3, kite", tolerance = exact, measure = "relative")
  # The source's parameters at their edges: order 0 and decay 0 return theta itself, decay 1 counts every walk
  # in full, and a negative decay (outside the source's [0, 1]) is accepted.
  path <- zoob_path(4L)
  paw <- zoob_graph(4L, c(1, 2, 1, 3, 2, 3, 1, 4))
  settings <- list(
    `path of 4, order 3` = list(graph = path, args = list(nd_order = 3), order = 3L, decay = 0.2, theta = rowSums(path)),
    `path of 4, order 0` = list(graph = path, args = list(nd_order = 0), order = 0L, decay = 0.2, theta = rowSums(path)),
    `path of 4, decay 0` = list(graph = path, args = list(nd_decay = 0), order = 2L, decay = 0, theta = rowSums(path)),
    `path of 4, coreness, order 0` = list(graph = path, args = list(nd_order = 0, nd_mass = "coreness"), order = 0L,
                                          decay = 0.2, theta = nd_kshell(path)),
    `triangle with a pendant, decay 1` = list(graph = paw, args = list(nd_decay = 1), order = 2L, decay = 1,
                                              theta = rowSums(paw)),
    `triangle with a pendant, decay 0.5, order 1` = list(graph = paw, args = list(nd_decay = 0.5, nd_order = 1),
                                                         order = 1L, decay = 0.5, theta = rowSums(paw)),
    `triangle with a pendant, coreness, decay -0.5, order 3` = list(
      graph = paw, args = list(nd_decay = -0.5, nd_order = 3, nd_mass = "coreness"), order = 3L, decay = -0.5,
      theta = nd_kshell(paw)))
  Map(\(setting, name) {
    expect_reference(do.call(centrality_neighbor_distance, c(list(setting$graph), setting$args)),
                     nd_definition(setting$graph, order = setting$order, decay = setting$decay, theta = setting$theta),
                     reference = independent, covers = c("centrality()", "centrality_neighbor_distance()"),
                     quantity = paste0("neighbor_distance, ", name), tolerance = exact)
  }, settings, names(settings))
})

test_that("neighbor distance equals its closed forms", {
  # Triangle with a pendant, node 1 by hand: theta = 3, the neighbours' degrees sum to 5, and the two
  # non-backtracking two-walks 1-2-3 and 1-3-2 end on degree 2.
  expect_reference(centrality_neighbor_distance(zoob_graph(4L, c(1, 2, 1, 3, 2, 3, 1, 4)))[1],
                   c(`1` = 3 + 0.2 * (2 + 2 + 1) + 0.04 * (2 + 2)), reference = independent,
                   covers = c("centrality()", "centrality_neighbor_distance()"),
                   quantity = "neighbor_distance, triangle with a pendant, node 1", tolerance = exact)
  # A cycle has exactly two non-backtracking walks of every length, each ending on degree 2.
  expect_reference(centrality_neighbor_distance(zoob_ring(6L), nd_order = 5), rep(2 + 4 * sum(0.2^(1:5)), 6),
                   reference = independent, covers = c("centrality()", "centrality_neighbor_distance()"),
                   quantity = "neighbor_distance, ring of 6, order 5", tolerance = exact)
  # A clique: each step has m - 2 continuations and every endpoint has degree m - 1 (also its core number).
  steps <- seq_len(3)
  expect_reference(centrality_neighbor_distance(zoob_complete(5L), nd_order = 3),
                   rep(4 + sum(0.2^steps * 4^2 * 3^(steps - 1)), 5), reference = independent,
                   covers = c("centrality()", "centrality_neighbor_distance()"),
                   quantity = "neighbor_distance, complete graph of 5, order 3", tolerance = exact)
  # A q-star: the centre's walks die after one step; a leaf reaches the other q - 1 leaves in two.
  expect_reference(centrality_neighbor_distance(zoob_star(5L)), c(1.2 * 5, rep(1 + 0.2 * 5 + 0.04 * 4, 5)),
                   reference = independent, covers = c("centrality()", "centrality_neighbor_distance()"),
                   quantity = "neighbor_distance, star of 5 leaves", tolerance = exact)
  expect_reference(centrality_neighbor_distance(zoob_star(5L), nd_mass = "coreness"),
                   c(1 + 0.2 * 5, rep(1.2 + 0.04 * 4, 5)), reference = independent,
                   covers = c("centrality()", "centrality_neighbor_distance()"),
                   quantity = "neighbor_distance, coreness, star of 5 leaves", tolerance = exact)
})
