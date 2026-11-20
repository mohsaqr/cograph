# Map equation centrality (Blocker, Nieves and Rosvall 2022) through centrality_map_equation() (a wrapper of
# centrality(measures = "map_equation")). The score of node i is the code length saved when i is silenced,
# -(s - p_i) log2((s - p_i) / s), with p_i the node's visit rate and s the use rate of its module's codebook.
# The paper's equation 2 counts module exits in s (map_convention = "paper", the default); Infomap and the
# paper's Table I count node visits only (map_convention = "infomap"). Both conventions are compared: with the
# printed Table I and Figure 2, and with the formula written out where the flow is known in closed form (on an
# undirected graph without teleportation the visit rate of a node is its degree over 2m, and each arc carries
# 1 / 2m).

# The toy network of the paper's Figure 3 / Table I (ten edges, eight nodes).
blocker_graph <- zoob_graph(8L, c(1, 2, 1, 3, 1, 4, 2, 4, 3, 4, 4, 5, 5, 6, 5, 7, 5, 8, 6, 7))
table_one_partitions <- list(one = rep(1, 8), suboptimal = c(rep(1, 3), rep(2, 5)), optimal = rep(1:2, each = 4))

# The silenced-code saving of the paper, from visit rates p and codebook rates s.
saving <- function(p, s) -(s - p) * log2((s - p) / s)

test_that("map equation (Infomap convention) equals every entry of Blocker et al.'s Table I", {
  # The first two columns print two decimals (half a unit 0.005), the third three (0.0005), absolute.
  printed <- list(one = c(.20, .14, .14, .26, .26, .14, .14, .07),
                  suboptimal = c(.16, .12, .12, .24, .24, .13, .13, .07),
                  optimal = c(.184, .130, .130, .228, .212, .127, .127, .068))
  half_unit <- c(one = 0.005, suboptimal = 0.005, optimal = 0.0005)
  lapply(names(printed), \(column) {
    expect_reference(centrality_map_equation(blocker_graph, membership = table_one_partitions[[column]],
                                             map_convention = "infomap"),
                     printed[[column]], reference = "published: Blocker, Nieves and Rosvall 2022, Table I",
                     covers = c("centrality()", "centrality_map_equation()"),
                     quantity = sprintf("map_equation, infomap convention, Table I, %s partition", column),
                     tolerance = half_unit[[column]])
  })
  # With exit flow counted (the default), the two-module columns move off the printed values.
  lapply(c("suboptimal", "optimal"), \(column) {
    expect_known_difference(centrality_map_equation(blocker_graph, membership = table_one_partitions[[column]]),
                            printed[[column]], reference = "published: Blocker, Nieves and Rosvall 2022, Table I",
                            covers = c("centrality()", "centrality_map_equation()"),
                            quantity = sprintf("map_equation, paper convention, Table I, %s partition", column),
                            tolerance = half_unit[[column]],
                            reason = paste("Table I counts only node visits in a module's codebook rate (as Infomap",
                                           "does); the default follows the paper's equation 2, which adds the",
                                           "module's exit flow."),
                            agrees_with = "map_convention = 'infomap'")
  })
})

test_that("map equation (paper convention) equals the saving at node 5 in Blocker et al.'s Figure 2", {
  # Figure 2 prints the code length with node 5 silent in the old (2.21) and the redesigned (1.99) codebooks,
  # each to two decimals, so their difference is known to within 0.01 (absolute).
  expect_reference(centrality_map_equation(blocker_graph, membership = table_one_partitions$optimal)[["5"]],
                   2.21 - 1.99, reference = "published: Blocker, Nieves and Rosvall 2022, Fig. 2",
                   covers = c("centrality()", "centrality_map_equation()"),
                   quantity = "map_equation, paper convention, Figure 2, node 5", tolerance = 0.01)
})

test_that("map equation equals the saving written out from closed-form flows", {
  degree <- rowSums(blocker_graph)
  p <- degree / sum(degree)
  groups <- table_one_partitions$optimal
  # Module visit rate plus its exit flow: one bridge arc 4 -> 5 (or 5 -> 4) of rate 1 / 2m leaves each module.
  exit <- 1 / sum(degree)
  s <- as.numeric(tapply(p, groups, sum)[groups]) + exit
  expect_reference(centrality_map_equation(blocker_graph), saving(p, 1),
                   reference = "independent: closed forms and hand derivations from the definition",
                   covers = c("centrality()", "centrality_map_equation()"),
                   quantity = "map_equation, Table I graph, one module", tolerance = 1e-10)
  expect_reference(centrality_map_equation(blocker_graph, membership = groups), saving(p, s),
                   reference = "independent: closed forms and hand derivations from the definition",
                   covers = c("centrality()", "centrality_map_equation()"),
                   quantity = "map_equation, paper convention, Table I graph, two halves", tolerance = 1e-10)
  expect_reference(centrality_map_equation(blocker_graph, membership = groups, map_convention = "infomap"),
                   saving(p, s - exit),
                   reference = "independent: closed forms and hand derivations from the definition",
                   covers = c("centrality()", "centrality_map_equation()"),
                   quantity = "map_equation, infomap convention, Table I graph, two halves", tolerance = 1e-10)
})

test_that("map equation equals the closed forms of sinks, singletons and edgeless graphs", {
  chain <- zoob_graph(3L, c(1, 2, 2, 3), directed = TRUE)
  independent <- "independent: closed forms and hand derivations from the definition"
  # Recorded teleportation at damping 0 visits the three nodes uniformly, one module.
  expect_reference(centrality_map_equation(chain, map_flow = "recorded", damping = 0), saving(rep(1 / 3, 3), 1),
                   reference = independent, covers = c("centrality()", "centrality_map_equation()"),
                   quantity = "map_equation, chain 1->2->3, recorded flow, damping 0", tolerance = 1e-10)
  # Unrecorded link teleportation at damping 0 picks each of the two links with probability 1/2; a node's
  # visit rate is the flow on the links into it.
  expect_reference(centrality_map_equation(chain, damping = 0), saving(c(0, 0.5, 0.5), 1),
                   reference = independent, covers = c("centrality()", "centrality_map_equation()"),
                   quantity = "map_equation, chain 1->2->3, unrecorded flow, damping 0", tolerance = 1e-10)
  # Each node alone in its codebook, visits only: s = p, nothing is saved.
  expect_reference(centrality_map_equation(chain, membership = 1:3, map_convention = "infomap"), rep(0, 3),
                   reference = independent, covers = c("centrality()", "centrality_map_equation()"),
                   quantity = "map_equation, chain 1->2->3, singleton modules, infomap convention", tolerance = 1e-10)
  # An edgeless graph under recorded flow: uniform visits 1/n, one module.
  lapply(2:4, \(n) expect_reference(centrality_map_equation(matrix(0, n, n), map_flow = "recorded"),
                                    saving(rep(1 / n, n), 1), reference = independent,
                                    covers = c("centrality()", "centrality_map_equation()"),
                                    quantity = sprintf("map_equation, %d isolates, recorded flow", n),
                                    tolerance = 1e-10))
})

test_that("map equation keeps savings far below the dominant node's mass", {
  # Arcs 1 -> 2 (weight 1) and 1 -> 3 (weight 1e-300): one module with visits about (0, 1, 1e-300), so node 2
  # saves 1e-300 log2(1e300) and node 3 about 1e-300 / log(2). Relative, because the values are near 1e-300.
  a <- matrix(0, 3, 3, dimnames = list(c("1", "2", "3"), c("1", "2", "3")))
  a[1, 2] <- 1
  a[1, 3] <- 1e-300
  expected <- c(0, 1e-300 * log2(1e300), 1e-300 / log(2))
  lapply(c("paper", "infomap"), \(convention) {
    expect_reference(centrality_map_equation(a, map_convention = convention), expected,
                     reference = "independent: closed forms and hand derivations from the definition",
                     covers = c("centrality()", "centrality_map_equation()"),
                     quantity = sprintf("map_equation, %s convention, weights 1 and 1e-300", convention),
                     tolerance = 1e-12, measure = "relative")
  })
})
