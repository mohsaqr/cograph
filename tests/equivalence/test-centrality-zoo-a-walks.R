# centrality() and its wrappers for the Centrality Zoo measures built on walks, spectra and Laplacians:
# diffusion centrality, dynamics-sensitive centrality, dynamical importance, resistance curvature, graph
# regularization, random walk decay, weighted LeaderRank and adaptive LeaderRank. Each is compared with its
# definition written in base R (helper-zoo-a.R: matrix powers, eigenvalues, a Laplacian pseudo-inverse, linear
# solves, stationary distributions), and random walk decay also with the tables printed in its source paper.

defined <- "independent: the measure's definition written in base R (helper-zoo-a.R)"
exact <- 1e-10

star5 <- za_star(5)
path5 <- za_path(5)
karate <- ref_karate()
connected <- ref_connected_graph(10, density = 0.3, seed = 21L)
weighted_arcs <- ref_connected_graph(8, density = 0.3, seed = 4L, directed = TRUE, weighted = TRUE)
weighted <- ref_connected_graph(9, density = 0.35, seed = 5L, weighted = TRUE)
arcs <- ref_connected_graph(8, density = 0.3, seed = 7L, directed = TRUE)
# A weighted triangle with labels out of alphabetical order.
triangle <- matrix(c(0, 1, 3, 1, 0, 2, 3, 2, 0), 3, 3, dimnames = list(c("C", "A", "B"), c("C", "A", "B")))

test_that("diffusion centrality is the sum of (qA)^t 1 for t = 1 .. T (Banerjee et al. 2013)", {
  # Arcs C -> A (0.5) and A -> B (0.25), q = 0.5, T = 2: C reaches 0.25 + 0.25 * 0.125, A 0.125.
  chain <- matrix(0, 3, 3, dimnames = list(c("C", "A", "B"), c("C", "A", "B")))
  chain["C", "A"] <- 0.5
  chain["A", "B"] <- 0.25
  expect_reference(centrality_diffusion_centrality(chain, diffusion_q = 0.5, diffusion_steps = 2),
                   za_diffusion(chain, 0.5, 2), reference = defined, covers = c("centrality()", "centrality_diffusion_centrality()"),
                   quantity = "diffusion_centrality, q 0.5, T 2, weighted chain", tolerance = exact)
  expect_reference(centrality_diffusion_centrality(chain, diffusion_q = 0.5, diffusion_steps = 2),
                   c(C = 0.28125, A = 0.125, B = 0), reference = defined, covers = c("centrality()", "centrality_diffusion_centrality()"),
                   quantity = "diffusion_centrality, q 0.5, T 2, weighted chain, by hand", tolerance = exact)
  expect_reference(centrality_diffusion_centrality(weighted_arcs), za_diffusion(weighted_arcs), reference = defined,
                   covers = c("centrality()", "centrality_diffusion_centrality()"), quantity = "diffusion_centrality, q 1, T 3, directed weighted",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_diffusion_centrality(karate, diffusion_q = 0.1, diffusion_steps = 5),
                   za_diffusion(karate, 0.1, 5), reference = defined, covers = c("centrality()", "centrality_diffusion_centrality()"),
                   quantity = "diffusion_centrality, q 0.1, T 5, karate", tolerance = exact, measure = "scaled")
})

test_that("dynamics-sensitive centrality equals its linearised spreading sum (Liu et al. 2016)", {
  expect_reference(centrality_dynamics_sensitive(star5, ds_beta = 0.5, ds_mu = 0.5, ds_steps = 2),
                   za_dynamics_sensitive(star5, 0.5, 0.5, 2), reference = defined, covers = c("centrality()", "centrality_dynamics_sensitive()"),
                   quantity = "dynamics_sensitive, beta 0.5, mu 0.5, T 2, star of 5", tolerance = exact)
  expect_reference(centrality_dynamics_sensitive(karate), za_dynamics_sensitive(karate), reference = defined,
                   covers = c("centrality()", "centrality_dynamics_sensitive()"), quantity = "dynamics_sensitive, beta 0.1, mu 1, T 5, karate", tolerance = exact,
                   measure = "scaled")
  expect_reference(centrality_dynamics_sensitive(connected, ds_beta = 0.2, ds_mu = 0.3, ds_steps = 4),
                   za_dynamics_sensitive(connected, 0.2, 0.3, 4), reference = defined, covers = c("centrality()", "centrality_dynamics_sensitive()"),
                   quantity = "dynamics_sensitive, beta 0.2, mu 0.3, T 4, connected random graph", tolerance = exact,
                   measure = "scaled")
  # By hand on one edge (degree 1): beta d (beta d + 1 - mu)^r summed over r = 0, 1, 2, beta = 0.5.
  edge <- za_graph(c(1, 2), 2)
  expect_reference(centrality_dynamics_sensitive(edge, ds_beta = 0.5, ds_mu = 0.5, ds_steps = 3),
                   rep(0.5 * (1 + 1 + 1), 2), reference = defined, covers = c("centrality()", "centrality_dynamics_sensitive()"),
                   quantity = "dynamics_sensitive, beta 0.5, mu 0.5, T 3, one edge, by hand", tolerance = exact)
})

test_that("dynamical importance is the relative loss of spectral radius on deletion (Restrepo et al. 2006)", {
  expect_reference(centrality_dynamical_importance(star5), za_dynamical_importance(star5), reference = defined,
                   covers = c("centrality()", "centrality_dynamical_importance()"), quantity = "dynamical_importance, star of 5", tolerance = exact)
  expect_reference(centrality_dynamical_importance(karate), za_dynamical_importance(karate), reference = defined,
                   covers = c("centrality()", "centrality_dynamical_importance()"), quantity = "dynamical_importance, karate", tolerance = exact)
  expect_reference(centrality_dynamical_importance(weighted_arcs), za_dynamical_importance(weighted_arcs),
                   reference = defined, covers = c("centrality()", "centrality_dynamical_importance()"), quantity = "dynamical_importance, directed weighted",
                   tolerance = exact)
  # By hand: the star's spectral radius is 2; without the hub 0, without a leaf sqrt(3).
  expect_reference(centrality_dynamical_importance(star5), c(1, rep(1 - sqrt(3) / 2, 4)), reference = defined,
                   covers = c("centrality()", "centrality_dynamical_importance()"), quantity = "dynamical_importance, star of 5, by hand", tolerance = exact)
})

test_that("resistance curvature is 1 - (1/2) sum_j w_ij R_ij (Devriendt & Lambiotte 2022, eq. 2)", {
  expect_reference(centrality_resistance_curvature(triangle), za_resistance_curvature(triangle), reference = defined,
                   covers = c("centrality()", "centrality_resistance_curvature()"), quantity = "resistance_curvature, weighted triangle", tolerance = exact)
  expect_reference(centrality_resistance_curvature(weighted), za_resistance_curvature(weighted), reference = defined,
                   covers = c("centrality()", "centrality_resistance_curvature()"), quantity = "resistance_curvature, weighted random graph", tolerance = exact)
  expect_reference(centrality_resistance_curvature(karate), za_resistance_curvature(karate), reference = defined,
                   covers = c("centrality()", "centrality_resistance_curvature()"), quantity = "resistance_curvature, karate", tolerance = exact)
  path_isolates <- za_union(za_path(4), matrix(0, 2, 2))
  expect_reference(centrality_resistance_curvature(path_isolates), za_resistance_curvature(path_isolates),
                   reference = defined, covers = c("centrality()", "centrality_resistance_curvature()"), quantity = "resistance_curvature, path and two isolates",
                   tolerance = exact)
  # By hand: on a tree 1 - degree / 2; on a cycle or complete graph of n nodes 1 / n.
  expect_reference(centrality_resistance_curvature(star5), c(-1, rep(0.5, 4)), reference = defined, covers = c("centrality()", "centrality_resistance_curvature()"),
                   quantity = "resistance_curvature, star of 5, by hand", tolerance = exact)
  expect_reference(centrality_resistance_curvature(za_ring(8)), rep(1 / 8, 8), reference = defined, covers = c("centrality()", "centrality_resistance_curvature()"),
                   quantity = "resistance_curvature, cycle of 8, by hand", tolerance = exact)
  # The expected degree in a random spanning tree of the triangle: trees of weight 2, 3, 6 with degrees
  # (1, 2, 1), (2, 1, 1), (1, 1, 2) give (14, 13, 17) / 11.
  expect_reference(centrality_resistance_curvature(triangle), c(C = 4 / 11, A = 9 / 22, B = 5 / 22), reference = defined,
                   covers = c("centrality()", "centrality_resistance_curvature()"), quantity = "resistance_curvature, weighted triangle, spanning trees by hand",
                   tolerance = exact)
})

test_that("graph regularization is 1 / diag((I + gamma L)^-1) (Dal Col & Petronetto 2023)", {
  expect_reference(centrality_graph_regularization(triangle), za_graph_regularization(triangle), reference = defined,
                   covers = c("centrality()", "centrality_graph_regularization()"), quantity = "graph_regularization, gamma 1, weighted triangle", tolerance = exact)
  expect_reference(centrality_graph_regularization(weighted, 2), za_graph_regularization(weighted, 2),
                   reference = defined, covers = c("centrality()", "centrality_graph_regularization()"), quantity = "graph_regularization, gamma 2, weighted random graph",
                   tolerance = exact)
  expect_reference(centrality_graph_regularization(karate, 0.5), za_graph_regularization(karate, 0.5),
                   reference = defined, covers = c("centrality()", "centrality_graph_regularization()"), quantity = "graph_regularization, gamma 0.5, karate",
                   tolerance = exact)
  # By hand: det(I + L) = 46 and the principal minors without each node are 20, 21, 19.
  expect_reference(centrality_graph_regularization(triangle), c(C = 46 / 20, A = 46 / 21, B = 46 / 19),
                   reference = defined, covers = c("centrality()", "centrality_graph_regularization()"),
                   quantity = "graph_regularization, gamma 1, weighted triangle, determinants by hand", tolerance = exact)
})

test_that("random walk decay equals the discounted first-arrival sum (Was et al. 2019, Definition 1, eq. 6)", {
  chain <- za_graph(c(1, 2, 2, 3), 3, directed = TRUE)
  expect_reference(centrality_random_walk_decay(chain), za_random_walk_decay(chain), reference = defined,
                   covers = c("centrality()", "centrality_random_walk_decay()"), quantity = "random_walk_decay, decay 0.5, directed chain", tolerance = exact)
  expect_reference(centrality_random_walk_decay(weighted_arcs, 0.8), za_random_walk_decay(weighted_arcs, 0.8),
                   reference = defined, covers = c("centrality()", "centrality_random_walk_decay()"), quantity = "random_walk_decay, decay 0.8, directed weighted",
                   tolerance = exact)
  expect_reference(centrality_random_walk_decay(path5), za_random_walk_decay(path5), reference = defined,
                   covers = c("centrality()", "centrality_random_walk_decay()"), quantity = "random_walk_decay, decay 0.5, path of 5", tolerance = exact)
  # The paper's Fig. 1: loop u -> u, two arcs u -> v (one arc of weight 2), v -> w, w -> v, w -> t, starting
  # masses (1, 0, 1, 0). The printed Example 3 is internally inconsistent (its v = 214/105 exceeds the total
  # mass 2); the definition gives the exact values below, worked out by hand.
  fig1 <- za_graph(c(1, 1, 1, 2, 2, 3, 3, 2, 3, 4), 4, weights = c(1, 2, 1, 1, 1), directed = TRUE,
                   names = c("u", "v", "w", "t"))
  mass <- c(u = 1, v = 0, w = 1, t = 0)
  expect_reference(centrality_random_walk_decay(fig1, rwd_node_weights = mass, directed = TRUE),
                   za_random_walk_decay(fig1, 0.5, mass), reference = defined, covers = c("centrality()", "centrality_random_walk_decay()"),
                   quantity = "random_walk_decay, decay 0.5, Fig. 1 multigraph", tolerance = exact)
  expect_reference(centrality_random_walk_decay(fig1, rwd_node_weights = mass, directed = TRUE),
                   c(u = 1, v = 13 / 20, w = 6 / 5, t = 12 / 35), reference = defined, covers = c("centrality()", "centrality_random_walk_decay()"),
                   quantity = "random_walk_decay, decay 0.5, Fig. 1 multigraph, by hand", tolerance = exact)
})

test_that("random walk decay reproduces Was et al. (2019), Examples 4 and 5", {
  published <- "published: Was et al. 2019, Examples 4 and 5 (tables of Figs. 2 and 3, p. 2203)"
  # Printed to two decimals: half a unit in the last place, 5e-3.
  # Example 4: two directed four-cycles u1..u4 and v1..v4 linked by v4 -> u4, u3 -> v3, u2 -> v2; G' adds u1 -> u4.
  example4 <- za_graph(c(1, 2, 2, 3, 3, 4, 4, 1, 5, 6, 6, 7, 7, 8, 8, 5, 8, 4, 3, 7, 2, 6), 8, directed = TRUE,
                       names = c(paste0("u", 1:4), paste0("v", 1:4)))
  example4_modified <- example4
  example4_modified["u1", "u4"] <- 1
  expect_reference(centrality_random_walk_decay(example4, 0.8), c(3.72, 3.75, 2.48, 3.68, 2.76, 4.13, 4.67, 4.43),
                   reference = published, covers = c("centrality()", "centrality_random_walk_decay()"), quantity = "random_walk_decay, decay 0.8, Example 4, G",
                   tolerance = 5e-3)
  expect_reference(centrality_random_walk_decay(example4_modified, 0.8), c(3.72, 3.02, 2.17, 3.94, 2.60, 3.77, 4.36, 4.21),
                   reference = published, covers = c("centrality()", "centrality_random_walk_decay()"), quantity = "random_walk_decay, decay 0.8, Example 4, G'",
                   tolerance = 5e-3)
  # Example 5: reciprocal triangles u1:u3 and v1:v3, a reciprocal four-cycle w1:w4 and a reciprocal triangle
  # u1, v1, w1; G' replaces u2 -> u1 and v2 -> v1 by u2 -> v1 and v2 -> u1.
  example5 <- za_union(za_complete(3), za_complete(3), za_ring(4))
  example5[cbind(c(1, 4, 7, 4, 7, 1), c(4, 7, 1, 1, 4, 7))] <- 1
  dimnames(example5) <- list(c(paste0("u", 1:3), paste0("v", 1:3), paste0("w", 1:4)),
                             c(paste0("u", 1:3), paste0("v", 1:3), paste0("w", 1:4)))
  example5_modified <- example5
  example5_modified["u2", "u1"] <- 0
  example5_modified["v2", "v1"] <- 0
  example5_modified["u2", "v1"] <- 1
  example5_modified["v2", "u1"] <- 1
  expect_reference(centrality_random_walk_decay(example5, 0.8, directed = TRUE),
                   c(4.15, 2.56, 2.56, 4.15, 2.56, 2.56, 4.40, 2.74, 2.62, 2.74), reference = published,
                   covers = c("centrality()", "centrality_random_walk_decay()"), quantity = "random_walk_decay, decay 0.8, Example 5, G", tolerance = 5e-3)
  expect_reference(centrality_random_walk_decay(example5_modified, 0.8, directed = TRUE),
                   c(4.46, 2.85, 2.70, 4.46, 2.85, 2.70, 4.40, 2.74, 2.62, 2.74), reference = published,
                   covers = c("centrality()", "centrality_random_walk_decay()"), quantity = "random_walk_decay, decay 0.8, Example 5, G'", tolerance = 5e-3)
})

test_that("weighted LeaderRank is the stationary resource of the grounded walk (Li et al. 2014)", {
  edge <- za_graph(c(1, 2), 2, directed = TRUE)
  expect_reference(centrality_weighted_leaderrank(edge), za_weighted_leaderrank(edge), reference = defined,
                   covers = c("centrality()", "centrality_weighted_leaderrank()"), quantity = "weighted_leaderrank, alpha 1, one arc", tolerance = exact)
  expect_reference(centrality_weighted_leaderrank(edge, 0), za_weighted_leaderrank(edge, 0), reference = defined,
                   covers = c("centrality()", "centrality_weighted_leaderrank()"), quantity = "weighted_leaderrank, alpha 0, one arc", tolerance = exact)
  expect_reference(centrality_weighted_leaderrank(star5), za_weighted_leaderrank(star5), reference = defined,
                   covers = c("centrality()", "centrality_weighted_leaderrank()"), quantity = "weighted_leaderrank, alpha 1, star of 5", tolerance = exact)
  expect_reference(centrality_weighted_leaderrank(arcs), za_weighted_leaderrank(arcs), reference = defined,
                   covers = c("centrality()", "centrality_weighted_leaderrank()"), quantity = "weighted_leaderrank, alpha 1, directed random graph", tolerance = exact)
  expect_reference(centrality_weighted_leaderrank(arcs, 0.5), za_weighted_leaderrank(arcs, 0.5), reference = defined,
                   covers = c("centrality()", "centrality_weighted_leaderrank()"), quantity = "weighted_leaderrank, alpha 0.5, directed random graph", tolerance = exact)
  expect_reference(centrality_weighted_leaderrank(karate, -1), za_weighted_leaderrank(karate, -1), reference = defined,
                   covers = c("centrality()", "centrality_weighted_leaderrank()"), quantity = "weighted_leaderrank, alpha -1, karate", tolerance = exact)
  # By hand on the arc 1 -> 2 (alpha 1): the ground feeds node 2 only; node 1 is transient and the resource of
  # 3 units splits evenly between node 2 and the ground.
  expect_reference(centrality_weighted_leaderrank(edge), c(v1 = 0, v2 = 1.5), reference = defined, covers = c("centrality()", "centrality_weighted_leaderrank()"),
                   quantity = "weighted_leaderrank, alpha 1, one arc, by hand", tolerance = exact)
})

test_that("adaptive LeaderRank weights arcs by the H-index of their target (Xu & Wang 2017)", {
  chain <- za_graph(c(1, 2, 2, 3), 3, directed = TRUE)
  expect_reference(centrality_adaptive_leaderrank(path5), za_adaptive_leaderrank(path5), reference = defined,
                   covers = c("centrality()", "centrality_adaptive_leaderrank()"), quantity = "adaptive_leaderrank, path of 5", tolerance = exact)
  expect_reference(centrality_adaptive_leaderrank(star5), za_adaptive_leaderrank(star5), reference = defined,
                   covers = c("centrality()", "centrality_adaptive_leaderrank()"), quantity = "adaptive_leaderrank, star of 5", tolerance = exact)
  expect_reference(centrality_adaptive_leaderrank(karate), za_adaptive_leaderrank(karate), reference = defined,
                   covers = c("centrality()", "centrality_adaptive_leaderrank()"), quantity = "adaptive_leaderrank, karate", tolerance = exact)
  expect_reference(centrality_adaptive_leaderrank(chain), za_adaptive_leaderrank(chain), reference = defined,
                   covers = c("centrality()", "centrality_adaptive_leaderrank()"), quantity = "adaptive_leaderrank, H on the skeleton, directed chain", tolerance = exact)
  expect_reference(centrality_adaptive_leaderrank(arcs, "out"), za_adaptive_leaderrank(arcs, "out"), reference = defined,
                   covers = c("centrality()", "centrality_adaptive_leaderrank()"), quantity = "adaptive_leaderrank, H on out-neighbours, directed random graph",
                   tolerance = exact)
  expect_reference(centrality_adaptive_leaderrank(arcs, "in"), za_adaptive_leaderrank(arcs, "in"), reference = defined,
                   covers = c("centrality()", "centrality_adaptive_leaderrank()"), quantity = "adaptive_leaderrank, H on in-neighbours, directed random graph",
                   tolerance = exact)
  # By hand on the path A-B-C-D: every H-index is 1, so the walk is LeaderRank's; the stationary resource is
  # proportional to the augmented degrees 2, 3, 3, 2 (ground 4), times 4 units.
  expect_reference(centrality_adaptive_leaderrank(za_path(4)), c(A = 4, B = 6, C = 6, D = 4) / 7, reference = defined,
                   covers = c("centrality()", "centrality_adaptive_leaderrank()"), quantity = "adaptive_leaderrank, path of 4, by hand", tolerance = exact)
})
