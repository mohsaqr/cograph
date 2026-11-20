# The geodesic-based Centrality Zoo measures that reweight, rescope or regroup betweenness and closeness:
# length-scaled, delta (distance-decayed), ego and bounded-distance (cutoff) betweenness, delta closeness, the
# gravity family, group betweenness, local reaching centrality, pairwise disconnectivity, the domain prestige,
# the Gould-Fernandez brokerage roles and trophic incoherence. Each is compared with its definition written in
# base R (helper-zoo-a.R: distances and shortest-path counts from powers of the adjacency matrix), and local
# reaching centrality with igraph's harmonic_centrality() (cograph computes it natively).

defined <- "independent: the measure's definition written in base R (helper-zoo-a.R)"
exact <- 1e-10

star5 <- za_star(5)
path5 <- za_path(5)
bridge6 <- za_bridge6()
karate <- ref_karate()
connected <- ref_connected_graph(10, density = 0.3, seed = 21L)
arcs <- ref_connected_graph(10, density = 0.3, seed = 8L, directed = TRUE)
pairs2 <- za_graph(c(1, 2, 3, 4), 4, names = LETTERS[1:4])

test_that("length-scaled and delta betweenness weight each pair as defined", {
  # Borgatti & Everett (2006): each pair weighted by 1 / d(s, t). Agneessens et al. (2017): by
  # (d(s, t) - 1)^-delta, so adjacent pairs never enter.
  expect_reference(centrality_length_scaled_betweenness(star5), za_betweenness(star5, \(d) 1 / d), reference = defined,
                   covers = c("centrality()", "centrality_length_scaled_betweenness()"), quantity = "length_scaled_betweenness, star of 5", tolerance = exact)
  expect_reference(centrality_length_scaled_betweenness(connected), za_betweenness(connected, \(d) 1 / d),
                   reference = defined, covers = c("centrality()", "centrality_length_scaled_betweenness()"), quantity = "length_scaled_betweenness, connected random graph",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_length_scaled_betweenness(arcs), za_betweenness(arcs, \(d) 1 / d), reference = defined,
                   covers = c("centrality()", "centrality_length_scaled_betweenness()"), quantity = "length_scaled_betweenness, directed", tolerance = exact,
                   measure = "scaled")
  # By hand on the path A-...-E: C lies on A-D (d 3), A-E (4), B-D (2), B-E (3).
  expect_reference(centrality_length_scaled_betweenness(path5)[["C"]], 1 / 3 + 1 / 4 + 1 / 2 + 1 / 3,
                   reference = defined, covers = c("centrality()", "centrality_length_scaled_betweenness()"), quantity = "length_scaled_betweenness, node C of a path, by hand",
                   tolerance = exact)
  expect_reference(centrality_delta_betweenness(connected), za_betweenness(connected, \(d) (d - 1)^-1),
                   reference = defined, covers = c("centrality()", "centrality_delta_betweenness()"), quantity = "delta_betweenness, delta 1, connected random graph",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_delta_betweenness(connected, betweenness_delta = 3),
                   za_betweenness(connected, \(d) (d - 1)^-3), reference = defined, covers = c("centrality()", "centrality_delta_betweenness()"),
                   quantity = "delta_betweenness, delta 3, connected random graph", tolerance = exact, measure = "scaled")
  expect_reference(centrality_delta_betweenness(arcs), za_betweenness(arcs, \(d) (d - 1)^-1), reference = defined,
                   covers = c("centrality()", "centrality_delta_betweenness()"), quantity = "delta_betweenness, delta 1, directed", tolerance = exact, measure = "scaled")
  expect_reference(centrality_delta_betweenness(za_complete(3), betweenness_delta = 2), c(0, 0, 0), reference = defined,
                   covers = c("centrality()", "centrality_delta_betweenness()"), quantity = "delta_betweenness, triangle (every pair adjacent)", tolerance = 0)
})

test_that("bounded-distance betweenness (cutoff) counts only pairs within k", {
  # Borgatti & Everett (2006), k-betweenness: the pairs no further apart than k.
  expect_reference(centrality(connected, measures = "betweenness", cutoff = 2),
                   ref_nodes(betweenness = za_betweenness(connected, cutoff = 2)), reference = defined,
                   covers = "centrality()", quantity = "betweenness, cutoff 2, connected random graph",
                   tolerance = exact, measure = "scaled", key = "node")
  expect_reference(centrality(connected, measures = "betweenness", cutoff = 3),
                   ref_nodes(betweenness = za_betweenness(connected, cutoff = 3)), reference = defined,
                   covers = "centrality()", quantity = "betweenness, cutoff 3, connected random graph",
                   tolerance = exact, measure = "scaled", key = "node")
  expect_reference(centrality(arcs, measures = "betweenness", cutoff = 2),
                   ref_nodes(betweenness = za_betweenness(arcs, cutoff = 2)), reference = defined,
                   covers = "centrality()", quantity = "betweenness, cutoff 2, directed", tolerance = exact,
                   measure = "scaled", key = "node")
})

test_that("ego betweenness is the betweenness inside the ego network (Everett & Borgatti 2005)", {
  expect_reference(centrality_ego_betweenness(star5), za_ego_betweenness(star5), reference = defined, covers = c("centrality()", "centrality_ego_betweenness()"),
                   quantity = "ego_betweenness, star of 5", tolerance = exact)
  expect_reference(centrality_ego_betweenness(connected), za_ego_betweenness(connected), reference = defined,
                   covers = c("centrality()", "centrality_ego_betweenness()"), quantity = "ego_betweenness, connected random graph", tolerance = exact)
  expect_reference(centrality_ego_betweenness(karate), za_ego_betweenness(karate), reference = defined,
                   covers = c("centrality()", "centrality_ego_betweenness()"), quantity = "ego_betweenness, karate", tolerance = exact)
  # The Everett-Borgatti shortcut on the ego network's matrix A: sum of 1 / (A^2 (1 - A)) over unlinked pairs
  # of neighbours, here on the star (the hub separates its six leaf pairs).
  expect_reference(centrality_ego_betweenness(star5), c(A = 6, B = 0, C = 0, D = 0, E = 0), reference = defined,
                   covers = c("centrality()", "centrality_ego_betweenness()"), quantity = "ego_betweenness, star of 5, by hand", tolerance = exact)
})

test_that("delta closeness equals the sum of d^-delta over n - 1 (Agneessens et al. 2017, eq. 2)", {
  expect_reference(centrality_delta_closeness(bridge6, closeness_delta = 0), za_delta_closeness(bridge6, 0),
                   reference = defined, covers = c("centrality()", "centrality_delta_closeness()"), quantity = "delta_closeness, delta 0, two triangles",
                   tolerance = exact)
  expect_reference(centrality_delta_closeness(bridge6), za_delta_closeness(bridge6, 1), reference = defined,
                   covers = c("centrality()", "centrality_delta_closeness()"), quantity = "delta_closeness, delta 1, two triangles", tolerance = exact)
  expect_reference(centrality_delta_closeness(bridge6, closeness_delta = 40), za_delta_closeness(bridge6, 40),
                   reference = defined, covers = c("centrality()", "centrality_delta_closeness()"), quantity = "delta_closeness, delta 40, two triangles",
                   tolerance = exact)
  expect_reference(centrality_delta_closeness(karate, closeness_delta = 2), za_delta_closeness(karate, 2),
                   reference = defined, covers = c("centrality()", "centrality_delta_closeness()"), quantity = "delta_closeness, delta 2, karate", tolerance = exact)
  # Two disjoint edges: each node reaches one of the other three, so delta 0 and delta 1 both give 1/3.
  expect_reference(centrality_delta_closeness(pairs2, closeness_delta = 0), za_delta_closeness(pairs2, 0),
                   reference = defined, covers = c("centrality()", "centrality_delta_closeness()"), quantity = "delta_closeness, delta 0, two disjoint edges",
                   tolerance = exact)
  expect_reference(centrality_delta_closeness(pairs2), c(A = 1, B = 1, C = 1, D = 1) / 3, reference = defined,
                   covers = c("centrality()", "centrality_delta_closeness()"), quantity = "delta_closeness, delta 1, two disjoint edges, by hand", tolerance = exact)
})

test_that("gravity and extended gravity equal the gravity formula (Ma et al. 2016; Li et al. 2019)", {
  core <- za_core(karate)
  degree <- rowSums(karate)
  expect_reference(centrality_gravity(karate), za_gravity(karate, core, 3), reference = defined, covers = c("centrality()", "centrality_gravity()"),
                   quantity = "gravity, k-shell mass, radius 3, karate", tolerance = exact, measure = "scaled")
  expect_reference(centrality_gravity(karate, gravity_mass = "degree", gravity_radius = NULL),
                   za_gravity(karate, degree, Inf), reference = defined, covers = c("centrality()", "centrality_gravity()"),
                   quantity = "gravity, degree mass, no radius, karate", tolerance = exact, measure = "scaled")
  # The legacy mass: sum over j of k_j k_s(j) / d^2, no mass on the focal node.
  d <- za_hops(karate)
  legacy <- rowSums(ifelse(is.finite(d) & d > 0, outer(rep(1, 34), degree * core) / d^2, 0))
  expect_reference(centrality_gravity(karate, gravity_mass = "legacy", gravity_radius = NULL), legacy,
                   reference = defined, covers = c("centrality()", "centrality_gravity()"), quantity = "gravity, legacy mass, no radius, karate",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_extended_gravity(star5, gravity_radius = 2), za_extended_gravity(star5, 2),
                   reference = defined, covers = c("centrality()", "centrality_extended_gravity()"), quantity = "extended_gravity, radius 2, star of 5",
                   tolerance = exact)
  expect_reference(centrality_extended_gravity(za_path(4), gravity_radius = 1), za_extended_gravity(za_path(4), 1),
                   reference = defined, covers = c("centrality()", "centrality_extended_gravity()"), quantity = "extended_gravity, radius 1, path of 4",
                   tolerance = exact)
  expect_reference(centrality_extended_gravity(karate), za_extended_gravity(karate), reference = defined,
                   covers = c("centrality()", "centrality_extended_gravity()"), quantity = "extended_gravity, radius 3, karate", tolerance = exact, measure = "scaled")
  pieces <- za_union(star5, za_complete(3))
  expect_reference(centrality_extended_gravity(pieces), za_extended_gravity(pieces), reference = defined,
                   covers = c("centrality()", "centrality_extended_gravity()"), quantity = "extended_gravity, star and triangle", tolerance = exact)
})

test_that("group betweenness counts the shortest paths that meet the group (Everett & Borgatti 1999)", {
  # Directed cycle 1 -> 2 -> 3 -> 4 -> 1: {2} lies on 1-3, 1-4 and 4-3; {2, 3} on 1-4 only, counted once.
  cycle <- za_ring(4, directed = TRUE)
  expect_reference(group_centrality(cycle, nodes = 2, measure = "betweenness", normalized = FALSE),
                   za_group_betweenness(cycle, 2), reference = defined, covers = "group_centrality()",
                   quantity = "group betweenness, node 2 of a directed cycle of 4", tolerance = exact)
  expect_reference(group_centrality(cycle, nodes = c(2, 3), measure = "betweenness", normalized = FALSE),
                   za_group_betweenness(cycle, c(2, 3)), reference = defined, covers = "group_centrality()",
                   quantity = "group betweenness, nodes 2 and 3 of a directed cycle of 4", tolerance = exact)
  six <- za_graph(c(1, 6, 2, 1, 3, 1, 4, 1, 5, 1, 2, 6, 6, 2, 1, 3, 3, 6, 2, 4, 3, 4, 5, 4, 1, 5, 4, 5), 6,
                  directed = TRUE)
  expect_reference(group_centrality(six, nodes = c(1, 2), measure = "betweenness", normalized = FALSE),
                   za_group_betweenness(six, c(1, 2)), reference = defined, covers = "group_centrality()",
                   quantity = "group betweenness, nodes 1 and 2 of a directed graph of 6", tolerance = exact)
  expect_reference(group_centrality(arcs, nodes = c(1, 4, 7), measure = "betweenness", normalized = FALSE),
                   za_group_betweenness(arcs, c(1, 4, 7)), reference = defined, covers = "group_centrality()",
                   quantity = "group betweenness, nodes 1, 4, 7 of a directed random graph", tolerance = exact)
  # By hand, as listed above.
  expect_reference(group_centrality(cycle, nodes = 2, measure = "betweenness", normalized = FALSE), 3,
                   reference = defined, covers = "group_centrality()",
                   quantity = "group betweenness, node 2 of a directed cycle of 4, by hand", tolerance = exact)
})

test_that("local reaching centrality equals igraph's normalised harmonic centrality", {
  skip_without_reference("igraph")
  # Mones et al. (2012): on an undirected graph the mean of 1 / d over the other nodes.
  sparse <- ref_graph(15, density = 0.2, seed = 51L)
  expect_reference(centrality_reaching_local(connected),
                   igraph::harmonic_centrality(ref_igraph(connected), normalized = TRUE), reference = "igraph",
                   covers = c("centrality()", "centrality_reaching_local()"),
                   quantity = "reaching_local, connected random graph", tolerance = exact)
  expect_reference(centrality_reaching_local(sparse), igraph::harmonic_centrality(ref_igraph(sparse), normalized = TRUE),
                   reference = "igraph", covers = c("centrality()", "centrality_reaching_local()"),
                   quantity = "reaching_local, sparse random graph", tolerance = exact)
})

test_that("local reaching centrality, pairwise disconnectivity and domain prestige equal their definitions", {
  # By hand on the path A-B-C-D: A reaches B, C, D at 1, 2, 3: (1 + 1/2 + 1/3) / 3 = 11/18.
  expect_reference(centrality_reaching_local(za_path(4))[["A"]], 11 / 18, reference = defined,
                   covers = c("centrality()", "centrality_reaching_local()"),
                   quantity = "reaching_local, end of a path of 4, by hand", tolerance = exact)
  # Potapov et al. (2008): the share of the ordered connected pairs lost with the node. A directed 3-cycle has
  # 6 such pairs, and 1 is left after any deletion: 5/6.
  cycle3 <- za_ring(3, directed = TRUE)
  sparse_arcs <- ref_graph(9, density = 0.25, seed = 31L, directed = TRUE)
  expect_reference(centrality_pairwisedis(cycle3), za_pairwise_disconnectivity(cycle3), reference = defined,
                   covers = c("centrality()", "centrality_pairwisedis()"), quantity = "pairwisedis, directed cycle of 3", tolerance = exact)
  expect_reference(centrality_pairwisedis(cycle3), rep(5 / 6, 3), reference = defined, covers = c("centrality()", "centrality_pairwisedis()"),
                   quantity = "pairwisedis, directed cycle of 3, by hand", tolerance = exact)
  expect_reference(centrality_pairwisedis(sparse_arcs), za_pairwise_disconnectivity(sparse_arcs), reference = defined,
                   covers = c("centrality()", "centrality_pairwisedis()"), quantity = "pairwisedis, sparse directed random graph", tolerance = exact)
  # Wasserman & Faust (1994): the domain of a node is the number of nodes that reach it.
  domain <- \(graph) colSums(is.finite(za_hops(graph))) - 1
  expect_reference(centrality_prestige_domain(cycle3), domain(cycle3), reference = defined,
                   covers = c("centrality()", "centrality_prestige_domain()"),
                   quantity = "prestige_domain, directed cycle of 3", tolerance = 0)
  expect_reference(centrality_prestige_domain(sparse_arcs), domain(sparse_arcs), reference = defined,
                   covers = c("centrality()", "centrality_prestige_domain()"),
                   quantity = "prestige_domain, sparse directed random graph", tolerance = 0)
})

test_that("the brokerage roles count the open two-paths by group (Gould & Fernandez 1989)", {
  # A -> B, A -> C, B -> C, B -> D, C -> D, D -> A, groups (A, B) and (C, D).
  small <- za_graph(c(1, 2, 1, 3, 2, 3, 2, 4, 3, 4, 4, 1), 4, directed = TRUE, names = LETTERS[1:4])
  small_groups <- c(1, 1, 2, 2)
  groups <- rep(1:3, length.out = 10)
  on_small <- za_brokerage(small, small_groups)
  on_arcs <- za_brokerage(arcs, groups)
  roles <- list(coordinator = centrality_brokerage_coordinator, itinerant = centrality_brokerage_itinerant,
                representative = centrality_brokerage_representative, gatekeeper = centrality_brokerage_gatekeeper,
                liaison = centrality_brokerage_liaison)
  # One comparison per role and graph, written out so that each states its function.
  expect_reference(roles$coordinator(small, membership = small_groups), on_small[, "coordinator"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_coordinator()"),
                   quantity = "brokerage_coordinator, four nodes in two groups", tolerance = 0)
  expect_reference(roles$itinerant(small, membership = small_groups), on_small[, "itinerant"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_itinerant()"),
                   quantity = "brokerage_itinerant, four nodes in two groups", tolerance = 0)
  expect_reference(roles$representative(small, membership = small_groups), on_small[, "representative"],
                   reference = defined, covers = c("centrality()", "centrality_brokerage_representative()"),
                   quantity = "brokerage_representative, four nodes in two groups", tolerance = 0)
  expect_reference(roles$gatekeeper(small, membership = small_groups), on_small[, "gatekeeper"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_gatekeeper()"),
                   quantity = "brokerage_gatekeeper, four nodes in two groups", tolerance = 0)
  expect_reference(roles$liaison(small, membership = small_groups), on_small[, "liaison"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_liaison()"),
                   quantity = "brokerage_liaison, four nodes in two groups", tolerance = 0)
  expect_reference(roles$coordinator(arcs, membership = groups), on_arcs[, "coordinator"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_coordinator()"),
                   quantity = "brokerage_coordinator, directed random graph, three groups", tolerance = 0)
  expect_reference(roles$itinerant(arcs, membership = groups), on_arcs[, "itinerant"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_itinerant()"),
                   quantity = "brokerage_itinerant, directed random graph, three groups", tolerance = 0)
  expect_reference(roles$representative(arcs, membership = groups), on_arcs[, "representative"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_representative()"),
                   quantity = "brokerage_representative, directed random graph, three groups", tolerance = 0)
  expect_reference(roles$gatekeeper(arcs, membership = groups), on_arcs[, "gatekeeper"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_gatekeeper()"),
                   quantity = "brokerage_gatekeeper, directed random graph, three groups", tolerance = 0)
  expect_reference(roles$liaison(arcs, membership = groups), on_arcs[, "liaison"], reference = defined,
                   covers = c("centrality()", "centrality_brokerage_liaison()"),
                   quantity = "brokerage_liaison, directed random graph, three groups", tolerance = 0)
  # By hand on the small graph: A brokers D -> A -> B (gatekeeper) and D -> A -> C (itinerant); B brokers
  # A -> B -> D (representative; A -> B -> C is closed by A -> C); C brokers A -> C -> D (gatekeeper);
  # D brokers B -> D -> A (itinerant) and C -> D -> A (representative).
  expect_reference(roles$itinerant(small, membership = small_groups), c(A = 1, B = 0, C = 0, D = 1),
                   reference = defined, covers = c("centrality()", "centrality_brokerage_itinerant()"),
                   quantity = "brokerage_itinerant, four nodes in two groups, by hand", tolerance = 0)
  expect_reference(roles$gatekeeper(small, membership = small_groups), c(A = 1, B = 0, C = 1, D = 0),
                   reference = defined, covers = c("centrality()", "centrality_brokerage_gatekeeper()"),
                   quantity = "brokerage_gatekeeper, four nodes in two groups, by hand", tolerance = 0)
})

test_that("trophic incoherence is zero on a perfect chain (Johnson et al. 2014)", {
  # Levels 1, 2, 3, 4 along 1 -> 2 -> 3 -> 4: every link spans one level, so the spread q is 0.
  chain <- za_graph(c(1, 2, 2, 3, 3, 4), 4, directed = TRUE)
  expect_reference(trophic_incoherence(chain), 0, reference = defined, covers = "trophic_incoherence()",
                   quantity = "trophic incoherence, chain of 4", tolerance = 1e-12)
})
