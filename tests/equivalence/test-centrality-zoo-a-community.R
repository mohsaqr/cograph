# centrality() and its wrappers for the community-aware measures of the Centrality Zoo: community-based
# centrality, community-based mediator, Comm centrality, the community hub-bridge measure and modularity
# vitality. The first two are compared with the tables printed in their source papers, all but the last with
# values worked out from their definitions, and modularity vitality with igraph's modularity() recomputed after
# each deletion.

defined <- "independent: the measure's definition worked out in base R"
exact <- 1e-10

# Zhao et al. (2015), Fig. 1 and Table 1: 21 nodes, 32 edges, four communities (every degree of Table 1 checks).
zhao <- za_graph(c(15, 17, 15, 16, 15, 20, 17, 16, 18, 16, 19, 16, 20, 16, 1, 2, 2, 21, 3, 4, 3, 5, 3, 6, 4, 5,
                   4, 6, 5, 6, 13, 12, 12, 10, 12, 11, 12, 14, 10, 11, 10, 7, 10, 14, 11, 7, 14, 9, 7, 9, 7, 8,
                   1, 16, 2, 16, 2, 10, 2, 7, 2, 3, 20, 10), 21)
zhao_membership <- c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 1)

# Tulu et al. (2018), Fig. 1 and Table 1: 32 nodes, 52 edges, six communities (every degree of Table 1 checks).
tulu <- za_graph(c(3, 4, 3, 5, 3, 6, 4, 5, 4, 6, 5, 6, 15, 16, 15, 17, 15, 20, 16, 17, 16, 18, 16, 19, 16, 20,
                   1, 2, 2, 21, 11, 12, 11, 10, 11, 7, 12, 13, 12, 10, 12, 14, 14, 10, 14, 9, 10, 7, 9, 7, 8, 29,
                   8, 26, 8, 25, 26, 30, 26, 31, 26, 32, 26, 25, 25, 32, 28, 22, 28, 27, 28, 23, 22, 27, 22, 23,
                   22, 24, 23, 24, 24, 27, 2, 6, 2, 10, 2, 7, 2, 16, 2, 22, 1, 16, 7, 8, 9, 26, 28, 8, 27, 25,
                   24, 25), 32)
tulu_membership <- c(4, 4, 1, 1, 1, 1, 2, 3, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5, 4, 6, 6, 6, 3, 3, 6, 6, 3, 3, 3, 3)

bridge6 <- za_bridge6()
bridge_membership <- c(1, 1, 1, 2, 2, 2)

test_that("community-based centrality reproduces Zhao et al. (2015), Table 1", {
  # Printed to six decimals: half a unit in the last place, 5e-7.
  printed <- c(0.428571, 1.52381, 0.714286, 0.571429, 0.571429, 0.571429, 1.666667, 0.380952, 0.761905, 1.952381,
               1.142857, 1.52381, 0.380952, 1.142857, 0.857143, 1.714286, 0.571429, 0.285714, 0.285714, 0.952381,
               0.142857)
  expect_reference(centrality_community_based(zhao, membership = zhao_membership), printed,
                   reference = "published: Zhao et al. 2015, Table 1",
                   covers = c("centrality()", "centrality_community_based()"),
                   quantity = "community_based, Zhao network", tolerance = 5e-7)
})

test_that("community-based centrality and mediator reproduce Tulu et al. (2018), Table 1", {
  # Both columns are printed to four decimals: half a unit in the last place, 5e-5. Fourteen CbC values are
  # multiples of 1/32 that end in a 5 at the fifth decimal (0.21875, 1.09375, ...): the table rounds them half
  # up (0.2188), so they sit exactly 5e-5 from the printed value. The printed decimal is not exact in binary
  # (0.2188 - 0.21875 evaluates to 5.0000000000105e-05), so the tolerance adds 1e-12 for that representation.
  cbc <- c(0.2813, 1.0938, 0.375, 0.375, 0.375, 0.4688, 0.9688, 1.0313, 0.6563, 0.9688, 0.6563, 0.875, 0.2188,
           0.6563, 0.5625, 1.125, 0.375, 0.1875, 0.1875, 0.375, 0.0938, 0.7188, 0.4688, 0.6875, 0.9688, 1.3125,
           0.6875, 0.6875, 0.2188, 0.2188, 0.2188, 0.4375)
  cbm <- c(0.0192, 0.1505, 0, 0, 0, 0.0312, 0.0659, 0.0659, 0.0265, 0.0347, 0, 0, 0, 0, 0, 0.0581, 0, 0, 0, 0, 0,
           0.0347, 0, 0.0312, 0.0467, 0.0375, 0.0312, 0.0312, 0, 0, 0, 0)
  expect_reference(centrality_community_based(tulu, membership = tulu_membership), cbc,
                   reference = "published: Tulu et al. 2018, Table 1",
                   covers = c("centrality()", "centrality_community_based()"),
                   quantity = "community_based, Tulu network", tolerance = 5e-5 + 1e-12)
  expect_reference(centrality_community_mediator(tulu, membership = tulu_membership), cbm,
                   reference = "published: Tulu et al. 2018, Table 1",
                   covers = c("centrality()", "centrality_community_mediator()"),
                   quantity = "community_mediator, Tulu network", tolerance = 5e-5)
})

test_that("community measures equal their definitions on two triangles joined by a bridge", {
  # Community-based centrality (Zhao et al. 2015, eq. 1): sum over communities w of d_iw |C_w| / N. A, B, E, F
  # have two links into a community of 3; C and D add one link into the other community of 3.
  expect_reference(centrality_community_based(bridge6, membership = bridge_membership),
                   c(A = 6, B = 6, C = 9, D = 9, E = 6, F = 6) / 6, reference = defined,
                   covers = c("centrality()", "centrality_community_based()"),
                   quantity = "community_based, two triangles", tolerance = exact)
  # Community-based mediator (Tulu et al. 2018, eqs. 11-12): the base-2 entropy of the node's links over the
  # communities, times its degree over 2L (L = 7 links). Only C and D have links outside: p = (2/3, 1/3).
  entropy <- -(2 / 3 * log2(2 / 3) + 1 / 3 * log2(1 / 3))
  expect_reference(centrality_community_mediator(bridge6, membership = bridge_membership),
                   c(A = 0, B = 0, C = entropy * 3 / 14, D = entropy * 3 / 14, E = 0, F = 0), reference = defined,
                   covers = c("centrality()", "centrality_community_mediator()"),
                   quantity = "community_mediator, two triangles", tolerance = exact)
  # Comm centrality (Gupta et al. 2016): (1 + mu) eta + (1 - mu) gamma^2, with eta and gamma the intra and
  # inter degrees scaled to R by the community maxima, and mu the community's mean share of inter links,
  # mean(0, 0, 1/3) = 1/9. With R = 2 (the largest intra degree): eta = 2 everywhere, gamma = 2 at C and D.
  mu <- 1 / 9
  inside <- (1 + mu) * 2
  expect_reference(centrality_comm_centrality(bridge6, membership = bridge_membership),
                   c(A = inside, B = inside, C = inside + (1 - mu) * 4, D = inside + (1 - mu) * 4, E = inside, F = inside),
                   reference = defined, covers = c("centrality()", "centrality_comm_centrality()"),
                   quantity = "comm_centrality, R = largest intra degree, two triangles", tolerance = exact)
  inside <- (1 + mu) * 1
  expect_reference(centrality_comm_centrality(bridge6, membership = bridge_membership, comm_r = 1),
                   c(A = inside, B = inside, C = inside + (1 - mu), D = inside + (1 - mu), E = inside, F = inside),
                   reference = defined, covers = c("centrality()", "centrality_comm_centrality()"),
                   quantity = "comm_centrality, R = 1, two triangles", tolerance = exact)
})

test_that("the community hub-bridge measure equals |C_k| d_in + d_out times the communities reached", {
  # Ghalmane et al. (2019): A, B, E, F: 3 * 2 intra links; C, D: 3 * 2 + one inter link to one community.
  expect_reference(centrality_community_hub_bridge(bridge6, membership = bridge_membership),
                   c(A = 6, B = 6, C = 7, D = 7, E = 6, F = 6), reference = defined, covers = c("centrality()", "centrality_community_hub_bridge()"),
                   quantity = "community_hub_bridge, two triangles", tolerance = exact)
  # Directed chain A -> B -> C, communities (A, B) and (C). Out-links: A has one intra link in a community of
  # 2; B one inter link reaching one community. In-links: B one intra link, C one inter link.
  chain <- za_graph(c(1, 2, 2, 3), 3, directed = TRUE, names = LETTERS[1:3])
  expect_reference(centrality_community_hub_bridge(chain, membership = c(1, 1, 2), mode = "out"),
                   c(A = 2 * 1, B = 1 * 1, C = 0), reference = defined, covers = c("centrality()", "centrality_community_hub_bridge()"),
                   quantity = "community_hub_bridge, mode out, directed chain", tolerance = exact)
  expect_reference(centrality_community_hub_bridge(chain, membership = c(1, 1, 2), mode = "in"),
                   c(A = 0, B = 2 * 1, C = 1 * 1), reference = defined, covers = c("centrality()", "centrality_community_hub_bridge()"),
                   quantity = "community_hub_bridge, mode in, directed chain", tolerance = exact)
})

test_that("modularity vitality equals igraph's modularity before minus after each deletion", {
  skip_without_reference("igraph")
  # Magelinski et al. (2021): Q(G) - Q(G - v), the partition restricted to the remaining nodes. igraph's
  # modularity() reads direction and the weights it is given.
  vitality <- \(graph, membership) {
    weights_of <- \(g) if (is.null(igraph::E(g)$weight)) NULL else igraph::E(g)$weight
    whole <- ref_igraph(graph)
    q <- igraph::modularity(whole, membership, weights = weights_of(whole))
    vapply(seq_len(nrow(graph)), \(v) {
      rest <- igraph::delete_vertices(whole, v)
      q - igraph::modularity(rest, membership[-v], weights = weights_of(rest))
    }, numeric(1)) |> stats::setNames(rownames(graph))
  }
  membership <- rep(1:3, length.out = 12)
  undirected <- ref_connected_graph(12, density = 0.3, seed = 41L)
  directed <- ref_connected_graph(12, density = 0.25, seed = 42L, directed = TRUE)
  weighted <- ref_connected_graph(12, density = 0.3, seed = 43L, weighted = TRUE, digits = 6L)
  # A modularity is a sum over at most n^2 pairs: agreement to 1e-10 on values of order 0.1.
  expect_reference(centrality_modularity_vitality(undirected, membership = membership), vitality(undirected, membership),
                   reference = "igraph", covers = c("centrality()", "centrality_modularity_vitality()"), quantity = "modularity_vitality, undirected", tolerance = exact)
  expect_reference(centrality_modularity_vitality(directed, membership = membership), vitality(directed, membership),
                   reference = "igraph", covers = c("centrality()", "centrality_modularity_vitality()"), quantity = "modularity_vitality, directed", tolerance = exact)
  expect_reference(centrality_modularity_vitality(weighted, membership = membership), vitality(weighted, membership),
                   reference = "igraph", covers = c("centrality()", "centrality_modularity_vitality()"), quantity = "modularity_vitality, undirected weighted",
                   tolerance = exact)
  expect_reference(centrality_modularity_vitality(bridge6, membership = bridge_membership),
                   vitality(bridge6, bridge_membership), reference = "igraph", covers = c("centrality()", "centrality_modularity_vitality()"),
                   quantity = "modularity_vitality, two triangles", tolerance = exact)
})
