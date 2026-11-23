# centrality() and its wrappers for brokerage and structural holes: Burt's constraint, effective size and
# redundancy, flow betweenness, the bridging family (bridging centrality, bridging coefficient, local bridging),
# the Gould-Fernandez brokerage roles and the domain prestige of a directed network. Constraint and effective
# size are compared with NetworkX and igraph (cograph computes them natively); flow betweenness, brokerage and
# prestige with sna; redundancy and the bridging family with their published definitions, written in base R in
# helper-centrality-a2.R.

undirected <- ref_connected_graph(12, density = 0.3, seed = 51L)
undirected_weighted <- ref_connected_graph(12, density = 0.3, seed = 52L, weighted = TRUE, digits = 6L)
directed <- ref_connected_graph(12, density = 0.25, seed = 53L, directed = TRUE)
# Not strongly connected: some nodes are reached by only part of the network.
directed_sparse <- ref_graph(12, density = 0.15, seed = 54L, directed = TRUE)
karate <- ref_karate()

# Ratios and sums of a few doubles: agreement to the last bits is expected.
exact <- 1e-10

test_that("constraint equals NetworkX's constraint() and igraph's constraint()", {
  skip_without_reference("igraph")
  weighted <- ref_networkx(undirected_weighted, c(constraint = "nx.constraint(G, weight='weight')"))
  unweighted <- ref_networkx(karate, c(constraint = "nx.constraint(G)"))
  expect_reference(centrality_constraint(undirected_weighted), weighted$constraint, reference = "networkx",
                   covers = c("centrality()", "centrality_constraint()"), quantity = "constraint, undirected weighted",
                   tolerance = exact)
  expect_reference(centrality_constraint(karate), unweighted$constraint, reference = "networkx",
                   covers = c("centrality()", "centrality_constraint()"), quantity = "constraint, karate",
                   tolerance = exact)
  expect_reference(centrality_constraint(undirected_weighted),
                   ref_named(igraph::constraint(ref_igraph(undirected_weighted)), undirected_weighted),
                   reference = "igraph", covers = c("centrality()", "centrality_constraint()"),
                   quantity = "constraint, undirected weighted", tolerance = exact)
  expect_reference(centrality_constraint(directed), ref_named(igraph::constraint(ref_igraph(directed)), directed),
                   reference = "igraph", covers = c("centrality()", "centrality_constraint()"),
                   quantity = "constraint, directed", tolerance = exact)
})

test_that("effective size equals NetworkX's effective_size(), and redundancy its complement and definition", {
  on_undirected <- ref_networkx(undirected, c(size = "nx.effective_size(G)",
                                              redundancy = "{v: G.degree(v) - s for v, s in nx.effective_size(G).items()}"))
  on_karate <- ref_networkx(karate, c(size = "nx.effective_size(G)"))
  expect_reference(centrality_effective_size(undirected), on_undirected$size, reference = "networkx",
                   covers = c("centrality()", "centrality_effective_size()"), quantity = "effective_size, undirected",
                   tolerance = exact)
  expect_reference(centrality_effective_size(karate), on_karate$size, reference = "networkx",
                   covers = c("centrality()", "centrality_effective_size()"), quantity = "effective_size, karate",
                   tolerance = exact)
  # Burt (1992) on a directed network (cograph 2.7.20; before, Borgatti's undirected form with a reciprocated
  # neighbour counted twice): the in- and out-neighbours, ties weighed by the mutual weight a_ij + a_ji, the formula
  # NetworkX documents and computes node by node (effective_size(G, nodes=...), its redundancy() path). Its default
  # call (nodes=None) takes a sparse-matrix path that gives other values for a node with a reciprocated tie
  # (verify run of snajs, 2026-11-22: directed node A 2.8 against 2.7 from the same NetworkX's node-by-node path).
  on_directed <- ref_networkx(directed, c(size = "nx.effective_size(G, nodes=list(G))"))
  expect_reference(centrality_effective_size(directed), on_directed$size, reference = "networkx",
                   covers = c("centrality()", "centrality_effective_size()"),
                   quantity = "effective_size, directed (NetworkX's node-by-node Burt formula)", tolerance = exact)
  # Borgatti (1997): redundancy = degree - effective size on a binary network.
  expect_reference(centrality_redundancy(undirected), on_undirected$redundancy, reference = "networkx",
                   covers = c("centrality()", "centrality_redundancy()"),
                   quantity = "redundancy, undirected (degree - effective size)", tolerance = exact)
  expect_reference(centrality_redundancy(karate), ref_a2_redundancy(karate),
                   reference = "independent: the brokerage and bridging definitions written in base R (each named in its note)", note = "Burt (1992) redundancy as 2t / k (Borgatti 1997), base R",
                   covers = c("centrality()", "centrality_redundancy()"), quantity = "redundancy, karate",
                   tolerance = exact)
})

test_that("bridging centrality, the bridging coefficient and local bridging equal their definitions", {
  skip_without_reference("igraph")
  coefficient <- ref_a2_bridging_coefficient(undirected_weighted)
  expect_reference(centrality_bridging_coefficient(undirected_weighted), coefficient,
                   reference = "independent: the brokerage and bridging definitions written in base R (each named in its note)", note = "Hwang et al. (2008) bridging coefficient, base R",
                   covers = c("centrality()", "centrality_bridging_coefficient()"),
                   quantity = "bridging_coefficient, undirected", tolerance = exact)
  expect_reference(centrality_bridging_coefficient(karate), ref_a2_bridging_coefficient(karate),
                   reference = "independent: the brokerage and bridging definitions written in base R (each named in its note)", note = "Hwang et al. (2008) bridging coefficient, base R",
                   covers = c("centrality()", "centrality_bridging_coefficient()"),
                   quantity = "bridging_coefficient, karate", tolerance = exact)
  # Bridging centrality (Hwang et al. 2008) is betweenness times the bridging coefficient; betweenness from
  # igraph, with the weights read as distances.
  expect_reference(centrality_bridging(undirected_weighted),
                   coefficient * ref_named(igraph::betweenness(ref_igraph(undirected_weighted)), undirected_weighted),
                   reference = "independent: the brokerage and bridging definitions written in base R (each named in its note)", note = "Hwang et al. (2008) bridging centrality, igraph betweenness",
                   covers = c("centrality()", "centrality_bridging()"), quantity = "bridging, undirected weighted",
                   tolerance = exact, measure = "scaled")
  # Local bridging (as CINNA): the bridging coefficient over the degree.
  expect_reference(centrality_local_bridging(karate),
                   ref_a2_bridging_coefficient(karate) / rowSums(ref_a2_binary(karate)),
                   reference = "independent: the brokerage and bridging definitions written in base R (each named in its note)", note = "bridging coefficient / degree (CINNA's local bridging), base R",
                   covers = c("centrality()", "centrality_local_bridging()"), quantity = "local_bridging, karate",
                   tolerance = exact)
})

test_that("flow betweenness equals sna's flowbet()", {
  skip_without_reference("sna")
  # Freeman, Borgatti and White (1991) as sna computes it (cmode = "rawflow"): over the pairs s, t, the maximum
  # flow from s to t less the maximum flow once the node is removed, as cograph does since 2.7.19. cograph 2.7.18
  # and earlier read the flow passing through the node from the one maximum flow igraph::max_flow() returns, which
  # is not unique (N1 42 against 19, N3 43 against 21 undirected).
  expect_reference(centrality_flow_betweenness(undirected), ref_named(sna::flowbet(undirected, gmode = "graph"), undirected),
                   reference = "sna", covers = c("centrality()", "centrality_flow_betweenness()"),
                   quantity = "flow_betweenness, undirected", tolerance = exact, measure = "scaled")
  expect_reference(centrality_flow_betweenness(directed), ref_named(sna::flowbet(directed, gmode = "digraph"), directed),
                   reference = "sna", covers = c("centrality()", "centrality_flow_betweenness()"),
                   quantity = "flow_betweenness, directed", tolerance = exact, measure = "scaled")
})

test_that("the five Gould-Fernandez brokerage roles equal sna's brokerage()", {
  skip_without_reference("sna")
  membership <- rep(c(1L, 2L, 3L), length.out = 12)
  roles <- sna::brokerage(directed, cl = membership)$raw.nli
  role_of <- \(column) ref_named(roles[, column], directed)
  covers_role <- \(role) c("centrality()", paste0("centrality_brokerage_", role, "()"))
  expect_reference(centrality_brokerage_coordinator(directed, membership = membership), role_of("w_I"),
                   reference = "sna", covers = covers_role("coordinator"), quantity = "brokerage coordinator (w_I)",
                   tolerance = exact)
  expect_reference(centrality_brokerage_itinerant(directed, membership = membership), role_of("w_O"),
                   reference = "sna", covers = covers_role("itinerant"), quantity = "brokerage itinerant (w_O)",
                   tolerance = exact)
  expect_reference(centrality_brokerage_representative(directed, membership = membership), role_of("b_IO"),
                   reference = "sna", covers = covers_role("representative"),
                   quantity = "brokerage representative (b_IO)", tolerance = exact)
  expect_reference(centrality_brokerage_gatekeeper(directed, membership = membership), role_of("b_OI"),
                   reference = "sna", covers = covers_role("gatekeeper"), quantity = "brokerage gatekeeper (b_OI)",
                   tolerance = exact)
  expect_reference(centrality_brokerage_liaison(directed, membership = membership), role_of("b_O"),
                   reference = "sna", covers = covers_role("liaison"), quantity = "brokerage liaison (b_O)",
                   tolerance = exact)
})

test_that("domain prestige equals sna's prestige(cmode = 'domain') and 'domain.proximity'", {
  skip_without_reference("sna")
  expect_reference(centrality(directed_sparse, measures = "prestige_domain"),
                   ref_nodes(prestige_domain = ref_named(sna::prestige(directed_sparse, cmode = "domain"), directed_sparse)),
                   reference = "sna", covers = "centrality()", quantity = "prestige_domain, directed, not strongly connected",
                   tolerance = exact, key = "node")
  # sna's domain.proximity zeros every node when any pair is unreachable (it multiplies FALSE by Inf, giving
  # NaN, and then sets NaN to 0), so it is compared on a strongly connected network only.
  expect_reference(centrality(directed, measures = "prestige_domain_proximity"),
                   ref_nodes(prestige_domain_proximity =
                               ref_named(sna::prestige(directed, cmode = "domain.proximity"), directed)),
                   reference = "sna", covers = "centrality()",
                   quantity = "prestige_domain_proximity, directed, strongly connected", tolerance = exact, key = "node")
  # Lin (1976) proximity prestige, defined on any directed network: with I_v the nodes that reach v and D_v the
  # sum of their distances to v, I_v^2 / ((n - 1) D_v), and 0 when nothing reaches v.
  to_v <- ref_a2_hops(directed_sparse)
  reached <- is.finite(to_v) & to_v > 0
  influence <- colSums(reached)
  proximity <- ifelse(influence > 0, influence^2 / ((nrow(to_v) - 1) * colSums(ifelse(reached, to_v, 0))), 0)
  expect_reference(centrality(directed_sparse, measures = "prestige_domain_proximity"),
                   ref_nodes(prestige_domain_proximity = proximity),
                   reference = "independent: the brokerage and bridging definitions written in base R (each named in its note)", note = "Lin (1976) proximity prestige, base R", covers = "centrality()",
                   quantity = "prestige_domain_proximity, directed, not strongly connected", tolerance = exact, key = "node")
})
