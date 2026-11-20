# The structural measures that are not one score per node: group centrality (of a set of nodes), dispersion
# (of a tie), the Estrada index and trophic incoherence (of a network). Each is compared with NetworkX, the
# Estrada index also with its spectral definition in base R. cograph computes all of these natively.

undirected <- ref_connected_graph(14, density = 0.3, seed = 61L)
directed <- ref_connected_graph(14, density = 0.25, seed = 62L, directed = TRUE)
karate <- ref_karate()
# A directed acyclic network (upper triangle of a random one): node 1 is basal, so trophic levels exist.
acyclic <- ref_graph(12, density = 0.35, seed = 63L, directed = TRUE)
acyclic[lower.tri(acyclic)] <- 0

# Counts, ratios and one eigen-decomposition: agreement to the last bits is expected.
exact <- 1e-10

test_that("group closeness and degree equal NetworkX's group centralities", {
  group <- c("N2", "N5", "N9")
  python_group <- "{'N2', 'N5', 'N9'}"
  on_undirected <- ref_networkx(undirected, c(
    closeness = sprintf("nx.group_closeness_centrality(G, %s)", python_group),
    degree = sprintf("nx.group_degree_centrality(G, %s)", python_group)))
  on_directed <- ref_networkx(directed, c(
    out_degree = sprintf("nx.group_out_degree_centrality(G, %s)", python_group),
    in_degree = sprintf("nx.group_in_degree_centrality(G, %s)", python_group)))
  # Group betweenness is not compared with NetworkX: nx.group_betweenness_centrality() 3.7 depends on Python's
  # string-hash order. On the ci machine 40 calls on one graph returned 0.048611 37 times (cograph's value) and
  # 0.0625, 0.076389, 0.090278 once each (probe of 2026-11-20). Group betweenness is checked against its
  # definition in test-centrality-zoo-a-betweenness.R.
  expect_reference(group_centrality(undirected, nodes = group, measure = "closeness"), on_undirected$closeness,
                   reference = "networkx", covers = "group_centrality()", quantity = "group closeness, undirected",
                   tolerance = exact)
  expect_reference(group_centrality(undirected, nodes = group, measure = "degree"), on_undirected$degree,
                   reference = "networkx", covers = "group_centrality()", quantity = "group degree, undirected",
                   tolerance = exact)
  expect_reference(group_centrality(directed, nodes = group, measure = "degree", mode = "out"), on_directed$out_degree,
                   reference = "networkx", covers = "group_centrality()", quantity = "group degree, mode out, directed",
                   tolerance = exact)
  expect_reference(group_centrality(directed, nodes = group, measure = "degree", mode = "in"), on_directed$in_degree,
                   reference = "networkx", covers = "group_centrality()", quantity = "group degree, mode in, directed",
                   tolerance = exact)
})

test_that("group closeness and degree equal their definitions", {
  group <- c(2L, 5L, 9L)
  others <- setdiff(seq_len(nrow(undirected)), group)
  hops <- ref_a2_hops(undirected)
  # Everett and Borgatti (1999): |V \ C| / sum over v outside C of min_{c in C} d(v, c); and the share of the
  # nodes outside C adjacent to C.
  closeness <- length(others) / sum(apply(hops[others, group, drop = FALSE], 1L, min))
  degree <- sum(rowSums(undirected[others, group, drop = FALSE]) > 0) / length(others)
  expect_reference(group_centrality(undirected, nodes = group, measure = "closeness"), closeness,
                   reference = "independent: Everett and Borgatti (1999) group closeness, base R",
                   covers = "group_centrality()", quantity = "group closeness, undirected", tolerance = exact)
  expect_reference(group_centrality(undirected, nodes = group, measure = "degree"), degree,
                   reference = "independent: Everett and Borgatti (1999) group degree, base R",
                   covers = "group_centrality()", quantity = "group degree, undirected", tolerance = exact)
})

test_that("dispersion equals NetworkX's dispersion()", {
  all_ties <- ref_networkx(karate, c(
    normalised = "[[u, v, d] for u, row in nx.dispersion(G).items() for v, d in row.items()]",
    raw = "nx.dispersion(G, '1', '34', normalized=False)"))
  expect_reference(dispersion(karate), ref_a2_triples(all_ties$normalised, karate, "dispersion"),
                   reference = "networkx", covers = "dispersion()", quantity = "dispersion, normalised, karate",
                   tolerance = exact, key = c("from", "to"))
  expect_reference(dispersion(karate, u = "1", v = "34", normalized = FALSE), all_ties$raw, reference = "networkx",
                   covers = "dispersion()", quantity = "dispersion, unnormalised, karate tie 1-34", tolerance = exact)
})

test_that("the Estrada index equals NetworkX's estrada_index() and the sum of exp(eigenvalues)", {
  nx <- ref_networkx(undirected, c(estrada = "nx.estrada_index(G)"))
  expect_reference(estrada_index(undirected), nx$estrada, reference = "networkx", covers = "estrada_index()",
                   quantity = "Estrada index, undirected", tolerance = exact, measure = "relative")
  # Estrada (2000): sum over the eigenvalues lambda of the adjacency matrix of exp(lambda).
  expect_reference(estrada_index(karate), sum(exp(eigen(karate, symmetric = TRUE, only.values = TRUE)$values)),
                   reference = "independent: Estrada (2000) sum of exp(eigenvalues), base R", covers = "estrada_index()",
                   quantity = "Estrada index, karate", tolerance = exact, measure = "relative")
})

test_that("trophic incoherence equals NetworkX's trophic_incoherence_parameter()", {
  nx <- ref_networkx(acyclic, c(incoherence = "nx.trophic_incoherence_parameter(G)"))
  expect_reference(trophic_incoherence(acyclic), nx$incoherence, reference = "networkx",
                   covers = "trophic_incoherence()", quantity = "trophic incoherence, directed acyclic",
                   tolerance = exact)
})
