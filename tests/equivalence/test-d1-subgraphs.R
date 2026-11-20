# Measures read off subgraphs and blocks: ego_networks() (ego and alter ties and densities, Burt's effective size
# and constraint), select_k_core() (Seidman 1983), split_components(), core_periphery() (the fit of Borgatti &
# Everett 2000 for the coreness it returns) and layer_degree_correlation() / ldegcor(). ego_networks() and
# split_components() read igraph (igraph::ego(), induced_subgraph(), constraint(), components()), so igraph is
# `internal` for them; NetworkX (ego_graph(), effective_size(), constraint(), k_core(), connected_components()) and
# stats are the counted references.
#
# Graphs: random undirected graphs from helper-reference.R (the ring of ref_connected_graph() gives every node at
# least two neighbours, so every alter density is defined).

undirected <- d1_binary(ref_connected_graph(12, density = 0.3, seed = 161L))
cored <- ref_graph(14, density = 0.3, seed = 141L)
# Coreness 2, 3 and 4: the 3-core and the 4-core differ.
layered_cores <- ref_graph(14, density = 0.35, seed = 145L)
fragmented <- ref_graph(12, density = 0.12, seed = 52L)

exact <- 1e-10

test_that("ego_networks() equals NetworkX's ego graphs, effective size and constraint", {
  nx <- ref_networkx(undirected, c(
    size = "{v: len(G[v]) for v in G}",
    ego_ties = "{v: nx.ego_graph(G, v).number_of_edges() for v in G}",
    ego_density = "{v: nx.density(nx.ego_graph(G, v)) for v in G}",
    alter_ties = "{v: G.subgraph(G[v]).number_of_edges() for v in G}",
    alter_density = "{v: nx.density(G.subgraph(G[v])) for v in G}",
    effective_size = "nx.effective_size(G)",
    constraint = "nx.constraint(G)",
    size_2 = "{v: len(nx.ego_graph(G, v, radius=2)) - 1 for v in G}",
    ego_ties_2 = "{v: nx.ego_graph(G, v, radius=2).number_of_edges() for v in G}"))
  measures <- c("size", "ego_ties", "ego_density", "alter_ties", "alter_density", "effective_size", "constraint")
  expect_reference(ego_networks(undirected), do.call(ref_nodes, nx[measures]), reference = "networkx",
                   covers = "ego_networks()", quantity = "ego network, order 1", tolerance = exact, key = "node")
  expect_reference(ego_networks(undirected, order = 2), ref_nodes(size = nx$size_2, ego_ties = nx$ego_ties_2),
                   reference = "networkx", covers = "ego_networks()", quantity = "ego network, order 2",
                   tolerance = exact, key = "node")
})

test_that("select_k_core() keeps the nodes of NetworkX's k_core()", {
  nx <- ref_networkx(layered_cores, c(core_3 = "sorted(nx.k_core(G, 3))", core_4 = "sorted(nx.k_core(G, 4))"))
  nodes <- rownames(layered_cores)
  kept <- \(k) nodes %in% rownames(select_k_core(layered_cores, k = k, keep_format = TRUE))
  expect_reference(data.frame(node = nodes, core_3 = kept(3), core_4 = kept(4)),
                   data.frame(node = nodes, core_3 = nodes %in% unlist(nx$core_3), core_4 = nodes %in% unlist(nx$core_4)),
                   reference = "networkx", covers = "select_k_core()", quantity = "node in the k-core", tolerance = 0,
                   key = "node")
})

test_that("split_components() equals NetworkX's connected_components()", {
  nx <- ref_networkx(fragmented, c(size = "{v: len(c) for c in nx.connected_components(G) for v in c}"))
  components <- split_components(fragmented, keep_format = TRUE)
  sizes <- unlist(lapply(components, \(component) stats::setNames(rep(nrow(component), nrow(component)), rownames(component))))
  names(sizes) <- sub("^component_[0-9]+\\.", "", names(sizes))
  expect_reference(sizes, nx$size, reference = "networkx", covers = "split_components()",
                   quantity = "size of the component of each node", tolerance = exact)
})

test_that("core_periphery() fitness and block densities equal their definitions for its coreness", {
  # Continuous: the correlation of the off-diagonal adjacency (lower triangle) with outer(c, c). Discrete: the
  # correlation with the ideal pattern (a tie between two core nodes), and the densities of the two blocks.
  binary <- d1_binary(cored)
  lower <- lower.tri(binary)
  density_of <- \(block) mean(block[row(block) != col(block)])
  continuous <- core_periphery(cored)
  discrete <- core_periphery(cored, method = "discrete")
  core_c <- continuous$role == "core"
  core_d <- discrete$role == "core"
  ideal <- outer(core_d, core_d, "&") * 1
  expect_reference(c(continuous_fitness = attr(continuous, "fitness"),
                     continuous_core_density = attr(continuous, "core_density"),
                     continuous_periphery_density = attr(continuous, "periphery_density"),
                     discrete_fitness = attr(discrete, "fitness"), discrete_core_density = attr(discrete, "core_density"),
                     discrete_periphery_density = attr(discrete, "periphery_density")),
                   c(continuous_fitness = stats::cor(binary[lower], outer(continuous$coreness, continuous$coreness)[lower]),
                     continuous_core_density = density_of(binary[core_c, core_c]),
                     continuous_periphery_density = density_of(binary[!core_c, !core_c]),
                     discrete_fitness = stats::cor(binary[lower], ideal[lower]),
                     discrete_core_density = density_of(binary[core_d, core_d]),
                     discrete_periphery_density = density_of(binary[!core_d, !core_d])),
                   reference = "stats", covers = "core_periphery()", quantity = "core-periphery fit for the returned roles",
                   tolerance = exact)
})

test_that("layer_degree_correlation() and ldegcor() equal stats::cor() of the layer degrees", {
  layers <- list(first = ref_graph(10, density = 0.3, seed = 171L, directed = TRUE),
                 second = ref_graph(10, density = 0.3, seed = 172L, directed = TRUE, weighted = TRUE),
                 third = ref_graph(10, density = 0.4, seed = 173L, directed = TRUE))
  # The degrees are row plus column sums of each layer (strength on a weighted layer), as documented.
  total <- vapply(layers, \(layer) rowSums(layer) + colSums(layer), numeric(10))
  incoming <- vapply(layers, colSums, numeric(10))
  expect_reference(layer_degree_correlation(layers), stats::cor(total), reference = "stats",
                   covers = "layer_degree_correlation()", quantity = "degree correlation, total", tolerance = exact)
  expect_reference(ldegcor(layers, mode = "in"), stats::cor(incoming), reference = "stats",
                   covers = c("layer_degree_correlation()", "ldegcor()"), quantity = "degree correlation, in",
                   tolerance = exact)
})
