# cluster_quality() and its alias cqual(): per-cluster cut, internal weight, density, expansion, cut ratio and
# conductance, and the global modularity and coverage of a given partition. cograph computes these itself (no
# igraph call), so NetworkX's cuts module (cut_size, volume, conductance), its subgraph sizes and densities,
# and its community.modularity() / partition_quality() are an independent implementation of the same
# definitions (Fortunato 2010, section 3.1; Newman and Girvan 2004; Leicht and Newman 2008 for directed
# modularity).
#
# The per-cluster quantities NetworkX does not name are the same cut and size combined by their definitions:
# cut ratio c_S / (n_S (n - n_S)), expansion c_S / n_S, average internal degree 2 m_S / n_S, and conductance
# c_S / vol(S) (cograph divides by the volume of the cluster itself; see the known difference below).

undirected <- ref_connected_graph(12, density = 0.3, seed = 31L, weighted = TRUE, digits = 6L)
directed <- ref_connected_graph(12, density = 0.25, seed = 32L, directed = TRUE, weighted = TRUE, digits = 6L)
clusters <- list(A = paste0("N", 1:4), B = paste0("N", 5:8), C = paste0("N", 9:12))
uneven <- list(small = paste0("N", 1:3), large = paste0("N", 4:12))
membership_of <- \(groups) stats::setNames(rep(seq_along(groups), lengths(groups)), unlist(groups))

# Sums of a few dozen weights and one division: rounding only.
exact <- 1e-10

# One NetworkX call per cluster and quantity, named "<quantity>.<cluster>".
nx_per_cluster <- function(graph, groups, expressions) {
  calls <- unlist(lapply(names(groups), \(name) stats::setNames(
    sprintf(expressions, py_strings(groups[[name]])), paste(names(expressions), name, sep = "."))))
  ref_networkx(graph, calls)
}
nx_column <- \(nx, quantity, groups) vapply(names(groups), \(name) nx[[paste(quantity, name, sep = ".")]], numeric(1))

test_that("per-cluster cut, internal weight and their ratios equal NetworkX's cuts on an undirected graph", {
  nx <- nx_per_cluster(undirected, clusters, c(
    cut = "(lambda S: nx.cut_size(G, S, set(G) - set(S), weight='weight'))(%s)", volume = "nx.volume(G, %s, weight='weight')",
    internal = "G.subgraph(%s).size(weight='weight')"))
  sizes <- lengths(clusters)
  cut <- nx_column(nx, "cut", clusters)
  internal <- nx_column(nx, "internal", clusters)
  expected <- data.frame(cluster_name = names(clusters), n_nodes = as.numeric(sizes), internal_edges = internal,
                         cut_edges = cut, avg_internal_degree = 2 * internal / sizes, expansion = cut / sizes,
                         cut_ratio = cut / (sizes * (12 - sizes)), conductance = cut / nx_column(nx, "volume", clusters))
  quality <- cluster_quality(undirected, clusters, directed = FALSE)
  expect_reference(quality$per_cluster, expected, reference = "networkx", covers = "cluster_quality()",
                   quantity = "per cluster, undirected weighted", tolerance = exact, key = "cluster_name")
  expect_reference(cqual(undirected, clusters, directed = FALSE)$per_cluster, expected, reference = "networkx",
                   covers = c("cqual()", "cluster_quality()"), quantity = "per cluster, cqual(), undirected weighted",
                   tolerance = exact, key = "cluster_name")
})

test_that("per-cluster cut and internal weight equal NetworkX's on a directed graph", {
  # The complement is passed explicitly: NetworkX 3.7's cut_size() with T = None fails on a directed graph.
  # NetworkX's cut_size() of a directed graph counts the arcs leaving S and the arcs entering S, as cograph does.
  nx <- nx_per_cluster(directed, clusters, c(
    cut = "(lambda S: nx.cut_size(G, S, set(G) - set(S), weight='weight'))(%s)", internal = "G.subgraph(%s).size(weight='weight')"))
  cut <- nx_column(nx, "cut", clusters)
  expected <- data.frame(cluster_name = names(clusters), internal_edges = nx_column(nx, "internal", clusters),
                         cut_edges = cut, cut_ratio = cut / (lengths(clusters) * (12 - lengths(clusters))))
  expect_reference(cluster_quality(directed, clusters, directed = TRUE)$per_cluster, expected,
                   reference = "networkx", covers = "cluster_quality()", quantity = "per cluster, directed weighted",
                   tolerance = exact, key = "cluster_name")
})

test_that("internal density of the binary graph equals NetworkX's density() of the cluster's subgraph", {
  binary_undirected <- (undirected != 0) * 1
  binary_directed <- (directed != 0) * 1
  on_undirected <- nx_per_cluster(binary_undirected, clusters, c(density = "nx.density(G.subgraph(%s))"))
  on_directed <- nx_per_cluster(binary_directed, clusters, c(density = "nx.density(G.subgraph(%s))"))
  expect_reference(cluster_quality(undirected, clusters, weighted = FALSE, directed = FALSE)$per_cluster,
                   data.frame(cluster_name = names(clusters), internal_density = nx_column(on_undirected, "density", clusters)),
                   reference = "networkx", covers = "cluster_quality()", quantity = "internal density, undirected binary",
                   tolerance = exact, key = "cluster_name")
  expect_reference(cluster_quality(directed, clusters, weighted = FALSE, directed = TRUE)$per_cluster,
                   data.frame(cluster_name = names(clusters), internal_density = nx_column(on_directed, "density", clusters)),
                   reference = "networkx", covers = "cluster_quality()", quantity = "internal density, directed binary",
                   tolerance = exact, key = "cluster_name")
})

test_that("conductance divides by the cluster's own volume, NetworkX's by the smaller side", {
  nx <- nx_per_cluster(undirected, uneven, c(conductance = "nx.conductance(G, %s, weight='weight')",
                                             cut = "(lambda S: nx.cut_size(G, S, set(G) - set(S), weight='weight'))(%s)",
                                             volume = "nx.volume(G, %s, weight='weight')"))
  quality <- cluster_quality(undirected, uneven, directed = FALSE)$per_cluster
  expect_known_difference(quality, data.frame(cluster_name = names(uneven),
                                              conductance = nx_column(nx, "conductance", uneven)),
                          reference = "networkx", covers = "cluster_quality()",
                          quantity = "conductance, two clusters of 3 and 9 nodes", tolerance = exact, key = "cluster_name",
                          reason = paste("cluster_quality() reports c_S / vol(S) for each cluster; NetworkX's",
                                         "conductance() is c_S / min(vol(S), vol(V - S)). They agree for the",
                                         "cluster whose volume is the smaller one and differ for the other."),
                          agrees_with = "NetworkX cut_size(S) / volume(S)")
  expect_reference(quality, data.frame(cluster_name = names(uneven),
                                       conductance = nx_column(nx, "cut", uneven) / nx_column(nx, "volume", uneven)),
                   reference = "networkx", covers = "cluster_quality()",
                   quantity = "conductance as cut / own volume, two clusters", tolerance = exact, key = "cluster_name")
})

test_that("modularity equals NetworkX's community.modularity(), undirected and directed", {
  nx_undirected <- ref_networkx(undirected, c(q = sprintf("nx.community.modularity(G, %s, weight='weight')",
                                                          py_partition(membership_of(clusters)))))
  nx_directed <- ref_networkx(directed, c(q = sprintf("nx.community.modularity(G, %s, weight='weight')",
                                                      py_partition(membership_of(clusters)))))
  expect_reference(cluster_quality(undirected, clusters, directed = FALSE)$global$modularity, nx_undirected$q,
                   reference = "networkx", covers = "cluster_quality()", quantity = "modularity, undirected weighted",
                   tolerance = exact)
  expect_reference(cluster_quality(directed, clusters, directed = TRUE)$global$modularity, nx_directed$q,
                   reference = "networkx", covers = "cluster_quality()", quantity = "modularity, directed weighted",
                   tolerance = exact)
})

test_that("coverage equals NetworkX's partition_quality() on the binary graph and the definition when weighted", {
  # partition_quality() counts edges (no weights): it is compared with cluster_quality(weighted = FALSE).
  nx <- ref_networkx((undirected != 0) * 1, c(quality = sprintf("list(nx.community.partition_quality(G, %s))",
                                                                py_partition(membership_of(clusters)))))
  expect_reference(cluster_quality(undirected, clusters, weighted = FALSE, directed = FALSE)$global$coverage,
                   nx$quality[[1L]], reference = "networkx", covers = "cluster_quality()",
                   quantity = "coverage, undirected binary", tolerance = exact)
  # coverage = weight inside the clusters / total weight (Fortunato 2010, eq. 11, weighted)
  inside <- sum(vapply(clusters, \(members) sum(undirected[members, members]), numeric(1)))
  expect_reference(cluster_quality(undirected, clusters, directed = FALSE)$global$coverage, inside / sum(undirected),
                   reference = "independent: weight inside clusters / total weight, base R", covers = "cluster_quality()",
                   quantity = "coverage, undirected weighted", tolerance = exact)
})

test_that("modularity equals Newman's definition written in base R", {
  expect_reference(cluster_quality(undirected, clusters, directed = FALSE)$global$modularity,
                   ref_modularity(undirected, membership_of(clusters)),
                   reference = "independent: Newman (2006) modularity, base R", covers = "cluster_quality()",
                   quantity = "modularity, undirected weighted", tolerance = exact)
  expect_reference(cluster_quality(directed, clusters, directed = TRUE)$global$modularity,
                   ref_modularity(directed, membership_of(clusters), directed = TRUE),
                   reference = "independent: Leicht and Newman (2008) directed modularity, base R",
                   covers = "cluster_quality()", quantity = "modularity, directed weighted", tolerance = exact)
})
