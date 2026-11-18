library(igraph)
network <- as.matrix(utils::read.delim("../data/karate.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "undirected")
result <- data.frame(node = V(graph)$name, harmonic = harmonic_centrality(graph),
                     eccentricity = eccentricity(graph), coreness = coreness(graph),
                     constraint = constraint(graph),
                     transitivity = transitivity(graph, type = "local"),
                     subgraph = subgraph_centrality(graph), alpha = alpha_centrality(graph),
                     power = power_centrality(graph))
result
