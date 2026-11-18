library(igraph)
network <- as.matrix(utils::read.delim("../data/network.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "undirected", weighted = TRUE)
result <- data.frame(node = V(graph)$name, degree = degree(graph), strength = strength(graph),
                     betweenness = betweenness(graph), closeness = closeness(graph),
                     eigenvector = eigen_centrality(graph)$vector)
result
