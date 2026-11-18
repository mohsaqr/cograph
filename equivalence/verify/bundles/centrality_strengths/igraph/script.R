library(igraph)
network <- as.matrix(utils::read.delim("../data/network.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "undirected", weighted = TRUE)
lengths <- 1 / E(graph)$weight
result <- data.frame(node = V(graph)$name, strength = strength(graph),
                     betweenness = betweenness(graph, weights = lengths),
                     closeness = closeness(graph, weights = lengths))
result
