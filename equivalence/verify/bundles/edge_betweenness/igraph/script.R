library(igraph)
network <- as.matrix(utils::read.delim("../data/karate.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "undirected")
ties <- as_edgelist(graph)
result <- data.frame(edge = paste(ties[, 1], ties[, 2], sep = "-"),
                     betweenness = edge_betweenness(graph))
result
