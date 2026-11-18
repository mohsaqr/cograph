library(igraph)
network <- as.matrix(utils::read.delim("../data/regulation.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "directed", weighted = TRUE)
hits <- hits_scores(graph)
result <- data.frame(node = V(graph)$name, pagerank = page_rank(graph)$vector,
                     hub = hits$hub, authority = hits$authority,
                     eigenvector = eigen_centrality(graph, directed = TRUE)$vector)
result
