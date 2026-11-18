library(cograph)
network <- as.matrix(utils::read.delim("../data/karate.tsv", row.names = 1))
edges <- edge_centrality(network, measures = "betweenness")
result <- data.frame(edge = paste(edges$from, edges$to, sep = "-"), betweenness = edges$betweenness)
result
