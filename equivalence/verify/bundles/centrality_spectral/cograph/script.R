library(cograph)
network <- as.matrix(utils::read.delim("../data/regulation.tsv", row.names = 1))
result <- centrality(network, measures = c("pagerank", "hub", "authority", "eigenvector"))
result
