library(cograph)
network <- as.matrix(utils::read.delim("../data/regulation.tsv", row.names = 1))
measures <- c("degree", "strength", "closeness", "harmonic", "eccentricity")
incoming <- centrality(network, measures = c(measures, "betweenness"), mode = "in")
outgoing <- centrality(network, measures = measures, mode = "out")
result <- merge(incoming, outgoing, by = "node")
result
