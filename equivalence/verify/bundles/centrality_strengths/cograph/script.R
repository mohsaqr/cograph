library(cograph)
network <- as.matrix(utils::read.delim("../data/network.tsv", row.names = 1))
result <- centrality(network, measures = c("strength", "betweenness", "closeness"),
                     invert_weights = TRUE)
names(result) <- sub("_all$", "", names(result))
result
