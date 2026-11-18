library(qgraph)
network <- as.matrix(utils::read.delim("../data/network.tsv", row.names = 1))
result <- centrality_auto(network)$node.centrality
result <- data.frame(node = rownames(result), strength = result$Strength,
                     betweenness = result$Betweenness, closeness = result$Closeness)
result
