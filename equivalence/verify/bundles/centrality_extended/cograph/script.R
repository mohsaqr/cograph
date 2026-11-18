library(cograph)
network <- as.matrix(utils::read.delim("../data/karate.tsv", row.names = 1))
result <- centrality(network, measures = c("harmonic", "eccentricity", "coreness", "constraint",
                                           "transitivity", "subgraph", "alpha", "power"))
names(result) <- sub("_all$", "", names(result))
result
