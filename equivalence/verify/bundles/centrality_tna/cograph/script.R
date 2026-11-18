library(cograph)
network <- as.matrix(utils::read.delim("../data/transitions.tsv", row.names = 1))
model <- tna::tna(network)
measures <- c("strength", "closeness")
incoming <- centrality(model, measures = c(measures, "diffusion"), mode = "in")
outgoing <- centrality(model, measures = measures, mode = "out")
both <- centrality(model, measures = c("closeness", "betweenness", "transitivity", "pagerank"),
                   mode = "all")
result <- merge(merge(incoming, outgoing, by = "node"), both, by = "node")
result
