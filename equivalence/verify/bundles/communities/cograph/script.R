library(cograph)
network <- as.matrix(utils::read.delim("../data/karate.tsv", row.names = 1))
describe <- function(method) {
  found <- communities(network, method = method)
  print(found)
  data.frame(quantity = paste(method, c("communities", "modularity", paste("community of", found$node))),
             kind = c("count", "modularity", rep("membership", nrow(found))),
             value = c(n_communities(found), igraph::modularity(found), membership(found)))
}
result <- do.call(rbind, lapply(c("fast_greedy", "walktrap", "leading_eigenvector",
                                  "edge_betweenness"), describe))
