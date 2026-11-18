library(igraph)
network <- as.matrix(utils::read.delim("../data/karate.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "undirected")
describe <- function(method, found) {
  print(found)
  data.frame(quantity = paste(method, c("communities", "modularity", paste("community of", V(graph)$name))),
             kind = c("count", "modularity", rep("membership", vcount(graph))),
             value = c(length(found), modularity(found), membership(found)))
}
result <- rbind(describe("fast_greedy", cluster_fast_greedy(graph)),
                describe("walktrap", cluster_walktrap(graph)),
                describe("leading_eigenvector", cluster_leading_eigen(graph)),
                describe("edge_betweenness", cluster_edge_betweenness(graph)))
