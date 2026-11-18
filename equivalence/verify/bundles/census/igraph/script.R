library(igraph)
network <- as.matrix(utils::read.delim("../data/regulation.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "directed", weighted = TRUE)
dyads <- dyad_census(graph)
result <- data.frame(
  type = c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", "030T", "030C",
           "201", "120D", "120U", "120C", "210", "300", "mutual", "asymmetric", "null"),
  count = c(triad_census(graph), dyads$mut, dyads$asym, dyads$null))
result
