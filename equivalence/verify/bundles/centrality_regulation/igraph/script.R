library(igraph)
network <- as.matrix(utils::read.delim("../data/regulation.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "directed", weighted = TRUE)
result <- data.frame(
  node = V(graph)$name,
  degree_in = degree(graph, mode = "in"), degree_out = degree(graph, mode = "out"),
  strength_in = strength(graph, mode = "in"), strength_out = strength(graph, mode = "out"),
  closeness_in = closeness(graph, mode = "in"), closeness_out = closeness(graph, mode = "out"),
  harmonic_in = harmonic_centrality(graph, mode = "in"),
  harmonic_out = harmonic_centrality(graph, mode = "out"),
  eccentricity_in = eccentricity(graph, mode = "in"),
  eccentricity_out = eccentricity(graph, mode = "out"),
  betweenness = betweenness(graph))
result
