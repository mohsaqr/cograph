library(igraph)
network <- as.matrix(utils::read.delim("../data/regulation.tsv", row.names = 1))
graph <- graph_from_adjacency_matrix(network, mode = "directed", weighted = TRUE)
result <- data.frame(
  network = "regulation", node_count = vcount(graph), edge_count = ecount(graph),
  density = edge_density(graph), component_count = count_components(graph),
  diameter = diameter(graph), mean_distance = mean_distance(graph),
  min_cut = min_cut(graph),
  centralization_degree = centr_degree(graph)$centralization,
  centralization_in_degree = centr_degree(graph, mode = "in")$centralization,
  centralization_out_degree = centr_degree(graph, mode = "out")$centralization,
  centralization_betweenness = centr_betw(graph)$centralization,
  centralization_closeness = centr_clo(graph, mode = "all")$centralization,
  centralization_eigen = centr_eigen(graph)$centralization,
  transitivity = transitivity(graph), reciprocity = reciprocity(graph),
  assortativity_degree = assortativity_degree(graph),
  girth = girth(graph)$girth, radius = radius(graph),
  vertex_connectivity = vertex_connectivity(graph),
  largest_clique_size = clique_num(graph),
  cut_vertex_count = length(articulation_points(graph)), bridge_count = length(bridges(graph)),
  global_efficiency = global_efficiency(graph),
  local_efficiency = average_local_efficiency(graph))
result
