# Edge-level measures: edge_betweenness() and edge_centrality() (Girvan & Newman 2002 edge betweenness, neighbourhood
# overlap of Onnela et al. 2007, Simmelian strength = triangles through a tie, reciprocity of an arc),
# neighborhood_overlap(), simmelian_strength() and edge_reciprocity(). cograph computes all of them itself
# (.cg_edge_betweenness() and the neighbour sets in R/centrality.R), so igraph's edge_betweenness() is a counted
# reference beside NetworkX's edge_betweenness_centrality() and the definitions.
#
# Graphs: connected random graphs from helper-reference.R with six-decimal weights (read as distances by edge
# betweenness), so that weighted shortest paths do not tie.

undirected_weighted <- ref_connected_graph(12, density = 0.3, seed = 111L, weighted = TRUE, digits = 6L)
directed_weighted <- ref_connected_graph(12, density = 0.3, seed = 112L, directed = TRUE, weighted = TRUE, digits = 6L)
undirected <- d1_binary(ref_connected_graph(12, density = 0.3, seed = 113L))

exact <- 1e-10

# Shared and all neighbours of the two ends of each tie, the ends themselves left out (in and out on a directed graph).
neighbour_counts <- \(graph, directed) {
  binary <- d1_binary(graph)
  linked <- (binary + t(binary)) > 0
  ties <- d1_ties(graph, directed)
  counts <- vapply(seq_len(nrow(ties)), \(e) {
    ends <- c(ties$row[[e]], ties$col[[e]])
    first <- setdiff(which(linked[ends[[1L]], ]), ends)
    second <- setdiff(which(linked[ends[[2L]], ]), ends)
    c(shared = length(intersect(first, second)), all = length(union(first, second)))
  }, numeric(2))
  data.frame(from = ties$from, to = ties$to, shared = counts["shared", ], all = counts["all", ])
}

test_that("edge_betweenness() and edge_centrality() betweenness equal NetworkX", {
  py <- c(betweenness = paste0("{(f'{u}|{v}' if int(u[1:]) < int(v[1:]) or G.is_directed() else f'{v}|{u}'): b ",
                               "for (u, v), b in nx.edge_betweenness_centrality(G, normalized=False, weight='weight').items()}"))
  on_undirected <- ref_networkx(undirected_weighted, py)
  on_directed <- ref_networkx(directed_weighted, py)
  expect_reference(edge_centrality(undirected_weighted, measures = "betweenness"),
                   d1_edge_table(on_undirected$betweenness, undirected_weighted, directed = FALSE, column = "betweenness"),
                   reference = "networkx", covers = "edge_centrality()", quantity = "edge betweenness, undirected weighted",
                   tolerance = exact, measure = "scaled", key = c("from", "to"))
  # edge_betweenness() names each value "from->to".
  directed_values <- on_directed$betweenness
  names(directed_values) <- sub("|", "->", names(directed_values), fixed = TRUE)
  expect_reference(edge_betweenness(directed_weighted), directed_values, reference = "networkx",
                   covers = "edge_betweenness()", quantity = "edge betweenness, directed weighted", tolerance = exact,
                   measure = "scaled")
})

test_that("edge_centrality() betweenness equals igraph's edge_betweenness()", {
  skip_without_reference("igraph")
  # igraph reads an adjacency matrix in the same row-major edge order cograph lists ties in.
  graph <- ref_igraph(directed_weighted)
  ends <- igraph::as_edgelist(graph)
  expect_reference(edge_centrality(directed_weighted, measures = "betweenness"),
                   data.frame(from = ends[, 1L], to = ends[, 2L], betweenness = igraph::edge_betweenness(graph)),
                   reference = "igraph", covers = "edge_centrality()", quantity = "edge betweenness, directed weighted",
                   tolerance = exact, measure = "scaled", key = c("from", "to"))
  on_undirected <- ref_igraph(undirected)
  ends_u <- igraph::as_edgelist(on_undirected)
  expect_reference(edge_betweenness(undirected),
                   stats::setNames(igraph::edge_betweenness(on_undirected), paste(ends_u[, 1L], ends_u[, 2L], sep = "->")),
                   reference = "igraph", covers = "edge_betweenness()", quantity = "edge betweenness, undirected",
                   tolerance = exact, measure = "scaled")
})

test_that("neighbourhood overlap and Simmelian strength equal their definitions", {
  # Onnela et al. (2007): O_ij = n_ij / ((k_i - 1) + (k_j - 1) - n_ij), the shared neighbours over all neighbours of
  # the two ends (0 when there are none); Simmelian strength: the triangles through the tie, n_ij.
  counts <- neighbour_counts(undirected_weighted, directed = FALSE)
  overlap <- data.frame(from = counts$from, to = counts$to,
                        overlap = ifelse(counts$all == 0, 0, counts$shared / counts$all),
                        shared_neighbors = counts$shared)
  expect_reference(neighborhood_overlap(undirected_weighted), overlap,
                   reference = "independent: neighbourhood overlap (Onnela et al. 2007)",
                   covers = c("neighborhood_overlap()", "edge_centrality()"), quantity = "neighbourhood overlap, undirected",
                   tolerance = exact, key = c("from", "to"))
  expect_reference(simmelian_strength(undirected_weighted),
                   data.frame(from = counts$from, to = counts$to, triangles = counts$shared),
                   reference = "independent: triangles through each tie", covers = c("simmelian_strength()", "edge_centrality()"),
                   quantity = "Simmelian strength, undirected", tolerance = exact, key = c("from", "to"))
  on_directed <- neighbour_counts(directed_weighted, directed = TRUE)
  expect_reference(edge_centrality(directed_weighted, measures = c("overlap", "simmelian")),
                   data.frame(from = on_directed$from, to = on_directed$to,
                              overlap = ifelse(on_directed$all == 0, 0, on_directed$shared / on_directed$all),
                              triangles = on_directed$shared),
                   reference = "independent: neighbourhood overlap and triangles over in- and out-neighbours",
                   covers = "edge_centrality()", quantity = "overlap and Simmelian strength, directed", tolerance = exact,
                   key = c("from", "to"))
})

test_that("Simmelian strength equals the triangles NetworkX counts through each tie", {
  nx <- ref_networkx(undirected, c(triangles = paste0(
    "{(f'{u}|{v}' if int(u[1:]) < int(v[1:]) else f'{v}|{u}'): len(set(G[u]) & set(G[v])) for u, v in G.edges()}")))
  expect_reference(simmelian_strength(undirected),
                   d1_edge_table(nx$triangles, undirected, directed = FALSE, column = "triangles"),
                   reference = "networkx", covers = "simmelian_strength()", quantity = "Simmelian strength, unweighted",
                   tolerance = exact, key = c("from", "to"))
})

test_that("edge_reciprocity() equals the reverse arcs of the weight matrix", {
  ties <- d1_ties(directed_weighted, directed = TRUE)
  forward <- directed_weighted[cbind(ties$row, ties$col)]
  backward <- directed_weighted[cbind(ties$col, ties$row)]
  reciprocated <- backward != 0
  expected <- data.frame(from = ties$from, to = ties$to, weight = forward, reciprocated = reciprocated,
                         reverse_weight = ifelse(reciprocated, backward, NA_real_),
                         weight_ratio = ifelse(reciprocated, backward / forward, NA_real_))
  # A one-way arc has no reverse weight and no ratio: those cells are NA on both sides (not comparable).
  expect_reference(edge_reciprocity(directed_weighted), expected,
                   reference = "independent: reverse arc of each arc in the weight matrix",
                   covers = c("edge_reciprocity()", "edge_centrality()"), quantity = "edge reciprocity, directed weighted",
                   tolerance = exact, key = c("from", "to"))
  # NetworkX's reciprocity of a node pair: an arc is reciprocated when the reverse arc exists.
  nx <- ref_networkx(directed_weighted, c(reciprocated = "{f'{u}|{v}': float(G.has_edge(v, u)) for u, v in G.edges()}"))
  reciprocated_nx <- d1_edge_table(nx$reciprocated, directed_weighted, directed = TRUE, column = "reciprocated")
  reciprocated_nx$reciprocated <- reciprocated_nx$reciprocated == 1
  expect_reference(edge_reciprocity(directed_weighted), reciprocated_nx, reference = "networkx",
                   covers = "edge_reciprocity()", quantity = "reciprocated arc", tolerance = 0, key = c("from", "to"))
})
