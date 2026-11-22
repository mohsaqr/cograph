# Weight verbs, the disparity backbone, simplify() and is_bipartite() set beside outside software, in addition to
# the definitions of test-weights.R, test-disparity.R and test-wrangle.R:
#
# - binarize() and threshold_edges() equal sna's event2dichot() (absolute rule), normalize_weights() by row and
#   column equals sna's make.stochastic();
# - disparity_filter() of every input type equals backbone's disparity model (Neal 2025), the implementation of
#   Serrano, Boguna and Vespignani (2009) on CRAN;
# - aggregate_weights() / wagg() equal stats::median() and psych's geometric.mean();
# - simplify() of a matrix and of a tna model equals igraph's simplify() (cograph does not call it there);
# - is_bipartite() of a square matrix equals NetworkX's is_bipartite() (it reports a computed property: whether
#   the graph is two-colourable).
#
# Inputs: random graphs of helper-reference.R, heavy-tailed weights for the backbone (strength concentrated on a
# few edges, so both outcomes occur), and simulab networks (helper-simulab.R) of other shapes and sizes.

directed <- ref_graph(10, density = 0.4, seed = 61L, directed = TRUE, weighted = TRUE, digits = 6L)
undirected <- ref_graph(10, density = 0.45, seed = 62L, weighted = TRUE, digits = 6L)
simulated <- list(
  bernoulli_directed = sim_graph(15, seed = 21L, model = "bernoulli", probability = 0.25, directed = TRUE,
                                 weight = "uniform"),
  bernoulli_undirected = sim_graph(15, seed = 22L, model = "bernoulli", probability = 0.25, directed = FALSE,
                                   weight = "uniform"),
  scale_free = sim_graph(18, seed = 23L, model = "barabasi_albert", directed = FALSE),
  block_directed = sim_graph(18, seed = 24L, model = "block", directed = TRUE, weight = "uniform"))
heavy_tailed <- function(nodes, density, seed, directed) {
  graph <- ref_graph(nodes, density = density, seed = seed, directed = directed)
  weights <- ref_with_seed(seed + 100L, matrix(round(stats::rexp(nodes * nodes), 6L) + 1e-3, nodes, nodes))
  if (!directed) weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]
  graph * weights
}
backbones <- list(undirected_15 = heavy_tailed(15, 0.35, 41L, FALSE), directed_15 = heavy_tailed(15, 0.3, 42L, TRUE),
                  undirected_25 = heavy_tailed(25, 0.25, 43L, FALSE), directed_25 = heavy_tailed(25, 0.2, 44L, TRUE))
# simulab uniform weights times exponential noise: a heavier tail on simulab's shapes
backbones$simulab_directed <- simulated$bernoulli_directed * ref_with_seed(45L, matrix(stats::rexp(225), 15, 15))

# One comparison or one division per number: rounding only.
exact <- 1e-12

test_that("binarize() and threshold_edges() equal sna's event2dichot()", {
  skip_without_reference("sna")
  graphs <- c(list(directed = directed, undirected = undirected), simulated)
  lapply(names(graphs), \(name) lapply(c(0, 0.4), \(cut) {
    graph <- graphs[[name]]
    expect_reference(binarize(graph, threshold = cut, keep_format = TRUE),
                     sna::event2dichot(graph, method = "absolute", thresh = cut), reference = "sna",
                     covers = "binarize()", quantity = sprintf("binarize above %s, %s", cut, name), tolerance = 0)
  }))
  lapply(names(graphs), \(name) {
    graph <- graphs[[name]]
    expect_reference(threshold_edges(graph, minimum = 0.4, keep_format = TRUE),
                     graph * sna::event2dichot(graph, method = "absolute", thresh = 0.4), reference = "sna",
                     covers = "threshold_edges()", quantity = sprintf("weights kept at minimum 0.4, %s", name),
                     tolerance = 0)
  })
  # absolute = TRUE reads |w|: sna is given |w|
  signed <- directed * ifelse(upper.tri(directed), -1, 1)
  expect_reference(binarize(signed, threshold = 0.4, keep_format = TRUE),
                   sna::event2dichot(abs(signed), method = "absolute", thresh = 0.4), reference = "sna",
                   covers = "binarize()", quantity = "binarize |w| above 0.4, signed", tolerance = 0)
})

test_that("normalize_weights() by row and by column equals sna's make.stochastic()", {
  skip_without_reference("sna")
  # Graphs in which every node sends and receives weight: make.stochastic() divides an empty row by 0.
  graphs <- list(directed = ref_connected_graph(10, density = 0.3, seed = 63L, directed = TRUE, weighted = TRUE, digits = 6L),
                 undirected = ref_connected_graph(10, density = 0.3, seed = 64L, weighted = TRUE, digits = 6L),
                 small_world = sim_graph(16, seed = 25L, model = "small_world"), scale_free = simulated$scale_free)
  lapply(names(graphs), \(name) {
    graph <- graphs[[name]]
    expect_reference(normalize_weights(graph, method = "row", keep_format = TRUE), sna::make.stochastic(graph, mode = "row"),
                     reference = "sna", covers = "normalize_weights()", quantity = sprintf("row proportions, %s", name),
                     tolerance = exact)
    expect_reference(normalize_weights(graph, method = "column", keep_format = TRUE),
                     sna::make.stochastic(graph, mode = "col"), reference = "sna", covers = "normalize_weights()",
                     quantity = sprintf("column proportions, %s", name), tolerance = exact)
  })
})

test_that("disparity_filter() of every input type equals backbone's disparity model", {
  skip_without_reference("backbone")
  kept <- \(graph, level) {
    out <- backbone::backbone_from_weighted(graph, model = "disparity", alpha = level, narrative = FALSE)
    matrix(as.integer(out != 0), nrow(graph), ncol(graph), dimnames = dimnames(graph))
  }
  lapply(names(backbones), \(name) lapply(c(0.05, 0.3), \(level) {
    expect_reference(disparity_filter(backbones[[name]], level = level), kept(backbones[[name]], level),
                     reference = "backbone", covers = c("disparity_filter()", "disparity_filter.matrix()"),
                     quantity = sprintf("kept edges, %s, level %s", name, level), tolerance = 0)
  }))
  graph <- backbones$directed_25
  expected <- kept(graph, 0.3)
  cells <- which(graph != 0, arr.ind = TRUE)
  edge_list <- data.frame(from = rownames(graph)[cells[, 1L]], to = colnames(graph)[cells[, 2L]], weight = graph[cells])
  expect_reference(disparity_filter(edge_list, level = 0.3)$significant, expected, reference = "backbone",
                   covers = c("disparity_filter()", "disparity_filter.default()"),
                   quantity = "kept edges, edge-list data frame, directed 25", tolerance = 0)
  expect_reference(disparity_filter(as_cograph(graph), level = 0.3)$significant, expected, reference = "backbone",
                   covers = c("disparity_filter()", "disparity_filter.cograph_network()"),
                   quantity = "kept edges, cograph_network, directed 25", tolerance = 0)
  expect_reference(disparity_filter(ref_igraph(graph), level = 0.3)$significant, expected, reference = "backbone",
                   covers = c("disparity_filter()", "disparity_filter.igraph()"),
                   quantity = "kept edges, igraph graph, directed 25", tolerance = 0)
  skip_without_reference("tna")
  model <- tna::tna(tna::group_regulation)
  lapply(c(0.3, 0.5), \(level) {
    expect_reference(disparity_filter(model, level = level)$significant, kept(model$weights, level),
                     reference = "backbone", covers = c("disparity_filter()", "disparity_filter.tna()"),
                     quantity = sprintf("kept edges, tna model of group_regulation, level %s", level), tolerance = 0)
  })
})

test_that("aggregate_weights() and wagg() equal stats::median() and psych's geometric.mean()", {
  samples <- list(five = c(0.4, 0, 1.7, 2.5, NA, 0.9, 3.1),
                  uniform_40 = ref_with_seed(3L, round(stats::runif(40, 0.1, 1), 6)),
                  simulab_weights = simulated$bernoulli_directed[simulated$bernoulli_directed != 0])
  lapply(names(samples), \(name) {
    present <- samples[[name]][!is.na(samples[[name]]) & samples[[name]] != 0]
    expect_reference(aggregate_weights(samples[[name]], method = "median"), stats::median(present), reference = "stats",
                     covers = "aggregate_weights()", quantity = sprintf("median of the non-zero weights, %s", name),
                     tolerance = exact)
    skip_without_reference("psych")
    expect_reference(wagg(samples[[name]], method = "geomean"), psych::geometric.mean(present), reference = "psych",
                     covers = c("wagg()", "aggregate_weights()"),
                     quantity = sprintf("geometric mean of the non-zero weights, %s", name), tolerance = exact)
  })
})

test_that("simplify() of a matrix and of a tna model equals igraph's simplify()", {
  skip_without_reference("igraph")
  without_loops <- \(graph) {
    simple <- igraph::simplify(ref_igraph(graph, weighted = TRUE), remove.loops = TRUE)
    igraph::as_adjacency_matrix(simple, attr = "weight", sparse = FALSE)[rownames(graph), colnames(graph)]
  }
  graphs <- list(directed = directed, undirected = undirected, block_directed = simulated$block_directed)
  lapply(names(graphs), \(name) {
    looped <- graphs[[name]]
    diag(looped) <- ref_with_seed(7L, round(stats::runif(nrow(looped), 0, 1), 6))
    expect_reference(simplify(looped), without_loops(looped), reference = "igraph",
                     covers = c("simplify()", "simplify.matrix()"), quantity = sprintf("matrix, loops dropped, %s", name),
                     tolerance = 0)
  })
  skip_without_reference("tna")
  model <- tna::tna(tna::group_regulation)
  expect_reference(simplify(model)$weights, without_loops(model$weights), reference = "igraph",
                   covers = c("simplify()", "simplify.tna()"), quantity = "tna model, loops dropped", tolerance = 0)
})

test_that("is_bipartite() of a square matrix equals NetworkX's is_bipartite()", {
  even_ring <- zoob_ring(8)
  odd_ring <- zoob_ring(7)
  incidence <- as.data.frame(simulab::simulate_bipartite_network(actors = 7, events = 5, probability = 0.4, seed = 3L),
                             what = "incidence")
  two_mode <- as.matrix(incidence[, setdiff(names(incidence), "actor")])
  labels <- c(incidence$actor, colnames(two_mode))
  one_mode <- matrix(0, length(labels), length(labels), dimnames = list(labels, labels))
  one_mode[seq_len(nrow(two_mode)), nrow(two_mode) + seq_len(ncol(two_mode))] <- two_mode
  one_mode <- one_mode + t(one_mode)
  graphs <- c(list(even_ring = even_ring, odd_ring = odd_ring, simulab_two_mode = one_mode,
                   random = (undirected != 0) * 1, tree = ref_tree(12, seed = 4L)), simulated)
  lapply(names(graphs), \(name) {
    graph <- graphs[[name]]
    nx <- ref_networkx(graph, c(b = "nx.is_bipartite(G)"))$b
    expect_reference(is_bipartite(graph), isTRUE(nx), reference = "networkx", covers = "is_bipartite()",
                     quantity = sprintf("bipartite, %s", name), tolerance = 0)
  })
})
