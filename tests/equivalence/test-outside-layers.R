# Multilayer similarity and aggregation, robustness summaries and the small-world coefficient set beside outside
# software, in addition to the definitions of test-multilayer.R, test-robustness.R and test-small-world.R:
#
# - layer_similarity() / lsim() and layer_similarity_matrix() / lsim_matrix() by Jaccard equal multinet's
#   layer_comparison_ml(method = "jaccard.edges"); the overlap coefficient |A & B| / min(|A|, |B|) is the larger
#   of multinet's two "coverage.edges" values;
# - aggregate_layers() / lagg() by sum of binary layers equals multinet's flatten_ml(method = "weighted"), which
#   weighs each flattened edge by the number of layers that hold it;
# - robustness_auc() and robustness_summary() equal pracma's trapz() of the attack curve, cograph's own curve and
#   brainGraph's robustness() curve.
# network_small_world() has no outside reference with its null model: qgraph's smallworldness(), NetworkX's sigma()
# and brainGraph's small.world() compare with degree-preserving random graphs (or local transitivity), cograph
# with G(n, m) graphs and global transitivity (test-small-world.R checks it by Monte Carlo in base R).
#
# Inputs: binary multiplex networks drawn by simulab (helper-simulab.R), directed and undirected; random graphs of
# helper-reference.R for the attacks.

multiplex <- list(directed_3 = sim_layers(10, 3, seed = 51L, probability = 0.3, directed = TRUE),
                  undirected_4 = sim_layers(12, 4, seed = 52L, probability = 0.25, directed = FALSE),
                  directed_2 = sim_layers(8, 2, seed = 53L, probability = 0.5, directed = TRUE))

# Ratios of small counts: rounding only.
exact <- 1e-12

#' A multinet network with one layer per matrix (directed when the matrix is not symmetric)
ml_of <- function(layers) {
  net <- multinet::ml_empty()
  lapply(names(layers), \(name) {
    mode <- if (isSymmetric(unname(layers[[name]]))) "undirected" else "directed"
    multinet::add_igraph_layer_ml(net, igraph::graph_from_adjacency_matrix(layers[[name]], mode = mode), name)
  })
  net
}

#' multinet's layer-by-layer table in the order of `layers`
ml_table <- function(net, layers, method) {
  table <- as.matrix(multinet::layer_comparison_ml(net, method = method))
  # multinet names the columns with make.names() ("Layer.1"); the rows carry the layer names
  colnames(table) <- rownames(table)
  table[names(layers), names(layers)]
}

test_that("Jaccard and overlap similarity of layers equal multinet's layer_comparison_ml()", {
  skip_without_reference("multinet")
  lapply(names(multiplex), \(name) {
    layers <- multiplex[[name]]
    net <- ml_of(layers)
    jaccard <- ml_table(net, layers, "jaccard.edges")
    coverage <- ml_table(net, layers, "coverage.edges")
    expect_reference(layer_similarity_matrix(layers, method = "jaccard"), jaccard, reference = "multinet",
                     covers = "layer_similarity_matrix()", quantity = sprintf("Jaccard of edges, every pair, %s", name),
                     tolerance = exact)
    expect_reference(lsim_matrix(layers, method = "jaccard"), jaccard, reference = "multinet",
                     covers = c("lsim_matrix()", "layer_similarity_matrix()"),
                     quantity = sprintf("lsim_matrix() Jaccard, %s", name), tolerance = exact)
    expect_reference(layer_similarity(layers[[1L]], layers[[2L]], method = "jaccard"), jaccard[[1L, 2L]],
                     reference = "multinet", covers = "layer_similarity()",
                     quantity = sprintf("Jaccard of layers 1 and 2, %s", name), tolerance = exact)
    expect_reference(lsim(layers[[1L]], layers[[2L]], method = "overlap"), max(coverage[[1L, 2L]], coverage[[2L, 1L]]),
                     reference = "multinet", covers = c("lsim()", "layer_similarity()"),
                     quantity = sprintf("overlap of layers 1 and 2, larger coverage, %s", name), tolerance = exact)
  })
})

test_that("the sum of binary layers equals multinet's weighted flattening", {
  skip_without_reference("multinet")
  lapply(names(multiplex), \(name) {
    layers <- multiplex[[name]]
    net <- ml_of(layers)
    multinet::flatten_ml(net, new.layer = "flat", layers = names(layers), method = "weighted")
    edges <- as.data.frame(multinet::edges_ml(net, layers1 = "flat"))
    weights <- multinet::get_values_ml(net, attribute = "weight", edges = edges)$weight
    nodes <- rownames(layers[[1L]])
    flattened <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
    flattened[cbind(edges$from_actor, edges$to_actor)] <- weights
    if (isSymmetric(unname(layers[[1L]]))) flattened[cbind(edges$to_actor, edges$from_actor)] <- weights
    expect_reference(aggregate_layers(layers, method = "sum"), flattened, reference = "multinet",
                     covers = "aggregate_layers()", quantity = sprintf("sum of binary layers, %s", name), tolerance = exact)
    expect_reference(lagg(layers, method = "sum"), flattened, reference = "multinet",
                     covers = c("lagg()", "aggregate_layers()"), quantity = sprintf("lagg() sum, %s", name),
                     tolerance = exact)
  })
})

test_that("robustness_auc() and robustness_summary() equal pracma's trapz() of the attack curves", {
  skip_without_reference(c("pracma", "brainGraph"))
  graphs <- list(sparse_14 = ref_connected_graph(14, density = 0.2, seed = 101L),
                 dense_16 = ref_connected_graph(16, density = 0.3, seed = 102L),
                 scale_free_20 = sim_graph(20, seed = 54L, model = "barabasi_albert", directed = FALSE))
  lapply(names(graphs), \(name) lapply(c("degree", "betweenness"), \(measure) {
    attack <- robustness(graphs[[name]], measure = measure, strategy = "static")
    expect_reference(robustness_auc(attack), pracma::trapz(attack$removed_pct, attack$comp_pct), reference = "pracma",
                     covers = "robustness_auc()", quantity = sprintf("area under the %s attack curve, %s", measure, name),
                     tolerance = exact)
    brain <- brainGraph::robustness(ref_igraph(graphs[[name]]), type = "vertex",
                                    measure = if (measure == "degree") "degree" else "btwn.cent")
    # robustness_summary() rounds to four decimals: half a unit in the fourth place, absolute.
    expect_reference(robustness_summary(attack)$auc, pracma::trapz(brain$removed.pct, brain$comp.pct),
                     reference = "brainGraph", covers = "robustness_summary()",
                     quantity = sprintf("area under brainGraph's %s attack curve (pracma trapz), %s", measure, name),
                     tolerance = 5e-5)
  }))
})
