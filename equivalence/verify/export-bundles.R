# Exports cograph comparison cases as bundles for Prova Verify (format:
# plan/24-verify.md in the prova repository), with Prova's R writer.
#
# Every side of a case is an R script, stored in the bundle and run there in its
# own R process: its console is the side's raw output and the table it leaves in
# `result` holds the side's values.
#
# Run from the project root:
#   Rscript equivalence/verify/export-bundles.R [case ...]
source(Sys.getenv("PROVA_VERIFY_R", file.path("..", "prova", "reporters", "verify.R")))

.verify_base <- file.path("equivalence", "verify")
.verify_generator <- "cograph equivalence/verify/export-bundles.R"

# ---- data --------------------------------------------------------------------

.verify_write_matrix <- function(network, name) {
  file <- file.path(tempdir(), name)
  utils::write.table(network, file, sep = "\t", quote = FALSE, col.names = NA)
  file
}

#' The hand-made network of the three demonstration cases
#'
#' Six nodes: a triangle A-B-C, the bridge C-D, a triangle D-E-F, with unequal
#' weights so that weighted measures differ from unweighted ones.
#' @return The path of a tab-separated adjacency matrix named `network.tsv`.
.verify_network <- function() {
  nodes <- LETTERS[seq_len(6L)]
  network <- matrix(0, 6L, 6L, dimnames = list(nodes, nodes))
  network[cbind(c(1, 1, 2, 3, 4, 4, 5), c(2, 3, 3, 4, 5, 6, 6))] <- c(2, 1, 3, 1, 2, 1, 4)
  .verify_write_matrix(network + t(network), "network.tsv")
}

#' cograph's bundled `regulation_net`: ten states, directed, weighted
#' @return The path of `regulation.tsv`.
.verify_regulation <- function() {
  bundled <- new.env()
  load(file.path("data", "regulation_net.rda"), envir = bundled)
  .verify_write_matrix(bundled$regulation_net, "regulation.tsv")
}

#' Zachary's karate club (34 members, 78 ties, undirected, unweighted), the
#' network cograph's examples and tests take from `igraph::make_graph("Zachary")`
#' @return The path of `karate.tsv`.
.verify_karate <- function() {
  network <- igraph::as_adjacency_matrix(igraph::make_graph("Zachary"), sparse = FALSE)
  members <- sprintf("v%02d", seq_len(nrow(network)))
  dimnames(network) <- list(members, members)
  .verify_write_matrix(network, "karate.tsv")
}

#' The transition probabilities of `tna::tna(tna::group_regulation)`: nine
#' states, directed, with self-loops, the model cograph's tna tests use
#' @return The path of `transitions.tsv`.
.verify_transitions <- function() {
  .verify_write_matrix(tna::tna(tna::group_regulation)$weights, "transitions.tsv")
}

.verify_data <- list(
  network = list(label = "Network", file = "network.tsv", write = .verify_network),
  regulation = list(label = "Regulation network", file = "regulation.tsv", write = .verify_regulation),
  karate = list(label = "Karate club", file = "karate.tsv", write = .verify_karate),
  transitions = list(label = "Transition probabilities", file = "transitions.tsv", write = .verify_transitions)
)

.verify_read <- function(data) {
  sprintf("network <- as.matrix(utils::read.delim(\"../data/%s\", row.names = 1))", .verify_data[[data]]$file)
}

# ---- sides -------------------------------------------------------------------

.cograph <- function(script, key = "node", data = "network") {
  verify_side("cograph", package = "cograph", primary = TRUE, load = ".", key = key,
              script = c("library(cograph)", .verify_read(data), script))
}
.reference <- function(package, script, key = "node", data = "network") {
  verify_side(tolower(package), package = package, authority = "open-source", key = key,
              script = c(sprintf("library(%s)", package), .verify_read(data), script))
}
.igraph <- function(script, key = "node", data = "network") .reference("igraph", script, key, data)
.qgraph <- function(script, key = "node", data = "network") .reference("qgraph", script, key, data)

# ---- cases -------------------------------------------------------------------

#' Every case: its words, its data, its tolerance and the script of each side
#'
#' A script's paths are relative to its own folder inside the bundle. A script
#' that compares numbers leaves a data frame `result`; a script that draws
#' writes `plot.png`. `needs` names the reference packages a case cannot be
#' exported without; `sides` is a function, called only once they are known to
#' be installed, because verify_side() reads a side's installed version. Every tolerance was written down before its case was first
#' run, with its reason beside it.
#' @return A named list of cases.
.verify_cases <- function() {
  list(
    # --- the three demonstration cases on the hand-made network ---------------
    centrality_distances = list(
      title = "Centralities, weights as distances",
      description = "Degree, strength, betweenness, closeness and eigenvector centrality of a six-node weighted network. A weight is the length of its edge, the convention of igraph.",
      covers = "centrality()", data = "network", needs = "igraph",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(c(
          "result <- centrality(network, measures = c(\"degree\", \"strength\", \"betweenness\", \"closeness\", \"eigenvector\"))",
          "names(result) <- sub(\"_all$\", \"\", names(result))",
          "result")),
        .igraph(c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"undirected\", weighted = TRUE)",
          "result <- data.frame(node = V(graph)$name, degree = degree(graph), strength = strength(graph),",
          "                     betweenness = betweenness(graph), closeness = closeness(graph),",
          "                     eigenvector = eigen_centrality(graph)$vector)",
          "result")))),
    centrality_strengths = list(
      title = "Centralities, weights as strengths",
      description = "The same network with a weight read as the strength of its edge, so that a path's length is the sum of inverse weights: the convention of qgraph.",
      covers = "centrality()", data = "network", needs = c("igraph", "qgraph"),
      tolerance = 1e-10, measure = "absolute",
      variant = list(of = "centrality_distances", option = "invert_weights", value = "TRUE"),
      sides = \() list(
        .cograph(c(
          "result <- centrality(network, measures = c(\"strength\", \"betweenness\", \"closeness\"),",
          "                     invert_weights = TRUE)",
          "names(result) <- sub(\"_all$\", \"\", names(result))",
          "result")),
        .igraph(c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"undirected\", weighted = TRUE)",
          "lengths <- 1 / E(graph)$weight",
          "result <- data.frame(node = V(graph)$name, strength = strength(graph),",
          "                     betweenness = betweenness(graph, weights = lengths),",
          "                     closeness = closeness(graph, weights = lengths))",
          "result")),
        .qgraph(c(
          "result <- centrality_auto(network)$node.centrality",
          "result <- data.frame(node = rownames(result), strength = result$Strength,",
          "                     betweenness = result$Betweenness, closeness = result$Closeness)",
          "result")))),
    plot_circle = list(
      title = "A weighted network drawn on a circle",
      description = "The same six-node network drawn by splot() and by qgraph(), both on a circular layout. A case for the eye: nothing numeric is compared.",
      covers = "splot()", data = "network", needs = "qgraph",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(key = NULL, c(
          "png(\"plot.png\", width = 900, height = 900, res = 150)",
          "splot(network, layout = \"circle\")",
          "dev.off()")),
        .qgraph(key = NULL, c(
          "png(\"plot.png\", width = 900, height = 900, res = 150)",
          "qgraph(network, layout = \"circle\")",
          "dev.off()")))),

    # --- the standard centralities on a bundled, directed, weighted network ---
    # Tolerance 1e-10 absolute: counts, sums of weights and shortest-path
    # quantities are closed-form in both packages, so only the order of
    # floating-point additions can differ (the largest value, a harmonic
    # centrality, is below 100).
    centrality_regulation = list(
      title = "Standard centralities of a directed weighted network",
      description = "In- and out-degree, strength, closeness, harmonic centrality and eccentricity, and betweenness, of cograph's bundled regulation network (ten states, thirty directed weighted edges). A weight is the length of its edge. centrality() computes these with its own kernels, without igraph.",
      covers = "centrality()", data = "regulation", needs = "igraph",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(data = "regulation", c(
          "measures <- c(\"degree\", \"strength\", \"closeness\", \"harmonic\", \"eccentricity\")",
          "incoming <- centrality(network, measures = c(measures, \"betweenness\"), mode = \"in\")",
          "outgoing <- centrality(network, measures = measures, mode = \"out\")",
          "result <- merge(incoming, outgoing, by = \"node\")",
          "result")),
        .igraph(data = "regulation", c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"directed\", weighted = TRUE)",
          "result <- data.frame(",
          "  node = V(graph)$name,",
          "  degree_in = degree(graph, mode = \"in\"), degree_out = degree(graph, mode = \"out\"),",
          "  strength_in = strength(graph, mode = \"in\"), strength_out = strength(graph, mode = \"out\"),",
          "  closeness_in = closeness(graph, mode = \"in\"), closeness_out = closeness(graph, mode = \"out\"),",
          "  harmonic_in = harmonic_centrality(graph, mode = \"in\"),",
          "  harmonic_out = harmonic_centrality(graph, mode = \"out\"),",
          "  eccentricity_in = eccentricity(graph, mode = \"in\"),",
          "  eccentricity_out = eccentricity(graph, mode = \"out\"),",
          "  betweenness = betweenness(graph))",
          "result")))),

    # Tolerance 1e-8 absolute: every score here is the fixed point of an
    # iteration (igraph uses ARPACK and PRPACK, cograph its own solvers), each
    # stopped at its own convergence threshold; the scores lie in [0, 1].
    centrality_spectral = list(
      title = "Eigenvector-type centralities of a directed weighted network",
      description = "PageRank, hub and authority scores and eigenvector centrality of the bundled regulation network, each the leading eigenvector of a matrix built from the directed weighted network.",
      covers = "centrality()", data = "regulation", needs = "igraph",
      tolerance = 1e-8, measure = "absolute",
      sides = \() list(
        .cograph(data = "regulation", c(
          "result <- centrality(network, measures = c(\"pagerank\", \"hub\", \"authority\", \"eigenvector\"))",
          "result")),
        .igraph(data = "regulation", c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"directed\", weighted = TRUE)",
          "hits <- hits_scores(graph)",
          "result <- data.frame(node = V(graph)$name, pagerank = page_rank(graph)$vector,",
          "                     hub = hits$hub, authority = hits$authority,",
          "                     eigenvector = eigen_centrality(graph, directed = TRUE)$vector)",
          "result")))),

    # Tolerance 1e-8 relative: the first five measures are counts and
    # shortest-path sums, but subgraph, alpha and power centrality come from an
    # eigendecomposition or a linear solve, whose rounding depends on the
    # routine and on the conditioning of the matrix; their values range over
    # three orders of magnitude, hence relative.
    centrality_extended = list(
      title = "Less common centralities on the karate club",
      description = "Harmonic centrality, eccentricity, coreness, Burt's constraint, local transitivity, subgraph centrality, alpha centrality and Bonacich power centrality of Zachary's karate club (34 members, 78 ties).",
      covers = "centrality()", data = "karate", needs = "igraph",
      tolerance = 1e-8, measure = "relative",
      sides = \() list(
        .cograph(data = "karate", c(
          "result <- centrality(network, measures = c(\"harmonic\", \"eccentricity\", \"coreness\", \"constraint\",",
          "                                           \"transitivity\", \"subgraph\", \"alpha\", \"power\"))",
          "names(result) <- sub(\"_all$\", \"\", names(result))",
          "result")),
        .igraph(data = "karate", c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"undirected\")",
          "result <- data.frame(node = V(graph)$name, harmonic = harmonic_centrality(graph),",
          "                     eccentricity = eccentricity(graph), coreness = coreness(graph),",
          "                     constraint = constraint(graph),",
          "                     transitivity = transitivity(graph, type = \"local\"),",
          "                     subgraph = subgraph_centrality(graph), alpha = alpha_centrality(graph),",
          "                     power = power_centrality(graph))",
          "result")))),

    # Tolerance 1e-10 absolute: cograph's own tests hold these to 1e-12 against
    # tna; the values are sums and ratios of probabilities below 3, and
    # betweenness is a count.
    centrality_tna = list(
      title = "Centralities of a transition network",
      description = "The centralities tna reports for a transition network, computed by centrality() on the same tna model (nine states, transition probabilities with self-loops, from tna's group_regulation data). cograph switches to tna's conventions when it is given a tna model: self-loops dropped, weights inverted for paths, Onnela clustering. Diffusion is asked of tna with normalize = TRUE, which is what tna from 1.5.8 on returns in any case and what cograph follows. tna's BetweennessRSP is left out: tna rounds it and shifts it so that the smallest is 1, cograph returns it unrounded.",
      covers = "centrality()", data = "transitions", needs = "tna",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(data = "transitions", c(
          "model <- tna::tna(network)",
          "measures <- c(\"strength\", \"closeness\")",
          "incoming <- centrality(model, measures = c(measures, \"diffusion\"), mode = \"in\")",
          "outgoing <- centrality(model, measures = measures, mode = \"out\")",
          "both <- centrality(model, measures = c(\"closeness\", \"betweenness\", \"transitivity\", \"pagerank\"),",
          "                   mode = \"all\")",
          "result <- merge(merge(incoming, outgoing, by = \"node\"), both, by = \"node\")",
          "result")),
        .reference("tna", data = "transitions", c(
          "model <- tna(network)",
          "measures <- c(\"InStrength\", \"OutStrength\", \"ClosenessIn\", \"ClosenessOut\", \"Closeness\",",
          "              \"Betweenness\", \"Clustering\", \"PageRank\")",
          "plain <- as.data.frame(centralities(model, measures = measures))",
          "scaled <- as.data.frame(centralities(model, measures = \"Diffusion\", normalize = TRUE))",
          "result <- data.frame(node = as.character(plain$state),",
          "                     strength_in = plain$InStrength, strength_out = plain$OutStrength,",
          "                     closeness_in = plain$ClosenessIn, closeness_out = plain$ClosenessOut,",
          "                     closeness_all = plain$Closeness, betweenness = plain$Betweenness,",
          "                     transitivity = plain$Clustering, pagerank = plain$PageRank,",
          "                     diffusion_in = scaled$Diffusion)",
          "result")))),

    # Tolerance 1e-10 absolute: edge betweenness on an unweighted network is a
    # sum of fractions of shortest paths (largest value below 100).
    edge_betweenness = list(
      title = "Edge betweenness on the karate club",
      description = "The betweenness of each of the 78 ties of Zachary's karate club, from edge_centrality(), which computes it with cograph's own kernel.",
      covers = "edge_centrality()", data = "karate", needs = "igraph",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(key = "edge", data = "karate", c(
          "edges <- edge_centrality(network, measures = \"betweenness\")",
          "result <- data.frame(edge = paste(edges$from, edges$to, sep = \"-\"), betweenness = edges$betweenness)",
          "result")),
        .igraph(key = "edge", data = "karate", c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"undirected\")",
          "ties <- as_edgelist(graph)",
          "result <- data.frame(edge = paste(ties[, 1], ties[, 2], sep = \"-\"),",
          "                     betweenness = edge_betweenness(graph))",
          "result")))),

    # Tolerance 1e-10 absolute: memberships and community counts are integers
    # and must be identical; modularity is one closed-form sum below 1.
    communities = list(
      title = "Community detection on the karate club",
      description = "Four deterministic algorithms (fast greedy, walktrap, leading eigenvector, edge betweenness) on Zachary's karate club: the community of every member, the number of communities and the modularity. communities() runs igraph's algorithms, so this case checks that cograph hands the network over and reads the answer back unchanged, not the algorithms themselves.",
      covers = c("communities()", "n_communities()", "membership()"), data = "karate", needs = "igraph",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(key = NULL, data = "karate", c(
          "describe <- function(method) {",
          "  found <- communities(network, method = method)",
          "  print(found)",
          "  data.frame(quantity = paste(method, c(\"communities\", \"modularity\", paste(\"community of\", found$node))),",
          "             kind = c(\"count\", \"modularity\", rep(\"membership\", nrow(found))),",
          "             value = c(n_communities(found), igraph::modularity(found), membership(found)))",
          "}",
          "result <- do.call(rbind, lapply(c(\"fast_greedy\", \"walktrap\", \"leading_eigenvector\",",
          "                                  \"edge_betweenness\"), describe))")),
        .igraph(key = NULL, data = "karate", c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"undirected\")",
          "describe <- function(method, found) {",
          "  print(found)",
          "  data.frame(quantity = paste(method, c(\"communities\", \"modularity\", paste(\"community of\", V(graph)$name))),",
          "             kind = c(\"count\", \"modularity\", rep(\"membership\", vcount(graph))),",
          "             value = c(length(found), modularity(found), membership(found)))",
          "}",
          "result <- rbind(describe(\"fast_greedy\", cluster_fast_greedy(graph)),",
          "                describe(\"walktrap\", cluster_walktrap(graph)),",
          "                describe(\"leading_eigenvector\", cluster_leading_eigen(graph)),",
          "                describe(\"edge_betweenness\", cluster_edge_betweenness(graph)))")))),

    # Tolerance 1e-10 absolute: every quantity is a count, a ratio of counts or
    # a shortest-path sum (all below 100); the one eigenvector quantity
    # (eigenvector centralization) is computed by igraph on both sides.
    network_summary = list(
      title = "Network-level metrics of a directed weighted network",
      description = "Size, density, components, diameter, mean distance, centralizations, transitivity, reciprocity, degree assortativity, girth, radius, connectivity, clique size and efficiency of the bundled regulation network. The igraph side makes each call as a user would, with igraph's defaults. network_summary() obtains most of these from igraph (transitivity is cograph's own kernel), so for those the case checks the arguments cograph chooses.",
      covers = "network_summary()", data = "regulation", needs = "igraph",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(key = "network", data = "regulation", c(
          "result <- data.frame(network = \"regulation\", network_summary(network, extended = TRUE, digits = NULL))",
          "result")),
        .igraph(key = "network", data = "regulation", c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"directed\", weighted = TRUE)",
          "result <- data.frame(",
          "  network = \"regulation\", node_count = vcount(graph), edge_count = ecount(graph),",
          "  density = edge_density(graph), component_count = count_components(graph),",
          "  diameter = diameter(graph), mean_distance = mean_distance(graph),",
          "  min_cut = min_cut(graph),",
          "  centralization_degree = centr_degree(graph)$centralization,",
          "  centralization_in_degree = centr_degree(graph, mode = \"in\")$centralization,",
          "  centralization_out_degree = centr_degree(graph, mode = \"out\")$centralization,",
          "  centralization_betweenness = centr_betw(graph)$centralization,",
          "  centralization_closeness = centr_clo(graph, mode = \"all\")$centralization,",
          "  centralization_eigen = centr_eigen(graph)$centralization,",
          "  transitivity = transitivity(graph), reciprocity = reciprocity(graph),",
          "  assortativity_degree = assortativity_degree(graph),",
          "  girth = girth(graph)$girth, radius = radius(graph),",
          "  vertex_connectivity = vertex_connectivity(graph),",
          "  largest_clique_size = clique_num(graph),",
          "  cut_vertex_count = length(articulation_points(graph)), bridge_count = length(bridges(graph)),",
          "  global_efficiency = global_efficiency(graph),",
          "  local_efficiency = average_local_efficiency(graph))",
          "result")))),

    # Tolerance 1e-10 absolute: censuses are counts and must be identical.
    census = list(
      title = "Triad and dyad census of a directed network",
      description = "The sixteen triad types (Holland and Leinhardt's MAN classes) and the mutual, asymmetric and null dyads of the bundled regulation network.",
      covers = c("triad_census()", "dyad_census()"), data = "regulation", needs = "igraph",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(key = "type", data = "regulation", c(
          "triads <- triad_census(network)",
          "dyads <- as.data.frame(dyad_census(network))",
          "triads",
          "dyads",
          "result <- rbind(triads, subset(dyads, select = c(type, count)))")),
        .igraph(key = "type", data = "regulation", c(
          "graph <- graph_from_adjacency_matrix(network, mode = \"directed\", weighted = TRUE)",
          "dyads <- dyad_census(graph)",
          "result <- data.frame(",
          "  type = c(\"003\", \"012\", \"102\", \"021D\", \"021U\", \"021C\", \"111D\", \"111U\", \"030T\", \"030C\",",
          "           \"201\", \"120D\", \"120U\", \"120C\", \"210\", \"300\", \"mutual\", \"asymmetric\", \"null\"),",
          "  count = c(triad_census(graph), dyads$mut, dyads$asym, dyads$null))",
          "result")))),

    plot_directed = list(
      title = "A directed weighted network drawn on a circle",
      description = "The bundled regulation network (ten states, thirty directed weighted edges) drawn by splot() and by qgraph(), both on a circular layout with the weights as edge labels. A case for the eye: nothing numeric is compared.",
      covers = "splot()", data = "regulation", needs = "qgraph",
      tolerance = 1e-10, measure = "absolute",
      sides = \() list(
        .cograph(key = NULL, data = "regulation", c(
          "png(\"plot.png\", width = 900, height = 900, res = 150)",
          "splot(network, layout = \"circle\", edge_labels = TRUE)",
          "dev.off()")),
        .qgraph(key = NULL, data = "regulation", c(
          "png(\"plot.png\", width = 900, height = 900, res = 150)",
          "qgraph(network, layout = \"circle\", edge.labels = TRUE)",
          "dev.off()")))),

    # --- measures cograph adds beyond igraph, each against the package whose
    # --- gap it closes. Skipped where that package is not installed. ----------
    # Tolerance 1e-8 relative for all three: the tolerance of cograph's own
    # equivalence tests against these packages (all.equal at 1e-8), over values
    # that range from below 1 to above 1000.
    centrality_centiserve = list(
      title = "Centralities of centiserve on the karate club",
      description = "Radiality, Lin, lobby index, maximum neighbourhood component, semi-local, leverage, Laplacian and Katz centrality of Zachary's karate club, against the centiserve package.",
      covers = "centrality()", data = "karate", needs = c("igraph", "centiserve"),
      tolerance = 1e-8, measure = "relative",
      sides = \() list(
        .cograph(data = "karate", c(
          "result <- centrality(network, measures = c(\"radiality\", \"lin\", \"lobby\", \"mnc\", \"semilocal\",",
          "                                           \"leverage\", \"laplacian\", \"katz\"), katz_alpha = 0.1)",
          "names(result) <- sub(\"_all$\", \"\", names(result))",
          "result")),
        .reference("centiserve", data = "karate", c(
          "graph <- igraph::graph_from_adjacency_matrix(network, mode = \"undirected\")",
          "result <- data.frame(node = igraph::V(graph)$name, radiality = radiality(graph),",
          "                     lin = lincent(graph), lobby = as.numeric(lobby(graph)),",
          "                     mnc = as.numeric(mnc(graph)), semilocal = semilocal(graph),",
          "                     leverage = leverage(graph), laplacian = laplacian(graph),",
          "                     katz = katzcent(graph, alpha = 0.1))",
          "result")))),
    centrality_sna = list(
      title = "Centralities of sna on the karate club",
      description = "Stress, load and information centrality of Zachary's karate club, against the sna package.",
      covers = "centrality()", data = "karate", needs = "sna",
      tolerance = 1e-8, measure = "relative",
      sides = \() list(
        .cograph(data = "karate", c(
          "result <- centrality(network, measures = c(\"stress\", \"load\", \"information\"))",
          "result")),
        .reference("sna", data = "karate", c(
          "result <- data.frame(node = rownames(network), stress = stresscent(network, gmode = \"graph\"),",
          "                     load = loadcent(network, gmode = \"graph\"),",
          "                     information = infocent(network, gmode = \"graph\"))",
          "result")))),
    centrality_influencer = list(
      title = "Effective network size on the karate club",
      description = "Burt's effective network size of every member of Zachary's karate club, against the influenceR package.",
      covers = "centrality()", data = "karate", needs = c("igraph", "influenceR"),
      tolerance = 1e-8, measure = "relative",
      sides = \() list(
        .cograph(data = "karate", c(
          "result <- centrality(network, measures = \"effective_size\")",
          "result")),
        .reference("influenceR", data = "karate", c(
          "graph <- igraph::graph_from_adjacency_matrix(network, mode = \"undirected\")",
          "result <- data.frame(node = igraph::V(graph)$name, effective_size = ens(graph))",
          "result"))))
  )
}

# ---- export ------------------------------------------------------------------

#' Export one case
#'
#' A case whose reference package is not installed is not exported: a classed
#' message (`cograph_verify_skipped`) names the package and the row says so.
#' @param name The case's name.
#' @return One row: the bundle, its sides, how its compared values came out, and
#'   `skipped` (the missing packages, or `""`).
export_case <- function(name) {
  cases <- .verify_cases()
  stopifnot("`name` must be one exported case" = length(name) == 1L && name %in% names(cases))
  case <- cases[[name]]
  bundle <- file.path(.verify_base, "bundles", name)
  absent <- case$needs[!vapply(case$needs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent) > 0L) {
    message(messageCondition(
      sprintf("Case `%s` skipped: %s not installed.\n", name, toString(absent)),
      class = "cograph_verify_skipped", call = NULL))
    return(data.frame(bundle = bundle, sides = NA_integer_, compared = NA_integer_,
                      agree = NA_integer_, disagree = NA_integer_, missing = NA_integer_,
                      skipped = toString(absent)))
  }
  data <- .verify_data[[case$data]]
  exported <- verify_export(bundle = bundle, title = case$title, description = case$description,
                            sides = case$sides(), data = stats::setNames(data$write(), data$label),
                            tolerance = case$tolerance, measure = case$measure, covers = case$covers,
                            variant = case$variant, generator = .verify_generator)
  cbind(exported, skipped = "")
}

requested <- commandArgs(trailingOnly = TRUE)
if (length(requested) == 0L) requested <- names(.verify_cases())
print(do.call(rbind, lapply(requested, export_case)), row.names = FALSE)
