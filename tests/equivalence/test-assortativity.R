# assortativity() (degree correlation, Newman 2002, 2003) and assortativity_attribute() / homophily() (nominal and
# scalar attribute assortativity, Newman 2003), compared with NetworkX and igraph. cograph computes the
# coefficients itself from the edge list (R/assortativity.R reads igraph::as_edgelist() and igraph::degree() only),
# so igraph's assortativity functions are a counted reference.
#
# Graphs: random graphs from helper-reference.R; weights are not read by any of these coefficients.

undirected <- ref_graph(14, density = 0.3, seed = 81L)
directed <- ref_graph(14, density = 0.25, seed = 82L, directed = TRUE)
categories <- rep(c("a", "b", "c"), length.out = 14)
scores <- round(seq(0.5, 7, length.out = 14)^1.3, 3)

exact <- 1e-10

# NetworkX node attributes are set in the expression, then the coefficient is read.
py_with_attribute <- \(values, call) {
  literal <- paste(sprintf("'N%d': %s", seq_along(values),
                           if (is.character(values)) sprintf("'%s'", values) else format(values, digits = 15)),
                   collapse = ", ")
  sprintf("(nx.set_node_attributes(G, {%s}, 'a'), %s)[1]", literal, call)
}

test_that("assortativity() equals NetworkX's degree_assortativity_coefficient()", {
  on_undirected <- ref_networkx(undirected, c(r = "nx.degree_assortativity_coefficient(G)"))
  # Directed: the out-degree of the source against the in-degree of the target ("out-in"), and the other three.
  on_directed <- ref_networkx(directed, c(
    out_in = "nx.degree_assortativity_coefficient(G, x='out', y='in')",
    in_in = "nx.degree_assortativity_coefficient(G, x='in', y='in')",
    out_out = "nx.degree_assortativity_coefficient(G, x='out', y='out')",
    in_out = "nx.degree_assortativity_coefficient(G, x='in', y='out')"))
  expect_reference(assortativity(undirected)$coefficient, on_undirected$r, reference = "networkx",
                   covers = "assortativity()", quantity = "degree assortativity, undirected", tolerance = exact)
  expect_reference(c(out_in = assortativity(directed)$coefficient,
                     in_in = assortativity(directed, type = "in-in")$coefficient,
                     out_out = assortativity(directed, type = "out-out")$coefficient,
                     in_out = assortativity(directed, type = "in-out")$coefficient),
                   unlist(on_directed[c("out_in", "in_in", "out_out", "in_out")]), reference = "networkx",
                   covers = "assortativity()", quantity = "degree assortativity, directed", tolerance = exact)
})

test_that("assortativity() equals igraph's assortativity_degree()", {
  skip_without_reference("igraph")
  expect_reference(c(undirected = assortativity(undirected)$coefficient,
                     directed = assortativity(directed, directed = TRUE)$coefficient,
                     regulation_net = assortativity(regulation_net)$coefficient),
                   c(undirected = igraph::assortativity_degree(ref_igraph(undirected), directed = FALSE),
                     directed = igraph::assortativity_degree(ref_igraph(directed), directed = TRUE),
                     regulation_net = igraph::assortativity_degree(ref_igraph(regulation_net), directed = TRUE)),
                   reference = "igraph", covers = "assortativity()", quantity = "degree assortativity",
                   tolerance = exact)
})

test_that("assortativity_attribute() and homophily() equal NetworkX's attribute assortativity", {
  nominal_u <- ref_networkx(undirected, c(r = py_with_attribute(categories, "nx.attribute_assortativity_coefficient(G, 'a')")))
  scalar_u <- ref_networkx(undirected, c(r = py_with_attribute(scores, "nx.numeric_assortativity_coefficient(G, 'a')")))
  nominal_d <- ref_networkx(directed, c(r = py_with_attribute(categories, "nx.attribute_assortativity_coefficient(G, 'a')")))
  scalar_d <- ref_networkx(directed, c(r = py_with_attribute(scores, "nx.numeric_assortativity_coefficient(G, 'a')")))
  expect_reference(c(nominal = assortativity_attribute(undirected, values = categories)$coefficient,
                     scalar = assortativity_attribute(undirected, values = scores)$coefficient),
                   c(nominal = nominal_u$r, scalar = scalar_u$r), reference = "networkx",
                   covers = "assortativity_attribute()", quantity = "attribute assortativity, undirected",
                   tolerance = exact)
  expect_reference(c(nominal = homophily(directed, values = categories)$coefficient,
                     scalar = homophily(directed, values = scores)$coefficient),
                   c(nominal = nominal_d$r, scalar = scalar_d$r), reference = "networkx",
                   covers = c("assortativity_attribute()", "homophily()"),
                   quantity = "attribute assortativity, directed", tolerance = exact)
})

test_that("assortativity_attribute() equals igraph's assortativity_nominal() and assortativity()", {
  skip_without_reference("igraph")
  graph <- ref_igraph(undirected)
  expect_reference(c(nominal = assortativity_attribute(undirected, values = categories)$coefficient,
                     scalar = assortativity_attribute(undirected, values = scores)$coefficient),
                   c(nominal = igraph::assortativity_nominal(graph, types = as.integer(factor(categories))),
                     scalar = igraph::assortativity(graph, values = scores)),
                   reference = "igraph", covers = "assortativity_attribute()",
                   quantity = "attribute assortativity, undirected", tolerance = exact)
})

# homophily() on more graphs and attributes. Graphs: undirected unweighted (above), undirected weighted with
# isolated nodes, two components, directed (above), directed weighted, and regulation_net (directed, weighted,
# 10 nodes). Attributes: 2, 3 and 4 categories (character and factor), a vector named in another order than the
# nodes, and numeric scores (doubles with negatives, whole numbers). No coefficient here reads weights: cograph,
# NetworkX's attribute_assortativity_coefficient() / numeric_assortativity_coefficient() and igraph 2.x's
# assortativity_nominal() / assortativity() take none, so a weighted graph is compared as its ties.

py_with_values <- \(values, call) {
  nodes <- names(values)
  literal <- paste(sprintf("'%s': %s", nodes,
                           if (is.numeric(values)) format(unname(values), digits = 15)
                           else sprintf("'%s'", as.character(values))),
                   collapse = ", ")
  sprintf("(nx.set_node_attributes(G, {%s}, 'a'), %s)[1]", literal, call)
}
two_components <- function(first, second) {
  n <- nrow(first) + nrow(second)
  graph <- matrix(0, n, n)
  graph[seq_len(nrow(first)), seq_len(nrow(first))] <- first
  graph[nrow(first) + seq_len(nrow(second)), nrow(first) + seq_len(nrow(second))] <- second
  dimnames(graph) <- list(paste0("N", seq_len(n)), paste0("N", seq_len(n)))
  graph
}
homophily_graphs <- list(
  undirected_14 = undirected,
  weighted_25 = ref_graph(25, density = 0.12, seed = 91L, weighted = TRUE),
  two_components = two_components(ref_connected_graph(9, 0.35, 93L), ref_connected_graph(8, 0.4, 94L)),
  directed_14 = directed,
  directed_weighted_20 = ref_graph(20, density = 0.2, seed = 92L, directed = TRUE, weighted = TRUE),
  regulation_net = regulation_net)
# Attribute vectors for a graph, named by node.
homophily_values <- \(graph) {
  n <- nrow(graph)
  nodes <- rownames(graph)
  list(two_categories = stats::setNames(rep(c("p", "q"), length.out = n), nodes),
       three_factor = stats::setNames(factor(rep(c("a", "b", "c"), times = c(4, 3, n - 7))), nodes),
       four_categories = stats::setNames(rep(c("w", "x", "y", "z"), length.out = n), nodes),
       scores = stats::setNames(round(sin(seq_len(n)) * 3 + cos(seq_len(n) / 2), 6), nodes),
       whole_numbers = stats::setNames((seq_len(n) * 7L) %% 5L, nodes))
}
homophily_cases <- unlist(lapply(names(homophily_graphs), \(graph_name) {
  values <- homophily_values(homophily_graphs[[graph_name]])
  stats::setNames(lapply(names(values), \(value_name) list(graph = homophily_graphs[[graph_name]],
                                                         values = values[[value_name]])),
                  paste0(graph_name, ": ", names(values)))
}), recursive = FALSE)
cograph_homophily <- vapply(homophily_cases, \(case) homophily(case$graph, values = case$values)$coefficient,
                            numeric(1))

test_that("homophily() equals NetworkX's attribute and numeric assortativity on six graphs and five attributes", {
  nx <- vapply(homophily_cases, \(case) ref_networkx(case$graph, c(r = py_with_values(case$values,
    if (is.numeric(case$values)) "nx.numeric_assortativity_coefficient(G, 'a')"
    else "nx.attribute_assortativity_coefficient(G, 'a')")))$r, numeric(1))
  expect_reference(cograph_homophily, nx, reference = "networkx", covers = "homophily()",
                   quantity = "attribute assortativity", tolerance = exact)
  # Values named in reverse node order are aligned by name.
  reversed <- homophily_values(directed)$four_categories
  reversed <- rev(reversed)
  expect_reference(homophily(directed, values = reversed)$coefficient,
                   ref_networkx(directed, c(r = py_with_values(reversed,
                     "nx.attribute_assortativity_coefficient(G, 'a')")))$r,
                   reference = "networkx", covers = "homophily()",
                   quantity = "attribute assortativity, values named in reverse order, directed", tolerance = exact)
})

test_that("homophily() equals igraph's assortativity_nominal() and assortativity() on six graphs and five attributes", {
  skip_without_reference("igraph")
  ig <- vapply(homophily_cases, \(case) {
    graph <- ref_igraph(case$graph)
    values <- case$values[rownames(case$graph)]
    if (is.numeric(values)) igraph::assortativity(graph, values = as.numeric(values), directed = TRUE)
    else igraph::assortativity_nominal(graph, types = as.integer(factor(values)), directed = TRUE)
  }, numeric(1))
  expect_reference(cograph_homophily, ig, reference = "igraph", covers = "homophily()",
                   quantity = "attribute assortativity", tolerance = exact)
})

test_that("homophily() with self-loops counts a loop's two ends as igraph does; NetworkX counts one", {
  skip_without_reference("igraph")
  looped <- homophily_graphs$weighted_25
  diag(looped)[c(1, 3, 8)] <- c(1, 0.5, 0.7)
  values <- homophily_values(looped)
  found <- c(nominal = homophily(looped, values = values$four_categories)$coefficient,
             scalar = homophily(looped, values = values$scores)$coefficient)
  graph <- ref_igraph(looped)
  expect_reference(found,
                   c(nominal = igraph::assortativity_nominal(graph, types = as.integer(factor(values$four_categories))),
                     scalar = igraph::assortativity(graph, values = values$scores)),
                   reference = "igraph", covers = "homophily()",
                   quantity = "attribute assortativity, undirected with three self-loops", tolerance = exact)
  nx <- c(nominal = ref_networkx(looped, c(r = py_with_values(values$four_categories,
            "nx.attribute_assortativity_coefficient(G, 'a')")))$r,
          scalar = ref_networkx(looped, c(r = py_with_values(values$scores,
            "nx.numeric_assortativity_coefficient(G, 'a')")))$r)
  expect_known_difference(found, nx, reference = "networkx", covers = "homophily()",
                          quantity = "attribute assortativity, undirected with three self-loops", tolerance = exact,
                          reason = paste("NetworkX's mixing matrix reads an undirected graph from each node's",
                                         "adjacency, so an edge is counted from both ends and a self-loop once;",
                                         "cograph and igraph count both ends of a loop (Newman 2003 counts edge",
                                         "ends), giving a loop twice NetworkX's weight"),
                          agrees_with = "igraph::assortativity_nominal() / assortativity()")
})
