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
