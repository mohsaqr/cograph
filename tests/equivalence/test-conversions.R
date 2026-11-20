# Conversions from other packages' objects: from_qgraph() and from_tna(). Each returns the arguments it hands
# to the plotting engine; the numbers among them are the network's weights (and for tna, the initial
# probabilities drawn as donuts). References: what qgraph stores in its object (the edge list $Edgelist, from
# which the weight matrix is rebuilt), and for tna the transition probabilities and initial proportions counted
# from the model's own sequences in base R (tna is the owner's package, so its stored matrix is shown as
# internal evidence only).

# qgraph keeps up to 6 decimals in the edge list it stores; one rebuild of the matrix: no arithmetic.
exact <- 1e-12

test_that("from_qgraph() returns the weights qgraph stores in its edge list", {
  skip_without_reference("qgraph")
  undirected <- ref_graph(7, density = 0.5, seed = 101L, weighted = TRUE, digits = 6L)
  directed <- ref_graph(7, density = 0.4, seed = 102L, directed = TRUE, weighted = TRUE, digits = 6L)
  rebuilt <- \(object, graph) ref_edgelist_matrix(object$Edgelist$from, object$Edgelist$to, object$Edgelist$weight,
                                                  rownames(graph), directed = all(object$Edgelist$directed))
  on_undirected <- qgraph::qgraph(undirected, DoNotPlot = TRUE)
  on_directed <- qgraph::qgraph(directed, DoNotPlot = TRUE)
  expect_reference(from_qgraph(on_undirected, plot = FALSE)$x, rebuilt(on_undirected, undirected),
                   reference = "qgraph", covers = "from_qgraph()", quantity = "weights, undirected", tolerance = exact)
  expect_reference(from_qgraph(on_directed, plot = FALSE)$x, rebuilt(on_directed, directed),
                   reference = "qgraph", covers = "from_qgraph()", quantity = "weights, directed", tolerance = exact)
})

test_that("from_tna() returns the transition probabilities and initial proportions of the sequences", {
  skip_without_reference("tna")
  sequences <- utils::head(tna::group_regulation, 300)
  model <- tna::tna(sequences)
  counted <- ref_transitions(sequences)
  converted <- from_tna(model, plot = FALSE)
  expect_reference(converted$x, counted$weights, reference = "independent: transitions counted from the sequences, base R",
                   covers = "from_tna()", quantity = "transition probabilities, 300 sequences of group_regulation",
                   tolerance = exact)
  expect_reference(stats::setNames(converted$donut_fill, rownames(converted$x)), counted$inits,
                   reference = "independent: transitions counted from the sequences, base R", covers = "from_tna()",
                   quantity = "initial proportions, 300 sequences of group_regulation", tolerance = exact)
  expect_reference(converted$x, model$weights, reference = "tna", covers = "from_tna()",
                   quantity = "weights, as stored by tna::tna()", tolerance = 0)
})
