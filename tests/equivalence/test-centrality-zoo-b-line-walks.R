# centrality() and its wrappers for two measures built on walks over edges: LineRank (Kang, Papadimitriou, Sun
# and Tong 2011, SDM, Definitions 2-6 and Algorithm 2; undirected construction as in Kosa et al. 2015, section 4)
# and X-degree (Torres, Chan, Tong and Eliassi-Rad 2021, SIMODS 3(2), Proposition 3.8, equation 3.15).
#
# LineRank is PageRank on the line graph, summed back onto the endpoints of each edge. NetworkX builds the line
# graph (nx.line_graph, or for weighted arcs the line digraph whose arc e -> f carries w(f)) and runs its own
# pagerank(); the endpoint sums are written in the expression. cograph solves the PageRank system directly, so
# the only error is NetworkX's power iteration, which stops once the L1 change falls below n * 1e-13.
#
# X-degree has no package implementation outside the author's repository; equation 3.15 (the squared sum of the
# neighbours' excess degrees minus the sum of their squares) is written out below in base R.

# NetworkX's pagerank() stops when the L1 change is below n * tol with tol = 1e-13; with n at most 78 line nodes
# the stationary vector is then within about 1e-11 of the limit. 1e-9 leaves room for the endpoint sums.
iterative <- 1e-9
exact <- 1e-10

# The line graph and the endpoint sums, as Python expressions over G.
line_unweighted <- "nx.line_graph(G)"
line_weighted <- paste0(
  "(lambda L: L.add_nodes_from(G.edges()) or L.add_weighted_edges_from(",
  "[((u, v), (v, w), d['weight']) for u, v in G.edges() for w, d in G[v].items()]) or L)(nx.DiGraph())")
linerank_nx <- function(line, damping, aggregate = c("probability", "weight")) {
  aggregate <- match.arg(aggregate)
  share <- if (aggregate == "probability") "p" else "p * G.edges[e]['weight']"
  sprintf(paste0("(lambda pr: {n: sum(%s for e, p in pr.items() if n in e) for n in G})",
                 "(nx.pagerank(%s, alpha=%s, weight='weight', tol=1e-13, max_iter=100000))"),
          share, line, format(damping))
}

path4 <- zoob_path(4)
directed_path4 <- zoob_graph(4, c(1, 2, 2, 3, 3, 4), directed = TRUE)
# A branching digraph with weighted arcs: A -> B (2), B -> C (1), B -> D (3).
branch <- zoob_graph(4, c(1, 2, 2, 3, 2, 4), directed = TRUE, labels = c("A", "B", "C", "D"))
branch["A", "B"] <- 2
branch["B", "D"] <- 3
karate <- zoob_karate()

test_that("linerank equals PageRank on NetworkX's line graph, summed over each edge's endpoints", {
  on_path <- ref_networkx(path4, c(half = linerank_nx(line_unweighted, 0.5), none = linerank_nx(line_unweighted, 0)))
  on_directed <- ref_networkx(directed_path4, c(half = linerank_nx(line_unweighted, 0.5)))
  on_reversed <- ref_networkx(t(directed_path4), c(half = linerank_nx(line_unweighted, 0.5)))
  with_isolate <- zoob_union(path4, matrix(0, 1, 1))
  on_isolate <- ref_networkx(with_isolate, c(half = linerank_nx(line_unweighted, 0.5)))
  on_ring <- ref_networkx(zoob_ring(5), c(default = linerank_nx(line_unweighted, 0.85)))
  on_clique <- ref_networkx(zoob_complete(5), c(default = linerank_nx(line_unweighted, 0.85)))
  on_karate <- ref_networkx(karate, c(default = linerank_nx(line_unweighted, 0.85)))
  expect_reference(centrality_linerank(path4, 0.5), on_path$half, reference = "networkx", covers = c("centrality()", "centrality_linerank()"),
                   quantity = "linerank, path of 4, damping 0.5", tolerance = iterative)
  expect_reference(centrality_linerank(path4, 0), on_path$none, reference = "networkx", covers = c("centrality()", "centrality_linerank()"),
                   quantity = "linerank, path of 4, damping 0", tolerance = iterative)
  expect_reference(centrality_linerank(directed_path4, 0.5), on_directed$half, reference = "networkx", covers = c("centrality()", "centrality_linerank()"),
                   quantity = "linerank, directed path of 4, damping 0.5", tolerance = iterative)
  expect_reference(centrality_linerank(t(directed_path4), 0.5), on_reversed$half, reference = "networkx",
                   covers = c("centrality()", "centrality_linerank()"), quantity = "linerank, reversed directed path of 4, damping 0.5",
                   tolerance = iterative)
  expect_reference(centrality_linerank(with_isolate, 0.5), on_isolate$half, reference = "networkx", covers = c("centrality()", "centrality_linerank()"),
                   quantity = "linerank, path of 4 and an isolate, damping 0.5", tolerance = iterative)
  expect_reference(centrality_linerank(zoob_ring(5)), on_ring$default, reference = "networkx", covers = c("centrality()", "centrality_linerank()"),
                   quantity = "linerank, ring of 5", tolerance = iterative)
  expect_reference(centrality_linerank(zoob_complete(5)), on_clique$default, reference = "networkx", covers = c("centrality()", "centrality_linerank()"),
                   quantity = "linerank, complete graph on 5", tolerance = iterative)
  expect_reference(centrality(karate, measures = "linerank"), ref_nodes(linerank = on_karate$default),
                   reference = "networkx", covers = "centrality()", quantity = "linerank, karate club",
                   tolerance = iterative, key = "node")
})

test_that("weighted linerank equals PageRank on NetworkX's weighted line digraph", {
  # Kang et al. weight the line arc e -> f by w(e) w(f); normalising the row of e cancels w(e), so the line
  # digraph below carries w(f). The "weight" aggregation multiplies each edge's probability by its weight.
  nx <- ref_networkx(branch, c(probability = linerank_nx(line_weighted, 0.5),
                               weight = linerank_nx(line_weighted, 0.5, "weight"),
                               unweighted = linerank_nx(line_unweighted, 0.5)))
  expect_reference(centrality_linerank(branch, 0.5), nx$probability, reference = "networkx", covers = c("centrality()", "centrality_linerank()"),
                   quantity = "linerank, weighted branching digraph, damping 0.5", tolerance = iterative)
  expect_reference(centrality_linerank(branch, 0.5, "weight"), nx$weight, reference = "networkx", covers = c("centrality()", "centrality_linerank()"),
                   quantity = "linerank, weighted branching digraph, weight aggregation", tolerance = iterative)
  expect_reference(centrality_linerank(branch, 0.5, weighted = FALSE), nx$unweighted, reference = "networkx",
                   covers = c("centrality()", "centrality_linerank()"), quantity = "linerank, branching digraph read unweighted", tolerance = iterative)
})

# Torres et al. (2021), equation 3.15: with e_j = d_j - 1 the excess degree of a neighbour j of i, on the simple
# undirected skeleton, X(i) = (sum_j e_j)^2 - sum_j e_j^2.
x_degree_definition <- function(graph) {
  skeleton <- (graph + t(graph) > 0) * 1
  diag(skeleton) <- 0
  excess <- rowSums(skeleton) - 1
  as.numeric(skeleton %*% excess)^2 - as.numeric(skeleton %*% excess^2)
}

test_that("x_degree equals equation 3.15 of Torres et al. written out", {
  graphs <- list(path5 = zoob_path(5), triangle_leaf = zoob_graph(4, c(1, 2, 2, 3, 3, 1, 1, 4)),
                 ring6 = zoob_ring(6), star5 = zoob_star(5), complete6 = zoob_complete(6), karate = karate)
  lapply(names(graphs), \(name) {
    expect_reference(centrality_x_degree(graphs[[name]]), x_degree_definition(graphs[[name]]),
                     reference = "independent: closed forms and hand derivations from the definition",
                     covers = c("centrality()", "centrality_x_degree()"), quantity = paste0("x_degree, ", name),
                     tolerance = exact)
  })
})

# The authors' own x_degree() from the code the paper links (github.com/leotrs/inbox, inbox.py, pinned commit
# 401bf028b56886ce67b15b2c5eacd7a28dbb4a48, MIT licence), copied verbatim, docstring included, and run by Python
# with NetworkX and NumPy on the same graphs.
authors_x_degree <- r"---(def x_degree(graph):
    """Return the X-degree centrality of each node.

    Params
    ------

    graph (nx.Graph): the graph.

    Returns
    -------

    centralities (dict): dictionary of {node: centrality} items.

    """
    degrees = graph.degree()
    agg = lambda arr: arr.sum()**2 - (arr**2).sum()
    return {node: agg(np.array([degrees[n] - 1 for n in graph.neighbors(node)]))
            for node in graph}
)---"

test_that("x_degree equals the authors' x_degree() of Torres et al. (2021)", {
  call <- sprintf("(lambda scope: exec(%s, scope) or scope['x_degree'](G))({'np': np})",
                  jsonlite::toJSON(authors_x_degree, auto_unbox = TRUE))
  graphs <- list(path5 = zoob_path(5), triangle_leaf = zoob_graph(4, c(1, 2, 2, 3, 3, 1, 1, 4)),
                 ring6 = zoob_ring(6), star5 = zoob_star(5), complete6 = zoob_complete(6), karate = karate)
  lapply(names(graphs), \(name) {
    authors <- ref_networkx(graphs[[name]], c(x_degree = call))
    expect_reference(centrality_x_degree(graphs[[name]]), authors$x_degree,
                     reference = "published: Torres et al. 2021, the authors' x_degree() (leotrs/inbox@401bf02), run in Python",
                     covers = c("centrality()", "centrality_x_degree()"), quantity = paste0("x_degree, ", name),
                     tolerance = exact)
  })
})
