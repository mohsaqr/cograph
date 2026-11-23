# centrality() and its wrappers for the spectral and linear-solve measures: eigenvector, hub, authority, katz,
# alpha, power, hubbell, subgraph, communicability, communicability betweenness, laplacian and non-backtracking.
# cograph computes all of them natively (base R eigen(), solve(), a Pade matrix exponential), not through igraph,
# so igraph counts as a separate implementation here. NetworkX is a second, in Python.
#
# Conventions, read from the sources before the run:
# - Spectral measures read a weight as a strength (an entry of the adjacency matrix), as igraph and NetworkX do.
#   subgraph, communicability and communicability betweenness use the 0/1 adjacency only, as do NetworkX's
#   subgraph_centrality(), communicability() and communicability_betweenness_centrality(). laplacian reads the
#   weights of an undirected weighted network (Qi et al. 2012; cograph 2.7.20).
# - cograph scales eigenvector, hub and authority scores so that the largest is 1 (igraph's scale). NetworkX
#   scales the eigenvector to unit Euclidean norm and HITS to unit sum: the NetworkX vector is rescaled to maximum
#   1 in the test (ref_unit_max()), which changes no ratio between nodes.
# - On a directed graph eigenvector and katz/alpha credit a node for its in-ties (A' x), as igraph and NetworkX do.
#   alpha's `mode` default "all" symmetrises a directed graph; mode = "in" is the directed measure igraph computes.
#   power's directed measure is mode = "out" (Bonacich 1987 sums out-ties), as in igraph and sna.

undirected <- ref_connected_graph(12, density = 0.25, seed = 31L)
undirected_weighted <- ref_connected_graph(12, density = 0.25, seed = 32L, weighted = TRUE, digits = 6L)
directed <- ref_connected_graph(12, density = 0.2, seed = 33L, directed = TRUE)
directed_weighted <- ref_connected_graph(12, density = 0.2, seed = 34L, directed = TRUE, weighted = TRUE, digits = 6L)
# The 5-node graph of the earlier tests: alpha and power solve (I - A) x = b with attenuation 1, which this
# undirected graph leaves non-singular.
small <- matrix(c(0, 1, 1, 0, 0,
                  1, 0, 1, 1, 0,
                  1, 1, 0, 1, 1,
                  0, 1, 1, 0, 1,
                  0, 0, 1, 1, 0), 5, 5, byrow = TRUE, dimnames = list(LETTERS[1:5], LETTERS[1:5]))

# Direct solves and symmetric eigen-decompositions: agreement to rounding is expected.
exact <- 1e-10
# A vector from ARPACK (igraph's eigen_centrality() and hits_scores(), NetworkX's eigenvector_centrality_numpy()
# and hits(), asked for full precision) converges to about 1e-12 relative on graphs this small; 1e-9 leaves room for the
# stopping rule of the Arnoldi iteration and nothing more.
arnoldi <- 1e-9

test_that("eigenvector equals NetworkX's eigenvector_centrality_numpy(), rescaled to maximum 1", {
  on_directed <- ref_networkx(directed_weighted, c(eigenvector = "nx.eigenvector_centrality_numpy(G, weight='weight')"))
  on_undirected <- ref_networkx(undirected_weighted, c(eigenvector = "nx.eigenvector_centrality_numpy(G, weight='weight')"))
  expect_reference(centrality_eigenvector(directed_weighted), ref_unit_max(on_directed$eigenvector),
                   reference = "networkx", covers = c("centrality()", "centrality_eigenvector()"),
                   quantity = "eigenvector, directed weighted", tolerance = arnoldi)
  expect_reference(centrality_eigenvector(undirected_weighted), ref_unit_max(on_undirected$eigenvector),
                   reference = "networkx", covers = c("centrality()", "centrality_eigenvector()"),
                   quantity = "eigenvector, undirected weighted", tolerance = arnoldi)
})

test_that("eigenvector equals igraph's eigen_centrality()", {
  skip_without_reference("igraph")
  expect_reference(centrality_eigenvector(directed_weighted),
                   igraph::eigen_centrality(ref_igraph(directed_weighted), directed = TRUE)$vector,
                   reference = "igraph", covers = c("centrality()", "centrality_eigenvector()"),
                   quantity = "eigenvector, directed weighted", tolerance = arnoldi)
  expect_reference(centrality_eigenvector(undirected), igraph::eigen_centrality(ref_igraph(undirected))$vector,
                   reference = "igraph", covers = c("centrality()", "centrality_eigenvector()"),
                   quantity = "eigenvector, undirected", tolerance = arnoldi)
})

test_that("hub and authority equal NetworkX's hits(), rescaled to maximum 1", {
  # NetworkX 3.7's hits() is a power iteration that stops when the summed change of one sweep is below `tol`
  # (default 1e-8, which left differences of 1.5e-9 in the first run); tol=1e-14 asks it for full precision.
  call <- "nx.hits(G, max_iter=100000, tol=1e-14)"
  nx <- ref_networkx(directed_weighted, c(hub = paste0(call, "[0]"), authority = paste0(call, "[1]")))
  expect_reference(centrality_hub(directed_weighted), ref_unit_max(nx$hub), reference = "networkx",
                   covers = c("centrality()", "centrality_hub()"), quantity = "hub, directed weighted",
                   tolerance = arnoldi)
  expect_reference(centrality_authority(directed_weighted), ref_unit_max(nx$authority), reference = "networkx",
                   covers = c("centrality()", "centrality_authority()"), quantity = "authority, directed weighted",
                   tolerance = arnoldi)
})

test_that("hub and authority equal igraph's hits_scores()", {
  skip_without_reference("igraph")
  hits <- igraph::hits_scores(ref_igraph(directed_weighted))
  expect_reference(centrality_hub(directed_weighted), hits$hub, reference = "igraph",
                   covers = c("centrality()", "centrality_hub()"), quantity = "hub, directed weighted",
                   tolerance = arnoldi)
  expect_reference(centrality_authority(directed_weighted), hits$authority, reference = "igraph",
                   covers = c("centrality()", "centrality_authority()"), quantity = "authority, directed weighted",
                   tolerance = arnoldi)
})

test_that("katz equals NetworkX's katz_centrality_numpy(normalized = False)", {
  # (I - alpha A')^-1 1 with beta = 1, Katz (1953); NetworkX solves the same system with numpy.
  on_directed <- ref_networkx(directed_weighted, c(
    katz = "nx.katz_centrality_numpy(G, alpha=0.1, beta=1.0, normalized=False, weight='weight')"))
  on_undirected <- ref_networkx(undirected, c(
    katz = "nx.katz_centrality_numpy(G, alpha=0.1, beta=1.0, normalized=False)"))
  expect_reference(centrality_katz(directed_weighted, katz_alpha = 0.1), on_directed$katz, reference = "networkx",
                   covers = c("centrality()", "centrality_katz()"), quantity = "katz, alpha 0.1, directed weighted",
                   tolerance = exact)
  expect_reference(centrality_katz(undirected, katz_alpha = 0.1), on_undirected$katz, reference = "networkx",
                   covers = c("centrality()", "centrality_katz()"), quantity = "katz, alpha 0.1, undirected",
                   tolerance = exact)
})

test_that("katz equals centiserve's katzcent() and igraph's alpha_centrality(alpha = 0.1)", {
  skip_without_reference(c("centiserve", "igraph"))
  expect_reference(centrality_katz(undirected, katz_alpha = 0.1),
                   ref_named(centiserve::katzcent(ref_igraph(undirected), alpha = 0.1), undirected),
                   reference = "centiserve", covers = c("centrality()", "centrality_katz()"),
                   quantity = "katz, alpha 0.1, undirected", tolerance = exact)
  expect_reference(centrality_katz(directed_weighted, katz_alpha = 0.1),
                   igraph::alpha_centrality(ref_igraph(directed_weighted), alpha = 0.1, exo = 1),
                   reference = "igraph", covers = c("centrality()", "centrality_katz()"),
                   quantity = "katz, alpha 0.1, directed weighted", tolerance = exact)
})

test_that("alpha equals igraph's alpha_centrality(alpha = 1, exo = 1)", {
  skip_without_reference("igraph")
  expect_reference(centrality_alpha(directed, mode = "in"), igraph::alpha_centrality(ref_igraph(directed), exo = 1),
                   reference = "igraph", covers = c("centrality()", "centrality_alpha()"),
                   quantity = "alpha, mode in, directed", tolerance = exact)
  expect_reference(centrality_alpha(small), igraph::alpha_centrality(ref_igraph(small), exo = 1),
                   reference = "igraph", covers = c("centrality()", "centrality_alpha()"),
                   quantity = "alpha, undirected", tolerance = exact)
})

test_that("alpha equals NetworkX's katz_centrality_numpy(alpha = 1, beta = 1)", {
  # Alpha centrality (Bonacich and Lloyd 2001) with exogenous status 1 is Katz's system at attenuation 1.
  on_directed <- ref_networkx(directed, c(alpha = "nx.katz_centrality_numpy(G, alpha=1.0, beta=1.0, normalized=False)"))
  on_small <- ref_networkx(small, c(alpha = "nx.katz_centrality_numpy(G, alpha=1.0, beta=1.0, normalized=False)"))
  expect_reference(centrality_alpha(directed, mode = "in"), on_directed$alpha, reference = "networkx",
                   covers = c("centrality()", "centrality_alpha()"), quantity = "alpha, mode in, directed",
                   tolerance = exact, measure = "scaled")
  expect_reference(centrality_alpha(small), on_small$alpha, reference = "networkx",
                   covers = c("centrality()", "centrality_alpha()"), quantity = "alpha, undirected",
                   tolerance = exact, measure = "scaled")
})

test_that("power equals igraph's power_centrality(exponent = 1) and sna's bonpow()", {
  skip_without_reference(c("igraph", "sna"))
  expect_reference(centrality_power(directed, mode = "out"), igraph::power_centrality(ref_igraph(directed), exponent = 1),
                   reference = "igraph", covers = c("centrality()", "centrality_power()"),
                   quantity = "power, mode out, directed", tolerance = exact)
  expect_reference(centrality_power(small), igraph::power_centrality(ref_igraph(small), exponent = 1),
                   reference = "igraph", covers = c("centrality()", "centrality_power()"),
                   quantity = "power, undirected", tolerance = exact)
  expect_reference(centrality_power(directed, mode = "out"),
                   ref_named(sna::bonpow(directed, gmode = "digraph", exponent = 1), directed),
                   reference = "sna", covers = c("centrality()", "centrality_power()"),
                   quantity = "power, mode out, directed", tolerance = exact)
  expect_reference(centrality_power(small), ref_named(sna::bonpow(small, gmode = "graph", exponent = 1), small),
                   reference = "sna", covers = c("centrality()", "centrality_power()"),
                   quantity = "power, undirected", tolerance = exact)
})

test_that("hubbell equals centiserve's hubbell() and the definition (I - wW)^-1 1", {
  skip_without_reference(c("centiserve", "igraph"))
  graph <- ref_igraph(undirected_weighted)
  # centiserve::hubbell() reads weights only when they are passed.
  expect_reference(centrality_hubbell(undirected_weighted, hubbell_weight = 0.2),
                   ref_named(centiserve::hubbell(graph, weightfactor = 0.2, weights = igraph::E(graph)$weight),
                             undirected_weighted),
                   reference = "centiserve", covers = c("centrality()", "centrality_hubbell()"),
                   quantity = "hubbell, weight factor 0.2, undirected weighted", tolerance = exact)
})

test_that("hubbell equals Hubbell's (1965) system solved in base R", {
  # Hubbell (1965): s = 1 + w W s, so s = (I - w W)^-1 1, the row (out-tie) direction of W.
  expected <- ref_named(solve(diag(1, nrow(directed_weighted)) - 0.2 * directed_weighted, rep(1, nrow(directed_weighted))),
                        directed_weighted)
  expect_reference(centrality_hubbell(directed_weighted, hubbell_weight = 0.2), expected,
                   reference = "independent: Hubbell (1965) input-output system solved with base R solve()",
                   covers = c("centrality()", "centrality_hubbell()"),
                   quantity = "hubbell, weight factor 0.2, directed weighted", tolerance = exact)
})

test_that("subgraph equals NetworkX's subgraph_centrality() and igraph's subgraph_centrality()", {
  nx <- ref_networkx(undirected, c(subgraph = "nx.subgraph_centrality(G)"))
  on_weighted <- ref_networkx(undirected_weighted, c(subgraph = "nx.subgraph_centrality(G)"))
  # Both read the 0/1 adjacency: the weighted graph checks that weights are ignored on both sides.
  expect_reference(centrality_subgraph(undirected), nx$subgraph, reference = "networkx",
                   covers = c("centrality()", "centrality_subgraph()"), quantity = "subgraph, undirected",
                   tolerance = exact, measure = "relative")
  expect_reference(centrality_subgraph(undirected_weighted), on_weighted$subgraph, reference = "networkx",
                   covers = c("centrality()", "centrality_subgraph()"), quantity = "subgraph, undirected weighted",
                   tolerance = exact, measure = "relative")
  skip_without_reference("igraph")
  expect_reference(centrality_subgraph(undirected), igraph::subgraph_centrality(ref_igraph(undirected)),
                   reference = "igraph", covers = c("centrality()", "centrality_subgraph()"),
                   quantity = "subgraph, undirected", tolerance = exact, measure = "relative")
})

test_that("communicability equals NetworkX's communicability() row sums and Matrix::expm()", {
  # Estrada and Hatano (2008): G = exp(A); cograph reports each node's row sum of G.
  nx <- ref_networkx(undirected, c(communicability = "{u: sum(row.values()) for u, row in nx.communicability(G).items()}"))
  expect_reference(centrality_communicability(undirected), nx$communicability, reference = "networkx",
                   covers = c("centrality()", "centrality_communicability()"), quantity = "communicability, undirected",
                   tolerance = exact, measure = "relative")
  skip_without_reference("Matrix")
  # NetworkX's communicability() is for undirected graphs only; Matrix::expm() (scaling and squaring, Higham 2005)
  # takes the asymmetric adjacency of a directed graph.
  expect_reference(centrality_communicability(directed),
                   ref_named(rowSums(as.matrix(Matrix::expm(Matrix::Matrix(directed)))), directed),
                   reference = "Matrix", covers = c("centrality()", "centrality_communicability()"),
                   quantity = "communicability, directed", tolerance = exact, measure = "relative")
})

test_that("communicability betweenness equals NetworkX's communicability_betweenness_centrality()", {
  # Estrada, Higham and Hatano (2009), normalised by (n - 1)(n - 2) on both sides.
  nx <- ref_networkx(undirected, c(cbc = "nx.communicability_betweenness_centrality(G)"))
  on_weighted <- ref_networkx(undirected_weighted, c(cbc = "nx.communicability_betweenness_centrality(G)"))
  expect_reference(centrality_communicability_betweenness(undirected), nx$cbc, reference = "networkx",
                   covers = c("centrality()", "centrality_communicability_betweenness()"),
                   quantity = "communicability betweenness, undirected", tolerance = exact, measure = "scaled")
  expect_reference(centrality_communicability_betweenness(undirected_weighted), on_weighted$cbc, reference = "networkx",
                   covers = c("centrality()", "centrality_communicability_betweenness()"),
                   quantity = "communicability betweenness, undirected weighted", tolerance = exact, measure = "scaled")
})

# Two disconnected graphs of the snajs fixtures (graphs 0093 and 0075 of cograph-centralities-all, the 0/1
# skeleton): 22 nodes in 8 components and 15 nodes in 3. cograph 2.7.21 reads the zeros of exp(A) as entries
# below 1e-15; its eigen-decomposition leaves up to 3.6e-15 between components, and those ratios of rounding errors
# enter the sum (node A of the first 0.12513 for 0.08745; audit A5, 2026-11-23). Declared as a defect, the fix being
# the owner's decision. And a sparse directed graph
# without the ring, so that some nodes cannot reach others.
disconnected_graph <- \(nodes, from, to) {
  adjacency <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  adjacency[cbind(from, to)] <- 1
  adjacency[cbind(to, from)] <- 1
  adjacency
}
eight_components <- disconnected_graph(LETTERS[1:22],
  from = c("A", "A", "A", "B", "E", "E", "H", "H", "H", "I", "K", "L", "M", "N", "O", "P"),
  to = c("B", "N", "V", "I", "R", "U", "I", "P", "V", "T", "V", "M", "U", "Q", "S", "V"))
three_components <- disconnected_graph(LETTERS[1:15],
  from = c("A", "C", "E", "F", "G", "G", "G", "G", "I", "J", "J", "K", "L"),
  to = c("G", "M", "L", "J", "H", "K", "L", "O", "K", "K", "L", "N", "M"))
directed_sparse <- ref_graph(12, density = 0.15, seed = 35L, directed = TRUE)

test_that("communicability betweenness of a disconnected graph equals the definition, exp(A) by its Taylor series", {
  # Estrada, Higham and Hatano (2009) sum over the pairs that communicate (G_st > 0). exp(A) of the 0/1 adjacency
  # by its Taylor series: every term is nonnegative, so nothing cancels and the entries between components are
  # exactly 0 (cograph takes an eigen-decomposition or a Pade approximant, and the reachability closure). 120
  # terms: the remainder is below 1e-100 for the largest degree here.
  taylor_exp <- \(a) Reduce(\(acc, k) {
    term <- acc$term %*% a / k
    list(term = term, sum = acc$sum + term)
  }, seq_len(120L), init = list(term = diag(nrow(a)), sum = diag(nrow(a))))$sum
  definition <- \(adjacency) {
    a <- (adjacency != 0) * 1
    n <- nrow(a)
    g <- taylor_exp(a)
    scores <- vapply(seq_len(n), \(r) {
      reduced <- a
      reduced[r, ] <- 0
      reduced[, r] <- 0
      g_r <- taylor_exp(reduced)
      keep <- g > 0 & row(g) != col(g) & row(g) != r & col(g) != r
      sum((g[keep] - g_r[keep]) / g[keep])
    }, numeric(1))
    ref_named(scores / ((n - 1) * (n - 2)), adjacency)
  }
  reference <- "independent: Estrada, Higham and Hatano (2009), exp(A) by its Taylor series, base R"
  covers <- c("centrality()", "centrality_communicability_betweenness()")
  expect_reference(centrality_communicability_betweenness(eight_components, directed = FALSE),
                   definition(eight_components), reference = reference, covers = covers,
                   quantity = "communicability betweenness, 8 components", tolerance = exact, measure = "scaled",
                   defect = "cograph sums ratios of rounding errors between components of a disconnected graph (zeros of exp(A) read as entries below 1e-15); owner decision pending, FINDINGS.md")
  expect_reference(centrality_communicability_betweenness(three_components, directed = FALSE),
                   definition(three_components), reference = reference, covers = covers,
                   quantity = "communicability betweenness, 3 components", tolerance = exact, measure = "scaled",
                   defect = "cograph sums ratios of rounding errors between components of a disconnected graph (zeros of exp(A) read as entries below 1e-15); owner decision pending, FINDINGS.md")
  expect_reference(centrality_communicability_betweenness(directed_sparse, directed = TRUE), definition(directed_sparse),
                   reference = reference, covers = covers,
                   quantity = "communicability betweenness, sparse directed", tolerance = exact, measure = "scaled")
})

test_that("communicability betweenness of a disconnected graph equals NetworkX's, component by component", {
  # NetworkX divides by G_st itself and gives NaN on a disconnected graph, so it is run on each connected
  # component, whose scores it normalises by (n_c - 1)(n_c - 2); the test multiplies that back and divides by
  # (n - 1)(n - 2), n of the whole graph: the definition's sum has no term across components. The split and the
  # rescaling are the test's own steps, so the comparison is declared `independent`, not as NetworkX.
  call <- paste("{v: s * (len(c) - 1) * (len(c) - 2) for c in nx.connected_components(G) for v, s in",
                "nx.communicability_betweenness_centrality(G.subgraph(c)).items()}")
  per_component <- \(graph) {
    n <- nrow(graph)
    ref_networkx(graph, c(cbc = call))$cbc[rownames(graph)] / ((n - 1) * (n - 2))
  }
  reference <- "independent: NetworkX communicability_betweenness_centrality() per component, rescaled in the test"
  covers <- c("centrality()", "centrality_communicability_betweenness()")
  expect_reference(centrality_communicability_betweenness(eight_components, directed = FALSE),
                   per_component(eight_components), reference = reference, covers = covers,
                   quantity = "communicability betweenness, 8 components, NetworkX per component",
                   tolerance = exact, measure = "scaled",
                   defect = "cograph sums ratios of rounding errors between components of a disconnected graph (zeros of exp(A) read as entries below 1e-15); owner decision pending, FINDINGS.md")
  expect_reference(centrality_communicability_betweenness(three_components, directed = FALSE),
                   per_component(three_components), reference = reference, covers = covers,
                   quantity = "communicability betweenness, 3 components, NetworkX per component",
                   tolerance = exact, measure = "scaled",
                   defect = "cograph sums ratios of rounding errors between components of a disconnected graph (zeros of exp(A) read as entries below 1e-15); owner decision pending, FINDINGS.md")
})

test_that("laplacian equals NetworkX's laplacian_centrality(normalized = False) and centiserve's laplacian()", {
  # Qi et al. (2012): the drop in Laplacian energy when the node is removed.
  nx <- ref_networkx(undirected, c(laplacian = "nx.laplacian_centrality(G, normalized=False, weight=None)"))
  expect_reference(centrality_laplacian(undirected), nx$laplacian, reference = "networkx",
                   covers = c("centrality()", "centrality_laplacian()"), quantity = "laplacian, undirected",
                   tolerance = exact, measure = "scaled")
  skip_without_reference("centiserve")
  expect_reference(centrality_laplacian(undirected), ref_named(centiserve::laplacian(ref_igraph(undirected)), undirected),
                   reference = "centiserve", covers = c("centrality()", "centrality_laplacian()"),
                   quantity = "laplacian, undirected", tolerance = exact, measure = "scaled")
})

test_that("laplacian of a weighted network equals NetworkX's laplacian_centrality(weight = 'weight')", {
  on_weighted <- ref_networkx(undirected_weighted, c(
    weighted = "nx.laplacian_centrality(G, normalized=False, weight='weight')",
    unweighted = "nx.laplacian_centrality(G, normalized=False, weight=None)"))
  # Qi et al. (2012) define Laplacian centrality for weighted networks: the drop in Laplacian energy
  # x_v^2 + sum_u w_uv^2 + 2 sum_u x_u w_uv (x the strengths). cograph reads the weights since 2.7.20 (before, the
  # 0/1 adjacency only); weighted = FALSE gives the unweighted formula.
  expect_reference(centrality_laplacian(undirected_weighted), on_weighted$weighted, reference = "networkx",
                   covers = c("centrality()", "centrality_laplacian()"),
                   quantity = "laplacian, undirected weighted, NetworkX weight='weight'", tolerance = exact,
                   measure = "scaled")
  expect_reference(centrality_laplacian(undirected_weighted, weighted = FALSE), on_weighted$unweighted,
                   reference = "networkx", covers = c("centrality()", "centrality_laplacian()"),
                   quantity = "laplacian, undirected weighted, weighted = FALSE, NetworkX weight=None",
                   tolerance = exact, measure = "scaled")
})

test_that("non-backtracking centrality equals Martin, Zhang and Newman's (2014) reduced 2n x 2n matrix", {
  # Martin, Zhang and Newman (2014), eq. 11: the leading eigenvector of M = [A, I - D; I, 0] has the node
  # centralities in its first n entries. cograph builds the 2m x 2m Hashimoto matrix on graphs of this size, so
  # the two routes share no matrix. Rescaled to maximum 1, as cograph reports it.
  martin <- \(graph) {
    n <- nrow(graph)
    adjacency <- (graph != 0) * 1
    reduced <- rbind(cbind(adjacency, diag(1, n) - diag(rowSums(adjacency))), cbind(diag(1, n), matrix(0, n, n)))
    decomposition <- eigen(reduced)
    leading <- Re(decomposition$vectors[seq_len(n), which.max(Re(decomposition$values))])
    ref_unit_max(ref_named(leading, graph))
  }
  expect_reference(centrality(undirected, measures = "nonbacktracking"), ref_nodes(nonbacktracking = martin(undirected)),
                   reference = "independent: Martin, Zhang and Newman (2014) eq. 11, leading eigenvector by base R eigen()",
                   covers = "centrality()", quantity = "nonbacktracking, undirected", tolerance = exact, key = "node")
  expect_reference(centrality(undirected_weighted, measures = "nonbacktracking"),
                   ref_nodes(nonbacktracking = martin(undirected_weighted)),
                   reference = "independent: Martin, Zhang and Newman (2014) eq. 11, leading eigenvector by base R eigen()",
                   covers = "centrality()", quantity = "nonbacktracking, undirected weighted (0/1 adjacency)",
                   tolerance = exact, key = "node")
})
