# Reference comparisons moved from the regression tests of the 2026-10 help-page revision
# (tests/testthat/test-bugfix-2026-10-algorithms.R, test-bugfix-2026-10-weights.R, test-bugfix-2026-10-network.R)
# and of the 2026-11-18 review: centrality measures checked against igraph, brainGraph or their formula after a
# fix, and cluster_significance()'s null on a complete graph. cograph computes these centralities itself
# (R/centrality*.R, the kernels), so igraph and brainGraph are counted references; cluster_significance() calls
# igraph::modularity(), so igraph is `internal` there and NetworkX's modularity() is the counted reference.
#
# Graphs: the bundled regulation_net (10 nodes, directed, weighted) where the bugs were found, and the six-node
# weighted network of the weights regression file.

exact <- 1e-10

weighted_six <- matrix(c(
  0,   2.5, 0,   1.2, 0,   0,
  0.4, 0,   3.1, 0,   0,   1.7,
  0,   0,   0,   2.2, 0.6, 0,
  1.9, 0,   0,   0,   2.8, 0,
  0,   1.1, 0,   0,   0,   0.9,
  2.4, 0,   0.7, 0,   1.5, 0
), 6, byrow = TRUE, dimnames = list(LETTERS[1:6], LETTERS[1:6]))

regulation_binary <- d1_binary(regulation_net)
regulation_loopless <- local({
  graph <- regulation_net
  diag(graph) <- 0
  graph
})

test_that("communicability on directed input equals the row sums of exp(A) by its Taylor series (bug 3)", {
  # exp(A) = sum_k A^k / k!, each term built from the previous one; 60 terms for a 0/1 matrix of 10 nodes.
  series <- Reduce(\(term, k) term %*% regulation_binary / k, seq_len(60L), accumulate = TRUE, init = diag(10))
  expect_reference(centrality_communicability(regulation_net), ref_named(rowSums(Reduce(`+`, series)), regulation_net),
                   reference = "independent: the definitions written in base R (each named in its note)", note = "row sums of exp(A), Taylor series (Estrada & Hatano 2008)",
                   covers = c("centrality()", "centrality_communicability()"),
                   quantity = "communicability, directed regulation_net", tolerance = exact, measure = "relative")
})

test_that("communicability betweenness on directed input equals its definition (bug 4)", {
  # Estrada, Higham & Hatano (2009): for each node r, the share of the communicability G_pq that passes through r,
  # (G_pq - G(r)_pq) / G_pq with r's ties removed, summed over p != q != r and divided by (n - 1)(n - 2);
  # exp(A) by its Taylor series.
  taylor <- \(a) Reduce(`+`, Reduce(\(term, k) term %*% a / k, seq_len(60L), accumulate = TRUE, init = diag(nrow(a))))
  n <- nrow(regulation_binary)
  whole <- taylor(regulation_binary)
  through <- vapply(seq_len(n), \(r) {
    without <- regulation_binary
    without[r, ] <- 0
    without[, r] <- 0
    ratio <- (whole - taylor(without)) / whole
    diag(ratio) <- 0
    ratio[r, ] <- 0
    ratio[, r] <- 0
    sum(ratio)
  }, numeric(1)) / ((n - 1) * (n - 2))
  expect_reference(centrality_communicability_betweenness(regulation_net), ref_named(through, regulation_net),
                   reference = "independent: the definitions written in base R (each named in its note)", note = "communicability betweenness (Estrada, Higham & Hatano 2009)",
                   covers = c("centrality()", "centrality_communicability_betweenness()"),
                   quantity = "communicability betweenness, directed regulation_net", tolerance = exact)
})

test_that("gateway coefficient equals brainGraph's gateway_coeff() (bug 5)", {
  skip_without_reference(c("brainGraph", "igraph"))
  membership <- rep(1:2, each = 5L)
  skeleton <- ((regulation_net + t(regulation_net)) != 0) * 1
  diag(skeleton) <- 0
  expect_reference(centrality_gateway(skeleton, membership = membership, directed = FALSE),
                   ref_named(brainGraph::gateway_coeff(ref_igraph(skeleton), membership, centr = "degree"), skeleton),
                   reference = "brainGraph", covers = c("centrality()", "centrality_gateway()"),
                   quantity = "gateway, undirected skeleton of regulation_net", tolerance = exact)
  # Directed with mode = "all": brainGraph reads the in + out tie counts it is given as A.
  expect_reference(centrality_gateway(regulation_net, membership = membership, mode = "all"),
                   ref_named(brainGraph::gateway_coeff(ref_igraph(regulation_binary, directed = TRUE), membership,
                                                       centr = "degree", A = regulation_binary + t(regulation_binary)),
                             regulation_net),
                   reference = "brainGraph", covers = c("centrality()", "centrality_gateway()"),
                   quantity = "gateway, directed regulation_net, mode all", tolerance = exact)
})

test_that("alpha and power centrality honour mode and equal igraph (bug 6)", {
  skip_without_reference("igraph")
  graph <- ref_igraph(regulation_loopless, directed = TRUE)
  expect_reference(centrality_alpha(regulation_net, mode = "in"), ref_named(igraph::alpha_centrality(graph, alpha = 1), regulation_net),
                   reference = "igraph", covers = c("centrality()", "centrality_alpha()"),
                   quantity = "alpha, mode in, regulation_net", tolerance = exact, measure = "relative")
  expect_reference(centrality_alpha(regulation_net, mode = "out"),
                   ref_named(igraph::alpha_centrality(igraph::reverse_edges(graph), alpha = 1), regulation_net),
                   reference = "igraph", covers = c("centrality()", "centrality_alpha()"),
                   quantity = "alpha, mode out, regulation_net", tolerance = exact, measure = "relative")
  collapsed <- igraph::as_undirected(graph, mode = "collapse", edge.attr.comb = "sum")
  expect_reference(centrality_alpha(regulation_net, mode = "all"), ref_named(igraph::alpha_centrality(collapsed, alpha = 1), regulation_net),
                   reference = "igraph", covers = c("centrality()", "centrality_alpha()"),
                   quantity = "alpha, mode all, regulation_net", tolerance = exact, measure = "relative")
  binary <- ref_igraph(regulation_binary, directed = TRUE)
  expect_reference(centrality_power(regulation_net, mode = "out"), ref_named(igraph::power_centrality(binary, exponent = 1), regulation_net),
                   reference = "igraph", covers = c("centrality()", "centrality_power()"),
                   quantity = "power, mode out, regulation_net", tolerance = exact, measure = "relative")
  expect_reference(centrality_power(regulation_net, mode = "in"),
                   ref_named(igraph::power_centrality(igraph::reverse_edges(binary), exponent = 1), regulation_net),
                   reference = "igraph", covers = c("centrality()", "centrality_power()"),
                   quantity = "power, mode in, regulation_net", tolerance = exact, measure = "relative")
  expect_reference(centrality_power(regulation_net, mode = "all"),
                   ref_named(igraph::power_centrality(igraph::as_undirected(binary), exponent = 1), regulation_net),
                   reference = "igraph", covers = c("centrality()", "centrality_power()"),
                   quantity = "power, mode all, regulation_net", tolerance = exact, measure = "scaled")
})

test_that("alpha centrality equals its formula, (I - A^T)^-1 1 (bug 6)", {
  n <- nrow(regulation_loopless)
  expect_reference(c(`in` = centrality_alpha(regulation_net, mode = "in"), out = centrality_alpha(regulation_net, mode = "out")),
                   c(`in` = ref_named(solve(diag(n) - t(regulation_loopless), rep(1, n)), regulation_net),
                     out = ref_named(solve(diag(n) - regulation_loopless, rep(1, n)), regulation_net)),
                   reference = "independent: the definitions written in base R (each named in its note)", note = "(I - A^T)^-1 1 (Bonacich & Lloyd 2001, alpha = 1)",
                   covers = c("centrality()", "centrality_alpha()"), quantity = "alpha, regulation_net",
                   tolerance = exact, measure = "relative")
})

test_that("expected influence equals its sums of edge weights (bug 7)", {
  # Robinaugh et al. (2016): one-step expected influence is the summed weight of a node's ties (each undirected
  # tie once); two-step adds the neighbours' one-step values weighted by the tie.
  symmetric <- regulation_net + t(regulation_net)
  loopless <- symmetric
  diag(loopless) <- 0
  one_step <- rowSums(loopless)
  expect_reference(centrality(symmetric, measures = c("expected_influence_1", "expected_influence_2"), mode = "all",
                              directed = FALSE),
                   ref_nodes(expected_influence_1_all = one_step,
                             expected_influence_2_all = one_step + stats::setNames(as.numeric(loopless %*% one_step), names(one_step))),
                   reference = "independent: the definitions written in base R (each named in its note)", note = "expected influence, one and two steps (Robinaugh et al. 2016)",
                   covers = "centrality()", quantity = "expected influence, undirected", tolerance = exact, key = "node")
  expect_reference(centrality_expected_influence_1(regulation_net, mode = "all"),
                   rowSums(regulation_net) + colSums(regulation_net) - diag(regulation_net),
                   reference = "independent: the definitions written in base R (each named in its note)", note = "expected influence, out + in weights",
                   covers = c("centrality()", "centrality_expected_influence_1()"),
                   quantity = "expected influence 1, directed, mode all", tolerance = exact)
})

test_that("local transitivity equals igraph on directed input, and Onnela's equals its formula (bug 9)", {
  skip_without_reference("igraph")
  reference <- ref_named(igraph::transitivity(ref_igraph(regulation_loopless, directed = TRUE), type = "local"), regulation_net)
  expect_reference(centrality_transitivity(regulation_net), reference, reference = "igraph",
                   covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, local, directed regulation_net", tolerance = exact)
  expect_reference(centrality_transitivity(regulation_net, transitivity_type = "localundirected"), reference,
                   reference = "igraph", covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, localundirected, regulation_net", tolerance = exact)
  # Onnela et al. (2005) in the form of Zhang & Horvath (2005): diag(W^3) / (s_i^2 - sum_j w_ij^2).
  weights <- regulation_net + t(regulation_net)
  diag(weights) <- 0
  expect_reference(centrality_transitivity(regulation_net, transitivity_type = "onnela"),
                   diag(weights %*% weights %*% weights) / (rowSums(weights)^2 - rowSums(weights^2)),
                   reference = "independent: the definitions written in base R (each named in its note)", note = "weighted clustering, diag(W^3) / (s^2 - sum w^2) (Zhang & Horvath 2005)",
                   covers = c("centrality()", "centrality_transitivity()"),
                   quantity = "transitivity, onnela, regulation_net", tolerance = exact)
})

test_that("DMNC indexes the deduplicated neighbour set (bug 13)", {
  skip_without_reference("igraph")
  # Lin et al. (2008): the ties among the largest component(s) of a node's neighbourhood over its size^1.7; strong
  # components of the directed neighbourhood, found with igraph.
  dmnc <- vapply(seq_len(nrow(regulation_binary)), \(v) {
    neighbours <- sort(unique(c(which(regulation_binary[v, ] != 0), which(regulation_binary[, v] != 0))))
    if (!length(neighbours)) return(0)
    components <- igraph::components(ref_igraph(regulation_binary[neighbours, neighbours, drop = FALSE], directed = TRUE),
                                     mode = "strong")
    largest <- which(components$csize == max(components$csize))
    keep <- neighbours[components$membership %in% largest]
    ties <- sum(regulation_binary[keep, keep] != 0)
    if (ties == 0) 0 else ties / max(components$csize)^1.7
  }, numeric(1))
  expect_reference(centrality_dmnc(regulation_net, mode = "all"), ref_named(dmnc, regulation_net),
                   reference = "independent: the definitions written in base R (each named in its note)", note = "DMNC over strong components of the neighbourhood (Lin et al. 2008)",
                   covers = c("centrality()", "centrality_dmnc()"), quantity = "dmnc, directed regulation_net",
                   tolerance = exact)
})

test_that("weighted = FALSE equals igraph's unweighted measures (bug 1)", {
  skip_without_reference("igraph")
  graph <- ref_igraph(weighted_six)
  expect_reference(centrality(weighted_six, measures = c("betweenness", "closeness", "strength"), mode = "out",
                              weighted = FALSE),
                   ref_nodes(betweenness = igraph::betweenness(graph, weights = NA),
                             closeness_out = igraph::closeness(graph, mode = "out", weights = NA),
                             strength_out = igraph::degree(graph, mode = "out")),
                   reference = "igraph", covers = "centrality()",
                   quantity = "betweenness, closeness and strength with weighted = FALSE", tolerance = exact, key = "node")
})

test_that("personalized PageRank equals igraph (bug 11)", {
  skip_without_reference("igraph")
  restart <- c(A = 5, B = 1, C = 0, D = 2, E = 0, F = 1)
  # Power iteration in cograph, PRPACK in igraph: agreement to 1e-8, the original test's tolerance.
  expect_reference(centrality_pagerank(weighted_six, personalized = rev(restart)),
                   ref_named(igraph::page_rank(ref_igraph(weighted_six), personalized = unname(restart))$vector, weighted_six),
                   reference = "igraph", covers = c("centrality()", "centrality_pagerank()"),
                   quantity = "pagerank, personalized, named restart vector", tolerance = 1e-8)
})

test_that("cluster_significance() nulls of a complete graph equal its modularity (bug 22)", {
  skip_without_reference("igraph")
  # Every G(n, m) draw with m = n (n - 1) arcs is the complete digraph, whose arcs all weigh the same: each null
  # value is the modularity of the observed partition on it.
  complete <- matrix(2, 5, 5, dimnames = list(letters[1:5], letters[1:5]))
  diag(complete) <- 0
  membership <- c(1, 1, 2, 2, 2)
  result <- cluster_significance(complete, membership, n_random = 5, method = "gnm", null = "fixed", seed = 7)
  expect_reference(result$null_values, rep(igraph::modularity(ref_igraph(complete / 2, directed = TRUE), membership), 5),
                   reference = "igraph, called by cograph", covers = "cluster_significance()",
                   quantity = "null modularity, complete digraph", tolerance = exact)
  nx <- ref_networkx(complete / 2, c(q = "nx.community.modularity(G, [{'a', 'b'}, {'c', 'd', 'e'}])"), directed = TRUE)
  expect_reference(result$null_values, rep(nx$q, 5), reference = "networkx", covers = "cluster_significance()",
                   quantity = "null modularity, complete digraph, NetworkX", tolerance = exact)
})

test_that("csig(), the alias of cluster_significance(), gives the observed and null modularity of a complete digraph", {
  # As above: every G(n, m) null of the complete digraph is the complete digraph, so the null and the observed
  # modularity are both the modularity of the partition on it, whatever the draws; here on five seeds.
  complete <- matrix(1, 6, 6, dimnames = list(letters[1:6], letters[1:6]))
  diag(complete) <- 0
  nx <- ref_networkx(complete, c(q = "nx.community.modularity(G, [{'a', 'b', 'c'}, {'d', 'e', 'f'}])"), directed = TRUE)
  lapply(1:5, \(seed) {
    result <- csig(complete, c(1, 1, 1, 2, 2, 2), n_random = 4, method = "gnm", null = "fixed", seed = seed)
    expect_reference(c(observed = result$observed_modularity, null_mean = result$null_mean, null_sd = result$null_sd),
                     c(observed = nx$q, null_mean = nx$q, null_sd = 0), reference = "networkx",
                     covers = c("csig()", "cluster_significance()"),
                     quantity = sprintf("csig() modularity, complete digraph of 6, seed %d", seed), tolerance = exact)
  })
})

test_that("network_summary(weighted = FALSE) transitivity of a zero-weight tie equals igraph (review 2026-11-18)", {
  skip_without_reference("igraph")
  triangle <- igraph::graph_from_data_frame(data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"), weight = c(1, 1, 0)))
  expect_reference(network_summary(triangle, weighted = FALSE)$transitivity, igraph::transitivity(triangle, type = "global"),
                   reference = "igraph", covers = "network_summary()",
                   quantity = "transitivity, directed triangle with a zero-weight arc", tolerance = exact)
})
