# Four measures of centrality() with no installed implementation, each written from its definition in base R:
# local average connectivity (Li et al. 2011), the infection number by self-avoiding walks (Bauer and Lizier
# 2012), the entropy variation of a node's deletion (Ai 2017), as entries of centrality(measures = ...), and the
# edge percolated component (Chin et al. 2014, cytoHubba's EPC), a Monte Carlo estimate.
#
# EPC draws its percolations inside the function (only a seed argument). Its expectation is computed exactly
# instead: on a graph of 10 edges every one of the 2^10 percolations is enumerated with its probability, giving
# each node's expected component share and its standard deviation. The estimate of `runs` percolations must lie
# within 4 standard errors (sd / sqrt(runs)) of the exact expectation; a probe of 60 seeds before this file was
# written passed 60 of 60 (largest |z| = 3.28, judged node by node).

defined <- "independent: the definition written in base R"
exact <- 1e-10

karate <- ref_karate()
connected <- ref_connected_graph(12, density = 0.3, seed = 81L)
directed <- ref_connected_graph(10, density = 0.3, seed = 82L, directed = TRUE)
star5 <- za_star(5)
path5 <- za_path(5)

#' Local average connectivity (Li et al. 2011): the mean degree of v's neighbours inside the subgraph their set
#' induces. `mode` takes the out- or in-neighbours of a directed graph and their out- or in-degree inside it.
lac_defined <- function(graph, mode = c("all", "out", "in")) {
  mode <- match.arg(mode)
  binary <- (graph != 0) * 1
  diag(binary) <- 0
  if (mode == "all") binary <- pmax(binary, t(binary))
  if (mode == "in") binary <- t(binary)
  vapply(seq_len(nrow(binary)), \(v) {
    neighbours <- which(binary[v, ] > 0)
    if (!length(neighbours)) return(0)
    sum(binary[neighbours, neighbours, drop = FALSE]) / length(neighbours)
  }, numeric(1)) |> stats::setNames(rownames(graph))
}

#' Infection number (Bauer and Lizier 2012): sum over the self-avoiding walks from v of beta^L (1 - mu)^(L - 1),
#' L the walk's length, L = 1 .. max_length, on an undirected graph
infection_defined <- function(graph, beta = 0.8, mu = 0, max_length = 6L) {
  binary <- vr_neighbours(graph)
  # walks(v, visited, L): the weighted count of the self-avoiding walks that extend a walk of length L ending at v
  walks <- function(v, visited, length) {
    if (length >= max_length) return(0)
    steps <- setdiff(which(binary[v, ] > 0), visited)
    sum(vapply(steps, \(u) beta^(length + 1) * (1 - mu)^length + walks(u, c(visited, u), length + 1L), numeric(1)))
  }
  vapply(seq_len(nrow(binary)), \(v) walks(v, v, 0L), numeric(1)) |> stats::setNames(rownames(graph))
}

#' The exact expected component share of each node under bond percolation, and its standard deviation
#'
#' @param graph An undirected 0/1 matrix with at most 12 edges.
#' @param removal The probability that an edge is removed.
#' @return A list: `mean` and `sd`, each named by node.
epc_exact <- function(graph, removal = 0.5) {
  binary <- vr_neighbours(graph)
  n <- nrow(binary)
  ends <- which(binary > 0 & upper.tri(binary), arr.ind = TRUE)
  stopifnot("the enumeration is for at most 12 edges" = nrow(ends) <= 12L)
  kept <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), nrow(ends))))
  share <- t(apply(kept, 1L, \(live) {
    sub <- matrix(0, n, n)
    sub[ends[live, , drop = FALSE]] <- 1
    sub <- sub + t(sub) + diag(n)
    # reachability by repeated squaring of (I + A)
    reach <- Reduce(\(r, i) (r %*% r > 0) * 1, seq_len(ceiling(log2(n)) + 1L), sub)
    rowSums(reach) / n
  }))
  probability <- apply(kept, 1L, \(live) prod(ifelse(live, 1 - removal, removal)))
  mean <- colSums(share * probability)
  list(mean = stats::setNames(mean, rownames(graph)),
       sd = stats::setNames(sqrt(colSums(share^2 * probability) - mean^2), rownames(graph)))
}

test_that("local average connectivity equals its definition", {
  covers <- c('centrality(measures = "lac")', "centrality_lac()")
  expect_reference(centrality_lac(karate), lac_defined(karate), reference = defined, covers = covers,
                   quantity = "lac, karate", tolerance = exact)
  expect_reference(centrality_lac(connected), lac_defined(connected), reference = defined, covers = covers,
                   quantity = "lac, connected random graph", tolerance = exact)
  expect_reference(centrality_lac(directed, mode = "out"), lac_defined(directed, "out"), reference = defined,
                   covers = covers, quantity = "lac, mode out, directed", tolerance = exact)
  expect_reference(centrality_lac(directed, mode = "in"), lac_defined(directed, "in"), reference = defined,
                   covers = covers, quantity = "lac, mode in, directed", tolerance = exact)
  # A star: the hub's neighbours share no tie (0); each leaf's one neighbour, the hub, has no other neighbour
  # among them (0).
  expect_reference(centrality_lac(star5), stats::setNames(rep(0, 5), LETTERS[1:5]), reference = defined,
                   covers = covers, quantity = "lac, star of 5, by hand", tolerance = exact)
})

test_that("the infection number sums beta^L over the self-avoiding walks from the node", {
  covers <- 'centrality(measures = "infection")'
  expect_reference(centrality(path5, measures = "infection"), ref_nodes(infection = infection_defined(path5)),
                   reference = defined, covers = covers, quantity = "infection, path of 5", tolerance = exact,
                   key = "node")
  expect_reference(centrality(connected, measures = "infection"), ref_nodes(infection = infection_defined(connected)),
                   reference = defined, covers = covers, quantity = "infection, connected random graph",
                   tolerance = exact, key = "node")
  # By hand on the path A - B - C - D - E from A: one walk of each length 1 to 4, 0.8 + 0.64 + 0.512 + 0.4096.
  expect_reference(centrality(path5, measures = "infection")$infection[[1L]], 0.8 + 0.64 + 0.512 + 0.4096,
                   reference = defined, covers = covers, quantity = "infection of an end of a path of 5, by hand",
                   tolerance = exact)
})

test_that("the entropy variation entries of centrality() equal the drop in entropy on deleting the node", {
  expect_reference(centrality(connected, measures = "entropy_variation_degree"),
                   ref_nodes(entropy_variation_degree_all = za_entropy_variation(connected, "degree")),
                   reference = "independent: the definition written in base R (helper-zoo-a.R)",
                   covers = 'centrality(measures = "entropy_variation_degree")',
                   quantity = "entropy_variation_degree, connected random graph", tolerance = exact, key = "node")
  expect_reference(centrality(connected, measures = "entropy_variation_betweenness"),
                   ref_nodes(entropy_variation_betweenness = za_entropy_variation(connected, "betweenness")),
                   reference = "independent: the definition written in base R (helper-zoo-a.R)",
                   covers = 'centrality(measures = "entropy_variation_betweenness")',
                   quantity = "entropy_variation_betweenness, connected random graph", tolerance = exact, key = "node")
})

test_that("EPC agrees with the exact expected component share within its Monte Carlo error", {
  # Two triangles and a square joined into one graph of 8 nodes and 10 edges: 1024 percolations.
  graph <- za_graph(c(1, 2, 1, 3, 2, 3, 3, 4, 4, 5, 5, 6, 4, 6, 6, 7, 7, 8, 2, 8), 8, names = LETTERS[1:8])
  truth <- epc_exact(graph, removal = 0.5)
  runs <- 2000
  # Four standard errors of the largest node SD: fixed from the exact SD before any run.
  tolerance <- 4 * max(truth$sd) / sqrt(runs)
  lapply(1:5, \(seed) {
    expect_reference(centrality_epc(graph, epc_runs = runs, epc_seed = seed), truth$mean,
                     reference = "independent: every percolation of 10 edges enumerated, base R",
                     covers = c('centrality(measures = "epc")', "centrality_epc()"),
                     quantity = sprintf("Monte Carlo summary: EPC mean component share, 2000 runs, seed %d", seed),
                     tolerance = tolerance)
  })
  expect_reference(centrality_epc(graph, epc_threshold = 0, epc_runs = 3, epc_seed = 1), rep(1, 8),
                   reference = "independent: no edge removed, every node in one component",
                   covers = "centrality_epc()", quantity = "EPC with removal probability 0", tolerance = exact)
})
