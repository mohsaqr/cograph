# disparity_filter() and its methods for a matrix, an edge-list data frame (default), an igraph graph, a
# cograph_network and a tna model: the backbone of Serrano, Boguna and Vespignani (2009). The reference is the
# filter written from the paper in helper-d2.R (ref_disparity()): alpha_ij = (1 - p_ij)^(k_i - 1) with
# p_ij = w_ij / s_i, an edge kept when alpha < level at either endpoint, out-edges tested against the source's
# out-strength and in-edges against the target's in-strength in a directed network. The comparison is the
# whole kept/dropped matrix, cell by cell.
#
# Graphs: random weighted graphs whose weights are drawn from an exponential distribution, so that a node's
# strength is concentrated on a few edges and both outcomes occur at the levels used. A p-value that equals the
# level exactly would make the comparison hinge on rounding; with six-decimal weights none does.

heavy_tailed <- function(nodes, density, seed, directed) {
  graph <- ref_graph(nodes, density = density, seed = seed, directed = directed)
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()),
          add = TRUE, after = FALSE)
  set.seed(seed + 100L)
  weights <- matrix(round(stats::rexp(nodes * nodes), 6L) + 1e-3, nodes, nodes)
  if (!directed) weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]
  graph * weights
}
undirected <- heavy_tailed(15, 0.35, 41L, directed = FALSE)
directed <- heavy_tailed(15, 0.3, 42L, directed = TRUE)
reference <- "independent: Serrano, Boguna and Vespignani (2009) disparity filter, base R"

test_that("the backbone of a weight matrix equals the disparity filter written from the paper", {
  expect_reference(disparity_filter(undirected, level = 0.3), ref_disparity(undirected, level = 0.3),
                   reference = reference, covers = c("disparity_filter()", "disparity_filter.matrix()"),
                   quantity = "kept edges, undirected, level 0.3", tolerance = 0)
  expect_reference(disparity_filter(undirected, level = 0.05), ref_disparity(undirected, level = 0.05),
                   reference = reference, covers = c("disparity_filter()", "disparity_filter.matrix()"),
                   quantity = "kept edges, undirected, level 0.05", tolerance = 0)
  expect_reference(disparity_filter(directed, level = 0.3), ref_disparity(directed, level = 0.3),
                   reference = reference, covers = c("disparity_filter()", "disparity_filter.matrix()"),
                   quantity = "kept edges, directed, level 0.3", tolerance = 0)
})

test_that("every input type gives the backbone written from the paper", {
  cells <- which(directed != 0, arr.ind = TRUE)
  edge_list <- data.frame(from = rownames(directed)[cells[, 1L]], to = colnames(directed)[cells[, 2L]],
                          weight = directed[cells])
  expected <- ref_disparity(directed, level = 0.3)
  expect_reference(disparity_filter(edge_list, level = 0.3)$significant, expected, reference = reference,
                   covers = c("disparity_filter()", "disparity_filter.default()"),
                   quantity = "kept edges, edge-list data frame, directed", tolerance = 0)
  expect_reference(disparity_filter(as_cograph(directed), level = 0.3)$significant, expected, reference = reference,
                   covers = c("disparity_filter()", "disparity_filter.cograph_network()"),
                   quantity = "kept edges, cograph_network, directed", tolerance = 0)
  expect_reference(disparity_filter(ref_igraph(directed), level = 0.3)$significant, expected, reference = reference,
                   covers = c("disparity_filter()", "disparity_filter.igraph()"),
                   quantity = "kept edges, igraph graph, directed", tolerance = 0)
})

test_that("the backbone of a tna model equals the disparity filter of its transition probabilities", {
  skip_without_reference("tna")
  model <- tna::tna(tna::group_regulation)
  probabilities <- model$weights
  filtered <- disparity_filter(model, level = 0.3)
  expect_reference(filtered$significant, ref_disparity(probabilities, level = 0.3), reference = reference,
                   covers = c("disparity_filter()", "disparity_filter.tna()"),
                   quantity = "kept edges, tna model of group_regulation, level 0.3", tolerance = 0)
  expect_reference(filtered$weights_filtered, probabilities * ref_disparity(probabilities, level = 0.3),
                   reference = reference, covers = c("disparity_filter()", "disparity_filter.tna()"),
                   quantity = "backbone weights, tna model of group_regulation, level 0.3", tolerance = 0)
})
