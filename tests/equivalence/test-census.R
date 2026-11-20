# dyad_census(), triad_census() and the observed counts of motif_census(). All three count with igraph
# (igraph::dyad_census(), igraph::triad_census(), igraph::motifs() in R/dyad-census.R and R/motifs.R), so igraph is
# `internal`; sna (dyad.census(), triad.census(), grecip()) and NetworkX (triadic_census(), overall_reciprocity())
# are the counted references. The null-model columns of motif_census() (null_mean, z_score, p_value) come from
# graphs drawn inside the function with no argument to supply them, so only `count` is compared.
#
# Graphs: random graphs from helper-reference.R and regulation_net (three reciprocated pairs, 24 one-way arcs).

directed <- ref_graph(12, density = 0.3, seed = 71L, directed = TRUE)
directed_dense <- ref_graph(10, density = 0.5, seed = 72L, directed = TRUE)
undirected <- ref_graph(12, density = 0.35, seed = 73L)

exact <- 1e-10
man_classes <- c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", "030T", "030C", "201", "120D", "120U",
                 "120C", "210", "300")

census_table <- \(counts, column = "count") {
  out <- data.frame(type = names(counts))
  out[[column]] <- as.numeric(counts)
  out
}

test_that("dyad_census() equals sna's dyad.census()", {
  skip_without_reference("sna")
  sna_counts <- \(graph) {
    counts <- sna::dyad.census(graph)
    census_table(c(mutual = unname(counts[1, "Mut"]), asymmetric = unname(counts[1, "Asym"]), null = unname(counts[1, "Null"])))
  }
  expect_reference(dyad_census(directed), sna_counts(directed), reference = "sna", covers = "dyad_census()",
                   quantity = "dyad census, directed", tolerance = exact, key = "type")
  expect_reference(dyad_census(regulation_net), sna_counts(regulation_net), reference = "sna",
                   covers = "dyad_census()", quantity = "dyad census, regulation_net", tolerance = exact, key = "type")
  # An undirected graph: every tie is a mutual dyad.
  expect_reference(dyad_census(undirected), sna_counts(undirected), reference = "sna", covers = "dyad_census()",
                   quantity = "dyad census, undirected", tolerance = exact, key = "type")
})

test_that("dyad_census() reciprocity equals NetworkX and sna's edgewise reciprocity", {
  skip_without_reference("sna")
  nx <- ref_networkx(regulation_net, c(reciprocity = "nx.overall_reciprocity(G)"))
  on_directed <- ref_networkx(directed, c(reciprocity = "nx.overall_reciprocity(G)"))
  reported <- c(regulation_net = attr(dyad_census(regulation_net), "reciprocity"),
                directed = attr(dyad_census(directed), "reciprocity"))
  expect_reference(reported, c(regulation_net = nx$reciprocity, directed = on_directed$reciprocity),
                   reference = "networkx", covers = "dyad_census()", quantity = "reciprocity, 2M / (2M + A)",
                   tolerance = exact)
  expect_reference(reported, c(regulation_net = unname(sna::grecip(regulation_net, measure = "edgewise")),
                               directed = unname(sna::grecip(directed, measure = "edgewise"))),
                   reference = "sna", covers = "dyad_census()", quantity = "reciprocity, edgewise", tolerance = exact)
})

test_that("triad_census() equals sna's triad.census() and NetworkX's triadic_census()", {
  skip_without_reference("sna")
  nx <- ref_networkx(directed, c(census = "nx.triadic_census(G)"))
  on_regulation <- ref_networkx(regulation_net, c(census = "nx.triadic_census(G)"))
  expect_reference(triad_census(directed), census_table(nx$census[man_classes]), reference = "networkx",
                   covers = "triad_census()", quantity = "triad census, directed", tolerance = exact, key = "type")
  expect_reference(triad_census(regulation_net), census_table(on_regulation$census[man_classes]),
                   reference = "networkx", covers = "triad_census()", quantity = "triad census, regulation_net",
                   tolerance = exact, key = "type")
  sna_census <- sna::triad.census(directed_dense)
  expect_reference(triad_census(directed_dense), census_table(stats::setNames(sna_census[1, ], colnames(sna_census))),
                   reference = "sna", covers = "triad_census()", quantity = "triad census, dense directed",
                   tolerance = exact, key = "type")
})

test_that("triad_census() of an edge list equals NetworkX's census of the undirected triangle", {
  # A pair with any positive row is a tie, and an edge list without a direction is read as undirected: the
  # 4-row edge list with a duplicated, negative row is the triangle A - B - C, one triad of class 300 in the
  # census of its symmetric digraph.
  edges <- data.frame(from = c("A", "A", "B", "C"), to = c("B", "B", "C", "A"), weight = c(1, -1, 1, 1))
  triangle <- matrix(1, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  diag(triangle) <- 0
  nx <- ref_networkx(triangle, c(census = "nx.triadic_census(G.to_directed())"))
  expect_reference(triad_census(edges), census_table(nx$census[man_classes]), reference = "networkx",
                   covers = "triad_census()", quantity = "triad census, edge list with a duplicated row",
                   tolerance = exact, key = "type")
})

test_that("triad_census() equals igraph's triad_census() (moved from the motif review of 2026-10-18)", {
  skip_without_reference("igraph")
  arcs <- matrix(c(0, 1, 0, 1,
                   0, 0, 1, 0,
                   1, 0, 0, 1,
                   0, 1, 0, 0), 4, 4, byrow = TRUE, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  expect_reference(triad_census(arcs), census_table(stats::setNames(igraph::triad_census(ref_igraph(arcs)), man_classes)),
                   reference = "igraph, called by cograph", covers = "triad_census()",
                   quantity = "triad census, four-node digraph", tolerance = exact, key = "type")
})

test_that("motif_census() counts equal NetworkX's triadic census, directed and undirected", {
  nx <- ref_networkx(directed, c(census = "nx.triadic_census(G)"))
  expect_reference(motif_census(directed, n_random = 10, seed = 1),
                   setNames(census_table(nx$census[man_classes]), c("motif", "count")),
                   reference = "networkx", covers = "motif_census()", quantity = "motif counts, directed",
                   tolerance = exact, key = "motif")
  # On an undirected graph the four classes are the triadic classes of its symmetric digraph: 003 (empty),
  # 102 (one edge), 201 (two edges, a wedge), 300 (triangle).
  on_undirected <- ref_networkx(undirected, c(census = "nx.triadic_census(G.to_directed())"))
  undirected_classes <- c(empty = "003", edge = "102", wedge = "201", triangle = "300")
  expect_reference(motif_census(undirected, n_random = 10, seed = 1),
                   data.frame(motif = names(undirected_classes),
                              count = as.numeric(unlist(on_undirected$census[undirected_classes]))),
                   reference = "networkx", covers = "motif_census()", quantity = "motif counts, undirected",
                   tolerance = exact, key = "motif")
})

test_that("motif_census() counts of a signed network equal sna's census of its positive arcs", {
  skip_without_reference("sna")
  # A negative weight is no tie (moved from the motif review of 2026-10-18).
  signed <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  signed["a", "b"] <- -2
  signed["b", "c"] <- 3
  signed["c", "a"] <- 3
  sna_census <- sna::triad.census((signed > 0) * 1)
  expect_reference(motif_census(signed, n_random = 5, seed = 1),
                   data.frame(motif = colnames(sna_census), count = as.numeric(sna_census[1, ])),
                   reference = "sna", covers = "motif_census()", quantity = "motif counts, signed network",
                   tolerance = exact, key = "motif")
})

test_that("motif_census(size = 4) counts equal igraph's motifs()", {
  skip_without_reference("igraph")
  # Moved from the motif review of 2026-10-18: two disjoint pairs among eight nodes. igraph returns NA for the
  # classes it does not count (the disconnected ones); both are recorded as not comparable.
  two_pairs <- matrix(0, 8, 8)
  two_pairs[1, 2] <- two_pairs[2, 1] <- 1
  two_pairs[3, 4] <- two_pairs[4, 3] <- 1
  census <- motif_census(two_pairs, size = 4, n_random = 5, seed = 1)
  expect_reference(census$count, igraph::motifs(ref_igraph(two_pairs, directed = FALSE), 4), reference = "igraph, called by cograph",
                   covers = "motif_census()", quantity = "motif counts, size 4, two pairs",
                   tolerance = exact)
})
