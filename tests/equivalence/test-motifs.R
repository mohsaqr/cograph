# motifs(), extract_motifs(), extract_triads() and the as.data.frame() methods of their results: the MAN class
# (Holland & Leinhardt 1970) of every triple and the census of classes, compared with NetworkX (triad_type(),
# triadic_census()) and sna (triad.census()). These functions classify triples with cograph's own code
# (.count_triad_types(), .classify_triads_vectorized() in R/motifs.R), so igraph's triad_census() is a counted
# reference here. Significance columns come from permutations drawn inside the functions and are not compared.
#
# Graphs: random directed graphs from helper-reference.R with at most nine nodes, so that NetworkX's sorted node
# names (N1 ... N9) are the matrix order.

directed <- ref_graph(9, density = 0.3, seed = 31L, directed = TRUE)
directed_weighted <- ref_graph(9, density = 0.4, seed = 33L, directed = TRUE, weighted = TRUE)

exact <- 1e-10
man_classes <- c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", "030T", "030C", "201", "120D", "120U",
                 "120C", "210", "300")

test_that("motifs(pattern = 'all') counts equal NetworkX's triadic census", {
  nx <- ref_networkx(directed, c(census = "nx.triadic_census(G)"))
  census <- nx$census[man_classes]
  # motifs() lists the classes that occur; the classes NetworkX counts 0 times are not rows of its table.
  present <- census[census > 0]
  result <- motifs(directed, pattern = "all", significance = FALSE, min_transitions = 0)
  expect_reference(as.data.frame(result, what = "types"), data.frame(type = names(present), count = as.numeric(present)),
                   reference = "networkx", covers = c("motifs()", "as.data.frame.cograph_motif_result()"),
                   quantity = "triad classes, directed", tolerance = exact, key = "type")
})

test_that("motifs(pattern = 'all') counts equal igraph's and sna's triad census (moved from adversarial fix 2)", {
  skip_without_reference(c("igraph", "sna"))
  sparse <- ref_graph(6, density = 0.2, seed = 7L, directed = TRUE)
  result <- as.data.frame(motifs(sparse, pattern = "all", significance = FALSE, min_transitions = 0,
                                 min_count = NULL), what = "types")
  from_igraph <- stats::setNames(igraph::triad_census(ref_igraph(sparse, directed = TRUE)), man_classes)
  present <- from_igraph[from_igraph > 0]
  expect_reference(result, data.frame(type = names(present), count = as.numeric(present)), reference = "igraph",
                   covers = c("motifs()", "as.data.frame.cograph_motif_result()"),
                   quantity = "triad classes, sparse directed", tolerance = exact, key = "type")
  from_sna <- sna::triad.census(directed_weighted)
  present_sna <- from_sna[1, from_sna[1, ] > 0]
  expect_reference(as.data.frame(motifs(directed_weighted, pattern = "all", significance = FALSE,
                                        min_transitions = 0, min_count = NULL), what = "types"),
                   data.frame(type = names(present_sna), count = as.numeric(present_sna)), reference = "sna",
                   covers = c("motifs()", "as.data.frame.cograph_motif_result()"),
                   quantity = "triad classes, weighted directed", tolerance = exact, key = "type")
})

test_that("extract_triads() classifies every connected triple as NetworkX's triad_type()", {
  expected <- d1_triad_table(ref_networkx(directed_weighted, c(types = d1_py_triad_types))$types)
  # extract_triads() lists the triples with at least one tie: every class but 003.
  expected <- expected[expected$type != "003", ]
  triads <- extract_triads(directed_weighted, min_total = 0)
  expect_reference(data.frame(node1 = triads$A, node2 = triads$B, node3 = triads$C, type = triads$type), expected,
                   reference = "networkx", covers = "extract_triads()", quantity = "triad class",
                   tolerance = 0, key = c("node1", "node2", "node3"))
})

test_that("extract_motifs() classifies every triple as NetworkX's triad_type()", {
  expected <- d1_triad_table(ref_networkx(directed, c(types = d1_py_triad_types))$types)
  result <- extract_motifs(directed, pattern = "all", min_transitions = 0)
  expect_reference(as.data.frame(result), expected, reference = "networkx",
                   covers = c("extract_motifs()", "as.data.frame.cograph_motif_analysis()"),
                   quantity = "triad class of each triple", tolerance = 0, key = c("node1", "node2", "node3"))
})

test_that("subgraphs() classifies every triple as NetworkX's triad_type()", {
  # subgraphs() is motifs(named_nodes = TRUE): one row per node triple, here every one of the C(9, 3) = 84 triples.
  expected <- d1_triad_table(ref_networkx(directed, c(types = d1_py_triad_types))$types)
  result <- subgraphs(directed, pattern = "all", significance = FALSE, min_count = NULL, min_transitions = 0)
  expect_reference(as.data.frame(result), expected, reference = "networkx",
                   covers = c("subgraphs()", "as.data.frame.cograph_motif_result()"),
                   quantity = "subgraphs() triad class of each triple", tolerance = 0, key = c("node1", "node2", "node3"))
})

test_that("extract_motifs(level = 'aggregate') classifies the pooled network (moved from adversarial fix 7)", {
  skip_without_reference("igraph")
  cycle <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
  transitive <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
  edges <- rbind(transform(cycle, actor = "c1"), transform(cycle, actor = "c2"), transform(transitive, actor = "t1"))
  pooled <- extract_motifs(data = edges, id = "actor", level = "aggregate", pattern = "triangle", min_transitions = 0)
  # The pooled network: A->B (3), B->C (3), C->A (2), A->C (1).
  arcs <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  arcs["A", "B"] <- 3
  arcs["B", "C"] <- 3
  arcs["C", "A"] <- 2
  arcs["A", "C"] <- 1
  census <- stats::setNames(igraph::triad_census(ref_igraph((arcs > 0) * 1, directed = TRUE)), man_classes)
  expect_reference(as.data.frame(pooled)$type, names(census)[census == 1], reference = "igraph",
                   covers = c("extract_motifs()", "as.data.frame.cograph_motif_analysis()"),
                   quantity = "triad class of the pooled network", tolerance = 0)
  expected <- d1_triad_table(ref_networkx(arcs, c(types = d1_py_triad_types))$types)
  expect_reference(as.data.frame(pooled), expected, reference = "networkx",
                   covers = c("extract_motifs()", "as.data.frame.cograph_motif_analysis()"),
                   quantity = "triad class of the pooled network, NetworkX", tolerance = 0,
                   key = c("node1", "node2", "node3"))
})

test_that("extract_triads() classifies all 63 non-empty labelled triads as NetworkX's triad_type()", {
  # Every pattern of the six possible arcs among three nodes (bit i of the code is the i-th arc below).
  arcs <- rbind(c(1, 2), c(2, 1), c(1, 3), c(3, 1), c(2, 3), c(3, 2))
  patterns <- lapply(seq_len(63L), \(code) {
    pattern <- matrix(0, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
    pattern[arcs[bitwAnd(code, 2L^(0:5)) > 0, , drop = FALSE]] <- 1
    pattern
  })
  classes <- vapply(patterns, \(pattern) extract_triads(pattern, min_total = 0, directed = TRUE)$type, character(1))
  nx <- ref_networkx(patterns[[63L]], c(types = paste0(
    "[(lambda D: (D.add_nodes_from([1, 2, 3]), D.add_edges_from([e for i, e in enumerate([(1, 2), (2, 1), (1, 3), ",
    "(3, 1), (2, 3), (3, 2)]) if (c >> i) & 1]), nx.triad_type(D))[2])(nx.DiGraph()) for c in range(1, 64)]")))
  expect_reference(stats::setNames(classes, seq_len(63L)), stats::setNames(unlist(nx$types), seq_len(63L)),
                   reference = "networkx", covers = "extract_triads()", quantity = "triad class of pattern code",
                   tolerance = 0)
})
