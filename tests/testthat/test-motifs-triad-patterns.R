# Regression tests for triad pattern definitions in motifs-data.R
#
# Guards against the bug reported 2026-08-12 (Mengli Zhang): the visual
# pattern set drew wrong structures for 021D/021U (transposed, i.e. D/U
# swapped) and 120D/120U/210 (120D and 120U were both isomorphic to 120C;
# 210 drew a 120-class triad), so plots mislabeled five glyphs and never
# drew 120U or 210 at all. Counting was unaffected (canonical set was
# already correct), but the visual and canonical sets must stay in
# lockstep: every matrix must belong to the isomorphism class its label
# claims, verified against igraph::triad_census().

# igraph triad_census() ordering (Davis & Leinhardt MAN order)
.census_names <- c("003", "012", "102", "021D", "021U", "021C",
                   "111D", "111U", "030T", "030C", "201",
                   "120D", "120U", "120C", "210", "300")

# Classify a single 3x3 adjacency matrix into its MAN triad class
.classify_triad <- function(m) {
  g <- igraph::graph_from_adjacency_matrix(m, mode = "directed")
  census <- igraph::triad_census(g)
  .census_names[which(census == 1L)]
}

test_that("visual triad patterns match their labeled MAN class", {
  patterns <- cograph:::.get_triad_patterns_visual()
  expect_setequal(names(patterns), .census_names)
  actual <- vapply(patterns, .classify_triad, character(1))
  expect_identical(actual, setNames(names(patterns), names(patterns)))
})

test_that("canonical triad patterns match their labeled MAN class", {
  patterns <- cograph:::.get_triad_patterns_canonical()
  expect_setequal(names(patterns), .census_names)
  actual <- vapply(patterns, .classify_triad, character(1))
  expect_identical(actual, setNames(names(patterns), names(patterns)))
})

test_that("all 16 MAN classes are represented exactly once in each set", {
  for (getter in list(cograph:::.get_triad_patterns_visual,
                      cograph:::.get_triad_patterns_canonical)) {
    classes <- vapply(getter(), .classify_triad, character(1))
    expect_identical(sort(unname(classes)), sort(.census_names))
  }
})

test_that("120D, 120U, 120C are pairwise non-isomorphic in the visual set", {
  patterns <- cograph:::.get_triad_patterns_visual()
  trio <- c("120D", "120U", "120C")
  graphs <- lapply(patterns[trio], function(m) {
    igraph::graph_from_adjacency_matrix(m, mode = "directed")
  })
  pairs <- utils::combn(trio, 2, simplify = FALSE)
  iso <- vapply(pairs, function(p) {
    igraph::isomorphic(graphs[[p[1]]], graphs[[p[2]]])
  }, logical(1))
  expect_false(any(iso))
})

test_that("the 64-entry classifier lookup matches igraph exhaustively", {
  edge_positions <- matrix(c(1L, 2L, 2L, 1L, 1L, 3L,
                             3L, 1L, 2L, 3L, 3L, 2L),
                           ncol = 2, byrow = TRUE)
  lookup <- cograph:::.get_triad_lookup()

  reference <- vapply(0:63, function(code) {
    m <- matrix(0L, 3, 3)
    bits <- as.integer(intToBits(code))[1:6]
    m[edge_positions] <- bits
    .classify_triad(m)
  }, character(1))

  expect_identical(unname(lookup), unname(reference))
})
