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
  skip_if_not_installed("igraph")
  patterns <- cograph:::.get_triad_patterns_visual()
  expect_setequal(names(patterns), .census_names)
  actual <- vapply(patterns, .classify_triad, character(1))
  expect_identical(actual, setNames(names(patterns), names(patterns)))
})

test_that("canonical triad patterns match their labeled MAN class", {
  skip_if_not_installed("igraph")
  patterns <- cograph:::.get_triad_patterns_canonical()
  expect_setequal(names(patterns), .census_names)
  actual <- vapply(patterns, .classify_triad, character(1))
  expect_identical(actual, setNames(names(patterns), names(patterns)))
})

test_that("all 16 MAN classes are represented exactly once in each set", {
  skip_if_not_installed("igraph")
  for (getter in list(cograph:::.get_triad_patterns_visual,
                      cograph:::.get_triad_patterns_canonical)) {
    classes <- vapply(getter(), .classify_triad, character(1))
    expect_identical(sort(unname(classes)), sort(.census_names))
  }
})

test_that("120D, 120U, 120C are pairwise non-isomorphic in the visual set", {
  skip_if_not_installed("igraph")
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

# --- Names must describe the structure ------------------------------------
#
# Guards the second half of the same class of bug: from 2026-02-12 until
# 2.7.11 the names in .get_man_descriptions() read the D/U letters as
# out/in, so 111D/111U and 120D/120U carried swapped names, and 201/210 were
# named as one-mutual shapes. Each structural word in a name is a claim about
# the pattern matrix; every claim is checked against the visual matrix
# (itself pinned to igraph above), and every name must make at least one.

# m[i, j] == 1 means the edge i -> j
.triad_facts <- function(m) {
  mut <- m * t(m)
  asym <- m * (1L - t(m))
  ids <- seq_len(3)
  triples <- expand.grid(i = ids, j = ids, k = ids)
  triples <- triples[triples$i != triples$j & triples$j != triples$k &
                       triples$i != triples$k, ]
  i <- triples$i; j <- triples$j; k <- triples$k
  list(
    n_edges = sum(m),
    n_mutual = sum(mut) / 2,
    n_asym = sum(asym),
    n_in_star = sum(colSums(m) == 2),
    n_out_star = sum(rowSums(m) == 2),
    cycle = any(m[cbind(i, j)] & m[cbind(j, k)] & m[cbind(k, i)]),
    chain = any(m[cbind(i, j)] & m[cbind(j, k)] &
                  !m[cbind(i, k)] & !m[cbind(k, i)]) && sum(m) == 2,
    feed_forward = any(asym[cbind(i, j)] & asym[cbind(j, k)] &
                         asym[cbind(i, k)]),
    onto_mutual = any(asym[cbind(k, i)] & asym[cbind(k, j)] &
                        mut[cbind(i, j)]),
    from_mutual = any(asym[cbind(i, k)] & asym[cbind(j, k)] &
                        mut[cbind(i, j)])
  )
}

# Vocabulary -> structural claim. Regexes are matched case-insensitively.
.name_rules <- list(
  "^empty$"       = \(f) f$n_edges == 0,
  "single edge"   = \(f) f$n_asym == 1 && f$n_mutual == 0,
  "mutual pair"   = \(f) f$n_mutual == 1 && f$n_asym == 0,
  "\\bmutual\\b"  = \(f) f$n_mutual == 1,
  "two mutuals"   = \(f) f$n_mutual == 2 && f$n_asym == 0,
  "near-clique"   = \(f) f$n_mutual == 2 && f$n_asym == 1,
  "^clique$"      = \(f) f$n_mutual == 3,
  "in-star"       = \(f) f$n_in_star >= 1,
  "out-star"      = \(f) f$n_out_star >= 1,
  "two in-stars"  = \(f) f$n_in_star == 2,
  "two out-stars" = \(f) f$n_out_star == 2,
  "chain"         = \(f) f$chain,
  "feed-forward"  = \(f) f$feed_forward && f$n_mutual == 0,
  "cycle"         = \(f) f$cycle,
  "onto mutual"   = \(f) f$onto_mutual,
  "from mutual"   = \(f) f$from_mutual
)

# One row per (type, rule the name invokes): did the structure honour it?
.check_triad_names <- function(names_vec, patterns) {
  rows <- lapply(names(names_vec), \(type) {
    hits <- names(.name_rules)[vapply(names(.name_rules), \(rx)
      grepl(rx, names_vec[[type]], ignore.case = TRUE), logical(1))]
    facts <- .triad_facts(patterns[[type]])
    data.frame(
      type = rep(type, max(1L, length(hits))),
      name = names_vec[[type]],
      rule = if (length(hits)) hits else NA_character_,
      holds = if (length(hits)) {
        vapply(hits, \(rx) isTRUE(.name_rules[[rx]](facts)), logical(1))
      } else FALSE,
      row.names = NULL
    )
  })
  do.call(rbind, rows)
}

test_that("every MAN name makes a structural claim that its triad satisfies", {
  desc <- cograph:::.get_man_descriptions()
  patterns <- cograph:::.get_triad_patterns_visual()
  expect_setequal(names(desc), .census_names)
  checks <- .check_triad_names(desc, patterns)
  expect_false(anyNA(checks$rule), info = "a name uses no structural word")
  expect_true(all(checks$holds),
              info = paste(with(checks[!checks$holds, ],
                                sprintf("%s '%s' fails rule '%s'",
                                        type, name, rule)),
                           collapse = "; "))
})

test_that("MAN names are unique", {
  desc <- cograph:::.get_man_descriptions()
  expect_false(anyDuplicated(desc) > 0)
})

test_that("the name check rejects the pre-2.7.11 swapped names", {
  old <- c("111D" = "Out-star + mutual", "111U" = "In-star + mutual",
           "201" = "Mutual + in-star", "120D" = "Two out-stars",
           "120U" = "Two in-stars", "120C" = "Mixed regulated",
           "210" = "Mutual + feed-forward")
  checks <- .check_triad_names(old, cograph:::.get_triad_patterns_visual())
  failed <- unique(checks$type[is.na(checks$rule) | !checks$holds])
  expect_setequal(failed, names(old))
})
