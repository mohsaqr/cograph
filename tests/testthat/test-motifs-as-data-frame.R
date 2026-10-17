test_that("as.data.frame() returns the census table", {
  census <- motifs(regulation_net, significance = FALSE)
  df <- as.data.frame(census)
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("type", "count"))
  expect_identical(rownames(df), as.character(seq_len(nrow(df))))
  expect_identical(df$type, census$results$type)
})

test_that("as.data.frame(what = 'types') matches the type summary", {
  census <- motifs(regulation_net, significance = FALSE)
  types <- as.data.frame(census, what = "types")
  expect_identical(names(types), c("type", "count"))
  expect_identical(types$type, names(census$type_summary))
  expect_identical(types$count, as.integer(census$type_summary))
  # A census counts each triad once, so both tables agree on the totals.
  expect_identical(sum(types$count), sum(as.data.frame(census)$count))
})

test_that("as.data.frame() carries the significance columns when tested", {
  skip_if_not_installed("igraph")
  census <- motifs(regulation_net, significance = TRUE, n_perm = 20, seed = 1)
  expect_identical(names(as.data.frame(census)),
                   c("type", "count", "expected", "z", "p", "p_adj", "sig"))
})

test_that("as.data.frame() returns the node triples of subgraphs()", {
  inst <- subgraphs(student_interactions, significance = FALSE)
  df <- as.data.frame(inst)
  expect_identical(names(df), c("triad", "node1", "node2", "node3", "type",
                                "observed", "pattern", "pattern_n",
                                "pattern_code"))
  types <- as.data.frame(inst, what = "types")
  # In instance mode the type counts are the number of node triples per type.
  expect_identical(sum(types$count), nrow(df))
})

test_that("as.data.frame() honours row.names and rejects an unknown table", {
  census <- motifs(regulation_net, significance = FALSE)
  labels <- paste0("r", seq_len(nrow(census$results)))
  expect_identical(rownames(as.data.frame(census, row.names = labels)), labels)
  expect_error(as.data.frame(census, what = "nodes"), "should be one of")
})

test_that("as.data.frame(significant = TRUE) keeps exactly the significant rows", {
  skip_if_not_installed("tna")
  inst <- subgraphs(tna::tna(coding), n_perm = 50, seed = 1, min_count = 1)
  all_rows <- as.data.frame(inst)
  sig_rows <- as.data.frame(inst, significant = TRUE)
  expect_gt(nrow(sig_rows), 0L)
  expect_lt(nrow(sig_rows), nrow(all_rows))
  expect_true(all(sig_rows$sig))
  # Same rows, same order as filtering the full table by its own flag.
  expected <- all_rows[all_rows$sig %in% TRUE, , drop = FALSE]
  rownames(expected) <- NULL
  expect_identical(sig_rows, expected)
  # The type table is unaffected by the row filter.
  expect_identical(as.data.frame(inst, what = "types", significant = TRUE),
                   as.data.frame(inst, what = "types"))
})

test_that("significant = TRUE needs a result with significance", {
  inst <- subgraphs(student_interactions, significance = FALSE)
  expect_error(as.data.frame(inst, significant = TRUE),
               class = "cograph_no_significance")
  expect_error(plot(inst, significant = TRUE),
               class = "cograph_no_significance")
  expect_error(as.data.frame(inst, significant = NA), "TRUE or FALSE")
})

test_that("plot(significant = TRUE) draws only the significant triads", {
  skip_if_not_installed("tna")
  skip_on_cran()
  inst <- subgraphs(tna::tna(coding), n_perm = 50, seed = 1, min_count = 1)
  drawn <- NULL
  local_mocked_bindings(
    .plot_triad_networks = function(x, n, ...) drawn <<- utils::head(x$results, n)
  )
  plot(inst, significant = TRUE, n = 1000)
  expect_identical(drawn, as.data.frame(inst, significant = TRUE))
})
