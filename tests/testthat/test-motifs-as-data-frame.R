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
  census <- motifs(regulation_net, significance = TRUE, n_perm = 20, seed = 1)
  expect_identical(names(as.data.frame(census)),
                   c("type", "count", "expected", "z", "p", "sig"))
})

test_that("as.data.frame() returns the node triples of subgraphs()", {
  inst <- subgraphs(student_interactions, significance = FALSE)
  df <- as.data.frame(inst)
  expect_identical(names(df), c("triad", "node1", "node2", "node3", "type", "observed"))
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
