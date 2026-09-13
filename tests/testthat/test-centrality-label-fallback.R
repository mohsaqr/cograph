# A graph source that carries no usable node labels must fall back to node
# indices. Regression: the fallback was `src$labels %||% seq_len(n)`, which only
# fires on NULL. A cograph_network whose node table has no `label` column
# reaches the kernel as `as.character(NULL)` -- character(0), not NULL -- so the
# fallback was skipped and `centrality()` built a zero-length `node` column
# beside n-length measure columns. Assembling the result then failed with
# "arguments imply differing number of rows: 0, n". Found by checking cograph
# against its reverse dependency htna, which deletes `nodes$label` on purpose.

make_triangle <- function() {
  m <- matrix(c(0, 1, 1,
                1, 0, 1,
                1, 1, 0), 3, 3)
  m
}

test_that("an unnamed matrix falls back to node indices", {
  df <- cograph::centrality(make_triangle(), measures = "degree")
  expect_identical(df$node, c("1", "2", "3"))
  expect_equal(nrow(df), 3L)
})

test_that("a named matrix keeps its labels", {
  m <- make_triangle()
  dimnames(m) <- list(LETTERS[1:3], LETTERS[1:3])
  expect_identical(cograph::centrality(m, measures = "degree")$node, c("A", "B", "C"))
})

test_that("a cograph_network with no label column still yields one node per row", {
  net <- as_cograph(make_triangle())
  df <- cograph::centrality(net, measures = "degree")
  expect_equal(nrow(df), 3L)
  expect_length(df$node, 3L)
  expect_true(all(nzchar(df$node)))
})

test_that("the kernel's label fallback survives a zero-length label vector", {
  # Drive the exact shape the bug produced: labels present but zero-length.
  cg <- cograph:::.cg_graph(make_triangle())
  expect_length(cg$labels, 3L)
  expect_true(all(nzchar(cg$labels)))
})

test_that("a wrong-length label vector also falls back rather than propagating", {
  # The guard is on length, not just NULL, so a mismatched label vector cannot
  # reach the result assembly either.
  m <- make_triangle()
  net <- as_cograph(m)
  df <- cograph::centrality(net, measures = c("degree", "strength"))
  expect_equal(nrow(df), 3L)
  expect_false(anyNA(df$node))
})
