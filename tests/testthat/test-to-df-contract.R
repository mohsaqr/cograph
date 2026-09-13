# Two different contracts that used to be one function, and must stay apart.
#
#   to_df() / to_data_frame()  -- the CONVERSION verb. Exactly from/to/weight.
#   as.data.frame(<network>)   -- the ACCESSOR. The edge table whole.
#
# Dropping the igraph round-trip in 2.4.9 let extra edge columns leak into
# to_df(), silently widening it from 3 columns to however many the network
# carried. That broke the reverse dependency lagdynamics, whose integration
# test pins the three-column shape.

make_net <- function() {
  m <- matrix(c(0, 1, 0,
                0, 0, 1,
                1, 0, 0), 3, 3, byrow = TRUE)
  dimnames(m) <- list(LETTERS[1:3], LETTERS[1:3])
  as_cograph(m)
}

test_that("to_df() returns exactly from, to and weight", {
  net <- make_net()
  expect_named(to_df(net), c("from", "to", "weight"))
  expect_named(to_data_frame(net), c("from", "to", "weight"))
  expect_equal(nrow(to_df(net)), 3L)
})

test_that("to_df() stays three columns when the network carries extra columns", {
  net <- make_net()
  e <- get_edges(net)
  e$session <- c("s1", "s2", "s3")
  e$time <- 1:3
  net <- set_edges(net, e)

  expect_named(get_edges(net), c("from", "to", "weight", "session", "time"))
  expect_named(to_df(net), c("from", "to", "weight"))
})

test_that("to_df() stays three columns after mutate_edges() adds one", {
  net <- mutate_edges(make_net(), dup = is_multiple)
  expect_named(to_df(net), c("from", "to", "weight"))
})

test_that("as.data.frame() keeps what to_df() drops", {
  net <- mutate_edges(make_net(), dup = is_multiple)
  df <- as.data.frame(net)
  expect_true(all(c("from", "to", "weight", "dup") %in% names(df)))
  expect_equal(nrow(df), 3L)
})

test_that("an empty network still reports the three columns", {
  empty <- as_cograph(matrix(0, 3, 3))
  expect_named(to_df(empty), c("from", "to", "weight"))
  expect_equal(nrow(to_df(empty)), 0L)
})

test_that("from and to are node labels, not indices", {
  df <- to_df(make_net())
  expect_type(df$from, "character")
  expect_true(all(df$from %in% LETTERS[1:3]))
})
