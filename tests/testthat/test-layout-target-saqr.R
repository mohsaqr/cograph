test_that("layout_saqr places Start on top and End on bottom", {
  labs <- c("Start", "A", "B", "C", "End")
  adj <- matrix(0, 5, 5, dimnames = list(labs, labs))
  adj["Start", "A"] <- 5; adj["Start", "B"] <- 3; adj["Start", "C"] <- 1
  adj["A", "End"] <- 2; adj["B", "End"] <- 4; adj["C", "End"] <- 1
  net <- CographNetwork$new(adj, directed = TRUE)

  co <- layout_saqr(net)

  expect_s3_class(co, "data.frame")
  expect_equal(nrow(co), 5)
  expect_true(all(c("x", "y") %in% names(co)))
  # Start row is the highest y, End row the lowest.
  expect_equal(which.max(co$y), 1L)        # Start
  expect_equal(which.min(co$y), 5L)        # End
  # Start and End are horizontally centred.
  expect_equal(co$x[1], 0.5)
  expect_equal(co$x[5], 0.5)
})

test_that("layout_saqr ranks middle nodes by outgoing weight from Start", {
  labs <- c("Start", "Hi", "Lo")
  adj <- matrix(0, 3, 3, dimnames = list(labs, labs))
  adj["Start", "Hi"] <- 9; adj["Start", "Lo"] <- 1
  net <- CographNetwork$new(adj, directed = TRUE)

  co <- layout_saqr(net)
  # No End label -> 2 rows total (Start + one middle row of both nodes).
  # The stronger node sorts first; with 2 middle nodes they share one row.
  expect_equal(nrow(co), 3)
  # Hi (stronger) is placed before Lo -> different x positions, both below Start.
  expect_true(co$y[2] < co$y[1])
  expect_true(co$y[3] < co$y[1])
})

test_that("layout_saqr falls back to highest out-degree node as Start", {
  m <- matrix(c(0, 5, 3, 0, 0, 2, 0, 0, 0), 3, 3, byrow = TRUE)
  rownames(m) <- colnames(m) <- c("P", "Q", "R")
  net <- CographNetwork$new(m, directed = TRUE)

  co <- layout_saqr(net)  # no "Start"/"End" labels present
  # P has the largest out-degree -> becomes Start -> highest y.
  expect_equal(which.max(co$y), 1L)
})

test_that("layout_saqr uses 3 middle rows when there are more than 10 middle nodes", {
  n <- 14
  labs <- c("Start", paste0("N", seq_len(n - 2)), "End")
  adj <- matrix(0, n, n, dimnames = list(labs, labs))
  adj["Start", labs[2:(n - 1)]] <- seq_len(n - 2)  # 12 middle nodes
  net <- CographNetwork$new(adj, directed = TRUE)

  co <- layout_saqr(net)
  # Start + 3 middle rows + End = 5 distinct row baselines (jitter perturbs row 1).
  expect_equal(nrow(co), n)
  expect_gte(length(unique(round(co$y, 2))), 5)
})

test_that("layout_saqr jitter widens the spread of the first middle row", {
  labs <- c("Start", "A", "B", "C", "D", "End")
  adj <- matrix(0, 6, 6, dimnames = list(labs, labs))
  adj["Start", c("A", "B", "C", "D")] <- c(4, 3, 2, 1)
  net <- CographNetwork$new(adj, directed = TRUE)

  co_none <- layout_saqr(net, jitter = 0)
  co_big  <- layout_saqr(net, jitter = 0.5)
  # First middle row = nodes 2,3 (cut = ceil(4/2) = 2). With jitter their y's split.
  expect_equal(co_none$y[2], co_none$y[3])
  expect_false(isTRUE(all.equal(co_big$y[2], co_big$y[3])))
})

test_that("layout_target orders nodes by BFS distance from the focal node", {
  m <- matrix(c(0, 1, 1, 1, 0, 0,
                1, 0, 0, 0, 1, 0,
                1, 0, 0, 0, 0, 1,
                1, 0, 0, 0, 0, 0,
                0, 1, 0, 0, 0, 0,
                0, 0, 1, 0, 0, 0), 6, 6, byrow = TRUE)
  rownames(m) <- colnames(m) <- LETTERS[1:6]
  net <- CographNetwork$new(m)

  co <- layout_target(net, target = "A")
  # Horizontal: focal at x = 0, level-1 neighbours at x = 1, level-2 at x = 2.
  expect_equal(co$x[1], 0)              # A focal
  expect_equal(unique(co$x[c(2, 3, 4)]), 1)  # B, C, D direct neighbours
  expect_equal(unique(co$x[c(5, 6)]), 2)     # E, F two hops away
})

test_that("layout_target defaults the focal node to highest degree", {
  m <- matrix(c(0, 1, 1, 1,
                1, 0, 0, 0,
                1, 0, 0, 0,
                1, 0, 0, 0), 4, 4, byrow = TRUE)
  net <- CographNetwork$new(m)
  co <- layout_target(net)  # no 'target' -> node 1 (degree 3) is focal
  expect_equal(co$x[1], 0)
})

test_that("layout_target vertical orientation flips axes", {
  m <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
  net <- CographNetwork$new(m)
  ch <- layout_target(net, target = 1, horizontal = TRUE)
  cv <- layout_target(net, target = 1, horizontal = FALSE)
  # Focal sits at x = 0 horizontally, but at the top (max y) vertically.
  expect_equal(ch$x[1], 0)
  expect_equal(which.max(cv$y), 1L)
})

test_that("layout_target handles disconnected graphs without erroring", {
  md <- matrix(0, 4, 4)
  md[1, 2] <- md[2, 1] <- 1  # nodes 3 and 4 isolated
  net <- CographNetwork$new(md)
  co <- layout_target(net, target = 1)
  expect_equal(nrow(co), 4)
  # Isolated nodes are pushed to the trailing level (largest x).
  expect_equal(co$x[3], max(co$x))
  expect_equal(co$x[4], max(co$x))
})

test_that("layout_target errors on an unknown focal label", {
  m <- matrix(c(0, 1, 1, 0), 2, 2)
  rownames(m) <- colnames(m) <- c("A", "B")
  net <- CographNetwork$new(m)
  expect_error(layout_target(net, target = "Z"), "not found")
})

test_that("both layouts handle single-node networks", {
  one <- CographNetwork$new(matrix(0, 1, 1))
  expect_equal(layout_saqr(one), data.frame(x = 0.5, y = 0.5))
  expect_equal(layout_target(one), data.frame(x = 0.5, y = 0.5))
})

test_that("target and saqr are reachable through splot() and the registry", {
  labs <- c("Start", "A", "B", "End")
  adj <- matrix(0, 4, 4, dimnames = list(labs, labs))
  adj["Start", "A"] <- 3; adj["Start", "B"] <- 1; adj["A", "End"] <- 2; adj["B", "End"] <- 1
  expect_false(is.null(get_layout("saqr")))
  expect_false(is.null(get_layout("target")))
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  expect_no_error(splot(adj, layout = "saqr", start = "Start", end = "End", directed = TRUE))
  expect_no_error(splot(adj, layout = "target", target = "Start", directed = TRUE))
  grDevices::dev.off()
  unlink(tmp)
})
