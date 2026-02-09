test_that("layout_circle produces correct coordinates", {
  adj <- matrix(c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0), nrow = 4)
  net <- CographNetwork$new(adj)
  coords <- layout_circle(net)

  expect_equal(nrow(coords), 4)
  expect_true(all(c("x", "y") %in% names(coords)))
  # Check roughly circular (distances from center similar)
  cx <- mean(coords$x)
  cy <- mean(coords$y)
  dists <- sqrt((coords$x - cx)^2 + (coords$y - cy)^2)
  expect_true(max(dists) - min(dists) < 0.01)
})

test_that("layout_spring produces coordinates", {
  adj <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3)
  net <- CographNetwork$new(adj)
  coords <- layout_spring(net, iterations = 10, seed = 42)

  expect_equal(nrow(coords), 3)
  expect_true(all(c("x", "y") %in% names(coords)))
  # Coordinates should be in [0, 1]
  expect_true(all(coords$x >= 0 & coords$x <= 1))
  expect_true(all(coords$y >= 0 & coords$y <= 1))
})

test_that("layout_groups arranges by group", {
  adj <- matrix(0, 6, 6)
  adj[1, 2] <- adj[2, 1] <- 1
  adj[3, 4] <- adj[4, 3] <- 1
  adj[5, 6] <- adj[6, 5] <- 1
  net <- CographNetwork$new(adj)
  groups <- c(1, 1, 2, 2, 3, 3)
  coords <- layout_groups(net, groups)

  expect_equal(nrow(coords), 6)
  # Nodes in same group should be close to each other
  dist_within_1 <- sqrt((coords$x[1] - coords$x[2])^2 + (coords$y[1] - coords$y[2])^2)
  dist_between <- sqrt((coords$x[1] - coords$x[3])^2 + (coords$y[1] - coords$y[3])^2)
  expect_true(dist_within_1 < dist_between)
})

test_that("CographLayout normalizes coordinates", {
  layout <- CographLayout$new("circle")
  coords <- data.frame(x = c(-10, 0, 10), y = c(-5, 0, 5))
  normalized <- layout$normalize_coords(coords)

  expect_true(all(normalized$x >= 0 & normalized$x <= 1))
  expect_true(all(normalized$y >= 0 & normalized$y <= 1))
})

test_that("registered layouts can be retrieved", {
  expect_true("circle" %in% list_layouts())
  expect_true("spring" %in% list_layouts())
  expect_true("groups" %in% list_layouts())

  circle_fn <- get_layout("circle")
  expect_true(is.function(circle_fn))
})
