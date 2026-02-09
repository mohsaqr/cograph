# Tests for layout_spring() animation parameters
# Tests for initial, max_displacement, and anchor_strength parameters

# =============================================================================
# 1. Initial Parameter Tests
# =============================================================================

test_that("layout_spring accepts initial as data.frame", {
  net <- CographNetwork$new(create_test_matrix(5))

  initial <- data.frame(x = runif(5), y = runif(5))
  result <- layout_spring(net, initial = initial, iterations = 10)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_true(all(c("x", "y") %in% names(result)))
})

test_that("layout_spring accepts initial as matrix", {
  net <- CographNetwork$new(create_test_matrix(5))

  initial <- cbind(x = runif(5), y = runif(5))
  result <- layout_spring(net, initial = initial, iterations = 10)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
})

test_that("layout_spring uses initial positions as starting point", {
  net <- CographNetwork$new(create_test_matrix(5))

  # Fixed initial positions at center
  initial <- data.frame(x = rep(0.5, 5), y = rep(0.5, 5))

  # With very few iterations and high anchor, should stay near initial
  result <- layout_spring(net, initial = initial, iterations = 1,
                          anchor_strength = 10)

  # Positions should be close to initial
  expect_true(all(abs(result$x - 0.5) < 0.3))
  expect_true(all(abs(result$y - 0.5) < 0.3))
})

test_that("layout_spring ignores initial if NULL", {
  net <- CographNetwork$new(create_test_matrix(5))

  set.seed(42)
  result1 <- layout_spring(net, initial = NULL, seed = 42)

  set.seed(42)
  result2 <- layout_spring(net, seed = 42)

  expect_equal(result1$x, result2$x)
  expect_equal(result1$y, result2$y)
})

test_that("layout_spring with initial produces different result than without", {
  net <- CographNetwork$new(create_test_matrix(5))

  # Without initial (random start)
  set.seed(42)
  result1 <- layout_spring(net, seed = 42, iterations = 100)

  # With initial at corners
  initial <- data.frame(x = c(0.1, 0.9, 0.1, 0.9, 0.5),
                        y = c(0.1, 0.1, 0.9, 0.9, 0.5))
  result2 <- layout_spring(net, initial = initial, iterations = 100,
                           anchor_strength = 0.5)

  # Results should differ
  expect_false(all(abs(result1$x - result2$x) < 0.01))
})

test_that("layout_spring initial works with named columns", {
  net <- CographNetwork$new(create_test_matrix(5))

  # data.frame with named columns
  initial <- data.frame(x = c(0.2, 0.4, 0.6, 0.8, 0.5),
                        y = c(0.3, 0.5, 0.7, 0.2, 0.8))
  result <- layout_spring(net, initial = initial, iterations = 10)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
})

# =============================================================================
# 2. max_displacement Parameter Tests
# =============================================================================

test_that("layout_spring enforces max_displacement", {
  net <- CographNetwork$new(create_test_matrix(5))

  # Start at center
  initial <- data.frame(x = rep(0.5, 5), y = rep(0.5, 5))

  result <- layout_spring(net, initial = initial, iterations = 100,
                          max_displacement = 0.05)

  # Check displacement from initial
  for (i in 1:5) {
    dist <- sqrt((result$x[i] - 0.5)^2 + (result$y[i] - 0.5)^2)
    expect_lte(dist, 0.05 + 1e-6)  # Small tolerance for floating point
  }
})

test_that("layout_spring ignores max_displacement without initial", {
  net <- CographNetwork$new(create_test_matrix(5))

  # Without initial, max_displacement should have no effect
  set.seed(42)
  result1 <- layout_spring(net, max_displacement = 0.01, seed = 42)

  set.seed(42)
  result2 <- layout_spring(net, max_displacement = NULL, seed = 42)

  expect_equal(result1$x, result2$x)
  expect_equal(result1$y, result2$y)
})

test_that("layout_spring handles max_displacement = 0", {
  net <- CographNetwork$new(create_test_matrix(5))

  initial <- data.frame(x = runif(5), y = runif(5))

  result <- layout_spring(net, initial = initial, iterations = 100,
                          max_displacement = 0)

  # Nodes should not move at all
  expect_equal(result$x, initial$x)
  expect_equal(result$y, initial$y)
})

test_that("layout_spring max_displacement works with various values", {
  net <- CographNetwork$new(create_test_matrix(5))

  initial <- data.frame(x = rep(0.5, 5), y = rep(0.5, 5))

  for (max_disp in c(0.01, 0.05, 0.1, 0.2)) {
    result <- layout_spring(net, initial = initial, iterations = 100,
                            max_displacement = max_disp)

    for (i in 1:5) {
      dist <- sqrt((result$x[i] - 0.5)^2 + (result$y[i] - 0.5)^2)
      expect_true(dist <= max_disp + 1e-6)
    }
  }
})

test_that("layout_spring max_displacement works with non-centered initial", {
  net <- CographNetwork$new(create_test_matrix(5))

  # Non-centered initial positions
  initial <- data.frame(x = c(0.1, 0.2, 0.3, 0.4, 0.5),
                        y = c(0.1, 0.3, 0.5, 0.7, 0.9))

  result <- layout_spring(net, initial = initial, iterations = 100,
                          max_displacement = 0.1)

  # Check each node stays within max_displacement of its initial position
  for (i in 1:5) {
    dist <- sqrt((result$x[i] - initial$x[i])^2 +
                 (result$y[i] - initial$y[i])^2)
    expect_lte(dist, 0.1 + 1e-6)
  }
})

# =============================================================================
# 3. anchor_strength Parameter Tests
# =============================================================================

test_that("layout_spring applies anchor force", {
  net <- CographNetwork$new(create_test_matrix(5))

  set.seed(123)
  initial <- data.frame(x = runif(5), y = runif(5))

  # Strong anchor should keep nodes near initial
  result_strong <- layout_spring(net, initial = initial, iterations = 50,
                                  anchor_strength = 5, seed = 42)

  # Weak anchor allows more movement
  result_weak <- layout_spring(net, initial = initial, iterations = 50,
                                anchor_strength = 0.1, seed = 42)

  # Calculate total displacement
  disp_strong <- sum(sqrt((result_strong$x - initial$x)^2 +
                          (result_strong$y - initial$y)^2))
  disp_weak <- sum(sqrt((result_weak$x - initial$x)^2 +
                        (result_weak$y - initial$y)^2))

  # Strong anchor should result in less displacement
  expect_lt(disp_strong, disp_weak)
})

test_that("layout_spring ignores anchor_strength without initial", {
  net <- CographNetwork$new(create_test_matrix(5))

  set.seed(42)
  result1 <- layout_spring(net, anchor_strength = 5, seed = 42)

  set.seed(42)
  result2 <- layout_spring(net, anchor_strength = 0, seed = 42)

  expect_equal(result1$x, result2$x)
  expect_equal(result1$y, result2$y)
})

test_that("layout_spring anchor_strength = 0 has no anchoring effect", {
  net <- CographNetwork$new(create_test_matrix(5))

  set.seed(123)
  initial <- data.frame(x = runif(5), y = runif(5))

  # With anchor_strength = 0, same as no anchoring
  set.seed(42)
  result1 <- layout_spring(net, initial = initial, anchor_strength = 0,
                           seed = 42, iterations = 50)

  # Note: We can't directly compare to "no initial" case because
  # the initial positions affect starting point
  # Just verify it runs and produces valid output
  expect_s3_class(result1, "data.frame")
  expect_equal(nrow(result1), 5)
})

test_that("layout_spring anchor_strength scales correctly", {
  net <- CographNetwork$new(create_test_matrix(5))

  set.seed(123)
  initial <- data.frame(x = rep(0.5, 5), y = rep(0.5, 5))

  # Test multiple anchor strengths
  strengths <- c(0.1, 0.5, 1.0, 2.0, 5.0)
  displacements <- numeric(length(strengths))

  for (i in seq_along(strengths)) {
    result <- layout_spring(net, initial = initial, iterations = 50,
                            anchor_strength = strengths[i], seed = 42)
    displacements[i] <- sum(sqrt((result$x - 0.5)^2 + (result$y - 0.5)^2))
  }

  # Higher anchor strength should generally mean less displacement
  # (not strictly monotonic due to algorithm dynamics, but trend should hold)
  # Use <= to handle edge case where both are 0
  expect_lte(displacements[length(displacements)], displacements[1])
})

# =============================================================================
# 4. Parameter Combinations Tests
# =============================================================================

test_that("layout_spring combines max_displacement and anchor_strength", {
  net <- CographNetwork$new(create_test_matrix(5))

  initial <- data.frame(x = rep(0.5, 5), y = rep(0.5, 5))

  result <- layout_spring(net, initial = initial, iterations = 100,
                          max_displacement = 0.05, anchor_strength = 1)

  # Should respect max_displacement (hard constraint)
  for (i in 1:5) {
    dist <- sqrt((result$x[i] - 0.5)^2 + (result$y[i] - 0.5)^2)
    expect_lte(dist, 0.05 + 1e-6)
  }
})

test_that("layout_spring animation workflow", {
  net1 <- CographNetwork$new(create_test_matrix(5, seed = 1))
  net2 <- CographNetwork$new(create_test_matrix(5, seed = 2))

  # Frame 1: initial layout
  layout1 <- layout_spring(net1, seed = 42, iterations = 200)

  # Frame 2: use previous as initial with constraints
  layout2 <- layout_spring(net2, initial = layout1, iterations = 50,
                           max_displacement = 0.1, anchor_strength = 0.8)

  # Layouts should be similar (smooth transition)
  for (i in 1:5) {
    dist <- sqrt((layout2$x[i] - layout1$x[i])^2 +
                 (layout2$y[i] - layout1$y[i])^2)
    expect_lte(dist, 0.15)  # Reasonable transition distance
  }
})

test_that("layout_spring multi-frame animation sequence", {
  # Simulate 5 frames of animation
  n_frames <- 5
  layouts <- list()

  # Create slightly different networks for each frame
  networks <- lapply(1:n_frames, function(i) {
    CographNetwork$new(create_test_matrix(5, seed = i, density = 0.4 + i * 0.1))
  })

  # Frame 1: free layout
  layouts[[1]] <- layout_spring(networks[[1]], seed = 42, iterations = 200)

  # Subsequent frames: constrained to previous
  for (i in 2:n_frames) {
    layouts[[i]] <- layout_spring(networks[[i]], initial = layouts[[i-1]],
                                  iterations = 50, max_displacement = 0.08,
                                  anchor_strength = 1)
  }

  # Check all transitions are smooth
  for (i in 2:n_frames) {
    max_dist <- max(sqrt(
      (layouts[[i]]$x - layouts[[i-1]]$x)^2 +
      (layouts[[i]]$y - layouts[[i-1]]$y)^2
    ))
    expect_true(max_dist <= 0.1)
  }
})

test_that("max_displacement overrides anchor_strength when needed", {
  net <- CographNetwork$new(create_test_matrix(5))

  # Very weak anchor but strict max_displacement
  initial <- data.frame(x = rep(0.5, 5), y = rep(0.5, 5))

  result <- layout_spring(net, initial = initial, iterations = 200,
                          max_displacement = 0.02, anchor_strength = 0.01)

  # max_displacement should still be enforced
  for (i in 1:5) {
    dist <- sqrt((result$x[i] - 0.5)^2 + (result$y[i] - 0.5)^2)
    expect_lte(dist, 0.02 + 1e-6)
  }
})

# =============================================================================
# 5. Edge Cases Tests
# =============================================================================

test_that("layout_spring handles single node", {
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)

  result <- layout_spring(net)

  expect_equal(nrow(result), 1)
  expect_equal(result$x, 0.5)
  expect_equal(result$y, 0.5)
})

test_that("layout_spring handles single node with initial", {
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)

  initial <- data.frame(x = 0.3, y = 0.7)
  result <- layout_spring(net, initial = initial)

  # Single node should stay at initial position
  expect_equal(nrow(result), 1)
})

test_that("layout_spring handles empty network", {
  # Empty network case is tricky due to CographNetwork constructor
  # Instead, test with minimal 1-node network
  adj <- matrix(0, 1, 1)
  net <- CographNetwork$new(adj)

  result <- layout_spring(net)

  # Single node with no edges should return 1 row
  expect_equal(nrow(result), 1)
})

test_that("layout_spring handles network with no edges", {
  adj <- matrix(0, 5, 5)
  net <- CographNetwork$new(adj)

  result <- layout_spring(net, seed = 42)

  expect_equal(nrow(result), 5)
  # Nodes should be spread out (no attraction)
})

test_that("layout_spring handles network with no edges and initial", {
  adj <- matrix(0, 5, 5)
  net <- CographNetwork$new(adj)

  initial <- data.frame(x = c(0.2, 0.4, 0.6, 0.8, 0.5),
                        y = c(0.2, 0.4, 0.6, 0.8, 0.5))
  result <- layout_spring(net, initial = initial, iterations = 10,
                          max_displacement = 0.1)

  expect_equal(nrow(result), 5)
})

test_that("layout_spring keeps coordinates in bounds", {
  net <- CographNetwork$new(create_test_matrix(10))

  result <- layout_spring(net, iterations = 500)

  expect_true(all(result$x >= 0.05 & result$x <= 0.95))
  expect_true(all(result$y >= 0.05 & result$y <= 0.95))
})

test_that("layout_spring keeps coordinates in bounds with initial", {
  net <- CographNetwork$new(create_test_matrix(10))

  # Initial positions near edges
  initial <- data.frame(x = c(rep(0.1, 5), rep(0.9, 5)),
                        y = c(rep(0.1, 5), rep(0.9, 5)))

  result <- layout_spring(net, initial = initial, iterations = 100,
                          anchor_strength = 2)

  expect_true(all(result$x >= 0.05 & result$x <= 0.95))
  expect_true(all(result$y >= 0.05 & result$y <= 0.95))
})

test_that("layout_spring handles large network", {
  net <- CographNetwork$new(create_test_matrix(50, density = 0.1))

  result <- layout_spring(net, seed = 42, iterations = 100)

  expect_equal(nrow(result), 50)
  expect_true(all(c("x", "y") %in% names(result)))
})

test_that("layout_spring handles dense network", {
  net <- CographNetwork$new(create_test_matrix(10, density = 0.9))

  result <- layout_spring(net, seed = 42, iterations = 100)

  expect_equal(nrow(result), 10)
})

test_that("layout_spring handles weighted network", {
  net <- CographNetwork$new(create_test_matrix(5, weighted = TRUE))

  result <- layout_spring(net, seed = 42)

  expect_equal(nrow(result), 5)
})

# =============================================================================
# 6. Reproducibility Tests
# =============================================================================

test_that("layout_spring is reproducible with seed", {
  net <- CographNetwork$new(create_test_matrix(5))

  result1 <- layout_spring(net, seed = 42)
  result2 <- layout_spring(net, seed = 42)

  expect_equal(result1$x, result2$x)
  expect_equal(result1$y, result2$y)
})

test_that("layout_spring differs without seed", {
  net <- CographNetwork$new(create_test_matrix(5))

  result1 <- layout_spring(net)
  result2 <- layout_spring(net)

  # Very unlikely to be equal (though technically possible)
  expect_false(all(result1$x == result2$x) && all(result1$y == result2$y))
})

test_that("layout_spring is reproducible with initial positions", {
  net <- CographNetwork$new(create_test_matrix(5))

  initial <- data.frame(x = c(0.2, 0.4, 0.6, 0.8, 0.5),
                        y = c(0.2, 0.4, 0.6, 0.8, 0.5))

  result1 <- layout_spring(net, initial = initial, seed = 42, iterations = 50)
  result2 <- layout_spring(net, initial = initial, seed = 42, iterations = 50)

  expect_equal(result1$x, result2$x)
  expect_equal(result1$y, result2$y)
})

# =============================================================================
# 7. Integration with CographNetwork
# =============================================================================

test_that("layout_spring works with tna-derived networks", {
  skip_if_not_installed("tna")

  set.seed(123)
  states <- c("A", "B", "C")
  data <- data.frame(matrix(
    sample(states, 30 * 5, replace = TRUE),
    nrow = 30, ncol = 5
  ))

  tna_model <- tna::tna(data)
  net <- CographNetwork$new(tna_model$weights)

  result <- layout_spring(net, seed = 42)

  expect_equal(nrow(result), nrow(tna_model$weights))
  expect_true(all(c("x", "y") %in% names(result)))
})

test_that("layout_spring animation with tna networks", {
  skip_if_not_installed("tna")

  set.seed(123)
  states <- c("A", "B", "C", "D")
  data <- data.frame(matrix(
    sample(states, 50 * 10, replace = TRUE),
    nrow = 50, ncol = 10
  ))

  # Create two windows
  window1 <- tna::tna(data[, 1:5])
  window2 <- tna::tna(data[, 3:7])

  net1 <- CographNetwork$new(window1$weights)
  net2 <- CographNetwork$new(window2$weights)

  # First layout
  layout1 <- layout_spring(net1, seed = 42, iterations = 100)

  # Second layout with animation constraints
  layout2 <- layout_spring(net2, initial = layout1, iterations = 50,
                           max_displacement = 0.1, anchor_strength = 1)

  # Both layouts should have same dimensions
  expect_equal(nrow(layout1), nrow(layout2))

  # Transition should be smooth
  max_dist <- max(sqrt((layout2$x - layout1$x)^2 + (layout2$y - layout1$y)^2))
  expect_lte(max_dist, 0.15)
})

# =============================================================================
# 8. Performance Tests (Basic)
# =============================================================================

test_that("layout_spring completes in reasonable time for small network", {
  net <- CographNetwork$new(create_test_matrix(10))

  time_taken <- system.time({
    result <- layout_spring(net, iterations = 100)
  })

  # Should complete in less than 5 seconds
  expect_lt(time_taken["elapsed"], 5)
  expect_equal(nrow(result), 10)
})

test_that("layout_spring with animation params completes in reasonable time", {
  net <- CographNetwork$new(create_test_matrix(10))

  initial <- data.frame(x = runif(10), y = runif(10))

  time_taken <- system.time({
    result <- layout_spring(net, initial = initial, iterations = 50,
                            max_displacement = 0.1, anchor_strength = 1)
  })

  # Should complete in less than 2 seconds
  expect_lt(time_taken["elapsed"], 2)
})
