test_that("regulation_net has the documented structure", {
  states <- c("Explore", "Plan", "Monitor", "Adapt", "Reflect",
              "Discuss", "Synthesize", "Evaluate", "Create", "Share")
  expect_true(is.matrix(regulation_net) && is.numeric(regulation_net))
  expect_identical(dim(regulation_net), c(10L, 10L))
  expect_identical(dimnames(regulation_net), list(states, states))
  expect_true(all(diag(regulation_net) == 0))
  expect_identical(sum(regulation_net > 0), 30L)
  expect_true(all(regulation_net >= 0))
  expect_equal(range(regulation_net[regulation_net > 0]), c(0.05, 0.49))
})

test_that("regulation_net matches its documented generating recipe", {
  # The help page states how the synthetic network was generated; the recipe
  # must reproduce the shipped data exactly.
  saved_rng <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit(if (!is.null(saved_rng)) assign(".Random.seed", saved_rng, envir = globalenv()),
          add = TRUE)
  set.seed(42)
  states <- dimnames(regulation_net)[[1]]
  rebuilt <- matrix(0, 10, 10, dimnames = list(states, states))
  cells <- sample(which(row(rebuilt) != col(rebuilt)), 30)
  rebuilt[cells] <- round(stats::runif(30, 0.05, 0.5), 2)
  expect_identical(rebuilt, regulation_net)
})
