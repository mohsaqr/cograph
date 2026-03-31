skip_on_cran()

test_that("plot_bootstrap_forest.tna_bootstrap linear works", {
  skip_if_not_installed("tna")
  library(tna)
  model <- tna(engagement)
  boot <- bootstrap(model, iter = 50)
  p <- plot_bootstrap_forest(boot)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_bootstrap_forest.tna_bootstrap circular works", {
  skip_if_not_installed("tna")
  library(tna)
  model <- tna(engagement)
  boot <- bootstrap(model, iter = 50)
  p <- plot_bootstrap_forest(boot, layout = "circular")
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_bootstrap_forest.tna_bootstrap grouped works", {
  skip_if_not_installed("tna")
  library(tna)
  model <- tna(engagement)
  boot <- bootstrap(model, iter = 50)
  p <- plot_bootstrap_forest(boot, layout = "grouped")
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_bootstrap_forest.net_bootstrap circular works", {
  skip_if_not_installed("Nestimate")
  library(Nestimate)
  net  <- build_network(human_wide, method = "relative")
  boot <- bootstrap_network(net, iter = 50, seed = 1)
  p <- plot_bootstrap_forest(boot, layout = "circular")
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_bootstrap_forest.net_bootstrap grouped works", {
  skip_if_not_installed("Nestimate")
  library(Nestimate)
  net  <- build_network(human_wide, method = "relative")
  boot <- bootstrap_network(net, iter = 50, seed = 1)
  p <- plot_bootstrap_forest(boot, layout = "grouped")
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_bootstrap_forest.net_bootstrap_group works", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("tna")
  library(Nestimate)
  nets  <- build_network(tna::group_regulation_long, method = "relative",
                         actor = "Actor", action = "Action", time = "Time",
                         group = "Achiever")
  boots <- bootstrap_network(nets, iter = 50, seed = 1)
  expect_true(inherits(boots, "net_bootstrap_group"))
  p <- plot_bootstrap_forest(boots)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_edge_diff_forest tile works", {
  skip_if_not_installed("Nestimate")
  library(Nestimate)
  net  <- build_network(srl_strategies, method = "glasso")
  boot <- boot_glasso(net, iter = 100, seed = 1, centrality = c("strength", "expected_influence"))
  p <- plot_edge_diff_forest(boot, layout = "tile")
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_edge_diff_forest linear works", {
  skip_if_not_installed("Nestimate")
  library(Nestimate)
  net  <- build_network(srl_strategies, method = "glasso")
  boot <- boot_glasso(net, iter = 100, seed = 1, centrality = c("strength", "expected_influence"))
  p <- plot_edge_diff_forest(boot, layout = "linear", n_top = 10)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_edge_diff_forest circular works", {
  skip_if_not_installed("Nestimate")
  library(Nestimate)
  net  <- build_network(srl_strategies, method = "glasso")
  boot <- boot_glasso(net, iter = 100, seed = 1, centrality = c("strength", "expected_influence"))
  p <- plot_edge_diff_forest(boot, layout = "circular", nonzero_only = TRUE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_edge_diff_forest chord works", {
  skip_if_not_installed("Nestimate")
  library(Nestimate)
  net  <- build_network(srl_strategies, method = "glasso")
  boot <- boot_glasso(net, iter = 100, seed = 1, centrality = c("strength", "expected_influence"))
  p <- plot_edge_diff_forest(boot, layout = "chord", nonzero_only = TRUE, n_top = 15)
  expect_true(inherits(p, "ggplot"))
})
