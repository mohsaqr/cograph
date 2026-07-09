skip_on_cran()

# Forest-plot fixtures are expensive to build and carry no per-test state: the
# tests below vary only the `layout` argument. `boot_glasso()` on
# `srl_strategies` costs ~14 s of CPU, so rebuilding it once per layout spent
# four times that on an object each test uses read-only. Build each fixture at
# most once per file instead.
#
# `cs_iter = 1` is the documented minimum (`boot_glasso()` validates
# `cs_iter >= 1`; there is no way to switch case-dropping stability off). The
# forest plots read only `$boot_edges`, `$edge_diff_p`, `$edge_ci` and `$alpha`,
# never the case-dropping output, so one resample is all we need to buy.
fixture <- local({
  cache <- new.env(parent = emptyenv())
  function(name, build) {
    if (is.null(cache[[name]])) cache[[name]] <- build()
    cache[[name]]
  }
})

tna_boot_fixture <- function() {
  fixture("tna_boot", function() {
    tna::bootstrap(tna::tna(tna::engagement), iter = 50)
  })
}

net_boot_fixture <- function() {
  fixture("net_boot", function() {
    net <- Nestimate::build_network(
      as.data.frame(Nestimate::trajectories),
      method = "relative"
    )
    Nestimate::bootstrap_network(net, iter = 50, seed = 1)
  })
}

net_boot_group_fixture <- function() {
  fixture("net_boot_group", function() {
    nets <- Nestimate::build_network(
      tna::group_regulation_long,
      method = "relative",
      actor = "Actor", action = "Action", time = "Time", group = "Achiever"
    )
    Nestimate::bootstrap_network(nets, iter = 50, seed = 1)
  })
}

boot_glasso_fixture <- function() {
  fixture("boot_glasso", function() {
    net <- Nestimate::build_network(Nestimate::srl_strategies, method = "glasso")
    Nestimate::boot_glasso(
      net,
      iter = 50, cs_iter = 1, seed = 1,
      centrality = c("strength", "expected_influence")
    )
  })
}

# `inherits(p, "ggplot")` alone is vacuous: ggplot2 is lazy, so a plot that
# errors on render still passes it. Build the plot to actually exercise the
# layout code.
expect_renders <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
}

test_that("plot_bootstrap_forest.tna_bootstrap linear works", {
  skip_if_not_installed("tna")
  expect_renders(plot_bootstrap_forest(tna_boot_fixture()))
})

test_that("plot_bootstrap_forest.tna_bootstrap circular works", {
  skip_if_not_installed("tna")
  expect_renders(plot_bootstrap_forest(tna_boot_fixture(), layout = "circular"))
})

test_that("plot_bootstrap_forest.tna_bootstrap grouped works", {
  skip_if_not_installed("tna")
  expect_renders(plot_bootstrap_forest(tna_boot_fixture(), layout = "grouped"))
})

test_that("plot_bootstrap_forest.net_bootstrap circular works", {
  skip_if_not_installed("Nestimate")
  expect_renders(plot_bootstrap_forest(net_boot_fixture(), layout = "circular"))
})

test_that("plot_bootstrap_forest.net_bootstrap grouped works", {
  skip_if_not_installed("Nestimate")
  expect_renders(plot_bootstrap_forest(net_boot_fixture(), layout = "grouped"))
})

test_that("plot_bootstrap_forest.net_bootstrap_group works", {
  skip_if_not_installed("Nestimate")
  skip_if_not_installed("tna")
  boots <- net_boot_group_fixture()
  expect_s3_class(boots, "net_bootstrap_group")
  expect_renders(plot_bootstrap_forest(boots))
})

test_that("plot_edge_diff_forest tile works", {
  skip_if_not_installed("Nestimate")
  expect_renders(plot_edge_diff_forest(boot_glasso_fixture(), layout = "tile"))
})

test_that("plot_edge_diff_forest linear works", {
  skip_if_not_installed("Nestimate")
  expect_renders(
    plot_edge_diff_forest(boot_glasso_fixture(), layout = "linear", n_top = 10)
  )
})

test_that("plot_edge_diff_forest circular works", {
  skip_if_not_installed("Nestimate")
  expect_renders(
    plot_edge_diff_forest(
      boot_glasso_fixture(), layout = "circular", nonzero_only = TRUE
    )
  )
})

test_that("plot_edge_diff_forest chord works", {
  skip_if_not_installed("Nestimate")
  # Named node colours used to leak into data.frame() row names, warning
  # "row names were found from a short variable" once per node arc.
  expect_no_warning(
    p <- plot_edge_diff_forest(
      boot_glasso_fixture(), layout = "chord", nonzero_only = TRUE, n_top = 15
    )
  )
  expect_renders(p)
})

test_that("plot_edge_diff_forest layouts are not interchangeable", {
  skip_if_not_installed("Nestimate")
  boot <- boot_glasso_fixture()
  geoms <- function(p) {
    sort(unique(vapply(p$layers, function(l) class(l$geom)[1], character(1))))
  }
  tile     <- plot_edge_diff_forest(boot, layout = "tile")
  linear   <- plot_edge_diff_forest(boot, layout = "linear", n_top = 10)
  circular <- plot_edge_diff_forest(boot, layout = "circular", nonzero_only = TRUE)
  chord    <- plot_edge_diff_forest(boot, layout = "chord", nonzero_only = TRUE)

  # A silent fallback to a different layout would leave these identical.
  signatures <- vapply(
    list(tile, linear, circular, chord),
    function(p) paste(nrow(p$data), paste(geoms(p), collapse = "+")),
    character(1)
  )
  expect_length(unique(signatures), 4L)
  expect_error(plot_edge_diff_forest(boot, layout = "banana"))
})
