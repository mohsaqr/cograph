# Monte Carlo equivalence suite for the motif subsystem.
#
# Generates COGRAPH_MC_DATASETS (default 1000) deterministic random datasets
# spanning 4 input categories (directed, weighted-directed, undirected,
# multi-actor edge list), 18 topologies (Erdos-Renyi at three densities, gnm,
# scale-free, small-world, ring, in/out stars, complete, tournament, DAG,
# bipartite blocks, disconnected components, isolates, reciprocity-heavy,
# empty), and 3-60 nodes, and validates every applicable motif verb against
# an independent reference:
#   directed censuses  -> igraph::triad_census (joined by MAN name)
#   undirected census  -> brute-force triple enumeration
#   edge-list census   -> per-actor igraph census, summed
#   instance mode      -> census totals on identical data
#   configuration null -> exact degree-sequence preservation
#
# Generators and checks live in helper-motif-equivalence.R. The checks are
# canary-verified: shadowing any verb with a lying wrapper is detected.

skip_on_cran()
skip_coverage_tests()

.mc_n_datasets <- as.integer(Sys.getenv("COGRAPH_MC_DATASETS", "1000"))

test_that(sprintf(
  "motif subsystem is equivalent to igraph/brute-force on %d datasets",
  .mc_n_datasets
), {
  all_fails <- character(0)
  categories <- character(.mc_n_datasets)
  shapes <- character(.mc_n_datasets)

  for (i in seq_len(.mc_n_datasets)) {
    ds <- make_motif_mc_dataset(i)
    categories[i] <- ds$category
    shapes[i] <- ds$shape
    fails <- tryCatch(
      check_motif_mc_dataset(ds),
      error = function(e) sprintf("[%s/%s seed=%d] ERROR: %s",
                                  ds$category, ds$shape, ds$seed,
                                  conditionMessage(e))
    )
    all_fails <- c(all_fails, fails)
  }

  # every category and every topology must actually have been exercised
  expect_setequal(unique(categories),
                  c("directed", "weighted", "undirected", "edgelist"))
  expect_gte(length(unique(shapes)), 15)

  if (length(all_fails)) {
    fail(paste0(length(all_fails), " equivalence failure(s); first 10:\n",
                paste(utils::head(all_fails, 10), collapse = "\n")))
  } else {
    succeed()
  }
})
