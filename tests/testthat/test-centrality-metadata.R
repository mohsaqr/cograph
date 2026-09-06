# ===========================================================================
# Measure metadata, tier semantics and the guarded numerical paths
#
# `list_centralities()` is the published description of what `centrality()`
# does, so these tests hold the two together: every measure listed must be
# computable, every computable measure must be listed, and each documented
# fact (orientation, mode awareness, membership, weights, cost) is checked
# against the behaviour rather than against another copy of the same list.
# ===========================================================================

adj6 <- matrix(0, 6, 6)
adj6[cbind(c(1, 1, 2, 4, 4, 5, 3), c(2, 3, 3, 5, 6, 6, 4))] <- 1
adj6 <- adj6 + t(adj6)
rownames(adj6) <- colnames(adj6) <- LETTERS[1:6]
memb6 <- c(1, 1, 1, 2, 2, 2)

# ===========================================================================
# The published measure table
# ===========================================================================

test_that("list_centralities returns a tidy table of every measure", {
  tab <- list_centralities()
  expect_s3_class(tab, "data.frame")
  expect_named(tab, c("measure", "orientation", "mode_aware",
                      "needs_membership", "uses_weights", "costly"))
  expect_false(anyNA(tab))
  expect_equal(anyDuplicated(tab$measure), 0L)
  expect_equal(tab$measure, sort(tab$measure))
  expect_true(all(tab$orientation %in% c("higher", "lower")))
  expect_type(tab$mode_aware, "logical")
})

test_that("list_centralities lists exactly the measures centrality() accepts", {
  listed <- list_centralities()$measure
  # The error path reports the accepted set, so it is an independent witness
  err <- tryCatch(centrality(adj6, measures = "not_a_measure"),
                  error = function(e) conditionMessage(e))
  accepted <- strsplit(sub(".*Available: ", "", err), ", ")[[1]]
  expect_setequal(listed, accepted)
})

test_that("list_centralities filters", {
  low <- list_centralities(orientation = "lower")
  expect_true(all(low$orientation == "lower"))
  expect_true(all(list_centralities(costly = TRUE)$costly))
  expect_false(any(list_centralities(costly = FALSE)$costly))
  expect_true(all(list_centralities(needs_membership = TRUE)$needs_membership))
  expect_error(list_centralities(orientation = "sideways"))
  expect_error(list_centralities(costly = "yes"),
               "`costly` must be TRUE, FALSE or NULL")
})

test_that("the mode_aware flag matches the column names centrality() emits", {
  tab <- list_centralities()
  df <- suppressWarnings(centrality(adj6, measures = c("degree", "betweenness"),
                                    membership = memb6))
  # degree is mode-aware and gets a suffix; betweenness is not and does not
  expect_true(tab$mode_aware[tab$measure == "degree"])
  expect_false(tab$mode_aware[tab$measure == "betweenness"])
  expect_true("degree_all" %in% names(df))
  expect_true("betweenness" %in% names(df))
})

test_that("the needs_membership flag matches which measures warn without one", {
  skip_on_cran()
  needs <- list_centralities(needs_membership = TRUE)$measure
  # A directed graph, since five of them are the directed brokerage roles
  g <- igraph::graph_from_adjacency_matrix(adj6, mode = "directed")
  for (m in needs) {
    expect_warning(centrality(g, measures = m), "membership",
                   info = m)
  }
})

test_that("the uses_weights flag matches whether weights change the result", {
  skip_if_not_installed("igraph")
  skip_on_cran()
  set.seed(11)
  g <- igraph::sample_gnp(24, 0.28)
  while (!igraph::is_connected(g)) g <- igraph::sample_gnp(24, 0.28)
  gw <- g
  igraph::E(gw)$weight <- round(stats::runif(igraph::ecount(g), 1, 9))
  memb <- igraph::membership(igraph::cluster_louvain(g))
  tab <- list_centralities()
  # The directed-only measures return NA on this graph, so they cannot be
  # decided here; they are checked by the equality of the two lists above.
  undecidable <- c("salsa", "leaderrank", "trophic_level", "hubbell",
                   "pairwisedis", "prestige_domain",
                   "prestige_domain_proximity", "brokerage_coordinator",
                   "brokerage_itinerant", "brokerage_representative",
                   "brokerage_gatekeeper", "brokerage_liaison")
  tab <- tab[!tab$measure %in% undecidable, ]
  # epc is a Monte Carlo estimate, so it needs a fixed seed before the two
  # calls can be compared at all.
  value <- function(gr, m) {
    out <- suppressWarnings(suppressMessages(centrality(
      gr, measures = m, membership = memb, epc_seed = 1
    )))
    out[[2]]
  }
  changed <- vapply(tab$measure, function(m) {
    !isTRUE(all.equal(value(g, m), value(gw, m)))
  }, logical(1))
  expect_equal(unname(changed), tab$uses_weights)
})

# ===========================================================================
# Tier semantics: type = "all" holds back the costly measures
# ===========================================================================

test_that("type = all omits the costly measures; include = adds them back", {
  costly <- list_centralities(costly = TRUE)$measure
  # A mode-aware measure arrives as `name_all`, so match on the stem.
  present <- function(df, m) any(names(df) == m | names(df) == paste0(m, "_all"))
  all_df <- suppressWarnings(centrality(adj6, type = "all", membership = memb6))
  expect_false(any(vapply(costly, present, logical(1), df = all_df)))
  with_one <- suppressWarnings(centrality(adj6, type = "all",
                                          include = costly[1],
                                          membership = memb6))
  expect_true(present(with_one, costly[1]))
  expect_equal(ncol(with_one), ncol(all_df) + 1L)
  with_all <- suppressWarnings(centrality(adj6, type = "all",
                                          include = "costly",
                                          membership = memb6))
  expect_true(all(vapply(costly, present, logical(1), df = with_all)))
  expect_equal(ncol(with_all), ncol(all_df) + length(costly))
})

test_that("naming a costly measure in measures = always computes it", {
  for (m in list_centralities(costly = TRUE)$measure) {
    df <- suppressWarnings(centrality(adj6, measures = m, membership = memb6))
    expect_equal(ncol(df), 2L, info = m)
    expect_false(anyNA(df[[2]]), info = m)
  }
})

test_that("the basic and extended tiers are unchanged by the costly rule", {
  expect_equal(ncol(centrality(adj6)), 7L)          # node + 6 basic measures
  costly <- list_centralities(costly = TRUE)$measure
  ext <- suppressWarnings(centrality(adj6, type = "extended"))
  expect_false(any(costly %in% names(ext)))
})

test_that("include = rejects an unknown measure by class", {
  expect_error(centrality(adj6, type = "all", include = "not_a_measure"),
               class = "cograph_unknown_measure")
})

# ===========================================================================
# Guarded numerical paths
# ===========================================================================

test_that("katz warns when the attenuation does not converge", {
  skip_if_not_installed("igraph")
  # A graph whose spectral radius exceeds 1 / 0.1, so the default diverges
  g <- igraph::make_full_graph(12)
  expect_warning(v <- centrality(g, measures = "katz")$katz,
                 class = "cograph_katz_diverged")
  # A valid attenuation is silent, and every Katz score is then at least 1
  expect_silent(w <- centrality(g, measures = "katz",
                                katz_alpha = 0.05)$katz)
  expect_true(all(w >= 1))
  expect_silent(centrality(igraph::make_graph("Zachary"), measures = "katz"))
})

test_that("alpha and power raise a classed error on a singular system", {
  skip_if_not_installed("igraph")
  # P2's adjacency has eigenvalue 1, so (I - A) is exactly singular
  p2 <- igraph::make_graph(c(1, 2), directed = FALSE)
  expect_error(centrality(p2, measures = "alpha"),
               class = "cograph_singular_system")
  expect_error(centrality(p2, measures = "power"),
               class = "cograph_singular_system")
  # An ordinary graph is unaffected
  expect_silent(centrality(igraph::make_graph("Zachary"), measures = "alpha"))
})

# ===========================================================================
# Orientation
# ===========================================================================

test_that("lower-is-central measures put the periphery on top when sorted", {
  # A star: the hub is the obviously central node. Each of these measures
  # must rank it last when the column is sorted the usual way.
  star <- matrix(0, 6, 6)
  star[1, 2:6] <- 1
  star <- star + t(star)
  rownames(star) <- colnames(star) <- LETTERS[1:6]
  for (m in c("eccentricity", "average_distance", "wiener", "constraint")) {
    df <- centrality(star, measures = m)
    hub <- df[[2]][1]
    expect_equal(hub, min(df[[2]], na.rm = TRUE), info = m)
  }
  tab <- list_centralities()
  flagged <- tab$orientation[tab$measure %in%
                               c("eccentricity", "constraint", "heatmap")]
  expect_true(all(flagged == "lower"))
})
