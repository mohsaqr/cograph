# as_tna() leaves out a cluster that cannot become a tna model and says so
# with a warning of class cograph_cluster_dropped.

test_that("a one-node cluster is dropped with a classed warning naming it", {
  skip_if_not_installed("tna")
  mat <- matrix(1, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
  diag(mat) <- 0
  cs <- csum(mat, list(G1 = c("A", "B"), G2 = c("C", "D"), Solo = "E"),
             type = "tna")
  expect_warning(res <- as_tna(cs), class = "cograph_cluster_dropped")
  expect_warning(as_tna(cs), "\"Solo\": single-node cluster (\"E\").", fixed = TRUE)
  expect_identical(names(res), c("macro", "G1", "G2"))
})

test_that("a node with no within-cluster tie drops its cluster, via both methods", {
  skip_if_not_installed("tna")
  mat <- matrix(1, 6, 6, dimnames = list(LETTERS[1:6], LETTERS[1:6]))
  diag(mat) <- 0
  mat["C", c("A", "B")] <- 0 # C only leaves its cluster
  clusters <- list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F"))
  cs <- csum(mat, clusters, type = "tna")

  expect_warning(res_cs <- as_tna(cs), class = "cograph_cluster_dropped")
  expect_warning(as_tna(cs), "\"G1\": no outgoing within-cluster transitions from \"C\".", fixed = TRUE)
  expect_identical(names(res_cs), c("macro", "G2"))

  expect_warning(res_mc <- as_tna(as_mcml(cs)),
                 class = "cograph_cluster_dropped")
  expect_identical(names(res_mc), c("macro", "G2"))
  # the dropped cluster is still a node of the macro model
  expect_true("G1" %in% res_mc$macro$labels)
})

test_that("one warning lists every dropped cluster", {
  skip_if_not_installed("tna")
  cs <- csum(regulation_net,
             list(Cognitive = c("Explore", "Plan", "Monitor", "Adapt", "Reflect"),
                  Social = c("Discuss", "Synthesize", "Share"),
                  Evaluative = c("Evaluate", "Create")),
             type = "tna")
  warnings_seen <- character(0)
  res <- withCallingHandlers(
    as_tna(cs),
    cograph_cluster_dropped = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings_seen, 1L)
  expect_match(warnings_seen, "Within-cluster TNA models for 2 clusters were not estimated", fixed = TRUE)
  expect_match(warnings_seen,
               "from \"Discuss\", \"Synthesize\" and \"Share\".",
               fixed = TRUE)
  expect_match(warnings_seen, "\"Evaluative\": no outgoing within-cluster transitions from \"Evaluate\".",
               fixed = TRUE)
  expect_identical(names(res), c("macro", "Cognitive"))
})

test_that("no warning when every cluster has positive within-cluster rows", {
  skip_if_not_installed("tna")
  mat <- matrix(1, 6, 6, dimnames = list(LETTERS[1:6], LETTERS[1:6]))
  diag(mat) <- 0
  cs <- csum(mat, list(G1 = c("A", "B", "C"), G2 = c("D", "E", "F")),
             type = "tna")
  expect_no_warning(res <- as_tna(cs))
  expect_identical(names(res), c("macro", "G1", "G2"))
  expect_s3_class(res$G1, "tna")
})
