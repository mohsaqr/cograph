# The test-network corpus verifies itself: every manifest row must be
# reproducible from its matrix by the native descriptors in
# helper-networks.R, and every matrix must survive cograph's own
# conversion round trip. See docs/test-networks-plan.md.

committed_tiers <- c("real_small", "degenerate")

test_that("committed corpus tiers are present and non-empty", {
  for (tier in committed_tiers) {
    # One assertion per committed tier.
    expect_false(is.na(.corpus_path(tier)), info = tier)
  }
  nets <- test_networks(tier = committed_tiers)
  expect_s3_class(nets, "data.frame")
  expect_gt(nrow(nets), 60)
  expect_true(all(vapply(nets$matrix, is.matrix, logical(1))))
})

test_that("manifest rows are reproducible from the stored matrices", {
  skip_if_not_installed("digest")
  nets <- test_networks(tier = committed_tiers)
  recomputed <- do.call(rbind, Map(
    function(name, m, row) {
      .describe_matrix(name, m, row$tier, row$family, row$source, row$license,
                       has_multi = row$has_multi, bipartite = row$bipartite,
                       labels_assigned = row$labels_assigned, seed = row$seed,
                       projection_of = row$projection_of,
                       zero_weight_edges = row$zero_weight_edges,
                       dropped_na_rows = row$dropped_na_rows)
    },
    nets$name, nets$matrix, split(nets, seq_len(nrow(nets)))
  ))
  stored <- nets[, setdiff(names(nets), "matrix")]
  rownames(stored) <- rownames(recomputed) <- NULL
  expect_equal(recomputed, stored[, names(recomputed)])
})

test_that("every committed network round-trips through as_cograph() and to_matrix()", {
  nets <- test_networks(tier = committed_tiers, min_n = 1)
  back <- lapply(nets$matrix, function(m) to_matrix(as_cograph(m)))
  same <- mapply(function(a, b) isTRUE(all.equal(unname(a), unname(b))), nets$matrix, back)
  expect_true(all(same), info = paste("round-trip changed:", paste(nets$name[!same], collapse = ", ")))
})

test_that("the corpus covers the cases the wrangling and kernel plans need", {
  nets <- test_networks(tier = committed_tiers)
  expect_true(any(nets$directed & nets$weighted & nets$n > 50))
  expect_true(any(nets$signed))
  expect_true(any(nets$has_loops & nets$directed))
  expect_true(any(nets$has_multi))
  expect_true(any(nets$bipartite))
  expect_true(any(nets$n_isolates > 0 & nets$n > 1))
  expect_true(any(nets$n_components > 1))
  expect_true(any(!nets$has_dimnames))
  expect_true(any(nets$n == 0))
})

test_that("committed RDS files stay within the size budget", {
  sizes <- vapply(committed_tiers, function(t) file.info(.corpus_path(t))$size, numeric(1))
  expect_lt(sizes[["real_small"]], 300 * 1024)
  expect_lt(sizes[["degenerate"]], 50 * 1024)
})

test_that("test_network() errors by class on an unknown name", {
  expect_error(test_network("no_such_network_xyz"), class = "cograph_test_network_missing")
  expect_identical(test_network("kite"), test_networks(tier = "real_small")$matrix[[which(test_networks(tier = "real_small")$name == "kite")]])
})

test_that("vertex-transitive graphs give constant degree, closeness and betweenness", {
  for (nm in c("petersen", "dodecahedral", "ring_10", "complete_6")) {
    # One network per iteration; the loop is over four names.
    m <- test_network(nm)
    cen <- centrality(m, measures = c("degree", "closeness", "betweenness"))
    scores <- cen[, setdiff(names(cen), "node"), drop = FALSE]
    expect_identical(ncol(scores), 3L)
    expect_true(all(vapply(scores, function(v) diff(range(v)) < 1e-10, logical(1))),
                info = nm)
  }
})

# Regression guard for the manifest hash. The corpus manifest is a committed
# fixture, so .hash_matrix() must depend on the matrix ALONE. It previously
# hashed serialize(m, NULL, version = 3), whose header carries the writing R
# version, so every CI runner recomputed a different hash and the manifest
# test failed everywhere except the machine that baked it. These are golden
# values: if the hash ever becomes R-version- or platform-coupled again, they
# fail on the very first runner that differs.
test_that(".hash_matrix() is a pure content hash, not an R-serialization hash", {
  skip_if_not_installed("digest")
  m <- matrix(c(0, 1, 2.5, -3), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))

  expect_identical(
    .hash_matrix(m),
    "8d3f945a5dcc6eebd968c81b6944bdb00bede8dc36274a80537f9d8fa2d9d18c"
  )
  expect_identical(
    .hash_matrix(matrix(numeric(0), 0, 0)),
    "ccbe684d87216976b9bf6a93b8b58530c124aae6c985558aaef78e5134876901"
  )

  # The R version must not leak into the hashed bytes. serialize() format 3
  # writes it at bytes 7-10; assert we are not hashing such a stream.
  rv <- as.raw(c(0L, as.integer(strsplit(as.character(getRversion()), ".", fixed = TRUE)[[1]])))
  expect_identical(serialize(m, NULL, version = 3)[7:10], rv)
  expect_false(identical(
    .hash_matrix(m),
    digest::digest(serialize(m, NULL, version = 3), algo = "sha256", serialize = FALSE)
  ))

  # Content sensitivity and equal-value invariance.
  changed <- m; changed[1, 2] <- 99
  expect_false(identical(.hash_matrix(m), .hash_matrix(changed)))
  renamed <- m; dimnames(renamed) <- list(c("a", "z"), c("a", "b"))
  expect_false(identical(.hash_matrix(m), .hash_matrix(renamed)))
  neg_zero <- m; neg_zero[1, 1] <- -0
  expect_identical(.hash_matrix(m), .hash_matrix(neg_zero))
  expect_identical(.hash_matrix(matrix(1:4, 2, 2)), .hash_matrix(matrix(as.double(1:4), 2, 2)))
})
