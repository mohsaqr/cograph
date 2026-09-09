# Every measure the igraph-backed code produced on the golden corpus must be
# reproduced by the current code, value for value, condition for condition.
# The golden file is produced by local_testing_and_equivalence/golden/
# make_golden.R from the pre-port code (docs/igraph-removal-plan.md).

for (tag in c("real_small", "degenerate")) test_that(paste("centrality surface reproduces the golden baseline:", tag), {
  skip_coverage_tests()
  path <- .golden_path(tag)
  skip_if(is.na(path), paste("golden", tag, "not available (local only)"))
  golden <- readRDS(path)
  nets <- test_networks(tier = golden$tiers, signed = FALSE)
  diffs <- lapply(seq_len(nrow(nets)), function(i) {
    rec <- golden$networks[[nets$name[i]]]
    if (is.null(rec)) return(NULL)
    .golden_compare_record(rec, .golden_recompute(rec, nets$matrix[[i]], time_limit = 300), skip = .golden_skip_measures(nets$matrix[[i]]))
  })
  diffs <- do.call(rbind, diffs[!vapply(diffs, is.null, logical(1))])
  if (!is.null(diffs)) diffs <- diffs[diffs$kind != "reference_timeout", , drop = FALSE]
  n_diff <- if (is.null(diffs)) 0L else nrow(diffs)
  expect_identical(n_diff, 0L,
                   info = if (n_diff) paste(utils::capture.output(print(head(diffs, 30))), collapse = "\n"))
})
