test_that("bootstrap plotting respects disabled labels and custom templates", {
  w <- matrix(
    c(0, 0.30, 0.10,
      0.20, 0, 0.40,
      0.30, 0.10, 0),
    3, 3,
    dimnames = list(LETTERS[1:3], LETTERS[1:3])
  )
  p <- matrix(
    c(1, 0.01, 0.50,
      0.03, 1, 0.001,
      0.20, 0.80, 1),
    3, 3,
    dimnames = dimnames(w)
  )
  boot <- structure(
    list(
      weights = w,
      weights_orig = w,
      p_values = p,
      ci_lower = w - 0.05,
      ci_upper = w + 0.05
    ),
    class = c("tna_bootstrap", "list")
  )

  .cograph_test_splot_call <<- NULL
  trace(
    "splot",
    where = asNamespace("cograph"),
    tracer = quote(.cograph_test_splot_call <<- as.list(match.call(expand.dots = TRUE))),
    print = FALSE
  )
  on.exit({
    untrace("splot", where = asNamespace("cograph"))
    rm(.cograph_test_splot_call, envir = .GlobalEnv)
  }, add = TRUE)

  with_temp_png(splot.tna_bootstrap(boot, edge_labels = FALSE, show_ci = TRUE))
  expect_identical(.cograph_test_splot_call[["edge_labels"]], FALSE)
  expect_null(.cograph_test_splot_call[["edge_label_template"]])
  expect_null(.cograph_test_splot_call[["edge_label_p"]])
  expect_null(.cograph_test_splot_call[["edge_ci_lower"]])

  with_temp_png(splot.tna_bootstrap(boot, show_ci = TRUE, show_stars = TRUE))
  expect_identical(.cograph_test_splot_call[["edge_label_template"]], "{est}{stars} [{low}, {up}]")
  expect_true(isTRUE(.cograph_test_splot_call[["edge_label_stars"]]))
  expect_equal(length(.cograph_test_splot_call[["edge_label_p"]]), 6L)
  expect_equal(length(.cograph_test_splot_call[["edge_ci_lower"]]), 6L)

  with_temp_png(splot.tna_bootstrap(
    boot,
    edge_label_template = "{est} {p}",
    show_stars = TRUE
  ))
  expect_identical(.cograph_test_splot_call[["edge_label_template"]], "{est} {p}")
  expect_true(isTRUE(.cograph_test_splot_call[["edge_label_stars"]]))
  expect_equal(length(.cograph_test_splot_call[["edge_label_p"]]), 6L)
})

test_that("permutation plotting respects disabled labels and custom templates", {
  labs <- LETTERS[1:3]
  diffs <- matrix(
    c(0, 0.30, -0.10,
      -0.20, 0, 0.40,
      0.10, -0.05, 0),
    3, 3,
    dimnames = list(labs, labs)
  )
  diffs_sig <- diffs
  diffs_sig[abs(diffs_sig) < 0.15] <- 0
  perm <- structure(
    list(edges = list(
      diffs_true = diffs,
      diffs_sig = diffs_sig,
      stats = data.frame(
        edge_name = c("A -> B", "A -> C", "B -> A", "B -> C", "C -> A", "C -> B"),
        diff_true = diffs[row(diffs) != col(diffs)],
        effect_size = seq_len(6),
        p_value = c(0.01, 0.20, 0.03, 0.001, 0.50, 0.80),
        stringsAsFactors = FALSE
      )
    )),
    class = "tna_permutation"
  )
  attr(perm, "labels") <- labs

  .cograph_test_splot_call <<- NULL
  trace(
    "splot",
    where = asNamespace("cograph"),
    tracer = quote(.cograph_test_splot_call <<- as.list(match.call(expand.dots = TRUE))),
    print = FALSE
  )
  on.exit({
    untrace("splot", where = asNamespace("cograph"))
    rm(.cograph_test_splot_call, envir = .GlobalEnv)
  }, add = TRUE)

  with_temp_png(plot_permutation(perm, show_nonsig = TRUE, edge_labels = FALSE))
  expect_identical(.cograph_test_splot_call[["edge_labels"]], FALSE)
  expect_null(.cograph_test_splot_call[["edge_label_p"]])

  with_temp_png(plot_permutation(
    perm,
    show_nonsig = TRUE,
    edge_label_template = "{est} {p}"
  ))
  expect_identical(.cograph_test_splot_call[["edge_label_template"]], "{est} {p}")
  expect_true(isTRUE(.cograph_test_splot_call[["edge_label_stars"]]))
  expect_equal(length(.cograph_test_splot_call[["edge_label_p"]]), 6L)
})
