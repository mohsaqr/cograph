# plot_mcml(expand =) — the top layer at a finer resolution than the partition.
#
# Before this, the top layer was indexed positionally against n_clusters, so a
# macro wider than the partition was silently truncated to its first
# n_clusters rows and drawn under the cluster names: a confident, wrong figure
# with no error. These tests pin both halves of the fix — the refusal, and the
# expanded drawing.
#
# The expansion is computed from cograph's own cluster_summary() on a refined
# partition, so the tests need no other package. A pre-built summary carries no
# source to re-count from; that path delegates to Nestimate and is tested
# separately, skipped when this Nestimate does not provide macro_network().

mcml_matrix <- function() {
  m <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m["A", "B"] <- 2
  m["B", "C"] <- 1
  m["C", "D"] <- 3
  m["D", "A"] <- 1
  m["A", "C"] <- 1
  m
}

mcml_clusters <- function() {
  list(G1 = c("A", "B"), G2 = c("C", "D"))
}

render <- function(...) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 700, height = 620)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot_mcml(...)
  f
}

# --- drawing ----------------------------------------------------------------

test_that("expand draws without error and leaves the default untouched", {
  m <- mcml_matrix()
  cl <- mcml_clusters()

  a <- render(m, cl, theme = "rich")
  b <- render(m, cl, theme = "rich")
  expect_equal(unname(tools::md5sum(a)), unname(tools::md5sum(b)))

  # the expanded figure differs from the collapsed one
  e <- render(m, cl, theme = "rich", expand = "G2")
  expect_false(identical(unname(tools::md5sum(a)), unname(tools::md5sum(e))))
})

test_that("every theme survives expansion", {
  m <- mcml_matrix()
  cl <- mcml_clusters()

  for (th in c("classic", "rich", "light")) {
    expect_silent(render(m, cl, theme = th, expand = "G2"))
  }
  # including the shell arrows, which read the COLLAPSED macro
  expect_silent(render(m, cl, theme = "light", expand = "G2",
                       between_arrows = TRUE))
})

test_that("expand accepts all / TRUE and rejects an unknown cluster", {
  m <- mcml_matrix()
  cl <- mcml_clusters()

  expect_silent(render(m, cl, expand = "all"))
  expect_silent(render(m, cl, expand = TRUE))
  expect_error(render(m, cl, expand = "nope"), "Unknown")
  expect_error(render(m, cl, expand = 1), "character vector")
})

# --- the macro really is re-counted, not split ------------------------------

test_that("the expanded macro is the macro of the refined partition", {
  m <- mcml_matrix()
  cl <- mcml_clusters()

  expanded <- cograph:::.mcml_expanded_macro(m, cl, "G2", "sum", TRUE)
  reference <- cluster_summary(m, list(G1 = c("A", "B"), C = "C", D = "D"),
                               method = "sum", type = "tna",
                               compute_within = TRUE)

  expect_equal(expanded$weights, reference$macro$weights)
})

test_that("expanding every cluster reproduces the node-level network", {
  m <- mcml_matrix()
  cl <- mcml_clusters()

  expanded <- cograph:::.mcml_expanded_macro(m, cl, "all", "sum", TRUE)

  expect_equal(nrow(expanded$weights), 4L)
  expect_equal(sort(rownames(expanded$weights)), LETTERS[1:4])
})

test_that("the refined partition splits only the named clusters", {
  refined <- cograph:::.mcml_refined_partition(mcml_clusters(), "G2")

  expect_equal(names(refined), c("G1", "C", "D"))
  expect_equal(refined$G1, c("A", "B"))
  expect_equal(refined$C, "C")
})

# --- refusal ----------------------------------------------------------------

test_that("a macro wider than the partition is refused, not truncated", {
  m <- mcml_matrix()
  cl <- mcml_clusters()

  # Hand-widen the macro the way a summary must never carry it. Previously
  # this drew bw[1:2, 1:2] under the cluster names, silently.
  cs <- cluster_summary(m, cl, type = "tna", compute_within = TRUE)
  wide <- cs
  w <- cluster_summary(m, list(G1 = c("A", "B"), C = "C", D = "D"),
                       type = "tna", compute_within = TRUE)$macro$weights
  wide$macro$weights <- w
  wide$macro$labels <- rownames(w)

  expect_error(render(wide), "3 nodes but the partition has 2 clusters")
  expect_error(render(wide), "expand")
})

test_that("expansion is a property of the macro, never of the object", {
  m <- mcml_matrix()
  cl <- mcml_clusters()
  before <- cl

  invisible(render(m, cl, expand = "G2"))

  # drawing must not mutate what it was handed
  expect_equal(cl, before)
})

# --- the delegated path -----------------------------------------------------

test_that("a pre-built summary without source data says what it needs", {
  m <- mcml_matrix()
  cs <- cluster_summary(m, mcml_clusters(), type = "tna", compute_within = TRUE)

  skip_if(requireNamespace("Nestimate", quietly = TRUE) &&
            exists("macro_network", envir = asNamespace("Nestimate"),
                   inherits = FALSE),
          "this Nestimate provides macro_network()")

  expect_error(render(cs, expand = "G2"), class = "cograph_expand_unavailable")
})

test_that("an mcml expands through Nestimate when it provides macro_network", {
  skip_if_not_installed("Nestimate")
  skip_if_not(exists("macro_network", envir = asNamespace("Nestimate"),
                     inherits = FALSE),
              "this Nestimate does not provide macro_network()")

  seqs <- data.frame(
    t1 = c("A", "C", "A", "B"), t2 = c("B", "D", "C", "A"),
    t3 = c("C", "A", "D", "C"), stringsAsFactors = FALSE
  )
  mc <- Nestimate::build_mcml(seqs, clusters = list(G1 = c("A", "B"),
                                                    G2 = c("C", "D")))

  expect_silent(render(mc, expand = "G2"))
  expect_equal(nrow(mc$macro$weights), length(mc$cluster_members))
})
