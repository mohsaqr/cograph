# Gated smoke test for tools/visual-harness.R. Mirrors the COGRAPH_COVERAGE_TESTS
# env-var pattern: opt-in by setting COGRAPH_VISUAL_SWEEP=true. The real
# purpose of the harness is manual review of contact sheets; this test only
# asserts that `run_visual_sweep(mode = "quick")` finishes without error and
# produces the expected artifacts.

test_that("run_visual_sweep('quick') writes manifest + PNGs", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("COGRAPH_VISUAL_SWEEP", unset = "false"),
                        "true"),
              "set COGRAPH_VISUAL_SWEEP=true to run")
  skip_if_not_installed("jsonlite")

  harness <- file.path(testthat::test_path("..", ".."), "tools",
                       "visual-harness.R")
  skip_if_not(file.exists(harness), "visual-harness.R not found")
  # source() inside a fresh local environment so we do not pollute the test
  # file globals.
  env <- new.env(parent = baseenv())
  sys.source(harness, envir = env)

  outdir <- tempfile("sweep-")
  sweep_dir <- env$run_visual_sweep(mode = "quick", outdir = outdir)

  expect_true(dir.exists(sweep_dir))
  manifest_path <- file.path(sweep_dir, "manifest.json")
  expect_true(file.exists(manifest_path))

  manifest <- jsonlite::read_json(manifest_path)
  expect_named(manifest, c("timestamp", "git_sha", "r_version", "ragg_available",
                            "mode", "reference_cell", "n_rows", "rows"),
               ignore.order = TRUE)
  expect_gt(manifest$n_rows, 0)
  # At least one row succeeded.
  ok_rows <- Filter(function(r) identical(r$status, "ok"), manifest$rows)
  expect_gt(length(ok_rows), 0)

  # Every successful row points to a PNG that actually exists on disk.
  for (r in ok_rows) {
    expect_true(file.exists(file.path(sweep_dir, r$file)),
                info = r$file)
  }

  unlink(outdir, recursive = TRUE)
})
