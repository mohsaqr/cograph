#!/usr/bin/env Rscript
#' ============================================================================
#' MASTER RUNNER: Comprehensive Validation Suite
#' ============================================================================
#'
#' Executes all comprehensive validation scripts and generates HTML reports.
#'
#' USAGE:
#'   Rscript validation/run_comprehensive_tests.R          # Default: 50 nets, seed=42
#'   Rscript validation/run_comprehensive_tests.R 100      # 100 networks
#'   Rscript validation/run_comprehensive_tests.R 100 123  # custom seed
#'
#' OUTPUT:
#'   - validation/results_*_comprehensive.{rds,txt}
#'   - validation/reports/*.html
#'
#' ============================================================================

args <- commandArgs(trailingOnly = TRUE)
n_tests <- if (length(args) >= 1) args[1] else "50"
seed_val <- if (length(args) >= 2) args[2] else "42"

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("  COGRAPH COMPREHENSIVE VALIDATION SUITE\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("  Networks:", n_tests, "| Seed:", seed_val, "\n")
cat("  Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# Track timing and results
suite_start <- Sys.time()
suite_results <- list()

run_script <- function(name, script, args) {
  cat(sprintf("\n[%s] Starting %s...\n", format(Sys.time(), "%H:%M:%S"), name))
  cat("-" |> rep(60) |> paste(collapse = ""), "\n")

  start_time <- Sys.time()
  exit_code <- tryCatch({
    system2("Rscript", args = c(script, args),
            stdout = "", stderr = "")
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    1L
  })
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  status <- if (exit_code == 0) "PASS" else "FAIL"
  cat(sprintf("[%s] %s completed in %.1f seconds (%s)\n",
              format(Sys.time(), "%H:%M:%S"), name, elapsed, status))

  list(name = name, status = status, elapsed = elapsed, exit_code = exit_code)
}

# ============================================================================
# Run validation scripts
# ============================================================================

# 1. Centrality
suite_results[[1]] <- run_script(
  "Centrality",
  "validation/test_centrality_comprehensive.R",
  c(n_tests, seed_val)
)

# 2. Communities
suite_results[[2]] <- run_script(
  "Communities",
  "validation/test_communities_comprehensive.R",
  c(n_tests, seed_val)
)

# 3. Network Properties
n_per_model <- max(1, as.integer(as.numeric(n_tests) / 6))
suite_results[[3]] <- run_script(
  "Network Properties",
  "validation/test_network_properties_comprehensive.R",
  c(as.character(n_per_model), seed_val)
)

# ============================================================================
# Generate HTML Reports
# ============================================================================

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("  Generating HTML Reports...\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")

report_start <- Sys.time()
report_exit <- tryCatch({
  system2("Rscript", args = "validation/generate_html_reports.R",
          stdout = "", stderr = "")
}, error = function(e) {
  cat(sprintf("  ERROR generating reports: %s\n", e$message))
  1L
})
report_elapsed <- as.numeric(difftime(Sys.time(), report_start, units = "secs"))

suite_results[[4]] <- list(
  name = "HTML Reports",
  status = if (report_exit == 0) "PASS" else "FAIL",
  elapsed = report_elapsed,
  exit_code = report_exit
)

# ============================================================================
# Final Summary
# ============================================================================

total_elapsed <- as.numeric(difftime(Sys.time(), suite_start, units = "secs"))

cat("\n\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("  VALIDATION SUITE COMPLETE\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

cat(sprintf("  %-25s  %-8s  %s\n", "Component", "Status", "Time"))
cat(sprintf("  %-25s  %-8s  %s\n", "---------", "------", "----"))
for (r in suite_results) {
  cat(sprintf("  %-25s  %-8s  %.1fs\n", r$name, r$status, r$elapsed))
}

cat(sprintf("\n  Total time: %.1f seconds\n", total_elapsed))

all_passed <- all(vapply(suite_results, function(r) r$status == "PASS", logical(1)))
if (all_passed) {
  cat("\n  *** ALL SUITES PASSED ***\n")
} else {
  failed <- vapply(suite_results[vapply(suite_results, function(r) r$status != "PASS", logical(1))],
                   function(r) r$name, character(1))
  cat(sprintf("\n  FAILED SUITES: %s\n", paste(failed, collapse = ", ")))
}

# List output files
cat("\n  Output files:\n")
rds_files <- list.files("validation", pattern = "results_.*comprehensive.*\\.(rds|txt)$",
                         full.names = TRUE)
for (f in sort(rds_files)) {
  cat(sprintf("    %s\n", f))
}

html_files <- list.files("validation/reports", pattern = "\\.html$", full.names = TRUE)
for (f in sort(html_files)) {
  cat(sprintf("    %s\n", f))
}

cat("\n")

# Exit with non-zero if any suite failed
if (!all_passed) quit(status = 1)
