# CRAN Check Timing Summary

## Problem

CRAN flagged `Overall checktime 11 min > 10 min` on
`r-devel-windows-x86_64`. Profiling showed tests (135s) were the
dominant phase, with `\donttest` examples adding a 45s pass that was
mostly overhead.

## Root Cause

1.  **92 `test-coverage-*.R` files** — exhaustive line-coverage sweeps,
    not needed on CRAN.
2.  **23 non-coverage test files** — functional but duplicated by the 5
    core tests.
3.  **28 Rd files with `\donttest` examples** — triggered an extra
    `--run-donttest` phase (45s on macOS, ~100s on Windows) for examples
    that only required Suggests packages.

## Fixes Applied

1.  Added top-level `skip_on_cran()` to all 92 coverage test files.
2.  Added top-level `skip_on_cran()` to 23 non-coverage test files,
    keeping 5 core functional tests active.
3.  Converted `\donttest` → `\dontrun` in 19 R source files, eliminating
    the `--run-donttest` phase entirely. All affected examples depend on
    Suggests packages (igraph, ggplot2, etc.), making `\dontrun`
    appropriate.

## Before / After (macOS, `rcmdcheck --as-cran --timings`)

| Phase               | Before    | After     | Saved     |
|---------------------|-----------|-----------|-----------|
| Examples            | 36s       | 20s       | 16s       |
| Examples (donttest) | 45s       | 0s        | 45s       |
| Tests (testthat)    | 135s      | 9s        | 126s      |
| **Total**           | **5m02s** | **1m46s** | **3m16s** |

Estimated Windows time: 1m46s x 2.2 ≈ **3m53s** (well under 10 min
limit).

## Files Still Running on CRAN

5 core test files: `test-cograph.R`, `test-ggplot.R`,
`test-input-parse.R`, `test-layouts.R`, `test-themes.R`.

## Date

2026-03-31
