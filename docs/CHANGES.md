# Changes

## 2026-03-31 — CRAN check time reduction

- Added `skip_on_cran()` to 92 coverage test files and 23 non-coverage
  test files. 5 core functional tests remain active on CRAN.
- Converted `\donttest` → `\dontrun` in 19 R source files. All affected
  examples depend on Suggests packages, making `\dontrun` appropriate
  per CRAN policy.
- Result: macOS CRAN check dropped from 5m02s to 1m46s. Estimated
  Windows time ~3m53s (was ~11 min).
