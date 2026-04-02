# Learnings

## CRAN check timing (2026-03-31)

- Windows CRAN check time is ~2.2x macOS. A 5 min macOS check → 11 min
  Windows.
- `\donttest` examples trigger a full re-run of ALL examples in a
  separate `--run-donttest` phase. Even if the code itself is trivial,
  the phase overhead (R session startup, package loading, Rd processing)
  costs 45s on macOS / ~100s on Windows.
- Converting `\donttest` → `\dontrun` eliminates that entire phase. This
  is valid when examples depend on Suggests packages.
- `skip_on_cran()` in test files works because `R CMD check --as-cran`
  does NOT set `NOT_CRAN=true`. But
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  DOES set it, so local runs are unaffected.
- Profile with `--timings` flag before changing anything. The bottleneck
  is not always where you expect.
