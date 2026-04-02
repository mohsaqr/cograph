# Handoff

## Current state (2026-03-31)

### CRAN timing fix — complete

- **Problem**: CRAN NOTE `Overall checktime 11 min > 10 min` on
  `r-devel-windows-x86_64`.
- **Fix**: Three changes:
  1.  `skip_on_cran()` on all 92 coverage test files + 23 non-coverage
      test files
  2.  `\donttest` → `\dontrun` in 19 R source files (all depend on
      Suggests)
  3.  Regenerated Rd docs via
      [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
- **Result**: macOS check 5m02s → 1m46s. Estimated Windows ~3m53s.
- **5 core test files** still run on CRAN: test-cograph, test-ggplot,
  test-input-parse, test-layouts, test-themes.

### Next steps

- Resubmit to CRAN and confirm the Windows timing NOTE is gone.
