## Resubmission

This is a resubmission of cograph 2.7.2, now version 2.7.3. The Intel macOS
checks for 2.7.2 failed because an example and several tests required the
optional igraph package without checking that it was available. Those cases
are now guarded; examples and tests that work without igraph still run.

The earlier 2.7.0 submission did not pass the incoming checks because of

  Flavor: r-devel-windows-x86_64
  Check: Overall checktime, Result: NOTE
    Overall checktime 16 min > 10 min

To shorten the check:

- Tests that compare cograph with other implementations (igraph, sna,
  centiserve, NetworkX and others) now live in `tests/equivalence/`, which is
  excluded from the package build and runs in our own CI.
- Tests written only to raise line coverage were removed.
- Tests that only check that a figure draws, and a few slow centrality tests,
  are skipped on CRAN; they run in CI.
- Examples fit their tna models on the first 100 sequences of
  `tna::group_regulation` instead of all 2,000, and several examples use the
  bundled `regulation_net` matrix.

On the same machine, with the same settings as the CRAN incoming check, the
test step went from 58 s of CPU time for 2.7.0 to 13 s. We could not confirm the Windows time on
win-builder before resubmitting: on 2026-09-30 every upload (R-devel and
R-release, three uploads) stopped at "checking CRAN incoming feasibility"
after 12-13 seconds, while the same step completes locally
(`checking CRAN incoming feasibility ... [6s/28s] OK`).

## Changes since 2.7.0

- `as_tna()` now warns (class `cograph_cluster_dropped`) when it leaves out a
  cluster that cannot become a tna model, instead of dropping it silently.
- cograph no longer registers a `print()` method for `mcml` objects; the class
  and its print method belong to Nestimate, and two registrations made the
  printed form depend on which package was loaded last.

The changes from 2.4.4 (the current CRAN version) are listed in NEWS.md.

## Test environments

- Local: R 4.5.2, macOS 26.3 (aarch64-apple-darwin20).
- GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release,
  oldrel-1).

## R CMD check results

For 2.7.3, the package builds and installs with igraph deliberately made
unavailable. Code and documentation checks, standard examples and
`--run-donttest` examples passed. After guarding the remaining triad-pattern
tests, the complete CRAN-mode suite passed against the installed 2.7.3
package: 1,893 passing expectations, 0 failures and 0 errors. The suite emits
12 existing warnings. The check also reported a NOTE that the current time
could not be verified. The PDF manual and vignettes were skipped in this
dependency-availability check.

The previous local check of 2.7.2 reported:

0 errors | 0 warnings | 0 notes

Run locally with `--as-cran`, `_R_CHECK_CRAN_INCOMING_=TRUE`,
`_R_CHECK_CRAN_INCOMING_REMOTE_=TRUE` and `_R_CHECK_DONTTEST_EXAMPLES_=TRUE`.

## Reverse dependencies

Seven reverse dependencies pass and the eighth fails only an unrelated timing
test (below). All were checked in their current CRAN versions
against 2.7.2 with `R CMD check --run-donttest` (and `NOT_CRAN=true`, so their
skip_on_cran() tests ran too).

```
package        version   result   tests
bibnets        0.6.0     OK       1265 passed
cooccure       0.4.0     OK        672 passed
htna           0.3.1     OK       7700 passed
idiographic    0.3.4     OK       1036 passed
lagdynamics    0.32      OK       1412 passed
Nestimate      0.8.5     see below  19611 passed, 1 timing test failed
psychnets      0.5.2     OK        798 passed
tna            1.3.1     OK        806 passed
```

In one Nestimate run a timing test failed (`test-prepare.R`, "a long single
sequence does not scale quadratically": 0.523 s against a 0.5 s bound) while
five checks ran in parallel. The test does not use cograph and is marked
skip_on_cran(); run on its own, test-prepare.R passes all 80 expectations.
Against 2.7.0 all 19612 Nestimate tests passed.
