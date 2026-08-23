## Test environments

- Local: R 4.5.2, macOS (aarch64-apple-darwin20)
- win-builder: R-devel and R-release
- GitHub Actions: macOS, Windows, Ubuntu (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

Checked with `_R_CHECK_CRAN_INCOMING_REMOTE_=TRUE`, `--as-cran` and
`--run-donttest`. (A transient `unable to verify current time` NOTE can appear
locally when the time server is unreachable; it is not present in the run above
and does not reproduce on CRAN.)

## Reverse dependencies

Two strong reverse dependencies on CRAN (`tna`, `htna`), plus three that use
cograph from Suggests (`Nestimate`, `bibnets`, `cooccure`).

Each was checked with `R CMD check` against both the current CRAN cograph
(2.3.6) and this version, in libraries identical except for the cograph
version. Results are identical in every case: no new errors, warnings or notes
are introduced by this release.

`tna` calls `cograph::splot()`, `cograph::plot_htna()` and
`cograph::plot_compare()`; `htna` imports `cograph::plot_htna()`. Every symbol
the reverse dependencies reference still resolves, and no signature changed in
a breaking way (`splot()` and `plot_mcml()` only gained arguments;
`plot_compare()` is now `function(x, ...)` forwarding to `plot_difference()`,
which retains all of its former formals). `plot_compare()` remains exported and
silent — it is a soft-deprecated alias of `plot_difference()` for end users, but
`tna` delegates to it by name, so it raises no deprecation condition.
