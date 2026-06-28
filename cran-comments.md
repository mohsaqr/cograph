## Test environments

- Local: R 4.5.2, macOS (aarch64-apple-darwin20)
- win-builder: R-devel and R-release
- GitHub Actions: macOS, Windows, Ubuntu (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE (`unable to verify current time`) is a transient local network
artifact; it does not reproduce on CRAN or under the strict remote
incoming check (0 errors | 0 warnings | 0 notes).

## Reverse dependencies

One strong reverse dependency (tna). It uses only `splot()`,
`plot_compare()`, and `plot_htna()`, all unchanged in this release;
checked against this version with no breakage.
