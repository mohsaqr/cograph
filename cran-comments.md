## Submission notes — cograph 2.1.0

This is a minor feature release following 2.0.0, bundling the
Batch 3–6 centrality expansion and a set of CRAN-readiness fixes.

### New functionality

- Added 12 new per-node centrality measures (Batches 3–5), all bit-exact
  against their reference implementations (centiserve, sna, igraph,
  NetworkX): Katz, Hubbell, Stephenson-Zelen information, pairwise
  disconnectivity, local reaching, Wasserman-Faust domain prestige,
  distance-weighted domain prestige, and the five Gould-Fernandez
  brokerage roles.
- Added 4 new top-level graph/set/pair-level functions (Batch 6):
  `estrada_index()`, `trophic_incoherence()`, `group_centrality()`,
  `dispersion()`. All bit-exact against NetworkX.
- Three documented divergences where cograph sides with the published
  textbook definition rather than a reference implementation's bug:
  `prestige_domain_proximity` (sna FALSE*Inf = NaN trap),
  `hubbell` (centiserve silently ignores edge weights),
  `group_centrality(measure = "betweenness")` (NetworkX Puzis iterative
  algorithm divergence). Each is flagged in the roxygen docs.

### Fixes in this release

- Vignette file renamed from `0_introduction.Rmd` to `introduction.Rmd`
  (CRAN forbids file names starting with a digit under `inst/doc/`).
- Added missing `@param dmnc_epsilon` to `centrality_dmnc.Rd`.
- Corrected a broken `\link{centrality_trophic_level}` reference in
  `trophic_incoherence.Rd` (the trophic level is a column in the
  `centrality()` output, not an exported function).
- Added `dplyr`, `influenceR`, `tidygraph`, and `tnet` to `Suggests`
  (used by gated equivalence tests via `skip_if_not_installed()`).
- Updated moved URLs in README (`codecov.io/github` → `app.codecov.io`)
  and in the introduction vignette (`saqr.me/cograph` → `saqr.me/cograph/`).

### Test environments

- Local: R 4.5.2 on macOS Tahoe 26.3.1 (aarch64-apple-darwin20)
- CI matrix (R-CMD-check.yaml): macOS-latest release, Windows-latest
  release, Ubuntu-latest devel / release / oldrel-1

### R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is `unable to verify current time`, which is a local
network-state artifact of the `rcmdcheck` run and does not reproduce
on the CI workers.

### Reverse dependency checks

cograph is a leaf package (no strong reverse dependencies). No revdep
checks required.

### Additional_repositories

`https://mohsaqr.r-universe.dev` is registered for the optional
`Nestimate` Suggest, matching the existing CRAN policy for packages
that depend on r-universe-hosted sources.
