## Submission notes — cograph 2.1.1

This is a patch release on top of 2.1.0, bundling plotting-dispatch
improvements, audit-driven correctness fixes, and a cleanup of
example timing. 2.1.0 itself was not submitted to CRAN; this tarball
therefore carries everything since 2.0.0.

### New in 2.1.1 (on top of 2.1.0)

- `splot.netobject` now routes on Nestimate's `$method` slot rather than
  just direction, so sequence-based undirected networks from `build_cna()`
  and `wtna(method = "cooccurrence")` get oval TNA-family styling while
  glasso / cor / pcor / ising networks keep the psych-style spring look.
- `from_tna()` auto-detects integer-valued weight matrices (ftna, ctna,
  raw counts) and renders edge labels as `2304` rather than `2304.00`.
- `psych_styling = TRUE` is now a first-class styling preset (undirected
  counterpart of `tna_styling`).
- `splot()` dispatch expanded to cover every current tna and Nestimate
  class; self-loops preserved in all plotting paths.
- Audit fixes: directed vs undirected semantics in `detect_duplicate_edges`
  / `aggregate_duplicate_edges` / `simplify.cograph_network`, vectorised
  modularity, `CographNetwork` R6 dispatch in `is_directed()`,
  `compute_layout_for_cograph` uses `layout$get_type()`, and
  `network_small_world()` returns 0 (not NA) when observed clustering is
  zero. Each fix is pinned by `test-audit-fixes.R`.
- `motifs()`, `extract_motifs()`, and `plot.cograph_motif_analysis`
  examples reworked to use `n_perm = 10L` (or `significance = FALSE`) and
  promoted from `\dontrun` to CRAN-runnable. Every example now runs in
  under 4 seconds (total 13.6 s across 281 running examples; slowest
  single example `extract_motifs` at 3.8 s).

### Carried over from 2.1.0 (never submitted)

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
