## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is the standard CRAN incoming feasibility check:

- **Nestimate in Suggests** is not on CRAN but is available via
  `Additional_repositories: https://mohsaqr.r-universe.dev` (confirmed
  resolvable). All Nestimate functionality is guarded with
  `requireNamespace("Nestimate", quietly = TRUE)`.

## Test environments

* local macOS aarch64 (Tahoe 26.3.1), R 4.5.2
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* R-hub

## Test results

13,659 tests pass, 0 failures. 39 skips are all device/environment-specific
(SVG device requires XQuartz, PostScript font issues on macOS — not package bugs).

## Downstream dependencies

No reverse dependencies on CRAN (verified via
`tools::package_dependencies("cograph", reverse = TRUE)`).

## Changes since last CRAN release (1.5.2)

### New analysis capabilities
* `centrality()` — 23 centrality measures with per-measure wrappers
* `detect_communities()` — 11 community detection algorithms
* `motifs()` / `subgraphs()` — triad census and instance extraction with significance testing
* `cluster_summary()` / `build_mcml()` — cluster-level aggregation and multi-level models
* `robustness()` — network robustness under targeted/random attack
* `disparity_filter()` — backbone extraction (Serrano et al. 2009)
* `network_summary()` — comprehensive network-level statistics

### New visualization functions
* `plot_transitions()` / `plot_alluvial()` / `plot_trajectories()` — flow diagrams
* `plot_mcml()` — multi-level cluster visualization
* `plot_chord()` — chord diagrams
* `plot_heatmap()` / `plot_ml_heatmap()` — matrix heatmaps
* `plot_compare()` — difference network visualization
* `plot_bootstrap()` / `plot_permutation()` — significance visualization
* `plot_mixed_network()` — combined directed/undirected edges
* `overlay_communities()` — community blob overlays
* `plot_simplicial()` — simplicial complex visualization
