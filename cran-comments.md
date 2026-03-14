## R CMD check results

0 errors | 0 warnings | 1 note

* Note: "data-raw" directory at top level — contains source data generation scripts, excluded from package build.

## Test environments

* local macOS aarch64, R 4.5.1
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* R-hub

## Downstream dependencies

No reverse dependencies on CRAN (verified via `tools::package_dependencies("cograph", reverse = TRUE)`).

## Changes since last CRAN release (1.5.2)

### New plot functions
* `plot_transitions()`, `plot_alluvial()`, `plot_trajectories()` — alluvial/Sankey flow diagrams
* `plot_heatmap()`, `plot_ml_heatmap()` — adjacency heatmaps with clustering
* `plot_chord()` — chord diagrams
* `plot_mixed_network()` — combined symmetric/asymmetric edge networks
* `plot_compare()` — difference network visualization
* `plot_bootstrap()`, `plot_permutation()` — statistical result visualization
* `overlay_communities()` — community blob overlays on network plots
* `plot_simplicial()` — higher-order pathway visualization
* `plot_htna()` — hierarchical multi-group TNA layouts
* `plot_mtna()` — multi-cluster TNA with shape containers
* `plot_mcml()` — Markov Chain Multi-Level visualization
* `plot_mlna()` — multilayer 3D perspective networks

### New analysis functions
* `centrality()` — 23+ centrality measures with individual wrappers
* `motifs()` / `subgraphs()` — motif/triad census with per-actor windowing
* `robustness()` — network robustness analysis
* `disparity_filter()` — backbone extraction (Serrano et al. 2009)
* `detect_communities()` — 11 community detection algorithms
* `cluster_summary()`, `build_mcml()` — cluster-level analysis
* `supra_adjacency()`, `layer_similarity()`, `aggregate_layers()` — multilayer networks
* `summarize_network()`, `verify_with_igraph()` — network-level statistics

### Improvements
* 100% test coverage (13,450+ tests)
* Added `@return` and `@examples` to all exported functions
* All URLs validated with `urlchecker::url_check()`
