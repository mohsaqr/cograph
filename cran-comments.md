## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS aarch64, R 4.5.1
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* R-hub

## Downstream dependencies

No reverse dependencies on CRAN (verified via `tools::package_dependencies("cograph", reverse = TRUE)`).

## Changes since last release (1.5.2)

* Added five new plot functions: `plot_transitions()`, `plot_heatmap()`,
  `plot_ml_heatmap()`, `plot_mixed_network()`, `plot_chord()`
* Added `threshold` parameter for filtering low-weight edges/cells
* Added directional shorthand names for `scale_nodes_by` (e.g., `instrength`, `outdegree`)
* Added `scale_nodes_scale` dampening parameter for centrality-based node sizing
* Added text styling parameters (`value_fontface`, `value_fontfamily`, `value_halo`) to `plot_heatmap()`
* Fixed viridis palette direction and alluvial halo spike artifacts
* Fixed centrality bugs and improved test coverage
