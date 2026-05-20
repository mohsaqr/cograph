
# cograph <img src="https://sonsoles.me/cograph/reference/figures/logo.png" align="right" width="139" />

<!-- badges: start -->

[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/sonsoleslp/cograph/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sonsoleslp/cograph/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/cograph)](https://CRAN.R-project.org/package=cograph)
[![codecov](https://codecov.io/github/sonsoleslp/cograph/coverage.svg?branch=main)](https://app.codecov.io/github/sonsoleslp/cograph?branch=main)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**cograph** is a modern R package for the analysis, visualization, and
manipulation of complex networks. It provides publication-ready plotting
with customizable layouts, node shapes, edge styles, and themes through
an intuitive, pipe-friendly API. It includes first-class support for
Transition Network Analysis (TNA), multilayer networks, and community
detection.

## Installation

``` r
# Install from CRAN
install.packages("cograph")

# Development version from GitHub
devtools::install_github("sonsoleslp/cograph")
```

## How to use it?

### Full tutorials

- [Network Visualization with
  `cograph`](https://sonsoles.me/cograph/articles/cograph-tutorial-plotting.html)
- [Visualization of communities and higher-order
  networks](https://sonsoles.me/cograph/articles/cograph-tutorial-communities.html)
- [Network Estimation and Visualization with `Nestimate` +
  cograph](https://sonsoles.me/cograph/articles/cograph-tutorial-nestimate.html)
- [Multi-Cluster Multi-Level Visualization with
  `plot_mcml`](https://sonsoles.me/cograph/articles/cograph-tutorial-mcml.html)
- [Higher-Order Network Analysis with Simplicial
  Complexes](https://sonsoles.me/cograph/articles/cograph-tutorial-simplicial.html)

### Quick guides

- [Why cograph?](https://sonsoles.me/cograph/articles/why-cograph.html)
- [Plotting TNA Models with
  `splot`](https://sonsoles.me/cograph/articles/plotting-tna-models.html)
- [Advanced
  examples](https://sonsoles.me/cograph/articles/mcml-examples.html)
- [Bootstrap Forest
  Plots](https://sonsoles.me/cograph/articles/bootstrap-forest.html)
- [Migrating from `qgraph` to
  `splot`](https://sonsoles.me/cograph/articles/qgraph-to-splot.html)

## Features

### Network Plotting

| Function               | Description                             |
|------------------------|-----------------------------------------|
| `splot()`              | Base R network plot (core engine)       |
| `soplot()`             | Grid/ggplot2 network rendering          |
| `tplot()`              | qgraph drop-in replacement for TNA      |
| `plot_htna()`          | Hierarchical multi-group TNA layouts    |
| `plot_mtna()`          | Multi-cluster TNA with shape containers |
| `plot_mcml()`          | Markov Chain Multi-Level visualization  |
| `plot_mlna()`          | Multilayer 3D perspective networks      |
| `plot_mixed_network()` | Combined symmetric/asymmetric edges     |

### Flow and Comparison Plots

| Function              | Description                            |
|-----------------------|----------------------------------------|
| `plot_transitions()`  | Alluvial/Sankey flow diagrams          |
| `plot_alluvial()`     | Alluvial wrapper with flow coloring    |
| `plot_trajectories()` | Individual tracking with line bundling |
| `plot_chord()`        | Chord diagrams with ticks              |
| `plot_heatmap()`      | Adjacency heatmaps with clustering     |
| `plot_compare()`      | Difference network visualization       |
| `plot_bootstrap()`    | Bootstrap CI result plots              |
| `plot_permutation()`  | Permutation test result plots          |

### Community and Higher-Order Structure

| Function | Description |
|----|----|
| `overlay_communities()` | Community blob overlays on network plots |
| `plot_simplicial()` | Higher-order pathway (simplicial complex) visualization |
| `detect_communities()` | 11 igraph algorithms with shorthand wrappers |
| `communities()` | Unified community detection interface |

### Network Analysis

| Function | Description |
|----|----|
| `centrality()` | 87 centrality measures, validated against centiserve/sna/igraph/NetworkX |
| `motifs()` / `subgraphs()` | Motif/triad census with per-actor windowing |
| `robustness()` | Network robustness analysis |
| `disparity_filter()` | Backbone extraction (Serrano et al. 2009) |
| `cluster_summary()` | Between/within cluster weight aggregation |
| `build_mcml()` | Markov Chain Multi-Level model construction |
| `summarize_network()` | Comprehensive network-level statistics |
| `verify_with_igraph()` | Cross-validation against igraph |
| `simplify()` | Prune weak edges |

### Multilayer Networks

| Function             | Description                             |
|----------------------|-----------------------------------------|
| `supra_adjacency()`  | Supra-adjacency matrix construction     |
| `layer_similarity()` | Layer comparison measures               |
| `aggregate_layers()` | Weight aggregation across layers        |
| `plot_ml_heatmap()`  | Multilayer heatmaps with 3D perspective |

## Examples

### TNA Plot

The primary use case: visualize transition networks from the `tna`
package.

``` r
library(tna)
library(cograph)

# Build a TNA model from sequence data
fit <- tna(group_regulation)

# One-liner visualization
splot(fit)
```

<img src="man/figures/README-tna-plot-1.jpeg" alt="" width="100%" />

### Donut + Pie

Combine outer donut ring with inner pie segments.

``` r
splot(mat,
  donut_fill = fills,
  donut_color = "steelblue",
  pie_values = pie_vals,
  pie_colors = c("#E41A1C", "#377EB8", "#4DAF4A")
)
```

<img src="man/figures/README-donut-pie-1.jpeg" alt="" width="100%" />

### Chord Diagram

``` r
plot_chord(mat, title = "Transition Chord Diagram")
```

<img src="man/figures/README-chord-1.jpeg" alt="" width="100%" />

### Heatmap

``` r
plot_heatmap(mat, show_values = TRUE, colors = "viridis",
             value_fontface = "bold", title = "Transition Heatmap")
```

<img src="man/figures/README-heatmap-1.jpeg" alt="" width="100%" />

## License

MIT License.
