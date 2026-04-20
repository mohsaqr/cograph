# cograph

**cograph** is a modern R package for the analysis, visualization, and
manipulation of complex networks. It provides publication-ready plotting
with customizable layouts, node shapes, edge styles, and themes through
an intuitive, pipe-friendly API. First-class support for Transition
Network Analysis (TNA), multilayer networks, and community detection.

## Installation

``` r
# Install from CRAN
install.packages("cograph")

# Development version from GitHub
devtools::install_github("sonsoleslp/cograph")
```

## How to use it?

- [Network Visualization with cograph: A Complete Plotting
  Guide](https://sonsoles.me/cograph/articles/1_cograph-tutorial-plotting.md)
- [Why cograph?](https://sonsoles.me/cograph/articles/2_why-cograph.md)
- [Plotting TNA Models with
  splot](https://sonsoles.me/cograph/articles/3_plotting-tna-models.md)
- [Visualization of communities and hyper order
  networks](https://sonsoles.me/cograph/articles/cograph-tutorial-communities.md)
- [Network Estimation and Visualization with Nestimate +
  cograph](https://sonsoles.me/cograph/articles/cograph-tutorial-nestimate.md)
- [Advanced
  examples](https://sonsoles.me/cograph/articles/mcml-examples.md)
- [Bootstrap Forest
  Plots](https://sonsoles.me/cograph/articles/bootstrap-forest.md)
- [Migrating from qgraph to
  splot](https://sonsoles.me/cograph/articles/qgraph-to-splot.md)

## Features

### Network Plotting

| Function                                                                              | Description                             |
|---------------------------------------------------------------------------------------|-----------------------------------------|
| [`splot()`](https://sonsoles.me/cograph/reference/splot.md)                           | Base R network plot (core engine)       |
| [`soplot()`](https://sonsoles.me/cograph/reference/soplot.md)                         | Grid/ggplot2 network rendering          |
| [`tplot()`](https://sonsoles.me/cograph/reference/plot_tna.md)                        | qgraph drop-in replacement for TNA      |
| [`plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.md)                   | Hierarchical multi-group TNA layouts    |
| [`plot_mtna()`](https://sonsoles.me/cograph/reference/plot_mtna.md)                   | Multi-cluster TNA with shape containers |
| [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)                   | Markov Chain Multi-Level visualization  |
| [`plot_mlna()`](https://sonsoles.me/cograph/reference/plot_mlna.md)                   | Multilayer 3D perspective networks      |
| [`plot_mixed_network()`](https://sonsoles.me/cograph/reference/plot_mixed_network.md) | Combined symmetric/asymmetric edges     |

### Flow and Comparison Plots

| Function                                                                            | Description                            |
|-------------------------------------------------------------------------------------|----------------------------------------|
| [`plot_transitions()`](https://sonsoles.me/cograph/reference/plot_transitions.md)   | Alluvial/Sankey flow diagrams          |
| [`plot_alluvial()`](https://sonsoles.me/cograph/reference/plot_alluvial.md)         | Alluvial wrapper with flow coloring    |
| [`plot_trajectories()`](https://sonsoles.me/cograph/reference/plot_trajectories.md) | Individual tracking with line bundling |
| [`plot_chord()`](https://sonsoles.me/cograph/reference/plot_chord.md)               | Chord diagrams with ticks              |
| [`plot_heatmap()`](https://sonsoles.me/cograph/reference/plot_heatmap.md)           | Adjacency heatmaps with clustering     |
| [`plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md)           | Difference network visualization       |
| `plot_bootstrap()`                                                                  | Bootstrap CI result plots              |
| [`plot_permutation()`](https://sonsoles.me/cograph/reference/plot_permutation.md)   | Permutation test result plots          |

### Community and Higher-Order Structure

| Function                                                                                | Description                                             |
|-----------------------------------------------------------------------------------------|---------------------------------------------------------|
| [`overlay_communities()`](https://sonsoles.me/cograph/reference/overlay_communities.md) | Community blob overlays on network plots                |
| [`plot_simplicial()`](https://sonsoles.me/cograph/reference/plot_simplicial.md)         | Higher-order pathway (simplicial complex) visualization |
| [`detect_communities()`](https://sonsoles.me/cograph/reference/detect_communities.md)   | 11 igraph algorithms with shorthand wrappers            |
| [`communities()`](https://sonsoles.me/cograph/reference/communities.md)                 | Unified community detection interface                   |

### Network Analysis

| Function                                                                                                                            | Description                                                              |
|-------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)                                                               | 87 centrality measures, validated against centiserve/sna/igraph/NetworkX |
| [`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) / [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md) | Motif/triad census with per-actor windowing                              |
| [`robustness()`](https://sonsoles.me/cograph/reference/robustness.md)                                                               | Network robustness analysis                                              |
| [`disparity_filter()`](https://sonsoles.me/cograph/reference/disparity_filter.md)                                                   | Backbone extraction (Serrano et al. 2009)                                |
| [`cluster_summary()`](https://sonsoles.me/cograph/reference/cluster_summary.md)                                                     | Between/within cluster weight aggregation                                |
| [`build_mcml()`](https://sonsoles.me/cograph/reference/build_mcml.md)                                                               | Markov Chain Multi-Level model construction                              |
| [`summarize_network()`](https://sonsoles.me/cograph/reference/summarize_network.md)                                                 | Comprehensive network-level statistics                                   |
| [`verify_with_igraph()`](https://sonsoles.me/cograph/reference/verify_with_igraph.md)                                               | Cross-validation against igraph                                          |
| [`simplify()`](https://sonsoles.me/cograph/reference/simplify.md)                                                                   | Prune weak edges                                                         |

### Multilayer Networks

| Function                                                                          | Description                             |
|-----------------------------------------------------------------------------------|-----------------------------------------|
| [`supra_adjacency()`](https://sonsoles.me/cograph/reference/supra_adjacency.md)   | Supra-adjacency matrix construction     |
| [`layer_similarity()`](https://sonsoles.me/cograph/reference/layer_similarity.md) | Layer comparison measures               |
| [`aggregate_layers()`](https://sonsoles.me/cograph/reference/aggregate_layers.md) | Weight aggregation across layers        |
| [`plot_ml_heatmap()`](https://sonsoles.me/cograph/reference/plot_ml_heatmap.md)   | Multilayer heatmaps with 3D perspective |

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

![](reference/figures/README-tna-plot-1.jpeg)

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

![](reference/figures/README-donut-pie-1.jpeg)

### Chord Diagram

``` r
plot_chord(mat, title = "Transition Chord Diagram")
```

![](reference/figures/README-chord-1.jpeg)

### Heatmap

``` r
plot_heatmap(mat, show_values = TRUE, colors = "viridis",
             value_fontface = "bold", title = "Transition Heatmap")
```

![](reference/figures/README-heatmap-1.jpeg)

## License

MIT License.
