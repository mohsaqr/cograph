# Introduction to cograph

``` r
library(cograph)
```

## Why cograph

R has several network packages — igraph for graph algorithms, qgraph for
psychometric networks, tidygraph for dplyr-style manipulation. Each does
one thing well but forces you into its own data format and API. Going
from a raw matrix to a filtered, annotated, publication-ready figure
typically means loading three packages, converting between formats, and
writing boilerplate code to stitch the results together.

cograph was designed to eliminate that friction. Every function —
plotting, centrality, community detection, filtering — accepts any major
network format directly: matrices, edge lists, igraph, statnet, qgraph,
and tna objects. No manual conversion. Centrality returns a tidy data
frame, not a list of separate calls. Community detection is one function
with 11 algorithms behind it. Statistical annotations (confidence
intervals, p-values, significance stars) render directly on the figure.
And when you need igraph or statnet for something cograph does not do,
[`to_igraph()`](http://sonsoles.me/cograph/reference/to_igraph.md) and
[`to_network()`](http://sonsoles.me/cograph/reference/to_network.md)
convert back without data loss.

Beyond standard network analysis, cograph visualizes higher-order
sequential pathways as simplicial blob diagrams, renders bootstrap
stability results with forest plots (linear, circular, and grouped
layouts), and performs motif analysis that identifies specific named
node triples — not just abstract type counts.

The result is a single package that covers the full workflow from data
import to publication-ready output, while remaining interoperable with
the rest of the R network ecosystem.

``` r
set.seed(42)
n <- 10
states <- c("Explore", "Plan", "Monitor", "Adapt", "Reflect",
            "Discuss", "Synthesize", "Evaluate", "Create", "Share")
mat <- matrix(0, n, n, dimnames = list(states, states))
# Sparse: ~30% of edges populated
edges <- sample(which(row(mat) != col(mat)), 30)
mat[edges] <- round(runif(30, 0.05, 0.5), 2)
```

## Plotting

[`splot()`](http://sonsoles.me/cograph/reference/splot.md) is the main
plotting function. One call, publication-ready output.

``` r
splot(mat, tna_styling = TRUE, minimum = 0.1,
  title = "Learning Regulation Network")
```

![](introduction_files/figure-html/unnamed-chunk-3-1.png)

Key parameters: `layout`, `minimum`, `node_fill`, `node_size`,
`edge_labels`, `curvature`, `scale_nodes_by`, `theme`, `tna_styling`.

``` r
splot(mat, layout = "spring")
splot(mat, minimum = 0.1, edge_labels = TRUE)
splot(mat, scale_nodes_by = "betweenness")
splot(mat, theme = "dark")
splot(mat, tna_styling = TRUE)
```

Layouts: `"oval"`, `"spring"`, `"circle"`, `"grid"`, `"mds"`, `"star"`,
`"bipartite"`, `"groups"`, or a custom coordinate matrix.

Themes: `"default"`, `"dark"`, `"minimal"`, `"gray"`, `"nature"`,
`"colorblind"`, `"viridis"`.

Node shapes: `"circle"`, `"square"`, `"triangle"`, `"diamond"`,
`"pentagon"`, `"hexagon"`, `"star"`, `"heart"`, `"ellipse"`, `"cross"`,
`"rectangle"`, `"pie"`, `"donut"`, or custom SVG via
[`register_svg_shape()`](http://sonsoles.me/cograph/reference/register_svg_shape.md).

## Specialized plots

| Function                                                                                                                                                                        | Purpose                                               |
|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------|
| [`splot()`](http://sonsoles.me/cograph/reference/splot.md)                                                                                                                      | Network graph (base R)                                |
| [`soplot()`](http://sonsoles.me/cograph/reference/soplot.md)                                                                                                                    | Grid/ggplot2 network                                  |
| [`plot_tna()`](http://sonsoles.me/cograph/reference/plot_tna.md) / [`tplot()`](http://sonsoles.me/cograph/reference/plot_tna.md)                                                | TNA-style wrappers with qgraph-compatible parameters  |
| [`plot_chord()`](http://sonsoles.me/cograph/reference/plot_chord.md)                                                                                                            | Chord diagram (directed/undirected ribbons)           |
| [`plot_heatmap()`](http://sonsoles.me/cograph/reference/plot_heatmap.md)                                                                                                        | Adjacency heatmap with clustering                     |
| [`plot_ml_heatmap()`](http://sonsoles.me/cograph/reference/plot_ml_heatmap.md)                                                                                                  | Multi-layer comparison heatmap                        |
| [`plot_transitions()`](http://sonsoles.me/cograph/reference/plot_transitions.md) / [`plot_alluvial()`](http://sonsoles.me/cograph/reference/plot_alluvial.md)                   | Alluvial / Sankey flow diagrams                       |
| [`plot_trajectories()`](http://sonsoles.me/cograph/reference/plot_trajectories.md)                                                                                              | Individual trajectory tracking                        |
| [`plot_compare()`](http://sonsoles.me/cograph/reference/plot_compare.md)                                                                                                        | Difference network between two matrices               |
| [`plot_comparison_heatmap()`](http://sonsoles.me/cograph/reference/plot_comparison_heatmap.md)                                                                                  | Side-by-side heatmap comparison                       |
| [`plot_mixed_network()`](http://sonsoles.me/cograph/reference/plot_mixed_network.md)                                                                                            | Directed + undirected edges combined                  |
| [`plot_bootstrap_forest()`](http://sonsoles.me/cograph/reference/plot_bootstrap_forest.md)                                                                                      | Bootstrap CI forest plots (linear, circular, grouped) |
| [`plot_edge_diff_forest()`](http://sonsoles.me/cograph/reference/plot_edge_diff_forest.md)                                                                                      | Edge difference plots (linear, circular, chord, tile) |
| [`plot_simplicial()`](http://sonsoles.me/cograph/reference/plot_simplicial.md)                                                                                                  | Higher-order pathway blob overlays                    |
| [`overlay_communities()`](http://sonsoles.me/cograph/reference/overlay_communities.md)                                                                                          | Community blob overlays on network                    |
| [`plot_mcml()`](http://sonsoles.me/cograph/reference/plot_mcml.md)                                                                                                              | Two-layer hierarchical cluster visualization          |
| [`plot_mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md)                                                                                                              | Flat multi-cluster layout                             |
| [`plot_mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md)                                                                                                              | Stacked multilayer 3D perspective                     |
| [`plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md)                                                                                                              | Multi-group heterogeneous TNA layout                  |
| [`plot_robustness()`](http://sonsoles.me/cograph/reference/plot_robustness.md)                                                                                                  | Robustness degradation curves                         |
| [`plot_permutation()`](http://sonsoles.me/cograph/reference/plot_permutation.md) / [`plot_group_permutation()`](http://sonsoles.me/cograph/reference/plot_group_permutation.md) | Permutation test results                              |

``` r
plot_simplicial(mat,
  c("Explore Plan -> Monitor",
    "Monitor Adapt -> Reflect",
    "Discuss Synthesize -> Evaluate",
    "Create Share -> Explore"),
  dismantled = TRUE, ncol = 2,
  title = "Higher-Order Pathways")
```

![](introduction_files/figure-html/unnamed-chunk-5-1.png)

## Input formats

Every function accepts six formats directly.

| Format    | Example                                               |
|-----------|-------------------------------------------------------|
| Matrix    | `splot(mat)`                                          |
| Edge list | `splot(data.frame(from = "A", to = "B", weight = 1))` |
| igraph    | `splot(igraph::make_ring(5))`                         |
| statnet   | `splot(network::network(mat))`                        |
| qgraph    | `from_qgraph(q)`                                      |
| tna       | `splot(tna::tna(data))`                               |

Conversion utilities:

| Function                        | Output                             |
|---------------------------------|------------------------------------|
| `as_cograph(x)`                 | cograph_network object             |
| `to_igraph(x)`                  | igraph object                      |
| `to_matrix(x)`                  | Adjacency matrix                   |
| `to_data_frame(x)` / `to_df(x)` | Edge list data frame               |
| `to_network(x)`                 | statnet network object             |
| `from_qgraph(q)`                | Extract qgraph styles into cograph |

## Filtering and selection

Filter edges and nodes with expressions. Centrality measures are
lazy-computed inside
[`filter_nodes()`](http://sonsoles.me/cograph/reference/filter_nodes.md).

``` r
strong <- filter_edges(mat, weight > 0.3)
get_edges(strong)
#>    from to weight
#> 1     8  3   0.33
#> 2    10  3   0.49
#> 3     8  4   0.43
#> 4    10  4   0.39
#> 5     1  5   0.35
#> 6     6  5   0.35
#> 7     7  5   0.42
#> 8     2  6   0.40
#> 9     4  6   0.34
#> 10    2  8   0.49
#> 11    9  8   0.39
#> 12    3  9   0.37
#> 13    2 10   0.36
```

``` r
top3 <- select_nodes(mat, top = 3, by = "betweenness")
get_labels(top3)
#> [1] "Plan"    "Monitor" "Adapt"
```

| Function                                        | Purpose                                    |
|-------------------------------------------------|--------------------------------------------|
| `filter_edges(x, ...)`                          | Filter by weight, from, to                 |
| `filter_nodes(x, ...)`                          | Filter by degree, centrality, label        |
| `select_nodes(x, ...)`                          | Top-N by centrality, by name, neighbors    |
| `select_edges(x, ...)`                          | Top-N, involving, between, bridges, mutual |
| `select_neighbors(x, of)`                       | Ego-network extraction (multi-hop)         |
| `select_component(x)`                           | Largest or named component                 |
| `select_top(x, n, by)`                          | Top-N nodes by any centrality              |
| `select_bridges(x)`                             | Bridge edges only                          |
| `select_top_edges(x, n)`                        | Top-N edges by weight                      |
| `select_edges_involving(x, nodes)`              | Edges touching specific nodes              |
| `select_edges_between(x, s1, s2)`               | Edges between two node sets                |
| `subset_nodes(x, ...)` / `subset_edges(x, ...)` | Base R-style subsetting                    |
| `simplify(x)`                                   | Remove multi-edges and self-loops          |

Getters and setters:

| Function                            | Purpose            |
|-------------------------------------|--------------------|
| `get_nodes(x)` / `set_nodes(x, df)` | Node data frame    |
| `get_edges(x)` / `set_edges(x, df)` | Edge data frame    |
| `get_labels(x)`                     | Node label vector  |
| `n_nodes(x)` / `n_edges(x)`         | Counts             |
| `is_directed(x)`                    | Directedness       |
| `set_groups(x)` / `get_groups(x)`   | Group assignments  |
| `set_layout(x, layout)`             | Layout coordinates |
| `summarize_network(x)`              | Network summary    |

## Centrality

[`centrality()`](http://sonsoles.me/cograph/reference/centrality.md)
computes up to 25 measures and returns a data frame.

``` r
centrality(mat, measures = c("degree", "betweenness", "pagerank"))
#>          node degree_all betweenness   pagerank
#> 1     Explore          6         5.0 0.11846147
#> 2        Plan          7        15.5 0.03640953
#> 3     Monitor          8        18.0 0.18376724
#> 4       Adapt          6        15.0 0.12356096
#> 5     Reflect          6        10.0 0.12513119
#> 6     Discuss          5         0.5 0.06803638
#> 7  Synthesize          4         6.5 0.03760071
#> 8    Evaluate          5         3.0 0.07386400
#> 9      Create          7        13.0 0.13821279
#> 10      Share          6         9.0 0.09495573
```

Individual functions return named vectors:

``` r
centrality_degree(mat)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#>          6          7          8          6          6          5          4 
#>   Evaluate     Create      Share 
#>          5          7          6
centrality_pagerank(mat)
#>    Explore       Plan    Monitor      Adapt    Reflect    Discuss Synthesize 
#> 0.11846147 0.03640953 0.18376724 0.12356096 0.12513119 0.06803638 0.03760071 
#>   Evaluate     Create      Share 
#> 0.07386400 0.13821279 0.09495573
```

All 25 measures:

| Category   | Functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Degree     | [`centrality_degree()`](http://sonsoles.me/cograph/reference/centrality_degree.md), [`centrality_strength()`](http://sonsoles.me/cograph/reference/centrality_strength.md), [`centrality_indegree()`](http://sonsoles.me/cograph/reference/centrality_degree.md), [`centrality_outdegree()`](http://sonsoles.me/cograph/reference/centrality_degree.md), [`centrality_instrength()`](http://sonsoles.me/cograph/reference/centrality_strength.md), [`centrality_outstrength()`](http://sonsoles.me/cograph/reference/centrality_strength.md)                                                                                   |
| Path       | [`centrality_betweenness()`](http://sonsoles.me/cograph/reference/centrality_betweenness.md), [`centrality_closeness()`](http://sonsoles.me/cograph/reference/centrality_closeness.md), [`centrality_harmonic()`](http://sonsoles.me/cograph/reference/centrality_harmonic.md), [`centrality_eccentricity()`](http://sonsoles.me/cograph/reference/centrality_eccentricity.md) (each with in/out variants)                                                                                                                                                                                                                     |
| Spectral   | [`centrality_eigenvector()`](http://sonsoles.me/cograph/reference/centrality_eigenvector.md), [`centrality_pagerank()`](http://sonsoles.me/cograph/reference/centrality_pagerank.md), [`centrality_authority()`](http://sonsoles.me/cograph/reference/centrality_authority.md), [`centrality_hub()`](http://sonsoles.me/cograph/reference/centrality_authority.md), [`centrality_alpha()`](http://sonsoles.me/cograph/reference/centrality_alpha.md), [`centrality_power()`](http://sonsoles.me/cograph/reference/centrality_power.md), [`centrality_subgraph()`](http://sonsoles.me/cograph/reference/centrality_subgraph.md) |
| Structural | [`centrality_coreness()`](http://sonsoles.me/cograph/reference/centrality_coreness.md), [`centrality_constraint()`](http://sonsoles.me/cograph/reference/centrality_constraint.md), [`centrality_transitivity()`](http://sonsoles.me/cograph/reference/centrality_transitivity.md), [`centrality_laplacian()`](http://sonsoles.me/cograph/reference/centrality_laplacian.md)                                                                                                                                                                                                                                                   |
| Flow       | [`centrality_current_flow_closeness()`](http://sonsoles.me/cograph/reference/centrality_current_flow_closeness.md), [`centrality_current_flow_betweenness()`](http://sonsoles.me/cograph/reference/centrality_current_flow_betweenness.md), [`centrality_load()`](http://sonsoles.me/cograph/reference/centrality_load.md)                                                                                                                                                                                                                                                                                                     |
| Spreading  | [`centrality_diffusion()`](http://sonsoles.me/cograph/reference/centrality_diffusion.md), [`centrality_leverage()`](http://sonsoles.me/cograph/reference/centrality_leverage.md), [`centrality_kreach()`](http://sonsoles.me/cograph/reference/centrality_kreach.md), [`centrality_voterank()`](http://sonsoles.me/cograph/reference/centrality_voterank.md), [`centrality_percolation()`](http://sonsoles.me/cograph/reference/centrality_percolation.md)                                                                                                                                                                     |

Edge centrality:
[`edge_centrality()`](http://sonsoles.me/cograph/reference/edge_centrality.md),
[`edge_betweenness()`](http://sonsoles.me/cograph/reference/edge_centrality.md).

## Network properties

[`network_summary()`](http://sonsoles.me/cograph/reference/network_summary.md)
computes up to 37 network-level metrics.

``` r
network_summary(mat)
#>   node_count edge_count density component_count diameter mean_distance min_cut
#> 1         10         30   0.333               1     0.97         0.435       1
#>   centralization_degree centralization_in_degree centralization_out_degree
#> 1                 0.123                    0.333                     0.222
#>   centralization_betweenness centralization_closeness centralization_eigen
#> 1                      0.149                    0.238                0.479
#>   transitivity reciprocity assortativity_degree hub_score authority_score
#> 1        0.423       0.111               -0.116        NA              NA
```

| Function                                                                                               | Purpose                                          |
|--------------------------------------------------------------------------------------------------------|--------------------------------------------------|
| [`network_summary()`](http://sonsoles.me/cograph/reference/network_summary.md)                         | 37 metrics (density, diameter, clustering, etc.) |
| [`network_small_world()`](http://sonsoles.me/cograph/reference/network_small_world.md)                 | Small-world coefficient                          |
| [`network_rich_club()`](http://sonsoles.me/cograph/reference/network_rich_club.md)                     | Rich-club coefficient                            |
| [`network_global_efficiency()`](http://sonsoles.me/cograph/reference/network_global_efficiency.md)     | Global efficiency                                |
| [`network_local_efficiency()`](http://sonsoles.me/cograph/reference/network_local_efficiency.md)       | Local efficiency                                 |
| [`degree_distribution()`](http://sonsoles.me/cograph/reference/degree_distribution.md)                 | Degree histogram                                 |
| [`network_girth()`](http://sonsoles.me/cograph/reference/network_girth.md)                             | Shortest cycle                                   |
| [`network_radius()`](http://sonsoles.me/cograph/reference/network_radius.md)                           | Minimum eccentricity                             |
| [`network_bridges()`](http://sonsoles.me/cograph/reference/network_bridges.md)                         | Bridge edges                                     |
| [`network_cut_vertices()`](http://sonsoles.me/cograph/reference/network_cut_vertices.md)               | Articulation points                              |
| [`network_vertex_connectivity()`](http://sonsoles.me/cograph/reference/network_vertex_connectivity.md) | Minimum vertices to disconnect                   |
| [`network_clique_size()`](http://sonsoles.me/cograph/reference/network_clique_size.md)                 | Largest complete subgraph                        |

## Community detection

11 algorithms with a consistent interface.

``` r
comms <- communities(mat, method = "walktrap")
comms
#> Community structure (walktrap)
#>   Number of communities: 2 
#>   Modularity: 0.1976 
#>   Community sizes: 5, 5 
#>   Nodes: 10
community_sizes(comms)
#> [1] 5 5
```

| Function                                                                                                   | Algorithm                 | Alias                                                                               |
|------------------------------------------------------------------------------------------------------------|---------------------------|-------------------------------------------------------------------------------------|
| [`community_louvain()`](http://sonsoles.me/cograph/reference/community_louvain.md)                         | Louvain modularity        | [`com_lv()`](http://sonsoles.me/cograph/reference/community_louvain.md)             |
| [`community_leiden()`](http://sonsoles.me/cograph/reference/community_leiden.md)                           | Leiden (improved Louvain) | [`com_ld()`](http://sonsoles.me/cograph/reference/community_leiden.md)              |
| [`community_fast_greedy()`](http://sonsoles.me/cograph/reference/community_fast_greedy.md)                 | Fast greedy               | [`com_fg()`](http://sonsoles.me/cograph/reference/community_fast_greedy.md)         |
| [`community_walktrap()`](http://sonsoles.me/cograph/reference/community_walktrap.md)                       | Random walk               | [`com_wt()`](http://sonsoles.me/cograph/reference/community_walktrap.md)            |
| [`community_infomap()`](http://sonsoles.me/cograph/reference/community_infomap.md)                         | Information flow          | [`com_im()`](http://sonsoles.me/cograph/reference/community_infomap.md)             |
| [`community_label_propagation()`](http://sonsoles.me/cograph/reference/community_label_propagation.md)     | Label propagation         | [`com_lp()`](http://sonsoles.me/cograph/reference/community_label_propagation.md)   |
| [`community_edge_betweenness()`](http://sonsoles.me/cograph/reference/community_edge_betweenness.md)       | Edge betweenness          | [`com_eb()`](http://sonsoles.me/cograph/reference/community_edge_betweenness.md)    |
| [`community_leading_eigenvector()`](http://sonsoles.me/cograph/reference/community_leading_eigenvector.md) | Leading eigenvector       | [`com_le()`](http://sonsoles.me/cograph/reference/community_leading_eigenvector.md) |
| [`community_spinglass()`](http://sonsoles.me/cograph/reference/community_spinglass.md)                     | Spin glass                | [`com_sg()`](http://sonsoles.me/cograph/reference/community_spinglass.md)           |
| [`community_optimal()`](http://sonsoles.me/cograph/reference/community_optimal.md)                         | Exact optimization        | [`com_op()`](http://sonsoles.me/cograph/reference/community_optimal.md)             |
| [`community_fluid()`](http://sonsoles.me/cograph/reference/community_fluid.md)                             | Fluid communities         | [`com_fl()`](http://sonsoles.me/cograph/reference/community_fluid.md)               |

Additional community functions:

| Function                                                                                 | Purpose                                           |
|------------------------------------------------------------------------------------------|---------------------------------------------------|
| [`community_consensus()`](http://sonsoles.me/cograph/reference/community_consensus.md)   | Run algorithm N times, keep stable assignments    |
| [`compare_communities()`](http://sonsoles.me/cograph/reference/compare_communities.md)   | Compare partitions (NMI, VI, Rand, adjusted Rand) |
| `modularity()`                                                                           | Modularity score                                  |
| [`community_sizes()`](http://sonsoles.me/cograph/reference/community_sizes.md)           | Size of each community                            |
| [`color_communities()`](http://sonsoles.me/cograph/reference/color_communities.md)       | Color vector from community membership            |
| [`cluster_quality()`](http://sonsoles.me/cograph/reference/cluster_quality.md)           | Quality metrics (silhouette, Dunn index)          |
| [`cluster_significance()`](http://sonsoles.me/cograph/reference/cluster_significance.md) | Permutation-based significance testing            |
| [`detect_communities()`](http://sonsoles.me/cograph/reference/detect_communities.md)     | Alternative interface (returns data frame)        |

## Motifs

Motif analysis identifies recurring 3-node patterns using the MAN
classification (16 directed triad types).

``` r
mot <- motifs(mat, significance = FALSE)
mot
#> Motif Census 
#> Level: aggregate | States: 10 | Pattern: triangle 
#> 
#> Type distribution:
#> 
#> 030C 030T 120C 120D 120U 
#>    1    1    1    1    1 
#> 
#> Top 5 results:
#>  type count
#>  030T    11
#>  120C     3
#>  030C     2
#>  120D     2
#>  120U     1
```

| Function                                                                     | Purpose                                   |
|------------------------------------------------------------------------------|-------------------------------------------|
| [`motifs()`](http://sonsoles.me/cograph/reference/motifs.md)                 | MAN type census with significance testing |
| [`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md)           | Named node triples forming each pattern   |
| [`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md)     | Low-level triad census                    |
| [`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md) | Per-individual motif extraction           |
| [`extract_triads()`](http://sonsoles.me/cograph/reference/extract_triads.md) | Extract specific triad types              |
| [`triad_census()`](http://sonsoles.me/cograph/reference/triad_census.md)     | Raw 16-type triad count                   |
| [`get_edge_list()`](http://sonsoles.me/cograph/reference/get_edge_list.md)   | Edge list from tna for motif input        |

Plot types: `plot(mot, type = "types")`, `"significance"`, `"triads"`,
`"patterns"`.

## Robustness

Simulate network degradation under targeted and random removal.

``` r
robustness(mat, type = "vertex", measure = "betweenness", n_iter = 100)
plot_robustness(x = mat, measures = c("betweenness", "degree", "random"))
```

| Function                                                                             | Purpose                                        |
|--------------------------------------------------------------------------------------|------------------------------------------------|
| [`robustness()`](http://sonsoles.me/cograph/reference/robustness.md)                 | Simulate removal attacks (vertex or edge)      |
| [`plot_robustness()`](http://sonsoles.me/cograph/reference/plot_robustness.md)       | Plot robustness curves for multiple strategies |
| [`robustness_summary()`](http://sonsoles.me/cograph/reference/robustness_summary.md) | AUC and summary statistics                     |
| [`robustness_auc()`](http://sonsoles.me/cograph/reference/robustness_auc.md)         | Area under the robustness curve                |

## Disparity filter

Backbone extraction using the disparity filter (Serrano et al. 2009).

``` r
disparity_filter(mat)
splot.tna_disparity(disparity_filter(mat))
```

## Multi-cluster visualization

``` r
clusters <- list(
  Cognitive  = c("Explore", "Plan", "Monitor", "Adapt", "Reflect"),
  Social     = c("Discuss", "Synthesize", "Share"),
  Evaluative = c("Evaluate", "Create")
)
plot_mcml(mat, clusters, mode = "tna")
plot_mtna(mat, clusters)
```

| Function                                                                                                                                              | Architecture                             |
|-------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------|
| [`plot_mcml()`](http://sonsoles.me/cograph/reference/plot_mcml.md)                                                                                    | Two-layer: detail nodes + summary pies   |
| [`plot_mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md)                                                                                    | Flat cluster layout                      |
| [`plot_mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md)                                                                                    | Stacked 3D multilayer                    |
| [`plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md)                                                                                    | Multi-group heterogeneous TNA            |
| [`cluster_summary()`](http://sonsoles.me/cograph/reference/cluster_summary.md) / [`build_mcml()`](http://sonsoles.me/cograph/reference/build_mcml.md) | Pre-compute cluster aggregation          |
| [`as_tna()`](http://sonsoles.me/cograph/reference/as_tna.md) / [`as_mcml()`](http://sonsoles.me/cograph/reference/as_mcml.md)                         | Convert cluster summaries to tna objects |
| [`cluster_network()`](http://sonsoles.me/cograph/reference/summarize_network.md)                                                                      | Extract cluster-level network            |

## Multilayer networks

Construct and analyze supra-adjacency matrices for multilayer/multiplex
networks.

| Function                                                                                                                                                              | Purpose                        |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------|
| [`mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md) / [`supra_adjacency()`](http://sonsoles.me/cograph/reference/supra_adjacency.md)                        | Build supra-adjacency matrix   |
| [`supra_layer()`](http://sonsoles.me/cograph/reference/supra_layer.md) / [`supra_interlayer()`](http://sonsoles.me/cograph/reference/supra_interlayer.md)             | Extract individual layers      |
| [`aggregate_layers()`](http://sonsoles.me/cograph/reference/aggregate_layers.md) / [`aggregate_weights()`](http://sonsoles.me/cograph/reference/aggregate_weights.md) | Combine layers                 |
| [`plot_mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md)                                                                                                    | 3D perspective visualization   |
| [`plot_ml_heatmap()`](http://sonsoles.me/cograph/reference/plot_ml_heatmap.md)                                                                                        | Multi-layer heatmap comparison |

## Higher-order networks

Detect sequential dependencies beyond first-order Markov models.
Requires the **Nestimate** package.

| Function                                                                              | Purpose                                      |
|---------------------------------------------------------------------------------------|----------------------------------------------|
| [`build_hon()`](https://rdrr.io/pkg/Nestimate/man/build_hon.html)                     | Higher-Order Network construction            |
| [`build_hypa()`](https://rdrr.io/pkg/Nestimate/man/build_hypa.html)                   | Path anomaly detection (hypergeometric null) |
| [`build_mogen()`](https://rdrr.io/pkg/Nestimate/man/build_mogen.html)                 | Multi-order model selection (AIC/BIC)        |
| [`path_counts()`](https://rdrr.io/pkg/Nestimate/man/path_counts.html)                 | k-step path frequencies                      |
| [`plot_simplicial()`](http://sonsoles.me/cograph/reference/plot_simplicial.md)        | Visualize pathways as blob overlays          |
| [`build_simplicial()`](https://rdrr.io/pkg/Nestimate/man/build_simplicial.html)       | Simplicial complex from cliques              |
| [`persistent_homology()`](https://rdrr.io/pkg/Nestimate/man/persistent_homology.html) | Topological persistence across thresholds    |
| [`q_analysis()`](https://rdrr.io/pkg/Nestimate/man/q_analysis.html)                   | Multi-level structural connectivity          |
| [`verify_simplicial()`](https://rdrr.io/pkg/Nestimate/man/verify_simplicial.html)     | Cross-validate via Euler-Poincare theorem    |

## Nestimate integration

**Nestimate** estimates networks from sequence data. Its objects
dispatch through
[`splot()`](http://sonsoles.me/cograph/reference/splot.md)
automatically.

``` r
library(Nestimate)
net <- build_network(data, method = "relative")
splot(net)

boot <- bootstrap_network(net, iter = 1000)
splot(boot)
plot_bootstrap_forest(boot)
plot_bootstrap_forest(boot, layout = "circular")
plot_bootstrap_forest(boot, layout = "grouped")

grp <- build_network(data, method = "relative", group = "condition")
splot(grp)

perm <- permutation_test(grp$A, grp$B, iter = 1000)
splot(perm)
```

Estimation methods: `"relative"`, `"frequency"`, `"attention"`,
`"glasso"`, `"pcor"`, `"co_occurrence"`.

| Object            | splot() produces                                     |
|-------------------|------------------------------------------------------|
| `netobject`       | Network plot (TNA styling)                           |
| `net_bootstrap`   | Stability-styled (solid = stable, dashed = unstable) |
| `netobject_group` | Multi-panel grid (one per group)                     |
| `net_permutation` | Colored difference network                           |
| `boot_glasso`     | GLASSO bootstrap stability                           |
| `wtna_mixed`      | Mixed window TNA                                     |

## TNA integration

Direct support for all tna package objects:

| Object                  | What splot() does                     |
|-------------------------|---------------------------------------|
| `tna`                   | Network with donut rings, TNA styling |
| `group_tna`             | Multi-panel grid per group            |
| `tna_bootstrap`         | Stability-styled edges                |
| `tna_permutation`       | Colored difference network            |
| `group_tna_permutation` | Multi-panel permutation results       |
| `tna_disparity`         | Backbone filter visualization         |

## Palettes

| Function                | Colors          |
|-------------------------|-----------------|
| `palette_viridis(n)`    | Viridis scale   |
| `palette_pastel(n)`     | Soft pastel     |
| `palette_blues(n)`      | Blue gradient   |
| `palette_reds(n)`       | Red gradient    |
| `palette_diverging(n)`  | Blue-white-red  |
| `palette_colorblind(n)` | Colorblind-safe |
| `palette_rainbow(n)`    | Rainbow         |

## Pipe API

The `sn_*` functions provide a chainable builder for the grid/ggplot2
rendering path.

``` r
mat |>
  cograph() |>
  sn_layout("spring") |>
  sn_theme("minimal") |>
  sn_nodes(size = 8, fill = "steelblue") |>
  sn_edges(curvature = 0.2) |>
  sn_render(title = "My Network")

mat |> cograph() |> sn_save("network.pdf")
p <- mat |> cograph() |> sn_ggplot()
```

| Function                                                                                                                                                                                                                                     | Purpose                                 |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------|
| [`cograph()`](http://sonsoles.me/cograph/reference/cograph.md) / [`as_cograph()`](http://sonsoles.me/cograph/reference/as_cograph.md)                                                                                                        | Create network object                   |
| [`sn_nodes()`](http://sonsoles.me/cograph/reference/sn_nodes.md)                                                                                                                                                                             | Node aesthetics                         |
| [`sn_edges()`](http://sonsoles.me/cograph/reference/sn_edges.md)                                                                                                                                                                             | Edge aesthetics                         |
| [`sn_layout()`](http://sonsoles.me/cograph/reference/sn_layout.md)                                                                                                                                                                           | Layout algorithm                        |
| [`sn_theme()`](http://sonsoles.me/cograph/reference/sn_theme.md)                                                                                                                                                                             | Visual theme                            |
| [`sn_palette()`](http://sonsoles.me/cograph/reference/sn_palette.md)                                                                                                                                                                         | Color palette                           |
| [`sn_render()`](http://sonsoles.me/cograph/reference/soplot.md)                                                                                                                                                                              | Render to screen                        |
| [`sn_save()`](http://sonsoles.me/cograph/reference/sn_save.md) / [`sn_save_ggplot()`](http://sonsoles.me/cograph/reference/sn_save_ggplot.md)                                                                                                | Save to file                            |
| [`sn_ggplot()`](http://sonsoles.me/cograph/reference/sn_ggplot.md)                                                                                                                                                                           | Convert to ggplot2 object               |
| [`register_theme()`](http://sonsoles.me/cograph/reference/register_theme.md) / [`register_layout()`](http://sonsoles.me/cograph/reference/register_layout.md) / [`register_shape()`](http://sonsoles.me/cograph/reference/register_shape.md) | Register custom themes, layouts, shapes |

## Further reading

**Package resources:**

- [cograph function reference](https://saqr.me/cograph/) — complete list
  of all functions with examples
- [cograph pkgdown site](https://sonsoleslp.github.io/cograph/) — full
  documentation and articles

**Blog posts:**

- [cograph: Complex Network Analysis and
  Visualization](https://saqr.me/blog/2026/cograph-network-visualization/)
  — overview of the package design and capabilities
- [Human–AI Interaction: A TNA with
  cograph](https://saqr.me/blog/2026/human-ai-interaction-cograph/) —
  worked example analyzing 13,002 turns of human–AI coding collaboration

**References:**

- Saqr, M., López-Pernas, S., Conde, M. A., & Hernández-García, A.
  (2024). Social Network Analysis: A Primer, a Guide and a Tutorial
  in R. In *Learning Analytics Methods and Tutorials*. Springer.
  <https://doi.org/10.1007/978-3-031-54464-4_15>

- Saqr, M., López-Pernas, S., Conde, M. A., & Hernández-García, A.
  (2024). Community Detection: A Practical Guide to Unraveling Learning
  Communities. In *Learning Analytics Methods and Tutorials*. Springer.
  <https://doi.org/10.1007/978-3-031-54464-4_16>

- Saqr, M., López-Pernas, S., Törmänen, T., Kaliisa, R., Misiejuk, K., &
  Tikka, S. (2025). Transition Network Analysis: A Novel Framework for
  Modeling, Visualizing, and Identifying the Temporal Patterns of
  Learners and Learning Processes. In *Proceedings of the 15th LAK
  Conference* (pp. 351–361). ACM.
  <https://doi.org/10.1145/3706468.3706513>

- Tikka, S., López-Pernas, S., & Saqr, M. (2025). tna: An R Package for
  Transition Network Analysis. *Applied Psychological Measurement*.
  <https://doi.org/10.1177/01466216251348840>
