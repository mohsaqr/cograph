# Changelog

## cograph 1.8.2

### New Features

- [`plot_simplicial()`](http://sonsoles.me/cograph/reference/plot_simplicial.md)
  now accepts `tna`, `netobject`, `net_hon`, and `net_hypa` objects
  directly — higher-order pathways are auto-built and visualized with
  proper state labels, no manual extraction needed. New parameters:
  `method` (`"hon"` / `"hypa"`), `max_pathways`, `ncol`. Dismantled mode
  uses `gridExtra` grid layout with scaled nodes
- [`print.cograph_network()`](http://sonsoles.me/cograph/reference/print.cograph_network.md)
  now shows a structured summary: node/edge counts, density,
  reciprocity, weight range, and top-degree nodes — replacing the
  minimal R6 default output
- Added `mcml` S3 class with
  [`as_mcml()`](http://sonsoles.me/cograph/reference/as_mcml.md) generic
  for type-safe handling of Markov Chain Multi-Level models — enables
  [`print()`](https://rdrr.io/r/base/print.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html), and method
  dispatch on MCML objects
- Added local `%||%` operator for R 4.1 compatibility (no longer
  requires R 4.4+)

### Breaking Changes

- MCML field names renamed for clarity: `$between` → `$macro`, `$within`
  → `$clusters`
- [`as_tna()`](http://sonsoles.me/cograph/reference/as_tna.md) on MCML
  objects now returns a flat `group_tna` list instead of a nested
  structure

### Bug Fixes

- [`plot_mcml()`](http://sonsoles.me/cograph/reference/plot_mcml.md) now
  suppresses zero-weight edges instead of drawing invisible lines, and
  strips leading zeros from edge labels (`.32` instead of `0.32`)
- Self-loops in
  [`cluster_summary()`](http://sonsoles.me/cograph/reference/cluster_summary.md)
  are now preserved in the macro diagonal, reflecting intra-cluster
  retention rates
- Sequence data is properly propagated through the full tna → macro →
  cluster pipeline, so downstream models can use bootstrap and
  permutation tests

## cograph 1.8.0

### New Features

- Added
  [`overlay_communities()`](http://sonsoles.me/cograph/reference/overlay_communities.md)
  for drawing community blob overlays on any network plot — accepts
  method names, membership vectors, or pre-computed community objects
- Added
  [`plot_simplicial()`](http://sonsoles.me/cograph/reference/plot_simplicial.md)
  for higher-order pathway visualization, rendering simplicial complexes
  as smooth blobs with flexible separators and a dismantled view option
- Added `value_nudge` parameter to
  [`plot_transitions()`](http://sonsoles.me/cograph/reference/plot_transitions.md)
  for controlling the distance between flow labels and nodes
- Added bundle legend label controls: `bundle_legend_size`,
  `bundle_legend_color`, `bundle_legend_fontface`,
  `bundle_legend_position`
- Added per-function label controls (`label_size`, `label_color`,
  `label_fontface`, `label_hjust`) to
  [`plot_transitions()`](http://sonsoles.me/cograph/reference/plot_transitions.md),
  [`plot_trajectories()`](http://sonsoles.me/cograph/reference/plot_trajectories.md),
  and
  [`plot_alluvial()`](http://sonsoles.me/cograph/reference/plot_alluvial.md)

### Bug Fixes

- Fixed spiky text halo artifacts in transition and heatmap plots by
  increasing circular offset directions from 8 to 16 (22.5° spacing for
  smooth outlines)

## cograph 1.7.0

### New Features

#### Cluster Analysis

- Added
  [`cluster_summary()`](http://sonsoles.me/cograph/reference/cluster_summary.md)
  for aggregating network weights at the cluster level, producing
  between-cluster and within-cluster matrices from raw transition data
- Added
  [`build_mcml()`](http://sonsoles.me/cograph/reference/build_mcml.md)
  for constructing Markov Chain Multi-Level models from edge lists or
  sequence data with automatic cluster detection
- Added
  [`cluster_quality()`](http://sonsoles.me/cograph/reference/cluster_quality.md)
  for modularity-based cluster quality metrics and
  [`cluster_significance()`](http://sonsoles.me/cograph/reference/cluster_significance.md)
  for permutation-based significance testing
- Added [`as_tna()`](http://sonsoles.me/cograph/reference/as_tna.md) to
  convert cluster summaries to TNA objects for bootstrapping,
  permutation testing, and plotting with
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md)

#### Network Operations

- Added [`simplify()`](http://sonsoles.me/cograph/reference/simplify.md)
  for pruning weak edges from networks, with configurable weight
  threshold and aggregation method
- Added
  [`disparity_filter()`](http://sonsoles.me/cograph/reference/disparity_filter.md)
  for backbone extraction (Serrano et al. 2009), with methods for
  matrices, tna, igraph, and cograph_network objects
- Added
  [`robustness()`](http://sonsoles.me/cograph/reference/robustness.md)
  for network robustness analysis with targeted (betweenness, degree)
  and random attack strategies, plus
  [`ggplot_robustness()`](http://sonsoles.me/cograph/reference/ggplot_robustness.md)
  for faceted ggplot2 output
- Added `temporal_edge_list()` for converting sequence data to
  timestamped edge lists
- Added
  [`supra_adjacency()`](http://sonsoles.me/cograph/reference/supra_adjacency.md),
  [`supra_layer()`](http://sonsoles.me/cograph/reference/supra_layer.md),
  [`supra_interlayer()`](http://sonsoles.me/cograph/reference/supra_interlayer.md)
  for multilayer supra-adjacency matrix construction
- Added
  [`layer_similarity()`](http://sonsoles.me/cograph/reference/layer_similarity.md),
  [`layer_similarity_matrix()`](http://sonsoles.me/cograph/reference/layer_similarity_matrix.md),
  and
  [`layer_degree_correlation()`](http://sonsoles.me/cograph/reference/layer_degree_correlation.md)
  for comparing layers in multilayer networks
- Added
  [`aggregate_weights()`](http://sonsoles.me/cograph/reference/aggregate_weights.md)
  and
  [`aggregate_layers()`](http://sonsoles.me/cograph/reference/aggregate_layers.md)
  for weight aggregation across layers
- Added
  [`verify_with_igraph()`](http://sonsoles.me/cograph/reference/verify_with_igraph.md)
  for cross-validating cograph centrality and network metrics against
  igraph

#### Motif Analysis

- Added [`motifs()`](http://sonsoles.me/cograph/reference/motifs.md) /
  [`subgraphs()`](http://sonsoles.me/cograph/reference/subgraphs.md) as
  a unified API for triad census (node-exchangeable counts) and instance
  extraction (named node triples), with auto-detection of actor/session
  columns, rolling/tumbling window support, and exact configuration
  model significance testing

#### Visualization

- Added
  [`plot_mcml()`](http://sonsoles.me/cograph/reference/plot_mcml.md) for
  Markov Chain Multi-Level visualization showing between-cluster summary
  edges alongside within-cluster detail, with pie charts, self-loops,
  and 22 customization parameters
- Added
  [`plot_chord()`](http://sonsoles.me/cograph/reference/plot_chord.md)
  for native chord diagrams with automatic weight-based arc sizing
- Added `plot_time_line()` for cluster membership timeline visualization
- Added
  [`plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md)
  orientations: `"facing"` (tip-to-tip columns) and `"circular"` (two
  semicircles), plus `intra_curvature` for drawing intra-group edges as
  dotted bezier arcs
- Added `threshold` parameter to all plot functions for filtering
  edges/cells below a minimum absolute weight
- Added `value_fontface`, `value_fontfamily`, and `value_halo`
  parameters to
  [`plot_heatmap()`](http://sonsoles.me/cograph/reference/plot_heatmap.md)
  for text styling control
- Added directional shorthands for `scale_nodes_by`: `indegree`,
  `outdegree`, `instrength`, `outstrength`, `incloseness`,
  `outcloseness`, `inharmonic`, `outharmonic`, `ineccentricity`,
  `outeccentricity`
- Added `scale_nodes_scale` parameter to
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md) for
  dampening (\< 1) or exaggerating (\> 1) centrality-based node sizing
  differences
- Added qgraph argument translation in
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md): when
  plotting tna objects, qgraph-style parameters (`vsize`, `asize`,
  `edge.color`, `lty`, `shape`) are automatically mapped to cograph
  equivalents

#### Transition Plot Enhancements

- Added intermediate labels with `node_label_format` (e.g.,
  `"{state} (n={count})"`) for showing counts on transition plot nodes
- Added line bundling via `bundle_size` for aggregating individual
  trajectories into weighted summary lines in large datasets
- Added flow value labels via `show_values` / `value_position` for
  displaying transition counts on flow lines
- Added `label_position` consistency across ALL columns (first, middle,
  last) in trajectory plots

#### Data & Infrastructure

- Added example datasets: `gamer_data`, `group_engagement`, `srl_data`
- Added `set_node_groups()` / `get_node_groups()` for managing cluster
  assignments on cograph_network objects
- Consolidated cograph_network metadata under `$meta` with getter/setter
  functions
- Added `group_tna` support to
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md) for direct
  plotting of grouped TNA models
- Gave each `centrality_*` wrapper its own focused help page

### Bug Fixes

- Fixed load and percolation centrality computation: the BFS assumed
  unit edge weights, causing infinite loops on weighted graphs; directed
  graphs now transpose correctly (matching sna convention); disconnected
  nodes no longer contribute spurious centrality
- Fixed self-loop and edge clipping in
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md) viewport
  calculation
- Fixed argument forwarding in splot dispatch for bootstrap/permutation
  objects — named parameters (minimum, threshold, layout, title) were
  consumed by
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md)’s signature
  and silently dropped when dispatching
- Fixed overlapping flow value labels in multi-step alluvial plots
- Fixed alluvial label halo rendering producing spike artifacts (8 → 16
  offset directions)
- Fixed viridis palette direction in
  [`plot_heatmap()`](http://sonsoles.me/cograph/reference/plot_heatmap.md)
  so high values get dark colors
- Fixed
  [`build_mcml()`](http://sonsoles.me/cograph/reference/build_mcml.md)
  density method crash when weight vector had no names
- Fixed display label priority resolution (labels \> label \>
  identifier)
- Removed zero-value labels that appeared after rounding in transition
  plots

### Improvements

- Simplified splot dispatch: extracted `.collect_dispatch_args()` helper
  to replace 6 copy-paste dispatch blocks, using
  [`match.call()`](https://rdrr.io/r/base/match.call.html) +
  [`mget()`](https://rdrr.io/r/base/get.html) for reliable argument
  capture

## cograph 1.6.0

### New Features

#### Centrality

- Added
  [`centrality()`](http://sonsoles.me/cograph/reference/centrality.md)
  with 23 measures and individual wrappers: degree, strength,
  betweenness, closeness, eigenvector, pagerank, harmonic, authority,
  hub, alpha, power, kreach, diffusion, percolation, eccentricity,
  transitivity, constraint, coreness, load, subgraph, leverage,
  laplacian, current-flow betweenness, current-flow closeness, voterank
- Added
  [`edge_betweenness()`](http://sonsoles.me/cograph/reference/edge_centrality.md)
  for edge-level centrality
- Added automatic weight inversion for path-based measures when working
  with tna transition matrices (where higher weight = stronger
  connection, not shorter distance)

#### Community Detection

- Added
  [`detect_communities()`](http://sonsoles.me/cograph/reference/detect_communities.md)
  with 11 algorithms: louvain, walktrap, fast_greedy, label_propagation,
  leading_eigenvector, infomap, spinglass, leiden, optimal,
  edge_betweenness, multilevel — plus `com_*` shorthand aliases
- Added consensus clustering and
  [`cluster_significance()`](http://sonsoles.me/cograph/reference/cluster_significance.md)
  for permutation-based validation

#### Network Metrics

- Added
  [`network_summary()`](http://sonsoles.me/cograph/reference/network_summary.md)
  and
  [`summarize_network()`](http://sonsoles.me/cograph/reference/summarize_network.md)
  for computing comprehensive network-level statistics (density,
  reciprocity, transitivity, diameter, components, degree distribution)

#### Visualization

- Added
  [`plot_transitions()`](http://sonsoles.me/cograph/reference/plot_transitions.md)
  for alluvial/Sankey flow diagrams, with
  [`plot_alluvial()`](http://sonsoles.me/cograph/reference/plot_alluvial.md)
  and
  [`plot_trajectories()`](http://sonsoles.me/cograph/reference/plot_trajectories.md)
  wrappers
- Added `plot_bootstrap()` and
  [`plot_permutation()`](http://sonsoles.me/cograph/reference/plot_permutation.md)
  for significance-styled visualization of bootstrap and permutation
  test results — significant edges rendered solid on top,
  non-significant edges dashed behind
- Added
  [`plot_mixed_network()`](http://sonsoles.me/cograph/reference/plot_mixed_network.md)
  for overlaying symmetric (undirected, straight) and asymmetric
  (directed, curved) edges on the same network
- Added
  [`plot_heatmap()`](http://sonsoles.me/cograph/reference/plot_heatmap.md)
  for adjacency matrix heatmaps with optional hierarchical clustering
  and
  [`plot_ml_heatmap()`](http://sonsoles.me/cograph/reference/plot_ml_heatmap.md)
  for multilayer 3D perspective heatmaps
- Added
  [`plot_compare()`](http://sonsoles.me/cograph/reference/plot_compare.md)
  for difference network visualization showing edge-weight changes
  between two networks
- Added [`splot()`](http://sonsoles.me/cograph/reference/splot.md) S3
  methods for `tna_bootstrap` and `tna_permutation` objects

#### Motif Analysis

- Added
  [`motif_census()`](http://sonsoles.me/cograph/reference/motif_census.md),
  [`triad_census()`](http://sonsoles.me/cograph/reference/triad_census.md),
  and
  [`extract_motifs()`](http://sonsoles.me/cograph/reference/extract_motifs.md)
  for triad motif analysis with pattern filtering, significance testing,
  and network diagram visualization

#### Network Utilities

- Added
  [`filter_edges()`](http://sonsoles.me/cograph/reference/filter_edges.md),
  [`subset_edges()`](http://sonsoles.me/cograph/reference/filter_edges.md),
  [`select_nodes()`](http://sonsoles.me/cograph/reference/select_nodes.md),
  [`select_edges()`](http://sonsoles.me/cograph/reference/select_edges.md)
  for flexible network subsetting
- Added
  [`set_groups()`](http://sonsoles.me/cograph/reference/set_groups.md)
  for storing cluster assignments on cograph_network objects with
  automatic dispatch to
  [`plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md) /
  [`plot_mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md)

#### Infrastructure

- All plot functions now accept `cograph_network` objects as input, in
  addition to matrices, igraph objects, and tna objects
- Layout computation is now lazy — coordinates are only calculated when
  first needed
- Improved `layout_spring` and `layout_gephi_fr` algorithms: vectorized
  attraction forces, edge aggregation for dense networks
- Renamed package from Sonnet to cograph

### Bug Fixes

- Fixed `par(pin)` error on exit when plot device state was corrupted
- Fixed motif plot scaling and margins for different device sizes

## cograph 1.5.2

CRAN release: 2026-03-02

### Breaking Changes

- Standardized first parameter name to `x` across all plotting
  functions:
  - [`plot_tna()`](http://sonsoles.me/cograph/reference/plot_tna.md):
    `input` → `x`
  - [`plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md):
    `input` → `x` (was `model`)
  - [`plot_mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md):
    `input` → `x` (was `model`)
  - [`splot()`](http://sonsoles.me/cograph/reference/splot.md) already
    used `x`

### Bug Fixes

- Fixed [`tplot()`](http://sonsoles.me/cograph/reference/plot_tna.md)
  default margins causing tiny plots compared to
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md)

### Documentation

- Added qgraph to splot migration guide (`vignettes/qgraph-to-splot.md`)

## cograph 1.5.1

### Breaking Changes (with backwards compatibility)

The following parameters have been renamed for consistency. The old
names still work but emit deprecation warnings:

| Old Name           | New Name              | Reason                                     |
|--------------------|-----------------------|--------------------------------------------|
| `esize`            | `edge_size`           | Add `edge_` prefix, expand abbreviation    |
| `cut`              | `edge_cutoff`         | Add `edge_` prefix, clarify meaning        |
| `usePCH`           | `use_pch`             | Fix camelCase to snake_case                |
| `positive_color`   | `edge_positive_color` | Add `edge_` prefix (matches theme storage) |
| `negative_color`   | `edge_negative_color` | Add `edge_` prefix (matches theme storage) |
| `donut_border_lty` | `donut_line_type`     | Expand `lty` abbreviation                  |

### Improvements

- `edge_label_fontface` now accepts string values (“plain”, “bold”,
  “italic”, “bold.italic”) in addition to numeric values

## cograph 1.4.0

### New Features

- Added [`mlna()`](http://sonsoles.me/cograph/reference/plot_mlna.md)
  for multilevel network visualization with 3D perspective
- Added [`mtna()`](http://sonsoles.me/cograph/reference/plot_mtna.md)
  for multi-cluster network visualization with shape-based cluster
  containers
- Added
  [`plot_htna()`](http://sonsoles.me/cograph/reference/plot_htna.md) for
  hierarchical multi-group network layouts with polygon and circular
  arrangements
- Added [`tplot()`](http://sonsoles.me/cograph/reference/plot_tna.md) as
  a qgraph drop-in replacement with automatic parameter translation
- Added `arrow_angle` parameter for customizable arrowhead geometry

### Bug Fixes

- Fixed Rd cross-reference warning in splot documentation
- Fixed pie/donut segment divider lines rendering when border width is 0

## cograph 1.3.1

### New Features

- Added `edge_start_dot_density` parameter for TNA-style dotted edge
  starts indicating direction
- Added direct support for tna objects via
  [`from_tna()`](http://sonsoles.me/cograph/reference/from_tna.md) — no
  manual matrix extraction needed
- Added direct support for statnet `network` and `qgraph` objects as
  input
- Added auto-conversion of `pie_values` vector to `donut_fill` when all
  values are in \[0,1\]

### Bug Fixes

- Fixed TNA visual defaults being silently overwritten in
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md) when other
  parameters were specified
- Fixed self-loop edge labels overlapping the loop arc
- Fixed `donut_shape` validation rejecting custom SVG shapes
- Fixed title clipping when title text exceeded plot margins
- Fixed edge rendering crash on certain edge/node configurations
- Removed underscore prefix requirement for custom SVG shape names

## cograph 1.2.7

### Bug Fixes

- Fixed oversized nodes in
  [`from_qgraph()`](http://sonsoles.me/cograph/reference/from_qgraph.md)
  when a layout override was provided
- Fixed oval layout using independent axis scaling, which distorted
  aspect ratios — now uses uniform scaling via `normalize_coords()`
- Fixed edge label alignment in
  [`from_qgraph()`](http://sonsoles.me/cograph/reference/from_qgraph.md)
  by using a matrix intermediary for per-edge vector reordering
- Fixed `nrow(el)` crash: qgraph’s Edgelist is a list, not a data.frame
- Fixed oval layout node distortion and donut fill values when
  converting from qgraph

## cograph 1.2.6

### New Features

- Added `donut_empty` parameter for rendering unfilled donut nodes
- Added
  [`from_qgraph()`](http://sonsoles.me/cograph/reference/from_qgraph.md)
  for converting qgraph objects to cograph format, reading resolved
  `graphAttributes` for accurate parameter extraction

### Bug Fixes

- Fixed oval `layout_info` guard causing errors on certain device
  configurations
- Fixed curvature extraction passing vector values instead of scalars

## cograph 1.2.0

### New Features

- Added [`soplot()`](http://sonsoles.me/cograph/reference/soplot.md) for
  grid/ggplot2-based network plotting — full feature parity with
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md) using a
  different rendering backend
- Added
  [`layout_oval()`](http://sonsoles.me/cograph/reference/layout_oval.md)
  for oval/elliptical node arrangements
- Added `layout_scale` parameter to expand or contract the network
  layout, with `"auto"` mode for node-count-based scaling
- Added Gephi-style Fruchterman-Reingold layout algorithm
- Added `edge_start_style` parameter for visually indicating edge
  direction via styled start segments (dashed, dotted)

### Bug Fixes

- Fixed [`soplot()`](http://sonsoles.me/cograph/reference/soplot.md)
  curve direction and edge defaults diverging from
  [`splot()`](http://sonsoles.me/cograph/reference/splot.md) behavior
- Fixed `rescale_layout` distorting oval aspect ratios by switching to
  uniform scaling
- Fixed edge scaling producing abnormally thick edges on small networks
- Fixed `par(pin)` restoration error on plot device exit

## cograph 1.1.0

### New Features

- Added [`splot()`](http://sonsoles.me/cograph/reference/splot.md) — a
  base R graphics engine for network visualization using
  [`polygon()`](https://rdrr.io/r/graphics/polygon.html),
  [`lines()`](https://rdrr.io/r/graphics/lines.html), and
  [`xspline()`](https://rdrr.io/r/graphics/xspline.html), providing
  better performance than grid-based rendering for large networks
- Added polygon-shaped donut nodes, custom SVG node shapes, and
  AI-generated shape support
- Added shadow/halo labels and fine-grained text control (fontface,
  fontfamily, hjust, vjust, angle)
- Added double donut nodes with separate inner/outer border controls
- Added edge CI (confidence interval) underlays and template-based edge
  labels
- Added comprehensive legend support: groups, edge color scales, and
  node size scales
- Added high-resolution output via
  [`sn_save()`](http://sonsoles.me/cograph/reference/sn_save.md) with
  configurable DPI
- Added edge curve modes, bidirectional arrows, self-loop rotation, and
  per-edge curve control

### Bug Fixes

- Fixed donut rendering producing artifacts and simplified the
  `donut_color` API to accept 1 color (fill), 2 colors (fill +
  background), or n colors (per-node)
- Fixed arrow positioning and curve direction for qgraph-style edges
- Fixed edge label positioning to avoid overlap with edge lines
- Fixed self-loop rendering to use qgraph-style circular arcs
- Fixed arrow placement on non-square viewports
- Fixed reciprocal edge auto-separation causing edge crossings

## cograph 1.0.0

- Initial release of cograph network visualization package
