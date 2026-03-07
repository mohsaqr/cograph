# cograph 1.6.0

## New Features

### Network Analysis
- Added `cluster_summary()` for aggregating network weights at the cluster level with between-cluster and within-cluster matrices
- Added `build_mcml()` for building Markov Chain Multi-Level models from raw transition data (edge lists or sequences)
- Added `cluster_quality()` for computing modularity-based cluster quality metrics
- Added `cluster_significance()` for permutation-based significance testing of cluster structure
- Added `as_tna()` to convert cluster summaries to TNA objects for downstream analysis
- Added `summarize_network()` for computing comprehensive network-level summary statistics
- Added `detect_communities()` with 11 community detection algorithms and shorthand wrappers (`com_lv`, `com_fg`, `com_wt`, etc.)
- Added `simplify()` for pruning weak edges from networks
- Added supra-adjacency matrix construction (`supra_adjacency`, `supra_layer`, `supra_interlayer`) for multilayer network analysis
- Added `layer_similarity()`, `layer_similarity_matrix()`, and `layer_degree_correlation()` for multilayer comparison
- Added `verify_with_igraph()` for cross-validating network metrics against igraph

### Centrality
- Added 23 centrality measures with individual help pages: `centrality_degree`, `centrality_strength`, `centrality_betweenness`, `centrality_closeness`, `centrality_eigenvector`, `centrality_pagerank`, `centrality_harmonic`, `centrality_authority`, `centrality_alpha`, `centrality_power`, `centrality_kreach`, `centrality_diffusion`, `centrality_percolation`, `centrality_eccentricity`, `centrality_transitivity`, `centrality_constraint`, `centrality_coreness`, `centrality_load`, `centrality_subgraph`, `centrality_leverage`, `centrality_laplacian`, `centrality_current_flow_betweenness`, `centrality_current_flow_closeness`, `centrality_voterank`
- Added `edge_betweenness()` for edge-level centrality
- Added directional shorthand names for `scale_nodes_by`: `indegree`, `outdegree`, `instrength`, `outstrength`, `incloseness`, `outcloseness`, `inharmonic`, `outharmonic`, `ineccentricity`, `outeccentricity`
- Added `scale_nodes_scale` parameter to `splot()` for dampening/exaggerating centrality-based node sizing

### Visualization
- Added `plot_mcml()` for Markov Chain Multi-Level network visualization with between-cluster summary edges and within-cluster detail
- Added `plot_htna()` for hierarchical multi-group network visualization with circular and polygon layouts
- Added `plot_mtna()` for multi-cluster TNA network visualization with shape-based cluster containers
- Added `plot_mlna()` / `mlna()` for multilayer network visualization with 3D perspective
- Added `plot_transitions()` for alluvial/Sankey flow diagrams with `plot_alluvial()` and `plot_trajectories()` wrappers
- Added `plot_heatmap()` for adjacency matrix heatmaps with optional clustering
- Added `plot_ml_heatmap()` for multilayer heatmaps with 3D perspective
- Added `plot_mixed_network()` for combined symmetric/asymmetric edge networks
- Added `plot_chord()` for chord diagrams
- Added `plot_compare()` for difference network visualization
- Added `splot()` S3 methods for bootstrap and permutation result plotting
- Added `threshold` parameter to all new plot functions for filtering low-weight edges/cells
- Added `value_fontface`, `value_fontfamily`, and `value_halo` parameters to `plot_heatmap()` for text styling

### Network Utilities
- Added `filter_edges()`, `subset_edges()` for edge filtering and subsetting
- Added `aggregate_weights()` and `aggregate_layers()` for weight aggregation across layers
- Added `set_node_groups()` / `get_node_groups()` for managing cluster assignments on cograph_network objects
- Consolidated cograph_network metadata under `$meta` with getter/setter functions

## Bug Fixes

- Fixed overlapping flow value labels in multi-step alluvial plots
- Removed zero-value labels that appeared after rounding in transition plots
- Fixed self-loop and edge clipping in `splot()` viewport calculation
- Fixed viridis palette direction in `plot_heatmap()` so high values get dark colors
- Fixed alluvial label halo rendering producing spike artifacts by using circular offsets
- Fixed load and percolation centrality computation bugs
- Fixed `build_mcml()` density method crash when weight vector had no names

## Improvements

- Achieved 100% test coverage (12,520 tests)
- Added `@return` and `@examples` to all exported functions for CRAN compliance
- Updated DESCRIPTION Title and Description for CRAN standards
- R CMD check: 0 errors, 0 warnings
- All URLs validated with `urlchecker::url_check()`

# cograph 1.5.2

## Breaking Changes

- Standardized first parameter name to `x` across all plotting functions:
  - `plot_tna()`: `input` → `x`
  - `plot_htna()`: `input` → `x` (was `model`)
  - `plot_mtna()`: `input` → `x` (was `model`)
  - `splot()` already used `x`

## Bug Fixes

- Fixed `tplot()` default margins causing tiny plots compared to `splot()`

## Documentation

- Added qgraph to splot migration guide (`vignettes/qgraph-to-splot.md`)

# cograph 1.5.1

## Breaking Changes (with backwards compatibility)

The following parameters have been renamed for consistency. The old names still work but emit deprecation warnings:

| Old Name | New Name | Reason |
|----------|----------|--------|
| `esize` | `edge_size` | Add `edge_` prefix, expand abbreviation |
| `cut` | `edge_cutoff` | Add `edge_` prefix, clarify meaning |
| `usePCH` | `use_pch` | Fix camelCase to snake_case |
| `positive_color` | `edge_positive_color` | Add `edge_` prefix (matches theme storage) |
| `negative_color` | `edge_negative_color` | Add `edge_` prefix (matches theme storage) |
| `donut_border_lty` | `donut_line_type` | Expand `lty` abbreviation |

## Improvements

- `edge_label_fontface` now accepts string values ("plain", "bold", "italic", "bold.italic") in addition to numeric values, matching other fontface parameters
- Added deprecation helper infrastructure for backwards-compatible parameter renaming
- Standardized all public API parameters to use consistent snake_case naming

# cograph 1.4.0

## New Features

- Added `mlna()` for multilevel network visualization with 3D perspective
- Added `mtna()` for multi-cluster network visualization
- Added `plot_htna()` for hierarchical multi-group polygon network layouts with circular layout option
- Added `tplot()` as qgraph drop-in replacement
- Added `arrow_angle` parameter for customizable arrowheads

## Bug Fixes

- Fixed Rd cross-reference warning in splot documentation
- Fixed pie/donut segment divider lines when border width is 0

# cograph 1.3.1

## New Features

- Added `edge_start_dot_density` parameter for TNA-style dotted edges
- Added direct support for tna objects via `from_tna()`
- Added direct support for statnet network and qgraph objects
- Added auto-conversion of `pie_values` vector to `donut_fill` when values are in [0,1]
- Removed underscore prefix requirement for custom SVG shapes

## Bug Fixes

- Fixed TNA defaults being overwritten in splot()
- Fixed self-loop edge labels and adjusted dotted pattern spacing
- Fixed donut_shape validation error with custom SVG shapes
- Fixed title clipping issue
- Fixed edge rendering crash
- Fixed R CMD check errors and warnings

## Improvements

- Added strict vectorization for node/edge label and donut parameters
- Added `edge_scale_mode` validation
- Improved roxygen documentation
- Added TNA plotting defaults and improved dotted edge style

# cograph 1.2.7

## Bug Fixes

- Fixed oversized nodes in `from_qgraph()` with layout override
- Fixed oval layout: use uniform scaling in `normalize_coords()`
- Fixed edge label alignment issues in `from_qgraph()`
- Fixed nrow(el) crash: qgraph Edgelist is a list, not data.frame
- Fixed misaligned edge labels by reordering per-edge vectors
- Fixed oval layout node distortion and donut fill for `from_qgraph()`

## Improvements

- Dropped qgraph cut-based edge_color/edge_width and cut param

# cograph 1.2.6

## New Features

- Added `donut_empty` parameter
- Added `from_qgraph()` function for qgraph compatibility

## Bug Fixes

- Fixed oval layout_info guard
- Fixed curvature extraction to only pass scalar values

## Improvements

- Scaled down edge width, label size, and arrow size from qgraph values
- Improved curvature, threshold/minimum, donut ratio, and node size defaults
- Rewrote `from_qgraph()` to read resolved graphAttributes

# cograph 1.2.0

## New Features

- Added `soplot()` qgraph-compatible plotting function
- Added `layout_oval()` function for oval/ellipse layouts
- Added `layout_scale` parameter to expand/contract network layout
- Added `layout_scale = "auto"` for node-count based scaling
- Added Gephi Fruchterman-Reingold layout algorithm
- Added `edge_start_style` parameter for direction indication
- Added start segment styling to edge drawing functions

## Bug Fixes

- Fixed soplot curving and edge defaults to match splot behavior
- Fixed rescale_layout to use uniform scaling, preserving oval aspect ratio
- Fixed edge scaling producing abnormally thick edges
- Fixed invalid par(pin) error on exit

## Improvements

- Added unified parameter scaling for splot/soplot alignment
- Synced all splot() parameters to soplot() for full feature parity

# cograph 1.1.0

## New Features

- Added `splot()` function for base R graphics network plotting
- Added polygon donuts, AI shapes, and SVG support
- Added shadow labels and text control options
- Added double donut nodes with separate border controls
- Added edge CI underlays and template-based edge labels
- Added comprehensive legend support for groups, edge colors, and node sizes
- Added high resolution output support
- Added edge curves mode and label styling options
- Added bidirectional arrows, loop rotation, and curve controls

## Bug Fixes

- Fixed donut rendering bug and simplified donut_color API
- Fixed arrow positioning and curve direction for qgraph-style edges
- Fixed edge label positioning to avoid overlap with edge lines
- Fixed inward curve direction for splot edges
- Fixed self-loop rendering with qgraph-style circular arcs
- Fixed arrow placement on non-square viewports
- Fixed reciprocal edge auto-separation

## Improvements

- Unified splot() and soplot() argument syntax with snake_case
- Standardized curve rules for splot and soplot
- Added layout rescaling to soplot for consistent rendering with splot
- Donut/pie content now drawn inside node boundary for visible arrows

# cograph 1.0.0

- Initial release of cograph network visualization package
