# Sidelined Functions — Re-integration Guide

## Overview

These 19 R files were sidelined from cograph v1.6.0 on 2026-03-03 to keep the core package lean for CRAN submission. The full version is preserved on `main`. Each function below includes its dependencies, cross-dependencies, CRAN issues to fix, and testing checklist.

## Re-integration Order (recommended)

```
Phase 1: cluster-metrics.R     (no deps, needed by Tier 3-4)
Phase 2: robustness.R          (standalone)
Phase 3: tna-animate.R         (standalone, needed by motifs)
Phase 4: motifs-data.R → motifs.R → motifs-extract.R → motifs-plot.R → motifs-temporal.R
Phase 5: plot-chord.R, plot-heatmap.R, plot-transitions.R, plot-mixed-network.R  (standalone viz)
Phase 6: plot-htna.R, plot-htna-multi.R, mlna.R, plot-mcml.R  (depend on cluster-metrics)
Phase 7: plot-timeline.R, plot-ml-heatmap.R  (depend on cluster-metrics)
```

Each phase: copy from `sidelined/R/` → `R/`, copy tests from `sidelined/tests/` → `tests/testthat/`, run `devtools::document()`, run `devtools::test()`, run `R CMD check`, fix issues, commit.

## Per-function Re-integration Protocol

1. Copy `.R` file from `sidelined/R/` to `R/`
2. Copy test files from `sidelined/tests/` to `tests/testthat/`
3. Review `\dontrun` examples — convert self-contained ones to `\donttest`
4. Check all `set.seed()` calls use `.save_rng()/.restore_rng()` pattern
5. Run `devtools::document()` to update NAMESPACE
6. Run `devtools::test()` — all tests must pass
7. Run `R CMD check` — 0 errors, 0 warnings
8. If function was dispatched from splot.R, re-add dispatch block (see "splot.R dispatch" below)
9. Commit

---

## splot.R Dispatch Block (re-add for Tier 4 functions)

When re-integrating plot_mlna, plot_mtna, or plot_htna, re-add this block to `splot.R` after the `ensure_cograph_network()` call (before "Apply theme if specified"):

```r
# AUTO-DISPATCH TO SPECIALIZED PLOT FUNCTIONS
if (!is.null(network$node_groups)) {
  ng <- network$node_groups
  adj_mat <- to_adjacency_matrix(network)
  if ("layer" %in% names(ng)) {
    group_list <- split(ng$node, ng$layer)
    return(plot_mlna(adj_mat, layer_list = group_list, ...))
  }
  if ("cluster" %in% names(ng)) {
    group_list <- split(ng$node, ng$cluster)
    return(plot_mtna(adj_mat, cluster_list = group_list, ...))
  }
  if ("group" %in% names(ng)) {
    group_list <- split(ng$node, ng$group)
    return(plot_htna(adj_mat, node_list = group_list, ...))
  }
}
```

Also update `set_groups()` roxygen in `class-network.R` to reference the dispatch functions.

---

## Tier 1: Analysis Functions (re-integrate first — other functions depend on these)

### 1. cluster-metrics.R — cluster_summary(), cluster_quality(), cluster_significance(), aggregate_weights()

- **Purpose**: Core MCML analysis — aggregates between/within-cluster transition weights. Used by plot_mcml, plot_mtna, plot_timeline.
- **Exports**: `cluster_summary`, `cluster_quality`, `cluster_significance`, `aggregate_weights`, `wagg`, `csum`, `cqual`, `csig`
- **S3 methods**: `print.cograph_cluster_summary`, `plot.cograph_cluster_summary`, `print.cograph_cluster_significance`, `plot.cograph_cluster_significance`
- **Internal helpers**: `.normalize_clusters()`, `.compute_modularity()`, `.process_weights()`
- **Dependencies**: stats::median, base R only. No external packages.
- **Cross-deps**: Called by `plot_mcml()`, `plot_mtna()`, `plot_time_line()`. Re-integrate BEFORE those.
- **CRAN issues**: Examples used `\dontrun` with tna objects. Write self-contained examples with raw matrices.
- **Test files**: `test-cluster-metrics.R`, `test-coverage-cluster-metrics-40.R`, `test-coverage-cluster-metrics-41.R`
- **Testing checklist**:
  - [ ] aggregate_weights with all 8 methods (sum, mean, median, max, min, prod, density, geomean)
  - [ ] cluster_summary with 2, 3, 5 clusters
  - [ ] cluster_summary directed vs undirected
  - [ ] cluster_quality modularity computation
  - [ ] cluster_significance permutation test (deterministic with seed)
  - [ ] S3 print/plot methods
  - [ ] Edge cases: single cluster, all nodes in one cluster, empty clusters
  - [ ] Run with synthetic data, compare results manually

### 2. robustness.R — robustness(), robustness_auc(), robustness_summary()

- **Purpose**: Targeted attack / random failure analysis on networks (Albert et al. 2000).
- **Exports**: `robustness`, `robustness_auc`, `robustness_summary`
- **S3 methods**: `print.cograph_robustness`, `plot.cograph_robustness`, `ggplot_robustness`
- **Internal helpers**: `robustness_vertex_attack()`, `robustness_edge_attack()`
- **Dependencies**: igraph (requireNamespace), `to_igraph()` from input-igraph.R
- **Cross-deps**: None — standalone analysis.
- **CRAN issues**: Uses `set.seed()` — must use `.save_rng()/.restore_rng()` pattern.
- **Test files**: `test-coverage-robustness-40.R`
- **Testing checklist**:
  - [ ] Vertex attack with betweenness, degree, random
  - [ ] Edge attack with betweenness, random
  - [ ] robustness_auc computation
  - [ ] robustness_summary with multiple attacks
  - [ ] Deterministic results with seed
  - [ ] S3 print/plot methods
  - [ ] Edge case: disconnected graph, single node, complete graph

---

## Tier 2: Motif Functions (self-contained module, re-integrate as a group)

### 3. motifs-data.R — Constants and helpers (re-integrate FIRST in this tier)

- **Purpose**: Shared constants — triad pattern matrices, MAN descriptions, pattern filters, ggplot theme.
- **Exports**: None (all internal)
- **Internal helpers**: `.get_triad_patterns_visual()`, `.get_triad_patterns_canonical()`, `.get_man_descriptions()`, `.get_pattern_filters()`, `.get_motif_names()`, `.get_tna_initialize_model()`, `.motifs_ggplot_theme()`
- **Dependencies**: ggplot2 (for theme helper), base R
- **Cross-deps**: Used by motifs.R, motifs-extract.R, motifs-temporal.R, motifs-plot.R
- **CRAN issues**: None — no examples, internal only.
- **Testing**: Verify pattern matrices are correct triad structures.

### 4. motifs.R — motif_census()

- **Purpose**: Triad/tetrad census with significance testing against null models.
- **Exports**: `motif_census`, `triad_census`, `extract_triads`, `get_edge_list`
- **S3 methods**: `print.cograph_motifs`, `plot.cograph_motifs`
- **Dependencies**: igraph (for graph creation/motif counting), stats::sd, stats::pnorm
- **CRAN issues**: Uses `set.seed()` — apply RNG save/restore.
- **Test files**: `test-coverage-motifs-40.R`, `test-coverage-motifs-41.R`, `test-coverage-motifs-43.R`
- **Testing checklist**:
  - [ ] 3-node motif census on known graphs
  - [ ] 4-node motif census
  - [ ] Significance z-scores against known values
  - [ ] Directed vs undirected
  - [ ] Configuration model vs GNM null model
  - [ ] S3 print/plot methods
  - [ ] Edge case: empty graph, complete graph, tree

### 5. motifs-extract.R — extract_motifs()

- **Purpose**: Extract and filter triad motifs with pattern selection and significance testing.
- **Exports**: `extract_motifs`
- **S3 methods**: `print.cograph_motif_analysis`, `plot.cograph_motif_analysis`
- **Dependencies**: tna package (uses `getFromNamespace("initialize_model", "tna")`), base R
- **CRAN issues**: `getFromNamespace()` is discouraged by CRAN — replace with `tna::` public API or make tna a Suggests dependency.
- **Testing checklist**:
  - [ ] Extract triangle, network, closed, all patterns
  - [ ] edge_method: any, expected, percent
  - [ ] Filtering: exclude_types, include_types, top
  - [ ] by_type grouping
  - [ ] Significance testing with n_perm
  - [ ] Individual vs aggregate level
  - [ ] Edge case: no motifs found, all filtered out

### 6. motifs-temporal.R — extract_motifs_temporal(), triad_persistence()

- **Purpose**: Rolling-window motif evolution analysis over temporal sequences.
- **Exports**: `extract_motifs_temporal`, `triad_persistence`
- **S3 methods**: `print.cograph_temporal_motifs`, `plot.cograph_temporal_motifs`, `print.cograph_triad_persistence`, `plot.cograph_triad_persistence`
- **Dependencies**: tna package, `tna_windows()` from tna-animate.R (cross-dep!)
- **Cross-deps**: Calls `tna_windows()` — re-integrate tna-animate.R first (Phase 3).
- **CRAN issues**: Depends on tna internals.
- **Testing checklist**:
  - [ ] Wide, edge_list, long input formats
  - [ ] Window size and step variations
  - [ ] Persistence metric computation
  - [ ] S3 plot method
  - [ ] Edge case: single window, all-NA window

### 7. motifs-plot.R — Motif visualization helpers

- **Purpose**: Bar charts, heatmaps, network diagrams for motif results.
- **Exports**: None (internal, called by S3 plot methods)
- **Dependencies**: ggplot2, igraph, grid, graphics, grDevices
- **Testing**: Visual output tests (save to HTML, user review).

---

## Tier 3: Standalone Visualizations (no internal cross-deps)

### 8. plot-chord.R — plot_chord()

- **Purpose**: Chord diagrams — nodes as arcs, edges as ribbons. Arc size proportional to flow.
- **Exports**: `plot_chord`
- **Dependencies**: graphics (base), grDevices (base), `recycle_to_length()`, `bezier_points()` from utils-geometry.R
- **~655 lines**, 12 internal helpers (`.chord_*`)
- **CRAN issues**: Example uses `\dontrun` with tna. Write self-contained example with raw matrix.
- **Test files**: `test-plot-chord.R`
- **Testing checklist**:
  - [ ] Directed and undirected matrices
  - [ ] Custom segment colors, border colors
  - [ ] Label positioning and rotation
  - [ ] Tick marks
  - [ ] Title and background
  - [ ] Edge case: 2-node, single non-zero edge, diagonal-only matrix
  - [ ] Visual test: save HTML, user reviews

### 9. plot-heatmap.R — plot_heatmap()

- **Purpose**: Adjacency matrix heatmaps. Single, multi-cluster (block-diagonal), group_tna modes.
- **Exports**: `plot_heatmap`
- **S3 methods**: `plot.cograph_cluster_summary` (depends on cluster-metrics!)
- **Dependencies**: ggplot2 (requireNamespace), `.extract_weights()`, `%||%`
- **Cross-deps**: cluster_summary mode depends on cluster-metrics.R — re-integrate cluster-metrics FIRST.
- **~654 lines**, 10 internal helpers
- **Test files**: `test-coverage-plot-heatmap-40.R`
- **Testing checklist**:
  - [ ] Single matrix heatmap
  - [ ] Clustered heatmap with 2, 3, 5 clusters
  - [ ] Group TNA heatmap
  - [ ] Color palettes: viridis, heat, custom
  - [ ] Legend position options
  - [ ] Edge case: 2x2 matrix, all-zero matrix
  - [ ] Visual test

### 10. plot-ml-heatmap.R — plot_ml_heatmap()

- **Purpose**: 3D-perspective multi-layer heatmaps — tilted planes showing different network layers.
- **Exports**: `plot_ml_heatmap`
- **Dependencies**: ggplot2 (requireNamespace), `to_matrix()`, `%||%`
- **~379 lines**, 7 internal helpers
- **Test files**: `test-coverage-plot-ml-heatmap-40.R`, `test-coverage-plot-ml-heatmap-41.R`
- **Testing checklist**:
  - [ ] 2-layer and 3-layer inputs
  - [ ] Skew and compress parameters
  - [ ] Connection lines between layers
  - [ ] Color palette options
  - [ ] Edge case: single layer, mismatched dimensions
  - [ ] Visual test

### 11. plot-transitions.R — plot_transitions(), plot_alluvial(), plot_trajectories()

- **Purpose**: Alluvial/Sankey diagrams for state transitions. Accepts matrices, vectors, data frames, or lists.
- **Exports**: `plot_transitions`, `plot_alluvial`, `plot_trajectories`
- **~1000+ lines**, many internal helpers
- **Dependencies**: graphics (base), matrix operations
- **Test files**: `test-coverage-plot-transitions-41.R`
- **Testing checklist**:
  - [ ] Matrix input (single transition)
  - [ ] Two-vector input
  - [ ] Data frame input
  - [ ] List of matrices (multi-step)
  - [ ] Label positions: beside, above, below, inside, outside
  - [ ] mid_label_position parameter
  - [ ] bundle_size line aggregation
  - [ ] show_values flow labels
  - [ ] node_label_format with count
  - [ ] Edge case: single state, all transitions to one state
  - [ ] Visual test

### 12. plot-timeline.R — plot_time_line()

- **Purpose**: Cluster timeline with within-cluster detail and curved between-cluster edges.
- **Exports**: `plot_time_line`
- **Dependencies**: `cluster_summary()` (from cluster-metrics.R!), `to_matrix()`, `draw_arrow_base()`
- **Cross-deps**: Depends on cluster-metrics.R — re-integrate cluster-metrics FIRST.
- **~687 lines**
- **Test files**: `test-plot-timeline.R`
- **Testing checklist**:
  - [ ] Horizontal and vertical orientation
  - [ ] weights vs tna mode
  - [ ] Aggregation methods: sum, mean, max
  - [ ] Multi-layer mode
  - [ ] Edge case: single cluster, 2 time points
  - [ ] Visual test

### 13. plot-mixed-network.R — plot_mixed_network()

- **Purpose**: Overlay symmetric + asymmetric edges on one network.
- **Exports**: `plot_mixed_network`
- **Dependencies**: `splot()` (kept), matrix operations
- **~179 lines** — smallest sidelined file
- **Test files**: `test-plot-mixed-network.R`
- **Testing checklist**:
  - [ ] Symmetric-only, asymmetric-only, both
  - [ ] Layout options
  - [ ] Edge labels
  - [ ] Visual test

---

## Tier 4: Multi-cluster/Hierarchical Visualizations (depend on Tier 1)

### 14. plot-htna.R — plot_htna(), htna()

- **Purpose**: Multi-group network with geometric layouts (bipartite, polygon, circular).
- **Exports**: `plot_htna`, `htna`
- **Dependencies**: `detect_communities()`, `get_nodes()`, `to_matrix()`, `abbrev_label()`, graphics
- **Cross-deps with splot.R**: Re-add dispatch block when re-integrating (see above).
- **~829 lines**, 4 layout computation helpers
- **Test files**: `test-coverage-plot-htna-40.R`
- **Testing checklist**:
  - [ ] 2-group bipartite layout
  - [ ] 3-group triangle layout
  - [ ] 4+ group polygon layout
  - [ ] Circular layout
  - [ ] Jitter variations
  - [ ] Community auto-detection
  - [ ] Custom colors/shapes per group
  - [ ] Edge case: single group, unequal group sizes
  - [ ] Visual test

### 15. plot-htna-multi.R — plot_mtna(), mtna()

- **Purpose**: Multi-cluster visualization with summary edges between clusters, detail within.
- **Exports**: `plot_mtna`, `mtna`
- **Dependencies**: `cluster_summary()` (cluster-metrics.R!), `detect_communities()`, `get_nodes()`, `to_matrix()`, `abbrev_label()`
- **Cross-deps**: Depends on cluster-metrics.R. Also re-add splot.R dispatch block.
- **~883 lines**, complex shell-drawing helpers
- **Test files**: `test-coverage-plot-htna-multi-40.R`
- **Testing checklist**:
  - [ ] Circle, square, diamond, triangle shapes
  - [ ] Summary edge aggregation methods
  - [ ] Label abbreviation
  - [ ] Custom spacing and scale
  - [ ] Edge case: 2 clusters, unbalanced clusters
  - [ ] Visual test

### 16. mlna.R — plot_mlna(), mlna()

- **Purpose**: 3D perspective multilevel network — stacked layers with dashed between-layer edges.
- **Exports**: `plot_mlna`, `mlna`
- **Dependencies**: `detect_communities()`, `get_nodes()`, `to_matrix()`, `abbrev_label()`
- **Cross-deps**: Re-add splot.R dispatch block when re-integrating.
- **~800+ lines**
- **Test files**: `test-coverage-mlna-40.R`, `test-coverage-mlna-41.R`
- **Testing checklist**:
  - [ ] 2-layer and 3-layer networks
  - [ ] Horizontal and vertical layout
  - [ ] Layer spacing
  - [ ] Within vs between layer edges
  - [ ] Edge case: single layer, nodes in only one layer
  - [ ] Visual test

### 17. plot-mcml.R — plot_mcml(), mcml()

- **Purpose**: Two-layer MCML — bottom=detailed clusters, top=summary nodes. 3D perspective.
- **Exports**: `plot_mcml`, `mcml`
- **Dependencies**: `cluster_summary()` (cluster-metrics.R!), `to_matrix()`, `draw_arrow_base()`, `abbrev_label()`
- **~829 lines**
- **Test files**: `test-coverage-plot-mcml-40.R`, `test-coverage-plot-mcml-41.R`
- **Testing checklist**:
  - [ ] weights vs tna mode
  - [ ] Pie chart self-transition nodes
  - [ ] Inter-layer connections
  - [ ] Legend rendering
  - [ ] Edge case: single cluster
  - [ ] Visual test

---

## Tier 5: Animation (optional package dependency)

### 18. tna-animate.R — tna_animate(), tna_windows()

- **Purpose**: Rolling-window TNA models + animated GIF output.
- **Exports**: `tna_animate`, `tna_windows`
- **Dependencies**: tna (requireNamespace), gifski (requireNamespace for animation)
- **Cross-deps**: `tna_windows()` is called by motifs-temporal.R. If motifs-temporal is re-integrated first, extract `tna_windows()` as a utility or re-integrate this first.
- **~250+ lines**
- **CRAN issues**: gifski as Suggests. Uses `set.seed()` — apply RNG pattern.
- **Test files**: `test-tna-animate.R`
- **Testing checklist**:
  - [ ] tna_windows with various window_size/step
  - [ ] tna_animate creates valid GIF
  - [ ] NA threshold filtering
  - [ ] Edge case: window larger than data, step > window
  - [ ] Deterministic with seed

---

## Additional Sidelined Files

### utils-globals.R

- **Purpose**: `globalVariables()` declarations for ggplot2 `aes()` NSE contexts and `@importFrom` tags.
- **Contents**: Variables `x, y, z, id, group, color, fill, label, xmin, xmax, ymin, ymax, value, total, motif, triad, count, direction, window, line_color, flow_color, lw, sd`. Import tags for `stats::pnorm`, `stats::sd`, `graphics::abline`, `graphics::legend`, `graphics::lines`.
- **When to re-integrate**: Only if re-integrated files use bare NSE variables (without `.data$` prefix). Kept files use `.data$` prefix exclusively.

### test-multilayer-labels.R

- **Purpose**: Tests for `abbrev_label()` + label parameters in plot_mcml, plot_mtna, plot_mlna, plot_htna.
- **When to re-integrate**: When Tier 4 functions are re-integrated. The `abbrev_label()` tests at the top can be extracted into a standalone test if needed earlier.

### test-coverage-aes-scales-40.R

- **Purpose**: Tests for aesthetic scaling functions used by sidelined visualization functions.
- **When to re-integrate**: When sidelined visualization functions are back.

---

## Sidelined Files Inventory

### R/ (19 files)
| File | LOC | Category |
|------|-----|----------|
| cluster-metrics.R | 400+ | Analysis |
| robustness.R | 300+ | Analysis |
| motifs-data.R | 128 | Motifs |
| motifs.R | 767 | Motifs |
| motifs-extract.R | 611 | Motifs |
| motifs-plot.R | 435 | Motifs |
| motifs-temporal.R | 1048 | Motifs |
| plot-chord.R | 655 | Visualization |
| plot-heatmap.R | 654 | Visualization |
| plot-ml-heatmap.R | 379 | Visualization |
| plot-transitions.R | 1000+ | Visualization |
| plot-timeline.R | 687 | Visualization |
| plot-htna.R | 829 | Visualization |
| plot-htna-multi.R | 883 | Visualization |
| plot-mcml.R | 829 | Visualization |
| plot-mixed-network.R | 179 | Visualization |
| mlna.R | 800+ | Visualization |
| tna-animate.R | 250+ | Animation |
| utils-globals.R | 25 | Infrastructure |

### tests/ (23 files)
- test-cluster-metrics.R
- test-coverage-cluster-metrics-40.R
- test-coverage-cluster-metrics-41.R
- test-coverage-robustness-40.R
- test-coverage-motifs-40.R
- test-coverage-motifs-41.R
- test-coverage-motifs-43.R
- test-plot-chord.R
- test-coverage-plot-heatmap-40.R
- test-coverage-plot-ml-heatmap-40.R
- test-coverage-plot-ml-heatmap-41.R
- test-coverage-plot-transitions-41.R
- test-plot-timeline.R
- test-coverage-plot-htna-40.R
- test-coverage-plot-htna-multi-40.R
- test-coverage-plot-mcml-40.R
- test-coverage-plot-mcml-41.R
- test-plot-mixed-network.R
- test-coverage-mlna-40.R
- test-coverage-mlna-41.R
- test-tna-animate.R
- test-multilayer-labels.R
- test-coverage-aes-scales-40.R

### man/ (44 files)
All .Rd documentation files for sidelined exports and internal helpers.
