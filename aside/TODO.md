# cograph TODO

## Documentation

- [ ] Revise `docs/triad-persistence-heatmap.html` - review content and add example images

---

## Sidelined Re-integration

Priority order: Standalone Viz → Analysis Foundations → Multi-cluster Viz → Animation → Motifs.
All 19 R files + 23 test files are in `sidelined/R/` and `sidelined/tests/`.
Full details in `sidelined/REIMPLEMENTATION.md`.

### Re-integration Protocol (every file)

1. `cp sidelined/R/<file>.R R/` and `cp sidelined/tests/<test>.R tests/testthat/`
2. Review examples: self-contained `\dontrun` → `\donttest`; pseudo-code stays `\dontrun`
3. All `set.seed()` calls → `.save_rng()/.restore_rng()` pattern (CRAN requirement)
4. `devtools::document()` → verify NAMESPACE
5. `devtools::test()` → 0 failures
6. `R CMD check --no-vignettes --no-manual` → 0 errors, 0 warnings
7. If function dispatches from `splot()`, re-add dispatch block (see Phase 3 notes)
8. Visual test: render to `tmp/`, user reviews
9. Commit per phase

---

### Phase 1: Standalone Visualizations (no internal cross-deps)

- [ ] **1a. plot-chord.R** (~655 LOC) — Chord diagrams. Nodes as arcs, edges as ribbons.
  - Exports: `plot_chord`
  - Deps: base R only (`recycle_to_length()`, `bezier_points()` already in package)
  - CRAN fix: rewrite `\dontrun` example to use raw matrix instead of tna object
  - Tests: `test-plot-chord.R`
  - Verify: directed/undirected, segment colors, label rotation, tick marks, 2-node edge case

- [ ] **1b. plot-transitions.R** (~1000+ LOC) — Alluvial / Sankey / trajectory plots.
  - Exports: `plot_transitions`, `plot_alluvial`, `plot_trajectories`
  - Deps: base R only (graphics, matrix operations)
  - Tests: `test-coverage-plot-transitions-41.R`
  - Verify: matrix/vector/dataframe/list-of-matrices input, label positions (beside/above/below/inside/outside), `mid_label_position`, `bundle_size`, `show_values`, `node_label_format` with count

- [ ] **1c. plot-mixed-network.R** (~179 LOC) — Overlay symmetric + asymmetric edges. **Smallest file.**
  - Exports: `plot_mixed_network`
  - Deps: `splot()` (already in package)
  - Tests: `test-plot-mixed-network.R`
  - Verify: symmetric-only, asymmetric-only, both overlaid, layout options

- [ ] **1d. plot-heatmap.R** (~654 LOC) — Adjacency matrix heatmaps (single, clustered, group_tna).
  - Exports: `plot_heatmap`
  - Deps: ggplot2 (Suggests), `.extract_weights()`, `%||%`
  - **WARNING**: S3 method `plot.cograph_cluster_summary` depends on cluster-metrics.R. Either:
    - (a) Stub/skip the cluster_summary S3 method, add it when cluster-metrics returns
    - (b) Re-integrate cluster-metrics first (Phase 2a)
  - Tests: `test-coverage-plot-heatmap-40.R`
  - Verify: single matrix, clustered with 2/3/5 clusters, group TNA, color palettes (viridis/heat/custom), legend positions, 2x2 and all-zero edge cases

- [ ] **1e. plot-ml-heatmap.R** (~379 LOC) — 3D perspective multi-layer heatmaps.
  - Exports: `plot_ml_heatmap`
  - Deps: ggplot2 (Suggests), `to_matrix()`, `%||%`
  - Tests: `test-coverage-plot-ml-heatmap-40.R`, `test-coverage-plot-ml-heatmap-41.R`
  - Verify: 2-layer/3-layer, skew/compress params, connection lines, color palettes, single-layer edge case

---

### Phase 2: Analysis Foundations (needed by Phase 3)

- [ ] **2a. cluster-metrics.R** (~400+ LOC) — Core cluster analysis. **PREREQUISITE for Phase 3.**
  - Exports: `cluster_summary`, `cluster_quality`, `cluster_significance`, `aggregate_weights` + aliases `wagg`, `csum`, `cqual`, `csig`
  - S3 methods: `print`/`plot` for `cograph_cluster_summary` and `cograph_cluster_significance`
  - Internal: `.normalize_clusters()`, `.compute_modularity()`, `.process_weights()`
  - Deps: base R only (stats::median)
  - RNG fix: `cluster_significance()` permutation test → `.save_rng()/.restore_rng()`
  - CRAN fix: rewrite `\dontrun` examples with raw matrices + manual cluster vectors
  - Tests: `test-cluster-metrics.R`, `test-coverage-cluster-metrics-40.R`, `test-coverage-cluster-metrics-41.R`
  - Verify: `aggregate_weights` × 8 methods (sum/mean/median/max/min/prod/density/geomean), `cluster_summary` with 2/3/5 clusters, directed vs undirected, `cluster_quality` modularity vs igraph, `cluster_significance` deterministic with seed, print/plot S3, edge cases (single cluster, all nodes in one cluster, empty clusters)

- [ ] **2b. robustness.R** (~300+ LOC) — Targeted attack / random failure analysis.
  - Exports: `robustness`, `robustness_auc`, `robustness_summary`
  - S3 methods: `print`/`plot`/`ggplot_robustness` for `cograph_robustness`
  - Deps: igraph (requireNamespace), `to_igraph()` (already in package)
  - RNG fix: `set.seed()` → `.save_rng()/.restore_rng()`
  - Tests: `test-coverage-robustness-40.R`
  - Verify: vertex attack (betweenness/degree/random), edge attack (betweenness/random), AUC matches manual integration, summary with multiple attacks, deterministic with seed, disconnected/single-node/complete graph edge cases

---

### Phase 3: Multi-cluster / Hierarchical Viz (depend on Phase 2a)

**PREREQUISITE**: cluster-metrics.R must be back first.

**splot.R dispatch block** — when re-integrating plot_htna/mtna/mlna, re-add this after `ensure_cograph_network()`:
```r
if (!is.null(network$node_groups)) {
  ng <- network$node_groups
  adj_mat <- to_adjacency_matrix(network)
  if ("layer" %in% names(ng))   return(plot_mlna(adj_mat, layer_list = split(ng$node, ng$layer), ...))
  if ("cluster" %in% names(ng)) return(plot_mtna(adj_mat, cluster_list = split(ng$node, ng$cluster), ...))
  if ("group" %in% names(ng))   return(plot_htna(adj_mat, node_list = split(ng$node, ng$group), ...))
}
```
Also update `set_groups()` roxygen in `class-network.R`.

- [ ] **3a. plot-htna.R** (~829 LOC) — Multi-group network with geometric layouts (bipartite/polygon/circular).
  - Exports: `plot_htna`, `htna`
  - Deps: `detect_communities()`, `get_nodes()`, `to_matrix()`, `abbrev_label()`, graphics
  - splot.R: re-add dispatch for `"group"`
  - Tests: `test-coverage-plot-htna-40.R`
  - Verify: 2-group bipartite, 3-group triangle, 4+ polygon, circular, jitter, community auto-detection, custom colors/shapes, single group edge case

- [ ] **3b. plot-htna-multi.R** (~883 LOC) — Multi-cluster summary with shell shapes (plot_mtna).
  - Exports: `plot_mtna`, `mtna`
  - Deps: `cluster_summary()` (cluster-metrics!), `detect_communities()`, `get_nodes()`, `to_matrix()`, `abbrev_label()`
  - splot.R: re-add dispatch for `"cluster"`
  - Tests: `test-coverage-plot-htna-multi-40.R`
  - Verify: circle/square/diamond/triangle shells, aggregation methods (sum/mean/max), label abbreviation, spacing/scale params, 2 clusters + unbalanced clusters

- [ ] **3c. mlna.R** (~800+ LOC) — 3D perspective multilevel stacked-layer network (plot_mlna).
  - Exports: `plot_mlna`, `mlna`
  - Deps: `detect_communities()`, `get_nodes()`, `to_matrix()`, `abbrev_label()`
  - splot.R: re-add dispatch for `"layer"`
  - Tests: `test-coverage-mlna-40.R`, `test-coverage-mlna-41.R`
  - Verify: 2-layer/3-layer, horizontal/vertical, layer spacing, within vs between edges (dashed), single-layer edge case

- [ ] **3d. plot-mcml.R** (~829 LOC) — Two-layer MCML: bottom=detailed clusters, top=summary nodes.
  - Exports: `plot_mcml`, `mcml`
  - Deps: `cluster_summary()` (cluster-metrics!), `to_matrix()`, `draw_arrow_base()`, `abbrev_label()`
  - Tests: `test-coverage-plot-mcml-40.R`, `test-coverage-plot-mcml-41.R`
  - Verify: weights vs tna mode, pie chart self-transitions, inter-layer connections, legend, single-cluster edge case

- [ ] **3e. plot-timeline.R** (~687 LOC) — Cluster timeline with within-cluster detail.
  - Exports: `plot_time_line`
  - Deps: `cluster_summary()` (cluster-metrics!), `to_matrix()`, `draw_arrow_base()`
  - Tests: `test-plot-timeline.R`
  - Verify: horizontal/vertical, weights vs tna, aggregation (sum/mean/max), multi-layer mode, single cluster + 2 time points

---

### Phase 4: Animation (optional gifski dep)

- [ ] **4a. tna-animate.R** (~250+ LOC) — Rolling-window TNA models + animated GIF.
  - Exports: `tna_animate`, `tna_windows`
  - Deps: tna (Suggests), gifski (Suggests)
  - RNG fix: `set.seed()` → `.save_rng()/.restore_rng()`
  - Add `gifski` to Suggests in DESCRIPTION if missing
  - **IMPORTANT**: `tna_windows()` is called by `motifs-temporal.R` — must come before Phase 5
  - Tests: `test-tna-animate.R`
  - Verify: `tna_windows` with various window_size/step, `tna_animate` creates valid GIF, NA threshold filtering, window > data edge case, deterministic with seed

---

### Phase 5: Motifs Module (5 files — re-integrate as a group)

**PREREQUISITE**: Phase 4a (`tna_windows()` needed by `motifs-temporal.R`).
All 5 files cross-reference each other. Copy all together, then test.

- [ ] **5a. motifs-data.R** (~128 LOC) — Shared constants: triad patterns, MAN descriptions, filters, ggplot theme. **Integrate first.**
  - Exports: none (all internal)
  - Deps: ggplot2 (theme helper)
  - CRAN fix: replace `getFromNamespace("initialize_model", "tna")` with public `tna::` API

- [ ] **5b. motifs.R** (~767 LOC) — Motif census with significance testing.
  - Exports: `motif_census`, `triad_census`, `extract_triads`, `get_edge_list`
  - S3: `print`/`plot` for `cograph_motifs`
  - Deps: igraph, stats::sd, stats::pnorm
  - RNG fix: `set.seed()` → `.save_rng()/.restore_rng()`
  - Tests: `test-coverage-motifs-40.R`, `test-coverage-motifs-41.R`, `test-coverage-motifs-43.R`
  - Verify: 3-node/4-node census, z-scores, directed vs undirected, config model vs GNM null, empty/complete/tree graphs

- [ ] **5c. motifs-extract.R** (~611 LOC) — Extract and filter triad motifs.
  - Exports: `extract_motifs`
  - S3: `print`/`plot` for `cograph_motif_analysis`
  - Deps: tna package
  - CRAN fix: replace `getFromNamespace()` with public tna API
  - Verify: triangle/network/closed/all patterns, edge_method (any/expected/percent), filtering, by_type, significance, individual vs aggregate, no-motifs-found edge case

- [ ] **5d. motifs-temporal.R** (~1048 LOC) — Rolling-window motif evolution. **Largest sidelined file.**
  - Exports: `extract_motifs_temporal`, `triad_persistence`
  - S3: `print`/`plot` for `cograph_temporal_motifs` and `cograph_triad_persistence`
  - Deps: tna, `tna_windows()` from tna-animate.R (must be re-integrated first)
  - Verify: wide/edge_list/long input, window size/step, persistence metrics, single window + all-NA window edge cases

- [ ] **5e. motifs-plot.R** (~435 LOC) — Bar charts, heatmaps, network diagrams for motif results.
  - Exports: none (internal — called by S3 plot methods)
  - Deps: ggplot2, igraph, grid, graphics, grDevices

---

### Infrastructure (as needed)

- [ ] **utils-globals.R** (~25 LOC) — `globalVariables()` for ggplot2 NSE. Only needed if re-integrated files use bare `aes(x, y)` instead of `aes(.data$x, .data$y)`.
- [ ] **test-multilayer-labels.R** — Tests for `abbrev_label()` + label params in plot_mcml/mtna/mlna/htna. Re-integrate with Phase 3.
- [ ] **test-coverage-aes-scales-40.R** — Tests for aesthetic scaling in sidelined viz functions.

---

### Summary

| Phase | Files | ~LOC | Deps | Prerequisite |
|-------|-------|------|------|-------------|
| 1. Standalone Viz | 5 R + 6 test | 2870 | base R, ggplot2 | None |
| 2. Analysis | 2 R + 4 test | 700 | base R, igraph | None |
| 3. Multi-cluster Viz | 5 R + 7 test | 4030 | cluster-metrics | Phase 2a |
| 4. Animation | 1 R + 1 test | 250 | tna, gifski | None |
| 5. Motifs | 5 R + 3 test | 2990 | tna, igraph, ggplot2 | Phase 4a |
| Infra | 1 R + 2 test | 25 | — | As needed |
| **Total** | **19 R + 23 test** | **~10,865** | | |

---

## Pending Features

### Add Metadata Storage to CographNetwork

**Goal:** Add metadata storage capability to CographNetwork, allowing users to store and retrieve custom attributes for the graph, nodes, and edges - similar to igraph's attribute system.

**Changes needed in `R/class-network.R`:**

1. Add private field:
   ```r
   .metadata = list()
   ```

2. Add graph-level metadata methods:
   - `set_metadata(key, value)`
   - `get_metadata(key = NULL)`
   - `set_metadata_all(metadata)` - for cloning

3. Add node attribute methods:
   - `set_node_attr(name, value, index = NULL)`
   - `get_node_attr(name, index = NULL)`
   - `list_node_attrs()` - returns non-core column names

4. Add edge attribute methods:
   - `set_edge_attr(name, value, index = NULL)`
   - `get_edge_attr(name, index = NULL)`
   - `list_edge_attrs()` - returns non-core column names

5. Update `clone_network()` to copy metadata

6. Update `as_cograph_network()` to expose `obj$metadata`

**Changes needed in `R/cograph.R`:**

Add pipe-friendly convenience functions:
- `sn_set_meta(network, key, value)`
- `sn_get_meta(network, key = NULL)`

**Usage examples:**
```r
# Graph-level metadata
net <- cograph(adj) |>
  sn_set_meta("title", "My Network") |>
  sn_set_meta("created", Sys.Date())

sn_get_meta(net, "title")  # "My Network"

# Node attributes
net$network$set_node_attr("type", c("A", "B", "A"))
net$network$get_node_attr("type")
net$network$list_node_attrs()

# Edge attributes
net$network$set_edge_attr("confidence", c(0.9, 0.8, 0.7))
net$network$list_edge_attrs()
```

---

### Meta-Network Visualization (Network of Networks)

**Goal:** Visualize multiple networks as interconnected "super-nodes", where each super-node is a complete network rendered in a circular layout, and edges connect between these network-nodes.

**Use cases:**
- Multi-layer/multiplex networks
- Temporal network comparison (networks at different time points)
- Community visualization (each community as a mini-network)
- Hierarchical/nested network structures
- Cross-group relationships

**Proposed API:**
```r
# List of networks (each becomes a "super-node")
networks <- list(
  group_a = adj_matrix_a,
  group_b = adj_matrix_b,
  group_c = adj_matrix_c
)

# Inter-network connections (which networks connect to which)
meta_edges <- data.frame(
  from = c("group_a", "group_a", "group_b"),
  to = c("group_b", "group_c", "group_c"),
  weight = c(0.8, 0.5, 0.6)
)

# Plot the meta-network
splot_meta(
  networks = networks,
  meta_edges = meta_edges,
  outer_layout = "circle",      # Layout of network-nodes
  inner_layout = "circle",      # Layout within each network
  network_size = "auto",        # Size of each network circle (or by n_nodes)
  network_colors = NULL,        # Fill colors for network backgrounds
  meta_edge_style = "curved",   # Style for inter-network edges
  show_network_labels = TRUE,   # Show network names
  show_node_labels = FALSE      # Show individual node labels
)
```

**Visual concept:**
```
        ┌─────────┐
       ╱  Net A    ╲
      │  ●──●──●   │
       ╲   ╲ ╱    ╱
        └────┼────┘
             │
    ╭────────┴────────╮
    │                 │
┌───┴───┐         ┌───┴───┐
│ Net B │─────────│ Net C │
│ ●─●─● │         │ ●─●─● │
└───────┘         └───────┘
```

**Implementation approach:**
1. Compute outer layout positions for each network
2. For each network, compute inner layout and scale to fit within a circle
3. Draw network backgrounds (optional circles/boundaries)
4. Draw all inner network edges
5. Draw all inner network nodes
6. Draw meta-edges connecting between networks
7. Draw labels

**Variations:**
- `splot_nested()` - For hierarchical nesting (networks within networks)
- `splot_layers()` - For multi-layer networks stacked vertically
- `splot_temporal()` - For time-series of networks with transitions

---

### Multi-Layer Stacked Network Visualization

**Goal:** Visualize multiplex/multi-layer networks as stacked rectangular planes with inter-layer edges, creating a 2.5D perspective view.

**Use cases:**
- Multiplex networks (same nodes, different edge types per layer)
- Temporal networks (network snapshots at different time points)
- Multi-relational data (e.g., friendship + work + family networks)
- Brain connectivity across frequency bands
- Multi-modal transportation networks

**Visual concept:**
```
    Layer 3 (t=3)
    ┌─────────────────┐
    │  ●───●     ●    │
    │  │╲  │    ╱│    │
    │  ● ╲─●───● │    │
    └──┼──┼───┼──┼────┘
       │  │   │  │
    Layer 2 (t=2)
    ┌──┼──┼───┼──┼────┐
    │  ●──●───●──●    │
    │   ╲ │   │ ╱     │
    │    ╲●───●╱      │
    └─────┼───┼───────┘
          │   │
    Layer 1 (t=1)
    ┌─────┼───┼───────┐
    │  ●──●   ●──●    │
    │  │   ╲ ╱   │    │
    │  ●────●────●    │
    └─────────────────┘
```

**Proposed API:**
```r
# Option 1: List of adjacency matrices (same nodes across layers)
layers <- list(
  "Time 1" = adj_t1,
  "Time 2" = adj_t2,
  "Time 3" = adj_t3
)

# Inter-layer edges (optional - connects node i in layer j to node k in layer l)
inter_edges <- data.frame(
  from_layer = c(1, 1, 2),
  from_node = c(1, 3, 2),
  to_layer = c(2, 2, 3),
  to_node = c(1, 3, 2),  # Often same node across layers
  weight = c(1, 1, 1)
)

splot_multilayer(
  layers = layers,
  inter_edges = inter_edges,       # NULL = auto-connect same nodes
  layout = "spring",               # Layout algorithm (shared or per-layer
  shared_layout = TRUE,            # Use same node positions across layers
  layer_spacing = 0.3,             # Vertical spacing between layers
  perspective = 0.15,              # 3D perspective amount (0 = flat)
  tilt = 15,                       # Tilt angle in degrees
  layer_fill = c("#E3F2FD", "#FFF3E0", "#E8F5E9"),  # Layer background colors
  layer_alpha = 0.3,               # Layer background transparency
  layer_border = TRUE,             # Draw layer borders
  inter_edge_color = "gray50",     # Color for inter-layer edges
  inter_edge_style = "dashed",     # Style for inter-layer edges
  inter_edge_alpha = 0.5,          # Transparency for inter-layer edges
  show_layer_labels = TRUE,        # Show layer names
  label_position = "left"          # Where to place layer labels
)
```

**Auto-connect modes for inter-layer edges:**
```r
# Connect same nodes across adjacent layers
splot_multilayer(layers, inter_edges = "adjacent")

# Connect same nodes across all layers
splot_multilayer(layers, inter_edges = "all")

# No inter-layer edges
splot_multilayer(layers, inter_edges = NULL)

# Custom inter-layer edges
splot_multilayer(layers, inter_edges = my_inter_edges_df)
```

**Additional features:**
- `highlight_node_across_layers(node_id)` - Highlight a node and its inter-layer connections
- `layer_opacity` - Fade distant layers for depth perception
- `rotate_view(angle)` - Rotate the 3D perspective
- Side-by-side mode as alternative to stacked
- Animation to "flip through" layers

**Implementation approach:**
1. Compute base layout (shared or per-layer)
2. Apply perspective transformation to each layer's coordinates
3. Add vertical offset for layer stacking
4. Draw layers back-to-front (painter's algorithm):
   - Layer background/border
   - Intra-layer edges
   - Inter-layer edges (from current to lower layers)
   - Nodes
   - Labels
5. Handle occlusion with transparency or smart ordering

---

### Export/Import Functions

- `sn_export(net, "network.graphml")` - Export to GraphML, GML, DOT formats
- `sn_save_rds(net, "network.rds")` / `sn_load_rds("network.rds")` - Save/load cograph networks
- `sn_to_json(net)` / `sn_from_json()` - JSON serialization

---

### Interactive Visualization

- `sn_interactive(net)` - Interactive plot with plotly/htmlwidgets
- Node hover tooltips showing attributes
- Click to select/highlight nodes
- Zoom and pan controls

---

### Additional Layouts

- Sugiyama layout for DAGs
- Hierarchical/tree layout improvements
- Radial layout
- Arc diagram layout
- Bipartite layout improvements

---

### Network Operations

- `sn_subset(net, nodes)` - Extract subnetwork
- `sn_filter_nodes(net, condition)` - Filter by node attribute
- `sn_filter_edges(net, condition)` - Filter by edge weight/attribute
- `sn_merge(net1, net2)` - Combine networks
- `sn_reverse(net)` - Reverse edge directions

---

### Analysis Integration

- `sn_color_by_community(net)` - Auto-detect and color communities
- `sn_size_by_centrality(net, type)` - Size nodes by degree/betweenness/etc.
- `sn_highlight_path(net, from, to)` - Highlight shortest path
- `sn_highlight_neighbors(net, node)` - Highlight node neighborhood

---

### Visual Enhancements

- Edge bundling for dense networks
- Curved edge improvements (Bezier curves)
- Node images/icons
- Custom node shapes from SVG
- Gradient edge colors
- Edge labels with automatic placement to avoid overlap

---

### Legend and Annotation

- Improved automatic legend generation
- `sn_annotate(net, x, y, text)` - Add text annotations
- `sn_add_title(net, title, subtitle)` - Structured titles
- Scale bars for weighted edges

---

### Animation

- `sn_animate(net1, net2, frames)` - Animate between layouts
- `sn_animate_growth(net)` - Show network growing over time
- GIF/video export for animations
