# cograph 2.3.6

## Bug fixes

- Removed the `cluster_network()` alias for `summarize_network()`. It
  collided with `Nestimate::cluster_network()` — a completely different
  function (PAM clustering on sequence data, one network per cluster) —
  and the two silently masked each other depending on package attach
  order, producing confusing `unused arguments (k = ..., cluster_by =
  ...)` errors. Use `summarize_network()` (or its remaining short form
  `cnet()`) for matrix-to-cluster aggregation in cograph.

# cograph 2.3.5

## Documentation

- Added Sonsoles López-Pernas as co-copyright holder in `LICENSE`.
- README / docs wording fixes (e.g. "hyper order" → "higher-order").
- Introduction vignette no longer asserts a fixed centrality-measure
  count, which kept drifting as the measure set grew.

# cograph 2.3.4

## Bug fixes

- `.smooth_blob()` (used by `plot_simplicial()` and
  `overlay_communities()`) now guards `grDevices::chull()` against
  non-finite anchor coordinates. Previously a node lacking layout
  coordinates (NA/Inf) aborted the blob with "finite coordinates are
  needed"; such anchors are now dropped before the convex-hull step.

# cograph 2.3.3

## Documentation

- Aligned the `motifs()` / `subgraphs()` roxygen documentation with the
  post-audit behavior shipped in 2.3.2 (census `type_summary` counts,
  `min_count` handling, and corrected plot legend descriptions).

# cograph 2.3.2

## Bug fixes

- Full audit pass over the motifs subsystem: `type_summary` now holds
  real MAN-type counts in census mode, `min_count` is honored in census
  mode, and the swapped source/target color description in
  `plot.cograph_motif_result()` is corrected.
- Unbroke `motifs()` and `plot_simplicial()` on Nestimate-backed
  workflows (HON / HYPA sequence inputs).
- `panel_layout()`: tightened dimension validation and made the
  restoration claim honest — it now restores only the `par()` settings
  it actually changed.

# cograph 2.3.1

## Multi-panel layout control

- New `combined` argument (default `TRUE`) on every multi-panel plot
  function: `splot()` group-cascade, `plot_netobject_group()`,
  `plot_netobject_ml()`, `plot_net_bootstrap_group()`,
  `plot_group_permutation()`, `plot_compare()`, `splot.net_mlvar()`,
  `plot_network_evolution()`, `plot.cograph_motifs()`,
  `plot.cograph_motif_result()`, `plot.cograph_motif_analysis()`, and
  `plot.tna_disparity()`. With `combined = FALSE` these functions draw
  panels into the active device without calling `graphics::par(mfrow=...)`,
  so callers can drive their own layout (e.g. `graphics::layout()` or
  the new `panel_layout()` helper). Default `TRUE` preserves prior
  behavior — every existing call site renders identically.
- New `panel_layout()` helper sets up a custom multi-panel device layout
  for use with `combined = FALSE`. Accepts either a uniform-grid
  `c(nrow, ncol)` or a `graphics::layout()` matrix for non-uniform
  layouts (e.g. one wide panel + two narrow ones). Returns a `par()`
  snapshot for restoration via `on.exit()`.

## Test suite hygiene

- `test-coverage-splot-{41,42}.R`: bumped `n_nodes` from 4 to 10 in
  seven per-edge attribute tests so the seed=42 sampler does not
  produce duplicate (1,2) pairs that trip cograph's
  undirected-duplicate-edge detector.
- `test-coverage-class-network-41.R`: aligned the
  `set_layout_coords()` mismatched-row-count test with the strict
  input validation already enforced by `R/class-network.R`.
- `test-overlay-communities.R`: prefixed two `communities()` calls
  with `cograph::` to avoid `tna` masking when both packages are
  loaded in the suite (per CLAUDE.md "namespace masking" gotcha).

# cograph 2.3.0

## Documentation

- Audited every R/*.R function file for roxygen/Rd accuracy. Corrected
  stale defaults (`cr_color` `#D4820A` -> `#D4829A` in `plot-forest.R`;
  `show_value` default `FALSE` -> `TRUE` in `splot-nodes.R`), corrected
  dataset dimensions in `data-hai.R` (`302` -> `429 x 287`), corrected a
  reference to the nonexistent `igraph::is_bipartite()` (now
  `bipartite_mapping()`), expanded `centrality()` `@param` measure
  lists for `mode`, `cutoff`, `invert_weights`, and `membership` to
  match the implementation, dropped baked-in measure counts that rot
  on each addition, and removed nonexistent themes from `sn_theme`
  documentation. No runtime behavior changes from the documentation
  pass itself.

## Bug fixes

- `plot_simplicial()` now warns when `anomaly` is set on an input that
  has no anomaly concept (HON, association rules, link prediction,
  character pathways, `method = "hon"` / `"rules"`). Previously the
  argument was silently dropped, so calls like
  `plot_simplicial(hon, anomaly = "over")` and
  `plot_simplicial(hon, anomaly = "under")` produced byte-identical
  plots. `anomaly` is honored only for `net_hypa` inputs and
  `method = "hypa"` auto-builds.

## Centrality

- `centrality()` gains an umbrella argument `tna_network` (logical or
  NULL). When `TRUE` (or auto-detected from a `tna`/`group_tna`/`ctna`/
  `ftna`/`atna` input), all measures shared with `tna::centralities()`
  match byte-for-byte: `loops = FALSE`, `invert_weights = TRUE`,
  `diffusion_method = "power_series"`, `transitivity_type = "onnela"`.
  Side-by-side audit confirms zero divergence on `OutStrength`,
  `InStrength`, `ClosenessIn/Out/All`, `Betweenness`, `Diffusion`,
  `Clustering` (`max|diff| = 0`). Any per-argument override the user
  passes explicitly always wins over the umbrella.
- `centrality()` (and `centrality_diffusion()`) gain a
  `diffusion_method = c("kandhway_kuri", "power_series")` argument. The
  default `NULL` auto-detects: `"power_series"` for tna inputs (matches
  `tna::centralities(., measures = "Diffusion")` byte-for-byte when
  `loops = FALSE`), `"kandhway_kuri"` (the existing 1-hop binary-degree
  formula, Kandhway & Kuri 2014) for everything else. Previously
  cograph's diffusion silently disagreed with tna's because cograph used
  an unweighted neighborhood-degree sum while tna uses
  `rowSums(P + P^2 + ... + P^n)` on the diagonal-zeroed weighted matrix
  — the same name covered two different statistics. Set explicitly to
  override the auto-detect.

## Tests

- Added a regression test in
  `tests/testthat/test-validate-nestimate-bootstrap-permutation.R`
  asserting that `centrality()` on a Nestimate `netobject` agrees with
  `centrality()` on its `$weights` matrix when the diagonal is
  non-zero. Locks in the upstream Nestimate fix to
  `.extract_edges_from_matrix()` (Nestimate >= 2026-05-02) which now
  preserves self-loops in `$edges`. Without that fix, loop-bearing
  netobjects (e.g. `Nestimate::build_mcml() |> Nestimate::as_tna()`)
  silently under-counted node degree by 2.

## Plotting — edge-label cex coupling (Phase 2)

- Default `edge_label_size` is now coupled to the node label cex at a
  fixed 0.55 fraction (`edge_cex = 0.55 * mean(node_label_cex)`) so the
  node-to-edge-label ratio stays a stable ~1.82x across canvases. This
  replaces the previous `EDGE_LABEL_SCALE_CAP`-based compensation, which
  let the ratio drift from 2.5x at reference to 3.6x at poster canvases
  because edge labels were clamped to a tighter 1.6 ceiling while node
  labels scaled freely to 2.3. The visible effect: edge weight
  annotations are now readable at poster sizes instead of shrinking
  relative to node labels. User-explicit `edge_label_size` still wins
  and receives the same (capped) visual-scale compensation as before;
  only the default path changed.
- Edge-label visual_scale resolution moved from `render_edges_splot()`
  into `splot.R` so the final cex is produced in a single place.

## Plotting — device-aware visual scaling

- `splot()` now applies device-dependent compensation to text, line, and
  point sizes so visual ratios (label-to-node, legend-to-plot, edge
  thickness) stay consistent when the output device changes. This fixes the
  long-standing "labels too big at high DPI" and "legend desynchronised from
  the plot" issues when saving PNGs at `res = 300` or `res = 600` with
  pixel-default `width`/`height`, and when resizing the RStudio plot pane.
  Implementation: a single `compute_visual_scale()` reads the active
  device's canvas size (`dev.size("in")`) and returns multipliers keyed off
  a 5.9-inch reference (matching the default RStudio 7×5" pane so
  backward-compatible behaviour at the default canvas is preserved).
  Multipliers are clamped to `[0.55, 1.9]` to keep thumbnails and posters
  legible. See the new `R/visual-scale.R`.
- New `scaling = "fixed"` mode on `splot()` — and corresponding global
  option `options(cograph.visual_scaling = FALSE)` — disables device
  compensation for reproducibility-sensitive workflows that calibrated
  against the previous behaviour.
- `splot()` return value now carries two attributes for downstream tooling:
  `cograph.visual_scale` (the multiplier list) and `cograph.node_diam_in`
  (the representative node diameter in inches at the rendered device).
- The splot-internal `render_legend_splot()` plus the new shared
  `.render_legend_base()` (`R/render-legend-shared.R`) replace the ad-hoc
  legend cex/pt.cex handling with a single compensated path. `plot_htna`,
  `plot_mtna`, `plot_mlna`, `plot_mcml` still use their historical scale
  multiplier arguments; Phase 2 will migrate them to the shared helper.

## Plotting

- `splot.netobject` now routes on the Nestimate `$method` slot rather than
  just direction. Undirected sequence-based networks from `build_cna()` and
  `wtna(method = "cooccurrence")` get oval TNA-family styling (layout,
  palette, donuts) with arrows and dotted edge starts automatically dropped
  because the matrix is symmetric. Glasso / cor / pcor / ising networks
  still get `psych_styling = TRUE` (spring layout, Okabe-Ito palette).
- `from_tna()` auto-detects integer-valued weight matrices (ftna, ctna, raw
  counts) and sets `weight_digits = edge_label_digits = 0` so edge labels
  render as `2304` rather than `2304.00`. Fractional weights still format
  to two decimals. Explicit user-supplied `weight_digits` still wins.
- `psych_styling = TRUE` is now exported as a first-class styling preset
  (undirected counterpart of `tna_styling`) — Okabe-Ito palette, spring
  layout, no arrows — applied by default to `splot.netobject` on
  correlation-family input and to the `$contemporaneous` / `$between`
  constituents of `net_mlvar`.
- Expanded `splot()` dispatch coverage across the tna and Nestimate class
  hierarchies, ensuring `tna`, `ftna`, `ctna`, `group_tna`, `tna_bootstrap`,
  `group_tna_bootstrap`, `tna_permutation`, `group_tna_permutation`,
  `netobject`, `netobject_group`, `netobject_ml`, `net_mlvar`, `wtna_mixed`,
  `net_bootstrap`, `net_permutation`, `boot_glasso`, `mcml`, `net_hon`,
  `net_hypa`, and `simplicial_complex` all reach the correct renderer.
- Self-loops are now preserved in every plot function.

## Correctness fixes (audit-driven)

- `detect_duplicate_edges()`, `aggregate_duplicate_edges()`,
  `simplify.cograph_network()`, and the internal `check_duplicate_edges()`
  helper now respect directed vs undirected semantics. Previously the
  canonical (min/max) endpoint key collapsed `A -> B` and `B -> A` into one
  edge even on directed graphs, matching `igraph::simplify()` ground truth.
- `.compute_modularity()` replaces a nested for loop with cluster-wise
  vectorization (`sum(A[idx, idx]) - sum(k_out[idx]) * sum(k_in[idx]) / m`),
  per the project "no for loops" rule. Results verified bit-exact against
  `igraph::modularity()`.
- `is_directed()` now recognises `CographNetwork` R6 objects — previously
  only the `cograph_network` list format dispatched correctly.
- `compute_layout_for_cograph()` uses `layout$get_type()` instead of the
  removed `$name` field on `CographLayout`.
- `network_small_world()` returns `0` (valid: no triangles means
  definitively not small-world) instead of `NA_real_` when the observed
  clustering coefficient is zero but path length is finite.
- `simplify.cograph_network()` threads the directed flag through to edge
  aggregation so directed multigraphs collapse correctly.

## Performance & documentation

- `simplify()` performance refactor for large networks plus a cleaner
  title-composition path.
- `motifs()`, `extract_motifs()`, and `plot.cograph_motif_analysis`
  examples reworked to use `n_perm = 10L` (or `significance = FALSE`) and
  promoted from `\dontrun` to CRAN-runnable (optional tna branches stay in
  `\donttest`). Retires 320 seconds of latent CRAN timing risk — every
  example now runs in under 4 seconds.

## New tests

- `test-audit-fixes.R` — ground-truth regressions for the directed edge
  semantics, modularity vectorization, and small-world behaviour changes.
- `test-integer-weight-labels.R` — locks `from_tna()` integer-weight
  auto-detect behaviour and precedence of explicit `weight_digits`.
- `test-equiv-{assortativity, cluster-quality, communities, disparity,
  edge-centrality, network-summary, robustness, standalone-measures}.R` —
  numerical equivalence against igraph, sna, centiserve, brainGraph,
  influenceR, tidygraph, and NetworkX. Gated by
  `skip_coverage_tests() + skip_on_cran()`, so they do not run on the
  CRAN pipeline.

# cograph 2.1.0

## New Features

### Batch 6 — new-API graph-level / set-level / pair-level measures

These measures don't fit the per-node `centrality()` data frame, so they live as standalone functions:

- `estrada_index()` — graph-level spectral invariant: \eqn{EE(G) = \sum_i e^{\lambda_i}}, equal to the trace of the matrix exponential of the adjacency. Equivalently, the sum of `subgraph_centrality()` across all nodes. Matches `networkx.estrada_index` at machine epsilon (max relative diff ~5e-15 across random test graphs).
- `trophic_incoherence()` — graph-level food-web stability measure (Johnson et al. 2014). Defined as the population standard deviation of per-edge trophic differences \eqn{s_v - s_u} where \eqn{s_i} is the trophic level of node \eqn{i}. Zero for perfectly coherent DAGs (e.g., a pure chain). Matches `networkx.trophic_incoherence_parameter` at machine epsilon. Directed-only; reuses the existing `trophic_level` calculator.
- `group_centrality(x, nodes, measure = c("betweenness", "closeness", "degree"))` — Everett-Borgatti (1999) group centrality for a *set* of nodes. Returns a scalar. Supports `mode = "in"/"out"` for directed-degree variants. **Group closeness and group degree** match `networkx.group_*_centrality` bit-exact. **Group betweenness** implements the textbook Everett-Borgatti / Puzis 2008 definition (fraction of shortest paths passing through at least one node in the group), which diverges from `networkx.group_betweenness_centrality` on some graphs due to a known quirk in NetworkX's Puzis-Yahalom-Elovici iterative algorithm. Verified via an independent Python brute-force: cograph matches the textbook definition; NX produces larger values on graphs with many overlapping shortest paths. Documented in the roxygen "Divergence from NetworkX" section.
- `dispersion(x, u = NULL, v = NULL, normalized = TRUE, alpha = 1, b = 0, c = 0)` — Backstrom-Kleinberg (2014 Facebook) pair-level measure of tie strength. Counts the number of "well-dispersed" mutual friends of `u` and `v` (pairs of common neighbors that are not directly connected and share no common neighbor inside `u`'s ego network other than `u` and `v`). Matches `networkx.dispersion` bit-exact across all 156 edges on the karate club graph. Returns a scalar, named vector, or data frame depending on which of `u`, `v` are specified.

### Centrality Batch 5 — Gould-Fernandez brokerage (5 roles)

Added the five Gould-Fernandez (1989) brokerage role counts, a foundational measure in social network analysis (~1500 citations). Each role is a separate per-node measure requiring a `membership` argument (following the same pattern as `participation`, `within_module_z`, `gateway`), and counts open directed 2-paths `a -> v -> c` through broker `v`:

- `centrality_brokerage_coordinator()` — all three in broker's group (w_I)
- `centrality_brokerage_itinerant()` — endpoints same group, broker different (w_O, "consultant")
- `centrality_brokerage_representative()` — broker + source same, target different (b_IO)
- `centrality_brokerage_gatekeeper()` — broker + target same, source different (b_OI)
- `centrality_brokerage_liaison()` — all three in different groups (b_O)

Bit-exact match against `sna::brokerage$raw.nli` for all five roles across 20 random directed graphs. Implemented natively (no runtime dependency on sna). Key implementation detail: the Gould-Fernandez counting rule requires **open 2-paths only** — triads where a direct edge `a -> c` already exists are excluded. This matches sna's C implementation exactly and was derived empirically (sna's `.C("brokerage_R", ...)` has no R-level source).

Directed-only; warns and returns `NA` on undirected input.

### Centrality Batch 4 — directed prestige family (Wasserman-Faust / sna)

- `centrality_prestige_domain()` — directed-graph prestige measure: for each node \eqn{v}, the number of other nodes that can reach \eqn{v} via a directed path. Classical Wasserman-Faust (1994) measure from `sna::prestige(cmode = "domain")`. Bit-exact match against sna, implemented natively via `igraph::distances(mode = "out")` + `colSums(is.finite(D)) - 1` (no runtime dependency on sna). Directed-only; returns NA with a warning on undirected input.
- `centrality_prestige_domain_proximity()` — distance-weighted variant: `R_v^2 / (D_v * (n - 1))` where `R_v` is the number of reachers and `D_v` is the sum of their geodesic distances to `v`. Bit-exact match against `sna::prestige(cmode = "domain.proximity")` on strongly connected directed graphs. On graphs with any unreachable pair, sna has a known bug (`FALSE * Inf = NaN` collapses the denominator, producing all-zero output); cograph's `is.finite()`-masked formula produces mathematically correct values on any directed graph. Directed-only.

### Centrality Batch 3 — classical measures with reference-package validation

- `centrality_katz()` — Katz (1953) status index. Bit-exact match against `centiserve::katzcent` (cograph mirrors centiserve's exact LAPACK call sequence). Also matches `igraph::alpha_centrality(exo = 1)` and `networkx.katz_centrality_numpy` at machine epsilon. New `katz_alpha` parameter (default 0.1).
- `centrality_hubbell()` — Hubbell (1965) input-output centrality. Bit-exact match against `centiserve::hubbell` (cograph mirrors centiserve's full-inverse LAPACK call path). Note: centiserve's default (`weights = NULL`) silently ignores `E(g)$weight`; to reproduce cograph's behavior with centiserve on weighted graphs, pass `weights = igraph::E(g)$weight` explicitly. New `hubbell_weight` parameter (default 0.5).
- `centrality_information()` — Stephenson-Zelen (1989) information centrality. Bit-exact match against `sna::infocent` on connected undirected graphs (cograph mirrors sna's exact construction and `solve()` call sequence).
- `centrality_pairwisedis()` — Pairwise disconnectivity (Potapov et al. 2008). Directed-only; fraction of reachable ordered pairs that become unreachable when each node is removed. Bit-exact match against `centiserve::pairwisedis`. Warns and returns `NA` on undirected input, matching the convention used by `salsa`, `leaderrank`, and `trophic_level`.
- `centrality_reaching_local()` / `reaching_global()` — Local and global reaching centrality (Mones, Vicsek & Vicsek 2012). Bit-exact match against `networkx.local_reaching_centrality` across the directed unweighted, undirected unweighted, and weighted branches. Undirected unweighted LRC coincides with `igraph::harmonic_centrality(normalized = TRUE)` (documented). `reaching_global()` is a graph-level hierarchy statistic in [0, 1].

# cograph 1.8.2

## New Features

- `plot_simplicial()` now accepts `tna`, `netobject`, `net_hon`, and `net_hypa` objects directly — higher-order pathways are auto-built and visualized with proper state labels, no manual extraction needed. New parameters: `method` (`"hon"` / `"hypa"`), `max_pathways`, `ncol`. Dismantled mode uses `gridExtra` grid layout with scaled nodes
- `print.cograph_network()` now shows a structured summary: node/edge counts, density, reciprocity, weight range, and top-degree nodes — replacing the minimal R6 default output
- Added `mcml` S3 class with `as_mcml()` generic for type-safe handling of Markov Chain Multi-Level models — enables `print()`, `plot()`, and method dispatch on MCML objects
- Added local `%||%` operator for R 4.1 compatibility (no longer requires R 4.4+)

## Breaking Changes

- MCML field names renamed for clarity: `$between` → `$macro`, `$within` → `$clusters`
- `as_tna()` on MCML objects now returns a flat `group_tna` list instead of a nested structure

## Bug Fixes

- `plot_mcml()` now suppresses zero-weight edges instead of drawing invisible lines, and strips leading zeros from edge labels (`.32` instead of `0.32`)
- Self-loops in `cluster_summary()` are now preserved in the macro diagonal, reflecting intra-cluster retention rates
- Sequence data is properly propagated through the full tna → macro → cluster pipeline, so downstream models can use bootstrap and permutation tests

# cograph 1.8.0

## New Features

- Added `overlay_communities()` for drawing community blob overlays on any network plot — accepts method names, membership vectors, or pre-computed community objects
- Added `plot_simplicial()` for higher-order pathway visualization, rendering simplicial complexes as smooth blobs with flexible separators and a dismantled view option
- Added `value_nudge` parameter to `plot_transitions()` for controlling the distance between flow labels and nodes
- Added bundle legend label controls: `bundle_legend_size`, `bundle_legend_color`, `bundle_legend_fontface`, `bundle_legend_position`
- Added per-function label controls (`label_size`, `label_color`, `label_fontface`, `label_hjust`) to `plot_transitions()`, `plot_trajectories()`, and `plot_alluvial()`

## Bug Fixes

- Fixed spiky text halo artifacts in transition and heatmap plots by increasing circular offset directions from 8 to 16 (22.5° spacing for smooth outlines)

# cograph 1.7.0

## New Features

### Cluster Analysis
- Added `cluster_summary()` for aggregating network weights at the cluster level, producing between-cluster and within-cluster matrices from raw transition data
- Added `build_mcml()` for constructing Markov Chain Multi-Level models from edge lists or sequence data with automatic cluster detection
- Added `cluster_quality()` for modularity-based cluster quality metrics and `cluster_significance()` for permutation-based significance testing
- Added `as_tna()` to convert cluster summaries to TNA objects for bootstrapping, permutation testing, and plotting with `splot()`

### Network Operations
- Added `simplify()` for pruning weak edges from networks, with configurable weight threshold and aggregation method
- Added `disparity_filter()` for backbone extraction (Serrano et al. 2009), with methods for matrices, tna, igraph, and cograph_network objects
- Added `robustness()` for network robustness analysis with targeted (betweenness, degree) and random attack strategies, plus `ggplot_robustness()` for faceted ggplot2 output
- Added `temporal_edge_list()` for converting sequence data to timestamped edge lists
- Added `supra_adjacency()`, `supra_layer()`, `supra_interlayer()` for multilayer supra-adjacency matrix construction
- Added `layer_similarity()`, `layer_similarity_matrix()`, and `layer_degree_correlation()` for comparing layers in multilayer networks
- Added `aggregate_weights()` and `aggregate_layers()` for weight aggregation across layers
- Added `verify_with_igraph()` for cross-validating cograph centrality and network metrics against igraph

### Motif Analysis
- Added `motifs()` / `subgraphs()` as a unified API for triad census (node-exchangeable counts) and instance extraction (named node triples), with auto-detection of actor/session columns, rolling/tumbling window support, and exact configuration model significance testing

### Visualization
- Added `plot_mcml()` for Markov Chain Multi-Level visualization showing between-cluster summary edges alongside within-cluster detail, with pie charts, self-loops, and 22 customization parameters
- Added `plot_chord()` for native chord diagrams with automatic weight-based arc sizing
- Added `plot_time_line()` for cluster membership timeline visualization
- Added `plot_htna()` orientations: `"facing"` (tip-to-tip columns) and `"circular"` (two semicircles), plus `intra_curvature` for drawing intra-group edges as dotted bezier arcs
- Added `threshold` parameter to all plot functions for filtering edges/cells below a minimum absolute weight
- Added `value_fontface`, `value_fontfamily`, and `value_halo` parameters to `plot_heatmap()` for text styling control
- Added directional shorthands for `scale_nodes_by`: `indegree`, `outdegree`, `instrength`, `outstrength`, `incloseness`, `outcloseness`, `inharmonic`, `outharmonic`, `ineccentricity`, `outeccentricity`
- Added `scale_nodes_scale` parameter to `splot()` for dampening (< 1) or exaggerating (> 1) centrality-based node sizing differences
- Added qgraph argument translation in `splot()`: when plotting tna objects, qgraph-style parameters (`vsize`, `asize`, `edge.color`, `lty`, `shape`) are automatically mapped to cograph equivalents

### Transition Plot Enhancements
- Added intermediate labels with `node_label_format` (e.g., `"{state} (n={count})"`) for showing counts on transition plot nodes
- Added line bundling via `bundle_size` for aggregating individual trajectories into weighted summary lines in large datasets
- Added flow value labels via `show_values` / `value_position` for displaying transition counts on flow lines
- Added `label_position` consistency across ALL columns (first, middle, last) in trajectory plots

### Data & Infrastructure
- Added example datasets: `gamer_data`, `group_engagement`, `srl_data`
- Added `set_node_groups()` / `get_node_groups()` for managing cluster assignments on cograph_network objects
- Consolidated cograph_network metadata under `$meta` with getter/setter functions
- Added `group_tna` support to `splot()` for direct plotting of grouped TNA models
- Gave each `centrality_*` wrapper its own focused help page

## Bug Fixes

- Fixed load and percolation centrality computation: the BFS assumed unit edge weights, causing infinite loops on weighted graphs; directed graphs now transpose correctly (matching sna convention); disconnected nodes no longer contribute spurious centrality
- Fixed self-loop and edge clipping in `splot()` viewport calculation
- Fixed argument forwarding in splot dispatch for bootstrap/permutation objects — named parameters (minimum, threshold, layout, title) were consumed by `splot()`'s signature and silently dropped when dispatching
- Fixed overlapping flow value labels in multi-step alluvial plots
- Fixed alluvial label halo rendering producing spike artifacts (8 → 16 offset directions)
- Fixed viridis palette direction in `plot_heatmap()` so high values get dark colors
- Fixed `build_mcml()` density method crash when weight vector had no names
- Fixed display label priority resolution (labels > label > identifier)
- Removed zero-value labels that appeared after rounding in transition plots

## Improvements

- Simplified splot dispatch: extracted `.collect_dispatch_args()` helper to replace 6 copy-paste dispatch blocks, using `match.call()` + `mget()` for reliable argument capture

# cograph 1.6.0

## New Features

### Centrality
- Added `centrality()` with 23 measures and individual wrappers: degree, strength, betweenness, closeness, eigenvector, pagerank, harmonic, authority, hub, alpha, power, kreach, diffusion, percolation, eccentricity, transitivity, constraint, coreness, load, subgraph, leverage, laplacian, current-flow betweenness, current-flow closeness, voterank
- Added `edge_betweenness()` for edge-level centrality
- Added automatic weight inversion for path-based measures when working with tna transition matrices (where higher weight = stronger connection, not shorter distance)

### Community Detection
- Added `detect_communities()` with 11 algorithms: louvain, walktrap, fast_greedy, label_propagation, leading_eigenvector, infomap, spinglass, leiden, optimal, edge_betweenness, multilevel — plus `com_*` shorthand aliases
- Added consensus clustering and `cluster_significance()` for permutation-based validation

### Network Metrics
- Added `network_summary()` and `summarize_network()` for computing comprehensive network-level statistics (density, reciprocity, transitivity, diameter, components, degree distribution)

### Visualization
- Added `plot_transitions()` for alluvial/Sankey flow diagrams, with `plot_alluvial()` and `plot_trajectories()` wrappers
- Added `plot_bootstrap()` and `plot_permutation()` for significance-styled visualization of bootstrap and permutation test results — significant edges rendered solid on top, non-significant edges dashed behind
- Added `plot_mixed_network()` for overlaying symmetric (undirected, straight) and asymmetric (directed, curved) edges on the same network
- Added `plot_heatmap()` for adjacency matrix heatmaps with optional hierarchical clustering and `plot_ml_heatmap()` for multilayer 3D perspective heatmaps
- Added `plot_compare()` for difference network visualization showing edge-weight changes between two networks
- Added `splot()` S3 methods for `tna_bootstrap` and `tna_permutation` objects

### Motif Analysis
- Added `motif_census()`, `triad_census()`, and `extract_motifs()` for triad motif analysis with pattern filtering, significance testing, and network diagram visualization

### Network Utilities
- Added `filter_edges()`, `subset_edges()`, `select_nodes()`, `select_edges()` for flexible network subsetting
- Added `set_groups()` for storing cluster assignments on cograph_network objects with automatic dispatch to `plot_htna()` / `plot_mtna()`

### Infrastructure
- All plot functions now accept `cograph_network` objects as input, in addition to matrices, igraph objects, and tna objects
- Layout computation is now lazy — coordinates are only calculated when first needed
- Improved `layout_spring` and `layout_gephi_fr` algorithms: vectorized attraction forces, edge aggregation for dense networks
- Renamed package from Sonnet to cograph

## Bug Fixes

- Fixed `par(pin)` error on exit when plot device state was corrupted
- Fixed motif plot scaling and margins for different device sizes

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

- `edge_label_fontface` now accepts string values ("plain", "bold", "italic", "bold.italic") in addition to numeric values

# cograph 1.4.0

## New Features

- Added `mlna()` for multilevel network visualization with 3D perspective
- Added `mtna()` for multi-cluster network visualization with shape-based cluster containers
- Added `plot_htna()` for hierarchical multi-group network layouts with polygon and circular arrangements
- Added `tplot()` as a qgraph drop-in replacement with automatic parameter translation
- Added `arrow_angle` parameter for customizable arrowhead geometry

## Bug Fixes

- Fixed Rd cross-reference warning in splot documentation
- Fixed pie/donut segment divider lines rendering when border width is 0

# cograph 1.3.1

## New Features

- Added `edge_start_dot_density` parameter for TNA-style dotted edge starts indicating direction
- Added direct support for tna objects via `from_tna()` — no manual matrix extraction needed
- Added direct support for statnet `network` and `qgraph` objects as input
- Added auto-conversion of `pie_values` vector to `donut_fill` when all values are in [0,1]

## Bug Fixes

- Fixed TNA visual defaults being silently overwritten in `splot()` when other parameters were specified
- Fixed self-loop edge labels overlapping the loop arc
- Fixed `donut_shape` validation rejecting custom SVG shapes
- Fixed title clipping when title text exceeded plot margins
- Fixed edge rendering crash on certain edge/node configurations
- Removed underscore prefix requirement for custom SVG shape names

# cograph 1.2.7

## Bug Fixes

- Fixed oversized nodes in `from_qgraph()` when a layout override was provided
- Fixed oval layout using independent axis scaling, which distorted aspect ratios — now uses uniform scaling via `normalize_coords()`
- Fixed edge label alignment in `from_qgraph()` by using a matrix intermediary for per-edge vector reordering
- Fixed `nrow(el)` crash: qgraph's Edgelist is a list, not a data.frame
- Fixed oval layout node distortion and donut fill values when converting from qgraph

# cograph 1.2.6

## New Features

- Added `donut_empty` parameter for rendering unfilled donut nodes
- Added `from_qgraph()` for converting qgraph objects to cograph format, reading resolved `graphAttributes` for accurate parameter extraction

## Bug Fixes

- Fixed oval `layout_info` guard causing errors on certain device configurations
- Fixed curvature extraction passing vector values instead of scalars

# cograph 1.2.0

## New Features

- Added `soplot()` for grid/ggplot2-based network plotting — full feature parity with `splot()` using a different rendering backend
- Added `layout_oval()` for oval/elliptical node arrangements
- Added `layout_scale` parameter to expand or contract the network layout, with `"auto"` mode for node-count-based scaling
- Added Gephi-style Fruchterman-Reingold layout algorithm
- Added `edge_start_style` parameter for visually indicating edge direction via styled start segments (dashed, dotted)

## Bug Fixes

- Fixed `soplot()` curve direction and edge defaults diverging from `splot()` behavior
- Fixed `rescale_layout` distorting oval aspect ratios by switching to uniform scaling
- Fixed edge scaling producing abnormally thick edges on small networks
- Fixed `par(pin)` restoration error on plot device exit

# cograph 1.1.0

## New Features

- Added `splot()` — a base R graphics engine for network visualization using `polygon()`, `lines()`, and `xspline()`, providing better performance than grid-based rendering for large networks
- Added polygon-shaped donut nodes, custom SVG node shapes, and AI-generated shape support
- Added shadow/halo labels and fine-grained text control (fontface, fontfamily, hjust, vjust, angle)
- Added double donut nodes with separate inner/outer border controls
- Added edge CI (confidence interval) underlays and template-based edge labels
- Added comprehensive legend support: groups, edge color scales, and node size scales
- Added high-resolution output via `sn_save()` with configurable DPI
- Added edge curve modes, bidirectional arrows, self-loop rotation, and per-edge curve control

## Bug Fixes

- Fixed donut rendering producing artifacts and simplified the `donut_color` API to accept 1 color (fill), 2 colors (fill + background), or n colors (per-node)
- Fixed arrow positioning and curve direction for qgraph-style edges
- Fixed edge label positioning to avoid overlap with edge lines
- Fixed self-loop rendering to use qgraph-style circular arcs
- Fixed arrow placement on non-square viewports
- Fixed reciprocal edge auto-separation causing edge crossings

# cograph 1.0.0

- Initial release of cograph network visualization package
