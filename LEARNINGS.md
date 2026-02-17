# Project Learnings

### 2026-02-17
- [cograph_network restructuring]: Field paths changed in the lean cograph_network object:
  - `$source` → `$meta$source`, `$tna` → `$meta$tna`, `$layout_info` → `$meta$layout`
  - `$layout`, `$layers`, `$clusters`, `$groups` removed as top-level fields
  - `.create_cograph_network()` now takes `meta = list()` instead of separate `source`, `tna`, `layout_info`, `layers`, `clusters`, `groups` params
  - Layout coordinates stored in `$nodes$x` and `$nodes$y`, not `$layout`
  - New `$data` field stores original estimation data (sequence matrix from tna, edge list df, or NULL)
  - New exported getters: `get_source()`, `get_data()`, `get_meta()`
  - Updated 31 files: 7 R source, 3 man pages, 14 test files, NAMESPACE, LEARNINGS
- [print method rewrite]: `print.cograph_network` now uses getters: `n_nodes(x)`, `n_edges(x)`, `is_directed(x)`, `get_edges(x)`, `get_nodes(x)`. Old formats (attr-based, R6 wrapper, fallback "Cograph network object") are removed. Test fake objects must have `$nodes` (df), `$edges` (df with weight), `$directed` (logical), optional `$meta`, `$data`.
- [mock tna objects for testing]: Replaced all `skip_if_not_installed("tna")` guards in `test-coverage-input-tna-40.R` with mock constructors `mock_tna()` and `mock_group_tna()`. tna structure is simple: `list(weights, labels, inits, data)` with class `c("tna", "list")`. group_tna: named list of tna objects with class `c("group_tna", "list")`. This gives 100% coverage without optional deps.
- [covr non-exported functions]: `covr::package_coverage(type = "none", code = ...)` with `library(pkg)` only exposes exported functions. Non-exported functions (like `parse_tna`, `.create_cograph_network`) aren't accessible to tests. Fix: add `attach(loadNamespace("cograph"), name = "cograph_ns", warn.conflicts = FALSE)` to the covr code string.
- [CographLayout in as.character]: `compute_layout_for_cograph()` must type-check `layout` before calling `as.character()` — CographLayout R6 objects are environments and `as.character(env)` errors. Use `if (inherits(layout, "CographLayout")) layout$name` instead.

### 2026-02-16
- [motifs refactor]: Split `R/motifs.R` (3,220 lines) into 5 files totaling 2,988 lines:
  - `R/motifs-data.R` (127 lines) - shared constants: triad patterns (2 versions), MAN descriptions, pattern filters, cache env, ggplot theme helper
  - `R/motifs.R` (767 lines) - core: motif_census, triad_census, extract_triads, get_edge_list + classify/lookup/count helpers
  - `R/motifs-extract.R` (611 lines) - extract_motifs + print/plot methods
  - `R/motifs-plot.R` (435 lines) - shared viz helpers: .plot_triad_networks, .plot_motif_patterns, .plot_motifs_bar/heatmap/network, arrow helpers
  - `R/motifs-temporal.R` (1,048 lines) - extract_motifs_temporal, triad_persistence + print/plot methods + helper functions
- [triad patterns]: Two distinct versions exist - visual (column-major, no byrow) for plotting and canonical (byrow=TRUE) for igraph-verified lookup classification. Must not unify them.
- [for-loop vectorization]: Converted ~12 for-loops to lapply/vapply/vectorized ops. Skipped: .build_triad_lookup (cached, 64 iters), plotting render loops (sequential), permutation test loop (complex random stream dependency).
- [ggplot theme dedup]: Created `.motifs_ggplot_theme(base_size)` returning `theme_minimal + bold title` as shared base; each plot adds specific overrides on top.

### 2026-02-15
- [as_tna refactored]: **Breaking change** - `as_tna()` now returns `cluster_tna` object with both between and within tna models:
  - `$between`: tna object for cluster-level transitions (uses `tna::tna()`)
  - `$within`: Named list of tna objects for each cluster's internal network
  - Single-node clusters and clusters with zero-row nodes are excluded from `$within`
  - Requires tna package to be installed
  - Has `print.cluster_tna()` method showing summary
- [cluster_summary diagonal]: Between-cluster matrix diagonal now contains within-cluster totals (self-loops at cluster level):
  - Diagonal represents within-cluster transition weight
  - When `type = "tna"`, diagonal = within-cluster retention rate
  - `verify_with_igraph()` zeros out both diagonals for comparison (igraph doesn't include self-loops)
- [mcml_demo.html]: Created polished HTML documentation at `docs/mcml_demo.html`:
  - Dark theme matching transitions_demo.html style
  - Cards with images for visualizations
  - Extensive parameter documentation for cluster_summary(), as_tna(), plot_mcml()
  - Method and type recommendation tables
  - Full return structure documentation
- [extensive roxygen docs]: Added comprehensive roxygen2 documentation to cluster_summary() and as_tna():
  - All parameters documented with examples and use cases
  - Return value structure fully documented
  - Workflow examples included
  - @seealso links added

- [mcml showcase]: Created comprehensive HTML showcase for MCML functions (`tmp/mcml_showcase.html`):
  - Documents `cluster_summary()`, `plot_mcml()`, `plot_mtna()`, and helper functions
  - Includes code examples with visual outputs for all major parameters
  - Shows aggregation methods, type parameter effects, layout options
  - Fixed community detection example to use `walktrap` instead of `louvain` (louvain only works with undirected graphs)
  - Source: `tmp/mcml_showcase.Rmd`
- [cluster_summary simplified]: **Major refactor** - cluster_summary now uses clean, non-redundant structure:
  - **New structure**: `$between` (weights, inits), `$within$X` (weights, inits per cluster), `$clusters`, `$meta` (type, method, directed, n_nodes, n_clusters, cluster_sizes)
  - **Removed duplicates**: `$tna`, `$inits`, `$between_weights`, `$within_tna`, `$within_inits`, `$summary` - all replaced by new structure
  - **New `type` parameter**: `"tna"` (default, row-normalized), `"cooccurrence"`, `"semi_markov"`, `"raw"` (no normalization)
  - **mcml() is now a wrapper**: Returns cluster_summary with backward compatibility
  - **summarize_network() uses type="raw"**: For compatibility with igraph aggregation
  - **verify_with_igraph defaults to type="raw"**: To match igraph's contract+simplify output
  - **plot_mcml/plot_mtna accept cluster_summary directly**: Can pre-compute and reuse
  - **Tests updated**: Use `$between$weights`, `$between$inits`, `$within$X$weights`, `$meta$method`, etc.
  - **tna masking issue**: Use `cograph::plot_compare()` in tests when tna package is loaded

- [mcml enhanced structure]: Enhanced `mcml()` with group_tna-like structure:
  - **New `$summary` field**: Organized view of between-cluster data with `tna`, `weights`, `inits`, `labels`. Points to same data as top-level fields.
  - **New `$within` field**: Named list of per-cluster TNA data. Each element has `tna` (row-normalized), `weights` (raw), `inits`, `labels`.
  - **New `within` parameter**: Set `within = FALSE` to skip within-cluster computation.
  - **100% backward compatible**: All top-level fields (`$tna`, `$inits`, `$between_weights`) unchanged.
- [plot_mcml mode parameter]: Added `mode` parameter to `plot_mcml()`:
  - `mode = "weights"` (default): Shows raw aggregated weights on edge labels.
  - `mode = "tna"`: Shows transition probabilities (row-normalized) on edge labels.
  - Uses `$within[[cluster]]$tna` for within-cluster edge labels when `mode = "tna"`.
- [NA handling in mcml]: Fixed NA handling in within-cluster TNA computation using `na.rm = TRUE` for `rowSums`/`colSums`.
- [cluster_summary aligned with mcml]: Updated `cluster_summary()` to have the same organized structure as `mcml()`:
  - **New `$summary` field**: When `between = "summary"`, includes `tna`, `weights`, `inits`, `labels`.
  - **New `$within` unified field**: When `within = "nodes"`, each cluster element has `tna`, `weights`, `inits`, `labels`.
  - Legacy `$within_tna` and `$within_inits` kept for backward compatibility.


- [cluster_summary modes]: Enhanced `cluster_summary()` with `within` and `between` parameters for flexible output granularity:
  - `within = "nodes"` (default): Returns LIST of per-cluster TNA matrices in `within_tna`. Each cluster gets its own n_i × n_i row-normalized transition matrix. `within_inits` is also a list.
  - `within = "summary"`: Returns only scalar aggregates per cluster in `within_weights`. No `within_tna` or `within_inits`.
  - `within = "none"`: Skips within-cluster computation entirely.
  - `between = "summary"` (default): Returns k × k cluster-to-cluster TNA in `tna` and `inits`.
  - `between = "nodes"`: Returns n × n matrix with only cross-cluster edges in `between_tna`. Within-cluster blocks are zeroed.
  - `between = "none"`: Skips between-cluster computation.
  - **Breaking change**: `within_tna` changed from n×n block-diagonal matrix to LIST of per-cluster matrices (cleaner, more useful).
- [mcml refactor]: Separated data extraction from plotting. `mcml()` is now a pure data extraction function that returns a `cograph_mcml` object. `plot_mcml()` uses `mcml()` internally and returns the data invisibly. This follows cograph's established pattern: extraction -> transformation -> visualization.
- [plot_mcml]: Added 22 new parameters for full visualization control. Summary labels now shown by default (`summary_labels = TRUE`). Arrows shown by default on summary edges (`summary_arrows = TRUE`). All hardcoded values (edge widths, alphas, layout multipliers) now parameterized.
- [cluster_summary alignment]: Aligned `cluster_summary()` with `mcml()` structure. Now includes `tna` (row-normalized transition matrix), `inits` (initial state distribution), and `as_tna` parameter. **Breaking change**: renamed `between` → `between_weights` and `within` → `within_weights` for consistency with mcml. Both functions now return identical `tna` and `inits` fields.

### 2026-02-16
- [motifs tests]: Created comprehensive test file `test-coverage-motifs-41.R` (898 lines, 39 tests) targeting:
  - `get_edge_list()` function with tna objects (not tested elsewhere)
  - `extract_motifs()` with tna objects and various level/significance options
  - `.motif_census_undirected()` additional method coverage
  - `.generate_random_graph()` edge cases for both directed/undirected
  - `plot.cograph_motifs` network type rendering all standard triads
  - `extract_triads()` combined filter edge cases
  - `plot.cograph_motif_analysis` spacing, n > nrow, types edge cases
  - `.plot_motif_patterns` various type counts
  - `extract_motifs_temporal` format auto-detection and step > window_size
  - `tna_windows` integration tests
  - `triad_persistence` status classification and edge_weight scaling
  - `plot.cograph_triad_persistence` fill/normalize/timeline options
  - `.classify_triads_vectorized` all 16 MAN types
  - Error handling for missing columns and invalid inputs
  - Full workflow integration test
  - Regression tests for reproducibility
- [empty test fix]: Tests with conditional `if (!is.null(result))` blocks need at least one expect_* call outside the block to avoid "empty test" warnings from testthat.
- [layout ... fix]: All layout functions (circle, oval, spring) now accept `...` to avoid "unused argument" errors when `do.call` passes extra params like `node_group`. This fixed `plot.cograph_communities` and `splot` with `node_group`.
- [CographLayout in compute_layout]: `compute_layout_for_cograph()` now handles CographLayout objects directly instead of wrapping them in another `CographLayout$new()` (which set `.type` to a non-atomic R6 object, causing `== "custom"` comparison failure).
- [layout_groups S3 support]: `layout_groups()` now handles S3 `cograph_network` objects using `n_nodes()` instead of `network$n_nodes` field access.
- [community_optimal hang]: Never test `community_optimal` on dense matrices >50 nodes — `igraph::cluster_optimal` is NP-hard and will hang. Use sparse graphs (e.g., ring) to trigger the >50 warning.
- [igraph callback signatures]: igraph `cluster_leading_eigen` callback now passes more args than documented. Always use `...` in callback functions.
- [membership/modularity generics]: `membership()` and `modularity()` generics come from igraph, not cograph. In tests without `library(igraph)`, call methods directly: `membership.cograph_communities(comm)`.
- [expect_s3_class info]: `expect_s3_class()` does NOT accept an `info` parameter (unlike `expect_equal()`). Remove it.
- [splot groups param]: `splot()` captures `groups` as a formal parameter for node coloring. It does NOT pass `groups` to the layout. Use pre-computed coords with `layout_groups()` instead.

### 2026-02-17
- [plot_time_line]: New exported function in `R/plot-timeline.R` for cluster timeline visualization:
  - Clusters arranged side-by-side with ellipse shells, within-cluster directed edges, pie chart nodes
  - Between-cluster edges: curved beziers connecting adjacent clusters only, top nodes curve up, bottom nodes curve down (style 4 "split")
  - `orientation = "horizontal"` (clusters left-to-right) or `"vertical"` (top-to-bottom)
  - `layers` parameter: named list of weight matrices for multi-layer stacking with dashed cross-layer connections
  - `between_minimum` threshold to control density of between-cluster edges (default 0.10)
  - `between_curve_strength` controls how much curves bow outward (default 0.8)
  - `global_scale = TRUE` normalizes edge widths across all layers consistently
  - Returns cluster_summary (single layer) or list of cluster_summary objects (multi-layer)
  - 27 tests in `tests/testthat/test-plot-timeline.R`
- [between-cluster edge styles]: Prototyped 4 styles for between-cluster edges:
  1. Curved bundled (arcs upward) 2. Gradient (source→target color) 3. Tapered ribbon 4. Bezier arcs (horizontal tangent)
  - User selected style 4 variant: split curves (top up, bottom down)
- [prototype iteration]: Extensive prototyping in `tmp/multilayer_prototype.Rmd` — iterated through ~15 versions before converging on final design. Key decisions: vertical ellipse shells, 7 nodes per cluster, adjacent-only between edges, split curved beziers, no summary layer needed.

### 2026-02-14
- [label resolution]: Node labels now use priority order: `labels` > `label` > identifier. The `labels` column takes precedence when both exist. Updated in `get_labels()`, `resolve_labels()`, `render_node_labels_grid()`, and `CographNetwork.node_labels`.
- [cograph support]: `cluster_summary()` now accepts cograph_network and tna objects directly, extracting the weight matrix automatically. Uses `x$weights` for cograph_network (efficient) or `to_matrix()` as fallback.
- [testing]: CographNetwork R6 class does not have a `node_labels` parameter in `$new()` - use the `nodes` parameter with a data frame instead. Tests were incorrectly written and needed fixing.
