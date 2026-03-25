# Changelog

### 2026-03-24 — CRAN prep: fix S3 method registration NOTE
- R/plot-nestimate.R: Removed `@method splot netobject` and `@method splot boot_glasso` tags — now registered as plain `export()` instead of `S3method()`, fixing S3 generic/method consistency NOTE (splot is not a formal S3 generic)
- R/plot-bootstrap.R: Removed `@method splot net_bootstrap` tag (same fix)
- R/plot-permutation.R: Removed `@method splot net_permutation` tag (same fix)
- NAMESPACE: Regenerated — 4 `S3method(splot,...)` entries replaced with `export(splot....)`, consistent with tna splot methods
- cran-comments.md: Updated test count (13,659), check results (0 errors, 0 warnings, 1 expected NOTE)
- Strict CRAN incoming check passes: 0 errors, 0 warnings, 1 NOTE (standard incoming feasibility — Nestimate in Additional_repositories)

### 2026-03-23 — Nestimate bootstrap/permutation validation
- R/plot-permutation.R: Fixed `splot.net_permutation` undirected edge indexing — was using full matrix `which(!=0)` instead of `upper.tri()` for undirected networks, causing per-edge array length mismatch error with `show_nonsig=TRUE`
- tests/testthat/test-validate-nestimate-bootstrap-permutation.R: New — 66 tests validating real Nestimate `bootstrap_network()` and `permutation_test()` objects against cograph's processing (significance classification, CI indexing, edge labels, directed/undirected handling)
- Tests: 155 bootstrap/permutation tests pass, 0 failures

### 2026-03-21 — plot_simplicial() full HON/HYPA/simplicial pipeline

**cograph changes:**
- R/plot-simplicial.R: Full rewrite — accepts tna/netobject/net_hon/net_hypa directly; auto-builds HON/HYPA from sequence data via Nestimate; new params: `method` (hon/hypa), `max_pathways`, `ncol`; dismantled mode uses gridExtra grid layout with scaled nodes and suppressed per-panel titles; `pathways` can be a net_hon/net_hypa object
- R/blob-helpers.R: Added .extract_hon_pathways(), .extract_hypa_pathways() (with label_map translation and sorting by count/ratio), .build_hon_label_map(), .extract_sequence_data(), .build_higher_order(); updated .extract_blob_states() for net_hon/net_hypa
- DESCRIPTION: Added Nestimate to Suggests
- tests/testthat/test-coverage-plot-hon-40.R: 74 tests — mock factories, label translation, direct tna/netobject support, grid layout, max_pathways, regression
- tests/testthat/test-plot-simplicial.R: Updated dismantled tests for grob return type
- tutorials/cograph-tutorial-simplicial.qmd + .html: Full tutorial — TNA → MOGen → HON → HYPA → simplicial visualization → simplicial complex analysis → persistent homology → Q-analysis

**Nestimate changes:**
- R/utils.R: Added .coerce_sequence_input() — tna/netobject to labeled data.frame
- R/hon.R: build_hon() accepts tna/netobject directly, outputs labeled states
- R/hypa.R: build_hypa() accepts tna/netobject directly
- R/mogen.R: build_mogen(), path_counts(), state_frequencies() accept tna/netobject
- R/build_network.R: Reverted column stripping; long-format passes full data to prepare_data
- R/prepare_data.R: .aggregate_metadata() now collects ties silently and reports one summary line instead of per-session warnings
- R/simplicial.R: New file — build_simplicial() (clique/pathway/VR), betti_numbers(), euler_characteristic(), persistent_homology(), simplicial_degree(), q_analysis(), verify_simplicial(); plot methods for all three object types; igraph-verified, Euler-Poincaré verified

- Tests: 13,593 pass, 0 failures (cograph)

### 2026-03-18 — Class demo tutorial + coverage fixes
- tutorials/cograph-class-demo.qmd: New class-ready tutorial — 15-node network, 5 layouts, 5 edge styles, Louvain community detection with colour coding, polished publication plot, heatmap, quick-reference cheatsheet
- tests/testthat/test-coverage-plot-nestimate-40.R: Fixed devtools::load_all() breaking covr; added tests for splot.net_bootstrap sig branch and show_ci+show_stars=FALSE path
- tests/testthat/test-coverage-methods-print-42.R: Added tests for method labels, negative weights, self-loops, and node_groups branches in print.cograph_network
- R/plot-nestimate.R: common_scale now computed before single-group early return; edge_alpha pre-rounds weights (length-mismatch fix); @method annotations corrected
- R/plot-bootstrap.R, R/plot-permutation.R: @method S3 annotations; dead-code double null-guard simplified
- NAMESPACE: all 4 new splot.* methods now emit S3method() entries
- Coverage: 100% (was 99.82% after first Nestimate commit)
- Tests: 13,518 pass, 0 failures

### 2026-03-18 — Nestimate plotting support
- R/splot.R: Added 5 dispatch branches for Nestimate object classes (net_bootstrap, net_permutation, boot_glasso, netobject_group, netobject_ml) after existing tna dispatch, before deprecated parameter handling
- R/plot-bootstrap.R: Appended splot.net_bootstrap() — styled/significant/full display modes, CI/stars, directed-aware edge indexing (upper.tri for undirected)
- R/plot-permutation.R: Appended splot.net_permutation() — p_values and effect_size as pre-built matrices, no edge-name parsing needed
- R/plot-nestimate.R: New file with splot.boot_glasso() (inclusion-scaled alpha), plot_netobject_group() (multi-panel grid with common scale), plot_netobject_ml() (side-by-side between/within), and S3 plot() aliases
- tests/testthat/test-coverage-plot-nestimate-40.R: 45 tests, 0 failures
- R CMD check: 0 errors, 0 warnings, 0 notes; full suite: 13,512 pass, 0 fail, 39 skips

### 2026-03-18 — CRAN prep: NEWS.md revision and CLAUDE.md rewrite
- NEWS.md: Full rewrite aligning all versions with actual git history. Moved 12 features from 1.6.0 → 1.7.0, 4 items from 1.7.0 → 1.8.0, expanded 1.8.2 section. Removed internal process items from all versions.
- CLAUDE.md: Complete rewrite with project overview, two rendering paths, splot dispatch pattern, TNA styling, key gotchas, test conventions
- cran-comments.md: Updated to reflect 0/0/0 check results and comprehensive changes-since-1.5.2 list

### 2026-03-17 — CRAN prep: clean --as-cran check
- .Rbuildignore: Added `^data-raw$` to suppress top-level NOTE
- R CMD check --as-cran: 0 errors, 0 warnings, 0 notes
- Tests: 13,466 pass, 0 fail, 39 skips (device/env-specific)

### 2026-03-14 — v1.8.2: 100% coverage, README overhaul

- README.md: Comprehensive rewrite with feature tables for all plot functions, network analysis, community detection, multilayer support, and quality metrics
- DESCRIPTION: Bumped to 1.8.2
- tests/testthat/test-coverage-100pct-v2.R: NEW — 26 tests covering disparity filter default/plot/splot methods, empty tna transitions, undirected edge simplification, multi-step value_min filtering
- R/motifs-data.R: Marked unreachable defensive guard as `# nocov` (line 203)
- Coverage: 99.73% → 100% (0 uncovered lines)
- Tests: 13,450+ pass, 0 fail

### 2026-03-14 — v1.7.0–1.8.0: overlay_communities, plot_simplicial, full community detection

- R/plot-communities.R: `overlay_communities()` — accepts 6 input formats: method name strings (walktrap, louvain, etc. with partial matching), named lists, numeric/factor membership vectors, `cograph_communities`, `tna_communities`. Auto-converts directed→undirected for igraph detection.
- R/plot-simplicial.R: `plot_simplicial()` — higher-order pathway visualization with smooth blob geometry, flexible separators, dismantled view
- R/blob-helpers.R: Shared geometry helpers (`.smooth_blob`, `.darken_colors`, `.extract_blob_states`, `.blob_layout`)
- tests/testthat/test-overlay-communities.R: 33+ tests
- tests/testthat/test-plot-simplicial.R: 78+ tests
- tutorials/cograph-tutorial-communities.qmd: Full tutorial
- Deleted R/hypa.R (unrequested, 0% coverage)
- Tests: 13,392+ pass, 0 fail. Coverage: 99.73%

### 2026-03-13 — Fix spiky text halo (8→16 directions)

- R/plot-transitions.R: `.text_or_halo()` and `.annotate_or_halo()` increased from 8 to 16 circular offset directions (22.5° spacing) for smooth halo outline.
- R/splot-edges.R: Edge label halo updated from 8 to 16 directions.
- R/plot-heatmap.R: Heatmap value label halo updated from 8 to 16 directions.
- Tests: 13,289 pass, 0 fail

### 2026-03-13 — Clean and package robustness + disparity filter

- R/robustness.R: `n_iter` default changed from 100 to 1000 (matching brainGraph). Sequential + static strategies, vertex/edge attacks, betweenness/degree/random measures.
- R/disparity.R: Disparity filter with methods for matrix, tna, cograph_network, igraph. Verified against tna and disparityfilter CRAN packages.
- R/cograph-package.R: Added `@importFrom graphics abline legend lines par plot` to fix R CMD check NOTEs.
- DESCRIPTION: Added brainGraph to Suggests (used in verification tests).
- tutorials/cograph-tutorial-robustness.qmd: Complete rewrite as proper tutorial (matching style of analysis/plotting tutorials). Covers backbone extraction, robustness analysis, strategy comparison, disparity filter levels.
- tests/testthat/test-robustness.R: 123 tests (including static strategy, brainGraph verification)
- tests/testthat/test-disparity.R: 29 tests (including igraph methods)
- Tests: 13,246 pass, 0 fail

### 2026-03-13 — Re-integrate robustness module from sidelined/

- R/robustness.R: NEW — `robustness()` (targeted/random vertex/edge removal), `plot_robustness()` (base R), `ggplot_robustness()` (ggplot2 faceted), `robustness_auc()` (AUC), `robustness_summary()` (critical points). Internal: `robustness_vertex_attack()`, `robustness_edge_attack()`.
- tests/testthat/test-robustness.R: 107 tests covering all measures, input types, edge cases, AUC, summary, both plot functions
- Tests: 13,302 pass, 0 fail

### 2026-03-13 — Update motifs tutorial to unified API

- tutorials/cograph-tutorial-motifs.qmd: Rewritten to use `motifs()`/`subgraphs()` API. Added auto-detection + windowing section, legacy function mapping table.

### 2026-03-13 — Implement unified motifs() / subgraphs() API

- R/motifs-api.R: NEW — `motifs()` (census, nodes exchangeable) + `subgraphs()` (instances, named node triples) with auto-detection of actor/session columns, windowing (rolling/tumbling), exact configuration model significance. Print + plot S3 methods for `cograph_motif_result`. Supports tna, cograph_network, matrix, igraph, data.frame inputs.
- R/motifs-data.R: Added `.detect_actor_column()`, `.detect_order_column()`, `.edgelist_to_trans_array()` with windowing support
- R/motifs.R: Marked `motif_census`, `triad_census`, `extract_triads` as `@keywords internal`
- R/motifs-extract.R: Marked `extract_motifs` as `@keywords internal`
- tests/testthat/test-motifs-api.R: NEW — 99 tests covering all dispatch paths, significance modes, windowing, auto-detection, plot types, error handling
- Tests: 13,094 pass, 0 fail. Coverage: 99.5% on motifs-api.R

### 2026-03-12 — Motif analysis tutorial + unified API benchmarking

- tutorials/cograph-tutorial-motifs.qmd: NEW — comprehensive tutorial (11 sections, 71 code chunks) covering motif_census, triad_census, extract_triads, extract_motifs with significance testing, visualization, group comparisons
- tutorials/cograph-tutorial-motifs.html: Rendered output (2.6MB self-contained HTML)
- docs/superpowers/plans/2026-03-12-unified-motifs-api.md: Implementation plan for unified motifs()/subgraphs() API with exact configuration model
- tmp/bench_all_modes_v2.R, tmp/bench_all_modes_coding.R: Full equivalence tests on coding dataset (3 modes, 1000 perms, seed=42)
- tmp/bench_config_model.R, tmp/bench_prealloc.R: Configuration model benchmarks
- tmp/verify_census_equiv.R, tmp/verify_igraph_equiv.R, tmp/verify_config_exact.R: Verification scripts for null model correctness

### 2026-03-12 — Re-integrate static motifs module (4 files)

- R/motifs-data.R: Restored from sidelined/ — shared constants (triad patterns, MAN descriptions, cache, ggplot theme)
- R/motifs.R: Restored — `motif_census()`, `triad_census()`, `extract_triads()`, `get_edge_list()` with print/plot S3 methods
- R/motifs-extract.R: Restored — `extract_motifs()` with print/plot S3 methods; fixed bare NSE vars to `.data$` prefix
- R/motifs-plot.R: Restored — viz helpers (.plot_motifs_bar/heatmap/network, .plot_triad_networks, .plot_motif_patterns); fixed bare NSE vars to `.data$` prefix
- Removed `@seealso` references to temporal functions (`extract_motifs_temporal`, `triad_persistence`) not yet re-integrated
- tests/testthat/test-coverage-motifs-{40,41,43}.R: Restored from sidelined/, truncated temporal test sections
- Tests: 12,809 pass, 0 fail

### 2026-03-12 — Add qgraph arg translation in splot's tna dispatch

- R/from-qgraph.R: Added `.translate_qgraph_dots()` — translates qgraph-style parameter names (size, vsize, edge.color, lty, shape, etc.) to cograph equivalents with value transforms (asize * 0.20, edge.label.cex * 1.2, numeric lty → name, shape mapping)
- R/splot.R: Call `.translate_qgraph_dots(.dots)` early in splot for all tna-family classes (tna, group_tna, tna_bootstrap, tna_permutation, group_tna_permutation, cluster_tna) — before any dispatch or arg collection
- tests/testthat/test-qgraph-args.R: 52 new tests — unit tests for `.translate_qgraph_dots()` (empty, unnamed, rename, precedence, all params, value transforms) + integration tests for splot dispatch with tna, group_tna, tna_bootstrap, tna_permutation, group_tna_permutation + non-tna regression test
- Tests: 12,628 pass, 0 fail

### 2026-03-10 — Fix splot dispatch arg forwarding, add title param, 100% coverage

- R/splot.R: Extracted `.collect_dispatch_args()` helper — replaces 6 copy-paste dispatch blocks with one-liner calls
- R/splot.R: Capture user args via `match.call(expand.dots = FALSE)` + `mget()` instead of `eval()`
- R/splot.R: Moved bootstrap/permutation/group_permutation dispatches before deprecated-param handling
- R/splot.R: Removed redundant `match.call()` at line 638, reuse `.user_explicit`
- R/plot-transitions.R: Added `title` parameter to `plot_transitions`, `plot_alluvial`, `plot_trajectories`; applied via `labs(title = title)` at 4 exit paths
- .Rbuildignore: Added `^tutorials` entry
- tests/testthat/test-coverage-plot-transitions-41.R: 5 new title coverage tests (4 paths + NULL default)
- tests/testthat/test-coverage-plot-bootstrap-40.R: 1 new `.subset_if_per_edge` else branch test
- Tests: 12,575 pass, 0 fail, 100% coverage
- R CMD check: 0 errors, 0 warnings

### 2026-03-09 — plot_htna: new orientations, intra-group edges, modern colors

- R/plot-htna.R: Added `orientation = "facing"` (tip-to-tip) and `orientation = "circular"` (two semicircles) for bipartite layouts
- R/plot-htna.R: Added `intra_curvature` parameter — draws intra-group edges as dotted bezier arcs, separate from inter-group edges; preserves tna object styling (donuts)
- R/plot-htna.R: New internal functions `.draw_intra_group_edges()` and `.draw_intra_arc()` for post-tplot bezier rendering with per-edge curve direction
- R/plot-htna.R: Uniform coordinate normalization for all layouts + `rescale = FALSE` + `layout_scale = 1`
- R/plot-htna.R: Updated default colors — `group1_color = "#4FC3F7"` (modern blue), `group2_color = "#fbb550"` (warm gold); edge palette updated to match
- R/plot-htna.R: Changed `legend_position` default from `"topright"` to `"bottomright"`
- R/plot-htna.R: Added `# nocov` on 5 unreachable defensive guards
- tests/testthat/test-coverage-plot-htna-41.R: 10 new tests (facing, circular, intra_curvature with all layouts, tna object, .draw_intra_arc, .draw_intra_group_edges)
- Tests: 12,548+ pass, 0 fail, 100% coverage

### 2026-03-04 — Achieve 100% test coverage

- tests/testthat/test-coverage-round7.R: NEW — 60 tests targeting remaining uncovered lines (centrality edge cases, shapes, edge labels, soplot title, plot_compare, plot_permutation, plot_bootstrap CI mode, color_communities palette recycling)
- R/zzz.R, R/aes-nodes.R, R/centrality.R, R/class-network.R, R/from-qgraph.R, R/input-igraph.R, R/input-qgraph.R, R/input-statnet.R, R/layout-registry.R, R/network-summary.R, R/network-utils.R, R/plot-bootstrap.R, R/plot-compare.R, R/render-edges.R, R/render-ggplot.R, R/render-grid.R, R/shapes-special.R, R/splot-edges.R, R/splot-nodes.R, R/splot-params.R, R/splot.R: Added `# nocov` markers to untestable code (package guards, dead code, .onLoad, stochastic edge cases, local functions)
- Coverage: 99.18% → 100.00% (66 → 0 uncovered expressions)
- Tests: 10,822 pass, 0 fail

### 2026-03-04 — Fix load/percolation centrality bugs + comprehensive validation suite

- R/centrality.R: `calculate_load()` complete rewrite — fixed infinite loop on weighted graphs (BFS assumed unit weights), directed graph transpose for sna compatibility, disconnected node handling, self-contribution bug, weights=NA for unweighted distances
- R/centrality.R: `calculate_percolation()` same BFS fix + /2 normalization for undirected graphs
- validation/test_centrality_comprehensive.R: Comprehensive centrality validation — 25+ measures, 49,579 tests on 1000 networks, all against external refs (igraph, centiserve, sna)
- validation/test_communities_comprehensive.R: All 12 community detection algorithms, 212 tests, deterministic+stochastic validation
- validation/test_network_properties_comprehensive.R: Network metrics on known graphs + 1000 simulated networks, 6,851 tests
- validation/generate_html_reports.R: Self-contained HTML report generator with "Validated Against" column
- validation/run_comprehensive_tests.R: Master runner for all 3 suites + report generation
- validation/README.md: Full documentation of approach, measures, and folder structure
- validation/reports/: 4 HTML reports (centrality, communities, network_properties, combined dashboard)
- Tests: 56,642 total validation tests, 100% pass rate

### 2026-03-03 — Sideline 19 newer function files for lean CRAN submission

- Sidelined 19 R files (19 source, 23 tests, 44 man pages, 1 vignette) to `sidelined/` folder
- Full version preserved on `main` branch
- **R files removed**: plot-chord.R, plot-heatmap.R, plot-ml-heatmap.R, plot-transitions.R, plot-timeline.R, plot-htna.R, plot-htna-multi.R, plot-mcml.R, plot-mixed-network.R, mlna.R, cluster-metrics.R, robustness.R, tna-animate.R, utils-globals.R, motifs.R, motifs-extract.R, motifs-temporal.R, motifs-plot.R, motifs-data.R
- **Cross-deps fixed**: Removed splot.R auto-dispatch block (lines 640-661), simplified set_groups() roxygen in class-network.R, removed print method integration test references
- **Vignettes**: Sidelined heatmaps-demo.Rmd (called plot_heatmap)
- sidelined/REIMPLEMENTATION.md: Detailed per-function re-integration guide with tiered phases
- Tests: 10,247 pass, 0 fail | R CMD check: 0 errors, 0 warnings

### 2026-03-03 — Integrate upstream CRAN compliance fixes and direction auto-detection

- R/aaa-globals.R: Added `.save_rng()` / `.restore_rng()` helpers for CRAN-compliant RNG state restoration
- R/cograph.R, R/splot.R, R/render-grid.R, R/communities.R, R/robustness.R, R/motifs.R, R/motifs-extract.R, R/motifs-temporal.R, R/mlna.R, R/cluster-metrics.R, R/input-igraph.R, R/layout-spring.R, R/layout-registry.R, R/layout-gephi-fr.R: Applied RNG save/restore to all 23 set.seed() calls
- R/from-qgraph.R, R/input-tna.R, R/tplot.R: Direction auto-detection via `is_symmetric_matrix()` instead of hardcoded `directed=TRUE`
- R/zzz.R: Removed `.onAttach` startup message
- R/aes-nodes.R: Added `requireNamespace("digest")` fallback with `utf8ToInt` hash
- R/cluster-metrics.R: Fixed tna API references (centrality→centralities, R→iter)
- R/network-summary.R, R/class-network.R, R/communities.R, R/plot-compare.R: Added `cograph::` prefix for namespace-masked functions in examples
- R/output-save.R, R/splot.R: Use `tempdir()` in example file paths
- R/input-parse.R, R/scale-constants.R, R/layout-gephi-fr.R, R/aes-edges.R: Removed examples from `@keywords internal` functions
- R/motifs-extract.R, R/plot-heatmap.R, R/plot-transitions.R, R/shapes-svg.R: Moved pseudo-code examples to `\dontrun`
- tests/: Fixed 4 test files for new behavior
- R CMD check: 0 errors, 0 warnings | Tests: 12,451 pass, 0 fail

### 2026-02-21 — Add mid_label_position, intermediate labels, node_label_format, line bundling, flow values

- R/plot-transitions.R: Added `mid_label_position` parameter for independent control of middle column node labels in `plot_trajectories()` / `plot_transitions()`. Refactored label rendering into `.add_labels()` helper. Added `node_label_format` for showing counts on labels (e.g., `"{state} (n={count})"`). Added `bundle_size` / `bundle_legend` for line aggregation in large datasets. Added `show_values` / `value_position` / `value_size` / `value_color` for flow count labels. Made all 5 label positions work on ALL columns. Made `label_position = "beside"` (right) and `"outside"` (left) consistent across all columns.
- R/utils-globals.R: Added `"lw"` to `globalVariables()` for bundled line width aes.
- tests/testthat/test-coverage-plot-transitions-41.R: Added 14 new tests for `mid_label_position` (163 total, all passing).
- man/plot_transitions.Rd, man/plot_trajectories.Rd, man/plot_alluvial.Rd: Regenerated with new parameter docs.
- Fixed roxygen `\%` double-escaping issue in bundle_size docs.
- R CMD check: 0 errors, 0 warnings, 0 notes.

### 2026-02-20 — Fix R CMD check errors, clean up project

- R/cluster-metrics.R: cluster_summary() now creates proper tna-compatible objects with `labels`, `data`, `type` attr, and `scaling` attr — fixes print.tna crash when tna package is loaded
- R/splot.R: Qualified `par()` calls with `graphics::` prefix (lines 559-560) — fixes "no visible global function definition for 'par'" NOTE
- R/layout-circle.R, R/layout-oval.R, R/layout-spring.R: Added `@param ...` roxygen docs — fixes "undocumented arguments" WARNING
- tests/testthat/test-coverage-layout-groups-41.R: Replaced tibble::tibble() with data.frame() — fixes "unstated dependency" WARNING
- tests/testthat/_problems/: Removed debug test fragments directory
- CLAUDE.md: Updated from Windows paths to macOS paths, added common commands
- Tests: 12,411 passing, 0 failures, R CMD check: 0 errors, 0 warnings, 1 note

### 2026-02-20 — Add plot_time_line and fix bootstrap edge mismatch

- R/plot-timeline.R: New `plot_time_line()` function for cluster timeline visualization
- man/plot_time_line.Rd: Roxygen documentation
- tests/testthat/test-plot-timeline.R: 27 tests for timeline function
- R/plot-bootstrap.R: Fix bootstrap edge count mismatch (weight_digits, directed, diagonal)
- R/splot-params.R: Guard against priority length mismatch in get_edge_order()
- showcase/cograph_showcase.Rmd: Comprehensive 24-section showcase
- tests/testthat/test-coverage-*-41.R: 8 coverage test files (series 41)
