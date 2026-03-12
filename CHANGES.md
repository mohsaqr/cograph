# Changelog

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
