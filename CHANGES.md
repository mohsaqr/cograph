# Changelog

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
