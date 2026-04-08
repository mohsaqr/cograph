# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Environment

- **Platform**: macOS (Darwin), R 4.1+ (currently R 4.5+)
- **Version**: 2.0.0
- **Rscript**: Available on PATH
- **Additional repo**: `https://mohsaqr.r-universe.dev` registered for Nestimate dependency resolution (see `Additional_repositories` in DESCRIPTION)

## Common Commands

```bash
# Load package
Rscript -e 'devtools::load_all(".")'

# Run all tests
Rscript -e 'devtools::test(".")'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-splot.R")'

# Build documentation
Rscript -e 'devtools::document(".")'

# Build vignettes
Rscript -e 'devtools::build_vignettes(".")'

# Quick R CMD check (no tests/examples/vignettes)
Rscript -e 'devtools::check(".", args = c("--no-tests", "--no-examples", "--no-vignettes", "--no-manual"))'

# Full R CMD check (as CRAN sees it)
Rscript -e 'rcmdcheck::rcmdcheck(".", args = c("--as-cran", "--no-manual"), build_args = "--compact-vignettes=gs+qpdf", error_on = "warning")'

# Strict CRAN incoming check (simulates CRAN + Posit extra checks)
_R_CHECK_CRAN_INCOMING_=TRUE _R_CHECK_CRAN_INCOMING_REMOTE_=TRUE \
_R_CHECK_FORCE_SUGGESTS_=FALSE _R_CHECK_DONTTEST_EXAMPLES_=TRUE \
Rscript -e 'rcmdcheck::rcmdcheck(".", args = c("--as-cran", "--no-manual"), build_args = "--compact-vignettes=gs+qpdf", error_on = "note")'

# Measure test coverage (takes ~25-30 min)
Rscript -e 'covr::package_coverage(".")'

# Install locally
Rscript -e 'devtools::install(".", upgrade = "never")'

# Build pkgdown site locally
Rscript -e 'pkgdown::build_site()'
```

## CI Matrix

GitHub Actions (`R-CMD-check.yaml`) tests on: macOS-latest (release), Windows-latest (release), Ubuntu-latest (devel, release, oldrel-1). The workflow registers `mohsaqr.r-universe.dev` via `~/.Rprofile` for Nestimate resolution.

**Test split**: Coverage tests (`test-coverage-*.R`, 92 files) are expensive and only run on Ubuntu release via `COGRAPH_COVERAGE_TESTS=true` env var. Feature tests (~45 files) run on all 5 platforms. Locally, `devtools::test()` runs everything (env var defaults to `"true"` when unset). The `skip_coverage_tests()` helper in `helper-test-utils.R` gates this.

## Project Overview

cograph is an R package for analysis and visualization of complex networks. Key entry points:

- `splot()` — Base R graphics network plotting (core engine)
- `soplot()` — Grid/ggplot2-style network plotting (separate rendering path)
- `plot_tna()` / `tplot()` — TNA-style wrappers around splot with qgraph-compatible parameters
- `plot_compare()` — Difference networks
- `plot_htna()` — Hierarchical multi-group TNA layouts
- `plot_mtna()` — Multi-cluster TNA with shape containers
- `plot_chord()` — Chord diagrams (base R, directed/undirected ribbons)
- `plot_heatmap()` — Adjacency matrix heatmaps
- `plot_mixed_network()` — Combined undirected + directed edge styling from two matrices
- `plot_transitions()` / `plot_alluvial()` / `plot_trajectories()` — Flow diagrams (`plot_alluvial` and `plot_trajectories` are aliases with different `track_individuals` defaults)
- `plot_bootstrap()` / `plot_permutation()` — Statistical result visualization
- `plot_bootstrap_forest()` / `plot_edge_diff_forest()` — ggplot2 forest plots for bootstrap CIs and edge differences
- `cluster_summary()` / `plot_mcml()` — Multi-cluster multi-layer analysis and visualization
- `plot_mlna()` — Multilayer 3D perspective networks
- `plot_simplicial()` — Higher-order pathway (simplicial complex) visualization
- `robustness()` / `plot_robustness()` — Network robustness under node/edge removal attacks
- `centrality()` — 82 node centrality measures (`R/centrality.R` + `R/centrality-extended.R`), equivalence-validated against centiserve, sna, brainGraph, influenceR, igraph, tidygraph, and NetworkX. **Batch 3 (Katz, Hubbell, Information, Pairwise Disconnectivity, Local Reaching)** and **Batch 4 (Domain Prestige, Domain Proximity Prestige)** are bit-exact (`expect_identical`) against their primary reference — see `tests/testthat/test-centrality-batch3.R` and `NEWS.md`. `edge_centrality()` + wrappers in `R/edge-metrics.R` provide the edge-level equivalents. `reaching_global()` adds a graph-level hierarchy statistic. **Note**: cograph's `prestige_domain_proximity` diverges from `sna::prestige(cmode = "domain.proximity")` on graphs with unreachable pairs because sna has a `FALSE * Inf = NaN` bug that zeros everything; cograph gives the mathematically correct values.
- `motifs()` / `subgraphs()` — Triad census and motif analysis
- `detect_communities()` — 11 community detection algorithms
- `disparity_filter()` — Backbone extraction via disparity filter (S3: matrix, igraph, tna, cograph_network)
- `cluster_quality()` / `cluster_significance()` — Cluster evaluation metrics
- **Tier 2 network features** (one module per file): `assortativity.R` (degree/attribute assortativity), `bipartite.R` (two-mode projection + bipartite metrics), `core-periphery.R` (Borgatti-Everett continuous + discrete), `rich-club.R` (Opsahl weighted rich-club), `vulnerability.R` (node vulnerability via efficiency drop), `paths.R` (shortest-path queries), `fit-distribution.R` (MLE degree-distribution fitting with AIC ranking). These are standalone analytics — no rendering dependency.

## Architecture

### Two Rendering Paths (Never Mix)

**`splot()`** uses base R graphics (`graphics::polygon`, `graphics::lines`, `xspline`). Helpers: `splot-nodes.R`, `splot-edges.R`, `splot-arrows.R`, `splot-labels.R`, `splot-geometry.R`, `splot-polygons.R`, `splot-params.R`.

**`soplot()`** uses grid graphics. Helpers: `render-nodes.R`, `render-edges.R`, `render-grid.R`, `render-ggplot.R`, `shapes-special.R`. The `sn_*` pipe-friendly functions (`sn_nodes`, `sn_edges`, `sn_layout`, `sn_theme`, `sn_palette`, `sn_render`, `sn_save`, `sn_ggplot`) provide the builder API for this path.

These paths are fully independent. A function in `splot-nodes.R` is *not* called by `soplot()`.

### Data Flow

```
Input (matrix / data.frame / igraph / tna / netobject / ...)
  -> parse_input()          # input-parse.R + input-*.R
  -> CographNetwork (R6)    # class-network.R
  -> splot() / soplot()     # rendering
```

`parse_input()` auto-detects type and delegates to `parse_matrix()`, `parse_igraph()`, `parse_tna()`, `parse_statnet()`, `parse_qgraph()`, `parse_edgelist()`. Returns a `CographNetwork` R6 object with private fields accessed via `get_nodes()`, `get_edges()`, `is_directed()`.

Conversion utilities: `to_igraph()`, `to_matrix()`, `to_network()`, `to_df()`, `from_qgraph()`, `from_tna()`, `as_cograph()`.

### splot() Dispatch

`splot()` has a large signature. Named parameters like `minimum`, `threshold`, `layout`, `title` are consumed by the function signature and do NOT appear in `...`. You cannot forward them via `handler(x, ...)`.

The correct dispatch pattern (used in splot.R ~lines 610-652):

```r
# WRONG -- named params are lost
return(splot.tna_bootstrap(x, ...))

# CORRECT -- capture user args, then forward via do.call
.user_explicit <- match.call(expand.dots = FALSE)
.user_args <- mget(setdiff(names(.user_explicit), "..."), envir = environment())
return(do.call(splot.tna_bootstrap, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
```

`.collect_dispatch_args()` (splot.R ~line 2077) merges user args + dots, with optional `base` defaults and `skip` exclusions. User-explicit args always win over base defaults.

Dispatches to:
- **TNA objects**: `plot_mcml`, `splot.tna_bootstrap`, `splot.tna_permutation`, `splot.group_tna_permutation`, `splot.tna_disparity`, `splot.wtna_mixed`
- **Nestimate objects**: `splot.netobject`, `splot.net_bootstrap`, `splot.net_permutation`, `splot.boot_glasso`, `plot_netobject_group`, `plot_netobject_ml`

**Known registration bug**: `splot.tna_disparity`, `splot.tna_bootstrap`, `splot.tna_permutation`, `splot.group_tna_permutation` are registered as `export()` in NAMESPACE instead of `S3method()`. This means `splot(obj)` does NOT dispatch via S3 for these classes — the explicit `inherits()` checks in splot.R handle them instead. The nestimate S3 methods (`splot.netobject`, `splot.net_bootstrap`, `splot.boot_glasso`) ARE properly registered as `S3method()`.

### Nestimate Integration

cograph plots nestimate objects without importing the package — dispatch is via `inherits()` class-name checking only. Supported classes: `netobject`, `boot_glasso`, `net_bootstrap`, `net_permutation`, `netobject_group`, `netobject_ml`. Implementation in `plot-nestimate.R`, `plot-bootstrap.R`, `plot-permutation.R`.

Nestimate also provides three higher-order network methods relevant to `plot_simplicial()`:
- **HON** (`build_hon`): Higher-Order Network construction from sequence data, expanding state space to capture variable-length memory dependencies
- **HYPA** (`hypa`): Hypothesis testing for path anomalies using multi-hypergeometric null model on De Bruijn graphs (LaRock et al. 2020)
- **HONEM** (`honem`): Higher-Order Network Embedding via matrix factorization of HON neighborhood matrices (Saebi et al. 2020)

### TNA Styling and qgraph Translation

`from-qgraph.R` has two key roles:

1. **`.translate_qgraph_dots()`** — renames qgraph-style params (`vsize` -> `node_size`, `asize` -> `arrow_size`, `edge.color` -> `edge_color`, etc.) with value transforms (e.g., `asize * 0.20`). Called early in splot before dispatch, gated by `inherits(x, c("tna", ...))`. When both cograph name and qgraph alias are present, cograph name wins.

2. **`.tna_style_defaults()`** — `tna_styling = TRUE` (used by `plot_tna()` and `splot.netobject`) applies TNA visual defaults:
   - NULL-default params: filled if user didn't set them
   - Non-NULL-default params: only overridden if user didn't explicitly pass them (checked via `"param_name" %in% explicit_args`)
   - User-explicit args always win

### MCML / Cluster Summary Pipeline

```
cluster_summary(x, cluster_list, ...)   # aggregate to cograph_mcml
  -> $macro       # k*k cluster-level transition matrix
  -> $clusters    # named list: per-cluster detail matrices
  -> $meta        # method, type, directed, n_nodes, n_clusters, cluster_sizes
  -> plot_mcml()  # two-layer visualization (detail layer + summary layer)
```

`plot_mcml()` accepts the raw matrix + `cluster_list` (calls `cluster_summary` internally) or a pre-computed `cograph_mcml` object directly.

### Multilayer Networks

`mlna.R` provides multilayer network analysis: `mlna()` creates supra-adjacency matrices, `extract_layer()` / `extract_interlayer()` pull individual layers, `aggregate_layers()` / `aggregate_weights()` combine them. Visualization via `plot_mlna()` (3D perspective) and `plot_ml_heatmap()`.

### Motifs Module

Four files form a subsystem: `motifs-api.R` (unified `motifs()` / `subgraphs()` API), `motifs.R` (census/instance engines + significance via configuration model), `motifs-extract.R` (per-individual triad extraction), `motifs-data.R` (MAN triad type definitions). `motifs-plot.R` provides `plot.cograph_motif_result`.

Two modes: **census** (`named_nodes = FALSE`) counts MAN type frequencies with significance; **instances** (`named_nodes = TRUE` or `subgraphs()`) enumerates specific node triples. Auto-detects individual-level data (tna objects, edge lists with session metadata) vs. aggregate inputs.

### Simplicial / HON / HYPA Pipeline

`plot_simplicial()` in `plot-simplicial.R` visualizes higher-order pathways as smooth blobs over a network layout. When given a `tna`/`netobject` with sequence data and no explicit pathways, it auto-builds HON or HYPA via `Nestimate::build_hon()` / `Nestimate::build_hypa()`. Shared node-expansion helpers (for repeated states in pathways) live in `blob-helpers.R`, which is also used by `overlay_communities()`.

### Scaling Constants

`QGRAPH_SCALE` and `COGRAPH_SCALE` in `scale-constants.R` — calibrated constants for qgraph-compatible visual formulas (vsize, esize, arrow sizing) and cograph-native defaults.

### Registries

Shapes, layouts, and themes stored in `.cograph_env` (package environment in `aaa-globals.R`). Registration via `register_svg_shape()`, `layout-registry.R`, `themes-registry.R`. Built-in themes: classic, dark, minimal, colorblind, gray, nature, viridis.

### RNG State

CRAN requires `set.seed()` callers to restore the caller's RNG:
```r
saved_rng <- .save_rng()
on.exit(.restore_rng(saved_rng), add = TRUE)
set.seed(seed)
```
Helpers in `aaa-globals.R`.

## Key Gotchas

- **Custom layout coords** require `rescale = FALSE` + `layout_scale = 1`, otherwise splot normalizes the coordinates.
- **R list NULL trap**: `args$x <- NULL` deletes the element. Use `args["x"] <- list(NULL)` to store NULL.
- **`igraph::distances(g, weights = NULL)`** auto-uses `E(g)$weight`. Use `weights = NA` to force unweighted distances.
- **qgraph arg translation**: `plot_tna()` / `tplot()` accept qgraph-style params (`vsize`, `asize`, `edge.color`). Translation in `.translate_qgraph_dots()` (`from-qgraph.R`).
- **tna API**: `centrality()` -> `centralities()` (plural). `bootstrap()` uses `iter` not `R`. `bootstrap()` requires a tna object built from sequence data (has `$data`), not a raw matrix.
- **namespace masking**: When `tna` or `igraph` are loaded, they mask `plot_compare()`, `communities()`, `degree_distribution()`, `is_directed()`. Use `cograph::` prefix in examples and tests.
- **`%||%`**: Defined locally in `aaa-globals.R` (not imported from rlang) for R 4.1 compatibility.
- **detect_communities()** returns a data.frame with columns `node` + `community`, not `$membership`. Use `setNames(comm$community, comm$node)` for named membership vectors.
- **S3method vs export in NAMESPACE**: `@export` on `splot.foo` emits `export(splot.foo)`. Use `@method splot foo` + `@export` to emit `S3method(splot,foo)`. The former breaks `UseMethod` dispatch (which is why the `inherits()` cascade exists).
- **`dontrun` vs `donttest`**: Do NOT blindly convert `\dontrun` to `\donttest`. Many examples use undefined variables or depend on optional packages that mask cograph functions. Only convert fully self-contained, runnable examples.
- **Nestimate field differences from tna**: `net_bootstrap$original$weights` (not `$weights`), `$ci_level` (not `$level`). `net_permutation` p_values/effect_size are already matrices. `boot_glasso` edge names use `" -- "` separator.

## Test Conventions

~137 test files (45 feature + 92 coverage), ~13,700+ expectations. Coverage tests follow `test-coverage-{module}-{round}.R` (rounds: 40, 41, 42, ...). Target: 100% line coverage (achieved). Use `# nocov` only for genuinely unreachable defensive guards.

**Centrality equivalence tests**: The 75 centrality measures are validated against external reference implementations (centiserve, sna, brainGraph, influenceR, igraph, tidygraph, NetworkX via reticulate). Equivalence tests live alongside coverage tests and use `tolerance` arguments per measure — see `HANDOFF.md` for the full validation matrix (exact-match vs. formula-verified vs. rank-correlation tiers).

Two test helper files load before every test:
- `tests/testthat/helper-cograph.R` — exposes internal functions via `cograph:::` for testing
- `tests/testthat/helper-test-utils.R` — test data generators (`create_test_matrix()`, `create_test_edgelist()`, etc.) and custom expectations

Never put `devtools::load_all()` inside test files — it breaks covr.

## Optional Dependencies

All suggested packages must be guarded with `requireNamespace("pkg", quietly = TRUE)`. Use `# nocov` on fallback branches unreachable in the test environment.

## Session Artifacts

- `docs/LEARNINGS.md` — Accumulated pitfalls/discoveries (e.g., CRAN timing behavior, Windows check quirks)
- `docs/CHANGES.md` — Human-readable changelog (newest first)
- `HANDOFF.md` — Session state for continuity across conversations

## pkgdown Site

`_pkgdown.yml` configures the documentation site (Bootstrap 5, Yeti theme). CI deploys to `gh-pages` on push to main. Uses `quarto-dev/quarto-actions/setup@v2` because some articles are `.qmd` (Quarto) rather than `.Rmd`.

## CRAN Submission

`cran-comments.md` tracks submission notes. Before submitting: run the strict CRAN incoming check (see Commands above), ensure zero NOTEs, and verify `Additional_repositories` points to `https://mohsaqr.r-universe.dev` for Nestimate.
