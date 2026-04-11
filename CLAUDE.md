# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Environment

- **Platform**: macOS (Darwin), R 4.1+ (currently R 4.5+)
- **Version**: 2.1.1 (DESCRIPTION is the source of truth — check there first)
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
- `centrality()` — 87 node centrality measures (`R/centrality.R` + `R/centrality-extended.R`), equivalence-validated against centiserve, sna, brainGraph, influenceR, igraph, tidygraph, and NetworkX. Standalone graph/set/pair-level measures (Batch 6): `estrada_index()`, `trophic_incoherence()`, `group_centrality()`, `dispersion()`. Two documented divergences from reference implementations: `prestige_domain_proximity` vs sna's `FALSE * Inf = NaN` bug, and `group_centrality(measure = "betweenness")` vs NetworkX's Puzis iterative algorithm (cograph matches the textbook Everett-Borgatti definition). See `NEWS.md` for the full per-batch changelog and validation status.
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

The correct dispatch pattern (search `.user_explicit` in `R/splot.R`):

```r
# WRONG -- named params are lost
return(splot.tna_bootstrap(x, ...))

# CORRECT -- capture user args, then forward via do.call
.user_explicit <- as.list(match.call(expand.dots = FALSE))[-1]
.user_explicit$x <- NULL
.user_args <- mget(setdiff(names(.user_explicit), "..."), envir = environment())
return(do.call(splot.tna_bootstrap, c(list(x = x), .collect_dispatch_args(.user_args, .dots))))
```

`.collect_dispatch_args()` (defined near the bottom of `R/splot.R`) merges user args + dots, with optional `base` defaults and `skip` exclusions. User-explicit args always win over base defaults. Use grep by symbol name, not line numbers — the file churns.

Dispatches to:
- **TNA objects**: `plot_mcml`, `splot.tna_bootstrap`, `splot.tna_permutation`, `splot.group_tna_permutation`, `splot.tna_disparity`, `splot.wtna_mixed`
- **Nestimate objects**: `splot.netobject`, `splot.net_bootstrap`, `splot.net_permutation`, `splot.boot_glasso`, `plot_netobject_group`, `plot_netobject_ml`

**`splot.*` methods don't dispatch via S3 — the `inherits()` cascade is the only path.** `splot()` (R/splot.R) is a regular function with a large explicit signature; it never calls `UseMethod`. Every `splot.*` S3 method in NAMESPACE is also registered as `export(splot.foo)` instead of `S3method(splot, foo)` — verify with `grep 'S3method(splot' NAMESPACE` (zero hits). This applies to all of them: tna (`splot.tna_disparity`, `splot.tna_bootstrap`, `splot.tna_permutation`, `splot.group_tna_permutation`, `splot.wtna_mixed`) and nestimate/mlvar (`splot.netobject`, `splot.net_bootstrap`, `splot.net_permutation`, `splot.boot_glasso`, `splot.net_mlvar`). Because `splot()` has no `UseMethod` call, fixing the roxygen to emit `S3method(splot, foo)` would change nothing at runtime — it's pure NAMESPACE hygiene. **Rule for adding a new class**: add an `inherits()` branch to the cascade in the body of `splot()`. That is the only way the method gets reached.

### Nestimate Integration

cograph plots nestimate objects without importing the package — dispatch is via `inherits()` class-name checking only. Supported classes: `netobject`, `boot_glasso`, `net_bootstrap`, `net_permutation`, `netobject_group`, `netobject_ml`. Implementation in `plot-nestimate.R`, `plot-bootstrap.R`, `plot-permutation.R`.

Nestimate also provides three higher-order network methods relevant to `plot_simplicial()`:
- **HON** (`build_hon`): Higher-Order Network construction from sequence data, expanding state space to capture variable-length memory dependencies
- **HYPA** (`hypa`): Hypothesis testing for path anomalies using multi-hypergeometric null model on De Bruijn graphs (LaRock et al. 2020)
- **HONEM** (`honem`): Higher-Order Network Embedding via matrix factorization of HON neighborhood matrices (Saebi et al. 2020)

### Nestimate `net_mlvar` (Multilevel VAR)

**Added 2026-04-11, refactored same day.** `Nestimate::build_mlvar()`
(alias `mlvar()`) returns a dual-class `c("net_mlvar",
"netobject_group")` — a **named list of three full
`c("netobject", "cograph_network")` objects** built via Nestimate's
package-wide `.wrap_netobject()` constructor:

```
fit  class = c("net_mlvar", "netobject_group")
├── $temporal         c("netobject","cograph_network")  directed = TRUE   method = "mlvar_temporal"
├── $contemporaneous  c("netobject","cograph_network")  directed = FALSE  method = "mlvar_contemporaneous"
└── $between          c("netobject","cograph_network")  directed = FALSE  method = "mlvar_between"
```

Model-level metadata (tidy `coefs` data.frame, `n_obs`, `n_subjects`,
`lag`, `standardize`) lives in attributes, retrieved via `coefs(fit)` or
`attr(fit, ...)`. The container itself is a pure `netobject_group` so
iteration-based dispatch (`centrality.netobject_group`, etc.) works
without surprises.

#### Plotting — `splot.net_mlvar` lives in `R/plot-mlvar.R`

Each constituent is already a standard `cograph_network`, so direct
indexing works:

```r
cograph::splot(fit$temporal)         # directed, existing splot.netobject path
cograph::splot(fit$contemporaneous)  # undirected
cograph::splot(fit$between)          # undirected
```

The package also ships `splot.net_mlvar(x, type = ...)` in
`R/plot-mlvar.R`, which routes `type = "temporal" / "contemporaneous" /
"between"` (or their single-letter aliases `"t" / "c" / "b"`) to the
right constituent, and `type = "all"` to a 1x3 panel layout via
`graphics::par(mfrow = c(1, 3))`. Because of the NAMESPACE registration
bug described above, dispatch from `splot(fit)` goes through the
`inherits()` cascade in `R/splot.R`, not via `UseMethod`. Nestimate
itself does not define `plot.net_mlvar` and never imports cograph.

#### Per-type styling convention

If/when cograph adds special styling for mlvar networks, use the
`$method` slot on each constituent netobject — not a `type` argument —
to branch. The canonical mapping:

| `method` value           | recommended styling preset                           |
|--------------------------|------------------------------------------------------|
| `"mlvar_temporal"`       | `tna_styling = TRUE` (directed transition look)     |
| `"mlvar_contemporaneous"`| `psych_styling = TRUE` (undirected Okabe-Ito look)  |
| `"mlvar_between"`        | `psych_styling = TRUE` (undirected Okabe-Ito look)  |

Users override by passing `tna_styling = FALSE` / `psych_styling = FALSE`
via `...`.

#### Agent note — things to not break

1. **Class is `c("net_mlvar", "netobject_group")`**, not
   `c("net_mlvar", "cograph_network")`. The *group* is NOT itself a
   cograph_network; only its three constituents are. Don't try to
   `splot(fit)` directly — call `splot(fit$temporal)` etc.
2. **Metadata is in attributes, not list elements.** `fit$coefs` is
   NULL; use `attr(fit, "coefs")` or `Nestimate::coefs(fit)`. Similarly
   for `n_obs`, `n_subjects`, `lag`, `standardize`. The list stays pure
   so `lapply(fit, ...)` over the three networks is safe.
3. **Do not mutate constituent matrices in place.** Nestimate pins
   those to bit-for-bit equivalence with `mlVAR::mlVAR()` across 25 real
   ESM datasets and 20 simulated seeds. If you need to threshold or
   rescale, clone first.
4. **Nestimate never imports or calls cograph.** Any plot-level logic
   (titles, `type` selectors, panel layouts) belongs in cograph, not in
   Nestimate. The rendering layer depends on the data layer, not the
   other way around.

### TNA Styling and qgraph Translation

`from-qgraph.R` has two key roles:

1. **`.translate_qgraph_dots()`** — renames qgraph-style params (`vsize` -> `node_size`, `asize` -> `arrow_size`, `edge.color` -> `edge_color`, etc.) with value transforms (e.g., `asize * 0.20`). Called early in splot before dispatch, gated by `inherits(x, c("tna", ...))`. When both cograph name and qgraph alias are present, cograph name wins.

2. **`.tna_style_defaults()`** — `tna_styling = TRUE` (used by `plot_tna()` and `splot.netobject` for directed nets) applies TNA visual defaults:
   - NULL-default params: filled if user didn't set them
   - Non-NULL-default params: only overridden if user didn't explicitly pass them (checked via `"param_name" %in% explicit_args`)
   - User-explicit args always win

3. **`.psych_style_defaults()`** — the undirected counterpart, enabled via `psych_styling = TRUE`. Defined in `R/from-qgraph.R` and applied in `R/splot.R` and `R/plot-nestimate.R`. Produces an Okabe-Ito-palette, psych-network-style look for association/correlation networks. This is the default for `splot.netobject` on undirected input (e.g. `net_mlvar` contemporaneous/between networks). Uses the same "NULL-default fill, explicit-arg wins" precedence rules as `.tna_style_defaults()`.

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
- **S3method vs export in NAMESPACE**: `@export` on `splot.foo` emits `export(splot.foo)`. Use `@method splot foo` + `@export` to emit `S3method(splot, foo)`. For `splot` specifically, neither form dispatches at runtime because `splot()` has no `UseMethod` call (see the `splot() Dispatch` section) — the `inherits()` cascade is authoritative. Still, prefer the `S3method` form for NAMESPACE hygiene on any *other* generic where `UseMethod` **is** used.
- **`dontrun` vs `donttest`**: Do NOT blindly convert `\dontrun` to `\donttest`. Many examples use undefined variables or depend on optional packages that mask cograph functions. Only convert fully self-contained, runnable examples.
- **Nestimate field differences from tna**: `net_bootstrap$original$weights` (not `$weights`), `$ci_level` (not `$level`). `net_permutation` p_values/effect_size are already matrices. `boot_glasso` edge names use `" -- "` separator.

## Test Conventions

~139 test files (~47 feature + 92 coverage), ~13,700+ expectations. Coverage tests follow `test-coverage-{module}-{round}.R` (rounds: 40, 41, 42, ...). Target: 100% line coverage (achieved). Use `# nocov` only for genuinely unreachable defensive guards.

**Centrality equivalence tests**: The centrality measures (see Project Overview for count) are validated against external reference implementations (centiserve, sna, brainGraph, influenceR, igraph, tidygraph, NetworkX via reticulate). Equivalence tests live alongside coverage tests and use `tolerance` arguments per measure — see `HANDOFF.md` for the full validation matrix (exact-match vs. formula-verified vs. rank-correlation tiers).

Two test helper files load before every test:
- `tests/testthat/helper-cograph.R` — exposes internal functions via `cograph:::` for testing
- `tests/testthat/helper-test-utils.R` — test data generators (`create_test_matrix()`, `create_test_edgelist()`, etc.) and custom expectations

Never put `devtools::load_all()` inside test files — it breaks covr.

## Optional Dependencies

All suggested packages must be guarded with `requireNamespace("pkg", quietly = TRUE)`. Use `# nocov` on fallback branches unreachable in the test environment.

## Session Artifacts

- `NEWS.md` — Per-release CRAN changelog. Newest version at the top. This is what CRAN / `utils::news(package = "cograph")` reads. Add user-visible changes here on bumps.
- `docs/LEARNINGS.md` — Accumulated pitfalls/discoveries (e.g., CRAN timing behavior, Windows check quirks). Append-only.
- `docs/CHANGES.md` — Dev-facing human changelog (newest first) — richer / more exploratory than `NEWS.md`.
- `HANDOFF.md` (repo root) — Session state for continuity across conversations. Overwritten each session.

## pkgdown Site

`_pkgdown.yml` configures the documentation site (Bootstrap 5, Yeti theme). CI deploys to `gh-pages` on push to main. Uses `quarto-dev/quarto-actions/setup@v2` because some articles are `.qmd` (Quarto) rather than `.Rmd`.

## CRAN Submission

`cran-comments.md` tracks submission notes. Before submitting: run the strict CRAN incoming check (see Commands above), ensure zero NOTEs, and verify `Additional_repositories` points to `https://mohsaqr.r-universe.dev` for Nestimate.
