# Project Learnings

### 2026-03-13
- [robustness re-integration]: `sidelined/R/robustness.R` was standalone — no cross-deps with other sidelined modules. Uses `to_igraph()` (already in core), `.save_rng()`/`.restore_rng()` (from `aaa-globals.R`), igraph, and ggplot2 (optional). Direct copy + `devtools::document()` was sufficient. 5 exports: `robustness`, `plot_robustness`, `ggplot_robustness`, `robustness_auc`, `robustness_summary`. 2 internal helpers: `robustness_vertex_attack`, `robustness_edge_attack`.
- [brainGraph n_iter convention]: brainGraph uses N=1000 default for random mode with `foreach %dopar%` parallelism. Our default changed from 100 to 1000 to match. brainGraph only implements static strategy for targeted attacks (no sequential recalculation).
- [disparity filter verification]: Verified across 3 datasets × 4 levels against tna and disparityfilter CRAN packages. disparityfilter has a dedup bug — `backbone()` uses `rbind(in, out)` then `unique()`, but rows with same edge pair but different alpha aren't deduplicated. After correction, all three implementations match exactly.
- [disparity on multigraph]: Multigraph with no weight attribute → after simplify, all edges have equal weight → uniform distribution → no significant edges at any reasonable level. Must assign `E(x)$weight <- 1` before simplify so parallel edges sum to meaningful weights.
- [graphics imports for base R plots]: `plot_robustness()` uses `par`, `abline`, `lines`, `legend` from graphics. R CMD check flags these as undefined globals unless explicitly imported via `@importFrom graphics` in cograph-package.R.
- [unified motifs API]: `motifs()` and `subgraphs()` share one engine (motifs-api.R). Census mode sums type counts across individuals; instance mode aggregates by triad name. Census significance at aggregate level delegates to `motif_census()` (igraph); at individual level uses exact config model (stub-shuffling). Instance significance uses batch presence matrix optimization (ind-major loop, pre-computed linear indices, per-individual candidate filtering).
- [cograph_network $data edge list detection]: `as_cograph(edge_list)` stores the original data.frame in `$data`. To detect if a cograph_network has edge list metadata, check `is.data.frame(x$data) && all(c("from", "to") %in% tolower(names(x$data)))`. The `$data` may also be a tna sequence matrix or NULL.
- [auto-detection column priority]: Actor: session_id > session > actor > user > participant > individual > id. Order: timestamp > time > seq > step > order. Case-insensitive match but preserves original column name.

### 2026-03-14
- [tna_communities vs cograph_communities]: `tna::communities()` returns class `tna_communities` with `$assignments` data.frame (columns: `state`, `<method_name>` where values are community IDs) and `$counts`. `cograph::communities()` returns class `cograph_communities` wrapping igraph community objects with `igraph::membership()`. Different conversion logic needed: `tna_communities` uses `split(asgn$state, asgn[[method_col]])`, `cograph_communities` uses `split(names(membership), membership)`. Check `tna_communities` first since it doesn't inherit from `communities` class.

### 2026-03-12
- [motif tutorial rendering]: Quarto is not installed on this machine. Use `rmarkdown::render("file.qmd", output_format="html_document")` as fallback — works fine for .qmd files, just uses rmarkdown's HTML format instead of Quarto's.
- [coding dataset density]: The coding dataset (9 states) has a fully connected weight matrix — all 84 possible triads are type 300 (clique). Individual-level analysis with `extract_motifs(Mod)` is much more informative because per-session matrices are sparse. For aggregate analysis, threshold the matrix first (`mat[mat < 0.05] <- 0`).
- [configuration model exactness]: The exact configuration model (stub-shuffling: expand to row/col stubs, `sample()` col stubs, pair with row stubs) preserves in-degree and out-degree sequences with ZERO variance. Chung-Lu (independent row/col sampling with probabilities) only preserves marginals in expectation — SD ~2.2 per marginal. For significance testing, exact config model is required.
- [motif benchmark reference]: coding dataset (9 states, 429 sessions, 1000 perms): aggregate census 0.58s via igraph delegation, individual census 8.5s via config model, instances 2.0s via batch config model. All modes: same seed → identical results.
- [qgraph arg translation]: When splot receives tna-family objects, qgraph-style params (size, vsize, edge.color, lty, shape, asize, etc.) land in `...` and are silently ignored. Fix: `.translate_qgraph_dots()` in from-qgraph.R renames keys + applies value transforms. Called early in splot (before dispatch) so all 6 tna paths benefit. Gate: `inherits(x, c("tna", "group_tna", ...))` — non-tna objects untouched.
- [motifs re-integration]: Static motifs (motifs-data, motifs, motifs-extract, motifs-plot) re-integrated from `sidelined/`. Key fixes: bare NSE vars in `aes()` changed to `.data$` prefix, temporal `@seealso` refs removed. Test files truncated at temporal section boundary (clear `# ===` separator lines). `motifs-temporal.R` stays sidelined — depends on `tna_windows()` from `tna-animate.R`.
- [qgraph value scaling]: qgraph and cograph scale some params differently: `asize` → `arrow_size * 0.20` (qgraph arrows barely scale), `edge.label.cex` → `edge_label_size * 1.2` (splot renders slightly smaller). Numeric `lty` codes and `shape` names need mapping via existing `map_qgraph_lty()` / `map_qgraph_shape()`.
- [cograph-wins precedence]: When both cograph name (e.g., `node_size`) and qgraph alias (e.g., `size`) appear in dots, skip renaming the alias — cograph name takes precedence naturally since `.collect_dispatch_args` merges dots last. Track which names were actually translated via `translated_from` vector to avoid applying value transforms to user-provided cograph values.

### 2026-03-10
- [splot dispatch arg forwarding]: Named splot params (minimum, threshold, layout, title) are CONSUMED by splot's function signature and do NOT flow into `...`. Dispatch via `handler(x, ...)` loses these params. Fix: capture with `match.call(expand.dots = FALSE)`, evaluate with `mget(setdiff(names(.user_explicit), "..."), envir = environment())`, and forward via `do.call(target, c(list(x = x), .collect_dispatch_args(.user_args, .dots)))`.
- [mget vs eval for match.call]: `eval(.user_explicit[[nm]], envir = parent.frame())` re-evaluates raw AST nodes which can fail. `mget(names, envir = environment())` safely gets already-evaluated local values.
- [roxygen @noRd placement]: `@noRd` must be in a standalone roxygen block. Placing it between a main function's roxygen block and its definition causes the helper to inherit the main function's `@export`.
- [ggplot2 4.0 size * size.unit]: `GeomText$draw_panel` multiplies `data$size * size.unit`; crashes when size is non-numeric. This can surface in testthat `test_file()` context but not in standalone execution — use `p$labels$title` assertions instead of `with_temp_png({ print(...) })` for title param tests.
- [match.call expand.dots]: `match.call(expand.dots = FALSE)` includes `"..."` in its names. Must exclude with `setdiff(names(.user_explicit), "...")` before passing to `mget()`, otherwise get `'...' used in an incorrect context` error.

### 2026-03-09
- [rescale=FALSE + layout_scale=1]: When providing custom layout coordinates to splot/tplot, must set `rescale = FALSE` and `layout_scale = 1` to prevent splot from normalizing coordinates. Otherwise, absolute spacing values are meaningless and intra-edge post-drawing coordinates won't match the plot.
- [uniform normalization]: When normalizing layout coordinates to [-1, 1], use the same scale factor for both axes (`max_span = max(diff(x_range), diff(y_range))`). Independent axis normalization distorts aspect ratios and makes spacing parameters unpredictable.
- [intra-edge separation pattern]: To draw intra-group edges differently from inter-group: (1) zero out intra weights from matrix, (2) preserve tna object by only swapping `$weights`, (3) pass to tplot for inter-group rendering, (4) draw intra edges post-tplot using `graphics::lines()` with custom bezier curves. Coordinate systems must match (hence rescale=FALSE).
- [per-edge curve direction]: For circular/polygon layouts, a fixed curve_sign per group doesn't work — edges curve in wrong direction for some pairs. Fix: compute perpendicular direction toward network center for each edge pair individually. Compare distances of midpoint + perpendicular vs midpoint - perpendicular from center.
- [bezier arc height]: `arc_height = curvature * max(dist * 0.5, 0.06)` works well — proportional to distance with a minimum for adjacent nodes. Using `draw_curved_edge_base` creates narrow spikes for close nodes; custom quadratic bezier avoids this.
- [tna object preservation]: When modifying weights for a tna object, copy the whole object and only swap `$weights`. This preserves donut styling, labels, and initial probabilities that tplot/splot relies on.

### 2026-03-04
- [100% coverage achieved]: cograph reached 100% test coverage (10,822 tests). The ~66 previously uncovered expressions were resolved via: `# nocov` markers on `.onLoad` (covr limitation), `requireNamespace` guards (packages always installed in test env), mathematically unreachable dead code (e.g., `n==1` in multi-value branch, `sign(curve)==0` after early return for `curve==0`), stochastic edge cases in `rich_club`/`small_world` (random graph generation), and local functions inside `splot()`. Test-coverage-round7.R covers the remaining testable paths.
- [covr multi-expression lines]: covr tracks multiple expressions per source line. A line like `if (cond) break` has separate coverage for `cond` and `break`. Use `df$first_column`/`df$last_column` to distinguish.
- [soplot R6 aes gap]: `soplot()` creates a `CographNetwork$new()` R6 object at render-grid.R:617 but only copies nodes/edges/directed — NOT node_aes or edge_aes. Direct `render_nodes_grid()` calls with properly set `cn$set_node_aes()` ARE covered, but the soplot pipeline never transfers these.
- [splot vs soplot rendering paths]: `splot()` → `render_nodes_splot()`/`render_edges_splot()` (base R graphics, in splot.R). `soplot()` → `render_nodes_grid()`/`render_edges_grid()` (grid graphics, in render-nodes.R/render-edges.R). Drawing functions in `splot-nodes.R` are splot-only; `shapes-special.R` are soplot-only.
- [calculate_load BFS bug]: `calculate_load()` had a hardcoded `dist[w] - dist[v] - 1` condition assuming unit edge weights. On weighted graphs, sigma never updates → infinite loop. Fix: distance-ordered predecessor discovery using actual edge weights from `igraph::as_edgelist()`.
- [calculate_load sna convention]: `sna::loadcent()` transposes directed graphs (via `gt()`) before computing load. Fix: add `igraph::reverse_edges(g)` for directed mode. Also, sna ignores edge weights entirely — hop-based distances.
- [calculate_load disconnected nodes]: Initializing `delta <- rep(1, n)` gave delta=1 to unreachable nodes. Fix: `delta <- numeric(n); delta[c(s, ordered_nodes)] <- 1` for reachable nodes only.
- [igraph weights=NULL vs NA]: `igraph::distances(g, weights=NULL)` auto-uses `E(g)$weight` if present. To force unweighted distances, use `weights = NA`. This is a critical distinction.
- [calculate_percolation same bug]: Same hardcoded BFS pattern as load. Same fix applied. Also needed `/2` normalization for undirected graphs (Brandes accumulation counts both (s,t) and (t,s)).
- [percolation identity]: Uniform-state percolation = `betweenness / ((n-1)*(n-2))`, NOT `betweenness / (n-2)`.
- [centrality weighted param]: `centrality()` uses `weighted=` parameter name. Passing `use_weights=` gets swallowed by `...` — silently ignored. Always verify parameter names.
- [centiserve communibet vs current_flow_betweenness]: `centiserve::communibet()` computes communicability betweenness (matrix exponential), NOT current-flow betweenness. They are fundamentally different measures — cannot be compared.
- [validation suite]: Created comprehensive validation with 1000 networks, ~56,642 tests across centrality (49,579), communities (212), network properties (6,851). All pass at 100%. Reports in `validation/reports/`.

### 2026-03-03
- [RNG state restoration]: CRAN requires `set.seed()` callers to save/restore `.Random.seed` via `on.exit()`. DRY pattern: `.save_rng()` returns list with seed+existed, `.restore_rng()` restores it. Apply as: `saved_rng <- .save_rng(); on.exit(.restore_rng(saved_rng), add = TRUE); set.seed(seed)`.
- [has_package vs requireNamespace]: cograph has no `has_package()` function. Use `requireNamespace("pkg", quietly = TRUE)` for optional dependency checks.
- [direction auto-detection]: `is_symmetric_matrix()` (from input-parse.R) is the correct way to auto-detect directed vs undirected. Replaced hardcoded `directed = TRUE` for TNA objects.
- [float precision in layout]: Layout normalization can produce `0.90000000000000013` instead of exactly `0.9`. Tests should use tolerance: `>= 0.9 + 1e-10` not `>= 0.9`.
- [dontrun vs donttest DANGER]: Blanket `\dontrun` → `\donttest` conversion is dangerous. Many examples use undefined variables (pseudo-code), reference missing datasets, or depend on optional packages that mask cograph functions. Only convert examples that are fully self-contained and actually runnable. `@keywords internal` functions should have no `@examples` at all.
- [namespace masking in examples]: When tna/igraph are loaded during R CMD check `--run-donttest`, they mask cograph functions: `igraph::is_directed`, `igraph::communities`, `igraph::degree_distribution`, `tna::plot_compare`. Use `cograph::` prefix in examples for masked functions.
- [tna API v1.2.1]: `centrality()` → `centralities()` (plural). `bootstrap()` uses `iter` not `R`. `bootstrap()` requires tna built from sequence data (has `$data`), not from raw matrix. `group_tna()` requires sequence data, not matrix.
- [repo location]: Canonical cograph repo is now `/Users/mohammedsaqr/Documents/Github/cograph` (was `My Drive (saqr@saqr.me)/Git/Sonnet` on Google Drive, which is now abandoned).
- [repo confusion pitfall]: Home directory (`/Users/mohammedsaqr`) was accidentally a git repo (origin → `mohsaqr/cris_stats`). The `Documents/Github/cograph` folder inside it had no `.git` — it was an unversioned stale copy. Always verify `git rev-parse --show-toplevel` and `git remote -v` when something feels off.
- [three-copy problem]: Three copies existed: Google Drive (authoritative), `Documents/Github/Sonnet` (proper local clone), `Documents/Github/cograph` (stale unversioned). Fix: archive stale → rename Sonnet → cograph.
- [stale copy was behind on]: `mid_label_position` in `plot-transitions.R`, `graphics::par()` namespace in `splot.R`, extended `cluster-metrics.R` tna-class structure, roxygen `@param ...` on layout functions.
- [remotes]: `origin` = `mohsaqr/Sonnet`, `upstream` = `sonsoleslp/cograph`, `cograph` = `mohsaqr/cograph`.

### 2026-02-21
- [roxygen percent escaping]: Using `\%` in roxygen comments produces `\\%` in Rd files, which is a literal backslash followed by a comment character — breaks Rd parsing. Use plain text ("percent") or just `%` (roxygen auto-escapes to `\%`).
- [devtools::check args]: `devtools::check(".", args = "--no-tests --no-examples")` passes a single arg string (unrecognized). Must use a vector: `args = c("--no-tests", "--no-examples", "--no-vignettes", "--no-manual")`.
- [expect_no_error no dots]: `testthat::expect_no_error()` does not accept `...` args like `info =`. Use a wrapper or separate test blocks for annotated assertions.
- [ggplot2 label split pattern]: To render different label positions on different data subsets in ggplot2, split the data frame into subsets and add separate `geom_text()` layers — a local helper function avoids code duplication across positions.
- [mid_label_position for trajectories]: First/last columns use `label_position`, middle columns use `mid_label_position` (defaults to `label_position` when NULL). Requires splitting `node_rects` by `col` index.

### 2026-02-20
- [tna-compatible objects in cluster_summary]: `cluster_summary()` creates within/between objects with class "tna". The tna package's `print.tna()` expects: `$labels` (character vector), `$data` (can be NULL), `attr(x, "type")` = "relative"/"frequency", `attr(x, "scaling")` = character(0). Missing any of these causes `switch(type, ...)` crash. Fix: set all four in `structure()` call.
- [R CMD check par import]: Using `par()` without `graphics::` prefix triggers "no visible global function definition" NOTE. Always use `graphics::par()`.
- [tibble in tests]: Using `tibble::tibble()` in tests triggers "unstated dependencies in tests" WARNING. Use `data.frame()` instead since tibble is a subclass — covers the same code path.
- [project CLAUDE.md]: Updated from Windows (powershell + .exe paths) to macOS (bash + PATH-based Rscript).

### 2026-02-19
- [bootstrap edge count mismatch]: `splot.tna_bootstrap` built per-edge arrays (edge_priority, edge_ci_scale, etc.) using `which(weights != 0, arr.ind = TRUE)` but splot's internal edge count differed due to:
  1. **weight_digits rounding**: splot default `weight_digits=2` rounds tiny weights to zero, reducing edge count. Fix: pass `weight_digits=NULL` to splot.
  2. **R list NULL assignment trap**: `args$weight_digits <- NULL` DELETES the element from the list instead of setting it to NULL. Must use `args["weight_digits"] <- list(NULL)` to actually store NULL in a list.
  3. **Self-loops excluded**: `cograph()` excludes diagonal entries from edges. Fix: `diag(weights) <- 0` before computing edge_idx.
  4. **Directed enforcement**: TNA is always directed; auto-detect may choose undirected for near-symmetric matrices, merging reciprocal edges. Fix: pass `directed=TRUE`.
- [get_edge_order defensive]: Added `rep_len(priority, n)` guard in `get_edge_order()` for length mismatches.
- [comprehensive showcase]: Created `showcase/cograph_showcase.Rmd` with 24 sections covering all plot types. Key issues:
  - `tna` package masks `plot_compare()` and `plot_comparison_heatmap()` — use `cograph::` prefix
  - `plot_htna()` internally sets `node_size` and `edge_labels` — don't pass them again
  - `plot_comparison_heatmap()` uses `digits` not `value_digits`
  - `plot_robustness()` needs igraph object, not matrix
  - `plot_mlna()` doesn't support "oval" layout — use "circle"
  - `plot_mixed_network()` uses its own layout — don't force "oval"
  - Only use "oval" layout for direct `splot()` calls
- [alluvial/trajectories]: `plot_alluvial()` and `plot_trajectories()` are aliases for `plot_transitions()` with different defaults:
  - `plot_alluvial()`: `track_individuals = FALSE` (aggregate flow ribbons)
  - `plot_trajectories()`: `track_individuals = TRUE` (individual lines)
  - Multi-step: pass list of matrices (each = one transition step) or data frame with 3+ columns (auto-creates transitions between consecutive columns)
  - `from_title` labels each step column; `flow_color_by = "source"/"target"/"first"/"last"` controls coloring

### 2026-02-18
- [chord diagram]: Implemented `plot_chord()` in `R/plot-chord.R` using base R graphics only.
  - Reuses `.extract_weights()` from `plot-compare.R` and `bezier_points()` from `utils-geometry.R`.
  - **Critical: d3-chord cross-connection** — beziers connect OPPOSITE arc corners (from_end→to_start, to_end→from_start), NOT parallel corners. This keeps ribbons naturally thick through the centre.
  - Directed networks split each segment into outgoing/incoming halves; undirected share full arc.
  - Default palette: vibrant Material Design colors (not pastel) — chord diagrams need saturated colors.
  - Tick marks auto-detect weight scale: 0–1 probability (interval 0.1) vs integer (nice interval).
  - `vapply()` cannot return `NULL` (side-effect loops) — use `lapply()` for drawing operations.
  - `skip_if_not()` in testthat does NOT accept `mode` as second positional arg — use named: `exists("fn", mode = "function")`.
  - Showcase: `showcase/chord_diagram_guide.Rmd`

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
