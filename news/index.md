# Changelog

## cograph 2.4.4

### New features

- **Producer-supplied splot metadata** (`x$meta$splot`): packages that
  create cograph-plottable objects can now attach a small rendering
  contract — `renderer` (resolved through a cograph-maintained whitelist
  of existing renderers; arbitrary function names are never evaluated),
  `weight` (which stored edge quantity to render: an edge column keeps
  the producer’s edge set, a matrix redefines the drawn network from its
  nonzero cells, aligned by dimnames), and `defaults` (renderer
  arguments). Precedence is always
  `user arguments > meta$splot$defaults > cograph defaults`; on the
  regular network path this includes deprecated argument aliases (a
  user-supplied `positive_color` still beats a metadata
  `edge_positive_color` default). See
  [`?splot`](https://sonsoles.me/cograph/reference/splot.md), section
  “Producer-Supplied splot Metadata”.

### Bug fixes / changes

- `plot_edge_diff_forest(layout = "chord")` no longer emits a spurious
  “row names were found from a short variable and have been discarded”
  warning for every node arc it draws.

- [`detect_communities()`](https://sonsoles.me/cograph/reference/detect_communities.md)
  with the `"louvain"` (the default) or `"leiden"` method no longer
  errors on a **directed** graph. These igraph algorithms are
  undirected-only, so `detect_communities(tna_object)` — a tna model is
  always directed — aborted with “Multi-level community detection works
  for undirected graphs only”. It now collapses the directed edges to
  undirected (mean, as the `"fast_greedy"` method already did) with a
  message, so the package’s primary object type works with the default
  algorithm. This also fixes `plot_htna(x, community = "louvain")` and
  other internal callers that ran community detection on a directed
  model.

- [`splot()`](https://sonsoles.me/cograph/reference/splot.md) on a
  Nestimate `netdifference` (from `subtract_networks()` /
  `as_netdifference()`) now routes to
  [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md).
  Previously it fell through to the `netobject` path, which styles by
  `$method` — “difference” is not a TNA-family method, so the asymmetric
  difference matrix was drawn with undirected psych styling: no
  arrowheads and one triangle of each asymmetric edge pair silently
  dropped. `splot(d, minimum = 3)` is now the straightforward call for a
  signed difference network.

- The `netdifference` routing excludes `net_permutation`-family objects:
  `net_bayes` carries both classes and must keep reaching
  `splot.net_permutation`, whose per-edge CI/star arrays are aligned by
  `Nestimate::plot.net_bayes` to that renderer’s edge ordering.

- [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  on a `netdifference` now draws the display matrix (`$weights` —
  e.g. only the credible differences when coerced with
  `as_netdifference(b, significant_only = TRUE)`), falling back to
  `$difference_matrix`. For `subtract_networks()` results the two are
  identical, so nothing changes there.

- [`plot_permutation()`](https://sonsoles.me/cograph/reference/plot_permutation.md)
  /
  [`splot.net_permutation()`](https://sonsoles.me/cograph/reference/splot.md):
  the `title` and `layout` defaults now use exact `[[` indexing.
  `args$title` on a dots-list holding `title_size` (but no `title`)
  partially matched `title_size`, so the default title was silently
  skipped and no title was drawn — this is why
  `Nestimate::plot.net_bayes()` output had no title. Same latent hazard
  fixed for `layout` / `layout_scale`.

- Edge label templates gain a `{p_diff}` placeholder (probability of the
  difference, for Bayesian comparisons), fed by the new
  `edge_label_p_diff` argument — a per-edge vector or a full
  node-by-node matrix (the matrix is indexed at each drawn edge, so it
  survives `minimum`/`threshold` filtering, and is aligned by dimnames
  so it may be supplied in any node order). Filled automatically from
  `$p_difference` by `splot.net_permutation` and by
  [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  on Bayesian `netdifference` coercions. Template example:
  `edge_label_template = "{est} (P={p_diff})"`.

- [`splot.netobject()`](https://sonsoles.me/cograph/reference/splot.md)
  styling classifier: `"edge_betweenness"` networks are now styled by
  their directedness. A directed edge-betweenness network previously
  fell into psych styling — drawn undirected, silently losing one
  direction of each asymmetric pair; it now gets the TNA presets with
  arrows. An undirected one (from a correlation-family source —
  Nestimate preserves the source’s directedness) keeps the psych look.

- Nestimate producers now use the `meta$splot` contract: `netdifference`
  objects carry `renderer = "difference"` and `net_bayes` carries
  `renderer = "permutation"`, so metadata routing (which runs before
  class dispatch) selects the renderer; the `netdifference` class branch
  remains as a fallback for objects built without metadata.

## cograph 2.4.3

### Bug fixes / changes

- [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  no longer hides small difference edges: it defaults `minimum = 0` (the
  style presets otherwise injected `minimum = 0.01`, silently dropping
  edges with `|x - y| < 0.01`). An explicit `minimum` still wins.

- `plot_difference(x, y, difference = TRUE)` now warns that `y` is
  ignored and uses `x` as the difference network, instead of silently
  computing `x - y`.

## cograph 2.4.2

### Bug fixes / changes

- [`plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md)
  is **no longer deprecated** — it is a plain alias of
  [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md).
  [`tna::plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md)
  delegates to it by name (`cograph::plot_compare(x, y, ...)`), so
  deprecating it wrongly made every
  [`tna::plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md)
  call emit a warning; the warning is removed. Both names call the same
  implementation;
  [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  is the preferred spelling for new cograph code.

- [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  also auto-detects a Nestimate `netdifference` object (or any object
  exposing `$difference_matrix`), alongside `tna_comparison`.

## cograph 2.4.1

### New features

- [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  can now consume a **pre-computed difference network**: a
  `tna_comparison` object (from
  [`tna::compare()`](http://sonsoles.me/tna/reference/compare.md)) is
  detected automatically and its `$difference_matrix` is plotted, and
  `difference = TRUE` treats `x` as an already-subtracted matrix/network
  (no `y` needed). The two-network `plot_difference(x, y)` path is
  unchanged.

## cograph 2.4.0

### New features

- Two focal-node flow layouts, usable anywhere a layout name is accepted
  (`splot(x, layout = "target")` / `layout = "saqr"`):
  - [`layout_target()`](https://sonsoles.me/cograph/reference/layout_target.md)
    ports qgraph’s [`flow()`](https://rdrr.io/pkg/qgraph/man/flow.html)
    — places one node of interest (`target =`) on the left and every
    other node in columns by unweighted BFS distance (hops). Unlike
    qgraph it handles disconnected graphs (isolated nodes go to a
    trailing column) instead of erroring.
  - [`layout_saqr()`](https://sonsoles.me/cograph/reference/layout_saqr.md)
    ports the Dynalytics Desktop “saqr” transition layout (Saqr et al.,
    LAK25): Start on top, End on bottom, middle nodes ranked by outgoing
    weight from Start and split into 2–3 sine-enveloped rows with a
    zig-zag first row (`start =`, `end =`, `jitter =`).

### Bug fixes / changes

- [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  now **styles the difference network automatically** instead of drawing
  bare default-blue nodes: an undirected difference gets the
  psychometric look (Okabe-Ito node palette, no arrows, thin edges), a
  directed difference gets the TNA look (TNA palette, arrows). Node size
  uses the calibrated preset (previously nodes could render
  near-invisible), and edges stay coloured by the sign of the
  difference. Explicit `node_*` arguments still override the preset.

- [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  is added as the preferred name for the difference-network plotter.
  [`plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md)
  remains a first-class alias of it
  ([`tna::plot_compare()`](http://sonsoles.me/tna/reference/plot_compare.md)
  delegates to
  [`cograph::plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md)
  by name, so the name must keep working).

- [`plot_difference()`](https://sonsoles.me/cograph/reference/plot_difference.md)
  (the renamed difference-network plotter) now treats an S3
  `cograph_network` (which is itself a list — e.g. a `psychnet` fit, a
  Nestimate `netobject`, or any
  [`as_cograph()`](https://sonsoles.me/cograph/reference/as_cograph.md)
  result) as a single network. Previously such an object fell into the
  “plain list of networks” branch and was misread as a list of
  sub-networks, failing with “x must be a matrix, cograph_network, tna,
  or igraph object”. Comparing two psychnet/netobject networks with
  `plot_difference(net1, net2)` now works.

## cograph 2.3.11

### New features

- [`dyad_census()`](https://sonsoles.me/cograph/reference/dyad_census.md)
  classifies every dyad of a directed network into mutual (M),
  asymmetric (A), or null (N), returning a tidy one-row-per-type
  data.frame with counts and proportions and a dyad-based reciprocity
  (`2M / (2M + A)`) attribute. It is the dyad-level companion to
  [`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md).
  Undirected input counts every edge as a mutual dyad.

- [`ego_networks()`](https://sonsoles.me/cograph/reference/ego_networks.md)
  reports tidy per-ego personal-network metrics — size, ego/alter tie
  counts and densities, and Burt’s structural-hole measures
  (`effective_size`, `constraint`, `order = 1` only) — with one row per
  ego. The structural-hole columns reuse the same implementations as
  [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md),
  so they match
  `centrality(x, measures = c("effective_size", "constraint"))` exactly.

## cograph 2.3.10

### Bug fixes / changes

- Bootstrap plots of undirected co-occurrence networks
  (`splot.net_bootstrap`) now default to the `"oval"` layout instead of
  the force-directed `"spring"` layout, matching `splot.tna_bootstrap`.
  Pass `layout = "spring"` to restore the previous behavior.

- Bootstrap plots now auto-suppress the `".00"` decimal tail on
  integer-valued weight matrices (co-occurrence counts, raw
  frequencies): `266.00**` renders as `266**`. Detection mirrors
  `splot.netobject` — when every nonzero weight is a whole number and
  the user has not set `weight_digits`, both `weight_digits` and
  `edge_label_digits` default to `0`. Applies to both
  `splot.net_bootstrap` and `splot.tna_bootstrap`. Non-integer
  (correlation/GLASSO) networks are unaffected, and an explicit
  `weight_digits` always wins.

## cograph 2.3.9

### New features

- [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  gains a `theme` argument: `"classic"` (default — the established
  pie-node / straight-edge look, now with thinner node and shell borders
  and slightly larger detail nodes), `"rich"` (donut nodes on both
  layers plus curved summary edges and splot self-loops), and `"light"`
  (`"rich"` with no shell outline and a softer fill). Granular overrides
  `node_donut`, `node_donut_inner_ratio`, `summary_donut_inner_ratio`,
  `summary_donut_show_value`, `curved_edges`, and `summary_curve` win
  over the preset.

- [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  now colors edges by weight sign on every layer (within-cluster,
  between-cluster, summary, and self-loops) via `edge_color_by`:
  `"auto"` (default) keeps cluster coloring for non-negative transition
  networks but switches to sign coloring when any negative weight is
  present (correlation / association networks), `"cluster"` and `"sign"`
  force either mode. Positive edges use `edge_positive_color`
  (`"#2E7D32"`, green) and negative edges `edge_negative_color`
  (`"#C62828"`, red), matching
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md). Edge
  visibility thresholding and width scaling now use the absolute weight,
  so negative edges are drawn rather than silently dropped, and a
  positive/negative key is added to the legend when sign coloring is
  active.

- [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  summary-node labels are now placed “on the clock”: each label sits
  just outside its node in the cardinal direction the node points from
  the arrangement center (top at 12, bottom at 6, left at 9, right at
  3), anchored at the node boundary so it always clears the node
  regardless of `summary_size`. An explicit `summary_label_position`
  still overrides this.

## cograph 2.3.8

### New features

- [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  and [`splot()`](https://sonsoles.me/cograph/reference/splot.md) accept
  `mcml_pc` objects (`Nestimate::build_mcml_pc()`, experimental
  psychometric MCML) and render them undirected via their
  `meta$directed` flag.

## cograph 2.3.7

### Breaking changes

- The exported names
  [`cluster_summary()`](https://saqr.me/Nestimate/reference/cluster_summary.html)
  and
  [`build_mcml()`](https://saqr.me/Nestimate/reference/build_mcml.html)
  are removed to end, permanently, the collision with
  [`Nestimate::cluster_summary()`](https://saqr.me/Nestimate/reference/cluster_summary.html)
  and
  [`Nestimate::build_mcml()`](https://saqr.me/Nestimate/reference/build_mcml.html)
  — different functions that silently masked each other depending on
  package attach order (the same disease as the
  [`cluster_network()`](https://saqr.me/Nestimate/reference/cluster_network.html)
  alias removed in 2.3.6, where load order silently flipped results
  between raw counts and row-normalized probabilities). Migration is
  name-for-name with identical behavior:
  - `cluster_summary(...)` → `csum(...)` (the existing short alias is
    now the canonical exported name; same arguments, same
    `cluster_summary` return object).
  - `build_mcml(...)` → `summarize_clusters(...)` (same arguments, same
    `mcml` return object). In sessions where both packages are attached,
    the bare names
    [`cluster_summary()`](https://saqr.me/Nestimate/reference/cluster_summary.html)
    /
    [`build_mcml()`](https://saqr.me/Nestimate/reference/build_mcml.html)
    now always refer to Nestimate’s data-layer verbs, regardless of
    attach order. The
    [`as_tna()`](https://sonsoles.me/cograph/reference/as_tna.md)
    generic is intentionally exported by both packages: the definitions
    are identical (`function(x) UseMethod("as_tna")`), so masking is
    harmless and S3 methods from both packages dispatch correctly.

### New features

- [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  gains a `directed` argument (default `NULL` = auto-detect). Undirected
  rendering suppresses arrowheads on all three edge layers
  (within-cluster, between-cluster, summary), draws each symmetric edge
  pair once instead of twice (previously a symmetric matrix produced
  overplotted reciprocal arrows), and moves edge labels to the edge
  midpoint. Auto-detection reads `$meta$directed` from
  `cluster_summary`/`mcml` input (e.g., co-occurrence aggregations such
  as `Nestimate::build_mcml(type = "cooccurrence")` now render
  undirected with no extra flag), the `$directed` field of network
  objects, or matrix symmetry for plain matrices — the same contract as
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md), which
  forwards `directed` when dispatching `mcml`/`cluster_summary` objects.
- [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  undirected matrix input is aggregated with
  `cluster_summary(type = "cooccurrence")` (symmetrized counts) instead
  of the row-normalized `type = "tna"`, whose output is asymmetric even
  for symmetric input and cannot be represented by undirected drawing.
  When `directed = FALSE` is forced on weights that are not symmetric,
  [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  now warns that only the upper triangle is drawn.

### Bug fixes

- [`cluster_summary()`](https://saqr.me/Nestimate/reference/cluster_summary.html)
  and the sequence path of
  [`build_mcml()`](https://saqr.me/Nestimate/reference/build_mcml.html)
  now record the *effective* directedness in `$meta$directed`: `FALSE`
  when `type = "cooccurrence"` (which symmetrizes the weights), instead
  of echoing the `directed` argument unchanged.

## cograph 2.3.6

CRAN release: 2026-05-31

### Bug fixes

- Removed the
  [`cluster_network()`](https://saqr.me/Nestimate/reference/cluster_network.html)
  alias for
  [`summarize_network()`](https://sonsoles.me/cograph/reference/summarize_network.md).
  It collided with
  [`Nestimate::cluster_network()`](https://saqr.me/Nestimate/reference/cluster_network.html)
  — a completely different function (PAM clustering on sequence data,
  one network per cluster) — and the two silently masked each other
  depending on package attach order, producing confusing
  `unused arguments (k = ..., cluster_by = ...)` errors. Use
  [`summarize_network()`](https://sonsoles.me/cograph/reference/summarize_network.md)
  (or its remaining short form
  [`cnet()`](https://sonsoles.me/cograph/reference/summarize_network.md))
  for matrix-to-cluster aggregation in cograph.

## cograph 2.3.5

### Documentation

- Added Sonsoles López-Pernas as co-copyright holder in `LICENSE`.
- README / docs wording fixes (e.g. “hyper order” → “higher-order”).
- Introduction vignette no longer asserts a fixed centrality-measure
  count, which kept drifting as the measure set grew.

## cograph 2.3.4

### Bug fixes

- `.smooth_blob()` (used by
  [`plot_simplicial()`](https://sonsoles.me/cograph/reference/plot_simplicial.md)
  and
  [`overlay_communities()`](https://sonsoles.me/cograph/reference/overlay_communities.md))
  now guards
  [`grDevices::chull()`](https://rdrr.io/r/grDevices/chull.html) against
  non-finite anchor coordinates. Previously a node lacking layout
  coordinates (NA/Inf) aborted the blob with “finite coordinates are
  needed”; such anchors are now dropped before the convex-hull step.

## cograph 2.3.3

### Documentation

- Aligned the
  [`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) /
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)
  roxygen documentation with the post-audit behavior shipped in 2.3.2
  (census `type_summary` counts, `min_count` handling, and corrected
  plot legend descriptions).

## cograph 2.3.2

### Bug fixes

- Full audit pass over the motifs subsystem: `type_summary` now holds
  real MAN-type counts in census mode, `min_count` is honored in census
  mode, and the swapped source/target color description in
  [`plot.cograph_motif_result()`](https://sonsoles.me/cograph/reference/motifs.md)
  is corrected.
- Unbroke [`motifs()`](https://sonsoles.me/cograph/reference/motifs.md)
  and
  [`plot_simplicial()`](https://sonsoles.me/cograph/reference/plot_simplicial.md)
  on Nestimate-backed workflows (HON / HYPA sequence inputs).
- [`panel_layout()`](https://sonsoles.me/cograph/reference/panel_layout.md):
  tightened dimension validation and made the restoration claim honest —
  it now restores only the
  [`par()`](https://rdrr.io/r/graphics/par.html) settings it actually
  changed.

## cograph 2.3.1

### Multi-panel layout control

- New `combined` argument (default `TRUE`) on every multi-panel plot
  function: [`splot()`](https://sonsoles.me/cograph/reference/splot.md)
  group-cascade,
  [`plot_netobject_group()`](https://sonsoles.me/cograph/reference/plot_netobject_group.md),
  [`plot_netobject_ml()`](https://sonsoles.me/cograph/reference/plot_netobject_ml.md),
  [`plot_net_bootstrap_group()`](https://sonsoles.me/cograph/reference/plot_net_bootstrap_group.md),
  [`plot_group_permutation()`](https://sonsoles.me/cograph/reference/plot_group_permutation.md),
  [`plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md),
  [`splot.net_mlvar()`](https://sonsoles.me/cograph/reference/splot.md),
  [`plot_network_evolution()`](https://sonsoles.me/cograph/reference/plot_network_evolution.md),
  [`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
  [`plot.cograph_motif_result()`](https://sonsoles.me/cograph/reference/motifs.md),
  [`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
  and
  [`plot.tna_disparity()`](https://sonsoles.me/cograph/reference/plot.tna_disparity.md).
  With `combined = FALSE` these functions draw panels into the active
  device without calling `graphics::par(mfrow=...)`, so callers can
  drive their own layout
  (e.g. [`graphics::layout()`](https://rdrr.io/r/graphics/layout.html)
  or the new
  [`panel_layout()`](https://sonsoles.me/cograph/reference/panel_layout.md)
  helper). Default `TRUE` preserves prior behavior — every existing call
  site renders identically.
- New
  [`panel_layout()`](https://sonsoles.me/cograph/reference/panel_layout.md)
  helper sets up a custom multi-panel device layout for use with
  `combined = FALSE`. Accepts either a uniform-grid `c(nrow, ncol)` or a
  [`graphics::layout()`](https://rdrr.io/r/graphics/layout.html) matrix
  for non-uniform layouts (e.g. one wide panel + two narrow ones).
  Returns a [`par()`](https://rdrr.io/r/graphics/par.html) snapshot for
  restoration via [`on.exit()`](https://rdrr.io/r/base/on.exit.html).

### Test suite hygiene

- `test-coverage-splot-{41,42}.R`: bumped `n_nodes` from 4 to 10 in
  seven per-edge attribute tests so the seed=42 sampler does not produce
  duplicate (1,2) pairs that trip cograph’s undirected-duplicate-edge
  detector.
- `test-coverage-class-network-41.R`: aligned the `set_layout_coords()`
  mismatched-row-count test with the strict input validation already
  enforced by `R/class-network.R`.
- `test-overlay-communities.R`: prefixed two
  [`communities()`](https://sonsoles.me/cograph/reference/communities.md)
  calls with `cograph::` to avoid `tna` masking when both packages are
  loaded in the suite (per CLAUDE.md “namespace masking” gotcha).

## cograph 2.3.0

### Documentation

- Audited every R/\*.R function file for roxygen/Rd accuracy. Corrected
  stale defaults (`cr_color` `#D4820A` -\> `#D4829A` in `plot-forest.R`;
  `show_value` default `FALSE` -\> `TRUE` in `splot-nodes.R`), corrected
  dataset dimensions in `data-hai.R` (`302` -\> `429 x 287`), corrected
  a reference to the nonexistent
  [`igraph::is_bipartite()`](https://r.igraph.org/reference/is_bipartite.html)
  (now `bipartite_mapping()`), expanded
  [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
  `@param` measure lists for `mode`, `cutoff`, `invert_weights`, and
  `membership` to match the implementation, dropped baked-in measure
  counts that rot on each addition, and removed nonexistent themes from
  `sn_theme` documentation. No runtime behavior changes from the
  documentation pass itself.

### Bug fixes

- [`plot_simplicial()`](https://sonsoles.me/cograph/reference/plot_simplicial.md)
  now warns when `anomaly` is set on an input that has no anomaly
  concept (HON, association rules, link prediction, character pathways,
  `method = "hon"` / `"rules"`). Previously the argument was silently
  dropped, so calls like `plot_simplicial(hon, anomaly = "over")` and
  `plot_simplicial(hon, anomaly = "under")` produced byte-identical
  plots. `anomaly` is honored only for `net_hypa` inputs and
  `method = "hypa"` auto-builds.

### Centrality

- [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
  gains an umbrella argument `tna_network` (logical or NULL). When
  `TRUE` (or auto-detected from a `tna`/`group_tna`/`ctna`/
  `ftna`/`atna` input), all measures shared with
  [`tna::centralities()`](http://sonsoles.me/tna/reference/centralities.md)
  match byte-for-byte: `loops = FALSE`, `invert_weights = TRUE`,
  `diffusion_method = "power_series"`, `transitivity_type = "onnela"`.
  Side-by-side audit confirms zero divergence on `OutStrength`,
  `InStrength`, `ClosenessIn/Out/All`, `Betweenness`, `Diffusion`,
  `Clustering` (`max|diff| = 0`). Any per-argument override the user
  passes explicitly always wins over the umbrella.
- [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
  (and
  [`centrality_diffusion()`](https://sonsoles.me/cograph/reference/centrality_diffusion.md))
  gain a `diffusion_method = c("kandhway_kuri", "power_series")`
  argument. The default `NULL` auto-detects: `"power_series"` for tna
  inputs (matches `tna::centralities(., measures = "Diffusion")`
  byte-for-byte when `loops = FALSE`), `"kandhway_kuri"` (the existing
  1-hop binary-degree formula, Kandhway & Kuri 2014) for everything
  else. Previously cograph’s diffusion silently disagreed with tna’s
  because cograph used an unweighted neighborhood-degree sum while tna
  uses `rowSums(P + P^2 + ... + P^n)` on the diagonal-zeroed weighted
  matrix — the same name covered two different statistics. Set
  explicitly to override the auto-detect.

### Tests

- Added a regression test in
  `tests/testthat/test-validate-nestimate-bootstrap-permutation.R`
  asserting that
  [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
  on a Nestimate `netobject` agrees with
  [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
  on its `$weights` matrix when the diagonal is non-zero. Locks in the
  upstream Nestimate fix to `.extract_edges_from_matrix()` (Nestimate
  \>= 2026-05-02) which now preserves self-loops in `$edges`. Without
  that fix, loop-bearing netobjects
  (e.g. `Nestimate::build_mcml() |> Nestimate::as_tna()`) silently
  under-counted node degree by 2.

### Plotting — edge-label cex coupling (Phase 2)

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

### Plotting — device-aware visual scaling

- [`splot()`](https://sonsoles.me/cograph/reference/splot.md) now
  applies device-dependent compensation to text, line, and point sizes
  so visual ratios (label-to-node, legend-to-plot, edge thickness) stay
  consistent when the output device changes. This fixes the
  long-standing “labels too big at high DPI” and “legend desynchronised
  from the plot” issues when saving PNGs at `res = 300` or `res = 600`
  with pixel-default `width`/`height`, and when resizing the RStudio
  plot pane. Implementation: a single `compute_visual_scale()` reads the
  active device’s canvas size (`dev.size("in")`) and returns multipliers
  keyed off a 5.9-inch reference (matching the default RStudio 7×5” pane
  so backward-compatible behaviour at the default canvas is preserved).
  Multipliers are clamped to `[0.55, 1.9]` to keep thumbnails and
  posters legible. See the new `R/visual-scale.R`.
- New `scaling = "fixed"` mode on
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md) — and
  corresponding global option `options(cograph.visual_scaling = FALSE)`
  — disables device compensation for reproducibility-sensitive workflows
  that calibrated against the previous behaviour.
- [`splot()`](https://sonsoles.me/cograph/reference/splot.md) return
  value now carries two attributes for downstream tooling:
  `cograph.visual_scale` (the multiplier list) and
  `cograph.node_diam_in` (the representative node diameter in inches at
  the rendered device).
- The splot-internal `render_legend_splot()` plus the new shared
  `.render_legend_base()` (`R/render-legend-shared.R`) replace the
  ad-hoc legend cex/pt.cex handling with a single compensated path.
  `plot_htna`, `plot_mtna`, `plot_mlna`, `plot_mcml` still use their
  historical scale multiplier arguments; Phase 2 will migrate them to
  the shared helper.

### Plotting

- `splot.netobject` now routes on the Nestimate `$method` slot rather
  than just direction. Undirected sequence-based networks from
  [`build_cna()`](https://saqr.me/Nestimate/reference/build_cna.html)
  and `wtna(method = "cooccurrence")` get oval TNA-family styling
  (layout, palette, donuts) with arrows and dotted edge starts
  automatically dropped because the matrix is symmetric. Glasso / cor /
  pcor / ising networks still get `psych_styling = TRUE` (spring layout,
  Okabe-Ito palette).
- [`from_tna()`](https://sonsoles.me/cograph/reference/from_tna.md)
  auto-detects integer-valued weight matrices (ftna, ctna, raw counts)
  and sets `weight_digits = edge_label_digits = 0` so edge labels render
  as `2304` rather than `2304.00`. Fractional weights still format to
  two decimals. Explicit user-supplied `weight_digits` still wins.
- `psych_styling = TRUE` is now exported as a first-class styling preset
  (undirected counterpart of `tna_styling`) — Okabe-Ito palette, spring
  layout, no arrows — applied by default to `splot.netobject` on
  correlation-family input and to the `$contemporaneous` / `$between`
  constituents of `net_mlvar`.
- Expanded [`splot()`](https://sonsoles.me/cograph/reference/splot.md)
  dispatch coverage across the tna and Nestimate class hierarchies,
  ensuring `tna`, `ftna`, `ctna`, `group_tna`, `tna_bootstrap`,
  `group_tna_bootstrap`, `tna_permutation`, `group_tna_permutation`,
  `netobject`, `netobject_group`, `netobject_ml`, `net_mlvar`,
  `wtna_mixed`, `net_bootstrap`, `net_permutation`, `boot_glasso`,
  `mcml`, `net_hon`, `net_hypa`, and `simplicial_complex` all reach the
  correct renderer.
- Self-loops are now preserved in every plot function.

### Correctness fixes (audit-driven)

- `detect_duplicate_edges()`, `aggregate_duplicate_edges()`,
  [`simplify.cograph_network()`](https://sonsoles.me/cograph/reference/simplify.md),
  and the internal `check_duplicate_edges()` helper now respect directed
  vs undirected semantics. Previously the canonical (min/max) endpoint
  key collapsed `A -> B` and `B -> A` into one edge even on directed
  graphs, matching
  [`igraph::simplify()`](https://r.igraph.org/reference/simplify.html)
  ground truth.
- `.compute_modularity()` replaces a nested for loop with cluster-wise
  vectorization
  (`sum(A[idx, idx]) - sum(k_out[idx]) * sum(k_in[idx]) / m`), per the
  project “no for loops” rule. Results verified bit-exact against
  [`igraph::modularity()`](https://r.igraph.org/reference/modularity.igraph.html).
- [`is_directed()`](https://sonsoles.me/cograph/reference/is_directed.md)
  now recognises `CographNetwork` R6 objects — previously only the
  `cograph_network` list format dispatched correctly.
- `compute_layout_for_cograph()` uses `layout$get_type()` instead of the
  removed `$name` field on `CographLayout`.
- [`network_small_world()`](https://sonsoles.me/cograph/reference/network_small_world.md)
  returns `0` (valid: no triangles means definitively not small-world)
  instead of `NA_real_` when the observed clustering coefficient is zero
  but path length is finite.
- [`simplify.cograph_network()`](https://sonsoles.me/cograph/reference/simplify.md)
  threads the directed flag through to edge aggregation so directed
  multigraphs collapse correctly.

### Performance & documentation

- [`simplify()`](https://sonsoles.me/cograph/reference/simplify.md)
  performance refactor for large networks plus a cleaner
  title-composition path.
- [`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
  [`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
  and `plot.cograph_motif_analysis` examples reworked to use
  `n_perm = 10L` (or `significance = FALSE`) and promoted from
  `\dontrun` to CRAN-runnable (optional tna branches stay in
  `\donttest`). Retires 320 seconds of latent CRAN timing risk — every
  example now runs in under 4 seconds.

### New tests

- `test-audit-fixes.R` — ground-truth regressions for the directed edge
  semantics, modularity vectorization, and small-world behaviour
  changes.
- `test-integer-weight-labels.R` — locks
  [`from_tna()`](https://sonsoles.me/cograph/reference/from_tna.md)
  integer-weight auto-detect behaviour and precedence of explicit
  `weight_digits`.
- `test-equiv-{assortativity, cluster-quality, communities, disparity, edge-centrality, network-summary, robustness, standalone-measures}.R`
  — numerical equivalence against igraph, sna, centiserve, brainGraph,
  influenceR, tidygraph, and NetworkX. Gated by
  `skip_coverage_tests() + skip_on_cran()`, so they do not run on the
  CRAN pipeline.

## cograph 2.1.0

### New Features

#### Batch 6 — new-API graph-level / set-level / pair-level measures

These measures don’t fit the per-node
[`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
data frame, so they live as standalone functions:

- [`estrada_index()`](https://sonsoles.me/cograph/reference/estrada_index.md)
  — graph-level spectral invariant: , equal to the trace of the matrix
  exponential of the adjacency. Equivalently, the sum of
  `subgraph_centrality()` across all nodes. Matches
  `networkx.estrada_index` at machine epsilon (max relative diff ~5e-15
  across random test graphs).
- [`trophic_incoherence()`](https://sonsoles.me/cograph/reference/trophic_incoherence.md)
  — graph-level food-web stability measure (Johnson et al. 2014).
  Defined as the population standard deviation of per-edge trophic
  differences where is the trophic level of node . Zero for perfectly
  coherent DAGs (e.g., a pure chain). Matches
  `networkx.trophic_incoherence_parameter` at machine epsilon.
  Directed-only; reuses the existing `trophic_level` calculator.
- `group_centrality(x, nodes, measure = c("betweenness", "closeness", "degree"))`
  — Everett-Borgatti (1999) group centrality for a *set* of nodes.
  Returns a scalar. Supports `mode = "in"/"out"` for directed-degree
  variants. **Group closeness and group degree** match
  `networkx.group_*_centrality` bit-exact. **Group betweenness**
  implements the textbook Everett-Borgatti / Puzis 2008 definition
  (fraction of shortest paths passing through at least one node in the
  group), which diverges from `networkx.group_betweenness_centrality` on
  some graphs due to a known quirk in NetworkX’s Puzis-Yahalom-Elovici
  iterative algorithm. Verified via an independent Python brute-force:
  cograph matches the textbook definition; NX produces larger values on
  graphs with many overlapping shortest paths. Documented in the roxygen
  “Divergence from NetworkX” section.
- `dispersion(x, u = NULL, v = NULL, normalized = TRUE, alpha = 1, b = 0, c = 0)`
  — Backstrom-Kleinberg (2014 Facebook) pair-level measure of tie
  strength. Counts the number of “well-dispersed” mutual friends of `u`
  and `v` (pairs of common neighbors that are not directly connected and
  share no common neighbor inside `u`’s ego network other than `u` and
  `v`). Matches `networkx.dispersion` bit-exact across all 156 edges on
  the karate club graph. Returns a scalar, named vector, or data frame
  depending on which of `u`, `v` are specified.

#### Centrality Batch 5 — Gould-Fernandez brokerage (5 roles)

Added the five Gould-Fernandez (1989) brokerage role counts, a
foundational measure in social network analysis (~1500 citations). Each
role is a separate per-node measure requiring a `membership` argument
(following the same pattern as `participation`, `within_module_z`,
`gateway`), and counts open directed 2-paths `a -> v -> c` through
broker `v`:

- [`centrality_brokerage_coordinator()`](https://sonsoles.me/cograph/reference/centrality_brokerage_coordinator.md)
  — all three in broker’s group (w_I)
- [`centrality_brokerage_itinerant()`](https://sonsoles.me/cograph/reference/centrality_brokerage_itinerant.md)
  — endpoints same group, broker different (w_O, “consultant”)
- [`centrality_brokerage_representative()`](https://sonsoles.me/cograph/reference/centrality_brokerage_representative.md)
  — broker + source same, target different (b_IO)
- [`centrality_brokerage_gatekeeper()`](https://sonsoles.me/cograph/reference/centrality_brokerage_gatekeeper.md)
  — broker + target same, source different (b_OI)
- [`centrality_brokerage_liaison()`](https://sonsoles.me/cograph/reference/centrality_brokerage_liaison.md)
  — all three in different groups (b_O)

Bit-exact match against `sna::brokerage$raw.nli` for all five roles
across 20 random directed graphs. Implemented natively (no runtime
dependency on sna). Key implementation detail: the Gould-Fernandez
counting rule requires **open 2-paths only** — triads where a direct
edge `a -> c` already exists are excluded. This matches sna’s C
implementation exactly and was derived empirically (sna’s
`.C("brokerage_R", ...)` has no R-level source).

Directed-only; warns and returns `NA` on undirected input.

#### Centrality Batch 4 — directed prestige family (Wasserman-Faust / sna)

- [`centrality_prestige_domain()`](https://sonsoles.me/cograph/reference/centrality_prestige_domain.md)
  — directed-graph prestige measure: for each node , the number of other
  nodes that can reach via a directed path. Classical
  Wasserman-Faust (1994) measure from `sna::prestige(cmode = "domain")`.
  Bit-exact match against sna, implemented natively via
  `igraph::distances(mode = "out")` + `colSums(is.finite(D)) - 1` (no
  runtime dependency on sna). Directed-only; returns NA with a warning
  on undirected input.
- [`centrality_prestige_domain_proximity()`](https://sonsoles.me/cograph/reference/centrality_prestige_domain_proximity.md)
  — distance-weighted variant: `R_v^2 / (D_v * (n - 1))` where `R_v` is
  the number of reachers and `D_v` is the sum of their geodesic
  distances to `v`. Bit-exact match against
  `sna::prestige(cmode = "domain.proximity")` on strongly connected
  directed graphs. On graphs with any unreachable pair, sna has a known
  bug (`FALSE * Inf = NaN` collapses the denominator, producing all-zero
  output); cograph’s
  [`is.finite()`](https://rdrr.io/r/base/is.finite.html)-masked formula
  produces mathematically correct values on any directed graph.
  Directed-only.

#### Centrality Batch 3 — classical measures with reference-package validation

- [`centrality_katz()`](https://sonsoles.me/cograph/reference/centrality_katz.md)
  — Katz (1953) status index. Bit-exact match against
  [`centiserve::katzcent`](https://rdrr.io/pkg/centiserve/man/katzcent.html)
  (cograph mirrors centiserve’s exact LAPACK call sequence). Also
  matches `igraph::alpha_centrality(exo = 1)` and
  `networkx.katz_centrality_numpy` at machine epsilon. New `katz_alpha`
  parameter (default 0.1).
- [`centrality_hubbell()`](https://sonsoles.me/cograph/reference/centrality_hubbell.md)
  — Hubbell (1965) input-output centrality. Bit-exact match against
  [`centiserve::hubbell`](https://rdrr.io/pkg/centiserve/man/hubbell.html)
  (cograph mirrors centiserve’s full-inverse LAPACK call path). Note:
  centiserve’s default (`weights = NULL`) silently ignores
  `E(g)$weight`; to reproduce cograph’s behavior with centiserve on
  weighted graphs, pass `weights = igraph::E(g)$weight` explicitly. New
  `hubbell_weight` parameter (default 0.5).
- [`centrality_information()`](https://sonsoles.me/cograph/reference/centrality_information.md)
  — Stephenson-Zelen (1989) information centrality. Bit-exact match
  against [`sna::infocent`](https://rdrr.io/pkg/sna/man/infocent.html)
  on connected undirected graphs (cograph mirrors sna’s exact
  construction and [`solve()`](https://rdrr.io/r/base/solve.html) call
  sequence).
- [`centrality_pairwisedis()`](https://sonsoles.me/cograph/reference/centrality_pairwisedis.md)
  — Pairwise disconnectivity (Potapov et al. 2008). Directed-only;
  fraction of reachable ordered pairs that become unreachable when each
  node is removed. Bit-exact match against
  [`centiserve::pairwisedis`](https://rdrr.io/pkg/centiserve/man/pairwisedis.html).
  Warns and returns `NA` on undirected input, matching the convention
  used by `salsa`, `leaderrank`, and `trophic_level`.
- [`centrality_reaching_local()`](https://sonsoles.me/cograph/reference/centrality_reaching_local.md)
  /
  [`reaching_global()`](https://sonsoles.me/cograph/reference/reaching_global.md)
  — Local and global reaching centrality (Mones, Vicsek & Vicsek 2012).
  Bit-exact match against `networkx.local_reaching_centrality` across
  the directed unweighted, undirected unweighted, and weighted branches.
  Undirected unweighted LRC coincides with
  `igraph::harmonic_centrality(normalized = TRUE)` (documented).
  [`reaching_global()`](https://sonsoles.me/cograph/reference/reaching_global.md)
  is a graph-level hierarchy statistic in \[0, 1\].

## cograph 1.8.2

### New Features

- [`plot_simplicial()`](https://sonsoles.me/cograph/reference/plot_simplicial.md)
  now accepts `tna`, `netobject`, `net_hon`, and `net_hypa` objects
  directly — higher-order pathways are auto-built and visualized with
  proper state labels, no manual extraction needed. New parameters:
  `method` (`"hon"` / `"hypa"`), `max_pathways`, `ncol`. Dismantled mode
  uses `gridExtra` grid layout with scaled nodes
- [`print.cograph_network()`](https://sonsoles.me/cograph/reference/print.cograph_network.md)
  now shows a structured summary: node/edge counts, density,
  reciprocity, weight range, and top-degree nodes — replacing the
  minimal R6 default output
- Added `mcml` S3 class with
  [`as_mcml()`](https://sonsoles.me/cograph/reference/as_mcml.md)
  generic for type-safe handling of Markov Chain Multi-Level models —
  enables [`print()`](https://rdrr.io/r/base/print.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html), and method
  dispatch on MCML objects
- Added local `%||%` operator for R 4.1 compatibility (no longer
  requires R 4.4+)

### Breaking Changes

- MCML field names renamed for clarity: `$between` → `$macro`, `$within`
  → `$clusters`
- [`as_tna()`](https://sonsoles.me/cograph/reference/as_tna.md) on MCML
  objects now returns a flat `group_tna` list instead of a nested
  structure

### Bug Fixes

- [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  now suppresses zero-weight edges instead of drawing invisible lines,
  and strips leading zeros from edge labels (`.32` instead of `0.32`)
- Self-loops in
  [`cluster_summary()`](https://saqr.me/Nestimate/reference/cluster_summary.html)
  are now preserved in the macro diagonal, reflecting intra-cluster
  retention rates
- Sequence data is properly propagated through the full tna → macro →
  cluster pipeline, so downstream models can use bootstrap and
  permutation tests

## cograph 1.8.0

### New Features

- Added
  [`overlay_communities()`](https://sonsoles.me/cograph/reference/overlay_communities.md)
  for drawing community blob overlays on any network plot — accepts
  method names, membership vectors, or pre-computed community objects
- Added
  [`plot_simplicial()`](https://sonsoles.me/cograph/reference/plot_simplicial.md)
  for higher-order pathway visualization, rendering simplicial complexes
  as smooth blobs with flexible separators and a dismantled view option
- Added `value_nudge` parameter to
  [`plot_transitions()`](https://sonsoles.me/cograph/reference/plot_transitions.md)
  for controlling the distance between flow labels and nodes
- Added bundle legend label controls: `bundle_legend_size`,
  `bundle_legend_color`, `bundle_legend_fontface`,
  `bundle_legend_position`
- Added per-function label controls (`label_size`, `label_color`,
  `label_fontface`, `label_hjust`) to
  [`plot_transitions()`](https://sonsoles.me/cograph/reference/plot_transitions.md),
  [`plot_trajectories()`](https://sonsoles.me/cograph/reference/plot_trajectories.md),
  and
  [`plot_alluvial()`](https://sonsoles.me/cograph/reference/plot_alluvial.md)

### Bug Fixes

- Fixed spiky text halo artifacts in transition and heatmap plots by
  increasing circular offset directions from 8 to 16 (22.5° spacing for
  smooth outlines)

## cograph 1.7.0

### New Features

#### Cluster Analysis

- Added
  [`cluster_summary()`](https://saqr.me/Nestimate/reference/cluster_summary.html)
  for aggregating network weights at the cluster level, producing
  between-cluster and within-cluster matrices from raw transition data
- Added
  [`build_mcml()`](https://saqr.me/Nestimate/reference/build_mcml.html)
  for constructing Markov Chain Multi-Level models from edge lists or
  sequence data with automatic cluster detection
- Added
  [`cluster_quality()`](https://sonsoles.me/cograph/reference/cluster_quality.md)
  for modularity-based cluster quality metrics and
  [`cluster_significance()`](https://sonsoles.me/cograph/reference/cluster_significance.md)
  for permutation-based significance testing
- Added [`as_tna()`](https://sonsoles.me/cograph/reference/as_tna.md) to
  convert cluster summaries to TNA objects for bootstrapping,
  permutation testing, and plotting with
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md)

#### Network Operations

- Added
  [`simplify()`](https://sonsoles.me/cograph/reference/simplify.md) for
  pruning weak edges from networks, with configurable weight threshold
  and aggregation method
- Added
  [`disparity_filter()`](https://sonsoles.me/cograph/reference/disparity_filter.md)
  for backbone extraction (Serrano et al. 2009), with methods for
  matrices, tna, igraph, and cograph_network objects
- Added
  [`robustness()`](https://sonsoles.me/cograph/reference/robustness.md)
  for network robustness analysis with targeted (betweenness, degree)
  and random attack strategies, plus
  [`ggplot_robustness()`](https://sonsoles.me/cograph/reference/ggplot_robustness.md)
  for faceted ggplot2 output
- Added `temporal_edge_list()` for converting sequence data to
  timestamped edge lists
- Added
  [`supra_adjacency()`](https://sonsoles.me/cograph/reference/supra_adjacency.md),
  [`supra_layer()`](https://sonsoles.me/cograph/reference/supra_layer.md),
  [`supra_interlayer()`](https://sonsoles.me/cograph/reference/supra_interlayer.md)
  for multilayer supra-adjacency matrix construction
- Added
  [`layer_similarity()`](https://sonsoles.me/cograph/reference/layer_similarity.md),
  [`layer_similarity_matrix()`](https://sonsoles.me/cograph/reference/layer_similarity_matrix.md),
  and
  [`layer_degree_correlation()`](https://sonsoles.me/cograph/reference/layer_degree_correlation.md)
  for comparing layers in multilayer networks
- Added
  [`aggregate_weights()`](https://sonsoles.me/cograph/reference/aggregate_weights.md)
  and
  [`aggregate_layers()`](https://sonsoles.me/cograph/reference/aggregate_layers.md)
  for weight aggregation across layers
- Added
  [`verify_with_igraph()`](https://sonsoles.me/cograph/reference/verify_with_igraph.md)
  for cross-validating cograph centrality and network metrics against
  igraph

#### Motif Analysis

- Added [`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) /
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md) as
  a unified API for triad census (node-exchangeable counts) and instance
  extraction (named node triples), with auto-detection of actor/session
  columns, rolling/tumbling window support, and exact configuration
  model significance testing

#### Visualization

- Added
  [`plot_mcml()`](https://sonsoles.me/cograph/reference/plot_mcml.md)
  for Markov Chain Multi-Level visualization showing between-cluster
  summary edges alongside within-cluster detail, with pie charts,
  self-loops, and 22 customization parameters
- Added
  [`plot_chord()`](https://sonsoles.me/cograph/reference/plot_chord.md)
  for native chord diagrams with automatic weight-based arc sizing
- Added `plot_time_line()` for cluster membership timeline visualization
- Added
  [`plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.md)
  orientations: `"facing"` (tip-to-tip columns) and `"circular"` (two
  semicircles), plus `intra_curvature` for drawing intra-group edges as
  dotted bezier arcs
- Added `threshold` parameter to all plot functions for filtering
  edges/cells below a minimum absolute weight
- Added `value_fontface`, `value_fontfamily`, and `value_halo`
  parameters to
  [`plot_heatmap()`](https://sonsoles.me/cograph/reference/plot_heatmap.md)
  for text styling control
- Added directional shorthands for `scale_nodes_by`: `indegree`,
  `outdegree`, `instrength`, `outstrength`, `incloseness`,
  `outcloseness`, `inharmonic`, `outharmonic`, `ineccentricity`,
  `outeccentricity`
- Added `scale_nodes_scale` parameter to
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md) for
  dampening (\< 1) or exaggerating (\> 1) centrality-based node sizing
  differences
- Added qgraph argument translation in
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md): when
  plotting tna objects, qgraph-style parameters (`vsize`, `asize`,
  `edge.color`, `lty`, `shape`) are automatically mapped to cograph
  equivalents

#### Transition Plot Enhancements

- Added intermediate labels with `node_label_format` (e.g.,
  `"{state} (n={count})"`) for showing counts on transition plot nodes
- Added line bundling via `bundle_size` for aggregating individual
  trajectories into weighted summary lines in large datasets
- Added flow value labels via `show_values` / `value_position` for
  displaying transition counts on flow lines
- Added `label_position` consistency across ALL columns (first, middle,
  last) in trajectory plots

#### Data & Infrastructure

- Added example datasets: `gamer_data`, `group_engagement`, `srl_data`
- Added `set_node_groups()` / `get_node_groups()` for managing cluster
  assignments on cograph_network objects
- Consolidated cograph_network metadata under `$meta` with getter/setter
  functions
- Added `group_tna` support to
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md) for direct
  plotting of grouped TNA models
- Gave each `centrality_*` wrapper its own focused help page

### Bug Fixes

- Fixed load and percolation centrality computation: the BFS assumed
  unit edge weights, causing infinite loops on weighted graphs; directed
  graphs now transpose correctly (matching sna convention); disconnected
  nodes no longer contribute spurious centrality
- Fixed self-loop and edge clipping in
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md) viewport
  calculation
- Fixed argument forwarding in splot dispatch for bootstrap/permutation
  objects — named parameters (minimum, threshold, layout, title) were
  consumed by
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md)’s
  signature and silently dropped when dispatching
- Fixed overlapping flow value labels in multi-step alluvial plots
- Fixed alluvial label halo rendering producing spike artifacts (8 → 16
  offset directions)
- Fixed viridis palette direction in
  [`plot_heatmap()`](https://sonsoles.me/cograph/reference/plot_heatmap.md)
  so high values get dark colors
- Fixed
  [`build_mcml()`](https://saqr.me/Nestimate/reference/build_mcml.html)
  density method crash when weight vector had no names
- Fixed display label priority resolution (labels \> label \>
  identifier)
- Removed zero-value labels that appeared after rounding in transition
  plots

### Improvements

- Simplified splot dispatch: extracted `.collect_dispatch_args()` helper
  to replace 6 copy-paste dispatch blocks, using
  [`match.call()`](https://rdrr.io/r/base/match.call.html) +
  [`mget()`](https://rdrr.io/r/base/get.html) for reliable argument
  capture

## cograph 1.6.0

### New Features

#### Centrality

- Added
  [`centrality()`](https://sonsoles.me/cograph/reference/centrality.md)
  with 23 measures and individual wrappers: degree, strength,
  betweenness, closeness, eigenvector, pagerank, harmonic, authority,
  hub, alpha, power, kreach, diffusion, percolation, eccentricity,
  transitivity, constraint, coreness, load, subgraph, leverage,
  laplacian, current-flow betweenness, current-flow closeness, voterank
- Added
  [`edge_betweenness()`](https://sonsoles.me/cograph/reference/edge_centrality.md)
  for edge-level centrality
- Added automatic weight inversion for path-based measures when working
  with tna transition matrices (where higher weight = stronger
  connection, not shorter distance)

#### Community Detection

- Added
  [`detect_communities()`](https://sonsoles.me/cograph/reference/detect_communities.md)
  with 11 algorithms: louvain, walktrap, fast_greedy, label_propagation,
  leading_eigenvector, infomap, spinglass, leiden, optimal,
  edge_betweenness, multilevel — plus `com_*` shorthand aliases
- Added consensus clustering and
  [`cluster_significance()`](https://sonsoles.me/cograph/reference/cluster_significance.md)
  for permutation-based validation

#### Network Metrics

- Added
  [`network_summary()`](https://sonsoles.me/cograph/reference/network_summary.md)
  and
  [`summarize_network()`](https://sonsoles.me/cograph/reference/summarize_network.md)
  for computing comprehensive network-level statistics (density,
  reciprocity, transitivity, diameter, components, degree distribution)

#### Visualization

- Added
  [`plot_transitions()`](https://sonsoles.me/cograph/reference/plot_transitions.md)
  for alluvial/Sankey flow diagrams, with
  [`plot_alluvial()`](https://sonsoles.me/cograph/reference/plot_alluvial.md)
  and
  [`plot_trajectories()`](https://sonsoles.me/cograph/reference/plot_trajectories.md)
  wrappers
- Added `plot_bootstrap()` and
  [`plot_permutation()`](https://sonsoles.me/cograph/reference/plot_permutation.md)
  for significance-styled visualization of bootstrap and permutation
  test results — significant edges rendered solid on top,
  non-significant edges dashed behind
- Added
  [`plot_mixed_network()`](https://sonsoles.me/cograph/reference/plot_mixed_network.md)
  for overlaying symmetric (undirected, straight) and asymmetric
  (directed, curved) edges on the same network
- Added
  [`plot_heatmap()`](https://sonsoles.me/cograph/reference/plot_heatmap.md)
  for adjacency matrix heatmaps with optional hierarchical clustering
  and
  [`plot_ml_heatmap()`](https://sonsoles.me/cograph/reference/plot_ml_heatmap.md)
  for multilayer 3D perspective heatmaps
- Added
  [`plot_compare()`](https://sonsoles.me/cograph/reference/plot_compare.md)
  for difference network visualization showing edge-weight changes
  between two networks
- Added [`splot()`](https://sonsoles.me/cograph/reference/splot.md) S3
  methods for `tna_bootstrap` and `tna_permutation` objects

#### Motif Analysis

- Added
  [`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
  [`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md),
  and
  [`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)
  for triad motif analysis with pattern filtering, significance testing,
  and network diagram visualization

#### Network Utilities

- Added
  [`filter_edges()`](https://sonsoles.me/cograph/reference/filter_edges.md),
  [`subset_edges()`](https://sonsoles.me/cograph/reference/filter_edges.md),
  [`select_nodes()`](https://sonsoles.me/cograph/reference/select_nodes.md),
  [`select_edges()`](https://sonsoles.me/cograph/reference/select_edges.md)
  for flexible network subsetting
- Added
  [`set_groups()`](https://sonsoles.me/cograph/reference/set_groups.md)
  for storing cluster assignments on cograph_network objects with
  automatic dispatch to
  [`plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.md) /
  [`plot_mtna()`](https://sonsoles.me/cograph/reference/plot_mtna.md)

#### Infrastructure

- All plot functions now accept `cograph_network` objects as input, in
  addition to matrices, igraph objects, and tna objects
- Layout computation is now lazy — coordinates are only calculated when
  first needed
- Improved `layout_spring` and `layout_gephi_fr` algorithms: vectorized
  attraction forces, edge aggregation for dense networks
- Renamed package from Sonnet to cograph

### Bug Fixes

- Fixed `par(pin)` error on exit when plot device state was corrupted
- Fixed motif plot scaling and margins for different device sizes

## cograph 1.5.2

CRAN release: 2026-03-02

### Breaking Changes

- Standardized first parameter name to `x` across all plotting
  functions:
  - [`plot_tna()`](https://sonsoles.me/cograph/reference/plot_tna.md):
    `input` → `x`
  - [`plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.md):
    `input` → `x` (was `model`)
  - [`plot_mtna()`](https://sonsoles.me/cograph/reference/plot_mtna.md):
    `input` → `x` (was `model`)
  - [`splot()`](https://sonsoles.me/cograph/reference/splot.md) already
    used `x`

### Bug Fixes

- Fixed [`tplot()`](https://sonsoles.me/cograph/reference/plot_tna.md)
  default margins causing tiny plots compared to
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md)

### Documentation

- Added qgraph to splot migration guide (`vignettes/qgraph-to-splot.md`)

## cograph 1.5.1

### Breaking Changes (with backwards compatibility)

The following parameters have been renamed for consistency. The old
names still work but emit deprecation warnings:

| Old Name | New Name | Reason |
|----|----|----|
| `esize` | `edge_size` | Add `edge_` prefix, expand abbreviation |
| `cut` | `edge_cutoff` | Add `edge_` prefix, clarify meaning |
| `usePCH` | `use_pch` | Fix camelCase to snake_case |
| `positive_color` | `edge_positive_color` | Add `edge_` prefix (matches theme storage) |
| `negative_color` | `edge_negative_color` | Add `edge_` prefix (matches theme storage) |
| `donut_border_lty` | `donut_line_type` | Expand `lty` abbreviation |

### Improvements

- `edge_label_fontface` now accepts string values (“plain”, “bold”,
  “italic”, “bold.italic”) in addition to numeric values

## cograph 1.4.0

### New Features

- Added [`mlna()`](https://sonsoles.me/cograph/reference/plot_mlna.md)
  for multilevel network visualization with 3D perspective
- Added [`mtna()`](https://sonsoles.me/cograph/reference/plot_mtna.md)
  for multi-cluster network visualization with shape-based cluster
  containers
- Added
  [`plot_htna()`](https://sonsoles.me/cograph/reference/plot_htna.md)
  for hierarchical multi-group network layouts with polygon and circular
  arrangements
- Added [`tplot()`](https://sonsoles.me/cograph/reference/plot_tna.md)
  as a qgraph drop-in replacement with automatic parameter translation
- Added `arrow_angle` parameter for customizable arrowhead geometry

### Bug Fixes

- Fixed Rd cross-reference warning in splot documentation
- Fixed pie/donut segment divider lines rendering when border width is 0

## cograph 1.3.1

### New Features

- Added `edge_start_dot_density` parameter for TNA-style dotted edge
  starts indicating direction
- Added direct support for tna objects via
  [`from_tna()`](https://sonsoles.me/cograph/reference/from_tna.md) — no
  manual matrix extraction needed
- Added direct support for statnet `network` and `qgraph` objects as
  input
- Added auto-conversion of `pie_values` vector to `donut_fill` when all
  values are in \[0,1\]

### Bug Fixes

- Fixed TNA visual defaults being silently overwritten in
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md) when other
  parameters were specified
- Fixed self-loop edge labels overlapping the loop arc
- Fixed `donut_shape` validation rejecting custom SVG shapes
- Fixed title clipping when title text exceeded plot margins
- Fixed edge rendering crash on certain edge/node configurations
- Removed underscore prefix requirement for custom SVG shape names

## cograph 1.2.7

### Bug Fixes

- Fixed oversized nodes in
  [`from_qgraph()`](https://sonsoles.me/cograph/reference/from_qgraph.md)
  when a layout override was provided
- Fixed oval layout using independent axis scaling, which distorted
  aspect ratios — now uses uniform scaling via `normalize_coords()`
- Fixed edge label alignment in
  [`from_qgraph()`](https://sonsoles.me/cograph/reference/from_qgraph.md)
  by using a matrix intermediary for per-edge vector reordering
- Fixed `nrow(el)` crash: qgraph’s Edgelist is a list, not a data.frame
- Fixed oval layout node distortion and donut fill values when
  converting from qgraph

## cograph 1.2.6

### New Features

- Added `donut_empty` parameter for rendering unfilled donut nodes
- Added
  [`from_qgraph()`](https://sonsoles.me/cograph/reference/from_qgraph.md)
  for converting qgraph objects to cograph format, reading resolved
  `graphAttributes` for accurate parameter extraction

### Bug Fixes

- Fixed oval `layout_info` guard causing errors on certain device
  configurations
- Fixed curvature extraction passing vector values instead of scalars

## cograph 1.2.0

### New Features

- Added [`soplot()`](https://sonsoles.me/cograph/reference/soplot.md)
  for grid/ggplot2-based network plotting — full feature parity with
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md) using a
  different rendering backend
- Added
  [`layout_oval()`](https://sonsoles.me/cograph/reference/layout_oval.md)
  for oval/elliptical node arrangements
- Added `layout_scale` parameter to expand or contract the network
  layout, with `"auto"` mode for node-count-based scaling
- Added Gephi-style Fruchterman-Reingold layout algorithm
- Added `edge_start_style` parameter for visually indicating edge
  direction via styled start segments (dashed, dotted)

### Bug Fixes

- Fixed [`soplot()`](https://sonsoles.me/cograph/reference/soplot.md)
  curve direction and edge defaults diverging from
  [`splot()`](https://sonsoles.me/cograph/reference/splot.md) behavior
- Fixed `rescale_layout` distorting oval aspect ratios by switching to
  uniform scaling
- Fixed edge scaling producing abnormally thick edges on small networks
- Fixed `par(pin)` restoration error on plot device exit

## cograph 1.1.0

### New Features

- Added [`splot()`](https://sonsoles.me/cograph/reference/splot.md) — a
  base R graphics engine for network visualization using
  [`polygon()`](https://rdrr.io/r/graphics/polygon.html),
  [`lines()`](https://rdrr.io/r/graphics/lines.html), and
  [`xspline()`](https://rdrr.io/r/graphics/xspline.html), providing
  better performance than grid-based rendering for large networks
- Added polygon-shaped donut nodes, custom SVG node shapes, and
  AI-generated shape support
- Added shadow/halo labels and fine-grained text control (fontface,
  fontfamily, hjust, vjust, angle)
- Added double donut nodes with separate inner/outer border controls
- Added edge CI (confidence interval) underlays and template-based edge
  labels
- Added comprehensive legend support: groups, edge color scales, and
  node size scales
- Added high-resolution output via
  [`sn_save()`](https://sonsoles.me/cograph/reference/sn_save.md) with
  configurable DPI
- Added edge curve modes, bidirectional arrows, self-loop rotation, and
  per-edge curve control

### Bug Fixes

- Fixed donut rendering producing artifacts and simplified the
  `donut_color` API to accept 1 color (fill), 2 colors (fill +
  background), or n colors (per-node)
- Fixed arrow positioning and curve direction for qgraph-style edges
- Fixed edge label positioning to avoid overlap with edge lines
- Fixed self-loop rendering to use qgraph-style circular arcs
- Fixed arrow placement on non-square viewports
- Fixed reciprocal edge auto-separation causing edge crossings

## cograph 1.0.0

- Initial release of cograph network visualization package
