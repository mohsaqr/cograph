# Network Motif Analysis

Two modes of directed MAN triad analysis for networks:

- **Census** (`named_nodes = FALSE`, default): Counts MAN type
  frequencies with significance testing. Nodes are exchangeable.

- **Instances** (`named_nodes = TRUE`, or use
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)):
  Lists specific node triples forming each pattern. Nodes are NOT
  exchangeable.

## Usage

``` r
motifs(
  x,
  named_nodes = FALSE,
  actor = NULL,
  window = NULL,
  window_type = c("rolling", "tumbling"),
  pattern = c("triangle", "network", "closed", "all"),
  include = NULL,
  exclude = NULL,
  significance = TRUE,
  n_perm = 1000L,
  cores = 1L,
  min_count = if (named_nodes) 5L else NULL,
  edge_method = c("any", "expected", "percent"),
  edge_threshold = 1.5,
  min_transitions = 5,
  top = NULL,
  seed = NULL
)

# S3 method for class 'cograph_motif_result'
print(x, ...)

# S3 method for class 'cograph_motif_result'
plot(
  x,
  type = c("triads", "types", "significance", "patterns"),
  n = 15,
  ncol = 5,
  colors = c("#2166AC", "#B2182B"),
  node_size = 5,
  label_size = 11,
  title_size = 12,
  stats_size = 13,
  legend_size = 13,
  legend = TRUE,
  motif_color = "#800020",
  spacing = 1,
  base_size = 12,
  combined = TRUE,
  ...
)
```

## Arguments

- x:

  Input data: a tna object, cograph_network, matrix, igraph, or
  data.frame (edge list).

- named_nodes:

  Logical. If FALSE (default), performs census (type-level counts). If
  TRUE, extracts specific node triples (instance-level).
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md) is
  a convenience wrapper that sets this to TRUE.

- actor:

  Character. Column name in the edge list metadata to group by. If NULL
  (default), auto-detects standard column names (session_id, session,
  actor, user, participant). If no grouping column found, performs
  aggregate analysis.

- window:

  Numeric. Window size for windowed analysis. Splits each actor's
  transitions into windows of this size. NULL (default) means no
  windowing.

- window_type:

  Character. Window type: "rolling" (default) or "tumbling". Only used
  when `window` is set.

- pattern:

  Which MAN triad types to include in the analysis:

  `"triangle"`

  :   (default) Only the 7 closed triangle types: 030C, 030T, 120C,
      120D, 120U, 210, 300. Excludes trivial open patterns (empty
      triads, single edges, chains, stars, mutual pairs).

  `"network"`

  :   All types except trivially open ones. Excludes 003 (empty), 012
      (single edge), 021C (chain).

  `"closed"`

  :   Like `"network"` but also excludes 120C (mixed regulated).
      Excludes 003, 012, 021C, 120C.

  `"all"`

  :   All 16 MAN types, including empty and trivial patterns.

- include:

  Character vector of MAN types to include exclusively. Overrides
  `pattern` and `exclude`.

- exclude:

  Character vector of MAN types to exclude. Applied after `pattern`
  filter.

- significance:

  Logical. Run permutation significance test? Default TRUE.

- n_perm:

  Number of permutations for significance. When `significance = TRUE`,
  must be a whole number of at least 2. Default 1000.

- cores:

  Number of worker processes for the permutation null. Default `1` runs
  serially and is the only setting that reproduces results from earlier
  versions: it consumes a single RNG stream in replicate-then-unit
  order, so a given `seed` gives the historical numbers. `cores > 1`
  gives each replicate its own L'Ecuyer-CMRG stream, which makes a
  result depend on `seed` alone and not on the worker count or on how
  replicates were chunked – but those are a *different* set of draws, so
  the p-values will not match a `cores = 1` run of the same seed. They
  remain a valid permutation null, and repeated parallel runs of one
  seed agree exactly with each other at any `cores`. Forking is used
  where available; Windows uses a PSOCK cluster. Only the
  individual-level census null is parallelized. Values above
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  are capped with a `cograph_cores_capped` warning.

- min_count:

  Inclusive minimum count to keep a row — rows with `count >= min_count`
  are retained. In instance mode (`named_nodes = TRUE`) this filters the
  `observed` column: at individual level the number of subjects
  exhibiting the triad, at aggregate level the triad's weighted edge
  mass (sum of its 6 directed edge weights). In census mode
  (`named_nodes = FALSE`) this filters the `count` column — the number
  of times each MAN type appears. Default 5 for instances, NULL for
  census (no filter).

- edge_method:

  Method for determining edge presence: `"any"` (default; any positive
  edge), `"expected"` (observed/expected ratio), or `"percent"` (edge
  weight divided by the six-edge triad total).

- edge_threshold:

  Threshold for `"expected"` or `"percent"` methods. For `"expected"`,
  1.5 means 50 percent above expected. For `"percent"`, values at or
  below 1 are proportions and values above 1 are percentages. Default
  1.5.

- min_transitions:

  Minimum total transitions for a unit to be included. Default 5.

- top:

  Return only the top N results. NULL returns all.

- seed:

  Random seed for reproducibility.

- ...:

  Additional arguments passed to internal plot helpers.

- type:

  Plot type:

  `"triads"`

  :   Network diagrams of specific node triples (instance mode) or falls
      back to patterns (census mode). Instance panels use a canonical
      representative of the MAN class: concrete labels identify
      participants, not their observed node-role orientation. Each panel
      title reads `"<MAN code>: <description>"` (e.g.
      `"030T: Feed-forward"`) and, in census mode, appends the z-score
      and a significance star (`*` p\<.05, `**` p\<.01, `***` p\<.001).
      Arranged in a grid.

  `"types"`

  :   Bar chart of MAN type frequencies. In census mode bars are colored
      by significance direction (see `colors`); in instance mode bars
      use a single fill because per-type significance would need an
      aggregation rule across multiple node-triple rows of the same
      type.

  `"significance"`

  :   Z-score bars per row of `x$results`. In census mode each bar is
      one MAN type; in instance mode each bar is one concrete
      node-triple, labeled `"<triple> [<MAN code>: <description>]"`.
      Bars are colored with the same three-tone rule (see `colors`).
      Requires `significance = TRUE` in the `motifs()` call.

  `"patterns"`

  :   Abstract MAN pattern diagrams showing the edge structure of each
      triad type. In census mode panel nodes are filled by significance
      direction (red sig over / blue sig under / grey ns); in instance
      mode panels use a single fill, same reason as `"types"`.

- n:

  Maximum number of items to plot. Default 15.

- ncol:

  Number of columns in the triad/pattern grid. Default 5.

- colors:

  Two-element color vector mapped to a three-tone significance scale
  (used by `type = "significance"`, plus `type = "types"` and
  `type = "patterns"` in census mode): `colors[1]` fills items that are
  significantly under-represented (`p < .05` and `z < 0`); `colors[2]`
  fills items that are significantly over-represented (`p < .05` and
  `z > 0`); everything else is filled neutral grey (`"#9E9E9E"`).
  Default `c("#2166AC", "#B2182B")` (blue for under, red for over). When
  significance was not run, `type = "types"` falls back to a single
  `colors[1]` fill and patterns nodes use `colors[1]`.

- node_size:

  Triad node radius (relative). Default 5. (`type = "triads"` only.)

- label_size:

  Triad node-label font size in points. Default 11.

- title_size:

  Per-panel title font size in points. Default 12.

- stats_size:

  Per-panel statistics caption font size in points (e.g.,
  `n=34 z=-55.3 p<.001`). Default 13.

- legend_size:

  Bottom legend font size in points. Default 13.

- legend:

  Logical. Show the abbreviation legend strip below the triad grid.
  Default `TRUE`. (`type = "triads"` only.)

- motif_color:

  Color of triad nodes/edges/labels. Default `"#800020"` (deep
  burgundy). (`type = "triads"` only.)

- spacing:

  Triangle spread inside each panel; `> 1` pulls nodes inward, `< 1`
  pushes them apart. Default 1.

- base_size:

  Base font size for the `ggplot2` themes used by `type = "types"` and
  `type = "significance"`. Default 12.

- combined:

  Logical: when TRUE (default) and `type = "patterns"` (or
  `type = "triads"` on unnamed-node input that falls back to pattern
  plotting), arrange the per-motif panels in an internal grid via
  `graphics::par(mfrow=...)`. Set to FALSE to draw into a layout the
  caller has already configured (e.g. via
  [`panel_layout()`](https://sonsoles.me/cograph/reference/panel_layout.md)).

## Value

A `cograph_motif_result` object (a list) with:

- results:

  Data frame of results. Census mode (`named_nodes = FALSE`): one row
  per retained, observed MAN type with columns `type`, `count`, and when
  `significance = TRUE` also `expected`, `z`, `p`, `sig`. Instance mode
  (`named_nodes = TRUE`): one row per concrete node-triple and MAN type
  with columns `triad`, `node1`, `node2`, `node3`, `type`, `observed`,
  and when `significance = TRUE` also `expected`, `z`, `p`, `sig`. At
  individual level, `observed` is the number of sessions/units in which
  that triple has that MAN type; one triple may therefore occupy
  multiple rows when its type differs across units.

- type_summary:

  Named `table` of MAN-type counts. In census mode the values come from
  the `count` column; in instance mode they come from
  `table(results$type)` and describe how many concrete node-triples fall
  under each MAN type. Sorted descending so `plot(., type = "patterns")`
  draws the most frequent types first.

- level:

  Analysis level: `"individual"` when the input carried per-subject
  sequence data (`tna` with `$data`, edge list with an actor column,
  Nestimate `netobject` built from `build_tna()`/similar), otherwise
  `"aggregate"` (a single transition matrix).

- named_nodes:

  Logical mirror of the `named_nodes` argument. Plot helpers gate
  per-type significance decoration on this so the instance-mode case
  (multiple triples per MAN type) doesn't get silently aggregated.

- n_units:

  Number of subjects/units. 1 at aggregate level, `nrow` of the input
  sequence data at individual level.

- params:

  List of the call's parameters (`pattern`, `edge_method`,
  `edge_threshold`, `significance`, `n_perm`, `min_count`, `labels`,
  `n_states`, and the window settings if any). Read by
  [`print()`](https://rdrr.io/r/base/print.html) and the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) dispatcher.

Invisibly returns the input `x` for `"triads"` and `"patterns"`, or the
underlying `ggplot` for `"types"` and `"significance"`.

## Details

Detects input type and analysis level automatically. For inputs with
individual/group data (tna objects, cograph networks from edge lists
with metadata), performs per-group analysis. For aggregate inputs
(matrices, igraph), analyzes the single network. The unified `motifs()`
and [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)
APIs classify the supplied adjacency as directed dyads in the 16-class
MAN system. For the separate four-class undirected census, use
`motif_census(..., directed = FALSE)`.

For aggregate inputs, significance delegates to
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
and its loop-free simple-graph rewiring null. Individual weighted inputs
use a directed stub-matching null: positive edge weights are converted
to at least one integer stub, target stubs are shuffled while preserving
each unit's integerized in/out margins, and the resulting multigraph
(which may contain loops or parallel edges) is evaluated through its
simple loopless triad projection. Observed self-loops are excluded
before both counting and null construction.

With `edge_method = "percent"`, edge presence is computed within each
node triple: an edge's weight is divided by the sum of the six possible
directed edge weights for that triple. A threshold above 1 is
interpreted as a percentage (for example, 1.5 means 1.5 percent); a
threshold at or below 1 is interpreted as a proportion.

Non-`"any"` significance has three important boundaries. For aggregate
census input, observed counts use the selected threshold but the
delegated null tests the unthresholded network; the function emits a
warning. For individual census input, the threshold is reapplied to each
integerized stub-null replicate. For individual named-instance input,
the optimized null classifies raw stub presence and therefore does not
reapply `edge_method`/`edge_threshold`. In all weighted individual
paths, positive fractional weights retain at least one stub, which
preserves support but can change the mass scale used by
`"percent"`/`"expected"`. These limitations do not affect descriptive
results with `significance = FALSE` or the default
`edge_method = "any"`.

## See also

[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`plot.cograph_motif_analysis()`](https://sonsoles.me/cograph/reference/plot.cograph_motif_analysis.md),
[`plot.cograph_motifs()`](https://sonsoles.me/cograph/reference/plot.cograph_motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
# Census from a matrix (no significance test -- fastest path)
mat <- matrix(c(0,3,2,0, 0,0,5,1, 0,0,0,4, 2,0,0,0), 4, 4, byrow = TRUE)
rownames(mat) <- colnames(mat) <- c("Plan","Execute","Monitor","Adapt")
motifs(mat, significance = FALSE)
#> Motif Census 
#> Level: aggregate | States: 4 | Pattern: triangle 
#> 
#> Type distribution:
#> 030C 030T 
#>    2    2 
#> 
#> Top 2 results:
#>  type count
#>  030C     2
#>  030T     2

# With a minimal significance test (set n_perm >= 500 in practice)
motifs(mat, n_perm = 10L, seed = 1)
#> Motif Census 
#> Level: aggregate | States: 4 | Pattern: triangle 
#> Significance: permutation (n_perm=10)
#> 
#> Type distribution:
#> 030C 030T 
#>    2    2 
#> 
#> Top 2 results:
#>  type count expected    z         p   sig
#>  030C     2      0.8 1.16 0.4545455 FALSE
#>  030T     2      0.8 1.16 0.4545455 FALSE
# \donttest{
Mod <- tna::tna(head(tna::group_regulation, 100))
motifs(Mod, n_perm = 10L, seed = 1)
#> Motif Census 
#> Level: individual | 100 units | States: 9 | Pattern: triangle 
#> Significance: permutation (n_perm=10)
#> 
#> Type distribution:
#> 120C 030C  210 030T 120U 120D  300 
#>   75   54   42   37   12   10    8 
#> 
#> Top 7 results:
#>  type count expected    z          p   sig
#>  030T    37     19.5 4.09 0.09090909 FALSE
#>  120C    75     59.0 2.39 0.09090909 FALSE
#>   210    42     34.4 1.30 0.27272727 FALSE
#>  120U    12      9.1 1.08 0.36363636 FALSE
#>  030C    54     49.0 0.96 0.27272727 FALSE
#>  120D    10      7.6 0.87 0.45454545 FALSE
#>   300     8      7.6 0.14 1.00000000 FALSE
subgraphs(Mod, n_perm = 10L, seed = 1)
#> Showing triangle patterns (count >= 5). For all MAN types use pattern = 'all'.
#> Motif Subgraphs 
#> Level: individual | 100 units | States: 9 | Pattern: triangle 
#> Significance: permutation (n_perm=10)
#> Min count: >= 5 
#> 
#> Type distribution:
#> 
#> 120C 030C  210 
#>    7    2    2 
#> 
#> Top 11 results:
#>                             triad     node1      node2     node3 observed type
#>   consensus - discuss - synthesis consensus    discuss synthesis        7 120C
#>   consensus - discuss - synthesis consensus    discuss synthesis        6 030C
#>     consensus - discuss - emotion consensus    discuss   emotion        9  210
#>    cohesion - consensus - emotion  cohesion  consensus   emotion        6 120C
#>        consensus - emotion - plan consensus    emotion      plan       10  210
#>     consensus - coregulate - plan consensus coregulate      plan        9 120C
#>  consensus - coregulate - emotion consensus coregulate   emotion        6 120C
#>        consensus - emotion - plan consensus    emotion      plan        6 030C
#>    cohesion - consensus - discuss  cohesion  consensus   discuss        5 120C
#>     consensus - discuss - emotion consensus    discuss   emotion        6 120C
#>        consensus - emotion - plan consensus    emotion      plan        5 120C
#>  expected     z          p   sig
#>       1.4  8.01 0.09090909 FALSE
#>       0.9  4.63 0.09090909 FALSE
#>       3.2  3.93 0.09090909 FALSE
#>       3.5  2.57 0.09090909 FALSE
#>       4.6  2.55 0.09090909 FALSE
#>       4.5  1.90 0.18181818 FALSE
#>       3.1  1.90 0.18181818 FALSE
#>       3.1  1.62 0.27272727 FALSE
#>       2.8  1.30 0.18181818 FALSE
#>       4.5  0.69 0.54545455 FALSE
#>       6.1 -0.49 0.90909091 FALSE
# }
```
