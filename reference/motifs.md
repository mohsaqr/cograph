# Network Motif Analysis

Two modes of motif analysis for networks:

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
  `pattern`.

- exclude:

  Character vector of MAN types to exclude. Applied after `pattern`
  filter.

- significance:

  Logical. Run permutation significance test? Default TRUE.

- n_perm:

  Number of permutations for significance. Default 1000.

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

  Method for determining edge presence: "any" (default), "expected", or
  "percent".

- edge_threshold:

  Threshold for "expected" or "percent" methods. Default 1.5.

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
      back to patterns (census mode). Each panel title reads
      `"<MAN code>: <description>"` (e.g. `"030T: Feed-forward"`) and,
      in census mode, appends the z-score and a significance star (`*`
      p\<.05, `**` p\<.01, `***` p\<.001). Arranged in a grid.

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
  per MAN type with columns `type`, `count`, and when
  `significance = TRUE` also `expected`, `z`, `p`, `sig`. Instance mode
  (`named_nodes = TRUE`): one row per concrete node triple with columns
  `triad`, `type`, `observed`, and when `significance = TRUE` also
  `expected`, `z`, `p`, `sig`.

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
(matrices, igraph), analyzes the single network.

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

if (FALSE) { # \dontrun{
# With a minimal significance test (set n_perm >= 500 in practice)
motifs(mat, n_perm = 10L, seed = 1)
} # }

if (FALSE) { # \dontrun{
Mod <- tna::tna(tna::group_regulation)
motifs(Mod, n_perm = 10L, seed = 1)
subgraphs(Mod, n_perm = 10L, seed = 1)
} # }
```
