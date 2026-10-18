# Network Motif Analysis

Classifies the node triples of a network into the 16 directed MAN triad
types and tests their frequencies against a permutation null. The
function has two modes.

- In census mode (`named_nodes = FALSE`, the default), the result counts
  the triads of each MAN type. Nodes are exchangeable.

- In instance mode (`named_nodes = TRUE`, or
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)),
  the result lists the node triples that form each type.

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
  min_count = if (named_nodes) 2L else NULL,
  edge_method = c("any", "expected", "percent"),
  edge_threshold = 1.5,
  min_transitions = if (named_nodes) 3 else 5,
  top = NULL,
  seed = NULL,
  family = c("observed", "all"),
  p_adjust = "none",
  strict = TRUE,
  count_repeats = FALSE
)

# S3 method for class 'cograph_motif_result'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("results", "types"),
  significant = FALSE
)
```

## Arguments

- x:

  Input data: a tna object, cograph_network, matrix, igraph object or
  edge-list data frame. An edge list has the columns `from` and `to`
  (any case), an optional weight column `weight` or `count` (1 per row
  otherwise), the optional actor column described under `actor`, and an
  optional order column (`timestamp`, `time`, `seq`, `step` or `order`)
  that sorts the transitions within each actor before windowing. An edge
  list without `from` or `to` raises an error of class
  `cograph_bad_edgelist`. For
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), a
  `cograph_motif_result` object returned by `motifs()` or
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md).

- named_nodes:

  Logical. If FALSE (default), the MAN type census is computed. If TRUE,
  the individual node triples are listed.
  [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md)
  sets this to TRUE.

- actor:

  Character. Name of the edge-list column that identifies the units. If
  NULL (default), the first column named `session_id`, `session`,
  `actor`, `user`, `participant`, `individual` or `id` (in that order,
  ignoring case) is used. Without such a column the analysis is
  aggregate.

- window:

  Numeric. Window size for edge-list input. Each actor's transitions are
  cut into windows of this many transitions, and every window is
  analysed as one unit (individual level). `window` needs an actor
  column; without one it raises an error of class
  `cograph_window_needs_actor`. NULL (default) applies no windowing.

- window_type:

  Character. `"rolling"` (default) windows start at every transition and
  overlap; `"tumbling"` windows are consecutive and do not overlap. Used
  only when `window` is set.

- pattern:

  Which MAN triad types to include in the analysis:

  `"triangle"`

  :   (default) The 7 closed triangle types 030C, 030T, 120C, 120D,
      120U, 210 and 300.

  `"network"`

  :   All types except 003 (empty), 012 (single edge) and 021C (chain).

  `"closed"`

  :   The same 7 closed types as `"triangle"`: every pair of the three
      nodes is linked.

  `"all"`

  :   All 16 MAN types.

- include:

  Character vector of MAN types to keep. When supplied, `pattern` and
  `exclude` are ignored, and the result records the pattern as
  `"custom"`.

- exclude:

  Character vector of MAN types to drop in addition to those removed by
  `pattern`.

- significance:

  Logical. If TRUE (default), a permutation significance test is run. In
  instance mode the test requires individual data, and for aggregate
  input it is skipped with a warning.

- n_perm:

  Number of permutations for significance. When `significance = TRUE`,
  must be a whole number of at least 2. Default 1000.

- cores:

  Number of worker processes for the permutation null. Default 1 runs
  serially and draws all replicates from a single RNG stream.
  `cores > 1` gives each replicate its own L'Ecuyer-CMRG stream, so the
  result depends on `seed` alone and is the same for every worker count.
  The serial and parallel streams differ, so a serial run and a parallel
  run with the same seed give different p-values. Forking is used where
  available, and Windows uses a PSOCK cluster. Only the individual-level
  census null is parallelized. Values above
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  are capped with a `cograph_cores_capped` warning.

- min_count:

  Inclusive minimum count for a row to be kept. In census mode it
  filters the `count` column, the number of triads of each MAN type. In
  instance mode it filters the `observed` column. At individual level
  this is the number of units showing the triad, and at aggregate level
  it is the weighted edge mass of the triad (the sum of its 6 directed
  edge weights). Default 2 in instance mode, since at individual level a
  pattern seen in a single unit cannot recur, and NULL (no filter) in
  census mode. At aggregate level the threshold applies to the edge
  weight mass, so for probability matrices lower it (for example
  `min_count = 1` or `NULL`).

- edge_method:

  Method for determining edge presence: `"any"` (default; any positive
  edge), `"expected"` (ratio of the observed weight to the weight
  expected from the row and column totals), or `"percent"` (edge weight
  divided by the six-edge triad total).

- edge_threshold:

  Threshold for `"expected"` or `"percent"` methods. For `"expected"`,
  1.5 means 50 percent above expected. For `"percent"`, values at or
  below 1 are proportions and values above 1 are percentages. Default
  1.5.

- min_transitions:

  Minimum total edge weight for a unit to be included, counted after
  self-loops are removed unless `count_repeats = TRUE`. Default 3 in
  instance mode, the fewest transitions that can form a closed triad
  (030C and 030T have three edges), and 5 in census mode. At aggregate
  level the network is the only unit, so a network whose weights sum to
  less than this value gives NULL; for a probability matrix set
  `min_transitions = 0`.

- top:

  Integer or NULL. Only the first `top` rows of the results are kept.
  NULL (default) keeps all rows.

- seed:

  Random seed. Default NULL. When supplied, the caller's RNG state is
  restored on exit.

- family:

  Which rows the instance-mode significance test covers. `"observed"`
  (default) tests the triple-type rows observed in at least `min_count`
  units. `"all"` tests every node triple crossed with every retained MAN
  type, including combinations no unit shows, and `min_count` then
  filters only the rows returned. Testing only observed rows selects on
  the outcome: a rare configuration enters the test only when it happens
  to occur, which inflates the false-positive rate well above the
  nominal level. `"all"` keeps it at or below the nominal level. The
  full family has `choose(n_states, 3)` rows per retained labelled
  pattern with `strict = TRUE` (for example six for 030T, two for 030C
  and one for 300), or per retained MAN type with `strict = FALSE`, so
  the permutation null grows accordingly. Ignored in census mode and
  without individual-level significance.

- p_adjust:

  Multiplicity correction applied to the p-values of every tested row,
  one of [`p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html).
  Default `"none"`. The adjusted values are returned in `p_adj` and
  `sig` is `p_adj < 0.05`. With `family = "all"` the adjustment covers
  the full family before `min_count` is applied. Plot colours and stars
  follow `p_adj`, so they match `sig`.

- strict:

  Logical. In instance mode, whether a row is one labelled pattern of a
  triple (`TRUE`, default) or one MAN type of a triple (`FALSE`). A MAN
  type fixes the shape of a triad but not which node plays which role:
  the cycles A to B to C to A and A to C to B to A are both 030C. With
  `strict = TRUE` the two are separate rows, a unit counts toward a row
  only when the triple shows exactly that set of arrows, and the null
  counts the same exact pattern. With `strict = FALSE` rows pool every
  labelled pattern of the type and `pattern` reports the most frequent
  one. Ignored in census mode.

- count_repeats:

  Logical. Whether repeated codes (self-transitions such as A to A)
  count as transitions. Default `FALSE`: repeats are removed before
  anything else, so only transitions between different codes decide
  which units are analysed and enter the permutation null. With `TRUE`,
  repeats count toward `min_transitions` (a unit coded A, A, A, B, C has
  four transitions instead of two) and are shuffled with the other
  transitions in the null, which treats repeated codes as distinct
  events whose order is random. The null then also reflects how often
  units repeat: units that repeat a lot have fewer ties between
  different codes than their shuffles, which raises the number of dense
  triads the null expects. Observed triads never contain a repeat in
  either case, because a triad is made of ties between three different
  nodes.

- row.names, optional:

  Standard [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)
  arguments. `row.names` replaces the default row names; `optional` is
  ignored.

- ...:

  Unused.

- what:

  Which table
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  returns, either `"results"` (default) or `"types"`. The Value section
  lists the columns of each.

- significant:

  Logical. When `TRUE`,
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) keeps
  only the rows whose `sig` flag is `TRUE`. It applies to
  `what = "results"` and needs a result computed with
  `significance = TRUE`; otherwise a `cograph_no_significance` error is
  raised. Default `FALSE`.

## Value

A `cograph_motif_result` object, or NULL with a message when no motif
passes the filters. The object is a list with the elements below.

- results:

  Data frame of results. In census mode it has one row per retained,
  observed MAN type and the columns `type` and `count`. In instance mode
  it has one row per node triple and labelled pattern (`strict = TRUE`)
  or per node triple and MAN type (`strict = FALSE`), with the columns
  `triad`, `node1`, `node2`, `node3`, `type`, `observed`, `pattern`,
  `pattern_n` and `pattern_code`. At individual level, `observed` is the
  number of units in which the triple shows that pattern (or type), so
  one triple can occupy several rows. `pattern` is the labelled edge set
  of the row (for example `"A -> B, B <-> C"`). With `strict = FALSE` a
  row pools patterns that differ in direction, so `pattern` is the most
  frequent of them and `pattern_n` the number of units showing it; at
  individual level with `strict = TRUE` `pattern_n` equals `observed`.
  `pattern_code` is the same edge set as a six-bit code in `node1`,
  `node2`, `node3` order (1: node1 to node2, 2: node2 to node1, 4: node1
  to node3, 8: node3 to node1, 16: node2 to node3, 32: node3 to node2).
  Modal ties go to the smallest code. Rows that no unit shows
  (`family = "all"`) have `observed = 0` and `pattern_n = 0`; their
  pattern is the row's own pattern when `strict = TRUE` and missing
  otherwise. At aggregate level `pattern_n` is 1. With
  `significance = TRUE`, the columns `expected` (null mean), `z`, `p`,
  `p_adj` and `sig` are added, unrounded, and the rows are sorted by
  decreasing absolute z-score. Otherwise the rows are sorted by
  decreasing count. Census results carry `expected`, `z`, `p`, `p_adj`
  and `sig` in the same way.

- type_summary:

  Named `table` of counts per MAN type, sorted in decreasing order. In
  census mode it holds the `count` column. In instance mode it holds the
  number of result rows of each type: node triple and labelled pattern
  with `strict = TRUE`, node triple and type with `strict = FALSE`.

- level:

  `"individual"` when the input carried per-unit data, otherwise
  `"aggregate"`.

- named_nodes:

  The value of the `named_nodes` argument.

- n_units:

  Number of units analyzed: the units whose transition total (loop-free
  unless `count_repeats = TRUE`) reaches `min_transitions`. 1 at
  aggregate level.

- n_units_total:

  Number of units supplied, before the `min_transitions` filter.

- params:

  List of the analysis settings: `labels`, `n_states`, `pattern`
  (`"custom"` when `include` is used), `include`, `edge_method`,
  `edge_threshold`, `significance`, `n_perm`, `min_count`,
  `min_transitions`, `count_repeats`, `family`, `p_adjust`, `strict`,
  `window`, `window_type` and `actor` (the actor column used, or NULL at
  aggregate level).

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
one of these tables as a plain data frame. With `what = "results"`
(default) it returns the `results` table described above. With
`what = "types"` it returns one row per MAN type with columns `type` and
`count`, where `count` is the number of triads of that type in a census
or the number of result rows of that type in instance mode (see
`type_summary`).

## Details

The input type and the analysis level are detected automatically. Inputs
that carry per-unit data are analyzed per unit. These are tna objects,
networks that store sequence data (such as
[`as_cograph()`](https://sonsoles.me/cograph/reference/as_cograph.md) of
a tna or a network from
[`Nestimate::build_network()`](https://pak.dynasite.org/Nestimate/reference/build_network.html)),
and edge lists (or networks built from edge lists) with an actor column.
Matrices, igraph objects and other networks are analyzed as one
aggregate network. The adjacency is always classified as directed. The
four-class undirected census is computed by
`motif_census(..., directed = FALSE)`.

For aggregate inputs, significance is computed by
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
with its degree-preserving rewiring null on the simple loop-free graph.
Individual weighted inputs use a directed stub-matching null. Each
positive edge weight is converted to at least one integer stub, and
target stubs are shuffled within each unit so that its integer in- and
out-margins are preserved. The resulting multigraph, which may contain
loops or parallel edges, is classified through its simple loop-free
triad projection. Self-loops are removed before counting. They are also
removed before the null is constructed, unless `count_repeats = TRUE`,
in which case they are shuffled with the other transitions.

With `edge_method = "percent"`, edge presence is computed within each
node triple. The weight of an edge is divided by the sum of the six
possible directed edge weights of that triple. A threshold above 1 is
read as a percentage (1.5 means 1.5 percent), and a threshold at or
below 1 is read as a proportion.

With an `edge_method` other than `"any"`, the significance test has
limits. For aggregate census input, the observed counts use the
threshold while the null tests the unthresholded network, and a warning
is raised. For individual census input, the threshold is reapplied to
each stub-null replicate. For individual instance input, the null
classifies raw stub presence and does not reapply `edge_method` or
`edge_threshold`. In every weighted individual null, a positive
fractional weight keeps at least one stub. This preserves the support
but can change the weight scale used by `"percent"` and `"expected"`.
Descriptive results with `significance = FALSE` or `edge_method = "any"`
are unaffected.

## Printing and plotting

Printing the result shows the analysis settings, the MAN type
distribution and the first 20 rows of the results table.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the tidy tables.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
is documented in
[`plot-results`](https://sonsoles.me/cograph/reference/plot-results.md).

## See also

[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md)

Other motifs:
[`extract_motifs()`](https://sonsoles.me/cograph/reference/extract_motifs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
census <- motifs(regulation_net, significance = FALSE)
as.data.frame(census, what = "types")
#>   type count
#> 1 030T    11
#> 2 120C     3
#> 3 030C     2
#> 4 120D     2
#> 5 120U     1
```
