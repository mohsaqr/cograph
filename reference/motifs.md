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
  min_count = if (named_nodes) 5L else NULL,
  edge_method = c("any", "expected", "percent"),
  edge_threshold = 1.5,
  min_transitions = 5,
  top = NULL,
  seed = NULL
)

# S3 method for class 'cograph_motif_result'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("results", "types")
)
```

## Arguments

- x:

  Input data: a tna object, cograph_network, matrix, igraph object or
  edge-list data frame. For
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
  split into windows of this size. NULL (default) applies no windowing.

- window_type:

  Character. `"rolling"` (default) or `"tumbling"`. Used only when
  `window` is set.

- pattern:

  Which MAN triad types to include in the analysis:

  `"triangle"`

  :   (default) The 7 closed triangle types 030C, 030T, 120C, 120D,
      120U, 210 and 300.

  `"network"`

  :   All types except 003 (empty), 012 (single edge) and 021C (chain).

  `"closed"`

  :   All types except 003, 012, 021C and 120C.

  `"all"`

  :   All 16 MAN types.

- include:

  Character vector of MAN types to keep. When supplied, `pattern` and
  `exclude` are ignored.

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
  edge weights). Default 5 in instance mode and NULL (no filter) in
  census mode.

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

  Minimum total edge weight for a unit to be included. Default 5. At
  aggregate level the network is the only unit, so a network whose
  weights sum to less than this value gives NULL.

- top:

  Integer or NULL. Only the first `top` rows of the results are kept.
  NULL (default) keeps all rows.

- seed:

  Random seed. Default NULL. When supplied, the caller's RNG state is
  restored on exit.

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

## Value

A `cograph_motif_result` object, or NULL with a message when no motif
passes the filters. The object is a list with the elements below.

- results:

  Data frame of results. In census mode it has one row per retained,
  observed MAN type and the columns `type` and `count`. In instance mode
  it has one row per node triple and MAN type and the columns `triad`,
  `node1`, `node2`, `node3`, `type` and `observed`. At individual level,
  `observed` is the number of units in which the triple has that type,
  so one triple can occupy several rows. With `significance = TRUE`, the
  columns `expected`, `z`, `p` and `sig` are added and the rows are
  sorted by decreasing absolute z-score. Otherwise the rows are sorted
  by decreasing count.

- type_summary:

  Named `table` of counts per MAN type, sorted in decreasing order. In
  census mode it holds the `count` column. In instance mode it holds the
  number of node triples of each type.

- level:

  `"individual"` when the input carried per-unit data, otherwise
  `"aggregate"`.

- named_nodes:

  The value of the `named_nodes` argument.

- n_units:

  Number of units analyzed. 1 at aggregate level.

- params:

  List of the analysis settings: `labels`, `n_states`, `pattern`,
  `edge_method`, `edge_threshold`, `significance`, `n_perm`,
  `min_count`, `window`, `window_type` and `actor`.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
one of these tables as a plain data frame. With `what = "results"`
(default) it returns the `results` table described above. With
`what = "types"` it returns one row per MAN type with columns `type` and
`count`, where `count` is the number of triads of that type in a census
or the number of node triples of that type in instance mode.

## Details

The input type and the analysis level are detected automatically. Inputs
that carry per-unit data are analyzed per unit. These are tna objects,
networks that store tna sequence data, and edge lists (or networks built
from edge lists) with an actor column. Matrices, igraph objects and
other networks are analyzed as one aggregate network. The adjacency is
always classified as directed. The four-class undirected census is
computed by `motif_census(..., directed = FALSE)`.

For aggregate inputs, significance is computed by
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
with its degree-preserving rewiring null on the simple loop-free graph.
Individual weighted inputs use a directed stub-matching null. Each
positive edge weight is converted to at least one integer stub, and
target stubs are shuffled within each unit so that its integer in- and
out-margins are preserved. The resulting multigraph, which may contain
loops or parallel edges, is classified through its simple loop-free
triad projection. Self-loops are removed before counting and before the
null is constructed.

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
