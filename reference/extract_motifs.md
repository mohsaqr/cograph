# Extract Motifs from Network Data

Extracts the triads of a network, classifies each one into the 16 MAN
types of a directed network, and optionally tests each triad against a
permutation null model. The analysis is individual-level for tna objects
and for `data` with `id`, and aggregate for matrices, networks and
igraph objects.

## Usage

``` r
extract_motifs(
  x = NULL,
  data = NULL,
  id = NULL,
  level = NULL,
  edge_method = c("any", "expected", "percent"),
  edge_threshold = 1.5,
  pattern = c("triangle", "network", "closed", "all"),
  exclude_types = NULL,
  include_types = NULL,
  top = NULL,
  by_type = FALSE,
  min_transitions = 3,
  significance = FALSE,
  n_perm = 100,
  seed = NULL
)
```

## Arguments

- x:

  Input data. Can be:

  - A `tna` object (supports individual-level analysis)

  - A matrix (aggregate analysis only, unless `data` and `id` provided)

  - A `cograph_network` object

  - An `igraph` object

- data:

  Optional data.frame of transitions for individual-level analysis, used
  together with `id`. Required columns: `from`, `to` and the column(s)
  named in `id`. A `weight` column, or a `count` column as returned by
  [`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
  gives the transition weights (1 otherwise). When `x` is not a tna
  object, `data` is used and `x` is ignored. `data` without `id` raises
  an error of class `cograph_data_needs_id`.

- id:

  Column name(s) identifying individuals/groups in `data`. Can be a
  single string or a character vector; each distinct combination of the
  values is one individual.

- level:

  Analysis level: "individual" counts how many individuals show each
  triad, and "aggregate" analyzes the network summed over individuals.
  The default is "individual" for a tna object or when `data` and `id`
  are supplied, and "aggregate" otherwise. Requesting "individual"
  without individual data raises a warning and the aggregate level is
  used.

- edge_method:

  Method for determining edge presence:

  "any"

  :   Edge exists if the weight is positive.

  "expected"

  :   Edge exists if observed / expected is at least the threshold, with
      the expectation from the row and column totals.

  "percent"

  :   Edge exists if the edge weight divided by the six-edge triad total
      is at least the threshold.

  Default "any".

- edge_threshold:

  Threshold for "expected" or "percent". For "expected", a ratio (1.5
  means 50\\ values at or below 1 are proportions and values above 1 are
  percentages, so the default 1.5 means 1.5 percent; set it explicitly
  for "percent". Ignored when edge_method = "any". Default 1.5.

- pattern:

  Pattern filter for which triads to include:

  "triangle"

  :   All 3 node pairs must be connected (any direction). Types: 030C,
      030T, 120C, 120D, 120U, 210, 300. Default.

  "network"

  :   Excludes the empty triad and the sequential patterns 003, 012 and
      021C. Stars and triangles are kept.

  "closed"

  :   The 7 closed types, the same set as `"triangle"`.

  "all"

  :   All 16 MAN types.

- exclude_types:

  Character vector of MAN types to explicitly exclude. Applied after
  pattern filter. E.g., c("300") to exclude cliques.

- include_types:

  Character vector of MAN types to include. When supplied, only these
  types are returned, and `pattern` and `exclude_types` are ignored.

- top:

  Number of rows to return, taken after sorting (and after the `by_type`
  grouping when `by_type = TRUE`). NULL or 0 returns all rows. Default
  NULL.

- by_type:

  If TRUE, the rows are grouped by MAN type in MAN order (003 to 300),
  keeping the sort order within each type. Default FALSE.

- min_transitions:

  At individual level: minimum total transitions, repeats excluded, for
  an individual to be analysed. At aggregate level: minimum six-edge
  weight for a triad to be listed. Default 3, the fewest transitions
  that can form a closed triad. With probability or relative weights at
  aggregate level, use 0.

- significance:

  Logical. Run the permutation test? Needs individual-level data.
  Default FALSE.

- n_perm:

  Number of permutations for the significance test. When
  `significance = TRUE`, must be a whole number of at least 2. Default
  100.

- seed:

  Optional random seed. The caller's random number state is restored on
  exit.

## Value

A `cograph_motif_analysis` object, or `NULL` with a warning when no
triad passes the filters.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
its tables:

- `what = "results"`:

  One row per node triple and MAN type, with columns `triad` (display
  label), `node1`, `node2`, `node3`, `type`, `observed` (units showing
  that type, or the six-edge weight at aggregate level) and, with
  `significance = TRUE`, `expected`, `z`, `p` (empirical permutation
  p-value) and `sig` (logical, `p < 0.05`). Rows are sorted by
  decreasing `|z|`, or by decreasing `observed` without significance. A
  triple that has different types in different individuals appears in
  more than one row.

- `what = "types"`:

  One row per MAN type with the total of `observed` over all rows of
  that type, before `top`.

## Details

`extract_motifs()` is
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md) with
`strict = FALSE`: a row is one node triple and MAN type, and its
`observed` value is the number of units in which the triple shows that
type. Counting, the activity filter and the permutation null are those
of [`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
described in
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md).
Self-loops are removed before counting.

Significance needs individual-level data. For aggregate input a warning
of class `cograph_no_units` is raised and the result has no significance
columns.

## MAN Notation

The 16 triad types use MAN (Mutual-Asymmetric-Null) notation where:

- First digit: number of mutual (bidirectional) pairs.

- Second digit: number of asymmetric (one-way) pairs.

- Third digit: number of null (no edge) pairs.

- Letter suffix: subtype (C = cycle, T = transitive, D = down, U = up).
  D and U describe how the type is drawn, not whether it contains an
  out-star or an in-star.

## Pattern Types

The names are those used in the plot titles.

- Triangle patterns (all pairs connected)::

  030C (cycle), 030T (feed-forward), 120C (cycle + mutual), 120D
  (out-star onto mutual), 120U (in-star from mutual), 210 (near-clique),
  300 (clique)

- Network patterns::

  021D (out-star), 021U (in-star), 102 (mutual pair), 111D (in-star +
  mutual), 111U (out-star + mutual), 201 (two mutuals), plus all
  triangle patterns

- Sequential patterns (chains)::

  012 (single edge), 021C (A-\>B-\>C chain)

- Empty::

  003 (no edges)

## Printing and plotting

Printing the result shows the analysis settings, the number of units
analysed, the MAN type distribution and the first 20 triads.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the result
is documented in
[`plot-results`](https://sonsoles.me/cograph/reference/plot-results.md).

## See also

[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)

Other motifs:
[`extract_triads()`](https://sonsoles.me/cograph/reference/extract_triads.md),
[`get_edge_list()`](https://sonsoles.me/cograph/reference/get_edge_list.md),
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md),
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md),
[`subgraphs()`](https://sonsoles.me/cograph/reference/subgraphs.md),
[`triad_census()`](https://sonsoles.me/cograph/reference/triad_census.md)

## Examples

``` r
m <- extract_motifs(regulation_net, min_transitions = 0)
m
#> Motif Analysis
#> Pattern: triangle | Edge method: any
#> Aggregate network | States: 10 | Triads: 19 shown of 19
#> 
#> Type distribution:
#> 030T 120C 120D 030C 120U 
#> 8.37 3.52 1.92 1.34 1.19 
#> 
#> Top 19 triads:
#>                             triad type observed
#> 1     Monitor - Evaluate - Create 120C     1.26
#> 2        Monitor - Create - Share 120C     1.26
#> 3          Plan - Monitor - Share 120U     1.19
#> 4        Plan - Evaluate - Create 030T     1.08
#> 5     Explore - Reflect - Discuss 120D     1.05
#> 6         Monitor - Adapt - Share 030T     1.04
#> 7           Plan - Create - Share 120C     1.00
#> 8       Plan - Monitor - Evaluate 030T     0.95
#> 9         Explore - Adapt - Share 030C     0.94
#> 10      Explore - Adapt - Discuss 030T     0.92
#> 11     Monitor - Adapt - Evaluate 030T     0.92
#> 12        Plan - Monitor - Create 120D     0.87
#> 13        Plan - Discuss - Create 030T     0.74
#> 14       Explore - Create - Share 030T     0.64
#> 15 Monitor - Reflect - Synthesize 030T     0.64
#> 16     Explore - Discuss - Create 030T     0.58
#> 17   Monitor - Reflect - Evaluate 030T     0.55
#> 18   Monitor - Adapt - Synthesize 030C     0.40
#> 19    Plan - Monitor - Synthesize 030T     0.31
as.data.frame(m)
#>                             triad   node1    node2      node3 type observed
#> 1     Monitor - Evaluate - Create Monitor Evaluate     Create 120C     1.26
#> 2        Monitor - Create - Share Monitor   Create      Share 120C     1.26
#> 3          Plan - Monitor - Share    Plan  Monitor      Share 120U     1.19
#> 4        Plan - Evaluate - Create    Plan Evaluate     Create 030T     1.08
#> 5     Explore - Reflect - Discuss Explore  Reflect    Discuss 120D     1.05
#> 6         Monitor - Adapt - Share Monitor    Adapt      Share 030T     1.04
#> 7           Plan - Create - Share    Plan   Create      Share 120C     1.00
#> 8       Plan - Monitor - Evaluate    Plan  Monitor   Evaluate 030T     0.95
#> 9         Explore - Adapt - Share Explore    Adapt      Share 030C     0.94
#> 10      Explore - Adapt - Discuss Explore    Adapt    Discuss 030T     0.92
#> 11     Monitor - Adapt - Evaluate Monitor    Adapt   Evaluate 030T     0.92
#> 12        Plan - Monitor - Create    Plan  Monitor     Create 120D     0.87
#> 13        Plan - Discuss - Create    Plan  Discuss     Create 030T     0.74
#> 14       Explore - Create - Share Explore   Create      Share 030T     0.64
#> 15 Monitor - Reflect - Synthesize Monitor  Reflect Synthesize 030T     0.64
#> 16     Explore - Discuss - Create Explore  Discuss     Create 030T     0.58
#> 17   Monitor - Reflect - Evaluate Monitor  Reflect   Evaluate 030T     0.55
#> 18   Monitor - Adapt - Synthesize Monitor    Adapt Synthesize 030C     0.40
#> 19    Plan - Monitor - Synthesize    Plan  Monitor Synthesize 030T     0.31
```
