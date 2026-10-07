# Extract Motifs from Network Data

Extracts the triads of a network, classifies each one into the 16 MAN
types of a directed network, and optionally tests each triad against a
permutation null model. The analysis is individual-level for tna objects
and grouped data, and aggregate for matrices and networks.

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
  min_transitions = 5,
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

  Optional data.frame containing transition data with an ID column for
  individual-level analysis. Required columns: `from`, `to`, and the
  column(s) specified in `id`. An optional `weight` column gives the
  transition weights (1 otherwise). `data` is used when `x` is not a tna
  object, and `x` is then ignored.

- id:

  Column name(s) identifying individuals/groups in `data`. Can be a
  single string or character vector for multiple grouping columns.
  Required for individual-level analysis with non-tna inputs.

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

  :   Edge exists if count \> 0.

  "expected"

  :   Edge exists if observed/expected \>= threshold.

  "percent"

  :   Edge exists if edge/total \>= threshold.

  Default "any".

- edge_threshold:

  Threshold value for "expected" or "percent" methods. For "expected", a
  ratio (1.5 means 50\\ For "percent", a proportion of the total triad
  weight (for example 0.15). The default 1.5 is intended for "expected",
  so a value for "percent" should be set explicitly. Ignored when
  edge_method = "any". Default 1.5.

- pattern:

  Pattern filter for which triads to include:

  "triangle"

  :   All 3 node pairs must be connected (any direction). Types: 030C,
      030T, 120C, 120D, 120U, 210, 300. Default.

  "network"

  :   Excludes the empty triad and the sequential patterns 003, 012 and
      021C. Stars and triangles are kept.

  "closed"

  :   Excludes 003, 012, 021C and 120C.

  "all"

  :   All 16 MAN types.

- exclude_types:

  Character vector of MAN types to explicitly exclude. Applied after
  pattern filter. E.g., c("300") to exclude cliques.

- include_types:

  Character vector of MAN types to include. When supplied, only these
  types are returned, and `pattern` and `exclude_types` are ignored.

- top:

  Number of rows to return, taken after sorting by observed count (by
  z-score when `significance = TRUE`). NULL returns all rows. Default
  NULL.

- by_type:

  If TRUE, the rows are sorted by MAN type and then by observed count.
  Default FALSE.

- min_transitions:

  At individual level: minimum total transitions for a person to be
  included in the analysis. At aggregate level: minimum triad weight to
  count as present. Default 5.

- significance:

  Logical. Run permutation significance test? Default FALSE.

- n_perm:

  Number of permutations for the significance test. When
  `significance = TRUE`, must be a whole number of at least 2. Default
  100.

- seed:

  Optional random seed. The caller's random number state is restored on
  exit.

## Value

A `cograph_motif_analysis` object (list) containing the elements below,
or `NULL` with a warning when no triad passes the filters.

- results:

  Data frame with one row per node-triple and MAN type, the display
  label `triad`, unambiguous `node1`/`node2`/ `node3` columns, its
  observed count, and (if `significance = TRUE`) expected count,
  z-score, empirical p-value, and significance marker. A node triple
  that has different types across individuals therefore appears in more
  than one row.

- type_summary:

  A `table` of triad counts by MAN type, summed over individuals and
  sorted in decreasing order.

- params:

  List of the settings used, with the number of individuals, the number
  of states and the node labels.

## Details

Self-loops are removed before the activity filter, the counting and the
null model. Significance is assessed against a directed weighted
stub-matching null model. Each weight is rounded to an integer number of
stubs, and a positive weight that rounds to zero keeps one stub. The
target stubs are shuffled, which preserves the in- and out-strengths of
every unit. Loops and multiple edges created by the shuffle are
collapsed to a simple graph before the triads are classified. The
aggregate mode of
[`motifs()`](https://sonsoles.me/cograph/reference/motifs.md) uses the
simple-graph rewiring null of
[`motif_census()`](https://sonsoles.me/cograph/reference/motif_census.md)
instead.

The selected `edge_method` is reapplied to each null replicate. Since
small positive weights are rounded up to one stub, the total weight of a
replicate can differ from the observed total under `"percent"` and
`"expected"`. Results without significance testing and results with
`edge_method = "any"` do not depend on this rounding.

## MAN Notation

The 16 triad types use MAN (Mutual-Asymmetric-Null) notation where:

- First digit: number of mutual (bidirectional) pairs.

- Second digit: number of asymmetric (one-way) pairs.

- Third digit: number of null (no edge) pairs.

- Letter suffix: subtype (C = cycle, T = transitive, D = down, U = up).

## Pattern Types

- Triangle patterns (all pairs connected)::

  030C (cycle), 030T (feed-forward), 120C (regulated cycle), 120D (two
  out-stars), 120U (two in-stars), 210 (mutual+asymmetric), 300 (clique)

- Network patterns::

  021D (out-star), 021U (in-star), 102 (mutual pair), 111D
  (out-star+mutual), 111U (in-star+mutual), 201 (two mutual pairs), plus
  all triangle patterns

- Sequential patterns (chains)::

  012 (single edge), 021C (A-\>B-\>C chain)

- Empty::

  003 (no edges)

## Printing and plotting

Printing the result shows the analysis settings, the MAN type
distribution and the first 20 triads.
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
extract_motifs(regulation_net, min_transitions = 0)
#> Motif Analysis
#> Pattern: triangle | Edge method: any
#> Individuals: 1 | States: 10 | Total triads: 19
#> 
#> Type distribution:
#> 
#> 030T 120C 030C 120D 120U 
#>   11    3    2    2    1 
#> 
#> Top 19 triads:
#>                             triad type observed
#> 1         Explore - Adapt - Share 030C        1
#> 2    Monitor - Adapt - Synthesize 030C        1
#> 3      Explore - Discuss - Create 030T        1
#> 4         Plan - Discuss - Create 030T        1
#> 5        Plan - Evaluate - Create 030T        1
#> 6       Explore - Adapt - Discuss 030T        1
#> 7      Monitor - Adapt - Evaluate 030T        1
#> 8       Plan - Monitor - Evaluate 030T        1
#> 9    Monitor - Reflect - Evaluate 030T        1
#> 10        Monitor - Adapt - Share 030T        1
#> 11       Explore - Create - Share 030T        1
#> 12    Plan - Monitor - Synthesize 030T        1
#> 13 Monitor - Reflect - Synthesize 030T        1
#> 14    Monitor - Evaluate - Create 120C        1
#> 15       Monitor - Create - Share 120C        1
#> 16          Plan - Create - Share 120C        1
#> 17        Plan - Monitor - Create 120D        1
#> 18    Explore - Reflect - Discuss 120D        1
#> 19         Plan - Monitor - Share 120U        1
```
