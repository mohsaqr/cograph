# Catalogue of the Centrality Measures

Returns a table of every measure
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) can
compute. For each measure the table records which end of the scale marks
a prominent node, whether the measure accepts `mode`, whether it
requires a community partition, whether it uses edge weights, and
whether `type = "all"` holds it back because its cost grows steeply with
network size.

## Usage

``` r
list_centralities(orientation = NULL, costly = NULL, needs_membership = NULL)
```

## Arguments

- orientation:

  Keep only measures with this orientation: `"higher"` or `"lower"`.
  Default `NULL` keeps both.

- costly:

  Keep only costly measures (`TRUE`) or only the rest (`FALSE`). Default
  `NULL` keeps both.

- needs_membership:

  Keep only measures that require a partition (`TRUE`) or only those
  that do not (`FALSE`). Default `NULL` keeps both.

## Value

A `data.frame` with one row per measure and the columns `measure` (the
name to pass to `centrality(measures = )`), `orientation` (`"higher"` or
`"lower"`, which end of the scale marks a prominent node), `mode_aware`
(whether the measure accepts `mode` and its column carries a mode
suffix), `needs_membership`, `uses_weights`, and `costly` (held back
from `type = "all"`; add it with `include = `). Rows are ordered by
measure name.

## Details

For some measures a low value marks the more central node, so a
descending sort of their column places the most peripheral nodes first.
`orientation = "lower"` lists them.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) to
compute them,
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md)
and the other one-measure verbs.

## Examples

``` r
list_centralities(orientation = "lower")
#>                   measure orientation mode_aware needs_membership uses_weights
#> 1      access_information       lower      FALSE            FALSE        FALSE
#> 2        average_distance       lower       TRUE            FALSE         TRUE
#> 3              constraint       lower      FALSE            FALSE         TRUE
#> 4            eccentricity       lower       TRUE            FALSE         TRUE
#> 5                 heatmap       lower       TRUE            FALSE        FALSE
#> 6        hide_information       lower      FALSE            FALSE        FALSE
#> 7         local_dimension       lower       TRUE            FALSE        FALSE
#> 8   local_dimension_fixed       lower       TRUE            FALSE        FALSE
#> 9           local_entropy       lower       TRUE            FALSE        FALSE
#> 10 local_volume_dimension       lower       TRUE            FALSE        FALSE
#> 11           second_order       lower      FALSE            FALSE        FALSE
#> 12                 wiener       lower       TRUE            FALSE         TRUE
#>    costly
#> 1   FALSE
#> 2   FALSE
#> 3   FALSE
#> 4   FALSE
#> 5   FALSE
#> 6   FALSE
#> 7   FALSE
#> 8   FALSE
#> 9   FALSE
#> 10  FALSE
#> 11  FALSE
#> 12  FALSE
```
