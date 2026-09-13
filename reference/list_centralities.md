# Catalogue of the Centrality Measures

A tidy table of every measure
[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) can
compute, with the facts you need before you read a column of results:
which end of the scale marks a prominent node, whether the measure needs
a community partition, whether it reads edge weights, and whether it is
held back from `type = "all"` because its cost grows steeply.

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

Twelve measures are oriented so that a **low** value marks the more
central node, and sorting their column the usual way puts the periphery
on top. Filter with `orientation = "lower"` to see them.

## See also

[`centrality`](https://sonsoles.me/cograph/reference/centrality.md) to
compute them,
[`centrality_degree`](https://sonsoles.me/cograph/reference/centrality_degree.md)
and the other one-measure verbs.

## Examples

``` r
# Every measure, with the facts needed to read its column
head(list_centralities())
#>               measure orientation mode_aware needs_membership uses_weights
#> 1  access_information       lower      FALSE            FALSE        FALSE
#> 2 adaptive_leaderrank      higher      FALSE            FALSE        FALSE
#> 3               alpha      higher       TRUE            FALSE         TRUE
#> 4           authority      higher      FALSE            FALSE         TRUE
#> 5    average_distance       lower       TRUE            FALSE         TRUE
#> 6          barycenter      higher       TRUE            FALSE         TRUE
#>   costly
#> 1  FALSE
#> 2  FALSE
#> 3  FALSE
#> 4  FALSE
#> 5  FALSE
#> 6  FALSE

# The measures where a low value marks the more central node
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

# The measures held back from type = "all"
list_centralities(costly = TRUE)
#>                          measure orientation mode_aware needs_membership
#> 1               bridging_capital      higher      FALSE            FALSE
#> 2                    controlrank      higher      FALSE            FALSE
#> 3           dynamical_importance      higher      FALSE            FALSE
#> 4  entropy_variation_betweenness      higher      FALSE            FALSE
#> 5                            epc      higher      FALSE            FALSE
#> 6                      exogenous      higher       TRUE            FALSE
#> 7        extended_local_bridging      higher      FALSE            FALSE
#> 8                  fragmentation      higher       TRUE            FALSE
#> 9                            iec      higher      FALSE            FALSE
#> 10                     infection      higher      FALSE            FALSE
#> 11                      linerank      higher      FALSE            FALSE
#> 12                           mcc      higher      FALSE            FALSE
#> 13     node_contraction_improved      higher      FALSE            FALSE
#> 14             random_walk_decay      higher      FALSE            FALSE
#> 15          resistance_curvature      higher      FALSE            FALSE
#> 16               rsp_betweenness      higher      FALSE            FALSE
#> 17                trust_pagerank      higher      FALSE            FALSE
#> 18                    two_way_rw      higher      FALSE            FALSE
#>    uses_weights costly
#> 1          TRUE   TRUE
#> 2          TRUE   TRUE
#> 3          TRUE   TRUE
#> 4         FALSE   TRUE
#> 5         FALSE   TRUE
#> 6         FALSE   TRUE
#> 7         FALSE   TRUE
#> 8          TRUE   TRUE
#> 9         FALSE   TRUE
#> 10        FALSE   TRUE
#> 11         TRUE   TRUE
#> 12        FALSE   TRUE
#> 13        FALSE   TRUE
#> 14         TRUE   TRUE
#> 15         TRUE   TRUE
#> 16         TRUE   TRUE
#> 17        FALSE   TRUE
#> 18         TRUE   TRUE
```
